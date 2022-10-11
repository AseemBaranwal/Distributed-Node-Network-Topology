%%%-------------------------------------------------------------------
%%% @author aseem
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2022 12:27 PM
%%%-------------------------------------------------------------------
-module(process).
-author("aseem").
-define(GossipConvergenceLimit, 10).
-define(PushSumConvergenceLimit, 3).

%% API
-behavior(gen_server).
-export([start/1, startGenServer/1, spawnProcesses/3, init/1, handle_cast/2, createTopology/5, handle_call/3, terminate/2]).

handle_call(_, _, _) ->
  erlang:error(not_implemented).

startGenServer(ProcessNumber) ->
  {ok, Pid} = gen_server:start_link({local, list_to_atom(string:concat("Process", integer_to_list(ProcessNumber)))}, ?MODULE, [], []),
  Pid.

spawnProcesses(AllProcesses, AllProcesses, ProcessList) -> ProcessList;
spawnProcesses(AllProcesses, StartVal, ProcessList) ->
  spawnProcesses(AllProcesses, StartVal + 1, ProcessList ++ [startGenServer(StartVal)]).

start({ProcessCount, Topology, Algorithm}) ->
  TType = string:lowercase(Topology),
  if TType =:= "2d"; TType =:= "3d" ->
    if
      TType =:= "3d", ProcessCount < 16 -> io:fwrite("You need at least 16 nodes for 3d Layout !! ~n Try Again!~n");
      true -> ok
    end,
    AllProcesses = round(math:pow(round(math:sqrt(ProcessCount)), 2));
    true -> AllProcesses = ProcessCount
  end,
  ProcessPIDList = spawnProcesses(AllProcesses, 0, []),
  server:startServer(),
  global:sync(),
  ServerPID = global:whereis_name(serverPid),
  StartTime = erlang:system_time(millisecond),
  gen_server:cast(ServerPID, {initializeServerState, ProcessPIDList, Topology, StartTime}),
  createTopology(ProcessPIDList, 0, Topology, Algorithm, AllProcesses),
  gen_server:cast(ServerPID, {startAlgorithm, ProcessPIDList, Algorithm}).

createTopology(_, TotalProcesses, _, _, TotalProcesses) -> ok;
createTopology(ProcessPIDList, Start, Topology, Algorithm, TotalProcesses) ->
  case string:lowercase(Topology) of
    "full" -> {Left, [_|Right]} = lists:split(Start -0, ProcessPIDList),
      NeighbourList = Left ++ Right;
    "2d"->
      NeighbourList = topology:get2DNeighbours(Start, ProcessPIDList, [], TotalProcesses);
    "3d" -> NeighbourList = topology:get3dNeighbours(Start, ProcessPIDList, [], TotalProcesses);
    "line" -> NeighbourList = topology:getLineNeighbours(Start, ProcessPIDList, [], TotalProcesses)
  end,
  ProcessName = list_to_atom(string:concat("Process", integer_to_list(Start))),
  gen_server:cast(ProcessName, {initializeProcessState, NeighbourList, Topology, Start}),
  createTopology(ProcessPIDList, Start + 1, Topology, Algorithm, TotalProcesses).

%% gen server callback functions
init(_Args) ->
  {ok, maps:new()}.

handle_cast({initializeProcessState, NeighbourList, Topology, StartVal}, State) ->
  StateHavingNeighbour = maps:merge(State, maps:put("neighbours", NeighbourList, State)),
  StateHavingVisitCount = maps:merge(StateHavingNeighbour, maps:put("count", 0, State)),
  StateHavingAlive = maps:merge(StateHavingVisitCount, maps:put("isAlive", true, State)),
  StateHavingS = maps:merge(StateHavingAlive, maps:put("s", StartVal + 1, State)),
  StateHavingW = maps:merge(StateHavingS, maps:put("w", 1, State)),
  StateHavingRatioOfSAndW = maps:merge(StateHavingW, maps:put("ratio", 1, State)),
  FinalState = maps:merge(StateHavingRatioOfSAndW, maps:put("topology", Topology, State)),
  {noreply, FinalState};
handle_cast({implementGossip, IsActive}, State) ->
  global:sync(),
  ServerPID = global:whereis_name(serverPid),
  VisitCount = maps:get("count", State, 0),
  IsAlive = maps:get("isAlive", State, false),
  if VisitCount < ?GossipConvergenceLimit ->
    case IsActive of
      false ->
        UpdatedState = maps:update_with("count", fun(X) -> X + 1 end, 0, State),
        gen_server:cast(self(), {implementGossip, true});
      true ->
        AllNeighbours = maps:get("neighbours", State, []),
        if length(AllNeighbours) =/= 0 ->
          Receiver = lists:nth(rand:uniform(length(AllNeighbours)), AllNeighbours),
          IsProcessAlive = is_process_alive(Receiver),
          if IsProcessAlive =:= true ->
            UpdatedState = State,
            gen_server:cast(Receiver, {implementGossip, false});
            IsProcessAlive =:= false ->
              NewNeighbours = AllNeighbours -- [Receiver],
              UpdatedState = maps:update("neighbours", NewNeighbours, State),
              gen_server:cast(self(), {implementGossip, true})
          end;
          true ->
            UpdatedState = maps:update("isAlive", false, State),
            gen_server:cast(ServerPID, {exitProcess, self(), true})
        end
    end;
    IsAlive =:= true, VisitCount >= ?GossipConvergenceLimit ->
      NewNeighbourState = maps:update("neighbours", [], State),
      UpdatedState = maps:update("isAlive", false, NewNeighbourState),
      gen_server:cast(ServerPID, {exitProcess, self(), true});
    IsAlive =:= false, VisitCount >= ?GossipConvergenceLimit ->
      gen_server:cast(ServerPID, {exitProcess, self(), true}),
      UpdatedState = State;
    true -> UpdatedState = State
  end,
  {noreply, UpdatedState};
handle_cast({implementPushSum, SVal, Weight}, State) ->
  global:sync(),
  ServerPID = global:whereis_name(serverPid),
  CheckIsAlive = maps:get("isAlive", State),
  AllNeighbours = maps:get("neighbours", State),
  CurrentStateValue = maps:get("s", State),
  CurrentWeightValue = maps:get("w", State),
  CurrentConvergenceCount = maps:get("count", State),
  CurrentStateWeightRatio = maps:get("ratio", State),
  UpdatedStateValue = CurrentStateValue + SVal,
  UpdatedWeightValue = CurrentWeightValue + Weight,
  UpdateStateValueRatio = UpdatedStateValue / UpdatedWeightValue,
  Change = abs(UpdateStateValueRatio - CurrentStateWeightRatio),
  ConvergenceCondition = math:pow(10, -10),
  if
    length(AllNeighbours) =/= 0 ->
      RandomNeighbour = lists:nth(rand:uniform(length(AllNeighbours)), AllNeighbours),
      IsNeighbourAlive = is_process_alive(RandomNeighbour),
      if
        (CheckIsAlive =:= false) ->
          gen_server:cast(ServerPID, {callOtherProcess, SVal, Weight}),
          FinalState = State;
        ((Change < ConvergenceCondition) and (CurrentConvergenceCount < ?PushSumConvergenceLimit)) ->
            TempState = updatePushSumProcessState(UpdatedStateValue/2, UpdatedWeightValue/2, CurrentConvergenceCount+1, UpdateStateValueRatio, State),
            if IsNeighbourAlive =:= true->
              gen_server:cast(RandomNeighbour, {implementPushSum, UpdatedStateValue/2, UpdatedWeightValue/2}),
              FinalState = TempState;
              true ->
                UpdatedNeighbours = AllNeighbours -- [RandomNeighbour],
                StateWithNewNeighbours = maps:update("neighbours", UpdatedNeighbours, TempState),
                FinalState = updatePushSumProcessState(CurrentStateValue, CurrentWeightValue, CurrentConvergenceCount, CurrentStateWeightRatio, StateWithNewNeighbours),
                gen_server:cast(self(), {implementPushSum, SVal, Weight})
            end;
        ((Change > ConvergenceCondition) and (CurrentConvergenceCount < ?PushSumConvergenceLimit)) ->
          TempState = updatePushSumProcessState(UpdatedStateValue/2, UpdatedWeightValue/2, 0, UpdateStateValueRatio, State),
          if IsNeighbourAlive =:= true->
            gen_server:cast(RandomNeighbour, {implementPushSum, UpdatedStateValue/2, UpdatedWeightValue/2}),
            FinalState = TempState;
            true ->
              UpdatedNeighbours = AllNeighbours -- [RandomNeighbour],
              StateWithNewNeighbours = maps:update("neighbours", UpdatedNeighbours, TempState),
              FinalState = updatePushSumProcessState(CurrentStateValue, CurrentWeightValue, CurrentConvergenceCount, CurrentStateWeightRatio, StateWithNewNeighbours),
              gen_server:cast(self(), {implementPushSum, SVal, Weight})
          end;
        true -> FinalState = State
      end;
    true ->
      gen_server:cast(ServerPID, {callOtherProcess, SVal, Weight}),
      FinalState = State
  end,
  CurrentRoundCount = maps:get("count", FinalState),
  if Change < ConvergenceCondition, CurrentRoundCount >= ?PushSumConvergenceLimit, CheckIsAlive =:= true ->
    EndState = maps:update("isAlive", false, FinalState),
    gen_server:cast(ServerPID, {exitPushSum, self(), UpdatedStateValue/2, UpdatedWeightValue/2});
    true -> EndState = FinalState
  end,
  {noreply, EndState};
handle_cast({makeRandomNodeInActive}, State) ->
  NewState = maps:update("isAlive", false, State),
  {noreply, NewState};
handle_cast({stopNode}, State) ->
  {stop, normal, State}.

terminate(_Reason, _State) ->
  ok.

updatePushSumProcessState(StateValue, WeightValue, ConvergenceCount, StateWeightRatio, State) ->
  if ConvergenceCount =:= -1 ->
    NewCount = 0;
    true -> NewCount = ConvergenceCount
  end,
  UpdatedStateValue = maps:update("s", StateValue, State),
  UpdatedWeightValue = maps:update("w", WeightValue, UpdatedStateValue),
  UpdatedConvergenceCount = maps:update("count", NewCount, UpdatedWeightValue),
  FinalState = maps:update("ratio", StateWeightRatio, UpdatedConvergenceCount),
  FinalState.