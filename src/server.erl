%%%-------------------------------------------------------------------
%%% @author aseem
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Oct 2022 6:16 PM
%%%-------------------------------------------------------------------
-module(server).
-author("aseem").

%% API
-export([startServer/0, init/1, handle_cast/2, terminate/2, handle_call/3]).
-behavior(gen_server).

startServer() ->
  {ok, Pid} = gen_server:start_link({global, serverPid}, ?MODULE, [], []),
  Pid.

init(_Args) ->
  {ok, maps:new()}.

handle_cast({initializeServerState, NodePidList, Topology, StartTime}, State) ->
  TotalNodes = length(NodePidList),
  StateWithNodePids = maps:merge(State, maps:put("nodePidList", NodePidList, State)),
  StateWithAliveNodes = maps:merge(StateWithNodePids, maps:put("aliveNodes", TotalNodes, State)),
  StateWithTopology = maps:merge(StateWithAliveNodes, maps:put("topology", Topology, State)),
  StateWithConvergence = maps:merge(StateWithTopology, maps:put("convergence", 0, State)),
  FinalState = maps:merge(StateWithConvergence, maps:put("startTime", StartTime, State)),
  {noreply, FinalState};

handle_cast({startAlgorithm, NodePidList, Algorithm}, State) ->
  AlgoUsed = string:lowercase(Algorithm),
  RandomProcessPID = lists:nth(rand:uniform(length(NodePidList)), NodePidList),
  if AlgoUsed =:= "gossip" ->
    gen_server:cast(RandomProcessPID, {implementGossip, false});
    AlgoUsed =:= "pushsum" ; AlgoUsed =:= "push-sum"->
      gen_server:cast(RandomProcessPID, {implementPushSum, 0, 0});
    true -> algorithm_not_matching
  end,
  {noreply, State};

handle_cast({exitProcess, Pid, IsNodeAlive}, State) ->
  AliveProcesses = maps:get("aliveNodes", State),
  ProcessPIList = maps:get("nodePidList", State),
  StartTime = maps:get("startTime", State),
%%  randomInactiveProcessMaker(ProcessPIList),
  if
    AliveProcesses =/= 0 , IsNodeAlive == true ->
      UpdatedPIDList = ProcessPIList -- [Pid],
      NewStateAliveNodes = maps:update("aliveNodes", AliveProcesses - 1, State),
      FinalState = maps:update("nodePidList", UpdatedPIDList, NewStateAliveNodes),
      if
        length(UpdatedPIDList) > 0 ->
          RandomPid = lists:nth(rand:uniform(length(UpdatedPIDList)), UpdatedPIDList),
          gen_server:cast(RandomPid, {implementGossip, false});
        true ->
          printConvergenceInfo(StartTime),
          stopServer(self())
      end;
      AliveProcesses =:= 0 ->
        printConvergenceInfo(StartTime),
        stopServer(self()),
        FinalState = State;
    true -> FinalState = State
  end,
  {noreply, FinalState};

handle_cast({exitPushSum, Pid, SVal, Weight}, State) ->
  AliveNodes = maps:get("aliveNodes", State),
  NodePidList = maps:get("nodePidList", State),
  StartTime = maps:get("startTime", State),
  if AliveNodes =/= 0 ->
    NewNodePidList = NodePidList -- [Pid],
    NewStateAliveNodes = maps:update("aliveNodes", AliveNodes - 1, State),
    FinalState = maps:update("nodePidList", NewNodePidList, NewStateAliveNodes),
    if length(NewNodePidList) > 0 ->
      RandomPid = lists:nth(rand:uniform(length(NewNodePidList)), NewNodePidList),
      gen_server:cast(RandomPid, {implementPushSum, SVal, Weight});
      true ->
        endProcessAndKillServer(StartTime)
    end;
    AliveNodes =:= 0 ->
      endProcessAndKillServer(StartTime),
      FinalState = State;
    true -> FinalState = State
  end,
  {noreply, FinalState};

handle_cast({callOtherProcess, SVal, Weight}, State) ->
  NodePidList = maps:get("nodePidList", State),
  StartTime = maps:get("startTime", State),
  if length(NodePidList) > 0 ->
    RandomPid = lists:nth(rand:uniform(length(NodePidList)), NodePidList),
    gen_server:cast(RandomPid, {implementPushSum, SVal, Weight});
    true -> endProcessAndKillServer(StartTime)
  end,
  {noreply, State};

handle_cast({stop}, State) -> {stop, normal,State}.

randomInactiveProcessMaker(NodePidList) ->
  RandomPid = lists:nth(rand:uniform(length(NodePidList)), NodePidList),
  gen_server:cast(RandomPid, {makeRandomNodeInActive}).

printConvergenceInfo(StartTime) ->
  EndTime = erlang:system_time(millisecond),
  TotalTime = EndTime - StartTime,
  io:format("Total time taken for convergence = ~p ms~n", [TotalTime]).

terminate(_Reason, _State) -> io:format("Server Terminated!~n").

stopServer(Pid)-> gen_server:cast(Pid, {stop}).

endProcessAndKillServer(StartTime) ->
  global:sync(),
  ServerPid = global:whereis_name(serverPid),
  EndTime = erlang:system_time(millisecond),
  TotalTime = EndTime - StartTime,
  io:format("Total time taken for convergence = ~p ms~n", [TotalTime]),
  exit(ServerPid,normal).

handle_call(_, _, _) ->
  erlang:error(not_implemented).