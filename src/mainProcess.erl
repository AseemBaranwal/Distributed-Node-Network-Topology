%%%-------------------------------------------------------------------
%%% @author aseem
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(mainProcess).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(MaxConvergence, 10).

-record(mainProcess_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  {ok, NumNodes} = io:read("Enter the number of Nodes: "),
  {ok, TopologyType} = io:read("Enter the type of topology: "),
  {ok, AlgorithmUsed} = io:read("Enter the algorithm to be used: "),
  {ok, Return} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
  io:format("Main Process PID: ~p~n", [Return]),
  StateMap = maps:new(),
  MasterPID = spawn(fun() -> buildTopologyNodes(TopologyType, StateMap, NumNodes) end),
  MasterPID ! {spawnProcess, MasterPID, NumNodes},
  Return.

init([]) ->
  State = [],
  Return = {ok, State},
  io:format("Building Topology ... ~n"),
  Return.

handle_call(_Request, _From, State) ->
  Reply = ok,
  Return = {reply, Reply, State},
  io:format("handle_call: ~p~n", [Return]),
  Return.

handle_cast(_Msg, State) ->
  Return = {noreply, State},
  io:format("handle_cast: ~p~n", [Return]),
  Return.

handle_info(_Info, State) ->
  Return = {noreply, State},
  io:format("handle_info: ~p~n", [Return]),
  Return.

terminate(_Reason, _State) ->
  Return = ok,
  io:format("terminate: ~p~n", [Return]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  Return = {ok, State},
  io:format("code_change: ~p~n", [Return]),
  Return.

%%%===================================================================
%%% Internal functions
%%%===================================================================


buildTopologyNodes(TopologyType, StateMap, NumNodes) ->
  receive
    {spawnProcess, MasterPID, NumNodes} ->
      lists:foreach(
        fun(_) ->
          link(spawn(fun() -> createNode(MasterPID) end))
        end, lists:seq(1, NumNodes)),
      buildTopologyNodes(TopologyType, StateMap, NumNodes);
    {NodePID} ->
      UpdatedStateMap = updateMap(StateMap, NodePID, 0),
      MapSize = maps:size(UpdatedStateMap),
      if
        MapSize == NumNodes ->
          createTopology(TopologyType, StateMap, NumNodes);
        true -> ok
      end,
      buildTopologyNodes(TopologyType, UpdatedStateMap, NumNodes)
  end.

createNode(MasterPID) ->
  NodePID = self(),
  io:fwrite("New Node created with PID ~p~n", [NodePID]),
  MasterPID ! {NodePID}.

updateMap(NodesMap, NodePID, N) ->
  Map = maps:new(),
  Map1 = maps:put(NodePID, [0, []], Map),
  Map2 = maps:merge(NodesMap, Map1),
  Map2.

createTopology(TopologyType, StateMap, NumNodes) ->
  erlang:error(not_implemented).