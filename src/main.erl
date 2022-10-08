%%%-------------------------------------------------------------------
%%% @author aseem
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2022 12:07 AM
%%%-------------------------------------------------------------------
-module(main).
-author("aseem").
-export([start/0]).

%% API
-export([]).

start() ->
  {ok, NumNodes} = io:read("Enter the number of Nodes: "),
  {ok, TopologyType} = io:read("Enter the type of topology: "),
  {ok, Algorithm} = io:read("Enter the algorithm to be used: "),

  io:fwrite("~n----------------------------------------------------------------------------------~n~n"),

  io:format("Main Process PID: ~p~n", [self()]),
  StateMap = maps:new(),
  MasterPID = spawn(fun() -> buildTopologyNodes(TopologyType, StateMap, NumNodes) end),
  MasterPID ! {spawnProcess, MasterPID, NumNodes}.

mapTopologyNodes(TopologyType, StateMap, NumNodes, NumNodes) ->
  io:format("~p~n",[maps:to_list(StateMap)]),
  createTopology(TopologyType, StateMap, NumNodes);

mapTopologyNodes(TopologyType, StateMap, NumNodes, CountStateMap) ->
  receive
    {NodePID} ->
      UpdatedStateMap = updateMap(StateMap, NodePID, 0),
      mapTopologyNodes(TopologyType, UpdatedStateMap, NumNodes, CountStateMap + 1)
  end.

buildTopologyNodes(TopologyType, StateMap, NumNodes) ->
  receive
    {spawnProcess, MasterPID, NumNodes} ->
      lists:foreach(
        fun(_) ->
          link(spawn(fun() -> createNode(MasterPID) end))
        end, lists:seq(1, NumNodes)),
      mapTopologyNodes(TopologyType, StateMap, NumNodes, 0)
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
  ok.