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

start() ->
  {_, [NumNodes]} = io:fread("Enter the number of Nodes: ", "~d"),
  {_, [TopologyType]} = io:fread("Enter the type of topology: ", "~s"),
  {_, [Algorithm]} = io:fread("Enter the algorithm to be used: ", "~s"),

  io:fwrite("~n----------------------------------------------------------------------------------~n~n"),

  io:format("Main Process PID: ~p~n", [self()]),
  StateMap = maps:new(),
  MasterPID = spawn(fun() -> buildTopologyNodes(TopologyType, StateMap, NumNodes) end),
  MasterPID ! {spawnProcess, MasterPID, NumNodes}.

mapTopologyNodes(TopologyType, StateMap, NumNodes, NumNodes) ->
%%  io:format("~p~n",[maps:to_list(StateMap)]),
  TopologyState = createTopology(TopologyType, StateMap, NumNodes);

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
  ProcessList = maps:keys(StateMap),
  TType = string:lowercase(TopologyType),
%%  io:fwrite("~p~n", [ProcessList]).
  case TType of
    "full" ->
      io:fwrite("Inside Create~n"),
      State = createFullTopology(StateMap, NumNodes, ProcessList);
    "2d" ->
      State = create2dTopology(StateMap, NumNodes, ProcessList);
    "line" ->
      State = createLineTopology(StateMap, NumNodes, ProcessList);
    "3d" ->
      State = create3dTopology(StateMap, NumNodes, ProcessList)
  end,
  State.

createFullTopology(StateMap, NumNodes, ProcessList) ->
  io:fwrite("Inside Full Create~n"),
  Neighbour = findNeighbour(0, StateMap, NumNodes, ProcessList),
  ok.

findNeighbour(NumNodes, StateMap, NumNodes, ProcessList) -> io:fwrite("~p~n",[maps:to_list(StateMap)]);
findNeighbour(Start, StateMap, NumNodes, ProcessList) ->
  io:fwrite("Inside Neighbour~n"),
  {Left, [_|Right]} = lists:split(Start, ProcessList),
  CurrentNeighbours = Left ++ Right,
  UpdatedState = maps:update(lists:nth(Start+1, ProcessList), [0, CurrentNeighbours], StateMap),
  findNeighbour(Start + 1, UpdatedState, NumNodes, ProcessList).

create2dTopology(StateMap, NumNodes, ProcessList) ->
  erlang:error(not_implemented).

createLineTopology(StateMap, NumNodes, ProcessList) ->
  erlang:error(not_implemented).

create3dTopology(StateMap, NumNodes, ProcessList) ->
  erlang:error(not_implemented).