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

updateMap(NodesMap, NodePID, _) ->
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
      State = createFullTopology(StateMap, NumNodes, ProcessList),
      io:format("~p~n",[State]);
    "2d" ->
      State = create2dTopology(StateMap, NumNodes, ProcessList),
      io:format("~p~n",[State]);
    "line" ->
      State = createLineTopology(StateMap, NumNodes, ProcessList),
      io:format("~p~n",[State]);
    "3d" ->
      State = create3dTopology(StateMap, NumNodes, ProcessList)
  end,
  State.

%%%---------------------------------------------------------------------
%%% Creation of Full Topology and filling Neighbours
createFullTopology(StateMap, NumNodes, ProcessList) -> findNeighboursInFull(0, StateMap, NumNodes, ProcessList).

findNeighboursInFull(NumNodes, StateMap, NumNodes, _) -> StateMap;
findNeighboursInFull(Position, StateMap, NumNodes, ProcessList) ->
  {Left, [_|Right]} = lists:split(Position, ProcessList),
  CurrentNeighbours = Left ++ Right,
  UpdatedState = maps:update(lists:nth(Position+1, ProcessList), [0, CurrentNeighbours], StateMap),
  findNeighboursInFull(Position + 1, UpdatedState, NumNodes, ProcessList).
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% Creation of 2D topology and filling Neighbors
create2dTopology(StateMap, NumNodes, ProcessList) -> findNeighboursIn2d(0, StateMap, NumNodes, ProcessList).

findNeighboursIn2d(NumNodes, StateMap, NumNodes, _) -> StateMap;
findNeighboursIn2d(Position, StateMap, NumNodes, ProcessList) ->
  N = round(math:sqrt(length(ProcessList))),
  RowIndex = Position div N,
  ColIndex = (Position rem N),
  io:format("N = ~p, RowIndex = ~p, ColIndex = ~p~n", [N, RowIndex, ColIndex]),
  if
    (RowIndex==0) -> VerticalNeighbours = [lists:nth((RowIndex+1)*N + ColIndex+1, ProcessList)];
    RowIndex==(NumNodes div N - 1) -> VerticalNeighbours = [lists:nth(ColIndex+1 + (RowIndex-1)*N, ProcessList)];
    true-> VerticalNeighbours = [lists:nth((RowIndex+1)*N + (ColIndex+1), ProcessList), lists:nth((ColIndex+1) + N*(RowIndex-1), ProcessList)]
  end,
  if
    ColIndex == 0 -> HorizontalNeighbours = [lists:nth(N*RowIndex + ColIndex+2, ProcessList)];
    ColIndex == (NumNodes div N - 1) -> HorizontalNeighbours = [lists:nth(ColIndex + N*RowIndex, ProcessList)];
    true-> HorizontalNeighbours = [lists:nth(N*RowIndex + ColIndex+2, ProcessList), lists:nth(ColIndex + N*RowIndex, ProcessList)]
  end,
  CurrentNeighbours = VerticalNeighbours ++ HorizontalNeighbours,
  UpdatedState = maps:update(lists:nth(Position+1, ProcessList), [0, CurrentNeighbours], StateMap),
  findNeighboursIn2d(Position+1, UpdatedState, NumNodes, ProcessList).
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% Creation of Line topology and filling Neighbors
createLineTopology(StateMap, NumNodes, ProcessList) -> findNeighboursInLine(0, StateMap, NumNodes, ProcessList).

findNeighboursInLine(NumNodes, StateMap, NumNodes, _) -> StateMap;
findNeighboursInLine(Position, StateMap, NumNodes, ProcessList) ->
  if
    ((Position == 0) and (length(ProcessList) > 0)) -> CurrentNeighbours = [lists:nth(2, ProcessList)];
    Position == length(ProcessList)-1 -> CurrentNeighbours = [lists:nth(NumNodes-1, ProcessList)];
    true -> CurrentNeighbours = [lists:nth(Position, ProcessList), lists:nth(Position+2, ProcessList)]
  end,
  UpdatedState = maps:update(lists:nth(Position+1, ProcessList), [0, CurrentNeighbours], StateMap),
  findNeighboursInLine(Position + 1, UpdatedState, NumNodes, ProcessList).
%%%----------------------------------------------------------------------

create3dTopology(StateMap, NumNodes, ProcessList) ->
  erlang:error(not_implemented).