%%%-------------------------------------------------------------------
%%% @author aseem
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2022 9:28 PM
%%%-------------------------------------------------------------------
-module(topology).
-author("aseem").

%% API
-export([findNeighboursInFull/4, findNeighboursInLine/4, findNeighboursIn2d/4, findNeighboursIn3d/4]).

%%%---------------------------------------------------------------------
%%% Creation of Full Topology and filling Neighbours
findNeighboursInFull(NumNodes, StateMap, NumNodes, _) -> StateMap;
findNeighboursInFull(Position, StateMap, NumNodes, ProcessList) ->
  {Left, [_|Right]} = lists:split(Position, ProcessList),
  CurrentNeighbours = Left ++ Right,
  UpdatedState = maps:update(lists:nth(Position+1, ProcessList), [0, CurrentNeighbours], StateMap),
  findNeighboursInFull(Position + 1, UpdatedState, NumNodes, ProcessList).
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% Creation of Line topology and filling Neighbors
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

%%%----------------------------------------------------------------------
%%% Creation of 2D topology and filling Neighbors
findNeighboursIn2d(NumNodes, StateMap, NumNodes, _) -> StateMap;
findNeighboursIn2d(Position, StateMap, NumNodes, ProcessList) ->
  N = round(math:sqrt(length(ProcessList))),
  RowIndex = Position div N,
  ColIndex = (Position rem N),
%%  io:format("N = ~p, RowIndex = ~p, ColIndex = ~p~n", [N, RowIndex, ColIndex]),
  %% Adding Vertical Neighbours
  if
    RowIndex==0 -> VerticalNeighbours = [lists:nth((RowIndex+1)*N + ColIndex+1, ProcessList)];
    RowIndex==(NumNodes div N - 1) -> VerticalNeighbours = [lists:nth(ColIndex+1 + (RowIndex-1)*N, ProcessList)];
    true-> VerticalNeighbours = [lists:nth((RowIndex+1)*N + (ColIndex+1), ProcessList), lists:nth((ColIndex+1) + N*(RowIndex-1), ProcessList)]
  end,
  %% Adding horizontal neighbours
  if
    ColIndex == 0 -> HorizontalNeighbours = [lists:nth(N*RowIndex + ColIndex+2, ProcessList)];
    ColIndex == (NumNodes div N - 1) -> HorizontalNeighbours = [lists:nth(ColIndex + N*RowIndex, ProcessList)];
    true-> HorizontalNeighbours = [lists:nth(N*RowIndex + ColIndex+2, ProcessList), lists:nth(ColIndex + N*RowIndex, ProcessList)]
  end,
  %% Adding Right Diagonal neighbors
  if
    ((RowIndex==0) and (ColIndex==(NumNodes div N) - 1)) -> RightDiagonalNeighbours = [];
    ((RowIndex==(NumNodes div N) - 1) and (ColIndex==0)) -> RightDiagonalNeighbours = [];
    ((RowIndex==0) or (ColIndex==0)) -> RightDiagonalNeighbours = [lists:nth((RowIndex+1)*N + ColIndex+2, ProcessList)];
    ((RowIndex==(NumNodes div N - 1)) or (ColIndex==(NumNodes div N - 1))) -> RightDiagonalNeighbours = [lists:nth((RowIndex-1)*N + ColIndex, ProcessList)];
    true -> RightDiagonalNeighbours = [lists:nth((RowIndex+1)*N + ColIndex+2, ProcessList), lists:nth((RowIndex-1)*N + ColIndex, ProcessList)]
  end,
  %% Adding Left Diagonal Neighbours
  if
    ((RowIndex==0) and (ColIndex==0)) -> LeftDiagonalNeighbours = [];
    ((RowIndex==(NumNodes div N - 1)) and (ColIndex==(NumNodes div N - 1))) -> LeftDiagonalNeighbours = [];
    (RowIndex==0) -> LeftDiagonalNeighbours = [lists:nth((RowIndex+1)*N + ColIndex, ProcessList)];
    (ColIndex==0) -> LeftDiagonalNeighbours = [lists:nth((RowIndex-1)*N + ColIndex+2, ProcessList)];
    (ColIndex==(NumNodes div N - 1)) -> LeftDiagonalNeighbours = [lists:nth((RowIndex+1)*N + ColIndex, ProcessList)];
    (RowIndex==(NumNodes div N - 1)) -> LeftDiagonalNeighbours = [lists:nth((RowIndex-1)*N + ColIndex+2, ProcessList)];
    true -> LeftDiagonalNeighbours = [lists:nth((RowIndex+1)*N + ColIndex, ProcessList), lists:nth((RowIndex-1)*N + ColIndex+2, ProcessList)]
  end,
  CurrentNeighbours = VerticalNeighbours ++ HorizontalNeighbours ++ RightDiagonalNeighbours ++ LeftDiagonalNeighbours,
  UpdatedState = maps:update(lists:nth(Position+1, ProcessList), [0, CurrentNeighbours], StateMap),
  findNeighboursIn2d(Position+1, UpdatedState, NumNodes, ProcessList).
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% Creation of 3d topology and filling Neighbors
findNeighboursIn3d(NumNodes, StateMap, NumNodes, _) -> StateMap;
findNeighboursIn3d(Position, StateMap, NumNodes, ProcessList) ->
  N = round(math:sqrt(length(ProcessList))),
  RowIndex = Position div N,
  ColIndex = (Position rem N),
  %% Adding Vertical Neighbours
  if
    RowIndex==0 -> VerticalNeighbours = [lists:nth((RowIndex+1)*N + ColIndex+1, ProcessList)];
    RowIndex==(NumNodes div N - 1) -> VerticalNeighbours = [lists:nth(ColIndex+1 + (RowIndex-1)*N, ProcessList)];
    true-> VerticalNeighbours = [lists:nth((RowIndex+1)*N + (ColIndex+1), ProcessList), lists:nth((ColIndex+1) + N*(RowIndex-1), ProcessList)]
  end,
  %% Adding horizontal neighbours
  if
    ColIndex == 0 -> HorizontalNeighbours = [lists:nth(N*RowIndex + ColIndex+2, ProcessList)];
    ColIndex == (NumNodes div N - 1) -> HorizontalNeighbours = [lists:nth(ColIndex + N*RowIndex, ProcessList)];
    true-> HorizontalNeighbours = [lists:nth(N*RowIndex + ColIndex+2, ProcessList), lists:nth(ColIndex + N*RowIndex, ProcessList)]
  end,
  %% Adding Right Diagonal neighbors
  if
    ((RowIndex==0) and (ColIndex==(NumNodes div N) - 1)) -> RightDiagonalNeighbours = [];
    ((RowIndex==(NumNodes div N) - 1) and (ColIndex==0)) -> RightDiagonalNeighbours = [];
    ((RowIndex==0) or (ColIndex==0)) -> RightDiagonalNeighbours = [lists:nth((RowIndex+1)*N + ColIndex+2, ProcessList)];
    ((RowIndex==(NumNodes div N - 1)) or (ColIndex==(NumNodes div N - 1))) -> RightDiagonalNeighbours = [lists:nth((RowIndex-1)*N + ColIndex, ProcessList)];
    true -> RightDiagonalNeighbours = [lists:nth((RowIndex+1)*N + ColIndex+2, ProcessList), lists:nth((RowIndex-1)*N + ColIndex, ProcessList)]
  end,
  %% Adding Left Diagonal Neighbours
  if
    ((RowIndex==0) and (ColIndex==0)) -> LeftDiagonalNeighbours = [];
    ((RowIndex==(NumNodes div N - 1)) and (ColIndex==(NumNodes div N - 1))) -> LeftDiagonalNeighbours = [];
    (RowIndex==0) -> LeftDiagonalNeighbours = [lists:nth((RowIndex+1)*N + ColIndex, ProcessList)];
    (ColIndex==0) -> LeftDiagonalNeighbours = [lists:nth((RowIndex-1)*N + ColIndex+2, ProcessList)];
    (ColIndex==(NumNodes div N - 1)) -> LeftDiagonalNeighbours = [lists:nth((RowIndex+1)*N + ColIndex, ProcessList)];
    (RowIndex==(NumNodes div N - 1)) -> LeftDiagonalNeighbours = [lists:nth((RowIndex-1)*N + ColIndex+2, ProcessList)];
    true -> LeftDiagonalNeighbours = [lists:nth((RowIndex+1)*N + ColIndex, ProcessList), lists:nth((RowIndex-1)*N + ColIndex+2, ProcessList)]
  end,
  %% Adding the extra Random Node
  TwoDNeighbours = VerticalNeighbours ++ HorizontalNeighbours ++ RightDiagonalNeighbours ++ LeftDiagonalNeighbours,
  AllNeighbours = add3dNode(Position, ProcessList, TwoDNeighbours, 0),
  UpdatedState = maps:update(lists:nth(Position+1, ProcessList), [0, AllNeighbours], StateMap),
  findNeighboursIn3d(Position+1, UpdatedState, NumNodes, ProcessList).

add3dNode(_, _, Neighbours, 1) -> Neighbours;
add3dNode(Position, ProcessList, TwoDNeighbours, Done) ->
  RandomNodePID = lists:nth(rand:uniform(length((ProcessList))), ProcessList),
  Present = lists:member(RandomNodePID, TwoDNeighbours),
  CurrentNode = lists:nth(Position+1, ProcessList),
  if
    ((Present) or (RandomNodePID == CurrentNode))->
      add3dNode(Position, ProcessList, TwoDNeighbours, Done);
    true ->
      add3dNode(Position, ProcessList, TwoDNeighbours++[RandomNodePID], 1)
  end.
%%%----------------------------------------------------------------------