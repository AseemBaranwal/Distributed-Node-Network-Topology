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
-export([getLineNeighbours/4, get2DNeighbours/4, get3dNeighbours/4]).

getElementAtIndex(List, Index) ->
  {_, [Element|_]} = lists:split(Index, List),
  Element.

getLineNeighbours(Index, ProcessIDList, _, AllProcessCount) ->
  if Index =:= 0 , Index + 1 < AllProcessCount ->
    NeighboursForLine = [getElementAtIndex(ProcessIDList, Index + 1)];
    Index =:= AllProcessCount - 1, Index - 1 >= 0 ->
      NeighboursForLine = [getElementAtIndex(ProcessIDList, Index - 1)];
    Index - 1 >= 0, Index + 1 < AllProcessCount ->
      NeighboursForLine = [getElementAtIndex(ProcessIDList, Index - 1)] ++ [getElementAtIndex(ProcessIDList, Index + 1)];
    true -> NeighboursForLine = []
  end,
  NeighboursForLine.

get3dNeighbours(Position, NodePidList, NeighbourList, TotalNodes)->
  TwoDNeighbourList = get2DNeighbours(Position, NodePidList, NeighbourList, TotalNodes),
  RemainingProcesses = NodePidList -- ([getElementAtIndex(NodePidList, Position)] ++ TwoDNeighbourList),
  if length(RemainingProcesses) =/= 0 ->
    RandomNeighbourFor3D = [lists:nth(rand:uniform(length(RemainingProcesses)), RemainingProcesses)];
    true -> RandomNeighbourFor3D = []
  end,
  ThreeDNeighbours = TwoDNeighbourList ++ RandomNeighbourFor3D,
  ThreeDNeighbours.

get2DNeighbours(Index, NodePidList, NeighbourList, TotalNodes) ->
  GridSize = round(math:sqrt(TotalNodes)),
  if ((Index + 1) rem GridSize) =/= 0 ->
    RightNeighbour = [getElementAtIndex(NodePidList, Index + 1)];
    true -> RightNeighbour = []
  end,
  if (Index - 1) >= 0 , (Index rem GridSize) =/= 0 ->
    LeftNeighbour = [getElementAtIndex(NodePidList, Index - 1)];
    true -> LeftNeighbour = []
  end,
  if (Index + GridSize) < TotalNodes ->
    DownNeighbour = [getElementAtIndex(NodePidList, Index + GridSize)];
    true -> DownNeighbour = []
  end,
  if (Index - GridSize) >= 0 ->
    UpNeighbour = [getElementAtIndex(NodePidList, Index - GridSize)];
    true-> UpNeighbour = []
  end,
  if (Index + GridSize + 1) < TotalNodes , (Index + 1) rem GridSize =/= 0->
    DownLeftNeighbour = [getElementAtIndex(NodePidList, Index + GridSize + 1)];
    true-> DownLeftNeighbour = []
  end,
  if (Index - GridSize - 1) >= 0 , Index rem GridSize =/= 0->
    UpLeftNeighbour = [getElementAtIndex(NodePidList, Index - GridSize - 1)];
    true-> UpLeftNeighbour = []
  end,
  if (Index + GridSize - 1) < TotalNodes , Index rem GridSize =/= 0 ->
    DownRightNeighbour = [getElementAtIndex(NodePidList, Index + GridSize - 1)];
    true-> DownRightNeighbour = []
  end,
  if (Index - GridSize + 1) >= 0 , (Index + 1) rem GridSize =/= 0 ->
    UpRightNeighbour = [getElementAtIndex(NodePidList, Index - GridSize + 1)];
    true-> UpRightNeighbour = []
  end,
  FinalNeighbours = sets:to_list(sets:from_list(NeighbourList ++ RightNeighbour ++ LeftNeighbour ++ UpNeighbour ++ DownNeighbour ++ DownLeftNeighbour ++ UpLeftNeighbour ++ DownRightNeighbour  ++ UpRightNeighbour)),
  FinalNeighbours.
