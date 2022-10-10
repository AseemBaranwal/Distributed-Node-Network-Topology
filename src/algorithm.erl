%%%-------------------------------------------------------------------
%%% @author aseem
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Oct 2022 5:53 PM
%%%-------------------------------------------------------------------
-module(algorithm).
-author("aseem").

%% API
-export([implementAlgorithm/0]).

implementAlgorithm() ->
  receive
    {neighbours, StateMap, Algorithm} ->
      io:fwrite("~p~n", [StateMap]),
      case Algorithm of
        "gossip" ->
          startGossip(StateMap)
%%        "pushsum" ->
%%          applyPushSum(StateMap)
      end
  end.

startGossip(StateMap) ->
  ProcessList = maps:keys(StateMap),
  GossipStarter = lists:nth(rand:uniform(length((ProcessList))), ProcessList),
  Message = "Hello World!",
  NeighbourList = lists:nth(2, maps:get(GossipStarter, StateMap)),
  UpdatedState = maps:update(GossipStarter, [1, NeighbourList], StateMap),
  io:fwrite("State: ~p~n", [UpdatedState]),
  ServerPID = spawn(fun() -> server:status() end),
  self() ! {worker},
  gossip(ServerPID, UpdatedState, GossipStarter, Message, ProcessList).

gossip(ServerPID, StateMap, Sender, Message, PList) ->
  receive
    {controller, GossipStarter, AliveList} ->
      io:fwrite("Came in This~n"),
      ActualSender = GossipStarter,
      ProcessList = AliveList;
    {worker} ->
      ActualSender = Sender,
      ProcessList = PList
  end,
  SenderNeighbourList = lists:nth(2, maps:get(ActualSender, StateMap)),
  Receiver = findAliveNode(StateMap, SenderNeighbourList, Sender, 0),
  [_,ReceiverNeighbourList] = maps:get(Receiver, StateMap),
  [VisitCount|_] = maps:get(Receiver, StateMap),
  UpdatedState = maps:update(Receiver, [VisitCount+1, ReceiverNeighbourList], StateMap),
  io:fwrite("AliveList = ~p, Receiver = ~p, VisitCount = ~p~n", [ProcessList, Receiver, VisitCount+1]),
  io:fwrite("~p~n", [UpdatedState]),
  if
    (VisitCount+1 < 10) ->
      self() ! {worker},
      gossip(ServerPID, UpdatedState, Receiver, Message, ProcessList);
    true ->
      ServerPID ! {self(), Receiver, ProcessList, UpdatedState},
      io:fwrite("Came in This Again Second Time~n"),
      gossip(ServerPID, UpdatedState, Receiver, Message, ProcessList)
  end.

findAliveNode(_, _, Node, 1) -> io:fwrite("Alive Node : ~p~n", [Node]), Node;
findAliveNode(StateMap, NeighbourList, Node, Found) ->
%%  io:fwrite("HAHAHA"),
  Receiver = lists:nth(rand:uniform(length((NeighbourList))), NeighbourList),
  [VisitCountOfReceiver|_] = maps:get(Receiver, StateMap),
  if
    (VisitCountOfReceiver == 10) ->
      findAliveNode(StateMap, NeighbourList, Node, Found);
    true ->
      findAliveNode(StateMap, NeighbourList, Receiver, 1)
  end.