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
    {neighbours, MasterPID, StateMap, Algorithm} ->
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
  [_,NeighbourList|_] = maps:get(GossipStarter, StateMap),
  UpdatedState = maps:update(GossipStarter, [1, NeighbourList], StateMap),
  gossip(UpdatedState, GossipStarter, Message),
  ServerPID = spawn(fun() -> server:status() end).

gossip(StateMap, Sender, Message) ->
  NeighbourList = maps:get(Sender, StateMap),
  io:fwrite("Process - ~p, Neighbours - ~p~n", [Sender, NeighbourList]).
%%  Receiver = lists:nth(rand:uniform(length((NeighbourList))), NeighbourList),
