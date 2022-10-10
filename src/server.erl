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
-export([status/0]).
remove(X, L) -> [Y || Y <- L, Y =/= X].

status() ->
  receive
    {WorkerPID, FinishedNode, ProcessList, StateMap} ->
      AliveList = remove(FinishedNode, ProcessList),
      if
        (length(AliveList) == 0) ->
          io:fwrite("~nThe Connection has converged and the message has been successfully passed to all Nodes.~n");
        true ->
          GossipStarter = lists:nth(rand:uniform(length((AliveList))), AliveList),
          NeighbourListForSender = lists:nth(2, maps:get(GossipStarter, StateMap)),
          AliveNodes = countAlive(NeighbourListForSender, AliveList, length(NeighbourListForSender)),
          if
            AliveNodes == 0 ->
              self() ! {WorkerPID, GossipStarter, ProcessList, StateMap};
            true ->
              io:fwrite("Alive Node is ~p~n", [GossipStarter]),
              WorkerPID ! {controller, GossipStarter, AliveList}
          end,
          status()
      end
  end.

countAlive(_, _, -1) -> 1;
countAlive(_, _, 0) -> 0;
countAlive(NeighbourList, AliveList, N) ->
  IsMember = lists:member(lists:nth(N,NeighbourList), AliveList),
  if
    IsMember ->
      io:fwrite("This ~p is member ~n", [lists:nth(N,NeighbourList)]),
      countAlive(NeighbourList, AliveList, -1);
    true -> countAlive(NeighbourList, AliveList, N-1)
  end.