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

status() ->
  receive
    {initialize, ProcessList} ->ok
  end.