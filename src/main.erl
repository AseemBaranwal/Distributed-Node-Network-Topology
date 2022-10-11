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

%% API
-export([start/0]).

start() ->
  {_, [Nodes]} = io:fread("Enter number of Nodes: ", "~d"),
  {_, [Topology]} = io:fread("Enter the type of topology: ", "~s"),
  {_, [Algorithm]} = io:fread("Enter the type of algorithm: ", "~s"),
  io:fwrite("~n----------------------------------------------------------------------------------~n~n"),

  io:format("Main Process PID: ~p~n", [self()]),
  process:start({Nodes, Topology, Algorithm}).