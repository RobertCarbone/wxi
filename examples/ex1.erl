%% The wxi library example 1: empty toplevel frame.

-module(ex1).

-include_lib("wxi.hrl").

-export([start/0]).

start() ->
    wxi:topFrame("WXI Example 1", 200, 100, ?wxHORIZONTAL ,[]).

