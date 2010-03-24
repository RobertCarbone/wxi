%% The wxi library example 2: empty toplevel frame.

-module(ex2).

-include_lib("wxi.hrl").

-define(ID_PLUS, 100).

-export([start/0]).

start() ->
    wxi:topFrame("WXI Example 2", 300, 100, ?wxHORIZONTAL, 
        {wxi:button("+", ?ID_PLUS), wxi:textLabel("~p", "0")}).

