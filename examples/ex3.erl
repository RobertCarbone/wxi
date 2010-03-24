%% The wxi library example 3: two buttons and message mapper.

-module(ex3).

-include_lib("wxi.hrl").

-define(ID_PLUS, 100).
-define(ID_MINUS, 200).

-export([start/0]).

start() ->
    wxi:topFrame("WXI Example 3", 300, 100, ?wxHORIZONTAL, 
        {[wxi:button("+", ?ID_PLUS), wxi:button("-", ?ID_MINUS)], 
         wxi:map(fun (#wx {id = I}) -> I end),
         wxi:textLabel("~p", "0")}).

