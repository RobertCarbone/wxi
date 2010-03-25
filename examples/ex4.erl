%% The wxi library example 4: stateful counter.

-module(ex4).

-include_lib("wxi.hrl").

-define(ID_PLUS, 100).
-define(ID_MINUS, 200).

-export([start/0]).

incd(?ID_PLUS, State) -> State + 1;
incd(?ID_MINUS, State) -> State - 1;
incd(_, State) -> State.

start() ->
    wxi:topFrame("WXI Example 4", 300, 100, ?wxHORIZONTAL, 
        {[wxi:button("+", ?ID_PLUS), wxi:button("-", ?ID_MINUS)], 
         wxi:map(fun (#wx {id = I}) -> I end),
         wxi:mapState(fun (A, B) -> incd(A, B) end, 0),
         wxi:textLabel("~B", "0")}).

