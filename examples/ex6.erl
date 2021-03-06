%% The wxi library example 6: filtering messages.

-module(ex6).

-include_lib("wxi.hrl").

-define(ID_PLUS, 100).
-define(ID_MINUS, 200).

-export([start/0]).

incd(?ID_PLUS, State) -> State + 1;
incd(?ID_MINUS, State) -> State - 1;
incd(_, State) -> State.

p(E) -> if
    E >= 0 -> {just, E};
    true -> nothing
end.

start() ->
    wxi:topFrame("WXI Example 6", 300, 100, ?wxHORIZONTAL, 
        wxi:modSizerFlags ([{border, 5}, {flag, ?wxALL bor ?wxALIGN_CENTER_VERTICAL}], 
            {wxi:panel(?wxVERTICAL, [wxi:button("+", ?ID_PLUS), wxi:button("-", ?ID_MINUS)]), 
             wxi:map(fun (#wx {id = I}) -> I end),
             wxi:mapState(fun (A, B) -> incd(A, B) end, 0),
             wxi:panel(?wxVERTICAL, [{wxi:always(), wxi:textLabel("~B", "0")}, 
                                     {wxi:maybe(fun(E) -> p(E) end), wxi:textLabel("~B", "0")}])})).

