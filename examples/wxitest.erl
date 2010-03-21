-module(wxitest).

-include("wxi.hrl").

-export([start/0]).

-define(ID_PLUS, 100).
-define(ID_MINUS, 200).

incd(?ID_PLUS, State) -> State + 1;
incd(?ID_MINUS, State) -> State - 1;
incd(_, State) -> State.
  

start() ->
    (wxi:topFrame("WXI Test", 200, 100, ?wxHORIZONTAL)) (
        wxi:modSizerFlags ([{border, 5}, {flag, ?wxALL bor ?wxALIGN_CENTER_VERTICAL}], ({
            wxi:panel(?wxVERTICAL, [wxi:button("+", ?ID_PLUS), wxi:button("-", ?ID_MINUS)]),
            wxi:map(fun (#wx {id = I}) -> I end),
            wxi:mapState(fun (A, B) -> incd(A, B) end, 0),
            wxi:textLabel("~B", "0")}
        ))
    ).



