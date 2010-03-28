-module(wxicalc).

-include_lib("wxi.hrl").

-export([start/0]).

-define(BTN_PLUS, 100).
-define(BTN_MINUS, 101).
-define(BTN_MULT, 102).
-define(BTN_DIV, 103).
-define(BTN_NEG, 104).
-define(BTN_EQU, 105).
-define(BTN_CLR, 106).
-define(BTN_ERA, 107).
-define(BTN_DOT, 108).
-define(BTN_INV, 109).

-record(calcst, {accum, disp, act, clr, dot}).

initstate() -> #calcst{accum = 0, disp = "0", act = null, clr = true, dot = false}.

start() -> 
    wxi:topFrame("Calculator", 400, 400, ?wxHORIZONTAL,
            wxi:panel(-?wxVERTICAL, {keyboard(), logic(), display()})).

logic() -> {wxi:map(fun (#wx {id = I}) -> I end), 
            wxi:mapState(fun (K, S) -> calc(K, S) end, initstate()),
            wxi:map(fun (#calcst {disp = S}) -> S end)}.

keyboard() -> wxi:modSizerFlags([{proportion, 5},
                                 {border, 5},
                                 {flag, ?wxALL bor ?wxALIGN_LEFT bor ?wxALIGN_CENTER_VERTICAL}], 
    wxi:grid(4, [wxi:button(L, I) || {I, L} <- [
        {?BTN_NEG, "+/-"}, {?BTN_CLR, "C"}, {?BTN_ERA, "CE"}, {?BTN_INV, "1/x"},
        {7, "7"}, {8, "8"}, {9, "9"}, {?BTN_MULT, "*"},
        {4, "4"}, {5, "5"}, {6, "6"}, {?BTN_DIV,  "/"},
        {1, "1"}, {2, "2"}, {3, "3"}, {?BTN_PLUS,  "+"},
        {0, "0"}, {?BTN_DOT, "."}, {?BTN_EQU, "="}, {?BTN_MINUS, "-"}]])).

display() -> wxi:modSizerFlags([{proportion, 1},
                                {border, 5},
                                {flag, ?wxALL bor ?wxALIGN_RIGHT bor ?wxALIGN_CENTER_VERTICAL}], 
    wxi:panel(?wxHORIZONTAL, {wxi:textLabel("~s", "0")})).

calc(K, S = #calcst{}) -> if
    K == ?BTN_CLR ->
        initstate();
    K >= 0 andalso K =< 9 ->
        D0 = if
            S#calcst.clr -> "";
            true -> S#calcst.disp
        end,
        D1 = D0 ++ [($0 + K)],
        S#calcst {disp = D1, clr = false};
    K == ?BTN_DOT -> if
            S#calcst.dot -> S;
            S#calcst.clr -> S#calcst {disp = "0.", dot = true, clr = false};
            true -> S#calcst {disp = S#calcst.disp ++ ".", dot = true}
        end;
    true -> S
end.

