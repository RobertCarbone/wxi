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

-record(calcst, {accum :: float(), 
                 disp :: string(), 
                 act :: integer(), 
                 clr :: boolean(), 
                 dot :: boolean()}).

initstate() -> #calcst{accum = null, disp = "0", act = null, clr = true, dot = false}.

start() -> 
    wxi:topFrame("Calculator", 400, 400, ?wxHORIZONTAL,
            {wxi:catchEvents([key_up]), wxi:maybe(fun (E) -> key2btn(E) end), 
             wxi:panel(-?wxVERTICAL, {[wxi:always(), keyboard()], logic(), display()})}).

key2btn(#wx {event = #wxKey {uniChar = C}}) -> case C of
    $1 -> {just, #wx {id = 1}};
    $2 -> {just, #wx {id = 2}};
    $3 -> {just, #wx {id = 3}};
    $4 -> {just, #wx {id = 4}};
    $5 -> {just, #wx {id = 5}};
    $6 -> {just, #wx {id = 6}};
    $7 -> {just, #wx {id = 7}};
    $8 -> {just, #wx {id = 8}};
    $9 -> {just, #wx {id = 9}};
    $0 -> {just, #wx {id = 0}};
    15 -> {just, #wx {id = ?BTN_EQU}};
    $+ -> {just, #wx {id = ?BTN_PLUS}};
    _ -> nothing
end;
key2btn(_) -> nothing.

logic() -> {wxi:map(fun (#wx {id = I}) -> I end), 
            wxi:mapState(fun (K, S) -> calc(K, S) end, initstate()),
            wxi:map(fun (#calcst {disp = S, clr = C}) -> if C -> rmtz(S); true -> S end end)}.

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
    K == ?BTN_ERA ->
        S#calcst {disp = "0", clr = true};
    K == ?BTN_NEG ->
        Dpx = case S#calcst.disp of
          [$-|Dp] -> Dp;
          Dp -> [$-|Dp]
        end,
        S#calcst {disp = Dpx, clr = true};
    K == ?BTN_INV -> 
        {ok, [Disp], _} = io_lib:fread("~f", S#calcst.disp ++ case S#calcst.dot of
                  true -> "0";
                  false -> ".0"
              end),
        if
            Disp == 0 -> (initstate())#calcst {disp = "Error"};
            true -> S#calcst {clr = true, disp = lists:flatten(io_lib:format("~f", [1.0/Disp]))}
        end;
    K == ?BTN_EQU ->
        calc_op(S);
    K >= ?BTN_PLUS andalso K =< ?BTN_DIV ->
        SR = calc_op(S),
        SR#calcst {act = K};
    K >= 0 andalso K =< 9 ->
        {D0, Dot} = if
            S#calcst.clr -> {"", false};
            true -> {S#calcst.disp, S#calcst.dot}
        end,
        D1 = D0 ++ [($0 + K)],
        S#calcst {disp = D1, clr = false, dot = Dot};
    K == ?BTN_DOT -> if
            S#calcst.clr -> S#calcst {disp = "0.", dot = true, clr = false};
            S#calcst.dot -> S;
            true -> S#calcst {disp = S#calcst.disp ++ ".", dot = true}
        end;
    true -> S
end.

calc_op(S = #calcst{}) -> 
    {ok, [Disp], _} = io_lib:fread("~f", S#calcst.disp ++ case S#calcst.dot of
               true -> "0";
               false -> ".0"
           end),
    case S#calcst.accum of
        null -> S#calcst {clr = true, accum = Disp};
        _ -> Res = case S#calcst.act of
                 ?BTN_PLUS -> S#calcst.accum + Disp;
                 ?BTN_MINUS -> S#calcst.accum - Disp;
                 ?BTN_MULT -> S#calcst.accum * Disp;
                 ?BTN_DIV -> if
                     Disp == 0 -> error;
                     true -> S#calcst.accum / Disp
                 end;
                 null -> Disp
             end,
             if
                 Res == error -> (initstate())#calcst {disp = "Error"};
                 true -> S#calcst {clr = true, 
                                   accum = Res, 
                                   act = null,
                                   disp = lists:flatten(io_lib:format("~f", [Res]))}
             end
    end.

rmtz(S) -> 
    Sz = re:replace(S, "\\.00*$", ""),
    D = lists:member($., Sz),
    if
        D -> re:replace(Sz, "0*$", "");
        true -> Sz
end.

