%% An example of an infix calculator written with the wxi library.
%% This example illustrates use of various widgets provided with the library
%% as well as methods to connect widgets and route messages.

-module(wxicalc).

-include_lib("wxi.hrl").

-export([start/0]).

%% Define numeric identifiers for calculator buttons - operations. 
%% Numeric buttons get identifiers 0 to 9.

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

%% Define a record for calculator state.

-record(calcst, {accum :: float(),             % accumulator to store the result
                 disp :: string(),             % calculator numeric display
                 act :: integer(),             % action (arithmetic operation) remembered
                 clr :: boolean(),             % clear the display upon user input
                 dot :: boolean()}).           % display contains decimal dot

%% Initial state of the calculator.

initstate() -> #calcst{accum = null, disp = "0", act = null, clr = true, dot = false}.

%% Start the calculator. This function defines the "toplevel wiring" among the components
%% of the calculator.

start() -> 

%% Define the toplevel window:

    wxi:topFrame("Calculator", 400, 400, ?wxHORIZONTAL,

%% Make the window send key events and map key events to simulated button events:

            {wxi:catchEvents([key_up]), wxi:maybe(fun (E) -> key2btn(E) end), 

%% Place the button pad and the display (note the reverse direction: 
%% the display is above the button pad. Use wxi:always() to "short-circuit" the
%% button pad widget so mapped key events are multiplexed with button events.
%% The calculator logics unit receives events from both key presses and buttons.
%% Output of the calculator logics unit is fed into the display.

             wxi:panel(-?wxVERTICAL, {[wxi:always(), btnpad()], logic(), display()})}).

%% Map events originating from key presses are mapped to button click events
%% by creating a fake #wx record with proper id. Not all possible keys are mapped
%% in this example, but the idea must be clear. Note that all returned values
%% are tuples tagged with 'just': the result will be processed by the wxi:maybe
%% widget.

key2btn(#wx {event = #wxKey {uniChar = C}}) -> case C of
    $0 -> {just, #wx {id = 0}};               % 0 - 9 of the upper keys row
    $1 -> {just, #wx {id = 1}};
    $2 -> {just, #wx {id = 2}};
    $3 -> {just, #wx {id = 3}};
    $4 -> {just, #wx {id = 4}};
    $5 -> {just, #wx {id = 5}};
    $6 -> {just, #wx {id = 6}};
    $7 -> {just, #wx {id = 7}};
    $8 -> {just, #wx {id = 8}};
    $9 -> {just, #wx {id = 9}};
    387 -> {just, #wx {id = ?BTN_MULT}};      % * on the numpad
    392 -> {just, #wx {id = ?BTN_DIV}};       % / on the numpad
    390 -> {just, #wx {id = ?BTN_MINUS}};     % etc.
    388 -> {just, #wx {id = ?BTN_PLUS}};
    46 -> {just, #wx {id = ?BTN_DOT}};
    61 -> {just, #wx {id = ?BTN_EQU}};
    27 -> {just, #wx {id = ?BTN_CLR}};
    _ -> nothing
end;
key2btn(_) -> nothing.

%% The calculator logics unit: extract id from an event, process the event keeping
%% internal state between events, remove trailing zeros from the part right
%% of the decimal dot, if present. The resulting value is ready for display.

logic() -> {wxi:map(fun (#wx {id = I}) -> I end), 
            wxi:mapState(fun (K, S) -> calc(K, S) end, initstate()),
            wxi:map(fun (#calcst {disp = S, clr = C}) -> if C -> rmtz(S); true -> S end end)}.

%% The button pad widget. Note the 'proportion' sizer flag: buttons take 5 rows while
%% the display takes one (see below).

btnpad() -> wxi:modSizerFlags([{proportion, 5},
                                 {border, 5},
                                 {flag, ?wxALL bor ?wxALIGN_LEFT bor ?wxALIGN_CENTER_VERTICAL}], 

%% Buttons are created from a list comprehension and parallel-composed, thus
%% all events from button clicks will be multiplexed. Grid sizer is used.

    wxi:grid(4, [wxi:button(L, I) || {I, L} <- [
        {?BTN_NEG, "+/-"}, {?BTN_CLR, "C"}, {?BTN_ERA, "CE"}, {?BTN_INV, "1/x"},
        {7, "7"}, {8, "8"}, {9, "9"}, {?BTN_MULT, "*"},
        {4, "4"}, {5, "5"}, {6, "6"}, {?BTN_DIV,  "/"},
        {1, "1"}, {2, "2"}, {3, "3"}, {?BTN_PLUS,  "+"},
        {0, "0"}, {?BTN_DOT, "."}, {?BTN_EQU, "="}, {?BTN_MINUS, "-"}]])).

%% The display widget. Just a static text field that can be updated by incoming events.

display() -> wxi:modSizerFlags([{proportion, 1},
                                {border, 5},
                                {flag, ?wxALL bor ?wxALIGN_RIGHT bor ?wxALIGN_CENTER_VERTICAL}], 
    wxi:panel(?wxHORIZONTAL, {wxi:textLabel("~s", "0")})).

%% The function from the current state and incoming event to the new state.
%% The wxi:mapState widget takes care of remembering the state (as an infinitely
%% recursive process holding state in its local variables).

calc(K, S = #calcst{}) -> if
    S#calcst.disp == "Error" -> initstate();
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
         Disp = readdisp(S),
         if
            Disp == 0 -> (initstate())#calcst {disp = "Error"};
            true -> S#calcst {clr = true, disp = fmtdisp(1.0/Disp)}
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

%% Read the numeric display (add decimal dot and trailing zero if needed for the ~f format).

readdisp(S = #calcst{}) ->
    {ok, [Disp], _} = io_lib:fread("~f", S#calcst.disp ++ case S#calcst.dot of
               true -> "0";
               false -> ".0"
           end),
    Disp.

%% Format a value for the display.

fmtdisp(Res) -> lists:flatten(io_lib:format("~f", [Res])).

%% Process an arithmetic operation.

calc_op(S = #calcst{}) -> 
    Disp = readdisp(S),
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
                                   disp = fmtdisp(Res)}
             end
    end.

%% Remove trailing zeros.

rmtz(S) -> 
    Sz = re:replace(S, "\\.00*$", ""),             % for 123.000
    D = lists:member($., Sz),                      % otherwise, if a decimal dot
    if                                             % is still there e. g. 123.400,
        D -> re:replace(Sz, "0*$", "");            % remove trailing zeros.
        true -> Sz
end.

