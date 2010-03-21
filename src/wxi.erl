-module(wxi).

-include("wxi.hrl").

-export([topFrame/4, addSelf/2, addSelf/3, textLabel/2, panel/2, comp/2, passEvent/2,
         map/1, mapState/2, button/2, modSizerFlags/2, panel/1, linkEvent/3]).


comp(Sub, C = #context {}) -> if
    is_tuple(Sub) andalso size(Sub) == 1 -> (element(1, Sub))(C);
    is_tuple(Sub) -> sercomp(lists:reverse(tuple_to_list(Sub)), C, []);
    is_list(Sub) -> parcomp(Sub, C, []);
    true -> Sub(C)
end.

sercomp([H], X = #context {parent = P}, Pns) ->
    Z = comp(H, X),
    Szf = [{flag, ?wxALL bor ?wxALIGN_CENTER_VERTICAL}, {border, 0}, {proportion, 1}],
    [addSelf(P, Pn, Szf) || Pn <- Pns],
    Z;

sercomp([H|T], X = #context {parent = P}, Pns) ->
    Pn = wxPanel:new(P),
    Sz = wxBoxSizer:new(?wxHORIZONTAL),
    wxWindow:setSizer(Pn, Sz),
    Z = comp(H, X#context {parent = Pn}),
    Ch = wxWindow:getChildren(Pn),
    Pnss = if
      length(Ch) == 0 -> Pns;
      true -> [Pn|Pns]
    end,
    sercomp(T, X#context {evtlink = Z}, Pnss).

passEvent(R, D) -> if
    is_pid(D) -> D ! R, ok;
    is_function(D, 2) -> D(R, null), ok;
    true -> ok
end.

parcomp([], _, Els) -> fun (R, _) -> 
    F = fun (D) -> passEvent(R, D) end,
    lists:foreach(F, Els)
end;

parcomp([H|T], X, Els) -> 
    Z = comp(H, X),
    parcomp(T, X, [Z|Els]).


modSizerFlags(F, Sub) -> fun (C = #context {szflags = T}) ->
    Tm = lists:keymerge(1, F, T),
    comp(Sub, C#context {szflags = Tm})
end.


hasSizer(F) ->
    S = wxWindow:getSizer(F),
    case S of
        {wx_ref, N, wxSizer, _} -> N /= 0;
        _ -> false
    end.

 

linkEvent(_, _, []) -> ok;

linkEvent(Src, Dst, Evts) -> if
    Dst == self() -> [wxEvtHandler:connect(Src, E) || E <- Evts];
    is_function(Dst, 2) -> [wxEvtHandler:connect(Src, E, [{callback, Dst}]) || E <- Evts];
    true -> ok
end.

addSelf(P, W) -> addSelf(P, W, []).

addSelf(P, W, F) ->
    S = hasSizer(P),
    if S -> 
        wxSizer:add(wxWindow:getSizer(P), W, F), 
        ok;
       true -> ok
    end,
    wxWindow:fit(P),
    ok.

topFrame(Title, X, Y, Dir) -> fun (Sub) ->
    Wx = wx:new(),
    {Frame, Udata} = wx:batch(fun () ->
        Fr = wxFrame:new(Wx, ?wxID_ANY, Title, [{size, {X, Y}}]),
        Sz = wxBoxSizer:new(Dir),
        wxWindow:setSizer(Fr, Sz),
        Ud = comp(Sub, #context {parent=Fr, 
                           szflags=[{proportion, 1}, {flag, 0}, {border, 0}]}),
        wxFrame:connect(Fr, close_window),
        {Fr, Ud}
        end),
    wxWindow:show(Frame),
    loop(Frame, Udata),
    wx:destroy(),
    ok
end.

panel(Dir, Sub) -> fun(C = #context{parent = X, szflags = F}) ->
    P = wxPanel:new(X),
    Sz = wxBoxSizer:new(Dir),
    wxWindow:setSizer(P, Sz),
    Ud = comp(Sub, C#context{parent = P}),
    addSelf(X, P, F),
    Ud
end.

panel(Sub) -> fun(C = #context{parent = X, szflags = F}) ->
    P = wxPanel:new(X),
    Ud = comp(Sub, C#context{parent = P}),
    addSelf(X, P, F),
    Ud
end.

button(T, I) -> fun (#context {parent = X, szflags = F, evtlink = E}) ->
    B = wxButton:new(X, I, [{label, T}]),
    addSelf(X, B, F),
    linkEvent(B, E, [command_button_clicked]),
    ok
end.

textLabel(Fmt, T) -> fun (#context {parent = X, szflags = F}) ->
    Tx = wxStaticText:new(X, -1, T),
    addSelf(X, Tx, F),
    fun(R, _) ->
        Z = io_lib:format(Fmt, [R]),
        wxStaticText:setLabel(Tx, Z),
        wxWindow:fit(X),
        ok
    end
end.

map(F) -> fun (#context {evtlink = E}) ->
    fun (R, _) ->
        Z = F(R),
        passEvent(Z, E)
    end
end.

mapState(F, S) -> fun (#context {evtlink = E}) ->
    W = wx:get_env(),
    spawn_link(fun() -> wx:set_env(W), maploop(F, S, E) end)
end.

maploop(F, S0, E) ->
    receive
        Msg ->
            S1 = F(Msg, S0),
            passEvent(S1, E),
            maploop(F, S1, E)
end.

loop(Frame, Udata) ->
    receive
        #wx{event=#wxClose{}} ->
            io:format("~p Closing window ~n",[self()]),
            wxWindow:destroy(Frame),
            ok;
        Msg ->
            io:format("Got ~p ~n", [Msg]),
            loop(Frame, Udata)
    end.



