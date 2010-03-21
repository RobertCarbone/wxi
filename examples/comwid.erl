-module(comwid).

-include_lib("wx/include/wx.hrl").

-export([start/0]).

-define(ID_PLUS, 100).
-define(ID_MINUS, 200).
-define(ID_TEXT, 0).

start() ->
    Wx = wx:new(),
    {Frame, SText} = wx:batch(fun() -> create_window(Wx) end),
    wxWindow:show(Frame),
    loop(Frame, SText, 0),
    wx:destroy(),
    ok.


create_window(Wx) ->
    Frame = wxFrame:new(Wx, 
			-999, % window id
			"Widgets Communicate", % window title
			[{size, {290,150}}]),

    Wbs = wxBoxSizer:new(?wxHORIZONTAL),
    wxWindow:setSizer(Frame, Wbs),
    SFlags = [{proportion, 1}, {flag, ?wxALIGN_CENTER_VERTICAL bor ?wxALL}, {border, 5}],
    LeftPanel = wxPanel:new(Frame),
    RightPanel = wxPanel:new(Frame),
    wxPanel:centre(LeftPanel),
    wxPanel:centre(RightPanel),
    BPlus = wxButton:new(LeftPanel, ?ID_PLUS, [{label, "+"}, {pos, {10, 10}}]),
    BMinus = wxButton:new(LeftPanel, ?ID_MINUS, [{label, "-"}, {pos, {10, 60}}]),
    [wxSizer:add(Wbs, P, SFlags) || P <- [LeftPanel, RightPanel]],
    SText = wxStaticText:new(RightPanel, ?ID_TEXT, "0", [{pos, {40, 60}}]),
    [wxButton:connect(Bt, command_button_clicked) || Bt <- [BPlus, BMinus]],

    %% if we don't handle this ourselves, wxwidgets will close the window
    %% when the user clicks the frame's close button, but the event loop still runs

    wxFrame:connect(Frame, close_window),
    {Frame, SText}.


loop(Frame, SText, N) ->
    receive 
   	#wx{event=#wxClose{}} ->
   	    io:format("~p Closing window ~n",[self()]),
   	    ok = wxFrame:setStatusText(Frame, "Closing...",[]),
	    wxWindow:destroy(Frame),
	    ok;
	#wx{id=?ID_PLUS} ->
	    N1 = N + 1,
	    Tx = io_lib:format("~B", [N1]),
	    wxStaticText:setLabel(SText, Tx),
	    loop(Frame, SText, N1);
	#wx{id=?ID_MINUS} ->
	    N1 = N - 1,
	    Tx = io_lib:format("~B", [N1]),
	    wxStaticText:setLabel(SText, Tx),
	    loop(Frame, SText, N1);
	Msg ->
	    io:format("Got ~p ~n", [Msg]),
	    loop(Frame, SText, N)
    end.



