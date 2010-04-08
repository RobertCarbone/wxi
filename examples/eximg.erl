%% The wxi library example of image manipulation.

-module(eximg).

-include_lib("wxi.hrl").

-export([start/0]).

start() ->
    wxi:topFrame("WXI Image Example", 500, 500, ?wxHORIZONTAL,
            imgFile("examples/ex6.png")).

%% The bitmap is prepared during the creation phase, but actual drawing
%% happens upon the paint event, so additional funciton has to be provided.

imgFile(File) -> wxi:modSizerFlags([{flag, ?wxEXPAND}], wxi:panel(fun (C = #context{}) ->
    wxWindow:setWindowStyle(C#context.parent, ?wxFULL_REPAINT_ON_RESIZE),
    Image = wxImage:new(File),
    Bmp = wxBitmap:new(Image),
    wxImage:destroy(Image),
    Draw = fun (_, _) ->
        DC = wxPaintDC:new(C#context.parent),
        wxDC:drawBitmap(DC, Bmp, {0, 0}),
        wxPaintDC:destroy(DC),
        wxWindow:fit(C#context.parent),
        ok
    end,
    wxEvtHandler:connect(C#context.parent, paint, [{callback, Draw}]),
    fun (_, _) -> ok end
end)).

