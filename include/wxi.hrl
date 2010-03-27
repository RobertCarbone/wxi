-include_lib("wx/include/wx.hrl").

%% @doc The Context record.

-record(context, {parent  :: {'wx_ref',integer(),_,_}, 
                  evtlink :: pid() | fun((_,_) -> 'ok') , 
                  szflags :: integer()}).

