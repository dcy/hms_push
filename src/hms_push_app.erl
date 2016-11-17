%%%-------------------------------------------------------------------
%% @doc hms_push public API
%% @end
%%%-------------------------------------------------------------------

-module(hms_push_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    hms_push_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
