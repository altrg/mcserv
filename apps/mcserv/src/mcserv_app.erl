%%%-------------------------------------------------------------------
%% @doc mcserv public API
%% @end
%%%-------------------------------------------------------------------

-module(mcserv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    mcserv_http:start(),
    mcserv_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
