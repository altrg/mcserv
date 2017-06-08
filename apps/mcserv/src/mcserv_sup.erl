%%%-------------------------------------------------------------------
%% @doc mcserv top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mcserv_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_one, 5, 10}, [
                                 {mcserv_streamer, {mcserv_streamer, start_link, []},
                                  permanent, 5000, worker, [mcserv_streamer]},
                                 {mcserv_generator, {mcserv_generator, start_link, []},
                                  permanent, 5000, worker, [mcserv_generator]}
                                ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
