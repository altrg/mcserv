%%%-------------------------------------------------------------------
%% @doc mcclient top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(mcclient_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================o===============================================
start_link(Meta) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Meta]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Meta]) ->
    {ok, {{one_for_one, 5, 10}, [
                                 {mcclient_assembler, {mcclient_assembler, start_link, [Meta]},
                                  permanent, 5000, worker, [mcclient_assembler]},
                                 {mcclient_receiver, {mcclient_receiver, start_link, [Meta]},
                                  permanent, 5000, worker, [mcclient_receiver]}
                                ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
