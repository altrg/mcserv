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

-define(WORKER(Mod, Args), {Mod, {Mod, start_link, Args}, permanent, 5000, worker, [Mod]}).

%%====================================================================
%% API functions
%%====================o===============================================
start_link(Meta) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Meta]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Meta]) ->
    {ok, {{one_for_one, 5, 10}, [
                                 ?WORKER(mcclient_assembler, [Meta]),
                                 ?WORKER(mcclient_receiver, [Meta]),
                                 ?WORKER(mcclient_corrector, [Meta])
                                ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
