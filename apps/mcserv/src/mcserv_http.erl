%%% @doc HTTP interface module
%%% Config:
%%%   http_port - positive integer port to listen
%%%   file_path   - path to file, string (list)
%%%   mcast_address - mutlicast address, ip tuple
%%%   mcast_port    - mutlicast port, integer
%%% @end
-module(mcserv_http).
-behavior(cowboy_handler).

-include("mcserv.hrl").

%% API
-export([start/0]).

%% cowboy_handler callbacks
-export([init/2]).

-define(PATH, "/").
-define(ACCEPTORS, 5).

%%====================================================================
%% API functions
%%====================================================================
%% @doc Start HTTP interface
-spec start() -> ok | no_return().
start() ->
    Port = ?CFG(http_port, undefined),
    Dispatch = cowboy_router:compile([{'_', % host match
                                       [{?PATH, ?MODULE, []} ]}
                                     ]),
    {ok, _} = cowboy:start_clear(http, ?ACCEPTORS, [{port, Port}],
                                 #{env => #{dispatch => Dispatch} }),
    ?LOG(info, "HTTP interface started: port=~b", [Port]).

%%====================================================================
%% cowboy_handler callbacks
%%====================================================================
init(Req, Opts) ->
    ?LOG(info, "HTTP request: peer=~p", [cowboy_req:peer(Req)]),
    Body = io_lib:format("file=~s~n"
                         "mcast_address=~p~n"
                         "mcast_port=~b~n", [?CFG(file_path, ""),
                                             ?CFG(mcast_address, ""),
                                             ?CFG(mcast_port, 0)]),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>},
                            Body, Req),
    {ok, Req1, Opts}.

%%====================================================================
%% Internal functions
%%====================================================================
