%%% @doc HTTP interface module
%%% Config:
%%%   http_port - positive integer port to listen
%%%   file_path   - path to file, string (list)
%%%   packet_size - generated packet size
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

%%====================================================================
%% API functions
%%====================================================================
%% @doc Start HTTP interface
-spec start() -> ok | no_return().
start() ->
    Port = ?CFG(http_port, undefined),
    Dispatch = cowboy_router:compile([{'_', % host match
                                       [{?HTTP_PATH, ?MODULE, []} ]}
                                     ]),
    {ok, _} = cowboy:start_clear(http, ?HTTP_ACCEPTORS, [{port, Port}],
                                 #{env => #{dispatch => Dispatch} }),
    ?LOG(info, "HTTP interface started: port=~b", [Port]).

%%====================================================================
%% cowboy_handler callbacks
%%====================================================================
init(Req0, Opts) ->
    {PeerIP, PeerPort} = cowboy_req:peer(Req0),
    Peer = io_lib:format("~s:~b", [inet:ntoa(PeerIP), PeerPort]),
    #{get := Get} = cowboy_req:match_qs([{get, [], undefined}], Req0),
    File = mcserv_generator:get_file_info(),
    Req = case Get == undefined orelse (catch binary_to_integer(Get)) of
              true ->
                  ?LOG(info, "HTTP info request from ~s", [Peer]),
                  Body = io_lib:format("name=~s~n"
                                       "size=~b~n"
                                       "md5=~s~n"
                                       "pos=~b~n"
                                       "data_size=~b~n"
                                       "mcast_address=~s~n"
                                       "mcast_port=~b~n",
                                       [File#file_info.name,
                                        File#file_info.size,
                                        File#file_info.md5,
                                        File#file_info.pos,
                                        ?CFG(packet_size, undefined) - ?HEADER_SIZE,
                                        inet:ntoa(?CFG(mcast_address, undefined)),
                                        ?CFG(mcast_port, undefined)]),
                  cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, Body, Req0);
              GetPos when GetPos >= 0 andalso GetPos =< File#file_info.size ->
                  ?LOG(info, "HTTP data pos=~b request from ~s", [GetPos, Peer]),
                  DataSize = ?CFG(packet_size, undefined) - ?HEADER_SIZE,
                  {ok, Data} = file:pread(File#file_info.pos, GetPos, DataSize),
                  cowboy_req:reply(200, #{<<"content-type">> => <<"application/octet-stream">>},
                                   Data, Req0);
              _ ->
                  ?LOG(info, "HTTP invalid data pos=~s request from ~s", [Get, Peer]),
                  cowboy_req:reply(404, Req0)
          end,
    {ok, Req, Opts}.

%%====================================================================
%% Internal functions
%%====================================================================
