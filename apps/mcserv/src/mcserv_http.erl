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
    #{get := Get} = cowboy_req:match_qs([{get, [], undefined}], Req0),
    #{name := FileName, size := FileSize, pos := CurPos,
      fd := FD} = mcserv_generator:get_file_info(),
    Peer = cowboy_req:peer(Req0),
    Req = case Get == undefined orelse (catch binary_to_integer(Get)) of
              true ->
                  ?LOG(info, "HTTP info request from peer=~p", [Peer]),
                  Body = io_lib:format("name=~s~nsize=~b~npos=~b~n"
                                       "mcast_address=~p~nmcast_port=~b~n",
                                       [FileName, FileSize, CurPos,
                                        ?CFG(mcast_address, ""), ?CFG(mcast_port, 0)]),
                  cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, Body, Req0);
              GetPos when GetPos >= 0 andalso GetPos =< FileSize ->
                  ?LOG(info, "HTTP chunk pos=~b request from peer=~p", [GetPos, Peer]),
                  ChunkSize = ?CFG(packet_size, undefined) - ?HEADER_SIZE,
                  {ok, Data} = file:pread(FD, GetPos, ChunkSize),
                  cowboy_req:reply(200, #{<<"content-type">> => <<"application/octet-stream">>},
                                   Data, Req0);
              _ ->
                  ?LOG(info, "HTTP invalid chunk pos=~s request from peer=~p", [Get, Peer]),
                  cowboy_req:reply(404, Req0)
          end,
    {ok, Req, Opts}.

%%====================================================================
%% Internal functions
%%====================================================================
