%%% @doc HTTP interface module
%%% Config:
%%%   http_port     - positive integer port to listen
%%%   file_path     - path to file, string (list)
%%%   packet_size   - transport packet size
%%%   rate          - stream rate, KB/s
%%%   mcast_address - mutlicast address, ip tuple
%%%   mcast_port    - mutlicast port, integer
%%%
%%% Routes:
%%%   http://host:port/ metadata request
%%%   http://host:port/?pos=xxx data at given position request

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
    #{pos := Pos0} = cowboy_req:match_qs([{pos, [], undefined}], Req0),
    FileInfo = mcserv_generator:get_file_info(),
    Req = case Pos0 == undefined orelse (catch binary_to_integer(Pos0)) of
              true ->
                  ?LOG(info, "HTTP metadata request from ~s", [Peer]),
                  handle_metadata_request(Req0, FileInfo);
              Pos when Pos >= 0 andalso Pos < FileInfo#file_info.size ->
                  ?LOG(info, "HTTP data pos=~b request from ~s", [Pos, Peer]),
                  handle_data_request(Req0, FileInfo, Pos);
              _ ->
                  ?LOG(info, "HTTP invalid data request pos=~s from ~s", [Pos0, Peer]),
                  cowboy_req:reply(404, Req0)
          end,
    {ok, Req, Opts}.

%%====================================================================
%% Internal functions
%%====================================================================
%% @doc Handle metadata request
handle_metadata_request(Req, FileInfo) ->
    Body = io_lib:format("name=~s~n"
                         "size=~b~n"
                         "md5=~s~n"
                         "pos=~b~n"
                         "packet_size=~b~n"
                         "rate=~b~n"
                         "mcast_address=~s~n"
                         "mcast_port=~b~n",
                         [FileInfo#file_info.name,
                          FileInfo#file_info.size,
                          FileInfo#file_info.md5,
                          FileInfo#file_info.pos,
                          ?CFG(packet_size, 0),
                          ?CFG(rate, 0),
                          inet:ntoa(?CFG(mcast_address, undefined)),
                          ?CFG(mcast_port, undefined)]),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, Body, Req).

%% @doc Handle data request from the given position
handle_data_request(Req, FileInfo, Pos) ->
    DataSize = ?CFG(packet_size, undefined) - ?HEADER_SIZE,
    {ok, Data} = file:pread(FileInfo#file_info.fd, Pos, DataSize),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/octet-stream">>},
                     Data, Req).
