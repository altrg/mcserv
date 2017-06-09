%%% @doc This module generates packets from configured file and feeds streamer at given rate
%%% Config:
%%%   rate        - stream rate, KB/s
%%%   packet_size - generated packet size
%%%   file_path   - path to file, string (list)
%%% @end
-module(mcserv_generator).
-behavior(gen_server).

-include("mcserv.hrl").

%% API
-export([start_link/0, get_file_info/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type file_info() :: #{path := list(),
                       name := list(),
                       size := pos_integer(),
                       pos  := non_neg_integer(),
                       fd   := file:io_device()}.
-export_type([file_info/0]).

-record(state, {packet_size :: pos_integer(),
                period :: pos_integer(),
                file :: file_info()
               }).

%%====================================================================
%% API functions
%%====================================================================
%% @doc Start generator process
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Get current file info
-spec get_file_info() -> file_info().
get_file_info() ->
    gen_server:call(?MODULE, get_file_info).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    Rate = ?CFG(rate, undefined),
    PacketSize = ?CFG(packet_size, undefined),
    Path = ?CFG(file_path, undefined),
    Period = round(1000 / (Rate * 1000 / PacketSize)), % ms
    File = open_file(Path),
    ?LOG(info, "Generator started: rate=~bKB/s packet=~b file=~s", [Rate, PacketSize, Path]),
    self() ! tick,
    {ok, #state{packet_size=PacketSize,
                period=Period,
                file=File}}.

handle_call(get_file_info, _From, State) ->
    {reply, State#state.file, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    erlang:send_after(State#state.period, self(), tick), % @TODO improve accuracy
    File = State#state.file,
    #{pos := Pos, fd := FD} = File,
    ChunkSize = State#state.packet_size - ?HEADER_SIZE,
    Pos1 = case file:pread(FD, Pos, ChunkSize) of % @TODO check perfomance
               {ok, Data} -> Pos + size(Data);
               eof ->
                   ?LOG(debug, "EOF, rewinding file"),
                   {ok, Data} = file:pread(FD, 0, ChunkSize),
                   size(Data)
           end,
    Header = <<Pos1:?HEADER_SIZE/little-unsigned-integer-unit:8>>, % encode position to header
    mcserv_streamer:send(<<Header/binary, Data/binary>>),
    File1 = File#{pos := Pos1},
    {noreply, State#state{file=File1}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
%% @doc Open given file for read
open_file(Path) ->
    {ok, FD} = file:open(Path, [read, binary, {read_ahead, ?BUFFER_SIZE}]),
    #{path => Path,
      name => filename:basename(Path),
      size => filelib:file_size(Path),
      pos => 0,
      fd => FD}.
