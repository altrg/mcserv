%%% @doc This module generates packets from configured file for streamer at given rate
%%% Config:
%%%   rate        - stream rate, KB/s
%%%   packet_size - generated packet size
%%%   file_path   - path to file, string (list)
%%% @end
-module(mcserv_generator).
-behavior(gen_server).

-include("mcserv.hrl").
-include_lib("kernel/include/file.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(BUFFER_SIZE, 512*1024). % %% @TODO find an optimal value

-record(state, {packet_size :: pos_integer(),
                period :: pos_integer(),
                fd :: file:io_device()
               }).

%%====================================================================
%% API functions
%%====================================================================
%% @doc Start generator process
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    Rate = ?CFG(rate, undefined),
    PSize = ?CFG(packet_size, undefined),
    File = ?CFG(file_path, undefined),
    Period = round(1000 / (Rate * 1000 / PSize)), % ms
    {ok, FD} = file:open(File, [read, binary, {read_ahead, ?BUFFER_SIZE}]),
    ?LOG(info, "Generator started: rate=~bKB/s packet=~b file=~s", [Rate, PSize, File]),
    self() ! tick,
    {ok, #state{packet_size=PSize,
                period=Period,
                fd=FD}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    #state{packet_size=PSize,
           period=Period,
           fd=FD} = State,
    erlang:send_after(Period, self(), tick), % @TODO improve accuracy
    case file:pread(FD, cur, PSize) of % @TODO check performance
        {ok, Data} -> ok;
        eof ->
            ?LOG(debug, "EOF, rewinding file"),
            {ok, 0} = file:position(FD, bof),
            {ok, Data} = file:pread(FD, cur, PSize)
    end,
    mcserv_streamer:send(Data),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
