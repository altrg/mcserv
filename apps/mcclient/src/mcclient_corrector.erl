%%% @doc This module finds and retrieves lost packets
%%% Config:
%%%   timeout - timeout to detect lost packets, ms, default 5000
%%% @end
-module(mcclient_corrector).
-behavior(gen_server).

-include("mcclient.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {meta    :: #metadata{},
                url     :: string(),
                timeout :: pos_integer(), % ms
                pos     :: non_neg_integer(),
                ref_ts  :: integer() % reference timestammp to check timeout
               }).

%%====================================================================
%% API functions
%%====================================================================
%% @doc Start generator process
-spec start_link(#metadata{}) -> {ok, pid()} | ignore | {error, term()}.
start_link(Meta) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Meta], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Meta]) ->
    Url = ?CFG(url, undefined),
    Timeout = ?CFG(timeout, 5000),
    ?LOG("Corrector started: timeout=~bms~n", [Timeout]),
    self() ! wait_first_packet,
    {ok, #state{meta=Meta,
                url=Url,
                timeout=Timeout}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(wait_first_packet, State) ->
    %% wait for the first packet to start corrector
    case ets:next(?TAB_RECEIVED, 0) of
        '$end_of_table' ->
            self() ! wait_first_packet, % @TODO check if need delay
            {noreply, State};
        Pos ->
            RefTS = erlang:monotonic_time(millisecond),
            {noreply, State#state{pos=Pos,
                                  ref_ts=RefTS}}
    end;

handle_info(tick, State) ->
    #state{meta=Meta,
           timeout=Timeout,
           pos=Pos,
           ref_ts=RefTS} = State,
    CurrentTS = erlang:monotonic_time(millisecond),
    State1 = case ets:lookup(?TAB_PENDING, Pos) of
                 [] ->  % pending packet was resolved, advance to the next
                     Pos1 = get_next_pos(Pos),
                     RefTS1 = CurrentTS + duration(Pos, Pos1, Meta),
                     State#state{pos=Pos1, ref_ts=RefTS1};
                 _ when CurrentTS < RefTS + Timeout -> % no timeout, wait
                     State;
                 [{Pos}] -> % timeout occured
                     ?LOG("Timeout pos=~b, downloading via http~n", [Pos]),
                   case download_data(State#state.url, Pos) of
                       {error, Err} ->
                           ?LOG("Error downloading pos=~b: ~p~n", [Pos, Err]),
                           State; % retry at the next tick, @TODO delay
                       {ok, Data} ->
                           mcclient_assembler:process_data(Pos, Data),
                           Pos1 = get_next_pos(Pos),
                           RefTS1 = CurrentTS + duration(Pos, Pos1, Meta),
                           State#state{pos=Pos1, ref_ts=RefTS1}
                   end
             end,
    self() ! tick, % @TODO check if need delay
    {noreply, State1}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
%% @doc Get next pending packet position
get_next_pos(Pos) ->
    case ets:next(?TAB_PENDING, Pos) of
        '$end_of_table' -> 0;
        Pos1 -> Pos1
    end.

%% @doc Download data from the position
download_data(Url, Pos) ->
    Url1 = Url ++ "?pos=" ++ integer_to_list(Pos),
    case httpc:request(get, {Url1, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Data}} -> {ok, Data};
        {_, Err} -> {error, Err}
    end.

%% @doc Estimate how long it was to reach from pos1 to pos2
duration(Pos1, Pos2, Meta) ->
    Bytes = bytes_sent(Pos1, Pos2, Meta#metadata.size),
    round(Bytes / Meta#metadata.rate). % Rate = KB/s = B/ms

%% @doc Calculate how many bytes were sent between the two positions
bytes_sent(Pos1, Pos2, _Size) when Pos2 >= Pos1 ->
    Pos2 - Pos1;
bytes_sent(Pos1, Pos2, Size) ->
    Size - Pos2 + Pos1.
