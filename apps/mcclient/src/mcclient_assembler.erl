%%% @doc This module assembles and saves received file
%%% Config:
%%%   path - path to save a file
%%% @end
-module(mcclient_assembler).
-behavior(gen_server).

-include("mcclient.hrl").

%% API
-export([start_link/1, process_packet/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TAB_DATA, mclient_assembler_data).
-define(TAB_INFO, mclient_assembler_info).

-record(state, {meta     :: #metadata{},
                filename :: binary(),
                fd       :: file:io_device(),
                key      :: non_neg_integer(),
                written  :: non_neg_integer()
               }).

%%====================================================================
%% API functions
%%====================================================================
%% @doc Start generator process
-spec start_link(#metadata{}) -> {ok, pid()} | ignore | {error, term()}.
start_link(Meta) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Meta], []).

%% @doc Process incoming packet
%% @TODO integrity checking
-spec process_packet(binary()) -> true | {error, term()}.
process_packet(Packet) ->
    case Packet of
        <<Pos:?HEADER_SIZE/little-unsigned-integer-unit:8, Data/binary>> ->
            case ets:lookup(?TAB_INFO, Pos) of
                [{Pos, todo}] -> ets:insert(?TAB_DATA, {Pos, Data});
                _ -> true
            end;
        _ -> {error, invalid_packet}
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Meta]) ->
    Filename = list_to_binary([?CFG(path, "."), "/", Meta#metadata.name]),
    case open_file(Filename) of
        {error, Err} ->
            ?LOG("Can't create '~s': ~p~n", [Filename, Err]),
            init:stop(); % hard but escript compatible :)
        {ok, FD} ->
            ets:new(?TAB_DATA, [ordered_set, public, named_table]),
            ets:new(?TAB_INFO, [ordered_set, named_table]),
            init_info_table(Meta),
            ?LOG("Assembler started: filename='~s'~n", [Filename]),
            self() ! tick,
            {ok, #state{meta=Meta,
                        filename=Filename,
                        fd=FD,
                        key=Meta#metadata.position,
                        written=0}}
    end.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) when State#state.key == '$end_of_table' ->
    #state{meta=Meta,
          filename=Filename} = State,
    if State#state.written < Meta#metadata.size -> handle_info(tick, State#state{key=0});
       true ->
            file:close(State#state.fd),
            case calc_md5(Filename) == Meta#metadata.md5 of
                false ->
                    {ok, FD} = open_file(Filename),
                    ?LOG("Invalid file checksum, retrying!~n"),
                    init_info_table(Meta),
                    handle_info(tick, State#state{fd=FD,
                                                  key=0,
                                                  written=0});
                true ->
                    ?LOG("File received successfully!~n"),
                    init:stop(),
                    {noreply, State}
            end
    end;
handle_info(tick, State) ->
    #state{meta=Meta,
           fd=FD,
           key=Key,
           written=Written} = State,
    Written1 = case ets:lookup(?TAB_DATA, Key) of
                   [{Pos, Data}] ->
                       case file:pwrite(FD, Pos, Data) of
                           {error, Err} ->
                               ?LOG("Can't write to file: ~p~n", [Err]),
                               Written;
                           ok ->
                               ets:delete(?TAB_DATA, Pos),
                               ets:delete(?TAB_INFO, Pos),
                               Sum = Written + size(Data),
                               ?LOG("~b/~b written~n", [Sum, Meta#metadata.size]),
                               Sum
                       end;
                   _ -> Written
               end,
    Key1 = ets:next(?TAB_DATA, Key),
    self() ! tick, % @TODO check if need delay
    {noreply, State#state{key=Key1, written=Written1}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
%% @doc Open file for writing
open_file(Filename) ->
    file:open(Filename, [write, binary, {delayed_write, ?BUFFER_SIZE, ?DELAY}]).

%% @doc (Re)initialize assembler info table
init_info_table(Meta) ->
    DataSize = Meta#metadata.data_size,
    [ets:insert(?TAB_INFO, {Key * DataSize, todo}) ||
        Key <- lists:seq(0, trunc(Meta#metadata.size / DataSize))].

%% @doc Calculate md5 hex checksum for file
calc_md5(Path) ->
    {ok, FD} = file:open(Path, [read]),
    calc_md5(FD, erlang:md5_init()).
calc_md5(FD, Context) ->
    case file:pread(FD, cur, ?BUFFER_SIZE) of
        {ok, Data} -> calc_md5(FD, erlang:md5_update(Context, Data));
        eof ->
            file:close(FD),
            list_to_binary([io_lib:format("~2.16.0b", [B]) || <<B>> <= erlang:md5_final(Context)])
    end.
