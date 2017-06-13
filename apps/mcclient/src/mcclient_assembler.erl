%%% @doc This module assembles and saves received file
%%% Config:
%%%   path - path to save a file
%%% @end
-module(mcclient_assembler).
-behavior(gen_server).

-include("mcclient.hrl").

%% API
-export([start_link/1, process_data/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

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

%% @doc Process incoming data
-spec process_data(non_neg_integer(), binary()) -> true | {error, term()}.
process_data(Pos, Data) ->
    case ets:lookup(?TAB_PENDING, Pos) of
        [{Pos}] ->
            ets:delete(?TAB_PENDING, Pos),
            ets:insert(?TAB_RECEIVED, {Pos, Data});
        _ -> true
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
            ets:new(?TAB_RECEIVED, [ordered_set, public, named_table]),
            ets:new(?TAB_PENDING, [ordered_set, public, named_table]),
            init_pending_table(Meta),
            ?LOG("Assembler started: filename='~s'~n", [Filename]),
            self() ! tick,
            {ok, #state{meta=Meta,
                        filename=Filename,
                        fd=FD,
                        key=0,
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
            case calculate_md5(Filename) == Meta#metadata.md5 of
                false ->
                    {ok, FD} = open_file(Filename),
                    ?LOG("Invalid file checksum, retrying!~n"),
                    init_pending_table(Meta),
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
    Written1 = case ets:lookup(?TAB_RECEIVED, Key) of
                   [{Pos, Data}] ->
                       case file:pwrite(FD, Pos, Data) of
                           {error, Err} ->
                               ?LOG("Can't write to file: ~p~n", [Err]),
                               Written;
                           ok ->
                               ets:delete(?TAB_RECEIVED, Pos),
                               Sum = Written + size(Data),
                               ?LOG("~b/~b written~n", [Sum, Meta#metadata.size]),
                               Sum
                       end;
                   _ -> Written
               end,
    Key1 = ets:next(?TAB_RECEIVED, Key),
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

%% @doc (Re)initialize pending positions table
init_pending_table(Meta) ->
    DataSize = Meta#metadata.packet_size - ?HEADER_SIZE,
    NumOfPackets = round(Meta#metadata.size / DataSize + 0.5),
    [ets:insert(?TAB_PENDING, {Key * DataSize}) || Key <- lists:seq(0, NumOfPackets - 1)].

%% @doc Calculate md5 hex checksum for file
calculate_md5(Path) ->
    {ok, FD} = file:open(Path, [read]),
    calculate_md5(FD, erlang:md5_init()).
calculate_md5(FD, Context) ->
    case file:pread(FD, cur, ?BUFFER_SIZE) of
        {ok, Data} -> calculate_md5(FD, erlang:md5_update(Context, Data));
        eof ->
            file:close(FD),
            list_to_binary([io_lib:format("~2.16.0b", [B]) || <<B>> <= erlang:md5_final(Context)])
    end.
