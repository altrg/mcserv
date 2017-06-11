%%% @doc This module receives multicast traffic and feeds it to Assembler
%%% @end
-module(mcclient_receiver).
-behavior(gen_server).

-include("mcclient.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {socket  :: gen_udp:socket()
               }).

%%====================================================================
%% API functions
%%====================================================================
%% @doc Start receiver process
-spec start_link(#metadata{}) -> {ok, pid()} | ignore | {error, term()}.
start_link(Meta) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Meta], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Meta]) ->
    {ok, Socket} = gen_udp:open(Meta#metadata.port,
                                [{add_membership, {Meta#metadata.address, {0,0,0,0}}},
                                 {active, once}, binary]),
    ?LOG("Receiver  started: port=~b~n", [Meta#metadata.port]),
    {ok, #state{socket=Socket}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, Socket, _IP, _InPortNo, Packet}, State) ->
    mcclient_assembler:process_packet(Packet),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({udp_passive, _Socket}, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
