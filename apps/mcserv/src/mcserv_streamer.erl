%%% @doc This sends packets to multicast group
%%% Config:
%%%   mcast_address - mutlicast address, ip tuple
%%%   mcast_port    - mutlicast port, integer
%%% @end
-module(mcserv_streamer).
-behavior(gen_server).

-include("mcserv.hrl").

%% API
-export([start_link/0, send/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {address :: inet:ip_address(),
                port    :: pos_integer(),
                socket  :: gen_udp:socket()
         }).

%%====================================================================
%% API functions
%%====================================================================
%% @doc Start caster process
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Send packet
-spec send(binary()) -> ok.
send(Packet) ->
    gen_server:cast(?MODULE, {send, Packet}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    Address = ?CFG(mcast_address, undefined),
    Port = ?CFG(mcast_port, undefined),
    {ok, Socket} = gen_udp:open(0),
    ?LOG(info, "Streamer started: address=~p port=~b", [Address, Port]),
    {ok, #state{address=Address,
                port=Port,
                socket=Socket}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({send, Packet}, State) ->
    case gen_udp:send(State#state.socket, State#state.address, State#state.port, Packet) of
        ok -> ok;
        Err -> ?LOG(error, "Can't sent packet: ~p", [Err])
    end,
    {noreply, State}.

handle_info(_info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
