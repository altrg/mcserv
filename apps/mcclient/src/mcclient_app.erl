%%%-------------------------------------------------------------------
%% @doc mcclient public API
%% @end
%%%-------------------------------------------------------------------
-module(mcclient_app).
-behaviour(application).

-include("mcclient.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
start(_StartType, _StartArgs) ->
    Url = ?CFG(url, undefined),
    case get_metadata(Url) of
        {error, Err} ->
            ?LOG("Can't get metadata from '~s'!~nError: ~p~n", [Url, Err]),
            init:stop(); % hard but escript compatible :)
        {ok, Meta} ->
            ?LOG("Metadata received: file='~s' size=~b position=~b~n",
                 [Meta#metadata.name, Meta#metadata.size, Meta#metadata.position]),
            mcclient_sup:start_link(Meta)
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
%% doc Get metadata from the specified url
get_metadata(Url) ->
    case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} -> parse_metadata(Body);
        {_, Err} -> {error, Err}
    end.

%% @doc Parse received metadata
parse_metadata(Body) ->
    parse_metadata(binary:split(Body, <<"\n">>, [global]), #metadata{}).
parse_metadata([<<"name=", Name/binary>>| Lines], Res) ->
    parse_metadata(Lines, Res#metadata{name=Name});
parse_metadata([<<"size=", Size0/binary>>| Lines], Res) ->
    case to_int(Size0) of
        {ok, Size} when Size > 0 -> parse_metadata(Lines, Res#metadata{size=Size});
        _ -> {error, "Can't parse size"}
    end;
parse_metadata([<<"md5=", MD5/binary>>| Lines], Res) ->
    parse_metadata(Lines, Res#metadata{md5=MD5});
parse_metadata([<<"pos=", Pos0/binary>>| Lines], Res) ->
    case to_int(Pos0) of
        {ok, Pos} when Pos >= 0 -> parse_metadata(Lines, Res#metadata{position=Pos});
        _ -> {error, "Can't parse pos"}
    end;
parse_metadata([<<"data_size=", PSize0/binary>>| Lines], Res) ->
    case to_int(PSize0) of
        {ok, Size} when Size >= 0 -> parse_metadata(Lines, Res#metadata{data_size=Size});
        _ -> {error, "Can't parse data size"}
    end;
parse_metadata([<<"mcast_address=", Addr0/binary>>| Lines], Res) ->
    case inet:parse_address(binary_to_list(Addr0)) of
        {ok, Addr} -> parse_metadata(Lines, Res#metadata{address=Addr});
        _ -> {error, "Can't parse multicast address"}
    end;
parse_metadata([<<"mcast_port=", Port0/binary>>| Lines], Res) ->
    case to_int(Port0) of
        {ok, Port} when Port > 0 -> parse_metadata(Lines, Res#metadata{port=Port});
        _ -> {error, "Can't parse multicast port"}
    end;
parse_metadata([_| Lines], Res) -> parse_metadata(Lines, Res);
parse_metadata([], Res) -> {ok, Res}.

%% @doc Convert binary to integer
to_int(Bin) ->
    case catch binary_to_integer(Bin) of
        Int when is_integer(Int) -> {ok, Int};
        _ -> {error, invalid_value}
    end.
