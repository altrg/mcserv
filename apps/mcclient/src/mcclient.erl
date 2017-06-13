%%% @doc escript main module
-module(mcclient).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================
%% escript Entry point
main(Args) when length(Args) > 0 ->
    Defaults = #{url => undefined,
                 path => ".",
                 timeout => 5000,
                 loss => 0},
    case parse_opts(Args, Defaults) of
        {error, Err} ->
            io:format("Error: ~p~n", [Err]),
            halt(254);
        {ok, Cfg} ->
            application:load(mcclient),
            set_config(Cfg),
            application:ensure_all_started(mcclient),
            receive _ -> ok end
    end;

main(_) ->
    io:format("Usage: mcclient [OPTION] URL~n~n"
              "  -p  path to save a file, default current directory~n"
              "  -t  timeout to detect lost packets, ms, default 5000~n"
              "  -l  packet loss emulation, percentage, can be float, default 0~n~n"),
    halt(1).

%%====================================================================
%% Internal functions
%%====================================================================
parse_opts(["-p", Path| Args], Cfg) ->
    parse_opts(Args, Cfg#{path := Path});
parse_opts(["-t", Timeout0| Args], Cfg) ->
    case catch list_to_integer(Timeout0) of
        Timeout when is_integer(Timeout), Timeout > 0 ->
            parse_opts(Args, Cfg#{timeout := Timeout});
        _ -> {error, invalid_timeout}
    end;
parse_opts(["-l", Loss0| Args], Cfg) ->
    case catch list_to_integer(Loss0) of
        Loss when Loss >= 0, Loss =< 100 ->
            parse_opts(Args, Cfg#{loss := Loss});
        _ -> case catch list_to_float(Loss0) of
                 Loss when Loss >= 0, Loss =< 100 ->
                     parse_opts(Args, Cfg#{loss := Loss});
                 _ -> {error, invalid_loss}
             end
    end;
parse_opts([Url], Cfg) -> {ok, Cfg#{url => Url}};
parse_opts([], _Cfg) -> {error, url_required}.

set_config(Cfg) ->
    [application:set_env(mcclient, K, V) || {K, V} <- maps:to_list(Cfg)].
