%%% @doc escript main module
-module(mcclient).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================
%% escript Entry point
main([Url| Args]) ->
    case Args of
        [Path| _] -> ok;
        _ -> Path = "."
    end,
    application:load(mcclient),
    set_config([{url, Url}, {path, Path}]),
    application:ensure_all_started(mcclient),
    receive _ -> ok end;

main(_) ->
    io:format("Usage: mcclient URL [PATH]~n"),
    halt(1).

%%====================================================================
%% Internal functions
%%====================================================================
set_config(Config) ->
    [application:set_env(mcclient, K, V) || {K, V} <- Config].
