-define(HTTP_PATH, "/").
-define(HTTP_ACCEPTORS, 5).

-define(HEADER_SIZE, 5). % packet header size (position placeholder), bytes

-define(BUFFER_SIZE, 512*1024). % file read_ahead buffer size

-compile([{parse_transform, lager_transform}]).
-define(LOG(Level, Text), lager:Level(Text)).
-define(LOG(Level, Text, Params), lager:Level(Text, Params)).

-define(CFG(Key, Default), application:get_env(mcserv, Key, Default)).

-record(file_info, {path :: string(),
                    name :: string(),
                    size :: pos_integer(),
                    md5  :: binary(),
                    pos  :: non_neg_integer(),
                    fd   :: file:io_device()
                   }).
