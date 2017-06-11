-define(HEADER_SIZE, 5). % packet header size (position placeholder), bytes

-define(BUFFER_SIZE, 512*1024). % file delayed_write buffer size
-define(DELAY, 2). % file delayed_write delay, seconds

-define(LOG, io:format).

-define(CFG(Key, Default), application:get_env(mcclient, Key, Default)).

-record(metadata, {name      :: binary(),
                   size      :: pos_integer(),
                   md5       :: binary(),
                   position  :: non_neg_integer(),
                   data_size :: pos_integer(),
                   address   :: tuple(),
                   port      :: pos_integer()}).
