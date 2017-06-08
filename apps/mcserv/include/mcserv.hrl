-compile([{parse_transform, lager_transform}]).

-define(LOG(Level, Text), lager:Level(Text)).
-define(LOG(Level, Text, Params), lager:Level(Text, Params)).

-define(CFG(Key, Default), application:get_env(mcserv, Key, Default)).
