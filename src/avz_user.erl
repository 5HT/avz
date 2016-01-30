
-module(avz_user).
-include("avz_user.hrl").
-include_lib("kvs/include/metainfo.hrl").
-compile(export_all).
metainfo() ->
    #schema{name = kvs,
            tables = [#table{name = avz_user,
                           container = feed,
                           fields = record_info(fields, avz_user),
                           keys = [email]}]}.
