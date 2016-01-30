-ifndef(AVZ_USER_HRL).
-define(AVZ_USER_HRL, true).

-include_lib("kvs/include/kvs.hrl").

-record(avz_user,
        {?ITERATOR(feed, true),
         email,
         username,
         password,
         display_name,
         register_date,
         tokens = [],
         images,
         names,
         surnames,
         birth,
         sex,
         date,
         status,
         zone,
         type,
         comments = []
        }).

-endif.
