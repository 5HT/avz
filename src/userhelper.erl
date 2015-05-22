-module(userhelper).
-export([updateProplist/2]).

-include_lib("kvs/include/user.hrl").

updateProplist({K,V},P) ->
    Prop = case P of undefine -> []; _P -> _P end,
    case proplists:get_value(K,Prop) of
        undefined -> [{K,V} | Prop];
	        _ -> lists:keyreplace(K,1,Prop,{K,V})
    end.

