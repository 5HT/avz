-module(userhelper).
-export([updateProplist/2,updateUser/2]).

-include_lib("kvs/include/user.hrl").

updateProplist({K,V},P) ->
    Prop = case P of undefine -> []; _P -> _P end,
    case proplists:get_value(K,Prop) of
        undefined -> [{K,V} | Prop];
	        _ -> lists:keyreplace(K,1,Prop,{K,V})
    end.

updateUser(E,N) ->
    Username = case E#user.username of undefined -> N#user.username; _ -> E#user.username end,
    Tokens = N#user.tokens,
    Images = N#user.images,
    Names = case E#user.names of undefined -> N#user.names; _ -> E#user.names end,
    Surnames = case E#user.surnames of undefined -> N#user.surnames; _ -> E#user.surnames end,
    Birth = case E#user.birth of undefined -> N#user.birth; _ -> E#user.birth end,
    Sex = case E#user.sex of undefined -> N#user.sex; _ -> E#user.sex end,
    E#user{username=Username,tokens=Tokens,images=Images,names=Names,surnames=Surnames,birth=Birth,sex=Sex}.
