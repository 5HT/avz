-module(avz).
-author('Maxim Sokhatsky').
-compile(export_all).
-include_lib("avz/include/avz.hrl").
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/user.hrl").

sha(Pass) -> crypto:hmac(wf:config(n2o,hmac,sha256),n2o_secret:secret(),wf:to_binary(Pass)).
update({K,V},P) -> wf:setkey(K,1,case P of undefined -> []; _P -> _P end,{K,V}).

coalesce(_,Y) -> Y.
merge(A,B) -> list_to_tuple([ coalesce(X,Y) || {X,Y} <- lists:zip(tuple_to_list(A),tuple_to_list(B)) ]).

callbacks(Methods) -> [ M:callback()     || M <- Methods].
sdk(Methods)       -> [ M:sdk()          || M <- Methods].
buttons(Methods)   -> [ M:login_button() || M <- Methods].

event(init) -> [];
event(logout) -> wf:user(undefined), wf:redirect(?LOGIN_PAGE);
event(to_login) -> wf:redirect(?LOGIN_PAGE);
event({register, #user{}=U}) -> kvs:add(U), login_user(U); % sample
event({login, #user{}=U, N}) -> Updated = merge(U,N), kvs:put(Updated), login_user(Updated); % sample
event({Method,Event}) -> Method:event({Method,Event});
event(Ev) ->  wf:info(?MODULE,"Page Event ~p",[Ev]).

api_event(plusLogin, Args, Term) -> google:api_event(plusLogin, Args, Term);
api_event(fbLogin, Args, Term)   -> facebook:api_event(fbLogin, Args, Term);
api_event(winLogin, Args, Term)  -> microsoft:api_event(winLogin, Args, Term);
api_event(Name, Args, Term)      -> wf:info(?MODULE,"Unknown API event: ~p ~p ~p",[Name, Args, Term]).

login_user(User) -> wf:user(User), wf:redirect(?AFTER_LOGIN).
login(_Key, [{error, E}|_Rest])-> wf:info(?MODULE,"Auth Error: ~p", [E]);
login(Key, Args) ->
    n2o_session:ensure_sid([],?CTX,[]),
    case kvs:get(user,Key:email_prop(Args,Key)) of
        {ok,Existed} ->
            RegData = Key:registration_data(Args, Key, Existed),
            (?CTX#cx.module):event({login, Existed, RegData});
        {error,_} ->
            RegData = Key:registration_data(Args, Key, #user{}),
            (?CTX#cx.module):event({register, RegData});
        U -> wf:info(?MODULE,"Unknown Login: ~p",[U]) end.

version() -> proplists:get_value(vsn,element(2,application:get_all_key(?MODULE))).
