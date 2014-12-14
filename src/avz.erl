-module(avz).
-author('Maxim Sokhatsky').
-compile(export_all).
-include_lib("avz/include/avz.hrl").
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/user.hrl").

callbacks(Methods) -> [ M:callback() || M <- Methods].
sdk(Methods) -> [ M:sdk() || M <- Methods].
buttons(Methods) -> [ M:login_button() || M <- Methods].

event(init) -> [];
event(logout) -> wf:user(undefined), wf:redirect(?LOGIN_PAGE);
event(to_login) -> wf:redirect(?LOGIN_PAGE);
event({register, #user{}=U}) -> kvs:add(U), login_user(U); % sample
event({login, #user{}=U}) -> login_user(U);                % sample
event({Method,Event}) -> Method:event({Method,Event});
event(Ev) ->  error_logger:info_msg("Page Event ~p",[Ev]).

api_event(plusLogin, Args, Term) -> google:api_event(plusLogin, Args, Term);
api_event(fbLogin, Args, Term)   -> facebook:api_event(fbLogin, Args, Term);
api_event(winLogin, Args, Term)  -> microsoft:api_event(winLogin, Args, Term);
api_event(Name, Args, Term)      -> error_logger:info_msg("Unknown API event: ~p ~p ~p",[Name, Args, Term]).

login_user(User) -> wf:user(User), wf:redirect(?AFTER_LOGIN).
login(_Key, [{error, E}|_Rest])-> error_logger:info_msg("oauth error: ~p", [E]);
login(Key, Args) ->
    wf:info("AVZ MODULE: ~p",[?CTX#cx.module]),
    case kvs:get(user,Key:email_prop(Args,Key)) of
        {ok,Existed} ->
            RegData = Key:registration_data(Args, Key, Existed),
            (?CTX#cx.module):event({login, RegData});
        {error,_} ->
            RegData = Key:registration_data(Args, Key, #user{}),
            (?CTX#cx.module):event({register, RegData});
        U -> wf:info("avz:login unknown: ~p",[U]) end.
