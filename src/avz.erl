-module(avz).
-author('Maxim Sokhatsky').
-compile(export_all).
-include_lib("avz/include/avz.hrl").
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").

callbacks(Methods) -> [ M:callback() || M <- Methods].
sdk(Methods) -> [ M:sdk() || M <- Methods].
buttons(Methods) -> [ M:login_button() || M <- Methods].

event(init) -> [];
event(logout) -> wf:user(undefined), wf:redirect(?LOGIN_PAGE);
event(login) -> wf:redirect(?AFTER_LOGIN);
event(to_login) -> wf:redirect(?LOGIN_PAGE);
event({Method,Event}) -> Method:event({Method,Event});
event(Ev) ->  error_logger:info_msg("Page Event ~p",[Ev]).

api_event(plusLogin, Args, Term) -> google:api_event(plusLogin, Args, Term);
api_event(fbLogin, Args, Term)   -> facebook:api_event(fbLogin, Args, Term);
api_event(winLogin, Args, Term)   -> microsoft:api_event(winLogin, Args, Term);
api_event(Name, Args, Term)      -> error_logger:info_msg("Unknown API event: ~p ~p ~p",[Name, Args, Term]).

login_user(User) -> wf:user(User), wf:redirect(?AFTER_LOGIN).
login(Key, Args)-> case Args of [{error, E}|_Rest] -> error_logger:info_msg("oauth error: ~p", [E]);
    _ -> case kvs:get(user,Key:email_prop(Args,Key)) of
              {ok,Existed} -> {_Id, RegData} = Key:registration_data(Args, Key, Existed), login_user(RegData);
              {error,_} -> {_Id, RegData} = Key:registration_data(Args, Key, #user{}),
                  kvs:put(RegData), login_user(RegData) end end.
