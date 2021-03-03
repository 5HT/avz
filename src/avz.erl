-module(avz).
-author('Maxim Sokhatsky').
-compile(export_all).
-include("avz.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/n2o.hrl").
%-include_lib("kvs/include/metainfo.hrl").

sha(Pass) -> crypto:hmac(application:get_env(n2o,hmac,sha256),n2o_secret:secret(),nitro:to_binary(Pass)).

update({K,V},P) -> lists:keyreplace(K,1,case P of undefined -> []; _ -> P end,{K,V}).

coalesce(X,undefined) -> X;
coalesce(_,Y) -> Y.
merge(A,B) -> list_to_tuple([ coalesce(X,Y) || {X,Y} <- lists:zip(tuple_to_list(A),tuple_to_list(B)) ]).

callbacks(Methods) -> [ M:callback()     || M <- Methods].
sdk(Methods)       -> [ M:sdk()          || M <- Methods].
buttons(Methods)   -> [ M:login_button() || M <- Methods].

event(init) -> [];
event(logout) -> n2o:user(undefined), nitro:redirect(?LOGIN_PAGE);
event(to_login) -> nitro:redirect(?LOGIN_PAGE);
event({register, U}) ->
  io:format("Reg: ~p~n", [U]),
%  kvs:put(U#user{id=kvs:next_id("user", 1)}),
  login_user(U); % sample
event({login, U, N}) ->
  io:format("login: ~p~n~p~n", [U, N]),
  Updated = merge(U,N),
%  kvs:put(Updated),
  login_user(Updated); % sample

event({error, E}) -> (?CTX#cx.module):event({login_failed, E});
event({Method,Event}) -> Method:event({Method,Event});
event(Ev) ->  io:format("Page Event ~p",[Ev]).

api_event(gLogin, Args, Term) -> google:api_event(gLogin, Args, Term);
api_event(gLoginFail, Args, Term) -> google:api_event(gLoginFail, Args, Term);
api_event(fbLogin, Args, Term)   -> facebook:api_event(fbLogin, Args, Term);
api_event(winLogin, Args, Term)  -> microsoft:api_event(winLogin, Args, Term);
api_event(Name, Args, Term)      -> io:format("Unknown API event: ~p ~p ~p",[Name, Args, Term]).

login_user(User) -> n2o:user(User), nitro:redirect(?AFTER_LOGIN).
login(_Key, [{error, E}|_Rest])-> io:format("Auth Error: ~p", [E]);
login(Key, Args) ->
  LoginFun = fun(K) ->
    Index1 = Key:index(K),
    #{Index1 := Index} = Args, % is_key
    %Index = proplists:get_value(Key:index(K), Args),
    case kvs:index(user,K,Index) of
      [Exists|_] ->
        %Diff = tuple_size(Exists) - tuple_size(#user{}),
        %{It, UsrExt} = lists:split(tuple_size(#iterator{}), tuple_to_list(Exists)),
        %{_,Usr} = lists:split(Diff, UsrExt),
        RegData = Key:registration_data(Args, Key, maps:from_list(lists:append([Exists]))),
        (?CTX#cx.module):event({login, Exists, RegData}),
        true;
      _ -> false end end,

%  Keys = [K || M<-kvs:modules(),T<-(M:metainfo())#schema.tables, T#table.name==user, K<-T#table.keys],
  Keys = [],

  LoggedIn = lists:any(LoginFun, Keys),

  if (LoggedIn =:= true) -> true; true -> 
    RegData = Key:registration_data(Args, Key, #{}),
    (?CTX#cx.module):event({register, RegData})
  end.

version() -> proplists:get_value(vsn,element(2,application:get_all_key(?MODULE))).
