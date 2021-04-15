-module(avz).
-author('Maxim Sokhatsky').
-description("Social Auth Login Nitro Protocol").
-behaviour(application).
-behaviour(supervisor).
-include_lib("n2o/include/n2o.hrl").
-include_lib("nitro/include/nitro.hrl").
-include("proto.hrl").

-export([init/1, start/2, stop/1]).
-export([info/3]).
-compile(export_all).

init([])    -> {ok, {{one_for_one, 5, 10}, []}}.
start(_,_)  -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
stop(_)     -> ok.

info(#avz{cid=Cid, cmd=init}=M,R,Ctx) -> {reply,{bert,nitro_n2o:io([],Ctx)},R,Ctx};
info(#eml{}=M,R,S) -> email:info(M,R,S);
info(#cph{}=M,R,S) -> cipher:info(M,R,S);
info(#tlg{}=M,R,S) -> telegram:info(M,R,S);
info(M,R,S) -> {unknown,M,R,S}.

sha(Pass) ->
  Key = application:get_env(n2o,secret,<<"ThisIsClassified">>),
  SubType = application:get_env(n2o,hmac,sha256),
  crypto:mac(hmac,SubType,Key,nitro:to_binary(Pass)).  

update({K,V},P) -> lists:keyreplace(K,1,case P of undefined -> []; _ -> P end,{K,V}).

coalesce(X,undefined) -> X;
coalesce(_,Y) -> Y.
merge(A,B) -> list_to_tuple([ coalesce(X,Y) || {X,Y} <- lists:zip(tuple_to_list(A),tuple_to_list(B)) ]).

callbacks(Methods) -> [ M:callback()     || M <- Methods].
sdk(Methods)       -> [ M:sdk()          || M <- Methods].
buttons(Methods)   -> [ M:login_button() || M <- Methods].

event(init) -> [];
event({Method,Event}) -> Method:event({Method,Event});
event(Ev) ->  io:format("Page Event ~p",[Ev]).

api_event(gLogin, Args, Term) -> google:api_event(gLogin, Args, Term);
api_event(gLoginFail, Args, Term) -> google:api_event(gLoginFail, Args, Term);
api_event(fbLogin, Args, Term)   -> facebook:api_event(fbLogin, Args, Term);
api_event(winLogin, Args, Term)  -> microsoft:api_event(winLogin, Args, Term);
api_event(Name, Args, Term)      -> io:format("Unknown API event: ~p ~p ~p",[Name, Args, Term]).

login(_Key, [{error, E}|_Rest])-> io:format("Auth Error: ~p", [E]).

version() -> proplists:get_value(vsn,element(2,application:get_all_key(?MODULE))).
