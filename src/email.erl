-module(email).
-author('Andrii Zadorozhnii').
-include_lib("avz/include/avz.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/user.hrl").
-compile(export_all).
-export(?API).

registration_data(Props, email, Ori)->
  Email = email_prop(Props, email),
  
  Ori#user{ display_name = proplists:get_value(<<"display_name">>, Props, Email),
            email = Email,
            names    = proplists:get_value(<<"first_name">>, Props,[]),
            surnames = proplists:get_value(<<"last_name">>,  Props,[]),
            register_date = os:timestamp(),
            tokens = avz:update({email,Email},Ori#user.tokens),
            status = proplists:get_value(<<"status">>, Props, ok),
            type   = proplists:get_value(<<"type">>, Props,[]),
            password = avz:sha(proplists:get_value(<<"password">>,Props))}.

index(K) -> wf:to_binary(K).
email_prop(Props, _) -> proplists:get_value(<<"email">>, Props).

login_button() -> #button{id=login, body= <<"Sign in">>, postback={email, loginemail}, source=[user,pass]}.
event({email,loginemail}) -> avz:login(email, [{<<"email">>, wf:q(user)}, {<<"password">>, wf:q(pass)}]);
event(_) -> ok.
api_event(_,_,_) -> ok.
callback() -> ok.
sdk() -> [].

