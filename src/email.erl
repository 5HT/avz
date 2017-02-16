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
  Ori#user{ display_name = Email,
            email = Email,
            register_date = os:timestamp(),
            tokens = avz:update({email,Email},Ori#user.tokens),
            status = ok,
            password = avz:sha(proplists:get_value(<<"password">>,Props))}.

index(K) -> wf:to_binary(K).
email_prop(Props, _) -> binary_to_list(proplists:get_value(<<"email">>, Props)).

login_button() -> #button{id=login, body= <<"Sign in">>, postback={email, loginemail}, source=[user,pass]}.
event({email,loginemail}) -> avz:login(email, [{<<"email">>, list_to_binary(wf:q(user))}, {<<"password">>, wf:q(pass)}]);
event(_) -> ok.
api_event(_,_,_) -> ok.
callback() -> ok.
sdk() -> [].

