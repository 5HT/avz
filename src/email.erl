-module(email).
-author('Andrii Zadorozhnii').
-include("avz.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/n2o.hrl").
-compile(export_all).
-export(?API).

registration_data(Props, email, Ori)->
  Email = email_prop(Props, email),
  #{<<"display_name">>:=DisplayName, <<"first_name">>:=FirstName, <<"last_name">>:=LastName,<<"status">>:=Status, <<"type">>:=Type, <<"password">>:=Pass} = Props,
  #{tokens:=Tokens} = Ori,
  maps:merge(Ori, #{ display_name => DisplayName,
            email => Email,
            names    => FirstName,
            surnames => LastName,
            register_date => os:timestamp(),
            tokens => avz:update({email,Email},Tokens),
            status => Status,
            type   => Type,
            password => avz:sha(Pass)}).

index(K) -> nitro:to_binary(K).
email_prop(Props, _) -> maps:get(<<"email">>, Props).

login_button() -> #button{id=login, body= <<"Sign in">>, postback={email, loginemail}, source=[user,pass]}.
event({email,loginemail}) -> avz:login(email, [{<<"email">>, nitro:q(user)}, {<<"password">>,nitro:q(pass)}]);
event(_) -> ok.
api_event(_,_,_) -> ok.
callback() -> ok.
sdk() -> [].

