-module(facebook).
-author('Andrii Zadorozhnii').
-include_lib("avz/include/avz.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/n2o.hrl").

-compile(export_all).
-export(?API).

-define(FB_APP_ID,    application:get_env(avz, fb_id,        [])).

callback() -> ok.
event({facebook,_Event}) -> nitro:wire("fb_login();"), ok.
api_event(fbLogin, Args, _Term) -> JSArgs = ?AVZ_JSON:decode(list_to_binary(Args)), avz:login(facebook, JSArgs).

registration_data(Props, facebook, Ori) ->
    #{<<"id">> :=Id, <<"first_name">>:=FirstName, <<"last_name">>:=LastName, <<"email">>:=Email} = Props,
    BirthDay = case maps:is_key(<<"birthday">>, Props) of true -> #{<<"birthday">>:=BD}=Props,
      list_to_tuple([list_to_integer(X) || X <- string:tokens(binary_to_list(BD), "/")]);false -> {1, 1, 1970} end,
    [UserName|_] = string:tokens(binary_to_list(Email),"@"),
    Cover = case maps:is_key(<<"cover">>,Props) of true -> #{<<"cover">>:=C} = Props, binary_to_list(C);false -> "" end,
    #{images:=Images, tokens:=Tokens} = Ori,
    maps:merge(Ori,   #{ display_name => UserName,
                images => avz:update({fb_cover,Cover},avz:update({fb_avatar,"https://graph.facebook.com/" ++ binary_to_list(Id) ++ "/picture?type=large"},Images)),
                email => Email,
                names => FirstName,
                surnames => LastName,
                tokens => avz:update({facebook,Id},Tokens),
                birth => {element(3, BirthDay), element(1, BirthDay), element(2, BirthDay)},
                register_date => os:timestamp(),
                status => ok }).

index(K) -> nitro:to_binary(K).
email_prop(Props, _) -> #{<<"email">>:=P}=Props, P.

login_button() -> 
  #link{id=loginfb, body=[<<"Facebook">>], postback={facebook,loginClick}}.

sdk() ->
    nitro:wire(#api{name=fbLogin, tag=fb}),
    [ #dtl{bind_script=false, file="facebook_sdk", ext="dtl", folder="priv/static/js",
        bindings=[{appid, ?FB_APP_ID}] } ].
