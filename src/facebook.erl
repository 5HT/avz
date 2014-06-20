-module(facebook).
-author('Andrii Zadorozhnii').
-include_lib("avz/include/avz.hrl").
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/user.hrl").
-compile(export_all).
-export(?API).
-define(HTTP_ADDRESS, case application:get_env(web, http_address) of {ok, A} -> A; _ -> "" end).
-define(FB_APP_ID, case application:get_env(web, fb_id) of {ok, Id} -> Id; _-> "" end).

callback() -> ok.
event({facebook,Event}) -> wf:wire("fb_login();"), ok.
api_event(fbLogin, Args, _Term)-> JSArgs = n2o_json:decode(Args), avz:login(facebook, JSArgs#struct.lst).

registration_data(Props, facebook, Ori)->
    Id = proplists:get_value(<<"id">>, Props),
    UserName = binary_to_list(proplists:get_value(<<"username">>, Props)),
    BirthDay = case proplists:get_value(<<"birthday">>, Props) of
        undefined -> {1, 1, 1970};
        BD -> list_to_tuple([list_to_integer(X) || X <- string:tokens(binary_to_list(BD), "/")]) end,
    error_logger:info_msg("User Ori: ~p",[Ori]),
    error_logger:info_msg("Props: ~p",[Props]),
    Id = proplists:get_value(<<"id">>, Props),
    Email = email_prop(Props, facebook),
    Ori#user{   id = Email,
                display_name = UserName,
                avatar = "https://graph.facebook.com/" ++ UserName ++ "/picture",
                email = Email,
                names = proplists:get_value(<<"first_name">>, Props),
                surnames = proplists:get_value(<<"last_name">>, Props),
                tokens = [{facebook,Id}|Ori#user.tokens],
                birth = {element(3, BirthDay), element(1, BirthDay), element(2, BirthDay)},
                register_date = erlang:now(),
                status = ok }.

email_prop(Props, _) -> proplists:get_value(<<"email">>, Props).

login_button() -> #panel{class=["btn-group"], body=
    #link{id=loginfb, class=[btn, "btn-primary", "btn-large", "btn-lg"],
        body=[#i{class=[fa,"fa-facebook","fa-lg","icon-facebook","icon-large"]}, <<"Facebook">>],
           postback={facebook,loginClick}
%    actions = "$('#loginfb').on('click', fb_login);"
             }}.

sdk() ->
    wf:wire(#api{name=setFbIframe, tag=fb}),
    wf:wire(#api{name=fbAutoLogin, tag=fb}),
    wf:wire(#api{name=fbLogin, tag=fb}),
  [ #dtl{bind_script=false, file="facebook_sdk", ext="dtl", folder="priv/static/js",
        bindings=[{appid, ?FB_APP_ID},{channelUrl, ?HTTP_ADDRESS ++ "/static/channel.html"} ] } ].
