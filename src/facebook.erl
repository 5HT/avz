-module(facebook).
-author('Andrii Zadorozhnii').
-include_lib("avz/include/avz.hrl").
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-export(?API).

-define(HTTP_ADDRESS, case application:get_env(web, http_address) of {ok, A} -> A; _ -> "" end).
-define(FB_APP_ID, case application:get_env(web, fb_id) of {ok, Id} -> Id; _-> "" end).

event({facebook,_}) -> ok.
api_event(fbLogin, Args, _Term)-> JSArgs = n2o_json:decode(Args), avz:login(facebook, JSArgs#struct.lst).

registration_data(Props, facebook, Ori)->
    Id = proplists:get_value(<<"id">>, Props),
    UserName = binary_to_list(proplists:get_value(<<"username">>, Props)),
    BirthDay = case proplists:get_value(<<"birthday">>, Props) of
        undefined -> {1, 1, 1970};
        BD -> list_to_tuple([list_to_integer(X) || X <- string:tokens(binary_to_list(BD), "/")]) end,
    error_logger:info_msg("User Ori: ~p",[Ori]),
    error_logger:info_msg("Props: ~p",[Props]), 
    { proplists:get_value(<<"id">>, Props), 
      Ori#user{ display_name = UserName,
                avatar = "https://graph.facebook.com/" ++ UserName ++ "/picture",
                email = email_prop(Props, facebook_id),
                name = proplists:get_value(<<"first_name">>, Props),
                surname = proplists:get_value(<<"last_name">>, Props),
                facebook_id = Id,
                age = {element(3, BirthDay), element(1, BirthDay), element(2, BirthDay)},
                register_date = erlang:now(),
                status = ok }}.

email_prop(Props, _) -> binary_to_list(proplists:get_value(<<"email">>, Props)).

login_button() -> #panel{class=["btn-group"], body=
    #link{id=loginfb, class=[btn, "btn-primary", "btn-large"], 
        body=[#i{class=["icon-facebook", "icon-large"]}, <<"Facebook">>],
            actions= "$('#loginfb').on('click', fb_login);" }}.

sdk() ->
    wf:wire(#api{name=setFbIframe, tag=fb}),
    wf:wire(#api{name=fbAutoLogin, tag=fb}),
    wf:wire(#api{name=fbLogin, tag=fb}),
  [ #panel{id="fb-root"},
    #dtl{bind_script=false, file="facebook_sdk", ext="dtl", folder="priv/static/js",
        bindings=[{appid, ?FB_APP_ID},{channelUrl, ?HTTP_ADDRESS ++ "/static/channel.html"} ] } ].
