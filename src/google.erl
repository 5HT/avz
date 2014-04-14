-module(google).
-author('Andrii Zadorozhnii').
-include_lib("avz/include/avz.hrl").
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/user.hrl").
-compile(export_all).
-export(?API).
-define(GPLUS_CLIENT_ID, case application:get_env(web, gplus_client_id) of {ok, Id} -> Id; _-> "" end).
-define(GPLUS_COOKIE_POLICY, case application:get_env(web, gplus_cookiepolicy) of {ok, P} -> P; _-> "" end).

api_event(plusLogin, Args, _)-> JSArgs = n2o_json:decode(Args), avz:login(google, JSArgs#struct.lst).

registration_data(Props, google, Ori)->
    Id = proplists:get_value(<<"id">>, Props),
    Name = proplists:get_value(<<"name">>, Props),
    GivenName = proplists:get_value(<<"givenName">>, Name#struct.lst),
    FamilyName = proplists:get_value(<<"familyName">>, Name#struct.lst),
    Image = proplists:get_value(<<"image">>, Props),
    Email = email_prop(Props,google),
    Ori#user{   id = Email,
                display_name = proplists:get_value(<<"displayName">>, Props),
                avatar = lists:nth(1,string:tokens(
                        binary_to_list(proplists:get_value(<<"url">>, Image#struct.lst)), "?")),
                email = Email,
                names = GivenName,
                surnames = FamilyName,
                tokens = [{google,Id}|Ori#user.tokens],
                register_date = erlang:now(),
                sex = proplists:get_value(gender, Props),
                status = ok }.

email_prop(Props, _) -> binary_to_list(proplists:get_value(<<"email">>, Props)).

login_button()-> #panel{id=plusloginbtn, class=["btn-group"], body=
    #link{class=[btn, "btn-google-plus", "btn-large","btn-lg"],
        body=[#i{class=[fa,"fa-google-plus","fa-lg","icon-google-plus", "icon-large"]}, <<"Google">>] }}.

event(_) -> ok.
callback() -> ok.
sdk() ->
    wf:wire(#api{name=plusLogin, tag=plus}),
    #dtl{bind_script=false, file="google_sdk", ext="dtl", folder="priv/static/js",
        bindings=[{loginbtnid, plusloginbtn},{clientid, ?GPLUS_CLIENT_ID},{cookiepolicy, ?GPLUS_COOKIE_POLICY}]}.

