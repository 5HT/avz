-module(microsoft).
-author('Maxim Sokhatsku').
-include_lib("avz/include/avz.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/user.hrl").
-compile(export_all).
-export(?API).
-define(CLIENT_ID,    "000000004C0FEEB0").
-define(REDIRECT_URI, "http://skyline.synrc.com:8000").

api_event(_, Args, _)->
    JSArgs = string:tokens(Args,"\\\\"),
    J = string:join(JSArgs,""),
    [_|K1] = J,
    [_|K2] = lists:reverse(K1),
    K = lists:reverse(K2),
    {D} = ?AVZ_JSON:decode(list_to_binary(K)),
    avz:login(microsoft, D).

registration_data(Props, microsoft, Ori)->
    wf:info(?MODULE,"Microsoft Login: ~p",[Props]),
    Id = proplists:get_value(<<"id">>, Props),
    GivenName = proplists:get_value(<<"first_name">>, Props),
    FamilyName = proplists:get_value(<<"last_name">>, Props),
    Email = email_prop(Props,microsoft),
    Ori#user{   id = Email,
                display_name = proplists:get_value(<<"name">>, Props),
                email = Email,
                names = GivenName,
                surnames = FamilyName,
                tokens = avz:update({microsoft,Id},Ori#user.tokens),
                register_date = os:timestamp(),
                sex = proplists:get_value(<<"gender">>, Props),
                status = ok }.

email_prop(Props, _) -> proplists:get_value(<<"id">>, Props).

login_button()-> application:get_env(avz,microsoft_button,#panel{class=["btn-group"], body=
    #link{id=microsoftbtn, class=[btn, "btn-microsoft", "btn-large"], 
        body=[#i{class=["icon-microsoft", "icon-large"]}, <<"Microsoft">>],
              actions= "$('#microsoftbtn').on('click', microsoft_login);" }}).

event(_) -> ok.
callback() -> ok.
sdk() ->
    wf:wire(#api{name=winLogin, tag=plus}),
    #dtl{bind_script=false, file="microsoft_sdk", ext="dtl", folder="priv/static/js",
        bindings=[{event, microsoft_login},{client, ?CLIENT_ID},{redirect, ?REDIRECT_URI}]}.

