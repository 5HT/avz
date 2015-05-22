-module(microsoft).
-author('Maxim Sokhatsku').
-include_lib("avz/include/avz.hrl").
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/user.hrl").
-compile(export_all).
-export(?API).
-define(CLIENT_ID, "000000004C0FEEB0").
-define(REDIRECT_URI, "http://skyline.synrc.com:8000").

api_event(_, Args, _)-> 
    error_logger:info_msg("Args: ~p",[Args]),
    JSArgs = string:tokens(Args,"\\\\"),
    J = string:join(JSArgs,""),
    error_logger:info_msg("J: ~p",[J]),
    [_|K1] = J,
    [_|K2] = lists:reverse(K1),
    K = lists:reverse(K2),
    error_logger:info_msg("K: ~p",[K]),
    Struct = n2o_json:decode(K),
    avz:login(microsoft, Struct#struct.lst).

registration_data(Props, microsoft, Ori)->
    error_logger:info_msg("Microsoft Login: ~p",[Props]),
    Id = proplists:get_value(<<"id">>, Props),
    GivenName = proplists:get_value(<<"first_name">>, Props),
    FamilyName = proplists:get_value(<<"last_name">>, Props),
    Email = email_prop(Props,microsoft),
    Ori#user{   id = Email,
                display_name = proplists:get_value(<<"name">>, Props),
                email = Email,
                names = GivenName,
                surnames = FamilyName,
                tokens = userhelper:updateProplist({microsoft,Id},Ori#user.tokens),
                register_date = erlang:now(),
                sex = proplists:get_value(<<"gender">>, Props),
                status = ok }.

email_prop(Props, _) -> proplists:get_value(<<"id">>, Props).

login_button()-> #panel{class=["btn-group"], body=
    #link{id=microsoftbtn, class=[btn, "btn-microsoft", "btn-large"], 
        body=[#i{class=["icon-microsoft", "icon-large"]}, <<"Microsoft">>],
              actions= "$('#microsoftbtn').on('click', microsoft_login);" }}.

event(_) -> ok.
callback() -> ok.
sdk() ->
    wf:wire(#api{name=winLogin, tag=plus}),
    #dtl{bind_script=false, file="microsoft_sdk", ext="dtl", folder="priv/static/js",
        bindings=[{event, microsoft_login},{client, ?CLIENT_ID},{redirect, ?REDIRECT_URI}]}.

