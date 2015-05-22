-module(github).
-author('Andrii Zadorozhnii').
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/user.hrl").
-include_lib("avz/include/avz.hrl").
-compile(export_all).
-export(?API).

-define(CLIENT_ID, case application:get_env(web, github_client_id) of {ok, K} -> K;_-> "" end).
-define(CLIENT_SECRET, case application:get_env(web, github_client_secret) of {ok, S} -> S; _-> "" end).
-define(OAUTH_URI, "https://github.com/login/oauth").
-define(AUTHORIZE_URI, ?OAUTH_URI ++ "/authorize").
-define(ACCESS_TOKEN_URI, ?OAUTH_URI ++ "/access_token").
-define(API_URI, "https://api.github.com").
-define(REQ_HEADER, [{"User-Agent", "Erlang PaaS"}]).

user(Props) -> api_call("/user", Props).

authorize_url() -> oauth:uri(?AUTHORIZE_URI, [{"client_id", ?CLIENT_ID}, {"state", "state"}]).

get_access_token(Code) ->
    ReqParams = [{"client_id", ?CLIENT_ID}, {"client_secret", ?CLIENT_SECRET}, {"code", binary_to_list(Code)}],
    HttpOptions = [{autoredirect, false}],
    case httpc:request(post, {oauth:uri(?ACCESS_TOKEN_URI, ReqParams), [], "", []}, HttpOptions, []) of
        {error, _} -> not_authorized;
        {ok, R = {{"HTTP/1.1",200,"OK"}, _, _}} ->
              Params = oauth:params_decode(R),
              case proplists:get_value("error", Params, undefined) of undefined -> Params; _E -> not_authorized end;
        {ok, _} -> not_authorized end.

api_call(Name, Props) ->
    Token = [{"access_token", proplists:get_value("access_token", Props)}],
    case httpc:request(get, {oauth:uri(?API_URI++Name, Token), ?REQ_HEADER}, [], []) of
         {error, reason} -> api_error;
         {ok, {HttpResponse, _, Body}} -> 
                case HttpResponse of {"HTTP/1.1", 200, "OK"} -> n2o_json:decode(Body); _ -> error end;
         {ok, _} -> api_error end.

sdk() -> [].
callback() ->
    Code = wf:q(<<"code">>),
    State = wf:q(<<"state">>),
    case wf:user() of
         undefined when Code =/= undefined andalso State == <<"state">> ->
            case github:get_access_token(Code) of
                 not_authorized -> skip;
                 Props -> UserData = github:user(Props), avz:login(github, UserData#struct.lst) end;
         _ -> skip end.

registration_data(Props, github, Ori) ->
    Id = proplists:get_value(<<"id">>, Props),
    Name = proplists:get_value(<<"name">>, Props),
    Email = email_prop(Props, github),
    Ori#user{   id= Email,
                username = binary_to_list(proplists:get_value(<<"login">>, Props)),
                display_name = Name,
                images = userhelper:updateProplist({gh_avatar,proplists:get_value(<<"avatar_url">>, Props)},Ori#user.images),
                email = Email,
                names  = Name,
                surnames = [],
                tokens = userhelper:updateProplist({github,Id},Ori#user.tokens),
                register_date = erlang:now(),
                status = ok }.

email_prop(Props, github) ->
    Mail = proplists:get_value(<<"email">>, Props),
    L = wf:to_list(Mail),
    case avz_validator:is_email(L) of
        true -> L;
        false -> binary_to_list(proplists:get_value(<<"login">>, Props)) ++ "@github"
    end.

login_button() -> #panel{ class=["btn-group"], body=
    #link{id=github_btn, class=[btn, "btn-large"], 
        body=[#i{class=["icon-github", "icon-large"]}, <<"Github">>], postback={github,logingithub} }}.

api_event(_,_,_) -> ok.
event({github,logingithub}) -> wf:redirect(github:authorize_url()).
