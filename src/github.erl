-module(github).
-author('Andrii Zadorozhnii').
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/n2o.hrl").
-include("avz.hrl").
-compile(export_all).
-export(?API).

-define(CLIENT_ID,        application:get_env(avz, github_client_id,     [])).
-define(CLIENT_SECRET,    application:get_env(avz, github_client_secret, [])).
-define(OAUTH_URI,        "https://github.com/login/oauth").
-define(AUTHORIZE_URI,    ?OAUTH_URI ++ "/authorize").
-define(ACCESS_TOKEN_URI, ?OAUTH_URI ++ "/access_token").
-define(API_URI,          "https://api.github.com").
-define(REQ_HEADER,       [{"User-Agent", "Erlang PaaS"}]).

user(Props) -> api_call("/user", Props).

authorize_url() -> oauth:uri(?AUTHORIZE_URI, [{"client_id", ?CLIENT_ID}, {"state", "state"}]).

get_access_token(Code) ->
    ReqParams = [{"client_id", ?CLIENT_ID}, {"client_secret", ?CLIENT_SECRET}, {"code", binary_to_list(Code)}],
    HttpOptions = [{autoredirect, false}],
    case httpc:request(post, {oauth:uri(?ACCESS_TOKEN_URI, ReqParams), [], "", []}, HttpOptions, []) of
        {error, _} -> not_authorized;
        {ok, {{"HTTP/1.1",200,"OK"}, Headers, Body}} ->
          Params = lists:append(Headers,
            [list_to_tuple(string:tokens(P1, "=")) || P1 <- string:tokens(Body, "&")]),
          case proplists:get_value(<<"error">>, Params, undefined) of undefined -> Params; _E -> not_authorized end;
        {ok, _} -> not_authorized end.

api_call(Name, Props) ->
    Token = [{"access_token", proplists:get_value("access_token", Props)}],
    case httpc:request(get, {oauth:uri(?API_URI++Name, Token), ?REQ_HEADER}, [], []) of
         {error, reason} -> api_error;
         {ok, {HttpResponse, _, Body}} -> 
                case HttpResponse of {"HTTP/1.1", 200, "OK"} -> {Res} = ?AVZ_JSON:decode(list_to_binary(Body)), Res; _ -> error end;
         {ok, _} -> api_error end.

sdk() -> [].
callback() ->
    Code = nitro:q(<<"code">>),
    State = nitro:q(<<"state">>),
    case n2o:user() of
         undefined when Code =/= undefined andalso State == <<"state">> ->
            case github:get_access_token(Code) of
                 not_authorized -> skip;
                 Props -> UserData = github:user(Props), avz:login(github, UserData) end;
         _ -> skip end.

registration_data(Props, github, Ori) ->
  #{<<"id">>:=Id,<<"name">>:=Name, <<"login">>:=Login, <<"avatar_url">>:=Avatar} = Props,
  #{images:= Images, tokens:=Tokens} = Ori,
    Email = email_prop(Props, github),
    maps:merge(Ori, #{   username => binary_to_list(Login),
                display_name => Name,
                images => avz:update({gh_avatar,Avatar},Images),
                email => Email,
                names  => Name,
                surnames => [],
                tokens => avz:update({github,Id},Tokens),
                register_date => os:timestamp(),
                status => ok }).

index(K) -> nitro:to_binary(K).
email_prop(Props, github) ->
  #{<<"email">>:=Mail, <<"login">>:=Login} = Props,
    L = nitro:to_list(Mail),
    case avz_validator:is_email(L) of
        true -> Mail;
        false -> binary_to_list(Login) ++ "@github"
    end.

login_button() -> 
  #link{id=github_btn, body=[<<"Github">>], postback={github,logingithub} }.

api_event(_,_,_) -> ok.
event({github,logingithub}) -> nitro:redirect(github:authorize_url()).
