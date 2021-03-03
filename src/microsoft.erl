-module(microsoft).
-author('Maxim Sokhatsku').
-include("avz.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/n2o.hrl").
-compile(export_all).
-export(?API).

-define(OAUTH_URI,      "https://login.microsoftonline.com/common/oauth2/v2.0").
-define(API_URI,        "https://graph.microsoft.com/v1.0/").
-define(AUTHORIZE,      ?OAUTH_URI ++ "/authorize").
-define(ACCESS_TOKEN,   ?OAUTH_URI ++ "/token").
-define(OAUTH_REDIRECT, application:get_env(avz, ms_redirect_uri, [])).
-define(CLIENT_ID,      application:get_env(avz, ms_client_id, [])).
-define(CLIENT_SECRET,  application:get_env(avz, ms_client_secret, [])).
-define(SCOPE,          "https://graph.microsoft.com/user.read"). % only one working scope so far 

% ms doesn't support sslv3
-define(HTTP_OPTS, [{ssl, [{versions,['tlsv1.2']}]}]).

-define(ATTS, #{email => <<"userPrincipalName">>}).

authorize_url() -> 
  Params = [{"client_id", ?CLIENT_ID},{"redirect_uri", ?OAUTH_REDIRECT},{"response_type", "code"},{"scope", ?SCOPE}],
  oauth:uri(?AUTHORIZE, Params).

api_event(_, Args, _)->
    JSArgs = string:tokens(Args,"\\\\"),
    J = string:join(JSArgs,""),
    [_|K1] = J,
    [_|K2] = lists:reverse(K1),
    K = lists:reverse(K2),
    {D} = ?AVZ_JSON:decode(list_to_binary(K)),
    avz:login(microsoft, D).

registration_data(Props, microsoft, Ori)->
  #{<<"id">>:=Id, <<"givenName">>:=GivenName, <<"surname">>:=FamilyName, <<"displayName">>:=DisplayName, <<"gender">>:=Gender} = Props,
  #{tokens:=Tokens} = Ori,
    Email = email_prop(Props,microsoft),
  maps:merge(Ori, #{   display_name => DisplayName,
                email => Email,
                names => GivenName,
                surnames => FamilyName,
                tokens => avz:update({microsoft,Id},Tokens),
                register_date => os:timestamp(),
                sex => Gender,
                status => ok }).

index(K) -> maps:get(K, ?ATTS, nitro:to_binary(K)).
email_prop(Props, _) -> maps:get(maps:get(email,?ATTS), Props).

login_button() -> 
  #link{id=microsoftbtn, body=[<<"Microsoft">>],postback={microsoft, login}}.

get_access_token(Code) ->
  Params = [
    {grant_type, "authorization_code"},
    {code, binary_to_list(Code)},
    {redirect_uri, ?OAUTH_REDIRECT},
    {client_id, ?CLIENT_ID},
    {client_secret, ?CLIENT_SECRET}
  ],
  Req = {?ACCESS_TOKEN, [], "application/x-www-form-urlencoded", oauth:uri_params_encode(Params)},

  case httpc:request(post, Req, ?HTTP_OPTS, []) of 
    {ok, {{"HTTP/1.1",200,"OK"},_, B1}} ->
      ?AVZ_JSON:decode(list_to_binary(B1), [{object_format, proplist}]);
    {error, E} -> avz:event({error, nitro:jse(E)}), not_authorized;
    {ok, {{"HTTP/1.1",_,_}, _, B}} ->
      Fail = ?AVZ_JSON:decode(list_to_binary(B), [{object_format, proplist}]),
      case proplists:get_value(<<"error">>, Fail, undefined) of undefined -> Fail;
        _E ->
          Desc = proplists:get_value(<<"error_description">>, Fail, undefined),
          avz:event({error, nitro:jse(Desc)}),
          not_authorized end end.

api_call(Name, Props) ->
  Token = proplists:get_value(<<"access_token">>, Props, undefined),
  TokenType = proplists:get_value(<<"token_type">>, Props, undefined),
  Authorization = [{"Authorization", string:join([nitro:to_list(TokenType),nitro:to_list(Token)]," ") }],
  Req = {?API_URI ++ Name, Authorization},

  case httpc:request(get, Req, ?HTTP_OPTS, []) of 
    {ok, {{"HTTP/1.1",200,"OK"},_Hh,Bh}} ->
      {Usr} = ?AVZ_JSON:decode(list_to_binary(Bh)), Usr;
    {ok, {{"HTTP/1.1",_,_},_,B}} ->
      {Err} = ?AVZ_JSON:decode(list_to_binary(B)), avz:event({error, nitro:jse(Err)}), api_error;
    {error, Err} -> avz:event({error, nitro:jse(Err)}), api_error
  end.

callback() -> 
  Code = nitro:q(<<"code">>),
  case n2o:user() of undefined when Code =/= undefined ->
    case get_access_token(Code) of 
      not_authorized -> skip;
      Props -> UserData = api_call("me/", Props), avz:login(microsoft, UserData)
    end;
  _ -> skip end.
sdk() -> [].
event({microsoft, _}) -> nitro:redirect(microsoft:authorize_url()).
