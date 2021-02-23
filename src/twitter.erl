-module(twitter).
-author('Andrii Zadorozhnii').
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/n2o.hrl").
-include_lib("avz/include/avz.hrl").
-compile(export_all).
-export(?API).
-define(CONSUMER_KEY,    application:get_env(avz, tw_consumer_key,    [])).
-define(CONSUMER_SECRET, application:get_env(avz, tw_consumer_secret, [])).
-define(CONSUMER,        {?CONSUMER_KEY, ?CONSUMER_SECRET, hmac_sha1}).

-define(ATTS, #{email => <<"screen_name">>}).

registration_data(Props, twitter, Ori)->
  #{<<"id_str">>:=Id, <<"screen_name">>:=ScreenName,<<"profile_image_url">>:=Image, <<"name">>:=Name} = Props,
  #{images:=Images, tokens:=Tokens} =Ori,
    UserName = binary_to_list(ScreenName),
    Email = email_prop(Props,twitter),

    maps:merge(Ori, #{   username => re:replace(UserName, "\\.", "_", [{return, list}]),
                display_name => ScreenName,
                images => avz:update({tw_avatar,Image},Images),
                names => Name,
                email => Email,
                surnames => [],
                tokens => avz:update({twitter,Id},Tokens),
                register_date => os:timestamp(),
                status => ok }).

index(K) -> maps:get(K, ?ATTS, nitro:to_binary(K)).
email_prop(Props, twitter) -> maps:get(maps:get(email,?ATTS), Props).

callback() ->
    Token = niro:q(<<"oauth_token">>),
    Verifier = nitro:q(<<"oauth_verifier">>),
    case n2o:user() of
         undefined ->
             if (Token /= undefined) andalso ( Verifier/= undefined) ->
                   case get_access_token(binary_to_list(Token), binary_to_list(Verifier)) of
                        not_authorized -> skip;
                        Props -> UserData = show(Props), avz:login(twitter, UserData) end;
                 true -> skip  end;
         _ -> skip end.

login_button() -> 
  #link{id=twlogin,body=[<<"Twitter">>],postback={twitter,logintwitter}}.

sdk() -> [].
api_event(_,_,_) -> ok.
event({twitter,logintwitter}) ->
    case get_request_token() of
         {RequestToken, _, _} -> nitro:redirect(authenticate_url(RequestToken));
         {error, R} -> io:format("Twitter request failed: ~p~n", [R]), [] end.

get_request_token()->
  URL = "https://api.twitter.com/oauth/request_token",
  case oauth:get(URL, [], ?CONSUMER) of
    {ok, Response} ->
      % json response {"errors":[{"code": Code,"message":Message}]} should be handled here
      Params = oauth:params_decode(Response),
      RequestToken = oauth:token(Params),
      RequestTokenSecret = oauth:token_secret(Params),
      CallbackConfirmed = proplists:get_value("oauth_callback_confirmed", Params),
      {RequestToken, RequestTokenSecret, CallbackConfirmed};
    {error, E}-> {error, E}
  end.

get_access_token(undefined, undefined)-> not_authorized;
get_access_token(undefined, _)-> not_authorized;
get_access_token(_, undefined)-> not_authorized;
get_access_token(Token, Verifier)->
  URL = "https://api.twitter.com/oauth/access_token",
  Signed = oauth:sign("GET", URL, [{"oauth_verifier", Verifier}], ?CONSUMER, Token, ""),
  {OauthParams, QueryParams} = lists:partition(fun({K, _}) -> lists:prefix("oauth_", K) end, Signed),
  Request = {oauth:uri(URL, QueryParams), [oauth:header(OauthParams)]},
  {ok, Response} = httpc:request(get, Request, [{autoredirect, false}], []),
  case Response of
    {HttpResponse, _, _}->
      case HttpResponse of
        {"HTTP/1.1",200,"OK"}->
          Params = oauth:params_decode(Response),
          Params;
        _ -> not_authorized
      end;
    _ -> not_authorized
  end.

authenticate_url(RequestToken)->
    oauth:uri("https://api.twitter.com/oauth/authenticate", [{"oauth_token", RequestToken}]).

authorize_url(RequestToken)->
    oauth:uri("https://api.twitter.com/oauth/authorize", [{"oauth_token", RequestToken}]).

show(Props)->
  URI = "https://api.twitter.com/1.1/users/show.json",
  {ok, Response} = oauth:get(URI, [{"user_id", proplists:get_value("user_id", Props)},
                                   {"include_entities", false}],
                            ?CONSUMER, oauth:token(Props), oauth:token_secret(Props)),
  case Response of
    {HttpResponse, _, Body} -> case HttpResponse of
                                    {"HTTP/1.1", 200, "OK"} ->  {Res} = ?AVZ_JSON:decode(list_to_binary(Body)), Res;
                                    _-> error end;
    _ -> error
  end.

