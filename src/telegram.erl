-module(telegram).
-include("avz.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/n2o.hrl").
-compile(export_all).
-export(?API).

-define(TL_BOT_NAME,    application:get_env(avz, tl_bot, [])).
-define(TL_BOT_TOKEN,   application:get_env(avz, tl_bot_token, [])).
-define(TL_AUTH_URL,    application:get_env(avz, tl_auth_url, [])).
-define(TL_ACCESS,      application:get_env(avz, tl_request_access, "write")).
-define(TL_BTN_SIZE,    application:get_env(avz, tl_btn_size, "large")).
-define(TL_BTN_RADIUS,  application:get_env(avz, tl_btn_radius, "20")).

-define(TL_USER, [<<"id">>, <<"first_name">>, <<"last_name">>,<<"username">>,<<"auth_date">>, <<"photo_url">>]).
-define(ATTS, #{email => <<"id">>}).

api_event(_,_,_) -> ok.

registration_data(Props, telegram, Ori) ->
  #{<<"username">>:=Uname, <<"photo_url">>:=Photo, <<"first_name">>:=FirstName, <<"last_name">>:=LastName} = Props,
  #{tokens:=Tokens, images:=Images} = Ori,
    Id = proplists:get_value(<<"id">>, Props),
    UserName = binary_to_list(Uname),
    Email = email_prop(Props,telegram),
    maps:merge(Ori, #{   username => re:replace(UserName, "\\.", "_", [{return, list}]),
                display_name => Uname,
                images => avz:update({tl_avatar,Photo},Images),
                names => FirstName,
                email => Email,
                surnames => LastName,
                tokens => avz:update({telegram,Id},Tokens),
                register_date => os:timestamp(),
                status => ok }).

index(K) -> maps:get(K, ?ATTS, nitro:to_binary(K)).
email_prop(Props, telegram) -> proplists:get_value(maps:get(email,?ATTS), Props).

login_button() ->
    #dtl{bind_script=false, file="telegram_login", ext="dtl", bindings=[
        {bot,             ?TL_BOT_NAME},
        {size,            ?TL_BTN_SIZE}, 
        {radius,          ?TL_BTN_RADIUS},
        {auth_url,        ?TL_AUTH_URL},
        {request_access,  ?TL_ACCESS} ]}.

event(_) -> ok.
sdk() -> [].

% HMAC-SHA-256 signature of the data-check-string with the SHA256 hash of the bot's token used as a secret key
callback() ->
    Hash = nitro:q(<<"hash">>),

    Rec  = lists:filter(fun({_, undefined}) -> false; (_) -> true end, [ {T, nitro:q(T)} || T <- lists:sort(?TL_USER) ]),
    Data = lists:join(<<"\n">>, [unicode:characters_to_nfkc_binary([K, <<"=">>, V]) || {K, V} <- Rec]),

    case crypto:hmac(sha256, crypto:hash(sha256, ?TL_BOT_TOKEN), Data) of <<X:256/big-unsigned-integer>> ->
        case list_to_binary(lists:flatten(io_lib:format("~64.16.0b", [X]))) of 
          Hash ->
              avz:login(telegram, Rec);
          _ -> skip
        end;
        _ -> skip
    end.
