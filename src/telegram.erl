-module(telegram).
-include("avz.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/n2o.hrl").
-include("proto.hrl").
-export([info/3]).

env(N,D) -> nitro:to_list(application:get_env(avz,N,D)).

info(#tlg{cid=Cid, cmd=init},R,Ctx) ->
    {reply,{bert,nitro_n2o:io({init,Cid},Ctx)},R,Ctx};

info(#tlg{cid=Cid, cmd=login, pld=Args},R,Ctx) ->
    #{<<"hash">> := Hash} = M = ?N2O_JSON:decode(Args, [{keys, 'binary'}]),
    M1 = maps:filter(fun(<<"hash">>,_) -> false; (_,_) -> true end, M),
    Rec = lists:sort(maps:to_list(M1)),
    Data = iolist_to_binary(lists:join(<<"\n">>, [unicode:characters_to_nfkc_binary([K, <<"=">>, nitro:to_list(V)]) || {K, V} <- Rec])),
    Token = crypto:hash(sha256, env(tl_token,[])),

    {Ev,Msg} = case crypto:mac(hmac, sha256, Token, Data) of <<X:256>> ->
        case list_to_binary(io_lib:format("~64.16.0b", [X])) of 
          Hash ->
            case nitro_n2o:io({login, Cid, M}, Ctx) of
                {io,<<>>,{error,E}} -> {error,E};
                {io,<<>>,{stack,S}} -> {error, protocol};
                {io,Act,_}->
                  nitro:wire(Act),
                  case n2o:user() of [] -> {error, fail};_->{login, ok} end
              end;
          _ -> {error, hash}
        end;
        _ -> {error,token}
    end,

    {reply, {bert, nitro_n2o:io(nitro:wire(
      #jq{target=Cid,
        method=["dispatchEvent"],
        args=[nitro:f("new CustomEvent('~s', {detail: {data:() => '~s'}})", [Ev,Msg])]}
      ))},R,Ctx};

info(M,R,S) -> {unknown, M, R, S}.
