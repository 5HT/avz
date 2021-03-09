-module(email).
-author('Andrii Zadorozhnii').
-description("").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/n2o.hrl").
-include("proto.hrl").

-export([info/3]).

info(#eml{cid=Cid, cmd=init},R,Ctx) ->
  {reply,{bert,nitro_n2o:io({init,Cid},Ctx)},R,Ctx};

info(#eml{cid=Cid, cmd=logout}, R,Ctx) ->
  n2o:user([]),
  n2o_session:delete({n2o:sid(),token}),
  {reply,{bert,nitro_n2o:io({logout,Cid},Ctx)},R,Ctx};

info(#eml{cid=Cid, cmd=login, pld=Args},R,#cx{}=Ctx) ->
  Props = maps:update_with(pass, fun avz:sha/1, [], maps:from_list(Args)),
  {Ev, Msg} = case nitro_n2o:io({login, Cid, Props},Ctx) of
    {io,<<>>,{error,E}} -> {error,E};
    {io,<<>>,{stack,S}} -> {error, protocol};
    _ -> case n2o:user() of [] -> {error, fail}; _ -> {login, ok} end end,

  {reply, {bert, nitro_n2o:io(nitro:wire(
    #jq{target=Cid,
        method=["dispatchEvent"],
        args=[nitro:f("new CustomEvent('~s', {detail: {data:() => '~s'}})", [Ev,Msg])]}
    ))},R,Ctx};

info(#eml{}=M,R,S) -> {unknown,M,R,S}.
