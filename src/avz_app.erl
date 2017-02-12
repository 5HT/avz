-module(avz_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  application:start(asn1),
  application:start(public_key),
  application:start(ssl),
  avz_sup:start_link().
stop(_State) -> ok.
