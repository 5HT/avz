-module(avz_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-compile(export_all).
-include_lib ("avz/include/avz.hrl").

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
init([]) -> {ok, {{one_for_one, 5, 10}, []}}.

