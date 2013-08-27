-module(avz_validator).
-export([is_email/1]).

is_email(Value) when is_list(Value) orelse is_binary(Value) ->
    case re:run(Value, "^[a-zA-Z0-9!#$%&'*\+-/=\?^_`\.{|}~]+@[a-zA-Z0-9][a-zA-Z0-9\.-]+\.[a-zA-Z]+$") of
        {match, _} -> true;
        _ -> false
    end;
is_email(_) -> false.
