-module(env_loader).

-export([load/0, get/1]).

-define(ENV_FILE, ".env").

load() ->
  case ets:info(env_vars) of
    undefined ->
      ets:new(env_vars, [named_table, set, public]);
    _ ->
      ok
  end,
  {ok, Bin} = file:read_file(?ENV_FILE),
  Lines = string:split(binary_to_list(Bin), "\n", all),
  lists:foreach(fun parse_line/1, Lines),
  ok.

parse_line(Line) ->
  case string:trim(Line) of
    "" ->
      ok;
    [$# | _] ->
      ok;
    L ->
      case string:split(L, "=", leading) of
        [Key, Val] ->
          ets:insert(env_vars, {Key, Val});
        _ ->
          ok
      end
  end.

get(Key) when is_list(Key) ->
  case ets:lookup(env_vars, Key) of
    [{_, Val}] ->
      Val;
    [] ->
      undefined
  end.
