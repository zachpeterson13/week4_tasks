-module(stateless_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SERVER, stateless).

start_test_() ->
  {setup,
   fun() -> stateless:start() end,
   fun({_Response, Pid}) ->
      gen_server:call(Pid, stop),
      ok
   end,
   fun({Response, Pid}) ->
      {inorder, [?_assertEqual(ok, Response), ?_assertEqual(up, sys:get_state(Pid))]}
   end}.

stop_test_() ->
  {setup,
   fun() -> gen_server:start({local, ?SERVER}, stateless, [], []) end,
   fun({_Response, _Pid}) -> ok end,
   fun({_Response, Pid}) ->
      {inorder,
       [?_assertEqual(up, sys:get_state(Pid)),
        ?_assertEqual(server_stopped, stateless:stop()),
        ?_assertExit({noproc, {gen_server, call, [?SERVER, stop]}}, stateless:stop())]}
   end}.

add_test_() ->
  {setup,
   fun() -> stateless:start() end,
   fun({_Response, Pid}) ->
      gen_server:call(Pid, stop),
      ok
   end,
   fun(_) ->
      {spawn,
       [?_assertEqual(4, stateless:add(2, 2)),
        ?_assertEqual(0, stateless:add(-2, 2)),
        ?_assertEqual(100, stateless:add(50, 50)),
        ?_assertEqual(0, stateless:add(0, 0)),
        ?_assertEqual(-100, stateless:add(-50, -50))]}
   end}.

subtract_test_() ->
  {setup,
   fun() -> stateless:start() end,
   fun({_Response, Pid}) ->
      gen_server:call(Pid, stop),
      ok
   end,
   fun(_) ->
      {spawn,
       [?_assertEqual(0, stateless:subtract(2, 2)),
        ?_assertEqual(-4, stateless:subtract(-2, 2)),
        ?_assertEqual(0, stateless:subtract(50, 50)),
        ?_assertEqual(0, stateless:subtract(0, 0)),
        ?_assertEqual(0, stateless:subtract(-50, -50)),
        ?_assertEqual(20, stateless:subtract(50, 30))]}
   end}.

divide_test_() ->
  {setup,
   fun() -> stateless:start() end,
   fun({_Response, Pid}) ->
      gen_server:call(Pid, stop),
      ok
   end,
   fun(_) ->
      {spawn,
       [?_assertEqual(1.0, stateless:divide(2, 2)),
        ?_assertEqual(-1.0, stateless:divide(-2, 2)),
        ?_assertEqual(2.0, stateless:divide(50, 25)),
        ?_assertEqual(divide_by_zero_error, stateless:divide(10, 0)),
        ?_assertEqual(5.0, stateless:divide(50, 10))]}
   end}.

multiply_test_() ->
  {setup,
   fun() -> stateless:start() end,
   fun({_Response, Pid}) ->
      gen_server:call(Pid, stop),
      ok
   end,
   fun(_) ->
      {spawn,
       [?_assertEqual(4, stateless:multiply(2, 2)),
        ?_assertEqual(-4, stateless:multiply(-2, 2)),
        ?_assertEqual(1250, stateless:multiply(50, 25)),
        ?_assertEqual(0, stateless:multiply(10, 0))]}
   end}.
