-module(stateful_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SERVER, stateful).

start_test_() ->
  {setup,
   fun() -> stateful:start() end,
   fun({_Response, Pid}) ->
      gen_server:call(Pid, stop),
      ok
   end,
   fun({Response, Pid}) ->
      {inorder, [?_assertEqual(ok, Response), ?_assertEqual([], sys:get_state(Pid))]}
   end}.

stop_test_() ->
  {setup,
   fun() -> gen_server:start({local, ?SERVER}, stateful, [], []) end,
   fun({_Response, _Pid}) -> ok end,
   fun({_Response, Pid}) ->
      {inorder,
       [?_assertEqual([], sys:get_state(Pid)),
        ?_assertEqual(server_stopped, stateful:stop()),
        ?_assertExit({noproc, {gen_server, call, [?SERVER, stop]}}, stateful:stop())]}
   end}.

add_test_() ->
  {setup,
   fun() ->
      {_, Pid} = gen_server:start({local, ?SERVER}, stateful, [], []),
      Pid
   end,
   fun(Pid) -> gen_server:call(Pid, stop) end,
   fun(Pid) ->
      {inorder,
       [?_assertEqual(ok, stateful:add(salt)),
        ?_assertEqual([salt], sys:get_state(Pid)),
        ?_assertEqual(ok, stateful:add(pepper)),
        ?_assertEqual([pepper, salt], sys:get_state(Pid))]}
   end}.

list_test_() ->
  {setup,
   fun() ->
      {_, Pid_empty} = gen_server:start({local, empty_test}, stateful, [], []),
      {_, Pid_full} =
        gen_server:start({local, full_test}, stateful, [salt, pepper, oil, chips], []),
      {Pid_empty, Pid_full}
   end,
   fun({Pid_empty, Pid_full}) ->
      gen_server:call(Pid_empty, stop),
      gen_server:call(Pid_full, stop)
   end,
   fun({Pid_empty, Pid_full}) ->
      {inorder,
       [?_assertEqual([], gen_server:call(Pid_empty, list)),
        ?_assertEqual([salt, pepper, oil, chips], gen_server:call(Pid_full, list))]}
   end}.

remove_test_() ->
  {setup,
   fun() ->
      {_, Pid} = gen_server:start({local, ?SERVER}, stateful, [salt, pepper, oil, chips], []),
      Pid
   end,
   fun(Pid) -> gen_server:call(Pid, stop) end,
   fun(Pid) ->
      {inorder,
       [?_assertEqual(ok, stateful:remove(salt)),
        ?_assertEqual([pepper, oil, chips], sys:get_state(Pid)),
        ?_assertEqual(ok, stateful:remove(pepper)),
        ?_assertEqual([oil, chips], sys:get_state(Pid)),
        ?_assertEqual(ok, stateful:remove(oil)),
        ?_assertEqual([chips], sys:get_state(Pid)),
        ?_assertEqual(ok, stateful:remove(chips)),
        ?_assertEqual([], sys:get_state(Pid))]}
   end}.

remove_all_test_() ->
  {setup,
   fun() ->
      {_, Pid} = gen_server:start({local, ?SERVER}, stateful, [salt, pepper, oil, chips], []),
      Pid
   end,
   fun(Pid) -> gen_server:call(Pid, stop) end,
   fun(Pid) ->
      {inorder,
       [?_assertEqual([salt, pepper, oil, chips], sys:get_state(Pid)),
        ?_assertEqual(ok, stateful:remove_all()),
        ?_assertEqual([], sys:get_state(Pid))]}
   end}.
