%% @author Lee Barney
%% @copyright 2022 Lee Barney licensed under the <a>
%%        rel="license"
%%        href="http://creativecommons.org/licenses/by/4.0/"
%%        target="_blank">
%%        Creative Commons Attribution 4.0 International License</a>
%%
%%
-module(stateful).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start/0, stop/0, add/1, list/0, remove/1, remove_all/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server assuming there is only one server started for
%% this module. The server is registered locally with the registered
%% name being the name of the module.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> {ok, pid()} | ignore | {error, term()}.
start() ->
  gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> {ok} | {error, term()}.
stop() ->
  gen_server:call(?SERVER, stop).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
add(Item) ->
  gen_server:call(?SERVER, {add, Item}).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
list() ->
  gen_server:call(?SERVER, list).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
remove(Item) ->
  gen_server:call(?SERVER, {remove, Item}).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
remove_all() ->
  gen_server:call(?SERVER, remove_all).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init(term()) -> {ok, term()} | {ok, term(), number()} | ignore | {stop, term()}.
init(State) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: pid(), State :: term()) ->
                   {reply, term(), term()} |
                   {reply, term(), term(), integer()} |
                   {noreply, term()} |
                   {noreply, term(), integer()} |
                   {stop, term(), term(), integer()} |
                   {stop, term(), term()}.
%% setting the server's internal state to down
handle_call(stop, _From, _State) ->
  {stop, normal, server_stopped, down};
handle_call({add, Item}, _From, List) ->
  {reply, ok, [Item] ++ List};
handle_call({remove, Item}, _From, List) ->
  {reply, ok, lists:delete(Item, List)};
handle_call(list, _From, List) ->
  {reply, List, List};
handle_call(remove_all, _From, _List) ->
  {reply, ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg :: term(), State :: term()) ->
                   {noreply, term()} | {noreply, term(), integer()} | {stop, term(), term()}.
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
-spec handle_info(Info :: term(), State :: term()) ->
                   {noreply, term()} | {noreply, term(), integer()} | {stop, term(), term()}.
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), term()) -> term().
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), term(), term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-ifdef(EUNIT).

-endif.
