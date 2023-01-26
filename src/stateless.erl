%% @author Lee Barney
%% @copyright 2022 Lee Barney licensed under the <a>
%%        rel="license"
%%        href="http://creativecommons.org/licenses/by/4.0/"
%%        target="_blank">
%%        Creative Commons Attribution 4.0 International License</a>
%%
%%
-module(stateless).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start/0, stop/0, add/2, subtract/2, divide/2, multiply/2]).
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
  gen_server:call(?MODULE, stop).

add(N1, N2) ->
  gen_server:call(?MODULE, {add, N1, N2}).

subtract(N1, N2) ->
  gen_server:call(?MODULE, {subtract, N1, N2}).

divide(N1, N2) ->
  gen_server:call(?MODULE, {divide, N1, N2}).

multiply(N1, N2) ->
  gen_server:call(?MODULE, {multiply, N1, N2}).

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
init([]) ->
  {ok, up}.

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
handle_call(stop, _From, _State) ->
  {stop,
   normal,
   server_stopped,
   down}; %% setting the server's internal state to down
handle_call({add, N1, N2}, _From, State) ->
  {reply, N1 + N2, State};
handle_call({subtract, N1, N2}, _From, State) ->
  {reply, N1 - N2, State};
handle_call({divide, _N1, 0}, _From, State) ->
  {reply, divide_by_zero_error, State};
handle_call({divide, N1, N2}, _From, State) ->
  {reply, N1 / N2, State};
handle_call({multiply, N1, N2}, _From, State) ->
  {reply, N1 * N2, State}.

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
