%%%-------------------------------------------------------------------
%%% @author tony <tony@faith>
%%% @copyright (C) 2016, tony
%%% @doc
%%%
%%% @end
%%% Created : 21 Apr 2016 by tony <tony@faith>
%%%-------------------------------------------------------------------
-module(globals).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([lookup/0,lookup/1,interval_timer/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CONFIG_CHECK_INTERVAL,10000).
-define(CONFIG_FILE,"globals.config.etf").
-define(CONFIG_FILE_NEW,"globals.config.etf.new").


%-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

lookup(X={_,'_',_}) ->
    gen_server:call({local, ?SERVER}, {lookup,X});
lookup(X={_,_,'_'}) ->
    gen_server:call({local, ?SERVER}, {lookup,X}).
lookup() ->
    gen_server:call({local, ?SERVER}, lookup).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    State = read_config(?CONFIG_FILE),
    start_interval_timer(),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call(lookup, _From, State) ->
    Reply = lists:usort([X || {X,_,_} <- State]),
    {reply, Reply, State};



handle_call({lookup,X={_,_,_}}, _From, State) ->
    Reply = lookup(X,[],State),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(check_for_new_config,State) ->
    NewState = maybe_new_config(file:read_file(?CONFIG_FILE_NEW),State),
    {noreply, NewState};
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
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("Global ~p terminating ~n",[self()]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

lookup({'_','_','_'},_,State) ->
    State;
lookup(Test={Key,'_','_'},Acc,[X={Key,_,_}|T]) ->
    lookup(Test,[X|Acc],T);
lookup(Test={Key,'_',Value},Acc,[X={Key,_,Value}|T]) ->
    lookup(Test,[X|Acc],T);
lookup(Test={Key,Value,'_'},Acc,[X={Key,Value,_}|T]) ->
    lookup(Test,[X|Acc],T);
lookup(Test,Acc,[_|T]) ->
    lookup(Test,Acc,T);
lookup(_,Acc,[]) ->
    Acc.

%% for this server it is acceptable that
%% configuration file does not exist.
%% It can be deployed at any time, including
%% after the server has been started.
read_config(File) ->
    maybe_read(file:read_file(File)).

maybe_read({ok,BinTerms}) ->
    [State] = binary_to_term(BinTerms),
    State;
maybe_read({error,enoent}) ->
    [].

start_interval_timer() ->
    spawn_link(?MODULE,interval_timer,[self()]).

interval_timer(GenServerPid) ->
    receive
	_ ->
	    ok
    after ?CONFIG_CHECK_INTERVAL ->
	    GenServerPid ! check_for_new_config
    end,
    interval_timer(GenServerPid).


maybe_new_config(X,State) ->
    maybe_new_state(maybe_read(X),State).

maybe_new_state([],State) ->
    State;
maybe_new_state(NewState,_) ->
    %% move new configuration to existing
    %% then return NewState
    ok=file:delete(?CONFIG_FILE),
    ok=file:rename(?CONFIG_FILE_NEW,?CONFIG_FILE),
    NewState.
