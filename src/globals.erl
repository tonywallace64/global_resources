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
-export([lookup/0,lookup/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

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
    {ok,BinTerms} = file:read_file("globals.config.etf"),
    [State] = binary_to_term(BinTerms),
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


