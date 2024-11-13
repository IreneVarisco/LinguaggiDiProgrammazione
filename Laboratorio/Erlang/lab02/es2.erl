
% ESERCIZIO NON FATTO DA ME

-module(distributed_store).
-behaviour(gen_server).

-export([start_link/0, stop/0, store/2, lookup/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {local_store, remote_nodes}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

store(Tag, Value) ->
    gen_server:call(?MODULE, {store, Tag, Value}).

lookup(Tag) ->
    gen_server:call(?MODULE, {lookup, Tag}).

init([]) ->
    {ok, RemoteNodes} = discover_remote_nodes(),
    {ok, #state{local_store = dict:new(), remote_nodes = RemoteNodes}}.

handle_call({store, Tag, Value}, _From, State = #state{local_store = LocalStore, remote_nodes = RemoteNodes}) ->
    NewLocalStore = dict:store(Tag, Value, LocalStore),
    store_on_remote_nodes(Tag, Value, RemoteNodes),
    {reply, ok, State#state{local_store = NewLocalStore}};
handle_call({lookup, Tag}, _From, State = #state{local_store = LocalStore, remote_nodes = RemoteNodes}) ->
    Values = case dict:find(Tag, LocalStore) of
        {ok, Value} -> [Value];
        error -> lookup_on_remote_nodes(Tag, RemoteNodes)
    end,
    {reply, Values, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

discover_remote_nodes() ->
    % Logica per scoprire gli altri nodi del sistema distribuito
    {ok, [node() || _ <- [1, 2]]}.

store_on_remote_nodes(Tag, Value, RemoteNodes) ->
    lists:foreach(fun(Node) ->
        gen_server:call({?MODULE, Node}, {store, Tag, Value})
    end, RemoteNodes).

lookup_on_remote_nodes(Tag, RemoteNodes) ->
    lists:flatmap(fun(Node) ->
        gen_server:call({?MODULE, Node}, {lookup, Tag})
    end, RemoteNodes).