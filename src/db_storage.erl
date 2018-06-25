%%%-------------------------------------------------------------------
%% @doc storage module for db
%%      manages dabases
%%      implements internal data-access functions
%% @end
%%%-------------------------------------------------------------------
-module(db_storage).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, register/1, unregister/1, whereis/1, get_names/0, insert/2, read/2, update/2, delete/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
        gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
        gen_server:call(?MODULE, stop).

register(Name) ->
        gen_server:call(?MODULE, {register, Name}).

unregister(Name) ->
        gen_server:call(?MODULE, {unregister, Name}).

whereis(Name) ->
        gen_server:call(?MODULE, {whereis, Name}).

get_names() ->
        gen_server:call(?MODULE, get_names).
insert(Record, Name) ->
        gen_server:call(?MODULE, {insert, Record, Name}).

update(Record, Name) ->
        gen_server:call(?MODULE, {update, Record, Name}).

read(Key, Name) ->
        gen_server:call(?MODULE, {read, Key, Name}).

delete(Key, Name) ->
        gen_server:call(?MODULE, {delete, Key, Name}).

init([]) -> {ok, dict:new()}.

handle_call({read, Key, Name}, _From, RegisteredNames) ->
        Reply = with_db(Name, RegisteredNames, fun(DB) -> do_read(DB, Key) end),
        {reply, Reply, RegisteredNames};
handle_call({insert, Record, Name}, _From, RegisteredNames) ->
        Reply = with_db(Name, RegisteredNames, fun(DB) -> do_insert(DB, Record) end),
        {reply, Reply, RegisteredNames};
handle_call({delete, Key, Name}, _From, RegisteredNames) ->
        Reply = with_db(Name, RegisteredNames, fun(DB) -> do_delete(DB, Key) end),
        {reply, Reply, RegisteredNames};
handle_call({update, Record, Name}, _From, RegisteredNames) ->
        Reply = with_db(Name, RegisteredNames, fun(DB) -> do_update(DB, Record) end),
        {reply, Reply, RegisteredNames};
handle_call(get_names, _From, RegisteredNames) ->
        {reply, dict:fetch_keys(RegisteredNames), RegisteredNames};
handle_call({unregister, Name}, _From, RegisteredNames) ->
        {reply, ok, dict:erase(Name, RegisteredNames)};
handle_call({register, Name}, _From, RegisteredNames) ->
        case dict:is_key(Name, RegisteredNames) of
                false ->
                        DB = ets:new(Name, []),
                        UpdatedRegisteredNames = dict:store(Name, DB, RegisteredNames),
                        {reply, {ok, DB}, UpdatedRegisteredNames};
                true -> {reply, {error, "Name already used"}, RegisteredNames}
        end;
handle_call({whereis, Name}, _From, RegisteredNames) ->
        Reply = case dict:find(Name, RegisteredNames) of
                        error -> {error, "There are not any DB with this name"};
                        Res -> Res
                end,
        {reply, Reply, RegisteredNames};
handle_call(_Event, _From, State) -> {noreply, State}.

handle_cast(_Event, State) -> {noreply, State}.
handle_info(_Event, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.

%%====================================================================
%% Internal functions
%%====================================================================

with_db(Name, RegisteredNames, Fun) ->
        case dict:find(Name, RegisteredNames) of
                error -> {error, "There are not any DB with this name"};
                {ok, DB} -> Fun(DB)
        end.

do_insert(DB, Record) ->
        case ets:insert_new(DB, Record) of
                false -> {error, "Key already taken"};
                true -> {ok, Record}
        end.

do_update(DB, {Key, _, _} = Record) ->
        case ets:member(DB, Key) of
                false -> {error, "Key does not exists"};
                true ->
                        ets:insert(DB, Record),
                        {ok, Record}
        end.

do_delete(DB, Key) ->
        case ets:member(DB, Key) of
                false -> {error, "Key does not exists"};
                true ->
                        ets:delete(DB, Key),
                        ok
        end.

do_read(DB, Key) ->
        case ets:lookup(DB, Key) of
                [Record] -> {ok, Record};
                _ -> {error, "Key does not exists"}
        end.
