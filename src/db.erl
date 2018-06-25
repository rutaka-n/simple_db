%%%-------------------------------------------------------------------
%% @doc db public API
%% @end
%%%-------------------------------------------------------------------

-module(db).

-behaviour(application).

%% Application callbacks
-export([start/0, stop/0,  start/2, stop/1]).
%% API
-export([new/1, create/2, read/2, update/2, delete/2]).

%%====================================================================
%% API
%%====================================================================

start() ->
        start([], []).

start(_, _) ->
        db_sup:start_link().

%%--------------------------------------------------------------------
stop() ->
        stop([]).

stop(_State) ->
        ok.

%%--------------------------------------------------------------------
new(Name) ->
        {ok, _} = db_storage:register(Name),
        Name.

create(Record, DBName) ->
        db_storage:insert(Record, DBName).

read(Key, DBName) ->
        db_storage:read(Key, DBName).

update(Record, DBName) ->
        db_storage:update(Record, DBName).

delete(Key, DBName) ->
        db_storage:delete(Key, DBName).

%%====================================================================
%% Internal functions
%%====================================================================
