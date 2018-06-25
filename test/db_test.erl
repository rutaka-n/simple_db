-module(db_test).

-include_lib("eunit/include/eunit.hrl").


crud_test_() ->
        {"test crud for db",
         {setup,
          fun start/0,
          fun stop/1,
          fun crud/1}}.

 %%%%%%%%%%%%%%%%%%%%%%%
 %%% SETUP FUNCTIONS %%%
 %%%%%%%%%%%%%%%%%%%%%%%
 start() ->
         db:start(),
         db:new(test_db),
         test_db.

stop(_) ->
        db:stop().

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
crud(DB) ->
        {Key, _, _} = Record = {42, "Ivan", "Volgograd"},
        Updated = {42, "Ivan", "Kastroma"},
        [?_assertEqual({ok, Record}, db:create(Record, DB)),
         ?_assertEqual({ok, Record}, db:read(Key, DB)),
         ?_assertEqual({ok, Updated}, db:update(Updated, DB)),
         ?_assertEqual(ok, db:delete(Key, DB))].
