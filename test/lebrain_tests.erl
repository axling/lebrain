-module(lebrain_tests).

-include_lib("eunit/include/eunit.hrl").

app_started_test() ->
    ok = application:start(lebrain),
    ?assertNot(undefined == whereis(lebrain_sup)),
    ok = application:stop(lebrain).
