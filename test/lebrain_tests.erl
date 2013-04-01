-module(lebrain_tests).

-include_lib("eunit/include/eunit.hrl").

app_started_test() ->
    ok = application:start(lebrain),
    ?assertNot(undefined == whereis(lebrain_sup)),
    ok = application:stop(lebrain).

neuron_spawned_test() ->
    ok = application:start(lebrain),
    Pid = lebrain_neuron:spawn_link(hidden, [self()], [self()]),
    ?assert(true == is_process_alive(Pid)),
    exit(Pid, normal),
    ok = application:stop(lebrain).

neuron_recv_expected_input_test() ->
    Self = self(),
    Pid = lebrain_neuron:spawn_link(hidden, [Self],[Self]),
    Pid ! {input, Self, 1.0},
    receive
	{input, Pid, 0.5} ->
	    ok
    end,
    exit(Pid, normal).

neuron_recv_expected_learning_test() ->
    Self = self(),
    Pid = lebrain_neuron:spawn_link(output, [Self],[Self]),
    Pid ! {input, Self, 1.0},
    receive
	{input, Pid, 0.5} ->
	    ok
    end,
    Pid ! {learning, self(), 0.4},
    receive
	{learning, Pid, _Val} ->
	    ok
    end,
    exit(Pid, normal).
	    
