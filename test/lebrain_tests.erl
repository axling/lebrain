-module(lebrain_tests).

-include_lib("eunit/include/eunit.hrl").

app_started_test() ->
    ok = application:start(lebrain),
    ?assertNot(undefined == whereis(lebrain_sup)),
    ok = application:stop(lebrain).

neuron_spawned_test() ->
    ok = application:start(lebrain),
    Pid = lebrain_neuron:spawn_link(hidden),
    ?assert(true == is_process_alive(Pid)),
    exit(Pid, normal),
    ok = application:stop(lebrain).

neuron_recv_expected_input_test() ->
    Self = self(),
    Pid = lebrain_neuron:spawn_link(hidden),
    Pid ! {add_neurons, {[Self], [Self]}},
    Pid ! {input, Self, 1.0},
    receive
	{input, Pid, 0.5} ->
	    ok
    end,
    exit(Pid, normal).

output_neuron_recv_expected_learning_test() ->
    Self = self(),
    Pid = lebrain_neuron:spawn_link(output),
    Pid ! {add_neurons, {[Self], [Self]}},
    Pid ! {input, Self, 1.0},
    receive
	{input, Pid, 0.5} ->
	    ok
    end,
    Pid ! {learning, self(), 0.4},
    Sensitivity = (0.4-0.5)*0.5,
    Weight = 0.5 + 0.005*Sensitivity*1.0,
    Expected = Sensitivity*Weight,
    receive
	{learning, Pid, Expected} ->
	    ok
    end,
    exit(Pid, normal).

hidden_neuron_recv_expected_learning_test() ->
    Self = self(),
    Pid = lebrain_neuron:spawn_link(output),
    Pid ! {add_neurons, {[Self], [Self]}},
    Pid ! {input, Self, 1.0},
    receive
	{input, Pid, 0.5} ->
	    ok
    end,
    Pid ! {learning, self(), 0.4},
    Sensitivity = (0.4-0.5)*0.5,
    Weight = 0.5 + 0.005*Sensitivity*1.0,
    Expected = Sensitivity*Weight,
    receive
	{learning, Pid, Expected} ->
	    ok
    end,
    exit(Pid, normal).

fail_when_recv_from_unkown_neuron_test() ->	    
    Pid = lebrain_neuron:spawn_link(output),
    Pid ! {add_neurons, {[], []}},
    unlink(Pid),
    MonRef = erlang:monitor(process, Pid),
    Pid ! {input, self(), 1.0},
    receive
	{'DOWN', MonRef, process, Pid,
	 {received_msg_from_unexpected_neuron, _}}->
	    ok
    end.

input_neuron_just_passes_value_test() ->
    Pid = lebrain_neuron:spawn_link(input),
    Pid ! {add_neurons, [self()]},
    Pid ! {input, self(), 0.67},
    receive
	{input, Pid, 0.67} ->
	    ok
    end,
    Pid ! {learning, self(), 0.543},
    Pid ! {input, self(), 0.69},
    receive
	{input, Pid, 0.69} ->
	    ok
    end,
    exit(Pid, normal).
    
