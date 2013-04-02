-module(lebrain_neuron).

%% exports for plain_fsm
-export([spawn_link/1]).

-export([data_vsn/0, code_change/3]).

-export([learning/1, input/1]).

-behaviour(plain_fsm).

-record(state, {
	  type :: hidden | output,
	  neurons,
	  last_output,
	  expected_neurons,
	  current_sum=0,
	  output_neurons,
	  input_neurons,
	  learning_rate=0.005
	 }).

-record(neuron, {
	  weight=0.5,
	  last_input
	 }).

data_vsn() ->
    0.

spawn_link(Type) ->
    plain_fsm:spawn_link(?MODULE,
			fun() ->
				process_flag(trap_exit, true),
				wait_for_connections(
				  #state{type=Type})
			end).

wait_for_connections(#state{type=Type}=State) ->
    receive
	{add_neurons, {InputNeurons, OutputNeurons}}
	  when Type =/= input ->
	    input(init_state(State, InputNeurons, OutputNeurons));
	{add_neurons, OutputNeurons} when Type == input ->
	    input(State#state{output_neurons=OutputNeurons})
    end.
	    

init_state(State, InputNeurons, OutputNeurons) ->
    Neurons0 = lists:foldl(
		fun(Pid, Neurons) when is_pid(Pid) ->
			store_neuron(Pid, #neuron{}, Neurons)
		end, dict:new(), InputNeurons ++ OutputNeurons),
    init_input_state(
      State#state{neurons=Neurons0, input_neurons=InputNeurons,
		  output_neurons=OutputNeurons}).

input(#state{type=Type}=State) ->
    receive
	{input, _NeuronPid, Value} when Type == input ->
	    %% If input node, jsut pass on value to all output neurons
	    lists:foreach(fun(Pid) -> Pid ! {input, self(), Value} end,
			  State#state.output_neurons),
	    learning(State);
	{input, NeuronPid, Value} ->
	    {NewStateFunc, NewState} = handle_incoming_value(NeuronPid, Value,
							     State),
	    apply(?MODULE, NewStateFunc, [NewState])	    
    end.

learning(#state{type=Type}=State) ->
    receive
	{learning, NeuronPid, SensitivityWeight} when Type==input -> 
	    %% Throw away value and go to input state
	    input(State);
	{learning, NeuronPid, SensitivityWeight} when Type==hidden -> 
	    {NewStateFunc, NewState} = handle_learning(NeuronPid,
						       SensitivityWeight,
						       State),
	    apply(?MODULE, NewStateFunc, [NewState]);
	{learning, NeuronPid, DesiredOutput} when Type==output -> 
	    Sensitivity = calculate_sensitivity(
			    DesiredOutput-State#state.last_output, State),
	    NewState = send_sensitivity_weight_backwards(Sensitivity, State),
	    input(NewState)
    end.


code_change(_OldVsn, State, _Extra) ->
    {ok, {State, data_vsn()}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handle incoming input values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_incoming_value(NeuronPid, Value, State) ->
    assert_expected_neuron_sender(NeuronPid, State#state.expected_neurons),
    Neuron = get_neuron(NeuronPid, State#state.neurons),
    Output = Neuron#neuron.weight * Value,
    NewNeurons = store_neuron(NeuronPid, Neuron#neuron{last_input=Value},
			      State#state.neurons),
    check_if_done_and_return_new_state(NeuronPid, Output,
				       State#state{neurons=NewNeurons}).

check_if_done_and_return_new_state(NeuronPid, Output, State) ->
    case remove_neuron_pid_from_expected(NeuronPid, State) of
	[] ->
	    %% Expected is empty, send output and go to new state
	    send_output_forward(State#state.current_sum+Output, State);
	Expected ->
	    {input, State#state{expected_neurons=Expected,
				current_sum=State#state.current_sum+Output}}
    end.

send_output_forward(Output, State) ->
    ValueToBeSent = apply_sigmoid(Output),
    lists:foreach(
      fun(NeuronPid) ->
	      NeuronPid ! {input, self(), ValueToBeSent}
      end, State#state.output_neurons),
    {learning, init_learning_state(State#state{last_output=Output})}.
    
init_learning_state(State) ->
    %% TODO: actually initialize the state here
    State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handle incoming learning values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_learning(NeuronPid, SensitivityWeight, State) ->
    assert_expected_neuron_sender(NeuronPid, State#state.expected_neurons),
    case remove_neuron_pid_from_expected(NeuronPid, State) of
	[] ->
	    %% Calculate new weight and sensitivity and send backward
	    NewSensitivity =
		calculate_sensitivity(SensitivityWeight+State#state.current_sum,
				      State),
	    NewState = send_sensitivity_weight_backwards(NewSensitivity, State),
	    {input, init_input_state(NewState)};
	Expected ->
	    {learning, State#state{expected_neurons=Expected,
				   current_sum=State#state.current_sum+
				       SensitivityWeight}}
    end.

send_sensitivity_weight_backwards(Sensitivity, State) ->
    NewNeurons = 
	lists:foldl(
	  fun(NeuronPid, Neurons) ->
		  Neuron = get_neuron(NeuronPid, Neurons),
		  NewWeight = Neuron#neuron.weight + State#state.learning_rate
		      * Sensitivity * Neuron#neuron.last_input,
		  NeuronPid ! {learning, self(), NewWeight*Sensitivity},
		  store_neuron(NeuronPid, Neuron#neuron{weight=NewWeight},
			       Neurons)
	  end, State#state.neurons, State#state.input_neurons),
    State#state{neurons=NewNeurons}.

init_input_state(State) ->
    %% TODO: actually initialize input state
    ExpectedNeurons = [{Pid, a} || Pid <- State#state.input_neurons],
    State#state{expected_neurons=dict:from_list(ExpectedNeurons)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Math functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
apply_sigmoid(Output) ->
    %% TODO: fix real sigmoid val
    Output.

apply_sigmoid_deriv(Output) ->
    %% TODO: fix real sigmoid val
    Output.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Neuron weight and state functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calculate_sensitivity(SensitivityWeightSum, State) ->
    apply_sigmoid_deriv(State#state.last_output) * SensitivityWeightSum.

get_neuron(NeuronPid, Neurons) ->
    dict:fetch(NeuronPid, Neurons).

store_neuron(NeuronPid, Neuron, Neurons) ->
    dict:store(NeuronPid, Neuron, Neurons).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Misc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assert_expected_neuron_sender(NeuronPid, ExpectedNeurons) -> 
    case dict:is_key(NeuronPid, ExpectedNeurons) of
	true ->
	    ok;
	false ->
	    error({received_msg_from_unexpected_neuron,
		   [{self, self()}, {sender, NeuronPid},
		    {expected_neurons, dict:fetch_keys(ExpectedNeurons)}]})
    end.

remove_neuron_pid_from_expected(NeuronPid, State) ->
    NewExpected = dict:erase(NeuronPid, State#state.expected_neurons),
    case dict:size(NewExpected) of
	0 ->
	    %% Empty list is expected to symbolize that no more incoming 
	    %% neuron inputs that are expected
	    [];
	_Else ->
	    NewExpected
    end.

