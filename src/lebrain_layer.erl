-module(lebrain_layer).

-export([spawn_link/1, init/1]).

-record(state {
	  neurons = dict:new(),
	  layer_type,
	  output_neurons = []
	  input_neurons = [],
	  number_of_neurons = 0
	 })

spawn_link(NumberOfNeurons) ->
    proc_lib:spawn_link(?MODULE, init, [NumberOfNeurons]).

add_layers(LayerPid, Neurons) ->
add_layers(LayerPid, InputNeurons, OutputNeurons) ->
    LayerPid ! {add_layers, InputNeurons, OutputNeurons},
    receive
	{layers_added, LayerPid} ->
	    ok
    end.

get_neurons(LayerPid) ->
    LayerPid ! {get_neurons, self()},
    receive
	{neurons, Neurons0} ->
	    Neurons0
    end.
	

init(NumberOfNeurons, LayerType) ->
    NeuronDict = orddict:new(),
    UpdatedNeuronDict = 
	lists:foldl(
	  fun(Nr, NeuronDict0) ->
		  orddict:store(Nr, lebrain_neuron:spawn_link(LayerType),
				NeuronDict0)
	  end, NeuronDict, lists:seq(1, NumberOfNeurons)),
    loop(#statee{
	    neurons=UpdatedNeuronDict,
	    layer_type=LayerType,
	    number_of_neurons=NumberOfNeurons
	   }).

loop(#state{layer_type=Type}=State) ->
    receive
	{From, get_neurons} ->
	    Neurons = dict:fetch_keys(State#state.neurons),
	    From ! {neurons, self(), Neurons};
	{add_layers, InputNeurons, OutputNeurons} when Type =/= input ->
	    lists:foreach(
	     fun(Pid) ->
		     Pid ! {add_neurons, {InputNeurons, OutputNeurons}}
	     end, dict:fetch_keys(State#state.neurons)),
	    loop(State);
	{add_layers, Neurons} when Type == input ->
	    lists:foreach(
	      fun(Pid) ->
		      Pid ! {add_neurons, Neurons}
	      end, dict:fetch_keys(State#state.neurons)),
	    loop(State)
    end.
