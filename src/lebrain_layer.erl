-module(lebrain_layer).

-export([spawn_link/1, init/1]).

spawn_link(NumberOfNeurons) ->
    proc_lib:spawn_link(?MODULE, init, [NumberOfNeurons]).

init(NumberOfNeurons) ->
    NeuronDict = orddict:new(),
    UpdatedNeuronDict = 
	lists:foldl(
	  fun(Nr, NeuronDict0) ->
		  orddict:store(Nr, lebrain_neuron:spawn_link(), NeuronDict0)
	  end, NeuronDict, lists:seq(1, NumberOfNeurons)),
    loop(UpdatedNeuronDict).

loop(_NeuronDict) ->
    receive
	ok ->
	    ok
    end.
