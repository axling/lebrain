-module(lebrain_layer).

-export([spawn_link/2,
         init/2]).

-export([add_layers/2,
         add_layers/3,
         get_neurons/1]).

-record(state, {
          neurons = dict:new(),
          layer_type,
          output_neurons = [],
          input_neurons = [],
          number_of_neurons = 0
         }).

spawn_link(NumberOfNeurons, LayerType) ->
    proc_lib:spawn_link(?MODULE, init, [NumberOfNeurons, LayerType]).

add_layers(LayerPid, Neurons) ->
    LayerPid ! {add_layers, Neurons},
    receive
        {layers_added, LayerPid} ->
            ok
    end.

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
    loop(#state{
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
