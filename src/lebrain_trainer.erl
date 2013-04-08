-module(lebrain_trainer).

-export([train/3]).

train(TrainingData, HiddenLayers, Epochs)
  when is_list(TrainingData), is_list(HiddenLayers), is_integer(Epochs) ->
    %% Steps to be done
    %% 1. Create all neuron layers
    %% 2. Connect Layers
    %% 3. Train epoch
    Input = lebrain_training_data:get_number_of_input(TrainingData),
    Output = lebrain_training_data:get_number_of_output(TrainingData),
    InputLayer = lebrain_layer:spawn_link(Input, input),
    OutputLayer = lebrain_layer:spawn_link(Output, output),
    Hidden = lists:map(
               fun(NrOfHiddenNeurons) ->
                       lebrain_layer:spawn_link(NrOfHiddenNeurons, hidden)
               end, HiddenLayers),
    connect_layers(InputLayer, Hidden, OutputLayer),
    train_epochs(TrainingData, InputLayer, OutputLayer, Epochs).

connect_layers(Input, [First | _] = Hidden, Output) when length(Hidden) > 1 ->
    lebrain_layer:add_layers(Input, lebrain_layer:add_layers(First)),
    connect_layers([Input] ++ Hidden ++ [Output]).

connect_layers([First, Second, Third | Rest]) ->
    lebrain_layer:add_layers(Second, lebrain_layer:add_layer(First),
                             lebrain_layer:add_layer(Third)),
    connect_layers([Second, Third | Rest]);
connect_layers([Hidden, Output]) ->
    ok = lebrain_layer:add_layers(Output, lebrain_layer:get_neurons(Hidden)).

train_epochs(_TrainingData, _InputLayer, _OuputLayer, 0) ->
    ok;
train_epochs(TrainingData, InputLayer, OutputLayer, Epochs) ->
    %% Steps to be done
    %% 1. Send inputs to input layer
    %% 2. Receive outputs from output layer
    %% 3. Send Desired Value back to output layer
    %% 4. Start new train epoch
    train_one_time(TrainingData, InputLayer, OutputLayer),
    train_epochs(TrainingData, InputLayer, OutputLayer, Epochs-1).

train_one_time([], _InputLayer, _OuputLayer) ->
    ok;
train_one_time(TrainingData, InputLayer, OutputLayer) ->
    {SessionData, NewTrainingData} =
        lebrain_training_data:get_training_data_for_one_session(TrainingData),
    Inputs = lebrain_training_data:get_inputs(SessionData),
    DesiredOutputs = lebrain_training_data:get_desired_outputs(SessionData),
    InputNeurons = lebrain_layer:get_neurons(InputLayer),
    OutputNeurons = lebrain_layer:get_neurons(OutputLayer),
    send_inputs_to_input_layer(lists:zip(Inputs, InputNeurons)),
    receive_outputs_and_feedback_desired(dict:from_list(
                                           lists:zip(OutputNeurons,
                                                     DesiredOutputs))),
    train_one_time(NewTrainingData, InputLayer, OutputLayer).

send_inputs_to_input_layer([]) ->
    ok;
send_inputs_to_input_layer([{Input, InputNeuron} | Rest]) ->
    InputNeuron ! {input, self(), Input},
    send_inputs_to_input_layer(Rest).

receive_outputs_and_feedback_desired(DesiredOutputDict) ->
    case dict:size() > 0 of
        true ->
            receive
                {input, NeuronPid, _Value} ->
                    DesiredOutput = dict:fetch(NeuronPid, DesiredOutputDict),
                    NeuronPid ! {learning, DesiredOutput},
                    receive_outputs_and_feedback_desired(
                      dict:erase(NeuronPid, DesiredOutputDict))
            end;
        false ->
            ok
    end.
