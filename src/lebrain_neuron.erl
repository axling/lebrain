-module(lebrain_neuron).

%% exports for plain_fsm
-export([spawn_link/0]).

-export([data_vsn/0, code_change/3]).

-behaviour(plain_fsm).

-record(state, {}).

data_vsn() ->
    0.

spawn_link() ->
    plain_fsm:spawn_link(?MODULE,
			fun() ->
				process_flag(trap_exit, true),
				input(#state{})
			end).

input(#state{}=_State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, {State, data_vsn()}}.
