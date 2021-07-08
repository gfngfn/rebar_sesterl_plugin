-module(rebar_sesterl).
-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State0) ->
    {ok, State1} = rebar_sesterl_prv_compile:init(State0),
    {ok, State2} = rebar_sesterl_prv_test:init(State1),
    {ok, State2}.
