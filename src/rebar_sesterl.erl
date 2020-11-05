-module(rebar_sesterl).
-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    rebar_sesterl_prv_compile:init(State).
