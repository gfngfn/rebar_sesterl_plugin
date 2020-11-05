-module(rebar_sesterl).
-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    io:format("DEBUG L('o' )J ~p:init~n", [?MODULE]),
    rebar_sesterl_prv_compile:init(State).
