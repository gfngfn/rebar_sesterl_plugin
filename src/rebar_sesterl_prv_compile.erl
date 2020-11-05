-module(rebar_sesterl_prv_compile).

-behaviour(provider).
-export([
    init/1,
    do/1,
    format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [lock]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    io:format("DEBUG L('o' )J ~p:init~n", [?MODULE]),
    Provider =
        providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {deps, ?DEPS},
            {bare, true},
            {short_desc, "Build Sesterl packages with rebar3"},
            {desc, "Build Sesterl packages with rebar3"},
            {example, "rebar3 compile"}
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    io:format("DEBUG L('o' )J ~p:do~n", [?MODULE]),
    case rebar_utils:sh("sesterl ./ -o _generated/", [use_stdout, return_on_error]) of
        {ok, _} -> rebar_prv_compile:do(State);
        _       -> {error, "Failed to compile Sesterl package(s)"}
    end.

-spec format_error(Reason :: term()) -> iolist().
format_error(Reason) ->
    rebar_prv_compile:format_error(Reason).
