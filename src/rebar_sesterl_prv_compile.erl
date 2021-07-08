-module(rebar_sesterl_prv_compile).

-behaviour(provider).

%%==============================================================================================
%% `provider' Callback API
%%==============================================================================================
-export([
    init/1,
    do/1,
    format_error/1
]).

%%==============================================================================================
%% Macros & Types
%%==============================================================================================
-define(PROVIDER, compile).
-define(DEPS, [{default, lock}]).

%%==============================================================================================
%% `provider' Callback Functions
%%==============================================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([
            {name, ?PROVIDER},
            {namespace, sesterl},
            {module, ?MODULE},
            {deps, ?DEPS},
            {bare, true},
            {short_desc, "Build Sesterl packages with Rebar3"},
            {desc, "Build Sesterl packages with Rebar3"},
            {example, "rebar3 sesterl compile"},
            {profiles, [test]}
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar_sesterl_common:compile(State) of
        ok -> rebar_prv_compile:do(State);
        _  -> {error, "Failed to compile Sesterl package(s)"}
    end.

-spec format_error(Reason :: term()) -> iolist().
format_error(Reason) ->
    rebar_prv_compile:format_error(Reason).
