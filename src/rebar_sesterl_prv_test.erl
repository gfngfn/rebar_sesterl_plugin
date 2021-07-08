-module(rebar_sesterl_prv_test).

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
-define(PROVIDER, test).
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
            {short_desc, "Build Sesterl packages and run tests with Rebar3"},
            {desc, "Build Sesterl packages and run tests with Rebar3"},
            {example, "rebar3 sesterl test"},
            {profiles, [test]}
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar_sesterl_common:compile(State) of
        ok -> rebar_prv_eunit:do(State);
        _  -> {error, "Failed to compile Sesterl package(s)"}
    end.

-spec format_error(Reason :: term()) -> iolist().
format_error(Reason) ->
    rebar_prv_eunit:format_error(Reason).
