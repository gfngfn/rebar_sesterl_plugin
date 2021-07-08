-module(rebar_sesterl_prv_compile).

-behaviour(provider).

%%==============================================================================================
%% `provider' Callback API
%%==============================================================================================
-export([
    init/1,
    do/1,
    format_error/1]).

%%==============================================================================================
%% Macros & Types
%%==============================================================================================
-define(PROVIDER, compile).
-define(DEPS, [{default, lock}]).
-define(CONFIG_FILE_NAME, "package.yaml").

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
    %% Deps :: [rebar_app_info.t()]
    Deps = rebar_state:all_deps(State),
    %% DepDirs :: [{string(), string()}]
    DepDirs =
        lists:filtermap(
            fun(Dep) ->
                NameBin = rebar_app_info:name(Dep),
                NameStr = erlang:binary_to_list(NameBin),
                Dir = rebar_app_info:dir(Dep),
                case filelib:is_regular(Dir ++ "/" ++ ?CONFIG_FILE_NAME) of
                    true  -> {true, {NameStr, Dir}};
                    false -> false
                end
            end,
            Deps),
    lists:foreach(
        fun({NameStr, Dir}) ->
            rebar_api:info("Sesterl dependency: ~s (at: ~s)", [NameStr, Dir])
        end,
        DepDirs),
    ExternalArg =
        lists:append(
            lists:map(
                fun({NameStr, Dir}) ->
                    " -p " ++ NameStr ++ ":" ++ Dir
                end,
                DepDirs)),
    CommandLine = lists:flatten(io_lib:format("sesterl build ./~s", [ExternalArg])),
    rebar_api:info("Compiling Sesterl programs (command: ~p) ...", [CommandLine]),
    case rebar_utils:sh(CommandLine, [use_stdout, return_on_error]) of
        {ok, _} -> rebar_prv_compile:do(State);
        _       -> {error, "Failed to compile Sesterl package(s)"}
    end.

-spec format_error(Reason :: term()) -> iolist().
format_error(Reason) ->
    rebar_prv_compile:format_error(Reason).
