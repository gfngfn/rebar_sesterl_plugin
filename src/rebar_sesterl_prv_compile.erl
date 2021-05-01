-module(rebar_sesterl_prv_compile).

-behaviour(provider).
-export([
    init/1,
    do/1,
    format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, lock}]).

-record(rebar_sesterl_settings, {
    output_dir :: string()
}).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([
            {name, ?PROVIDER},
            {namespace, sesterl},
            {module, ?MODULE},
            {deps, ?DEPS},
            {bare, true},
            {short_desc, "Build Sesterl packages with rebar3"},
            {desc, "Build Sesterl packages with rebar3"},
            {example, "rebar3 sesterl compile"}
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% Deps :: [rebar_app_info.t()]
    Deps = rebar_state:all_deps(State),
    lists:foreach(
        fun(Dep) ->
            Name     = rebar_app_info:name(Dep),
            Dir      = rebar_app_info:dir(Dep),
            FetchDir = rebar_app_info:fetch_dir(Dep),
            OutDir   = rebar_app_info:out_dir(Dep),
            rebar_api:info("Deps: ~p (fetch_dir: ~p, dir: ~p, out_dir: ~p) ~p",
                [Name, FetchDir, Dir, OutDir, Dep])
        end,
        Deps),
    case get_settings_from_config(State) of
        {error, _} = Err ->
            Err;

        {ok, Settings} ->
            #rebar_sesterl_settings{output_dir = OutputDir} = Settings,
            CommandLine = lists:flatten(io_lib:format("sesterl build ./ -o ~s", [OutputDir])),
            rebar_api:info("Compiling Sesterl programs (command: ~p) ...", [CommandLine]),
            case rebar_utils:sh(CommandLine, [use_stdout, return_on_error]) of
                {ok, _} -> rebar_prv_compile:do(State);
                _       -> {error, "Failed to compile Sesterl package(s)"}
            end
    end.

-spec format_error(Reason :: term()) -> iolist().
format_error(Reason) ->
    rebar_prv_compile:format_error(Reason).

-spec get_settings_from_config(rebar_state:t()) -> {ok, #rebar_sesterl_settings{}} | {error, string()}.
get_settings_from_config(State) ->
    Assoc = rebar_state:get(State, sesterl_opts, []),
    OutputDir = proplists:get_value(output_dir, Assoc, "_generated"),
    try
        lists:all(fun erlang:is_integer/1, OutputDir)
    of
        true  -> {ok, #rebar_sesterl_settings{output_dir = OutputDir}};
        false -> {error, "non-string value is specified for 'output_dir'"}
    catch
        _:_ -> {error, "non-string value is specified for 'output_dir'"}
    end.
