-module(rebar_sesterl_common).

%%==============================================================================================
%% Exported API
%%==============================================================================================
-export([
    compile/1
]).

%%==============================================================================================
%% Macros & Types
%%==============================================================================================
-define(CONFIG_FILE_NAME, "package.yaml").

%%==============================================================================================
%% Exported Functions
%%==============================================================================================
-spec compile(rebar_state:t()) -> ok | (Error :: term()).
compile(State) ->
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
        {ok, _} -> ok;
        Error   -> Error
    end.
