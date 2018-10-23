-module(rebar3_eunit_tests_plugin_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, eunit_tests).
-define(DEPS, [lock]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 eunit_tests -t mod:fun,mod:fun"}, % How to use the plugin
            {opts, opts()},               % list of options understood by the plugin
            {short_desc, "A rebar plugin \"eunit_tests\""},
            {desc, "A rebar plugin to invoke eunit with overriding eunit_tests value by command line options."},
            {profiles, [test]}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State0) ->
    case override_state(State0) of
        {ok, State1} ->
            rebar_prv_eunit:do(State1);
        Err -> Err
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private functions
%% ===================================================================
-spec opts() -> [{atom(), atom(), string(), atom(), string()}].
opts() ->
    [
     {tests, $t, "tests", string, "Comma separated list of test functions. Form: modname:funname | funname"},
     {module, $m, "module", string, "Default module."},
     {function_suffix, undefined, "function_suffix", string, "Function suffix. Defalts to \"_test_\"."},
     {module_suffix, undefined, "module_suffix", string, "Module suffix. Defalts to \"_tests\"."},
     {cover, $c, "cover", boolean, "Generate cover data. Defaults to false."},
     {cover_export_name, undefined, "cover_export_name", "Base name of the coverdata file to write."},
     {verbose, $v, "verbose", boolean, "Verbose output. Defaults to false."},
     {name, undefined, "name", atom, "Gives a long name to the node."},
     {sname, undefined, "sname", atom, "Gives a short name to the node."},
     {setcookie, undefined, "setcookie", atom, "Set the cookie if the node is distributed."}
    ].

-spec override_state(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
override_state(State0) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State0),
    Tests0 = proplists:get_value(tests, RawOpts, []),
    Module0 = proplists:get_value(module, RawOpts, []),
    FunSuffix = proplists:get_value(function_suffix, RawOpts, "_test_"),
    ModSuffix = proplists:get_value(module_suffix, RawOpts, "_tests"),
    try
        if Tests0 =:= [] -> error("Tests is not specified.");
           true -> ok
        end,
        Tests1 = re:split(Tests0, ",", [{return, list}]),
        Gens =
            lists:map(
              fun([]) ->
                      error("Empty component in tests.");
                 (T) ->
                      {ModP, FunP} =
                          case re:split(T, ":", [{return, list}]) of
                              [M, F] when M =:= []; F =:= [] ->
                                  error(io_lib:format("Malformed component in tests: ~p", [T]));
                              [M, F] -> {M, F};
                              _ when Module0 =:= [] -> error("Module is not specified.");
                              [F] -> {Module0, F}
                          end,
                      Mod = list_to_atom(ModP ++ ModSuffix),
                      Fun = list_to_atom(FunP ++ FunSuffix),
                      {generator, Mod, Fun}
              end, Tests1),
        {ok, rebar_state:set(State0, eunit_tests, Gens)}
    catch
        error:Reason -> {error, lists:flatten(Reason)}
    end.
