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
     %% eunit_tests specific options.
     {raw, $r, "raw", string, "Raw eunit_tests value. Proir all other options. Example: \"[{module, foo_tests}]\""},
     {tests, $t, "tests", string, "Comma separated list of test functions. Form: modname:funname | funname"},
     {default_module, $u, "default_module", string, "Default module to be tested."},
     {function_suffix, undefined, "function_suffix", string, "Function suffix. Defaults to \"_test_\"."},
     {module_suffix, undefined, "module_suffix", string, "Module suffix. Defaults to \"_tests\"."},
     %% eunit compatible options -- these are exclusive above.
     {app, undefined, "app", string, "Comma separated list of application test suites to run."},
     {application, undefined, "application", string, "Same as \"app\""},
     {dir, $d, "dir", string, "Comma separated list of dirs to load tests from."},
     {file, $f, "file", string, "Comma separated list of files to load tests from."},
     {module, $m, "module", string, "Comma separated list of modules to load tests from."},
     {suite, $s, "suite", string, "Comma separated list of modules to load tests from."},
     %% passthrough to eunit provider.
     {cover, $c, "cover", boolean, "Generate cover data. Defaults to false."},
     {cover_export_name, undefined, "cover_export_name", string, "Base name of the coverdata file to write."},
     {verbose, $v, "verbose", boolean, "Verbose output. Defaults to false."},
     {name, undefined, "name", atom, "Gives a long name to the node."},
     {sname, undefined, "sname", atom, "Gives a short name to the node."},
     {setcookie, undefined, "setcookie", atom, "Set the cookie if the node is distributed."}
    ].

-spec override_state(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
override_state(State0) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State0),
    Raw = proplists:get_value(raw, RawOpts, []),
    Tests0 = proplists:get_value(tests, RawOpts, []),
    DefModule = proplists:get_value(default_module, RawOpts, []),
    FunSuffix = proplists:get_value(function_suffix, RawOpts, "_test_"),
    ModSuffix = proplists:get_value(module_suffix, RawOpts, "_tests"),
    try
        if Raw =/= [] ->
                %% rebar3 eunit_tests -r [foo, bar]
                %%   -> [foo, bar]
                if Tests0 =/= [] -> error("Exclusive --raw and --tests"); true -> ok end,
                check_exclusive_options(raw, RawOpts),
                Tokens =
                    case erl_scan:string(Raw ++ ".") of
                        {ok, T, _} -> T;
                        _ -> error(io_lib:format("Cannot scan --raw: ~p", [Raw]))
                    end,
                Tests =
                    case erl_parse:parse_term(Tokens) of
                        {ok, Term} when is_list(Term) -> Term;
                        {ok, Term} -> [Term];
                        _ -> error(io_lib:format("Cannot parse --raw: ~p", [Raw]))
                    end;
           Tests0 =:= [] ->
                if DefModule =:= [] ->
                        %% rebar3 eunit_tests
                        %%   -> []
                        Tests = [];
                   true ->
                        %% rebar3 eunit_tests -u mod
                        %%   -> [{module, mod_tests}]
                        check_exclusive_options(default_module, RawOpts),
                        Tests = [{module, list_to_atom(DefModule ++ ModSuffix)}]
                end;
           true ->
                %% rebar3 eunit_tests -t mod1:fun1,mod2:fun2,...,modN:funN
                %%   -> [{generator, mod1_tests, fun1_test_},
                %%       {generator, mod2_tests, fun2_test_},
                %%       ...
                %%       {generator, modN_tests, funN_test_}]
                %% rebar3 eunit_tests -u mod -t fun1,fun2,...,funN
                %%   -> [{generator, mod_tests, fun1_test_},
                %%       {generator, mod_tests, fun2_test_},
                %%       ...
                %%       {generator, mod_tests, funN_test_}]
                check_exclusive_options(tests, RawOpts),
                Tests1 = re:split(Tests0, ",", [{return, list}]),
                Tests =
                    lists:map(
                      fun([]) ->
                              error("Empty component in tests.");
                         (T) ->
                              {ModP, FunP} =
                                  case re:split(T, ":", [{return, list}]) of
                                      [M, F] when M =:= []; F =:= [] ->
                                          error(io_lib:format("Malformed component in tests: ~p", [T]));
                                      [M, F] -> {M, F};
                                      _ when DefModule =:= [] -> error("Module is not specified.");
                                      [F] -> {DefModule, F}
                                  end,
                              Mod = list_to_atom(ModP ++ ModSuffix),
                              Fun = list_to_atom(FunP ++ FunSuffix),
                              {generator, Mod, Fun}
                      end, Tests1)
        end,
        {ok, rebar_state:set(State0, eunit_tests, Tests)}
    catch
        error:Reason -> {error, lists:flatten(Reason)}
    end.

-spec check_exclusive_options(atom(), [proplists:property()]) -> ok.
check_exclusive_options(Opt, RawOpts) ->
    lists:foreach(fun(K) ->
                          case K of
                              app -> true;
                              application -> true;
                              dir -> true;
                              file -> true;
                              module -> true;
                              suite -> true;
                              _ -> false
                          end andalso error(io_lib:format("Exclusive --~p and --~p", [Opt, K]))
                  end, proplists:get_keys(RawOpts)).
