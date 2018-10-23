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
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"},
            {profiles, [test]}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec opts() -> [{atom(), atom(), string(), atom(), string()}].
opts() ->
    [
     {tests, $t, "tests", string, "Comma separated list of test functions. form: modname:funname | funname"},
     {module, $m, "module", string, "Default module."},
     {cover, $c, "cover", boolean, "Generate cover data. Defaults to false."},
     {cover_export_name, undefined, "cover_export_name", "Base name of the coverdata file to write."},
     {verbose, $v, "verbose", boolean, "Verbose output. Defaults to false."},
     {name, undefined, "name", atom, "Gives a long name to the node."},
     {sname, undefined, "sname", atom, "Gives a short name to the node."},
     {setcookie, undefined, "setcookie", atom, "Set the cookie if the node is distributed."}
    ].
