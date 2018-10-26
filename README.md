# rebar3_eunit_tests_plugin
A rebar plugin to invoke eunit with overriding eunit_tests value by command line options.

## Use
Add the plugin to your rebar config:

```
{plugins, [
    {rebar3_eunit_tests_plugin, {git, "https://github.com/AoiMoe/rebar3_eunit_tests_plugin.git", {tag, "v1.0.1"}}}
]}.
```

Then just call your plugin directly in an existing application:

```
$ rebar3 help eunit_tests
===> Fetching rebar3_eunit_tests_plugin ...
===> Compiling rebar3_eunit_tests_plugin
A rebar plugin to invoke eunit with overriding eunit_tests value by command line options.
Usage: rebar3 eunit_tests [-r <raw>] [-t <tests>] [-m <default_module>]
                          [--function_suffix <function_suffix>]
                          [--module_suffix <module_suffix>] [--app <app>]
                          [--application <application>] [-d <dir>]
                          [-f <file>] [-s <suite>] [-c <cover>]
                          [--cover_export_name <cover_export_name>]
                          [-v <verbose>] [--name <name>]
                          [--sname <sname>] [--setcookie <setcookie>]

  -r, --raw             Raw eunit_tests value. Proir all other options. 
                        Example: "[{module, foo_tests}]"
  -t, --tests           Comma separated list of test functions. Form: 
                        modname:funname | funname
  -m, --default_module  Default module.
  --function_suffix     Function suffix. Defaults to "_test_".
  --module_suffix       Module suffix. Defaults to "_tests".
  --app                 Comma separated list of application test suites to 
                        run.
  --application         Same as "app"
  -d, --dir             Comma separated list of dirs to load tests from.
  -f, --file            Comma separated list of files to load tests from.
  -s, --suite           Comma separated list of modules to load tests from.
  -c, --cover           Generate cover data. Defaults to false.
  --cover_export_name   Base name of the coverdata file to write.
  -v, --verbose         Verbose output. Defaults to false.
  --name                Gives a long name to the node.
  --sname               Gives a short name to the node.
  --setcookie           Set the cookie if the node is distributed.


$ mkdir test
$ echo '-module(foo_tests).' > test/foo_tests.erl
$ echo '-export([bar_test_/0,baz_test_/0]).' >> test/foo_tests.erl
$ echo 'bar_test_() -> fun() -> ok end.' >> test/foo_tests.erl
$ echo 'baz_test_() -> fun() -> ok end.' >> test/foo_tests.erl

$ rebar3 eunit_tests -v -t foo:bar
===> Performing EUunit tests...
======================== EUnit ========================
foo_tests: bar_test_...ok
=======================================================
  Test passed.

$ rebar3 eunit_tests -v -m foo -t bar,baz
===> Verifying dependencies...
===> Compiling rebar3_eunit_tests_plugin_test
===> Performing EUnit tests...
======================== EUnit ========================
foo_tests: bar_test_...ok
foo_tests: baz_test_...ok
=======================================================
  2 tests passed.
```
