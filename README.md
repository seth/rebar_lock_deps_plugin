# lock_deps: Generate Locked Dependencies for Rebar Projects #

The `lock_deps` script generates an alternate rebar.config file that
lists every dependency of a project and locks it at a given git
revision using the `{tag, SHA}` syntax.

Use this script to create reproducible builds for a top-level OTP
release project that pulls in a number of different projects as rebar
dependencies. To lock a build, run the script from the top level
directory of your project like this:

    ./lock_deps deps

This will generate a `rebar.config.lock` file containing locked
dependencies for all dependencies found in the `deps` directory or
specified in the project's own rebar.config file. If there are
dependencies which you do not wish to lock, you can list them after
the deps directory argument on the command line. For example,

    ./lock_deps deps meck

would lock all dependencies except for `meck` which would retain the
spec specified in one of the rebar.config files that declared it. Note
that if a dependency is declared more than once, the script picks a
spec "at random" to use.

You can now use the resulting rebar.config.lock file to produce a
build where all dependencies will match your current state. If
something fails during the get-deps rebar stage, take care to run
`rebar get-deps skip_deps=true` to try to repair it. Otherwise, rebar
will pull deps based on specs of your dependencies rather than the
locked version at the top-level.






