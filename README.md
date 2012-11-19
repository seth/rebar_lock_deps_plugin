# A Rebar Plugin that Generates Locked Dependencies for Rebar Projects #

## tl;dr ##

Use this plugin to create reproducible builds for a rebar
project. Generate a `rebar.config.lock` file by running the command
from the top level directory of your project like this:

    rebar lock-deps

The generated `rebar.config.lock` file lists every dependency of the
project and locks it at the git revision found in the `deps` directory.

A clean build using the lock file (`rebar -C rebar.config.lock`) will
pull all dependencies as found at the time of generation. Suggestions
for integrating with Make are described below.

## Installation ##

Add the following to your top-level rebar.config:

    %% Plugin dependency
    {deps, [
    	{rebar_lock_deps_plugin, ".*",
         {git, "git://github.com/seth/rebar_lock_deps_plugin.git", {branch, "master"}}}
    ]}.

    %% Plugin usage
    {plugins, [rebar_lock_deps_plugin]}.

## How it works ##

The command must be run from a rebar project directory in which
`get-deps` has already been run. It generates a `rebar.config.lock`
file where the dependencies are locked to the versions available
locally (the current state of the project).

The lock-deps command goes through each directory in your deps dir and
calls `git rev-parse HEAD` to determine the currently checked out
version. It also extracts the dependency specs (the `deps` key) from
the rebar.config files inside each directory in deps (non-recursively)
along with the top-level rebar.config. Using this data, the script
creates a new rebar.config.lock file as a clone of the top-level
rebar.config file, but with the `deps` key replaced with the complete
list of dependencies set to SHA that is based on
what is currently checked out.

If something fails during the get-deps rebar stage, take care to run
`rebar get-deps skip_deps=true` to try to repair it. Otherwise, rebar
will pull deps based on specs of your dependencies rather than the
locked version at the top-level.

## Ignoring some dependencies ##

If there are dependencies which you do not wish to lock, you can list
them using the `ignore` option on the command line (comma separate
multiple values).  For example, `rebar lock-deps ignore=meck,eper`
would lock all dependencies except for `meck` and `eper` which would
retain the spec as found in one of the `rebar.config` files that
declared it. Note that if a dependency is declared more than once, the
script picks a spec "at random" to use.

## How you can integrate it into your build ##

Assuming you build your project with `make`, add the following to your
`Makefile`:

    # The release branch should have a file named USE_REBAR_LOCKED
    use_locked_config = $(wildcard USE_REBAR_LOCKED)
    ifeq ($(use_locked_config),USE_REBAR_LOCKED)
      rebar_config = rebar.config.locked
    else
      rebar_config = rebar.config
    endif
    REBAR = rebar -C $(rebar_config)

    update_locked_config:
    	@rebar lock-deps ignore=meck

To tag the release branch, you would create a clean build and verify
it works as desired. Then run `make update_locked_config` and check-in
the resulting `rebar.config.lock` file. For the release branch, `touch
USE_REBAR_LOCKED` and check that in as well. Now create a tag.

The idea is that a clean build from the tag will pull deps based on
rebar.config.lock and you will have reproduced what you tested.

On master, you don't have a `USE_REBAR_LOCKED` file checked in and will
use the standard `rebar.config` file.

This approach should keep merge conflicts to a minimum. It is also
nice that you can easily review which dependencies have changed by
comparing the `rebar.config.lock` file in version control.

