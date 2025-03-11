# Hoff

Hoff is a bot for GitHub that enforces a clean history, and ensures that master
always builds.

Hoff intends to replace the merge button in the GitHub UI.  Hoff integrates
changes into master using a *rebase*. This keeps the history clean, free of
random fork points and merge commits.  (TODO: In the future Hoff will also
enforce a commit message format.)

Furthermore, Hoff implements the [Not Rocket Science Principle][not-rocket-science]
of software engineering:

> Automatically maintain a repository of code that always passes all the tests.

The application watches a repository for new pull requests. Once a pull request
has been approved (through an `@hoffbot merge` comment left by the PR author or
reviewer), it integrates the changes into master, and pushes those to a testing
branch. When CI reports a successful build for this branch, master is forwarded
to it. If the build fails, the commits never make it into master, keeping the
build green at all times.

Merge trains: if a second PR receives a merge comment while a build is still
happening, Hoff starts a speculative rebase on top of the previous build.  The
same goes for a third PR.  Assuming all of the three builds pass, the third PR
is merged after the time of a single build.


## Using Hoff

Supposing Hoff is set up to listen for the comment prefix `@hoffbot` with a
matching GitHub user, you use it by commenting on a PR with any of the
following commands:

* `@hoffbot merge`: rebase then merge;
* `@hoffbot merge and tag`: rebase, merge then tag.
* `@hoffbot merge and deploy <subprojects> to <env>`: rebase, merge and tag, then set the specified
  labels on the merge commit message. `<subprojects>` is optional, comma-separate list, that should
  be subset of the configured subprojects of the project. It corresponds to the `Deploy-Subprojects`
  field on the merge commit.`to <env>` is optional if there is only a single deployment environment
  configured for the project, and required otherwise. It corresponds to the `Deploy-Environment`
  field on the merge commit.
* `@hoffbot retry`: retry the last operation with the same parameters after the
  CI build has failed. Equivalent to closing and reopening the PR and then
  tagging the bot again with the same command.

For all the commands, Hoff will wait for the builds to pass after rebasing and
before merging.  When the PR is merged, GitHub closes the PR as merged and,
when configured to automatically do so, deletes the PR branch.

Hoff does not actually do the deploying.  It just adds a special marker to the
tag message indicating to the CI job that the tag should be deployed.

Subprojects and environments can be configured per project (see `package/example-config.json`)

On Fridays, by default, Hoff refuses to do the above actions.  To force merges
on Fridays, simply add `on friday` at the end of your commands, like so:

* `@hoffbot merge on friday`;
* `@hoffbot merge and tag on friday`.
* `@hoffbot merge and deploy on friday`;


## Installing

See the [installation guide](doc/installing.md) if you want to run a self-hosted
version of Hoff.

TODO: Write a proper guide to build a package.
TODO: Publish official deb packages?

## Building

Hoff is written in Haskell and builds with [Cabal][cabal]:

    $ cabal build -j
    $ cabal test -j

## Running as a developer

To run the application locally
you first need to create an appropriate `config.json` file:

    $ cp doc/example-dev-config.json config.json

Edit `config.json` to match your required settings.
You can generate a personal access token in the
["personal access tokens" tab of GitHub settings](https://github.com/settings/tokens).
Give it a 7 days expiration and access to just "repo"s.

    $ mkdir -p run/state
    $ cabal run hoff config.json

Hoff also comes with an `.envrc` file that can be used by direnv. You have to run
`direnv allow` once to give permission. From then on you will always be in the right
nix develop environment when cd'ing to the Hoff project.

You can then access [http://localhost:1979](http://localhost:1979)
to see the open PRs and build queue.

The build queue is fetched through GitHub's web interface,
so you will be able to see the full list right away.

[Comment]s and [build status]es are only sent in though a webhook.
While running without a public IP address,
webhooks will have to be forwarded to your local instance somehow.
You can use the official [`gh webhook forward`] tool for this, by running

    $ gh webhook forward --repo=REPO --events='*' --url="http://localhost:1979/hook/github" --secret="SECRET"

where `REPO` is the repo you're testing with (in the `OWNER/NAME` format), and
`SECRET` should be the value of `secret` in your `config.json`.

> [!TIP]
> If you have `jq` installed, you can avoid manually copying the webhook secret
> around by passing `--secret=$(jq -r '.secret' config.json)`.

> [!NOTE]
> If your testing repo doesn't have CI that marks commits' build status, you
> will still need to manually trigger a build status change for the speculative
> rebase commit that Hoff produces, as described below.

Alternatively, to manually trigger webhooks for a local Hoff instance,
you can use some of the scripts in the `tools/` folder to simulate them:

    $ ./tools/comment deckard 31337 @hoffbot merge

	$ ./tools/build-status c033170123456789abcdef0123456789abcdef01

[Comment]: https://docs.github.com/en/developers/webhooks-and-events/webhooks/webhook-events-and-payloads#issue_comment
[build status]: https://docs.github.com/en/developers/webhooks-and-events/webhooks/webhook-events-and-payloads#status
[`gh webhook forward`]: https://docs.github.com/en/webhooks/testing-and-troubleshooting-webhooks/using-the-github-cli-to-forward-webhooks-for-testing

The tests of Hoff are extensive, you may be able to get by just by running them
when making changes to the code.  To run a specific test, use `--match` giving
part of the test title:

	$ nix develop --file default.nix -c cabal run spec -- --match "part of the test title"

The implementation uses free monads and some of the tests replace lower level
functionality with mocks.


## Running on a server

To run Hoff on a server, you can build a self-contained squashfs file system
image with [Nix][nix]:

    $ nix build --out-link hoff.img
    $ cp package/example-config.json config.json
    $ vi config.json # edit the file appropriately
    $ sudo systemd-nspawn \
      --ephemeral         \
      --image hoff.img    \
      --bind-ro=$PWD:/etc \
      -- /usr/bin/hoff /etc/config.json

The image includes Hoff and all of its dependencies (Git, SSH). You can run it
with systemd. TODO: Make it work with portablectl.

You can also build Hoff as a deb package by running:
`./package/build-and-ship.sh`


## Features

### Comment interface

The main Hoff interface is through GitHub comments,
here is [an example](https://github.com/channable/hoff/pull/166):

![GitHub comments to and by Hoff](doc/comments.png)

Channable's Hoff repository is tracked itself using Hoff ([@OpsBotPrime]),
so you can see many examples of use in past merged PRs
([#173], [#167], [#138], [#108], ...).

[#173]: https://github.com/channable/hoff/pull/173
[#167]: https://github.com/channable/hoff/pull/167
[#138]: https://github.com/channable/hoff/pull/138
[#108]: https://github.com/channable/hoff/pull/108


### Web interface

In addition to the comment interface, you can check the status of running and
failed builds through a web interface:

![Web interface](doc/web-interface.png)


### Merge trains

Hoff supports parallel speculative builds of subsequently approved PRs.
Without merge trains, here is how a timeline of interaction with Hoff
would go:

![Without merge trains there are delays to start building.](doc/no-merge-train.svg)

Supposing the build time is of ≈10 minutes:

* the authors of PR#1 wait ≈10 minutes for their PR to be merged;
* the authors of PR#2 wait ≈16 minutes for their PR to be merged;
* the authors of PR#3 wait **≈22 minutes** for their PR to be merged.

With merge trains, Hoff reduces the waiting time for merges.
Here is the same timeline of interaction for Hoff with merge trains active:

![With merge trains, speculative builds start immediately.](doc/merge-train.svg)

The first PR is merged and rebased immediatelly as usual.
The second and third PRs are now merged and rebased immediatelly
on top of the first and second respectively.
Assuming all builds eventually pass,
the authors of all PRs only have to wait 10 minutes each
for their PRs to reach `master`.
The waiting time for authors of the third PR is _reduced by 12 minutes_!

**Failing merge trains.**
If at some point in the train a build fails, subsequent PRs are (speculatively)
reintegrated and their builds are restarted:

![A merge train with a failure.](doc/merge-train-failure.svg)

1. Three merge commands are issued in a short period for PR#1, PR#2 and PR#3.

2. PR#2 fails while PR#1 and PR#3 are still building.
    1. Since PR#2 is built on top of PR#1,
       Hoff cannot assume PR#2 is the culprit of the failure
       so Hoff delays reporting this until PR#1 finishes.
    2. Since PR#3 has been built on top of PR#2,
       its build is restarted with a rebase and merge on top of PR#1.
       This is done by pushing to the `testing/2` branch.

3. PR#1 build passes, `testing/1` is pushed (promoted) to be the new master.
   The speculative failure of PR#2 is now real so this is reported.

4. PR#3 build passes, `testing/3` is pushed (promoted) to be the new master.

5. At this point, the authors of PR#1 have fixed their PR
   and issue a new merge command.  Hoff carries on as usual.

When PRs are closed or receive a new commit,
the behaviour is similar to a build failing for that PR
with the only difference being the comment posted by Hoff.

**Builds started later finishing earlier.**
Builds results do not always come in the same order
as they were started in the train.
When the build result of a later PR
arrives before the result of an earlier PR
Hoff waits before merging:

![A merge train where the second PR finishes building first](doc/merge-train-delay.svg)

Hoff is conservative in what it allows to be pushed to master,
it tries to guarantee that the builds of all PRs pass by themselves alone.
Hoff will not merge a sequence of two PRs
where the second fixes the build of the first.
This is a choice that trades CI time for confidence that individual PRs
are buildable points in the project history
as opposed to rollups that include multiple PRs in a single test build.

**Complex scenario.**
Here is a more complex scenario involving builds arriving in the unexpected order
and a failure of a build of the first PR in a merge train.
This is similar two the two previous scenarios,
the only difference being that PR#1 alone causes the build to fail.

![A merge train being restarted.](doc/merge-train-restart.svg)

1. Hoff receives the command to merge the three PRs in a short period of time.

2. Hoff rebases and merges accordingly.

3. PR#2 build fails first:
    1. The build of PR#3 is restarted on top of PR#1 because of this.
    2. We have to wait for the result of PR#1
       to decide if the failure of PR#2 is a real failure.

4. PR#1 eventually fails (it was the culprit!).  This is reported.
   The train is restarted with PR#2 first then PR#3 integrated on top of PR#2.

5. PR#2 and PR#3 builds eventually pass and
   they are pushed to `master` in sequence.

6. A developer eventually fixes PR#1
   and issues a second merge command.
   Hoff proceeds as usual.

**Rebase failures.**
When Hoff fails to rebase a branch in a train due to a conflict,
it does not know whether this is a conflict with `master`
or any other PRs in the train.
We have to wait for the build results of the last PR it is based on
before doing anything.

If the build eventually passes for the parent PR,
Hoff reports a rebase failure as the parent PR has become the new `master`:

![A merge train with a rebase failure (1).](doc/merge-train-rebase-failure-1.svg)

If the build eventually fails for the parent PR,
Hoff does a new rebase and merge:

![A merge train with a rebase failure (2).](doc/merge-train-rebase-failure-2.svg)

When the rebase failure is because of an orphaned `fixup!` commit,
the rebase failure is reported immediatelly
as this is not dependent on previous PRs in the train.


## Further reading

More information is available in the doc directory:

 * [Background](doc/background.md): My original intention was more ambitious
   than building a GitHub bot. This document gives some background about what I
   want to build.
 * [Installing](doc/installing.md): The installation guide.

## License

Hoff is free software. It is licensed under the [Apache 2.0][apache2] license.
It may be used both for commercial and non-commercial use under the conditions
given in the license.

[not-rocket-science]: https://graydon2.dreamwidth.org/1597.html
[cabal]:              https://cabal.readthedocs.io/en/stable/
[nix]:                https://nixos.org/nix/
[apache2]:            https://www.apache.org/licenses/LICENSE-2.0
[GitHub webhook for the build status]: https://docs.github.com/en/developers/webhooks-and-events/webhooks/webhook-events-and-payloads#status
[faf04c9]:            https://github.com/channable/hoff/commit/faf04c96486cb9dc30b6f133f6574ceab3401955
[@OpsBotPrime]:       https://github.com/OpsBotPrime
