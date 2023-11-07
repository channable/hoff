# Changelog

## 0.32.0

Release 2023-11-07

  - Add `featureFreezeWindow` configuration option. During a configured feature freeze, only commands suffixed with `as hotfix` are allowed. The feature is similar to the `on friday` feature.

## 0.31.11

Release 2023-10-26

  - Disallow `merge and deploy` without a specified environment if multiple environments are available.

## 0.31.10

Released 2023-10-26

  - Always create merge commits when a single-commit PR is approved for merge and tag.

## 0.31.9

Released 2023-10-23

  * Allow a merge command in the description of a merge request, to be able to
    directly create a pull request and approve it for merging.

## 0.31.8

Released 2023-10-16

  * Restart integration after a push to the target branch. This is to prevent
    waiting on CIs that are not up to date anymore.
  * Respond with 204 (NoContent) instead of 501 (UnImplemented) when responding to
    unsupported event hook types sent by Github. This is to prevent Github from
    interpreting the responses as errors.
  * Log unsupported event hook types to STDOUT>.


## 0.31.7

Released 2023-10-10.

 * Increased internal event queue sizes from 10 to 128 to reduce the chance of
   Hoff dropping events when receiving dozens of webhook events at the exact
   same time.
 * When Hoff drops a webhook event because its internal event queue is full, it
   will now log the evenet to STDOUT>

## 0.31.6

Released 2023-10-05.

 * Fix fat-fingering the number version in the cabal file.

## 0.31.5

Released 2023-10-05.

 * Avoid corrupting the internal project state when a `merge` command is issued
   multiple times. Now, issuing a `merge` command on a pull request that is in
   progress of being integrated, the entire pull request's state is reset and
   the new `merge` command is processed.

## 0.31.4

Released 2023-06-09.

 * Fixed a regression from 0.31.3 where links in Hoff's messages are not
   properly displayed on GitHub.

## 0.31.3

Released 2023-06-07.

 * Changed the build system to use plain Cabal instead of Stack. Check the
   readme for more details.
 * Prevent Hoff from replying to its own feedback messages. This is relevant
   when the command prefix is set to the bot's username, and the merge command
   was posted from that same account. Before this change Hoff would reply with a
   harmless but annoying parser error message.

## 0.31.2

Released 2023-05-23.

 * Fixed a regression from 0.31.0 where Hoff would enter a feedback loop when
   posting a parser error message, as those error messages also contain Hoff's
   command prefix.

## 0.31.1

Released 2023-05-22.

 * Fixed mismatching version numbers in the Nix build.

## 0.31.0

Released 2023-05-22.

 * Added a new, stricter parser that posts clear error messages when the comment
   contains invalid merge commands. This avoids situations like `@hoffbot merge
   and deploy to foobar` causing a deploy to the default environment as Hoff
   would previously simply match the `@hoffbot merge and deploy` part. Merge
   commands now need to be either on their own line, or at the end of a line
   optionally followed by some punctuation.
 * There is now a `@hoffbot retry` command to retry merges with failing test.
   This is equivalent to closing the PR, reopening the PR, and then asking the
   bot to initiate the merge again, but in a single step and without actually
   closing the PR first.

## 0.30.0

Released 2023-05-15.

 * Hoff no longer prints the internal state any time it receives an event.
 * Hoff's internals have been rewritten using
   [effectful](https://hackage.haskell.org/package/effectful).

## 0.19.2

Released 2021-05-02.

 * Updated the Nix builds to use GHC 9.2.

## 0.19.1

Released 2021-04-12.

 * Fixed URLs to tag pages on GitHub.

## 0.19.0

Released 2021-02-12.

 * Hoff now interprets instructions left through a pull request review, so you
   can say “@bot merge” directly from a review. This applies to the overall
   review body, not to individual review comments left on the diff. Hoff ignores
   the status of the review (approval, changes requested, etc.), it only looks
   for the “@bot merge” command.

## 0.18.0

Released 2021-01-27.

**Compatibility**:

 * The state serialization format of 0.18.0 is incompatible with 0.17.0. The
   recommended way to update is to stop Hoff 0.17.0 at a quiet time when no
   builds are in progress, delete the state files, and start 0.18.0. Hoff will
   scan for open pull requests at startup, but approval status will be lost.

   The new version of Hoff will automatically create a new state file using the
   new format when run.

Changes:

 - Add `Auto-deploy: false` as a Git trailer to merge commit messages of PRs
   approved with the "{prefix} merge" command.

   This change does not affect single-commit PRs, as these are merged by
   fast-forwarding the target branch and don't have a merge message.

 - Add support for a new merge command "{prefix} merge and deploy".

   When this command is used Hoff will perform a rebase and merge in the same
   way as is done in response to the "merge" command.

   Unlike the "merge" command, Hoff will
     - Always create a merge commit, even for single-commit PRs, and
     - Append `Auto-deploy: true` (instead of `Auto-deploy: false`) to the
       commit message of the resulting merge commit.

       This information can later be used in a CI/CD pipeline to trigger an
       automatic deploy.

## 0.17.0

Released 2021-01-06.

 * Include pull request title in the merge commit message.

## 0.16.0

Released 2020-08-05.

**Compatibility**:

 * The state serialization format of 0.16.0 is incompatible with 0.15.0. The
   recommended way to update is to stop Hoff 0.15.0 at a quiet time when no
   builds are in progress, delete the state files, and start 0.16.0. Hoff will
   scan for open pull requests at startup, but approval status will be lost.

Bugfixes:

 * Hoff will no longer incorrectly leave a comment that it will rebase a pull
   request after re-approving a pull request that failed to build. It now
   re-reports the build failure in the reply instead. Close and re-open the pull
   request to clear the build status.

## 0.15.0

Released 2020-06-29.

**Compatibility**:

 * The state serialization format of 0.15.0 is incompatible with 0.14.0. The
   recommended way to update is to stop Hoff 0.14.0 at a quiet time when no
   builds are in progress, delete the state files, and start 0.15.0. Hoff will
   scan for open pull requests at startup, but approval status will be lost.

Other changes:

 * The bot user will now leave a comment when it abandons a pull request that
   was being integrated.
 * Unhandled exceptions now crash the process, instead of only the thread. This
   means failures are now loud, rather than processing slowly grindinding to a
   halt due to bounded queues filling up.
 * Fix a crash that could happen when pushing a previously successfully rebased
   pull request failed (because something else was pushed in the meantime), and
   a new rebase attempt ended with a conflict.

## 0.14.0

Released 2020-03-27.

 * Fix formatting typo in the comment that the bot leaves on a failed rebase.

## 0.13.0

 * The bot user will now comment with detailed instructions on how to rebase,
   if the automated rebase fails.
 * Synchronize the state with GitHub at startup. It is no longer necessary to
   close and reopen a pull request if a webhook delivery was missed, restarting
   Hoff should bring everything in sync again.
 * The binary now accepts `--read-only`, which will prevent disruptive side
   effects such as pushing and leaving comments, but it will still pull, and
   make API calls to the GitHub API that only read data. This is useful for
   local development, or for a dry run of a new setup.

## 0.12.0

 * **Compatibility**: The schema of the state files has changed. Hoff v0.12.0
   can read v0.11.0 state files, so an upgrade is seamless, but a downgrade
   would require manual intervention.
 * Fix bug where Hoff would stop to try merging, after pushing to master fails
   because something else was pushed to master meanwhile.

## 0.11.0

 * Accept merge command anywhere in comments.
 * Take dependencies from Stackage LTS 14.21, up from 9.0.

## 0.10.0

 * Do not delete branches after merging a pull request, GitHub has that
   functionality now, and it can interfere with dependent pull requests.
 * Add overview pages that show everything in all repositories of an owner
   on one page.
 * Fix a bug in the queue position comment that the bot leaves.

## Older versions

 * TODO: Backfill the changelog.
