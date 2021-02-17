# Changelog

## 0.19.1

Released 2021-02-17.

 * Fix a bug that prevented pull request review webhooks from being processed.

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
