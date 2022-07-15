#!/bin/bash

# Shared commands for the set-up phase of jobs

# This file is NOT usable with Semaphore's "commands_file" property, as this
# puts all the comments in this file in the build output and requires us to
# write all build steps on one line each. Instead, this file must be executed
# in the build pipelines where we use it.

# We don't use `set -x` here to show the run commands, because this would show
# authentication tokens in the build output. Instead, `set -v` prints the
# commands "as they are read".
# We do want to exit immediately after encountering an error.
# We also cannot use "set -u", as Semaphore's tooling uses unset variables in
# some places (which we can't change).
set -evo pipefail

# Clear disk space by removing docker related services, mounts and files.
sudo systemctl stop docker.service docker.socket

if [[ $(findmnt /var/lib/docker) ]]; then
  sudo umount /var/lib/docker
fi

if [[ -e "/dev/nbd0" ]]; then
  sudo qemu-nbd --disconnect /dev/nbd0
fi

# Clear disk space by removing tooling that we don't use. This is needed to run
# on the smallest machine type Semaphore offers, as this machine type has too
# little disk space to build the package due to a lot of extra tooling that we
# don't need. Removing these directories clears about 4 GiB.
sudo rm -rf \
    /home/semaphore/.rbenv \
    /home/semaphore/.kerl \
    /home/semaphore/.nvm \
    /home/semaphore/.phpbrew \
    /home/semaphore/.kiex \
    /opt/google \
    /opt/firefox-esr \
    /opt/firefox-esr-prev \
    /usr/local/golang \
    /mnt/docker.qcow2

# Originally found on channable/requestmachine/.semaphore/prologue.sh
mount_nix_store() {
  # Before we can successfully restore to `/nix` it needs to be created and owned
  # by the CI user. Without this, the `cache restore` command fails because it
  # doesn't have permission to create `/nix`. (We cannot run the cache restore
  # command as `root` as it takes settings from environment variables.)
  # We use the local scratch SSD mounted at `/mnt` to prevent running out of disk
  # space, as the Nix store takes up about 10 GiB when uncompressed, and a build
  # on the smallest Semaphore instance starts out with only 13 GiB of disk space,
  # of which 4 GiB is used by the cloned repository.
  # The SSD has (at the time of writing) 80 GiB of space, of which 32 GiB is free
  # when a build starts. If we really want to we can also consider wiping the SSD
  # before using it, we don't need to do this yet.
  sudo mkdir -p /mnt/nix /nix
  sudo mount --bind /mnt/nix /nix
  sudo chown -R semaphore: /nix
}
mount_nix_store

# Attempt to restore the Semaphore cache entry for `/nix`.
#
# We have this in addition to Cachix because we want to avoid hitting Cachix
# for individual store entries, as restoring `/nix` from the Semaphore cache in
# one go is a lot faster than downloading individual cache entries from
# Cachix's S3 + Cloudflare.
#
# We refresh the Nix store cache entry daily. It is populated after the first
# successful build of the day by our main pipeline.
#
# Restoring the cache can fail when the cache entry is only partially matched,
# because then it might still be in the process of being uploaded by Semaphore,
# which can be caused by a concurrent build. Since using the Semaphore cache is
# only an optimization, but not strictly necessary, we make sure that the build
# doesn't fail in this case, by making sure the exit code is always 0. When
# restoring the cache fails, we delete /nix to ensure that we are not left with
# a partially restored Nix store.

# Note: Uses /home/semaphore/.toolbox/cache instead of /usr/bin/cache. This is
# because the latter refers to /home/semaphore/.toolbox/new-cache, which is a
# reimplementation of the cache using Go. This implementation sometimes locks up
# during unpacking.
/home/semaphore/.toolbox/cache restore "nix-store-$(date -u -Idate),nix-store-$(date -u -Idate --date=yesterday),nix-store-" || {
  sudo umount /nix
  sudo rm -fr /mnt/nix
  mount_nix_store
}
