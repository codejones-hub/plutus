#!/usr/bin/env bash

set -e

if [ -z "$PR_NUMBER" ] ; then
   echo "[ci-plutus-benchmark]: 'PR_NUMBER' is not set! Exiting"
   exit 1
fi
echo "[ci-plutus-benchmark]: Processing benchmark comparison for PR $PR_NUMBER"

echo "[ci-plutus-benchmark]: Updating cabal database ..."
cabal update

echo "[ci-plutus-benchmark]: Running benchmark for PR branch ..."
cabal bench plutus-benchmark:validation >bench-PR.log 2>&1

echo "[ci-plutus-benchmark]: Switching branches ..."
git checkout "$(git merge-base HEAD master)"

echo "[ci-plutus-benchmark]: Running benchmark for base branch ..."
cabal bench plutus-benchmark:validation >bench-base.log 2>&1

echo "[ci-plutus-benchmark]: Comparing results ..."
./plutus-benchmark/bench-compare bench-base.log bench-PR.log >bench-compare-result.log
nix-shell -p jq --run "jq -Rs '.' bench-compare-result.log >bench-compare.json"

echo "[ci-plutus-benchmark]: Posting results to GitHub ..."
curl -s -H "Authorization: token $(</run/keys/buildkite-github-token)" -X POST -d "{\"body\": $(<bench-compare.json)}" "https://api.github.com/repos/input-output-hk/plutus/issues/${PR_NUMBER}/comments"
