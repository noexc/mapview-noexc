#!/usr/bin/env bash

set -e

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"
cd "$cwd/.."
f=`mktemp -d`
git clone git@github.com:noexc/mapview-noexc.git "$f/mapview-noexc.git"
cabal haddock
pushd "$f/mapview-noexc.git"
  git checkout gh-pages
  set +e
    git rm -rf *
  set -e
popd
mv dist/doc/html/mapview-noexc/* "$f/mapview-noexc.git"
pushd "$f/mapview-noexc.git"
  git add -A
  git commit -m "Manual docs deploy."
  git push origin gh-pages
popd
rm -rf "$f"

if [ $? == 0 ]; then
  echo "*** Done: https://noexc.github.io/mapview-noexc/"
  exit 0
else
  echo "*** ERROR!!! Fix the above and try again."
  exit 1
fi
