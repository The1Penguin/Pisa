#! /usr/bin/env bash

coproc entr -n echo
trap 'kill $COPROC_PID' EXIT

cat <(ls runner.sh) \
    <(fd '\.json$') \
    <(fd '\.hs$') \
    <(fd '\.lean$' ../lean) \
  >&${COPROC[1]}
exec {COPROC[1]}>&-

start=`date -r "$0" +%s`
echo "Starting reloader: $start" >&2

files="Examples.List Examples.Bool Examples.Test Examples.Nat"
with="$files : Examples.Bool.B Examples.Bool.not Examples.Bool.and"

while read -u ${COPROC[0]} r; do
  if [ "$(date -r "$0" +%s)" -gt "$start" ]
  then
    kill $COPROC_PID
    exec bash "$0"
  fi

  echo -n "reload $(date --iso) ... " >&2
  if cabal build &> err
  then
    echo >> err
    sh -c "cd ../lean; pisa-lean $with" 2>> err \
      | jq \
      | "$(cabal list-bin pisa 2>/dev/null | head -n 1)" > out 2>> err
  fi

  echo "done" >&2
done
