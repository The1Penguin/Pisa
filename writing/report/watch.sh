#! /usr/bin/env bash

pid=""
stop() {
  kill "$pid" # Send sigterm to make
  wait "$pid" # Wait for gracefull stop
}
trap stop TERM

# Watch all relevant files
inotifywait \
  --event close_write,delete \
  --include '.*\.(bib|tex|png)$' \
  --monitor --recursive src \
| while : # Always process the file updates
do
  if read -rt 0.1 r
  then : # Consume all pending input
  else
    if [[ "$?" -ne 142 ]]
    then break; fi # Stop if EOF

    # Build in the background
    make --no-builtin-rules pdf &
    pid="$!"
    read -r r # Await next update
    stop # Stop build if it's still running
  fi
done
