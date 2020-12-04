#!/bin/bash

read -r -d  '' filter <<EOS
.members | to_entries | sort_by(.value.local_score)[].value | { name, local_score, id, "cdls": .completion_day_level | map_values(. | map_values(.get_star_ts | tonumber | todate ))}
EOS
jq --sort-keys "$filter"
