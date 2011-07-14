#!/usr/bin/env bash

default_filters="$(dirname $0)/patterns.ini"
for profile in `ls ~/.mozilla/firefox/*/prefs.js 2> /dev/null`
do
  adblock=${profile%prefs.js}adblockplus
  mkdir -p $adblock
  cp -i $default_filters $adblock/patterns.ini
done

cat << EOF
Done ! don't forget :
1. install adblockplus to use those filters
2. go to the adblockplus' preferences and update all filters to download the filter lists
EOF
