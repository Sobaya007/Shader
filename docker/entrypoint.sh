#!/bin/bash

dub build dscanner
dub build dcd
export PATH=$(dub describe dscanner --data=target-path --data-list):$PATH
export PATH=$(dub describe dcd --data=target-path --data-list):$PATH

exec "$@"
