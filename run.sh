#!/bin/bash

cd $(cd -P -- "$(dirname -- "$0")" && pwd -P)

echo "Running detector"
lein run

echo "Submit alerts to medusa"
python python/sender.py
