#!/bin/bash

cd $(cd -P -- "$(dirname -- "$0")" && pwd -P)

echo "Setting up leiningen"
if [ ! -f lein ];
then
    wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
    chmod +x lein
fi
export PATH=$PWD:$PATH

echo "Running detector"
lein run

echo "Submit alerts to medusa"
python python/sender.py
