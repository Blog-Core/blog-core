#!/usr/bin/env bash

swipl -s tests/server.pl -- --port=8888 --fork=false
