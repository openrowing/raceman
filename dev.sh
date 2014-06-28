#!/bin/sh
mkdir -p log
erl -sname raceman -pa $PWD/ebin -pa $PWD/deps/*/ebin -boot start_sasl -s lager -s raceman
