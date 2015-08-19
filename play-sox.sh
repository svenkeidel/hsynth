#!/bin/sh

$@ | play -t raw -b 16 -e signed -c 1 -r 48000 -
