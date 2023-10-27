#!/usr/bin/bash

run6502 -l F000 $1 -d F000 +C00 \
-M E000 -G E010 -P E020 -R F000 -X 0


