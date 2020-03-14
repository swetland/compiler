#!/bin/bash

## Copyright 2020, Brian Swetland <swetland@frotz.net>
## Licensed under the Apache License, Version 2.0.

src="$1"
txt="$2"
bin="${txt%.txt}.bin"
lst="${txt%.txt}.lst"
log="${txt%.txt}.log"
msg="${txt%.txt}.msg"
gold="${src%.src}.log"

#echo "RUNTEST: $src: compiling..."
if bin/compiler -o "$bin" -l "$lst" "$src" 2> "$msg"; then
	# success!
	if [[ "$txt" == *"-err"* ]]; then
		# but this was an error test, so...
		echo "RUNTEST: $src: FAIL: compiler did not detect error"
		echo "FAIL: $src" > "$txt"
	else
		#echo "RUNTEST: $src: running..."
		if bin/r5e "$bin" > "$log"; then
			if diff "$log" "$gold" >/dev/null ; then
				echo "RUNTEST: $src: PASS"
				echo "PASS: $src" > "$txt"
			else
				echo "RUNTEST: $src: FAIL: output differs from expected"
				diff "$log" "$gold" | head
				echo "FAIL: %src" > "$txt"
			fi
		else
			echo "RUNTEST: $src: FAIL: emulator crashed"
			echo "FAIL: %src" > "$txt"
		fi
	fi
else
	# failure
	if [[ "$txt" == *"-err"* ]]; then
		# but this was an error test, so...
		echo "RUNTEST: $src: PASS"
		echo "PASS: $src" > "$txt"
	else
		echo "RUNTEST: $src: FAIL: compiler error"
		echo "FAIL: $src" > "$txt"
		cat "$msg"
	fi
fi

