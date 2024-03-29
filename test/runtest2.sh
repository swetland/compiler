#!/bin/bash

## Copyright 2020, Brian Swetland <swetland@frotz.net>
## Licensed under the Apache License, Version 2.0.

src="$1"
txt="$2"
bin="${txt%.txt}.bin"
lst="${txt%.txt}.lst"
ast="${txt%.txt}.ast"
log="${txt%.txt}.log"
msg="${txt%.txt}.msg"
gold="${src%.src}.log"

#echo "RUNTEST2: $src: compiling..."
#echo bin/cast -o "$bin" -l "$lst" "$src" "$msg"
if bin/cast -o "$bin" -a "$ast" -l "$lst" "$src" 2> "$msg"; then
	# success!
	if [[ "$txt" == *"-err"* ]]; then
		# but this was an error test, so...
		echo "RUNTEST2: $src: FAIL: compiler did not detect error"
		echo "FAIL: $src" > "$txt"
	else
		#echo "RUNTEST: $src: running..."
		if bin/r5e "$bin" > "$log"; then
			if diff "$log" "$gold" >/dev/null ; then
				echo "RUNTEST2: $src: PASS"
				echo "PASS: $src" > "$txt"
			else
				echo "RUNTEST2: $src: FAIL: output differs from expected"
				diff "$log" "$gold" | head
				echo "FAIL: %src" > "$txt"
			fi
		else
			echo "RUNTEST2: $src: FAIL: emulator crashed"
			echo "FAIL: %src" > "$txt"
		fi
	fi
else
	if [[ $? > 127 ]]; then
		echo "RUNTEST2: $src: FAIL: compiler crashed"
		cat "$msg"
	# failure
	elif [[ "$txt" == *"-err"* ]]; then
		# but this was an error test, so...
		echo "RUNTEST2: $src: PASS"
		echo "PASS: $src" > "$txt"
	else
		echo "RUNTEST2: $src: FAIL: compiler error"
		echo "FAIL: $src" > "$txt"
		cat "$msg"
	fi
fi

