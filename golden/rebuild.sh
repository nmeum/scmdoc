#!/bin/sh
set -e

prettify() {
	tidy \
		--wrap 0 \
		--indent yes \
		--show-body-only yes \
		"$@" 2>/dev/null
}

for test in *; do
	[ -d "${test}" ] || continue

	scmdoc "${test}/input.scm" -o - | prettify \
	        > "${test}/expected.html"
done
