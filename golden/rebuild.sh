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

	rm -rf "${test}/expected"
	scmdoc "${test}"/*.scm -o "${test}"/expected

	find "${test}/expected" -name '*.html' \
		-exec tidy --wrap 0 --indent yes \
		--show-body-only yes -o {} {} \; 2>/dev/null
done
