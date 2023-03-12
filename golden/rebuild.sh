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
	scmdoc "${test}/input.scm" -o "${test}"/expected

	for file in "${test}/expected"/*.html; do
		prettify -o "${file}" "${file}"
	done
done
