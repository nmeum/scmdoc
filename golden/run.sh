#!/bin/sh

cd "$(dirname "$0")"

abort() {
	printf "${1}\n" 1>&2
	exit 1
}

if ! command -v scmdoc 1>/dev/null; then
	abort "Error: Couldn't find 'scmdoc' in \$PATH" 1>&2
elif ! command -v tidy 1>/dev/null; then
	abort "Error: Couldn't find 'tidy' in \$PATH" 1>&2
fi

TESTDIR="/tmp/scmdoc-test"

mkdir -p "${TESTDIR}"
trap "rm -rf '${TESTDIR}'" INT EXIT

for test in *; do
	[ -d "${test}" ] || continue

	name="${test##*/}"
	printf "Running test case '%s': " "${name}"

	scmdoc "${test}"/*.scm -o "${TESTDIR}"
	find "${TESTDIR}" -name '*.html' \
		-exec tidy --wrap 0 --indent yes \
		--show-body-only yes -o {} {} \; 2>/dev/null

	diff=$(diff -ur "${test}/expected" "${TESTDIR}")
	if [ $? -ne 0 ]; then
		printf "FAIL: Output differs.\n\n"
		printf "%s\n" "${diff}"
		exit 1
	fi

	rm -r "${TESTDIR}"/*
	printf "OK.\n"
done
