#!/bin/sh

cd "$(dirname "$0")"

abort() {
	printf "${1}\n" 1>&2
	exit 1
}

prettify() {
	tidy \
		--wrap 0 \
		--indent yes \
		--show-body-only yes \
		"$@" 2>/dev/null
}

TESTDIR="/tmp/edward-test"
mkdir -p "${TESTDIR}"
trap "rm -rf '${TESTDIR}'" INT EXIT

for test in *; do
	[ -d "${test}" ] || continue

	name="${test##*/}"
	printf "Running test case '%s': " "${name}"

	diff=$(scmdoc "${test}/input.scm" -o - | \
		prettify | \
		diff -u "${test}/expected.html" -)
	if [ $? -ne 0 ]; then
		printf "FAIL: Output differs.\n\n"
		printf "%s\n" "${diff}"
		exit 1
	fi

	printf "OK.\n"
done
