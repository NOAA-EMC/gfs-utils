#!/bin/bash

echo TAP version 14

declare -i test_num=1
function tap_log_test() {
    local -i status="$1"
    local message="$2"

    if [ "${status}" -ne 0 ]
    then
	echo -n 'not '
    fi

    echo "ok ${test_num} - ${message}"
    test_num=$((${test_num} + 1))
}

declare -i subtest_num=1
declare -i subtest_suite_result=0
function tap_log_subtest() {
    local -i status="$1"
    local message="$2"
    echo -n '    '

    if [ "${status}" -ne 0 ]
    then
	echo -n 'not '
	subtest_suite_result=$((${subtest_suite_result} + ${status}))
    fi

    echo "ok ${subtest_num} - ${message}"
    subtest_num=$((${subtest_num} + 1))
}

function tap_start_subtest() {
    local name="${1}"
    local -i ntests="${2}"
    echo "# Subtest: ${name}"
    echo "    1..${ntests}"
    subtest_num=1
    subtest_suite_result=0
}

function tap_end_subtest() {
    local name="${1}"
    local -i status="${subtest_suite_result}"
    
    if [ "${status}" -ne 0 ];
    then
	echo -n 'not '
    fi

    echo "ok ${test_num} - ${name}"
    test_num=$((${test_num} + 1))
}

############################################################

echo 1..5

# Comment out the output from loading the test functions
echo -n '# '
. test_find_date.sh 19990929 d+10

tap_start_subtest isleap 22
for year in $(seq 1995 2005) $(seq 2095 2105);
do
    if echo 1996 2000 2004 2096 2104 | grep --quiet -F -e ${year} -;
    then
	expected=0
    else
	expected=1
    fi

    if isleap "${year}";
    then
	actual=0
    else
	actual=1
    fi

    if [ "${expected}" -eq "${actual}" ];
    then
	result=0
    else
	result=1
    fi

    tap_log_subtest "${result}" "Year ${year}"
done
tap_end_subtest isleap

tap_start_subtest days_per_month 24

for year_month_days in 20210131 20210228 20210331 20210430 20210531 \
    20210630 20210731 20210831 20210930 20211031 20211130 20211231 \
    19950228 19960229 19990228 20000229 20010228 20040229 21000228;
do
    declare -i year="${year_month_days:0:4}"
    declare -i month="10#${year_month_days:4:2}"
    declare -i ndays="10#${year_month_days:6:2}"
    declare -i month_days="10#$(days_per_month "${year}" "${month}")"
    if [ "${month_days}" -eq "${ndays}" ];
    then
	result=0
    else
	result=1
    fi
    tap_log_subtest "${result}" "${year}-${month} expected ${ndays} got ${month_days}"
done

tap_end_subtest days_per_month

tap_start_subtest n_days_ahead 6
start_date=19990929
for days_year in 03662000 -3651998 07312001 -7301997 36532009 73052019;
do
    declare -i ndays="10#${days_year:0:4}"
    declare -i expected="10#${days_year:4}0929"
    actual="$(n_days_ahead "${start_date:0:4}" "${start_date:4:2}" "${start_date:6:2}" "${ndays}")"

    if [ "${actual}" -eq "${expected}" ];
    then
	result=0
    else
	result=1
    fi
    tap_log_subtest "${result}" \
	"${ndays} days from ${start_date}: exptected ${expected} got ${actual}"
done
tap_end_subtest n_days_ahead

tap_start_subtest sequence_n_days_ahead 24
declare -i start_date=19990929
declare -i start_year="10#${start_date:0:4}"
declare -i start_month="10#${start_date:4:2}"
declare -i start_day="10#${start_date:6:2}"
for sign in '' '-';
do
    for ndays in $(seq 1 2 10) 400;
    do
	actual="$(sequence_n_days_ahead "${start_year}" "${start_month}" "${start_day}" "${sign}${ndays}")"
	nwords="$(echo "${actual}" | wc -w)"
	if [ "${nwords}" -eq "${ndays}" ];
	then
	    result=0
	else
	    result=1
	fi
	tap_log_subtest "${result}" "Sequence length: ${sign}${ndays}"

	actual_last_date="$(echo "${actual}" | awk '{ print $NF; }')"
	expected_last_date="$(n_days_ahead "${start_year}" "${start_month}" "${start_day}" "${sign}${ndays}")"
	if [ "${actual_last_date}" = "${expected_last_date}" ];
	then
	    result=0
	else
	    result=1
	fi
	tap_log_subtest "${result}" \
	    "Last date in sequence: expected ${expected_last_date} actual ${actual_last_date}"
    done
done
tap_end_subtest sequence_n_days_ahead
