#!/bin/bash
# finddate.sh
# author:  Luke Lin    phone:  457-5047           24 June 1998
# author:  Daniel Wesloh                          30 January 2024
# abstract:  This script looks in ether forward or backward in time to
# generate  either a variable containing sequential date/time stamps
# for a period up to a month or just the date/time stamp occurring
# at the end of such a period.
# Time stamp is in the form yyyyddmm.  The script should be good for many
# years. Leap years are accounted for.  Years go 1998, 1999, 2000, 2001,
#   2002, 2003, ....
# etc.
#
# usage:  examples assume today's date is 19990929.
# To generate a sequence looking 10 days forward then execute:
#     list=`sh /nwprod/util/scripts/finddate.sh 19990929 s+10`
# To generate just the date/time 10 days from now then execute:
#     list=`sh /nwprod/util/scripts/finddate.sh 19990929 d+10`
# To generate a sequence looking 10 days backward then execute:
#     list=`sh /nwprod/util/scripts/finddate.sh 19990929 s-10`
# To generate just the date/time 10 days ago then execute:
#     list=`sh /nwprod/util/scripts/finddate.sh 19990929 d-10`
# list will contain 10 time stamps starting with 19990929.  Time stamps
# are separated by blanks.
set +x

# Takes four-digit year as argument
# Returns 0/true if argument is leap year
# Returns 1/false if argument is not leap year
function isleap() {
    local -i year="10#$1"
    local -i isleap=$((${year} % 4 == 0))
    if [ $((${year} % 100)) -eq 0 ]
    then
	isleap=$((${year} % 400 == 0))
    fi
    test "${isleap}" -eq 1
}

# for year in $(seq 1995 2005) $(seq 2095 2105);
# do
#     if isleap "${year}";
#     then
# 	echo "${year} is a leap year"
#     else
# 	echo "${year} is not a leap year"
#     fi
# done


# Takes four-digit year and two-digit month as argument
# Prints days in that month to stdout
function days_per_month() {
    local -i year="10#$1"
    local -i month="10#$2"
    case "${month}" in
	1|3|5|7|8|10|12)
	    echo 31
	    ;;
	4|6|9|11)
	    echo 30
	    ;;
	2)
	    if isleap "${year}"
	    then
		echo 29
	    else
		echo 28
	    fi
	    ;;
	*)
	    exit 1
    esac
}

# for year_month in $(seq 202101 202112) $(seq 199502 100 200502) 210002;
# do
#     echo -n "Days in ${year_month}: "
#     days_per_month $(($year_month / 100)) $(($year_month % 100))
# done

# Takes four-digit year, month, day and days ahead as arguments
# Prints date the given number of days after the given date in YYYYMMDD format to stdout
function n_days_ahead() {
    local -i year="10#$1"
    local -i month="10#$2"
    local -i ndays="10#$4"
    local -i day=$((10#$3 + ${ndays}))

    local -i month_days="$(days_per_month "${year}" "${month}")"
    while [ "${day}" -gt "${month_days}" ];
    do
	month=$((${month} + 1))
	day=$((${day} - ${month_days}))

	if [ "${month}" -gt 12 ];
	then
	    year=$((${year} + 1))
	    month=$((${month} - 12))
	fi

	month_days="$(days_per_month "${year}" "${month}")"
    done

    while [ "${day}" -lt "1" ];
    do
	month=$((${month} - 1))

	if [ "${month}" -lt "1" ];
	then
	    year=$((${year} - 1))
	    month=$((${month} + 12))
	fi

	day=$(( ${day} + $(days_per_month "${year}" "${month}") ))
    done

    printf '%04d%02d%02d' "${year}" "${month}" "${day}"
}

# for startday in 20210501 20191201;
# do
#     for ndays in $(seq 55 65) $(seq 85 95) 365 730;
#     do
# 	echo -n "${ndays} days after ${startday} is "
# 	n_days_ahead "${startday:0:4}" "${startday:4:2}" "${startday:6:2}" "${ndays}"
# 	echo
#     done
#     echo
#
#     for ndays in $(seq 55 65) $(seq 85 95) 365 730;
#     do
# 	echo -n "${ndays} days before ${startday} is "
# 	n_days_ahead "${startday:0:4}" "${startday:4:2}" "${startday:6:2}" "-${ndays}"
# 	echo
#     done
#     echo
# done

function sequence_n_days_ahead() {
    local -i year="10#${1}"
    local -i month="10#${2}"
    local -i day="10#${3}"
    local -i ndays="10#${4}"

    if [ "${ndays}" -ge 0 ];
    then
	local -i month_days="$(days_per_month "${year}" "${month}")"
	for (( days_so_far=0 ; ${days_so_far} < ${ndays} ; days_so_far=${days_so_far} + 1 ));
	do
	    # day=$((${day} + 1))

	    # if [ "${day}" -gt "${month_days}" ];
	    # then
	    # 	day=$((${day} - ${month_days}))
	    # 	month=$((${month} + 1))

	    # 	if [ "${month}" -gt 12 ];
	    # 	then
	    # 	    month=$((${month} - 12))
	    # 	    year=$((${year} + 1))
	    # 	fi

	    # 	month_days="$(days_per_month "${year}" "${month}")"
	    # fi
	    # printf -v date_so_far '%04d%02d%02d ' "${year}" "${month}" "${day}"

	    local -i date_so_far="$(n_days_ahead "${year}" "${month}" "${day}" 1)"
	    printf '%08d ' "${date_so_far}"

	    year="10#${date_so_far:0:4}"
	    month="10#${date_so_far:4:2}"
	    day="10#${date_so_far:6:2}"
	done
    else
	for (( days_so_far=0 ; ${days_so_far} > ${ndays} ; days_so_far=${days_so_far} - 1));
	do
	    # day=$((${day} - 1))

	    # if [ "${day}" -lt 1 ];
	    # then
	    # 	month=$((${month} - 1))

	    # 	if [ "${month}" -lt 1 ];
	    # 	then
	    # 	    year=$((${year} - 1))
	    # 	    month=$((${month} + 12))
	    # 	fi
	    # 	day=$((${day} + $(days_per_month "${year}" "${month}")))
	    # fi
	    # printf '%04d%02d%02d ' "${year}" "${month}" "${day}"

	    local -i date_so_far="$(n_days_ahead "${year}" "${month}" "${day}" -1)"
	    printf '%08d ' "${date_so_far}"

	    year="10#${date_so_far:0:4}"
	    month="10#${date_so_far:4:2}"
	    day="10#${date_so_far:6:2}"
	done
    fi
}

# for start_date in 20210531;
# do
#     echo "The 32 dates starting with ${start_date}"
#     sequence="$(sequence_n_days_ahead "${start_date:0:4}" "${start_date:4:2}" "${start_date:6:2}"  32)"
#     echo "${sequence}: $(echo "${sequence}" | wc -w) dates"

#     echo "The 32 dates before ${start_date}"
#     sequence="$(sequence_n_days_ahead "${start_date:0:4}" "${start_date:4:2}" "${start_date:6:2}" -32)"
#     echo "${sequence}: $(echo "${sequence}" | wc -w) dates"
#     echo
# done

# Copy of finddate.sh
# Prints date or date sequence a given number of days from given date
# Given date is YYYYMMDD format
function finddate() {
    local -i year_month_day="${1}"
    local -i year="10#${year_month_day:0:4}"
    local -i month="10#${year_month_day:4:2}"
    local -i day="10#${year_month_day:6:2}"

    local second_arg="${2}"
    local day_or_sequence="${second_arg:0:1}"
    local -i ndays="${second_arg:1}"

    if [ "${day_or_sequence}" = "d" ];
    then
	n_days_ahead "${year}" "${month}" "${day}" "${ndays}"
    else
	sequence_n_days_ahead "${year}" "${month}" "${day}" "${ndays}"
    fi
    echo
}

# finddate 19990929 s+10
# echo
# finddate 19990929 d+10
# echo
# finddate 19990929 s-10
# echo
# finddate 19990929 d-10
# echo

# finddate 19990929 d+366
# echo
# finddate 19990929 d+3653
# echo
# finddate 19990929 d+7305
# echo

finddate "$1" "$2"
