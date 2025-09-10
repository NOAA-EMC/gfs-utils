#!/bin/bash

###########################################################################################

# GENERATE MONTHLY/DAILY MEAN GRIB2 FILES ON INTERPOLATED 360-x181 LAT-LON  GRID 
# FOR SFS MASTER 6-HOURLY DATA FILES. THIS SCRIPT GENERATES 4 DIFFERENT KINDS OF DATASETS:
# DAILY AND MONTHLY GRIB2 FILES FOR ACC/AVE/MIN/MAX AND INSTANTANEOUS VALUES
# FOR ACC/AVE/MIN/MAX VARIABLES, THE DAILY ACC/AVE/MIN/MAX IS FIRST COMPUTED FROM
# THE 6-HOURLY AND THE MONTHLY IS COMPUTED FROM THOSE DAILY ACC/AVE/MIN/MAX RESULTS

# THIS SCRIPT WILL BE CALLED FROM G-W JOBS, WHERE THE FOLLOWING VARIABLES WILL BE
# PREDETERIMED:

###  MEMDIR: path to directory with the SFS master files for a member
###  CC: cycle of MEMDIR data
###  ENS: ensemble member of MEMDIR data
###  GMERGE: path to gmerge executable file
###  OUTDIR: path to directory where monthly means will be saved

##########################################################################################

firstfile="${MEMDIR}/sfs.t${CC}z.master.grb2f000"
lastfile=$(ls -v "${MEMDIR}"/sfs.t${CC}z.master.grb2f* | tail -1)

# get validation date of first file
vt_init=$(wgrib2 "${firstfile}" -d 1 -vt)
vt_date=${vt_init:7:10}  # for filename
yy_init=${vt_init:7:4}
yy_init_next=$((yy_init+1))
mm_init=${vt_init:11:2}

# get dates and times of last file
lastftimemsg=$(wgrib2 "${lastfile}" -d 1 -ftime2)
lastftime=$(echo "${lastftimemsg% hour fcst}")
lastfhr=${lastftime:4:4}
vt_final=$(wgrib2 "${lastfile}" -d 1 -vt)
mm_final=${vt_final:11:2}
dd_final=${vt_final:13:2}

# set filenames for valid date year and following year
filename_start="${ENS}.${vt_date}.${yy_init}"
filename_start_next="${ENS}.${vt_date}.${yy_init_next}"
filename_end=".grib.${CC}Z.grb2"

#### Set indexes for finding months of validation date for loops
months_in_year=("01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12")
start_idx=$((mm_init-1))

# if the last file vt date ends on day 01, do not loop over it.
if (( dd_final == 01 )); then
  end_idx=$((mm_final-2))
else
  end_idx=$((mm_final-1))
fi

#### check for leap year
itime=$(wgrib2 -t "${firstfile}"|head -1|cut -d= -f2)
for i in {1..12}
do
  yyyy=${itime:0:4}
  mm=${itime:4:2}

  ndays=$(date -d "$yyyy-$mm-01 +$i month -1 day" "+%d")

  if (( "$ndays"==28 )); then
    month_days_in_year=("31" "28" "31" "30" "31" "30" "31" "31" "30" "31" "30" "31")
    break
  fi
  if (( "$ndays"==29 )); then
    month_days_in_year=("31" "29" "31" "30" "31" "30" "31" "31" "30" "31" "30" "31")
    break
  fi 
done

### If the end month is higher than start month, loop once. Otherwise loop twice for
### start month to end of year and beginning of year to start month
if (( start_idx < end_idx )); then
  end_loop_idx=$end_idx  # one loop, start to end month
else
  end_loop_idx=$((${#months_in_year[@]}-1)) # there will be two loops, the first one from start month to end of year
fi

daysf=0   # day no. at end of month

# loop from valid date start month to end of calendar year OR end month
for (( i=start_idx; i<end_loop_idx+1; i++ ))
do
  daysf=$((daysf+month_days_in_year[i]))
  daysi=$((daysf-month_days_in_year[i]))

  fhi=$((daysi*24+6))  # initial fhr for start of month (acc values)
  fhiinst=$((fhi-6))   # initial fhr for start of month (inst values)
  fhf=$((daysf*24))    # final fhr for end of month

  # make sure the last fhr exists
  if [ "$fhf" -gt "$lastfhr" ]; then
    fhf=$lastfhr
  fi

  ### Make list of files for the whole month
  ### For instantaneous values, 6 hours less on the FIRST file, no need for acc time interval
  list=$(seq -f "${MEMDIR}/sfs.t${CC}z.master.grb2f%03.0f" $fhi 6 $fhf)
  listinst=$(seq -f "${MEMDIR}/sfs.t${CC}z.master.grb2f%03.0f" $fhiinst 6 $fhf)
  
  # month of loop for filename
  filemm="${months_in_year[$i]}"

  #merge the min/max/acc/ave variables into daily periods
  # shellcheck disable=SC2086
  # shellcheck disable=SC2086
  ${GMERGE} - ${list} | wgrib2 - -match ' (ave|min|max|acc) ' -merge_fcst 4 "${OUTDIR}/acc.daily.${ENS}/IN.grb"

  # get the monthly averages of the daily min/max/acc/ave values
  wgrib2 "${OUTDIR}/acc.daily.${ENS}/IN.grb" -fcst_ave 24hr "${OUTDIR}/acc.monthly.${ENS}/IN.grb"

  #interpolate: bilinear for most, except acc/ave precipitation variables which use budget interpolation
  wgrib2 "${OUTDIR}/acc.daily.${ENS}/IN.grb" | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i "${OUTDIR}/acc.daily.${ENS}/IN.grb" -grib "${OUTDIR}/acc.daily.${ENS}/OUT.grb"
  wgrib2 "${OUTDIR}/acc.daily.${ENS}/OUT.grb" -if ':(PRATE|CPRAT|CPOFP|TSNOWP|ACPCP|APCP|NCPCP):' -new_grid_interpolation budget -fi -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 "${OUTDIR}/acc.daily.${ENS}/acc.daily.${filename_start}${filemm}${filename_end}"
  wgrib2 "${OUTDIR}/acc.monthly.${ENS}/IN.grb" | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i "${OUTDIR}/acc.monthly.${ENS}/IN.grb" -grib "${OUTDIR}/acc.monthly.${ENS}/OUT.grb"
  wgrib2 "${OUTDIR}/acc.monthly.${ENS}/OUT.grb" -if ':(PRATE|CPRAT|CPOFP|TSNOWP|ACPCP|APCP|NCPCP):' -new_grid_interpolation budget -fi -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 "${OUTDIR}/acc.monthly.${ENS}/acc.monthly.${filename_start}${filemm}${filename_end}"

  rm "${OUTDIR}/acc.daily.${ENS}/IN.grb"
  rm "${OUTDIR}/acc.daily.${ENS}/OUT.grb"
  rm "${OUTDIR}/acc.monthly.${ENS}/IN.grb"
  rm "${OUTDIR}/acc.monthly.${ENS}/OUT.grb"

  # monthly averages for instantaneous forecasts 
  # shellcheck disable=SC2086
  # shellcheck disable=SC2086
  ${GMERGE} - ${listinst} | wgrib2 - -not ' (ave|min|max|acc) ' -fcst_ave 6hr "${OUTDIR}/inst.monthly.${ENS}/IN.grb"
  
  # interpolate: bilinear 
  wgrib2 "${OUTDIR}/inst.monthly.${ENS}/IN.grb" | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i "${OUTDIR}/inst.monthly.${ENS}/IN.grb" -grib "${OUTDIR}/inst.monthly.${ENS}/OUT.grb"                                 
  wgrib2 "${OUTDIR}/inst.monthly.${ENS}/OUT.grb" -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 "${OUTDIR}/inst.monthly.${ENS}/inst.monthly.${filename_start}${filemm}${filename_end}"

  rm "${OUTDIR}/inst.monthly.${ENS}/IN.grb"
  rm "${OUTDIR}/inst.monthly.${ENS}/OUT.grb"

  # daily averages for instantaneous variables
  for j in $(seq $fhi 24 $fhf)
  do
    start_hr=$((j-6))
    end_hr=$((j+24-6))
    list_6hrly=$(seq -f "${MEMDIR}/sfs.t${CC}z.master.grb2f%03.0f" $start_hr 6 $end_hr)
    # shellcheck disable=SC2086
    ${GMERGE} - ${list_6hrly} | wgrib2 - -match  "MSLET|PRMSL|PWAT|PRES:surface|TMP:2 m above|:TMP:surface|SPFH:2 m above|DPT:2 m above|UGRD:10 m above|VGRD:10 m above|HGT:(2|10|50|100|200|500|700|850|1000) mb|(PVORT|TMP):(450|550|650) K|(UGRD|VGRD):(2|10|50|100|200|500|600|700|850|925|1000) mb|SPFH:(100|200|300|500|600|700|850|925|1000) mb|VVEL:500 mb|TMP:(2|10|50|100|200|250|300|500|600|700|850|925|1000) mb|TOZNE|ICEC|ICETK|(TSOIL|SOILW):(0-0.1|0.1-0.4|0.4-1|1-2) m|WEASD|PEVPR|LAND|HGT:surface|CSDLF:surface|CSDSF:surface|CSUSF:surface|NDDSF:surface|VDDSF:surface|SOILM:0-0.2|TMP:1 hybrid" -fcst_ave 6hr "${OUTDIR}/inst.daily.${ENS}/daily_${end_hr}.grb"
  done

  list_daily=$(ls -v "${OUTDIR}"/inst.daily."${ENS}"/daily_*.grb)

  #### merge all days into single grib2 file and remove unneeded files
  # shellcheck disable=SC2086
  ${GMERGE} - ${list_daily} > "${OUTDIR}/inst.daily.${ENS}/IN.grb"
  rm "${OUTDIR}"/inst.daily."${ENS}"/daily*.grb

  #interpolate: bilinear 
  wgrib2 "${OUTDIR}/inst.daily.${ENS}/IN.grb" | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i "${OUTDIR}/inst.daily.${ENS}/IN.grb" -grib "${OUTDIR}/inst.daily.${ENS}/OUT.grb"                          
  wgrib2 "${OUTDIR}/inst.daily.${ENS}/OUT.grb" -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 "${OUTDIR}/inst.daily.${ENS}/inst.daily.${filename_start}${filemm}${filename_end}"

  rm "${OUTDIR}/inst.daily.${ENS}/IN.grb"
  rm "${OUTDIR}/inst.daily.${ENS}/OUT.grb"

done

### This second loop needs to be done if the end month is earlier
### than the start month or the same (e.g., full year run)
if (( start_idx==end_idx )) || (( end_idx < start_idx )); then

# loop from start of calendar year to valid date month
for (( i=0; i<end_idx+1; i++ ))
do
  daysf=$((daysf+month_days_in_year[i]))
  daysi=$((daysf-month_days_in_year[i]))

  fhi=$((daysi*24+6))  # initial fhr for start of month (acc values)      
  fhiinst=$((fhi-6))   # initial fhr for start of month (inst values)      
  fhf=$((daysf*24))    # final fhr for end of month   

  # make sure the last fhr exists
  if [ "$fhf" -gt "$lastfhr" ]; then
    fhf=$lastfhr
  fi

  ### Make list of files for the whole month
  ### For instantaneous values, 6 hours less on the FIRST file, no need for acc time interval
  list=$(seq -f "${MEMDIR}/sfs.t${CC}z.master.grb2f%03.0f" $fhi 6 $fhf)
  listinst=$(seq -f "${MEMDIR}/sfs.t${CC}z.master.grb2f%03.0f" $fhiinst 6 $fhf)

  # month of loop for filename
  filemm="${months_in_year[$i]}"

  #merge the min/max/acc/ave variables into daily periods
  # shellcheck disable=SC2086
  ${GMERGE} - ${list} | wgrib2 - -match ' (ave|min|max|acc) ' -merge_fcst 4 "${OUTDIR}/acc.daily.${ENS}/IN.grb"

  # get the monthly averages of the daily min/max/acc/ave values
  wgrib2 "${OUTDIR}/acc.daily.${ENS}/IN.grb" -fcst_ave 24hr "${OUTDIR}/acc.monthly.${ENS}/IN.grb"

  #interpolate: bilinear for most, except acc/ave precipitation variables which use budget interpolation
  wgrib2 "${OUTDIR}/acc.daily.${ENS}/IN.grb" | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i "${OUTDIR}/acc.daily.${ENS}/IN.grb" -grib "${OUTDIR}/acc.daily.${ENS}/OUT.grb"
  wgrib2 "${OUTDIR}/acc.daily.${ENS}/OUT.grb" -if ':(PRATE|CPRAT|CPOFP|TSNOWP|ACPCP|APCP|NCPCP):' -new_grid_interpolation budget -fi -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 "${OUTDIR}/acc.daily.${ENS}/acc.daily.${filename_start_next}${filemm}${filename_end}"
  wgrib2 "${OUTDIR}/acc.monthly.${ENS}/IN.grb" | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i "${OUTDIR}/acc.monthly.${ENS}/IN.grb" -grib "${OUTDIR}/acc.monthly.${ENS}/OUT.grb"
  wgrib2 "${OUTDIR}/acc.monthly.${ENS}/OUT.grb" -if ':(PRATE|CPRAT|CPOFP|TSNOWP|ACPCP|APCP|NCPCP):' -new_grid_interpolation budget -fi -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 "${OUTDIR}/acc.monthly.${ENS}/acc.monthly.${filename_start_next}${filemm}${filename_end}"

  rm "${OUTDIR}/acc.daily.${ENS}/IN.grb"
  rm "${OUTDIR}/acc.daily.${ENS}/OUT.grb"
  rm "${OUTDIR}/acc.monthly.${ENS}/IN.grb"
  rm "${OUTDIR}/acc.monthly.${ENS}/OUT.grb"

  # monthly averages for instantaneous forecasts 
  # shellcheck disable=SC2086
  ${GMERGE} - ${listinst} | wgrib2 - -not ' (ave|min|max|acc) ' -fcst_ave 6hr "${OUTDIR}/inst.monthly.${ENS}/IN.grb"
  
  # interpolate: bilinear
  wgrib2 "${OUTDIR}/inst.monthly.${ENS}/IN.grb" | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i "${OUTDIR}/inst.monthly.${ENS}/IN.grb" -grib "${OUTDIR}/inst.monthly.${ENS}/OUT.grb" 
  wgrib2 "${OUTDIR}/inst.monthly.${ENS}/OUT.grb" -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 "${OUTDIR}/inst.monthly.${ENS}/inst.monthly.${filename_start_next}${filemm}${filename_end}"

  rm "${OUTDIR}/inst.monthly.${ENS}/IN.grb"
  rm "${OUTDIR}/inst.monthly.${ENS}/OUT.grb"

  # daily averages for instantaneous variables
  for j in $(seq $fhi 24 $fhf)
  do
    start_hr=$((j-6))
    end_hr=$((j+24-6)) 
    list_6hrly=$(seq -f "${MEMDIR}/sfs.t${CC}z.master.grb2f%03.0f" $start_hr 6 $end_hr)
    # shellcheck disable=SC2086
    ${GMERGE} - ${list_6hrly} | wgrib2 - -match  "MSLET|PRMSL|PWAT|PRES:surface|TMP:2 m above|:TMP:surface|SPFH:2 m above|DPT:2 m above|UGRD:10 m above|VGRD:10 m above|HGT:(2|10|50|100|200|500|700|850|1000) mb|(PVORT|TMP):(450|550|650) K|(UGRD|VGRD):(2|10|50|100|200|500|600|700|850|925|1000) mb|SPFH:(100|200|300|500|600|700|850|925|1000) mb|VVEL:500 mb|TMP:(2|10|50|100|200|250|300|500|600|700|850|925|1000) mb|TOZNE|ICEC|ICETK|(TSOIL|SOILW):(0-0.1|0.1-0.4|0.4-1|1-2) m|WEASD|PEVPR|LAND|HGT:surface|CSDLF:surface|CSDSF:surface|CSUSF:surface|NDDSF:surface|VDDSF:surface|SOILM:0-0.2|TMP:1 hybrid" -fcst_ave 6hr "${OUTDIR}/inst.daily.${ENS}/daily_${end_hr}.grb"
  done

  list_daily=$(ls -v "${OUTDIR}"/inst.daily."${ENS}"/daily_*.grb)

  #### merge all days into single grib2 file and remove unneeded files
  # shellcheck disable=SC2086
  ${GMERGE} - ${list_daily} > "${OUTDIR}/inst.daily.${ENS}/IN.grb"
  rm "${OUTDIR}"/inst.daily."${ENS}"/daily*.grb

  # interpolate: bilinear 
  wgrib2 "${OUTDIR}/inst.daily.${ENS}/IN.grb" | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i "${OUTDIR}/inst.daily.${ENS}/IN.grb" -grib "${OUTDIR}/inst.daily.${ENS}/OUT.grb"                          
  wgrib2 "${OUTDIR}/inst.daily.${ENS}/OUT.grb" -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 "${OUTDIR}/inst.daily.${ENS}/inst.daily.${filename_start_next}${filemm}${filename_end}"

  rm "${OUTDIR}/inst.daily.${ENS}/IN.grb"
  rm "${OUTDIR}/inst.daily.${ENS}/OUT.grb"

done

fi  # end of if block for checking end month vs. start month
