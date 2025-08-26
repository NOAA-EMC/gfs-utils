#!/bin/bash

######################################################################################

# GENERATE MONTHLY/DAILY MEAN GRIB2 FILES ON INTERPOLATED 360-x181 LAT-LON  GRID 
# FOR SFS MASTER DATA FILES

# THIS SCRIPT WILL BE CALLED FROM G-W JOBS, WHERE THE FOLLOWING VARIABLES WILL BE
# PREDETERIMED:

###  memdir: path to directory with the SFS master files for a member
###  gmerge: path to gmerge (from wgrib2) executable file
###  monthly_dir: path to directory where monthly means will be saved

#####################################################################################

firstfile=$memdir/sfs.t00z.master.grb2f000

# get validation date of first file
vt_init=$(wgrib2 $firstfile -d 1 -vt)
mm_init=${vt_init:11:2}`

#### Set index for finding month of validation date for loops
months_in_year=("1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12")
start_idx=$(($(($mm_init))-1))

#### check for leap year
for i in {1..12}
do
  itime=$(wgrib2 -t $firstfile|head -1|cut -d= -f2)
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

daysf=0   # day no. at end of month

# loop from valid date month to end of calendar year
for (( i=$start_idx; i<${#months_in_year[@]}; i++))
do
  daysf=$((daysf+month_days_in_year[i]))
  daysi=$((daysf-month_days_in_year[i]))

  fhi=$(($(($daysi*24+6))))  # initial fhr for start of month (acc values)
  fhiinst=$(($(($fhi-6))))   # initial fhr for start of month (inst values)
  fhf=$(($(($daysf))*24))    # final fhr for end of month

  ### Make list of files for the whole month
  ### For FCST MONTHLY, 6 hours less on the FIRST file
  list=`seq -f $memdir/sfs.t00z.master.grb2f%03.0f $fhi 6 $fhf`
  listinst=`seq -f $memdir/sfs.t00z.master.grb2f%03.0f $fhiinst 6 $fhf`

  filedate=month_${months_in_year[$i]}

  #merge the min/max variables into daily periods
  $gmerge - $list | wgrib2 - -match ' (ave|min|max|acc) ' -merge_fcst 4 $monthly_dir/acc_daily/IN.grb

  # get the monthly averages of the daily min/max values (these are already interpolated)
  wgrib2 $monthly_dir/acc_daily/IN.grb -fcst_ave 24hr $monthly_dir/acc_monthly/IN.grb

  #interpolate
  wgrib2 $monthly_dir/acc_daily/IN.grb | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i $monthly_dir/acc_daily/IN.grb -grib $monthly_dir/acc_daily/OUT.grb
  wgrib2 $monthly_dir/acc_daily/OUT.grb -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 $monthly_dir/acc_daily/acc_daily_${filedate}.grb
  wgrib2 $monthly_dir/acc_monthly/IN.grb | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i $monthly_dir/acc_monthly/IN.grb -grib $monthly_dir/acc_monthly/OUT.grb
  wgrib2 $monthly_dir/acc_monthly/OUT.grb -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 $monthly_dir/acc_monthly/acc_monthly_${filedate}.grb

  rm $monthly_dir/acc_daily/IN.grb
  rm $monthly_dir/acc_daily/OUT.grb
  rm $monthly_dir/acc_monthly/IN.grb
  rm $monthly_dir/acc_monthly/OUT.grb

  # monthly averages for instantaneous forecasts (not ave,acc,min,max)
  $gmerge - $listinst | wgrib2 - -not ' (ave|min|max|acc) ' -fcst_ave 6hr $monthly_dir/inst_monthly/IN.grb
  
  #interpolate
  wgrib2 $monthly_dir/inst_monthly/IN.grb | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i $monthly_dir/inst_monthly/IN.grb -grib $monthly_dir/inst_monthly/OUT.grb                                 
  wgrib2 $monthly_dir/inst_monthly/OUT.grb -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 $monthly_dir/inst_monthly/inst_monthly_${filedate}.grb               

  rm $monthly_dir/inst_monthly/IN.grb
  rm $monthly_dir/inst_monthly/OUT.grb


  # daily averages for instantaneous variables
  for j in $(seq $fhi 24 $fhf)
  do
    start_hr=$(($(($j-6))))
    end_hr=$(($(($j+24-6))))
    list_6hrly=`seq -f $memdir/sfs.t00z.master.grb2f%03.0f $start_hr 6 $end_hr`
    $gmerge - $list_6hrly | wgrib2 - -match  "MSLET|PRMSL|PWAT|PRES:surface|TMP:2 m above|:TMP:surface|SPFH:2 m above|DPT:2 m above|UGRD:10 m above|VGRD:10 m above|HGT:(2|10|50|100|200|500|700|850|1000) mb|(PVORT|TMP):(450|550|650) K|(UGRD|VGRD):(2|10|50|100|200|500|600|700|850|925|1000) mb|SPFH:(100|200|300|500|600|700|850|925|1000) mb|VVEL:500 mb|TMP:(2|10|50|100|200|250|300|500|600|700|850|925|1000) mb|TOZNE|ICEC|ICETK|(TSOIL|SOILW):(0-0.1|0.1-0.4|0.4-1|1-2) m|WEASD|PEVPR|LAND|HGT:surface|CSDLF:surface|CSDSF:surface|CSUSF:surface|NDDSF:surface|VDDSF:surface|SOILM:0-0.2|TMP:1 hybrid" -fcst_ave 6hr $monthly_dir/inst_daily/daily_${end_hr}.grb
  done

  daily_start=$(($(($fhf+24-6))))
  daily_end=$(($(($fhi-6))))
  list_daily=`ls -v $monthly_dir/inst_daily/daily_*.grb`

  #### merge all days into single grib2 file (use end_hr for this) and remove uneeded files
  $gmerge - $list_daily > $monthly_dir/inst_daily/IN.grb
  rm $monthly_dir/inst_daily/daily*.grb

  #interpolate
  #interpolate
  wgrib2 $monthly_dir/inst_daily/IN.grb | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i $monthly_dir/inst_daily/IN.grb -grib $monthly_dir/inst_daily/OUT.grb                          
  wgrib2 $monthly_dir/inst_daily/OUT.grb -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 $monthly_dir/inst_daily/inst_daily_${filedate}.grb               

  rm $monthly_dir/inst_daily/IN.grb
  rm $monthly_dir/inst_daily/OUT.grb


done


# loop from start of calendar year to valid date month
for (( i=0; i<$start_idx; i++))
do
  daysf=$((daysf+month_days_in_year[i]))
  daysi=$((daysf-month_days_in_year[i]))

  fhi=$(($(($daysi*24+6))))  # initial fhr for start of month (acc values)
  fhiinst=$(($(($fhi-6))))   # initial fhr for start of month (inst values)
  fhf=$(($(($daysf))*24))    # final fhr for end of month

  ### Make list of files for the whole month
  ### For FCST MONTHLY, 6 hours less on the FIRST file
  list=`seq -f $memdir/sfs.t00z.master.grb2f%03.0f $fhi 6 $fhf`
  listinst=`seq -f $memdir/sfs.t00z.master.grb2f%03.0f $fhiinst 6 $fhf`

  filedate=month_${months_in_year[$i]}

  #merge the min/max variables into daily periods
  $gmerge - $list | wgrib2 - -match ' (ave|min|max|acc) ' -merge_fcst 4 $monthly_dir/acc_daily/IN.grb

  # get the monthly averages of the daily min/max values (these are already interpolated)
  wgrib2 $monthly_dir/acc_daily/IN.grb -fcst_ave 24hr $monthly_dir/acc_monthly/IN.grb

  #interpolate
  wgrib2 $monthly_dir/acc_daily/IN.grb | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i $monthly_dir/acc_daily/IN.grb -grib $monthly_dir/acc_daily/OUT.grb
  wgrib2 $monthly_dir/acc_daily/OUT.grb -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 $monthly_dir/acc_daily/acc_daily_${filedate}.grb
  wgrib2 $monthly_dir/acc_monthly/IN.grb | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i $monthly_dir/acc_monthly/IN.grb -grib $monthly_dir/acc_monthly/OUT.grb
  wgrib2 $monthly_dir/acc_monthly/OUT.grb -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 $monthly_dir/acc_monthly/acc_monthly_${filedate}.grb

  rm $monthly_dir/acc_daily/IN.grb
  rm $monthly_dir/acc_daily/OUT.grb
  rm $monthly_dir/acc_monthly/IN.grb
  rm $monthly_dir/acc_monthly/OUT.grb

  # monthly averages for instantaneous forecasts (not ave,acc,min,max)
  $gmerge - $listinst | wgrib2 - -not ' (ave|min|max|acc) ' -fcst_ave 6hr $monthly_dir/inst_monthly/IN.grb
  
  #interpolate
  wgrib2 $monthly_dir/inst_monthly/IN.grb | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i $monthly_dir/inst_monthly/IN.grb -grib $monthly_dir/inst_monthly/OUT.grb                                 
  wgrib2 $monthly_dir/inst_monthly/OUT.grb -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 $monthly_dir/inst_monthly/inst_monthly_${filedate}.grb               

  rm $monthly_dir/inst_monthly/IN.grb
  rm $monthly_dir/inst_monthly/OUT.grb


  # daily averages for instantaneous variables
  for j in $(seq $fhi 24 $fhf)
  do
    start_hr=$(($(($j-6))))
    end_hr=$(($(($j+24-6))))
    list_6hrly=`seq -f $memdir/sfs.t00z.master.grb2f%03.0f $start_hr 6 $end_hr`
    $gmerge - $list_6hrly | wgrib2 - -match  "MSLET|PRMSL|PWAT|PRES:surface|TMP:2 m above|:TMP:surface|SPFH:2 m above|DPT:2 m above|UGRD:10 m above|VGRD:10 m above|HGT:(2|10|50|100|200|500|700|850|1000) mb|(PVORT|TMP):(450|550|650) K|(UGRD|VGRD):(2|10|50|100|200|500|600|700|850|925|1000) mb|SPFH:(100|200|300|500|600|700|850|925|1000) mb|VVEL:500 mb|TMP:(2|10|50|100|200|250|300|500|600|700|850|925|1000) mb|TOZNE|ICEC|ICETK|(TSOIL|SOILW):(0-0.1|0.1-0.4|0.4-1|1-2) m|WEASD|PEVPR|LAND|HGT:surface|CSDLF:surface|CSDSF:surface|CSUSF:surface|NDDSF:surface|VDDSF:surface|SOILM:0-0.2|TMP:1 hybrid" -fcst_ave 6hr $monthly_dir/inst_daily/daily_${end_hr}.grb
  done

  daily_start=$(($(($fhf+24-6))))
  daily_end=$(($(($fhi-6))))
  list_daily=`ls -v $monthly_dir/inst_daily/daily_*.grb`

  #### merge all days into single grib2 file (use end_hr for this) and remove uneeded files
  $gmerge - $list_daily > $monthly_dir/inst_daily/IN.grb
  rm $monthly_dir/inst_daily/daily*.grb

  #interpolate
  #interpolate
  wgrib2 $monthly_dir/inst_daily/IN.grb | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i $monthly_dir/inst_daily/IN.grb -grib $monthly_dir/inst_daily/OUT.grb                          
  wgrib2 $monthly_dir/inst_daily/OUT.grb -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 $monthly_dir/inst_daily/inst_daily_${filedate}.grb               

  rm $monthly_dir/inst_daily/IN.grb
  rm $monthly_dir/inst_daily/OUT.grb

done

