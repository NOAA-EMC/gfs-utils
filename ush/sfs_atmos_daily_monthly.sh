#!/bin/bash

######################################################################################

# GENERATE MONTHLY/DAILY MEAN GRIB2 FILES ON INTERPOLATED 360-x181 LAT-LON  GRID 
# FOR SFS MASTER DATA FILES

# THIS SCRIPT WILL BE CALLED FROM G-W JOBS, WHERE THE FOLLOWING VARIABLES WILL BE
# PREDETERIMED:

###  MEMDIR: path to directory with the SFS master files for a member
###  CC: cycle of MEMDIR data
###  ENS: ensemble member of MEMDIR data
###  GMERGE: path to gmerge executable file
###  OUTDIR: path to directory where monthly means will be saved

#####################################################################################

firstfile=$MEMDIR/sfs.t${CC}z.master.grb2f000

# get validation date of first file
vt_init=$(wgrib2 $firstfile -d 1 -vt)
vt_date=${vt_init:7:10}  # for filename
yy_init=${vt_init:7:4}
yy_init_next=$((yy_init+1))
mm_init=${vt_init:11:2}

# set filenames for valid date year and following year
filename_start="${ENS}.${vt_date}.${yy_init}"
filename_start_next="${ENS}.${vt_date}.${yy_init_next}"
filename_end=".grib.${CC}Z.grb2"

#### Set index for finding month of validation date for loops
months_in_year=("01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12")
start_idx=$((mm_init-1))

#### check for leap year
itime=$(wgrib2 -t $firstfile|head -1|cut -d= -f2)
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

daysf=0   # day no. at end of month

# loop from valid date month to end of calendar year
for (( i=start_idx; i<${#months_in_year[@]}; i++ ))
do
  daysf=$((daysf+month_days_in_year[i]))
  daysi=$((daysf-month_days_in_year[i]))

  fhi=$((daysi*24+6))  # initial fhr for start of month (acc values)
  fhiinst=$((fhi-6))   # initial fhr for start of month (inst values)
  fhf=$((daysf*24))    # final fhr for end of month

  ### Make list of files for the whole month
  ### For FCST MONTHLY, 6 hours less on the FIRST file
  list=$(seq -f $MEMDIR/sfs.t00z.master.grb2f%03.0f $fhi 6 $fhf)
  listinst=$(seq -f $MEMDIR/sfs.t00z.master.grb2f%03.0f $fhiinst 6 $fhf)
  
  # month of loop for filename
  filemm="${months_in_year[$i]}"

  #merge the min/max variables into daily periods
  $GMERGE - $list | wgrib2 - -match ' (ave|min|max|acc) ' -merge_fcst 4 $OUTDIR/acc.daily.${ENS}/IN.grb

  # get the monthly averages of the daily min/max values, which are already interpolated
  wgrib2 $OUTDIR/acc.daily.${ENS}/IN.grb -fcst_ave 24hr $OUTDIR/acc.monthly.${ENS}/IN.grb

  #interpolate
  wgrib2 $OUTDIR/acc.daily.${ENS}/IN.grb | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i $OUTDIR/acc.daily.${ENS}/IN.grb -grib $OUTDIR/acc.daily.${ENS}/OUT.grb
  wgrib2 $OUTDIR/acc.daily.${ENS}/OUT.grb -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 $OUTDIR/acc.daily.${ENS}/acc.daily.${filename_start}${filemm}${filename_end}
  wgrib2 $OUTDIR/acc.monthly.${ENS}/IN.grb | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i $OUTDIR/acc.monthly.${ENS}/IN.grb -grib $OUTDIR/acc.monthly.${ENS}/OUT.grb
  wgrib2 $OUTDIR/acc.monthly.${ENS}/OUT.grb -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 $OUTDIR/acc.monthly.${ENS}/acc.monthly.${filename_start}${filemm}${filename_end}

  rm $OUTDIR/acc.daily.${ENS}/IN.grb
  rm $OUTDIR/acc.daily.${ENS}/OUT.grb
  rm $OUTDIR/acc.monthly.${ENS}/IN.grb
  rm $OUTDIR/acc.monthly.${ENS}/OUT.grb

  # monthly averages for instantaneous forecasts 
  $GMERGE - $listinst | wgrib2 - -not ' (ave|min|max|acc) ' -fcst_ave 6hr $OUTDIR/inst.monthly.${ENS}/IN.grb
  
  # interpolate
  wgrib2 $OUTDIR/inst.monthly.${ENS}/IN.grb | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i $OUTDIR/inst.monthly.${ENS}/IN.grb -grib $OUTDIR/inst.monthly.${ENS}/OUT.grb                                 
  wgrib2 $OUTDIR/inst.monthly.${ENS}/OUT.grb -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 $OUTDIR/inst.monthly.${ENS}/inst.monthly.${filename_start}${filemm}${filename_end}

  rm $OUTDIR/inst.monthly.${ENS}/IN.grb
  rm $OUTDIR/inst.monthly.${ENS}/OUT.grb

  # daily averages for instantaneous variables
  for j in $(seq $fhi 24 $fhf)
  do
    start_hr=$((j-6))
    end_hr=$((j+24-6))
    list_6hrly=$(seq -f $MEMDIR/sfs.t00z.master.grb2f%03.0f $start_hr 6 $end_hr)
    $GMERGE - $list_6hrly | wgrib2 - -match  "MSLET|PRMSL|PWAT|PRES:surface|TMP:2 m above|:TMP:surface|SPFH:2 m above|DPT:2 m above|UGRD:10 m above|VGRD:10 m above|HGT:(2|10|50|100|200|500|700|850|1000) mb|(PVORT|TMP):(450|550|650) K|(UGRD|VGRD):(2|10|50|100|200|500|600|700|850|925|1000) mb|SPFH:(100|200|300|500|600|700|850|925|1000) mb|VVEL:500 mb|TMP:(2|10|50|100|200|250|300|500|600|700|850|925|1000) mb|TOZNE|ICEC|ICETK|(TSOIL|SOILW):(0-0.1|0.1-0.4|0.4-1|1-2) m|WEASD|PEVPR|LAND|HGT:surface|CSDLF:surface|CSDSF:surface|CSUSF:surface|NDDSF:surface|VDDSF:surface|SOILM:0-0.2|TMP:1 hybrid" -fcst_ave 6hr $OUTDIR/inst.daily.${ENS}/daily_${end_hr}.grb
  done

  list_daily=$(ls -v $OUTDIR/inst.daily.${ENS}/daily_*.grb)

  #### merge all days into single grib2 file and remove unneeded files
  $GMERGE - $list_daily > $OUTDIR/inst.daily.${ENS}/IN.grb
  rm $OUTDIR/inst.daily.${ENS}/daily*.grb

  #interpolate
  wgrib2 $OUTDIR/inst.daily.${ENS}/IN.grb | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i $OUTDIR/inst.daily.${ENS}/IN.grb -grib $OUTDIR/inst.daily.${ENS}/OUT.grb                          
  wgrib2 $OUTDIR/inst.daily.${ENS}/OUT.grb -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 $OUTDIR/inst.daily.${ENS}/inst.daily.${filename_start}${filemm}${filename_end}

  rm $OUTDIR/inst.daily.${ENS}/IN.grb
  rm $OUTDIR/inst.daily.${ENS}/OUT.grb

done

# loop from start of calendar year to valid date month
for (( i=0; i<start_idx; i++ ))
do
  daysf=$((daysf+month_days_in_year[i]))
  daysi=$((daysf-month_days_in_year[i]))

  fhi=$((daysi*24+6))  # initial fhr for start of month (acc values)      
  fhiinst=$((fhi-6))   # initial fhr for start of month (inst values)      
  fhf=$((daysf*24))    # final fhr for end of month   

  ### Make list of files for the whole month
  ### For FCST MONTHLY, 6 hours less on the FIRST file
  list=$(seq -f $MEMDIR/sfs.t00z.master.grb2f%03.0f $fhi 6 $fhf)
  listinst=$(seq -f $MEMDIR/sfs.t00z.master.grb2f%03.0f $fhiinst 6 $fhf)

  # month of loop for filename
  filemm="${months_in_year[$i]}"

  #merge the min/max variables into daily periods
  $GMERGE - $list | wgrib2 - -match ' (ave|min|max|acc) ' -merge_fcst 4 $OUTDIR/acc.daily.${ENS}/IN.grb

  # get the monthly averages of the daily min/max values, which are already interpolated
  wgrib2 $OUTDIR/acc.daily.${ENS}/IN.grb -fcst_ave 24hr $OUTDIR/acc.monthly.${ENS}/IN.grb

  # interpolate
  wgrib2 $OUTDIR/acc.daily.${ENS}/IN.grb | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i $OUTDIR/acc.daily.${ENS}/IN.grb -grib $OUTDIR/acc.daily.${ENS}/OUT.grb
  wgrib2 $OUTDIR/acc.daily.${ENS}/OUT.grb -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 $OUTDIR/acc.daily.${ENS}/acc.daily.${filename_start_next}${filemm}${filename_end}
  wgrib2 $OUTDIR/acc.monthly.${ENS}/IN.grb | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i $OUTDIR/acc.monthly.${ENS}/IN.grb -grib $OUTDIR/acc.monthly.${ENS}/OUT.grb
  wgrib2 $OUTDIR/acc.monthly.${ENS}/OUT.grb -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 $OUTDIR/acc.monthly.${ENS}/acc.monthly.${filename_start_next}${filemm}${filename_end}

  rm $OUTDIR/acc.daily.${ENS}/IN.grb
  rm $OUTDIR/acc.daily.${ENS}/OUT.grb
  rm $OUTDIR/acc.monthly.${ENS}/IN.grb
  rm $OUTDIR/acc.monthly.${ENS}/OUT.grb

  # monthly averages for instantaneous forecasts 
  $GMERGE - $listinst | wgrib2 - -not ' (ave|min|max|acc) ' -fcst_ave 6hr $OUTDIR/inst.monthly.${ENS}/IN.grb
  
  # interpolate
  wgrib2 $OUTDIR/inst.monthly.${ENS}/IN.grb | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i $OUTDIR/inst.monthly.${ENS}/IN.grb -grib $OUTDIR/inst.monthly.${ENS}/OUT.grb                                 
  wgrib2 $OUTDIR/inst.monthly.${ENS}/OUT.grb -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 $OUTDIR/inst.monthly.${ENS}/inst.monthly.${filename_start_next}${filemm}${filename_end}

  rm $OUTDIR/inst.monthly.${ENS}/IN.grb
  rm $OUTDIR/inst.monthly.${ENS}/OUT.grb

  # daily averages for instantaneous variables
  for j in $(seq $fhi 24 $fhf)
  do
    start_hr=$((j-6))
    end_hr=$((j+24-6)) 
    list_6hrly=$(seq -f $MEMDIR/sfs.t00z.master.grb2f%03.0f $start_hr 6 $end_hr)
    $GMERGE - $list_6hrly | wgrib2 - -match  "MSLET|PRMSL|PWAT|PRES:surface|TMP:2 m above|:TMP:surface|SPFH:2 m above|DPT:2 m above|UGRD:10 m above|VGRD:10 m above|HGT:(2|10|50|100|200|500|700|850|1000) mb|(PVORT|TMP):(450|550|650) K|(UGRD|VGRD):(2|10|50|100|200|500|600|700|850|925|1000) mb|SPFH:(100|200|300|500|600|700|850|925|1000) mb|VVEL:500 mb|TMP:(2|10|50|100|200|250|300|500|600|700|850|925|1000) mb|TOZNE|ICEC|ICETK|(TSOIL|SOILW):(0-0.1|0.1-0.4|0.4-1|1-2) m|WEASD|PEVPR|LAND|HGT:surface|CSDLF:surface|CSDSF:surface|CSUSF:surface|NDDSF:surface|VDDSF:surface|SOILM:0-0.2|TMP:1 hybrid" -fcst_ave 6hr $OUTDIR/inst.daily.${ENS}/daily_${end_hr}.grb
  done

  list_daily=$(ls -v $OUTDIR/inst.daily.${ENS}/daily_*.grb)

  #### merge all days into single grib2 file and remove unneeded files
  $GMERGE - $list_daily > $OUTDIR/inst.daily.${ENS}/IN.grb
  rm $OUTDIR/inst.daily.${ENS}/daily*.grb

  # interpolate
  wgrib2 $OUTDIR/inst.daily.${ENS}/IN.grb | sed -e 's/:UFLX:/:UFLXa:/' -e 's/:VFLX:/:UFLXb:/' | sort -t: -k3,3 -k6n,6 -k5,5 -k4,4 | wgrib2 -i $OUTDIR/inst.daily.${ENS}/IN.grb -grib $OUTDIR/inst.daily.${ENS}/OUT.grb                          
  wgrib2 $OUTDIR/inst.daily.${ENS}/OUT.grb -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 $OUTDIR/inst.daily.${ENS}/inst.daily.${filename_start_next}${filemm}${filename_end}

  rm $OUTDIR/inst.daily.${ENS}/IN.grb
  rm $OUTDIR/inst.daily.${ENS}/OUT.grb

done

