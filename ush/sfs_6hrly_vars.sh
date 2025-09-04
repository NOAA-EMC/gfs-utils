#!/bin/bash

######################################################################################

# GENERATE 6-HOURLY GRIB2 FILES ON INTERPOLATED 360-x181 LAT-LON  GRID 
# FOR SELECT SFS VARIABLES

# THIS SCRIPT READS ALL GRIB2 FILES FOR A GIVEN SFS FORECAST PERIOD TO GENERATE
# A SINGLE FILE PER VARIABLE WITH ALL 6-HOURLY FORECASTS

# THIS SCRIPT WILL BE CALLED FROM G-W JOBS, WHERE THE FOLLOWING VARIABLES WILL BE
# PREDETERIMED:

###  MEMDIR: path to directory with the SFS master files for a member
###  CC: cycle of MEMDIR data
###  ENS: ensemble member of MEMDIR data
###  OUTDIR: path to directory where 6hourly files will be saved

#####################################################################################

declare -a vars=( "DLWRF:surface" "DSWRF:surface" "ULWRF:surface" "USWRF:surface" "ULWRF:top of atmosphere" "LHTFL" "SHTFL" "PRMSL" "PRATE" ":TMP:2 m above" "TMAX:2 m above" "TMIN:2 m above" "DPT:2 m above" "HGT:200 mb" "HGT:500 mb" "HGT:700 mb" "HGT:850 mb" "SPFH:500 mb" "SPFH:700 mb" "SPFH:850 mb" 
"SPFH:925 mb" ":TMP:50 mb" ":TMP:200 mb" ":TMP:500 mb" ":TMP:700 mb" ":TMP:850 mb" "TCDC" "ICEC" "TSOIL:0-0.1" "SOILM" "WATR" "WEASD" "LAND" "HGT:surface" "(UGRD|VGRD):10 m above" "(UGRD|VGRD):200 mb" "(UGRD|VGRD):500 mb" "(UGRD|VGRD):700 mb" "(UGRD|VGRD):850 mb" "(UGRD|VGRD):925 mb" "(UFLX|VFLX)" ) 

declare -a filevars=( "dlwrfsfc" "dswrfsfc" "ulwrfsfc" "uswrfsfc" "ulwrftoa" "lhtflsfc" "shtflsfc" "prmsl" "prate" "tmp2m" "tmax2m" "tmin2m" "dpt2m" "hgt200mb" "hgt500mb" "hgt700mb" "hgt850mb" "spfh500mb" "spfh700mb" "spfh850mb" 
"spfh925mb" "tmp50mb" "tmp200mb" "tmp500mb" "tmp700mb" "tmp850mb" "tcdc" "icec" "tsoil10cm" "soilm" "watr" "weasd" "land" "hgtsfc" "wind10m" "wind200mb" "wind500mb" "wind700mb" "wind850mb" "wind925mb" "flux" )

# get validation date of first file
firstfile="${MEMDIR}/sfs.t${CC}z.master.grb2f000"
vt_init="$(wgrib2 "${firstfile}" -d 1 -vt)"
vt_date="${vt_init:7:10}"  

for (( i=0; i<${#vars[@]}; i++)); do
  
  var="${vars[$i]}"
  filevar="${filevars[$i]}"

  filename="${filevar}.${ENS}.${vt_date}.6hourly.grb2"

   # shellcheck disable=SC2046
   # shellcheck disable=SC2002
  cat $(eval "ls -v ${MEMDIR}/*") | wgrib2 - -match "${var}" -grib "${OUTDIR}/IN.grb"

  # interpolate: bilinear for all except PRATE which uses budget
  wgrib2 "${OUTDIR}/IN.grb" -if ':PRATE:' -new_grid_interpolation budget -fi -new_grid_winds earth -new_grid latlon 0:360:1 90:181:-1 "${OUTDIR}/${filename}"
  rm "${OUTDIR}/IN.grb"

done

