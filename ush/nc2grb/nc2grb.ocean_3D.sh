#!/bin/bash
set -x

# This is an example script to convert a sample 0.25 degree latlon
# grid netCDF file outputted from ocnicepost to a GRIB2 file in the
# same grid. In addition to an input file, this script requires a
# template.global.0p25.gb2 file to run. The template file is located
# in the fix directory of the global-workflow repository. If the
# input netCDF file has a different resolution, use wgrib2 to regrid
# the template file.
# Author: L. Gwen Chen (lichuan.chen@noaa.gov), 11/02/2023

module load intel/19.1.3.304
module load wgrib2/2.0.8
module list

# ocean 3D variables
infile=ocean.0p25.nc
outfile=ocean_3D.0p25.gb2
ddate=2013100100
aperiod="0-6"
levlst="5 15 25 35 45 55 65 75 85 95 105 115 125 135 145 155 165 175 185 195 205 215 225.86945 241.06255 266.5239 308.7874 373.9288 467.3998 593.87915 757.1453 959.97325 1204.059 1489.9735 1817.1455 2183.879 2587.3995 3023.9285 3488.7875 3976.524 4481.0625"

zl=0
for depth in $levlst; do

wgrib2 template.global.0p25.gb2 \
  -import_netcdf $infile "temp" "0:1:$zl:1:0:721:0:1440" \
    -set_var WTMP -set center 7 -rpn "273.15:+" \
    -set_lev "$depth m below water surface" \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out tmp.gb2 \
  -import_netcdf $infile "so" "0:1:$zl:1:0:721:0:1440" \
    -set_var SALTY -set center 7 -rpn "1000.0:/" \
    -set_lev "$depth m below water surface" \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out tmp.gb2 \
  -import_netcdf $infile "uo" "0:1:$zl:1:0:721:0:1440" \
    -set_var UOGRD -set center 7 \
    -set_lev "$depth m below water surface" \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out tmp.gb2 \
  -import_netcdf $infile "vo" "0:1:$zl:1:0:721:0:1440" \
    -set_var VOGRD -set center 7 \
    -set_lev "$depth m below water surface" \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out tmp.gb2

cat tmp.gb2 >> $outfile
rm -f tmp.gb2
zl=`expr $zl + 1`
done

# Notes:
#   WATPTEMP (water potential temperature (theta)) may be a better
#   GRIB2 parameter than WTMP (water temperature) if MOM6 outputs
#   potential temperature. WATPTEMP is not available in NCEP
#   (-set center 7) tables in wgrib2 v2.0.8.

exit
