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
#         updated on 05/20/2024

module load intel/19.1.3.304
module load wgrib2/2.0.8
module list

# ocean 3D variables
infile=ocean.0p25.nc
outfile=ocean_3D.0p25.gb2
ddate=2013100100
aperiod="0-6"
levlst="5 15 25 35 45 55 65 75 85 95 105 115 125 135 145 155 165 175 185 195 205 215 226 241 267 309 374 467 594 757 960 1204 1490 1817 2184 2587 3024 3489 3977 4481"

zl=0
for depth in $levlst; do

wgrib2 template.global.0p25.gb2 \
  -import_netcdf $infile "temp" "0:1:$zl:1:0:721:0:1440" \
    -set_var WTMP -set center 7 -rpn "273.15:+" \
    -set_lev "$depth m below sea level" \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out tmp.gb2 \
  -import_netcdf $infile "so" "0:1:$zl:1:0:721:0:1440" \
    -set_var SALIN -set center 7 \
    -set_lev "$depth m below sea level" \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out tmp.gb2 \
  -import_netcdf $infile "uo" "0:1:$zl:1:0:721:0:1440" \
    -set_var UOGRD -set center 7 \
    -set_lev "$depth m below sea level" \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out tmp.gb2 \
  -import_netcdf $infile "vo" "0:1:$zl:1:0:721:0:1440" \
    -set_var VOGRD -set center 7 \
    -set_lev "$depth m below sea level" \
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
