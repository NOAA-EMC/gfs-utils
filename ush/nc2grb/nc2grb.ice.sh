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

# ice surface variables
infile=ice.0p25.nc
outfile=ice.0p25.gb2
ddate=2013100100
aperiod="0-6"

wgrib2 template.global.0p25.gb2 \
  -import_netcdf $infile "hi_h" "0:1:0:721:0:1440" \
    -set_var ICETK -set center 7 \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out $outfile \
  -import_netcdf $infile "aice_h" "0:1:0:721:0:1440" \
    -set_var ICEC -set center 7 \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out $outfile \
  -import_netcdf $infile "Tsfc_h" "0:1:0:721:0:1440" \
    -set_var ICETMP -set center 7 -rpn "273.15:+" \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out $outfile \
  -import_netcdf $infile "uvel_h" "0:1:0:721:0:1440" \
    -set_var UICE -set center 7 \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out $outfile \
  -import_netcdf $infile "vvel_h" "0:1:0:721:0:1440" \
    -set_var VICE -set center 7 \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out $outfile

# Additional variables needed for GFSv17/GEFSv13 operational forecast
# files, but GRIB2 parameters not available in NCEP (-set center 7)
# tables in wgrib2 v2.0.8:
#
#  -import_netcdf $infile "hs_h" "0:1:0:721:0:1440" \
#    -set_var SNVOLSI -set center 7 \
#    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
#    -set_scaling same same -set_grib_type c1 -grib_out $outfile \ 
#  -import_netcdf $infile "frzmlt_h" "0:1:0:721:0:1440" \
#    -set_var FRZMLTPOT -set center 7 \
#    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
#    -set_scaling same same -set_grib_type c1 -grib_out $outfile \
#  -import_netcdf $infile "albsni_h" "0:1:0:721:0:1440" \
#    -set_var ALBDOICE -set center 7 -rpn "100.0:/" \
#    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
#    -set_scaling same same -set_grib_type c1 -grib_out $outfile \
#  -import_netcdf $infile "mlt_onset_h" "0:1:0:721:0:1440" \
#    -set_var MLTDATE -set center 7 \
#    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
#    -set_scaling same same -set_grib_type c1 -grib_out $outfile \
#  -import_netcdf $infile "frz_onset_h" "0:1:0:721:0:1440" \
#    -set_var FRZDATE -set center 7 \
#    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
#    -set_scaling same same -set_grib_type c1 -grib_out $outfile \

exit
