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

# ocean 2D variables
infile=ocean.0p25.nc
outfile=ocean_2D.0p25.gb2
ddate=2013100100
aperiod="0-6"

wgrib2 template.global.0p25.gb2 \
  -import_netcdf $infile "SSH" "0:1:0:721:0:1440" \
    -set_var SSHG -set center 7 \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out $outfile \
  -import_netcdf $infile "SST" "0:1:0:721:0:1440" \
    -set_var WTMP -set center 7 -rpn "273.15:+" \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out $outfile \
  -import_netcdf $infile "SSS" "0:1:0:721:0:1440" \
    -set_var SALIN -set center 7 \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out $outfile \
  -import_netcdf $infile "speed" "0:1:0:721:0:1440" \
    -set_var SPC -set center 7 \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out $outfile \
  -import_netcdf $infile "SSU" "0:1:0:721:0:1440" \
    -set_var UOGRD -set center 7 \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out $outfile \
  -import_netcdf $infile "SSV" "0:1:0:721:0:1440" \
    -set_var VOGRD -set center 7 \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out $outfile \
  -import_netcdf $infile "latent" "0:1:0:721:0:1440" \
    -set_var LHTFL -set center 7 \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out $outfile \
  -import_netcdf $infile "sensible" "0:1:0:721:0:1440" \
    -set_var SHTFL -set center 7 \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out $outfile \
  -import_netcdf $infile "SW" "0:1:0:721:0:1440" \
    -set_var NSWRF -set center 7 \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out $outfile \
  -import_netcdf $infile "LW" "0:1:0:721:0:1440" \
    -set_var NLWRF -set center 7 \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out $outfile \
  -import_netcdf $infile "LwLatSens" "0:1:0:721:0:1440" \
    -set_var THFLX -set center 7 \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out $outfile \
  -import_netcdf $infile "MLD_003" "0:1:0:721:0:1440" \
    -set_var WDEPTH -set center 7 -set_lev "mixed layer depth" \
    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out $outfile

# Additional variables needed for GFSv17/GEFSv13 operational forecast
# files, but GRIB2 parameters not available in NCEP (-set center 7)
# tables in wgrib2 v2.0.8:
#
#  -import_netcdf $infile "Heat_PmE" "0:1:0:721:0:1440" \
#    -set_var DWHFLUX -set center 7 \
#    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
#    -set_scaling same same -set_grib_type c1 -grib_out $outfile \
#  -import_netcdf $infile "taux" "0:1:0:721:0:1440" \
#    -set_var XCOMPSS -set center 7 \
#    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
#    -set_scaling same same -set_grib_type c1 -grib_out $outfile \
#  -import_netcdf $infile "tauy" "0:1:0:721:0:1440" \
#    -set_var YCOMPSS -set center 7 \
#    -set_date $ddate -set_ftime "$aperiod hour ave fcst" \
#    -set_scaling same same -set_grib_type c1 -grib_out $outfile

exit
