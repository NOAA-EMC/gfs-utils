# tref_calc.x

This program calculates near sea surface temperature fields for the Global Constituent Data Assimilation System.

## Overview

This code is borrowed heavily from `enkf_chgres_recenter_nc`. It takes the Gaussian surface analysis from GDAS, regrids it to the GCDAS resolution, and differences them to compute `dtf` and writes `dtf` and `msk` to a file matching the format of the GSI output to feed into global_cycle.

## Namelist Options

The program uses a namelist configuration file to control its behavior. The following namelist groups and variables are supported:

### tref_calc_setup

- `i_output`: Number of longitudes on the target grid.
- `j_output`: Number of latitudes on the target grid.
- `sfcanl_file`: Path to the input GDAS Gaussian surface analysis NetCDF (source tref grid).
- `sfcf006_file`: Path to the input forecast surface NetCDF (target tref grid and land mask).
- `output_file`: Path to the output NetCDF file to write (`dtf`, `msk`, `latitude`, `longitude`).

Example namelist file:

```
&tref_calc_setup
	i_output    = 384,
	j_output    = 193,
	sfcanl_file = '/path/to/sfcanl.nc',
	sfcf006_file= '/path/to/sfcf006.nc',
	output_file = '/path/to/dtfanl.nc'
/
```

Notes:
- `dtf` is computed as the difference between `tref` on the target grid and the source `tref` interpolated to the target grid.
- `msk` is derived from the `land` variable in the target file and written as a byte integer to match GSI output.
- Latitude and longitude are written in degrees, with two extra latitude points appended at −90 and 90 degrees.


