#!/bin/bash

. /apps/lmod/lmod/init/sh
module purge
module use /contrib/spack-stack/spack-stack-1.9.2/envs/ue-oneapi-2024.2.1/install/modulefiles/Core
module load stack-oneapi/2024.2.1
module load stack-intel-oneapi-mpi/2021.13
module load netcdf-fortran/4.6.1

LIBS="-L$netcdf_c_ROOT/lib -L$netcdf_fortran_ROOT/lib"
INCS="-I$netcdf_c_ROOT/include -I$netcdf_fortran_ROOT/include"

ifort add-vars.f90 -diag-disable=10448 $LIBS $INCS -lnetcdff -lnetcdf -o ./add-vars.x
