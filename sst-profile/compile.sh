#!/bin/sh

. /apps/lmod/lmod/init/sh
module purge
module use /contrib/spack-stack/spack-stack-1.9.2/envs/ue-oneapi-2024.2.1/install/modulefiles/Core
module load stack-oneapi/2024.2.1
module load stack-intel-oneapi-mpi/2021.13
module load w3emc/2.10.0
module load bacio/2.4.1

ifort -diag-disable=10448 sst-profile.f90 \
      -I${W3EMC_INC4} -I${BACIO_INC4} \
      -L${w3emc_ROOT}/lib64 -L${bacio_ROOT}/lib \
      -lw3emc_4 -lbacio_4 \
      -o ./sst-profile.x
