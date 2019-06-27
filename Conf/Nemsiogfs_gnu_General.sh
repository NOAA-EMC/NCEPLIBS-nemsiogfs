# *** manually set environments (for gnu compiler) of nemsiogfs ***

 : ${USERMODE:=false}  # user mode (USERMODE) is closed by default
                       # set env var USERMODE to "true" to active it
 ${USERMODE} && {
    echo "Environment set by user"
    echo "Use default GCC compiler for compatible to w3emc"
    source /apps/intel/impi/5.1.2.150/bin64/mpivars.sh
 }

 ANCHORDIR=..
 export COMP=gnu/impi
 export NEMSIOGFS_VER=v2.2.0
 export NEMSIOGFS_SRC=
 export NEMSIOGFS_INC=$ANCHORDIR/${COMP#*/}/include/nemsiogfs_${NEMSIOGFS_VER}
 export NEMSIOGFS_LIB=$ANCHORDIR/${COMP#*/}/libnemsiogfs_${NEMSIOGFS_VER}.a

 NEMSIO_DIR=../../NCEPLIBS-nemsio
 export NEMSIO_VER=v2.2.4
 export NEMSIO_INC=$NEMSIO_DIR/${COMP#*/}/include/nemsio_${NEMSIO_VER}

 export CC=gcc
 export FC=gfortran
 export CPP=cpp
 export OMPCC="$CC -fopenmp"
 export OMPFC="$FC -fopenmp"
 export MPICC=mpigcc
 export MPIFC=mpif90

 export DEBUG="-g -fbacktrace -O0"
 export CFLAGS="-g -O3 -fPIC"
 export FFLAGS="-g -fbacktrace -O3 -fno-range-check -fPIC"
 export FREEFORM="-ffree-form"
 export FPPCPP="-cpp"
 export CPPFLAGS="-P -traditional-cpp"
 export MPICFLAGS="-g -O3 -fPIC"
 export MPIFFLAGS="-g -fbacktrace -O3 -fno-range-check -fPIC"
 export MODPATH="-J"
 export I4R4=""
 export I4R8="-fdefault-real-8"
 export I8R8="-fdefault-integer-8 -fdefault-real-8"

 export CPPDEFS=""
 export CFLAGSDEFS="-DUNDERSCORE -DLINUX"
 export FFLAGSDEFS=""

 export USECC=""
 export USEFC="YES"
 export DEPS="NEMSIO $NEMSIO_VER"
