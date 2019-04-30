# *** manually set environments (for intel compiler) of nemsiogfs ***

# !!! module environment (*THEIA*) !!!
 module load intel/18.1.163
 module load impi/2018.0.1

 ANCHORDIR=..
 export COMP=ips/impi
 export NEMSIOGFS_VER=v2.2.0
 export NEMSIOGFS_SRC=
 export NEMSIOGFS_INC=$ANCHORDIR/${COMP#*/}/include/nemsiogfs_${NEMSIOGFS_VER}
 export NEMSIOGFS_LIB=$ANCHORDIR/${COMP#*/}/libnemsiogfs_${NEMSIOGFS_VER}.a

 NEMSIO_DIR=../../NCEPLIBS-nemsio
 export NEMSIO_VER=v2.2.4
 export NEMSIO_INC=$NEMSIO_DIR/${COMP#*/}/include/nemsio_${NEMSIO_VER}

 export CC=icc
 export FC=ifort
 export CPP=cpp
 export OMPCC="$CC -qopenmp"
 export OMPFC="$FC -qopenmp"
 export MPICC=mpiicc
 export MPIFC=mpiifort

 export DEBUG="-g -O0"
 export CFLAGS="-O3 -DUNDERSCORE -DLINUX -fPIC"
 export FFLAGS="-O3 -xHOST -traceback -fPIC"
 export CPPFLAGS="-P -traditional-cpp"
 export MPICFLAGS="-O3 -DUNDERSCORE -DLINUX -fPIC"
 export MPIFFLAGS="-O3 -xHOST -traceback -fPIC"
 export MODPATH="-module "
 export I4R4="-integer-size 32 -real-size 32"
 export I4R8="-integer-size 32 -real-size 64"
 export I8R8="-integer-size 64 -real-size 64"

 export CPPDEFS=""
 export CFLAGSDEFS=""
 export FFLAGSDEFS=""

 export USECC=""
 export USEFC="YES"
 export DEPS="NEMSIO $NEMSIO_VER"
