
LIBDIR = .
LIB     = $(LIBDIR)/libnemsiogfs.a
INCMOD  = $(LIBDIR)/incmod/nemsiogfs

FC      = ifort
FFLAGS  = -O -FR -I/nwprod/lib/incmod/nemsio
AR      = ar
ARFLAGS = -rvu

MODULES  = nemsio_gfs.o

$(LIB): $(MODULES)
	$(AR) $(ARFLAGS) $@ $(MODULES)
	mv *.mod $(INCMOD)
	rm -f *.o

clean:
	rm -f *.o *.mod $(LIB) $(INCMOD)/*.mod

.SUFFIXES:
.SUFFIXES: .f90 .o

.f90.o:
	$(FC) $(FFLAGS) -c $*.f90

