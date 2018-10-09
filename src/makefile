
LIB     = $(LIBDIR)/libnemsiogfs_$(VER).a
INCMOD  = $(LIBDIR)/$(INC)/nemsiogfs_$(VER)

FC      = $(FCOMP)
FFLAGS  = $(FCFLAGS)
AR      = ar
ARFLAGS = -rvu

MODULES  = nemsio_gfs.o

$(LIB): $(MODULES)
	$(AR) $(ARFLAGS) $@ $(MODULES)
	mkdir -p $(INCMOD)
	mv *.mod $(INCMOD)
	rm -f *.o

clean:
	rm -f *.o *.mod *.a $(LIB) $(INCMOD)/*.mod

.SUFFIXES:
.SUFFIXES: .f90 .o

.f90.o:
	$(FC) $(FFLAGS) -c $*.f90

