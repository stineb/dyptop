## --------------------------------
## Benjamin Stocker, May 2013 - June 2014         
## --------------------------------


## --------------------------------
## select compiler; flag used for omitting line length limit of Fortran code
# FCOM=pgf95 -Mextend
FCOM=gfortran -ffixed-line-length-0
## --------------------------------

export FCOM

# name of executable
EXE = dyptop

standard: 
	 $(FCOM) -c params_core.mod.F
	 $(FCOM) -c params_dyptop.mod.F
	 $(FCOM) -c globalvars.mod.F
	 $(FCOM) -c getdyptoppar.F
	 $(FCOM) -c gridcellfraction.F
	 $(FCOM) -c fflooded.F
	 $(FCOM) params_core.mod.o params_dyptop.mod.o globalvars.mod.o getdyptoppar.o gridcellfraction.o fflooded.o main.F -o $(EXE)

.PHONY: clean
clean:
	rm $(EXE) *.o *.mod

