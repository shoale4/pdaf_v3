#######################################################

.SUFFIXES: .F90 .o 

######################################################


# Root directory of PDAF package
BASEDIR = ../../..

# Include machine-specific definitions
# For available include files see directory make.arch
# To choose a file, set PDAF_ARCH either here or by an
# environment variable.
include $(BASEDIR)/make.arch/$(PDAF_ARCH).h

######################################################

# ifeq ($(FILEOUT_FLAG), 0)
# 	ENS_GEN = generate_ens.o
# 	OBS_GEN = generate_obs.o
# else ifeq ($(FILEOUT_FLAG), 1)
# 	ENS_GEN = generate_ens_txt.o
# 	OBS_GEN = generate_obs_txt.o
# endif

ENS_GEN = generate_ens_txt.o
OBS_GEN = generate_obs_txt.o

# OBJ_GENOBS = generate_obs.o \
# 			 parser_no_mpi.o \
# 			 generate_obs_txt.o

# OBJ_ENS  = generate_ens.o \
# 		   parser_no_mpi.o \
# 		   generate_ens_txt.o 

OBJ_GENOBS = $(OBS_GEN) \
			 output_txt.o \
			 parser_no_mpi.o 

OBJ_ENS  = $(ENS_GEN) \
		   output_txt.o \
		   parser_no_mpi.o 
	
### Name of Executables ##############################################

EXE_GENOBS = generate_obs
EXE_ENS  = generate_ens

######################################################

info:
	@echo "Makefile to generate observations and ensemble for Modified Mitchell-Schaeffer Model"

$(EXE_GENOBS) : $(OBJ_GENOBS)
	rm -f $@
	$(LD)  $(OPT)  -o $@ $(MODULES) $(OBJ_GENOBS) $(NC_LIB) $(LINK_LIBS)

$(EXE_ENS) : libpdaf-d.a $(OBJ_ENS)
	rm -f $@
	$(LD)  $(OPT)  -o $@ $(MODULES) $(OBJ_ENS) $(NC_LIB) -L$(BASEDIR)/lib -lpdaf-d $(LINK_LIBS)

all: $(EXE_GENOBS) $(EXE_ENS)

.F90.o :
	$(FC) $(OPT) $(CPP_DEFS) $(NC_INC) -c $*.F90 

libpdaf-d.a: 
	@echo "++++++ Generate Filter library ++++++"
	@cd $(BASEDIR)/src; make;

clean :
	rm -f *.o *.mod *.a $(EXE_GENOBS) $(EXE_ENS)