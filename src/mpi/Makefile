BIN = ./
SRC = ../
SEQ_DIR =./
MPI_DIR =./

FSWITCHES = -O3 -fopenmp -fbackslash -ffree-line-length-512 -fallow-argument-mismatch # release - linux (cluster)
# FSWITCHES = -O -Wall -Wtabs -g -fopenmp -ffree-line-length-512 -fallow-argument-mismatch # debug - linux

CSWITCHES = -O -DLINUX -I/usr/X11R6/include -L/usr/X11R6/lib

FC = gfortran # fortran compiler
CC = cc # c compiler

# O_EXT = .obj # windows
O_EXT = .o # linux
MOD_EXT = .mod

# objects for sequential code
objects = 	types_mod$(O_EXT) \
			misc_submod$(O_EXT) \
			input_mod$(O_EXT) \
			beam_loop_mod$(O_EXT) \
			diff_mod$(O_EXT) \
			outputs_mod$(O_EXT) \
			cc_hex_mod$(O_EXT) \
			pbt_mpi_mod$(O_EXT)

# all modules
modules = 	types_mod$(MOD_EXT) \
			misc_submod$(MOD_EXT) \
			input_mod$(MOD_EXT) \
			beam_loop_mod$(MOD_EXT) \
			diff_mod$(MOD_EXT) \
			outputs_mod$(MOD_EXT) \
			cc_hex_mod$(MOD_EXT) \
			mpi_mod$(MOD_EXT) \
			pbt_mpi_mod$(MOD_EXT)

.PHONY: mpi # mpi target
.PHONY: triangle # triangle target

mpi: $(SRC)main_mpi.f90 $(objects) mpi_mod$(O_EXT) $(TRI_BIN)triangle # note that mpi_mod points to openmpi module
	mpifort $(FSWITCHES) $(objects) mpi_mod$(O_EXT) $(SRC)main_mpi.f90 -o $(MPI_DIR)pbt

pbt_mpi_mod$(O_EXT) : $(SRC)pbt_mpi_mod.f90 input_mod$(O_EXT) misc_submod$(O_EXT) beam_loop_mod$(O_EXT) types_mod$(O_EXT) diff_mod$(O_EXT) outputs_mod$(O_EXT) mpi_mod$(O_EXT)
	$(FC) $(FSWITCHES) -c $(SRC)pbt_mpi_mod.f90

outputs_mod$(O_EXT) : $(SRC)outputs_mod.f90 misc_submod$(O_EXT)
	$(FC) $(FSWITCHES) -c $(SRC)outputs_mod.f90

diff_mod$(O_EXT) : $(SRC)diff_mod.f90 misc_submod$(O_EXT) types_mod$(O_EXT)
	$(FC) $(FSWITCHES) -c $(SRC)diff_mod.f90

beam_loop_mod$(O_EXT) : $(SRC)beam_loop_mod.f90 input_mod$(O_EXT) misc_submod$(O_EXT) types_mod$(O_EXT) diff_mod$(O_EXT)
	$(FC) $(FSWITCHES) -c $(SRC)beam_loop_mod.f90

input_mod$(O_EXT) : $(SRC)input_mod.f90 misc_submod$(O_EXT) types_mod$(O_EXT) cc_hex_mod$(O_EXT)
	$(FC) $(FSWITCHES) -c $(SRC)input_mod.f90

misc_submod$(O_EXT) : $(SRC)misc_submod.f90
	$(FC) $(FSWITCHES) -c $(SRC)misc_submod.f90

types_mod$(O_EXT) : $(SRC)types_mod.f90
	$(FC) $(FSWITCHES) -c $(SRC)types_mod.f90

cc_hex_mod$(O_EXT) : $(SRC)cc_hex_mod.f90
	$(FC) $(FSWITCHES) -c $(SRC)cc_hex_mod.f90

mpi_mod$(O_EXT) : $(SRC)mpi_mod.f90 types_mod$(O_EXT)
	mpifort $(FSWITCHES) -c $(SRC)mpi_mod.f90

./tri/triangle: ./tri/triangle.c
	$(CC) $(CSWITCHES) ./tri/triangle.c -lm -o ./tri/triangle

clean :
	rm $(objects) $(modules) $(MPI_DIR)pbt $(MPI_DIR)mpi_mod$(O_EXT)
