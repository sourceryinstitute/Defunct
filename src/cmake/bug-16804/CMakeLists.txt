cmake_minimum_required(VERSION 3.4.0)

project(opencoarrays LANGUAGES C Fortran)

#Print an error message on an attempt to build inside the source directory tree:
if ("${CMAKE_CURRENT_SOURCE_DIR}" STREQUAL "${CMAKE_CURRENT_BINARY_DIR}")
  message(FATAL_ERROR 
    "CMAKE_CURRENT_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}"
    " == CMAKE_CURRENT_BINARY_DIR=${CMAKE_CURRENT_BINARY_DIR}"
    "\nERROR: This archive does not support in-source builds:\n"
    )
endif()

#----------------------------------------------------------------------------
# Find MPI and set some flags so that FC and CC can point to gfortran and gcc
#----------------------------------------------------------------------------

# If the user passes FC=mpif90 etc. check and prefer that location
get_filename_component( FTN_COMPILER_NAME "${CMAKE_Fortran_COMPILER}"
  NAME )
get_filename_component( C_COMPILER_NAME "${CMAKE_C_COMPILER}"
  NAME )
get_filename_component( FTN_COMPILER_DIR "${CMAKE_Fortran_COMPILER}"
  REALPATH )
get_filename_component( C_COMPILER_DIR "${CMAKE_C_COMPILER}"
  REALPATH )

if (FTN_COMPILER_NAME MATCHES "^[mM][pP][iI]")
  set (MPI_Fortran_COMPILER "${CMAKE_Fortran_COMPILER}")
endif()
if (C_COMPILER_NAME MATCHES "^[mM][pP][iI]")
  set (MPI_C_COMPILER "${CMAKE_C_COMPILER}")
endif()

find_package( MPI )

if ( (NOT MPI_C_FOUND) OR (NOT MPI_Fortran_FOUND) )
  find_program (MY_MPI_EXEC NAMES mpirun mpiexec lamexec srun
    PATHS "${CMAKE_SOURCE_DIR}/prerequisites/installations/mpich/3.1.4" "${CMAKE_SOURCE_DIR}/prerequisites/installations/mpich/*" ENV PATH
    HINTS "${FTN_COMPILER_DIR}" "${C_COMPILER_DIR}"
    PATH_SUFFIXES bin)
  set ( MPI_HOME "${MPI_HOME}" "${MY_MPI_EXEC}" "${MY_MPI_EXEC}/.." )
  find_package( MPI REQUIRED )
endif()

<<<<<<< HEAD
message(STATUS "MPIEXEC=${MPIEXEC}, MPI_Fortran_COMPILER=${MPI_Fortran_COMPILER} MPI_C_COMPILER=${MPI_C_COMPILER}")
=======
message(STATUS
   "\n\n**********\n\n"
   "CMake_Fortran_COMPILER=${CMAKE_Fortran_COMPILER}\n"
   "CMake_C_COMPILER=${CMAKE_C_COMPILER}\n"
   "CMake_CXX_COMPILER=${CMAKE_CXX_COMPILER}\n"
   "MPI_C_COMPILER=${MPI_C_COMPILER}\n"
   "MPI_Fortran_COMPILER=${MPI_Fortran_COMPILER}\n"
   "MPI_CXX_COMPILER=${MPI_CXX_COMPILER}\n"
   "MPIEXEC=${MPIEXEC}\n"
   "\n\n**********\n\n"
)
>>>>>>> 15afa7a8bf4b172c21fa7f33eafa8baa67eb275d
