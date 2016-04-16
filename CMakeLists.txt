cmake_minimum_required(VERSION 3.0)

#Name project and specify version and languages
project(AdHoc VERSION 0.0.0 LANGUAGES C Fortran)

#Print an error message on an attempt to build inside the source directory tree:
if ("${CMAKE_CURRENT_SOURCE_DIR}" STREQUAL "${CMAKE_CURRENT_BINARY_DIR}")
  message(FATAL_ERROR "ERROR! "
    "CMAKE_CURRENT_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}"
    " == CMAKE_CURRENT_BINARY_DIR=${CMAKE_CURRENT_BINARY_DIR}"
    "\nThis archive does not support in-source builds:\n"
    "You must now delete the CMakeCache.txt file and the CMakeFiles/ directory under"
    "the 'src' source directory or you will not be able to configure correctly!"
    "\nYou must now run something like:\n"
    "  $ rm -r CMakeCache.txt CMakeFiles/"
    "\n"
    "Please create a directory outside the opencoarrays source tree and build under that outside directory "
    "in a manner such as\n"
    "  $ mkdir build-opencarrays\n"
    "  $ cd build-opencoarrays\n"
    "  $ CC=mpicc FC=mpif90 cmake <path-to-opencoarrays-source-directory> -DCMAKE_INSTALL_PREFIX=<path-to-install-directory>\n"
    "\nsubstituting the appropriate syntax for your shell (the above line assumes the bash shell)."
    )
endif()

#Report untested Fortran compiler unless explicitly directed to build all examples.
if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU" )
  set(gfortran_compiler true)
elseif ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "PGI" )
  set(portland_group_compiler true)
elseif ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Cray" )
  set(cray_compiler true)
endif()

include(AddCompileTimeTest.cmake)
include(AddCMakeTest.cmake)

enable_testing()

if("${gfortran_compiler}")
  add_compile_time_test(gnu-bug-55824 src/gnu/nasa/bug-55824 ice-on-pack-unlimited-polymorphic f90)
  add_compile_time_test(gnu-bug-68196 src/gnu/numrich/bug-68196 ice-on-proc-ptr-func-comp f90)
  add_compile_time_test(gnu-bug-66681 src/gnu/opencoarrays/bug-66681 assign_this_image_to_complex f90)
  add_compile_time_test(gnu-bug-66833 src/gnu/opencoarrays/bug-66833 ice-on-assumed-rank-character f90)
  add_compile_time_test(gnu-bug-70696  src/gnu/opencoarrays/bug-70696 post_host_associated_event f90)
  add_compile_time_test(gnu-bug-70697  src/gnu/opencoarrays/bug-70697 tree f90)
  add_compile_time_test(gnu-bug-59694  src/gnu/sourcery/bug-59694 final f90)
elseif("${portland_group_compiler}")
  add_compile_time_test(pgi-bug-21894    src/pgi/opencoarrays/bug-21894 c_ptrdiff_t f90)
  add_compile_time_test(pgi-bug-21918-01 src/pgi/opencoarrays/bug-xxxxx save-bind-c f90)
  add_compile_time_test(pgi-bug-21918-02 src/pgi/opencoarrays/bug-xxxxy assumed-rank f90)
  add_compile_time_test(pgi-bug-21918-03 src/pgi/opencoarrays/bug-xxxxz do-concurrent f90)
elseif("${cray_compiler}")
  add_compile_time_test(cce-bug-821337 src/cce/nasa/bug-821337 generic-resolution f90)
  add_cmake_test(cmake-bug-xxxxx src/cmake/bug-xxxxx)
else()
  message(FATAL_ERROR
    "\n"
    "${PROJECT_NAME} contains no tests for the ${CMAKE_Fortran_COMPILER_ID} Fortran compiler. "
    "Please add tests in the top level CMakeLists.txt file and subimt a pull request or ask " 
    "for assistance via our Issues page (https://github.com/sourceryinstitute/AdHoc/issues)"
  )
endif()
