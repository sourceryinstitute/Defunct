cmake_minimum_required(VERSION 3.0)

#Name project and specify version and languages
project(adhoc VERSION 0.0.0 LANGUAGES C Fortran)

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
else()
  message(WARNING 
    "*** Untested Fortran compiler detected: ${CMAKE_Fortran_COMPILER_ID}."
    "*** Building all tests.  Please report failures to damian@sourceryinstitute.com"
  )
endif()

add_subdirectory(src)

enable_testing()

set(compiler ${CMAKE_Fortran_COMPILER_ID})

if (CMAKE_VERSION VERSION_GREATER 3.2.3) 
  # Detect Fortran compiler version directly
  set(compiler_version "${CMAKE_Fortran_COMPILER_VERSION}")
else()
  # Use the C compiler version as a proxy for the Fortran compiler version (won't work with NAG)
  set(compiler_version "${CMAKE_C_COMPILER_VERSION}")
endif()

function(add_compile_time_test name path base ext)
  set(executable base)
  # Copy the source to the binary tree 
  configure_file(
    ${CMAKE_SOURCE_DIR}/${path}/${base}.${ext} 
    ${CMAKE_BINARY_DIR}/${path}/${base}.${ext} 
    COPYONLY
  )
  # Write a script to compile the code at test time instead of at build time so 
  # CMake doesn't bail during the build process because of the compilation failure
  set(bug_harness "${CMAKE_BINARY_DIR}/staging/test-${base}.sh")
  install(
      FILES "${bug_harness}"
      PERMISSIONS WORLD_EXECUTE WORLD_READ WORLD_WRITE OWNER_EXECUTE OWNER_READ OWNER_WRITE GROUP_EXECUTE GROUP_READ GROUP_WRITE
      DESTINATION ${CMAKE_BINARY_DIR}/${path}
  )
  file(WRITE  "${bug_harness}" "#!/bin/bash\n")
  file(APPEND "${bug_harness}" "cd ${CMAKE_BINARY_DIR}/${path}\n")
  file(APPEND "${bug_harness}" 
              "$ENV{FC} ${base}.${ext} -o ${base} &> ${CMAKE_SOURCE_DIR}/${path}/${compiler}-${compiler_version}.out\n")
  add_test(NAME ${name} COMMAND "${path}/test-${base}.sh")
  set_property(TEST ${name} PROPERTY PASS_REGULAR_EXPRESSION "Test passed.")
endfunction(add_compile_time_test)

add_compile_time_test(gnu-bug-55824 src/gnu/nasa/bug-55824 ice-on-pack-unlimited-polymorphic f90)
