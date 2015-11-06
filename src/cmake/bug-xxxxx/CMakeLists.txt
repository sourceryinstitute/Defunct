project(cray_compiler_version_bug LANGUAGES Fortran C CXX)
if(
   "${CMAKE_Fortran_COMPILER_VERSION}" MATCHES "" AND 
   "${CMAKE_C_COMPILER_VERSION}"       MATCHES "" AND 
   "${CMAKE_CXX_COMPILER_VERSION}"     MATCHES "" 
  )
  message(FATAL_ERROR 
    "CMAKE_Fortran_COMPILER_VERSION, CMAKE_C_COMPILER_VERSION, CMAKE_CXX_COMPILER_VERSION are all empty."
  )
endif()
