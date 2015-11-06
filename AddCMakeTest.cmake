macro(add_CMake_test name path)

  set(compiler "cmake")
  set(compiler_version "${CMAKE_VERSION}")
  set(base "CMakeLists.txt")

  # Copy the source to the binary tree 
  configure_file(
    ${CMAKE_SOURCE_DIR}/${path}/CMakeLists.txt 
    ${CMAKE_BINARY_DIR}/${path}/CMakeLists.txt 
    COPYONLY
  )
  # Write a script to execute the CMake script at test time instead of at build time so 
  # CMake doesn't bail during the build process because of the compilation failure
  set(bug_harness "${CMAKE_BINARY_DIR}/staging/test-${base}.sh")
  install(
      FILES "${bug_harness}"
      PERMISSIONS WORLD_EXECUTE WORLD_READ WORLD_WRITE OWNER_EXECUTE OWNER_READ OWNER_WRITE GROUP_EXECUTE GROUP_READ GROUP_WRITE
      DESTINATION ${CMAKE_BINARY_DIR}/${path}
  )
  file(WRITE  "${bug_harness}" "#!/bin/bash\n")
  file(APPEND "${bug_harness}" "cd ${CMAKE_BINARY_DIR}/${path}\n")
  file(APPEND "${bug_harness}" "mkdir build\n")
  file(APPEND "${bug_harness}" "cd build\n")
  file(APPEND "${bug_harness}" 
              "cmake .. &> ${CMAKE_SOURCE_DIR}/${path}/${compiler}-${compiler_version}.out\n")
  add_test(NAME ${name} COMMAND "${path}/test-${base}.sh")
  set_property(TEST ${name} PROPERTY PASS_REGULAR_EXPRESSION "Test passed.")
endmacro(add_CMake_test)
