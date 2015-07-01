## Copy the source to the binary tree 
#set(source ice-on-pack-unlimited-polymorphic.f90)
#set(executable ice-on-pack-unlimited-polymorphic)
#configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${source} ${CMAKE_CURRENT_BINARY_DIR} COPYONLY)
#
## Write a script to compile the code at test time instead of at build time so 
## CMake doesn't bail during the build process because of the build failure
#set(bug_harness "${CMAKE_BINARY_DIR}/bin_staging/test-${executable}.sh")
#install(
#    FILES "${bug_harness}"
#    PERMISSIONS WORLD_EXECUTE WORLD_READ WORLD_WRITE OWNER_EXECUTE OWNER_READ OWNER_WRITE GROUP_EXECUTE GROUP_READ GROUP_WRITE
#    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
#)
#file(WRITE  "${bug_harness}" "#!/bin/bash\n")
#file(APPEND "${bug_harness}" "cd ${CMAKE_CURRENT_BINARY_DIR}\n")
#file(APPEND "${bug_harness}" "${FC} ${source} -o ${executable}\n")
