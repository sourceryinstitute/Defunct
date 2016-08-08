MODULE CmakeReproducerMod
    IMPLICIT NONE

    INTERFACE TestFunc
        MODULE FUNCTION submodule_testFunc() RESULT(c)
            INTEGER              :: c
        END FUNCTION submodule_testFunc
    END INTERFACE TestFunc
END MODULE CmakeReproducerMod

SUBMODULE (CmakeReproducerMod) CmakeReproducerSub
    IMPLICIT NONE
CONTAINS
    MODULE FUNCTION submodule_testFunc()
            INTEGER              :: c
    END FUNCTION submodule_testFunc
END SUBMODULE CmakeReproducerSub

program main
  implicit none
  ! Dummy program just to facilitate producing an executable
end program
