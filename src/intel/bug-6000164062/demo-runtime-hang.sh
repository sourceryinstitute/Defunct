# source this file
#
# To work arond this but, set the following environment variable:
# export I_MPI_COLL_INTRANODE=pt2pt
#
module load intel/17
module load mpi/intel-2017
/opt/intel/bin/compilervars.sh intel64
# this will hang on iteration 16:
for x in {1..20}
  do echo $x 
    ifort -coarray=shared -coarray-num-images=$x hello.f90 
    ./a.out
  done

