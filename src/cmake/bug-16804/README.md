[This document is formatted in GitHub-flavored Markdown]:#

CMake Issue Reproducer: MPIEXEC not found even if arguments specify MPI compiler locations
==========================================================================================

## Issue demonstration
To demonstrate the issue, execute the following at the command line in a bash shell:
```bash
cd cmake-issue
./cmake-issue.sh
```
which takes about 6 minutes to run inside a Lubuntu Linux virtual machine given access to all four cores on a 2017 MacBook Pro laptop.  Near the bottom of the output is the message that represents the issue:

-- MPIEXEC=MPIEXEC-NOTFOUND

## Sample output

Included in this archive is the output file cmake-issue.log from executing
```bash
time ./cmake-issue.sh >& cmake-issue.log
```
in a bash shell inside a nearly bare Lubuntu Linux virtual machine with GCC 6.2 and CMake 3.5.2 installed.  Previous tests indicate that the same behavior is obtained with CMake 3.4.0 and 3.7.1 and the same behavior is observed on macOS Sierra.
