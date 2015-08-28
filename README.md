[This document is formatted with GitHub-Flavored Markdown.              ]:# 
[For better viewing, including hyperlinks, read it online at            ]:# 
[https://github.com/sourceryinstitute/AdHoc/blob/master/README.md]:#

# AdHoc #
An unordered set of ad hoc tests for modern Fortran compilers.

* [Overview]
* [Compatibility]
* [Prerequisites]
* [Installation]
* [Contributing]
* [Support]
* [Acknowledgements]

## <a name="overview">Overview</a> ##
[AdHoc] is an open-source software project that archives ad hoc tests for modern Fortran compilers used by Sourcery Institute, Sourcery, Inc., their collaborators, sponsors, and clients.   These tests emphasize support for the Fortran 2003 and 2008 standards along with features proposed for Fortran 2015 in technical specification [TS 29113] Further Interoperability with C and the draft technical specification [TS 18508] Additional Parallel Features in Fortran.

A primary motivation of the design of AdHoc lies in the need for tolerating compile-time errors during the building of the tests.  Inspired by Test-Driven Development (TDD), it is expected that every test committed to AdHoc initially fails and that the test therefore becomes a user specificiation against which the corresponding compiler team can build.  What makes AdHoc a bit unusual is that a large percentage of the failures are compile-time errors.  Compilation errors complicate TDD's traditional write-build-test-refactor development cycle because most build systems halt progress when a source file fails to compile.

[AdHOc] addresses this scenario by delaying the test compilation to the post-build phase of the cycle.  At build time, AdHoc simply builds a script for compiling and running each test.  When the tests are run, source compilation failures are reported as test failures and the execution of the test suite continues onward to any subsequent tests.  [AdHoc] uses the [CMake] capability for detecting compiler identity and version to determine which tests to build. 

(Another use case for test-phase compilation arises when the code must be pre-processed before being passed to the compiler.  This use case occurs with the compiler wrapper in [OpenCoarrays], the project that spawned [AdHoc].)

## <a name="compatibility">Compatibility</a> ##
[AdHoc] is intended to support any modern Fortran compiler for which it is desired to gather users requirements in the form of tests. Specific support for each compiler is added as tests for that compiler are added.

## <a name="prerequisites">Prerequisites</a> ##

* [CMake] 3.0 or higher (3.3.0 or higher preferred for robust detection of the compiler version)
* A Fortran compiler.  

Fortran 2003 specifies certain behavior and values that depend on the implementation of a companion C compiler.  The C compilelr need not be present to build AdHoc, but knowledge of which C compiler is the companion might be helpful in determining the corresponding behavior and values.  In most instances, the companion C compiler is the C compiler that is part of the compiler collection that includes the Fortran compiler being used.  In cases where the compiler vendor does not produce a C compiler, please refer to the vendor documentation for the identity of the companion C compiler.

<a name="installation">
## Installation</a> ##

Download AdHoc via git or as a [Zip file].  From inside a bash shall, issue commands of the following form:
    
    cd <AdHoc-source-directory>
    mkdir -p build
    cd build
    FC=<fortran-compiler-name> cmake .. -DCMAKE_INSTALL_PREFIX=${PWD}
    make
    make install
    ctest

AdHoc produces no executable files or libraries other than those specified in the tests.  Thus, the above "make install" command simply constructs the scripts that actually compile and run the tests during the subsequent "ctest" command.  The scripts  write the test output back into the source tree using a name of the form <compiler-identity>-<compiler-version>.out.  If you are using git, you can determine whether the compiler results differ from the previously committed result by executing  "git diff", which produces no output if the contents of the test result files have not changed.

<a name="contributing">
## Contributing</a> ##

Please see the [CONTRIBUTING file].

<a name="status">
## Status</a> ##

[AdHoc] currently contains tests for the Intel and GNU compilers.  Consequently, these are the only compilers [AdHoc] recognizes at build time.  Support for additional compilers will be added as tests for the corresponding compilers is added.

<a name="support">
## Support</a> ##

Please submit bug reports and feature requests via our [Issues] page.

<a name="acknowledgements">
## Acknowledgements</a> ##
Sourcery Insitute and Sourcery, Inc., gratefully acknowledge support from the following institutions:

* The National Aeronautics and Space Administration ([NASA]) under subcontract from Science Systems and Applications Incorporated ([SSAI]).
* [Cray, Inc.] for computing time via its Marketing Partner Network.
* [Portland Group] for a PGI Workstation license.
* Numerical Algorithms Group ([NAG]) for a NAG Fortran compiler license.
* [Intel Corp.] for an Intel Parallel Studio XE license.

[Hyperlinks]:#

[Table of Contents]:#
[Overview]: #overview
[Compatibility]: #compatibility
[Prerequisites]: #prerequisites
[Installation]: #installation
[Contributing]: #contributing
[Support]: #support
[Acknowledgements]: #acknowledgements
 

[Document Body]:#
[OpenCoarrays]: http://www.opencoarrays.org
[CMake]: http://www.cmake.org
[TS 18508]: http://isotc.iso.org/livelink/livelink?func=ll&objId=17181227&objAction=Open
[TS 29113]: http://ftp.nag.co.uk/sc22wg5/N1901-N1950/N1942.pdf
[AdHoc]: https://github.com/sourceryinstitute/AdHoc
[Zip file]: https://github.com/sourceryinstitute/AdHoc/archive/master.zip
[GCC]: http://gcc.gnu.org
[gfortran]: https://gcc.gnu.org/wiki/GFortran
[Sourcery, Inc.]: http://www.sourceryinstitute.org
[Sourcery Institute]: http://www.sourceryinstitute.org
[Contributing file]: ./CONTRIBUTING
[Issues]: https://github.com/sourceryinstitute/AdHoc/issues


[Acknowledgements Section]:#
[NASA]: http://www.nasa.gov
[SSAI]: http://www.ssaihq.com
[Cray, Inc.]: http://www.cray.com
[Portland Group]: http://pgroup.com
[NAG]: http://www.nag.com
[Intel Corp.]: http://www.intel.com



