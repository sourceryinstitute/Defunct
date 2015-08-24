[This document is formatted with GitHub-Flavored Markdown.              ]:# 
[For better viewing, including hyperlinks, read it online at            ]:# 
[https://github.com/sourceryinstitute/AdHoc/blob/master/README.md]:#

# AdHoc #
An unordered set of tests for modern Fortran compilers

* [Overview]
* [Compatibility]
* [Prerequisites]
* [Installation]
* [Contributing]
* [Acknowledgements]

## <a name="overview">Overview</a> ##
[AdHoc] is an open-source software project that archives ad hoc tests for modern Fortran compilers.  The tests emphasize support the Fortran 2003 and 2008 standards as well as features that have been proposed in a Technical Specification or draft Technical Specification for inclusion in Fortran 2015.

A common challenge in archiving compiler tests include tolerance of compile-time errors with error messages that might vary by compiler version. Most common approaches to building codes results in termination of the build when compile-time errors are encountered.  AdHoc addresses this issue by delays the test compilation to the test phase using CTest, which is part of [CMake]. 

## <a name="compatibility">Compatibility</a> ##
Ultimately, this archive will likely contain tests for each of the compiler projects that support modern Fortran.  Specific support for each compiler will be added as tests for that compiler are added.

## <a name="prerequisites">Prerequisites</a> ##

* [CMake]
* A Fortran compiler.  

As of Fortran 2003, the Fortran standard further specifies certain behavior and values that depend on the implementation of a companion C compiler.  The C compilelr need not be present to build AdHoc, but knowledge of which C compiler is the companion might be helpful in determining the corresponding behavior and values.  In most instances, the companion C compiler is the C compiler that is part of the compiler collection that includes the Fortran compiler being used.  In cases where the compiler vendor does not produce a C compiler, see the vendor documentation for the identity of the companion C compiler.

<a name="installation">
## Installation</a> ##

Please see the [INSTALL.md] file.

<a name="installation">
## Getting Started</a> ##

To start using AdHoc, please see the [GETTING-STARTED.md] file.

<a name="contributing">
## Contributing</a> ##

Please see the [CONTRIBUTING] file.

<a name="status">
## Status</a> ##

Please see the [STATUS.md] file.

## <a name="support">Support</a> ##

* Please submit bug reports and feature requests via our [Issues] page.

## <a name="acknowledgements">Acknowledgements</a> ##
Sourcery Insitute and Sourcery, Inc., gratefully acknowledge support from the following institutions:

* [National Air and Space Administration] (NASA) under subcontract from [Science Systems and Applications Incorporated] (SSAI).
* [Cray, Inc.] for computing time via its Marketing Partner Network.
* [Portland Group] for a PGI Workstation license.
* [Numerical Algorithms Group] (NAG) for a NAG Fortran compiler license.
* [Intel Corp.] for an Intel Parallel Studio XE license.

[Hyperlinks]:#

[Overview]: #overview
[Compatibility]: #compatibility
[Prerequisites]: #prerequisites
[Installation]: #installation
[Contributing]: #contributing
[Acknowledgements]: #acknowledgements

[CMake]: http://www.cmake.org

[AdHoc]: https://github.com/sourceryinstitute/AdHoc
[National Air and Spce Administration]: http://www.nasa.gov
[Science Systems and Applications Incorporated]: http://www.ssaihq.com
[Cray, Inc.]: http://www.cray.com
[Portland Group]: http://pgroup.com
[Numerical Algorithms Group]: http://www.nag.com
[Intel Corp.]: http://www.intel.com

[GCC]: http://gcc.gnu.org
[gfortran]: https://gcc.gnu.org/wiki/GFortran
[Sourcery, Inc.]: http://www.sourceryinstitute.org
[Sourcery Institute]: http://www.sourceryinstitute.org
[INSTALL.md]: ./INSTALL.md 
[CONTRIBUTING.md]: ./CONTRIBUTING.md
[STATUS.md]: ./STATUS.md
[GETTING_STARTED.md]: ./GETTING-STARTED.md
[Issues]: https://github.com/sourceryinstitute/opencoarrays/issue
