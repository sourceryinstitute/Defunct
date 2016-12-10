---
project: CME214
summary: Software Design in Modern Fortran for Scientists and Engineers exam documentation
src_dir: .
output_dir: doc
preprocess: true
display: public
         protected
         private
source: true
graph: true
sort: alpha
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
project_github: https://github.com/sourceryinstitute/cme-214-homework-<username>
project_download: https://github.com/sourceryinstitute/cme-214-homework-<username>/releases/latest
license: bsd
author: <insert-your-name>
email: <insert-your-email-address>
author_description: <insert-your-department-and-degree-program>
author_pic: <insert-url-to-a-photo-of-yourself>
github: https://github.com/<insert-your-github-user-name>/<insert-your-repository-name>
website: http://www.sourceryinstitute.org
---

[source: display source code corresponding to item being documented]:#
[graph: generate call graphs, module dependency graphs, derive type composition/inheritance graphs ]:#
[sort: different sorting schemes for the modules or procedures or programs or derived types (alpha = alphabetical see wiki).]:#
[extra_mods: documentation for intrinsic modules]:#

[This document is a FORD project file, formatted with Pythonic Markdown                                      ]:#
[See https://github.com/cmacmackin/ford/wiki/Project-File-Options for more info on writing FORD project files]:#

--------------------


Brief description
-----------------

@warning
This archive uses advaned features of Fortran 2015.  As of October 2016, only GNU Fortran compiler supports all features employed.

### ToDo

X marks indicated solved problems (insert X for "Solved" after complet the exam):

 - [X] Provided [library](./src/library)
 - [X] Provided [object-oriented-basketball](./src/object-oriented-basketball)
 - [X] Provided [inheritance-basketball](./src/inheritance-basketball)
 - [X] Provided [composition-basketball](./src/composition-basketball)
 - [ ] Solved   [puppeteer-basketball](./src/puppetteer-basketball)

Compilers
---------

This archive has been tested with the pre-release version 7.0.0 GNU Fortran compiler (gfortran) with multi-image execution support from [OpenCoarrays](http://www.opencoarrays.org).
