set sccs_tarball=sccs_1.0.tar.gz
R CMD check %sccs_tarball%
R CMD build %sccs_tarball%
R CMD INSTALL %sccs_tarball%
