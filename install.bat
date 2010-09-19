set sccs_tarball=sccs_1.0.tar.gz
R CMD check sccs
R CMD build sccs
R CMD INSTALL %sccs_tarball%
