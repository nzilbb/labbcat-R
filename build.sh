echo delete editor backup files...
find . -name "*~" -exec rm \{\} \;

echo running tests...
R -e "devtools::test()"

echo remove older versions
rm nzilbb.labbcat_*.tar.gz

echo generate documentation ...
rm -r man
R -e "devtools::document()"
R -e "pkgdown::build_site()"
cat docs/nzilbb.css >> docs/pkgdown.css

echo build nzilbb.labbcat ...
R CMD build .

echo check nzilbb.labbcat ...
R CMD check --as-cran nzilbb.labbcat_*.tar.gz

echo finished.
