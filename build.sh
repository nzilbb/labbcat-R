echo delete editor backup files...
find . -name "*~" -exec rm \{\} \;

echo remove older versions
rm nzilbb.labbcat_*.tar.gz

echo generate documentation ...
R -e "devtools::document('nzilbb.labbcat')"

echo build nzilbb.labbcat ...
R CMD build nzilbb.labbcat

echo check nzilbb.labbcat ...
R CMD check --as-cran nzilbb.labbcat_*.tar.gz

echo finished.
