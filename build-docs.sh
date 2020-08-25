echo build documentation with pkgdown

cp NEWS.md README.md nzilbb.labbcat/
R -e "pkgdown::build_site(pkg='nzilbb.labbcat')"
cp -r nzilbb.labbcat/docs .

echo finished documentation.
