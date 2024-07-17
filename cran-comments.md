This patch fixes a bug in the appendInputParams_pgsql() function to ensure that
the constrs vector gets stored properly in the database.

The patch also adds a Citations button to replext_pgsql() and mmirestriktor()
'shiny' apps so that users can find the citations for statistical methods,
software implementations of statistical methods, and web application without
leaving the apps.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Revdepcheck results

This package has no reverse dependencies.

