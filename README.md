The opVaR package by Anna Patrycja Zalewska is mentioned here and available for download:

http://mi2.mini.pw.edu.pl/index.php/nasze-projekty/

however that download appears to be a binary built package for R version 2.9.

The "License" field in the DESCRIPTION file says "GPL-3", which means it should be distributed with source code, or a way to get
the source code. It seems not to be.

So I've attempted to reverse engineer an R version 3+ compatible source package.

There's still a lot to do, but you should be able to load it using `devtools`. Download the files in this repository
as a folder called `opVaR`, and then use `load_all`:

```
library(devtools)
load_all("path/to/opVaR")
```
