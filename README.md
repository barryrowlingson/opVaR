The opVaR package by Anna Patrycja Zalewska is mentioned here and available for download:

http://mi2.mini.pw.edu.pl/index.php/nasze-projekty/

however that download appears to be a binary built package for R version 2.9.

The "License" field in the DESCRIPTION file says "GPL-3", which means it should be distributed with source code, or a way to get
the source code. It seems not to be.

So I've attempted to reverse engineer an R version 3+ compatible source package.

There's still a lot to do, but you should be able to load it using `devtools`. Download the files in this repository
as a folder called `opVaR`, and then use `load_all`. It *wont* work as an installed package until all the import/export
stuff is done. Also, you may have to install the dependencies manually (`colorspace`, `vcd` and `SuppDists`).

```
> library(devtools)
> load_all("path/to/opVaR")

> ls(pos=3)
 [1] "dtriangle"         "fit.plot"          "hist.period"      
 [4] "key.sum"           "loss.density"      "loss.fit.dist"    
 [7] "loss.matrix"       "loss.matrix.image" "mc"               
[10] "period.loss"       "print.fitplot"     "print.lf"         
[13] "ptriangle"         "qtriangle"         "read.loss"        
[16] "root.period"       "rtriangle"        

> data(loss.data.object)
      
> loss.data.object$blines[1] # business line "Agency Services"
[1] "Agency Services"
> loss.data.object$rcateg[2] # risk category "Clients, Products & Business Practices"
[1] "Clients, Products & Business Practices"
> x<- read.loss(1,2,loss.data.object) # reads losses (dates and amounts)
> head(x)
   First_Date_of_Event Gross_Loss_Amount
2           2010-01-02          15485.24
7           2010-01-05         252095.01
8           2010-01-06          52635.05
10          2010-01-07         252390.48
13          2010-01-07         169054.77
14          2010-01-08          32316.33

```

I've no idea what this package does, so read the help and figure it out yourself. The help pages seem to work.


