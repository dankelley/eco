DK test: stripped-down oce

## 0.0-1
`read.met.csv2(csv3,encoding="UTF-8")`

```R
rhub::check(platform="debian-clang-devel",email="dan.kelley@dal.ca",show_status=FALSE)
```

This ought to fail, and it does, but the message (below) doesn't tell me what
was wrong, so it's not too helpful.

```
-- Error (test_met.R:8:5): read.met() handles type="csv3" files ----------------
Error in ``$<-.data.frame`(`*tmp*`, "time", value = structure(numeric(0), class = c("POSIXct", 
"POSIXt"), tzone = "UTC"))`: replacement has 0 rows, data has 5
```

## 0.0-2

`read.met.csv2(csv3,encoding="UTF-8-BOM")`

```R
rhub::check(platform="debian-clang-devel",email="dan.kelley@dal.ca",show_status=FALSE)
```

Does it pass?  If so, then we have evidence that the new method for passing the
encoding to `file()` and not to `read*()` has worked.  (Many thanks to Ivan
Krylov for telling me about this scheme.)

