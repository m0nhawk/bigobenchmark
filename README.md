# `bigobenchmark`
Wrapper for `microbenchmark` to nicely run it on multiple inputs and plot.

## Install from GitHub

```splus
# library("devtools")
devtools::install_github("m0nhawk/bigobenchmark")
```

## Usage
```splus
library("bigobenchmark")
# currently the function SHOULD include parameter `n`
bench <- bigobenchmark(1:n, for(i in 1:n) for(i in 1:n) 1:n, args=seq(from=1, to=100, length.out = 50))
autoplot(bench)
```
