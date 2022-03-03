You'll need to build [this ghc merge request](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7245). 

Then you can make your changes, and run with:

``` sh
nix-shell --pure --run 'cabal build --allow-newer -w ~/programming/ghc/_prefetch/stage1/bin/ghc'
taskset -c 0 perf stat -r5 ./dist-newstyle/build/x86_64-linux/ghc-9.3.20220302/testing-prefetch-0.1.0.0/x/testing-prefetch/build/testing-prefetch/testing-prefetch
```

where `-w` is the path to the merge request's ghc binary.
