## Test environments
* Local Ubuntu 18.04 install, R 3.6.1
* Ubuntu 16.04 on travis-ci (devel and release)
* win-builder (devel and release)


## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Christopher D. Eager <eager.stats@gmail.com>'

New maintainer:
  Christopher D. Eager <eager.stats@gmail.com>
Old maintainer(s):
  Christopher D. Eager <eagerstats@gmail.com>
```

The maintainer email changed (it's actually the same email, but I realized that I left out the period, which is how I prefer to write it, and this does not make a difference for gmail).


## revdepcheck results
There are no issues with the only current downstream dependency (Countr).
