
*photoSpec is a WORK IN PROGRESS and is pretty crude shape*

### photoSpec: A photographic spectrophotometer

At this time, photoSpec is largely a bare package structure/shell with a few working parts.  Do not trust anything in it.  There is no documentation and we won't write it yet, too much is changing.  It cannot be installed in any normal way, but if you want to try something you need to source it and possibly do library("some package") in order to load all the necessary external functions.

The functions which mostly work, _but may still change a lot_ are:

+ plotCIEchrom
+ plotSampleCard
+ showCalColSpace
+ getGamutValues (not user facing)
+ getWhiteValues (not user facing)