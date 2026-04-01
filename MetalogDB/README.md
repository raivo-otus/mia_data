# Usage

To use this function download the directory and source the `fetchMetalogTSE.R`
script. You should now have the function available in your R session.

The function will create a `.data_cache` dir in your working directory if one
is not present. 

To build a `tse` -object you can call the function like so;

`tse <- fetchMetalogTSE("human", "core")`

It will attempt to download the latest versions of datafiles from the Metalog
Database and compile them into a TSE.

By default it will use the cache on subsequent calls. 

Available options are;

"human", "animal", "ocean", and "environmental"

It will download the metaphlan4 profiles for these collections.

For metadata options are;

"core", "extended", "all"

Core metadata is available for most, if not all, samples. Extended
is the partially harmonized metadata set, while "all" downloads all the metadata
available down to study specific variables. 

You can disable cache with the `use_cache = FALSE` option, this will force
a download of the latest available datasets. 

Additionally you can use the metalog webUI to explore the samples. This allows
you to download a samplelist file that you can pass to the function, and 
it will subset the tse to the selected samples upon creation.