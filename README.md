# forwast-to-rdf


Need to first install the `rrdf` package as described [here](https://github.com/egonw/rrdf#install-from-r):
```
> install.packages("rJava") # if not present already
> install.packages("devtools") # if not present already
> library(devtools)
> install_github("egonw/rrdf", subdir="rrdflibs")
> install_github("egonw/rrdf", subdir="rrdf", build_vignettes = FALSE)
```

Install this library from github:

```
> library(devtools)
> install_github('cbdavis/forwast-to-rdf')
```

Load the library and run the `forwast_to_rdf` function which will download the source data and output RDF in a `forwast.ttl` file
```
> library(FORWAST)
> forwast_to_rdf()
```

Load the data into a triplestore:
```
$ wget 'http://downloads.sourceforge.net/project/bigdata/bigdata/2.1.4/blazegraph.jar'
$ java -server -Xmx4g -jar blazegraph.jar
$ curl -X POST -H 'Content-Type:application/x-turtle' --data-binary '@forwast.ttl' http://localhost:9999/blazegraph/sparql
```

Delete all triples
```
$ curl -X POST http://localhost:9999/blazegraph/sparql --data-urlencode 'update=DROP ALL;'
```
