works_with_R("2.15.2",RJSONIO="1.0.1",rjson="0.2.12")

## try to use both, see which is more convenient.
L <- list(geoms=list(geom1=list(element="polyline",data="geom1.csv"),
            geom2=list(element="line",clickSelects="generation")),
          plots=list(ts=list("geom1","geom2"),loci=list("geom3","geom4")))
funs <- list(RJSONIO=RJSONIO::toJSON,
             rjson=rjson::toJSON)
             
results <- list()
for(pkg in names(funs)){
  print(pkg)
  cat(funs[[pkg]](L))
}

## RJSONIO::rjson is easier to read.

