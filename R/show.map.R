show.map<-function(){
  map.file <- system.file("extdata", "aline_mapset_workingcopy.csv", package="alineR")
  if (map.file == "") {
    stop("Required ALINE map file is missing form the package!")
  }

  map<-read.csv(map.file)

  show.map<-cbind(
    apply(data.frame(map$U.Val), MARGIN = 1, FUN = intToUtf8),
    map,
    deparse.level = 0
    )

  colnames(show.map)[1] <- "IPA"
  return(show.map)
}
