# Copyright (c) 2015 by Sean Downey
# Authors: Sean Downey (sean@codexdata.com) and Guowei Sun (gwsun@umd.edu)
# This software is distributed under GPL-3.

encode.ALINE <-
  function(x, mark=FALSE, m1=NULL, m2=NULL){
    map_file <- system.file("extdata", "aline_mapset_workingcopy.csv", package = "alineR")
    diacritics_file <- system.file("extdata", "aline_diacritic_set.csv", package = "alineR")

    if (map_file == "" || diacritics_file == "") {
        stop("Required ALINE data files are missing from the package!")
    }

#     map<-read.csv("./data/aline_mapset_workingcopy.csv")
    map<-read.csv(map_file, stringsAsFactors=FALSE)
#     diacritics<-read.csv("./data/aline_diacritic_set.csv")
    diacritics<-read.csv(diacritics_file, stringsAsFactors=FALSE)

    v<-intToUtf8(c(65:90, 97:122), multiple=T) 
    `%nin%` <- function (x, table) match(x, table, nomatch = 0L) == 0L 
    
    sapply(x, USE.NAMES=T, FUN=function (word){
      out<-""
      for (i in unlist(strsplit(word, "")) ){
        
        catch<-FALSE
        
        for(j in 1:nrow(map)){ 
          # if( utf8ToInt(i) == map$IPA[j] ){
          if( utf8ToInt(i) == map$U.Val[j] ){
            i <- intToUtf8(unlist(strsplit(as.character(map$A.Val[j]), " ")))
            catch<-TRUE
            break
          } 
        }

        if (!catch) {
          for (j in 1:nrow(diacritics)) {
            if( utf8ToInt(i) == diacritics$U.Val[j] ){
              i <- intToUtf8(unlist(strsplit(as.character(diacritics$A.Val[j]), " ")))
              catch<-TRUE
              break
            }
          }
        }
        
        if (!catch && i %nin% v){
          message(paste("Invalid character:",i,"in",word))
          i <- ifelse(mark,'@','') 
        }
        
        out<-append(out,i)
      }
      
      paste(out,sep='',collapse='')
    } 
    )
  }
