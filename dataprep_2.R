library("readxl")
library("rlist")
library("dplyr")
library("stringr")
library("tsfeatures")
library("scales")

setwd("...")

parent.folder = "..."
sub.folders = list.dirs(parent.folder, recursive=TRUE)[-1]
names = list.dirs(parent.folder, recursive=TRUE,full.names = FALSE)[-1]

read = function(pattern = "A.xlsx"){
  data.lst = list()
  for (i in sub.folders){
    parent.tmp = i
    sub.tmp = list.files(parent.tmp, full.names = TRUE, pattern=pattern)
    for (j in sub.tmp){
      data.lst[[i]][[j]] = read_excel(j,1,range = cell_rows(72:132),col_types = c("numeric", "numeric", "numeric", 
                                                                                  "numeric", "numeric", "numeric", 
                                                                                  "numeric", "numeric", "numeric", 
                                                                                  "numeric", "skip", "skip", 
                                                                                  "skip", "skip", "skip", "skip", 
                                                                                  "skip", "skip"), skip = 71)
    }
  }
  return(data.lst)
}

extract.features = function(fun.name = stability, lst.object){
  tmp = list()
  for (i in 1:length(lst.object)){
    tmp[[i]] = sapply(as.data.frame(lst.object[[i]]),fun.name)
  } 
  res = as.data.frame(do.call(rbind, tmp))
}

oberer.atem = read("A.xlsx")
unterer.atem = read("B.xlsx")

for (i in 1:length(oberer.atem)){
  oberer.atem[[i]] = lapply(as.data.frame(oberer.atem[[i]]), function(x) rescale(x))
  names(oberer.atem[[i]]) <- c("W1C",	"W5S",	"W3C",	"W6S",	"W5C",	"W1S",	"W1W",
                          "W2S", "W2W",	"W3S")
}

for (i in 1:length(unterer.atem)){
  unterer.atem[[i]] = lapply(as.data.frame(unterer.atem[[i]]), function(x) rescale(x))
  names(unterer.atem[[i]]) <- c("W1C",	"W5S",	"W3C",	"W6S",	"W5C",	"W1S",	"W1W",
                               "W2S", "W2W",	"W3S")
}



# extract features oberer Atemweg
stability.oa = extract.features(stability, oberer.atem)
colnames(stability.oa) = paste(colnames(stability.oa), "oa", sep = ".")
lumpiness.oa = extract.features(lumpiness, oberer.atem)
colnames(lumpiness.oa) = paste(colnames(lumpiness.oa), "oa", sep = ".")
std.oa = extract.features(std1st_der, oberer.atem)
colnames(std.oa) = paste(colnames(std.oa), "sd.oa", sep = ".")
fluctanal.oa = extract.features(fluctanal_prop_r1, oberer.atem)
colnames(fluctanal.oa) = paste(colnames(fluctanal.oa), "fluc.oa", sep = ".")
min.oa = extract.features(min, oberer.atem)
colnames(min.oa) = paste(colnames(min.oa), "min.oa", sep = ".")
max.oa = extract.features(max, oberer.atem)
colnames(max.oa) = paste(colnames(max.oa), "max.oa", sep = ".")

# extract features unterer Atemweg
stability.ua = extract.features(stability, unterer.atem)
colnames(stability.ua) = paste(colnames(stability.ua), "ua", sep = ".")
lumpiness.ua = extract.features(lumpiness, unterer.atem)
colnames(lumpiness.ua) = paste(colnames(lumpiness.ua), "ua", sep = ".")
std.ua = extract.features(std1st_der, unterer.atem)
colnames(std.ua) = paste(colnames(std.ua), "sd.ua", sep = ".")
fluctanal.ua = extract.features(fluctanal_prop_r1, unterer.atem)
colnames(fluctanal.ua) = paste(colnames(fluctanal.ua), "fluc.ua", sep = ".")
min.ua = extract.features(min, unterer.atem)
colnames(min.ua) = paste(colnames(min.ua), "min.ua", sep = ".")
max.ua = extract.features(max, unterer.atem)
colnames(max.ua) = paste(colnames(max.ua), "max.ua", sep = ".")

data = cbind(stability.oa,lumpiness.oa,std.oa,fluctanal.oa,min.oa,max.oa,
             stability.ua, lumpiness.ua, std.ua, fluctanal.ua,min.ua, max.ua)


# create label 
label.names = list.files(parent.folder, full.names = FALSE)
data$id = label.names

label.pos = c("...")

data.pos = data.frame(id = label.pos)
data.pos$label = 1

label.neg = c("...")

data.neg = data.frame(id = label.neg)
data.neg$label = 0

data.label = rbind(data.pos,data.neg)
data.label$id = as.character(data.label$id)

final = merge(x = data, y = data.label, by = "id", all = TRUE)



rm(list = ls()[!(ls() %in% c("final"))])