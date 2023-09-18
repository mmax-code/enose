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

oberer.atem = read("A.xlsx")
unterer.atem = read("B.xlsx")



transformed_data = list()
for (i in 1:length(oberer.atem)){
  tmp = as.data.frame(oberer.atem[[i]], col.names = "")
  tmp_2 = data.frame(Name = paste(colnames(tmp)[col(tmp)],row.names(tmp)[row(tmp)],
                                  sep= '_'), Val = as.vector(as.matrix(tmp[,])))
  row.names(tmp_2) = tmp_2$Name
  tmp_2$Name = NULL
  tmp_2 = as.data.frame(t(tmp_2))
  transformed_data[[i]] = tmp_2
}

data.oberer = bind_rows(transformed_data)


transformed_data = list()
for (i in 1:length(unterer.atem)){
  tmp = as.data.frame(unterer.atem[[i]], col.names = "")
  tmp_2 = data.frame(Name = paste(colnames(tmp)[col(tmp)],row.names(tmp)[row(tmp)],
                                  sep= '_'), Val = as.vector(as.matrix(tmp[,])))
  row.names(tmp_2) = tmp_2$Name
  tmp_2$Name = NULL
  tmp_2 = as.data.frame(t(tmp_2))
  transformed_data[[i]] = tmp_2
}

data.unterer = bind_rows(transformed_data)


colnames(data.oberer) <- paste(colnames(data.oberer), "Oberer", sep = "_")
colnames(data.unterer) <- paste(colnames(data.unterer), "Unterer", sep = "_")


data_oberer_unterer = cbind(data.oberer, data.unterer)




# create label 
label.names = list.files(parent.folder, full.names = FALSE)
data_oberer_unterer$id = label.names

label.pos = c("...")

data.pos = data.frame(id = label.pos)
data.pos$label = 1

label.neg = c("...")

data.neg = data.frame(id = label.neg)
data.neg$label = 0

data.label = rbind(data.pos,data.neg)
data.label$id = as.character(data.label$id)

final = merge(x = data_oberer_unterer, y = data.label, by = "id", all = TRUE)


rm(list = ls()[!(ls() %in% c("final"))])