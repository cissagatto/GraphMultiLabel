#############################################################################
# BUILD DATA FRAME FOR BUILD GRAPH IN MULTILABEL CLASSIFICATION
# Copyright (C) 2021
#
# This code is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version. This code is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
# Public License for more details.
#
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/)
# Campus Sao Carlos Computer Department (DC: https://site.dc.ufscar.br/)
# Program of Post Graduation in Computer Science
# (PPG-CC: http://ppgcc.dc.ufscar.br/) Bioinformatics and Machine Learning
# Group (BIOMAL: http://www.biomal.ufscar.br/)                                                                                                              #
#########################################################################


FolderRoot = "~/BuildDataFrameGraphMLC"
FolderScripts = paste(FolderRoot, "/R", sep="")

##################################################
# SPARSIFICATION WITH THRESHOLD
##################################################
spars_threshold <- function(parameters){

  # ordering
  df = arrange(parameters$graph, desc(similarity))

  # removing self loops
  df = df[c(-1:-parameters$Dataset.Info$Labels),]
  n_df = nrow(df) # lines number

  setwd(parameters$FolderSplit)
  # cat("\nThreshold: 0")
  write.csv(df, paste(parameters$Dataset.Name, "-threshold-0.csv", sep=""), row.names = FALSE)

  maximo = as.numeric(df[1,]$similarity) # maximum value similarity
  minimo = as.numeric(df[n_df,]$similarity) # minimum value similarity

  valores = c()

  k = 1
  a = 90
  while(a>0){
    cat("\nThreshold: ", k)
    maximo = (maximo*a)/100
    valores[k] = maximo
    df_1 = df[(df$similarity <= maximo),]
    setwd(parameters$FolderSplit)
    write.csv(df_1, paste(parameters$Dataset.Name, "-threshold-",k ,".csv", sep=""), row.names = FALSE)
    res1 = data.frame(count(df_1, from))
    res2 = data.frame(count(df_1, to))
    if((nrow(res1)!=parameters$Dataset.Info$Labels)&&((nrow(res2)!=parameters$Dataset.Info$Labels))){break}
    a = a - 10
    k = k + 1
    gc()
  }

  setwd(parameters$FolderSplit)
  valores = c(0, valores)
  n = length(valores)
  res = data.frame(threshold = seq(0,n-1,by=1), values = valores)
  write.csv(valores,  "sparsification-tr-values.csv", row.names = FALSE)
}


##################################################
# SPARSIFICATION WITH KNN
##################################################
spars_knn <- function(parameters){

  #cat("\n\tGet the labels names")
  setwd(parameters$Folders$folderNamesLabels)
  nomes_labels = data.frame(read.csv(paste(parameters$Dataset.Name,
                                           "-NamesLabels.csv", sep="")))
  colnames(nomes_labels) = c("i", "nomes")

  #cat("\n\tOrdering DF")
  df = arrange(parameters$graph, desc(parameters$similarity))
  df = df[c(-1:-parameters$Dataset.Info$Labels),] # removendo self loops
  #df2 = arrange(df, desc(from))

  #cat("\n\tRange max and min to generate random values")
  minimo = 1
  if(parameters$Dataset.Info$Labels<100){
    maximo=round((0.7*parameters$Dataset.Info$Labels))
  } else {
    maximo = 100
  }

  if(maximo < 5){
    #cat("\n\tMAXIMO < 5")
    values = seq(1, maximo, by=1)
    res = data.frame(knn = seq(1,maximo,by=1), values)
    setwd(parameters$FolderSplit)
    write.csv(res, "sparsification-knn-values.csv", row.names = FALSE)

    # from 1 to last
    j = 1
    for(j in 1:maximo){
      cat("\n\tK = ", values[j])
      df_graph_knn = data.frame()

      # do primeiro até o último rótulo
      i = 1
      for(i in 1:parameters$Dataset.Info$Labels){
        cat("\n\t\tlabel: ", i)
        df_res = df[df$from == toString(parameters$Labels.Names$nomes[i]),]
        df_res = df_res[order(df_res[,4], decreasing=TRUE), ]
        n_res = nrow(df_res)
        valor = values[j] + 1
        df_res[valor:n_res,]$weights = 0
        df_graph_knn = rbind(df_graph_knn, df_res)
        i = i + 1
        gc()
      } # end for

      setwd(parameters$FolderSplit)
      write.csv(df_graph_knn, paste(parameters$Dataset.Name, "-knn-", j, ".csv", sep=""),
                row.names = FALSE)

      df_graph_knn_2 <- df_graph_knn[rowSums(is.na(df_graph_knn)) == 0, ]
      write.csv(df_graph_knn_2, paste("v2-", parameters$Dataset.Name,
                                      "-knn-", j, ".csv", sep=""),
                row.names = FALSE)

      #nrow(df_graph_knn)
      #nrow(df_graph_knn_2)

      j = j + 1
      gc()
    } # end for

  } else {
    #cat("\n\tMINIMO != MAXIMO")
    set.seed(123)
    values = sample(minimo:maximo, 5)
    n = 5
    res = data.frame(knn=seq(1,n,by=1), values)
    setwd(parameters$FolderSplit)
    write.csv(res, "sparsification-knn-values.csv", row.names = FALSE)

    #if(is.na(values)){break}

    # from 1 to last
    j = 1
    for(j in 1:n){
      cat("\n\tK = ", values[j])
      df_graph_knn = data.frame()

      # do primeiro até o último rótulo
      i = 1
      for(i in 1:parameters$Dataset.Info$Labels){
        cat("\n\t\tlabel: ", i)
        df_res = df[df$from == toString(parameters$Labels.Names$nomes[i]),]
        df_res = df_res[order(df_res[,4], decreasing=TRUE), ]
        n_res = nrow(df_res)
        valor = values[j] + 1
        df_res[valor:n_res,]$weights = 0
        df_graph_knn = rbind(df_graph_knn, df_res)
        i = i + 1
        gc()
      } # end for

      #cat("\n\tPRUNNING")
      df_graph_knn = df_graph_knn[order(df_graph_knn[,3], decreasing=TRUE), ]

      setwd(parameters$FolderSplit)
      write.csv(df_graph_knn, paste(parameters$Dataset.Name, "-knn-", j, ".csv", sep=""),
                row.names = FALSE)

      j = j + 1
      gc()
    } # end for
  } # fim do else

} # end function


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
