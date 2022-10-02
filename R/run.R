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


FolderRoot = "~/GraphMultiLabel"
FolderScripts = paste(FolderRoot, "/R", sep="")


#################################################################
# MAIN FUNCTIOn
#################################################################
execute <- function(parameters){

  FolderRoot = "~/GraphMultiLabel"
  FolderScripts = paste(FolderRoot, "/R", sep="")
  
  if(parameters$Number.Cores  == 0){
    cat("\n\n##################################################################################################")
    cat("\n# Zero is a disallowed value for number_cores. Please choose a value greater than or equal to 1. #")
    cat("\n##################################################################################################\n\n")
  } else {
    cl <- parallel::makeCluster(parameters$Number.Cores)
    doParallel::registerDoParallel(cl)
    print(cl)

    if(parameters$Number.Cores==1){
      cat("\n\n###########################################################")
      cat("\n# RUN: Running Sequentially!                              #")
      cat("\n###########################################################\n\n")
    } else {
      cat("\n\n######################################################################")
      cat("\n# RUN: Running in parallel with ", parameters$Number.Cores, " cores! #")
      cat("\n######################################################################\n\n")
    }
  }

  retorno = list()

  setwd(FolderScripts)
  source("libraries.R")

  setwd(FolderScripts)
  source("utils.R")

  f = 1
  foldsParalel <- foreach(f = 1:number_folds) %dopar% {

    FolderRoot = "~/GraphMultiLabel"
    FolderScripts = paste(FolderRoot, "/R/", sep="")

    setwd(FolderScripts)
    source("utils.R")

    setwd(FolderScripts)
    source("libraries.R")

    setwd(FolderScripts)
    source("build.R")

    Folder = paste(parameters$Folders$folderResults, "/DFG", sep="")
    if(dir.exists(Folder)==FALSE){dir.create(Folder)}
    parameters$Folder = Folder

    FolderSplit = paste(Folder, "/Split-", f, sep="")
    if(dir.exists(FolderSplit)==FALSE){dir.create(FolderSplit)}
    parameters$FolderSplit = FolderSplit

    FolderSimSplit = paste(parameters$Folders$folderSimilarities,
                           "/Split-", f, sep="")
    parameters$FolderSimSplit = FolderSimSplit

    #########################################################################
    setwd(parameters$Folders$folderNamesLabels)
    nomes_labels = data.frame(read.csv(paste(parameters$Dataset.Name,
                                             "-NamesLabels.csv", sep="")))
    colnames(nomes_labels) = c("i", "nomes")
    parameters$Labels.Names = nomes_labels

    file.name = paste(parameters$similarity, ".csv", sep="")

    cat("\nSplit:", f, "\tSimilarity:", parameters$similarity)

    #########################################################################
    #cat("\nOpen CSV similarity result and organize the data frame")
    setwd(FolderSimSplit)
    sim = data.frame(read.csv(file.name, stringsAsFactors = FALSE))
    rownames(sim) = sim$X
    sim = sim[,-1]
    sim.2 = as.matrix(sim)

    #########################################################################
    #cat("\nCONVERT DATA FRAME IN TABELA WITH 2 COLUMNS")
    df_graph = data.frame(pivot_longer(sim,
                                       cols = 1:parameters$Dataset.Info$Labels))
    colnames(df_graph) = c("to", "similarity")

    #####################################################################
    #cat("\nFunction Aditional labels")
    create.pivot.labels <- function(nomes_labels){
      i = 1
      j = 1
      a = 1
      labels.pivot = c()
      for(i in 1:parameters$Dataset.Info$Labels){
        for(j in 1:parameters$Dataset.Info$Labels){
          labels.pivot[a] = nomes_labels$nomes[i]
          a = a + 1
        }
      }
      return(labels.pivot)
    }

    # creating
    from = create.pivot.labels(nomes_labels)

    # join collumns
    df_graph = cbind(from, df_graph)

    #########################################################################
    #cat("\nNormalized data - range 0 and 1")
    res = data.frame(between(df_graph$similarity, 0, 1))
    normalized <- function(x){(x-min(x))/(max(x)-min(x))}
    df_graph_norm = df_graph %>% mutate(similarity = normalized(df_graph$similarity))
    df_graph_norm = cbind(df_graph_norm, weights = df_graph_norm$similarity)

    parameters$graph = df_graph_norm

    setwd(FolderSplit)
    write.csv(df_graph_norm, paste(parameters$similarity,
                                   "-graph-norm.csv", sep=""),
              row.names = FALSE)

    #########################################################################
    #cat("\nPlot Graph")
    setwd(FolderSplit)
    graph_dataset = graph_from_data_frame(df_graph_norm, directed=FALSE)
    nome_3 = paste(parameters$similarity, "-graph.pdf", sep="")
    pdf(nome_3, width = 10, height = 8)
    print(plot(graph_dataset))
    dev.off()
    cat("\n")

    #########################################################################
    #cat("\nCall Sparsification TR \n")
    spars_threshold(parameters)

    #cat("\nCall Sparsification KNN \n")
    spars_knn(parameters)

    gc()

  } # fim do for each

  cat("\n\n################################################################################################")
  cat("\nRun: Stop Parallel                                                                              #")
  parallel::stopCluster(cl)
  cat("\n##################################################################################################\n\n")

  cat("\n######################################################################")
  cat("\n# END RUN.R                                                          #")
  cat("\n######################################################################")
  cat("\n\n\n\n")

} # fim da função



##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
