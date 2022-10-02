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


##############################################################################
# FUNCTION
##############################################################################
createDirs <- function(parametere){

  retorno = list()

  FolderRoot = "~/BuildDataFrameGraphMLC"
  FolderScripts = paste(FolderRoot, "/R", sep="")

  FolderScripts = paste(FolderRoot, "/R", sep="")
  retorno$FolderScripts = FolderScripts

  #############################################################################
  # RESULTS FOLDER:                                                           #
  # Parameter from command line. This folder will be delete at the end of the #
  # execution. Other folder is used to store definitely the results.          #
  # Example: "/dev/shm/j-GpositiveGO"                                         #
  #############################################################################
  folderResults = parameters$Folder.Results
  retorno$folderResults = folderResults
  if(dir.exists(folderResults) == TRUE){
    setwd(folderResults)
    dir_folderResults = dir(folderResults)
    n_folderResults = length(dir_folderResults)
  } else {
    dir.create(folderResults)
    setwd(folderResults)
    dir_folderResults = dir(folderResults)
    n_folderResults = length(dir_folderResults)
  }

  #############################################################################
  # DATASETS FOLDER:                                                          #
  # Get the information within DATASETS folder that already exists in the     #
  # project. This folder store the files from cross-validation and will be    #
  # use to get the label space to modeling the label correlations and         #
  # compute silhouete to choose the best hybrid partition.                    #
  # "/dev/shm/j-GpositiveGO/datasets"                                         #
  #############################################################################
  folderDatasets = paste(folderResults, "/datasets", sep="")
  retorno$folderDatasets = folderDatasets
  if(dir.exists(folderDatasets) == TRUE){
    setwd(folderDatasets)
    dir_folderDatasets = dir(folderDatasets)
    n_folderDatasets = length(dir_folderDatasets)
  } else {
    dir.create(folderDatasets)
    setwd(folderDatasets)
    dir_folderDatasets = dir(folderDatasets)
    n_folderDatasets = length(dir_folderDatasets)
  }

  #############################################################################
  # LABEL SPACE FOLDER:                                                       #
  # Path to the specific label space from the dataset that is runing.         #
  # This folder store the label space for each FOLD from the cross-validation #
  # which was computed in the Cross-Validation Multi-Label code.              #
  # In this way, we don't need to load the entire dataset into the running    #
  # "/dev/shm/j-GpositiveGO/datasets/LabelSpace"                              #
  #############################################################################
  folderLabelSpace = paste(folderDatasets, "/LabelSpace", sep="")
  retorno$folderLabelSpace = folderLabelSpace
  if(dir.exists(folderLabelSpace) == TRUE){
    setwd(folderLabelSpace)
    dir_folderLabelSpace = dir(folderLabelSpace)
    n_folderLabelSpace = length(dir_folderLabelSpace)
  } else {
    dir.create(folderLabelSpace)
    setwd(folderLabelSpace)
    dir_folderLabelSpace = dir(folderLabelSpace)
    n_folderLabelSpace = length(dir_folderLabelSpace)
  }


  #############################################################################
  # NAMES LABELS FOLDER:                                                      #
  # Get the names of the labels from this dataset. This will be used in the   #
  # code to create the groups for each partition. Is a way to guarantee the   #
  # use of the correct names labels.                                          #
  # "/dev/shm/j-GpositiveGO/datasets/NamesLabels"                             #
  #############################################################################
  folderNamesLabels = paste(folderDatasets, "/NamesLabels", sep="")
  retorno$folderNamesLabels = folderNamesLabels
  if(dir.exists(folderNamesLabels) == TRUE){
    setwd(folderNamesLabels)
    dir_folderNamesLabels = dir(folderNamesLabels)
    n_folderNamesLabels = length(dir_folderNamesLabels)
  } else {
    dir.create(folderNamesLabels)
    setwd(folderNamesLabels)
    dir_folderNamesLabels = dir(folderNamesLabels)
    n_folderNamesLabels = length(dir_folderNamesLabels)
  }


  #############################################################################
  # CROSS VALIDATION FOLDER:                                                  #
  # Path to the folders and files from cross-validation for the specific      #
  # dataset                                                                   #
  # "/dev/shm/j-GpositiveGO/datasets/CrossValidation"                         #
  #############################################################################
  folderCV = paste(folderDatasets, "/CrossValidation", sep="")
  retorno$folderCV = folderCV
  if(dir.exists(folderCV) == TRUE){
    setwd(folderCV)
    dir_folderCV = dir(folderCV)
    n_folderCV = length(dir_folderCV)
  } else {
    dir.create(folderCV)
    setwd(folderCV)
    dir_folderCV = dir(folderCV)
    n_folderCV = length(dir_folderCV)
  }


  #############################################################################
  # TRAIN CROSS VALIDATION FOLDER:                                            #
  # Path to the train files from cross-validation for the specific dataset    #                                                                   #
  # "/dev/shm/j-GpositiveGO/datasets/CrossValidation/Tr"                      #
  #############################################################################
  folderCVTR = paste(folderCV, "/Tr", sep="")
  retorno$folderCVTR = folderCVTR
  if(dir.exists(folderCVTR) == TRUE){
    setwd(folderCVTR)
    dir_folderCVTR = dir(folderCVTR)
    n_folderCVTR = length(dir_folderCVTR)
  } else {
    dir.create(folderCVTR)
    setwd(folderCVTR)
    dir_folderCVTR = dir(folderCVTR)
    n_folderCVTR = length(dir_folderCVTR)
  }


  #############################################################################
  # TEST CROSS VALIDATION FOLDER:                                             #
  # Path to the test files from cross-validation for the specific dataset     #                                                                   #
  # "/dev/shm/j-GpositiveGO/datasets/CrossValidation/Ts"                      #
  #############################################################################
  folderCVTS = paste(folderCV, "/Ts", sep="")
  retorno$folderCVTS = folderCVTS
  if(dir.exists(folderCVTS) == TRUE){
    setwd(folderCVTS)
    dir_folderCVTS = dir(folderCVTS)
    n_folderCVTS = length(dir_folderCVTS)
  } else {
    dir.create(folderCVTS)
    setwd(folderCVTS)
    dir_folderCVTS = dir(folderCVTS)
    n_folderCVTS = length(dir_folderCVTS)
  }


  #############################################################################
  # VALIDATION CROSS VALIDATION FOLDER:                                       #
  # Path to the validation files from cross-validation for the specific       #
  # dataset                                                                   #
  # "/dev/shm/j-GpositiveGO/datasets/CrossValidation/Vl"                      #
  #############################################################################
  folderCVVL = paste(folderCV, "/Vl", sep="")
  retorno$folderCVVL = folderCVVL
  if(dir.exists(folderCVVL) == TRUE){
    setwd(folderCVVL)
    dir_folderCVVL = dir(folderCVVL)
    n_folderCVVL = length(dir_folderCVVL)
  } else {
    dir.create(folderCVVL)
    setwd(folderCVVL)
    dir_folderCVVL = dir(folderCVVL)
    n_folderCVVL = length(dir_folderCVVL)
  }


  #############################################################################
  # FOLDER PARTITIONS                                                         #
  # "/dev/shm/j-GpositiveGO/Partitions"                                       #
  #############################################################################
  folderSimilarities = paste(folderResults, "/Similarities", sep="")
  retorno$folderSimilarities = folderSimilarities
  if(dir.exists(folderSimilarities) == TRUE){
    setwd(folderSimilarities)
    dir_folderSimilarities = dir(folderSimilarities)
    n_folderSimilarities = length(dir_folderSimilarities)
  } else {
    dir.create(folderSimilarities)
    setwd(folderSimilarities)
    dir_folderSimilarities = dir(folderSimilarities)
    n_folderSimilarities = length(dir_folderSimilarities)
  }


  #############################################################################
  # FOLDER PARTITIONS                                                         #
  # "/dev/shm/j-GpositiveGO/Partitions"                                       #
  #############################################################################
  folderRS = paste(FolderRoot, "/Similarities", sep="")
  retorno$folderRS = folderRS
  if(dir.exists(folderRS) == TRUE){
    setwd(folderRS)
    dir_folderRS = dir(folderRS)
    n_folderRS = length(dir_folderRS)
  } else {
    dir.create(folderRS)
    setwd(folderRS)
    dir_folderRS = dir(folderRS)
    n_folderRS = length(dir_folderRS)
  }


  #############################################################################
  # FOLDER PARTITIONS                                                         #
  # "/dev/shm/j-GpositiveGO/Partitions"                                       #
  #############################################################################
  folderDFG = paste(FolderRoot, "/DataFramesGraph", sep="")
  retorno$folderDFG  = folderDFG
  if(dir.exists(folderDFG ) == TRUE){
    setwd(folderDFG )
    dir_folderDFG = dir(folderDFG )
    n_folderDFG = length(dir_folderDFG )
  } else {
    dir.create(folderDFG )
    setwd(folderDFG )
    dir_folderDFG = dir(folderDFG )
    n_folderDFG = length(dir_folderDFG )
  }

  return(retorno)
  gc()

}



################################################################################################
# FUNCTION INFO DATA SET                                                                       #
#  Objective                                                                                   #
#     Gets the information that is in the "datasets-hpmlk.csv" file.                           #
#  Parameters                                                                                  #
#     dataset: the specific dataset                                                            #
#  Return                                                                                      #
#     Everything in the "datasets-hpmlk.csv" file.                                             #
################################################################################################
infoDataSet <- function(dataset){

  retorno = list()

  retorno$id = dataset$ID
  retorno$name = dataset$Name
  retorno$instances = dataset$Instances
  retorno$inputs = dataset$Inputs
  retorno$labels = dataset$Labels
  retorno$LabelsSets = dataset$LabelsSets
  retorno$single = dataset$Single
  retorno$maxfreq = dataset$MaxFreq
  retorno$card = dataset$Card
  retorno$dens = dataset$Dens
  retorno$mean = dataset$Mean
  retorno$scumble = dataset$Scumble
  retorno$tcs = dataset$TCS
  retorno$attStart = dataset$AttStart
  retorno$attEnd = dataset$AttEnd
  retorno$labStart = dataset$LabelStart
  retorno$labEnd = dataset$LabelEnd
  retorno$distinct = dataset$Distinct
  retorno$xn = dataset$xn
  retorno$yn = dataset$yn
  retorno$gridn = dataset$gridn

  return(retorno)

  gc()
}

##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
