rm(list = ls())

###############################################################################
# BUILD DATA FRAME GRAPH MLC
# Copyright (C) 2022
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
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus
# Sao Carlos Computer Department (DC: https://site.dc.ufscar.br/)
# Program of Post Graduation in Computer Science
# (PPG-CC: http://ppgcc.dc.ufscar.br/)
# Bioinformatics and Machine Learning Group
# (BIOMAL: http://www.biomal.ufscar.br/)
#
###############################################################################


FolderRoot = "~/BuildDataFrameGraphMLC"
FolderScripts = paste(FolderRoot, "/R", sep="")


###############################################################################
# LOAD LIBRARY/PACKAGE                                                        #
###############################################################################
setwd(FolderScripts)
source("libraries.R")


###############################################################################
# READING DATASET INFORMATION FROM DATASETS-ORIGINAL.CSV                      #
###############################################################################
setwd(FolderRoot)
datasets = data.frame(read.csv("datasets-original.csv"))
n = nrow(datasets)


###############################################################################
# CREATING FOLDER TO SAVE CONFIG FILES                                        #
###############################################################################
FolderCF = paste(FolderRoot, "/config-files", sep = "")
if (dir.exists(FolderCF) == FALSE) {dir.create(FolderCF)}

similarity = c("jaccard", "rogers.tanimoto")
sim = c("j", "ro")

s = 1
while(s<=length(similarity)){

  FolderS = paste(FolderCF, "/", similarity[s], sep = "")
  if (dir.exists(FolderS) == FALSE) {dir.create(FolderS)}

  d = 1
  while (d <= n) {

    ds = datasets[d, ]

    cat("\n\n#===============================================")
    cat("\n# Similarity \t", similarity[s])
    cat("\n# Dataset \t", ds$Name)
    cat("\n#===============================================")

    name = paste("m-", sim[s], "-", ds$Name, sep = "")

    file_name = paste(FolderS, "/", name, ".csv", sep = "")

    output.file <- file(file_name, "wb")

    write("Config, Value", file = output.file, append = TRUE)

    # write("Dataset_Path, /home/u704616/Datasets", file = output.file, append = TRUE)
    # write("Dataset_Path, /home/cissa/Datasets", file = output.file, append = TRUE)

    write("Dataset_Path, ~/BuildDataFrameGraphMLC/Datasets", file = output.file, append = TRUE)

    # folder_name = paste("\"/scratch/", job_name, "\"", sep = "")
    # folder_name = paste("~/Exhaustive-MiF1-ECC/", job_name, sep = "")
    # folder_name = paste("~/tmp/", job_name, sep = "")
    folder_name = paste("/dev/shm/", name, sep = "")
    # folder_name = paste("/scratch/", job_name, sep = "")

    # pasta para fazer o processamento
    str1 = paste("Temporary_Path, ", folder_name, sep = "")
    write(str1, file = output.file, append = TRUE)

    # pasta para salvar os resultados finais
    # str5 = paste("/home/cissa/Partitions/Similarities/", similarity[s], sep = "")

    str5 = paste("~/BuildDataFrameGraphMLC/Similarities/", similarity[s], sep = "")
    str2 = paste("Similarities_Path, ", str5, sep = "")
    write(str2, file = output.file, append = TRUE)

    str4 = paste("Similarity, ", similarity[s], sep = "")
    write(str4, file = output.file, append = TRUE)

    # dataset name
    str3 = paste("Dataset_Name, ", ds$Name, sep = "")
    write(str3, file = output.file, append = TRUE)

    # Dataset number according to "datasets-original.csv" file
    str2 = paste("Number_Dataset, ", ds$Id, sep = "")
    write(str2, file = output.file, append = TRUE)

    # Number used for X-Fold Cross-Validation
    write("Number_Folds, 10", file = output.file, append = TRUE)

    # Number of cores to use for parallel processing
    write("Number_Cores, 2", file = output.file, append = TRUE)

    # finish writing to the configuration file
    close(output.file)

    # increment
    d = d + 1
    gc()
  } # fim do dataset


  s = s + 1
  gc()
}




rm(list = ls())

###############################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                #
# Thank you very much!                                                        #                                #
###############################################################################
