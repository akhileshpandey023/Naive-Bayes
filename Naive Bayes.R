NB <- function(dataTable,class){
  as.data.frame(dataTable) -> dataTable
  #Saving the index of the column of the Entered class
  grep(class, colnames(dataTable)) -> temp
  #Check if the table is "Frequency" distributed.
  #If there is a column called 'Freq', expand the table,
  #else, keep the table
  for(i in 1:ncol(dataTable)){
    if(((colnames(dataTable)[i] == "Freq"))){
      dataTable[rep(1:nrow(dataTable), dataTable[["Freq"]]), ] -> expandedDataTable
      checkFlag <- 1
      expandedDataTable[,-i] -> expandedDataTable
    }
  }
  if(checkFlag != 1){
    dataTable -> expandedDataTable
  }
  #Saving the column names other than the Entered class column
  colnames(expandedDataTable[,-temp]) -> otherClasses
  #Saving the different class values in a matrix for other column classes
  lapply(expandedDataTable[,-temp], levels) -> otherClassesLevels
  #Saving the class values of the User entered class
  as.vector(unique(expandedDataTable[,temp])) -> classValues
  #We will iterate through each of the different class labels
  #and get the count of other different labels in the other columns
  print(noquote('Apriori Proababilities:'))
  for(i in 1:length(classValues)){
    expandedDataTable[which(expandedDataTable[,temp]==classValues[i]),] -> classTable
    classTable[,-temp] -> subsetClassTable
    nrow(classTable) -> classNumbers
    classNumbers/nrow(expandedDataTable) -> classProbability
    print(noquote(paste(classValues[i],':',classProbability)))
    for(j in 1:NCOL(subsetClassTable)){
      as.data.frame(otherClassesLevels[j]) -> df.otherClassLevels
      colnames(as.data.frame(otherClassesLevels))[j] -> colNames
      for(k in 1:NROW(df.otherClassLevels)){
        length(which(as.character(df.otherClassLevels[k,])==subsetClassTable[,j])) -> otherClassesLevelsCount
        as.character(df.otherClassLevels[k,]) -> variable
        print(noquote(paste(classValues[i],'and', colNames,'=',as.character(variable),'is', otherClassesLevelsCount/classNumbers)))
      }
    }
  }
}
