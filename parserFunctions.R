VPJ41086<- 30000
VPJ41086K<-1
VPJ41086Z<-2
#install.packages("BBmisc")
#library(BBBmisc)
string <- "VTJ41003' +IF(VPJ41086 <= -5000, 'A' , if(VPJ41086 > -5000 && VPJ41086 <= 0, 'B', if (VPJ41086 > 0 && VPJ41086 <= 5000, 'C', If (VPJ41086 > 5000 && VPJ41086 <= 10000, 'D', if (VPJ41086 > 10000 && VPJ41086 <= 15000, 'E', if(VPJ41086 > 15000 && VPJ41086 <= 20000, 'F', if(VPJ41086 > 20000 && VPJ41086 <= 25000, 'G', if(VPJ41086 > 25000 && VPJ41086 <= 30000, 'H', if(VPJ41086 > 30000 && VPJ41086 <= 35000, 'I', if (VPJ41086 > 35000 && VPJ41086 <= 40000, 'J', if (VPJ41086 > 40000 && VPJ41086 <= 50000, 'K', 'Z')))))))))))"
#eval(parse(text=string))
#syntex <- "IF(VPJ41086 <= -5000"
#ifclause <- trimws(paste0(gsub("if|IF|IF\\(|If|iF","if( ",syntex ),")"))

problem1 <- function(string){

#gsub("^IF", "if", string)
#install.packages("stringr")
library(stringr)
string1 <- strsplit(string, "\\+")[[1]][1]
string2 <- strsplit(string, "\\+")[[1]][2]
string3 <- gsub("[\\(\\)]", "", regmatches(string2, gregexpr("\\(.*?\\)", string2)))

string4 <- strsplit(string3,",")[[1]]
varible<- word(string4, 1)
varible[1]
#regmatches(string2, gregexpr("(?=\\().*?(?<=\\))", string2, perl=T))

while(length(string4) >= 3){
  
lastPart <- tail(string4,3)

ifclause <- trimws(paste0(gsub("if|IF|IF\\(|If|iF|if\\(|If\\(|iF\\(|^if","if( ",lastPart[1]) ,")"))
class(ifclause)
#if(nchar(word(ifclause, 2),type="width") > 3 ){
 
# print(nchar(word(ifclause, 2),type="width"))
 # varible<- word(ifclause, 2)
#}else {
 
#}

#print(varible)
#print(lastPart[3])
if(nchar(trimws(gsub("'","",lastPart[3])),type="width") < 3){
  
lastSyntex <- paste0(ifclause,"{ ",varible[1],trimws(gsub("'","",lastPart[2]))," } ","else { ",varible[1],trimws(gsub("'","",lastPart[3]))," }")
}else{
lastSyntex <- paste0(ifclause,"{ ",varible[1],trimws(gsub("'","",lastPart[2]))," } ","else { ",trimws(lastPart[3])," }")
}


string4 <- append(head(string4,-3),lastSyntex)
#print(string4)
}

string5 <- paste0(gsub("'","",string1)," + ","if( ",string4)
return (string5)

}
#problem1(string)

#is.error(eval(parse(text=problem1(string))))
######## problem 2 

string <- "IF(VAJ41177<0,VAJ41177,0)+IF(VAJ41178<0,VAJ41178,0)+IF(VAJ41179<0,VAJ41179,0)+IF(VAJ41180<0,VAJ41180,0)+IF(VAJ41181<0,VAJ41181,0)"
#gsub("IF\\(","if(","IF(VAJ41177<0")

problem2 <- function(string){
  
  library(stringr)

  string1 <- gsub("\\+| + ",",",string)
  string4 <- strsplit(string1,",")[[1]]
  #varible<- word(string4, 1)
  #varible[1]
  #regmatches(string2, gregexpr("(?=\\().*?(?<=\\))", string2, perl=T))
  
  while(length(string4) >= 3){
    
    lastPart <- tail(string4,3)
    
    #ifclause <- trimws(paste0(gsub("if|IF|IF\\(|If|iF|if\\(|If\\(|iF\\(|^if|^IF|IF\\(","if( ",lastPart[1]) ," )"))
    ifclause <- trimws(paste0(gsub("IF\\(","if( ",lastPart[1]) ," )"))
    lastPart[3]<- gsub("0\\)",0,lastPart[3])
    
    #if(nchar(word(ifclause, 2),type="width") > 3 ){
    
    # print(nchar(word(ifclause, 2),type="width"))
    # varible<- word(ifclause, 2)
    #}else {
    
    #}
    
    #print(varible)
    #print(lastPart[3])
    if(nchar(trimws(gsub("'","",lastPart[3])),type="width") < 3){
      
      lastSyntex <- paste0(ifclause,"{ ",trimws(gsub("'","",lastPart[2]))," } ","else { ",trimws(gsub("'","",lastPart[3]))," }")
    }else{
      lastSyntex <- paste0(ifclause,"{ ",trimws(gsub("'","",lastPart[2]))," } ","else { ",trimws(lastPart[3])," }")
    }
    
    
    string4 <- append(head(string4,-3),lastSyntex)
    string4 <- gsub("IF\\(","if(",string4)
    #print(string4)
  }
  
  
  return (string4)
  
}
#problem2(string)


#######end 

######## problem three

string <- "YZSI4310(PQ1)+YZSI4310(PQ2)+YZSI4310(PQ3)+YZSI4310(PQ4)"

problem3 <- function(string){
string1 <- strsplit(string, "\\+")[[1]]
string2 <- data.frame(strsplit(string1, "\\(")) 

string4 <- gsub("\\)", "", as.matrix(string2[,-1]))

return(string4)
}
#problem3(string)

#####end
#error function

is.error.element <- function(x){
  testError   <- inherits(x, "error")
  if(testError == TRUE){
    testSimple  <- inherits(x, "simpleError")
    errMsg      <- x$message
  } else {
    testSimple  <- FALSE
    errMsg      <- NA
  }
  return(data.frame(testError, testSimple, errMsg, stringsAsFactors = FALSE))
}

is.error <- function(testObject){
  quickTest <- is.error.element(testObject)
  if(quickTest$testError == TRUE){
    return(quickTest)
  } else {
    return(lapply(testObject, is.error.element))
  }
}


