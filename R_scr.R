library(readr)
library(xlsx)

source("/Users/naru/Documents/R_workshop/txtAdujstment/parserFunctions.R")
formulaType <- function(formula){
  
  #string1 <- strsplit(formula, "\\+")[[1]][1]
  string2 <- strsplit(formula, "\\+")[[1]][2]
  string3 <- gsub("[\\(\\)]", "", regmatches(string2, gregexpr("\\(.*?\\)", string2)))
  string4 <- strsplit(string3,",")[[1]]
  if(nchar(trimws(gsub("'","",string4[2])))==1){
   return( problem1(formula))
  }else{
    string1 <- gsub("\\+| + ",",",formula)
    string4 <- strsplit(string1,",")[[1]]
    
    if(nchar(string4[2]) >5){
      return(problem2(formula))
    }
    else{
      string1 <- strsplit(formula, "\\+")[[1]]
      if(nchar(string1[2]) >8){
        return(problem3(formula))
      }else(
        return(formula)
      )
    }
  }
}

string <- "VTJ41003' +IF(VPJ41086 <= -5000, 'A' , if(VPJ41086 > -5000 && VPJ41086 <= 0, 'B', if (VPJ41086 > 0 && VPJ41086 <= 5000, 'C', If (VPJ41086 > 5000 && VPJ41086 <= 10000, 'D', if (VPJ41086 > 10000 && VPJ41086 <= 15000, 'E', if(VPJ41086 > 15000 && VPJ41086 <= 20000, 'F', if(VPJ41086 > 20000 && VPJ41086 <= 25000, 'G', if(VPJ41086 > 25000 && VPJ41086 <= 30000, 'H', if(VPJ41086 > 30000 && VPJ41086 <= 35000, 'I', if (VPJ41086 > 35000 && VPJ41086 <= 40000, 'J', if (VPJ41086 > 40000 && VPJ41086 <= 50000, 'K', 'Z')))))))))))"

eval(parse(text=formulaType(string)))

##client code 

t_main <- read.csv("Excel/T_Main.csv")
r_adjust  <- read.csv("Excel/Rates_Adjustments.csv",header = TRUE)
rownames(r_adjust) <- r_adjust[, 1]
input <- read.csv("Excel/Input.csv")

head(t_main)
head(r_adjust)
head(input)
t_out <- t_main[,1:3]
head(t_out,150)
dim(t_out)
t_out$Cvalue <- NA
head(t_out)

for (i in 1:nrow(t_out)){
  code <- t_out[i,"Code"]
  formula <- t_out[i,"Formula"]
  period <- t_out[i,"Period"]
  
  dyn_vars <- input[input$Period == period,]

  for (j in 1:nrow(dyn_vars)) {
    assign(dyn_vars[j,"Code"], dyn_vars[j,"Value"]) 
  }
  
  t_out[t_out$Code == code & t_out$Period == period,]$Cvalue <- eval(parse(text=formulaType(formula)))
  
}
head(t_out) 
head(t_out)
write.csv(t_out, "Ouput.csv")
