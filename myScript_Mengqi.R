#Set your working directory
setwd('Documents/cuw4701_edav/')

#Load CSV file into Data Frame
df = read.csv("Survey+Response.csv")

col.list = c("Matlab", "R", "Github", "Excel", "SQL", "RStudio", "ggplot2", "shell (terminal / command line)", "C/C++", "Python", "Stata", "LaTeX", "XML", "Web: html css js", "google drive (formerly docs)", "Sweave/knitr","dropbox", "SPSS", "regular expressions (grep)", "lattice" )

#Create colums initializing at 0
df[,col.list] = 0

for(i in col.list){
  #Need an If Statement because of R vs RStudio. 
  if(i == "R"){ 
    #Use Reg expressions "R,|R$" which looks for "R," and for "R$" which means there is nothing after R (line 87 caused this issue)
    fnd = "R,|R$"
    #try to find fnd within the vector, return Row # if True
    rows = grep(pattern = fnd, x = df$Experiences.with.tools)
  }else{
    #Same as above
    fnd = paste(i, sep = "")
    rows = grep(pattern = fnd, x = df$Experiences.with.tools, fixed = TRUE)
  }
  df[rows,i] = 1
}

write.csv(df_clean, 'df_clean.csv', row.names=FALSE)


separate_major <- function(survey){
  IDSE = which(survey$Program == "IDSE (master)")
  Other = which(survey$Program == "Other masters")
  DSC = which(survey$Program == "Data Science Certification")
  STATS = which(survey$Program == "Statistics (master)")
  QMSS = which(survey$Program == "QMSS")
  DS = which(survey$Program == "Data Science")
  MS_DS = which(survey$Program == "Ms in ds")
  MS_QMSS = which(survey$Program == "QMSS (master)")
  PHD = which(survey$Program == "Ph.D.")
  APP_MATH = which(survey$Program == "Applied Math")
  PHD_BIO = which(survey$Program == "PhD Biomedical Informatics")
  MSDS = which(survey$Program == "MSDS")
  
  
  MSDS = c(MSDS, IDSE, DSC, DS, MS_DS)
  QMSS = c(QMSS, MS_QMSS)
  PHD = c(PHD, PHD_BIO)
  OTHER = c(PHD, APP_MATH, Other)
  a = list(MSDS, QMSS, STATS, Other)
  return(a) 
}

# filter features
df_clean <- df[,c(39:52)]
colnames(df_clean)[8] = 'Shell'
colnames(df_clean)[14] = 'Web'
#colnames(df_clean)[2] = "ProgrammingProficiency"
#df_clean[13, 1] = df_clean[1, 1]
majors = separate_major(df)
MSDS = colSums(df_clean[majors[[1]],])
QMSS = colSums(df_clean[majors[[2]],])
STATS = colSums(df_clean[majors[[3]],])
Other = colSums(df_clean[majors[[4]],])


# Chord Diagram
install.packages("circlize")
library(circlize)

df_cd = rbind(MSDS, QMSS, STATS, Other)
df_cdc = data.frame(from = rep(rownames(df_cd), times = ncol(df_cd)), to = rep(colnames(df_cd), each = nrow(df_cd)),
                value = as.vector(df_cd),
                stringsAsFactors = FALSE)
grid.col = NULL
grid.col[colnames(df_clean)] = 'grey'
grid.col[c("MSDS", "QMSS", "STATS", "Other")] = c('blue', 'green', 'red', 'yellow')
chordDiagram(df_cdc, grid.col = grid.col)
