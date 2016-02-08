# Load libraries (install these, if you haven't)
library(plyr)
library(magrittr)
library(igraph)
library(statnet)

#Set your working directory
#setwd('/Users/bobminnich/Documents/Columbia/Courses/DataVisualization/EDAV_Project1')
setwd("~/Documents/3. Exploratory Data Analysis and Visualization/3. HW/HW1/Git/test")

#Load CSV file into Data Frame
df = read.csv("edit_data.csv")

col.list = c("Matlab", "R", "Excel", "RStudio", "ggplot2", "Stata", "Sweave/knitr", 
             "SPSS", "lattice", "Github", "LaTeX", "google drive (formerly docs)", 
             "dropbox", "SQL", "shell (terminal / command line)",  "C/C++", "Python", 
             "regular expressions (grep)", "XML", "Web: html css js")


#Count Columns with NAs
#na.check = df%>%is.na() %>% apply(2,sum)

#Remove NAs
#df_clean = df[,which(na.check==0)]
df_clean = df

#Create colums initializing at 0
df_clean[,col.list] = 0

for(i in col.list){
  #Need an If Statement because of R vs RStudio. 
  if(i == "R"){ 
    #Use Reg expressions "R,|R$" which looks for "R," and for "R$" which means there is nothing after R (line 87 caused this issue)
    fnd = "R,|R$"
    #try to find fnd within the vector, return Row # if True
    rows = grep(pattern = fnd, x = df_clean$Experiences.with.tools)
  }else{
    #Same as above
    fnd = paste(i, sep = "")
    rows = grep(pattern = fnd, x = df_clean$Experiences.with.tools, fixed = TRUE)
  }
  df_clean[rows,i] = 1
}




df_clean = data.frame(df_clean)

colnames(df_clean)[c(18, 23, 24, 26, 27, 29, 31)] <- c("Sweave_knitr", "Google Drive", "Dropbox", 
                                                       "Shell", "C_CPP", "Regular Expression", "Web")    
    # From (Sweave/knitr, google drive (formerly docs), dropbox, 
    # shell (terminal / command line), C/C++, regular expressions (grep), Web: html css js)

# Modified skill names
sk_list = c("Matlab", "R", "Excel", "RStudio", "ggplot2", "Stata", "Sweave_knitr", 
            "SPSS", "lattice", "Github", "LaTeX", "Google Drive", "Dropbox", "SQL", 
            "Shell",  "C_CPP", "Python", "Regular Expression", "XML", "Web")

sk_sum <- apply(df_clean[12:31], 2, sum) # Total number of students who chose it

# Category of each skill set, we can adjust as needed. (ST: Statistics, GE: General, CS: Computer Science)
area_list <- c("ST", "ST", "ST", "ST", "ST", "ST", "ST", "ST", "ST", "GE", "GE", "GE", "GE", 
               "CS", "CS", "CS", "CS", "CS", "CS", "CS")
sk_set <- data.frame(sk_sum, area_list) # Summary of skill set

prog.list <- c("IDSE (master)", "Data Science Certification", "Statistics (master)", "Other masters", "Ph.D.")
prog_set <- count(df_clean, "Program") # Summary of program (number of students)


# Adjacency matrix (+1 when a student selected both skills)
num_student = nrow(df_clean)
num_sk = length(sk_list)
sk_rel = data.frame(matrix(rep(0,num_sk*num_sk), ncol = num_sk, nrow = num_sk))
colnames(sk_rel) = sk_list
rownames(sk_rel) = sk_list

for(i in 1:num_student) {
  for(sk1 in sk_list) {
    for(sk2 in sk_list) {
      if((df_clean[i, sk1] == 1 && df_clean[i, sk2] == 1) && (sk1 != sk2)) {
        sk_rel[sk1, sk2] = sk_rel[sk1, sk2] + 1
        # Set weight between same skill (Matlab, Matlab) as 0
      }
    }
  }
}


# Drawing graph
fradj = as.matrix(sk_rel)
colnames(fradj) = sk_list
frnet = graph.adjacency(fradj, weight=TRUE, mode="undirected")

V(frnet)$size = sk_sum # Size of vertices
V(frnet)$color = rgb(1,1,0.5,0.5) # Color of vertices

E(frnet)$label = E(frnet)$weight # Label of edge
#E(frnet)$label = NA

plot.igraph(frnet, layout=layout.fruchterman.reingold, edge.width=E(frnet)$weight/10)
tkplot(frnet, edge.width=E(frnet)$weight/10)
