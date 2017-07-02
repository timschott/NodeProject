## Tim schott Node Project
setwd("~/Documents/4thSemester/theNode")
library(rvest)
library(qdapRegex)
library(dplyr)
library(stringr)
library(readr)

#gather TML data. 
tml_page <- read_html("http://www.modernlibrary.com/top-100/100-best-novels/") #url we'll be using
critics <- html_nodes(tml_page, ".list-100")
critics <- html_text(critics, trim=FALSE)
critics<-critics[1] #only want the critics col, not the readers
new_crit = strsplit(critics,"\n") #separate into a list.

temp_vect <-c("")
temp_vect[1:103] <- new_crit[[1]]
temp_vect<-temp_vect[3:102]

temp <-strsplit(temp_vect, "by")
#temp[[1]][1] <- "dummy     entry"
#temp[[1]][103] <-"another    dummy"
mat<- matrix(unlist(temp), ncol=2, byrow=TRUE) #coerce into matrix
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
mat[,1] <- trim(mat[,1])
mat[,2] <- trim(mat[,2])

colnames(mat) <- c("Book", "Author")
#write.csv(mat, "books_authors.csv")

#gather goodreads data 
goodreads_page <- read_html("https://www.goodreads.com/list/show/9366.Modern_Library_100_Best_Novels_The_Readers_List")
readers <- html_nodes(goodreads_page,"td")
readers <- html_text(readers, trim=FALSE)
test<-rm_white(readers)
my_seq<-seq(3,399, by=4)
reader_data<-test[my_seq]
reader_data <-gsub(" *\\(.*?\\) *", "", reader_data[1:100])

reader_data[2] <- "1984 by\n George Orwell"
reader_data[4] <- "Fahrenheit 451 by\n Ray Bradbury"
reader_data[19] <- "Catch-22 by\n Joseph Heller"
reader_data[25] <- "I, Claudius by\n Robert Graves"
reader_data[30] <- "The Lord of the Rings by\n J.R.R. Tolkien"
reader_data[37] <- "The Hitchhiker's Guide to the Galaxy by\n Douglas Adams"              
reader_data[38] <- "Ender's Game by\n Orson Scott Card"
reader_data[49] <- "Watership Down by\n Richard Adams"
reader_data[65] <- "Battlefield Earth: A Saga of the Year by\n L. Ron Hubbard"
reader_data[68] <- "The Invaders Plan by\n L. Ron Hubbard"
reader_data[74] <- "Someplace to be FLying by\n Charles de Lint"
reader_data[77] <- "Memory and Dream by\n Charles de Lint"
reader_data[78] <- "Trader by\n Charles de Lint"
reader_data[83] <- "Something Wicked This Way Comes by\n Ray Bradbury"
reader_data[85] <- "The Wood Wife by\n Terri Windling"
reader_data[88] <- "Guilty Pleasures by\n Laurell K. Hamilton"
reader_data[89] <- "The Hunt for Read October by\n Tom Clancy"
reader_data[90] <- "The Puppet Master by\n Peter O'Donnel"
reader_data[98] <- "Mythago Wood by\n Robert Holdstock"
reader_data[100] <- "The Cunning Man by\n Robertson Davies"
reader_data
reader_data<-gsub("^[a-zA-Z]+$", "",reader_data[1:100])

temp2<-strsplit(reader_data, "by\n")
temp2<-gsub("^[a-zA-Z]+$", "",temp2[1:100][2])
temp3<-mapply(c, temp, temp2, SIMPLIFY=FALSE)
mat2<- matrix(unlist(temp3), ncol=4, byrow=TRUE) #coerce into matrix
write.csv(mat2, "both_lists.csv")

##gather scores!! for reader data
goodreads_page <- read_html("https://www.goodreads.com/list/show/9366.Modern_Library_100_Best_Novels_The_Readers_List")
readers <- html_nodes(goodreads_page,"td")
readers <- html_text(readers, trim=FALSE)
test2<-rm_white(readers)
my_seq<-seq(3,399, by=4)
reader_scores<-test2[my_seq]
reader_data<-reader_scores

reader_data[2] <- "1984 by\n George Orwell"
reader_data[4] <- "Fahrenheit 451 by\n Ray Bradbury"
reader_data[19] <- "Catch-22 by\n Joseph Heller"
reader_data[25] <- "I, Claudius by\n Robert Graves"
reader_data[30] <- "The Lord of the Rings by\n J.R.R. Tolkien"
reader_data[37] <- "The Hitchhiker's Guide to the Galaxy by\n Douglas Adams"              
reader_data[38] <- "Ender's Game by\n Orson Scott Card"
reader_data[49] <- "Watership Down by\n Richard Adams"
reader_data[65] <- "Battlefield Earth: A Saga of the Year by\n L. Ron Hubbard"
reader_data[68] <- "The Invaders Plan by\n L. Ron Hubbard"
reader_data[74] <- "Someplace to be FLying by\n Charles de Lint"
reader_data[77] <- "Memory and Dream by\n Charles de Lint"
reader_data[78] <- "Trader by\n Charles de Lint"
reader_data[83] <- "Something Wicked This Way Comes by\n Ray Bradbury"
reader_data[85] <- "The Wood Wife by\n Terri Windling"
reader_data[88] <- "Guilty Pleasures by\n Laurell K. Hamilton"
reader_data[89] <- "The Hunt for Read October by\n Tom Clancy"
reader_data[90] <- "The Puppet Master by\n Peter O'Donnel"
reader_data[98] <- "Mythago Wood by\n Robert Holdstock"
reader_data[100] <- "The Cunning Man by\n Robertson Davies"
reader_data
reader_scores<-reader_data
test_list <-strsplit(reader_scores, "by\n")

my_vec <-c("")

for(i in seq(1:100)){
  my_vec[i] <- test_list[[i]][2]
}
reader_avg<-c("")
reader_ratings<- c("")
x<-strsplit(my_vec, " rating ")
for(i in seq(1:100)){
  reader_avg[i] <- x[[i]][1]
  reader_ratings[i] <- x[[i]][2]
}
reader_avg<-gsub("[^0-9]","", reader_avg[1:100])
reader_ratings<-gsub("[^0-9]", "", reader_ratings[1:100])

goodreads_crit_page<-read_html("https://www.goodreads.com/list/show/8797.Modern_Library_100_Best_Novels_The_Board_s_List")
crit <- html_nodes(goodreads_crit_page,"td")
crit <- html_text(crit, trim=FALSE)
test3<-rm_white(crit)
my_seq<-seq(3,399, by=4)
crit_scores<-test3[my_seq]

test_list2 <-strsplit(crit_scores, "by\n")


my_vec2 <-c("")

for(i in seq(1:100)){
  my_vec2[i] <- test_list2[[i]][2]
}
crit_avg<-c("")
crit_ratings<- c("")
x<-strsplit(my_vec2, " rating ")
for(i in seq(1:100)){
  crit_avg[i] <- x[[i]][1]
  crit_ratings[i] <- x[[i]][2]
}
crit_avg<-gsub("[^0-9]","", crit_avg[1:100])
crit_ratings<-gsub("[^0-9]", "", crit_ratings[1:100])

#these aren't sorted correctly though.......

#read back in sorted csv -- phew! 

#read in sorted crit data. 
check_it <-read.csv("new_list.csv", sep=",")
colnames(check_it) <- c("Original Rank", "Avg. Ratings", "Total Ratings")
reader_temp<-mapply(c, reader_avg, reader_ratings,SIMPLIFY=FALSE)
reader_mat<- matrix(unlist(reader_temp), ncol=2, byrow=TRUE) #coerce into matrix

#crit_temp<-mapply(c, crit_avg, crit_ratings,SIMPLIFY=FALSE)
#crit_mat<- matrix(unlist(crit_temp), ncol=2, byrow=TRUE) #coerce into matrix
#write.csv(crit_mat, "crit.csv")
##impt objects are: read/crit avg & ratings; 
temp2
temp
my_df<-as.data.frame(mat2) #data frame of author book author book

reader_df <-as.data.frame(reader_mat)
crit_df <- as.data.frame(check_it)
temp_df <- cbind(reader_df, crit_df)

all_df <-cbind(my_df, temp_df)
all_df
colnames(all_df)<-c("Author (Crit)", "Title (Crit)", "Author (User)", "Title (User)",
                     "Avg. Rating", "Total Ratings", "Original Rank", "Avg. Rating", "Total Ratings")
all_df <- all_df[,c(1,2,7,8,9,3,4,5,6)]
all_df$`Title (User)` <-sub("^[a-zA-Z]+$", "", all_df$`Title (User)`)
write.csv(all_df, file = "results.csv")
