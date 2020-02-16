meta <- read.csv("/Users/abcdefg/Downloads/Datathon/Metadata.csv")
part1 <- read.csv("/Users/abcdefg/Downloads/Datathon/Part\ 1.csv")
part2 <- read.csv("/Users/abcdefg/Downloads/Datathon/Part\ 2.csv")
part3 <- read.csv("/Users/abcdefg/Downloads/Datathon/Part\ 3.csv")
part4a <- read.csv("/Users/abcdefg/Downloads/Datathon/Part\ 4a.csv")
part4b <- read.csv("/Users/abcdefg/Downloads/Datathon/Part\ 4b.csv")
part5 <- read.csv("/Users/abcdefg/Downloads/Datathon/Part\ 5.csv")
View(part1)
part1 <- part1[-1,]
part2 <- part2[-1,]
part3 <- part3[-1,]
part4a <- part4a[-1,]
part4a <- part4a[-1,]
part5 <- part5[-1,]
datathon <- rbind(part1, part2, part3, part4a, part4b, part5)

#datathon_all, datathon_detail, data_all(only with range)
View(datathon)
geo <- datathon[,2:3]
geo_unique <- unique(geo)
View(geo)
View(unique(geo))
position = which(datathon$NAICS.id == "44-45")
datathon_all <- datathon[c(position),]
View(datathon_all)
datathon_detail <- datathon[-c(position),]
View(datathon_detail)

position_1 <- which(datathon_all$RCPSZFE.display.label =="All establishments"|datathon_all$RCPSZFE.display.label == "Establishments operated for the entire year"|datathon_all$RCPSZFE.display.label == "Establishments not operated for the entire year")
data_all <- datathon_all[-c(position_1),]

data_all <- data_all[c(2,7,9)]

View(data_all)

#data_low

position_low <- which(data_all$RCPSZFE.display.label == "Establishments operated entire year with sales/receipts/revenue less than $100,000")
data_low <- data_all[c(position_low),]
data_low <- data_low[c(1,3)]
View(data_low)
write.csv(data_low, "/Users/abcdefg/Downloads/Datathon/data_low.csv")


#data_mid
position_mi <- which(data_all$RCPSZFE.display.label == "Establishments operated entire year with sales/receipts/revenue of $100,000 to $249,999"|data_all$RCPSZFE.display.label =="stablishments operated entire year with sales/receipts/revenue of $250,000 to $499,999"|data_all$RCPSZFE.display.label =="Establishments operated entire year with sales/receipts/revenue of $500,000 to $999,999")
data_mi <- data_all[c(position_mi),]
#data_mi <- na.omit(data_mid)

data_mid <- data.frame(aggregate(as.numeric(data_mi$ESTAB), by = list(data_mi$GEO.id2), FUN = sum))
View(data_mid)
write.csv(data_mid, "/Users/abcdefg/Downloads/Datathon/data_mid.csv")

#data_high
position_high <- which(data_all$RCPSZFE.display.label == "Establishments operated entire year with sales/receipts/revenue of $1,000,000 or more")
data_high <- data_all[c(position_high),]
data_high <- data_high[c(1,3)]
View(data_high)
write.csv(data_high, "/Users/abcdefg/Downloads/Datathon/data_high.csv")


#data_not_rate, data_all_not, data_all_es
View(datathon_all)
position_2 <- which(datathon_all$RCPSZFE.display.label == "All establishments")
data_all_es <- datathon_all[c(position_2),]
View(data_all_es)
position_3 <- which(datathon_all$RCPSZFE.display.label == "Establishments not operated for the entire year")
data_all_not <- datathon_all[c(position_3),]
View(data_all_not)
data_all_not <- data_all_not[c(2,9)]
data_all_es <- data_all_es[c(2,9)]
View(data_all_es)
data_not <- data.frame()
names <- c("GEO.id2","ESTABrate")
for (k in names) data_not[[k]]<-as.numeric()
View(data_not)


count <- 1
for (i in data_all_es$GEO.id2){
  data_not[count,1] <- i
  count <- 1 + count
}


count <- 1
for (i in data_all_es$GEO.id2){
  p <- which(data_all_not$GEO.id2 == i)
  if (length(p) != 1){
    data_not$ESTABrate[count] = 0
    #print(i)
  }
  if (length(p) == 1){
    #print(as.numeric(as.character(data_all_not$ESTAB[p])))
    #print(data_all_es$ESTAB[which(data_all_es$GEO.id2 == i)])
    data_not$ESTABrate[count] = (as.numeric(as.character(data_all_not$ESTAB[p])))/(as.numeric(as.character(data_all_es$ESTAB[which(data_all_es$GEO.id2 == i)])))
  }
  count <- count +1
}

View(data_not)

write.csv(data_not, "/Users/abcdefg/Downloads/Datathon/data_not.csv")



#data_channel
#View(data_all)
#View(datathon_detail)
position_3 <- which(datathon_detail$RCPSZFE.display.label =="All establishments"|datathon_detail$RCPSZFE.display.label == "Establishments operated for the entire year"|datathon_detail$RCPSZFE.display.label == "Establishments not operated for the entire year")
data_detail_all <- datathon_detail[-c(position_3),]
#View(data_detail_all)
data_detail_all <- data_detail_all[c(2,3,5,7,9)]
#View(data_detail_all)
data_detail_all$RCPSZFE.display.label <- as.character(data_detail_all$RCPSZFE.display.label)
data_detail_all$NAICS.display.label <- as.character(data_detail_all$NAICS.display.label)
data_detail_all$ESTAB <- as.numeric(as.character(data_detail_all$ESTAB))
data_detail_all$GEO.display.label <- as.character(data_detail_all$GEO.display.label)
data_detail_all <- na.omit(data_detail_all)
#View(data_detail_all)
position_4 <- which(data_detail_all$RCPSZFE.display.label == "Establishments operated entire year with sales/receipts/revenue less than $100,000")
data_detail_low <- data_detail_all[c(position_4),]
#View(data_detail_low)
position_5 <- which(data_detail_all$RCPSZFE.display.label == "Establishments operated entire year with sales/receipts/revenue of $100,000 to $249,999"|data_detail_all$RCPSZFE.display.label =="stablishments operated entire year with sales/receipts/revenue of $250,000 to $499,999"|data_detail_all$RCPSZFE.display.label =="Establishments operated entire year with sales/receipts/revenue of $500,000 to $999,999")
data_detail_mid <- data_detail_all[c(position_5),]
#View(data_detail_mid)
position_6 <- which(data_detail_all$RCPSZFE.display.label == "Establishments operated entire year with sales/receipts/revenue of $1,000,000 or more")
data_detail_high <- data_detail_all[c(position_6),]
#View(data_detail_high)

data_detail_low$ESTAB <- data_detail_low$ESTAB*100000
#View(data_detail_low)
data_detail_mid$ESTAB <- data_detail_mid$ESTAB*500000
#View(data_detail_mid)
data_detail_high$ESTAB <- data_detail_high$ESTAB*1000000
#View(data_detail_high)

data_detail_new <- rbind(data_detail_low, data_detail_mid,data_detail_high)
View(data_detail_new)

library(dplyr)
data_detail_new <- mutate(data_detail_new, state <- substr(data_detail_new$GEO.display.label, nchar(data_detail_new$GEO.display.label) - 3 + 1, nchar(data_detail_new$GEO.display.label) - 3 + 2))
View(data_detail_new)

colnames(data_detail_new)[6] <- "state"
data_detail_new <- data_detail_new[c(1,3,4,5,6)]


data_graph <- aggregate(data_detail_new$ESTAB, by = list(data_detail_new$NAICS.display.label, data_detail_new$state), FUN = sum)
colnames(data_graph)[1] <- "channels" 
colnames(data_graph)[2] <- "state"
colnames(data_graph)[3] <- "expected_revenue"
View(data_graph)

state_graph <- unique(data_graph$state)
state_graph
rank = data.frame()
for (i in state_graph){
   a <- filter(data_graph, state == i)
   a <- arrange(a, desc(expected_revenue))
  rank <- rbind(rank, a[1:3,])

}
unique(rank$channels)

View(rank)
rank <- rank[-c(3)]

write.csv(rank, "/Users/abcdefg/Downloads/Datathon/rank.csv")


# tree_graph
library(ggraph)
library(igraph)
library(tidyverse)
library(dendextend)
library(colormap)
library(kableExtra)
options(knitr.table.format = "html")

# create a data frame 
data=data.frame(
  level1="US",
  level2=c( rep("1",3), rep("2",3),rep("3",3), rep("4",3),rep("5",3), rep("6",3),
            rep("7",3), rep("8",3),rep("9",3), rep("10",3),rep("11",3), rep("12",3),
            rep("13",3), rep("14",3),rep("15",3), rep("16",3),rep("17",3), rep("18",3),
            rep("19",3), rep("20",3),rep("21",3), rep("22",3),rep("23",3), rep("24",3),
            rep("25",3), rep("26",3),rep("27",3), rep("28",3),rep("29",3), rep("30",3),
            rep("31",3), rep("32",3),rep("33",3), rep("34",3),rep("35",3), rep("36",3),
            rep("37",3), rep("38",3),rep("39",3), rep("40",3),rep("41",3), rep("42",3),
            rep("43",3), rep("44",3),rep("45",3), rep("46",3),rep("47",3)),
  level3=paste0("mister_", c(1:141))
)



# transform it to a edge list!
edges_level1_2 = data %>% select(level1, level2) %>% unique %>% rename(from=level1, to=level2)
edges_level2_3 = data %>% select(level2, level3) %>% unique %>% rename(from=level2, to=level3)
edge_list=rbind(edges_level1_2, edges_level2_3)

# Now we can plot that
mygraph <- graph_from_data_frame( edge_list )
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() +
  geom_node_point(color="#69b3a2", size=3) +
  
  geom_node_text(
    aes(label=c("US",unique(rank$state)[1:47],rank$channels[141:1]) ), 
    hjust=c(1,rep(0.5,47), rep(1.3,141)), 
    nudge_y = c(-.02, rep(0,47), rep(0.02,141)),
    nudge_x = c(0, rep(0.3, 47), rep(0,141))
  ) +
  
  theme_void() +
  coord_flip() +
  scale_y_reverse() 





