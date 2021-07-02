# import relevant librarys
library(readxl)
library(plyr)
library(tidyr)
library(dplyr)
library(arules)
library(arulesViz)
library(RColorBrewer)

# load the dataset
Online_Retail <- read_excel("Online_Retail.xlsx")

# removed all the rows that included missing values. 
Online_Retail <- Online_Retail[complete.cases(Online_Retail), ] 

Online_Retail <- as.data.frame(Online_Retail)

# split the the column to 2 pieces 
Online_Retail <- separate(Online_Retail, InvoiceDate, c("InvoiceDate", "InvoiceTime"), sep = " ")

# rename the variable in position 1 to avoid spaces in variable names
colnames(Online_Retail)[1] <- "InvoiceNo"

# remove the non-sense negative values for quantitiy
Online_Retail <- Online_Retail[Online_Retail$Quantity >= 0, ]

# remove doublicated rows
Online_Retail <- unique(Online_Retail)

# observe the type of variables and change them
str(Online_Retail)
Online_Retail$Country <- as.factor(Online_Retail$Country)
Online_Retail$InvoiceDate <- as.Date(Online_Retail$InvoiceDate)
#Online_Retail$CustomerID <- as.numeric(Online_Retail$CustomerID)

# sort transactions by InvoiceNo 
Online_Retail.sorted <- Online_Retail[order(Online_Retail$InvoiceNo),]

# transform data from dataframe to transcations
# so each row will be one transaction
Online_Retail.itemList <- ddply(Online_Retail.sorted,c("InvoiceNo","InvoiceDate"),
                                function(df1)paste(df1$Description,
                                                   collapse = ","))

# create a column listing the day of the transaction
Online_Retail.itemList$Day <- weekdays(as.Date(Online_Retail.itemList$InvoiceDate))
Online_Retail.itemList$Day <- as.character(Online_Retail.itemList$Day)

# create a plot showing how many transactions happened on each day
# on saturdays there were 0 transcations
counts <- table(Online_Retail.itemList$Day)
barplot(counts, main="Days",
        xlab="Days")

Online_Retail.List <- Online_Retail.itemList[,3]
write.csv(Online_Retail.List, "ItemList.csv", quote = FALSE, row.names = FALSE)

#read the transaction data for association rules analysis
ItemList = read.transactions(file = "ItemList.csv", rm.duplicates = TRUE, 
                             header = TRUE ,format="basket",sep=",",cols=NULL);

# summary
summary(ItemList)

# frequency plot
itemFrequencyPlot(ItemList, topN = 10,col=brewer.pal(8,'Pastel1'), cex.names=0.70
                  ,type="absolute",main="Relative Item Frequency Plot")



#visualise the sparse matrix 
image(ItemList[1:100], aspect = "fill",pch = 1, main = "Sparse matrix sample")

# generate assosication riles
basket.rules = apriori(ItemList, parameter = list(supp=0.001, conf=0.8,maxlen=10))

summary(basket.rules)

inspect(sort(basket.rules, by = "lift")[1:10])

# paracoord visualisation of the top 10 rules by lift
subRules<-head(basket.rules, n=10, by="lift")
plot(subRules, method="paracoord")

# visualisation of the rules

plot(basket.rules)
plot(basket.rules,method="two-key plot")

