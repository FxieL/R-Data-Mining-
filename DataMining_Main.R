transaction <- read.csv("transaction_data.csv", fileEncoding = "UTF-8-BOM")
product <- read.csv("product_data.csv", fileEncoding = "UTF-8-BOM")

#a.	Show distribution of products where each product appeared in transactions more than 30 times.

data_transaction <- merge(product ,transaction, by='product_name')
data1 <- table(data_transaction$product_name)
data1 <- data1[data1>30]



pie(
  data1,
  labels = paste(rownames(data1), data1, sep = "\n"),
  data1,
  main = "Product Distribution",
  col = rainbow(4)
)

#b.	Show the frequency of product ratings.

hist(
 product$rating,
 xlab = "Rating",
 main = "Product Ratings Frequency",
 col="lightblue"
)

#c.	Show Number of Product based on Price. Separate the price into 3 categories below:

#	High: price is greater than equals to 20000.
#	Medium: price is greater than equals to 10000 and less than 20000.
#	Low: price is below 10000.


product_categoried <- product
product_categoried["category"] <- ifelse( product_categoried$price >= 20000, "High",
                                          ifelse(product_categoried$price >= 10000 & product_categoried$price < 20000, "Medium", "Low"))

data3 <- table(product_categoried$category)

ordered_product_categoried <- c("High", "Medium", "Low")
data3[ordered_product_categoried]

barplot(data3[ordered_product_categoried],
        main = "Price Range of Products",
        xlab = "Price Categories",
        ylab = "Number of Product",
        col = rainbow(3))

#2.	Frequent Pattern Analysis
#a.	Data pre-processing
#In the Data pre-processing phase, there are some data that can’t be used for the further analysis. Do the #following tasks to cleanse the data:
#•	Remove all duplicated data for the analysis.
#•	Remove all product that has a product rating less than equals to 4.1.
#•	Remove all product that has Helio G70 Processor.
#•	Remove all product that the warranty is not defined.


preprocess <- data_transaction[!duplicated(data_transaction),]
preprocess <- preprocess[preprocess$rating >= 4.1,]
preprocess <- preprocess[preprocess$product_name != "Helio G70 Processor",]
preprocess <- preprocess[preprocess$warranty != "",]

#b.	Data Transformation
#In this phase, you need to change the data, so it is suitable to be used in the apriori analysis. Prepare the data in terms of the product name.

data_apriori <- split(preprocess$product_name, preprocess$transaction_id)

#c.	Data Mining
#Show frequent product using apriori algorithm with minimum support: 0.2 based on the data that have already pre-processed.

#Frequent Items
library(arules)

freq_item <- apriori(data_apriori, parameter = 
                       list(
                          support = 0.2,
                          target = "frequent itemsets"
                       ))
inspect(freq_item)

#Show the association rules using minimum confidence: 0.6 based on the frequent product that resulted from step above.

#association rules

assoc_rule <- ruleInduction(freq_item, confidence=0.6)

inspect(assoc_rule)