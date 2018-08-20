# Association Rules -------------------------------------------------------
# arules and arulesViz packages install
install.packages("arules")
install.packages("arulesSequences")
install.packages("arulesViz")
install.packages("wordcloud")

library(arules)
library(arulesSequences)
library(arulesViz)
library(wordcloud)

# Part 1: Transform a data file into transaction format
# Basket type
tmp_basket <- read.transactions("Transaction_Sample_Basket.csv", 
                                format = "basket", sep = ",", rm.duplicates=TRUE)
inspect(tmp_basket)

# Single type
tmp_single <- read.transactions("Transaction_Sample_Single.csv", 
                                format = "single", cols = c(1,2), rm.duplicates=TRUE)
inspect(tmp_single)

# Part 2: Association Rule Mining without sequence information
data("Groceries")
summary(Groceries)
str(Groceries)
itemInfo(Groceries)

groceries_df <- as(Groceries, "data.frame")

# Item inspection
itemName <- itemLabels(Groceries)
itemCount <- itemFrequency(Groceries)*9835

col <- brewer.pal(8, "Dark2")
wordcloud(words = itemName, freq = itemCount, min.freq = 1, scale = c(3, 0.2), col = col , random.order = FALSE)

itemFrequencyPlot(Groceries, support = 0.05, cex.names=0.8)

# Rule generation by Apriori
rules <- apriori(Groceries, parameter=list(support=0.01, confidence=0.35))

# Check the generated rules
inspect(rules)

# List the first three rules with the highest lift values
inspect(sort(rules, by="lift"))

# Save the rules in a text file
write.csv(as(rules, "data.frame"), "Groceries_rules.csv", row.names = FALSE)

# Plot the rules
plot(rules, method = "scatterplot")
plotly_arules(rules, method = "scatterplot", measure = c("support", "confidence"), shading = "lift")

plot(rules, method="matrix")
plotly_arules(rules, method = "matrix", measure = c("support", "confidence"), shading = "lift")

# Rule generation by Apriori with another parameters
rules <- apriori(Groceries, parameter=list(support=0.01, confidence=0.5))

plot(rules, method="graph")
plot(rules, method="paracoord")
              
# Part 3: Association Rule Mining with sequence information
foodmart_tr <- read_baskets("foodmart_transactions2.txt",  info = c("sequenceID","eventID","SIZE"))
foodmart_df <- as(foodmart_tr, "data.frame")

# Find frequent sequences
start.time <- proc.time()
seq_rules <- cspade(foodmart_tr, parameter = list(support = 0.0005, maxsize = 10, maxlen = 5),
                    control = list(verbose = TRUE))
proc.time() - start.time

summary(seq_rules)

# Filter frequent sequences with the length greater than 2
seq_rules_df <- as(seq_rules, "data.frame")
seq_rules_size <- size(seq_rules)
seq_rules_df <- cbind(seq_rules_df, seq_rules_size)
seq_rules_df_filtered <- subset(seq_rules_df, seq_rules_df$seq_rules_size > 2)
write.csv(seq_rules_df_filtered, file = "seq_rules_filterd.csv", row.names = FALSE)

# Find association rules
seq_rules_induced <- ruleInduction(seq_rules, confidence = 0.2)
summary(seq_rules_induced)                    
inspect(seq_rules_induced)

# Save the results
seq_rules_induced_df <- as(seq_rules_induced, "data.frame")
write.csv(seq_rules_induced_df, file = "seq_rules_induced_df.csv", row.names = FALSE)
