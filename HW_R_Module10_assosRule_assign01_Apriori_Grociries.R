#R for Data Science - class August-Sept 2022 with Prateek
#Homework
#Module 10, Assignment 01 - Apriory for Groceries

#Task1: build Apriori algorithm:
#a) load "Groceries" dataset
#b) build Apriori algorithm where support value is 0.05 and the confidence value is 0.3. Store the result in 'rule1'
#c) Inslect the rule
#d) Plot the rule and set the method to be 'grouped'

#Task2: build another Apriori algorithm:
#a) build Apriori algorithm where support value is 0.07.  Store the result in 'rule2'. Inslect the rule.
#b) Plot the rule and set the method to be 'grouped'

install.packages("arules")
install.packages("arulesViz")
install.packages("RColorBrewer")
library(arules)
library(arulesViz)
library(RColorBrewer)


data(Groceries)
head(Groceries)
#inspect(Groceries[1:5])
inspect(Groceries[1:10])

frequent <- eclat(Groceries, parameter=list(supp = 0.05, maxlen=15))
frequent <- sort(frequent, by = 'support')
inspect(frequent)

itemFrequencyPlot(Groceries, topN=10)

#a.rules <- apriori(Groceries, parameter = list(supp = 0.05,conf = 0.3, maxlen = 15))
a.rules <- apriori(Groceries, parameter = list(supp = 0.05,conf = 0.3))

inspect(a.rules)

#a.rules.viz <- apriori(Groceries, parameter = list(supp = 0.05, conf = 0.3), appearance = list(default='rhs', lhs='yogurt'))

a.rules.viz <- apriori(Groceries, parameter = list(supp = 0.05, conf = 0.3))

plot(a.rules.viz, method = 'graph', measure = 'confidence', shading = 'lift')


args(getS3method("plot","rules"))
plot(rules)
plot(rules, measure = c("support", "lift"), shading = "confidence")

a.rules.viz <- apriori(Groceries, parameter = list(supp = 0.05, conf = 0.3), appearance = list(default='rhs', lhs='yogurt'))

plot(rules, method = "two-key plot")
plot(rules, method = "grouped")
plot(rules, method = "grouped", control = list(k = 50))
#subrules2 <- head(rules, n = 10, by = "lift")
#plot(subrules2)
#plot(subrules2, method = "paracoord")
#plot(subrules2, method = "paracoord", control = list(reorder = TRUE))
#---------------

#Task2: build another Apriori algorithm:
#a) build Apriori algorithm where support value is 0.07.  Store the result in 'rule2'. Inslect the rule2.
#b) Plot the rule2 and set the method to be 'grouped'

#3) sup=0.07, conf=-0.2
data(Groceries)
head(Groceries)
#inspect(Groceries[1:5])
inspect(Groceries[1:10])

frequent <- eclat(Groceries, parameter=list(supp = 0.007, maxlen=15))
frequent <- sort(frequent, by = 'support')
inspect(frequent)

itemFrequencyPlot(Groceries, topN=10)

#a.rules <- apriori(Groceries, parameter = list(supp = 0.007,conf = 0.2, maxlen = 5))

a.rules <- apriori(Groceries, parameter = list(supp = 0.007,conf = 0.2))

inspect(a.rules)
a.rules.viz <- apriori(Groceries, parameter = list(supp = 0.007, conf = 0.2), appearance = list(default='rhs', lhs='yogurt'))
plot(a.rules.viz, method = 'graph', measure = 'confidence', shading = 'lift')






#----------------------
#3) sup=0.001, conf=-0.2
data(Groceries)
head(Groceries)
#inspect(Groceries[1:5])
inspect(Groceries[1:10])

frequent <- eclat(Groceries, parameter=list(supp = 0.001, maxlen=15))
frequent <- sort(frequent, by = 'support')
inspect(frequent)

itemFrequencyPlot(Groceries, topN=10)

#a.rules <- apriori(Groceries, parameter = list(supp = 0.001,conf = 0.2, maxlen = 5))

a.rules <- apriori(Groceries, parameter = list(supp = 0.001,conf = 0.2))
                   
inspect(a.rules)
#a.rules.viz <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.2), appearance = list(default='rhs', lhs='yogurt'))
a.rules.viz <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.2))

plot(a.rules.viz, method = 'graph', measure = 'confidence', shading = 'lift')



a.rules.viz <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.2), appearance = list(default='rhs', lhs='yogurt'))
#a.rules.viz <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.2))

plot(rules, method = "scatterplot", engine = "ggplot2", control = "help")

plot(rules, method="grouped matrix", k = 5)

plot(rules, lhs='yogurt')

plot(rules, measure = c("support", "lift"), shading = "confidence", limit = 100)

plot(rules, measure = c("support", "lift"), shading = "confidence")

plot(rules, method = "two-key plot", limit = 100)

plot(rules, method = "grouped")

plot(rules, method = "grouped", control = list(k = 50))

plot(rules, method = "grouped matrix", engine = "htmlwidget")

subrules2 <- head(rules, n = 10, by = "lift")

plot(subrules2)

plot(subrules2, method = "paracoord")

plot(subrules2, method = "paracoord", control = list(reorder = TRUE))




