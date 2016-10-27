#http://www.rdatamining.com/examples/association-rules

setwd("~/Desktop/assoc")
load("~/Desktop/assoc/titanic.raw.rdata")
str(titanic.raw)

library(arules)
rules <- apriori(titanic.raw)
inspect(rules)

#maximum items of 10, support at 0.1, confidence of 0.8
#find rules on survival
# RHS is the consequent, 1 item in the consequent
# LHS is an empty antecedent
# A lift ratio larger than 1.0 implies that the relationship between the antecedent and the consequent is more significant than would be expected if the two sets were independent. 
# The larger the lift ratio, the more significant the association.
# The first number is called the support for the rule. The support is simply the number of transactions that include all items in the antecedent and consequent parts of the rule. The support is sometimes 
# expressed as a percentage of the total number of records in the database.) 
# Confidence is the ratio of the number of transactions that include all items in the consequent, as well as the antecedent (the support) 
# to the number of transactions that include all items in the antecedent.

rules <- apriori(titanic.raw,
                 parameter = list(minlen=2, supp=0.1, conf=0.8),
                 appearance = list(rhs=c("Survived=No", "Survived=Yes"),
                 default="lhs"),
                 control = list(verbose=F))

rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)


subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

rules.pruned <- rules.sorted[!redundant]

inspect(rules.pruned)

library(arulesViz)
plot(rules)

plot(rules, method="graph", control=list(type="items"))

plot(rules, method="paracoord", control=list(reorder=TRUE))

