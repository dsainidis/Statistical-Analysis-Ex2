values <- read.csv("AutoOnline_values.csv", sep = ";")
labels <- read.csv("AutoOnline_labels.csv", sep = ";")

colnames(values)[1] <- 'qnrnumb'
colnames(labels)[1] <- 'qnrnumb'
