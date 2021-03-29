analysr_env<-new.env(parent =emptyenv())

# create data frame for measures
analysr_env$measures<-data.frame(matrix(ncol = 4, nrow = 0))
colnames(analysr_env$measures)<- c("stat_unit", "date", "tag", "value")