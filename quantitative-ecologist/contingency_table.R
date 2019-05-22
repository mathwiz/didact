AgriType<- factor(c(1,3,3,3,1,2,2,3))
length(AgriType)
RecoType<- factor(c("a", "c", "c", "b", "a", "b", "a", "c"))
length(RecoType)

data<- data.frame(cbind(AgriType, RecoType))
data

table(data)

