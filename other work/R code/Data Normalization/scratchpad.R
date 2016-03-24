
dat<-read.csv("../data/dataset_scale.csv")

sc<-scale(dat$A, center=T, scale=T)
class(sc)



a<-0.75*0.3
b<-0.25*0.7
p<-a/(a+b)

/((0.75*0.3)+(0.25*0.7))


