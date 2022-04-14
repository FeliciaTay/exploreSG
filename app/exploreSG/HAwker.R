hawker.centres<-read.csv("/Users/jasperchong/Desktop/DBA group project/Datasets/list-of-government-markets-hawker-centres/hawker_data_final.csv")
hawker.centres[c(3,4,5,7)] <-NULL

hawker<-na.omit(hawker.centres)
 