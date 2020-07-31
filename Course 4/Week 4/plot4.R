library(dplyr)
library(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
sccCoal<-SCC[grepl("coal",SCC$Short.Name,ignore.case=TRUE),]
NEIcoal<-NEI[NEI$SCC %in% sccCoal$SCC,]
h<-NEIcoal%>%group_by(year,type) %>% summarise(Total_Emissions=sum(Emissions))
png(file="plot4.png")
g<-ggplot(h,aes(year,Total_Emissions,col=type))+geom_line()+geom_point()+ggtitle("Emissions from coal combustion related sources")
print(g)
dev.off()
