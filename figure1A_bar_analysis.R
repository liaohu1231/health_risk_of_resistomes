##input data
data=read.table("013.normalize_16s.type.tab.txt",row.names = 1,header = 1,sep='\t',check.names = F)
data_1=read.table("antibiotic_1.normalize_16s.type.tab.txt",row.names = 1,header = 1,sep='\t',check.names = F)
data_1$G13=data[as.character(rownames(data)),1]

##order data
sum=as.data.frame(apply(data_1,2,sum))
data_1=data_1[,order(sum,decreasing = T)]
row_sum=apply(data_1, 1, sum)
data_1=data_1[order(row_sum,decreasing = T),]
summary(t(data_1))

other=as.data.frame(apply(data_1[10:nrow(data_1),],2,sum))
colnames(other)='other'
data_1=rbind(data_1[1:9,],t(other))
library(reshape2)
data_1$arg=rownames(data_1)

##input metadata
metadata=read.table("groups.txt",header = 1,sep='\t',check.names = F)
rownames(metadata)=metadata$sample

##reshpe data
data=melt(data_1)

##annotate metadata
data$group=metadata[as.character(data$variable),1]

library(ggplot2)
library(ggsci)
library(ggpubr)
#ÏäÐÍÍ¼
sum$group=metadata[as.character(rownames(sum)),1]
sum$group=factor(sum$group,levels = c("forest","park","treelawn","vegetable","paddy"))
colnames(sum)[1]="value"
median(sum[grep(pattern = "paddy",x = sum$group),1])

p1=ggplot(sum,aes(x = group,y = value))+
  geom_boxplot(alpha=0.7, width=0.6,outlier.size = 1)+
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=60, hjust=1.0,colour = "black",size = 8))+
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(panel.background = element_rect(fill = NA))+
  labs(y="Abundance (copy per 16S rRNA gene)",x="Types of land use")+ylim(c(0,1.6))+
  #scale_fill_d3(palette = "category20")+
  theme(panel.grid = element_line(color = 'gray', linetype = 2, size = 0.05), 
        panel.background = element_rect(color = 'black', fill = 'transparent'), 
        legend.key = element_rect(fill = 'transparent'))+
  stat_compare_means(paired = F,method = "wilcox.test",
                     label = "p.signif",comparisons = list(c("vegetable","paddy"),
                                                           c("treelawn","paddy"),
                                                           c("park","paddy"),
                                                           c("forest","paddy")))
p1



data$group=factor(data$group,levels = c("forest","park","treelawn","vegetable","paddy"))
arg=as.factor(rownames(data_1))
data$arg=factor(data$arg,levels = arg)
ggplot(data,aes(x = variable,y = value,fill=arg))+geom_bar(stat="identity", width=0.8)+
  facet_wrap(~group, scales = 'free_x', ncol = 5) +
  theme(text = element_text(size=8),axis.text.x = element_text(angle=60, hjust=1.0,colour = "black",size = 8))+
  theme(legend.key.size=unit(5,'mm'),
        panel.background = element_blank())+
  theme(legend.spacing.x = unit(0.1, 'cm'),legend.key.size = unit(5, 'mm'))+
  labs(y="Abundance (copy per 16S rRNA gene)",x="Samples")+
  scale_fill_d3(palette = "category20c")+
  theme(panel.grid = element_line(color = 'gray', linetype = 2, size = 0.1), 
        panel.background = element_rect(color = 'black', fill = 'transparent'), 
        legend.key = element_rect(fill = 'transparent'))

