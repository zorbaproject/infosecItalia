if(1==0){
# Creo un istogramma raggruppato
histogram = ggplot(data.frame(tmpdata), aes(x=tmpdata)) + geom_histogram(position = "identity", fill=tmpdata, stat="count", alpha = 1);
mylabels = tmpdata;
histogram = histogram + stat_count(aes(y=..count..,label=mylabels),geom="text", angle=-90, hjust=-1, vjust=-1)
histogram = histogram + labs(x = NULL, y = NULL, fill = NULL, title = sub('-', ' ', tmpname));

histogram = histogram + facet_wrap(tmpgroup, scales = "free");

histogram = histogram + theme_classic() + theme(axis.line = element_blank(),
 axis.text = element_blank(),
 axis.ticks = element_blank(),
 plot.title = element_text(hjust = 0.5, color = "#666666"));

#Esporto il grafico in un file SVG
print(histogram);
grid.export(paste("./", basename, tmpname, ".svg", sep=""),addClasses=TRUE);
}
