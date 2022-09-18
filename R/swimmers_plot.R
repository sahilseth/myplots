

if(FALSE){
	
	set.seed(33)
	dat = data.frame(Subject = 1:10, 
									 Months = sample(4:20, 10, replace=TRUE),
									 Treated=sample(0:1, 10, replace=TRUE),
									 Stage = sample(1:4, 10, replace=TRUE),
									 Continued=sample(0:1, 10, replace=TRUE))
	
	dat = dat %>%
		group_by(Subject) %>%
		mutate(Complete=sample(c(4:(max(Months)-1),NA), 1, 
													 prob=c(rep(1, length(4:(max(Months)-1))),5), replace=TRUE),
					 Partial=sample(c(4:(max(Months)-1),NA), 1, 
					 							 prob=c(rep(1, length(4:(max(Months)-1))),5), replace=TRUE),
					 Durable=sample(c(-0.5,NA), 1, replace=TRUE))
	
	# Order Subjects by Months
	dat$Subject = factor(dat$Subject, levels=dat$Subject[order(dat$Months)])
	
	# Melt part of data frame for adding points to bars
	dat.m = melt(dat %>% select(Subject, Months, Complete, Partial, Durable),
							 id.var=c("Subject","Months"))
	
}

swimmers_plot_v1 <- function(){
	library(ggplot2)
	library(reshape2)
	library(dplyr)
	library(grid)
	
	
	# https://stackoverflow.com/questions/33131184/swimmer-survival-plot
	ggplot(dat, aes(Subject, Months)) +
		geom_bar(stat="identity", aes(fill=factor(Stage)), width=0.7) +
		geom_point(data=dat.m, 
							 aes(Subject, value, colour=variable, shape=variable), size=4) +
		# add arrows
		geom_segment(data=dat %>% filter(Continued==1), 
								 aes(x=Subject, xend=Subject, y=Months + 0.1, yend=Months + 1), 
								 pch=15, size=0.8, arrow=arrow(type="closed", length=unit(0.1,"in"))) +
		coord_flip() +
		scale_fill_manual(values=hcl(seq(15,375,length.out=5)[1:4],100,70)) +
		scale_colour_manual(values=c(hcl(seq(15,375,length.out=3)[1:2],100,40),"black")) +
		scale_y_continuous(limits=c(-1,20), breaks=0:20) +
		labs(fill="Disease Stage", colour="", shape="", 
				 x="Subject Recevied Study Drug") +
		theme_bw() +
		theme(panel.grid.minor=element_blank(),
					panel.grid.major=element_blank(),
					axis.text.y=element_blank(),
					axis.ticks.y=element_blank())
	
}