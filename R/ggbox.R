
ggbox <- function(df, x, y, color = NULL, shape = NULL, comps = NULL){
	p_load(ggplot2, cowplot, ggsci, ggpubr)

	p = ggplot(df, aes_string(x, y))
	if(!is.null(color))
		p = ggplot(df, aes_string(x, y, color = color))

	# calc quantiles (to determine plotting locations)
	quant = Hmisc::smedian.hilow(as.numeric(unlist(df[,y])))

	# , color = sensitive_us
	p = p +	geom_point(position=position_jitter(w=0.1, h=0), alpha = 0.5) + 
		stat_summary(fun = "median", colour = "red", size = 2, geom = "point") +
		# stat_summary(fun.data = mean_cl_boot, conf.int = .95, B = 5000, geom = "errorbar", width = 0.2) + 
		stat_summary(fun.data = median_hilow, conf.int = .95,
								 geom = "errorbar", width = 0.2, colour = "red", alpha = 0.5) +
		# scale_y_log10(breaks=c(25,100,400)) + 
		xlab("") + scale_color_npg() +
		# facet_wrap(~sensitive_us) + 
		theme_cowplot() +
		theme(panel.grid.major = element_blank(),
					panel.grid.minor = element_blank(),
					strip.background = element_blank(),
					panel.border = element_blank())
		coord_cartesian(ylim = c(quant[2], quant[3]*(1.2)))

	if(!is.null(comps))
		p = p + stat_compare_means(comparisons = comps, label.y = quant[3]*(1.1))
	
	p
}


color = "red"