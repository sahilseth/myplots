


plot_survival_plots <- function(){
	
	p_load(survminer, survival)
	
	#source('~/projects/packs_myplots/R/ggsurv.R')
	formulas <- list(
		dss = Surv(dfs, rec) ~ gender,
		os = Surv(os, vital_status) ~ gender)
	
	col_set1 = RColorBrewer::brewer.pal(9, "Set1")
	fit2 <- surv_fit(formulas, data = clin2)
	surv_pvalue(fit2)
	#' http://www.sthda.com/english/rpkgs/survminer/reference/surv_median.html
	surv_median(fit2, combine = FALSE)
	p_surv = ggsurvplot(fit2, pval = T, 
											risk.table = TRUE, 
											palette = "Set1",
											risk.table.y.text.col = TRUE, 
											xlim = c(0, 2000), 
											break.x.by = 500)
	p_surv$table <- p_surv$table + 
		theme(axis.line = element_blank(), title = element_text(size = 1))
	p_surv = arrange_ggsurvplots(p_surv, ncol = 2)
	p_surv
	
	pdf(glue("p_surv_gender.pdf"), width = 12, height = 7)
	print(p_surv)
	dev.off()
	
	## forest plot --------
	# Fit a Cox proportional hazards model
	# tmp = mutate(df_surv_tcga, 
	#              cls = factor(cls, levels = c("gem_sens", "other", "gem_res")))
	# lst_fit.coxph <- lapply(1:3, function(i){
	#   coxph(formula = formulas[[i]], data = tmp)
	# })
	# 
	# lst_p = lapply(lst_fit.coxph, ggforest, data = df_surv_tcga)
	# pdf(glue("{odir}/p_ggfor_{top_n}_{ntp_q}.pdf"), width = 5, height = 5)
	# print(lst_p)
	# dev.off()
	# 
	# pval_dss = pairwise_survdiff(formulas[[1]], data = df_surv_tcga) %>% 
	#   broom::tidy() %>% mutate(type = "dss")
	# pval_pfi = pairwise_survdiff(formulas[[2]], data = df_surv_tcga) %>%
	#   broom::tidy() %>% mutate(type = "pfi")
	# pval_os = pairwise_survdiff(formulas[[3]], data = df_surv_tcga) %>%
	#   broom::tidy() %>% mutate(type = "os")
	
	
}