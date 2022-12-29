library(tidyverse)
library(latex2exp)

library(gridExtra)
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#case = 'poisson1'
case = 'bernoulli1'

sig = read_csv(paste(case,'_sig.csv',sep=''))

sigplot = sig %>% mutate(Method=factor(method, levels=c("Ours","NormCI","AsympCS1","AsympCS2","HoeffCS"))) %>% mutate(Method=recode_factor(Method,Ours="uCS")) %>% ggplot() + aes(t0, sig, color=Method) + geom_line(size = 1) + geom_hline(yintercept=0.1,linetype=2) + xlab(TeX("Monitoring start time, $t_0$")) + ylab(TeX("Probability of rejecting within experiment, $P_0(\\tau\\leq t_{\\max})$"))  + scale_x_log10()  + scale_color_manual(values = c("#F8766D", "#00BFC4", "#B79F00", "#7CAE00", "#C77CFF"))

rej =read_csv(paste(case,'_105_rej.csv',sep=''))

rejplot = rej %>% group_by(t0,method) %>% summarise(mean=mean(stoppingtime), sd=sd(stoppingtime), p25=quantile(stoppingtime,.25), p75=quantile(stoppingtime,.75)) %>%
  mutate(Method=factor(method, levels=c("Ours","NormCI","AsympCS1","AsympCS2","HoeffCS"))) %>% mutate(Method=recode_factor(Method,Ours="uCS")) %>% ggplot() + aes(t0, mean, ymin=p25, ymax=p75, color=Method, fill=Method) + geom_line(size = 1) + geom_line(aes(x=t0,y=t0),color='black',linetype=2) + geom_line(aes(x=t0,y=-2*log(0.1)/.05^2),color='black',linetype=3) + geom_ribbon(alpha=.3, colour = NA)  + xlab(TeX("Monitoring start time, $t_0$")) + ylab(TeX("Time to reject, $\\tau$, mean and $25^{th}$ to $75^{th}$ percentile")) + scale_x_log10() + scale_fill_manual(values =  c("#F8766D", "#00BFC4", "#B79F00", "#7CAE00", "#C77CFF")) + scale_color_manual(values =  c("#F8766D", "#00BFC4", "#B79F00", "#7CAE00", "#C77CFF"))

ggsave(paste(case,'_sig.pdf',sep=''), plot = sigplot + theme(legend.position="none", plot.margin=grid::unit(c(0,0,0,0), "mm")), width=5, height=4)
ggsave(paste(case,'_rej.pdf',sep=''), plot = rejplot + theme(legend.position="none", plot.margin=grid::unit(c(0,0,0,0), "mm")), width=5, height=4)
ggsave(paste(case,'_leg.pdf',sep=''), plot=g_legend(rejplot + theme(legend.margin=margin(c(0,0,0,0)))), width = 1, height = 2)
