# tariff_2025_x_y.R 
# Do Firms Benefit from Reciprocal Tariffs?

# packages used
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX in ggplot
#library(xtable)# export data frames to LaTeX tables
#library(dplyr)# sorting by group (by year)

# model initial parameters
(n1 = 1720)# 
#n1 = 180
(n2 = 120)
(n1 - n2)
(c = 1)
(delta = 4)
(Delta_v = 0 )
(T.vec = seq(0, 0.5, 0.01))# for reciprocal tariff

# Section 4: Result 3 Figure 3 simulations N1 > N2 ####
#(profit A increases with T when n1-n2 is large)
delta
(2*c*(n1-n2)/(3*n2))# Condition for Result 3b
(delta < 2*c*(n1-n2)/(3*n2))
#(n1=n2+(1440))  

#
# equilibrium profits, eq (11)
(profita_T.vec = (T.vec*c*(n1-n2) +3*delta*(n1+n2))^2 / (18*delta*(T.vec*n2 +n1 +n2)) )
#
(profitb_T.vec = (T.vec*c*(n1-n2) -3*delta*(n1+n2))^2 / (18*delta*(T.vec*n1 +n1 +n2)) )

# plotting profits as functions of T=t1=t2
#
(max(profita_T.vec))# max for the plot
(min(profitb_T.vec))# min for the plot

# make it data frame
(profit_T.df = data.frame(T.vec, profita_T.vec, profitb_T.vec))

# Plotting Figure 3 (N1 > N2)
#
ggplot(profit_T.df, aes(x=T.vec)) +geom_line(aes(y=profita_T.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=profitb_T.vec), linetype="longdash", size=1.2, color="black") + scale_x_continuous(breaks = seq(0,0.5,0.05)) + scale_y_continuous(breaks = seq(2300, 3900, 100)) +theme(axis.text.x = element_text(size = 20, color = "black"),  axis.text.y = element_text(size = 20, color = "black"), text = element_text(size = 20)) +labs(x=TeX("Reciprocal tariff rates: $t_1=t_2=T$"), y=TeX("Profits of brand-producing firms:  $\\pi^A$, $\\pi^B")) +annotate("text", x = 0.31, y = 2870, label =TeX("$\\pi^B(T,T)$"), size = 8, color="black") +annotate("text", x = 0.31, y = 3690, label =TeX("$\\pi^A(T,T)$"), size = 8, color="black") +annotate("text", x = 0.12, y = 3550, label =TeX("Free trade"), size = 8, color="black") +geom_segment(aes(x=0.12, y=3575, xend = 0.02, yend = 3660), arrow=arrow(type="closed"), size=1.0) +geom_point(aes(x=0, y=profita_T.vec[1]), size=4)

# Section 4: Figure 4 simulations of xhat1, xhat2 under N1 > N2 ####
# equilibrium prices (not in the paper)
(pa_T.vec = (T.vec*c*(n1+2*n2) +3*(c+delta)*(n1+n2)) / (3*(T.vec*n2 +n1 +n2)) )
#
(pb_T.vec = (T.vec*c*(2*n1 +n2) +3*(c+delta)*(n1+n2)) / (3*(T.vec*n1 +n1 +n2)) )

# equilibrium market shares, eq (3)
(xhat1.vec = 0.5 + (pb_T.vec*(1+T.vec) -pa_T.vec +Delta_v)/(2*delta))
#
(xhat2.vec = 0.5 + (pb_T.vec -pa_T.vec*(1+T.vec) +Delta_v)/(2*delta))
#
(T.vec)# => the above prices valid only for T not exceeding 0.5

# add to data frame
names(profit_T.df)
#
(profit_xhat_T.df = data.frame(profit_T.df, xhat1.vec, xhat2.vec))

# Figure 4 
# plotting xhat_large.f (market share x1 with unequal market size)
ggplot(profit_xhat_T.df, aes(x=T.vec)) +geom_line(aes(y=xhat1.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=xhat2.vec), linetype="longdash", size=1.2, color="black")  +scale_x_continuous(breaks = seq(0,0.5,0.05)) + scale_y_continuous(breaks = seq(0, 1, 0.05)) +theme(axis.text.x = element_text(size = 20, color = "black"),  axis.text.y = element_text(size = 20, color = "black"), text = element_text(size = 20)) +labs(x=TeX("Reciprocal tariff rates: $t_1=t_2=T$"), y=TeX("Brand A's domestic and foreign market shares:  $\\hat{x}_1$, $\\hat{x}_2$"))  +annotate("text", x = 0.12, y = 0.46, label =TeX("Free trade"), size = 8, color="black") +geom_segment(aes(x=0.12, y=0.47, xend = 0.02, yend = 0.49), arrow=arrow(type="closed"), size=1.0) +annotate("text", x = 0.25, y = 0.505, label =TeX("$\\hat{x}_1$"), size = 8, color="black") +annotate("text", x = 0.25, y = 0.27, label =TeX("$\\hat{x}_2$"), size = 8, color="black") +geom_point(aes(x=0, y=0.5), size=4)


# Subsection 5.1: Cost asymmetry ca > cb (not in paper)####
(n = 120)# 
(ca1 = 2)
(ca2 = 3)
(cb1 = 1)
(cb2 = 1)
(Delta_c1 = ca1-cb1)
(Delta_c2 = ca2-cb2)
(delta = 2)
(Delta_v = 0 )
(T.vec = seq(0, 0.5, 0.01))# for reciprocal tariff

#verify condition for Result 4 (Appendix E)
(Delta_c1 < 3*delta)
(Delta_c2 < 3*delta)

# profits case 1 (eq 12 in the paper)
(profita1.vec = n*(T.vec*Delta_c1 +2*(Delta_c1 -3*delta))^2 / (18*delta*(T.vec + 2)))
#
(profitb1.vec = n*(T.vec*Delta_c1 +2*(Delta_c1 +3*delta))^2 / (18*delta*(T.vec + 2)))

# market shares case 1 (eq E1 in the paper)
(xhat1_1.vec = (T.vec^2*(ca1 +2*cb1) +T.vec*(ca1 +5*cb1 +9*delta) -2*(ca1-cb1-3*delta)) / (6*delta*(T.vec+2)) )
#
(xhat2_1.vec = -(T.vec^2*(2*ca1 +cb1) +T.vec*(5*ca1 +cb1 +3*delta) +2*(ca1-cb1-3*delta)) / (6*delta*(T.vec+2)) )

# profits case 2 (eq 12 in the paper)
(profita2.vec = n*(T.vec*Delta_c2 +2*(Delta_c2 -3*delta))^2 / (18*delta*(T.vec + 2)))
#
(profitb2.vec = n*(T.vec*Delta_c2 +2*(Delta_c2 +3*delta))^2 / (18*delta*(T.vec + 2)))

# market shares case 1 (eq E1 in the paper)
(xhat1_2.vec = (T.vec^2*(ca2 +2*cb2) +T.vec*(ca2 +5*cb2 +9*delta) -2*(ca2-cb2-3*delta)) / (6*delta*(T.vec+2)) )
#
(xhat2_2.vec = -(T.vec^2*(2*ca2 +cb2) +T.vec*(5*ca2 +cb2 +3*delta) +2*(ca2-cb2-3*delta)) / (6*delta*(T.vec+2)) )



