# tariff_2025_x_y.R 
# Do Firms Benefit from Tariff Wars?

# packages used
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX in ggplot
#library(xtable)# export data frames to LaTeX tables
#library(dplyr)# sorting by group (by year)

# model initial parameters
(n1 = 120)
#n1 = 180
(n2 = 120)
(c = 1)
(delta = 4)
(t1.vec = seq(0, 1, 0.1))
(t2.vec = seq(0, 1, 0.1))
(T.vec = seq(0, 1, 0.1))# for reciprocal tariff

# equilibrium prices, eq (6)
(pa.vec = ( c*(n1*(t1.vec+3) +n2*(2*t2.vec+3)) +3*delta*(n1+n2) )/(3*(n1+n2*(t2.vec+1))))
#
(pb.vec = ( c*(n1*(2*t1.vec+3) +n2*(t2.vec+3)) +3*delta*(n1+n2) )/(3*(n1*(t1.vec+1)+n2)))

# equilibrium market shares, eq (3)
(xhat1.vec = 0.5 + (pb.vec*(1+t1.vec) -pa.vec)/(2*delta))
#
(xhat2.vec = 0.5 + (pb.vec -pa.vec*(1+t2.vec))/(2*delta))

### check market shares when t2 = 0 (no retaliation). Not in the paper
(t2_temp = 0)
# equilibrium prices, eq (6)
(pa.vec_temp = ( c*(n1*(t1.vec+3) +n2*(2*t2_temp+3)) +3*delta*(n1+n2) )/(3*(n1+n2*(t2_temp+1))))
#
(pb.vec_temp = ( c*(n1*(2*t1.vec+3) +n2*(t2_temp+3)) +3*delta*(n1+n2) )/(3*(n1*(t1.vec+1)+n2)))

# equilibrium market shares, eq (3)
(xhat1.vec_temp = 0.5 + (pb.vec_temp*(1+t1.vec) -pa.vec_temp)/(2*delta))
#
(xhat2.vec_temp = 0.5 + (pb.vec_temp -pa.vec_temp*(1+t2_temp))/(2*delta))
### end of check t2=0


# Result 2c simulations #### 
#(showing a possibility that pa increases with T)
n1
n2
delta
(3*n2*delta/c)# RHS of Result 2c
(n2=120)
(n1=n2+(1440))  
(n1=n2+(1500))  
#
# equilibrium prices, eq (6)
(pa.vec = ( c*(n1*(T.vec+3) +n2*(2*T.vec+3)) +3*delta*(n1+n2) )/(3*(n1+n2*(T.vec+1))))
#
(pb.vec = ( c*(n1*(2*T.vec+3) +n2*(T.vec+3)) +3*delta*(n1+n2) )/(3*(n1*(T.vec+1)+n2)))

# equilibrium market shares, eq (3)
(xhat1.vec = 0.5 + (pb.vec*(1+T.vec) -pa.vec)/(2*delta))
#
(xhat2.vec = 0.5 + (pb.vec -pa.vec*(1+T.vec))/(2*delta))
### end of simulating Result 2c


# Result 5 simulations #### 
#(n1=n2). Also confirms Figure 2
(n1=120)
(n2=120)
(T.vec = seq(0, 1, 0.1))# for reciprocal tariff
#
(pa.vec = ( c*(n1*(T.vec+3) +n2*(2*T.vec+3)) +3*delta*(n1+n2) )/(3*(n1+n2*(T.vec+1))))
#
(pb.vec = ( c*(n1*(2*T.vec+3) +n2*(T.vec+3)) +3*delta*(n1+n2) )/(3*(n1*(T.vec+1)+n2)))

# equilibrium market shares, eq (3)
(xhat1.vec = 0.5 + (pb.vec*(1+T.vec) -pa.vec)/(2*delta))
#
(xhat2.vec = 0.5 + (pb.vec -pa.vec*(1+T.vec))/(2*delta))
# verify symmetry: 1-xhat2 = xhat1
(1-xhat2.vec)


# Result 6 simulations ####
#(profit A increases with T when n1-n2 is large)
delta
(n2=120)
(3*n2*delta/c)# RHS of Result 2c
#(n1=n2+(1440))  
(n1=n2+(1500))  
(T.vec = seq(0, 0.5, 0.01))# for reciprocal tariff
#
# equilibrium prices, eq (6)
(pa.vec = ( c*(n1*(T.vec+3) +n2*(2*T.vec+3)) +3*delta*(n1+n2) )/(3*(n1+n2*(T.vec+1))))
#
(pb.vec = ( c*(n1*(2*T.vec+3) +n2*(T.vec+3)) +3*delta*(n1+n2) )/(3*(n1*(T.vec+1)+n2)))

# equilibrium market shares, eq (3)
(xhat1.vec = 0.5 + (pb.vec*(1+T.vec) -pa.vec)/(2*delta))
#
(xhat2.vec = 0.5 + (pb.vec -pa.vec*(1+T.vec))/(2*delta))
#
(T.vec)# => the above prices valid only for T not exceeding 0.5

# plotting profits as functions of T=t1=t2
(profita_T.vec = (c*(n1*T.vec-n2*T.vec) +3*delta*(n1+n2))^2/(18*delta*(n1 +n2*(T.vec+1))))#
#
(profitb_T.vec = (c*(n1*T.vec-n2*T.vec) -3*delta*(n1+n2))^2/(18*delta*(n1*(T.vec+1) +n2)))
#
(max(profita_T.vec))# max for the plot
(min(profitb_T.vec))# min for the plot

# make it data frame
(profit_T.df = data.frame(T.vec, profita_T.vec, profitb_T.vec, xhat1.vec, xhat2.vec))

# Figure 3 in paper ####
# plotting profit_T_n.f (profits with unequal market size)
ggplot(profit_T.df, aes(x=T.vec)) +geom_line(aes(y=profita_T.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=profitb_T.vec), linetype="longdash", size=1.2, color="black") + scale_x_continuous(breaks = seq(0,0.5,0.05)) + scale_y_continuous(breaks = seq(2200, 3700, 100)) +theme(axis.text.x = element_text(size = 20, color = "black"),  axis.text.y = element_text(size = 20, color = "black"), text = element_text(size = 20)) +labs(x=TeX("Reciprocal tariff rates: $t_1=t_2=T$"), y=TeX("Profits of brand-producing firms:  $\\pi^A$, $\\pi^B")) +annotate("text", x = 0.31, y = 2730, label =TeX("$\\pi^B(T,T)$"), size = 8, color="black") +annotate("text", x = 0.31, y = 3490, label =TeX("$\\pi^A(T,T)$"), size = 8, color="black") +annotate("text", x = 0.12, y = 3350, label =TeX("Free trade"), size = 8, color="black") +geom_segment(aes(x=0.12, y=3375, xend = 0.02, yend = 3460), arrow=arrow(type="closed"), size=1.0) +geom_point(aes(x=0, y=profita_T.vec[1]), size=4)

# Figure 4 in paper ####
# plotting xhat_large.f (market share x1 with unequal market size)
ggplot(profit_T.df, aes(x=T.vec)) +geom_line(aes(y=xhat1.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=xhat2.vec), linetype="longdash", size=1.2, color="black")  +scale_x_continuous(breaks = seq(0,0.5,0.05)) + scale_y_continuous(breaks = seq(0, 1, 0.05)) +theme(axis.text.x = element_text(size = 20, color = "black"),  axis.text.y = element_text(size = 20, color = "black"), text = element_text(size = 20)) +labs(x=TeX("Reciprocal tariff rates: $t_1=t_2=T$"), y=TeX("Brand A's domestic and foreign market shares:  $\\hat{x}_1$, $\\hat{x}_2$"))  +annotate("text", x = 0.12, y = 0.46, label =TeX("Free trade"), size = 8, color="black") +geom_segment(aes(x=0.12, y=0.47, xend = 0.02, yend = 0.49), arrow=arrow(type="closed"), size=1.0) +annotate("text", x = 0.25, y = 0.505, label =TeX("$\\hat{x}_1$"), size = 8, color="black") +annotate("text", x = 0.25, y = 0.27, label =TeX("$\\hat{x}_2$"), size = 8, color="black") +geom_point(aes(x=0, y=0.5), size=4)

### end of simulating Result 6 => back to n1=n2=120

### begin Section 4: Segmented markets
# Constructing Figure seg.f X3 for three values of n1 [only 2 values are plotted]
(c = 1)
(delta = 4)
#
(T.vec = seq(0, 1, 0.01))# focus on reciprocal tariff only
(t1.vec = T.vec)
(t2.vec = T.vec)
#(t1.vec = seq(0, 1, 0.1))
#(t2.vec = seq(0, 1, 0.1))
#(t1.vec = seq(0, 0.5, 0.001))
#(t2.vec = seq(0, 0.5, 0.001))

## Case 01 (benchmark) begins:
# Run this 3 times for 3 different values of n1
(n2 = 120)
(n1 = 120)# benchmark case where n1 = n2
(n2 = 380)
(n1 = 380)# benchmark case where n1 = n2
#(n1 = 540)# intermediate case n1 > n2
#(n1 = 720)# large difference between n1 and n2

# verify that Assumption 2 is satisfied. 
(RHS_seg = 3*delta/c)
RHS_seg > 1

# eql prices, equation 14 in the paper
(pa1_seg.vec = (c*(t1.vec+3) +3*delta)/3)
(pa2_seg.vec = (c*(2*t2.vec+3) +3*delta)/(3*(t2.vec+1)))
#
(pb1_seg.vec = (c*(2*t1.vec+3)+3*delta)/(3*(t1.vec+1)))
(pb2_seg.vec = (c*(t2.vec+3)+3*delta)/3)

# eql market shares
(xhat1_seg.vec = 0.5 +c*t1.vec/(6*delta))
(xhat2_seg.vec = 0.5 -c*t2.vec/(6*delta))
#(1-xhat2_seg.vec)# verify equals xhat1_seg.vec under n1=n2

# equilibrium profits: 3 times for benchmark cases 01, 02, 03
(profita_seg_01.vec = (pa1_seg.vec-c)*n1*xhat1_seg.vec +(pa2_seg.vec-c)*n2*xhat2_seg.vec)
#
(profitb_seg_01.vec = (pb1_seg.vec-c)*n1*(1-xhat1_seg.vec) +(pb2_seg.vec-c)*n2*(1-xhat2_seg.vec))

## Case 02 (intermediate) begins:
# Run this 3 times for 3 different values of n1
(n2 = 120)
#(n1 = 120)# benchmark case where n1 = n2
(n1 = 540)# intermediate case n1 > n2 
#(n1 = 320)# intermediate case n1 > n2
#(n1 = 720)# large difference between n1 and n2

# verify that Assumption 2 is satisfied. 
(RHS_seg = 3*delta/c)
RHS_seg > 1

# eql prices, equation 14 in the paper
(pa1_seg.vec = (c*(t1.vec+3) +3*delta)/3)
(pa2_seg.vec = (c*(2*t2.vec+3) +3*delta)/(3*(t2.vec+1)))
#
(pb1_seg.vec = (c*(2*t1.vec+3)+3*delta)/(3*(t1.vec+1)))
(pb2_seg.vec = (c*(t2.vec+3)+3*delta)/3)

# eql market shares
(xhat1_seg.vec = 0.5 +c*t1.vec/(6*delta))
(xhat2_seg.vec = 0.5 -c*t2.vec/(6*delta))
#(1-xhat2_seg.vec)# verify equals xhat1_seg.vec under n1=n2

# equilibrium profits: 3 times for benchmark cases 01, 02, 03
(profita_seg_02.vec = (pa1_seg.vec-c)*n1*xhat1_seg.vec +(pa2_seg.vec-c)*n2*xhat2_seg.vec)
#
(profitb_seg_02.vec = (pb1_seg.vec-c)*n1*(1-xhat1_seg.vec) +(pb2_seg.vec-c)*n2*(1-xhat2_seg.vec))

## Case 03 (highest) begins: [not used in paper]
# Run this 3 times for 3 different values of n1
(n2 = 120)
#(n1 = 120)# benchmark case where n1 = n2
#(n1 = 540)# intermediate case n1 > n2
#(n1 = 720)# large difference between n1 and n2
(n1 = 650)# large difference between n1 and n2

# verify that Assumption 2 is satisfied. 
(RHS_seg = 3*delta/c)
RHS_seg > 1

# eql prices, equation 14 in the paper
(pa1_seg.vec = (c*(t1.vec+3) +3*delta)/3)
(pa2_seg.vec = (c*(2*t2.vec+3) +3*delta)/(3*(t2.vec+1)))
#
(pb1_seg.vec = (c*(2*t1.vec+3)+3*delta)/(3*(t1.vec+1)))
(pb2_seg.vec = (c*(t2.vec+3)+3*delta)/3)

# eql market shares
(xhat1_seg.vec = 0.5 +c*t1.vec/(6*delta))
(xhat2_seg.vec = 0.5 -c*t2.vec/(6*delta))
#(1-xhat2_seg.vec)# verify equals xhat1_seg.vec under n1=n2

# equilibrium profits: 3 times for benchmark cases 01, 02, 03
(profita_seg_03.vec = (pa1_seg.vec-c)*n1*xhat1_seg.vec +(pa2_seg.vec-c)*n2*xhat2_seg.vec)
#
(profitb_seg_03.vec = (pb1_seg.vec-c)*n1*(1-xhat1_seg.vec) +(pb2_seg.vec-c)*n2*(1-xhat2_seg.vec))

# make it a data frame
#(seg.df = data.frame(t1.vec, t2.vec, pa1_seg.vec, pa2_seg.vec, xhat1_seg.vec, xhat2_seg.vec, profita_seg.vec, profitb_seg.vec))
#
#(seg.df = data.frame(T.vec, profita_seg_01.vec, profitb_seg_01.vec, profita_seg_02.vec, profitb_seg_02.vec, profita_seg_03.vec, profitb_seg_03.vec))
# focus on profit a only
(seg.df = data.frame(T.vec, profita_seg_01.vec, profita_seg_02.vec,  profita_seg_03.vec))
max(profita_seg_01.vec)# for y-axis in the plot
min(profita_seg_01.vec)


# Figure 5 in paper ####
ggplot(seg.df, aes(x=T.vec)) +geom_line(aes(y=profita_seg_01.vec), linetype="longdash", size=1.2, color="black") +geom_line(aes(y=profita_seg_02.vec), linetype="solid", size=1.2, color="black") + scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(min(1200), 1600, 20)) +theme(axis.text.x = element_text(size = 20, color = "black"),  axis.text.y = element_text(size = 20, color = "black"), text = element_text(size = 20)) +labs(x=TeX("Reciprocal tariff rates: $t_1=t_2=T$"), y=TeX("Profit of firm A:  $\\pi^A(T,T)$")) +annotate("text", x = 0.25, y = 1465, label =TeX("$\\pi^A(T,T)$ for $N_1=N_2"), size = 8, color="black") +annotate("text", x = 0.70, y = 1365, label =TeX("$\\pi^A(T,T)$ for $N_1 > N_2"), size = 8, color="black") 

#+annotate("text", x = 0.31, y = 3490, label =TeX("$\\pi^A(T,T)$"), size = 8, color="black") +annotate("text", x = 0.12, y = 3350, label =TeX("Free trade"), size = 8, color="black") +geom_segment(aes(x=0.12, y=3375, xend = 0.02, yend = 3460), arrow=arrow(type="closed"), size=1.0) +geom_point(aes(x=0, y=profita_T.vec[1]), size=4)



# Unused code ####
#
# ### Drawing iso-profit lines in the t1-t2 space (cancelled, not in the paper. )
# #
# (n1 = 120)
# n1 = 240
# (n2 = 120)
# (c = 1)
# (delta = 4)
# (t1.vec = seq(0, 1, 0.1))
# (t2.vec = seq(0, 1, 0.1))
# #(t1.vec = seq(0, 0.1, 0.001))
# #(t2.vec = seq(0, 0.1, 0.001))
# 
# 
# # equilibrium prices, eq (6)
# (pa.vec = ( c*(n1*(t1.vec+3) +n2*(2*t2.vec+3)) +3*delta*(n1+n2) )/(3*(n1+n2*(t2.vec+1))))
# #
# (pb.vec = ( c*(n1*(2*t1.vec+3) +n2*(t2.vec+3)) +3*delta*(n1+n2) )/(3*(n1*(t1.vec+1)+n2)))
# 
# # equilibrium market shares, eq (3)
# (xhat1.vec = 0.5 + (pb.vec*(1+t1.vec) -pa.vec)/(2*delta))
# #
# (xhat2.vec = 0.5 + (pb.vec -pa.vec*(1+t2.vec))/(2*delta))
# # verify symmetry
# (1-xhat2.vec)# should be equal to xhat1.vec under symmetry
# 
# # equilibrium profit eq (7)
# (profita.vec = (c*(n1*t1.vec-n2*t2.vec) +3*delta*(n1+n2))^2/(18*delta*(n1 +n2*(t2.vec+1))))
# #
# (profitb.vec = (c*(n1*t1.vec-n2*t2.vec) -3*delta*(n1+n2))^2/(18*delta*(n1*(t1.vec+1) +n2)))
# # cross verify 
# (profita.vec = (pa.vec-c)*(n1*xhat1.vec +n2*xhat2.vec))
# #
# (profitb.vec = (pb.vec-c)*(n1*(1-xhat1.vec) +n2*(1-xhat2.vec)))
# 
# ### draw iso-profit lines => Canceled, not in paper
# # Evaluate profit under free trade: t1=t2=0
# (profita_ft = (c*(n1*0-n2*0) +3*delta*(n1+n2))^2/(18*delta*(n1 +n2*(0+1))))#
# (profitb_ft = (c*(n1*0-n2*0) -3*delta*(n1+n2))^2/(18*delta*(n1*(0+1) +n2)))
# 
# # A's iso-profit line: t2 as function of t1 from Derive
# #(t2_iso_a.vec = (3*sqrt(profita_ft)*sqrt(delta)*sqrt(9*profita_ft*delta +2*c*(c*(n1*(t1.vec+1) +n2) +3*delta*(n1+n2))) +9*profita_ft*delta +c*(c*n1*t1.vec +3*delta*(n1+n2))) /(c^2*n2)) # 2nd root => not OK
# #
# (t2_iso_a.vec = -(3*sqrt(profita_ft)*sqrt(delta)*sqrt(9*profita_ft*delta +2*c*(c*(n1*(t1.vec+1) +n2) +3*delta*(n1+n2))) -9*profita_ft*delta -c*(c*n1*t1.vec +3*delta*(n1+n2))) /(c^2*n2))# 1st root => OK
# 
# # B's iso-profit curve: t2 as function of t1 from Derive
# (t2_iso_b.vec = (3*sqrt(2)*sqrt(profitb_ft)*sqrt(delta)*sqrt(n1*(t1.vec+1) +n2) +c*n1*t1.vec -3*delta*(n1+n2)) /(c*n2))# 2nd root => OK
# #
# #(t2_iso_b.vec = -(3*sqrt(2)*sqrt(profitb_ft)*sqrt(delta)*sqrt(n1*(t1.vec+1) +n2) -c*n1*t1.vec +3*delta*(n1+n2)) /(c*n2))# 1st root => not OK
# 
# # make it a data frame
# (iso.df = data.frame(t1.vec, t2_iso_a.vec, t2_iso_b.vec))
# dim(iso.df)
# #
# ggplot(iso.df, aes(x=t1.vec)) +geom_line(aes(y=t2_iso_a.vec), linetype="longdash", size=1.2, color="red") +geom_line(aes(y=t2_iso_b.vec), linetype="solid", size=1.2, color="black")
# # end of iso-profits => Canceled, not in paper

