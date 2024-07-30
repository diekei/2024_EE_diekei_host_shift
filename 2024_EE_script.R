### INTRODUCTION #####

# Welcome. For the description of the project please visit: https://github.com/diekei/2024_EE_diekei_host_shift
# Article is available at: 


## LIBRARY ####

library(tidyverse)
library(car)
library(lme4)
library(emmeans)
library(DHARMa)

library(ggplot2)
library(plotly)
library(ggtext)
library(ggtern)
library(ggalt)
library(ggridges)
library(svglite)

library(survival)
library(survminer)
library(ggsurvfit)

Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
install.packages("showtext")
library(showtext)
showtext_auto()


## HOST ACCEPTANCE - 2021 ####


## data ##

data.hac <- read.csv("2024_EE_data_host_acceptance.csv")

data.hac$rmik <- with(data.hac, ifelse(mik + leu == 0, 0, mik / (mik + leu)))
data.hac$rleu <- with(data.hac, ifelse(mik + leu == 0, 0, leu / (mik + leu)))
data.hac$rreuse <- with(data.hac, ifelse(mik + leu == 0, 0, 
                                         ifelse(host == "mik", 
                                         reuse / (reuse + mik),
                                         reuse / (reuse + leu))))

data.hac$alt <- with(data.hac, ifelse(host == "mik",
                                      (leu - mik) / (leu + mik),
                                      (mik - leu) / (mik + leu)))

data.hac$retain <- with(data.hac, ifelse(host == "mik", 
                                         (reuse - mik) / (reuse + mik), 
                                         (reuse - leu) / (reuse + leu)))

## plot ##

hac.summary <- data.hac %>%
  group_by(host) %>%
  summarise(
    mean_mik = mean(mik, na.rm = TRUE),
    sem_mik = sd(mik, na.rm = TRUE) / sqrt(sum(!is.na(mik))), 
    
    mean_leu = mean(leu, na.rm = TRUE),
    sem_leu = sd(leu, na.rm = TRUE) / sqrt(sum(!is.na(leu))),
  )

hac.labs <- data.frame(
  label = c("italic('L')*' (3\\u2640 2\\u2642)'", "italic('M')*' (6\\u2640 9\\u2642)'"),
  x = c(160, 93), 
  y = c(225, 45)
)

plot.hac <- ggplot(data.hac, aes(x = mik, y = leu)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.4) +
  geom_hline(yintercept = 0, size = 0.2) +
  geom_vline(xintercept = 0, size = 0.2) +
  geom_point(aes(colour = host, shape = host, fill = host, size = total), alpha = 0.5) +  
  geom_hline(data = hac.summary, aes(yintercept = mean_leu, color = host), linetype = "dashed", size = 0.4) +
  geom_vline(data = hac.summary, aes(xintercept = mean_mik, color = host), linetype = "dashed", size = 0.4) +
  geom_segment(data = hac.summary, aes(x = mean_mik, xend = mean_mik, y = mean_leu - sem_leu, yend = mean_leu + sem_leu, color = host), size = 1) +
  geom_segment(data = hac.summary, aes(y = mean_leu, yend = mean_leu, x = mean_mik - sem_mik, xend = mean_mik + sem_mik, color = host), size = 1) +
  geom_point(data = hac.summary, aes(x = mean_mik, y = mean_leu, shape = host, fill = host), size = 4, stroke = 1, colour = "grey30") +
  scale_color_manual(values = c("#FDB462", "#8DD3C7")) + 
  scale_fill_manual(values = c("#FDB462", "#8DD3C7")) + 
  scale_shape_manual(values = c(24,21)) +
  scale_y_continuous(breaks = seq(0, 500, by = 100)) +
  xlab(bquote(italic(Mikania) ~ leaf ~ area ~ consumed ~ (mm^2))) +
  ylab(bquote(italic(Leucas) ~ leaf ~ area ~ consumed ~ (mm^2))) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(), 
        legend.text = element_text(size = 10),
        #strip.background = element_blank(),
        #panel.border = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.85, 0.25),
        legend.key.size = unit(0.5, "cm"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 16),
        axis.title.x = element_text(margin = margin(t = 10, b = 0)), 
        axis.title.y = element_text(margin = margin(r = 10, l = 0))) +
  geom_text(data = hac.labs, aes(x = x, y = y, label = label), size = 4, 
            color = "black", parse = TRUE) +
  guides(colour = "none", fill = "none", shape = "none")

plot.hac
ggsave(filename = "Plots/leu_hac1.png", width = 4, height = 5, device = 'png', dpi = 1200)
ggsave(filename = "Plots/leu_hac1.pdf", width = 4, height = 5, device = 'pdf', dpi = 1200)
ggsave(filename = "Plots/leu_hac1.svg", width = 4, height = 5, device = 'svg')


hac.summary2 <- data.hac %>%
  group_by(host) %>%
  summarise(
    mean_alt = mean(alt, na.rm = TRUE),
    sem_alt = sd(alt, na.rm = TRUE) / sqrt(sum(!is.na(alt))), 
    
    mean_retain = mean(retain, na.rm = TRUE),
    sem_retain = sd(retain, na.rm = TRUE) / sqrt(sum(!is.na(retain))),
  )

hac.labs2 <- data.frame(
  label = c("italic('L')*' (3\\u2640 2\\u2642)'", "italic('M')*' (6\\u2640 9\\u2642)'"),
  x = c(-0.63, 0.79), 
  y = c(-0.55, -0.45)
)

plot.hac2 <- ggplot(data.hac, aes(x = retain, y = alt)) +
  geom_point(aes(colour = host, shape = host, fill = host, size = total), alpha = 0.5) +  
  geom_hline(yintercept = 0, size = 0.2) +
  geom_vline(xintercept = 0, size = 0.2) +
  geom_hline(data = hac.summary2, aes(yintercept = mean_alt, color = host), linetype = "dashed", size = 0.4) +
  geom_vline(data = hac.summary2, aes(xintercept = mean_retain, color = host), linetype = "dashed", size = 0.4) +
  geom_segment(data = hac.summary2, aes(x = mean_retain, xend = mean_retain, y = mean_alt - sem_alt, yend = mean_alt + sem_alt, color = host), size = 1) +
  geom_segment(data = hac.summary2, aes(y = mean_alt, yend = mean_alt, x = mean_retain - sem_retain, xend = mean_retain + sem_retain, color = host), size = 1) +
  geom_point(data = hac.summary2, aes(x = mean_retain, y = mean_alt, shape = host, fill = host), size = 4, stroke = 1, colour = "grey30") +
  scale_color_manual(values = c("#FDB462", "#8DD3C7")) + 
  scale_fill_manual(values = c("#FDB462", "#8DD3C7")) + 
  scale_shape_manual(values = c(24,21)) +
  scale_y_continuous(expand = c(0.02, 0.02), limits = c(-1, 1),
                     breaks = c(-1, -0.5, 0, 0.5, 1), labels = c("-1", "-0.5", "0", "0.5", "1")) +
  scale_x_continuous(expand = c(0.05, 0.02), limits = c(-1, 1),
                     breaks = c(-1, -0.5, 0, 0.5, 1), labels = c("-1", "-0.5", "0", "0.5", "1")) +
  xlab("\nAbility to reutilise original host") + 
  ylab("Ability to use alternative host\n") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(), 
        legend.text = element_text(size = 10),
        #strip.background = element_blank(),
        #panel.border = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.14, 0.85),
        legend.key.size = unit(0.5, "cm"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 16),
        axis.title.x = element_text(margin = margin(t = 10, b = 0)), 
        axis.title.y = element_text(margin = margin(r = 10, l = 0))) +
  geom_text(data = hac.labs2, aes(x = x, y = y, label = label), size = 4, 
            color = "black", parse = TRUE) +
  guides(colour = "none", fill = "none", shape = "none")

plot.hac2
ggsave(filename = "Plots/leu_hac2.png", width = 5, height = 5, device = 'png', dpi = 1200)
ggsave(filename = "Plots/leu_hac2.pdf", width = 5, height = 5, device = 'pdf', dpi = 1200)


## HOST PREFERENCE - 2023 ####

## data ##

data.pref <- read.csv("2024_EE_data_host_preference.csv")

data.pref$rmik <- with(data.pref, ifelse(mik + leu + col == 0, NA, mik / (mik + leu + col)))
data.pref$rleu <- with(data.pref, ifelse(mik + leu + col == 0, NA, leu / (mik + leu + col)))
data.pref$rcol <- with(data.pref, ifelse(mik + leu + col == 0, NA, col / (mik + leu + col)))

pref.summary <- data.pref %>%
  group_by(loc, host) %>%
  summarise(
    mik = mean(mik, na.rm = TRUE),
    leu = mean(leu, na.rm = TRUE),
    col = mean(col, na.rm = TRUE),
    .groups = 'drop'
  )

plot.pref <- ggtern(data.pref %>% filter(!is.na(rmik) & !is.na(rleu) & !is.na(rcol)), aes(x = mik, y = col, z = leu)) +
  geom_point(aes(group = loc, shape = host, colour = host, fill = host, size = total), alpha = 0.5) +
  geom_encircle(aes(group = loc, colour = host), alpha = 1, size = 2) + 
  #geom_crosshair_tern(data = pref.summary, lty = 1, alpha = 0.5) +
  #geom_point(data = pref.summary, shape = 21, size = 5, col = "black", bg = "slategray1") +
  #geom_point(data = pref.summary, aes(shape = host, colour = host, fill = host), size = 5, alpha = 0.5) +
  scale_color_manual(values = c("#FCCDE5", "#FDB462", "#8DD3C7")) + 
  scale_fill_manual(values = c("#FCCDE5", "#FDB462", "#8DD3C7")) + 
  tern_limits(T = 1.2, L = 1.2, R = 1.2) +
  xlab("Mikania") + 
  ylab("Coleus") +
  zlab("Leucas") +
  Tarrowlab("prefer only Coleus") + 
  Larrowlab("prefer only Mikania") + 
  Rarrowlab("prefer only Leucas") +
  theme_showarrows() +
  theme_bw() +
  theme(legend.position = 'none',
        tern.axis.title = element_text(face = "italic", size = 16),
        tern.axis.arrow.show = T,
        tern.axis.arrow.text = element_text(face = "italic", size = 16),
        tern.axis.arrow.linewidth = 2,
        tern.axis.arrow = element_line(linewidth = 2))

plot.pref
ggsave(filename = "Plots/leu_hap1.png", width = 7, height = 7, device = 'png', dpi = 1200)
ggsave(filename = "Plots/leu_hap1.pdf", width = 7, height = 7, device = 'pdf', dpi = 1200)
ggsave(filename = "Plots/leu_hap1.svg", width = 7, height = 7, device = 'svg', dpi = 1200)


## HOST PREFERENCE - 2023 - TERNARY FUNCTIONS ####

tickx1<-function(xcoordo,ticks_inner_outer,tick_length,spaco2,...){
  x1<-((sqrt(3)*(1-xcoordo)-2*spaco2)/(2*sqrt(3)))-0.5
  y1<-sqrt(3)*(x1+0.5)+2*spaco2
  x2<-((sqrt(3)*(1-xcoordo)-(2*(spaco2+tick_length*sin(60*(pi/180)))))/(2*sqrt(3)))-0.5
  y2<-sqrt(3)*(x2+0.5)+2*(spaco2+tick_length*sin(60*(pi/180)))
  segments(x1,y1,x2,y2,...)
}

tickx2<-function(xcoordo,ticks_inner_outer,tick_length,spaco2,...){
  if(ticks_inner_outer=="outer"){
    add_space<-spaco2
  } else{
    add_space<-0
  }
  xtransl<-xcoordo-add_space*(3^(-(1/2)))-0.5
  ynew<-(-add_space-tick_length*sin(60*(pi/180)))
  xnew<-xcoordo+ynew*(3^(-(1/2)))-0.5
  segments(xtransl,-add_space,xnew,ynew,...)
}

tickx3<-function(xcoordo,ticks_inner_outer,tick_length,spaco2,...){
  if(ticks_inner_outer=="outer"){
    add_space<-spaco2
  } else{
    add_space<-0
  }
  xtransl<-1-(xcoordo/2)+(add_space/sin(60*(pi/180)))-0.5
  yboth<-xcoordo*(sqrt(3)/2)
  xnew<-xtransl+tick_length
  segments(xtransl,yboth,xnew,yboth,...)
}


axx1<-function(xcoordo,ticks_inner_outer,tick_length,axis_d,spaco2,
               y_over_x_ext1,...){
  x2<-((sqrt(3)*(1-xcoordo)-(2*(spaco2+(tick_length+axis_d)*sin(60*(pi/180)))))/(2*sqrt(3)))-0.5
  y2<-sqrt(3)*(x2+0.5)+2*(spaco2+(tick_length+axis_d)*sin(60*(pi/180)))
  text(x2,y2,xcoordo,srt=360-(atan(sqrt(3)*y_over_x_ext1)*180)/pi,...)
}

axx2<-function(xcoordo,ticks_inner_outer,tick_length,axis_d,spaco2,
               y_over_x_ext1,...){
  if(ticks_inner_outer=="outer"){
    add_space<-spaco2
  } else{
    add_space<-0
  }
  ynew<-(-add_space-(tick_length+axis_d)*sin(60*(pi/180)))
  xnew<-xcoordo+ynew*(3^(-(1/2)))-0.5
  text(xnew,ynew,xcoordo,srt=(atan(sqrt(3)*y_over_x_ext1)*180)/pi,...)
}

axx3<-function(xcoordo,ticks_inner_outer,tick_length,axis_d,spaco2,...){
  if(ticks_inner_outer=="outer"){
    add_space<-spaco2
  } else{
    add_space<-0
  }
  xtransl<-1-(xcoordo/2)+(add_space/sin(60*(pi/180)))-0.5
  xnew<-xtransl+(tick_length+axis_d)
  ynew<-xcoordo*(sqrt(3)/2)
  text(xnew,ynew,xcoordo,...)
}


ax1<-function(xcoordo,...){
  xstart<-1-xcoordo-0.5
  ystart<-0
  xnew<-(0.5-xcoordo/2)-0.5
  ynew<-(1-xcoordo)*(sqrt(3)/2)
  segments(xstart,ystart,xnew,ynew,...)
}

ax2<-function(xcoordo,...){
  ynew<-(1-xcoordo)*(sqrt(3)/2)
  xnew<-((ynew+sqrt(3)*xcoordo)/sqrt(3))-0.5
  segments(xcoordo-0.5,0,xnew,ynew,...)
}

ax3<-function(xcoordo,...){
  xstart<-(1-xcoordo/2)-0.5
  ystart<-xcoordo*(sqrt(3)/2)
  xnew<-(-abs(xstart))
  ynew<-xcoordo*(sqrt(3)/2)
  segments(xstart,ystart,xnew,ynew,...)
}

transf_x<-function(left,bottom,right){
  return(0.5*((2*bottom+right)/(left+bottom+right))-0.5)
}
transf_y<-function(left,bottom,right){
  return((sqrt(3)/2)*(right/((left+bottom+right))))
}


ternary_choice <- function(
    output_file="",
    data_fr,
    dot_cols,
    dot_sizes,
    dot_shapes,
    transp=1/3,
    jitter_dots=0,
    est_coord,
    est_cols,
    est_line=T,
    est_shapes,
    cont_yn=T,
    cont_list,
    cont_wts,
    nlevels_cont=10,
    cont_cols=c("black"),
    remove_outer=F,
    bg_col=adjustcolor("red",0),
    ticks_inner_outer="outer",
    spaco2=0,
    disto=0.15,
    half_length_arrow=0.3,
    arrow_lwd=1,
    disto_lab=0.03,
    tick_length1=0.03,
    space_around=c(0.25,0.2,0.1,0.2),
    line_at=seq(0,1,0.1),
    tck_at=seq(0,1,0.1),
    axis_l=seq(0,1,0.1),
    axis_cex=0.7,
    axis_d1=0.025,
    title_names=c("sp1","sp2","sp3"),
    title_cex=1,
    scales_to_plot=1:3
){
  
  # Open png if wished. Set dimensions such that the angles of the triangle are 60 degrees.
  if(output_file!=""){
    png(output_file,width=1500,
        height=((((sqrt(3))/2)+sum(space_around[c(1,3)]))/(1+sum(space_around[c(2,4)])))*1500,
        res=300)
  }
  
  # Replace if some arguments were not passed by user.
  if(missing(dot_cols)&length(data_fr)>0){
    dot_cols<-rep("gray",nrow(data_fr))
  }
  if(missing(dot_shapes) & length(data_fr) > 0){
    dot_shapes <- rep(21, nrow(data_fr))
  }
  if(missing(dot_sizes)&length(data_fr)>0){
    dot_sizes<-rep(1,nrow(data_fr))
  }
  if(missing(cont_list)&length(data_fr)>0){
    cont_list<-list(1:nrow(data_fr))
  }
  if(missing(cont_wts)&cont_yn==T){
    cont_wts<-lapply(1:length(cont_list),function(x) rep(1,length(cont_list[[x]])))
  }
  
  # Calculate ternary position of points (and jitter, if wished)
  if(length(data_fr)>0){
    new_x<-jitter(transf_x(left=data_fr[,1],bottom=data_fr[,2],right=data_fr[,3]),jitter_dots)
    new_y<-jitter(transf_y(left=data_fr[,1],bottom=data_fr[,2],right=data_fr[,3]),jitter_dots)
  }
  
  # Remove outer white space
  # Save first old mar setting
  old_mar<-par("mar")
  par(mar=c(0,0,0,0))
  
  # Open plot
  plot(1,1,xlim=c(-space_around[2]-0.5,1+space_around[4]-0.5),
       ylim=c(-space_around[1],((sqrt(3))/2)+space_around[3]),xaxs="i",yaxs="i",
       xaxt="n",yaxt="n",xlab="",ylab="",type="n",bty="n")
  
  # This helps later with positioning. 
  # It's the ratio of width and height of plotting device times
  # ratio of width and height of Cartesian coordinate system.
  y_over_x_ext<-(par("pin")[2]/par("pin")[1])*(diff(par("usr")[1:2])/diff(par("usr")[3:4]))
  
  # Add desired background color
  rect(-1000,-1000,1000,1000,border=NA,col=bg_col)
  
  # Add white triangle
  polygon(c(0-0.5,1-0.5,0.5-0.5),c(0,0,sqrt(3)/2),col="white",border=NA)
  
  # The coordinate system grid
  ax1(line_at,col="grey70",lwd=0.5)
  ax2(line_at,col="grey70",lwd=0.5)
  ax3(line_at,col="grey70",lwd=0.5)
  
  # The scales for each axis
  if(1%in%scales_to_plot){
    segments(1-0.5,0,0.25-0.5,0.5*(sqrt(3)/2),lty="dashed")
  }
  if(2%in%scales_to_plot){
    segments(0.5-0.5,0,0.5-0.5,sqrt(3)/2,lty="dashed")
  }
  if(3%in%scales_to_plot){
    segments(0-0.5,0,0.75-0.5,0.5*(sqrt(3)/2),lty="dashed")
  }
  # (REMOVED: These would be the 1/3 lines)
  # ax1(1/3,col="red",lty="dashed")
  # ax2(1/3,col="blue",lty="dashed")
  # ax3(1/3,col="green",lty="dashed")
  
  # If outer area should be removed
  if(remove_outer){
    # Add points
    if(length(data_fr)>0){
      points(new_x,new_y,pch= dot_shapes,
             bg=adjustcolor(dot_cols,transp),cex=dot_sizes)
    }
    # Remove all outside triangle
    polygon(c(-1000,1000,1000,-1000),
            c(-1000,-1000,-spaco2,-spaco2),col=bg_col,border=NA)
    m_new<-(-sqrt(3))
    t_new<-(-spaco2)-(m_new*(1+spaco2*(3/sqrt(3))))
    polygon(c(1+spaco2*(3/sqrt(3)),1000,1000+(1000-t_new)/m_new,(1000-t_new)/m_new)-0.5,
            c(-spaco2,-spaco2,1000,1000),col=bg_col,border=NA)
    t_new2<-(-spaco2)-(-m_new*(-spaco2*(3/sqrt(3))))
    polygon(c(-1000,-spaco2*(3/sqrt(3)),(1000-t_new2)/(-m_new),-1000+spaco2*(3/sqrt(3))+(1000-t_new2)/(-m_new))-0.5,
            c(-spaco2,-spaco2,1000,1000),col=bg_col,border=NA)
  }
  
  # Draw "axes"
  segments(c(0,0.5,0)-0.5,
           c(0,sqrt(3)/2,0),
           c(0.5,1,1)-0.5,
           c(sqrt(3)/2,0,0),"black",lwd=0.75)
  
  # Draw outer triangle
  segments(c(-spaco2*(3/sqrt(3)),0.5,-spaco2*(3/sqrt(3)))-0.5,
           c(-spaco2,(sqrt(3)/2)+2*spaco2,-spaco2),
           c(0.5,1+spaco2*(3/sqrt(3)),1+spaco2*(3/sqrt(3)))-0.5,
           c((sqrt(3)/2)+2*spaco2,-spaco2,-spaco2),lwd=1.5)
  
  # Add ticks
  tickx1(tck_at,ticks_inner_outer=ticks_inner_outer,col="black",lwd=1,
         tick_length=tick_length1,spaco2=spaco2)
  tickx2(tck_at,ticks_inner_outer=ticks_inner_outer,col="black",lwd=1,
         tick_length=tick_length1,spaco2=spaco2)
  tickx3(tck_at,ticks_inner_outer=ticks_inner_outer,col="black",lwd=1,
         tick_length=tick_length1,spaco2=spaco2)
  
  # Add axis labels
  if(axis_cex>0){
    axx1(xcoordo=axis_l,ticks_inner_outer=ticks_inner_outer,
         tick_length=tick_length1,axis_d=axis_d1,cex=axis_cex,spaco2=spaco2,
         y_over_x_ext1=y_over_x_ext)
    axx2(xcoordo=axis_l,ticks_inner_outer=ticks_inner_outer,
         tick_length=tick_length1,axis_d=axis_d1,cex=axis_cex,spaco2=spaco2,
         y_over_x_ext1=y_over_x_ext)
    axx3(xcoordo=axis_l,ticks_inner_outer=ticks_inner_outer,
         tick_length=tick_length1,axis_d=axis_d1,cex=axis_cex,spaco2=spaco2)
  }
  
  # Add points (if not added before already)
  if(!remove_outer){
    if(length(data_fr)>0){
      points(new_x, new_y, pch=dot_shapes,
             bg=adjustcolor(dot_cols, transp), col=dot_cols, cex=dot_sizes)
    }
  }
  
  # If outer area should be removed
  if(remove_outer){
    if(length(data_fr)>0){
      points(new_x, new_y, pch=dot_shapes,
             bg=adjustcolor(dot_cols, transp), col=dot_cols, cex=dot_sizes)
    }
  }
  
  # Add contours, if wished.
  if(cont_yn){
    if(length(data_fr)>0){
      # Call Ternary package and set ternDirection to 1.
      if(!"Ternary"%in% rownames(installed.packages())){
        install.packages("Ternary")
      }
      suppressMessages(suppressWarnings(library(Ternary)))
      options(ternDirection=1)
      # Go one by one through groups
      for(conturo in 1:length(cont_list)){
        cont_sub<-data_fr[cont_list[[conturo]],c(3,2,1)]
        # Inflate table by weights
        cont_sub_new<-cont_sub[-(1:nrow(cont_sub)),]
        for(multipl in 1:nrow(cont_sub)){
          for(x in 1:cont_wts[[conturo]][multipl]){
            cont_sub_new<-rbind(cont_sub_new,cont_sub[multipl,])
          }
        }
        # Calculate Ternary density (using the weighted/bloated data set)
        TernaryDensityContour(coordinates=cont_sub_new,
                              drawlabels=F,col=cont_cols[conturo],direction=1,
                              nlevels=nlevels_cont)
      }
    }
  }
  
  # Positions of text
  x_textx1<-0.25-sin(60*(pi/180))*(spaco2+disto+disto_lab)-0.5
  y_textx1<-0.5*(sqrt(3)/2)+cos(60*(pi/180))*(spaco2+disto+disto_lab)
  x_textx2<-0.5-0.5
  y_textx2<-(-(spaco2+disto+disto_lab))
  x_textx3<-0.75+sin(60*(pi/180))*(spaco2+disto+disto_lab)-0.5
  y_textx3<-0.5*(sqrt(3)/2)+cos(60*(pi/180))*(spaco2+disto+disto_lab)
  
  # The respective position on the arrow
  arrx_x1<-0.25-sin(60*(pi/180))*(spaco2+disto)-0.5
  arry_x1<-0.5*(sqrt(3)/2)+cos(60*(pi/180))*(spaco2+disto)
  arrx_x3<-0.75+sin(60*(pi/180))*(spaco2+disto)-0.5
  arry_x3<-0.5*(sqrt(3)/2)+cos(60*(pi/180))*(spaco2+disto)
  
  # Calculate gradient triangle we need
  grad_x<-cos(60*(pi/180))*half_length_arrow
  grad_y<-sin(60*(pi/180))*half_length_arrow
  
  # Add arrows
  arrows(arrx_x1+grad_x,
         arry_x1+grad_y,
         arrx_x1-grad_x,
         arry_x1-grad_y,
         lend=3,length=0.1,lwd=arrow_lwd)
  arrows(x_textx2-half_length_arrow,
         -spaco2-disto,
         x_textx2+half_length_arrow,
         -spaco2-disto,
         lend=3,length=0.1,lwd=arrow_lwd)
  arrows(arrx_x3+grad_x,
         arry_x3-grad_y,
         arrx_x3-grad_x,
         arry_x3+grad_y,
         lend=3,length=0.1,lwd=arrow_lwd)
  
  # Add axis names
  safe_xpd<-par("xpd")
  par(xpd=NA)
  text(x_textx1,
       y_textx1,
       substitute(italic(b),list(b=title_names[1])),
       srt=(atan(sqrt(3)*y_over_x_ext)*180)/pi,
       cex=title_cex)
  text(x_textx2,
       y_textx2,
       substitute(italic(b),list(b=title_names[2])),srt=0,
       cex=title_cex)
  text(x_textx3,
       y_textx3,
       substitute(italic(b),list(b=title_names[3])),
       srt=360-(atan(sqrt(3)*y_over_x_ext)*180)/pi,
       cex=title_cex)
  par(xpd=safe_xpd)
  
  # CrIs
  if(length(est_coord)>0){
    
    # Go through each CrI table one by one
    for(est_list in 1:length(est_coord)){
      
      # CrI hexagons
      # 6 intersections
      # Starting at x1 upper, x2 lower
      # x1 upper: y = -sqrt(3) * x + (1-est_coord[[est_list]][1,3])*sqrt(3)
      # x2 lower: y = sqrt(3) * x - est_coord[[est_list]][2,2]*sqrt(3)
      x_x1_upp_x2_low<-(((1-est_coord[[est_list]][1,3])*sqrt(3)+est_coord[[est_list]][2,2]*sqrt(3))/(2*sqrt(3)))-0.5
      y_x1_upp_x2_low<-sqrt(3)*(x_x1_upp_x2_low+0.5)-est_coord[[est_list]][2,2]*sqrt(3)
      
      # x1 upper, x3 lower
      # x1 upper: y = -sqrt(3) * x + (1-est_coord[[est_list]][1,3])*sqrt(3)
      # x3 lower: y = (sqrt(3)/2)*est_coord[[est_list]][3,2]
      x_x1_upp_x3_low<-(((sqrt(3)/2)*est_coord[[est_list]][3,2]-(1-est_coord[[est_list]][1,3])*sqrt(3))/(-sqrt(3)))-0.5
      y_x1_upp_x3_low<-(sqrt(3)/2)*est_coord[[est_list]][3,2]
      
      # x2 upper, x3 lower
      # x2 upper: y = sqrt(3) * x - est_coord[[est_list]][2,3]*sqrt(3)
      # x3 lower: y = (sqrt(3)/2)*est_coord[[est_list]][3,2]
      x_x2_upp_x3_low<-(((sqrt(3)/2)*est_coord[[est_list]][3,2]+est_coord[[est_list]][2,3]*sqrt(3))/(sqrt(3)))-0.5
      y_x2_upp_x3_low<-(sqrt(3)/2)*est_coord[[est_list]][3,2]
      
      # x2 upper, x1 lower
      # x2 upper: y = sqrt(3) * x - est_coord[[est_list]][2,3]*sqrt(3)
      # x1 lower: y = -sqrt(3) * x + (1-est_coord[[est_list]][1,2])*sqrt(3)
      x_x2_upp_x1_low<-(((1-est_coord[[est_list]][1,2])*sqrt(3)+est_coord[[est_list]][2,3]*sqrt(3))/(2*sqrt(3)))-0.5
      y_x2_upp_x1_low<-sqrt(3)*(x_x2_upp_x1_low+0.5)-est_coord[[est_list]][2,3]*sqrt(3)
      
      # x3 upper, x1 lower
      # x3 upper: y = (sqrt(3)/2)*est_coord[[est_list]][3,3]
      # x1 lower: y = -sqrt(3) * x + (1-est_coord[[est_list]][1,2])*sqrt(3)
      x_x3_upp_x1_low<-(((sqrt(3)/2)*est_coord[[est_list]][3,3]-(1-est_coord[[est_list]][1,2])*sqrt(3))/(-sqrt(3)))-0.5
      y_x3_upp_x1_low<-(sqrt(3)/2)*est_coord[[est_list]][3,3]
      
      # x3 upper, x2 lower
      # x3 upper: y = (sqrt(3)/2)*est_coord[[est_list]][3,3]
      # x2 lower: y = sqrt(3) * x - est_coord[[est_list]][2,2]*sqrt(3)
      x_x3_upp_x2_low<-(((sqrt(3)/2)*est_coord[[est_list]][3,3]+est_coord[[est_list]][2,2]*sqrt(3))/(sqrt(3)))-0.5
      y_x3_upp_x2_low<-(sqrt(3)/2)*est_coord[[est_list]][3,3]
      
      # Add CrI hexagon (with little black outline)
      polygon(c(x_x1_upp_x2_low,x_x1_upp_x3_low,x_x2_upp_x3_low,
                x_x2_upp_x1_low,x_x3_upp_x1_low,x_x3_upp_x2_low),
              c(y_x1_upp_x2_low,y_x1_upp_x3_low,y_x2_upp_x3_low,
                y_x2_upp_x1_low,y_x3_upp_x1_low,y_x3_upp_x2_low),
              col=adjustcolor("white",0),border="black",lwd=6)
      polygon(c(x_x1_upp_x2_low,x_x1_upp_x3_low,x_x2_upp_x3_low,
                x_x2_upp_x1_low,x_x3_upp_x1_low,x_x3_upp_x2_low),
              c(y_x1_upp_x2_low,y_x1_upp_x3_low,y_x2_upp_x3_low,
                y_x2_upp_x1_low,y_x3_upp_x1_low,y_x3_upp_x2_low),
              col=adjustcolor("white",0),border=est_cols[est_list],lwd=6)
    }
    
    # Go through each CrI table one by one
    for(est_list in 1:length(est_coord)){
      
      # Translate coordinates into ternary space
      est_x<-transf_x(left=est_coord[[est_list]][1,1],bottom=est_coord[[est_list]][2,1],right=est_coord[[est_list]][3,1])
      est_y<-transf_y(left=est_coord[[est_list]][1,1],bottom=est_coord[[est_list]][2,1],right=est_coord[[est_list]][3,1])
      
      # Helpful estimator lines (going from estimator to outer triangle)
      if(est_line){
        x1<-((sqrt(3)*(1-est_coord[[est_list]][1,1])-2*spaco2)/(2*sqrt(3)))-0.5
        y1<-sqrt(3)*(x1+0.5)+2*spaco2
        
        x2<-est_coord[[est_list]][2,1]-spaco2*(3^(-(1/2)))-0.5
        y2<-(-spaco2)
        
        x3<-1-(est_coord[[est_list]][3,1]/2)+(spaco2/sin(60*(pi/180)))-0.5
        y3<-est_coord[[est_list]][3,1]*(sqrt(3)/2)
        # Add lines (with little black outline)
        segments(rep(est_x,3),rep(est_y,3),
                 c(x1,x2,x3),c(y1,y2,y3),col=adjustcolor("black",0.5),lwd=2)
        segments(rep(est_x,3),rep(est_y,3),
                 c(x1,x2,x3),c(y1,y2,y3),col=est_cols[est_list],lwd=1.6)
      }
      
      # Add estimator points
      if(length(est_coord) > 0){
        for(est_list in 1:length(est_coord)){
          est_x <- transf_x(left=est_coord[[est_list]][1,1], bottom=est_coord[[est_list]][2,1], right=est_coord[[est_list]][3,1])
          est_y <- transf_y(left=est_coord[[est_list]][1,1], bottom=est_coord[[est_list]][2,1], right=est_coord[[est_list]][3,1])
          
          # Updated code to use custom shapes
          points(est_x, est_y, pch=est_shapes[est_list], bg=est_cols[est_list], cex=1.3)
        }
      }
      
    }
    
  }
  
  # Close png if wished
  if(output_file!=""){
    invisible(dev.off())
  }
  
  # Restore old mar setting
  par(mar=old_mar)
}

## HOST PREFERENCE - 2023 - TERNARY PLOT ####

calculate_estimator <- function(data, column) {
  mean_val <- mean(data[[column]], na.rm = TRUE)
  se_val <- sd(data[[column]], na.rm = TRUE) / sqrt(nrow(data))
  lower_val <- mean_val - se_val
  upper_val <- mean_val + se_val
  return(data.frame(estimator = mean_val, lower = lower_val, upper = upper_val))
}

locs <- unique(data.pref$loc)
estimator_table_1 <- lapply(locs, function(loc) {
  loc_data <- data.pref %>% filter(loc == !!loc)
  rleu_stats <- calculate_estimator(loc_data, "rleu")
  rmik_stats <- calculate_estimator(loc_data, "rmik")
  rcol_stats <- calculate_estimator(loc_data, "rcol")
  return(data.frame(estimator = c(rmik_stats$estimator, rleu_stats$estimator, rcol_stats$estimator),
                    lower = c(rmik_stats$lower, rleu_stats$lower, rcol_stats$lower),
                    upper = c(rmik_stats$upper, rleu_stats$upper, rcol_stats$upper)))
})

estimator_table_2 <- data.frame(estimator = c(0.33, 0.33, 0.33), lower = c(NA, NA, NA), upper = c(NA, NA, NA))
estimator_table <- append(estimator_table_1, list(estimator_table_2))

png("Plots/leu_hap2.png", height = 7, width = 7, units = "in", res = 1200)
pdf("Plots/leu_hap2.pdf", height = 7, width = 7)
svg("Plots/leu_hap2.svg", height = 7, width = 7)


ternary_choice(
  output_file="",
  data_fr=data.pref[ , c("rmik","rleu","rcol")],
  dot_cols=ifelse(data.pref$host == "mik", "#8DD3C7",
                  ifelse(data.pref$host == "leu", "#FDB462",
                         ifelse(data.pref$host == "col", "#FCCDE5", "black"))),
  dot_shapes=ifelse(data.pref$host == "mik", 21,
                    ifelse(data.pref$host == "leu", 24, 
                           ifelse(data.pref$host == "col", 25,
                                  19))), 
  dot_sizes=0.3*sqrt(data.pref$total),
  transp=0.2,
  jitter_dots=0.5,
  est_coord=estimator_table,
  est_cols=c("#8DD3C7", "#8DD3C7", "#8DD3C7", "#8DD3C7", "#FCCDE5", "#FCCDE5", "#FDB462", "#FDB462", "grey50"),
  est_shapes=c(21, 21, 21, 21, 25, 25, 24, 24, 22),
  est_line=T,
  cont_yn=F,
  remove_outer=F,
  ticks_inner_outer="outer",
  spaco2=0.07,
  disto=100,
  half_length_arrow=0.3,
  arrow_lwd=2,
  disto_lab=0.1,
  tick_length1=0.02,
  space_around=c(((1-((sqrt(3))/2)))+0.125,0.25,0.25,0.25),
  line_at=seq(0,1,0.1),
  tck_at=seq(0,1,0.1),
  axis_l=seq(0,1,0.1),
  axis_cex=1.2,
  axis_d1=0.04,
  title_names=c(),
  title_cex=1.5,
  scales_to_plot=c()
)

legend(x = 0.5, y = 0.7, 
       legend = c(50, 150, 300), 
       pt.cex = 0.3 * sqrt(c(50, 150, 300)), 
       pch = 21, col = "black", pt.bg = "grey",
       #title = "total leaf area consumed\n(mm^2)", 
       bty = "n", x.intersp = 1.5)

text_positions <- data.frame(
  x = c(-0.45, -0.25, -0.42, -0.17, 0.01, 0.05, 0.25, 0.42), 
  y = c(-0.04, -0.04, 0.04, 0.12, 0.77, 0.83, -0.04, -0.04),
  labels = c("NUBO[2,1]", "MASG[8,0]", "PIND[3,2]", "SIWA[6,2]", "RANT[7,1]", "BATG[6,1]", "PETA[6,4]", "MINA[4,4]")
)

for(i in 1:nrow(text_positions)) {
  text(x = text_positions$x[i], y = text_positions$y[i], labels = text_positions$labels[i], cex = 1, col = "grey30")
}

dev.off()



## HOST SURVIVORSHIP ####

data.surv <- read.csv("2024_EE_data_host_survivorship.csv")
data.surv$food <- factor(data.surv$food, levels = c("mik", "leu", "col"))
data.surv$pop <- factor(data.surv$pop, levels = c("MINA", "SIWA", "MASG"))

curve.surv <- survfit(Surv(dur, stc) ~ pop + food, data = data.surv)
print(curve.surv)
summary(curve.surv)
summary(curve.surv)$table

df.surv <- data.frame(time = curve.surv$time,
                      n.risk = curve.surv$n.risk,
                      n.event = curve.surv$n.event,
                      n.censor = curve.surv$n.censor,
                      surv = curve.surv$surv,
                      upper = curve.surv$upper,
                      lower = curve.surv$lower)

head(df.surv)

plot.surv <- ggsurvplot(curve.surv, 
                        surv.median.line = "hv", 
                        size = 1.5,
                        conf.int = TRUE, 
                        conf.int.alpha = 0.1,
                        pval = FALSE, 
                        risk.table = FALSE,  
                        ncensor.plot = FALSE, 
                        break.time.by = 2, 
                        palette = c("#8DD3C7", "#FDB462", "#FCCDE5", 
                                    "#8DD3C7", "#FDB462", "#FCCDE5", 
                                    "#8DD3C7", "#FDB462", "#FCCDE5"),
                        linetype = "pop",
                        xlab = "\ntime in days", 
                        ylab = "survival probability\n",
                        ggtheme = theme_bw(),
                        legend = c(0.85,0.75), 
                        legend.labs = c("MASG on Mikania", "MASG on Leucas", "MASG on Coleus",
                                        "MINA on Mikania", "MINA on Leucas", "MINA on Coleus", 
                                        "SIWA on Mikania", "SIWA on Leucas", "MINA on Coleus"),
                        legend.title = "")

plot.surv$plot <- plot.surv$plot +
  scale_y_continuous(expand = expansion(mult = 0.01, add = 0.01), 
                     breaks = seq(0, 1, by = 0.25), 
                     labels = seq(0, 1, by = 0.25), 
                     minor_breaks = seq(0, 1, 1)) +
  scale_x_continuous(expand = expansion(mult = 0.01, add = 0.01), 
                     breaks = seq(0, 41, by = 2), 
                     minor_breaks = seq(0, 41, 1)) +
  scale_linetype_manual(values = c("MINA" = "solid", "SIWA" = "longdash", "MASG" = "dotdash"),
                        labels = c("MINA[18,18,17]", "SIWA[22,21,22]", "MASG[30,30,30]")) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        #legend.position = c(0.9, 0.75),
        legend.position = "right",
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(face = "italic", size = 10),
        legend.key.size = unit(0.5, "cm"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.border = element_rect(fill = NA, color = "grey70", size = 1.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 16))

plot.surv
ggsave(filename = "Plots/leu_surv.png", width = 10, height = 6, device = 'png', dpi = 1200)
ggsave(filename = "Plots/leu_surv.pdf", width = 10, height = 6, device = 'pdf', dpi = 1200)
ggsave(filename = "Plots/leu_surv.svg", width = 10, height = 6, device = 'svg', dpi = 1200)


## DEVELOPMENTAL TIME ####

devtime.summary <- data.surv %>%
  filter(status == "alive") %>%
  group_by(pop, food) %>%
  summarise(
    mean = mean(dur, na.rm = TRUE),
    se = sd(dur, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

data.surv$pop_num <- as.numeric(as.factor(data.surv$pop))
devtime.summary$pop_num <- as.numeric(as.factor(devtime.summary$pop))

plot.devtime <- ggplot(data.surv[data.surv$status == "alive", ], aes(x = dur, y = pop, fill = food)) +
  #geom_density_ridges(jittered_points = TRUE, point_shape = "|", point_size = 5, 
  #                    position = position_points_jitter(height = 0), alpha = 0.4, scale = 2, rel_min_height = 0.005) +
  geom_vline(data = devtime.summary, aes(xintercept = mean, linetype = pop), colour = "grey30", size = 0.5) +
  geom_density_ridges(alpha = 0.4, scale = 2, rel_min_height = 0.005) +
  geom_point(aes(colour = food, fill = food, shape = food), size = 2, alpha = 0.6, position = position_jitter(width = 0.1, height = 0.1)) +
  geom_segment(data = devtime.summary, aes(y = pop_num - 0.1, yend = pop_num - 0.1, x = mean - se, xend = mean + se), colour = "grey30", size = 1.5) +
  geom_point(data = devtime.summary, aes(y = pop_num - 0.1, x = mean, fill = food, shape = food), size = 4, colour = "grey30", stroke = 1) +
  scale_fill_manual(values = c("#8DD3C7", "#FDB462", "#FCCDE5"),
                    labels = c("Mikania", "Leucas", "Coleus")) +
  scale_colour_manual(values = c("#8DD3C7", "#FDB462", "#FCCDE5"),
                      labels = c("Mikania", "Leucas", "Coleus")) +
  scale_shape_manual(values = c(21, 24, 25)) +
  scale_x_continuous(breaks = seq(0, 46, by = 5), limits = c(14, 45), expand = c(0.01, 0.01)) +
  scale_linetype_manual(values = c("MINA" = "solid", "SIWA" = "longdash", "MASG" = "dotdash")) +
  xlab("\ndevelopmental time until pupation (days)") +
  ylab("") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(), 
        legend.text = element_text(face = "italic", size = 10),
        #strip.background = element_blank(),
        #panel.border = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.9, 0.2),
        legend.key.size = unit(0.5, "cm"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        axis.title.x = element_text(margin = margin(t = 10, b = 0)), 
        axis.title.y = element_text(margin = margin(r = 10, l = 0))) +
  guides(shape = FALSE)

plot.devtime
ggsave(filename = "Plots/leu_devtime.png", width = 10, height = 6, device = 'png', dpi = 1200)
ggsave(filename = "Plots/leu_devtime.pdf", width = 10, height = 6, device = 'pdf', dpi = 1200)
ggsave(filename = "Plots/leu_devtime.svg", width = 10, height = 6, device = 'svg', dpi = 1200)


mass.summary <- data.surv %>%
  filter(status == "alive") %>%
  group_by(pop, food) %>%
  summarise(
    mean = mean(mass*1000, na.rm = TRUE),
    se = sd(mass*1000, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

mass.summary$pop_num <- as.numeric(as.factor(mass.summary$pop))

plot.mass <- ggplot(data.surv[data.surv$status == "alive", ], aes(x = mass*1000, y = pop, fill = food)) +
  #geom_density_ridges(jittered_points = TRUE, point_shape = "|", point_size = 5, 
  #                    position = position_points_jitter(height = 0), alpha = 0.4, scale = 2, rel_min_height = 0.005) +
  geom_vline(data = mass.summary, aes(xintercept = mean, linetype = pop), colour = "grey30", size = 0.5) +
  geom_density_ridges(alpha = 0.4, scale = 2, rel_min_height = 0.005) +
  geom_point(aes(colour = food, fill = food, shape = food), size = 2, alpha = 0.6, position = position_jitter(width = 0.1, height = 0.1)) +
  geom_segment(data = mass.summary, aes(y = pop_num - 0.1, yend = pop_num - 0.1, x = mean - se, xend = mean + se), colour = "grey30", size = 1.5) +
  geom_point(data = mass.summary, aes(y = pop_num - 0.1, x = mean, fill = food, shape = food), size = 4, colour = "grey30", stroke = 1) +
  scale_fill_manual(values = c("#8DD3C7", "#FDB462", "#FCCDE5"),
                    labels = c("Mikania", "Leucas", "Coleus")) +
  scale_colour_manual(values = c("#8DD3C7", "#FDB462", "#FCCDE5"),
                      labels = c("Mikania", "Leucas", "Coleus")) +
  scale_shape_manual(values = c(21, 24, 25)) +
  #scale_x_continuous(breaks = seq(0, 40, by = 5), limits = c(14, 40), expand = c(0.01, 0.01)) +
  scale_linetype_manual(values = c("MINA" = "solid", "SIWA" = "longdash", "MASG" = "dotdash")) +
  xlab("\npupal mass (mg)") +
  ylab("") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(), 
        legend.text = element_text(face = "italic", size = 10),
        #strip.background = element_blank(),
        #panel.border = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.9, 0.2),
        legend.key.size = unit(0.5, "cm"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        axis.title.x = element_text(margin = margin(t = 10, b = 0)), 
        axis.title.y = element_text(margin = margin(r = 10, l = 0))) +
  guides(shape = FALSE)

plot.mass
ggsave(filename = "Plots/leu_mass.png", width = 10, height = 6, device = 'png', dpi = 1200)
ggsave(filename = "Plots/leu_mass.pdf", width = 10, height = 6, device = 'pdf', dpi = 1200)
ggsave(filename = "Plots/leu_mass.svg", width = 10, height = 6, device = 'svg', dpi = 1200)


data.stg <- read.csv("2024_EE_data_devstage.csv")

stg.summary <- data.stg %>%
  group_by(stg, pop, food) %>%
  summarise(
    mean = mean(rsurv, na.rm = TRUE),
    se = sd(rsurv, na.rm = TRUE) / sqrt(n()),
    total_surv = sum(surv, na.rm = TRUE),  # For dot sizes
    .groups = 'drop'
  )

stg.summary <- stg.summary %>%
  mutate(
    stg_numeric = case_when(
      stg == "1st" ~ 1,
      stg == "2nd" ~ 2,
      stg == "3rd" ~ 3,
      stg == "4th" ~ 4,
      stg == "pupa" ~ 5,
      TRUE ~ NA_real_ 
    ),
    stg_offset = stg_numeric + case_when(
      food == "mik" & pop == "MINA" ~ 0.3,
      food == "mik" & pop == "SIWA" ~ 0.4,
      food == "mik" & pop == "MASG" ~ 0.5,
      food == "leu" & pop == "MINA" ~ 0.0,
      food == "leu" & pop == "SIWA" ~ 0.1,
      food == "leu" & pop == "MASG" ~ 0.2,
      food == "col" & pop == "MINA" ~ -0.1,
      food == "col" & pop == "SIWA" ~ -0.2,
      food == "col" & pop == "MASG" ~ -0.3,
      TRUE ~ 0  # Default case, no offset
    )
  )

data.stg <- data.stg %>%
  mutate(
    stg_numeric = case_when(
      stg == "1st" ~ 1,
      stg == "2nd" ~ 2,
      stg == "3rd" ~ 3,
      stg == "4th" ~ 4,
      stg == "pupa" ~ 5,
      TRUE ~ NA_real_
    )
  )

data.stg$food <- factor(data.stg$food, levels = c("mik", "leu", "col"))
stg.summary$food <- factor(stg.summary$food, levels = c("mik", "leu", "col"))
data.stg$pop <- factor(data.stg$pop, levels = c("MINA", "SIWA", "MASG"))
stg.summary$pop <- factor(stg.summary$pop, levels = c("MINA", "SIWA", "MASG"))

plot.stg <- ggplot(stg.summary, aes(x = stg_offset, y = mean, group = interaction(pop, food))) +
  geom_point(data = data.stg, aes(x = stg_numeric, y = rsurv, colour = food, shape = food, fill = food), 
             size = 4, alpha = 0.5, position = position_jitter(width = 0.1, height = 0.05)) +
  geom_line(aes(linetype = pop, colour = food), size = 1) +
  geom_segment(aes(xend = stg_offset, y = mean - se, yend = mean + se, colour = food), size = 2) +
  geom_point(aes(x = stg_offset, y = mean, size = total_surv, fill = food, shape = food), 
             colour = "grey30", stroke = 1) +
  scale_fill_manual(values = c("#8DD3C7", "#FDB462", "#FCCDE5"),
                    labels = c("Mikania", "Leucas", "Coleus")) +
  scale_colour_manual(values = c("#8DD3C7", "#FDB462", "#FCCDE5"),
                      labels = c("Mikania", "Leucas", "Coleus")) +
  scale_shape_manual(values = c(21, 24, 25)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = c("0", "0.25", "0.5", "0.75", "1")) +
  scale_x_continuous(breaks = seq(1, 5, by = 1), labels = c("1st", "2nd", "3rd", "4th", "pupa")) +
  scale_linetype_manual(values = c("MINA" = "solid", "SIWA" = "longdash", "MASG" = "dotdash"),
                        labels = c("MINA[18, 18, 17]", "SIWA[22, 21, 22]", "MASG[30, 30, 30]")) +
  scale_size(range = c(1, 10)) +
  labs(
    x = "\ndevelopmental stage",
    y = "survival probability\n",
    color = "Food Type",
    size = "Total Survival",
    linetype = "Population"
  ) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(), 
        legend.text = element_text(face = "italic", size = 10),
        #strip.background = element_blank(),
        #panel.border = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right",
        legend.key.size = unit(0.5, "cm"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16)) +
  guides(shape = FALSE, fill = FALSE)

plot.stg
ggsave(filename = "Plots/leu_stg.png", width = 10, height = 9, device = 'png', dpi = 1200)
ggsave(filename = "Plots/leu_stg.pdf", width = 10, height = 9, device = 'pdf', dpi = 1200)
ggsave(filename = "Plots/leu_stg2.svg", width = 10, height = 9, device = 'svg', dpi = 1200)




## ANALYSIS - GLM EFFECT SIZES ####

# test whether degree of acceptance differ between population in 2021 despite accepting both host plants

glm.hac <- glmer(cbind(round(mik), round(leu)) ~ host + (1|id), family = binomial, data = data.hac[data.hac$mik + data.hac$leu > 0, ])
simulateResiduals(glm.hac, plot = TRUE)
summary(glm.hac)

glm.hac.null <- glmer(cbind(round(mik), round(leu)) ~ 1 + (1|id), family = binomial, data = data.hac[data.hac$mik + data.hac$leu > 0, ])
anova(glm.hac, glm.hac.null, test="LRT")

summary(emmeans(object = glm.hac,
                specs = pairwise~host,
                regrid = "response")$contrasts, infer=T)


# test whether degree of preference differ among populations in 2023 (Mikania vs Leucas)

glm.pref.ml <- glmer(cbind(round(mik), round(leu)) ~ loc + (1|id), family = binomial, data = data.pref[data.pref$mik + data.pref$leu > 0, ])
simulateResiduals(glm.pref.ml, plot = TRUE)
summary(glm.pref.ml)

glm.pref.ml.null <- glmer(cbind(round(mik), round(leu)) ~ 1 + (1|id), family = binomial, data = data.pref[data.pref$mik + data.pref$leu > 0, ])
anova(glm.pref.ml, glm.pref.ml.null, test="LRT")

summary(emmeans(object = glm.pref.ml,
                specs = ~loc,
                regrid = "response"))

summary(emmeans(object = glm.pref.ml,
                specs = pairwise~loc,
                regrid = "response")$contrasts, infer=T)


# test whether degree of preference differ among populations in 2023 (Mikania vs Coleus)

glm.pref.mc <- glmer(cbind(round(mik), round(col)) ~ loc + (1|id), family = binomial, data = data.pref[data.pref$mik + data.pref$col > 0, ])
simulateResiduals(glm.pref.mc, plot = TRUE)
summary(glm.pref.mc)

glm.pref.mc.null <- glmer(cbind(round(mik), round(col)) ~ 1 + (1|id), family = binomial, data = data.pref[data.pref$mik + data.pref$col > 0, ])
anova(glm.pref.mc, glm.pref.mc.null, test="LRT")

summary(emmeans(object = glm.pref.mc,
                specs = ~loc,
                regrid = "response"))

summary(emmeans(object = glm.pref.mc,
                specs = pairwise~loc,
                regrid = "response")$contrasts, infer=T)


# test whether degree of preference differ among populations in 2023 (Leucas vs Coleus)

glm.pref.lc <- glmer(cbind(round(leu), round(col)) ~ loc + (1|id), family = binomial, data = data.pref[data.pref$leu + data.pref$col > 0, ])
simulateResiduals(glm.pref.lc, plot = TRUE)
summary(glm.pref.lc)

glm.pref.lc.null <- glmer(cbind(round(leu), round(col)) ~ 1 + (1|id), family = binomial, data = data.pref[data.pref$leu + data.pref$col > 0, ])
anova(glm.pref.lc, glm.pref.lc.null, test="LRT")

summary(emmeans(object = glm.pref.lc,
                specs = ~loc,
                regrid = "response"))

summary(emmeans(object = glm.pref.lc,
                specs = pairwise~loc,
                regrid = "response")$contrasts, infer=T)


data.pref.eff1 <- as.data.frame(summary(emmeans(object = glm.pref.ml,
                                               specs = pairwise~loc,
                                               regrid = "response")$contrasts, infer=T))
data.pref.eff2 <- as.data.frame(summary(emmeans(object = glm.pref.mc,
                                                specs = pairwise~loc,
                                                regrid = "response")$contrasts, infer=T))
data.pref.eff3 <- as.data.frame(summary(emmeans(object = glm.pref.lc,
                                                specs = pairwise~loc,
                                                regrid = "response")$contrasts, infer=T))

data.pref.eff1$pair <- "ML"
data.pref.eff2$pair <- "MC"
data.pref.eff3$pair <- "LC"
data.pref.eff <- rbind(data.pref.eff1, data.pref.eff2, data.pref.eff3)
data.pref.eff$pair <- factor(data.pref.eff$pair, levels = c("ML", "MC", "LC"))
data.pref.eff$contrast <- factor(data.pref.eff$contrast, 
                                 levels = c("BATG - MASG", "BATG - MINA", "BATG - NUBO", "BATG - PETA", "BATG - PIND", "BATG - SIWA", "RANT - SIWA",
                                            "MINA - NUBO", "MINA - PIND", "MINA - RANT", "MINA - SIWA", 
                                            "PETA - PIND", "PETA - RANT", "PETA - SIWA",
                                            "MASG - MINA", "MASG - PETA", "MASG - RANT", 
                                            "NUBO - RANT", "NUBO - PETA", 
                                            "PIND - RANT", 
                                            "MINA - PETA", "BATG - RANT",
                                            "MASG - NUBO", "MASG - PIND", "MASG - SIWA", 
                                            "NUBO - PIND", "NUBO - SIWA", "PIND - SIWA"))


data.pref.eff <- data.pref.eff %>%
  mutate(
    con_numeric = case_when(
      contrast == "BATG - MASG" ~ 1,
      contrast == "BATG - MINA" ~ 2,
      contrast == "BATG - NUBO" ~ 3,
      contrast == "BATG - PETA" ~ 4,
      contrast == "BATG - PIND" ~ 5,
      contrast == "BATG - SIWA" ~ 6,
      contrast == "RANT - SIWA" ~ 7,
      contrast == "MINA - NUBO" ~ 8,
      contrast == "MINA - PIND" ~ 9,
      contrast == "MINA - RANT" ~ 10,
      contrast == "MINA - SIWA" ~ 11,
      contrast == "PETA - PIND" ~ 12,
      contrast == "PETA - RANT" ~ 13,
      contrast == "PETA - SIWA" ~ 14,
      contrast == "MASG - MINA" ~ 15,
      contrast == "MASG - PETA" ~ 16,
      contrast == "MASG - RANT" ~ 17,
      contrast == "NUBO - RANT" ~ 18,
      contrast == "NUBO - PETA" ~ 19,
      contrast == "PIND - RANT" ~ 20,
      contrast == "MINA - PETA" ~ 21,
      contrast == "BATG - RANT" ~ 22,
      contrast == "MASG - NUBO" ~ 23,
      contrast == "MASG - PIND" ~ 24,
      contrast == "MASG - SIWA" ~ 25,
      contrast == "NUBO - PIND" ~ 26,
      contrast == "NUBO - SIWA" ~ 27,
      contrast == "PIND - SIWA" ~ 28,
      TRUE ~ NA_real_ 
    ),
    pair_offset = con_numeric + case_when(
      pair == "MC" ~ 0.3,
      pair == "ML" ~ 0.0,
      pair == "LC" ~ -0.3,
      TRUE ~ 0  # Default case, no offset
    )
  )


pref.labs <- data.frame(
  label = c("LC", "ML", "MC"),
  x = c(-0.9, -0.9, -0.9), 
  y = c(14.7, 15, 15.3)
)

plot.pref.eff <- ggplot(data.pref.eff, aes(x = estimate, y = contrast)) +
  geom_segment(aes(y = pair_offset, yend = pair_offset, x = asymp.LCL, xend = asymp.UCL), colour = "grey30", size = 1) +
  #geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL), height = 0.5, color = "grey30", size = 0.75) +
  geom_point(aes(y = pair_offset, x = estimate, fill = p.value <= 0.05, shape = pair), colour = "grey30", size = 3, stroke = 1) +
  geom_vline(xintercept = 0, size = 0.2, linetype = "dashed") +
  geom_hline(yintercept = 22.5, size = 0.2) +
  geom_hline(yintercept = 20.5, size = 0.2) +
  geom_hline(yintercept = 14.5, size = 0.2) +
  geom_hline(yintercept = 7.5, size = 0.2) +
  scale_shape_manual(values = c(23, 21, 22)) +
  scale_fill_manual(values = c("TRUE" = "white", "FALSE" = "grey30")) +
  scale_x_continuous(breaks = seq(-1, 1, by = 0.5), labels = c(-1, -0.5, 0, 0.5, 1)) +
  scale_y_continuous(breaks = seq(1, 28, by = 1),
                     labels = c("BATG - MASG", "BATG - MINA", "BATG - NUBO", "BATG - PETA", "BATG - PIND", "BATG - SIWA", "RANT - SIWA",
                                "MINA - NUBO", "MINA - PIND", "MINA - RANT", "MINA - SIWA", 
                                "PETA - PIND", "PETA - RANT", "PETA - SIWA",
                                "MASG - MINA", "MASG - PETA", "MASG - RANT", 
                                "NUBO - RANT", "NUBO - PETA", 
                                "PIND - RANT", 
                                "MINA - PETA", "BATG - RANT",
                                "MASG - NUBO", "MASG - PIND", "MASG - SIWA", 
                                "NUBO - PIND", "NUBO - SIWA", "PIND - SIWA"), expand = c(0.01, 0.01)) +
  geom_text(data = pref.labs, aes(x = x, y = y, label = label), size = 3, 
            color = "grey30", parse = TRUE) +
  xlab("\nEffect size") +
  #facet_wrap(~pair) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(), 
        legend.text = element_text(face = "italic", size = 10),
        strip.background = element_blank(),
        #panel.border = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        legend.key.size = unit(0.5, "cm"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 16))

plot.pref.eff
ggsave(filename = "Plots/leu_pref_eff.png", width = 3, height = 10, device = 'png', dpi = 1200)
ggsave(filename = "Plots/leu_pref_eff.svg", width = 3, height = 10, device = 'svg')


plot.pref.eff <- ggplot(data.pref.eff, aes(x = estimate, y = contrast)) +
  geom_segment(aes(y = contrast, yend = pair_offset, x = asymp.LCL, xend = asymp.UCL), colour = "grey30", size = 1) +
  #geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL), height = 0.5, color = "grey30", size = 0.75) +
  geom_point(aes(y = contrast, x = estimate, fill = p.value <= 0.05, shape = pair), colour = "grey30", size = 3, stroke = 1) +
  geom_vline(xintercept = 0, size = 0.2, linetype = "dashed") +
  geom_hline(yintercept = 22.5, size = 0.2) +
  geom_hline(yintercept = 20.5, size = 0.2) +
  geom_hline(yintercept = 14.5, size = 0.2) +
  geom_hline(yintercept = 7.5, size = 0.2) +
  facet_wrap(~pair) +
  scale_shape_manual(values = c(23, 21, 22)) +
  scale_fill_manual(values = c("TRUE" = "white", "FALSE" = "grey30")) +
  scale_x_continuous(breaks = seq(-1, 1, by = 0.5), labels = c(-1, -0.5, 0, 0.5, 1)) +
  xlab("\neffect size") +
  #facet_wrap(~pair) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(), 
        legend.text = element_text(face = "italic", size = 10),
        strip.background = element_blank(),
        #panel.border = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        legend.key.size = unit(0.5, "cm"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 16))

plot.pref.eff
ggsave(filename = "Plots/leu_pref_eff2.png", width = 8, height = 10, device = 'png', dpi = 1200)
ggsave(filename = "Plots/leu_pref_eff2.svg", width = 8, height = 10, device = 'svg')
ggsave(filename = "Plots/leu_pref_eff2.pdf", width = 6, height = 10, device = 'pdf')


# test whether survival probability is different among populations and food which they're reared on

lrt.surv <- pairwise_survdiff(Surv(dur, stc) ~ pop + food, data = data.surv)
lrt.surv


# test whether developmental time until pupation is different among populations and food which they're reared on

glm.devtime <- glm(dur ~ pop * food, family = gaussian, data = data.surv)
simulateResiduals(glm.devtime, plot = TRUE)

glm.devtime.null <- glm(dur ~ 1, family = gaussian, data = data.surv)
anova(glm.devtime, glm.devtime.null, test="LRT")

summary(emmeans(object = glm.devtime,
                specs = ~pop*food,
                regrid = "response"))

summary(emmeans(object = glm.devtime,
                specs = pairwise~pop*food,
                regrid = "response")$contrasts, infer=T)


# test whether pupal mass is different among populations and food which they're reared on

glm.mass <- glm(mass ~ pop * food, family = gaussian, data = data.surv)
simulateResiduals(glm.mass, plot = TRUE)

glm.mass.null <- glm(mass ~ 1, family = gaussian, data = data.surv)
anova(glm.mass, glm.mass.null, test="LRT")

summary(emmeans(object = glm.mass,
                specs = ~pop*food,
                regrid = "response"))

summary(emmeans(object = glm.mass,
                specs = pairwise~pop*food,
                regrid = "response")$contrasts, infer=T)


data.dev.eff <- as.data.frame(summary(emmeans(object = glm.devtime,
                                              specs = pairwise~pop*food,
                                              regrid = "response")$contrasts, infer=T))

data.dev.eff <- data.dev.eff %>%
  separate(contrast, into = c("Group1", "Group2"), sep = " - ")

data.dev.eff <- data.dev.eff %>%
  separate(Group1, into = c("pop1", "food1"), sep = " ") %>%
  separate(Group2, into = c("pop2", "food2"), sep = " ")

data.dev.eff <- data.dev.eff %>% 
  filter(pop1 == pop2)

data.dev.eff <- data.dev.eff %>%
  mutate(pair = case_when(
    food1 == "mik" & food2 == "leu" ~ "ml",
    food1 == "leu" & food2 == "col" ~ "lc",
    food1 == "mik" & food2 == "col" ~ "mc"))

data.dev.eff$pop1 <- factor(data.dev.eff$pop1, levels = c("MINA", "SIWA", "MASG"))

data.dev.eff <- data.dev.eff %>%
  mutate(
    pop_numeric = case_when(
      pop1 == "MINA" ~ 1,
      pop1 == "SIWA" ~ 2,
      pop1 == "MASG" ~ 3,
      TRUE ~ NA_real_ 
    ),
    pair_offset = pop_numeric + case_when(
      pair == "mc" ~ 0.3,
      pair == "ml" ~ 0.0,
      pair == "lc" ~ -0.3,
      TRUE ~ 0  # Default case, no offset
    )
  )

dev.labs <- data.frame(
  label = c("LC", "ML", "MC"),
  x = c(-12.5, -12.5, -12.5), 
  y = c(1.7, 2, 2.3)
)

plot.dev.eff <- ggplot(data.dev.eff, aes(x = estimate, y = pop1)) +
  geom_segment(aes(y = pop_numeric + pair_offset, x = lower.CL, xend = upper.CL), colour = "grey30", size = 1) +
  #geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL, y = pop_numeric + pair_offset), height = 0.5, color = "black", size = 0.75) +
  geom_point(aes(y = pop_numeric + pair_offset, x = estimate, fill = p.value <= 0.05, shape = pair), colour = "grey30", size = 3, stroke = 1) +
  geom_vline(xintercept = 0, size = 0.2, linetype = "dashed") +
  scale_shape_manual(values = c(23, 21, 22)) +
  scale_fill_manual(values = c("TRUE" = "white", "FALSE" = "grey30")) +
  scale_x_continuous(breaks = seq(-10, 20, by = 10), labels = c(-10, 0, 10, 20)) +
  scale_y_continuous(breaks = seq(2, 6, by = 2), labels = c("MINA", "SIWA", "MASG")) +
  geom_text(data = dev.labs, aes(x = x, y = y, label = label), size = 3, 
            color = "grey30", parse = TRUE) +
  xlab("\neffect size") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(), 
        legend.text = element_text(face = "italic", size = 10),
        strip.background = element_blank(),
        #panel.border = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        legend.key.size = unit(0.5, "cm"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 16))

plot.dev.eff
ggsave(filename = "Plots/leu_devtime_eff.png", width = 2.5, height = 3, device = 'png', dpi = 1200)
ggsave(filename = "Plots/leu_devtime_eff.svg", width = 2.5, height = 3, device = 'svg', dpi = 1200)



data.mass.eff <- as.data.frame(summary(emmeans(object = glm.mass,
                                              specs = pairwise~pop*food,
                                              regrid = "response")$contrasts, infer=T))

data.mass.eff <- data.mass.eff %>%
  separate(contrast, into = c("Group1", "Group2"), sep = " - ")

data.mass.eff <- data.mass.eff %>%
  separate(Group1, into = c("pop1", "food1"), sep = " ") %>%
  separate(Group2, into = c("pop2", "food2"), sep = " ")

data.mass.eff <- data.mass.eff %>% 
  filter(pop1 == pop2)

data.mass.eff <- data.mass.eff %>%
  mutate(pair = case_when(
    food1 == "mik" & food2 == "leu" ~ "ml",
    food1 == "leu" & food2 == "col" ~ "lc",
    food1 == "mik" & food2 == "col" ~ "mc"))

data.mass.eff$pop1 <- factor(data.mass.eff$pop1, levels = c("MINA", "SIWA", "MASG"))

data.mass.eff <- data.mass.eff %>%
  mutate(
    pop_numeric = case_when(
      pop1 == "MINA" ~ 1,
      pop1 == "SIWA" ~ 2,
      pop1 == "MASG" ~ 3,
      TRUE ~ NA_real_ 
    ),
    pair_offset = pop_numeric + case_when(
      pair == "mc" ~ 0.3,
      pair == "ml" ~ 0.0,
      pair == "lc" ~ -0.3,
      TRUE ~ 0  # Default case, no offset
    )
  )

mass.labs <- data.frame(
  label = c("LC", "ML", "MC"),
  x = c(-0.01, -0.01, -0.01), 
  y = c(1.7, 2, 2.3)
)

plot.mass.eff <- ggplot(data.mass.eff, aes(x = estimate, y = pop1)) +
  geom_segment(aes(y = pop_numeric + pair_offset, x = lower.CL, xend = upper.CL), colour = "grey30", size = 1) +
  #geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL, y = pop_numeric + pair_offset), height = 0.5, color = "black", size = 0.75) +
  geom_point(aes(y = pop_numeric + pair_offset, x = estimate, fill = p.value <= 0.05, shape = pair), colour = "grey30", size = 3, stroke = 1) +
  geom_vline(xintercept = 0, size = 0.2, linetype = "dashed") +
  scale_shape_manual(values = c(23, 21, 22)) +
  scale_fill_manual(values = c("TRUE" = "white", "FALSE" = "grey30")) +
  scale_x_continuous(breaks = seq(-0.005, 0.01, by = 0.005), labels = c(-0.005, 0, 0.005, 0.01)) +
  scale_y_continuous(breaks = seq(2, 6, by = 2), labels = c("MINA", "SIWA", "MASG")) +
  geom_text(data = mass.labs, aes(x = x, y = y, label = label), size = 3, 
            color = "grey30", parse = TRUE) +
  xlab("\neffect size") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(), 
        legend.text = element_text(face = "italic", size = 10),
        strip.background = element_blank(),
        #panel.border = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        legend.key.size = unit(0.5, "cm"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 16))

plot.mass.eff
ggsave(filename = "Plots/leu_mass_eff.png", width = 2.5, height = 3, device = 'png', dpi = 1200)
ggsave(filename = "Plots/leu_mass_eff.svg", width = 2.5, height = 3, device = 'svg', dpi = 1200)



# test whether survivorship is different among populations and food which they're reared on

data.stg$fail <- data.stg$total-data.stg$surv
glm.stg <- glm(cbind(surv, fail) ~ pop * food * stg, family = quasibinomial, data = data.stg)
simulateResiduals(glm.stg, plot = TRUE)

glm.stg.null <- glm(cbind(surv, fail) ~ 1, family = quasibinomial, data = data.stg)
anova(glm.stg, glm.stg.null, test="LRT")

summary(emmeans(object = glm.stg,
                specs = ~pop*food*stg,
                regrid = "response"))

summary(emmeans(object = glm.stg,
                specs = pairwise~pop*food*stg,
                regrid = "response")$contrasts, infer=T)

data.stg.eff <- as.data.frame(summary(emmeans(object = glm.stg,
                                              specs = pairwise~pop*food*stg,
                                              regrid = "response")$contrasts, infer=T))

data.stg.eff <- data.stg.eff %>%
  separate(contrast, into = c("Group1", "Group2"), sep = " - ")

data.stg.eff <- data.stg.eff %>%
  separate(Group1, into = c("pop1", "food1", "stg1"), sep = " ") %>%
  separate(Group2, into = c("pop2", "food2", "stg2"), sep = " ")

data.stg.eff <- data.stg.eff %>% 
  filter(pop1 == pop2, stg1 == stg2)

data.stg.eff <- data.stg.eff %>%
  mutate(pair = case_when(
    food1 == "mik" & food2 == "leu" ~ "ml",
    food1 == "leu" & food2 == "col" ~ "lc",
    food1 == "mik" & food2 == "col" ~ "mc"))

data.stg.eff$pop1 <- factor(data.stg.eff$pop1, levels = c("MINA", "SIWA", "MASG"))

data.stg.eff <- data.stg.eff %>%
  mutate(
    pop_numeric = case_when(
      pop1 == "MINA" ~ 1,
      pop1 == "SIWA" ~ 2,
      pop1 == "MASG" ~ 3,
      TRUE ~ NA_real_ 
    ),
    pair_offset = pop_numeric + case_when(
      pair == "mc" ~ 0.3,
      pair == "ml" ~ 0.0,
      pair == "lc" ~ -0.3,
      TRUE ~ 0  # Default case, no offset
    )
  )

data.stg.eff <- data.stg.eff %>%
  mutate(
    stg_numeric = case_when(
      stg1 == "1st" ~ 1,
      stg1 == "2nd" ~ 2,
      stg1 == "3rd" ~ 3,
      stg1 == "4th" ~ 4,
      stg1 == "pupa" ~ 5,
      TRUE ~ NA_real_ 
    ),
    pair_offset = stg_numeric + case_when(
      pair == "mc" ~ 0.3,
      pair == "ml" ~ 0.0,
      pair == "lc" ~ -0.3,
      TRUE ~ 0  # Default case, no offset
    )
  )

stg.labs <- data.frame(
  label = c("LC", "ML", "MC"),
  x = c(-1, -1, -1), 
  y = c(1.7, 2, 2.3)
)

plot.stg.eff <- ggplot(data.stg.eff, aes(x = estimate, y = stg1)) +
  geom_segment(aes(y = stg_numeric + pair_offset, x = asymp.LCL, xend = asymp.UCL), colour = "grey30", size = 1) +
  #geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL, y = stg_numeric + pair_offset), height = 0.5, color = "black", size = 0.75) +
  geom_point(aes(y = stg_numeric + pair_offset, x = estimate, fill = p.value <= 0.05, shape = pair), colour = "grey30", size = 3, stroke = 1) +
  geom_vline(xintercept = 0, size = 0.2, linetype = "dashed") +
  scale_shape_manual(values = c(23, 21, 22)) +
  scale_fill_manual(values = c("TRUE" = "white", "FALSE" = "grey30")) +
  scale_x_continuous(breaks = seq(-1, 1, by = 0.5), labels = c(-1, -0.5, 0, 0.5, 1)) +
  scale_y_continuous(breaks = seq(2, 10, by = 2), labels = c("1st", "2nd", "3rd", "4th", "pupa")) +
  geom_text(data = stg.labs, aes(x = x, y = y, label = label), size = 3, 
            color = "grey30", parse = TRUE) +
  xlab("\neffect size") +
  facet_wrap(~pop1, ncol = 5) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(), 
        legend.text = element_text(face = "italic", size = 10),
        strip.background = element_blank(),
        #panel.border = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        legend.key.size = unit(0.5, "cm"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 16))

plot.stg.eff
ggsave(filename = "Plots/leu_stg_eff.png", width = 8, height = 4, device = 'png', dpi = 1200)
ggsave(filename = "Plots/leu_stg_eff.svg", width = 8, height = 4, device = 'svg', dpi = 1200)

## ANALYSIS - EXTRA ####

data.morph <- read.csv("2024_EE_data_morphology.csv")

glm.morph <- glm(ely_len_ely ~ host * sex, family = gaussian, data = data.morph)
simulateResiduals(glm.morph, plot = TRUE)

glm.morph.null <- glm(ely_len_ely ~ 1, family = gaussian, data = data.morph)
anova(glm.morph, glm.morph.null, test="LRT")

summary(emmeans(object = glm.morph,
                specs = pairwise~host*sex,
                regrid = "response")$contrasts, infer=T)



