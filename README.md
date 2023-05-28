Daily Activity Pattern Analysis
================
Nicolás Caruso
May 2023

# Introduction

Welcome to the Daily Activity Pattern Analysis Notebook! In this
notebook, we will explore the fascinating world of animal behavior by
analyzing the daily activity patterns of multiple species and examining
the level of overlap among them. Understanding these patterns provides
valuable insights into ecological roles, resource utilization, and
species interactions within ecosystems. Through statistical analyses and
data visualization, we aim to identify peak activity times, assess
temporal overlap, and help to understand the factors influencing
variations in activity patterns.

By studying the daily activity patterns of different species, we gain a
deeper understanding of their adaptations, survival strategies, and the
broader functioning of ecosystems. Animals exhibit a diverse range of
activity patterns influenced by evolutionary history, environmental
conditions, and interactions with other organisms. Understanding them we
can unlock essential insights into species interactions and their impact
on community structure and ecosystem dynamics.

Throughout this notebook, we provide step-by-step instructions, code
snippets, and data visualizations to facilitate your analysis. By
actively engaging with the data and interpreting the results, you can
uncover the intricate relationships embedded within daily activity
patterns. The analysis of these patterns serves as a powerful tool for
understanding animal behavior and gaining a deeper appreciation for the
complexities of our natural world.

# Code

### Packages needed

R is a programming language specifically designed for statistical
analysis. Like many other programming languages, it provides libraries
(also known as packages) that enable various types of analyses. It is
customary to list all the required packages at the beginning of the
analysis. To use a package, you need to install it first using the
`install.packages()` command, and then load it using the `library()`
function.

``` r
library(circular)
library(overlap)
library(readxl)
library(lubridate)
library(png)
library(grid)
library(cowplot)
library(ggplot2)
library(dplyr)
```

### getting data

The `setwd()` command sets the working directory, which allows you to
directly read all the files in that directory without having to type the
entire path again. Then, with the read.csv() command, we load the file
containing the data. In essence, each row in the file represents a
photograph, while the columns contain information about the species and
the corresponding datetime of the picture.

``` r
# set the work directory
setwd("/home/nicolas/Documentos/PATRONES_ACTIVIDAD")

# load data
total<-read.csv("registros2.csv")
```

### Processing the data

Following the recommendation of the package authors and in order to
represent time as a circular variable, we will convert the datetime
values into radians (an into decimal first) for further analysis.

``` r
# This function splits the time column into hours, minutes, and seconds, and then applies the formula to convert it into decimal hours.

total$decimal <- sapply(strsplit(total$Time,":"), function(x){
  x <- as.numeric(x)
  x[1]+x[2]/60+ x[3]/3600
}
)

# Convert into radians
#total$timeRad<-total$decimal*2*pi

total$timeRad<-total$decimal/24*2*pi
```

### Rayleigh test

The Rayleigh test is a statistical test used to determine if there is a
significant circular (periodic) pattern in a set of angular data. It
assesses whether the observed angles are uniformly distributed or
exhibit a preferred direction. In the context of analyzing daily
activity patterns, the Rayleigh test can be applied to examine if there
is a significant preference for specific times of the day in the
behavior of the studied species. If the test indicates a significant
result, it suggests the presence of a non-random pattern in the activity
behavior, indicating a preferred time or direction of activity.

``` r
# select the species
sp <- total %>%
  filter(Species== "Imaginosa nicolensis") 

#test Rayleigh
rayleigh.test(sp$timeRad)
```

    ## 
    ##        Rayleigh Test of Uniformity 
    ##        General Unimodal Alternative 
    ## 
    ## Test Statistic:  0.6904 
    ## P-value:  0

### Plotting daily activity patterns using Kernel

Kernel density estimation is a powerful technique that can be used to
analyze daily activity patterns in a comprehensive and informative
manner. By applying kernel density estimation to temporal data, such as
the timing of animal behaviors throughout the day, we can obtain a
smooth and continuous representation of the activity distribution. This
estimation method allows us to identify the peaks and troughs of
activity, revealing the preferred time periods and intensity of certain
behaviors. By visualizing the kernel density estimate, we can gain
insights into the temporal patterns, detect bimodal or multimodal
activity distributions, and observe potential variations across
different species. Additionally, kernel density estimation enables the
quantification of overlap between activity patterns, allowing for a
deeper understanding of potential interactions and resource utilization
among species.

``` r
#plot
sp_name='Conepatus leuconotus'

densityPlot(sp$timeRad,rug=T, main=sp_name, adjust=2, xlab="Time", 
            ylab="Density", xcenter = 'midnight',extend = F)
```

![](ActivityPatterns_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### A much nicer plot

In order to explore and visualize the daily activity patterns we can
also utilize a polar plot, which is a specialized variation of a
histogram specifically designed for directional data. While traditional
histograms are suitable for representing frequencies in numerical data,
a polar plot is particularly well-suited for analyzing circular or
angular data, such as time or direction. By applying a polar plot to our
analysis of daily activity patterns, we can effectively depict the
distribution and concentration of activity across the 24-hour day. The
circular nature of the plot allows us to visualize the cyclic nature of
behavior, identifying peaks and troughs that correspond to different
times of the day. This representation provides a clear and intuitive
understanding of the dominant activity periods and the intensity of
activity during specific time intervals.

``` r
# plot
plot<-ggplot(sp, aes(x = decimal))+ 
      geom_histogram(breaks = seq(0, 24), 
                     fill="steelblue4", 
                     colour = "black", linewidth=0.3)+
      scale_x_continuous("", limits = c(0, 24), 
                         breaks = seq(0, 24), 
                         labels = seq(0, 24))+
      
      # here you define the night hours
      annotate("rect", 
               xmin = c(18,0), xmax = c(24, 6.5),  
               ymin = 0, ymax = 13, 
               alpha=0.3, 
               fill="grey25")+ 
      
      # here you define the day haurs
      annotate("rect",
               xmin=6, xmax = 19, 
               ymin = 0, ymax = 13, 
               alpha=0.3, fill="#FFD819")+
      
      # title and axes labels
      labs(title="Conepatus leuconotus",
            y= "Number of records")+
      
      # convert to circular plot
      coord_polar(start = 0)  +
      
      # background color
      theme(panel.background = element_rect(fill = 'grey95'))


# sun and moon images
sun <- readPNG("sun.png")
moon <- readPNG("moon.png")
sun<- rasterGrob(sun, interpolate = T)
moon<- rasterGrob(moon, interpolate = T)

# add the images
plot<-ggdraw(plot)+
      draw_grob(sun,0.33, 0.43, 0.08, 0.07)+
      draw_grob(moon,0.67, 0.55, 0.08, 0.07, scale = 0.75)

# print plot
print(plot)
```

![](ActivityPatterns_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Temporal overlap between two species

We will now calculate density curves for two species and plot them
overlaid. Additionally, the “Overlap” package allows us to calculate an
overlap index and its confidence interval.

``` r
sp1 <- total %>%
  filter(Species== "Imaginosa nicolensis") 

sp2 <- total %>%
  filter(Species== "Pyronicolus imaginarius") 

# overlap indexes (which one to use depends on the sampling size. See Ridout & Linkie 2009)
over<-overlapEst(sp1$timeRad, sp2$timeRad)

# Bootstrap confidence intervals
sp1.boot<-resample(sp1$timeRad,1000)
sp2.boot<-resample(sp2$timeRad,1000)
over_CI<-bootEst(sp1.boot,sp2.boot,adjust=c(1,NA,NA))
CI<-bootCI(over[1],over_CI[,1])

# plot
overlapPlot(sp1$timeRad, 
            sp2$timeRad,
            main=expression(""), 
            xlab="Time", 
            ylab="Density",
            ylim=c(0,0.13), 
            linecol=c("black","black"), 
            olapcol="black", 
            frame.plot=T, 
            adjust = 2,
            xcenter = 'midnight')

#legend
legend("topleft", 
       c(expression(italic("Sp1")),
         expression(italic("Sp2"))),
       lty=c(1,2),
       col=c(1,1),
       bty="n") 

#delta
mtext(bquote(hat(Delta)[1] == .(round(over[1],3))), 
      side = 3, 
      line = -5,
      at = -10.5)

#confidence interval
mtext(bquote('CI' == .(round(CI[1,1],3)) - .(round(CI[1,2],3))),
      side = 3,
      line=-6,
      at=-9.3)
```

![](ActivityPatterns_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
