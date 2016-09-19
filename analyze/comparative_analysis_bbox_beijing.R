library(ggplot2)
library(plyr)
library(gridExtra)
library(RColorBrewer)
library(reshape2)
library(scales)

##### Loading and Preparing the Data #####

results <- read.csv("~/path/to/csv.csv",
                    header = TRUE, stringsAsFactors = FALSE)

 # remove any COUNT queries
remove <- subset(results, grepl("-COUNT", results$queryName))
results <- results[!(results$queryName %in% remove$queryName), ]

 # because of the wide range of the number of resutls, only queries with 5000 or
 # less will be looked at for right now
results <- results[results$result <= 5000, ]

maxPlus <- max(results$result) + 200

results$bin <- cut(results$result, breaks = seq(0, maxPlus, by = 200),
                   include.lowest = TRUE, dig.lab = 10)

results$timeToFirstResult <- results$timeAtFirstResult - results$startTime


##### Loose #####

# Format data that compares GM, GM-LOOSE, and GW so that it can be plotted

format_loose <- function(x) {
  
  loose_queries <- subset(x, grepl("-LOOSE", x$queryName))
  names <- loose_queries$queryName
  sub <- lapply(names, function(y) substr(y, 0, nchar(y) - 6))
  loose <- x[x$queryName %in% names | x$queryName %in% sub, ]
  
  loose <- within(loose, system[system == 'GM'
                                & grepl("-LOOSE", queryName)] <- "GM-Loose")
  
  index <- loose$system == "GM-Loose"
  loose$queryName[index] <- substr(loose$queryName[index], 0,
                                   nchar(loose$queryName[index]) - 6)
  
  loose$queryName <- gsub("-LOOSE", "", loose$queryName)
  loose
}


##### Beijing #####

 # Format data specifically for the Beijing queries

beijing <- subset(results, grepl("BEIJING", results$queryName))
beijing$bbox_area <- substr(beijing$queryName, 32, 33)
index <- beijing$bbox_area == "4-" | beijing$bbox_area == "8-"
beijing$bbox_area[index] <- substr(beijing$bbox_area[index], 0, 1)
beijing <- format_loose(beijing)
beijing_list <- split(beijing, beijing$bbox_area)


##### Making the Graoh #####

 # removes the outliers from a dataset
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


# creates and saves the facet graphs

make_graph_bbox <- function(i, myPath, outliers) {
  
  if (outliers) {
    name <- paste("Total Duration Time for BBOX of area,",
                  i$bbox_area, "\nin Beijing With Outliers")
    out <- paste0(i$bbox_area, "_with_outliers")
  } else {
    
    i$duration <- remove_outliers(i$duration)
    i <- i[complete.cases(i), ]
    name <- paste("Total Duration Time for BBOX of area,",
                  i$bbox_area, "\nin Beijing Without Outliers")
    out <- paste0(i$bbox_area, "_without_outliers")
  }

  duration <- ggplot(i, aes(x=system, y=duration))+
    facet_wrap(~bin, ncol=5, nrow=5, scales = "fixed")+
    geom_violin(fill = "grey80", colour = "#3366FF", scale = "width")+
    geom_jitter(width = 0.25, height = 0.25, alpha = 0.5)+
    labs(x = "System", y = "Duration (miliseconds", title = name)+
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
  
  ggsave(out, width = 20, height = 20, device = "png", path = myPath)
}

# creates and saves density graphs

density_graph <- function(i, myPath, outliers) {
  
  if (outliers) {
    name <- paste("Total Duration Time for BBOX of area,",
                  i$bbox_area, "\nin Beijing With Outliers")
    out <- paste0(i$bbox_area, "_with_outliers")
  } else {
    
    i$duration <- remove_outliers(i$duration)
    i <- i[complete.cases(i), ]
    name <- paste("Total Duration Time for BBOX of area,",
                  i$bbox_area, "\nin Beijing Without Outliers")
    out <- paste0(i$bbox_area, "_without_outliers")
  }
  duration  <- ggplot(i, aes(duration, colour=system, fill=system))+
    facet_wrap(~bin, ncol=5, nrow=5, scales = "fixed")+
    geom_density(alpha=0.55)+
    labs(x = "System", y = "Duration (miliseconds", title = name)+
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
  
  ggsave(i$uuid, width = 20, height = 20, device = "png", path = myPath)
}

 # creates a graph for a single test

create_graph <- function(i, myPath, loose) {
  duration_medians <- aggregate(i$duration, list(i$system), median)
  dm <- duration_medians[[2]]
  first_medians <- aggregate(i$timeToFirstResult, list(i$system), median)
  fm <- first_medians[[2]]
  
  if (loose) {
    annotation <- annotate("text", x = c("GM", "GM-Loose", "GW"), y = dm, label = dm)
  } else {
    annotation <- annotate("text", x = c("GM", "GW"), y = dm, label = dm) 
  }
  
  duration <- ggplot(i, aes(x=system, y=duration))+
    geom_violin(fill="darkseagreen4")+
    coord_flip()+
    labs(x="System", y="Duration (miliseconds)",
         title=paste("Comparative Analysis Results of Geowave and \n Geomesa in terms \
       of Duration in Miliseconds In the Query\n", i$queryName))+
    theme(plot.title = element_text(size = 10, face="bold", vjust=2))+
    annotation
  
  first <- ggplot(i, aes(x=system, y=timeToFirstResult))+
    geom_violin(fill="darkseagreen4")+
    coord_flip()+
    labs(x="System", y="Time to First Result (miliseconds)",
         title=paste("Comparative Analysis Results of Geowave and \n Geomesa in terms \
       of the Time to First Result in Miliseconds In the Query,\n", i$queryName))+
    theme(plot.title = element_text(size = 10, face="bold", vjust=2))+
    annotation
  
  g <- grid.arrange(duration, first, ncol=1)
  ggsave(i$uuid, g, device = "png", path = myPath)
}


##### Saving the Graphs #####

 # saves two sets of graphs as pngs to a given directory.
 # one set contains no outliers while the other one does

lapply(names(beijing_list), function(x) make_graph_bbox(beijing_list[[x]],
                                                   "~/path/to/graph", TRUE))
lapply(names(beijing_list), function(x) make_graph_bbox(beijing_list[[x]],
                                                   "~/path/to/graph", FALSE))
