library(HiveR)

makeHiveData <- function(data, groups=NULL){
  
  n_obs <- dim(data)[1];
  n_axis <- dim(data)[2];
  hive_data <- list()
  
  # Make the node dataframe
  # The +1 is because at the end we add in extra nodes
  # to extend the axis of the hive plot into the middle
  ids <- 1:((n_obs+1) * n_axis);
  lab <- sapply(ids,as.character);
  size <- rep(0.1,(n_obs+1) * n_axis);
  color <- rep("#000000",(n_obs+1) * n_axis);
  axis <- c();
  radius <- c();
  axis_counter <- 1
  for (name in names(data)){
    column <- data[[name]];
    if (is.factor(column)){
      column <- as.numeric(column);
    }
    else if (!is.numeric(column)){
      column <- as.numeric(factor(column));
    }
    if (min(column) == max(column)){
      column[] <- 1
    }
    else{
      # The data will live between 0.5 and 1.5 to keep it away from the center
      column <- (column - min(column))/(max(column) - min(column))+0.5;
    }
    radius <- c(radius,column);
    axis <- c(axis, rep(as.integer(axis_counter), n_obs));
    axis_counter <- axis_counter + 1;
  }
  axis <- c(axis, 1:n_axis)
  radius <- c(radius, rep(0,n_axis))
  nodes <- data.frame(ids,lab,axis,radius,size,color,stringsAsFactors=FALSE);
  
  
  # Make the edge dataframe
  if (is.null(groups)){
    groups <- rep(1,n_obs)
  }
  groups <- factor(as.numeric(factor(groups)));
  num_groups <- length(levels(groups));
  group_colors <- rainbow(num_groups);
  
  
  id1 <- 1:(n_obs * n_axis);
  # Since the IDs correspond to stacking columns on top of eachother, to match observations,
  # you increase by n_obs. The modular part wraps around so the last axis pairs with the first
  # and the -1,+1 is to deal with the fact that R likes to be different and indexes things from 1
  id2 <- as.integer((id1+n_obs-1)%%(n_obs*n_axis)+1);
  weight <- rep(2,n_obs*n_axis);
  color <- rep(sapply(groups,function(x) group_colors[x]), n_axis);
  edges <- data.frame(id1,id2,weight,color,stringsAsFactors=FALSE);
  
  
  # Some other stuff that's needed
  desc <- sprintf("%d axes -- %d nodes -- %d edges",n_axis,n_axis*n_obs,n_axis*n_obs);
  axis.cols <- rep("#000000",n_axis);
  
  # Putting it all together
  hive_data$nodes <- nodes;
  hive_data$edges <- edges;
  hive_data$desc <- desc;
  hive_data$axis.cols <- axis.cols;
  hive_data$type <- "2D";
  class(hive_data) <- "HivePlotData";
  
  return (hive_data)
}


plotHiveDF <- function(data, groups=NULL){
    hive_data <- makeHiveData(data, groups);
    plotHive(HPD = hive_data, ch = 0.1, bkgnd = 'grey', axLabs = names(data))
}