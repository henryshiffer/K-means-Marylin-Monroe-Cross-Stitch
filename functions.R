

##Henry Shiffer - 1003866881



#devtools::install_github("sharlagelfand/dmc")
library(dmc)
library(imager)
library(tidyverse)
library(tidymodels)
library(dplyr)
library(cowplot)

cluster_info <- process_image <- function(image_file_name = "AndyWarhol.jpg", k_list){
  ## process_image(image_file_name, k_list) takes an image file the user provides as a JPEG,
  ## and a user specified list of k values in a vector. process_image
  ## then computes a k-means clustering for each k in k_list and stores the
  ## important information extracted in a tibble for each k value. The 
  ## information in the output of this function is then used as the input
  ## for each other function. 
  ## 
  ##
  ## 
  ## Input:
  ##  -image_file_name: a PNG or JPEG
  ##  -k_list: vector full of cluster center k values
  ## Output:
  ##  -cluster_info: A tibble derived from k_means that has 
  ##                 a clustering computed for each value in k_list.
  ##                 The tibble has the k values in k_list, their
  ##                 associated RGB values, size, withinss, the cluster size,
  ##                 col, DMC_num, DMC_name, Hex, a tibble with the
  ##                 data with the glance data and a tibble with the 
  ##                 augmented data. 
  ##   
  ## 
  ## Example:library(tidyverse)
  ##         library(tidymodels)
  ##         library(dplyr)
  ##         library(dmc)
  ##         vec_list = c(4, 5, 6)
  ##         image_for_function = "AndyWarhol.jpg"
  ##         cluster_info = process_image(image_for_function, vec_list)
  ##         
  ##
  ## getting the data from the image file:
  mm_image = imager::load.image(image_file_name)
  mm_image_data <- as.data.frame(mm_image, wide = "c") %>% rename(R = c.1, G = c.2, B = c.3)
  
  ## creating a tibble of information for t:
  kclusts_info_tib <- tibble(k = k_list)
  for (cluster_val in k_list){
    kclusts_info_tib <- add_row(kclusts_info_tib, k =cluster_val)
    kclusts_info_tib <- unique(kclusts_info_tib)
  }
  
  ## computing the clusterings
  kclusts_info_tib <- add_column(kclusts_info_tib, 
                                 k_clustering = map(kclusts_info_tib$k,
                                                    ~kmeans(select(mm_image_data, -x, -y),
                                                            centers = .x, nstart = 20)))
  
  
  ## add the tidy data column
  kclusts_info_tib <- add_column(kclusts_info_tib,
                                 tidy_clusterings = map(kclusts_info_tib$k_clustering, tidy))
  
  
  ## add columns with DMC information to the data frame
  kclusts_info_tib$tidy_clusterings <- lapply(kclusts_info_tib$tidy_clusterings,
                                              transform,
                                              col = rgb(R,G,B))
  tided_clusters_with_colours <- kclusts_info_tib$tidy_clusterings
  tided_clusters_with_colours <- unnest(kclusts_info_tib,
                                        cols = tidy_clusterings)
  cluster_colours <- c(tided_clusters_with_colours$col)
  dmc_cluster_colours <- sapply(cluster_colours, dmc)
  dmc_list <- dmc_cluster_colours[c(1,2,3), c(1:sum(k_list))]
  dmc_data_frame <- data.frame(matrix(unlist(dmc_list),
                                      nrow= sum(k_list),
                                      byrow=T),
                               stringsAsFactors=FALSE)
  colnames(dmc_data_frame) <- c("DMC_num", "DMC_name", "Hex")
  final_clusters <- tided_clusters_with_colours %>% mutate(DMC_num = dmc_data_frame$DMC_num)
  final_clusters <- final_clusters %>% mutate(DMC_name = dmc_data_frame$DMC_name)
  final_clusters <- final_clusters %>% mutate(Hex = dmc_data_frame$Hex)
  tided_clusters_with_colours <- final_clusters
  
  ## add glanced data column
  tided_clusters_with_colours <- add_column(tided_clusters_with_colours,
                                            glanced = map(tided_clusters_with_colours$k_clustering,
                                                          glance))
  ## add augmented data column
  tided_clusters_with_colours <- add_column(tided_clusters_with_colours,
                                            augmented = map(tided_clusters_with_colours$k_clustering,
                                                            augment,
                                                            mm_image_data))
  
  
  return(tided_clusters_with_colours)
}

scree_plot <- function(cluster_info){
  ## scree_plot(cluster_info) takes the output from 
  ## process_image and plots the total within sum of squares
  ## of each value of k, by the corresponding k value in k_list.
  ## From this we are able to gage an appropriate k value to use,
  ## by looking for the k value where the line stops decreasing rapidly.
  ## 
  ##
  ## Input:
  ##  - cluster_info: the information that is the output of 
  ##                  process_image(image_file_name, k_list).
  ##  
  ## Output:
  ##  - a ggplot graph that has each k value in k_list on the
  ##    x-axis, and the total within sum of squares of each
  ##    k value on the y-axis.
  ##   
  ## 
  ## Example:library(tidyverse)
  ##         library(tidymodels)
  ##         library(dplyr)
  ##         cluster_info = process_image("AndyWarhol.jpg", c(1, 2, 3, 4, 5))
  ##         scree_plot(cluster_info)
  
  
  ## getting the data from the image file:
  cluster_info_data <- cluster_info
  
  ## unnesting the column that corresponds to glanced
  clusterings <- 
    cluster_info_data %>% 
    unnest(cols = c(glanced))
  
  ## create the ggplot with k on the x-axis
  ## and tot.withinss on the y-axis
  scree_plotted <- ggplot(clusterings, aes(k, tot.withinss)) + geom_line() + geom_point()
  return(scree_plotted)
}




change_resolution <- function(image_df, x_size)
{
  ## change_resolution(image_df, x_size) subsamples an image to produce
  ## a lower resolution image. Any non-coordinate columns in the data
  ## frame are summarized with their most common value in the larger
  ## grid cell.
  ##
  ## Input:
  ## - image_df: A data frame in wide format. The x-coordinate column MUST
  ##             be named 'x' and the y-coordinate column MUST be named 'y'.
  ##             Further columns have no naming restrictions.
  ## - x_size:   The number of cells in the x-direction. The number of cells
  ##             in the vertical direction will be computed to maintain the 
  ##             perspective. There is no guarantee that the exact number
  ##             of cells in the x-direction is x_size
  ##
  ## Output:
  ## - A data frame with the same column names as image_df, but with fewer 
  ##   entries that corresponds to the reduced resolution image.
  ##
  ## Example:
  ##   library(imager)
  ##   library(dplyr)
  ##   fpath <- system.file('extdata/Leonardo_Birds.jpg',package='imager') 
  ##   im <- load.image(fpath)
  ##   im_dat<- as.data.frame(im,wide = "c") %>% rename(R = c.1, G = c.2, B = c.3) %>%
  ##            select(x,y,R,G,B)
  ##   agg_image <- change_resolution(im_dat, 50)
  
  if(!require(sp)) {
    stop("The sp packages must be installed. Run install.packages(\"sp\") and then try again.")
  }
  if(!require(dplyr)) {
    stop("The dplyr packages must be installed. Run install.packages(\"dplyr\") and then try again.")
  }
  
  sp_dat <- image_df 
  gridded(sp_dat) = ~x+y
  
  persp = (gridparameters(sp_dat)$cells.dim[2]/gridparameters(sp_dat)$cells.dim[1])
  y_size = floor(x_size*persp)
  orig_x_size = gridparameters(sp_dat)$cells.dim[1]
  orig_y_size = gridparameters(sp_dat)$cells.dim[2]
  
  x_res = ceiling(orig_x_size/x_size)
  y_res = ceiling(orig_y_size/y_size)
  
  gt = GridTopology(c(0.5,0.5), c(x_res, y_res),
                    c(floor(orig_x_size/x_res), floor(orig_y_size/y_res)))
  SG = SpatialGrid(gt)
  agg = aggregate(sp_dat, SG, function(x) names(which.max(table(x)))[1] )
  agg@grid@cellsize <- c(1,1)
  df <- agg %>% as.data.frame %>% rename(x = s1, y = s2)  %>% select(colnames(image_df))
  
  return(df)
  
}



colour_strips <- function(cluster_info){
  ## colour_strips produces colour strips with the DMC
  ## colour that is closest to the cluster centre colour
  ## for each k value in k_list.
  ## 
  ## Input:
  ##  - cluster_info: the information that is the output of 
  ##                  process_image(image_file_name, k_list).
  ## Output:
  ## - A DMC colour strip for each value of k in k_list. For example 
  ##   a k_list of c(2, 3) would output two colour strips, the first
  ##   with 2 colours, and the second with 3 colours.
  ##
  ## Example: 
  ##         library(dmc)
  ##         library(tidyverse)
  ##         library(tidymodels)
  ##         library(dplyr)
  ##         library(cowplot)
  ##          x <- process_image("AndyWarhol.jpg", c(2, 3))
  ##          colour_strips(x)
  ##   
  ##  
  
  ## get data from cluster_info
  cluster_info_data <- cluster_info
  ## get the k values out for a loop
  k <- cluster_info_data$k
  k <- as.vector(unique(k))
  ## loop over the values of K and show the colour strip for each K
  for (cluster_val in k){
    k_clust <- subset(cluster_info_data, k %in% c(cluster_val))
    cluster_colour <- k_clust$Hex
    show_col(cluster_colour)
  }
}







make_pattern <- function(cluster_info, k, x_size, black_white = FALSE, background_colour = NULL){
  ## make_pattern(cluster_info, k, x_size, black_white, background_colour) 
  ## plots the cross stitch pattern. The user specifies a specific k value,
  ## size of the pattern, and whether the plot should include colour
  ## or be black and white. The user can also specify if the pattern should
  ## include background colour.
  ## 
  ##
  ## Input:
  ##  - cluster_info: the information that is the output of 
  ##                  process_image(image_file_name, k_list).
  ##  - k: The chosen cluster size by the user.
  ##  - x_size: The number of cells in the x-direction.
  ##            The (approximate) total number of possible stitches
  ##            in the horizontal direction.
  ##  - black_white: (logical) Print the pattern in black and
  ##    white (TRUE) or print the pattern colour by having the default (FALSE).
  ##  - background_colour: The colour of the background, which should not be stitched in the
  ##    pattern. (Default is to not have a colour).
  ##  
  ##  
  ## Output:
  ##  - A ggplot that has the plotted cross stitch pattern of the image,
  ##    with a legend that corresponds to each cluster symbol with the
  ##    associated cluster colour name. The plotted pattern image also 
  ##    includes a black grid ontop of it.
  ##   
  ## 
  ## Example:library(dmc)
  ##         library(tidyverse)
  ##         library(tidymodels)
  ##         library(dplyr)
  ##         library(cowplot)
  ##         cluster_info <- process_image("AndyWarhol.jpg", c(6))
  ##         make_pattern(cluster_info, 6, 75, TRUE)
  
  ## get the data frame with the cluster information at the specific k
  cluster_info_data <- cluster_info("AndyWarhol.jpg", k)
  cluster_data <- cluster_info_data$augmented
  cluster_info_dataframe <- as.data.frame(cluster_data)
  cluster_info_data1 <- select(cluster_info_dataframe, c(x,y,.cluster))
  
  
  ## apply the change_resolution function to change the resolution of the data
  changed_resolution_data <- change_resolution(cluster_info_data1, x_size = x_size)
  
  
  ## create a frame of colour information
  colour_frame <- tibble(cluster = c(1:k), name = c(cluster_info_data$DMC_name), 
                         col = c(cluster_info_data$Hex))
  ## create a theme for the plotted image which includes a grid line for the plot
  mytheme <- theme(panel.grid.major = element_line(colour="black", size = (1.5)),
                   panel.grid.minor = element_line(size = (0.2), colour="grey"),
                   panel.grid = element_line(color = "black"),
                   panel.ontop = TRUE, panel.background = element_rect(color = NA, fill = NA))
  
  ## create the plotted image with colour
  plotted = ggplot(changed_resolution_data, aes(x=x, y=y)) + mytheme +
    geom_point(aes(col = .cluster, shape = .cluster)) +
    scale_colour_manual(name = "DMC Colour", values = colour_frame %>% select(cluster, col) %>% deframe,
                        label =  colour_frame %>% select(cluster, name) %>% deframe) +
    scale_y_reverse() + mytheme
  
  ## create the plotted image in black and white
  if (black_white == TRUE){
    vec = rep("black", k)
    plotted = ggplot(changed_resolution_data, aes(x=x, y=y)) +
      geom_point(aes(col = .cluster, shape = .cluster)) +
      scale_colour_manual(name = "DMC Colour", values = vec,
                          label =  colour_frame %>% select(cluster, name) %>% deframe) +
      scale_y_reverse() + mytheme}
  
  
  
  
  return(plotted)
  
}