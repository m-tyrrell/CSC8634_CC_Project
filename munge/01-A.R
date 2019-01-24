# preprocessing script.

##### Load Data

# Load csv data file, correct timestamp using lubridate, convert factors to char, delete unused variable and cache
gpu = read.csv(file = "data/gpu.csv")
gpu$timestamp = ymd_hms(gpu$timestamp)
gpu$hostname = as.character(gpu$hostname)
gpu$gpuUUID = NULL

# Check for NAs
sum(is.na(gpu[,]))

cache("gpu")

# Load each csv data file, correct timestamp using lubridate, convert factors
app_check = read.csv(file = "data/application-checkpoints.csv", colClasses = c('character','character','character','character','character','character'))
app_check$timestamp = ymd_hms(app_check$timestamp)

task_xy = read.csv(file = "data/task-x-y.csv")
task_xy$taskId = as.character(task_xy$taskId)
task_xy$jobId = as.character(task_xy$jobId)

# Join datasets and cache
app_task = app_check %>%
        # Join task_xy to app_check on taskId
        left_join(task_xy) %>%
        # Drop unused variables
        mutate(jobId = NULL) %>%
        # Arrange by taskId and timestamp !! important for later processing !!
        arrange(taskId, timestamp)

# Check for NAs
sum(is.na(app_task[,]))

cache('app_task')



##### Load functions

### Build Map Function
# Build matrix map from input vectors - used to reconstruct x,y coordinates in image form for heatmaps
# Loop through list of row vectors, binding to empty df, then insert friendly column names
build_map = function(x,n){
        df = data.frame()
        for (i in length(x):1){
                df = rbind(df, x[[i]])
        }
        names(df) = 1:n
        return(as.matrix(df))
}

cache("build_map")


### Heat Visualisation Function
# Heatmap allowing comparison of resource usage by tile (ALL METRICS)

# Filter and aggregate joined app_check/taskxy to display only relevant variables/tasks
heat_vis = function(event, metric, outlier_var = 'duration', outlier_qty = 1, caption='on'){
        cap_label = paste(event,'',toTitleCase(metric),' by Tile')
        if(caption == 'off'){
                cap_label = ""
        }
        comp_tile = app_task %>%
                # Filter by selected event type and remove non-level 12 observations (because there are basically none compared to level 12)
                filter(eventName == event, level == 12) %>%
                # Aggregate by taskId computing duration of task using difftime (for each taskId)
                group_by(taskId, eventName, x, y) %>%
                summarise(duration = as.numeric(difftime(last(timestamp), first(timestamp), unit = 'sec'))) %>%
                # Order df by row vectors to prepare for reordering tile durations by tile coordinates
                arrange(x,y) %>%
                # Join gpu_task dataset (computed means of resource usage by taskId and event)
                left_join(gpu_task)
        
        # Specify conditional filter for heatmap (can't use dplyr pipe because of fun argument input issues (lazyeval possible solution))
        comp_tile$outlier = ifelse(comp_tile[[outlier_var]] > outlier_qty,1,1000)
        
        # Split duration vector into 256 row vectors
        x = split(comp_tile[[metric]], ceiling(seq_along(comp_tile[[metric]])/256))
        # Call mapping function   
        map_matrix = build_map(x,256)
        # Create heatmap of tile render durations
        heatmap(map_matrix, Rowv=NA, Colv=NA, labRow=NA, labCol = NA, xlab = cap_label)
        
}

cache("heat_vis")


##### Execution Time Plot - histograms showing distribution of execution times for each event type (Render & Tiling)

# Look at start/stop times for each render and fit them into gpu stats based on timestamps
plot_hist = function(df, var_col,event,title=event,lab=var_col, n=0.1, colour = 'white', rnd=2, a=0,b=1,c=0,d=1,e=0,f=1,g=0,h=1,i=1,j=1){
        # Filter as per selected event
        x = df %>%
                filter(eventName == event)
        # Setup histogram using ggplot
        comp_hist_rend = ggplot(x, aes_string(var_col)) + 
                geom_histogram(binwidth = n, fill=I("#0066CC"), col=I(colour), alpha=I(0.8))
        # Get ymax of count to orientate annotations
        ymax = max(ggplot_build(comp_hist_rend)$data[[1]]$count)
        # Add vlines and annotations
        comp_hist_rend +  geom_vline(xintercept = min(x[[var_col]]), color = 'darkgreen', linetype = 'dotdash') +
                annotate("text", x=min(x[[var_col]]), y=ymax*b, label= paste("Min",round(min(x[[var_col]]),rnd)), size=2.5, hjust=a) +
                # mean
                geom_vline(xintercept = mean(x[[var_col]]), color = 'red', linetype = 'dotdash') +
                annotate("text", x=mean(x[[var_col]]), y=ymax*d, label= paste("Arth. Mean",round(mean(x[[var_col]]),rnd)), size=2.5, hjust=c) +
                # median
                geom_vline(xintercept = median(x[[var_col]]), color = 'purple', linetype = 'dotdash') +
                annotate("text", x=median(x[[var_col]]), y=ymax*f, label= paste("Median",round(median(x[[var_col]]),rnd)), size=2.5, hjust=e) +
                # 95th percentile
                geom_vline(xintercept = quantile(x[[var_col]], probs = seq(0, 1, 0.05), type = 6)[20], color = 'orange',linetype = 'dashed') + 
                annotate("text", x=quantile(x[[var_col]], probs = seq(0, 1, 0.05), type = 6)[20], y=ymax*h, label= paste("95th Percentile",round(quantile(x[[var_col]], probs = seq(0, 1, 0.05), type = 6)[20],rnd)), size=2.5, hjust=g) +
                # max
                geom_vline(xintercept = max(x[[var_col]]), color = 'red', linetype = 'dotdash') +
                annotate("text", x=max(x[[var_col]]), y=ymax*j, label= paste("Max",round(max(x[[var_col]]),rnd)), size=2.5, hjust=i) +
                # Labels
                labs(title = title, x = lab, y = "Frequency (n)")
        # length(x[[var_col]])
}
cache("plot_hist")


### Tabulate function 
# (shows percentage)
tblFun <- function(x){
        tbl <- table(x)
        res <- cbind(tbl,round(prop.table(tbl)*100,2))
        colnames(res) <- c('Count','Percentage')
        res
}

cache("tblFun")

