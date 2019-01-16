# preprocessing script.

##### Load Data

# Load csv data file, correct timestamp using lubridate, convert factors to char, delete unused variable and cache
gpu = read.csv(file = "data/gpu.csv")
gpu$timestamp = ymd_hms(gpu$timestamp)
gpu$hostname = as.character(gpu$hostname)
gpu$gpuUUID = NULL

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


### Tabulate function 
# (shows percentage)
tblFun <- function(x){
        tbl <- table(x)
        res <- cbind(tbl,round(prop.table(tbl)*100,2))
        colnames(res) <- c('Count','Percentage')
        res
}

cache("tblFun")

