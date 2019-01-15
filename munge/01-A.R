# preprocessing script.

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



#### Load functions

#### Build Map
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

