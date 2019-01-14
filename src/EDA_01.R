# EDA

gpu_id = data.frame(gpuSerial = sort(unique(gpu$gpuSerial)), id =  1:1024)
gpu_r = gpu
gpu_r = left_join(gpu_r, gpu_id)

hist(gpu_r$id)
summary(gpu_r$id)



t = gpu %>%
        filter(gpuSerial == 323217055910)








gpu_r$timestamp <- strptime(x = as.character(gpu_r$timestamp),
                                format = "%Y/%m/%d %H:%M")

gpu_r$timestamp = ymd_hms(as.character(gpu_r$timestamp))

max(app_check$timestamp) - min(app_check$timestamp)


gpu_t = gpu_r %>%
        filter(id == 500)

ggplot(gpu_t, aes(timestamp, powerDrawWatt)) + geom_line() 
ggplot(gpu_t, aes(timestamp, gpuTempC)) + geom_line() 
ggplot(gpu_t, aes(timestamp, gpuUtilPerc)) + geom_line() 
ggplot(gpu_t, aes(timestamp, gpuMemUtilPerc)) + geom_line() 


# GPU EDA - looks like GPU dataset is an automated output, taking readings at a specified time for all GPUs.

# HOSTNAMES: 1024
# TaskID: 65793 (level 12 @ 256*256; level 8 @ 16*16; level 4 @ 1*1)
# JobID: 3 (one for each level)

# GpuSerial: 1024
# GPuUUID: 1024


# Filter task_xy for only level 12 (because levels 4 and 8 are sparsley represented - i.e. 1 and 256 samples respectively)
# Drop unused columns for joining
task_xy_red = task_xy %>%
        # filter(level == 12) %>%
        mutate(jobId = NULL)
        

# Join task_xy_red to app_check joining on taskId
app_task = left_join(app_check, task_xy_red)

app_task = app_task %>%
        mutate(jobId = NULL) %>%
        arrange(taskId, timestamp)

test2 = left_join(gpu_r, test)
    
 




# Q1 Which tasks dominate runtimes?
# For each different taskId, calculate difference in time for each task (stop - start) and enter in new column

q1 = app_task %>%
        group_by(taskId, eventName) %>%
        summarise(duration = as.numeric(difftime(last(timestamp), first(timestamp), unit = 'sec'))) %>%
        group_by(eventName) %>%
        summarise(mean_dur = mean(duration))



# Q2 interplay between GPU Temp and Performance

q2 = gpu %>%
        select(5:8)

x = sample(1:dim(q2)[1],10000)
q2_samp = q2[x,]

plot(q2_samp$powerDrawWatt,q2_samp$gpuTempC)

plot(q2_samp$gpuUtilPerc,q2_samp$gpuTempC)


# Q3 Increased power draw and render time?
# Want to look at gpu stats for each render (right?)
# Look at start/stop times for each render and fit them into gpu stats based on timestamps

q3 = app_task %>%
        filter(eventName == 'Render') %>%
        group_by(taskId, eventName, x, y) %>%
        summarise(duration = as.numeric(difftime(last(timestamp), first(timestamp), unit = 'sec')))

gpu_q3 = gpu %>%
        arrange(hostname, timestamp)




# Compare resource usage by tile (Q3b)
# Filter and aggregate joined app_check/taskxy to display only relevant variables/tasks
comp_tile = app_task %>%
        # Remove non-render tasks and all non level 12 observations (because there are basically none compared to level 12)
        filter(eventName == 'Render', level == 12) %>%
        # Aggregate by taskId computing duration of task using difftime (for each taskId)
        group_by(taskId, eventName, x, y) %>%
        summarise(duration = as.numeric(difftime(last(timestamp), first(timestamp), unit = 'sec'))) %>%
        # Order df by row vectors to prepare for reordering tile durations by tile coordinates
        arrange(x,y)

# Drop unused variables (must be removed from dplyr pipe)
comp_tile = comp_tile[,-(1:2)]
# Split duration vector into 256 row vectors
x = split(comp_tileb$duration, ceiling(seq_along(comp_tile$duration)/256))

# Loop through list of row vectors, binding to empty df, then insert friendly column names
df = data.frame()
for (i in length(x):1){
        df = rbind(df, x[[i]])
}
names(df) = 1:256

# Create heatmap of tile render durations
heatmap(as.matrix(df), Rowv=NA, Colv=NA, labRow=NA, labCol = NA, xlab = "Relative Rendering Duration by Tile")

# Summary statistics for render durations
summary(comp_tile$duration)



# Compare performance by GPU (Q3b)
#Strip down gpu dataset to only hostnames and gpu serial
gpu_ser = gpu %>%
        distinct(hostname, gpuSerial, .keep_all = FALSE)

# Filter and aggregate joined app_check/taskxy to display only relevant variables/tasks
comp_gpu = app_task %>%
        # Remove non-render tasks and all non level 12 observations (because there are basically none compared to level 12)
        filter(eventName == 'Render', level == 12) %>%
        # Aggregate by taskId computing duration of task using difftime (for each taskId)
        group_by(hostname, taskId, eventName, x, y) %>%
        summarise(duration = as.numeric(difftime(last(timestamp), first(timestamp), unit = 'sec'))) %>%
        left_join(gpu_ser, by = 'hostname') %>%
        group_by(gpuSerial) %>%
        summarise(avg_dur = mean(duration))
        
# Plot gpus by mean duration
plot(comp_gpu$avg_dur)

# Seems to be a distinct clustering of mean rendering times into approximately 2 clusters
summary(comp_gpu$avg_dur)


        






