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
        
length(unique(task_xy_red$taskId))


# Join task_xy_red to app_check joining on taskId
test = left_join(app_check, task_xy_red)

test = test %>%
        mutate(jobId = NULL) %>%
        arrange(taskId, timestamp)

test2 = left_join(gpu_r, test)
    
 




# Q1 Which tasks dominate runtimes?
# For each different taskId, calculate difference in time for each task (stop - start) and enter in new column

q1 = test %>%
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




