# Heavy preprocessing scripts

##### Compute task runtimes for all tasks in app_task
# For each different taskId, calculate difference in time for each task (stop - start) and enter in new column
task_runtimes = app_task %>%
        # Remove all non-level 12
        filter(level == 12) %>%
        # Aggregate by task and event, taking difference between stop and start times as new column (duration)
        group_by(taskId, eventName) %>%
        summarise(duration = as.numeric(difftime(last(timestamp), first(timestamp), unit = 'sec')))

cache('task_runtimes')



##### Compute mean resource usage for each event under each task
# !!!! Do not run unless you have alot of time - takes around 3 hours on Core i5!!!

# Arrange datasets in preparation for loop function
gpu_perf = gpu %>%
        arrange(hostname, timestamp)

app_task_2 = app_task %>%
        arrange(x, y, hostname, taskId, eventName, timestamp)

# Designate empty dataframe
gpu_task = data.frame()
# Loop through app_task dataset computing mean resource usage for each event time window
for(i in seq(1,length(app_task$timestamp),2)){
        # Progress monitor
        print(i)
        # Filter and aggregate gpu dataset for each app_task event
        x = gpu_perf %>%
                # Filter on separate statements to increase alogrithm speed
                filter(hostname == app_task_2$hostname[i]) %>%
                filter(timestamp >= app_task_2$timestamp[i] & timestamp <= app_task_2$timestamp[i+1]) %>%
                group_by(hostname, gpuSerial) %>%
                summarise(watt = mean(powerDrawWatt), temp = mean(gpuTempC), cpu = mean(gpuUtilPerc), mem = mean(gpuMemUtilPerc)) %>%
                # Add event name and task id for current app_task event
                mutate(eventName = app_task_2$eventName[i], taskId = app_task_2$taskId[i])
        # Bind row to gpu_task df
        gpu_task = rbind(gpu_task, as.data.frame(x))
}

cache("gpu_task")


##### Join resource usage dataset to app_task
comp_tile = app_task %>%
        # Filter by selected event type and remove non-level 12 observations (because there are basically none compared to level 12)
        filter(level == 12) %>%
        # Aggregate by taskId computing duration of task using difftime (for each taskId)
        group_by(taskId, eventName, x, y) %>%
        summarise(duration = as.numeric(difftime(last(timestamp), first(timestamp), unit = 'sec'))) %>%
        # Order df by row vectors to prepare for reordering tile durations by tile coordinates
        arrange(x,y) %>%
        # Join gpu_task dataset (computed means of resource usage by taskId and event)
        left_join(gpu_task)

cache('comp_tile')





##### Compute mean resource usage for each event under each task
# !!!! Do not run unless you have alot of time - takes around 3 hours on Core i5!!!

# Arrange datasets in preparation for loop function
gpu_perf = gpu %>%
        arrange(hostname, timestamp) %>%
        filter(hostname == '04dc4e9647154250beeee51b866b071500000G')


app_task_2 = app_task %>%
        filter(level == 12, eventName == 'TotalRender',hostname == '04dc4e9647154250beeee51b866b071500000G') %>%
        arrange(x, y, hostname, taskId, eventName, timestamp)

# Designate empty dataframe
gpu_task2 = data.frame()
# Loop through app_task dataset computing mean resource usage for each event time window
for(i in seq(1,length(app_task_2$timestamp),2)){
        # Progress monitor
        print(i)
        # Filter and aggregate gpu dataset for each app_task event
        x = gpu_perf %>%
                # Filter on separate statements to increase alogrithm speed
                filter(hostname == app_task_2$hostname[i]) %>%
                filter(timestamp >= app_task_2$timestamp[i] & timestamp <= app_task_2$timestamp[i+1]) %>%
                # group_by(hostname, gpuSerial) %>%
                # summarise(watt = mean(powerDrawWatt), temp = mean(gpuTempC), cpu = mean(gpuUtilPerc), mem = mean(gpuMemUtilPerc)) %>%
                # Add event name and task id for current app_task event
                mutate(eventName = app_task_2$eventName[i], taskId = app_task_2$taskId[i])
        # Bind row to gpu_task df
        gpu_task2 = rbind(gpu_task2, as.data.frame(x))
}

gpu_task2 = gpu_task2 %>%
        arrange(timestamp)


