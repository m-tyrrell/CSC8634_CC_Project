##### DATA Description

# Check unique values for particular variables
length(unique(app_task$hostname))
length(unique(app_task$taskid))
length(unique(app_task$x))
length(unique(app_task$y))
length(unique(app_task$level))
length(unique(gpu$hostname))
length(unique(gpu$gpuSerial))

# HOSTNAMES: 1024
# TaskID: 65793 (level 12 @ 256*256; level 8 @ 16*16; level 4 @ 1*1)
# JobID: 3 (one for each level)
# GpuSerial: 1024
# GPuUUID: 1024

# Examine extra 247 observations in app_task (should = 65793x10)
hostname_task_n = app_task %>%
        group_by(hostname, taskId) %>%
        summarise(n = n())

hostname_task_n %>%
        filter(n == 20)

# Task scheduling (durations): Total Render = Saving Config + Render + Uploading (Does not include Tiling!)
f = 0
sum(task_runtimes[(f+1):(f+2),4]) + sum(task_runtimes[(f+5),4]) - sum(task_runtimes[(f+4),4])


##### EDA

# Looking at individual GPU performance - approximately 1500 observations per GPU in gpu dataset

# Create index column with accessible numbers for each gpu serial number
gpu_id = data.frame(gpuSerial = sort(unique(gpu$gpuSerial)), id =  1:1024)
# Create a copy of gpu dataset and insert gpu index vector
gpu_r = gpu
gpu_r = left_join(gpu_r, gpu_id)

# Filter gpu_r dataset by unique gpu index number for plotting and first 10 minutes
gpu_t = gpu_r %>%
        filter(id == 500) %>%
        arrange(timestamp) %>%
        filter(timestamp < min(timestamp)+600)

# Plot each metric in the subsetted gpu_r dataset
p1 = ggplot(gpu_t, aes(timestamp, powerDrawWatt)) + geom_line(color='#0066CC') + labs( x = 'timestamp (s)', y = 'Power (W)')
p2 = ggplot(gpu_t, aes(timestamp, gpuTempC)) + geom_line(color='#0066CC') + labs(x = 'timestamp (s)', y = 'Temperature (C)')
p3 = ggplot(gpu_t, aes(timestamp, gpuUtilPerc)) + geom_line(color='#0066CC') + labs(x = 'timestamp (s)', y = 'GPU Usage (%)')
p4 = ggplot(gpu_t, aes(timestamp, gpuMemUtilPerc)) + geom_line(color='#0066CC') + labs(x = 'timestamp (s)', y = 'Memory Usage (%)')
# Plot grid of 4
grid.arrange(p1, p2, p3, p4, ncol=2)

# Actual s/n of gpu used for plot
as.character(gpu_t$gpuSerial[1])
 

# Q2 interplay between GPU Performance metrics
q2 = gpu %>%
        select(4:7)

# Take sample of q2 index
x = sample(1:dim(q2)[1],1000)
# Slice q2 by sample
q2_samp = q2[x,]

p5 = ggplot(q2_samp, aes(powerDrawWatt, gpuTempC)) + geom_point(size=0.25) + stat_smooth() + labs( x = 'Power (W)', y = 'Temperature (C)')
p6 = ggplot(q2_samp, aes(gpuUtilPerc, gpuTempC)) + geom_point(size=0.25) + stat_smooth()+ labs( x = 'GPU Usage (%)', y = 'Temperature (C)')
p7 = ggplot(q2_samp, aes(gpuUtilPerc, powerDrawWatt)) + geom_point(size=0.25) + stat_smooth() + labs( x = 'GPU Usage (%)', y = 'Power (W)')
p8 = ggplot(q2_samp, aes(gpuUtilPerc, gpuMemUtilPerc)) + geom_point(size=0.25) + stat_smooth() + labs( x = 'GPU Usage (%)', y = 'Memory Usage (%)')
p9 = ggplot(q2_samp, aes(powerDrawWatt, gpuMemUtilPerc)) + geom_point(size=0.25) + stat_smooth() + labs( x = 'Power (W)', y = 'Memory Usage (%)')
p10 = ggplot(q2_samp, aes(gpuTempC, gpuMemUtilPerc)) + geom_point(size=0.25) + stat_smooth() + labs( x = 'Temperature (C)', y = 'Memory Usage (%)')

# Plot grid of 4
p11 = grid.arrange(p5, p6, p7, p8, p9, p10, ncol=3)
ggsave(filename="graphs/p11.png", plot=p11)



##### QUESTIONS

# Q1 Which tasks dominate runtimes?
task_runtime_means = task_runtimes %>%
        # Aggregate by event taking mean of all observations for each event
        group_by(eventName) %>%
        summarise(mean_dur = mean(duration), n = n())







##### Execution Time Plot - histograms showing distribution of execution times for each event type (Render & Tiling)

# Execution Time
plot_hist(task_runtimes,'duration','Render',lab = 'Execution Time (s)',n=0.5,c=1)
plot_hist(task_runtimes,'duration','Tiling',lab = 'Execution Time (s)',n=0.005,c=1)
plot_hist(task_runtimes,'duration','Uploading',lab = 'Execution Time (s)',colour='#0066CC',d=0.9,f=0.95,h=0.85)
plot_hist(task_runtimes,'duration','Saving Config',lab = 'Execution Time (s)',colour='#0066CC',n=0.0001,rnd=4,d=0.95,f=0.9,h=0.85)

# Power Consumption
plot_hist(gpu_task,'watt','Render',lab = 'Power Consumption (W)', n=0.5,c=1)
plot_hist(gpu_task,'watt','Tiling',lab = 'Power Consumption (W)', n=0.2,c=0,e=1)
plot_hist(gpu_task,'watt','Uploading',lab = 'Power Consumption (W)', n=0.2,c=1)
plot_hist(gpu_task,'watt','Saving Config',lab = 'Power Consumption (W)', n=0.2,c=0)

# Temperature
plot_hist(gpu_task,'temp','Render',lab = 'Temperature (C)', n=0.1,c=0,e=1)
plot_hist(gpu_task,'temp','Tiling',lab = 'Temperature (C)', n=0.1,c=0,e=1)
plot_hist(gpu_task,'temp','Uploading',lab = 'Temperature (C)', n=0.1,c=0,e=1)
plot_hist(gpu_task,'temp','Saving Config',lab = 'Temperature (C)', n=0.1,c=1,e=0)

# CPU
plot_hist(gpu_task,'cpu','Render',lab = 'CPU (%)', n=0.2,c=1,e=0)
#minimal data
plot_hist(gpu_task,'cpu','Tiling',lab = 'CPU (%)', n=0.005,c=0,e=1)
plot_hist(gpu_task,'cpu','Uploading',lab = 'CPU (%)', n=0.1,c=0,e=1)
plot_hist(gpu_task,'cpu','Saving Config',lab = 'CPU (%)', n=0.1,c=0,e=1)

# Memory
plot_hist(gpu_task,'mem','Render',lab = 'Memory (%)', n=0.2,c=1,e=0)
#minimal data
plot_hist(gpu_task,'mem','Tiling',lab = 'Memory (%)', n=0.005,c=0,e=1)
plot_hist(gpu_task,'mem','Uploading',lab = 'Memory (%)', n=0.1,c=0,e=1)
plot_hist(gpu_task,'mem','Saving Config',lab = 'Memory (%)', n=0.1,c=0,e=1)






# Test for normality
x = task_runtimes %>%
        filter(eventName == 'Render')

qqnorm(x$duration, pch = 1, frame = FALSE)
qqline(x$duration, col = "steelblue", lwd = 2)


# Compare resource usage by tile (Q3b)
# Filter and aggregate joined app_check/taskxy to display only relevant variables/tasks
heat_vis('Render','duration',caption='on')





# Compare performance by GPU (Q3b)
#Strip down gpu dataset to only hostnames and gpu serial
gpu_ser = gpu %>%
        distinct(hostname, gpuSerial, .keep_all = FALSE) %>%
        arrange(gpuSerial)

cache('gpu_ser')

# Filter and aggregate joined app_check/taskxy to display only relevant variables/tasks
comp_gpu = task_runtimes %>%
        # Remove non-render tasks 
        filter(eventName == 'Render') %>%
        # Join stripped down gpu dataset on hostname
        left_join(gpu_ser, by = 'hostname') %>%
        # Aggregate by mean render time
        group_by(gpuSerial) %>%
        summarise(avg_dur = mean(duration)) %>%
        # Add index column for plotting
        mutate(index = 1:1024)


# Plot render time by S/N        
ggplot(comp_gpu) + geom_point(aes(gpuSerial, avg_dur)) + labs(x = 'GPU S/N', y = 'Mean Render Time (s)') + 
        geom_hline(yintercept = mean(comp_gpu$avg_dur), color = 'red')
# Plot render time by S/N index to more easily show spread
ggplot(comp_gpu) + geom_point(aes(index, avg_dur)) + labs(x = 'GPU S/N index', y = 'Mean Render Time (s)') + 
        geom_hline(yintercept = mean(comp_gpu$avg_dur), color = 'red')

# Seems to be a distinct clustering of mean rendering times into approximately 2 clusters around the mean (41.3s)
summary(comp_gpu$avg_dur)




# Compare GPU clusters by tile
heat_vis('Render','watt',caption='on')


        





gpu_plot_agg = gpu_task %>%
        group_by(eventName) %>%
        summarise(watt = mean(watt), temp = mean(temp), cpu = mean(cpu), mem = mean(mem), n = n())


gpu_render = gpu_task %>%
        filter(eventName == 'Render') %>%
        group_by(gpuSerial, eventName) %>%
        summarise(watt = mean(watt), temp = mean(temp), cpu = mean(cpu), mem = mean(mem)) %>%
        arrange(gpuSerial)

# Render event: Performance plots by S/N
ggplot(gpu_render) + geom_point(aes(gpuSerial, watt)) + labs(x = 'GPU S/N', y = 'Power Consumption (s)')
ggplot(gpu_render) + geom_point(aes(gpuSerial, temp)) + labs(x = 'GPU S/N', y = 'Temperature (C)') 
ggplot(gpu_render) + geom_point(aes(gpuSerial, cpu)) + labs(x = 'GPU S/N', y = 'CPU (%)')
ggplot(gpu_render) + geom_point(aes(gpuSerial, mem)) + labs(x = 'GPU S/N', y = 'Memory (%)') 




        

     






