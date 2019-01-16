##### DATA Description

# GPU EDA - looks like GPU dataset is an automated output, taking readings at a specified time for all GPUs.

# HOSTNAMES: 1024
# TaskID: 65793 (level 12 @ 256*256; level 8 @ 16*16; level 4 @ 1*1)
# JobID: 3 (one for each level)

# GpuSerial: 1024
# GPuUUID: 1024



##### EDA

# Looking at individual GPU performance
gpu_id = data.frame(gpuSerial = sort(unique(gpu$gpuSerial)), id =  1:1024)
gpu_r = gpu
gpu_r = left_join(gpu_r, gpu_id)

t = gpu %>%
        filter(gpuSerial == 323217055910)

gpu_t = gpu_r %>%
        filter(id == 500)

ggplot(gpu_t, aes(timestamp, powerDrawWatt)) + geom_line() 
ggplot(gpu_t, aes(timestamp, gpuTempC)) + geom_line() 
ggplot(gpu_t, aes(timestamp, gpuUtilPerc)) + geom_line() 
ggplot(gpu_t, aes(timestamp, gpuMemUtilPerc)) + geom_line() 

 
##### QUESTIONS

# Q1 Which tasks dominate runtimes?
task_runtime_means = task_runtimes %>%
        # Aggregate by event taking mean of all observations for each event
        group_by(eventName) %>%
        summarise(mean_dur = mean(duration), n = n())


# Q2 interplay between GPU Temp and Performance

q2 = gpu %>%
        select(4:7)

# Take sample of q2 index
x = sample(1:dim(q2)[1],1000)
# Slice q2 by sample
q2_samp = q2[x,]

pairs(q2)


##### Execution Times - histograms showing distribution of execution times for each event type (Render & Tiling)

# Look at start/stop times for each render and fit them into gpu stats based on timestamps
plot_hist = function(event, n=0.1, colour = 'white', rnd=2, a=0,b=1,c=0,d=1,e=0,f=1,g=0,h=1,i=1,j=1){
        # Filter as per selected event
        x = task_runtimes %>%
                filter(eventName == event)
        # Setup histogram using ggplot
        comp_hist_rend = ggplot(x, aes(duration)) + 
                geom_histogram(binwidth = n, fill=I("#0066CC"), col=I(colour), alpha=I(0.8))
        # Get ymax of count to orientate annotations
        ymax = max(ggplot_build(comp_hist_rend)$data[[1]]$count)
        # Add vlines and annotations
        comp_hist_rend +  geom_vline(xintercept = min(x$duration), color = 'darkgreen', linetype = 'dotdash') +
                annotate("text", x=min(x$duration), y=ymax*b, label= paste("Min",round(min(x$duration),rnd)), size=2.5, hjust=a) +
                # mean
                geom_vline(xintercept = mean(x$duration), color = 'red', linetype = 'dotdash') +
                annotate("text", x=mean(x$duration), y=ymax*d, label= paste("Arth. Mean",round(mean(x$duration),rnd)), size=2.5, hjust=c) +
                # median
                geom_vline(xintercept = median(x$duration), color = 'purple', linetype = 'dotdash') +
                annotate("text", x=median(x$duration), y=ymax*f, label= paste("Median",round(median(x$duration),rnd)), size=2.5, hjust=e) +
                # 95th percentile
                geom_vline(xintercept = quantile(x$duration, probs = seq(0, 1, 0.05), type = 6)[20], color = 'orange',linetype = 'dashed') + 
                annotate("text", x=quantile(x$duration, probs = seq(0, 1, 0.05), type = 6)[20], y=ymax*h, label= paste("95th Percentile",round(quantile(x$duration, probs = seq(0, 1, 0.05), type = 6)[20],rnd)), size=2.5, hjust=g) +
                # max
                geom_vline(xintercept = max(x$duration), color = 'red', linetype = 'dotdash') +
                annotate("text", x=max(x$duration), y=ymax*j, label= paste("Max",round(max(x$duration),rnd)), size=2.5, hjust=i) +
                # Labels
                labs(x = "Execution Time (s)", y = "Frequency")
                
}
cache("plot_hist")

# Execution time plots
plot_hist('Saving Config',colour='#0066CC',n=0.0001,rnd=4,d=0.95,f=0.9,h=0.85)
plot_hist('Tiling', n=0.005,c=1)
plot_hist('Render', n=0.5,c=1)
plot_hist('Uploading',colour='#0066CC',d=0.9,f=0.95,h=0.85)

# ggsave(file.path('graphs', 'Execution_Uploading.pdf'))



# Test for normality
x = task_runtimes %>%
        filter(eventName == 'Tiling')

qqnorm(x$duration, pch = 1, frame = FALSE)
qqline(x$duration, col = "steelblue", lwd = 2)


# Compare resource usage by tile (Q3b)
# Filter and aggregate joined app_check/taskxy to display only relevant variables/tasks


# Summary statistics for render durations
summary(comp_tile$duration)



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
ggplot(comp_gpu) + geom_point(aes(gpuSerial, avg_dur)) + labs(x = 'GPU S/N', y = 'Mean Render Time (s)') + geom_hline(yintercept = mean(comp_gpu$avg_dur), color = 'red')
# Plot render time by S/N index to more easily show spread
ggplot(comp_gpu) + geom_point(aes(index, avg_dur)) + labs(x = 'GPU S/N index', y = 'Mean Render Time (s)') + geom_hline(yintercept = mean(comp_gpu$avg_dur), color = 'red')

# Seems to be a distinct clustering of mean rendering times into approximately 2 clusters around the mean (41.3s)
summary(comp_gpu$avg_dur)




# Compare GPU clusters by tile
heat_vis('Render','temp',cap_label='off')


        





gpu_plot_agg = gpu_task %>%
        group_by(eventName) %>%
        summarise(watt = mean(watt), temp = mean(temp), cpu = mean(cpu), mem = mean(mem), n = n())


# Plot    
ggplot(gpu_render) + geom_point(aes(gpuSerial, watt)) + labs(x = 'GPU S/N', y = 'Mean Render Time (s)')
ggplot(gpu_perf_avg ) + geom_point(aes(index, watt)) + labs(x = 'GPU S/N', y = 'Mean Render Time (s)') 

ggplot(gpu_perf_avg ) + geom_point(aes(gpuSerial, temp)) + labs(x = 'GPU S/N', y = 'Mean Render Time (s)') 
ggplot(gpu_perf_avg ) + geom_point(aes(index, temp)) + labs(x = 'GPU S/N', y = 'Mean Render Time (s)') 
ggplot(gpu_perf_avg ) + geom_boxplot(aes(index, temp)) + labs(x = 'GPU S/N', y = 'Mean Render Time (s)') 


ggplot(gpu_perf_avg ) + geom_point(aes(gpuSerial, cpu)) + labs(x = 'GPU S/N', y = 'Mean Render Time (s)')
ggplot(gpu_perf_avg ) + geom_point(aes(gpuSerial, mem)) + labs(x = 'GPU S/N', y = 'Mean Render Time (s)') 



gpu_render = gpu_task %>%
        filter(eventName == 'Render') %>%
        group_by(gpuSerial, eventName) %>%
        summarise(watt = mean(watt), temp = mean(temp), cpu = mean(cpu), mem = mean(mem)) %>%
        arrange(gpuSerial)
        

ggplot(gpu_render) + geom_point(aes(gpuSerial, watt)) + labs(x = 'GPU S/N', y = 'Mean Render Time (s)')        






