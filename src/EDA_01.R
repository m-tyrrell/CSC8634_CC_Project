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
log_corr = gpu %>%
        select(4:7)

# Take sample of q2 index
x = sample(1:dim(log_corr)[1],100000)
# Slice q2 by sample
log_corr_samp = log_corr[x,]
cache('log_corr_samp')

p5 = ggplot(log_corr_samp, aes(powerDrawWatt, gpuTempC)) + geom_point(size=0.25) + stat_smooth() + labs( x = 'Power (W)', y = 'Temperature (C)')
p6 = ggplot(log_corr_samp, aes(gpuUtilPerc, gpuTempC)) + geom_point(size=0.25) + stat_smooth()+ labs( x = 'GPU Usage (%)', y = 'Temperature (C)')
p7 = ggplot(log_corr_samp, aes(gpuUtilPerc, powerDrawWatt)) + geom_point(size=0.25) + stat_smooth() + labs( x = 'GPU Usage (%)', y = 'Power (W)')
p8 = ggplot(log_corr_samp, aes(gpuUtilPerc, gpuMemUtilPerc)) + geom_point(size=0.25) + stat_smooth() + labs( x = 'GPU Usage (%)', y = 'Memory Usage (%)')
p9 = ggplot(log_corr_samp, aes(powerDrawWatt, gpuMemUtilPerc)) + geom_point(size=0.25) + stat_smooth() + labs( x = 'Power (W)', y = 'Memory Usage (%)')
p10 = ggplot(log_corr_samp, aes(gpuTempC, gpuMemUtilPerc)) + geom_point(size=0.25) + stat_smooth() + labs( x = 'Temperature (C)', y = 'Memory Usage (%)')

# Plot grid of 6
p11 = grid.arrange(p5, p6, p7, p8, p9, p10, ncol=3)
ggsave('graphs/p11.png', p11, width = 6.3, height = 3.87)





# Which tasks dominate runtimes?
task_runtime_means = task_runtimes %>%
        # Aggregate by event taking mean of all observations for each event
        group_by('Event' = eventName) %>%
        summarise('Duration (s)' = round(mean(duration),2), n = n())

# Table showing means of all gpu metrics for each task mean by event
gpu_plot_agg = gpu_task %>%
        filter(eventName != 'TotalRender') %>%
        group_by('Event' = eventName) %>%
        summarise('Power (W)' = round(mean(watt),2), 'Temperature (s)' = round(mean(temp),2), 'GPU Usage (%)' = round(mean(cpu),3), 'Memory Usage (%)' = round(mean(mem),3), n = n())






##### Execution Time Plot - histograms showing distribution of execution times for each event type (Render & Tiling)

# Execution Time
p12 = plot_hist(task_runtimes,'duration','Render',lab = 'Execution Time (s)',n=0.5,c=1)
p13 = plot_hist(task_runtimes,'duration','Tiling',lab = 'Execution Time (s)',n=0.005,c=1)
#minimal data
plot_hist(task_runtimes,'duration','Uploading',lab = 'Execution Time (s)',colour='#0066CC',d=0.9,f=0.95,h=0.85)
plot_hist(task_runtimes,'duration','Saving Config',lab = 'Execution Time (s)',colour='#0066CC',n=0.0001,rnd=4,d=0.95,f=0.9,h=0.85)
# Plot Render & Tiling
cache('p12')
cache('p13')
grid.arrange(p12, p13, ncol=1)




# Power Consumption
p14 = plot_hist(gpu_task,'watt','Render',title=NULL,lab = 'Power Consumption (W)', n=0.7,c=1,h=0.9)
#minimal data
plot_hist(gpu_task,'watt','Tiling',lab = 'Power Consumption (W)', n=0.2,c=0,e=1,h=0.9)
plot_hist(gpu_task,'watt','Uploading',lab = 'Power Consumption (W)', n=0.2,c=1,h=0.9)
plot_hist(gpu_task,'watt','Saving Config',lab = 'Power Consumption (W)', n=0.2,c=0,e=1,h=0.9)

# Temperature
p15 = plot_hist(gpu_task,'temp','Render',title=NULL,lab = 'Temperature (C)', n=0.2,c=0,e=1,h=0.9)
#minimal data
plot_hist(gpu_task,'temp','Tiling',lab = 'Temperature (C)', n=0.2,c=0,e=1)
plot_hist(gpu_task,'temp','Uploading',lab = 'Temperature (C)', n=0.1,c=0,e=1)
plot_hist(gpu_task,'temp','Saving Config',lab = 'Temperature (C)', n=0.1,c=1,e=0)

# CPU
p16 = plot_hist(gpu_task,'cpu','Render',title=NULL,lab = 'GPU Usage (%)', n=0.4,c=1,e=0,h=0.9)
#minimal data
plot_hist(gpu_task,'cpu','Tiling',lab = 'GPU Usage (%)', n=0.005,c=0,e=1)
plot_hist(gpu_task,'cpu','Uploading',lab = 'GPU Usage (%)', n=0.1,c=0,e=1)
plot_hist(gpu_task,'cpu','Saving Config',lab = 'GPU Usage (%)', n=0.1,c=0,e=1)

# Memory
p17 = plot_hist(gpu_task,'mem','Render',title=NULL,lab = 'Memory Usage (%)', n=0.3,c=1,e=0,h=0.9)
#minimal data
plot_hist(gpu_task,'mem','Tiling',lab = 'Memory Usage (%)', n=0.005,c=0,e=1)
plot_hist(gpu_task,'mem','Uploading',lab = 'Memory Usage (%)', n=0.1,c=0,e=1)
plot_hist(gpu_task,'mem','Saving Config',lab = 'Memory Usage (%)', n=0.1,c=0,e=1)

# Plot render for GPU metrics
cache('p14')
cache('p15')
cache('p16')
cache('p17')
grid.arrange(p14, p15, ncol=1)
grid.arrange(p16, p17, ncol=1)






# QQ Test for normality on GPU perfmormance vars (render only)
# Get execution time vector
qq = task_runtimes %>%
        filter(eventName == 'Render')

test = filter(qq, duration <= 25)

# plot
p18 = ggplot(qq, aes(sample = duration)) + stat_qq() + stat_qq_line(color='#0066CC')

# Get GPU metrics
qq2 = gpu_task %>%
        filter(eventName == 'Render')
# plot
p19 = ggplot(qq2, aes(sample = watt)) + stat_qq() + stat_qq_line(color='#0066CC') + labs(title = 'Power Consumption (W)')
p20 = ggplot(qq2, aes(sample = temp)) + stat_qq() + stat_qq_line(color='#0066CC') + labs(title = 'Temperature (C)')
p21 = ggplot(qq2, aes(sample = cpu)) + stat_qq() + stat_qq_line(color='#0066CC') + labs(title = 'GPU Usage (%)')
p22 = ggplot(qq2, aes(sample = mem)) + stat_qq() + stat_qq_line(color='#0066CC') + labs(title = 'Memory Usage (%)')
cache('p19')
cache('p20')
cache('p21')
cache('p22')
p23 = grid.arrange(p19, p20, p21, p22, ncol=2)
ggsave('graphs/p23.png', p23, width = 6.3, height = 5.8)






##### Compare performance by GPU S/N 
#Strip down gpu dataset to only hostnames and gpu serial
gpu_serial = gpu %>%
        distinct(hostname, gpuSerial, .keep_all = FALSE) %>%
        arrange(gpuSerial)
cache('gpu_serial')

# Filter and aggregate joined app_check/taskxy to display only relevant variables/tasks
comp_gpu = task_runtimes %>%
        # Remove non-render tasks 
        filter(eventName == 'Render') %>%
        # Join stripped down gpu dataset on hostname
        left_join(gpu_serial, by = 'hostname') %>%
        # Aggregate by mean render time
        group_by(gpuSerial) %>%
        summarise(avg_dur = mean(duration)) %>%
        # Add index column for plotting
        mutate(index = 1:1024)
cache('comp_gpu')

# Plot render time by S/N
h = median(comp_gpu$avg_dur)
p24 = ggplot(comp_gpu) + geom_point(aes(gpuSerial, avg_dur)) + labs(x = 'GPU S/N', y = 'Mean Render Execution Time (s)') + 
        geom_hline(yintercept = h, color = 'red', linetype = 'dotdash') +
        annotate("text", x=comp_gpu$gpuSerial[1]+500000000, y=h, label= paste("Median",round(h,2)), size=2.5, vjust=1.5)
cache('p24')

# Seems to be a distinct clustering of mean rendering times into approximately 2 clusters around the mean (41.3s)
# Group S/Ns into 4 clusters based on plot
c1 = comp_gpu$gpuSerial[comp_gpu$gpuSerial < 322000000000]
c2 = comp_gpu$gpuSerial[comp_gpu$gpuSerial > max(c1) & comp_gpu$gpuSerial < 323500000000]
c3 = comp_gpu$gpuSerial[comp_gpu$gpuSerial > max(c2) & comp_gpu$gpuSerial < 324000000000]
c4 = comp_gpu$gpuSerial[comp_gpu$gpuSerial > max(c3)]
# Check length of vectors sums to 1024
length(c1)+length(c2)+length(c3)+length(c4)
# Create dataframe to display S/N ranges by cluster
sn_cluster_df = data_frame(c(min(c1),max(c1),length(c1)),c(min(c2),max(c2),length(c2)),c(min(c3),max(c3),length(c3)),c(min(c4),max(c4),length(c4)))
row.names(sn_cluster_df) = c('Minimum S/N','Maximum S/N','n')
names(sn_cluster_df) = c('Series 1', 'Series 2','Series 3', 'Series 4')
sn_cluster_df$`Series 1` = as.character(sn_cluster_df$`Series 1`)
sn_cluster_df$`Series 2` = as.character(sn_cluster_df$`Series 2`)
sn_cluster_df$`Series 3` = as.character(sn_cluster_df$`Series 3`)
sn_cluster_df$`Series 4` = as.character(sn_cluster_df$`Series 4`)
cache('sn_cluster_df')

##### Compare GPU log metrics by GPU S/N
# Filter GPU task to allow comarison of S/N by GPU log
gpu_render = gpu_task %>%
        filter(eventName == 'Render') %>%
        group_by(gpuSerial, eventName) %>%
        summarise(watt = mean(watt), temp = mean(temp), cpu = mean(cpu), mem = mean(mem)) %>%
        arrange(gpuSerial)

cache('gpu_render')
# Render event: Performance plots by S/N
h = mean(gpu_render$watt)
p25 = ggplot(gpu_render) + geom_point(aes(gpuSerial, watt)) + labs(x = 'GPU S/N', y = 'Power Consumption (s)') +
        geom_hline(yintercept = h, color = 'red', linetype = 'dotdash') +
        annotate("text", x=comp_gpu$gpuSerial[1]+1000000000, y=h, label= paste("Arithmetic Mean",round(h,2)), size=2.5, vjust=1.5)
cache('p25')
h = mean(gpu_render$temp)
p26 = ggplot(gpu_render) + geom_point(aes(gpuSerial, temp)) + labs(x = 'GPU S/N', y = 'Temperature (C)') +
        geom_hline(yintercept = h, color = 'red', linetype = 'dotdash') +
        annotate("text", x=comp_gpu$gpuSerial[1]+1000000000, y=h, label= paste("Arithmetic Mean",round(h,2)), size=2.5, vjust=1.5)
cache('p26')
h = median(gpu_render$cpu)
p27 = ggplot(gpu_render) + geom_point(aes(gpuSerial, cpu)) + labs(x = 'GPU S/N', y = 'GPU Usage (%)') +
        geom_hline(yintercept = h, color = 'red', linetype = 'dotdash') +
        annotate("text", x=comp_gpu$gpuSerial[1]+1000000000, y=h, label= paste("Median",round(h,2)), size=2.5, vjust=1.5)
cache('p27')
h = mean(gpu_render$mem)
p28 = ggplot(gpu_render) + geom_point(aes(gpuSerial, mem)) + labs(x = 'GPU S/N', y = 'Mem Usage (%)') +
        geom_hline(yintercept = h, color = 'red', linetype = 'dotdash') +
        annotate("text", x=comp_gpu$gpuSerial[1]+1000000000, y=h, label= paste("Arithmetic Mean",round(h,2)), size=2.5, vjust=1.5)
cache('p28')

grid.arrange(p25, p26, p27, p28, ncol=2)




        
##### Compare resource usage by tile (heatmaps)
# Setup image device, plot heatmap using heat_vis function, output image to graphs folder
# Render and Tiling only (no contrast on other events)
png('graphs/p29.png')
heat_vis('Render','duration',caption='on')
dev.off ();

png('graphs/p30.png')
heat_vis('Render','watt',caption='on')
dev.off ();

png('graphs/p31.png')
heat_vis('Render','outlier',outlier_qty=50,out_sign='>',caption='on')
dev.off ();

png('graphs/p32.png')
heat_vis('Render','outlier',outlier_qty=25,out_sign='<',caption='on')
dev.off ();

png('graphs/p33.png')
heat_vis('Render','cpu',caption='on')
dev.off ();

png('graphs/p34.png')
heat_vis('Render','mem',caption='on')
dev.off ();


# Load interesting plot images and cache
i29 <-  rasterGrob(as.raster(readPNG("graphs/p29.png")), interpolate = FALSE)
cache('i29')
i30 <-  rasterGrob(as.raster(readPNG("graphs/p30.png")), interpolate = FALSE)
cache('i30')
i31 <-  rasterGrob(as.raster(readPNG("graphs/p31.png")), interpolate = FALSE)
cache('i31')
i32 <-  rasterGrob(as.raster(readPNG("graphs/p32.png")), interpolate = FALSE)
cache('i32')
i33 <-  rasterGrob(as.raster(readPNG("graphs/p33.png")), interpolate = FALSE)
cache('i33')
i34 <-  rasterGrob(as.raster(readPNG("graphs/p34.png")), interpolate = FALSE)
cache('i34')

# Plot images
grid.arrange(i30, i33, i34, i29, i31, i32, ncol = 3)

     



##### Interplay between execution time and GPU log metrics
# Filter task_runtimes and joined task_gpu
task_master = task_runtimes %>%
        select(-hostname) %>%
        # Join stripped down gpu dataset on hostname
        left_join(gpu_task, by = c("taskId" = "taskId", "eventName" = "eventName"))
cache('task_master')
# remove other phases leaving render
task_render = filter(task_master, eventName == 'Render')
cache('task_render')

#Plot invidual correlations
p35 = ggplot(task_render, aes(watt, duration)) + geom_point(size=0.25) + stat_smooth() + labs( x = 'Power (W)', y = 'Execution Time (s)')
p36 = ggplot(task_render, aes(temp, duration)) + geom_point(size=0.25) + stat_smooth() + labs( x = 'Temperature (C)', y = 'Execution Time (s)')
p37 = ggplot(task_render, aes(cpu, duration)) + geom_point(size=0.25) + stat_smooth() + labs( x = 'GPU Usage (%)', y = 'Execution Time (s)')
p38 = ggplot(task_render, aes(mem, duration)) + geom_point(size=0.25) + stat_smooth() + labs( x = 'Memory Usage (%)', y = 'Execution Time (s)')
cache('p35')
cache('p36')
cache('p37')
cache('p38')
# Plot images
p39 = grid.arrange(p35, p36, p37, p38, ncol = 2)
ggsave('graphs/p39.png', p39, width = 6.3, height = 5.8)








