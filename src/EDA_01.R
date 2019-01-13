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

