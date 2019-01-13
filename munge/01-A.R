# preprocessing script.



task_xy = read.csv(file = "data/terra/task-x-y.csv")
cache("task_xy")

gpu = read.csv(file = "data/terra/gpu.csv")
gpu$timestamp = ymd_hms(as.character(gpu$timestamp))
cache("gpu")

app_check = read.csv(file = "data/terra/application-checkpoints.csv")
app_check$timestamp = ymd_hms(as.character(app_check$timestamp))
cache("app_check")


