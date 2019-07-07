
library(rrundeck)

rc %>% listProjects()

rc %>% getProject("APH")

rc %>% getProject("APH") %>% listJobs()

rc %>% getProject("APH") %>% getJob(name = "update-reporting") %>% run(follow = TRUE)
