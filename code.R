library(tidyverse)

# Read in
df <- read_csv("optim_problem.csv")

# Convert to long format
df_long <- df %>%
  select(Week,"StudentA", "StudentB", "StudentC", "StudentD", "StudentE", "StudentF", "StudentG", "StudentH", "StudentI", "StudentJ") %>%
  gather(key, value, - Week) %>%
  group_by(key) %>%
  arrange(key,desc(value))

# The problem:
# We have 10 students and 11 weeks.
# Each students must be assigned to 2 weeks. Each students cannot be assigned the same week.
# There are 22 spots to assign and 2 missing spots. Those missing spots can be the same week, or not.
# Each student gave a ranking out of 11 (0-10) to each week.
# We want to (1) maximise the total points (Utilitarian style).
# We want to (2) maximise the worst student (John Rawls)
# For instance, if all students had their first and second choices (10+9) the total points would be 19*10=190.
# I was able to maximise it at around 154 points. I was able to get the max minimum at 12 points. i.e. the worst student had 12 points.
# Can we do better

# MY STRATEGY: is that I draw thousands of random dataframes with student names, just at random (the lapply with timer).
# Then just below, if a=b this means it's the same student twice in one week, so I remove. But if the same student is missing, that's fine.
# Then I join the points for people, and I do various calculations using group by sum, etc.


# unique weeks and unique student names
w <- sort(unique(df_long$Week))
n <- c(sort(unique(df_long$key)),"Missing")

set.seed(222)
st <- Sys.time()
el <- list()
iter = 100000
u <- lapply(1:iter,function(x){data.frame(a=sample(n),b=sample(n))})
Sys.time()-st

uu <- bind_rows(u) %>% as_tibble() %>% mutate(id = rep(1:iter,each=11), week = rep(w,iter)) %>%
  mutate(c = (a==b & a!="Missing"))

uuu <- uu %>%
  group_by(id) %>% mutate(sc=sum(c)) %>% filter(sc==0) %>% select(-c,-sc) %>%
  left_join(df_long,c("week"="Week", "a"="key")) %>%
  left_join(df_long,c("week"="Week", "b"="key")) %>%
  ungroup() %>%
  mutate_all(function(x){ifelse(is.na(x),0,x)}) %>%
  mutate(s = value.x+value.y)
# Below: id means the random draw. s_s means the sum of scores out of 190 (with above seed 146)
uuu %>% group_by(id) %>% summarise(s_s = sum(s)) %>% arrange(desc(s_s))
pareto_id <- uuu %>% group_by(id) %>% summarise(s_s = sum(s)) %>% arrange(desc(s_s)) %>% slice(1:10000) %>% pull(id)
# TOP RESULT MAXIMISING POINTS
uuu %>% filter(id %in% pareto_id)
# I CALL IT RAWLS CAUSE I MAXIMISE THE WORST STUDENT (under the veil of ignorance)
rawls_id <- bind_rows(uuu %>% filter(id %in% pareto_id) %>% select(name=a,id,week,value=value.x),
                      uuu %>% filter(id %in% pareto_id) %>% select(name=b,id,week,value=value.y)) %>%
  group_by(id,name) %>% summarise(s_v = sum(value)) %>% arrange() %>%
  arrange(id,desc(s_v)) %>% slice(10:10) %>% ungroup() %>% arrange(desc(s_v)) %>% slice(1) %>% pull(id)
uuu %>% filter(id %in% rawls_id)
# This is bad, students got between 11 and 16, with sum 143
uuu %>% filter(id %in% rawls_id) %>% pull(s) %>% sum()
bind_rows(uuu %>% filter(id %in% rawls_id) %>% select(name=a,id,week,value=value.x),
          uuu %>% filter(id %in% rawls_id) %>% select(name=b,id,week,value=value.y)) %>%
  group_by(id,name) %>% summarise(s_v = sum(value)) %>% arrange(desc(s_v))
