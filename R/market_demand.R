# Computes employer demand for each tool from real job postings and writes
#   data/market_demand.csv       (skill, pct)
#   data/market_demand_meta.yml  (source details shown under the heatmap)
#
# Data: "data_jobs" by Luke Barousse (Apache-2.0), ~786k data job postings from 2023:
#   https://huggingface.co/datasets/lukebarousse/data_jobs
# The 231 MB CSV is not kept in this repo. Download it, then run from the project root:
#   Rscript R/market_demand.R path/to/data_jobs.csv

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(yaml)
})

args <- commandArgs(trailingOnly = TRUE)
csv_path <- if (length(args)) args[1] else "data_jobs.csv"
role <- "Data Analyst"
country <- "Australia"

jobs <- read_csv(
  csv_path,
  col_select = c(job_title_short, job_country, job_posted_date, job_skills),
  show_col_types = FALSE
) |>
  filter(job_title_short == role, job_country == country) |>
  mutate(id = row_number())

n_postings <- nrow(jobs)

# job_skills looks like "['sql', 'python', 'power bi']"
skills <- jobs |>
  filter(!is.na(job_skills)) |>
  mutate(job_skills = gsub("\\[|\\]|'", "", job_skills)) |>
  separate_rows(job_skills, sep = ",\\s*") |>
  filter(job_skills != "") |>
  distinct(id, skill = job_skills)

# Tags that are usually ordinary English picked up by the dataset's keyword
# matching ("go", "flow", "express") rather than real tool requirements
ambiguous <- c("go", "flow", "express")

demand <- skills |>
  filter(!skill %in% ambiguous) |>
  count(skill, name = "postings") |>
  mutate(pct = round(postings / n_postings, 3)) |>
  arrange(desc(pct)) |>
  filter(postings >= 5)

write_csv(select(demand, skill, pct), "data/market_demand.csv")

dates <- range(as.Date(jobs$job_posted_date))
write_yaml(list(
  is_sample = FALSE,
  source = "data_jobs dataset by Luke Barousse (Apache-2.0)",
  source_url = "https://huggingface.co/datasets/lukebarousse/data_jobs",
  role = role,
  country = country,
  n_postings = n_postings,
  period = paste(format(dates, "%b %Y"), collapse = " – ")
), "data/market_demand_meta.yml")

message(sprintf("%s %s postings: %d; skills kept: %d", country, role, n_postings, nrow(demand)))
print(head(demand, 20))
