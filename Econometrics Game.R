library(haven)
STAR_public_use <- read_dta("STARdatapost 116327-V1 2009 Replication data for Incentives and Services for College Achievement Evidence from a Randomized Trial Angrist et al/STAR_public_use.dta")
student_pres_data <- read_dta("EXTD 112446-V1 2011 Replication data for Peer Effects, Teacher Incentives, and the Impact of Tracking Evidence from a Randomized Evaluation in Kenya Dufflo et al/data/student_pres_data.dta")
student_test_data <- read_dta("EXTD 112446-V1 2011 Replication data for Peer Effects, Teacher Incentives, and the Impact of Tracking Evidence from a Randomized Evaluation in Kenya Dufflo et al/data/student_test_data.dta")
teacher_pres_data <- read_dta("EXTD 112446-V1 2011 Replication data for Peer Effects, Teacher Incentives, and the Impact of Tracking Evidence from a Randomized Evaluation in Kenya Dufflo et al/data/teacher_pres_data.dta")

path <- "C:/Users/daind/Documents/NYU/NYU 25-26/2026 Econometrics Game/EXTD 112523-V1 2012 Replication data for Incentives Work Getting Teachers to Come to School Dufflo et al/data"

files <- list.files(
  path = path,
  pattern = "\\.dta$",
  full.names = TRUE
)

data_list <- lapply(files, read_dta)

names(data_list) <- tools::file_path_sans_ext(basename(files))
