#######  Generating visualization of Adverse Events Reporting By Using Of R Language ####################### 
#Author        :Sanumolu
#Input Datasets  :ADAE, ADSL
##Date created  : 
#Date Modified : 
###############################################################################

# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("forcats")
# install.packages("purrr")
# install.packages("pharmaverseadam")

library(dplyr)
library(ggplot2)
library(forcats)
library(purrr)
library(pharmaverseadam)

# Start log --------------------------------------------------
# sink("logs/02_create_Visualization.log", split = TRUE)
cat("Program started:", as.character(Sys.time()), "\n")

data("adae")
data("adsl")

plot_png1 <- adae %>%
  filter(!is.na(TRT01A), !is.na(AESEV)) %>%
  ggplot(aes(x = TRT01A, fill = AESEV)) +
  geom_bar(position = "stack") +
  labs(
    title = "AE severity distribution by Treatment",
    x = "Treatment Arm",
    y = "Count of AEs",
    fill = "Severity/Intensity"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

# Save outputs ----------------------------------------------
ggsave("outputs/02_create_Visualization_1.png", plot = plot_png1)



# Top 10 most frequent AEs based on subjects with event
top10_ae <- adae %>%
  filter(!is.na(AETERM)) %>%
  distinct(USUBJID, AETERM) %>%
  count(AETERM, name = "n") %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)

# Total subjects for incidence denominator
n_subj <- adae %>%
  distinct(USUBJID) %>%
  nrow()

# Calculate incidence + Clopper-Pearson exact CI
plot_dat <- top10_ae %>%
  mutate(
    ci = map(
      n,
      ~ binom.test(.x, n_subj, conf.level = 0.95)$conf.int
    ),
    lower = map_dbl(ci, 1),
    upper = map_dbl(ci, 2),
    pct = (n / n_subj) * 100,
    lower_pct = lower * 100,
    upper_pct = upper * 100
  ) %>%
  arrange(pct) %>%
  mutate(AETERM = fct_inorder(AETERM))

plot_png2 <- ggplot(plot_dat, aes(x = pct, y = AETERM)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(xmin = lower_pct, xmax = upper_pct),
    width = 0.2,
    orientation = "y"
  ) +
  scale_x_continuous(
    breaks = seq(0, 40, 10),
    labels = scales::label_number(suffix = "%")
  ) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n =" , n_subj," subjects; 95% Clopper-Pearson CIs"),
    x = "Percentage of Patients (%)",
    y = ""
  ) +
  theme_minimal(base_size = 12)


# Save outputs ----------------------------------------------
ggsave("outputs/02_create_Visualization_2.png", plot = plot_png2)

# Start log --------------------------------------------------
# sink("logs/02_create_Visualization.log", split = TRUE)
cat("Program started:", as.character(Sys.time()), "\n")

