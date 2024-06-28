# Clean up instructor-provided data for analysis
library(magrittr)
library(dplyr)
library(purrr)
library(stringr)

load("data/218-c-1238.Rdata")


consent_names <- c("id", "section", "consent_submit_time", "_1",
                   "share", "share_bin",
                   "of_age", "of_age_bin",
                   "_2", "_3", "_4")
consent <- anonymized_responses$informed_consent %>%
  set_names(consent_names) %>%
  select(-starts_with("_")) %>%
  mutate(across(matches("_bin"), as.logical)) %>%
  filter(share_bin, of_age_bin)

pre_exp_names <- c("id", "section", "pre_exp_submit_time", "_1",
                   "process_investigation", "_2", "_3", "_4", "_5")
pre_exp <- anonymized_responses$pre_experiment %>%
  filter(id %in% consent$id) %>%
  set_names(pre_exp_names) %>%
  select(-starts_with("_"))

exp_part_names <- c("id", "section", "exp_submit_time", "_1",
                    "response_code", "_2", "_3", "_4", "_5")
exp_part <- anonymized_responses$exp_participation %>%
  filter(id %in% consent$id) %>%
  set_names(exp_part_names) %>%
  select(-starts_with("_"))

post_exp_names <- c("id", "section", "post_exp_submit_time", "_1",
                    "exp_purpose", "_2",
                    "exp_hypothesis", "_3",
                    "exp_error", "_4",
                    "exp_variables", "_5",
                    "exp_design", "_6",
                    "_7", "_8", "_9")
post_exp <- anonymized_responses$post_experiment %>%
  filter(id %in% consent$id) %>%
  set_names(post_exp_names) %>%
  select(-starts_with("_"))

abs_ref_names <- c("id", "section", "abs_submit_time", "_1",
                   "abs_clearer", "_2",
                   "_7", "_8", "_9")
abs_ref <- anonymized_responses$abstract_reflect %>%
  filter(id %in% consent$id) %>%
  set_names(abs_ref_names) %>%
  select(-starts_with("_"))

pres_ref_names <- c("id", "section", "pres_submit_time", "_1",
                    "pres_info_differ", "_2",
                    "pres_emphasis", "_3",
                    "pres_design_critique", "_4",
                    "pres_type_prefer", "_5",
                    "_7", "_8", "_9")
pres_ref <- anonymized_responses$presentation_reflect %>%
  filter(id %in% consent$id) %>%
  set_names(pres_ref_names) %>%
  select(-starts_with("_"))


questions <- anonymized_responses %>%
  purrr::map(., names) %>%
  purrr::map2_df(names(.), ., ~tibble(table = .x, variables = .y)) %>%
  mutate(table = str_remove_all(table, "informed_|eriment|icipation|tract|lect|entation")) %>%
  mutate(full_name = c(consent_names, pre_exp_names, exp_part_names, post_exp_names, abs_ref_names, pres_ref_names)) %>%
  mutate(variables = str_remove_all(variables, "\\d{3,}: ")) %>%
  filter(!str_detect(full_name, "^_"))

library(rlang)
write_q_lines <- function(df, qtext, qname) {
  writeLines(c(qtext, "-----", df[[qname]]), paste0("data/student_responses/", qname))
}

filter(questions, table != "consent", nchar(variables) > nchar("section_sis_id")) %>%
  mutate(table_act = purrr::map(table, get)) %>%
  mutate(write_out = purrr::pmap(list(df = .$table_act, qtext = .$variables, qname = .$full_name), write_q_lines))
