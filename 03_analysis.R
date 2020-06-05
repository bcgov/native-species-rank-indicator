# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


#remotes::install_github("bcgov/ranktrends")

library(ranktrends)
library(tidyr)
library(dplyr)

#if (!exists("indata"))
indata = readRDS(file.path("data","indata.rds"))


indata <- indata %>%
  filter(!is.na(srank)) %>%
  select(-c(elcode, bc_list, origin))


status_data <- indata %>%
  mutate(crank = clean_ranks(srank)) %>%
  drop_na(crank)


# not sure if this is needed now?
#status_complete <- status_data %>%
#  group_by(taxonomic_group) %>%
#  complete(nesting(scientific_name, common_name), year) %>%
#  semi_join(
#    group_by(., taxonomic_group, scientific_name, common_name) %>%
#      summarize())



# remove those species which are extinct
species_to_remove <- status_data %>%
  filter(year == min(year) & srank == "SX") %>%
  pull(scientific_name) %>%
  unique()


status_data_final <- status_complete  %>%
  filter(!scientific_name %in% species_to_remove) %>%
  mutate(taxonomic_group = ifelse(
    taxonomic_group %in% c("Amphibians", "Reptiles and Turtles"),
    "Reptiles & Amphibians",
    taxonomic_group))



# run function

ranks_prob_key <- read.csv("ranks_prob_key.csv", stringsAsFactors = FALSE)



sampled_index <- function(wt_data, tax_group, wts_col, yr_col, nreps = 1000){

  wt_data = status_data_final
  tax_group = "taxonomic_group"
  rank_col = "crank"
  yr_col = "year"
  nreps = 10


  # use the look up table to get numeric, create wts column from character col

  wt_data <- dplyr::mutate_(wt_data, rank = rank_col)

  wt_data <- dplyr::left_join(wt_data, ranks_prob_key, by = c("rank" = "basic_rank"))
  wt_data <-  wt_data[!is.na( wt_data$rank), ]

  wt_data$prob_vec <- lapply(seq_len(nrow(wt_data)), function(x) {
    as.numeric(t(wt_data[x, c("p_0", "p_1", "p_2", "p_3", "p_4", "p_5")]))
  })

  wt_data <- dplyr::select(wt_data, -c("p_0", "p_1", "p_2", "p_3", "p_4", "p_5"))


  wt_data <- filter(wt_data, taxonomic_group == "Odonata")

 # wt_data <- wt_data %>%
#    mutate(samples = lapply(wt_data$prob_vec, function(x) {
#       sample(5:0, 100, replace = TRUE, prob = x)
#      }))

#      sample(5:0, 100, replace = TRUE, prob = prob_vec))

  #wt_data$samples <-lapply(wt_data$prob_vec, function(x) {
  # sample(5:0, 100, replace = TRUE, prob = x)
  #})

  #wt_data$rli <-lapply(wt_data$samples, function(x) {
  #  rli(x)
  #})


#  wt_data$samples <-lapply(wt_data$prob_vec, function(x) {
#   sample(5:0, 1, replace = TRUE, prob = x)
#  })


  make_samples <- function(sdata, prob_vect) {

    purrr::map_dbl(sdata, sample(5:0, 100, replace = TRUE, prob = prob_vect))
  }

  make_samples(wt_data$data, wt_data$prob_vect)

  purrr::map_dbl(sdata, sample(5:0, 100, replace = TRUE, prob = prob_vect))


  wt_data <- dplyr::group_by_(wt_data, tax_group, yr_col)
  wt_data <- tidyr::nest(wt_data)

##

  csi <- dplyr::mutate(wt_data,
                       N = purrr::map_dbl(.data$data, nrow),
                      # samples = purrr::map(
                      #   .data$data,
                      #   ~ replicate(nreps, rli(purrr::map_dbl(.x$wts, sample, 1)))
                      # ),

                       samples = purrr::map(
                         .data$data,
                         ~ replicate(nreps, rli(purrr::map_dbl(.x$wts, sample, 1)))
                       ),




                    #     mean_wt = purrr::map_dbl(.data$samples, mean),
                    #     min_wt = purrr::map_dbl(.data$samples, min),
                    #     max_wt = purrr::map_dbl(.data$samples, max),
                    #     lci = purrr::map_dbl(.data$samples, stats::quantile, probs = 0.025), # TODO AT: makes into params
                    #     uci = purrr::map_dbl(.data$samples, stats::quantile, probs = 0.975)
                       )
                      )

                       csi






                       rli(purrr::map_dbl(.x$prob_vec, sample(5:0, 1, replace = TRUE)))




# function to create probability vector

xx <- lapply(wt_data$rank, function(iranks){

  probs(iranks)

})



purrr::map(wt_data$rank, probs(.x))


rank_col = wt_data$rank

probs <- function(rank_col) {

   lapply(as.list(rank_col), function(x) {
    #rank = "S2S3"
  ranks_prob = dplyr::filter(ranks_prob_key, basic_rank == (x))
  ranks_prob = dplyr::select(ranks_prob, grep("^p_", names(ranks_prob), perl = T))
  as.numeric(t(ranks_prob))
  })
}


probs(wt_data$rank)


probs(wt_data$rank)





csi <- csi %>% drop_na(taxonomic_group)


saveRDS(csi, file.path("data","csi_taxonomic.R"))


# Group by BC Listing  ----------------------------------------------------

status_complete <- status_data_wts %>%
  group_by(bc_list) %>%
  complete(nesting(scientific_name, common_name), year) %>%
  semi_join(
    group_by(., bc_list, scientific_name, common_name) %>%
      summarize())

# remove those species which are extinct
species_to_remove <- status_complete %>%
  filter(year == min(year) & rank == "SX") %>%
  pull(scientific_name) %>%
  unique()


status_data_final <- status_complete  %>%
  filter(!scientific_name %in% species_to_remove) %>%
  ungroup() #%>%
#  mutate(taxonomic_group = ifelse(
#    taxonomic_group %in% c("Amphibians", "Reptiles and Turtles"),
#    "Reptiles & Amphibians",
#    taxonomic_group))

# remove NAs
status_data_final <- status_data_final %>%
  filter(!is.na(wts))

#run function
csi_bc <- sampled_index(status_data_final, "bc_list","wts","year")
csi_bc <- csi_bc %>% drop_na(bc_list)


saveRDS(csi_bc, file.path("data","csi_bclist.R"))



