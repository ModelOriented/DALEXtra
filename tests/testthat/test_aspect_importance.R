context("Check aspect_importance() functions")

test_that("check output for aspects importance (glm, default)",{
  library("DALEX")
  library("ingredients")

  aspect_importance_titanic_glm <- aspect_importance(titanic_glm_model, titanic_data,
                                                     new_observation = titanic_new_observation,
                                                     aspects = titanic_aspects)

  expect_true("data.frame" %in% class(aspect_importance_titanic_glm))
  expect_true(dim(aspect_importance_titanic_glm)[1] == 4)
  expect_true(dim(aspect_importance_titanic_glm)[2] == 3)
})

test_that("check output for aspects importance (lm, binom)",{
  library("DALEX")
  library("ingredients")

  if (getRversion() >= "3.6")
  {
    suppressWarnings(set.seed(123, sample.kind = "Rounding"))
  } else {
    set.seed(123)
  }

  aspect_importance_apartments <- aspect_importance(apartments_lm_model, apartments,
                                                    new_observation = apartments_new_observation,
                                                    aspects =  apartments_aspects, sample_method = "binom")
  expect_true("aspect_importance" %in% class(aspect_importance_apartments))
  expect_true(floor(aspect_importance_apartments[aspect_importance_apartments$aspects == "district",]$importance) == 279)
})

test_that("check output for aspects importance (additional parameters)",{
  library("DALEX")
  library("ingredients")

  if (getRversion() >= "3.6")
  {
    suppressWarnings(set.seed(123, sample.kind = "Rounding"))
  } else {
    set.seed(123)
  }

  aspect_importance_apartments_1000 <-
    aspect_importance(apartments_lm_model, apartments,
                      new_observation = apartments_new_observation,
                      aspects =  apartments_aspects, N = 1000, f = 3)
  aspect_importance_apartments_500 <-
    aspect_importance(apartments_lm_model, apartments,
                      new_observation = apartments_new_observation,
                      aspects =  apartments_aspects, N = 500, f = 3)

  res_1 <- aspect_importance_apartments_1000[aspect_importance_apartments_1000$aspects == "district",]$importance
  res_2 <- aspect_importance_apartments_500[aspect_importance_apartments_500$aspects == "district",]$importance

  expect_true(res_1 != res_2)
})


test_that("check aspects_importance for explainer",{
  library("DALEX")
  library("ingredients")

  titanic_without_target <- titanic_data[,colnames(titanic_data)!="survived"]

  titanic_explainer <- explain(model = titanic_glm_model,
                               data = titanic_without_target,
                               verbose = FALSE)

  aspect_importance_titanic_glm <- aspect_importance(titanic_explainer,
                                                     new_observation = titanic_new_observation,
                                                     aspects = titanic_aspects)

  expect_true("data.frame" %in% class(aspect_importance_titanic_glm))
})


test_that("check plot for aspects importance",{
  library("DALEX")
  library("ingredients")

  aspect_importance_apartments <- aspect_importance(apartments_lm_model, apartments,
                                                    new_observation = apartments_new_observation,
                                                    aspects =  apartments_aspects, method = "binom")

  expect_is(plot(aspect_importance_apartments), "gg")
})

test_that("check plot (facets) for aspects importance",{
  library("DALEX")
  library("ingredients")

  aspect_importance_apartments1 <- aspect_importance(apartments_lm_model, apartments,
                                                    new_observation = apartments_new_observation,
                                                    aspects =  apartments_aspects, method = "binom",
                                                    label = "model 1")

  aspect_importance_apartments2 <- aspect_importance(apartments_lm_model, apartments,
                                                    new_observation = apartments_new_observation,
                                                    aspects =  apartments_aspects, label = "model 2")

  aspect_importance_apartments3 <- aspect_importance(apartments_lm_model, apartments,
                                                     new_observation = apartments_new_observation,
                                                     aspects =  apartments_aspects, label = "model 3")

  expect_is(plot(aspect_importance_apartments1, aspect_importance_apartments2,
                 aspect_importance_apartments3, add_importance = TRUE,
                 aspects_on_axis = FALSE, digits_to_round = 0), "gg")
})


test_that("check alias for aspect_importance",{
  library("DALEX")
  library("ingredients")


  aspect_importance_apartments <- lime(apartments_lm_model, apartments,
                                       new_observation = apartments_new_observation,
                                       aspects =  apartments_aspects, method = "binom")
  expect_true("aspect_importance" %in% class(aspect_importance_apartments))

})

test_that("plot for aspect_importance works",{
  library("DALEX")
  library("ingredients")

  aspect_importance_apartments <- aspect_importance(apartments_lm_model, apartments,
                                                    new_observation = apartments_new_observation,
                                                    aspects =  apartments_aspects, method = "binom")
  p <- plot(aspect_importance_apartments)
  expect_true(is.ggplot(p))
  expect_identical(p$labels$y, "Aspects importance")
  expect_error(plot.aspect_importance(apartments))
})

test_that("check for aspect_importance with lasso",{
  library("DALEX")
  library("ingredients")

  aspect_importance_apartments <- aspect_importance(apartments_lm_model, apartments,
                                                    new_observation = apartments_new_observation,
                                                    aspects =  apartments_aspects, n_var = 3)
  aspect_importance_apartments_0 <- aspect_importance(apartments_lm_model, apartments,
                                                      new_observation = apartments_new_observation,
                                                      aspects =  apartments_aspects, n_var = 1)


  expect_true("aspect_importance" %in% class(aspect_importance_apartments))
  expect_true(sum(aspect_importance_apartments[,2] != 0) == 3)
  expect_true(sum(aspect_importance_apartments_0[,2] != 0) == 0)

})

test_that("check for aspect_importance with show_cor",{
  library("DALEX")
  library("ingredients")

  aspect_list_apartments_num <- group_variables(
    apartments_num[,!colnames(apartments_num) == "m2.price"], 0.5)

  aspect_importance_apartments_num <- aspect_importance(
    apartments_num_lm_model, apartments_num,
    new_observation = apartments_num_new_observation,
    aspects =  aspect_list_apartments_num, show_cor = TRUE)

  expect_true("aspect_importance" %in% class(aspect_importance_apartments_num))
  expect_true(dim(aspect_importance_apartments_num)[2] == 5)
  expect_true(aspect_importance_apartments_num[1,5] == "pos")
})


test_that("check get_sample function with binom",{
  library("DALEX")
  library("ingredients")

  x <- get_sample(100,4,"binom")
  expect_true(ncol(x) == 4)
  expect_true(nrow(x) == 100)
  expect_true(max(x) == 1)
  expect_true(min(x) == 0)
  expect_error(get_sample(-100,4,"binom"))
})

test_that("check get_sample function with default sampling",{
  library("DALEX")
  library("ingredients")

  x <- get_sample(50,10,"default")
  expect_true(ncol(x) == 10)
  expect_true(nrow(x) == 50)
  expect_true(max(x) == 1)
  expect_true(min(x) == 0)
})

test_that("check group_variables function",{
  library("DALEX")
  library("ingredients")

  aspect_list <- group_variables(apartments_num, 0.52, draw_tree = TRUE,
                                 draw_abline = TRUE)
  expect_true(length(aspect_list) == 4)
  expect_error(group_variables(apartments, 0.6))
  expect_error(group_variables(apartments_num, 1.6))

})

test_that("check plot_group_variables function",{
  library("DALEX")
  library("ingredients")

  apartments_hc <- hclust(as.dist(1 - abs(cor(apartments_num, method = "spearman"))))
  p <- plot_group_variables(apartments_hc, p = 0.5, draw_abline = TRUE)
  expect_true("ggplot" %in% class(p))
})



test_that("check aspect_importance_single function",{
  library("DALEX")
  library("ingredients")

  aspect_importance_titanic_single <-
    aspect_importance_single(x = apartments_lm_model,
                             data = apartments[,colnames(apartments) != "m2.price"],
                             new_observation = apartments_new_observation)

  expect_true("data.frame" %in% class(aspect_importance_titanic_single))
  expect_true(dim(aspect_importance_titanic_single)[1] == 5)
  expect_true(dim(aspect_importance_titanic_single)[2] == 3)

})

test_that("check aspect_importance_single.explainer function",{
  library("DALEX")
  library("ingredients")

  titanic_without_target <- titanic_data[,colnames(titanic_data)!="survived"]

  titanic_explainer <- explain(model = titanic_glm_model,
                               data = titanic_without_target,
                               verbose = FALSE)

  aspect_importance_titanic_glm_single <- aspect_importance_single(titanic_explainer,
                                                                   new_observation = titanic_new_observation)

  expect_true("data.frame" %in% class(aspect_importance_titanic_glm_single))

})


test_that("check custom_tree_cutting function",{
  library("DALEX")
  library("ingredients")

  apartments_hc <- hclust(as.dist(1 - abs(cor(apartments_num, method = "spearman"))))
  aspect_list <- custom_tree_cutting(apartments_hc, 0.6)
  one_aspect <- custom_tree_cutting(apartments_hc, 0)

  expect_true(class(aspect_list) == "list")
  expect_true(length(aspect_list) == 4)
  expect_true(length(one_aspect) == 1)
  expect_true("surface" %in% aspect_list$aspect.group3)

})

test_that("check plot_aspects_importance_grouping function",{
  library("DALEX")
  library("ingredients")

  p1 <- plot_aspects_importance_grouping(x = apartments_num_lm_model,
                                        data = apartments_num_mod,
                                        new_observation = apartments_num_new_observation,
                                        cumulative_max = TRUE, absolute_value = TRUE)
  p2 <- plot_aspects_importance_grouping(x = apartments_num_lm_model,
                                        data = apartments_num_mod,
                                        new_observation = apartments_num_new_observation,
                                        cumulative_max = TRUE, absolute_value = FALSE)

  expect_true("ggplot" %in% class(p1))
  expect_true("ggplot" %in% class(p2))
})


test_that("check triplot function",{
  library("DALEX")
  library("ingredients")

  p <- triplot(x = apartments_num_lm_model,
               data = apartments_num_mod, new_observation = apartments_num_new_observation)

  expect_true("gtable" %in% class(p))
})

test_that("check triplot.explainer function",{
  library("DALEX")
  library("ingredients")

  apartments_explainer <- explain(model = apartments_num_lm_model,
                                  data = apartments_num_mod,
                                  verbose = FALSE)

  p <- triplot(x = apartments_explainer,
               new_observation = apartments_num_new_observation)

  expect_true("gtable" %in% class(p))
})

