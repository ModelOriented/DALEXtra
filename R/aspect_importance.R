#' Calculates the feature groups importance (called aspects importance) for a
#' selected observation
#'
#' Aspect Importance function takes a sample from a given dataset and modifies
#' it. Modification is made by replacing part of its aspects by values from the
#' observation. Then function is calculating the difference between the
#' prediction made on modified sample and the original sample. Finally, it
#' measures the impact of aspects on the change of prediction by using the
#' linear model or lasso.
#'
#'
#' @param x an explainer created with the \code{DALEX::explain()} function
#' or a model to be explained.
#' @param data dataset, it will be extracted from \code{x} if it's an explainer
#' NOTE: It is best when target variable is not present in the \code{data}
#' @param predict_function predict function, it will be extracted from \code{x}
#'   if it's an explainer
#' @param new_observation selected observation with columns that corresponds to
#'   variables used in the model
#' @param aspects list containting grouping of features into aspects
#' @param N number of observations to be sampled (with replacement) from data
#' @param label name of the model. By default it's extracted from the 'class'
#'   attribute of the model.
#' @param sample_method sampling method in \code{\link{get_sample}}
#' @param n_var maximum number of non-zero coefficients after lasso fitting,
#'   if zero than linear regression is used
#' @param f frequency in \code{\link{get_sample}}
#' @param show_cor show if all features in aspect are pairwise positivly
#'   correlated, works only if dataset contains solely numeric values
#' @param ... other parameters
#'
#' @return An object of the class \code{aspect_importance}. Contains dataframe
#'   that describes aspects' importance.
#'
#' @importFrom stats lm
#' @importFrom stats coef
#' @importFrom stats model.matrix
#' @importFrom glmnet glmnet
#'
#' @examples
#' library("DALEX")
#' library("ingredients")
#'
#' titanic_imputed$country <- NULL
#'
#' model_titanic_glm <- glm(survived == "yes" ~
#'                          class+gender+age+sibsp+parch+fare+embarked,
#'                          data = titanic_imputed,
#'                          family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                                data = titanic_imputed[,-8],
#'                                y = titanic_imputed$survived == "yes",
#'                                verbose = FALSE)
#'
#' aspects <- list(wealth = c("class", "fare"),
#'                 family = c("sibsp", "parch"),
#'                 personal = c("gender", "age"),
#'                 embarked = "embarked")
#'
#' aspect_importance(explain_titanic_glm,
#'                   new_observation = titanic_imputed[1,],
#'                   aspects = aspects)
#'
#' \donttest{
#' library("randomForest")
#' model_titanic_rf <- randomForest(survived ~ class + gender + age + sibsp +
#'                                  parch + fare + embarked,
#'                                  data = titanic_imputed)
#'
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                               data = titanic_imputed[,-8],
#'                               y = titanic_imputed$survived == "yes",
#'                               verbose = FALSE)
#'
#' aspect_importance(explain_titanic_rf,
#'                   new_observation = titanic_imputed[1,],
#'                   aspects = aspects)
#'
#' }
#'
#' @export

aspect_importance <- function(x, ...)
  UseMethod("aspect_importance")

#' @export
#' @rdname aspect_importance

aspect_importance.explainer <- function(x, new_observation, aspects,
                                        N = 100, sample_method = "default",
                                        n_var = 0, f = 2,
                                        show_cor = FALSE, ...) {

  # extracts model, data and predict function from the explainer
  data <- x$data
  model <- x$model
  predict_function <- x$predict_function
  label <- x$label

  # calls target function
  aspect_importance.default(model, data, predict_function,
                            new_observation, aspects, N, label, sample_method,
                            n_var, f, show_cor)
}

#' @export
#' @rdname aspect_importance

aspect_importance.default <- function(x, data, predict_function = predict,
                                      new_observation,
                                      aspects, N = 100,
                                      label = class(x)[1],
                                      sample_method = "default", n_var = 0,
                                      f = 2, show_cor = FALSE, ...) {

  # look only for common variables in data and new observation
  if ("data.frame" %in% class(data)) {
    common_variables <- intersect(colnames(new_observation), colnames(data))
    new_observation <- new_observation[, common_variables, drop = FALSE]
    data <- data[, common_variables, drop = FALSE]
  }

  # stop if no common variables are found
  stopifnot(length(common_variables) > 0,
            length(setdiff(unlist(aspects),
                           colnames(new_observation))) == 0)

  #number of expected coefficients cannot be negative
  stopifnot(n_var >= 0)

  # create empty matrix and data frames
  n_sample <- ingredients::select_sample(data, n = N)
  n_sample_changed <- n_sample

  # sample which aspects will be replaced
  new_X <- get_sample(N, length(aspects), sample_method, f)

  # replace aspects
  for (i in seq_len(nrow(n_sample))) {
    vars <- unlist(aspects[new_X[i, ] == 1])
    n_sample_changed[i, vars] <- new_observation[vars]
  }

  # calculate change in predictions
  y_changed <- predict_function(x, n_sample_changed) -
    predict_function(x, n_sample)

  # fit linear model/lasso to estimate aspects importance
  colnames(new_X) <- names(aspects)
  new_df <- data.frame(y_changed, new_X)

  if (n_var == 0) {
    lm_model <- lm(y_changed~., data = new_df)
    model_coef <- lm_model$coefficients
  } else {
    x_new_df <- model.matrix(y_changed ~ ., data = new_df)[, -1]
    y_new_df <- y_changed
    glmnet_model <- glmnet(x_new_df, y_new_df, alpha = 1)
    indx <- max(which(glmnet_model$df <= n_var))
    model_coef <- coef(glmnet_model)[, indx]
  }

  #prepare dataframe with results
  res <- data.frame(names(model_coef), unname(model_coef))
  colnames(res) <- c("aspects", "importance")
  res <- res[!res$aspects == "(Intercept)", ]
  res <- res[order(-abs(res$importance)), ]

  for (i in seq_along(aspects)) {
    res$features[i] <- aspects[as.character(res[i, 1])]
    vars <- unlist(res$features[i])
    if (all(sapply(data[, vars], is.numeric)) & length(vars) > 1 & show_cor == T) {
      cor_matrix <- cor(data[, vars], method = "spearman")
      res$min_cor[i] <- min(abs(cor_matrix))
      res$sign[i] <- ifelse(max(cor_matrix) > 0 & min(cor_matrix) < 0,
                            "neg", "pos")
    } else if (show_cor == T) {
      res$min_cor[i] <- NA
      res$sign[i] <- ""
    }
  }

  res$importance <- as.numeric(format(res$importance, digits = 4))
  class(res) <- c("aspect_importance", "data.frame")

  attr(res, "label") <- rep(label, length.out = nrow(res))

  return(res)
}

#' Function for plotting aspect_importance results
#'
#' This function plots the results of aspect_importance.
#'
#' @param x object of aspect_importance class
#' @param bar_width bar width
#' @param aspects_on_axis if TRUE, labels on axis Y show aspect names, oherwise
#'   they show features names
#' @param add_importance if TRUE, plot is annotated with values of aspects
#'   importance
#' @param digits_to_round integer indicating the number of decimal places used
#'   for rounding values of aspects importance shown on the plot
#' @param text_size size of labels annotating values of aspects importance,
#'   if applicable
#' @param ... other parameters
#'
#' @return a ggplot2 object
#'
#' @examples
#' library("DALEX")
#'
#' titanic_imputed$country <- NULL
#'
#' model_titanic_glm <- glm(survived == "yes" ~
#'                          class+gender+age+sibsp+parch+fare+embarked,
#'                          data = titanic_imputed,
#'                          family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                                data = titanic_imputed[,-8],
#'                                y = titanic_imputed$survived == "yes",
#'                                verbose = FALSE)
#'
#' aspects <- list(wealth = c("class", "fare"),
#'                 family = c("sibsp", "parch"),
#'                 personal = c("gender", "age"),
#'                 embarked = "embarked")
#'
#' plot(aspect_importance(explain_titanic_glm,
#'                   new_observation = titanic_imputed[1,],
#'                   aspects = aspects))
#'
#' @import ggplot2
#'
#' @export


plot.aspect_importance <- function(x, ..., bar_width = 10,
                                   aspects_on_axis = TRUE,
                                   add_importance = FALSE,
                                   digits_to_round = 2,
                                   text_size = 3) {

  stopifnot("aspect_importance" %in% class(x))

  importance <- a_sign <- aspects <- features <- hjust <- NULL

  # order bars
  x$aspects <- reorder(x$aspects, abs(x[, 2]), na.rm = TRUE)
  features_ordered <- sapply(x$features, paste0, collapse = ", ")

  # bind aspect_importance data frames
  dfl <- c(list(x), list(...))
  labels_list <- unlist(lapply(dfl, attr, "label"))
  x <- do.call(rbind, dfl)
  x <- cbind(x, labels_list)

  # reformat features list
  if (!aspects_on_axis) {
    x$features <- sapply(x$features, paste0, collapse = ", ")
  }

  # prep data for plotting
  colnames(x)[ncol(x)] <- "label"
  x$a_sign <- ifelse(x$importance > 0, "positive", "negative")
  x$hjust <- ifelse(x$importance > 0, 1.1, -0.1)

  # prep plot
  if (aspects_on_axis) {
    p <- ggplot(x, aes(aspects, ymin = 0, ymax = importance, color = a_sign)) +
      geom_linerange(size = bar_width) +
      facet_wrap(~label, scales = "free_y", nrow = 1)
  } else {
    x$features <- factor(x$features, levels = rev(features_ordered))
    p <- ggplot(x, aes(features, ymin = 0, ymax = importance, color = a_sign)) +
      geom_linerange(size = bar_width) +
    facet_wrap(~label, scales = "free_y", nrow = 1)
  }

  if (add_importance & aspects_on_axis) {
    p <- p + geom_text(aes(x = aspects, y = importance,
                           label = round(importance, digits_to_round),
                           hjust = hjust), vjust = 0.5, color = "#371ea3",
                       size = text_size)
  } else if (add_importance & !aspects_on_axis) {
    p <- p + geom_text(aes(x = features, y = importance,
                           label = round(importance, digits_to_round),
                           hjust = hjust),
                       vjust = 0.5, color = "#371ea3", size = text_size)
  }

  p <- p + coord_flip() +
    ylab("Aspects importance") + xlab("") + theme_drwhy_vertical() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  # plot it
  p
}

#' @export
#' @rdname aspect_importance
lime <- function(x, ...) {
  aspect_importance(x, ...)
}

#' Aspects importance for single aspects
#'
#' Calculates aspect_importance for single aspects (every aspect contains only
#' one feature).
#'
#' @param x an explainer created with the \code{DALEX::explain()} function
#' or a model to be explained.
#' @param data dataset, it will be extracted from \code{x} if it's an explainer
#' NOTE: Target variable shouldn't be present in the \code{data}
#' @param predict_function predict function, it will be extracted from \code{x}
#'   if it's an explainer
#' @param new_observation selected observation with columns that corresponds to
#' variables used in the model, should be without target variable
#' @param N number of observations to be sampled (with replacement) from data
#' @param label name of the model. By default it's extracted from the 'class'
#'   attribute of the model.
#' @param sample_method sampling method in \code{\link{get_sample}}
#' @param n_var how many non-zero coefficients for lasso fitting, if zero than
#'   linear regression is used
#' @param f frequency in in \code{\link{get_sample}}
#' @param ... other parameters
#'
#' @return An object of the class 'aspect_importance'. Contains dataframe that
#'   describes aspects' importance.
#'
#' @examples
#' library("DALEX")
#'
#' model_titanic_glm <- glm(survived == "yes" ~ class + gender + age +
#'                          sibsp + parch + fare + embarked,
#'                          data = titanic_imputed,
#'                          family = "binomial")
#'
#' aspect_importance_single(model_titanic_glm, data = titanic_imputed[,-9],
#'                          new_observation = titanic_imputed[1,-9])
#'
#' @export

aspect_importance_single <- function(x, ...)
  UseMethod("aspect_importance_single")

#' @export
#' @rdname aspect_importance_single

aspect_importance_single.explainer <- function(x, new_observation,
                                               N = 100,
                                               sample_method = "default",
                                               n_var = 0, f = 2, ...) {

  # extracts model, data and predict function from the explainer
  model <- x$model
  data <- x$data
  predict_function <- x$predict_function
  label <- x$label

  # calls target function
  aspect_importance_single.default(x = model, data = data,
                                   predict_function = predict_function,
                                   new_observation = new_observation, N = N,
                                   label = label,
                                   sample_method = sample_method,
                                   n_var = n_var, f = f)
}

#' @export
#' @rdname aspect_importance_single

aspect_importance_single.default <- function(x, data,
                                             predict_function = predict,
                                             new_observation, N = 100,
                                             label = class(x)[1],
                                             sample_method = "default",
                                             n_var = 0,
                                             f = 2, ...) {

  #create aspect list
  single_aspect_list <- vector("list", length(colnames(data)))
  names(single_aspect_list) <- colnames(data)

  for (i in seq_along(single_aspect_list)) {
    single_aspect_list[i] <- colnames(data)[i]
  }

  #call aspect importance function
  res_ai <- aspect_importance(x, data, predict_function,
                              new_observation, single_aspect_list, N,
                              label, sample_method, n_var, f)

  #create data frame with results
  res_ai[, 3] <- as.character(res_ai[, 1])
  for (i in c(1:dim(res_ai)[1])) {
    tmp_val <- new_observation[as.character(res_ai[i, 1])]
    if (is.numeric(tmp_val[1, 1])) {
      tmp_val <- round(tmp_val[1, 1], digits = 2)
    } else {
      tmp_val <- as.character(tmp_val[1, 1])
    }
    res_ai[i, 3] <- paste0(res_ai[i, 1], " = ", tmp_val)
  }
  colnames(res_ai)[3] <- "new observation"

  return(res_ai)
}

#' Function for getting binary matrix
#'
#' Function creates binary matrix, to be used in aspect_importance method. It
#' starts with a zero matrix. Then it replaces some zeros with ones. It either
#' randomly replaces one or two zeros per row. Or replace random number of zeros
#' per row - average number of replaced zeros can be controled by parameter f.
#' Function doesn't allow the returned matrix to have rows with only zeros.
#'
#' @param n number of rows
#' @param p number of columns
#' @param sample_method sampling method
#' @param f frequency for binomial sampling
#'
#' @return a binary matrix
#'
#' @importFrom stats rbinom
#'
#' @examples
#'  \dontrun{
#'  get_sample(100,6,"binom",3)
#' }
#' @export
#'
#' @rdname get_sample

get_sample <- function(n, p, sample_method = c("default", "binom"), f = 2) {
  sample_method <- match.arg(sample_method)
  stopifnot(n > 0, p > 0, f > 0)
  x <- matrix(0, n, p)
  if (sample_method == "binom") {
    for (i in 1:n) {
      n_of_changes <- pmax(rbinom(1, p, f / p), 1)
      x[i, unique(sample(1:p, n_of_changes, replace = TRUE))] <- 1
    }
  } else {
    for (i in 1:n) {
      x[i, unique(sample(1:p, 2, replace = TRUE))] <- 1
    }
  }
  return(x)
}


#' Groups numeric features into aspects
#'
#' Divides correlated features into groups, called aspects. Division is based on
#' correlation cutoff level.
#'
#' @param x dataframe with only numeric columns
#' @param p correlation value for cut-off level
#' @param clust_method the agglomeration method to be used,
#' see \code{\link[stats]{hclust}} methods
#' @param draw_tree if TRUE, function plots tree that illustrates grouping
#' @param draw_abline if TRUE, function plots vertical line at cut-off level
#'
#' @return list of aspects
#'
#' @importFrom stats hclust
#' @importFrom stats cor
#'
#' @examples
#' library("DALEX")
#' dragons_data <- dragons[,c(2,3,4,7,8)]
#' group_variables(dragons_data, p = 0.7, clust_method = "complete")
#'
#' @export
#'
#' @rdname group_variables

group_variables <- function(x, p = 0.5, clust_method = "complete",
                            draw_tree = FALSE, draw_abline = TRUE) {
  stopifnot(all(sapply(x, is.numeric)))
  stopifnot(p >= 0, p <= 1)

  # build and cut a tree
  x_hc <- hclust(as.dist(1 - abs(cor(x, method = "spearman"))),
                 method = clust_method)

  res <- custom_tree_cutting(x_hc, p)

  # plot a tree
  if (draw_tree == TRUE) {
    plot(plot_group_variables(x_hc, p, draw_abline))
  }

  return(res)
}


#' Plots tree with correlation values
#'
#' Plots tree that illustrates the results of group_variables function.
#'
#' @param x hclust object
#' @param p correlation value for cutoff level
#' @param show_labels if TRUE, plot will have annotated axis Y
#' @param draw_abline if TRUE, cutoff line will be drawn
#' @param axis_lab_size size of labels on axis Y, if applicable
#' @param text_size size of labels annotating values of correlations
#'
#' @return tree plot
#'
#' @importFrom stats hclust
#' @importFrom ggdendro dendro_data
#' @importFrom ggdendro segment
#' @importFrom ggdendro label
#' @importFrom ggplot2 .pt
#'
#' @examples
#' library("DALEX")
#' dragons_data <- dragons[,c(2,3,4,7,8)]
#' group_variables(dragons_data, p = 0.7, clust_method = "complete",
#'                 draw_tree = TRUE)
#'
#' @export

plot_group_variables <- function(x, p, show_labels = TRUE, draw_abline = TRUE,
                                 axis_lab_size = 10, text_size = 3) {
  stopifnot(p >= 0, p <= 1)
  stopifnot(class(x) == "hclust")

  y <- xend <- yend <- h <- NULL

  #convert tree to dendogram
  dhc <- as.dendrogram(x)
  ddata <- dendro_data(dhc, type = "rectangle")

  #get correlation values
  xy <- ddata$segments[ddata$segments$x == ddata$segments$xend, ][, c(1, 4)]
  xy <- xy[xy$yend != 0, ]
  xy <- cbind(xy, 1 - xy[, 2])
  colnames(xy) <- c("x", "y", "h")

  #build plot
  cor_plot <- ggplot(segment(ddata)) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_text(data = xy, aes(x = x, y = y, label = round(h, digits = 2)),
              hjust = 1.3, size = text_size) +
    coord_flip() +
    scale_y_continuous(expand = c(0.3, 0.3)) +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(colour = "white"),
          axis.ticks.x = element_blank(),
          axis.title.x = theme_drwhy()$axis.title,
          panel.background = element_rect(fill = "white"),
          panel.grid = element_blank()) +
    labs(y = "Spearman correlations")

  if (show_labels) {
    cor_plot <- cor_plot +
      geom_text(aes(x = x, y = y, label = label, hjust = 1),
                data = label(ddata), nudge_y = -0.01,
                size = axis_lab_size / .pt,
                colour = theme_drwhy()$axis.title$colour)
  }

  #add line that shows correlation cut off level
  if (draw_abline == TRUE) {
    cor_plot <- cor_plot + geom_hline(yintercept = 1 - p, linetype = "dashed")
  }

  attr(cor_plot, "order") <- x$order
  attr(cor_plot, "labels") <- x$labels

  return(cor_plot)
}


#' Custom tree cutting
#'
#' This function creates aspect list after cutting a tree at a given height.
#'
#' @param x hclust object
#' @param h correlation value for tree cutting
#'
#' @return dataframe with aspect
#'
#' @importFrom stats cutree
#'
#' @noRd

custom_tree_cutting <- function(x, h) {
  val <- NULL
  clust_list <- cutree(x, h = 1 - h)

  #prepare a list with aspects grouping
  df <- data.frame(names(clust_list), unname(clust_list))
  colnames(df) <- c("name", "val")
  res <- vector("list", max(clust_list))
  names(res) <- paste0("aspect.group", seq_along(res))

  for (i in seq_along(res)) {
    res[i] <- list(as.character(subset(df, val == i)$name))
  }

  return(res)
}

#' Function plots tree with aspect importance values
#'
#' This function plots tree that shows order of feature grouping and aspect
#' importance values of every newly created aspect.
#'
#' @param x a model to be explained
#' @param data dataset, should be without target variable
#' @param predict_function predict function
#' @param new_observation selected observation with columns that corresponds to
#'   variables used in the model, should be without target variable
#' @param N number of observations to be sampled (with replacement) from data
#' @param clust_method the agglomeration method to be used, see
#'   \code{\link[stats]{hclust}} methods
#' @param absolute_value if TRUE, aspect importance values will be drawn as
#'   absolute values
#' @param cumulative_max if TRUE, aspect importance shown on tree will be max
#'   value of children and node aspect importance values
#' @param show_labels if TRUE, plot will have annotated axis Y
#' @param axis_lab_size size of labels on axis Y, if applicable
#' @param text_size size of labels annotating values of aspects importance
#'
#' @return ggplot
#'
#' @import stats
#' @import ggplot2
#' @importFrom ggdendro dendro_data
#' @importFrom ggdendro segment
#' @importFrom ggdendro label
#'
#' @examples
#' library(DALEX)
#' apartments_num <- apartments[,unlist(lapply(apartments, is.numeric))]
#' apartments_num_lm_model <- lm(m2.price ~ ., data = apartments_num)
#' apartments_num_new_observation <- apartments_num[2,-1]
#' apartments_num_mod <- apartments_num[,-1]
#' plot_aspects_importance_grouping(x = apartments_num_lm_model,
#' data = apartments_num_mod, new_observation = apartments_num_new_observation)
#'
#'
#' @export
#'


plot_aspects_importance_grouping <- function(x, data,
                                             predict_function = predict,
                                             new_observation, N = 100,
                                             clust_method = "complete",
                                             absolute_value = FALSE,
                                             cumulative_max = FALSE,
                                             show_labels = TRUE,
                                             axis_lab_size = 10,
                                             text_size = 3) {

  #building additional objects
  y <- xend <- yend <- yend_val <- NULL

  aspect_importance_leaves <- aspect_importance_single(x, data,
                                                       predict_function,
                                                       new_observation, N)
  x_hc <- hclust(as.dist(1 - abs(cor(data, method = "spearman"))),
                 method = clust_method)
  cutting_heights <- x_hc$height
  aspects_list_previous <-  custom_tree_cutting(x_hc, 1)
  int_node_importance <- as.data.frame(NULL)

  #calculating aspect importance
  for (i in c(1:(length(cutting_heights) - 1))) {

    aspects_list_current <- custom_tree_cutting(x_hc, 1 - cutting_heights[i])

    t1 <- match(aspects_list_current, setdiff(aspects_list_current,
                                             aspects_list_previous))
    t2 <- which(t1 == 1)
    t3 <- aspects_list_current[t2]
    group_name <- names(t3)

    res_ai <- aspect_importance(x = x, data = data,
                                predict_function = predict_function,
                                new_observation = new_observation,
                                aspects = aspects_list_current, N = N)

    int_node_importance[i, 1] <- res_ai[res_ai$aspects == group_name, ]$importance
    int_node_importance[i, 2] <- group_name
    int_node_importance[i, 3] <- cutting_heights[i]
    aspects_list_previous <- aspects_list_current
  }

  int_node_importance[length(cutting_heights), 1] <- NA

  #inserting importance values into x_hc tree
  x_hc$height <- int_node_importance$V1

  #modifing importance if cumulative_max/absolute_value are true
  if (cumulative_max == TRUE) {
    for (i in seq_along(x_hc$height)) {
      if (x_hc$merge[i, 1] < 0) {
        a1 <- aspect_importance_leaves[
          aspect_importance_leaves[, 1] == x_hc$labels[-x_hc$merge[i, 1]], ]$importance
      } else {
        a1 <- x_hc$height[x_hc$merge[i, 1]]
      }
      if (x_hc$merge[i, 2] < 0) {
        a2 <- aspect_importance_leaves[
          aspect_importance_leaves[, 1] == x_hc$labels[-x_hc$merge[i, 2]], ]$importance
      } else {
        a2 <- x_hc$height[x_hc$merge[i, 2]]
      }
      a3 <- x_hc$height[i]

      if (absolute_value == TRUE) {
        x_hc$height[i] <- max(abs(a1), abs(a2), abs(a3))
      } else {
        x_hc$height[i] <- max(a1, a2, a3)
      }
    }}

  #buidling dendogram
  dend_mod <- NULL
  dend_mod <- as.dendrogram(x_hc, hang = -1)
  ddata <- dendro_data(dend_mod, type = "rectangle")

  #preparing labels for aspect_importance values
  ai_labels <-  na.omit(segment(ddata))
  ai_labels <- ai_labels[ai_labels$yend != 0, ]
  ai_labels$yend_val <- ai_labels$yend

  #removing values of aspect_importance for single aspects
  cc_vector <- !complete.cases(ddata$segments)
  ddata$segments[cc_vector, ] <- 1
  ddata$segments[min(which(cc_vector == TRUE)), c(1, 3)] <- min(ddata$labels$x)
  ddata$segments[min(which(cc_vector == TRUE)) + 1, c(1, 3)] <- max(ddata$labels$x)

  #modifing importance if absolute_value is true
  if (absolute_value == TRUE) {
    ddata$segments$y <- abs(ddata$segments$y)
    ddata$segments$yend <- abs(ddata$segments$yend)
    ai_labels$yend_val <- abs(ai_labels$yend)
  }

  #adding new observation values to labels
  ddata$labels[, 3] <- as.character(ddata$labels[, 3])
  for (i in seq_along(ddata$labels[, 3])) {
    ddata$labels[i, 3] <- paste0(ddata$labels[i, 3], " = ",
                                round(new_observation[ddata$labels[i, 3]],
                                      digits = 2))
  }

  #moving labels
  nudge_value <- ifelse(min(ddata$segments$yend) == 0, -0.2,
                        min(ddata$segments$yend) * 1.35)

  #building plot
  p <- ggplot(segment(ddata)) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_text(data = ai_labels, aes(x = x, y = yend_val,
                                    label = round(yend, digits = 2)),
              hjust = 1.3, size = text_size) +
    coord_flip() +
    scale_y_continuous(expand = c(.3, .3)) +
    theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(colour = "white"),
      axis.title.x = theme_drwhy()$axis.title,
      panel.background = element_rect(fill = "white"),
      panel.grid = element_blank()
    ) +
    labs(y = "Aspect importance for correlated variables")

  if (show_labels) {
    p <- p + geom_text(aes(x = x, y = y, label = label, hjust = 1),
                           data = label(ddata),
                           colour = theme_drwhy()$axis.title$colour,
                           size = axis_lab_size / .pt, nudge_y = nudge_value)
  }

  return(p)
}

#' Three plots that sum up automatic aspect importance grouping
#'
#' This function shows: \itemize{ \item plot for aspect_importance with
#' single aspect \item tree that shows aspect_importance for every newly
#' expanded aspect
#' \item clustering tree. }
#'
#' @param x an explainer created with the \code{DALEX::explain()} function
#' or a model to be explained.
#' @param data dataset, it will be extracted from \code{x} if it's an explainer
#' NOTE: Target variable shouldn't be present in the \code{data}
#' @param predict_function predict function, it will be extracted from \code{x}
#'   if it's an explainer
#' @param new_observation selected observation with columns that corresponds to
#'   variables used in the model, should be without target variable
#' @param N number of rows to be sampled from data
#' @param clust_method the agglomeration method to be used, see
#'   \code{\link[stats]{hclust}} methods
#' @param absolute_value if TRUE, aspect importance values will be drawn as
#'   absolute values
#' @param cumulative_max if TRUE, aspect importance shown on tree will be max
#'   value of children and node aspect importance values
#' @param add_importance_labels if TRUE, first plot is annotated with values of aspects
#'   importance
#' @param show_axis_y_duplicated_labels if TRUE, every plot will have annotated
#'   axis Y
#' @param abbrev_labels if greater than 0, labels for axis Y in single aspect
#'   importance plot will be abbreviated according to this parameter
#' @param axis_lab_size size of labels on axis
#' @param text_size size of labels annotating values of aspects importance and
#'   correlations
#' @param ... other parameters
#'
#' @import stats
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#'
#' @examples
#' library(DALEX)
#' apartments_num <- apartments[,unlist(lapply(apartments, is.numeric))]
#' apartments_num_lm_model <- lm(m2.price ~ ., data = apartments_num)
#' apartments_num_new_observation <- apartments_num[30,-1]
#' apartments_num_mod <- apartments_num[,-1]
#' triplot(x = apartments_num_lm_model,
#'   data = apartments_num_mod,
#'   new_observation = apartments_num_new_observation,
#'   add_importance_labels = FALSE)
#'
#'
#' @export

triplot <- function(x, ...)
  UseMethod("triplot")

#' @export
#' @rdname triplot

triplot.explainer <- function(x, new_observation, N = 500,
                              clust_method = "complete",
                              absolute_value = FALSE, cumulative_max = FALSE,
                              add_importance_labels = TRUE,
                              show_axis_y_duplicated_labels = FALSE,
                              axis_lab_size = 10,
                              text_size = 3,
                              ...) {

  # extracts model, data and predict function from the explainer
  data <- x$data
  model <- x$model
  predict_function <- x$predict_function

  # calls target function
  triplot.default(model, data, predict_function, new_observation, N,
                  clust_method, absolute_value = FALSE, cumulative_max = FALSE,
                  add_importance_labels, show_axis_y_duplicated_labels,
                  axis_lab_size = axis_lab_size, text_size = text_size)
}

#' @export
#' @rdname triplot


triplot.default <- function(x, data, predict_function = predict, new_observation,
                            N = 500, clust_method = "complete",
                            absolute_value = FALSE, cumulative_max = FALSE,
                            add_importance_labels = TRUE,
                            show_axis_y_duplicated_labels = FALSE,
                            abbrev_labels = 0,
                            axis_lab_size = 10,
                            text_size = 3,
                            ...) {

  stopifnot(all(sapply(data, is.numeric)))

  # Build plot 2
  p2 <- plot_aspects_importance_grouping(x, data, predict_function,
                                         new_observation, N, clust_method,
                                         absolute_value,
                                         cumulative_max,
                                         show_labels = show_axis_y_duplicated_labels,
                                         axis_lab_size = axis_lab_size,
                                         text_size = text_size)
  p2$labels$y <- "Hierarchical aspect importance"

  p2 <- p2 + theme(axis.title = element_text(size = axis_lab_size))

  # Build plot 3
  x_hc <- hclust(as.dist(1 - abs(cor(data, method = "spearman"))),
                 method = clust_method)
  p3 <- plot_group_variables(x_hc, 0, show_labels = show_axis_y_duplicated_labels,
                             draw_abline = FALSE, text_size = text_size,
                             axis_lab_size = axis_lab_size)
  p3 <- p3 + theme(axis.title = element_text(size = axis_lab_size))

  # Build plot 1
  aspect_importance_leaves <- aspect_importance_single(x, data,
                                                       predict_function,
                                                       new_observation, N,
                                                       label = "")
  p1 <- plot(aspect_importance_leaves, add_importance = add_importance_labels,
             text_size = text_size)
  p1$labels$y <- "Single aspects importance"
  if (abbrev_labels > 0) {
    p1$data$`new observation` <- abbreviate(p1$data$`new observation`,
                                            minlength = abbrev_labels)
  }
  order_mod <- attr(p3, "labels")[reorder(attr(p3, "labels"), attr(p3, "order"))]
  order_mod <-  match(order_mod, p1$data$aspects)
  lev_mod <- p1$data$`new observation`[order_mod]
  p1$data$`new observation` <- factor(p1$data$`new observation`,
                                      levels = lev_mod)
  p1$data$aspects <- p1$data$`new observation`
  p1 <- p1 + theme(axis.text = element_text(size = axis_lab_size),
                   axis.title = element_text(size = axis_lab_size)) +
    scale_x_discrete(expand = expand_scale(mult = .01))

  # Plot
  plot_list <- list(p1, p2, p3)
  do.call("grid.arrange", c(plot_list, nrow = 1, top = "Triplot"))

}
