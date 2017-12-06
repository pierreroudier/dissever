#' @include dissever-package.R

# Horrible hack to keep CRAN happy and suppress NOTES about
# parts of the code that use non-standard evaluation.
# See:
# http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
# https://github.com/smbache/magrittr/issues/29
utils::globalVariables(c(
  "cell",
  "diss",
  ".",
  "matches",
  "i"
))

# simple wrapper around raster::as.data.frame that
# handles categorical data columns correctly
.as_data_frame_factors <- function(x, ...) {

  # We only need to do something if
  # there is categorical data in the stack
  if (any(is.factor(x))) {

    # idx <- which(is.factor(x))
    #
    # lapply(idx, function(i) {
    #   r <- x[[i]]
    #   rat <- levels(r)[[1]]
    #
    # })

    # Get data.frame
    res <- as.data.frame(x, ...)

    # Get names of original stack
    # (not affected by bug)
    nm <- names(x)

    # Test if coordinates have been returned
    args <- list(...)

    if("xy" %in% names(args)) {
      xy <- args[['xy']]
    } else {
      xy <- FALSE
    }

    # If coordinates asked to be returned, we need
    # to take this into account
    if (xy) {
      names(res)[-1*1:2] <- nm
    } else {
      names(res) <- nm
    }

  } else {
    res <- as.data.frame(x, ...)
  }

  res
}

.join_interpol <- function(coarse_df, fine_df, attr, by = 'cell'){
  # Nearest-neighbour interpolation as an inner SQL join
  left_join(fine_df, coarse_df , by = by) %>%
    select(matches(attr))
}

.create_lut_fine <- function(coarse, fine) {
  extract(coarse, coordinates(fine))
}

.default_control_init <- caret::trainControl(
  method = 'cv',
  number = 5#,
  #verboseIter = TRUE
)

# fits one model to the entire training set
.default_control_iter <- caret::trainControl(method = 'none')

.create_tune_grid <- function(model, tune_length) {
  params <- modelLookup(model)$parameter
  grid <- expand.grid(lapply(1:length(params), function(x) 1:tune_length))
  names(grid) <- as.character(params)
  grid
}

# Computes the regression model between some coarse data and
# the stack of covariates
.update_model <- function(x, y, method = 'rf', control, tune_grid){
  # Picks the parameters of the model using RMSE on the first run
  # Then use the optimised parameters in the iteration loop to save
  # on computing time
  # Basically we just need to change the trainControl object to do that

  # *** Ensemble modelling ***
#   if (length(method > 1)) {
#     require(caretEnsemble)
#
#     # Create the list of caret models to fit
#     models <- lapply(method, function(x) {
#       caretModelSpec(method = x, tuneGrid = tune_grid)
#     })
#     # Use the model names to index the models in list
#     names(models) <- method
#
#     # create CaretList
#     model_list <- caretList(
#       x = x,
#       y = y,
#       trControl = control,
#       tuneList = models
#     )
#
#     fit <- caretEnsemble(model_list)
#
#   } else {

    # if there is only one layer, we need to convert the numeric vector to a one
    # column data.frame (https://stackoverflow.com/questions/32462588/error-in-train-from-caret)
    # See Issue #4: https://github.com/pierreroudier/dissever/issues/4
    if (is.null(dim(x))) {
      x <- data.frame(x)
    }

    fit <- train(
      x = x,
      y = y, # in this case train needs a vector
      method = method,
      trControl = control,
      tuneGrid  = tune_grid
    )
  # }

  fit
}

.has_parallel_backend <- function() getDoParWorkers() > 1

.get_split_idx <- function(n, p) sort(1:n %% p)

# Generates prediction intervals using bootstraping
#
.bootstrap_ci <- function(fit, fine_df, level = 0.9, n = 50L) {

  # training data
  df <- fit$trainingData

  # Regression method
  reg_method <- fit$method

  # Bootstrap
  boot_samples <- boot(df, function(data, idx, method = reg_method) {

    bootstrap_df <- data[idx, ]

    bootstrap_fit <- train(
      .outcome ~ .,
      data = bootstrap_df,
      method = method,
      trControl = trainControl(method = "none"),
      tuneGrid = fit$bestTune)

    # generate predictions
    predict(bootstrap_fit, fine_df)

  }, n)

  # generate CI estimates + mean

  # If level is a number < 1
  if (is.numeric(level)) {
    ci <- c((1 - level) / 2, 1 - (1 - level) / 2)
    res <- data.frame(
      lower = aaply(boot_samples$t, 2, quantile, probs = ci[1]),
      mean = aaply(boot_samples$t, 2, mean),
      upper = aaply(boot_samples$t, 2, quantile, probs = ci[2])
    )
    # if level is a function
  } else if (is.function(level)) {
    res <- data.frame(
      mean = aaply(boot_samples$t, 2, mean),
      uncert = aaply(boot_samples$t, 2, level)
    )
    # else we throw an error
  } else {
    stop('Incorrect value for the "level" option.')
  }

  res
}

.generate_ci <- function(object, covariates, level = 0.9, n = 50L) {

  fine_df <- na.exclude(.as_data_frame_factors(covariates, xy = TRUE))

  b <- .bootstrap_ci(object$fit, fine_df, level = 0.9, n = 50L)

  res <- rasterFromXYZ(
    data.frame(
      fine_df[, 1:2],
      b
    ),
    res = res(covariates),
    crs = projection(covariates)
  )

  res
}

.predict_map <- function(fit, data, split = NULL, boot = NULL, level = 0.9) {
  if (.has_parallel_backend()) {
    # Get number of registered workers
    n_workers <- length(unique(split))

    # The split vector is assigning a cmputing core to
    # each sample in `data`
    if (n_workers < 1) stop('Wrong split vector')

    # Parallel prediction
    res <- foreach(
      i = 0:(n_workers - 1),
      .combine = c,
      .packages = 'caret'
    ) %dopar% {
      if (is.null(boot)) {
        predict(fit, newdata = data[split == i, ])
      } else {
        # Use bootstraping to get confidence intervals
        .bootstrap_ci(fit = fit, fine_df = data[split == i, ], level = level, n = boot)
      }
    }
  } else {
    if (is.null(boot)) {
      res <- predict(fit, data)
    } else {
      # Use bootstraping to get confidence intervals
      res <- .bootstrap_ci(fit = fit, fine_df = data, level = level, n = boot)
    }
  }

  res
}

.dissever <- function(
    coarse,
    fine,
    method = "rf",
    p = 0.5, nmax = NULL,
    thresh = 0.01,
    min_iter = 5,
    max_iter = 20,
    boot = NULL,
    level = 0.9,
    tune_length = 3,
    tune_grid = .create_tune_grid(model = method, tune_length = tune_length),
    train_control_init = .default_control_init,
    train_control_iter = .default_control_iter,
    verbose = FALSE
  ){

  # Stop if resolution of covariates is not higher than resolution of coarse data
  if (min(res(fine)) >= min(res(coarse))) {
    stop('Resolution of fine data should be higher than resolution of coarse data')
  }

  # Store names of coarse data and fine-scale covariates
  nm_coarse <- names(coarse)
  nm_covariates <- names(fine)

  # Get cell numbers of the coarse grid
  ids_coarse <- raster(coarse)
  ids_coarse[] <- 1:ncell(coarse)
  names(ids_coarse) <- 'cell'

  # Convert coarse data to data.frame
  coarse_df <- .as_data_frame_factors(coarse, xy = TRUE)
  # coarse_df$cell <- 1:nrow(coarse_df) # integer
  coarse_df[['cell']] <- as.integer(.create_lut_fine(ids_coarse, coarse))
  coarse_df <- na.exclude(coarse_df)

  # Convert fine data to data.frame
  fine_df <- .as_data_frame_factors(fine, xy = TRUE)
  # Add coarse cell ID to fine data.frame
  fine_df[['cell']] <- as.integer(.create_lut_fine(ids_coarse, fine))
  # Remove NA values
  fine_df <- na.exclude(fine_df)

  # Resampled national model onto fine grid
  fine_df <- cbind(
    fine_df,
    .join_interpol(coarse_df = coarse_df[, c('cell', nm_coarse)], fine_df = fine_df, attr = nm_coarse, by = 'cell')
  )

  # Sub-sample for modelling
  n_spl <- ceiling(nrow(fine_df) * p) # Number of cells to sample

  if (!is.null(nmax) && nmax > 0) {
    n_spl <- min(n_spl, nmax)
  }

  id_spl <- sample(1:nrow(fine_df), size = n_spl) # sample random grid cells

  # Compute initial model
  if (verbose) message('Selecting best model parameters')

  fit <- .update_model(
    x = fine_df[id_spl, nm_covariates],
    y = fine_df[id_spl, nm_coarse, drop = TRUE],
    method = method,
    control = train_control_init,
    tune_grid = tune_grid
  )

  # Getting best setof params
  best_params <- fit$bestTune

  if (verbose) {
    best_params_str <- paste(
      lapply(names(best_params), function(x) paste(x, " = ", best_params[[x]], sep = "")),
      collapse = ", ")
    message("Parameters retained: ", best_params_str)
  }

  # Initiate matrix to store performance of disseveration
  perf <- matrix(ncol = 3, nrow = 0, dimnames = list(NULL,c("lower_rmse", "rmse", "upper_rmse")))

  # Initiate dissever result data.frame
  diss_result <- fine_df[, c('x', 'y', 'cell', nm_coarse)]
  # Our first approximation is actually the nearest neighbour interpolation
  diss_result$diss <- fine_df[[nm_coarse]]

  # Initiate dissever results data.frame aggregated back to coarse grid
  diss_coarse <- coarse_df
  diss_coarse$diss <- coarse_df[[nm_coarse]]

  # Initialising best model selection
  best_fit <- Inf

  # If parallel computing: Get split vector, assigning each row in fine data
  # to a core (for prediction of models on fine grid)
  if (.has_parallel_backend()) {
    n_cores <- getDoParWorkers()
    split_cores <- .get_split_idx(nrow(fine_df), p = n_cores)
  } else {
    split_cores <- NULL
  }

  for (k in 1:max_iter){

    if (verbose) message('| - iteration ', k)
    # if (verbose) message('| -- computing adjustement factor')

    # Calculate adjustment factor
    diss_coarse$adjust <- diss_coarse[[nm_coarse]] / diss_coarse[['diss']]

    # Resample adjustement factor to fine grid
    diss_result$adjust <- .join_interpol(diss_coarse, fine_df, attr = 'adjust', by = 'cell')[, 'adjust']

    # Apply adjustement and replace the current
    diss_result$diss <- diss_result$adjust * diss_result$diss

    # Update model
    if (verbose) message('| -- updating model')

    # Sampling points
    # id_spl <- sample(1:nrow(fine_df), size = n_spl)

    fit <- .update_model(
      x = fine_df[id_spl, nm_covariates],
      y = diss_result[id_spl, 'diss', drop = TRUE],
      method = method,
      control = train_control_iter,
      tune_grid = best_params
    )

    if (verbose) message('| -- updating predictions')

    # Update dissever predictions on fine grid
    diss_result$diss <- .predict_map(fit, fine_df[, nm_covariates], split = split_cores, boot = NULL)

    # if (verbose) message('| -- averaging prediction on coarse grid')

    # Aggregate average prediction on coarse grid
    diss_coarse <- diss_result %>%
      group_by(cell) %>%
      summarise(diss = mean(diss)) %>%
      inner_join(coarse_df, ., by = "cell")

    # if (verbose) message('| -- computing performance stats')

    # RMSE
    n <- nrow(diss_coarse)
    sqe <- (diss_coarse[[nm_coarse]] - diss_coarse[['diss']])^2
    mse <- mean(sqe)
    rmse <- sqrt(mse) # RMSE

    # Confidence intervals
    # In this case we use 5% C.I.
    t_student <- qt(1 - (0.05/2), df = n - 1) # 0.975 quantile from Student t distribution
    var <- ((1)^2/(n * (n - 1))) * sum(sqe) # Variance
    se <- sqrt(var) # Standard error
    ci <- se * t_student
    upper_ci <- mse + ci
    lower_ci <- max(0, mse - ci)

    # Fill result matrix
    perf <- rbind(perf, c(sqrt(lower_ci), rmse, sqrt(upper_ci)))

    if (verbose) message("| -- RMSE = ", round(rmse, 3))

    # Choose whether we retain the model or not
    if (rmse < best_fit){
      best_model <- fit
      best_fit <- rmse
      best_iteration <- k
    }

    # We only test improvement if more than 5 iterations
    if (k >= min_iter & k >= 3) {

      # Computing stop criterion
      stop_criterion <- mean(
        (perf[k - 2, 2] - perf[k - 1, 2]) + # improvement at last iteration
          (perf[k - 1, 2] - rmse) + # improvement at this iteration
          (perf[k - 2, 2] - rmse) # improvement over last 2 iterations
      )

      # If we have reach some kind of pseudo-optimium,
      # we finish iteration stage
      if (stop_criterion <= thresh) break
    }
  }

  if (verbose) message('Retaining model fitted at iteration ', best_iteration)

  # Create Raster result
  map <- rasterFromXYZ(
    data.frame(
      diss_result[, c('x', 'y')],
      diss = .predict_map(best_model, fine_df[, nm_covariates], split = split_cores, boot = boot, level = level)
    ),
    res = res(fine),
    crs = projection(fine)
  )

  res <- list(
    fit = fit,
    map = map,
    perf = data.frame(perf)
  )

  class(res) <- c(class(res), 'dissever')

  return(res)
}

#' @name plot.dissever
#' @title Plots a dissever result
#' @method plot dissever
#' @description Plots a dissever result. Two modes are possible to visualise either the resulting map or the convergence of the disseveration.
#' @param x object of class \code{dissever}, output from the \code{dissever} function
#' @param type character, type of visualisation to produce. Options are "map", to produce a map of the dissevered coarse map, or "perf", to show the convergence of the RMSE during the disseveration procedure.
#' @param ... Additional arguments passed to plot
#' @author Pierre Roudier
#' @examples
#' # See ?dissever
plot.dissever <- function(x, type = 'map', ...) {

  if (! type %in% c('map', 'perf')) stop('Invalid type of plot.')

  if (type == 'map') {
    raster::plot(x$map, col = viridis(100), ...)
  } else {
    # Get number of iterations
    n_iter <- nrow(x$perf)
    plot(1:n_iter, x$perf$rmse, type = 'l', xlab = 'Iteration', ylab = 'RMSE', ylim = range(x$perf), ...)
    lines(1:n_iter, x$perf$upper_rmse, lty = 3)
    lines(1:n_iter, x$perf$lower_rmse, lty = 3)
    # Get selected model
    best <- which.min(x$perf$rmse)
    points(best, x$perf[best, 'rmse'], pch = 16, col = 2)
  }
}

#' @name print.dissever
#' @title Prints the performance of the dissever procedure
#' @description Prints the performance of the model used to do the dissever procedure.
#' @param x object of class \code{dissever}, output from the \code{dissever} function
#' @param ... Additional arguments passed to print
#' @author Pierre Roudier
print.dissever <- function(x, ...) {
  print(x$fit, ...)
}

#' @name summary.dissever
#' @title Prints summary of the model used in the dissever procedure
#' @description Prints summary of the model used in the dissever procedure.
#' @param object object of class \code{dissever}, output from the \code{dissever} function
#' @param ... Additional arguments passed to summary
#' @author Pierre Roudier
summary.dissever <- function(object, ...) {
  summary(object$fit, ...)
}

if(!isGeneric("generate_ci")) {
  setGeneric("generate_ci", function(object, covariates, ...) {
    standardGeneric("generate_ci")
  })
}

#' @name generate_ci
#' @aliases generate_ci,list,RasterStack-method
#' @title Confidence intervals using bootstraping
#' @description Generates confidence intervals of a dissever output using bootstraping
#' @param object object of class \code{dissever}, output from the \code{dissever} function
#' @param covariates object of class \code{"RasterStack"}, the fine-resolution stack of predictive covariates used to generate the dissever output
#' @param level If this is a numeric value, it is used to derive confidence intervals using quantiles. If it is a function, it is used to derive the uncertainty using this function.
#' @param n the number of bootstrap replicates used to derive the confidence intervals
#' @docType methods
#' @author Pierre Roudier
#' @examples
#' # Load the Edgeroi dataset (see ?edgeroi)
#' data(edgeroi)
#'
#' # Create a dissever output
#' diss <- dissever(
#'   coarse = edgeroi$carbon,
#'   fine = edgeroi$predictors,
#'   method = "lm",
#'   min_iter = 5, max_iter = 10,
#'   p = 0.05
#' )
#'
#' # Generate the confidence intervals
#' \dontrun{
#' ci <- generate_ci(diss, edgeroi$predictors, n = 5)
#'
#' plot(ci)
#' }
setMethod(
  'generate_ci',
  signature(object = "list", covariates = "RasterStack"),
  .generate_ci
)

if(!isGeneric("dissever")) {
  setGeneric("dissever", function(coarse, fine, ...) {
    standardGeneric("dissever")
  })
}

#' @title Spatial downscaling
#' @name dissever
#' @aliases dissever,RasterLayer,RasterStack-method dissever,RasterLayer,RasterLayer-method
#' @description Performs spatial downscaling of coarse grid mapping to fine grid mapping using predictive covariates and a model fitted using the caret package.
#' @param coarse object of class \code{"RasterLayer"}, the coarse-resolution layer that needs to be downscaled
#' @param fine object of class \code{"RasterStack"}, the fine-resolution stack of predictive covariates
#' @param method a string specifying which classification or regression model to use (via the caret package). Possible values are found using names(caret::getModelInfo()).
#' @param p numeric, proportion of the fine map that is sampled for fitting the dissever model (between 0 and 1, defaults to 0.5)
#' @param nmax numeric maximum number of pixels selected for fitting the dissever model. It will override the number of pixels chosen by the \code{p} option if that number is over the value passed to \code{nmax}.
#' @param thresh numeric, dissever iterations will proceed until the RMSE of the dissever model reaches this value, or until the maximum number of iterations is met (defaults to 0.01)
#' @param min_iter numeric, minimum number of iterations (defaults to 5)
#' @param max_iter numeric, maximum number of iterations (defaults to 20)
#' @param boot numeric, if not NULL (default), the number of bootstrap replicates used to derive the confidence intervals.
#' @param level If this is a numeric value, it is used to derive confidence intervals using quantiles. If it is a function, it is used to derive the uncertainty using this function.
#' @param tune_length numeric, the number of parameters to test to find the optimal parametrisation of the caret model (defaults to 3)
#' @param tune_grid a data frame with possible tuning values
#' @param train_control_init Control parameters for finding the optimal parameters of the caret model (see trainControl)
#' @param train_control_iter Control parameters for fitting the caret model during the iteration phase (see trainControl)
#' @param verbose controls the verbosity of the output (TRUE or FALSE)
#' @docType methods
#' @author Brendan Malone, Pierre Roudier
#' @references Malone, B.P, McBratney, A.B., Minasny, B., Wheeler, I., (2011) A general method for downscaling earth resource information. Computers & Geosciences, 41: 119-125. \url{http://dx.doi.org/10.1016/j.cageo.2011.08.021}
#' @examples
#' # Load the Edgeroi dataset (see ?edgeroi)
#' data(edgeroi)
#'
#' # Plot the Edgeroi dataset (using the raster package)
#' library(raster)
#' plot(edgeroi$carbon) # coarse resolution layer
#' plot(edgeroi$predictors) # fine resolution predictors
#'
#' # Run dissever using a simple linear model.
#'
#' # In this instance we are subsampling heavily (p = 0.05) to keep
#' # run time short
#' res_lm <- dissever(
#'   coarse = edgeroi$carbon,
#'   fine = edgeroi$predictors,
#'   method = "lm",
#'   min_iter = 5, max_iter = 10,
#'   p = 0.05
#' )
#'
#' # A lot more models are available through caret:
#' \dontrun{
#' subset(caret::modelLookup(), forReg == TRUE, select = 'model')
#' }
#'
#' # Plot dissever results
#' plot(res_lm, type = 'map', main = "Dissever using GAM")
#' plot(res_lm, type = 'perf', main = "Dissever using GAM")
#'
setMethod(
  'dissever',
  signature(coarse = "RasterLayer", fine = "RasterLayer"),
  .dissever
)

setMethod(
  'dissever',
  signature(coarse = "RasterLayer", fine = "RasterStack"),
  .dissever
)
