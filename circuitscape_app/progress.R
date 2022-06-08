#' A class recording current progress for a long running task
#'
#' @param shinyProgress
#' @param totalProgressCount
TaskProgress <- R6Class(
    "TaskProgress",
    public=list(
        shinyProgress = NULL,
        totalProgressCount = 0,
        currentProgressCount = 0,
        verbose = FALSE,
        initialize = function(shinyProgress, totalProgressCount, verbose=FALSE) {
            self$shinyProgress = shinyProgress
            self$totalProgressCount = totalProgressCount * 100
            self$verbose = verbose
        },
        incrementProgress = function(incrementAmount) {
            self$currentProgressCount = self$currentProgressCount + incrementAmount
            self$shinyProgress$inc(incrementAmount)
            if (self$verbose) {
                self$printProgress()
            }
        },
        finalizeProgress = function() {
            self$incrementProgress(self$totalProgressCount - self$currentProgressCount)
        },
        printProgress = function() {
            print(glue("currentProgressCount={self$currentProgressCount}; totalProgressCount={totalProgressCount}; difference={totalProgressCount-currentProgressCount}"))
        }
    )
)