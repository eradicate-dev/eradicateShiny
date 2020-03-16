#' @export
run_assessment_app <- function() {
	appDir <- system.file("shiny-examples", "eradication_app", package = "eradicateShiny")
	if (appDir == "") {
		stop("Could not find example directory. Try re-installing `eradicateShiny`.", call. = FALSE)
	}

	shiny::runApp(appDir, display.mode = "normal")
}
