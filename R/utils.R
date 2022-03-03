
load_file <- function(name, path) {
	if(!is.null(name)) {
		csv = vroom::vroom(path, delim = ","),
	}
	else csv<- NULL
	csv
}

