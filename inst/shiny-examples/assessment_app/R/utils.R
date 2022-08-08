# Utils for assessment app

load_file <- function(name, path) {
	if(!is.null(name)) {
		csv = vroom::vroom(path, delim = ",")  %>%
			dplyr::select(-any_of("ID"))
	}
	else csv<- NULL
	csv
}

make_summary<- function(mod, mod_type, pp){
	mod_summ<- summary(mod)
	if(mod_type == "Occ") {
		if(pp==1){
			state<- sum_model(mod_summ, 1)
			detect<- sum_model(mod_summ, 2)
			out<- rbind(state, detect)
		} else {
			state<- sum_model(mod_summ, 1)
			growth<- sum_model(mod_summ, 2)
			surv<- sum_model(mod_summ, 3)
			detect<- sum_model(mod_summ, 4)
			out<- rbind(state,growth,surv,detect)
		}
	}	else if(mod_type %in% c("RN","Nmix")) {
		state<- sum_model(mod_summ, 1)
		detect<- sum_model(mod_summ, 2)
		out<- rbind(state, detect)
	} else if(mod_type == "REST") {
		state<- sum_model(mod_summ, 1)
		detect<- sum_model(mod_summ, 2)
		out<- rbind(state,detect)
	}
	out
}

sum_model<- function(x, id) {
	tab<- x[[id]]
	tab<- data.frame("Type"=names(x)[id],"Covariate"=row.names(tab), tab)
	row.names(tab)<- NULL
	names(tab)[6]<- "P"
	tab
}

make_abund<- function(mod, mod_type, pp){
	if(mod_type %in% c("Occ")) {
		if(pp == 1) {
			Nhat<- calcN(mod)
			out<- data.frame(Parameter = "Occupancy", Session=1, Nhat$Nhat)
			row.names(out)<- NULL
		}
		else {
			Nhat<- calcN(mod)
			tmp<- Nhat$Nhat
			seas<- tmp[['.season']]
			tmp[['.season']]<- NULL
			out<- data.frame(Parameter = "Occupancy", Session=seas, tmp)
			row.names(out)<- NULL
		}
	}
	if(mod_type %in% c("RN","Nmix")) {
		if(pp == 1) {
			Nhat<- calcN(mod)
			out<- data.frame(Parameter = "Total", Session=1, Nhat$Nhat)
			row.names(out)<- NULL
		}
		else {
				Nhat<- calcN(mod)
				tmp<- Nhat$Nhat
				seas<- tmp[['.season']]
				tmp[['.season']]<- NULL
				out<- data.frame(Parameter = "Total", Session=seas, tmp)
				row.names(out)<- NULL
			}
		}
	out
}

sum_abund<- function(x, id) {
	tab<- x[[id]]
	if(id==1) par<- "Total" else par<- "Residual"
	tab<- data.frame("Parameter"=par, tab)
	row.names(tab)<- NULL
	tab
}


unstack.data<- function(df) {
	# helper function to take multi-session df and produce one
	# wide matrix with dimensions M x JT
	# df must have a column 'session' with at least 2 unique values
	y<- split(df, ~factor(session))
	y<- lapply(y, function(x) x[setdiff(names(x),"session")])
	T<- length(y)
	M<- max(sapply(y, nrow))
	J<- max(sapply(y, ncol))
	list(y=y,M=M,J=J,T=T)
}
