

load_file <- function(name, path) {
	if(!is.null(name)) {
		csv = vroom::vroom(path, delim = ",")  %>%
			dplyr::select(-any_of("ID"))
	}
	else csv<- NULL
	csv
}

make_summary<- function(mod, mod_type){
	mod_summ<- summary(mod)
	if(mod_type == "remGP") {
		if("efitGPlist" %in% class(mod)) n<- length(mod)
		else
			n<- 1
		if(n > 1){
			state<- mod_summ %>% map_dfr(sum_model, 1, .id="Session")
			catch<- mod_summ %>% map_dfr(sum_model, 2, .id="Session")
		}
		else{
			state<- sum_model(mod_summ, 1)
			catch<- sum_model(mod_summ, 2)
		}
		out<- rbind(state, catch)
	} else if(mod_type == "remGPI") {
		if("efitGPlist" %in% class(mod)) n<- length(mod)
		else
			n<- 1
		if(n > 1){
			state<- mod_summ %>% map_dfr(sum_model, 1, .id="Session")
			catch<- mod_summ %>% map_dfr(sum_model, 2, .id="Session")
			det<- mod_summ %>% map_dfr(sum_model, 3, .id="Session")
		}
		else{
			state<- sum_model(mod_summ, 1)
			catch<- sum_model(mod_summ, 2)
			det<- sum_model(mod_summ, 3)
		}
		out<- rbind(state, catch, det)
	}
	else if(mod_type %in% "remMN") {
		state<- sum_model(mod_summ, 1)
		detect<- sum_model(mod_summ, 2)
		out<- rbind(state, detect)
	}	else if(mod_type %in% "remGRM") {
			state<- sum_model(mod_summ, 1)
			detect<- sum_model(mod_summ, 2)
			mdetect<- sum_model(mod_summ, 3)
			out<- rbind(state, detect, mdetect)
	} else if(mod_type %in% "occMS") {
		state<- sum_model(mod_summ, 1)
		growth<- sum_model(mod_summ, 2)
		surv<- sum_model(mod_summ, 3)
		detect<- sum_model(mod_summ, 4)
		out<- rbind(state,growth,surv,detect)
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
	if(mod_type == "remGP" | mod_type == "remGPI") {
		Nhat<- calcN(mod, CI.calc="norm")
		if("efitGPlist" %in% class(mod)) n<- length(mod)
		else
			n<- 1
		if(n > 1){
			total<- Nhat %>% map_dfr(sum_abund, 1, .id="Session")
			resid<- Nhat %>% map_dfr(sum_abund, 2, .id="Session")
			out<- rbind(total, resid[n,])
		}
		else{
			total<- sum_abund(Nhat, 1)
			resid<- sum_abund(Nhat, 2)
			out<- rbind(total, resid)
			out$Session <- c(1,2)
		}
	} else
		if(mod_type %in% c("remMN","remGRM")) {
			if(pp == 1) {
				Nhat<- calcN(mod)
				total<- data.frame(Parameter = "Total", Session=1, Nhat$Nhat)
				resid<- data.frame(Parameter = "Residual", Session=2, Nhat$Nresid)
				out<- rbind(total, resid)
				row.names(out)<- NULL
			}
		else {
			Nhat<- calcN(mod)
			tmp<- Nhat$Nhat
			seas<- tmp[['.season']]
			tmp[['.season']]<- NULL
			total<- data.frame(Parameter = "Total", Session=seas, tmp)
			resid<- data.frame(Parameter = "Residual", Session=max(seas)+1, Nhat$Nresid)
			out<- rbind(total, resid)
			row.names(out)<- NULL
		}
		} else if(mod_type %in% "occMS") {
			Nhat<- calcN(mod)
			tmp<- Nhat$Nhat
			seas<- tmp[['.season']]
			tmp[['.season']]<- NULL
			out<- data.frame(Parameter = "Occupancy", Session=seas, tmp)
			row.names(out)<- NULL
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

