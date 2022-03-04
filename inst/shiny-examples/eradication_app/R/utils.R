

load_file <- function(name, path) {
	if(!is.null(name)) {
		csv = vroom::vroom(path, delim = ",")
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
	} else if(mod_type %in% c("remMN","remGRM")) {
		state<- sum_model(mod_summ, 1)
		detect<- sum_model(mod_summ, 2)
		out<- rbind(state, detect)
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

make_abund<- function(mod, mod_type){
	if(mod_type == "remGP") {
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
		}
	} else
		if(mod_type %in% c("remMN","remGRM")) {
			Nhat<- calcN(mod)
			total<- data.frame(Parameter = "Total", Nhat$Nhat)
			resid<- data.frame(Parameter = "Residual", Nhat$Nresid)
			out<- rbind(total, resid)
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


