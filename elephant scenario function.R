elephant_scenario= function(mortality.rate) {
	#program to run scenarios for no poaching, current poaching, and high poaching
	popdata=fread('C:/_Fabio/post-doc/whale economics/central africa value calculation/all parks with agb.csv')
	#change country code ISO2 to lowercase
	popdata$ISO2=str_to_lower(popdata$ISO2)
	#calculate total number of individuals per PA
	popdata$individuals=round(popdata$mergedens*popdata$AED.area)
	#set a minimum population of 2 elephant
	popdata[individuals==0]$individuals=2
	# set target population to 1 elephant/km2
	popdata$target=round(popdata$AED.area)
	#agc-present in T of C/ha > T of CO2/km2 
	popdata$AGCp=popdata$agb*11/3*100
	#turnover of forest - time takes to return to AGC-equilibrium with elephants
	#mortality rate of forest trees
	paste("Tree mortality rate for simulation is",mortality.rate)
	# years needed to completely regenerate the forest
	time.regeneration=100/mortality.rate
	#determine year since decline based on current density
	popdata$time.since.decline=-100*popdata$mergedens+100
	#calculate ratio of time since decline (td) and regeneration time (tr) needed in eq.8 and eq.10
	popdata$td.tr.ratio=popdata$time.since.decline/time.regeneration
	#the AGC-equilibrium is calculated from the current AGC and accounting for the time since decline and the regeneration time of the forest. If the time since decline is longer than regeneration time we assume all elephant contribution to AGC has been lost, otherwise the fraction lost is calculated based on the ratio between decline and regeneration time
	#eq. 8
	popdata$AGCe=0
	for (i in 1:nrow(popdata)) {
	    popdata[i]$AGCe = ifelse (popdata[i]$td.tr.ratio>=1, popdata[i]$AGCp*(1+0.0014*popdata[i]$time.since.decline), popdata[i]$AGCp*(1+0.0014*popdata[i]$time.since.decline*popdata[i]$td.tr.ratio))
	}

	#====tree mortality rate 1.5%
	#****natural elephant growth rate 0.0361 - no poaching
	#run the population model to determine the time to reach 1 elephant/km2
	print("No poaching")
	print("----Estimating carrying capacity-----")
	popdata$time.to.1elephkm2=time_to_carryingcapacity(dato=popdata,growth.rate=0.0361)
	print("-----Calculating carbon value-------")
	tmp=poaching_scenario(dato=popdata,growth.rate=0.0361)
	print("-----Building time series-------")
	tmp.ts.natural=create_ts(dato=tmp,label='No poaching')

	#save the 1st year, 100th year carbon value and the 1000th, and convert into millions
	#this represents the value of the current elephant population
	popdata$proj.initial=data.table::transpose(tmp$totale)[[1]]/10^6
	popdata$proj.100=data.table::transpose(tmp$totale)[[100]]/10^6
	popdata$proj.max=data.table::transpose(tmp$totale)[[2000]]/10^6
	popdata$individuals.100=round(data.table::transpose(tmp$popolo)[[100]])
	#popdata$corpo=round(tmp$corpo/10^6,2)
	#calculate the yearly change in AGC in CO2
	popdata[, r := (AGCe - AGCp)/time.to.1elephkm2]
	#maximum attainable carbon gain
	popdata[, carbon.max := (AGCe - AGCp)*AED.area]
	#carbon gain after 100 years in T of CO2
	popdata[, carbon.100 := r*100*AED.area]
	#when time to reach 1eleph/km2 is < 100 year maximum carbon is reached before 100. In those cases we set carbon.100 to carbon.max meaning that all the carbon gain from elephants is realized
	popdata[carbon.100>carbon.max]$carbon.100=popdata[carbon.100>carbon.max]$carbon.max

	#****current poaching - elephant growth rate 0.019
	print("Current poaching")
	popdata$time.to.1elephkm2=time_to_carryingcapacity(dato=popdata,growth.rate=0.019)
	#value and population calculation
	tmp=poaching_scenario(dato=popdata,growth.rate=0.019)
	#create time series of total value
	tmp.ts.poaching=create_ts(dato=tmp,label='Current poaching')
	popdata$proj.100.poaching=round(data.table::transpose(tmp$totale)[[100]]/10^6,1)
	#calculate the yearly change in AGC in CO2
	popdata[, r.poaching := (AGCe - AGCp)/time.to.1elephkm2]
	#carbon gain after 100 years
	popdata[, carbon.100.poaching := r.poaching*100*AED.area]
	#same as above for when time to 1 elephant is less than 100 years
	popdata[carbon.100.poaching>carbon.max]$carbon.100.poaching=popdata[carbon.100.poaching>carbon.max]$carbon.max
	#popdata$corpo.poaching=round(tmp$corpo/10^6,2)

	#****heavy poaching - elephant growth rate 0.0095
	print("Heavy poaching")
	popdata$time.to.1elephkm2=time_to_carryingcapacity(dato=popdata,growth.rate=0.0095)
	#value and population calculation
	tmp=poaching_scenario(dato=popdata,growth.rate=0.0095)
	#create time series of total value
	tmp.ts.poaching.heavy=create_ts(dato=tmp,label='Heavy poaching')

	popdata$proj.100.poaching.high=round(data.table::transpose(tmp$totale)[[100]]/10^6,1)
	#calculate the yearly change in AGC in CO2
	popdata[, r.poaching.heavy := (AGCe - AGCp)/time.to.1elephkm2]
	#carbon gain after 100 years
	popdata[, carbon.100.poaching.heavy := r.poaching.heavy*100*AED.area]
	#same as above when time to 1 eleph < 100
	popdata[carbon.100.poaching.heavy>carbon.max]$carbon.100.poaching.heavy=popdata[carbon.100.poaching.heavy>carbon.max]$carbon.max
	
	#now combine the three time series of no poaching and current poaching and high poaching
	ts.totvalue.scenarios=rbind(tmp.ts.natural,tmp.ts.poaching,tmp.ts.poaching.heavy)
	return(list(popdata,ts.totvalue.scenarios))
}