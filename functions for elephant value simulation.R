#this function takes in input the information for each protected area (elephant density, AGC equilibrium, AGC current, etc. ) and the growth rate of the population to produce devaluation for each PA given a certain poaching scenario. The evaluation is performed by the elephant_popmodel function for each PA
poaching_scenario=function(dato,growth.rate) {
    dato$totale=list()
    dato$popolo=list()
    dato$corpo=0
    for (i in 1:nrow(dato)) {
        tmp.dato=dato[i,]
        my=elephant_popmodel(dt=tmp.dato,growth.rate=growth.rate)
        #saving the results
        dato$totale[[i]]=my[[1]]
        dato$popolo[[i]]=my[[2]]
        dato$corpo[[i]]=my[[3]]
        cat(i)
    }
    return(dato)
}

elephant_popmodel= function(dt,growth.rate) {

    # Created by Thomas F Cosimano and Fabio Berzaghi.
    # Program creates history of Populations births and deaths over time for elephants.
	# We set the maximum time as 1000 years.
    #time horizon (elephants steady state)
    T=2000
    #time horizon of investment
    th.scenario=100
    #The price of carbon credits is set at historic average in EU Market in 2021.
    #carbon price
    Pcar=51.56
    #Population is from report on protected areas in Central Africa. 
    # target population is based on km^2 in forest and a density of 1. 
    Npw=dt$target
    # current population in each PA
    Nw=dt$individuals
    #population density
    pop.density=dt$mergedens
    # Set interest/discount rate rate to calculate Present values
    dr=0.02
    # The growth rate of the population passed as argument 
    v1=growth.rate
    v1=v1/(1-Nw/Npw)
    
    #carbon calculations
    AGCe = dt$AGCe
    #The starting.carbon is the current AGC average of PA estimated from remote sensing
    AGCp = dt$AGCp
    # Let increase in biomass change over x years depending on the years to return to a pop density of 1 eleph/km2
    Te = dt$time.to.1elephkm2
    # Change in co2 stored in forest per year from increase in elephant density.
    r=(AGCe - AGCp)/Te
    #ratio of td/tr
	td.tr.ratio=dt$td.tr.ratio
	#area in km2 of PA
	area.of.PA=dt$AED.area

    # Logistic  model for Gabon Population
    P=rep(0,T)
    P[1]=Nw
    for (t in 2:T) {

    	P[t]=Nw/ ( (Nw/Npw) * (1-exp(-(v1*(t-1)))) + exp(-(v1*(t-1))) )
    }

    #present value of annuity
    An=rep(NA,T)
    for (i in 1:T) {
        An[i]=(1-(1/(1+dr))^i)/dr
    }
    #revenue
    Rev= Pcar*r

    #First term eq.10 - present value of current population
    PV2t1=ifelse(td.tr.ratio>=1, Pcar*AGCp*pop.density*0.14*area.of.PA, Pcar*AGCp*(pop.density*0.14*(1- td.tr.ratio)*area.of.PA) )
        
    # Calculate present value of future payment. Second term in eq.10 . PV of Future Value
    PV2t2.future=Rev*P[1]*An
    
    # Calculate the 3rd term in eq.10 . Value of change in future generation over 1000 generations. Capital T is the horizon investment
    #annuity over the time to equilbrium
    An.Te=(1-(1/(1+dr))^Te)/dr
    #equation 11.
    Vg=rep(0,T)
    PVg=rep(0,T)
    for (g in 1:(T-1)) {
    	# This is the discount term for each cash payment in eq. 11
    	Temp=1/((1+dr)^(g-1))
    	Vg[g]=Rev*(P[g+1]-P[g])*An.Te*(1+dr)
    	# This is the present value of cash flow each year. 
    	PVg[g]=Vg[g]*Temp
    }

    # We can now calculate the sum of Vg over the investment horizon
    #Third term in eq.10
    PV2t3=rep(0,T)
    for (i in 1:T) {
        PV2t3[i]=sum(PVg[1:i])
    }
    
    # Sum first, second, and third term in eq.10
    #this is the value for investment horizon at th.scenario's years 
    PV2 = PV2t1+PV2t2.future[th.scenario]+PV2t3[th.scenario]
    #this is the time series, it represents the value with an investment horizon at year ith
    PV2.timehorizon = PV2t1+PV2t2.future+PV2t3
    #CO2e (tons of CO2) stored in elephant body 3000kg * 0.24 * 11/3
    Cb=2.640
    #value of carbon in elephant bodies
    RevCb=Pcar*Cb
    #PV1, first term in eq. 6
    V1=rep(0,T)
    V1[1]=RevCb*P[1]
    for (g in 1:(T-1)) {
        # This is the discount term for each cash payment. 
        Temp=1/((1+dr)^(g))
        # This calculates each term for value of carbon capture in elephant bodies. 
        V1[g+1]=V1[g]+RevCb*(P[g+1]-P[g])*Temp
    }
    #Now add value in body with carbon service value
    Total=V1+PV2.timehorizon
    return(list(Total,P,V1[th.scenario]))
}

#this function only calculates the time to reach 1eleph/km2
elephant_popmodel_novalue= function(dt,growth.rate) {
	
    #time horizon (elephants steady state)
    T=10000
    #Population is from report on protected areas in Central Africa. 
    # target population is based on km^2 in forest and a density of 1. 
    Npw=dt$target
    # current population in each PA
    Nw=dt$individuals
    # The growth rate of the population passed as argument 
    v1=growth.rate
    v1=v1/(1-Nw/Npw)

    # Logistic  model for Gabon Population
    P=rep(0,T)
    P[1]=Nw
    for (t in 2:T) {

    	P[t]=Nw/ ( (Nw/Npw) * (1-exp(-(v1*(t-1)))) + exp(-(v1*(t-1))) )
    }
    return(P)
}

#calling this function just to estimate how long it will take for population to reach 1 eleph/km2 in each protected area. In this case, the time.to.equilibrium and the other carbon-related parameters are only used to make the elephant_popmodel function work but are not used.
time_to_carryingcapacity=function(dato,growth.rate) {
	dato$popolo=list()
	for (i in 1:nrow(dato)) {
	#only take the PA of interest 
	tmp.dato=dato[i,]
	#returns the population time series
    dato$popolo[[i]]=elephant_popmodel_novalue(dt=tmp.dato,growth.rate=growth.rate)
    cat(i)
	}
	#now determine the years it takes to reach one elephant per km2
	dato$time.to.1elephkm2=0
	for (i in 1:nrow(dato)) {
		#take the 99th percentile of the target population because the target population is never reached but only approached
		dato$years.1elephkm2[i]=which.max(dato$popolo[[i]]>dato$AED.area[[i]]*.99)
	}
	return(dato$years.1elephkm2)
}

create_ts=function(dato,label) {
	#create time series of total value
	ts=data.table()
	for (i in unique(dato$Country)) {
	    #select country
	    tmp=dato[Country==i]
	    tmp2=data.table::transpose(tmp$totale)
	    tmp3=data.table::transpose(tmp$popolo)
	    #sum value of all protected areas each year
	    result=data.table(unlist(lapply(tmp2, sum)))
	    result.popolo=data.table(unlist(lapply(tmp3, sum)))
	    # add year and country column
	    result$year=1:nrow(result)
	    result$Country=i
	    result$popolo=round(result.popolo)
	    ts=rbind(ts,result)
	}
	names(ts)[1]='Total.value'
	#make time series of total value with sum of all countries
	ts.totvalue=ts[,list(Total.value=sum(Total.value),Total.population=sum(popolo)),by=year]
	ts.totvalue$scenario=label
	return(ts.totvalue)
}