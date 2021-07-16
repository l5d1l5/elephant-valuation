#load data.table package
library(data.table)
#output folder
folderout='C:/_Fabio/post-doc/whale economics/central africa value calculation/plots/'
#load functions
source('C:/_Fabio/post-doc/whale economics/central africa value calculation/elephant scenario function.R')
source('C:/_Fabio/post-doc/whale economics/central africa value calculation/functions for elephant value simulation.R')

#this function will launch the simulation 
s1.5=elephant_scenario(mortality.rate=1.5)
s2=elephant_scenario(mortality.rate=2)
s1=elephant_scenario(mortality.rate=1)
#saving the results of the 1.5 simulation in a data table
popdata=s1.5[[1]]
popdata.s2=s2[[1]]
popdata.s1=s1[[1]]

## ---------- this section is only for making the plots -----
#time series of carbon value
names(s2[[2]])[2]='max'
names(s1[[2]])[2]='min'
maxo=s2[[2]][,2]
mino=s1[[2]][,2]
ts.totvalue.scenarios=cbind(s1.5[[2]],mino,maxo)

#barplot of values - take 100th year
totvalue.barplot=ts.totvalue.scenarios[year==100]

#total.bar.byscenario=
#ggplot(data=totvalue.barplot,aes(x=reorder(scenario,-Total.value)))+geom_boxplot(stat = "identity",aes(y=Total.value/10^9,fill=scenario),alpha=0.9,position="dodge")+ylab("Carbon value (B$)") +theme_bw(base_size=14)+ theme(panel.grid.major.x = element_blank(),legend.position ='bottom',axis.title.x=element_blank())+scale_y_log10() + labs(fill='Poaching intensity') + scale_fill_manual(values=c('#7fc97f','#beaed4','#fdc086')) + geom_errorbar( aes(ymin=min/10^9, ymax=max/10^9), width=0.2, colour="black", alpha=0.4, size=1)

#+ geom_errorbar( aes(ymin=min/10^9, ymax=max/10^9), width=0.2, colour="black", alpha=0.4, size=1)


#time series of carbon value by scenario
ts.plot.scenario=
ggplot(data=ts.totvalue.scenarios[year<300]) +geom_ribbon(aes(ymax=max/10^6,ymin=min/10^6,x=year,fill=scenario),alpha=0.4, show.legend = FALSE) +geom_line(aes(y=Total.value/10^6,x=year,color=scenario),size=2)+theme_bw(base_size=14) + ylab(expression(paste('Carbon caputre value (M$)'))) + scale_color_manual(values=c('#7fc97f','#beaed4','#fdc086')) + labs(color='Poaching intensity') + scale_fill_manual(values=c('#7fc97f','#beaed4','#fdc086')) +xlab('Year')
ggsave(ts.plot.scenario,file=paste0(folderout,'value by poaching scenario.png'),width=25,height=18,units='cm',dpi=600)
#time series of carbon value by country
ts.plot=
ggplot()+geom_line(data=ts[year<300],aes(y=Total.value/10^6,x=year,color=Country),size=2, show.legend = FALSE)+theme_bw(base_size=14) + ylab(expression(paste('Total value, ',log[10],' (Millions of $)'))) +scale_y_log10()

#time series of elephant pop by scenario
ts.plot.population=
ggplot(data=ts.totvalue.scenarios[year<1500]) +geom_line(aes(y=Total.population/10^3,x=year,color=scenario),size=2)+theme_bw(base_size=14) + ylab(expression(paste('Elephant population (N x ',10^3,')'))) + scale_color_manual(values=c('#7fc97f','#beaed4','#fdc086')) + labs(color='Poaching intensity') + scale_fill_manual(values=c('#7fc97f','#beaed4','#fdc086')) + xlab("Year")
ggsave(ts.plot.population,file=paste0(folderout,'population by poaching scenario.png'),width=25,height=18,units='cm',dpi=600)


#time series of elephant population
ggplot()+geom_line(data=ts[year<300],aes(y=popolo,x=year,color=Country),size=2, show.legend = FALSE)+theme_bw(base_size=14) + ylab(expression(paste('Elephant population, ',log[10],' (Millions of $)'))) 
#before and after 100 plot
pop.before.after=ts[year %in% c(1,100)]

before.after.plot=
ggplot()+geom_bar(stat = "identity",data=pop.before.after,aes(y=popolo,x=Country,fill=factor(year)),position="dodge",alpha=0.9)+ylab("Elephant population (N)") +theme_bw(base_size=14)+ theme(panel.grid.major.x = element_blank(),legend.position ='bottom',axis.title.x=element_blank())+labs(fill='Years since protection')

#plot by country with and without poaching
#the percentage in body is 0.1% of the total so I don't show it in the plots
#total.bycountry=popdata[,list(natural=sum(proj.100),poaching=sum(proj.100.poaching),body=sum(corpo),body.poaching=sum(corpo.poaching)),by=Country]
total.bycountry=popdata[,list(natural=sum(proj.100),poaching=sum(proj.100.poaching),heavy.poaching=sum(proj.100.poaching.high)),by=Country]
total.bycountry=rollup(total.bycountry, j = lapply(.SD, sum), by = c("Country"))
total.bycountry[is.na(Country)]$Country='Total'
total.bycountry.m=melt(total.bycountry, variable.name = "poaching")
library(forcats)
total.bycountry.m$Country=factor(total.bycountry.m$Country)
total.bycountry.m$Country=fct_relevel(total.bycountry.m$Country,'Total',after = Inf)
total.bycountry.m$poaching=fct_recode(total.bycountry.m$poaching, 'No poaching'='natural','Current poaching'='poaching','Heavy poaching'='heavy.poaching')
bynation.plot=
ggplot()+geom_bar(stat = "identity",data=total.bycountry.m[Country!='Total'],aes(y=value,x=reorder(Country,-value),fill=poaching),alpha=0.9,position="dodge")+ylab("Carbon value (M$)") +theme_bw(base_size=14)+ theme(panel.grid.major.x = element_blank(),legend.position ='bottom',axis.title.x=element_blank())+scale_y_log10() + labs(fill='Poaching intensity') + scale_fill_manual(values=c('#7fc97f','#beaed4','#fdc086'))
ggsave(bynation.plot,file=paste0(folderout,'valuation by nation.png'),width=37,height=25,units='cm',dpi=600)

#total.bar.byscenario=
# ggplot(data=totvalue.barplot,aes(x=reorder(scenario,-Total.value)))+geom_boxplot(stat='identity', aes(middle=Total.value/10^9,fill=scenario,ymin=min/10^9,ymax=max/10^9,upper=max/10^9,lower=min/10^9),alpha=0.9)+ylab("Carbon value (B$)") +theme_bw(base_size=14)+ theme(panel.grid.major.x = element_blank(),legend.position ='bottom',axis.title.x=element_blank())+scale_y_log10() + labs(fill='Poaching intensity') + scale_fill_manual(values=c('#7fc97f','#beaed4','#fdc086'))
inset.plot=
ggplot(data=totvalue.barplot,aes(x=reorder(scenario,-Total.value)))+geom_boxplot(stat='identity', aes(middle=Total.value/10^9,fill=scenario,ymin=min/10^9,ymax=max/10^9,upper=max/10^9,lower=min/10^9),alpha=0.9)+ylab("Carbon value (B$)") +xlab("All countries") +theme_bw(base_size=12)+ theme(panel.grid.major.x = element_blank(),legend.position ='none',axis.ticks.x=element_blank(),axis.text.x=element_blank())+scale_y_log10() + scale_fill_manual(values=c('#7fc97f','#beaed4','#fdc086'))

plot.with.inset = 
ggdraw() + draw_plot(bynation.plot) +  draw_plot(inset.plot, x = 0.73, y = .62, width = .25, height = .32)
ggsave(plot.with.inset,file=paste0(folderout,'value by poaching scenario barplot.png'),width=25,height=18,units='cm',dpi=600)


#statistics by country
country.stats.iso2=popdata[,list(individuals=sum(individuals),area=sum(AED.area),natural=sum(proj.100),maximum=sum(proj.max),poaching=sum(proj.100.poaching),carbon.max=sum(carbon.max),carbon.100=sum(carbon.100),carbon.100.poaching=sum(carbon.100.poaching),carbon.100.poaching.heavy=sum(carbon.100.poaching.heavy),nPA=.N),by=ISO2]
#calculate value of $ per km2
country.stats.iso2[ , value.km2 := natural/area]
#country.stats.iso2[ , value.km2.poaching := poaching/area]
#maximum value $ per km2
country.stats.iso2[ , value.km2.max := maximum/area]

country.stats.iso2[ , potential.km2 := value.km2/value.km2.max*100]
country.stats.iso2[ , potential := natural/maximum*100]
country.stats.iso2[ , carbon.potential := carbon.100/carbon.max*100]

country.stats.iso2[ , density := individuals/area]
setkey(country.stats.iso2,ISO2);setkey(popdata,ISO2)
ccode=popdata[,c('ISO2','Country')];ccode=unique(ccode)
country.stats.iso2= country.stats.iso2[ccode]
#nudging a bit Rwanda so doesn't touch the borders
country.stats.iso2[Country=='Rwanda']$density=0.045
country.stats.iso2[Country=='Rwanda']$area=1350

#plot.bar.nation=
#ggplot(data=country.stats.iso2)+geom_bar(stat = "identity",aes(y=value.km2,x=reorder(Country,value.km2),fill=area),alpha=0.9,position="dodge")+ylab(expression(paste("Carbon value per area ($/ ",km^2,")"))) +theme_bw(base_size=14)+ theme(panel.grid.major.x = element_blank(),legend.position ='right',axis.title.x=element_blank())+scale_fill_viridis_b()+ geom_errorbar( aes(x=Country,ymin=value.km2.poaching, ymax=value.km2.max), width=0.2, colour="grey", alpha=0.9, size=1)+labs(fill=expression(atop('Extent of PAs', '('~km^2~')')))
#ggplot(data=country.stats.iso2,aes(x=area,y=density,color=potential))+geom_point(size=10)+scale_color_viridis_c()+scale_x_log10()
library(ggflags)
conflict_prefer("layer", "ggplot2")
library(ggrepel)

#melt.country=melt(tmp.country,measure.vars=c('potential','carbon.potential'))
set.seed(42)
plot.potential=
ggplot(data=country.stats.iso2)+geom_label_repel(min.segment.length = 0,segment.colour="black",force=5,point.size = 18,aes(x=area,y=density,label=Country,fill=potential),color=c(rep('black',7),'grey','black'),ylim=c(0.05,0.45),xlim=c(3,5.3))+geom_flag(aes(x=area,y=density,country=ISO2),size=13)+theme_bw(base_size = 14) +ylab(expression(paste('Elephant density (indiv./ ',km^2,')')))+xlab(expression(paste('Extent of PAs (',km^2,')')))+labs(fill='Realized carbon value (%)')+scale_fill_viridis_b(breaks=c(87,90,92,95))+ theme(panel.grid.minor=element_blank(),legend.position ='bottom')+scale_x_log10()

ggsave(plot.potential,file=paste0(folderout,'flags potential value nation.png'),width=25,height=18,units='cm',dpi=600)
set.seed(42)
plot.potential.carbon=
ggplot(data=country.stats.iso2)+geom_label_repel(min.segment.length = 0,segment.colour="black",force=5,point.size = 18,aes(x=area,y=density,label=Country,fill=carbon.potential),color=c(rep('#f0f0f0',2),'black','#f0f0f0','black',rep('#f0f0f0',3),'black'),ylim=c(0.05,0.45),xlim=c(3,5.3))+geom_flag(aes(x=area,y=density,country=ISO2),size=13)+theme_bw(base_size = 14) +ylab(expression(paste('Elephant density (indiv./ ',km^2,')')))+xlab(expression(paste('Extent of PAs (',km^2,')')))+labs(fill='Realized carbon sink (%)')+scale_fill_viridis_b(breaks=c(45,55,65,75))+ theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),legend.position ='bottom',panel.grid.minor=element_blank())+scale_x_log10() + scale_color_viridis_c(option='C', direction = -1)
	
ggsave(plot.potential.carbon,file=paste0(folderout,'flags potential carbon nation.png'),width=25,height=18,units='cm',dpi=600)
library(cowplot)
set.seed(42)
flag.plot=plot_grid(plot.potential,plot.potential.carbon,labels=c('A'),label_size = 14,ncol=2,rel_widths=c(1.05,0.95))
set.seed(1)
ggsave(flag.plot,file=paste0(folderout,'ts flag nation.png'),width=25,height=15,units='cm',dpi=600)


total.plotto=
plot_grid(ts.plot.scenario,plot.bar.nation,plot.potential,plot.potential.carbon,labels=c('AUTO'),ncol=2,align='hv')
ggsave(total.plotto,file=paste0(folderout,'ts flag nation.png'),width=30,height=20,units='cm',dpi=600)


#old code plot results in map
# load background map
mappa.sfondo=sf::st_as_sf(rworldmap::countriesCoarseLessIslands); colnames(mappa.sfondo)[36]='incontinenza'
tsf=st_as_sf(popdata,coords = c('lon','lat'),crs="+proj=longlat +ellps=WGS84 +datum=WGS84")
#by park
ggplot()+geom_sf(data=mappa.sfondo,size=0.1)+ cowplot::theme_map() +geom_sf(data=tsf,size=2,aes(color=log10(proj.100)))+coord_sf(ylim = c(-10, 10),xlim=c(0,40),expand=FALSE)+scale_color_viridis_c()+labs(color="Park carbon value\n log10 ($)")
# blue red 10 scales
colfunc=colorRampPalette(c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2'))
#by park different scale
bypark.plot=ggplot()+geom_sf(data=mappa.sfondo,size=0.1)+ cowplot::theme_map()+geom_sf(data=tsf,size=2,aes(color=log10(proj.100)))+coord_sf(ylim = c(-20, 20),xlim=c(0,40),expand=FALSE)+labs(color="PA carbon value\n (log $)")+scale_color_gradientn(colors=colfunc(10),trans='reverse')+ guides(colour = guide_colorbar(reverse=T))



a=st_join(mappa.sfondo,tsf)

#by park and total of country
plotosave=
ggplot()+geom_sf(data=mappa.sfondo,size=0.1)+ cowplot::theme_map()+geom_sf(data=a,aes(fill=proj.100/10^6))+geom_sf(data=tsf,size=2,aes(color=log10(proj.100)))+coord_sf(ylim = c(-20, 20),xlim=c(0,40),expand=FALSE)+labs(fill="National carbon value\n log10 ($)",color="PA carbon value\n log10 ($)")+ guides(fill = guide_colorbar(frame.colour = "black",reverse=TRUE))+scale_fill_viridis_c(na.value="transparent",n.breaks=5)+scale_color_gradientn(colors=colfunc(10),trans='reverse')+ guides(colour = guide_colorbar(reverse=T),fill = guide_colorbar(reverse=T))


library(cowplot)
#comboplot=
right.panel=plot_grid(bynation.plot,ts.plot,nrow=2,labels=c('B','C'),rel_heights =  c(1,0.8))
left.panel=plot_grid(bypark.plot,before.after.plot,nrow=2)
total.plot=plot_grid(left.panel,right.panel,labels=('A'))

ggsave(total.plot,file=paste0(folderout,'valuation plot.png'),width=37,height=25,units='cm',dpi=600)

ggplot(data=popdata ,aes(x=mergedens,y=agb))+geom_point(alpha=0.9) + geom_smooth(method='lm',formula=y ~ exp(x),se=TRUE,color="black",size=.7) 

+ stat_poly_eq(aes(label = paste(..eq.label..,..rr.label..,sep=" | ")),size=3,formula = y ~ x,parse = TRUE,na.rm=TRUE) + labs(color='Climate zone') +theme_bw(base_size=14)

+ ylab('Total biomass (log kg)') + xlab('Actual Evapotranspiration (mm)')
+facet_wrap(~continent) + theme(legend.spacing.x = unit(0, 'cm'),legend.position ='right',panel.background = element_rect(fill=NA, colour='black'),strip.background =element_rect(fill='#e8e8e8'),strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")))



# Set interest rate to calculate Present values
# int=0.02
# #carbon calculations
# # Set addition carbon credits in Gabon at 7%  of biomass at density 0.5 eleph/km2. 
# C100 = 12683
# C0=0
# # Let increase in biomass  change over 100 years (biomass steady state).
# M=100
# # Change in co2 stored in forest per year from increase in elephant density. (tonnes/year/elephant)
# change=(C100-C0)/M



# OLDER CODE in R matrix[row,column]
#discounted cash flow per year per generation
    C=matrix(0,nrow=TN,ncol=TN)
    #cashflow in particular year not discounted
    Cashflow=matrix(0,nrow=TN,ncol=TN)
    DFin=0
    #discount for interest rates - need number of year / (1 + interest rate)^10 for 10 years
    disct=matrix(0,nrow=TN,ncol=TN)
    #carbon level - change per year per generation
    carlevel=matrix(0,nrow=TN,ncol=TN)

for (t in 2:(TN-99)) {
    for (i in t:(t+99)) {
        disct[i,t] = 1/((1+int)^(i+t-2))
        carlevel[i,t]=change
        #change in population * carbon level * price of carbon * discount
        #contribution of each cohort in each period
        C[i,t]=Pcar*carlevel[i,t]*(P[t]-P[t-1])*disct[i,t]
        #present value of each cohort/time
        Cashflow[i,t]=Pcar*carlevel[i,t]*(P[t]-P[t-1])
    }
}


# cash flow 
CFy=rep(0,TN)
for (t in 2:(TN-99)) {
    for (i in 2:TN) {
     CFy[t]=CFy[t] + Cashflow[t,i] 
    }
}

#carbon in body

body=2.64;
Initbody = Pcar*body*Nw;

Pbody=rep(0,TN)
disctb=rep(0,TN)

for (i in 2:TN) {
    disctb[i] = 1/((1+int)^(i-1))
    Pbody[i]=Pcar*(P[i]-P[i-1])*disctb[i]
}

Totalbody=Initbody+sum(Pbody);

Totalbodyel=Totalbody/Nw;

groP=rep(0,TN)

# for (t in 1:TN) {
#   groP[t,1] = (P[t]-P[t-1])/P[t-1]
#     diffP[t,1]= P[t]-P[t-1]
#     grodP[t,1]=(beta-cn)-beta*(P[t])/alpha
#     deaths[t,1]=cn*P[t-1] 
#     #birpop[t,1]=B[t]./P[t]
#     deathcalf[t,1]=(1-So)*P[t]*(AFR-1)/T 
#     deathsa[t,1]=(1-Sa)*P[t]*(T-AFR)/T
#     grossbir[t,1] = diffP[t,1] + deathcalf[t,1]+deathsa[t,1]
# }
