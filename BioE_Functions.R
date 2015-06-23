############################################################
# NewBioE.r
# Program to Implement the BioEngergetics Model
# Matt Nahorniak
# South Fork Research, Inc
# 2012


# Set record = TRUE to enable scrolling through plots with page up/ page down
# record = TRUE

#############################################################


##############################################################
# Function to read input file
ReadInputFile <- function() {

# read data from the "Input" worksheet on "BioEInput.xlsx"
#	Header=read.xlsx("BioEInput.xlsx", sheetName="Input",
#     	   rowIndex=2:11, colIndex=2, header=F)
#
#	Header=read.xlsx2("BioEInput.xlsx", sheetName="Input",
#    	   startRow=2, endRow=11, colIndex=2, header=F)

	Header = read.csv("Input_BioE_header.csv", skip=1,
      nrows=9, header=F)[,2]

	N.sites = as.numeric(as.character(Header[3]))
      N.steps = as.numeric(as.character(Header[4]))
      Pred= as.numeric(as.character(Header[5]))
#      Prey= as.numeric(as.character(Header[6]))
      Oxygen= as.numeric(as.character(Header[6]))
	PFF = as.numeric(as.character(Header[7]))
      stab.factor= as.numeric(as.character(Header[8]))
      epsilon = as.numeric(as.character(Header[9]))

input.file.names =
as.character(read.csv("Input_BioE_header.csv", skip=10, header=F, nrows=2)[,2])

###############################
# Initialize Stuff
temperature = matrix(rep(0, N.steps*N.sites), c(N.steps, N.sites))
pvalues = matrix(rep(0, N.steps*N.sites), c(N.steps, N.sites))
prey.energy.density = matrix(rep(0, N.steps*N.sites), c(N.steps, N.sites))
startweights = rep(0, N.sites)
endweights = rep(0, N.sites)
TotalConsumption = rep(0, N.sites)
sitenames = rep("", N.sites)


##########
      # read start weight, end weight, and TotalConsumption vals for site
      sw.ew.tc = read.csv(input.file.names[2], header=T)
      sw.ew.tc
      startweights=sw.ew.tc[,3][1:N.sites]
      endweights=sw.ew.tc[,4][1:N.sites]
      TotalConsumption=sw.ew.tc[,5][1:N.sites]
      sitenames = as.character(sw.ew.tc[,2])[1:N.sites]

##########
#Number of Fish per Site
Sites = levels(factor(sw.ew.tc$Site))
Sites
Fish.Per.Site = rep(0, length(Sites))
for (i in 1:length(Fish.Per.Site)){ Fish.Per.Site[i]= length(sw.ew.tc$Site[sw.ew.tc$Site == Sites[i]])}
Fish.Per.Site







input.file.names[2]
temp.t = read.csv(input.file.names[1], skip=5, header=T)
temp.t = temp.t[is.na(temp.t[,1])==F,]
# When there are more than 1 fish per site, duplicate those rows in the "temp" data.frame
temp.t
i=1
temp.t
temp = temp.t[0,]
Fish.Per.Site
for (i in 1:length(Sites)){
 for (j in 1:Fish.Per.Site[i]){
#  print(j)
   rows.to.add = temp.t[temp.t$Site == Sites[i],]
   rows.to.add$Site = rows.to.add$Site+.0001*j
   temp=rbind(temp, rows.to.add)   
#print(temp[,1:3])
}
}
temp
nrow(temp)

temp
N.sites
Sites
index=0
for (k in 1:length(Sites)) {
 for (k2 in 1:Fish.Per.Site[k]) {
index=index+1
site.temp = temp[temp$Site==k+k2*.0001,]
site.temp
exp.site.temp = array(-99, c(N.steps, 24))
dim(exp.site.temp)
colnames(exp.site.temp)=colnames(site.temp)
exp.site.temp = as.data.frame(exp.site.temp)
exp.site.temp$Day
exp.site.temp[,1]
exp.site.temp$Day=1:(N.steps)

site.temp
idx=match(site.temp$Day, exp.site.temp$Day)

for (j in 2:(length(idx))){
 idx.low = idx[j-1]
 idx.high = idx[j]
idx.low
idx.high

dim(exp.site.temp)
for (jj in idx.low:idx.high) {
    exp.site.temp[jj,] = site.temp[(j-1),]+ (jj-idx.low)/(idx.high-idx.low)*(site.temp[j,]-site.temp[(j-1),])
}
}

names(exp.site.temp)
temperature[,index] = exp.site.temp$Temp
pvalues[,index] = exp.site.temp$P.Val

	# calculate weighted average prey energy density for site
	prey.vals = read.csv(input.file.names[1],skip=2, header=F, nrows=1)[5:24]

	prey.energy.density[,index]= as.matrix(prey.vals)%*%as.matrix((t(exp.site.temp[,5:24])))
} # End of loop through j fish per site
} # End of loop through k sites


temperature
pvalues
prey.energy.density
##########################################################################################







# If there's only 1 site, we still need the data structured as an
# array with only 1 column (rather than a vector) for consistency

if (N.sites==1) {temperature=array(temperature, c(N.steps, 1))
                 pvalues = array(pvalues, c(N.steps, 1)) 
		     sitenames = array(sitenames, c(1, N.sites))}

N.sites
Header
pvalues
temperature
# Return all this good stuff.
	return(list(	
			"Species" = as.character(Header[1]),
			"SimMethod" = as.numeric(as.character(Header[2])),
			"Wstart"=startweights,
			"Endweights" = endweights,
			"TotalConsumption" = TotalConsumption,
			"p"=pvalues, "Temps"=temperature,
                  "N.sites"=N.sites,
                  "N.steps"=N.steps,
                   "sitenames"=sitenames,
                   "Pred"=Pred,
                   "prey.energy.density"=prey.energy.density,
			 "Oxygen"=Oxygen,
                   "stab.factor"=stab.factor,
			 "PFF" = PFF,
			 "epsilon" = epsilon)
)

	}



################################################################
# Function toRead in Constants from Lookup Table File for the given 
# equations to be used
ReadConstants <-function(species) {


#	Constants=read.xlsx("BioELookup_Tables.xlsx", sheetName="Constants",
#         rowIndex=4:50, colIndex=4:200, header=F)
#	Constants=read.xlsx2("BioELookup_Tables.xlsx", sheetName="Constants",
#         startRow=4, endRow=31, colIndex=4:100, header=F)

      Constants =read.csv("Input_BioELookup_Tables.csv", nrows=28, header=T)
      Constants = Constants[,3:ncol(Constants)]
ncol(Constants)

fishlist = c(read.csv("Input_BioELookup_Tables.csv", header=F,nrows=1, colClasses="character"))
fishlist = fishlist[3:length(fishlist)]
fishlist
# get column index for correct fish species)
	index=seq(1, length(fishlist))
	fish.index = index[species==fishlist]
	# Get consumption constants
	Cons = data.frame(
   	   ConsEQ=Constants[1, fish.index],
	   CA=Constants[2, fish.index],
	   CB=Constants[3, fish.index],
	   CQ=Constants[4, fish.index],
	   CTO=Constants[5, fish.index],
	   CTM=Constants[6, fish.index],
	   CTL=Constants[7, fish.index],
	   CK1=Constants[8, fish.index],
	   CK4=Constants[9, fish.index])

	# Get respiration constants
		Resp=data.frame(
	  	RespEQ=Constants[10, fish.index], 
	  	RA=Constants[11, fish.index],
		RB=Constants[12, fish.index],
		RQ=Constants[13, fish.index],
		RTO=Constants[14, fish.index],
		RTM=Constants[15, fish.index],
		RTL=Constants[16, fish.index],
		RK1=Constants[17, fish.index],
		RK4=Constants[18, fish.index],
		ACT=Constants[19, fish.index],
		BACT=Constants[20, fish.index],
		SDA=Constants[21, fish.index])

	# Get Excretion / Egestion Constants
		Excr=data.frame(
	      ExcrEQ=Constants[22, fish.index], 
	 	FA=Constants[23, fish.index],  
		FB=Constants[24, fish.index], 
		FG=Constants[25, fish.index], 
		UA=Constants[26, fish.index], 
		UB=Constants[27, fish.index], 
		UG=Constants[28, fish.index])

# Return the Constants
	return(list("Consumption"=Cons, 
      	      "Respiration"=Resp, 
			"Excretion"=Excr))
	}



########################################################################
# Consumption Equation 1
ConsumptionEQ1 <-function(W, TEMP, P, PREY, CA, CB, CQ) {

	CMAX <- CA*(W**CB)			#max specific feeding rate (g_prey/g_pred/d)
	CONS <<- (CMAX*P*exp(CQ*TEMP))	#specific consumption rate (g_prey/g_pred/d) - grams prey consumed per gram of predator mass per day
	CONSj <<- CONS*PREY          		 #specific consumption rate (J/g_pred/d) - Joules consumed for each gram of predator for each day
	return(list("CMAX"=CMAX, "CONS"=CONS, "CONSj"=CONSj))
}


########################################################################
# Consumption Equation 2
	ConsumptionEQ2 <-function(W, TEMP, P, PREY, CA, CB, CTM, CTO, CQ) {

	Y=log(CQ)*(CTM-CTO+2)
	Z=log(CQ)*(CTM-CTO)
	X=(Z^2*(1+(1+40/Y)^.5)^2)/400
	V=(CTM-TEMP)/(CTM-CTO)

	CMAX <- CA*(W**CB)		
	CONS <- CMAX*P* (V**X)*exp(X*(1-V))
	CONSj <- CONS*PREY             

	return(list("CMAX"=CMAX, "CONS"=CONS, "CONSj"=CONSj))
}


########################################################################
# Consumption Equation #3: Temperature Dependence for cool-cold water species
ConsumptionEQ3 <-function(W,TEMP,P,PREY,CA, CB, CK1, CTO, CQ, CK4, CTL, CTM) {
	G1 <- (1/(CTO-CQ))*(log((0.98*(1-CK1))/(CK1*0.02)))
	L1 <- exp(G1*(TEMP-CQ))
	KA <- (CK1*L1)/(1+CK1*(L1-1))
	G2 <- (1/(CTL-CTM))*(log((0.98*(1-CK4))/(CK4*0.02)))
	L2 <- exp(G2*(CTL-TEMP))
#print(paste("temp =", TEMP))

	KB <- (CK4*L2)/(1+CK4*(L2-1))


	CMAX <- CA*(W**CB)		#max specific feeding rate (g_prey/g_pred/d)
	CONS <- (CMAX*P*KA*KB)		#specific consumption rate (g_prey/g_pred/d) - grams prey consumed per gram of predator mass per day
	CONSj <- CONS*PREY             #specific consumption rate (J/g_pred/d) - Joules consumed for each gram of predator for each day

	return(list("CMAX"=CMAX, "CONS"=CONS, "CONSj"=CONSj))
	}



################################################################
# Excretion Equation 1
ExcretionEQ1 <- function(CONS, CONSj, TEMP,P,FA, UA) {


	EG <- FA*CONS				# egestion (fecal waste) in g_waste/g_pred/d
	U <- UA*(CONS-EG)	 			# excretion (nitrogenous waste) in g_waste/g_pred/d

	EGj <- FA*CONSj				# egestion in J/g/d
	Uj <- UA*(CONSj-EGj)			# excretion in J/g/d

	return(list("EG"=EG, "EGj"=EGj, "U"=U, "Uj"=Uj))
	}	



################################################################
# Excretion Equation 2
ExcretionEQ2 <- function(CONS, CONSj, TEMP,P,FA, UA, FB, FG, UB, UG) {

	EG <- FA*TEMP^FB * exp(FG*P)*CONS			# egestion (fecal waste) in g_waste/g_pred/d
	U <- UA*TEMP^UB*exp(UG*P)*(CONS-EG)			# excretion (nitrogenous waste) in g_waste/g_pred/d

	EGj <- EG*CONSj/CONS					# egestion in J/g/d
	Uj <- U*CONSj/CONS					# excretion in J/g/d

	return(list("EG"=EG, "EGj"=EGj, "U"=U, "Uj"=Uj))
	}


################################################
# Excretion Equation 3
# Egestion/Excretion Equation 3 (W/ correction for indigestible 
# prey as per Stewart 1983)

ExcretionEQ3 <- function(CONS, CONSj, TEMP,P,FA, UA, FB, FG, UB, UG, PFF) {

	#Note: In R, "F" means "FALSE", so I've switched from F (as in the FishBioE 3.0 manual) to EG as the variable name for egestion
	#Note:  PFF = 0 assumes prey are entirely digestible, making this essentially the same as Equation 2, but I programmed it . . .
	# . . . this way so we could change it in the future if we wanted

	PE = FA*(TEMP**FB)*exp(FG*P)
#	PFF = 0

	PF = ((PE - 0.1)/0.9)*(1-PFF)+PFF

	EG <- PF*CONS					# egestion (fecal waste) in g_waste/g_pred/d
	U <- UA*(TEMP**UB)*(exp(UG*P))*(CONS-EG)	# excretion (nitrogenous waste) in g_waste/g_pred/d

	EGj <- PF*CONSj				# egestion in J/g/d
	Uj <- UA*(TEMP**UB)*(exp(UG*P))*(CONSj-EGj)	# excretion in J/g/d

	return(list("EG"=EG, "EGj"=EGj, "U"=U, "Uj"=Uj))
	}	



############################################
# Respiration Equation 1
RespirationEQ1 <- function(W, TEMP, CONS, EG, PREY, OXYGEN,
                           RA,RB,ACT,SDA,RQ,RTO,RK1,RK4,RTL,BACT){

	VEL=(RK1*W^RK4)*(TEMP>RTL) +  ACT*W^RK4*exp(BACT*TEMP)*(1-1*(TEMP>RTL))

	ACTIVITY=exp(RTO*VEL)
	S <- SDA*(CONS - EG)					# proportion of assimilated energy lost to SDA in g/g/d (SDA is unitless)
	Sj <- S*PREY						# proportion of assimilated energy lost to SDA in J/g/d - Joules lost to digestion per gram of predator mass per day
	R <- RA*(W**RB)*ACTIVITY*exp(RQ*TEMP)       	# energy lost to respiration (metabolism) in g/g/d
	Rj <- R*OXYGEN           				# energy lost to respiration (metabolism) in J/g/d - Joules per gram of predator mass per day
	return(list("R"=R, "Rj"=Rj, "S"=S, "Sj"=Sj))
	}


############################################
# Respiration Equation 2

RespirationEQ2 <- function(W, TEMP, CONS, EG, PREY, OXYGEN,
                            RA,RB,ACT,SDA,RTM,RTO,RQ) {


	# Respiration Equation 2 (Temp dependent w/ ACT multiplier)
	#------------------------------------------------------------

	V <- (RTM - TEMP)/(RTM - RTO)
	Z <- (log(RQ))*(RTM - RTO)
	Y <- (log(RQ))*(RTM - RTO+2)
	X <- ((Z**2)*(1 + (1 + 40/Y)**0.5)**2)/400

	S <- SDA*(CONS - EG)					# proportion of assimilated energy lost to SDA in g/g/d (SDA is unitless)
	Sj <- S*PREY						# proportion of assimilated energy lost to SDA in J/g/d - Joules lost to digestion per gram of predator mass per day
	R <- RA*(W**RB)*ACT*((V**X)*(exp(X*(1 - V) )))     	# energy lost to respiration (metabolism) in g/g/d
	Rj <- R*OXYGEN           				# energy lost to respiration (metabolism) in J/g/d - Joules per gram of predator mass per day
	return(list("R"=R, "Rj"=Rj, "S"=S, "Sj"=Sj))
	}

######################################################










########################################################################
# Calculate Growth Function
######################################################################

CalculateGrowth <- function(Consts, InputData, W) {

attach(InputData)
attach(Consts)

#prey=InputData$Prey
pred=InputData$Pred
Oxygen=InputData$Oxygen


# Initialize Fish Weights
	W=array(rep(0, (N.sites * (N.steps+1))), c(N.steps+1, N.sites))
	W[1,]=as.numeric(Wstart[1:ncol(W)])
      Growth=array(rep(0, N.sites*N.steps), c(N.steps, N.sites))
	Growth_j=Growth
	Consumpt=Growth
	Consumpt_j=Growth
	Excret=Growth
	Excret_j=Growth
	Egest=Growth
	Egest_j=Growth   
	Respirat=Growth 
	Respirat_j=Growth 
	S.resp=Growth 
	Sj.resp =Growth 
	Gg_WinBioE=Growth
      Gg_ELR=Growth
 	TotalC=rep(0, N.sites)


##Start Looping Through Time - for Known Consumption, solving for Weight

t=1
	for (t in 1:(N.steps)) {

	### Consumption 
	    if (Consumption$ConsEQ==1) {Cons = with(Consts$Consumption,
        	ConsumptionEQ1(W[t,],Temps[t,],p[t,], prey.energy.density[t,], CA, CB, CQ)
	)
	}  else if (Consumption$ConsEQ==2) {Cons = with(Consts$Consumption,
        ConsumptionEQ2(W[t,],Temps[t,],p[t,], prey.energy.density[t,], CA, CB, CTM, CTO, CQ)
	)
	  }  else if (Consumption$ConsEQ==3) {Cons = with(Consts$Consumption,
	  ConsumptionEQ3(W[t,],Temps[t,],p[t,], prey.energy.density[t,],CA, CB, CK1, CTO, CQ, CK4, CTL, CTM))}

	TotalC= TotalC + Cons$CONS*W[t,]

	# store daily consumption 
		Consumpt[t,]= as.numeric(Cons$CONS)
		Consumpt_j[t,] = as.numeric(Cons$CONSj)


### Excretion / Egestion
	if (Excretion$ExcrEQ==1) {ExcEgest<-
	with(Consts$Excretion,
       ExcretionEQ1(Cons$CONS, Cons$CONSj, Temps[t,],p[t,] ,FA, UA))
	  }  else if (Excretion$ExcrEQ==2) {
	ExcEgest<- with(Consts$Excretion,
       ExcretionEQ2(Cons$CONS, Cons$CONSj, Temps[t,],p[t,],FA, UA, FB, FG, UB, UG ))
	} else
		if (Excretion$ExcrEQ==3) {ExcEgest<-with(Consts$Excretion,
	       ExcretionEQ3(Cons$CONS, Cons$CONSj, Temps[t,],p[t,],FA, UA, FB, FG, UB, UG,PFF) )}

	# store daily excretion and egestion
		Excret[t,]=as.numeric(ExcEgest$U)
		Excret_j[t,]=as.numeric(ExcEgest$Uj)
		Egest[t,]=as.numeric(ExcEgest$EG)
		Egest_j[t,]=as.numeric(ExcEgest$EGj)


### Respiration
	if (Respiration$RespEQ==1) {Resp<- with(Consts$Respiration,
	RespirationEQ1(W[t,],Temps[t,], Cons$CONS, ExcEgest$EG, prey.energy.density[t,], 
        Oxygen,RA,RB,ACT,SDA,RQ, RTO, RK1, RK4, RTL, BACT))
	} else if (Respiration$RespEQ==2) {Resp<- with(Consts$Respiration,
	RespirationEQ2(W[t,],Temps[t,], Cons$CONS, ExcEgest$EG, prey.energy.density[t,], 
        Oxygen,RA,RB,ACT,SDA,RTM,RTO,RQ))}

	#store daily respiration results
		Respirat[t,] = as.numeric(Resp$R)
		Respirat_j[t,] = as.numeric(Resp$Rj)
		S.resp[t,] = as.numeric(Resp$S)
		Sj.resp[t,] = as.numeric(Resp$Sj)


### Now calculate Growth

	# growth in J/g/d - Joules allocated to growth for each gram of predator on each day
	Gj <- Cons$CONSj - Resp$Rj - ExcEgest$EGj - ExcEgest$Uj - Resp$Sj	
	G <- Cons$CONS - Resp$R - ExcEgest$EG - ExcEgest$U - Resp$S
	# growth in g/d - Grams of predator growth each day
	Growth[t,] <- as.numeric(Gj*W[t,])/pred
      Growth_j[t,] <- as.numeric(Gj)


	# [Eric's comment]: Calculate growth in g/g/d as I believe the Fish BioE 3.0 program does it, using fish weight at each time step	
	# growth in g/g/d (DailyWeightIncrement divided by fishWeight)
	Gg_WinBioE[t,] <- as.numeric(Growth[t,]/W[t,])			

	# Calculate growth in g/g/d as Nick B. has suggested, using average of start and end weights
	# growth in g/g/d (DailyWeightIncrement divided by average of fish start end weights)
	Gg_ELR[t,] <- Growth[t,]/((as.numeric(Wstart[1:ncol(W)])+W[t,])/2)		

	# Calculate absolute weight at time t+1
	W[t+1,]=W[t,]+Growth[t,]


} # End of cycles through time

	# data clean up
		detach(InputData)
		detach(Consts)

# Return Results	
	return(list(
		"TotalC"=TotalC,
		"W"=W, 
		"Growth"=Growth, 
		"Gg_WinBioE"=Gg_WinBioE, 
		"Gg_ELR"=Gg_ELR,
		"Growth_j"=Growth_j,
		"Consumption"=Consumpt,
		"Consumption_j"=Consumpt_j,
		"Excretion"=Excret,
		"Excretion_j"=Excret_j,
		"Egestion"= Egest,
		"Egestion_j"=Egest_j,
		"Respiration"=Respirat, 
		"Respiration_j"=Respirat_j, 
		"S.resp"=S.resp,
		"Sj.resp"=Sj.resp
	))
	}

###############################################################
# Here's the main function that calls the functions other functions

BioE <- function (Input, Constants) {

# for simulation method =1 (we have p-vals, and want to solve for weights
if (Input$SimMethod==1) {
	# Method 1: Calculate Growth from p-values and Temperatures
	Results=CalculateGrowth(Constants, Input, W)
	W=Results$W
	Growth=Results$Growth


} else{

# We don't know p-values, but need to iteratively solve for them
# Method 2 or 3:  Calculte P-values from Total Growth or Consumption
# Need to assume p-values are constant with time
	
	# initialize first guess p-values of .5
	Input$p=array(rep(0.5, Input$N.sites*Input$N.step), 
		 c(Input$N.step,Input$N.sites))

	# set error at high value, iterate until it's small
	Error=rep(99, Input$N.sites)
	iteration = 0

### Interate until error is less than .1
	while(max(abs(Error)) > Input$epsilon) 
{
	iteration = iteration + 1
		Results=CalculateGrowth(Constants, Input, W)
		W=Results$W
     		TConsumption=Results$TotalC

 # Find error (depending on which thing on which we're converging), and
 # and come up with new estimate for average p-value
		if (Input$SimMethod==2)  {
		      Error = (W[Input$N.step+1,]-Input$Endweights)
  # Delta is the amount by which we'll change the p-value (prior to
  # scaling by the stability factor
		  	Delta= (Input$Endweights) / (W[Input$N.step+1,]) *
      	         Input$p[1,] - Input$p[1,]

		Pnew = as.vector(Input$p[1,] + Input$stab.factor*Delta)
	# Guard against negative p-values (maybe I shouldn't for convergence' sake)
	 for (i in 1:length(Pnew)) {Pnew[i]=max(0, Pnew[i])}
		} else 
		{
	      Error= (TConsumption-Input$TotalConsumption)/Input$TotalConsumption
	  	Delta= Input$TotalConsumption/TConsumption * Input$p[1,] - Input$p[1,]
		Pnew = as.vector(Input$p[1,] + Input$stab.factor*  Delta)
	}
	# Update for user, show p-values and error, see if we're converging
		for (i in 1:Input$N.step) {Input$p[i,]=as.numeric(Pnew)}
			print(paste("Pnew=",Pnew, "  Error=", Error))
	            print(paste("iteration =", iteration))	
		} # end of while statement 
	} 

Results$p = Input$p
# We're done.  Let's return the results!
return(Results)
}



###################################################################
##########################################################
#### Post Processing ########################################
##########################################################
plot.results <- function(Input, Results) {

W=Results$W
Growth=Results$Growth
Grange = max(Growth) - min(Growth)
title=paste(Input$Species, ": Growth Rate vs Time, by Site")
	plot(seq(1:(Input$N.steps)), Growth[,1], type="l", 
ylim = c(min(Growth)-.1*Grange, max(Growth)+.3*Grange ),
#ylim=c(  min(Growth), ( max(Growth)+ .3*(max(Growth)-min(Growth))  )   ),
#ylim=c((min(W     )-(.3*(max(W     )-min(W     ))  ) ), max(W     )),

#ylim=c(min(Growth), max(Growth)),
	main=title, xlab="time (days)", ylab="Growth Rate(g/day)")
	for (i in 1:Input$N.sites) {
		lines(seq(1:(Input$N.steps)), Growth[,i], type="l", lwd=3, col=i)
	}
	
#  	legendtext= paste("site",as.character(seq(1:Input$N.sites)))
site.names=rep(" ", Input$N.sites)
for (i in 1:Input$N.sites) {site.names[i]=as.character(Input$sitenames[i])}
		legendtext = site.names
#print(legendtext)
		legend("topright",legendtext, col=seq(1:Input$N.sites), pch=16, cex=.7)


Wrange = max(W)-min(W)
title=paste(Input$Species, ": Fish Weight vs Time, by Site")
	plot(seq(1:(Input$N.steps+1)), W[,1], type="l", 
ylim = c(min(W)-.1*Wrange, max(W)+.3*Wrange ), 
#   ylim=c((min(W)-(.3*(max(W)-min(W))  ) ), max(W)),
	main=title, xlab="time (days)", ylab="Weight (g)")
	for (i in 1:Input$N.sites) {
		lines(seq(1:(Input$N.steps+1)), W[,i], type="l", lwd=3, col=i)
	}
#		legendtext= paste("site",as.character(seq(1:Input$N.sites)))
		legend("topright",legendtext, col=seq(1:Input$N.sites), pch=16, cex=.7)
} # End of Function




#####################################################
# Write Output File
######################################################
write.output <- function(Input, Results){

# looks like I have N.sites and N.site (w/o "s").  Need to 
# clean this up. ######



#### Weights and P-values
output1= list("W"=Results$W[2:(Input$N.steps+1),], "p"=Results$p)
col.names = rep(0, 2*Input$N.sites+1)
site.names=rep(" ", Input$N.sites)
for (i in 1:Input$N.site) {site.names[i]=as.character(Input$sitenames[i])}

col.names[1] = "time"
for (i in 1:Input$N.sites){
col.names[i+1] = paste(site.names[i],"Weight")
col.names[Input$N.sites + i+1] = paste(site.names[i],"p-value")
}

output1= cbind(seq(1:Input$N.steps), "W"=Results$W[2:(Input$N.steps+1),], "p"=Results$p)
colnames(output1)=col.names
#write.xlsx(output1,"BioEOutput.xlsx", sheetName="Weights & Pvals", col.names=T)
write.csv(output1, "Results_Weights&Pvals.csv")

##################
# Daily Growth
output=cbind(seq(1:Input$N.steps), "Growth(g)"=Results$Growth, 
  "Growth(Joules)"=Results$Growth_j)
col.names[1] = "time"
for (i in 1:Input$N.sites){
col.names[i+1] = paste(site.names[i],"Growth(g)")
col.names[Input$N.sites + i+1] = paste(site.names[i],"Growth(joules)")
}
colnames(output) = col.names
#write.xlsx(output,"BioEOutput.xlsx", sheetName="Growth", col.names=T, append=T)
write.csv(output, "Results_Growth.csv")
########################
# Consumption
output=cbind(seq(1:Input$N.steps), "Cons(g)"=Results$Consumption, 
  "Cons(Joules)"=Results$Consumption_j)
col.names[1] = "time"
for (i in 1:Input$N.sites){
col.names[i+1] = paste(site.names[i],"Cons(g)")
col.names[Input$N.sites + i+1] = paste(site.names[i],"Cons(joules)")
}
colnames(output) = col.names
#write.xlsx(output,"BioEOutput.xlsx", sheetName="Consumption", col.names=T, append=T)
write.csv(output, "Results_Consumption.csv")

##################
names(Results)
# Egestion
output=cbind(seq(1:Input$N.steps), "Egestion(g)"=Results$Egestion, 
  "Egestion(Joules)"=Results$Egestion_j)
col.names[1] = "time"
for (i in 1:Input$N.sites){
col.names[i+1] = paste(site.names[i],"Egestion(g)")
col.names[Input$N.sites + i+1] = paste(site.names[i],"Egestion(joules)")
}
colnames(output) = col.names
#write.xlsx(output,"BioEOutput.xlsx", sheetName="Egestion", col.names=T, append=T)
write.csv(output, "Results_Egestion.csv")

##################
# Daily Excretion
output=cbind(seq(1:Input$N.steps), "Excretion(g)"=Results$Excretion, 
  "Excretion(Joules)"=Results$Excretion_j)
col.names[1] = "time"
for (i in 1:Input$N.sites){
col.names[i+1] = paste(site.names[i],"Excretion(g)")
col.names[Input$N.sites + i+1] = paste(site.names[i],"Excretion(joules)")
}
colnames(output) = col.names
#write.xlsx(output,"BioEOutput.xlsx", sheetName="Excretion", col.names=T, append=T)
write.csv(output, "Results_Excretion.csv")

##################
# Daily Respiration
output=cbind(seq(1:Input$N.steps), "Respiration(g)"=Results$Respiration, 
  "Respiration(Joules)"=Results$Respiration_j)
col.names[1] = "time"
for (i in 1:Input$N.sites){
col.names[i+1] = paste(site.names[i],"Repiration(g)")
col.names[Input$N.sites + i+1] = paste(site.names[i],"Respiration(joules)")
}
colnames(output) = col.names
#write.xlsx(output,"BioEOutput.xlsx", sheetName="Respiration", col.names=T, append=T)
write.csv(output,"Results_Respiration.csv")
} # End of Function write.output. (Note - Nothing returned from this function).












