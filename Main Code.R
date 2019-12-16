************************************************************************************************
* 					     #DATA_DOWNLOADING_PART_AND_SAVE				                       *
************************************************************************************************
rm(list=ls(all=TRUE))

############# Downloading data from federalreserve web site ####################

if(! "downloader" %in% installed.packages()) install.packages("downloader", depend = TRUE)
	library(downloader)
	
if(! "readstata13" %in% installed.packages()) install.packages("readstata13", depend = TRUE)
	library(readstata13)
	
if(! "foreign" %in% installed.packages()) install.packages("foreign", depend = TRUE)
	library(foreign)
	
if(!file.exists("scf1998.rda") & !file.exists("scf2001.rda")) {
	  downloads <-
		data.frame(
			year = c( 1998, 2001),
			extract = c( 'scfp1998s' , 'scfp2001s' )
		)
	  read.scf <-
		function( zip.fn , http.path = "http://www.federalreserve.gov/econresdata/scf/files/" ){
			tf <- tempfile() ; td <- tempdir()
			download.file(paste0( http.path , zip.fn , ".zip" ), tf , mode = 'wb')
			lfn <- unzip( tf , exdir = td )
			x <- read.dta13(lfn)
			file.remove( tf )
			file.remove( lfn )
			return( x )
	}

	  for ( i in seq( nrow( downloads ) ) ) {
		scf.e <- read.scf( downloads[ i , 'extract' ] ) 
		imp1 <- scf.e[ substr( scf.e$Y1 , nchar( scf.e$Y1 ) , nchar( scf.e$Y1 ) ) == 1 , ]
				
		gc()
		
		imp1 <- imp1[ order( imp1$YY1 ) , ]
		
		file.to.save <- paste0( 'scf' , downloads[ i , 'year' ] , '.rda' )
		save( imp1 ,  file = file.to.save )
		
		rm( imp1 )
		gc()
	}
	 }else {
		print("files are existed")
	 }
	 

######################################################

data=list()


load("scf1998.rda")
data[[1]]=imp1
rm( imp1 )

load("scf2001.rda")
data[[2]]=imp1
rm( imp1 )


rm( imp1 )

AttrName=function(x) {
	names(x)=toupper(names(x))
	return(x)
}


for (i in 1:2) {
	data[[i]]=AttrName(data[[i]])
}



print.result=function (x, y) {
	resmatrix=matrix(12, 2, 6)
	colnames(resmatrix)=c("Names", "Classes number", "Good instances", 
			"Bad instances", "Total instances", "Total Features")
	rownames(resmatrix)=c("Training", "Test")

	resmatrix[1,1]="SCF2010"
	resmatrix[2,1]="SCF2013"

	resmatrix[1,2]=length(unique(x$LATE60))
	resmatrix[2,2]=length(unique(y$LATE60))

	resmatrix[1,3]=length(which(x$LATE60==0))
	resmatrix[2,3]=length(which(y$LATE60==0))

	resmatrix[1,4]=length(which(x$LATE60==1))
	resmatrix[2,4]=length(which(y$LATE60==1))

	resmatrix[1,5]=length((x$LATE60))
	resmatrix[2,5]=length((y$LATE60))

	resmatrix[1,6]=length((x[1,]))
	resmatrix[2,6]=length((y[1,]))
	
	return(resmatrix)
	}


print(print.result(data[[1]], data[[2]]))



log_scale = function(x) {
	x[which(x==0)]=x[which(x==0)]+1
	x[which(x>0)] = log(x[which(x>0)],10)
	x[which(x<0)]=-log(abs(x[which(x<0)]),10)
	return(x)
}

Convert=function(x) {

x=x[which(x$DEBT>0), ]

x$MMDA=log_scale(x$MMDA)
x$MMMF=log_scale(x$MMMF)
x$MMA=log_scale(x$MMA)
x$CHECKING=log_scale(x$CHECKING)
x$SAVING=log_scale(x$SAVING)
x$CALL=log_scale(x$CALL)
x$LIQ=log_scale(x$LIQ)
x$CDS=log_scale(x$CDS)
x$STMUTF=log_scale(x$STMUTF)
x$TFBMUTF=log_scale(x$TFBMUTF)
x$GBMUTF=log_scale(x$GBMUTF)
x$OBMUTF=log_scale(x$OBMUTF)
x$COMUTF=log_scale(x$COMUTF)
x$OMUTF=log_scale(x$OMUTF)
x$NMMF=log_scale(x$NMMF)
x$SAVBND=log_scale(x$SAVBND)
x$STOCKS=log_scale(x$STOCKS)
x$NOTXBND=log_scale(x$NOTXBND)
x$MORTBND=log_scale(x$MORTBND)
x$GOVTBND=log_scale(x$GOVTBND)
x$OBND=log_scale(x$OBND)
x$BOND=log_scale(x$BOND)
x$CASHLI=log_scale(x$CASHLI)
x$ANNUIT=log_scale(x$ANNUIT)
x$TRUSTS=log_scale(x$TRUSTS)
x$OTHMA=log_scale(x$OTHMA)
x$FUTPEN=log_scale(x$FUTPEN)
x$CURRPEN=log_scale(x$CURRPEN)
x$THRIFT=log_scale(x$THRIFT)
x$IRAKH=log_scale(x$IRAKH)
x$RETQLIQ=log_scale(x$RETQLIQ)
x$RETEQ=log_scale(x$RETEQ)
x$OTHFIN=log_scale(x$OTHFIN)
x$DEQ=log_scale(x$DEQ)
x$EQUITY=log_scale(x$EQUITY)
x$HOMEEQ=log_scale(x$HOMEEQ)
x$VLEASE=log_scale(x$VLEASE)
x$PENACCTWD=log_scale(x$PENACCTWD)
x$FIN=log_scale(x$FIN)

x$VEHIC=log_scale(x$VEHIC)
x$HOUSES=log_scale(x$HOUSES)
x$ORESRE=log_scale(x$ORESRE)
x$NNRESRE=log_scale(x$NNRESRE)
x$ACTBUS=log_scale(x$ACTBUS)
x$NONACTBUS=log_scale(x$NONACTBUS)
x$BUS=log_scale(x$BUS)
x$OTHNFIN=log_scale(x$OTHNFIN)
x$NFIN=log_scale(x$NFIN)


x$FARMBUS=log_scale(x$FARMBUS)
x$NHNFIN=log_scale(x$NHNFIN)

x$ASSET=log_scale(x$ASSET)


x$NH_MORT=x$NH_MORT/x$DEBT
x$HELOC=x$HELOC/x$DEBT
x$MRTHEL=x$MRTHEL/x$DEBT
x$RESDBT=x$RESDBT/x$DEBT
x$OTHLOC=x$OTHLOC/x$DEBT
x$CCBAL=x$CCBAL/x$DEBT
x$EDN_INST=x$EDN_INST/x$DEBT
x$VEH_INST=x$VEH_INST/x$DEBT
x$OTH_INST=x$OTH_INST/x$DEBT
x$INSTALL=x$INSTALL/x$DEBT
x$ODEBT=x$ODEBT/x$DEBT

x$DEBT=log_scale(x$DEBT)

x$INSTUTION=(x$LLOAN1+x$LLOAN2+x$LLOAN3+x$LLOAN4+
	x$LLOAN5+x$LLOAN6+x$LLOAN7+
	x$LLOAN8+x$LLOAN9+x$LLOAN10+x$LLOAN11+x$LLOAN12)
x$INSTUTION=log_scale(x$INSTUTION)

x$LLOAN1[which(x$LLOAN1==0)]=0
x$LLOAN1[which(x$LLOAN1>0)]=log(x$LLOAN1[which(x$LLOAN1>0)])
x$LLOAN2[which(x$LLOAN2==0)]=0
x$LLOAN2[which(x$LLOAN2>0)]=log(x$LLOAN2[which(x$LLOAN2>0)])
x$LLOAN3[which(x$LLOAN3==0)]=0
x$LLOAN3[which(x$LLOAN3>0)]=log(x$LLOAN3[which(x$LLOAN3>0)])
x$LLOAN4[which(x$LLOAN4==0)]=0
x$LLOAN4[which(x$LLOAN4>0)]=log(x$LLOAN4[which(x$LLOAN4>0)])
x$LLOAN5[which(x$LLOAN5==0)]=0
x$LLOAN5[which(x$LLOAN5>0)]=log(x$LLOAN5[which(x$LLOAN5>0)])
x$LLOAN6[which(x$LLOAN6==0)]=0
x$LLOAN6[which(x$LLOAN6>0)]=log(x$LLOAN6[which(x$LLOAN6>0)])
x$LLOAN7[which(x$LLOAN7==0)]=0
x$LLOAN7[which(x$LLOAN7>0)]=log(x$LLOAN7[which(x$LLOAN7>0)])
x$LLOAN8[which(x$LLOAN8==0)]=0
x$LLOAN8[which(x$LLOAN8>0)]=log(x$LLOAN8[which(x$LLOAN8>0)])
x$LLOAN9[which(x$LLOAN9==0)]=0
x$LLOAN9[which(x$LLOAN9>0)]=log(x$LLOAN9[which(x$LLOAN9>0)])
x$LLOAN10[which(x$LLOAN10==0)]=0
x$LLOAN10[which(x$LLOAN10>0)]=log(x$LLOAN10[which(x$LLOAN10>0)])
x$LLOAN11[which(x$LLOAN11==0)]=0
x$LLOAN11[which(x$LLOAN11>0)]=log(x$LLOAN11[which(x$LLOAN11>0)])
x$LLOAN12[which(x$LLOAN12==0)]=0
x$LLOAN12[which(x$LLOAN12>0)]=log(x$LLOAN12[which(x$LLOAN12>0)])

x$PURPOSE=x$PLOAN1+x$PLOAN2+x$PLOAN3+x$PLOAN4+
		x$PLOAN5+x$PLOAN6+x$PLOAN7+x$PLOAN8
x$PURPOSE[which(x$PURPOSE==0)]=0
x$PURPOSE[which(x$PURPOSE>0)]=log(x$PURPOSE[which(x$PURPOSE>0)])
				
x$PLOAN1[which(x$PLOAN1==0)]=0
x$PLOAN1[which(x$PLOAN1>0)]=log(x$PLOAN1[which(x$PLOAN1>0)])
x$PLOAN2[which(x$PLOAN2==0)]=0
x$PLOAN2[which(x$PLOAN2>0)]=log(x$PLOAN2[which(x$PLOAN2>0)])
x$PLOAN3[which(x$PLOAN3==0)]=0
x$PLOAN3[which(x$PLOAN3>0)]=log(x$PLOAN3[which(x$PLOAN3>0)])
x$PLOAN4[which(x$PLOAN4==0)]=0
x$PLOAN4[which(x$PLOAN4>0)]=log(x$PLOAN4[which(x$PLOAN4>0)])
x$PLOAN5[which(x$PLOAN5==0)]=0
x$PLOAN5[which(x$PLOAN5>0)]=log(x$PLOAN5[which(x$PLOAN5>0)])
x$PLOAN6[which(x$PLOAN6==0)]=0
x$PLOAN6[which(x$PLOAN6>0)]=log(x$PLOAN6[which(x$PLOAN6>0)])
x$PLOAN7[which(x$PLOAN7==0)]=0
x$PLOAN7[which(x$PLOAN7>0)]=log(x$PLOAN7[which(x$PLOAN7>0)])
x$PLOAN8[which(x$PLOAN8==0)]=0
x$PLOAN8[which(x$PLOAN8>0)]=log(x$PLOAN8[which(x$PLOAN8>0)])

x$MORTGAGE=x$MORT1+x$MORT2+x$MORT3
x$MORTGAGE[which(x$MORTGAGE==0)]=0
x$MORTGAGE[which(x$MORTGAGE>0)]=log(x$MORTGAGE[which(x$MORTGAGE>0)])

x$MORT1[which(x$MORT1==0)]=0
x$MORT1[which(x$MORT1>0)]=log(x$MORT1[which(x$MORT1>0)])
x$MORT2[which(x$MORT2==0)]=0
x$MORT2[which(x$MORT2>0)]=log(x$MORT2[which(x$MORT2>0)])
x$MORT3[which(x$MORT3==0)]=0
x$MORT3[which(x$MORT3>0)]=log(x$MORT3[which(x$MORT3>0)])


x$MORTPAY=x$PAYMORT1+x$PAYMORT2+x$PAYMORT3+x$PAYMORTO
x$MORTPAY[which(x$MORTPAY==0)]=0
x$MORTPAY[which(x$MORTPAY>0)]=log(x$MORTPAY[which(x$MORTPAY>0)])

x$PAYMORT1[which(x$PAYMORT1>0)]=1
x$PAYMORT2[which(x$PAYMORT2>0)]=1
x$PAYMORT3[which(x$PAYMORT3>0)]=1
x$PAYMORTO[which(x$PAYMORTO>0)]=1
x$MORTNUM=x$PAYMORT1+x$PAYMORT2+x$PAYMORT3+x$PAYMORTO

x$LOCPAY=x$PAYLOC1+x$PAYLOC2+x$PAYLOC3+x$PAYLOCO
x$LOCPAY[which(x$LOCPAYY==0)]=0
x$LOCPAY[which(x$LOCPAY>0)]=log(x$LOCPAY[which(x$LOCPAY>0)])

x$PAYLOC1[which(x$PAYLOC1>0)]=1
x$PAYLOC2[which(x$PAYLOC2>0)]=1
x$PAYLOC3[which(x$PAYLOC3>0)]=1
x$PAYLOCO[which(x$PAYLOCO>0)]=1
x$MORTNUM=x$PAYLOC1+x$PAYLOC2+x$PAYLOC3+x$PAYLOCO

x$HIPAY=x$PAYHI1+x$PAYHI2
x$HIPAY[which(x$HIPAY==0)]=0
x$HIPAY[which(x$HIPAY>0)]=log(x$HIPAY[which(x$HIPAY>0)])

x$PAYHI1[which(x$PAYHI1>0)]=1
x$PAYHI2[which(x$PAYHI2>0)]=1
x$IMNUM=x$PAYHI1+x$PAYHI2

x$LCPAY=x$PAYLC1+x$PAYLC2+x$PAYLCO
x$LCPAY[which(x$LCPAY==0)]=0
x$LCPAY[which(x$LCPAY>0)]=log(x$LCPAY[which(x$LCPAY>0)])

x$PAYLC1[which(x$PAYLC1>0)]=1
x$PAYLC2[which(x$PAYLC2>0)]=1
x$PAYLCO[which(x$PAYLCO>0)]=1
x$LCNUM=x$PAYLC1+x$PAYLC2+x$PAYLCO

x$OREPAY=x$PAYORE1+x$PAYORE2+x$PAYORE3+x$PAYOREV
x$OREPAY[which(x$OREPAY==0)]=0
x$OREPAY[which(x$OREPAY>0)]=log(x$OREPAY[which(x$OREPAY>0)])

x$PAYORE1[which(x$PAYORE1>0)]=1
x$PAYORE2[which(x$PAYORE2>0)]=1
x$PAYORE3[which(x$PAYORE3>0)]=1
x$PAYOREV[which(x$PAYOREV>0)]=1
x$ORENUM=x$PAYORE1+x$PAYORE2+x$PAYORE3+x$PAYOREV

x$EHPAY=x$PAYVEH1+x$PAYVEH2+x$PAYVEH3+x$PAYVEH4+x$PAYVEHM
x$EHPAY[which(x$EHPAY==0)]=0
x$EHPAY[which(x$EHPAY>0)]=log(x$EHPAY[which(x$EHPAY>0)])

x$PAYVEH1[which(x$PAYVEH1>0)]=1
x$PAYVEH2[which(x$PAYVEH2>0)]=1
x$PAYVEH3[which(x$PAYVEH3>0)]=1
x$PAYVEH4[which(x$PAYVEH4>0)]=1
x$PAYVEHM[which(x$PAYVEHM>0)]=1
x$VENUM=x$PAYVEH1+x$PAYVEH2+x$PAYVEH3+x$PAYVEHM

x$EOPAY=x$PAYVEO1+x$PAYVEO2+x$PAYVEOM
x$EOPAY[which(x$EOPAY==0)]=0
x$EOPAY[which(x$EOPAY>0)]=log(x$EOPAY[which(x$EOPAY>0)])

x$PAYVEO1[which(x$PAYVEO1>0)]=1
x$PAYVEO2[which(x$PAYVEO2>0)]=1
x$PAYVEOM[which(x$PAYVEOM>0)]=1
x$VEONUM=x$PAYVEO1+x$PAYVEO1+x$PAYVEO1

x$EDUPAY=x$PAYEDU1+x$PAYEDU2+x$PAYEDU3+x$PAYEDU4+
	x$PAYEDU5+x$PAYEDU6+x$PAYEDU7
x$EDUPAY[which(x$EDUPAY==0)]=0
x$EDUPAY[which(x$EDUPAY>0)]=log(x$EDUPAY[which(x$EDUPAY>0)])

x$PAYEDU1[which(x$PAYEDU1>0)]=1
x$PAYEDU2[which(x$PAYEDU2>0)]=1
x$PAYEDU3[which(x$PAYEDU3>0)]=1
x$PAYEDU4[which(x$PAYEDU4>0)]=1
x$PAYEDU5[which(x$PAYEDU5>0)]=1
x$PAYEDU6[which(x$PAYEDU6>0)]=1
x$PAYEDU7[which(x$PAYEDU7>0)]=1
x$EDUNUM=x$PAYEDU1+x$PAYEDU2+x$PAYEDU3+x$PAYEDU4+
	x$PAYEDU5+x$PAYEDU6+x$PAYEDU7

x$ILNPAY=x$PAYILN1+x$PAYILN2+x$PAYILN3+x$PAYILN4+
	x$PAYILN5+x$PAYILN6+x$PAYILN7
x$ILNPAY[which(x$ILNPAY==0)]=0
x$ILNPAY[which(x$ILNPAY>0)]=log(x$ILNPAY[which(x$ILNPAY>0)])

x$PAYILN1[which(x$PAYILN1>0)]=1
x$PAYILN2[which(x$PAYILN2>0)]=1
x$PAYILN3[which(x$PAYILN3>0)]=1
x$PAYILN4[which(x$PAYILN4>0)]=1
x$PAYILN5[which(x$PAYILN5>0)]=1
x$PAYILN6[which(x$PAYILN6>0)]=1
x$PAYILN7[which(x$PAYILN7>0)]=1
x$ILNNUM=x$PAYILN1+x$PAYILN2+x$PAYILN3+x$PAYILN4+
	x$PAYILN5+x$PAYILN6+x$PAYILN7

x$PAYMARG[which(x$PAYMARG==0)]=0
x$PAYMARG[which(x$PAYMARG>0)]=log(x$PAYMARG[which(x$PAYMARG>0)])
x$PAYINS[which(x$PAYINS==0)]=0
x$PAYINS[which(x$PAYINS>0)]=log(x$PAYINS[which(x$PAYINS>0)])

x$PENPAY=x$PAYPEN1+x$PAYPEN2+x$PAYPEN3+x$PAYPEN4+
	x$PAYPEN5+x$PAYPEN6
x$PENPAY[which(x$PENPAY==0)]=0
x$PENPAY[which(x$PENPAY>0)]=log(x$PENPAY[which(x$PENPAY>0)])

x$PAYPEN1[which(x$PAYPEN1>0)]=1
x$PAYPEN2[which(x$PAYPEN2>0)]=1
x$PAYPEN3[which(x$PAYPEN3>0)]=1
x$PAYPEN4[which(x$PAYPEN4>0)]=1
x$PAYPEN5[which(x$PAYPEN5>0)]=1
x$PAYPEN6[which(x$PAYPEN6>0)]=1
x$PENNUM=x$PAYPEN1+x$PAYPEN2+x$PAYPEN3+x$PAYPEN4+
	x$PAYPEN5+x$PAYPEN6

x$REVPAY=log_scale(x$REVPAY)
x$MORTPAY=log_scale(x$MORTPAY)
x$CONSPAY=log_scale(x$CONSPAY)
x$TPAY=log_scale(x$TPAY)

x$NORMINC=log_scale(x$NORMINC)

x$BUSSEFARMINC=log_scale(x$BUSSEFARMINC)
x$INTDIVINC=log_scale(x$INTDIVINC)
x$SSRETINC=log_scale(x$SSRETINC)
x$TRANSFOTHINC=log_scale(x$TRANSFOTHINC)
x$WAGEINC=log_scale(x$WAGEINC)
x$KGINC=log_scale(x$KGINC)
x$INCOME=log_scale(x$INCOME)

x$KGTOTAL[which(x$KGTOTAL==0)]=1
x$KGBUS=x$KGBUS/x$KGTOTAL
x$KGHOUSE=x$KGHOUSE/x$KGTOTAL
x$KGORE=x$KGORE/x$KGTOTAL
x$KGSTMF=x$KGSTMF/x$KGTOTAL

x$KGTOTAL[which(x$KGTOTAL==1)]=0
x$KGTOTAL[which(x$KGTOTAL>0)]=log(x$KGTOTAL[which(x$KGTOTAL>0)])
x$KGTOTAL[which(x$KGTOTAL<0)]=-log(abs(x$KGTOTAL[which(x$KGTOTAL<0)]))
x$FARMBUS_KG[which(x$FARMBUS_KG<0)]=-log(abs(x$FARMBUS_KG[which(x$FARMBUS_KG<0)]))
x$FARMBUS_KG[which(x$FARMBUS_KG==0)]=0
x$FARMBUS_KG[which(x$FARMBUS_KG>0)]=log(x$FARMBUS_KG[which(x$FARMBUS_KG>0)])

x$NETWORTH[which(x$NETWORTH==0)]=0
x$NETWORTH[which(x$NETWORTH>0)]=log(x$NETWORTH[which(x$NETWORTH>0)])
x$NETWORTH[which(x$NETWORTH<0)]=-log(abs(x$NETWORTH[which(x$NETWORTH<0)]))

return(x)
}



remove_missing = function(x) {
col=c()
	for(i in (1:length(x[1,]))) {
		if(all(is.na(x[,i]))) {
			col=c(col, i)
		}
	}
	if(length(col)==0) {
		print('no missing data')
	}else{
		x=x[,-col]
	}
	return(x)
}


for (i in 1:2) {
	data[[i]]=Convert(data[[i]])
	data[[i]]=remove_missing(data[[i]])
}


print(print.result(data[[1]], data[[2]]))


att_name=c()
for(i in 1:2) {
	att_name=c(att_name, names(data[[i]]))
}

count=as.data.frame(table(att_name))
att_name = count[which(count[,2]==2),1]

for (i in 1:2) {
	data[[i]]=data[[i]][,match(att_name, names(data[[i]]))]
}

	

print(print.result(data[[1]], data[[2]]))



Outlier=function(x) {
	c=c()
	for(i in 1:length(x[1,])) {
	if(length(unique(x[, i]))<19) {
			c=c
	}else if (sd(x[,i])==0 ) {
			print(names(x[,i]))
	}else{
		c=c(c, x$YY1[which(abs((x[,i]-mean(x[,i]))/sd(x[,i]))>qlnorm(0.99))])
		}	
	}	
	c=unique(c)
	x=x[-match(c, x$YY1), ]
	
	c=c("YY1", "Y1", "WGT", "WILSH", "LATE")
	x=x[,-match(c, names(x))]
	
	return(x)
}

training=data[[1]]
test=data[[2]]

###############            Outlier detection                       #########################
training=Outlier(training)
test=Outlier(test)

print.result(training, test)




feature.selection=function(x){

#################     function for calculating t test for continuoes data   ####################
t.test4cont=function(x, y) {
	a=x[which(y==1)]
	b=x[which(y==0)]
	n.a = length(a)
	n.b = length(b)
	if((sqrt(sd(a)/n.a + sd(b)/n.b))==0 | is.na(sqrt(sd(a)/n.a + sd(b)/n.b))) {
	t.test=sample(10:99, 1, replace=T)/100
	}else{
	t.test = (mean(a) - mean(b)) / (sqrt(sd(a)/n.a + sd(b)/n.b))
	}
	return(1-pt(abs(t.test), df=(n.a+n.b-2)))
}

#################   function for calculating chi-square for discrete data   ###################

chi.test4disc=function(x, y) {
	tab=table(y, x)
	etab=matrix(1:(2*length(tab[1,])), 2, length(tab[1,]))
	for(i in 1:length(tab[,1])) {
		for(j in 1:length(tab[1,])) {
			etab[i, j]=(sum(tab[,j])*sum(tab[i,]))/sum(tab)
		}
	}
	
	chi.square=sample(10:99, 1, replace=T)/100
	for(i in 1:length(tab[,1])) {
		for(j in 1:length(tab[1,])) {
			chi.square=chi.square + ((tab[i, j]-etab[i, j])^2)/etab[i, j]
		}
	}
	return(1-pchisq(abs(chi.square), df=(length(tab[1, ])-1)))
}

if(! "FSelectorRcpp" %in% installed.packages()) install.packages("FSelectorRcpp", depend = TRUE)
library(FSelectorRcpp)
library(FSelector)

weights=matrix(1:(3*length(x[1,])), length(x[1,]), 3)
colnames(weights)=c("NAME", "Importance", "P_value")
weights[,1]=as.character(names(x))

for(i in 1:length(x[1,])) {
	if(length(unique(x[,i]))==1) {
		weights[i, 3] = sample(10:99, 1, replace=T)/100
	}else if(length(unique(x[,i]))>19) {
		weights[i, 3]=t.test4cont(x[,i], x$LATE60)
	}else{
		weights[i, 3]=chi.test4disc(x[,i], x$LATE60)
	}
}

weights=as.data.frame(weights)
weights[,3] = as.numeric(as.character(weights[,3]))
weights = weights[order(weights[,3], decreasing = TRUE),]
weights[,2] = 0.05

weights=weights[-match("LATE60", as.character(weights[,1])), ]

if(! "ggplot2" %in% installed.packages()) install.packages("ggplot2", depend = TRUE)
library(ggplot2)
if(! "ggpmisc" %in% installed.packages()) install.packages("ggpmisc", depend = TRUE)
library(ggpmisc)
if(! "ggpubr" %in% installed.packages()) install.packages("ggpubr", depend = TRUE)
library(ggpubr)
if(! "ggrepel" %in% installed.packages()) install.packages("ggrepel", depend = TRUE)
library(ggrepel)

weights$Features = c(1:length(weights[,1]))
weights$Statistical_Significance='Significant'
weights$Statistical_Significance[which(weights$P_value>0.05)]='Not significant'
weights$Statistical_Significance=as.factor(weights$Statistical_Significance)

b <- ggplot(weights, aes(x = Features, y = P_value))
.labs=weights$NAME


tiff(paste('Hypothesis analysis', '.tif', sep=""), width = 15, height = 6, units = 'in', res = 1080)
b + geom_point(aes(color = Statistical_Significance)) +
  geom_text_repel(aes(label = .labs,  color = Statistical_Significance), size = 1, angle=90, point.padding=unit(0.02, "npc"), 
  segment.size  = 0.1) + theme(legend.position="bottom") +
  scale_color_manual(values = c("#FC4E07", "#00AFBB"))
dev.off()

weights=weights[-which(weights[,3]>0.05), ]
x=x[ ,match(c(as.character(weights[,1]), "LATE60"), names(x))]

weights[,2]=as.matrix(random.forest.importance(LATE60~., x,  importance.type = 1))

weights=as.data.frame(weights)
print(weights)


weights[,2]=as.numeric(as.character(weights[,2]))
weights=weights[order(weights[,2], decreasing = TRUE),]
weights$Features = c(1:length(weights[,1]))


x1=x[ ,match(as.character(weights[,1]), names(x))]


#################  correlation and detecting some not important attributes  ###################

if(! "spatstat" %in% installed.packages()) install.packages("spatstat", depend = TRUE)
library(spatstat)

corr <- cor(x1)

weights1=weights
i=0
while(i!=length(corr[1,])) {
	i=i+1
	j=1
	while(length(which(abs(corr[i, ])>=0.5 & abs(corr[i, ])<=1))!=1) {
		  if(abs(corr[i, j])>=0.5 & i!=j & weights1[i,2]>=weights1[j,2]) {
			weights1=weights1[-j, ]
			corr=corr[-j, ]
			corr=corr[, -j]
			j=j
		}else if(abs(corr[i, j])>=0.5 & i!=j & weights1[i,2]<weights1[j,2]) {
			weights1=weights1[-i, ]
			corr=corr[-i, ]
			corr=corr[, -i]
			j=1
			i=i
		}else {
			j=j+1
		}
	}
}

imp_attr=c("LATE60", as.character(weights1[,1]))
x=x[ ,match(imp_attr, names(x))]
weights$Feature_Status = 'High correlated with other important feature'
weights$Feature_Status[match(weights1$NAME, weights$NAME)]='Higher importance than other similar features'

b <- ggplot(weights, aes(x = Features, y = Importance))
.labs=weights$NAME

tiff(paste('Random forest importance', '.tif', sep=""), width = 10, height = 4, units = 'in', res = 1080)
b + geom_point(aes(color = Feature_Status)) +
  geom_text_repel(aes(label = .labs,  color = Feature_Status), size = 1, angle=90, point.padding=unit(0.02, "npc"), 
  segment.size  = 0.1) + theme(legend.position="bottom") +
  scale_color_manual(values = c("#FC4E07", "#00AFBB"))
dev.off()

return(x)
}


###############    Filter approach for Feature selectin            #########################
training=feature.selection(training)

print.result(training, test)


file.to.save <- ( 'training.rda' )
		save( training, file = file.to.save )
file.to.save <- ( 'test.rda' )
		save( test, file = file.to.save )

************************************************************************************************
*	   #ANALYSE PART USING LOGISTIC REGRESSION, SVM AND RF REGRESSION                          *
************************************************************************************************
	
load("training.rda")
load("test.rda")

		
###################################  SVM regression  #####################################

if(! "e1071" %in% installed.packages()) install.packages("e1071", depend = TRUE)
library(e1071)

tuning = function(data, nfold=5) {
if(! "e1071" %in% installed.packages()) install.packages("e1071", depend = TRUE)
library(e1071)
if(! "ROCR" %in% installed.packages()) install.packages("ROCR", depend = TRUE)
library(ROCR)
	ranges=list(epsilon = c(0.05, 0.25, 0.5, 0.75), gamma=10^(-3:-1), cost=10^(1:3))
	para.set=expand.grid(ranges)
	
	training=list()
	test=list()
	row_names=rownames(data)
	for (i in 1:nfold) {
		sample = sample(row_names, size=floor(nrow(data)/nfold), replace = FALSE)
		test[[i]] <- data[match(sample, rownames(data)), ]
		training[[i]]  <- data[-match(sample, rownames(data)), ]
		row_names=row_names[-match(sample, row_names)]
		print(length(row_names))
	}
	
	current_auc=0
	for (i in 1:length(para.set[,1])) {
		model=list()
		AUC=matrix(1:nfold, nfold, 1)
		for(j in 1:nfold) {
		model[[j]] = svm(LATE60~., data=training[[j]], predict.func = predict,
		probability=TRUE, epsilon = para.set[i,1], gamma=para.set[i,2], cost=para.set[i,3])
		fit <- predict(model[[j]], type="response", newdata=test[[j]])
		pred = prediction(fit, test[[j]]$LATE60)
		perf <- performance(pred, "tpr", "fpr")
		AUC[j] = performance(pred, "auc")@y.values[[1]]
		}
		model_auc=mean(AUC)
		if(current_auc<model_auc) {
			best_model = model
			current_auc=model_auc
			print(current_auc)
		}else{
			print("not improve")
		}
	}
	return(best_model)
}


svm_model=tuning(training, nfold=10)
				
				
file.to.save <- ( 'svm_model.rda' )
		save( svm_model, file = file.to.save)
		


############################  Random Forest regression  #################################


if(! "randomForest" %in% installed.packages()) install.packages("randomForest", depend = TRUE)
library(randomForest)
if(! "e1071" %in% installed.packages()) install.packages("e1071", depend = TRUE)
library(e1071)

load('training.rda')

parametrall=tune(randomForest, LATE60~., data=training, cross = 10,
			ranges=list(mtry=seq(3, 21, 3), nodesize =seq(20, 110, 30), ntree = seq(500, 2500, 1000)))

			
file.to.save <- ( 'modelRF.rda' )
		save( parametrall, file = file.to.save)
		
rf_model=list()				
for (i in 1:10) {
	rf_model[[i]] =randomForest(LATE60~., data=training, ranges=list(mtry=parametrall$best.parameters[1], 
		nodesize=parametrall$best.parameters[2], ntree=parametrall$best.parameters[3])) 
}
				
file.to.save <- ( 'rf_model.rda' )
		save( rf_model, file = file.to.save)

	
	
############################  XGBoost regression  #################################

if(! "xgboost" %in% installed.packages()) install.packages("xgboost", depend = TRUE)
library(xgboost)
library(ROCR)


tune.xgboost = function(data) {
		trainingy=as.matrix(data$LATE60)
		trainingx=as.matrix(data[,-1])
		
		ranges=list(max_depth=seq(2, 8, 2), min_child_weight=c(1:4), eta=0.1, subsample=c(0.9,0.95,1), 
			colsample_bytree=c(0.9,0.95,1), nthread=60, gamma=c(0, 0.001), max_delta_step = c(0.4,0.6,0.8,1))
		para.set=expand.grid(ranges)
		auc=0
		
		for(i in 1:length(para.set[,1])) {
			model = xgb.cv(data = trainingx, label = trainingy, prediction = TRUE, nfold=10, max_depth = para.set[i,1], min_child_weight= para.set[i, 2], eta = para.set[i,3], 
				subsample = para.set[i,4], colsample_bytree=para.set[i,5], nthread = para.set[i,6], gamma=para.set[i,7],
				max_delta_step=para.set[i,8], nrounds = 500, objective = "binary:logistic", eval_metric='auc', callbacks=list(cb.early.stop(100, maximize = TRUE, 
				metric_name = 'test_auc', verbose = 1), cb.cv.predict(save_models = TRUE)))
			auc_current=(model$evaluation_log[model$best_iteration,4])
			if(auc<auc_current) {
				best_model=model
				auc=auc_current
				print(auc)
				rm(model)
				gc()
			}else{
				rm(model)
				best_model=best_model
				print('not improve')
				gc()
		}
	}
	return(best_model)
}




XGBoost_model= tune.xgboost(training)

file.to.save <- ( 'XGBoost_model.rda' )
		save( XGBoost_model, file = file.to.save)

	

############################  Multi-Layer perceptron #################################

library(dummies)
library(keras)

test=test[,match(names(training), names(test))]
for (i in 1:length(training[1,])) {
	max_val = max(training[,i], test[,i])
	min_val = min(training[,i], test[,i])
	
	training[,i] = (training[,i] - min_val)/(max_val-min_val)
	test[,i] = (test[,i] - min_val)/(max_val-min_val)
}

traininglog=training[,match(names(traininglog), names(training))]
testlog=test[,match(names(testlog), names(test))]

trainingmars=training[,match(names(trainingmars), names(training))]
testmars=test[,match(names(testmars), names(test))]

MLP=function(training, test) {
	library(dummies)
	library(keras)
	result=list()
	fit=list()
	fit_train=list()
	plot_track=list()
	for (i in 1:10) {
		trainingx=dummy.data.frame(training[,-1])

		trainingx=data.matrix(trainingx)
		trainingy=data.matrix(training[,1])

		testx=dummy.data.frame(test[,-1])
		testx=data.matrix(testx)
		testy=data.matrix(test[,1])

		model <- keras_model_sequential()
		model %>%
		layer_dense(units = 8, input_shape = ncol(trainingx)) %>%
		layer_activation('sigmoid') %>%
		layer_dense(units = 1) %>%
		layer_activation('sigmoid')

		model %>% compile(
		   loss = 'binary_crossentropy',
		   optimizer = optimizer_adam( lr= 0.0001 , decay = 1e-6 ),  
		   metrics = c('accuracy')
		 )

		track = model %>% fit(trainingx, trainingy, epochs = 1000, batch_size = 32,
					   callbacks = callback_early_stopping(patience = 25, monitor = 'val_loss'),
					   validation_split = 0.1
		)
		
		plot_track[[i]]=track
		
		fit_test <- model %>% predict(testx, batch_size = 32)
		fit_training <- model %>% predict(trainingx, batch_size = 32)

		fit[[i]] <- fit_test
		fit_train[[i]] <- fit_training
	}
	result$plot=plot_track
	result$fit=fit
	result$fit_train=fit_train
	return(result)
}


MLP_all=MLP(training, test)

file.to.save <- ( 'MLP_all.rda' )
		save( MLP_all, file = file.to.save)
	

DMLP=function(training, test) {
	library(dummies)
	library(keras)
	result=list()
	fit=list()
	fit_train=list()
	plot_track=list()
	for (i in 1:10) {
		trainingx=dummy.data.frame(training[,-1])

		trainingx=data.matrix(trainingx)
		trainingy=data.matrix(training[,1])

		test=test[,match(names(training), names(test))]
		testx=dummy.data.frame(test[,-1])
		testx=data.matrix(testx)
		testy=data.matrix(test[,1])

		model <- keras_model_sequential()
		model %>%
		layer_dense(units = 8, input_shape = ncol(trainingx)) %>%
		layer_activation('sigmoid') %>%
		layer_dense(units = 8, input_shape = ncol(trainingx)) %>%
		layer_activation('sigmoid') %>%
		layer_dense(units = 8, input_shape = ncol(trainingx)) %>%
		layer_activation('sigmoid') %>%
		layer_dense(units = 1) %>%
		layer_activation('sigmoid')

		model %>% compile(
		   loss = 'binary_crossentropy',
		   optimizer = optimizer_adam( lr= 0.0001 , decay = 1e-6 ),  
		   metrics = c('accuracy')
		 )

		track = model %>% fit(trainingx, trainingy, epochs = 1000, batch_size = 32,
					   callbacks = callback_early_stopping(patience = 25, monitor = 'val_loss'),
					   validation_split = 0.1
		)
		
		plot_track[[i]]=track
		
		fit_test <- model %>% predict(testx, batch_size = 32)
		fit_training <- model %>% predict(trainingx, batch_size = 32)

		fit[[i]] <- fit_test
		fit_train[[i]] <- fit_training
	}
	result$plot=plot_track
	result$fit=fit
	result$fit_train=fit_train
	return(result)
}

DMLP_all=DMLP(training, test)

file.to.save <- ( 'DMLP_all.rda' )
		save( DMLP_all, file = file.to.save)
	

MDMLP=function(training, test) {
	library(dummies)
	library(keras)
	result=list()
	fit=list()
	fit_train=list()
	plot_track=list()
	for (i in 1:10) {
		trainingx=dummy.data.frame(training[,-1])

		trainingx=data.matrix(trainingx)
		trainingy=data.matrix(training[,1])

		test=test[,match(names(training), names(test))]
		testx=dummy.data.frame(test[,-1])
		testx=data.matrix(testx)
		testy=data.matrix(test[,1])

		model <- keras_model_sequential()
		model %>%
		layer_dense(units = 8, input_shape = ncol(trainingx)) %>%
		layer_activation('sigmoid') %>%
		layer_dense(units = 8, input_shape = ncol(trainingx)) %>%
		layer_activation('sigmoid') %>%
		layer_dense(units = 8, input_shape = ncol(trainingx)) %>%
		layer_activation('sigmoid') %>%
		layer_dense(units = 8, input_shape = ncol(trainingx)) %>%
		layer_activation('sigmoid') %>%
		layer_dense(units = 8, input_shape = ncol(trainingx)) %>%
		layer_activation('sigmoid') %>%
		layer_dense(units = 1) %>%
		layer_activation('sigmoid')

		model %>% compile(
		   loss = 'binary_crossentropy',
		   optimizer = optimizer_adam( lr= 0.0001 , decay = 1e-6 ),  
		   metrics = c('accuracy')
		 )

		track = model %>% fit(trainingx, trainingy, epochs = 1000, batch_size = 32,
					   callbacks = callback_early_stopping(patience = 25, monitor = 'val_loss'),
					   validation_split = 0.3
		)
		
		plot_track[[i]]=track
		
		fit_test <- model %>% predict(testx, batch_size = 32)
		fit_training <- model %>% predict(trainingx, batch_size = 32)

		fit[[i]] <- fit_test
		fit_train[[i]] <- fit_training
	}
	result$plot=plot_track
	result$fit=fit
	result$fit_train=fit_train
	return(result)
}

MDMLP_all=MDMLP(training, test)

file.to.save <- ( 'MDMLP_all.rda' )
		save( MDMLP_all, file = file.to.save)



		
		
		
