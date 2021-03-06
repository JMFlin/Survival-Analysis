#lue aineisto R:��n ja nime� muuttujat nimill�stage(taudin vaihe),tdeath(kuolinaika),agedg(ik� diagnoosihetkell�),yrdg(vuosi diagnoosihetkell�) jastatus
#(potilaan tila). Muodosta viel� taudin tilaa kuvaavat indikaattorimuuttujat,st2,st3jast4, pit�en tasoa 1 referenssin�. Lataasurvival-paketti ja sovita aneistoon Coxin malli (
#fit1), jossa tarkastelet taudin vaiheen (st2, st3, st4) jai�n (    agedg) vaikutusta kuolinriskiin. K�yt� mallissa vain muuttujien p��vaikutuksia. 
#N�ytt��k� ik� sekoittavan taudin vaiheen merkityst� kuolinriskille? Miten taudin vaihe n�ytt� ennustavan
#kuolinriski�? Laske 4. vaiheen 50-vuotiaan potilaan kuolinriskin suhde samassa vaiheessa olevaan
#40-vuotiaaseen potilaaseen

#K�ytet��n aineistoa larynx.txt, joka sis�lt�� mittauksia 90:st� kurkunp��nsy�p�� sairastaneista miehist�.

#teht 1

# Luetaan aineisto sis��n ja nimet��n muuttujat
hav <- read.table("larynx.txt")
colnames(hav) <- c("stage","tdeath","agedg","yrdg","status")
# muodostetaan indikaattorimuuttujat
hav$st2 <- ifelse(hav$stage==2,1,0)
hav$st3 <- ifelse(hav$stage==3,1,0)
hav$st4 <- ifelse(hav$stage==4,1,0)
# ladataan survival paketti
library(survival)
# sovitetaan Coxin malli
fit1 <- coxph(Surv(tdeath, status)~st2+st3+st4+agedg,data=hav)
summary(fit1)

#I�ll� ei n�yt� olevan suurta vaikutusta elinaikaan. Suhteellisen riskin luottamusv�li on [0.991, 1.048] mik� tarkot-
#taisi kymmeniss� vuosissa noin 90%-120% kokoista riskin muutosta. I�ll� voitaneen olettaa olevan riski� lis��v�
#vaikutus, mutta aineiston koko ei riit� sit� t�m�n tarkemmin estimoimaan. Lasketaan suhteellinen kuolinriski 50v
#vs. 40v 4. vaiheen potilas:
#e^(1.706+0.019*50)/    e^(1.706+0.019*40) = e^0.019*(50-40) = 1.209

#teht 2

#Estimoi nyt pelkk�� i�n vaikutusta testavaa malli (fit2). 
#Laske osam��r�testifit1:n jafit2:n v�lilleja tulosta anova-taulu eri tekij�iden merkityksest� fit1-mallissa. 
#Mit� p��ttelet taulusta? (Vinkki:anova-funktio). 
#Tarkastele sen j�lkeen i�n ja taudin vaiheen indikaattoreiden yhdysvaikutusta mallissa fit3. 
#N�ytt��k� silt�, ett� i�ll� on jossakin taudin vaiheessa enemm�n merkityst�?

# malli jos pelk�st��n ik� kovariaattina
fit2 <- coxph(Surv(tdeath, status)~agedg,data=hav)
# uskottavuusosam��r�testi
obs <- -2*(fit2$loglik[2]-fit1$loglik[2])
1-pchisq(obs,df=3)# yksinkertaistaa liikaa
# anova-taulu
anova(fit1)
# sama testi uudestaan helpommin
anova(fit1, fit2)

#Uskottavuusosam��r�testiin liittyv� p-arvo on hyvin pieni. T�st� voimme p��tell�, ett� malli josta on pudotettu
#taudin vaihe -muuttuja pois on liian yksinkertainen suhteessa rikkaampaan malliin. Siis taudin vaihe on merkityk-
#sellinen muuttuja, jonka avulla voimme paremmin selitt�� potilaiden elinajan vaihtelua. Sanotaan, ett� muuttuja
#on tilastollisesti merkitsev� selitt�j� vasteelle.
#Anova-taulusta saamme yksityiskohtaisempaa tietoa muuttujan merkityksest�. Vaikuttaa silt�, ett� erityisesti tau-
#din vaihe 4 eroaa ensimm�isest� vaiheesta elini�n suhteen. Muihin vaiheisiin 2 ja 3 liittyv�t p-arvot ovat suurehkot,
#joten n�ytt�� vaiheiden 1, 2 ja 3 eroista ei l�ydy.

# sovitetaan malli yhdysvaikutusten kanssa
fit3 <- coxph(Surv(tdeath, status)~agedg*st2+agedg*st3+agedg*st4,data=hav)
fit3

#I�ll� ja taudin vaiheella n�ytt�� olevan tilastollisesti merktisev�� yhdysvaikutusta. Ainoat merkitsev�t termit liit-
#tyv�t taudin vaiheeseen 2 ja sen yhdysvaikutukseen i�n kanssa. T�ytyy kuitenkin muistaa, ett� mallissa on mon-
#ta parametria, joten muutamia tilastollisesti merkitsevi� tekij�it� on odotettavissa jo pelk�st��n sattumalta. N�m�
#tulokset vaikutavalta oudoilta erityisesti verrattuna mallin fit1 tuloksiin. Voi olla, ett� meill� ei ole tarpeeksi havain-
#toja k�ytett�viss�mme monimutkaisemman mallin estimoimiseen. Mallit fit3 ja fit1 ovat sis�kk�isi�, joten voimme
#j�lleen k�ytt�� uskottavuusosam��r�testi� niiden vertailemiseen:

anova(fit3, fit1)

#Testin tulos ei ole merkitsev� tasolla 0.05, joten meill� ei ole n�ytt�� rikkaamman mallin valitsemiseen.

#teht 3

#Sovita aineistoon kiihtyv�n ajan Weibull-malli (fit4) k�ytt�en survreg-funktiota. 
#Huomaa mallintulostuksessa Log(scale). Verrataksesi nyt Coxin mallin estimoituja regressiokertoimia tulee sinunjakaa mallin kertoimet skaalaparametrilla fit4$scale. 
#Miten kiihtyv�n ajan mallin n�yttisi sopivan
#aineistoon Coxin malliin verrattuna ja miten mallien kertoimet eroavat toisistaan? Laske viel� perushasardia vastaava Weibull-mallin vakiotermi muodosta lambda
#fit4$coef[1]/fit4$scale. Tarkastele
#viel� Weibull-oletuksen sopivuutta piirt�m�ll�. Weibull-mallin kumulatiivinen hasardi l�ytyy luentomateriaalista.

fit4<-survreg(Surv(tdeath, status)~st2+st3+st4+agedg,dist="weibull",data=hav)
-fit4$coefficients/fit4$scale

# verrataan coxin mallin parametriestimaatteihin
fit1$coefficients

# lambda
exp(-fit4$coef[1]/fit4$scale)

# Tarkastellaan mallin sopivuutta. Yleens� kuvaan piirret��n ep�parametrinen
# estimatti kumulatiiviselle hasardille eri covariaattiryhmiss�.
# pilkotaan ik�muuttuja kuvaa varten
age65 <- ifelse(hav$agedg>65,1,0)
# KM estimaatti
KM <- survfit(Surv(tdeath, status)~st2+st3+st4+age65,data=hav)
# kuva
plot(log(KM$time),log(-log(KM$surv)),col=hav$stage,pch=age65+1)

# kuva ilman ik�muuttujaa
KM <- survfit(Surv(tdeath, status)~st2+st3+st4,data=hav)
plot(log(KM$time),log(-log(KM$surv)),col=hav$stage)

# piirret��n mallin perusteella kumuloitu hazardi ep�parametrista kumulatiivista
# hasardia vastaan
alpha <- 1/fit4$scale
lambda <- exp(-fit4$linear.predictors)
times<-t(replicate(90,seq(0,11,by=0.5)))
# ajankohtia jokaiselle potilaalle
H <- (lambda*times)^alpha
# jokaisen potilaan kumulatiivinen hasardi hetkell� t
H <- colSums(H)
# odotettu tapahtumien lkm yhteens� hetkeen t menness�
plot(log(seq(0,11,by=0.5)),log(H))
KM.pooled <- survfit(Surv(tdeath, status)~1,data=hav)
NeAa <- cumsum(KM.pooled$n.event/KM.pooled$n.risk)
points(log(KM.pooled$time),log(90*NeAa),col="red")

# mallin sopivuutta voi tutkailla my�s "simuloimalla aineistoja" ehdolla mallin
# parametrit ja katsoa niiden k�ytt�ytymist� suhteessa havaittuun v�ltt��n
plot(KM.pooled)
for(i in 1:1000){
    y <- sort(rweibull(90,shape= alpha,scale=1/lambda))
    # generoidaan havainnot
    points(y, (length(y):1)/length(y),type="l",col=rgb(1,0,0,0.05))
}

#teht 4

#Estimoi Coxin malli (fit5), jossa tarkastelet sek� ik��(agedg) ett� taudin vaihetta (stage) lineaari-sesti. 
#K�yt� siis indikaattorimuuttujien sijasta alkuper�ist� muuttujaa stage. Piirr� Schoenfeldin j��nn�kset. 
#N�ytt��k� vaihemuuttujan merkitys olevan ajan suhteen vakio? Tutki sitten standar-doidun score-prosessin arvoja ajansuhteen eli jaa jokaisen havainnon kontribuutio score-funktioon
#vastaavien informaatiomatriisin kontribuutioiden neli�juurilla. N�m� l�yd�t coxph.detail-funktion
#palauttaman olion alkioista score ja imat. Tuloksena tulisi olla kohinalta n�ytt�v� k�yr�, joka pysyy
#90 prosenttia ajata rajojen +/- 1.3581 sis�ll�
    
# sovitetaan coxin malli lineaarisella stage-muuttujalla
fit5 <- coxph(Surv(tdeath, status)~agedg+stage,data=hav)
# Schoenfeldin j��nn�kset (raakaj��nn�kset)
res <- residuals(fit5,type="schoenfeld")
par(mfrow=c(1,2))
plot(res[,"agedg"])
plot(res[,"stage"])

# standardoidut Schoenfeldin j��nn�kset
cz <- cox.zph(fit5)
plot(cz[1])
plot(cz[2])

#Schoenfeldin j��nn�ksiss� n�kyy yhteytt� ajan ja j��nn�sten v�lill�. T�m� viittaisi siihen, ett� muuttujien vaikutus
#ei ole homogeeninen ajan yli. Katsotaan seuraavaksi standardoitua score-prosessia.

# tarvitaan tarkempaa tietoa mallista
cd <- coxph.detail(fit5)
U <- cd$score
# kontribuutiot score-funktioon
I <- t(apply(cd$imat,3, diag))
# kontribuutiot informaatiomatriisiin
U.stand <- U/sqrt(I)
# standardoitu prosessi
par(mfrow=c(1,2))
plot(U.stand[,1],type="l")
abline(h=c(-1.3581,1.3581))
plot(U.stand[,2],type="l")
abline(h=c(-1.3581,1.3581))

#teht 5

#Tutki mallin (fit5) antamia ennusteitapredict-funktiolla taudin eri vaiheissa. 
#Kokeile eri tyyppisi� ennusteita (risk, expected, linear predictor). Miten hyvin malli n�ytt�isi ennustavan kuolinriski�?
#Tutki sen j�lkeen mallin ennustuskyky� Arjas-plotilla eli piirr� kumulatiiviset toteutuneet tapahtumat vs. ennustetut kumulatiiviset tapahtumat. 
#Lis�� kuvan diagonaalille suora (y=x), jonka suhteen poikkeamia tarkastellaan. 
#Ennustaako malli liikaa vai liian v�h�n tapahtumia ja vaihteleeko t�m� ajan mukaan? 
#Huomaa, ett� diagonaali ei suoraan vastaa seuranta-aikaa, koska eroja tarkastellaan vain tapahtumahetkin�.

# otetaan ennusteet talteen:
hav$p.risk <- predict(fit5,type="risk")
hav$p.exp <- predict(fit5,type="expected")
hav$p.lp <- predict (fit5,type="lp")
par(mfrow=c(1,3))
plot(hav$p.risk,col=hav$stage,main="hazardien suhde")# exp(beta'z)
plot(hav$p.exp,col=hav$stage,main="mallin ennustama tapahtumien m��r�")
plot(hav$p.lp,col=hav$stage,main="lineaarinen prediktori")# beta'z

# samat kuvat v�h�n eri j�rjestyksess�
plot(hav$agedg, hav$p.risk,col=hav$stage)
# exp(beta'z), hazardien suhde, on nyt porrasmainen
plot(hav$tdeath, hav$p.exp, col=hav$stage)
# mallin ennustama tapahtumien m��r�
plot(hav$agedg, hav$p.lp,col=hav$stage)
# linear predictor, beta'z

#T�m�n kohdan kysymykseen mallin ennusteiden tarkkuudesta saamme ehk� helpommin vastauksen katsomalla
#Arjas-kuvaajaa.

par(mfrow=c(1,1))
p <- predict(fit5,type="expected")[order(hav$tdeath)]

y <- hav$status[order(hav$tdeath)]
plot(cumsum(p),cumsum(y),asp=1)
curve(x^1,add=T)

#Havainnot kulkevat j�rjest��n kuvaan piirretyn suoran yl�puolella, mik� tarkoittaa ett� malli aliestimoi tapahtu-
#mien lukum��r��. Tapahtumia havaitsemme aineistossa siis enemm�n kuin mit� malli ennustaisi.