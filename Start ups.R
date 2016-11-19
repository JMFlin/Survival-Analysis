#teht 1

#Tarkastellaan samana vuonna syntyneiden lasten joukkoa (syntym‰kohorttia), jota seurataan kun-
#nes sen j‰senet t‰ytt‰v‰t 20 vuotta. Halutaan ennustaa huostaanoton ja vanhempien avioeron vai-
#kutusta lapsen tai nuoren riskille joutua psykiatriseen sairaalahoitoon ennen 20. ik‰vuotta. Huom.
#sairaalajaksoja voi olla useampia. Aineistossa on kohortin j‰senist‰ seuraavia mittauksia, jotka ovat
#mallissa aikariippuvina kovariaatteina:

#sairaalap‰ivien lukum‰‰r‰ ik‰‰n t menness‰
#lastensuojeluviranomaisten sijoitus sijaiskotiin i‰ss‰ t
#paluu biologisten vanhempien kotiin i‰ss‰ t
#vanhempien avioero i‰ss‰ t

#Kirjoita seuraavan kolme tapahtumahistorian aineisto R:n Surv-oliomuodossa:
#1. lapsella sijoitus 5-vuotiaana ja vanhempien avioero 6-vuotiaana, 1. sairaalajakso 17-vuotiaana 21 p‰iv‰‰
#2. lapsella vanhempien avioero 12-vuotiaana, sijoitus 14-vuotiaana ja paluu biologisten vanhempien
#luo 15-vuotiaana, ei sairaalahoitoja
#3. lapsella vanhempien avioero 4-vuotiaana, 1. sairaalajakso 19-vuotiaana 14 p‰iv‰‰, ei sijoituksia.
#Mieti montako tietuetta tarvitset kullekin henkilˆlle, mik‰ on 'start' ja mik‰ 'stop' kun aikav‰li on
#yksi vuosi ja miten kovariaattien arvot t‰ytyy p‰ivitt‰‰. Merkitse status-muuttujaan tapahtumat
#1= sairaalahoito, 2= sijoitus, 3= avioero, 4= paluu biol. kotiin. Muista, ett‰ seuranta jatkuu 20. ik‰vuoteen asti.

# Tapahtuma-ajat rivitettyn‰ lapsittain
Start <- c(0,5,6,17,0,12,14,15,0,4,19)
Stop <- c(5,6,17,20,12,14,15,20,4,19,20)
# Id-muuttuja
Lapsi <- c(1,1,1,1,2,2,2,2,3,3,3)
# Sairaalap‰ivien lkm aikav‰lill‰
sairaalapv <- c(0,0,0,21,0,0,0,0,0,0,14)
# Tapahtumaindikaattori kuten teht‰v‰nannossa
status <- c(2,3,1,0,3,2,4,0,3,1,0)
# Historiamuuttujat
sairaalahoito <- c(0,0,0,1,0,0,0,0,0,0,1)
sijoitus <- c(0,1,1,1,0,0,1,1,0,0,0)
avioero <- c(0,0,1,1,0,1,1,1,0,1,1)
paluu <- c(0,0,0,0,0,0,0,1,0,0,0)
# kootaan kaikki havaintomatriisiksi
hav <- data.frame(Lapsi, Start, Stop, sairaalapv, status, sairaalahoito,sijoitus, avioero, paluu)
hav
#luetaan Surv-objektiksi
library(survival)
Surv(time=hav$Start,time2=hav$Stop,event=hav$status,type="mstate")

#teht 2

#Seurataan 45 start-up yrityst‰ kahdelta eri yrityssektorilta (n1= 25;n2= 20)
#kunnes ne saavuttavat kannattavuustason. Osa yrityksist‰ sensuroituu kyselyst‰ (+) syyst‰, joka ei liity aikaan.
#Aineisto on tiedostossa profitability.txt.
#a) Miten mittaat sektorien v‰list‰ eroa kannattavuustason saavuttamisessa? K‰yt‰ exponentiaalista mallia
#b)Kokeile nyt samaan aineistoon Weibull-mallia, josta kohdan a) malli on erikoistapaus. Mit‰
#p‰‰ttelet mallien sopivuudesta?

#profitability.txt:
#sector 1: 5 1 5+ 1 1 2 1+ 2 11+ 2 4 2 1 3 1 2+ 1 2 3 2 13+ 1 2 2 1
#sector 2: 12+ 2 11 15 12 10+ 17 4 8 5 30 4+ 3 8 4 14 6+ 2 1 6

#Koodataan ensin aineisto analysoitavaksi
# Tapahtuma-ajat
y <- c(5,1,5,1,1,2,1,2,11,2,4,2,1,3,1,2,1,2,3,2,13,1,2,2,1,12,2,11,15,12,10,17,4,8,5,30,4,3,8,4,14,6,2,1,6)
# Tapahtumaindikaattori (1=tapaus, 0=sensuroitu)
status <- c(1,1,0,1,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,0,1,1,1)
# Surv-objekti
library(survival)
surv <- Surv(time=y,event=status)
# Kovariaatti eli yrityssektori
x <- factor(rep(c(-1,1),c(25,20)))

#a)-kohta
fit <- survreg(surv~x,dist="exponential")
#Sektoreiden v‰linen ero tiivistyy t‰ss‰ mallissa regressiokertoimeen:
beta <- fit$coefficients[2]
beta
# betan keskivirhe
se <- sqrt(fit$var["x1",2])
se
# normaalijakauma-approksimaatioon perustuva 95% luottamusv‰li:
conf <- c(beta-1.96*se, beta+1.96*se)
# Hazard ratio
exp(beta)
exp(conf)
# vaihtoehtoinen helpompi tapa laskea luottamusv‰li:
exp(confint(fit))

#Riskisuhteen piste-estimaatti on 3,06 ja sen 95% luottamusv‰li [1.58,5.91]. Tulkinta on, ett‰ sektorilla 1 toimivilla
#yrityksill‰ on noin 3 kertaa suurempi "riski"saavuttaa kannattavuustaso kuin sektorin 2 yrityksill‰. Eroa voidaan
#pit‰‰ tilastollisesti merkitsev‰n‰, sill‰ 95% luottamusv‰li ei sis‰ll‰ lukua 1. Piirret‰‰n estimoidut v‰lttˆfuntiot sa-
#maan kuvaan ep‰parametrisen Kaplan-Meier -estimaatin kanssa:

# Kaplan-Meier
plot(survfit(surv~x))
# Sovitettu lambda kummallekin ryhm‰lle
lambda1 <- 1/exp(fit$coefficients[1])
lambda2 <- 1/exp(sum(fit$coefficients))
# piirret‰‰n estimoidut v‰lttˆfunktiot kuvaan
temp <- seq(0,30,length.out=100)
lines(temp,exp(-lambda1*temp))
lines(temp,exp(-lambda2*temp),col="red")
legend("topright",legend=c("Sektori 1","Sektori 2"),lwd=2,col=c(1,2))
#Mallin sopivuus ei n‰yt‰ kuvan perusteella kovin huonolta.

#b)-kohta
fit.weib <- survreg(surv~x,dist="weibull")
# mallin tietoja
summary(fit.weib)
# luottamusv‰li
exp(confint(fit.weib))

#Sektoreiden v‰linen ero saadaa j‰lleen parametrinbeta
#kautta kuten a)-kohdassa. Saamme hyvin samankaltaisen
#piste-estimaatinriskisuhteelle 3.01,muttahiemankapeammanluottamusv‰lin[1.70,5.23].Weibull-jakaumanskaa-
#laparametrin arvoksi estimoituu 0.858 mik‰ ei ole kovin kaukana exponentiaalijakaumasta (1).
#Piirret‰‰n j‰lleen estimoidut v‰lttˆfunktiot.

# Kaplan-Meier
plot(survfit(surv~x))
# Sovitettu lambda kummallekin ryhm‰lle
lambda1 <- 1/exp(fit$coefficients[1])
lambda2 <- 1/exp(sum(fit$coefficients))
scale <- fit$scale
# piirret‰‰n estimoidut v‰lttˆfunktiot kuvaan
temp <- seq(0,30,length.out=100)
lines(temp,exp(-(lambda1*temp)^scale))
lines(temp,exp(-(lambda2*temp)^scale),col="red")
legend("topright",legend=c("Sektori 1","Sektori 2"),lwd= 2,col=c(1,2))

#Mallin sopivuus vaikuttaa edelleen hyv‰lt‰. Suurta muutosta Weibull-jakaumaan liittyv‰n skaalaparametrin lis‰‰-
#minen ei kuitenkaan n‰yt‰ tuoneen. Malleja voimme vertailla kesken‰‰n k‰ytt‰m‰ll‰ esimerkiksi uskottavuuteen
#perustuvaa Akaiken informaatiokriteeri‰ (AIC). Pienemm‰t AICn arvot tarkoittavat mallin parempaa sopivuutta.
#AIC ensimm‰iselle mallille on 203.0 ja toiselle 203.6. Suurta eroa mallien v‰lill‰ ei siis n‰yt‰ olevan.

# kuvat saa helpommin flexsurv-paketilla:
library(flexsurv)
fit.exp <- flexsurvreg(surv~x,dist="exponential")
fit.weib <- flexsurvreg(surv~x,dist="weibull")
plot(fit.exp,col=c("red","orange"))
lines(fit.weib,col=c("blue","cyan"))
