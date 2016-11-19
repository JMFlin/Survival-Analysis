#lue aineisto R:ään ja nimeä muuttujat nimillästage(taudin vaihe),tdeath(kuolinaika),agedg(ikä diagnoosihetkellä),yrdg(vuosi diagnoosihetkellä) jastatus
#(potilaan tila). Muodosta vielä taudin tilaa kuvaavat indikaattorimuuttujat,st2,st3jast4, pitäen tasoa 1 referenssinä. Lataasurvival-paketti ja sovita aneistoon Coxin malli (
#fit1), jossa tarkastelet taudin vaiheen (st2, st3, st4) jaiän (    agedg) vaikutusta kuolinriskiin. Käytä mallissa vain muuttujien päävaikutuksia. 
#Näyttääkö ikä sekoittavan taudin vaiheen merkitystä kuolinriskille? Miten taudin vaihe näyttä ennustavan
#kuolinriskiä? Laske 4. vaiheen 50-vuotiaan potilaan kuolinriskin suhde samassa vaiheessa olevaan
#40-vuotiaaseen potilaaseen

#Käytetään aineistoa larynx.txt, joka sisältää mittauksia 90:stä kurkunpäänsyöpää sairastaneista miehistä.

#teht 1

# Luetaan aineisto sisään ja nimetään muuttujat
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

#Iällä ei näytä olevan suurta vaikutusta elinaikaan. Suhteellisen riskin luottamusväli on [0.991, 1.048] mikä tarkot-
#taisi kymmenissä vuosissa noin 90%-120% kokoista riskin muutosta. Iällä voitaneen olettaa olevan riskiä lisäävä
#vaikutus, mutta aineiston koko ei riitä sitä tämän tarkemmin estimoimaan. Lasketaan suhteellinen kuolinriski 50v
#vs. 40v 4. vaiheen potilas:
#e^(1.706+0.019*50)/    e^(1.706+0.019*40) = e^0.019*(50-40) = 1.209

#teht 2

#Estimoi nyt pelkkää iän vaikutusta testavaa malli (fit2). 
#Laske osamäärätestifit1:n jafit2:n välilleja tulosta anova-taulu eri tekijöiden merkityksestä fit1-mallissa. 
#Mitä päättelet taulusta? (Vinkki:anova-funktio). 
#Tarkastele sen jälkeen iän ja taudin vaiheen indikaattoreiden yhdysvaikutusta mallissa fit3. 
#Näyttääkö siltä, että iällä on jossakin taudin vaiheessa enemmän merkitystä?

# malli jos pelkästään ikä kovariaattina
fit2 <- coxph(Surv(tdeath, status)~agedg,data=hav)
# uskottavuusosamäärätesti
obs <- -2*(fit2$loglik[2]-fit1$loglik[2])
1-pchisq(obs,df=3)# yksinkertaistaa liikaa
# anova-taulu
anova(fit1)
# sama testi uudestaan helpommin
anova(fit1, fit2)

#Uskottavuusosamäärätestiin liittyvä p-arvo on hyvin pieni. Tästä voimme päätellä, että malli josta on pudotettu
#taudin vaihe -muuttuja pois on liian yksinkertainen suhteessa rikkaampaan malliin. Siis taudin vaihe on merkityk-
#sellinen muuttuja, jonka avulla voimme paremmin selittää potilaiden elinajan vaihtelua. Sanotaan, että muuttuja
#on tilastollisesti merkitsevä selittäjä vasteelle.
#Anova-taulusta saamme yksityiskohtaisempaa tietoa muuttujan merkityksestä. Vaikuttaa siltä, että erityisesti tau-
#din vaihe 4 eroaa ensimmäisestä vaiheesta eliniän suhteen. Muihin vaiheisiin 2 ja 3 liittyvät p-arvot ovat suurehkot,
#joten näyttöä vaiheiden 1, 2 ja 3 eroista ei löydy.

# sovitetaan malli yhdysvaikutusten kanssa
fit3 <- coxph(Surv(tdeath, status)~agedg*st2+agedg*st3+agedg*st4,data=hav)
fit3

#Iällä ja taudin vaiheella näyttää olevan tilastollisesti merktisevää yhdysvaikutusta. Ainoat merkitsevät termit liit-
#tyvät taudin vaiheeseen 2 ja sen yhdysvaikutukseen iän kanssa. Täytyy kuitenkin muistaa, että mallissa on mon-
#ta parametria, joten muutamia tilastollisesti merkitseviä tekijöitä on odotettavissa jo pelkästään sattumalta. Nämä
#tulokset vaikutavalta oudoilta erityisesti verrattuna mallin fit1 tuloksiin. Voi olla, että meillä ei ole tarpeeksi havain-
#toja käytettävissämme monimutkaisemman mallin estimoimiseen. Mallit fit3 ja fit1 ovat sisäkkäisiä, joten voimme
#jälleen käyttää uskottavuusosamäärätestiä niiden vertailemiseen:

anova(fit3, fit1)

#Testin tulos ei ole merkitsevä tasolla 0.05, joten meillä ei ole näyttöä rikkaamman mallin valitsemiseen.

#teht 3

#Sovita aineistoon kiihtyvän ajan Weibull-malli (fit4) käyttäen survreg-funktiota. 
#Huomaa mallintulostuksessa Log(scale). Verrataksesi nyt Coxin mallin estimoituja regressiokertoimia tulee sinunjakaa mallin kertoimet skaalaparametrilla fit4$scale. 
#Miten kiihtyvän ajan mallin näyttisi sopivan
#aineistoon Coxin malliin verrattuna ja miten mallien kertoimet eroavat toisistaan? Laske vielä perushasardia vastaava Weibull-mallin vakiotermi muodosta lambda
#fit4$coef[1]/fit4$scale. Tarkastele
#vielä Weibull-oletuksen sopivuutta piirtämällä. Weibull-mallin kumulatiivinen hasardi löytyy luentomateriaalista.

fit4<-survreg(Surv(tdeath, status)~st2+st3+st4+agedg,dist="weibull",data=hav)
-fit4$coefficients/fit4$scale

# verrataan coxin mallin parametriestimaatteihin
fit1$coefficients

# lambda
exp(-fit4$coef[1]/fit4$scale)

# Tarkastellaan mallin sopivuutta. Yleensä kuvaan piirretään epäparametrinen
# estimatti kumulatiiviselle hasardille eri covariaattiryhmissä.
# pilkotaan ikämuuttuja kuvaa varten
age65 <- ifelse(hav$agedg>65,1,0)
# KM estimaatti
KM <- survfit(Surv(tdeath, status)~st2+st3+st4+age65,data=hav)
# kuva
plot(log(KM$time),log(-log(KM$surv)),col=hav$stage,pch=age65+1)

# kuva ilman ikämuuttujaa
KM <- survfit(Surv(tdeath, status)~st2+st3+st4,data=hav)
plot(log(KM$time),log(-log(KM$surv)),col=hav$stage)

# piirretään mallin perusteella kumuloitu hazardi epäparametrista kumulatiivista
# hasardia vastaan
alpha <- 1/fit4$scale
lambda <- exp(-fit4$linear.predictors)
times<-t(replicate(90,seq(0,11,by=0.5)))
# ajankohtia jokaiselle potilaalle
H <- (lambda*times)^alpha
# jokaisen potilaan kumulatiivinen hasardi hetkellä t
H <- colSums(H)
# odotettu tapahtumien lkm yhteensä hetkeen t mennessä
plot(log(seq(0,11,by=0.5)),log(H))
KM.pooled <- survfit(Surv(tdeath, status)~1,data=hav)
NeAa <- cumsum(KM.pooled$n.event/KM.pooled$n.risk)
points(log(KM.pooled$time),log(90*NeAa),col="red")

# mallin sopivuutta voi tutkailla myös "simuloimalla aineistoja" ehdolla mallin
# parametrit ja katsoa niiden käyttäytymistä suhteessa havaittuun välttöön
plot(KM.pooled)
for(i in 1:1000){
    y <- sort(rweibull(90,shape= alpha,scale=1/lambda))
    # generoidaan havainnot
    points(y, (length(y):1)/length(y),type="l",col=rgb(1,0,0,0.05))
}

#teht 4

#Estimoi Coxin malli (fit5), jossa tarkastelet sekä ikää(agedg) että taudin vaihetta (stage) lineaari-sesti. 
#Käytä siis indikaattorimuuttujien sijasta alkuperäistä muuttujaa stage. Piirrä Schoenfeldin jäännökset. 
#Näyttääkö vaihemuuttujan merkitys olevan ajan suhteen vakio? Tutki sitten standar-doidun score-prosessin arvoja ajansuhteen eli jaa jokaisen havainnon kontribuutio score-funktioon
#vastaavien informaatiomatriisin kontribuutioiden neliöjuurilla. Nämä löydät coxph.detail-funktion
#palauttaman olion alkioista score ja imat. Tuloksena tulisi olla kohinalta näyttävä käyrä, joka pysyy
#90 prosenttia ajata rajojen +/- 1.3581 sisällä
    
# sovitetaan coxin malli lineaarisella stage-muuttujalla
fit5 <- coxph(Surv(tdeath, status)~agedg+stage,data=hav)
# Schoenfeldin jäännökset (raakajäännökset)
res <- residuals(fit5,type="schoenfeld")
par(mfrow=c(1,2))
plot(res[,"agedg"])
plot(res[,"stage"])

# standardoidut Schoenfeldin jäännökset
cz <- cox.zph(fit5)
plot(cz[1])
plot(cz[2])

#Schoenfeldin jäännöksissä näkyy yhteyttä ajan ja jäännösten välillä. Tämä viittaisi siihen, että muuttujien vaikutus
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
#Kokeile eri tyyppisiä ennusteita (risk, expected, linear predictor). Miten hyvin malli näyttäisi ennustavan kuolinriskiä?
#Tutki sen jälkeen mallin ennustuskykyä Arjas-plotilla eli piirrä kumulatiiviset toteutuneet tapahtumat vs. ennustetut kumulatiiviset tapahtumat. 
#Lisää kuvan diagonaalille suora (y=x), jonka suhteen poikkeamia tarkastellaan. 
#Ennustaako malli liikaa vai liian vähän tapahtumia ja vaihteleeko tämä ajan mukaan? 
#Huomaa, että diagonaali ei suoraan vastaa seuranta-aikaa, koska eroja tarkastellaan vain tapahtumahetkinä.

# otetaan ennusteet talteen:
hav$p.risk <- predict(fit5,type="risk")
hav$p.exp <- predict(fit5,type="expected")
hav$p.lp <- predict (fit5,type="lp")
par(mfrow=c(1,3))
plot(hav$p.risk,col=hav$stage,main="hazardien suhde")# exp(beta'z)
plot(hav$p.exp,col=hav$stage,main="mallin ennustama tapahtumien määrä")
plot(hav$p.lp,col=hav$stage,main="lineaarinen prediktori")# beta'z

# samat kuvat vähän eri järjestyksessä
plot(hav$agedg, hav$p.risk,col=hav$stage)
# exp(beta'z), hazardien suhde, on nyt porrasmainen
plot(hav$tdeath, hav$p.exp, col=hav$stage)
# mallin ennustama tapahtumien määrä
plot(hav$agedg, hav$p.lp,col=hav$stage)
# linear predictor, beta'z

#Tämän kohdan kysymykseen mallin ennusteiden tarkkuudesta saamme ehkä helpommin vastauksen katsomalla
#Arjas-kuvaajaa.

par(mfrow=c(1,1))
p <- predict(fit5,type="expected")[order(hav$tdeath)]

y <- hav$status[order(hav$tdeath)]
plot(cumsum(p),cumsum(y),asp=1)
curve(x^1,add=T)

#Havainnot kulkevat järjestään kuvaan piirretyn suoran yläpuolella, mikä tarkoittaa että malli aliestimoi tapahtu-
#mien lukumäärää. Tapahtumia havaitsemme aineistossa siis enemmän kuin mitä malli ennustaisi.