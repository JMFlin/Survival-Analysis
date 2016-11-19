#Tarkastellaan seuranta-aineistoa kidney munuaispotilaista, jotka käyttävät mukana kuljetettavaa dialyysilaitetta. 
#Aineisto löytyy valmiiksi R:n survival-paketista. Havaintoajat ovat infektion takia tehtyjä katedrin vaihtoaikoja. 
#Kultakin potilaalta on havaittu kaksi vaihtoaikaa. Ne, joilla katedri on vaihdettu muusta syystä, ovat sensuroituneita havaintoja. 
#Aineiston muuttujia ovat id, aika vaihtoon/sensuroitumiseen, status(1=vaihto, 0=muu), ikä, sukupuoli
#(1=mies, 2=nainen) taudin tyyppi (glomerulonefriitti, akuutti nefriitti, polykystinen munuaistauti, muu). Käytä R:n coxph-ohjelmaaa.

# ladataan paketti ja aineisto
library(survival)
data(kidney)
# tehdään sukupuolesta factor
kidney$sex <-factor(kidney$sex, labels =c("M", "F"))
# katsotaan ensimmäiset havainnot
head(kidney)

# katsotaan vähän miltä aineisto näyttää
library(ggplot2)
ggplot(data=kidney,aes(x=sex, y=age, col=sex, label=id)) +
facet_grid(. ~ disease) +geom_text(position="jitter") +theme_light() +ggtitle("Ikäjakaumat eri ryhmissä, id numerot piirrettynä")

#Aineistossa nimetyt taudit näyttäävät vaivaavan lähinnä yli kolmekymmentä-vuotiaita kun taas muut-nimikkeen
#alla on myös nuorempia potilaita. GN-tautia sairastavissa on yksi 10-vuotias poikkeustapaus.

#a) Tutki ensin iän ja sukupuolen vaikutusta infektioriskiin

fit.a <- coxph(Surv(time, event=status) ~ sex + age, data=kidney)
summary(fit.a)

#Tämän mallin perusteella vaikuttaisi siltä, että sukupuolilla on selvä ero tapauksen riskissä. 95 prosentin luotta-
#muksella voimme sanoa, että naisten riski on noin 0.250-0.784 kertainen miehiin nähden. Iän vaikutus näyttää
#olevan suhteellisen vähäinen eikä sen suunnasta ole varmuutta. Riskisuhteen luottamusväli 10-vuoden ikäerolle on 0.817-1.218.

#b) Lisää malliin taudin tyyppi faktorina käyttäen referenssiluokkana tilaa 'muu'. Vertaa edel-
#liseen mallin osamäärätestiä käyttäen.

fit.b <- coxph(Surv(time, event=status) ~ sex + age + disease, data=kidney)
summary(fit.b)

#Taudin tyypin lisääminen malliin näyttää selventävän entisestään sukupuolen vaikutusta, luottamusväli nyt 0.112-0.458. 
#Tämä voi johtua siitä, että riskiä lisäävä tauti "PKD" näyttää olevan suhteellisen harvinaisempi naisilla
#kuin miehillä. PKD on ainoa tautityyppi, jonka riskiä lisäävästä vaikutuksesta meillä on näyttöä, luottamusväli: 0.069-0.824. 
#Voimme vertailla malleja myös uskottavuusosamäärätestillä ja todeta, että a)-kohdan malli on merkitsevästi liian yksinkertainen:
anova(fit.a, fit.b)


#c) Huomioi nyt, että kustakin potilaasta on kaksi havaintoa lisäämällä malliin frailty-muuttuja.
#Mikä on R:n oletusjakauma frailtylle? Miten arvioit tarvitaanko frailty-mallia toistuvien infek tioiden korreloituneisuuden
#ja ei-mitattujen vaikutusten kattamiseksi? 
#Entä jos jätät mallista pois tautityypin (disease)? Miten frailty nyt käyttäytyy?
# frailty-termi lisättynä
fit.c1 <- coxph(Surv(time, event=status) ~ sex + age + disease +frailty(id), data=kidney)
summary(fit.c1)

#R:n oletusjakauma frailtylle on Gamma keskiarvolla 1. Sen tarpeellisuus on mallin mukaan kuitenkin vähäinen,
#sillä sen varianssin estimaatti on käytännössä 0. Näin käy helposti, jos potilaiden sisäinen korrelaatio on suhteelli-
#sen pientä verrattuna selittävien tekijöiden vaikutukseen. Katsotaan mitä tapahtuu, jos pudotetaan taudin tyyppi
#pois mallista.

fit.c2 <- coxph(Surv(time, event=status) ~ sex + age +frailty(id), data=kidney)
summary(fit.c2)

#Nyt kun mallissa on vähemmän selittäjiä, saadaan frailtytermi estimoitua. Henkilöiden välisen riskin varianssin
#piste-estimaatti on 0.41. Tämä vastaa oheista jakaumaa:

curve(dgamma(x, shape=1/0.412, scale=0.412), from=0, to=5, ylab="f(u)", xlab="u", lwd=3)

#d) Plottaa nyt 1. mallin martingaaliresiduaalit tautityypeittäin. Onko löydettävissä yksilöllisiä eroja, 
#jotka selittäisivät kahden viimeisen mallin eron frailtyn suhteen? 
#Poista aineistosta tarvittaessa äärimmäiset havainnot ja estimoi mallit uudelleen.

# tallennetaan jäännökset aineistoon ja toistetaan aikasempi kuva
kidney$res.b <- residuals(fit.b, type="martingale")
ggplot(data=kidney,aes(x=sex, y=res.b, col=sex, label=id)) +facet_grid(. ~ disease) +
geom_text(position="jitter") +theme_light() +ggtitle("Jäännökset eri ryhmissä")

#Aineistossa on kaksi poikkeavaa havaintoa, jotka voivat selittää eron mallien c1 ja c2 välillä. 
#Poistetaan nämä ja sovitetaan mallit uudestaan.

# poistetaan kaksi poikkeavaa havaintoa ja sovitetaan uudestaan
kidney2 <- subset(kidney, subset=!(id %in% c(10, 21)))
fit.d1 <-coxph(Surv(time, event=status) ~ sex + age + disease +frailty(id),data=kidney2)
fit.d2 <-coxph(Surv(time, event=status) ~ sex + age +frailty(id), data=kidney2)
summary(fit.d1)
summary(fit.d2)

#e) Määrittele nyt frailtyn jakaumaksi normaalijakauma. Survival moduli käyttää tässä tapauksessa 
#ns. rajoitettua ML-estimointia (REstricted ML, REML). Miten malli eroaa aikaisemmasta?

fit.e <-coxph(Surv(time, event=status) ~ sex + age + disease +frailty(id, distribution="gaussian"), data=kidney2)
summary(fit.e)

