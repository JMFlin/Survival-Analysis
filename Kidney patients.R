#Tarkastellaan seuranta-aineistoa kidney munuaispotilaista, jotka k�ytt�v�t mukana kuljetettavaa dialyysilaitetta. 
#Aineisto l�ytyy valmiiksi R:n survival-paketista. Havaintoajat ovat infektion takia tehtyj� katedrin vaihtoaikoja. 
#Kultakin potilaalta on havaittu kaksi vaihtoaikaa. Ne, joilla katedri on vaihdettu muusta syyst�, ovat sensuroituneita havaintoja. 
#Aineiston muuttujia ovat id, aika vaihtoon/sensuroitumiseen, status(1=vaihto, 0=muu), ik�, sukupuoli
#(1=mies, 2=nainen) taudin tyyppi (glomerulonefriitti, akuutti nefriitti, polykystinen munuaistauti, muu). K�yt� R:n coxph-ohjelmaaa.

# ladataan paketti ja aineisto
library(survival)
data(kidney)
# tehd��n sukupuolesta factor
kidney$sex <-factor(kidney$sex, labels =c("M", "F"))
# katsotaan ensimm�iset havainnot
head(kidney)

# katsotaan v�h�n milt� aineisto n�ytt��
library(ggplot2)
ggplot(data=kidney,aes(x=sex, y=age, col=sex, label=id)) +
facet_grid(. ~ disease) +geom_text(position="jitter") +theme_light() +ggtitle("Ik�jakaumat eri ryhmiss�, id numerot piirrettyn�")

#Aineistossa nimetyt taudit n�ytt��v�t vaivaavan l�hinn� yli kolmekymment�-vuotiaita kun taas muut-nimikkeen
#alla on my�s nuorempia potilaita. GN-tautia sairastavissa on yksi 10-vuotias poikkeustapaus.

#a) Tutki ensin i�n ja sukupuolen vaikutusta infektioriskiin

fit.a <- coxph(Surv(time, event=status) ~ sex + age, data=kidney)
summary(fit.a)

#T�m�n mallin perusteella vaikuttaisi silt�, ett� sukupuolilla on selv� ero tapauksen riskiss�. 95 prosentin luotta-
#muksella voimme sanoa, ett� naisten riski on noin 0.250-0.784 kertainen miehiin n�hden. I�n vaikutus n�ytt��
#olevan suhteellisen v�h�inen eik� sen suunnasta ole varmuutta. Riskisuhteen luottamusv�li 10-vuoden ik�erolle on 0.817-1.218.

#b) Lis�� malliin taudin tyyppi faktorina k�ytt�en referenssiluokkana tilaa 'muu'. Vertaa edel-
#liseen mallin osam��r�testi� k�ytt�en.

fit.b <- coxph(Surv(time, event=status) ~ sex + age + disease, data=kidney)
summary(fit.b)

#Taudin tyypin lis��minen malliin n�ytt�� selvent�v�n entisest��n sukupuolen vaikutusta, luottamusv�li nyt 0.112-0.458. 
#T�m� voi johtua siit�, ett� riski� lis��v� tauti "PKD" n�ytt�� olevan suhteellisen harvinaisempi naisilla
#kuin miehill�. PKD on ainoa tautityyppi, jonka riski� lis��v�st� vaikutuksesta meill� on n�ytt��, luottamusv�li: 0.069-0.824. 
#Voimme vertailla malleja my�s uskottavuusosam��r�testill� ja todeta, ett� a)-kohdan malli on merkitsev�sti liian yksinkertainen:
anova(fit.a, fit.b)


#c) Huomioi nyt, ett� kustakin potilaasta on kaksi havaintoa lis��m�ll� malliin frailty-muuttuja.
#Mik� on R:n oletusjakauma frailtylle? Miten arvioit tarvitaanko frailty-mallia toistuvien infek tioiden korreloituneisuuden
#ja ei-mitattujen vaikutusten kattamiseksi? 
#Ent� jos j�t�t mallista pois tautityypin (disease)? Miten frailty nyt k�ytt�ytyy?
# frailty-termi lis�ttyn�
fit.c1 <- coxph(Surv(time, event=status) ~ sex + age + disease +frailty(id), data=kidney)
summary(fit.c1)

#R:n oletusjakauma frailtylle on Gamma keskiarvolla 1. Sen tarpeellisuus on mallin mukaan kuitenkin v�h�inen,
#sill� sen varianssin estimaatti on k�yt�nn�ss� 0. N�in k�y helposti, jos potilaiden sis�inen korrelaatio on suhteelli-
#sen pient� verrattuna selitt�vien tekij�iden vaikutukseen. Katsotaan mit� tapahtuu, jos pudotetaan taudin tyyppi
#pois mallista.

fit.c2 <- coxph(Surv(time, event=status) ~ sex + age +frailty(id), data=kidney)
summary(fit.c2)

#Nyt kun mallissa on v�hemm�n selitt�ji�, saadaan frailtytermi estimoitua. Henkil�iden v�lisen riskin varianssin
#piste-estimaatti on 0.41. T�m� vastaa oheista jakaumaa:

curve(dgamma(x, shape=1/0.412, scale=0.412), from=0, to=5, ylab="f(u)", xlab="u", lwd=3)

#d) Plottaa nyt 1. mallin martingaaliresiduaalit tautityypeitt�in. Onko l�ydett�viss� yksil�llisi� eroja, 
#jotka selitt�isiv�t kahden viimeisen mallin eron frailtyn suhteen? 
#Poista aineistosta tarvittaessa ��rimm�iset havainnot ja estimoi mallit uudelleen.

# tallennetaan j��nn�kset aineistoon ja toistetaan aikasempi kuva
kidney$res.b <- residuals(fit.b, type="martingale")
ggplot(data=kidney,aes(x=sex, y=res.b, col=sex, label=id)) +facet_grid(. ~ disease) +
geom_text(position="jitter") +theme_light() +ggtitle("J��nn�kset eri ryhmiss�")

#Aineistossa on kaksi poikkeavaa havaintoa, jotka voivat selitt�� eron mallien c1 ja c2 v�lill�. 
#Poistetaan n�m� ja sovitetaan mallit uudestaan.

# poistetaan kaksi poikkeavaa havaintoa ja sovitetaan uudestaan
kidney2 <- subset(kidney, subset=!(id %in% c(10, 21)))
fit.d1 <-coxph(Surv(time, event=status) ~ sex + age + disease +frailty(id),data=kidney2)
fit.d2 <-coxph(Surv(time, event=status) ~ sex + age +frailty(id), data=kidney2)
summary(fit.d1)
summary(fit.d2)

#e) M��rittele nyt frailtyn jakaumaksi normaalijakauma. Survival moduli k�ytt�� t�ss� tapauksessa 
#ns. rajoitettua ML-estimointia (REstricted ML, REML). Miten malli eroaa aikaisemmasta?

fit.e <-coxph(Surv(time, event=status) ~ sex + age + disease +frailty(id, distribution="gaussian"), data=kidney2)
summary(fit.e)

