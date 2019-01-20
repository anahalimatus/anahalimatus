#BANTEN#
BANTEN=read.csv(file.choose(),header=T)
BANTEN
ts.plot(BANTEN,ylab="",main="Data Curah Hujan DJF BANTEN",col=c(1:8))
legend("topleft",c("Kab. Lebak","Kab. Pandeglang","Kab. Serang","Kab. Tangerang","Kota Cilegon","Kota Serang","Kota Tangerang","Kota Tangerang Selatan"),cex=0.8, lty = 1, text.font=, col=c(1:8))
Banten1=colMeans(BANTEN)
Banten1
Banten=sort(Banten1)
Banten
#STATISTIKA DESKRIPTIF#
summary(Banten)
a=c("Banten")
b=c(mean(Banten))
c=c(max(Banten)-min(Banten))
d=c(var(Banten))
e=c(sd(Banten))
f=c((sd(Banten)/mean(Banten))*100)
g=data_frame=data.frame(a,b,c,d,e,f)
names(data_frame)=c("Lokasi","Rata2","Rentang","Variansi","St Deviasi","Koef Variasi")
data_frame
par(mfrow=c(1,2))
plot(Banten,main="Plot Banten",xlab="Indeks Lokasi Curah Hujan",ylab="Rata2 CH DJF Banten")
hist(Banten,main="Histogram Banten",xlab="Rata2 CH DJF Banten",ylab="Frequency")
#RUMUS INDEKS GINI BROWN#
y1<-c(0,sort(Banten))
y1
k1<-0:8
k1
f1<-c(0,(rep(1,each=8))/8)
f1
l1<-c(y1/sum(y1))
l1
L1<-cumsum(l1)
L1
F1<-cumsum(f1)
F1
A1<-c(L1[1],(L1[2:9]+L1[1:8]))
A1
B1<-c(F1[1],(F1[2:9]-F1[1:8]))
B1
C1<-A1*B1
C1
IndeksGiniBrownBanten<-abs(1-sum(C1))
IndeksGiniBrownBanten
#RUMUS INDEKS GINI ZITIKIS#
y1<-Banten
y1
i1<-0:8/8
i1
z1<-c(0,sort(y1))
z1
lk1<-cumsum((z1)/sum(z1))
lk1
ii1<-1:8
ii1
a1<-c(((2*ii1)-1)*y1)
a1
IndeksGiniZitikisBanten<-(-1)+sum(a1)/(mean(y1)*8^2)
IndeksGiniZitikisBanten
#KURVA LORENZ#
par(mfrow=c(1,1))
plot(i1,i1,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=1)
par(new=T)
plot(i1,lk1,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=2)
title("Kurva Lorenz Provinsi Banten")
typ.names<-c("Garis Diagonal","Lorenz Banten")
legend(locator(1),legend=typ.names,fill=1:2)





#DKI JAKARTA#
DKI=read.csv(file.choose(),header=T)
DKI
ts.plot(DKI,ylab="",main="Data Curah Hujan di DKI Jakarta",col=c(1:6))
legend("topleft",c("Kab. Kep. Seribu","Kota Jakarta Barat","Kota Jakarta Pusat","Kota Jakarta Selatan","Kota Jakarta Timur","Kota Jakarta Utara"),cex=0.8, lty = 1, text.font=, col=c(1:6))
DKI1=colMeans(DKI)
DKI1
DKIJakarta=sort(DKI1)
DKIJakarta
#STATISTIKA DESKRIPTIF#
summary(DKIJakarta)
a=c("DKIJakarta")
b=c(mean(DKIJakarta))
c=c(max(DKIJakarta)-min(DKIJakarta))
d=c(var(DKIJakarta))
e=c(sd(DKIJakarta))
f=c((sd(DKIJakarta)/mean(DKIJakarta))*100)
g=data_frame=data.frame(a,b,c,d,e,f)
names(data_frame)=c("Lokasi","Rata2","Rentang","Variansi","St Deviasi","Koef Variasi")
data_frame
par(mfrow=c(1,2))
plot(DKIJakarta,main="Plot DKI Jakarta",xlab="Indeks Lokasi Curah Hujan",ylab="Rata2 CH DJF DKI Jakarta")
hist(DKIJakarta,main="Histogram DKI Jakarta",xlab="Rata2 CH DJF DKI Jakarta",ylab="Frequency")
#RUMUS INDEKS GINI BROWN#
y2<-c(0,sort(DKIJakarta))
y2
k2<-0:6
k2
f2<-c(0,(rep(1,each=6))/6)
f2
l2<-c(y2/sum(y2))
l2
L2<-cumsum(l2)
L2
F2<-cumsum(f2)
F2
A2<-c(L2[1],(L2[2:7]+L2[1:6]))
A2
B2<-c(F2[1],(F2[2:7]-F2[1:6]))
B2
C2<-A2*B2
C2
IndeksGiniBrownDKIJakarta<-abs(1-sum(C2))
IndeksGiniBrownDKIJakarta
#RUMUS INDEKS GINI ZITIKIS#
y2<-DKIJakarta
y2
i2<-0:6/6
i2
z2<-c(0,sort(y2))
z2
lk2<-cumsum((z2)/sum(z2))
lk2
ii2<-1:6
ii2
a2<-c(((2*ii2)-1)*y2)
a2
IndeksGiniZitikisDKIJakarta<-(-1)+sum(a2)/(mean(y2)*6^2)
IndeksGiniZitikisDKIJakarta
#KURVA LORENZ#
par(mfrow=c(1,1))
plot(i2,i2,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=2)
par(new=T)
plot(i2,lk2,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujani",ylab="Persentase Kumulatif Curah Hujan",type="l",col=3)
typ.names<-c("Garis Diagonal","Lorenz DKI Jakarta")
title("Kurva Lorenz Provinsi DKI Jakarta")
legend(locator(1),legend=typ.names,fill=2:3)





#JAWA BARAT#
JABAR=read.csv(file.choose(),header=T)
JABAR
ts.plot(JABAR,ylab="",main="Data Curah Hujan DJF Jawa Barat",col=c(1:27))
legend("topleft",c("Kab. Bandung","Kab. Bandung Barat","Kab. Bekasi","Kab. Bogor","Kab. Ciamis","Kab. Cianjur","Kab. Cirebon","Kab. Garut","Kab. Indramayu","Kab. Karawang","Kab. Kuningan","Kab. Majalengka","Kab. Pangandaran","Kab. Purwakarta"),cex=0.6, lty = 1, text.font=, col=c(1:14))
legend("top",c("Kab. Subang","Kab. Sukabumi","Kab. Sumedang","Kab. Tasikmalaya","Kota Bandung","Kota Banjar","Kota Bekasi","Kota Bogor","Kota Cimahi","Kota Cirebon","Kota Depok","Kota Sukabumi","Kota Tasikmalaya"),cex=0.6, lty = 1, text.font=, col=c(15:27))
JABAR1=colMeans(JABAR)
JABAR1
JawaBarat=sort(JABAR1)
JawaBarat
#STATISTIKA DESKRIPTIF#
summary(JawaBarat)
a=c("JawaBarat")
b=c(mean(JawaBarat))
c=c(max(JawaBarat)-min(JawaBarat))
d=c(var(JawaBarat))
e=c(sd(JawaBarat))
f=c((sd(JawaBarat)/mean(JawaBarat))*100)
g=data_frame=data.frame(a,b,c,d,e,f)
names(data_frame)=c("Lokasi","Rata2","Rentang","Variansi","St Deviasi","Koef Variasi")
data_frame
par(mfrow=c(1,2))
plot(JawaBarat,main="Plot Jawa Barat",xlab="Indeks Lokasi Curah Hujan",ylab="Rata2 CH DJF Jawa Barat")
hist(JawaBarat,main="Histogram Jawa Barat",xlab="Rata2 CH DJF Jawa Barat",ylab="Frequency")
#RUMUS INDEKS GINI BROWN#
y3<-c(0,sort(JawaBarat))
y3
k3<-0:27
k3
f3<-c(0,(rep(1,each=27))/27)
f3
l3<-c(y3/sum(y3))
l3
L3<-cumsum(l3)
L3
F3<-cumsum(f3)
F3
A3<-c(L3[1],(L3[2:28]+L3[1:27]))
A3
B3<-c(F3[1],(F3[2:28]-F3[1:27]))
B3
C3<-A3*B3
C3
IndeksGiniBrownJawaBarat<-abs(1-sum(C3))
IndeksGiniBrownJawaBarat
#RUMUS INDEKS GINI ZITIKIS#
y3<-JawaBarat
y3
i3<-0:27/27
i3
z3<-c(0,sort(y3))
z3
lk3<-cumsum((z3)/sum(z3))
lk3
ii3<-1:27
ii3
a3<-c(((2*ii3)-1)*y3)
a3
IndeksGiniZitikisJawaBarat<-(-1)+sum(a3)/(mean(y3)*27^2)
IndeksGiniZitikisJawaBarat
#KURVA LORENZ#
par(mfrow=c(1,1))
plot(i3,i3,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=3)
par(new=T)
plot(i3,lk3,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=4)
typ.names<-c("Garis Diagonal","Lorenz Jawa Barat")
title("Kurva Lorenz Provinsi Jawa Barat")
legend(locator(1),legend=typ.names,fill=3:4)





#JAWA TENGAH#
JATENG=read.csv(file.choose(),header=T)
JATENG
ts.plot(JATENG,ylab="",main="Data Curah Hujan DJF Jawa Tengah", col=c(1:35))
legend("topleft",c("Kab. Banjarnegara","Kab. Banyumas","Kab. Batang","Kab. Blora","Kab. Boyolali","Kab. Brebes","Kab. Cilacap","Kab. Demak","Kab. Grobogan","Kab. Jepara","Kab. Karanganyar","Kab. Kebumen","Kab. Kendal"),cex=0.6, lty = 1, text.font=, col=c(1:13))
legend("top",c("Kab. Klaten","Kab. Kudus","Kab. Magelang","Kab. Pati","Kab. Pekalongan","Kab. Pemalang","Kab. Purbalingga","Kab. Purworejo","Kab. Rembang","Kab. Semarang","Kab. Sragen","Kab. Sukoharjo","Kab. Tegal"),cex=0.6, lty = 1, text.font=, col=c(14:26))
legend("topright",c("Kab. Temanggung","Kab. Wonogiri","Kab. Wonosobo","Kota Magelang","Kota Pekalongan","Kota Salatiga","Kota Semarang","Kota Surakarta","Kota Tegal"),cex=0.6, lty = 1, text.font=, col=c(27:35))
JATENG1=colMeans(JATENG)
JATENG1
JawaTengah=sort(JATENG1)
JawaTengah
#STATISTIKA DESKRIPTIF#
summary(JawaTengah)
a=c("JawaTengah")
b=c(mean(JawaTengah))
c=c(max(JawaTengah)-min(JawaTengah))
d=c(var(JawaTengah))
e=c(sd(JawaTengah))
f=c((sd(JawaTengah)/mean(JawaTengah))*100)
g=data_frame=data.frame(a,b,c,d,e,f)
names(data_frame)=c("Lokasi","Rata2","Rentang","Variansi","St Deviasi","Koef Variasi")
data_frame
par(mfrow=c(1,2))
plot(JawaTengah,main="Plot Jawa Tengah",xlab="Indeks Lokasi Curah Hujan",ylab="Rata2 CH DJF Jawa Tengah")
hist(JawaTengah,main="Histogram Jawa Tengah",xlab="Rata2 CH DJF Jawa Tengah",ylab="Frequency")
#RUMUS INDEKS GINI BROWN#
y4<-c(0,sort(JawaTengah))
y4
k4<-0:35
k4
f4<-c(0,(rep(1,each=35))/35)
f4
l4<-c(y4/sum(y4))
l4
L4<-cumsum(l4)
L4
F4<-cumsum(f4)
F4
A4<-c(L4[1],(L4[2:36]+L4[1:35]))
A4
B4<-c(F4[1],(F4[2:36]-F4[1:35]))
B4
C4<-A4*B4
C4
IndeksGiniBrownJawaTengah<-abs(1-sum(C4))
IndeksGiniBrownJawaTengah
#RUMUS INDEKS GINI ZITIKIS#
y4<-JawaTengah
y4
i4<-0:35/35
i4
z4<-c(0,sort(y4))
z4
lk4<-cumsum((z4)/sum(z4))
lk4
ii4<-1:35
ii4
a4<-c(((2*ii4)-1)*y4)
a4
IndeksGiniZitikisJawaTengah<-(-1)+sum(a4)/(mean(y4)*35^2)
IndeksGiniZitikisJawaTengah
#KURVA LORENZ#
par(mfrow=c(1,1))
plot(i4,i4,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=4)
par(new=T)
plot(i4,lk4,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=5)
typ.names<-c("Garis Diagonal","Lorenz Jawa Tengah")
title("Kurva Lorenz Provinsi Jawa Tengah")
legend(locator(1),legend=typ.names,fill=4:5)





#DI YOGYAKARTA#
DIY=read.csv(file.choose(),header=T)
DIY
ts.plot(DIY,ylab="",main="Data Curah Hujan di DI Yogyakarta", col=c(1:5))
legend("topright",c("Kab. Bantul","Kab. Gunungkidul","Kab. Kulon Progo","Kab. Sleman","Kota Yogyakarta"),cex=0.8, lty = 1, text.font=, col=c(1:5))
DIY1=colMeans(DIY)
DIY1
DIYogyakarta=sort(DIY1)
DIYogyakarta
#STATISTIKA DESKRIPTIF#
summary(DIYogyakarta)
a=c("DIYogyakarta")
b=c(mean(DIYogyakarta))
c=c(max(DIYogyakarta)-min(DIYogyakarta))
d=c(var(DIYogyakarta))
e=c(sd(DIYogyakarta))
f=c((sd(DIYogyakarta)/mean(DIYogyakarta))*100)
g=data_frame=data.frame(a,b,c,d,e,f)
names(data_frame)=c("Lokasi","Rata2","Rentang","Variansi","St Deviasi","Koef Variasi")
data_frame
par(mfrow=c(1,2))
plot(DIYogyakarta,main="Plot DIYogyakarta",xlab="Indeks Lokasi Curah Hujan",ylab="Rata2 CH DJF DIYogyakarta")
hist(DIYogyakarta,main="Histogram DIYogyakarta",xlab="Rata2 CH DJF DIYogyakarta",ylab="Frequency")
#RUMUS INDEKS GINI BROWN#
y5<-c(0,sort(DIYogyakarta))
y5
k5<-0:5
k5
f5<-c(0,(rep(1,each=5))/5)
f5
l5<-c(y5/sum(y5))
l5
L5<-cumsum(l5)
L5
F5<-cumsum(f5)
F5
A5<-c(L5[1],(L5[2:6]+L5[1:5]))
A5
B5<-c(F5[1],(F5[2:6]-F5[1:5]))
B5
C5<-A5*B5
C5
IndeksGiniBrownDIYogyakarta<-abs(1-sum(C5))
IndeksGiniBrownDIYogyakarta
#RUMUS INDEKS GINI ZITIKIS#
y5<-DIYogyakarta
y5
i5<-0:5/5
i5
z5<-c(0,sort(y5))
z5
lk5<-cumsum((z5)/sum(z5))
lk5
ii5<-1:5
ii5
a5<-c(((2*ii5)-1)*y5)
a5
IndeksGiniZitikisDIYogyakarta<-(-1)+sum(a5)/(mean(y5)*5^2)
IndeksGiniZitikisDIYogyakarta
#KURVA LORENZ#
par(mfrow=c(1,1))
plot(i5,i5,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=5)
par(new=T)
plot(i5,lk5,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=6)
typ.names<-c("Garis Diagonal","Lorenz DI Yogyakarta")
title("Kurva Lorenz Provinsi DI Yogyakarta")
legend(locator(1),legend=typ.names,fill=5:6)





#JAWA TIMUR#
JATIM=read.csv(file.choose(),header=T)
JATIM
ts.plot(JATIM,ylab="",main="Data Curah Hujan di Jawa Timur", col=c(1:38))
legend("topleft",c("Kab. Bangkalan","Kab. Banyuwangi","Kab. Blitar","Kab. Bojonegoro","Kab. Bondowoso","Kab. Gresik","Kab. Jember","Kab. Jombang","Kab. Kediri","Kab. Lamongan","Kab. Lumajang","Kab. Madiun","Kab. Magetan"),cex=0.55, lty = 1, text.font=, col=c(1:13))
legend("top",c("Kab. Malang","Kab. Mojokerto","Kab. Nganjuk","Kab. Ngawi","Kab. Pacitan","Kab. Pamekasan","Kab. Pasuruan","Kab. Ponorogo","Kab. Probolinggo","Kab. Sampang","Kab. Sidoarjo","Kab. Situbondo","Kab. Sumenep"),cex=0.55, lty = 1, text.font=, col=c(14:26))
legend("topright",c("Kab. Trenggalek","Kab. Tuban","Kab. Tulungagung","Kota Batu","Kota Blitar","Kota Kediri","Kota Madiun","Kota Malang","Kota Mojokerto","Kota Pasuruan","Kota Probolinggo","Kota Surabaya"),cex=0.55, lty = 1, text.font=, col=c(27:38))
JATIM1=colMeans(JATIM)
JATIM1
JawaTimur=sort(JATIM1)
JawaTimur
#STATISTIKA DESKRIPTIF#
summary(JawaTimur)
a=c("JawaTimur")
b=c(mean(JawaTimur))
c=c(max(JawaTimur)-min(JawaTimur))
d=c(var(JawaTimur))
e=c(sd(JawaTimur))
f=c((sd(JawaTimur)/mean(JawaTimur))*100)
g=data_frame=data.frame(a,b,c,d,e,f)
names(data_frame)=c("Lokasi","Rata2","Rentang","Variansi","St Deviasi","Koef Variasi")
data_frame
par(mfrow=c(1,2))
plot(JawaTimur,main="Plot Jawa Timur",xlab="Indeks Lokasi Curah Hujan",ylab="Rata2 CH DJF Jawa Timur")
hist(JawaTimur,main="Histogram Jawa Timur",xlab="Rata2 CH DJF Jawa Timur",ylab="Frequency")
#RUMUS INDEKS GINI BROWN#
y6<-c(0,sort(JawaTimur))
y6
k6<-0:38
k6
f6<-c(0,(rep(1,each=38))/38)
f6
l6<-c(y6/sum(y6))
l6
L6<-cumsum(l6)
L6
F6<-cumsum(f6)
F6
A6<-c(L6[1],(L6[2:39]+L6[1:38]))
A6
B6<-c(F6[1],(F6[2:39]-F6[1:38]))
B6
C6<-A6*B6
C6
IndeksGiniBrownJawaTimur<-abs(1-sum(C6))
IndeksGiniBrownJawaTimur
#RUMUS INDEKS GINI ZITIKIS#
y6<-JawaTimur
y6
i6<-0:38/38
i6
z6<-c(0,sort(y6))
z6
lk6<-cumsum((z6)/sum(z6))
lk6
ii6<-1:38
ii6
a6<-c(((2*ii6)-1)*y6)
a6
IndeksGiniZitikisJawaTimur<-(-1)+sum(a6)/(mean(y6)*38^2)
IndeksGiniZitikisJawaTimur
#KURVA LORENZ#
par(mfrow=c(1,1))
plot(i6,i6,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=6)
par(new=T)
plot(i6,lk6,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=7)
typ.names<-c("Garis Diagonal","Lorenz Jawa Timur")
title("Kurva Lorenz Provinsi Jawa Timur")
legend(locator(1),legend=typ.names,fill=6:7)





#JAWA#
JAWA<-c(mean(Banten), mean(DKIJakarta), mean(JawaBarat), mean(JawaTengah), mean(DIYogyakarta), mean(JawaTimur))
JAWA
Jawa=sort(JAWA)
Jawa
#STATISTIKA DESKRIPTIF#
summary(Jawa)
a=c("Jawa")
b=c(mean(Jawa))
c=c(max(Jawa)-min(Jawa))
d=c(var(Jawa))
e=c(sd(Jawa))
f=c((sd(Jawa)/mean(Jawa))*100)
g=data_frame=data.frame(a,b,c,d,e,f)
names(data_frame)=c("Lokasi","Rata2","Rentang","Variansi","St Deviasi","Koef Variasi")
data_frame
par(mfrow=c(1,2))
plot(Jawa,main="Plot Pulau Jawa",xlab="Indeks Lokasi Curah Hujan",ylab="Rata2 CH DJF Pulau Jawa")
hist(Jawa,main="Histogram Pulau Jawa",xlab="Rata2 CH DJF Pulau Jawa",ylab="Frequency")
#RUMUS INDEKS GINI BROWN#
y7<-c(0,sort(Jawa))
y7
k7<-0:6
k7
f7<-c(0,(rep(1,each=6))/6)
f7
l7<-c(y7/sum(y7))
l7
L7<-cumsum(l7)
L7
F7<-cumsum(f7)
F7
A7<-c(L7[1],(L7[2:7]+L7[1:6]))
A7
B7<-c(F7[1],(F7[2:7]-F7[1:6]))
B7
C7<-A7*B7
C7
IndeksGiniBrownJawaTimur<-abs(1-sum(C7))
IndeksGiniBrownJawaTimur
#RUMUS INDEKS GINI ZITIKIS#
y7<-Jawa
y7
i7<-0:6/6
i7
z7<-c(0,sort(y7))
z7
lk7<-cumsum((z7)/sum(z7))
lk7
ii7<-1:6
ii7
a7<-c(((2*ii7)-1)*y7)
a7
IndeksGiniZitikisJawa<-(-1)+sum(a7)/(mean(y7)*6^2)
IndeksGiniZitikisJawa
#KURVA LORENZ#
par(mfrow=c(1,1))
plot(i7,i7,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=7)
par(new=T)
plot(i7,lk7,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=8)
title("Kurva Lorenz Pulau Jawa")
typ.names<-c("Garis Diagonal","Lorenz Jawa")
legend(locator(1),legend=typ.names,fill=7:8)







par(mfrow=c(3,2))
plot(Banten,main="Plot Banten",xlab="Indeks Lokasi Curah Hujan",ylab="Rata2 CH DJF Banten")
hist(Banten,main="Histogram Banten",xlab="Rata2 CH DJF Banten",ylab="Frequency")
plot(DKIJakarta,main="Plot DKI Jakarta",xlab="Indeks Lokasi Curah Hujan",ylab="Rata2 CH DJF DKI Jakarta")
hist(DKIJakarta,main="Histogram DKI Jakarta",xlab="Rata2 CH DJF DKI Jakarta",ylab="Frequency")

plot(JawaBarat,main="Plot Jawa Barat",xlab="Indeks Lokasi Curah Hujan",ylab="Rata2 CH DJF Jawa Barat")
hist(JawaBarat,main="Histogram Jawa Barat",xlab="Rata2 CH DJF Jawa Barat",ylab="Frequency")
plot(JawaTengah,main="Plot Jawa Tengah",xlab="Indeks Lokasi Curah Hujan",ylab="Rata2 CH DJF Jawa Tengah")
hist(JawaTengah,main="Histogram Jawa Tengah",xlab="Rata2 CH DJF Jawa Tengah",ylab="Frequency")

plot(DIYogyakarta,main="Plot DIYogyakarta",xlab="Indeks Lokasi Curah Hujan",ylab="Rata2 CH DJF DIYogyakarta")
hist(DIYogyakarta,main="Histogram DIYogyakarta",xlab="Rata2 CH DJF DIYogyakarta",ylab="Frequency")
plot(JawaTimur,main="Plot Jawa Timur",xlab="Indeks Lokasi Curah Hujan",ylab="Rata2 CH DJF Jawa Timur")
hist(JawaTimur,main="Histogram Jawa Timur",xlab="Rata2 CH DJF Jawa Timur",ylab="Frequency")



#KURVA LORENZ#
par(mfrow=c(2,2))
plot(i1,i1,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=1)
par(new=T)
plot(i1,lk1,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=2)
title("Kurva Lorenz Provinsi Banten")
typ.names<-c("Garis Diagonal","Lorenz Banten")
legend(locator(1),legend=typ.names,fill=1:2)

plot(i2,i2,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=2)
par(new=T)
plot(i2,lk2,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=3)
typ.names<-c("Garis Diagonal","Lorenz DKI Jakarta")
title("Kurva Lorenz Provinsi DKI Jakarta")
legend(locator(1),legend=typ.names,fill=2:3)


plot(i3,i3,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=3)
par(new=T)
plot(i3,lk3,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=4)
typ.names<-c("Garis Diagonal","Lorenz Jawa Barat")
title("Kurva Lorenz Provinsi Jawa Barat")
legend(locator(1),legend=typ.names,fill=3:4)

plot(i4,i4,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=4)
par(new=T)
plot(i4,lk4,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=5)
typ.names<-c("Garis Diagonal","Lorenz Jawa Tengah")
title("Kurva Lorenz Provinsi Jawa Tengah")
legend(locator(1),legend=typ.names,fill=4:5)


plot(i5,i5,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=5)
par(new=T)
plot(i5,lk5,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=6)
typ.names<-c("Garis Diagonal","Lorenz DI Yogyakarta")
title("Kurva Lorenz Provinsi DI Yogyakarta")
legend(locator(1),legend=typ.names,fill=5:6)

plot(i6,i6,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=6)
par(new=T)
plot(i6,lk6,xlim=c(0,1), ylim=c(0,1), xlab="Persentase Kumulatif Indeks Lokasi Curah Hujan",ylab="Persentase Kumulatif Curah Hujan",type="l",col=7)
typ.names<-c("Garis Diagonal","Lorenz Jawa Timur")
title("Kurva Lorenz Provinsi Jawa Timur")
legend(locator(1),legend=typ.names,fill=6:7)