#!/usr/bin/Rscript

#Se lo script viene eseguito da amministratore (permessi di scrittura nella cartella delle librerie), installa le librerie
if (file.access(.libPaths()[1],2)==0) {
    install.packages("ggplot2",repos = "https://cran.stat.unipd.it/");
    install.packages("gridSVG",repos = "https://cran.stat.unipd.it/");
    print("Se ci sono stati errori, esegui sudo apt-get install libxml2-dev e riprova.")
    print("Sembra che tu sia amministratore, sarebbe meglio procedere solo da utente semplice. Vuoi comunque creare i grafici? [y/N]");
    choice <- readLines("stdin", 1);
    if (choice != "Y" && choice != "y") quit();
}

library(ggplot2);
require(gridSVG);


histogramsByGroup <- function(tmpname, tmpgroup, tmpdata) {

for(i in levels(tmpgroup)){
    if (i != "") {

        mytb <- subset(tmpdata, tmpgroup==i & tmpdata!="");
        mytb <- droplevels(mytb);
        #print(mytb);
        print(levels(mytb));
        
        histogram = ggplot(data.frame(mytb), aes(x=mytb, fill=mytb)) + geom_histogram(position = "identity", stat="count", alpha = 1);
        histogram = histogram + geom_text(stat = "count", aes(label = ..count.., y = ..count..));
        histogram = histogram + labs(x = NULL, y = NULL, fill = NULL, title = sub('-', ' ', tmpname));

        histogram = histogram + theme_classic() + theme(axis.line = element_blank(),
         axis.text = element_blank(),
         axis.ticks = element_blank(),
         plot.title = element_text(hjust = 0.5, color = "#666666"));

        #Esporto il grafico in un file SVG
        print(histogram);
        grid.export(paste("./svg/", tmpname, "-", sub('[^A-Za-z0-9]', '', i), ".svg", sep=""),addClasses=TRUE);
    }
}

}


histogramSorted <- function(tmpname, tmpdata) {

        mytb <- data.frame(table(tmpdata))
        colnames(mytb) <- c("Dati","Freq")
        #mytb <- mytb[order(mytb$Freq),];
        mytb$Dati <- factor(mytb$Dati, levels = mytb$Dati[order(mytb$Freq)])
        print(mytb);
        
        histogram = ggplot(mytb, aes(x=Dati, y=Freq, fill=mytb$Dati )) + geom_bar(position = "identity", stat="identity", alpha = 1);

        histogram = histogram + geom_text(stat = "count", aes(label = mytb$Freq, y = ..count..));
        histogram = histogram + labs(x = NULL, y = NULL, fill = NULL, title = sub('-', ' ', tmpname));

        histogram = histogram + theme_classic() + theme(axis.line = element_blank(),
         axis.text = element_blank(),
         axis.ticks = element_blank(),
         plot.title = element_text(hjust = 0.5, color = "#666666"));

        #Esporto il grafico in un file SVG
        print(histogram);
        grid.export(paste("./svg/", tmpname, ".svg", sep=""),addClasses=TRUE);

}


#Discriminazione
path <- "./discriminazione.csv";
#args <- commandArgs(trailingOnly=TRUE);
#if (length(args)>0) path <- args[1];
#basename <- sub('\\.csv$', '', path);
file <- read.table(path,header=TRUE, sep=",");

tmpname <- "Gender gap";
tmpgroup <- file$Di.che.genere.sei.;
tmpdata <- file$Ritieni.vi.sia.disparità.di.trattamento.tra.uomini.e.donne.nel.campo.della.sicurezza.informatica.;
histogramsByGroup(tmpname, tmpgroup, tmpdata);

tmpname <- "Discriminazione";
tmpgroup <- file$Di.che.genere.sei.;
tmpdata <- file$Ritieni.vi.sia.disparità.di.trattamento.tra.uomini.e.donne.nel.campo.della.sicurezza.informatica.;
histogramsByGroup(tmpname, tmpgroup, tmpdata);


#Tutele
path <- "./tutele.csv";
file <- read.table(path,header=TRUE, sep=",");

tmpname <- "Tutele dei lavoratori";
tmpgroup <- file$In.che.classe.di.età.rientri.;
tmpdata <- file$Come.definiresti.le.tutele.del.lavoratore.per.chi.lavora.nella.sicurezza.informatica...Giorni.di.ferie..malattia..maternità.paternità..assegno.di.disoccupazione....;
histogramsByGroup(tmpname, tmpgroup, tmpdata);

tmpname <- "Fiducia";
tmpgroup <- file$In.che.classe.di.età.rientri.;
tmpdata <- file$In.generale..quanta.fiducia.hai.in.questo.mestiere.per.il.tuo.sostentamento.;
histogramsByGroup(tmpname, tmpgroup, tmpdata);

tmpname <- "Contratto";
tmpgroup <- file$In.che.classe.di.età.rientri.;
tmpdata <- file$Se.lavori.per.una.azienda.specializzata.in.sicurezza.informatica..con.quale.contratto.sei.assunto.;
histogramsByGroup(tmpname, tmpgroup, tmpdata);

tmpname <- "Famiglia";
tmpgroup <- file$In.che.classe.di.età.rientri.;
tmpdata <- file$Pensi.che.il.lavoro.nel.campo.della.sicurezza.informatica.incida.sulle.possibilità.di.formare.un.nucleo.familiare.;
histogramsByGroup(tmpname, tmpgroup, tmpdata);

tmpname <- "Sindacato";
tmpgroup <- file$In.che.classe.di.età.rientri.;
tmpdata <- file$Riterresti.utile.la.costituzione.di.un.vero.e.proprio.sindacato.di.categoria.per.i.PenTester..i.programmatori..o.più.in.generale.gli.informatici..Attualmente.esiste.il.sindacato.networkers.ma.non.è.riconosciuto.ufficialmente.e.non.ha.molto.potere.contrattuale.;
histogramsByGroup(tmpname, tmpgroup, tmpdata);

#Stipendi
path <- "./stipendi.csv";
file <- read.table(path,header=TRUE, sep=",");

tmpname <- "Stipendio Junior";
tmpgroup <- file$In.che.classe.di.età.rientri.;
tmpdata <- file$Nella.tua.esperienza..quanto.guadagna.un.PenTester.in.Italia.nei.primi.5.anni.di.lavoro.;
histogramsByGroup(tmpname, tmpgroup, tmpdata);

tmpname <- "Stipendio Senior";
tmpgroup <- file$In.che.classe.di.età.rientri.;
tmpdata <- file$Nella.tua.esperienza..quanto.guadagna.un.PenTester.senior.in.Italia.;
histogramsByGroup(tmpname, tmpgroup, tmpdata);

tmpname <- "Orario";
tmpgroup <- file$In.che.classe.di.età.rientri.;
tmpdata <- file$Come.ritieni.che.siano.i.ritmi.di.lavoro.;
histogramsByGroup(tmpname, tmpgroup, tmpdata);

tmpname <- "Confronto Italia Estero";
tmpgroup <- file$In.che.classe.di.età.rientri.;
tmpdata <- file$Nella.tua.esperienza..ritieni.che.un.PenTester.in.Italia.venga.pagato.meno.rispetto.a.altri.stati.come.Regno.Unito..Germania..Francia..oppure.che.nel.complesso.la.situazione.sia.simile.;
histogramsByGroup(tmpname, tmpgroup, tmpdata);



#Estero
path <- "./estero.csv";
file <- read.table(path,header=TRUE, sep=",");
tmpname <- "Estero";
tmpdata <- file$Paese;
histogramSorted(tmpname, tmpdata);

#Regioni
path <- "./regioni.csv";
file <- read.table(path,header=TRUE, sep=",");
tmpname <- "Regioni";
tmpdata <- file$Regione;
histogramSorted(tmpname, tmpdata);

#Apprendimento
path <- "./apprendimento.csv";
file <- read.table(path,header=TRUE, sep=",");
tmpname <- "Apprendimento";
tmpdata <- file$Apprendimento;
histogramSorted(tmpname, tmpdata);

#Competenze
path <- "./competenze.csv";
file <- read.table(path,header=TRUE, sep=",");
tmpname <- "Competenze";
tmpdata <- file$Competenze;
histogramSorted(tmpname, tmpdata);
