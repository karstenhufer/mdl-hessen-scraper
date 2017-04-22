
################################## MdL-Hessen-Scraper ##################################

library(xml2)
library(rvest)
library(magrittr)

mdl_list <- c("tarek-al-wazir-0","ulrike-alex-0","dr-walter-arnold","lena-arnoldt","sabine-b%C3%A4chle-scholz","j%C3%BCrgen-banzer","dr-ralf-norbert-bartelt","elke-barth","alexander-bauer","nicola-beer","holger-bellino","peter-beuth","marcus-bocklet","michael-boddenberg","volker-bouffier","ulrich-caspar","wolfgang-decker","christoph-degen","corrado-di-benedetto","klaus-dietz","angela-dorn","tobias-eckert","sigrid-erfurth","nancy-faeser","gabriele-faulhaber","martina-feldmayer","uwe-frankenberger","dieter-franz","j%C3%BCrgen-fr%C3%B6mmrich","kerstin-geis","lisa-gnadl","eva-goldbach","wolfgang-greilich","timon-gremmels","stephan-gr%C3%BCger","gernot-grumbach","stefan-gr%C3%BCttner","heike-habermann","dr-hc-j%C3%B6rg-uwe-hahn","ursula-hammann","karin-hartmann","christian-heinz","heike-hofmann","andreas-hofmeister","brigitte-hofmeyer","r%C3%BCdiger-holschuh","hartmut-honka","hans-j%C3%BCrgen-irmer","norbert-kartmann","heiko-kasseckert","frank-kaufmann","irmgard-klaff-isselmann","horst-klee","hugo-klein","kai-klose","eva-k%C3%BChne-h%C3%B6rmann","gerald-kummer","dirk-landau","judith-lannert","j%C3%BCrgen-lenders","angelika-l%C3%B6ber","frank-lortz","heinz-lotz","daniel-may","gerhard-merz","markus-meysner","klaus-peter-m%C3%B6ller","regine-m%C3%BCller","karin-m%C3%BCller","petra-m%C3%BCller-klepper","handan-%C3%B6zg%C3%BCven","m%C3%BCrvet-%C3%B6zt%C3%BCrk","manfred-pentz","lucia-puttrich","lothar-quanz","claudia-ravensburg","clemens-reif","florian-rentsch","michael-reul","boris-rhein","ren%C3%A9-rock","ernst-ewald-roth","g%C3%BCnter-rudolph","dr-thomas-sch%C3%A4fer","thorsten-sch%C3%A4fer-g%C3%BCmbel","jan-schalauske","hermann-schaus","norbert-schmitt","marjana-schott","armin-schwarz","uwe-serke","michael-siebel","dr-daniela-sommer","peter-stephan","ismail-tipi","tobias-utter","joachim-veyhelmann","mathias-wagner","astrid-wallmann","torsten-warnecke","sabine-waschke","marius-wei%C3%9F","kurt-wiegel","bettina-wiesmann","dr-ulrich-wilken","axel-wintermeyer","janine-wissler","karin-wolff","andrea-ypsilanti","turgut-y%C3%BCksel")

mdl_einkuenfte_roh <- mdl_list

for(i in mdl_list){
  
  mdl_page <- paste("https://hessischer-landtag.de/content/",i, sep="")
  mdl_page <- read_html(mdl_page)
  
  mdl_data <-
    data.frame(Komplettname = mdl_page %>% html_nodes("div h1") %>% html_text() ,
               Verhaltensregeln = mdl_page %>% html_node("div div .field-name-field-person-code-of-conduct") %>% html_text() ,
               stringsAsFactors=FALSE)
  mdl_einkuenfte_roh <- rbind(mdl_einkuenfte_roh,mdl_data)
}

mdl_einkuenfte_roh <- mdl_einkuenfte_roh[-1, ]

mdl_namen <- c("Al-Wazir","Alex","Arnold","Arnoldt","Bächle-Scholz","Banzer","Bartelt","Barth","Bauer","Beer","Bellino","Beuth","Bocklet","Boddenberg","Bouffier","Caspar","Decker","Degen","Di Benedetto","Dietz","Dorn","Eckert","Erfurth","Faeser","Faulhaber","Feldmayer","Frankenberger","Franz","Frömmrich","Geis","Gnadl","Goldbach","Greilich","Gremmels","Grüger","Grumbach","Grüttner","Habermann","Hahn","Hammann","Hartmann","Heinz","Hofmann","Hofmeister","Hofmeyer","Holschuh","Honka","Irmer","Kartmann","Kasseckert","Kaufmann","Klaff-Isselmann","Klee","Klein","Klose","Kühne-Hörmann","Kummer","Landau","Lannert","Lenders","Löber","Lortz","Lotz","May","Merz","Meysner","Möller","Müller","Müller","Müller-Klepper","Özgüven","Öztürk","Pentz","Puttrich","Quanz","Ravensburg","Reif","Rentsch","Reul","Rhein","Rock","Roth","Rudolph","Schäfer","Schäfer-Gümbel","Schalauske","Schaus","Schmitt","Schott","Schwarz","Serke","Siebel","Sommer","Stephan","Tipi","Utter","Veyhelmann","Wagner","Wallmann","Warnecke","Waschke","Weiß","Wiegel","Wiesmann","Wilken","Wintermeyer","Wissler","Wolff","Ypsilanti","Yüksel")
mdl_vornamen <- c("Tarek","Ulrike","Walter","Lena","Sabine","Jürgen","Ralf-Norbert","Elke","Alexander","Nicola","Holger","Peter","Marcus","Michael","Volker","Ulrich","Wolfgang","Christoph","Corrado","Klaus","Angela","Tobias","Sigrid","Nancy","Gabriele","Martina","Uwe","Dieter","Jürgen","Kerstin","Lisa","Eva","Wolfgang","Timon","Stephan","Gernot","Stefan","Heike","Jörg-Uwe","Ursula","Karin","Christian","Heike","Andreas","Brigitte","Rüdiger","Hartmut","Hans-Jürgen","Norbert","Heiko","Frank","Irmgard","Horst","Hugo","Kai","Eva","Gerald","Dirk","Judith","Jürgen","Angelika","Frank","Heinz","Daniel","Gerhard","Markus","Klaus Peter","Regine","Karin","Petra","Handan","Mürvet","Manfred","Lucia","Lothar","Claudia","Clemens","Florian","Michael","Boris","René","Ernst-Ewald","Günter","Thomas","Thorsten","Jan","Hermann","Norbert","Marjana","Armin","Uwe","Michael","Daniela","Peter","Ismail","Tobias","Joachim","Mathias","Astrid","Torsten","Sabine","Marius","Kurt","Bettina","Ulrich","Axel","Janine","Karin","Andrea","Turgut")
mdl_partei <- c("Grüne","SPD","CDU","CDU","CDU","CDU","CDU","SPD","CDU","FDP","CDU","CDU","Grüne","CDU","CDU","CDU","SPD","SPD","SPD","CDU","Grüne","SPD","Grüne","SPD","Linke","Grüne","SPD","SPD","Grüne","SPD","SPD","Grüne","FDP","SPD","SPD","SPD","CDU","SPD","FDP","Grüne","SPD","CDU","SPD","CDU","SPD","SPD","CDU","CDU","CDU","CDU","Grüne","CDU","CDU","CDU","Grüne","CDU","SPD","CDU","CDU","FDP","SPD","CDU","SPD","Grüne","SPD","CDU","CDU","Grüne","SPD","CDU","SPD","Grüne","CDU","CDU","SPD","CDU","CDU","FDP","CDU","CDU","FDP","SPD","SPD","CDU","SPD","Linke","Linke","SPD","Linke","CDU","CDU","SPD","SPD","CDU","CDU","CDU","CDU","Grüne","CDU","SPD","SPD","SPD","CDU","CDU","Linke","CDU","Linke","CDU","SPD","SPD")

mdl_einkuenfte_roh <- cbind(mdl_einkuenfte_roh, Name = mdl_namen, Vorname = mdl_vornamen, Partei = mdl_partei)

write.csv(x = mdl_einkuenfte_roh, file = "mdl_einkuenfte_roh.csv", row.names = FALSE)


################################## Start Text Analyse

library(NLP)
library(tm)
library(magrittr)

mdl_einkuenfte_analyse <- read.csv(file = "mdl_einkuenfte_roh.csv",
                                   header = TRUE, sep = ",", stringsAsFactors = FALSE)

mdl_einkuenfte_analyse$Partei <- as.factor(mdl_einkuenfte_analyse$Partei)

mdl_einkuenfte_analyse$Verhaltensregeln <- removePunctuation(mdl_einkuenfte_analyse$Verhaltensregeln, preserve_intra_word_dashes = TRUE)

mdl_einkuenfte_analyse$Verhaltensregeln <- gsub("jährlich", " jährlich", mdl_einkuenfte_analyse$Verhaltensregeln)
mdl_einkuenfte_analyse$Verhaltensregeln <- gsub("monatlich", " monatlich", mdl_einkuenfte_analyse$Verhaltensregeln)
mdl_einkuenfte_analyse$Verhaltensregeln <- gsub("2016", " 2016", mdl_einkuenfte_analyse$Verhaltensregeln)

mdl_einkuenfte_corpus <- Corpus(VectorSource(mdl_einkuenfte_analyse$Verhaltensregeln))

mdl_einkuenfte_corpus <- tm_map(mdl_einkuenfte_corpus, content_transformer(tolower))
mdl_einkuenfte_corpus <- tm_map(mdl_einkuenfte_corpus, stripWhitespace)

library(textreg)

mdl_einkuenfte_stufe1_monat <- phrase.count("monatlich stufe 1", mdl_einkuenfte_corpus)
mdl_einkuenfte_stufe1_jahr <- phrase.count("jährlich stufe 1", mdl_einkuenfte_corpus)
mdl_einkuenfte_stufe1_2016 <- phrase.count("2016 stufe 1", mdl_einkuenfte_corpus)

mdl_einkuenfte_stufe2_monat <- phrase.count("monatlich stufe 2", mdl_einkuenfte_corpus)
mdl_einkuenfte_stufe2_jahr <- phrase.count("jährlich stufe 2", mdl_einkuenfte_corpus)
mdl_einkuenfte_stufe2_2016 <- phrase.count("2016 stufe 2", mdl_einkuenfte_corpus)

mdl_einkuenfte_stufe3_monat <- phrase.count("monatlich stufe 3", mdl_einkuenfte_corpus)
mdl_einkuenfte_stufe3_jahr <- phrase.count("jährlich stufe 3", mdl_einkuenfte_corpus)
mdl_einkuenfte_stufe3_2016 <- phrase.count("2016 stufe 3", mdl_einkuenfte_corpus)

mdl_einkuenfte_stufe4_monat <- phrase.count("monatlich stufe 4", mdl_einkuenfte_corpus)
mdl_einkuenfte_stufe4_jahr <- phrase.count("jährlich stufe 4", mdl_einkuenfte_corpus)
mdl_einkuenfte_stufe4_2016 <- phrase.count("2016 stufe 4", mdl_einkuenfte_corpus)

mdl_einkuenfte_stufe5_monat <- phrase.count("monatlich stufe 5", mdl_einkuenfte_corpus)
mdl_einkuenfte_stufe5_jahr <- phrase.count("jährlich stufe 5", mdl_einkuenfte_corpus)
mdl_einkuenfte_stufe5_2016 <- phrase.count("2016 stufe 5", mdl_einkuenfte_corpus)

mdl_einkuenfte_stufe6_monat <- phrase.count("monatlich stufe 6", mdl_einkuenfte_corpus)
mdl_einkuenfte_stufe6_jahr <- phrase.count("jährlich stufe 6", mdl_einkuenfte_corpus)
mdl_einkuenfte_stufe6_2016 <- phrase.count("2016 stufe 6", mdl_einkuenfte_corpus)

mdl_einkuenfte_stufe7_monat <- phrase.count("monatlich stufe 7", mdl_einkuenfte_corpus)
mdl_einkuenfte_stufe7_jahr <- phrase.count("jährlich stufe 7", mdl_einkuenfte_corpus)
mdl_einkuenfte_stufe7_2016 <- phrase.count("2016 stufe 7", mdl_einkuenfte_corpus)

mdl_einkuenfte_stufe8_monat <- phrase.count("monatlich stufe 8", mdl_einkuenfte_corpus)
mdl_einkuenfte_stufe8_jahr <- phrase.count("jährlich stufe 8", mdl_einkuenfte_corpus)
mdl_einkuenfte_stufe8_2016 <- phrase.count("2016 stufe 8", mdl_einkuenfte_corpus)

mdl_einkuenfte_stufe9_monat <- phrase.count("monatlich stufe 9", mdl_einkuenfte_corpus)
mdl_einkuenfte_stufe9_jahr <- phrase.count("jährlich stufe 9", mdl_einkuenfte_corpus)
mdl_einkuenfte_stufe9_2016 <- phrase.count("2016 stufe 9", mdl_einkuenfte_corpus)

mdl_einkuenfte_stufe10_monat <- phrase.count("monatlich stufe 10", mdl_einkuenfte_corpus)
mdl_einkuenfte_stufe10_jahr <- phrase.count("jährlich stufe 10", mdl_einkuenfte_corpus)
mdl_einkuenfte_stufe10_2016 <- phrase.count("2016 stufe 10", mdl_einkuenfte_corpus)

mdl_einkuenfte_analyse <- cbind(mdl_einkuenfte_analyse,
                                St1_Monat = mdl_einkuenfte_stufe1_monat,
                                St1_Jahr = mdl_einkuenfte_stufe1_jahr,
                                St1_2016 = mdl_einkuenfte_stufe1_2016,
                                St2_Monat = mdl_einkuenfte_stufe2_monat,
                                St2_Jahr = mdl_einkuenfte_stufe2_jahr,
                                St2_2016 = mdl_einkuenfte_stufe2_2016,
                                St3_Monat = mdl_einkuenfte_stufe3_monat,
                                St3_Jahr = mdl_einkuenfte_stufe3_jahr,
                                St3_2016 = mdl_einkuenfte_stufe3_2016,
                                St4_Monat = mdl_einkuenfte_stufe4_monat,
                                St4_Jahr = mdl_einkuenfte_stufe4_jahr,
                                St4_2016 = mdl_einkuenfte_stufe4_2016,
                                St5_Monat = mdl_einkuenfte_stufe5_monat,
                                St5_Jahr = mdl_einkuenfte_stufe5_jahr,
                                St5_2016 = mdl_einkuenfte_stufe5_2016,
                                St6_Monat = mdl_einkuenfte_stufe6_monat,
                                St6_Jahr = mdl_einkuenfte_stufe6_jahr,
                                St6_2016 = mdl_einkuenfte_stufe6_2016,
                                St7_Monat = mdl_einkuenfte_stufe7_monat,
                                St7_Jahr = mdl_einkuenfte_stufe7_jahr,
                                St7_2016 = mdl_einkuenfte_stufe7_2016,
                                St8_Monat = mdl_einkuenfte_stufe8_monat,
                                St8_Jahr = mdl_einkuenfte_stufe8_jahr,
                                St8_2016 = mdl_einkuenfte_stufe8_2016,
                                St9_Monat = mdl_einkuenfte_stufe9_monat,
                                St9_Jahr = mdl_einkuenfte_stufe9_jahr,
                                St9_2016 = mdl_einkuenfte_stufe9_2016,
                                St10_Monat = mdl_einkuenfte_stufe10_monat,
                                St10_Jahr = mdl_einkuenfte_stufe10_jahr,
                                St10_2016 = mdl_einkuenfte_stufe10_2016)

mdl_einkuenfte_analyse$Summe <-
  mdl_einkuenfte_analyse[1:110,6] * 12 * 1000 +
  mdl_einkuenfte_analyse[1:110,7] * 1000 +
  mdl_einkuenfte_analyse[1:110,8] * 1000 +
  mdl_einkuenfte_analyse[1:110,9] * 12 * 3501 +
  mdl_einkuenfte_analyse[1:110,10] * 3501 +
  mdl_einkuenfte_analyse[1:110,11] * 3501 +
  mdl_einkuenfte_analyse[1:110,12] * 12 * 7001 +
  mdl_einkuenfte_analyse[1:110,13] * 7001 +
  mdl_einkuenfte_analyse[1:110,14] * 7001 +
  mdl_einkuenfte_analyse[1:110,15] * 12 * 15001 +
  mdl_einkuenfte_analyse[1:110,16] * 15001 +
  mdl_einkuenfte_analyse[1:110,17] * 15001 +
  mdl_einkuenfte_analyse[1:110,18] * 12 * 30001 +
  mdl_einkuenfte_analyse[1:110,19] * 30001 +
  mdl_einkuenfte_analyse[1:110,20] * 30001 +
  mdl_einkuenfte_analyse[1:110,21] * 12 * 50001 +
  mdl_einkuenfte_analyse[1:110,22] * 50001 +
  mdl_einkuenfte_analyse[1:110,23] * 50001 +
  mdl_einkuenfte_analyse[1:110,24] * 12 * 75001 +
  mdl_einkuenfte_analyse[1:110,25] * 75001 +
  mdl_einkuenfte_analyse[1:110,26] * 75001 +
  mdl_einkuenfte_analyse[1:110,27] * 12 * 100001 +
  mdl_einkuenfte_analyse[1:110,28] * 100001 +
  mdl_einkuenfte_analyse[1:110,29] * 100001 +
  mdl_einkuenfte_analyse[1:110,30] * 12 * 150001 +
  mdl_einkuenfte_analyse[1:110,31] * 150001 +
  mdl_einkuenfte_analyse[1:110,32] * 150001 +
  mdl_einkuenfte_analyse[1:110,33] * 12 * 250001 +
  mdl_einkuenfte_analyse[1:110,34] * 250001 +
  mdl_einkuenfte_analyse[1:110,35] * 250001


################################## Start Detail Analyse

library(magrittr)
library(ggplot2)
library(NLP)

mdl_einkuenfte_verdiener <- subset(mdl_einkuenfte_analyse, mdl_einkuenfte_analyse$Summe > 0)

mdl_einkuenfte_verdiener <- 
  data.frame(Name = mdl_einkuenfte_verdiener$Name, stringsAsFactors = FALSE) %>%
  cbind(Vorname = mdl_einkuenfte_verdiener$Vorname, Partei = mdl_einkuenfte_verdiener$Partei, Summe = mdl_einkuenfte_verdiener$Summe, stringsAsFactors = FALSE)

mdl_einkuenfte_verdiener <- mdl_einkuenfte_verdiener[order(mdl_einkuenfte_verdiener$Summe, decreasing = TRUE) , ]

mdl_gg_bar <- ggplot(mdl_einkuenfte_verdiener,
                     aes(x = reorder(Name, Summe), y = Summe, fill = Partei, group = Partei)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Summe), size = 4, hjust = -0.2) +
  scale_y_continuous(limits = c(0,80000)) +
  coord_flip() +
  theme_minimal() +
  ggtitle("Mindesteinkünfte der Landtagsabgeordneten", subtitle = "Im Jahr 2016") +
  xlab("") +
  ylab("Angaben in Euro") +
  scale_fill_manual(values = c("CDU" = "#000000",
                               "SPD" = "#ff0000",
                               "FDP" = "#ffff00",
                               "Linke" = "#ff00ff",
                               "Grüne" = "#00ff00"))

mdl_gg_bar

mdl_fraktionen <- summary(mdl_einkuenfte_analyse$Partei)
mdl_fraktionen_verdiener <- subset(mdl_einkuenfte_analyse, mdl_einkuenfte_analyse$Summe > 0)
mdl_fraktionen_verdiener_anzahl <- summary(mdl_fraktionen_verdiener$Partei)
mdl_fraktionen_anteil <- data.frame(Abgeordnete = mdl_fraktionen, stringsAsFactors = FALSE) %>%
  cbind(Verdiener = mdl_fraktionen_verdiener_anzahl)

mdl_fraktionen_parteinamen <- rownames(mdl_fraktionen_anteil)

mdl_fraktionen_anteil_prozent <- mdl_fraktionen_anteil$Verdiener / mdl_fraktionen_anteil$Abgeordnete *100
mdl_fraktionen_anteil_restprozent <- 100 - mdl_fraktionen_anteil_prozent

mdl_fraktionen_anteil <- cbind(mdl_fraktionen_anteil, Partei = mdl_fraktionen_parteinamen,
                               Restprozent = mdl_fraktionen_anteil_restprozent,
                               Prozent = mdl_fraktionen_anteil_prozent)

mdl_fraktionen_anteil$Abgeordnete <- NULL
mdl_fraktionen_anteil$Verdiener <- NULL

library(reshape2)

mdl_fraktionen_anteil <- melt(mdl_fraktionen_anteil, id.var="Partei")

mdl_gg_fraktionen <- ggplot(mdl_fraktionen_anteil,
                            aes(x = Partei, y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Anteil der Abgeordneten mit Nebeneinkünften in den Fraktionen", subtitle = "Im Jahr 2016") +
  xlab("") +
  ylab("Anteil in Prozent") +
  scale_fill_manual("",
                    values = c("Prozent" = "#0000ff",
                               "Restprozent" = "#cccccc"),
                    labels = c("ohne Nebeneinkünfte", "mit Nebeneinkünften"))

mdl_gg_fraktionen
