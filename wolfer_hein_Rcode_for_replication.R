## Start of script ----------------------------------------------------
# Analyses for the article "Konsequenzen der los-Suffigierung
# im Deutschen: Korpushäufigkeit, emotional-affektive Effekte
# und konstruktionsgrammatische Perspektiven" 
# DOI: https://doi.org/10.3726/zwjw.2022.02.03
# Sascha Wolfer, wolfer@ids-mannheim.de
#### --- #### --- #### --- #### --- #### --- #### --- #### --- #### ---

rm(list=ls())

library(data.table)
library(ggrepel)
library(readxl)
library(progress)
library(vroom)
library(dplyr)
library(parallel)
library(scales)

# Set your path here, data files have to be located here.
# Names and links for data files are included in comments were applicable.
setwd("< INSERT PATH HERE >")

# Preparations ------------------------------------------------------------

# Loading DeReKo hits
los.agg <- vroom("dereko_hits.csv")

# Filtering for -los, aggregating over lemma
los.agg %>% dplyr::filter(typ == "-los") %>%
  group_by(low.lemma) %>%
  summarize(freq = sum(freq)) %>%
  ungroup() %>%
  arrange(desc(freq)) -> los.agg

# Section 3.1: los-Ableitungen in DeReKo ----------------------------------

nrow(los.agg) # Number of lemma types
sum(los.agg$freq) # Token frequency (sum)

# Tab. 1: 20 most frequent lemmas in DeReKo
# (Table can be converted to a Word table pretty easily,
#  hence the weird output procedure)
for (i in 1:20) {
  cat(paste0(los.agg[i, "low.lemma"],
             ";",
             los.agg[i, "freq"]), "\n")
}
rm(i)

# Tab. 2: All -los items on BAWL that are present in DeReKo data
## Note: This dataset already contains the ratings from the BAWL-R which is
## available from https://osf.io/hx6r8/

los.bawl <- fread("los.in.BAWL.csv")
los.agg$bawl.label <- ifelse(los.agg$low.lemma %in% los.bawl$low.word,
                             as.character(los.agg$low.lemma), NA)
los.agg$in.bawl <- ifelse(!is.na(los.agg$bawl.label), "ja", "nein")

los.agg$rank <- 1:nrow(los.agg)
los.agg.bawl <- los.agg[los.agg$in.bawl == "ja",]

for (i in 1:nrow(los.agg.bawl)) {
  cat(paste0(los.agg.bawl[i, "low.lemma"],
             ";",
             los.agg.bawl[i, "freq"],
             ";",
             los.agg.bawl[i, "rank"]), "\n")
}
rm(i)

# Abstractness of base noun -----------------------------------------------
## Footnote 7 and associated text in Section 3.1

## Note: You have to download the ratings from
## https://www.ims.uni-stuttgart.de/forschung/ressourcen/experiment-daten/affective-norms/
## to include them in the dataset with the code below.

ssiw <- vroom("ratings_lrec16_koeper_ssiw.txt")

# Abstractness of all nouns
ssiw$cap <- substr(ssiw$Word, 1, 1) %in% toupper(letters)
ssiw.nouns <- ssiw[ssiw$cap,]
ssiw.nouns$lowWord <- tolower(ssiw.nouns$Word)
ssiw.nouns %>% group_by(lowWord) %>%
  summarize(AbstConc = mean(AbstConc)) %>%
  ungroup() -> ssiw.nouns

# Find base nouns in different variations
los.agg$Basis <- gsub("los$", "", los.agg$low.lemma)
los.agg$Basis.dS <- gsub("s$", "", los.agg$Basis)
los.agg$Basis.aE <- gsub("$", "e", los.agg$Basis)
los.agg$Basis.dEN <- gsub("en$", "", los.agg$Basis)
los.agg$Basis.dN <- gsub("n$", "", los.agg$Basis)
los.agg$Basis.dER <- gsub("er$", "", los.agg$Basis)

# Custom mappings
los.agg$Basis.custom <- ""
los.agg[los.agg$low.lemma == "männerlos", "Basis.custom"] <- "mann"
los.agg[los.agg$low.lemma == "ausnahmslos", "Basis.custom"] <- "ausnahme"
los.agg[los.agg$low.lemma == "töchterlos", "Basis.custom"] <- "tochter"
los.agg[los.agg$low.lemma == "bewußtlos", "Basis.custom"] <- "bewusst"
los.agg[los.agg$low.lemma == "kriterienlos", "Basis.custom"] <- "kriterium"
los.agg[los.agg$low.lemma == "teilnahmslos", "Basis.custom"] <- "teilnahme"
los.agg[los.agg$low.lemma == "bücherlos", "Basis.custom"] <- "buch"
los.agg[los.agg$low.lemma == "blätterlos", "Basis.custom"] <- "blatt"
los.agg[los.agg$low.lemma == "pferdelos", "Basis.custom"] <- "pferd"
los.agg[los.agg$low.lemma == "bewußtlos", "Basis.custom"] <- "bewusst"
los.agg[los.agg$low.lemma == "ständelos", "Basis.custom"] <- "stand"
los.agg[los.agg$low.lemma == "prinzipienlos", "Basis.custom"] <- "prinzip"
los.agg[los.agg$low.lemma == "leblos", "Basis.custom"] <- "leben"
los.agg[los.agg$low.lemma == "schadlos", "Basis.custom"] <- "schaden"
los.agg[los.agg$low.lemma == "punktelos", "Basis.custom"] <- "punkt"
los.agg[los.agg$low.lemma == "zeit-los", "Basis.custom"] <- "zeit"
los.agg[los.agg$low.lemma == "wertelos", "Basis.custom"] <- "wert"
los.agg[los.agg$low.lemma == "wegelos", "Basis.custom"] <- "weg"
los.agg[los.agg$low.lemma == "räderlos", "Basis.custom"] <- "rad"
los.agg[los.agg$low.lemma == "schutz-los", "Basis.custom"] <- "schutz"
los.agg[los.agg$low.lemma == "öffnungslos", "Basis.custom"] <- "öffnung"
los.agg[los.agg$low.lemma == "jahreslos", "Basis.custom"] <- "jahr"
los.agg[los.agg$low.lemma == "söhnelos", "Basis.custom"] <- "sohn"
los.agg[los.agg$low.lemma == "götterlos", "Basis.custom"] <- "gott"

# Map to ratings data (takes around 1 to 2 minutes on an M1 MacBook Pro)
t0 <- Sys.time()
los.agg$AbstConc <- apply(los.agg, 1, FUN = function (row) {
  res.raw <- ssiw.nouns[ssiw.nouns$lowWord %in% row["Basis"],]$AbstConc # raw base
  if (length(res.raw) != 0) {
    return(as.numeric(res.raw))
  } else {
    res.dS <- ssiw.nouns[ssiw.nouns$lowWord %in% row["Basis.dS"],]$AbstConc # deleted Fugen-s
    if (length(res.dS) != 0) {
      return(as.numeric(res.dS))
    } else {
      res.aE <- ssiw.nouns[ssiw.nouns$lowWord %in% row["Basis.aE"],]$AbstConc # additional Fugen-e
      if (length(res.aE) != 0) {
        return(as.numeric(res.aE))
      } else {
        res.dEN <- ssiw.nouns[ssiw.nouns$lowWord %in% row["Basis.dEN"],]$AbstConc # deleted Fugen-en
        if (length(res.dEN) != 0) {
          return(as.numeric(res.dEN))
        } else {
          res.dN <- ssiw.nouns[ssiw.nouns$lowWord %in% row["Basis.dN"],]$AbstConc # deleted Fugen-n
          if (length(res.dN) != 0) {
            return(as.numeric(res.dN))
          } else {
            res.dER <- ssiw.nouns[ssiw.nouns$lowWord %in% row["Basis.dER"],]$AbstConc # deleted Fugen-er
            if (length(res.dER) != 0) {
              return(as.numeric(res.dER))
            } else {
              res.custom <- ssiw.nouns[ssiw.nouns$lowWord %in% row["Basis.custom"],]$AbstConc # custom replacements (e.g., Umlautung)
              if (length(res.custom) != 0) {
                return(as.numeric(res.custom))
              } else {
                NA
              }
            }
          }
        }
      }
    }
  }
})
difftime(Sys.time(), t0)

table(is.na(los.agg$AbstConc))

# Abstractness all nouns vs. found base nouns
# Footnote 7 in the paper
t.test(ssiw.nouns$AbstConc, los.agg$AbstConc, var.equal = F)
median(ssiw.nouns$AbstConc)
median(los.agg$AbstConc, na.rm = T)
wilcox.test(ssiw.nouns$AbstConc, los.agg$AbstConc, conf.int = T)

# Permutation test (not reported in the paper)
# You can read about permutation tests in corpus-linguistic research here:
# https://doi.org/10.1371/journal.pone.0222703
## creating a dataset with both datasources
perm.dat <- data.frame(word = c(ssiw.nouns$lowWord, los.agg$low.lemma),
                       AbstConc = c(ssiw.nouns$AbstConc, los.agg$AbstConc),
                       src = c(rep("ssiw", nrow(ssiw.nouns)), rep("los", nrow(los.agg))))
perm.dat <- perm.dat[!is.na(perm.dat$AbstConc),]
obs.mean.diff <- diff(tapply(perm.dat$AbstConc, perm.dat$src, mean))

# Doing permutations
perm.diffs <- mclapply(1:10000, mc.cores = 6, FUN = function (i) {
  diff(tapply(perm.dat$AbstConc, sample(perm.dat$src), mean))
})
perm.diffs <- do.call("c", perm.diffs)
table(abs(perm.diffs) >= obs.mean.diff)

### Result of permutation test:
## - For 10000 permutations, there is no difference for a permuted dataset that is higher than
##   the observed difference. Therefore, we have to assume that there is a indeed a 'non-random' difference
##   between the two groups.

# Section 3.3,  Figure 1 --------------------------------------------------
los.agg$bee.label <- ifelse(los.agg$low.lemma %in% c("kostenlos", "arbeitslos",
                                                     "hilflos", "mondlos"),
                            los.agg$low.lemma, NA)

ggplot(los.agg, aes(x = 1, y = freq, col = in.bawl, label = bee.label)) +
  scale_y_log10(labels = comma) +
  scale_color_manual(values = c(ja = "blue", nein = "grey50")) +
  scale_x_continuous(breaks = NULL) +
  ggbeeswarm::geom_beeswarm(size = 2) +
  geom_label(nudge_x = .15) +
  labs(x = "", y = "DeReKo-Frequenz", col = "In BAWL",
       subtitle = paste0("n = ", nrow(los.agg),
                         ", davon auf BAWL: ", sum(los.agg$in.bawl == "ja"))) +
  theme_minimal()
ggsave("Figure 1.pdf", width = 10, height = 10)

# Section 4.1 -------------------------------------------------------------

## This dataset contains all -los-adjectives with associated nouns
subadj <- as.data.frame(fread("/Users/sascha/Nextcloud/R-Scripts/los/los.BAWL.with.Noun.csv"))

# Preparing Figure 2
noun.labs.repel <- c("Mut", "Lust", "Hilfe", "Kraft", "Ton", "Wert")
adj.labs.repel <- NA

subadj$noun.lab <- ifelse(subadj$noun %in% noun.labs.repel, NA, subadj$noun)
subadj$noun.lab.rpl <- ifelse(subadj$noun %in% noun.labs.repel, subadj$noun, NA)

subadj$adj.lab <- ifelse(subadj$adj %in% adj.labs.repel, NA, subadj$adj)
subadj$adj.lab.rpl <- ifelse(subadj$adj %in% adj.labs.repel, subadj$adj, NA)

# Plotting Figure 2
ggplot(data = subadj) +
  geom_vline(xintercept = 0, lty = "dotted", alpha = .5) +
  geom_segment(aes(x = emo.mean.noun, y = arousal.mean.noun, xend = emo.mean.adj, yend = arousal.mean.adj),
               alpha = .3) +
  geom_label(aes(x = emo.mean.noun, y = arousal.mean.noun, label = noun.lab, col = "Substantiv"), size = 3, alpha = .5) + # non-repelled labs
  geom_label(aes(x = emo.mean.adj, y = arousal.mean.adj, label = adj.lab, col = "Adjektiv"), size = 3, alpha = .5) +
  geom_label_repel(aes(x = emo.mean.noun, y = arousal.mean.noun, label = noun.lab.rpl, col = "Substantiv"), size = 3, min.segment.length = 0) + # repelled labs
  geom_label_repel(aes(x = emo.mean.adj, y = arousal.mean.adj, label = adj.lab.rpl, col = "Adjektiv"), size = 3, min.segment.length = 0) +
  scale_color_manual(values = c(Substantiv = "red", Adjektiv = "blue")) +
  theme_minimal() +
  labs(x = "Emotionale Valenz", y = "Arousal", color = "Wortart")
ggsave("Figure2.pdf", width = 10, height = 8)

## Changed signs emotional valence
subadj$val.signch <- (subadj$emo.mean.adj < 0 & subadj$emo.mean.noun > 0) | (subadj$emo.mean.adj > 0 & subadj$emo.mean.noun < 0)
subadj[!subadj$val.signch,] # No sign changes

## Valence difference in relation to valence strength
subadj$abs.val.chg <- abs(subadj$emo.mean.adj - subadj$emo.mean.noun)
ct <- cor.test(abs(subadj$emo.mean.noun), subadj$abs.val.chg, method = "spearman")

### Permutation test for correlation value
perm.cors <- mclapply(1:10000, mc.cores = 6, FUN = function (i) {
  cor(abs(subadj$emo.mean.noun), sample(subadj$abs.val.chg), method = "spearman")
})
perm.cors <- do.call("c", perm.cors)
table(abs(perm.cors) >= ct$estimate)
### Result of permutation test:
## - For 10000 permutations, there is no correlation for a permuted dataset that is higher than
##   the observed correlation Therefore, we have to assume that there is a indeed a 'non-random' correlation.

subadj$noun.val.sgn <- ifelse(subadj$emo.mean.noun > 0, "positiv", "negativ")

# Figure 3
ggplot(subadj, aes(x = abs(emo.mean.noun), y = abs.val.chg, col = noun.val.sgn)) +
  geom_point() +
  geom_smooth(inherit.aes = F, aes(x = abs(emo.mean.noun), y = abs.val.chg),
              method = "lm", se = F, col = "grey") +
  geom_label_repel(aes(label = noun)) +
  annotate("text", x = Inf, y = -Inf, label = 'r = 0,86; p < 0,0001', vjust = -1, hjust = 1, size = 6) +
  scale_color_manual(values = c(positiv = "blue", negativ = "red")) +
  labs(x = "Betrag emotionale Valenz Substantiv", y = "Betrag Valenzdifferenz Substantiv - Adjektiv",
       col = "Valenzpolarität\nBasissubstantiv") +
  theme_minimal()
ggsave("Figure3.pdf", width = 10, height = 6)

# Parameters for regression line in Figure 3 (see Figure 3's caption)
summary(lm(abs.val.chg ~ abs(emo.mean.noun), data = subadj))

# Section 4.2 -------------------------------------------------------------

# Footnote 17
t.test(subadj$arousal.mean.noun, subadj$arousal.mean.adj, paired = T, alternative = "greater")
subadj$arousal.diff <- subadj$arousal.mean.adj - subadj$arousal.mean.noun
wilcox.test(subadj$arousal.mean.noun, subadj$arousal.mean.adj, paired = T, conf.int = T)

# Permutation test
## Setting up dataset
arou.permdat <- data.frame(Arousal = c(subadj$arousal.mean.noun,
                                       subadj$arousal.mean.adj),
                           Group = rep(c("Noun", "Adjective"), each = nrow(subadj)))
obs.diff <- diff(tapply(arou.permdat$Arousal, arou.permdat$Group, mean))

## Doing permutations
perm.diffs <- mclapply(1:50000, mc.cores = 6, FUN = function (i) {
  diff(tapply(arou.permdat$Arousal, sample(arou.permdat$Group), mean))
})
perm.diffs <- do.call("c", perm.diffs)
prop.table(table(abs(perm.diffs) >= obs.mean.diff)) * 100
prop.table(table(perm.diffs >= obs.mean.diff)) * 100
plot(density(perm.diffs))
abline(v = obs.diff, col = "grey")
### Result for permutation test:
## With 50,000 permutations, approx. 7.8% of all absolute differences for permuted datasets
## are larger than the observed absolute difference. One-sided test: 3.9%.
## This roughly corresponds to the figures reported in Footnote 17.

table(subadj$arousal.diff < 0)
subadj[subadj$arousal.diff > 0, "noun"]

# Bayesian analysis for difference (also footnote 17)
library(BEST)

priors <- list(muM = mean(bawl$arousal.mean), muSD = mean(bawl$arousal.std))
BESTout <- BESTmcmc(subadj$arousal.mean.noun, subadj$arousal.mean.adj, priors = priors, parallel = T)
plot(BESTout)

# Plotting Figure 4
bpdf <- data.frame(Wortart = rep(c("Substantive", "Adjektive"), each = nrow(subadj)),
                   mean.Arousal = c(subadj$arousal.mean.noun, subadj$arousal.mean.adj))
ggplot(bpdf, aes(x = Wortart, y = mean.Arousal)) +
  geom_boxplot() +
  ggbeeswarm::geom_beeswarm(size = 2, col = "blue") +
  geom_point(stat = "summary", size = 10, col = "red", alpha = .5) +
  coord_flip() +
  labs(x = "", y = "Arousal") +
  theme_minimal() + theme(axis.text = element_text(size = 16),
                          axis.title = element_text(size = 16))
ggsave("Figure 4.pdf", width = 10, height = 4)

# Digression: https://link.springer.com/article/10.3758/s13415-011-0026-1 (Herbert et al., 2011) say:
# "negation decreased arousal ratings for unpleasant nouns and reversed the valence ratings for pleasant nouns"

subadj$is.negative.noun <- subadj$emo.mean.noun < 0
tapply(subadj$arousal.diff, subadj$is.negative.noun, mean)
t.test(arousal.diff ~ is.negative.noun, data = subadj)
wilcox.test(subadj[subadj$is.negative.noun,]$arousal.diff, subadj[!subadj$is.negative.noun,]$arousal.diff)
# Indeed: Arousal is decreased more for negative nouns. But we only have 8 negative Nouns on the list.
# So that might be a random effect.

# Are adjectives generally associated with lower arousal than nouns? (see Footnote 18)
## To replicate this, get the full BAWL dataset from https://osf.io/hx6r8/
bawl <- read_excel("BAWL-R.xlsx")
t.test(bawl[bawl$WORD_CLASS == "A", "AROUSAL_MEAN"], bawl[bawl$WORD_CLASS == "N", "AROUSAL_MEAN"], var.equal = T)

# Permutation test equivalent for the t-test above.

## Creating dataset
perm.dat <- data.frame(arousal = c(bawl[bawl$WORD_CLASS == "A",]$AROUSAL_MEAN,
                                   bawl[bawl$WORD_CLASS == "N",]$AROUSAL_MEAN),
                       group = c(rep("A", nrow(bawl[bawl$WORD_CLASS == "A",])),
                                 rep("N", nrow(bawl[bawl$WORD_CLASS == "N",]))))
obs.diff <- diff(tapply(perm.dat$arousal, perm.dat$group, mean))

perm.diffs <- mclapply(1:10000, mc.cores = 6, FUN = function (x) {
  diff(tapply(perm.dat$arousal, sample(perm.dat$group), mean))
})
perm.diffs <- do.call("c", perm.diffs)
prop.table(table(abs(perm.diffs) > obs.diff))
# Permutation test results: approx. 30% of differences in permuted datasets
# are larger than the observed difference. Observed difference might have been
# observed by chance.
