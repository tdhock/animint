works_with_R("3.0.2", reshape2="1.2.2")

samp.f <- "Vervet_intestinal_samples.csv"
samp.info <- read.csv(samp.f, skip=1, nrows=23, check.names=FALSE,
                      na.strings="")
names(samp.info) <- tolower(names(samp.info))
samp.info$monkey <- samp.info$wfid
samp.info$years <- samp.info$age

f <- "V13A-Matrix-55mer-SixtyFour-Nov21-2013-FilesWithHostDNA.csv"
## The first few rows describe the samples. There are several samples
## per monkey, and some samples were re-sequenced.
sample.transpose <- read.table(f, nrow=15, sep=",", colClasses="character")
sample.char <- t(sample.transpose)
sample.df <- as.data.frame(sample.char[-(1:2),])
names(sample.df) <- sample.char[1,]
write.csv(sample.df, "samples.csv", row.names=FALSE)
samples <- read.csv("samples.csv", check.names=FALSE)
pattern <- paste0("(?<monkey>[0-9]+)",
                  "(?<resequenced>[^/]+)?",
                  "/",
                  "(?<location_code>[A-Za-z]+)")
match <- str_match_perl(as.character(samples$Org), pattern)
## Nikoleta wrote "The samples highlighted in pink have a high level
## of contamination with host DNA, so best to exclude them."
pink.samples <- c("1588/Ti", "1588/Ei", "1588/Du", "1232/Ti", "1232/Tc",
                  "1232/Si", "1232/Sc", "1109/Ti", "1101/Ei")
samples$contamination <-
  ifelse(samples$Org %in% pink.samples, "hostDNA", "none")
stopifnot(sum(samples$contamination=="hostDNA") == length(pink.samples))
samples$run <- samples[["Run#"]]
samples$monkey <- as.integer(match[,"monkey"])
samples$experiment <-
  factor(ifelse(match[,"resequenced"]=="April", "reseqApril", "sequenced"))
location.codes <-
  c(Re="rectum",
    Tc="transverse colon",
    Ce="cecum",
    Ti="terminal ileum",
    Sc="sigmoid colon",
    Du="duodenum",
    SigCol="sigmoid colon",
    Rectum="rectum",
    Si="small intestine",
    Ei="small intestine", # TODO: verify with Nikoleta!
    Cecum="cecum",
    TransCol="transverse colon")
location.order <- c("duodenum",
                    "small intestine",
                    "terminal ileum",
                    "cecum",
                    "transverse colon",
                    "sigmoid colon",
                    "rectum")
samples$location <-
  factor(location.codes[match[,"location_code"]], rev(location.order))

## The last rows are the count data of the 55-mers found.
count.df <- read.table(f, skip=16, sep=",", row.names=2)
stopifnot(nchar(as.character(count.df[,1]))==55)
names(count.df)[1] <- "55mer"
count.mat.t <- as.matrix(count.df[,-1])
stopifnot(ncol(count.mat.t)==nrow(samples))
count.mat <- t(count.mat.t)
names(dimnames(count.mat)) <- c("sample", "id55mer")
rownames(count.mat) <- 1:nrow(count.mat)
prop.mat <- count.mat/apply(count.mat, 1, sum)
stopifnot(apply(prop.mat, 1, sum) == 1)
id.cols <- c("monkey", "location", "experiment", "run", "contamination",
             "Total Words(55-mers)", "Total Frequency of Words 55-mers")
sample.counts <- data.frame(samples[,id.cols], prop.mat, check.names=FALSE)
counts <- melt(sample.counts, id=id.cols,
               value.name="reads", variable.name="id55mer")
## Some 55mer-specific info...
vervet <- list(samples=samples,
               counts=counts,
               monkeys=samp.info)
save(vervet, file="vervet.RData")
