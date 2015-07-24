\name{vervet}
\alias{vervet}
\docType{data}
\title{
Vervet monkey intestinal microbiome
}
\description{
DNA sequences (55mers) of bacteria in the gastrointestinal tract of 23
vervet (African green) monkeys from Wake Forest University Primate
Center Vervet Research Colony. Some monkeys were measured in several
locations, so there are a total of 64 samples. The goal in analyzing
these data was to determine whether or not similar bacteria are detected
in nearby locations, assuming that each 55mer uniquely identifies a
bacterium.
}
\usage{data(vervet)}
\format{
  The format is a named list of data.frames: samples contains 64 rows
  with sample-specific info, counts contains 1190208 rows with counts of
  55mers observed in each sample, and monkeys contains 23 rows with
  monkey-specific information.
}
\source{
  DNA sequencing was funded by Genome Quebec and generated at the McGill
  Innovation Center. More info about the Vervet Microbiome project
  http://www.genomequebec.mcgill.ca/compgen/vervet_research/ 
  http://genome.wustl.edu/projects/detail/vervet-microbiome/
}
\keyword{datasets}
