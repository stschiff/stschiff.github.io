---
title: Resources
---

# Data
...

# Teaching Material

-   May 2019: Six-hour practical course on \"Computational Population
    Genetics\" at the University of Jena, Germany,
    see <https://github.com/stschiff/popgen_course>
-   January 22-24, 2019: Workshop on \"Computational Population
    Genetics\" at the MPI-SHH in Jena, Germany,
    see <https://comppopgenworkshop2019.readthedocs.io/en/latest/>
-   November 2018: Workshop on \"Computational Methods to Analyse Human
    Genetic Variation\", see
     <https://compvar-workshop.readthedocs.io/en/latest/>
-   May 2016: Workshop on Analysis of Genomic data. See
    <https://gaworkshop.readthedocs.io/en/latest/>

# Software

All of my software can be found on github. For discussions around MSMC,
we have a [google
group](https://groups.google.com/forum/#!forum/msmc-popgen) that you can
participate in. Most of my recent software is written
in [Haskell](https://www.haskell.org).

### [MSMC](http://www.github.com/stschiff/msmc)

A method for analysing multiple genome sequences to infer past
population sizes and separation history between populations. A utilities
framework [msmc-tools](http://www.github.com/stschiff/msmc-tools) is
available as well. Written in [D](https://dlang.org) and
[python](https://www.python.org).

### [MSMC2](http://www.github.com/stschiff/msmc2)

A modified version of MSMC that is still under development. For
analysing population size history, MSMC2 is recommended over MSMC. For
cross-population analyses I would suggest to wait, since we are still
dealing with some issues. A utilities framework
[msmc-tools](http://www.github.com/stschiff/msmc-tools) is available as
well. Written in [D](https://dlang.org) and
[python](https://www.python.org).

### [Rarecoal](http://www.github.com/stschiff/rarecoal)

A method to model population graphs from the rare site frequency
spectrum in multiple populations with large numbers of samples. A
utilities package
[rarecoal-tools](https://github.com/stschiff/rarecoal-tools) is
available as well. Written in [Haskell](https://www.haskell.org).

### [Tman](http://www.github.com/stschiff/tman)

This is a tool for running and monitoring large numbers of jobs on
Cluster environments. The tool is still in alpha-phase. Written in
[Haskell](https://www.haskell.org).

### [SequenceTools](http://www.github.com/stschiff/sequenceTools)

This package contains two programs \"pileupCaller\" and
\"vcf2eigenstrat\". PileupCaller is used to sample alleles from an
alignment file at a set of positions. VCF2eigenstrat converts a VCF file
into eigenstrat format. Written in [Haskell](https://www.haskell.org).

### [MergeAndClip](https://github.com/stschiff/mergeAndClipFastq)

A program to process raw paired-end sequencing data from ancient DNA
libraries. Written in [Haskell](https://www.haskell.org).
