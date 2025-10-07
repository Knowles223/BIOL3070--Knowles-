Warm-up mini-Report: Mosquito Blood Hosts in Salt Lake City, Utah
================
Hailey Knowles
2025-09-25

- [ABSTRACT](#abstract)
- [BACKGROUND](#background)
- [STUDY QUESTION and HYPOTHESIS](#study-question-and-hypothesis)
  - [Questions](#questions)
  - [Hypothesis](#hypothesis)
  - [Prediction](#prediction)
- [METHODS](#methods)
  - [Fill in first analysis](#fill-in-first-analysis)
- [DISCUSSION](#discussion)
  - [Interpretation - fill in
    analysis](#interpretation---fill-in-analysis)
  - [Interpretation - fill in
    analysis/plot](#interpretation---fill-in-analysisplot)
- [CONCLUSION](#conclusion)
- [REFERENCES](#references)

# ABSTRACT

Fill in abstract at the end after we have finished the methods, results,
discussion, conclusions and know what our data “says”.

# BACKGROUND

Fill in some text here that provides background info on the WNV system,
the blood meal DNA extractions, PCR, sequencing, etc. and the foundation
for our question/hypothesis. For example, we can use the viremia
duration (Kumar et al., 2003) bar plot (make sure to reference
sources!!!) to illustrate the potential importance of house finches in
WNV transmission and as the logical foundation for our hypothesis that
house finches serve as amplifying hosts for WNV… and the prediction that
locations with more house finches in our blood host analysis are also
the same locations with higher positive tests for WNV in mosquito pools…

This data was collected from traps located at various locations
throughout Salt Lake City. Some of these samples that were collected
included some traces and strains of West Nile Virus. The mosquitoes were
collected and then ran through the process of PCR to determine where
they got their blood meal from. Each mosquito was smashed and ran
through PCR. PCR is a way to amplify small amounts of DNA so it can be
BLASTn to determine what kind of organism the DNA belongs to. The
sequence that was amplified was the COi gene. This gene is found in most
organisms and is used to identify organisms. The DNA that was amplified
was then ran through BLASTn to determine what types of species were fed
on by the various mosquitoes in the various locations across SLC
(Viremia data citation). It was found in other studies that house
finches may act as a amplifying host for WNV and that the competetnce of
the house finch was very high in relation to 67 different bird species
that were also studied. (Taieb et al., 2020)

NOTE: Examples of data you can plot for the background info at
<https://github.com/saarman/BIOL3070/>

``` r
## import counts_matrix: data.frame with column 'loc_positives' (0/1) and host columns 'host_*'
counts_matrix <- read.csv("./bloodmeal_plusWNV_for_BIOL3070.csv")

## 1) Identify host columns
host_cols <- grep("^host_", names(counts_matrix), value = TRUE)

if (length(host_cols) == 0) {
  stop("No columns matching '^host_' were found in counts_matrix.")
}

## 2) Ensure loc_positives is present and has both levels 0 and 1 where possible
counts_matrix$loc_positives <- factor(counts_matrix$loc_positives, levels = c(0, 1))

## 3) Aggregate host counts by loc_positives
agg <- stats::aggregate(
  counts_matrix[, host_cols, drop = FALSE],
  by = list(loc_positives = counts_matrix$loc_positives),
  FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
)

## make sure both rows exist; if one is missing, add a zero row
need_levels <- setdiff(levels(counts_matrix$loc_positives), as.character(agg$loc_positives))
if (length(need_levels)) {
  zero_row <- as.list(rep(0, length(host_cols)))
  names(zero_row) <- host_cols
  for (lv in need_levels) {
    agg <- rbind(agg, c(lv, zero_row))
  }
  ## restore proper type
  agg$loc_positives <- factor(agg$loc_positives, levels = c("0","1"))
  ## coerce numeric host cols (they may have become character after rbind)
  for (hc in host_cols) agg[[hc]] <- as.numeric(agg[[hc]])
  agg <- agg[order(agg$loc_positives), , drop = FALSE]
}

## 4) Decide species order (overall abundance, descending)
overall <- colSums(agg[, host_cols, drop = FALSE], na.rm = TRUE)
host_order <- names(sort(overall, decreasing = TRUE))
species_labels <- rev(sub("^host_", "", host_order))  # nicer labels

## 5) Build count vectors for each panel in the SAME order
counts0 <- rev(as.numeric(agg[agg$loc_positives == 0, host_order, drop = TRUE]))
counts1 <- rev(as.numeric(agg[agg$loc_positives == 1, host_order, drop = TRUE]))

## 6) Colors: reuse your existing 'cols' if it exists and is long enough; otherwise generate
if (exists("cols") && length(cols) >= length(host_order)) {
  species_colors <- setNames(cols[seq_along(host_order)], species_labels)
} else {
  species_colors <- setNames(rainbow(length(host_order) + 10)[seq_along(host_order)], species_labels)
}

## 7) Shared x-limit for comparability
xmax <- max(c(counts0, counts1), na.rm = TRUE)
xmax <- if (is.finite(xmax)) xmax else 1
xlim_use <- c(0, xmax * 1.08)

## 8) Plot: two horizontal barplots with identical order and colors
op <- par(mfrow = c(1, 2),
          mar = c(4, 12, 3, 2),  # big left margin for species names
          xaxs = "i")           # a bit tighter axis padding

## Panel A: No WNV detected (loc_positives = 0)
barplot(height = counts0,
        names.arg = species_labels, 
        cex.names = .5,
        cex.axis = .5,
        col = rev(unname(species_colors[species_labels])),
        horiz = TRUE,
        las = 1,
        xlab = "Bloodmeal counts",
        main = "Locations WNV (-)",
        xlim = xlim_use)

## Panel B: WNV detected (loc_positives = 1)
barplot(height = counts1,
        names.arg = species_labels, 
        cex.names = .5,
        cex.axis = .5,
        col = rev(unname(species_colors[species_labels])),
        horiz = TRUE,
        las = 1,
        xlab = "Bloodmeal counts",
        main = "Locations WNV (+)",
        xlim = xlim_use)
```

<img src="Warm-up-mosquitoes-TEMPLATE_files/figure-gfm/horiz-plot-1.png" style="display: block; margin: auto auto auto 0;" />

``` r
par(op)

## Keep the colors mapping for reuse elsewhere
host_species_colors <- species_colors
```

``` r
# Manually transcribe duration (mean, lo, hi) from the last table column
duration <- data.frame(
  Bird = c("Canada Goose","Mallard", 
           "American Kestrel","Northern Bobwhite",
           "Japanese Quail","Ring-necked Pheasant",
           "American Coot","Killdeer",
           "Ring-billed Gull","Mourning Dove",
           "Rock Dove","Monk Parakeet",
           "Budgerigar","Great Horned Owl",
           "Northern Flicker","Blue Jay",
           "Black-billed Magpie","American Crow",
           "Fish Crow","American Robin",
           "European Starling","Red-winged Blackbird",
           "Common Grackle","House Finch","House Sparrow"),
  mean = c(4.0,4.0,4.5,4.0,1.3,3.7,4.0,4.5,5.5,3.7,3.2,2.7,1.7,6.0,4.0,
           4.0,5.0,3.8,5.0,4.5,3.2,3.0,3.3,6.0,4.5),
  lo   = c(3,4,4,3,0,3,4,4,4,3,3,1,0,6,3,
           3,5,3,4,4,3,3,3,5,2),
  hi   = c(5,4,5,5,4,4,4,5,7,4,4,4,4,6,5,
           5,5,5,7,5,4,3,4,7,6)
)

# Choose some colors
cols <- c(rainbow(30)[c(10:29,1:5)])  # rainbow colors

# horizontal barplot
par(mar=c(5,12,2,2))  # wider left margin for names
bp <- barplot(duration$mean, horiz=TRUE, names.arg=duration$Bird,
              las=1, col=cols, xlab="Days of detectable viremia", xlim=c(0,7))

# add error bars
arrows(duration$lo, bp, duration$hi, bp,
       angle=90, code=3, length=0.05, col="black", xpd=TRUE)
```

<img src="Warm-up-mosquitoes-TEMPLATE_files/figure-gfm/viremia-1.png" style="display: block; margin: auto auto auto 0;" />

# STUDY QUESTION and HYPOTHESIS

## Questions

Fill in here, the question we want to answer… e.g. What bird species is
acting as WNV amplifying host in Salt Lake City?

During this study we were hoping to discover what types of birds are
more likely to conduct the WNV in the Salt Lake City area?

## Hypothesis

Fill in hypothesis… e.g. House finches are acting as important
amplifying hosts of WNV in Salt Lake City.

We hypothesize that based on data collected that the house finch will be
the main host for the WNV from samples collected within the Salt Lake
City area.

## Prediction

Fill in prediction… e.g. If house finches are acting as important
amplifying hosts, we predict that trapping locations where mosquitoes
feed on house finches will also have higher rates of confirmed WNV in
tested mosquito pools.

If if our hypothesis is true then we will predict that the areas with
the highest count of WNV will be in areas that also have the highest
number of house finches.

# METHODS

Fill in here, including overview of procedure and methods used for this
project.

1.  Samples of mosquitoes were collected from the traps set at various
    locations across the Salt Lake City area,
2.  The mosquito samples were then sent to our class where we ran PCR to
    amplify the DNA strands that had the COi gene to determine where the
    mosquito’s blood meal came from.
3.  These are the steps of the PCR ran in class and in the lab

<!-- -->

1.  The first step I observed in the demo involved placing the master
    mix into the vials to be run through the PCR process. We also have
    positive and negative samples. The positive sample contains DNA with
    a known sequence, allowing us to compare it with the other samples
    we run. The negative sample is typically just water and contains no
    DNA, allowing us to ensure there were no contaminants. The next step
    was adding the DNA samples to the master mix.
2.  The next couple of steps are run in a lab, which we weren’t able to
    see in class. These steps include denaturation, which occurs when
    the DNA is separated into single strands. Annealing, which is when
    the primers are inserted, this targets the specific sequence.
    Extension, where the polymerase adds new nucleotides to the target
    strands, creating new strands of DNA.
3.  The reagents used are template DNA, which is the sample inserted
    that we aim to replicate. Primers are used to mark where the copying
    should start. The following reagent would be loose nucleotides,
    which is what allow the new strands to be made. Some enzymes are
    also used to help build the new DNA strands.

<!-- -->

4.  After the PCR was successfully ran it was put into A’s, T’s, C’s and
    G’s. These sequences for each sample was then ran in BLASTn. The
    types of organisms that the bloodmeals came from were inserted into
    the datasheet.
5.  The final step was analyzing the data to determine if the house
    finch acted as the main host for WNV in the Salt Lake City area.
6.  what types of coding analysis we ran…………………………………….

## Fill in first analysis


    ## Fill in second analysis/plot





    ``` r
    #glm with house finch alone against binary +/_
    glm1 <- glm(loc_positives ~ host_House_finch,
                data = counts_matrix,
                family = binomial)
    summary(glm1)

    ## 
    ## Call:
    ## glm(formula = loc_positives ~ host_House_finch, family = binomial, 
    ##     data = counts_matrix)
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)       -0.1709     0.1053  -1.622   0.1047  
    ## host_House_finch   0.3468     0.1586   2.187   0.0287 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 546.67  on 394  degrees of freedom
    ## Residual deviance: 539.69  on 393  degrees of freedom
    ## AIC: 543.69
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
#glm with house-finch alone against positivity rate
glm2 <- glm(loc_rate ~ host_House_finch,
            data = counts_matrix)
summary(glm2)
```

    ## 
    ## Call:
    ## glm(formula = loc_rate ~ host_House_finch, data = counts_matrix)
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      0.054861   0.006755   8.122 6.07e-15 ***
    ## host_House_finch 0.027479   0.006662   4.125 4.54e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.01689032)
    ## 
    ##     Null deviance: 6.8915  on 392  degrees of freedom
    ## Residual deviance: 6.6041  on 391  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: -484.56
    ## 
    ## Number of Fisher Scoring iterations: 2

# DISCUSSION

## Interpretation - fill in analysis

## Interpretation - fill in analysis/plot

# CONCLUSION

# REFERENCES

1.  Komar N, Langevin S, Hinten S, Nemeth N, Edwards E, Hettler D, Davis
    B, Bowen R, Bunning M. Experimental infection of North American
    birds with the New York 1999 strain of West Nile virus. Emerg Infect
    Dis. 2003 Mar;9(3):311-22. <https://doi.org/10.3201/eid0903.020628>

2.  ChatGPT. OpenAI, version Jan 2025. Used as a reference for functions
    such as plot() and to correct syntax errors. Accessed 2025-09-25.

3.  Taieb, L., Ludwig, A., Ogden, N. H., Lindsay, R. L., Iranpour, M.,
    Gagnon, C. A., & Bicout, D. J. (2020). Bird Species Involved in West
    Nile Virus Epidemiological Cycle in Southern Québec. International
    journal of environmental research and public health, 17(12), 4517.
    <https://doi.org/10.3390/ijerph17124517>
