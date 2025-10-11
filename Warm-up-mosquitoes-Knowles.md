Warm-up mini-Report: Mosquito Blood Hosts in Salt Lake City, Utah
================
Hailey Knowles
2025-10-11

- [ABSTRACT](#abstract)
- [BACKGROUND](#background)
- [STUDY QUESTION and HYPOTHESIS](#study-question-and-hypothesis)
  - [Question](#question)
  - [Hypothesis](#hypothesis)
  - [Prediction](#prediction)
- [METHODS](#methods)
  - [1st Analysis - Barplot](#1st-analysis---barplot)
  - [2nd Analysis - Generalized Linear
    Model](#2nd-analysis---generalized-linear-model)
- [DISCUSSION](#discussion)
  - [Interpretation of 1st Analysis
    (Barplot)](#interpretation-of-1st-analysis-barplot)
  - [Interpretation of 2nd Analysis (Generalized Linear
    Model)](#interpretation-of-2nd-analysis-generalized-linear-model)
- [CONCLUSION](#conclusion)
- [REFERENCES](#references)

# ABSTRACT

There are many fears around getting WNV and how it is transmitted. This
study is aiming to look at what types of bird species that are more
likely to act as amplifying hosts for WNV and whether or not these
species have a larger population in the areas where WNV is most
prevalent. It is hypothesized that House Finches will act as amplifying
hosts for WNV and that areas with a higher number of WNV will also have
a larger population of House Finches. Data for this experiment was
collected by collecting mosquitoes from various spots around the Salt
Lake City area. These samples were then tested to determine the blood
meal host and also whether it was positive or negative for WNV. Based
upon the results of the study, our findings showed, that it is proven
that the areas with higher number of WNV cases also has a larger number
of House Finches being used as blood meals for the mosquitoes. The
statistical analyses run also show support for these results.

# BACKGROUND

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
that were also studied. (Taieb et al., 2020) In the bar plot made in
class (found below) it was visually detectable that House Finches and
Great Horned Owls tend to have Viremia for the most amount of days.

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

<img src="Warm-up-mosquitoes-Knowles_files/figure-gfm/viremia-1.png" style="display: block; margin: auto auto auto 0;" />

# STUDY QUESTION and HYPOTHESIS

## Question

During this study we were seeking to discover what types of birds are
more likely to conduct the WNV in the Salt Lake City area?

## Hypothesis

We hypothesize that based on data collected, that the house finch will
be the main host for the WNV from samples collected within the Salt Lake
City area.

## Prediction

If the hypothesis is proven to be true then we will find that the areas
with the highest count of WNV will be in areas that also have the
highest number of House Finches.

# METHODS

Samples of mosquitoes were collected from the traps set at various
locations across the Salt Lake City area. The mosquito samples were then
sent to our class where we ran PCR to amplify the DNA strands that had
the COi gene to determine where the mosquito’s blood meal came from.
These are the steps of the PCR ran in class and in the lab. The first
step I observed in the demo involved placing the master mix into the
vials to be run through the PCR process. We also have positive and
negative samples. The positive sample contains DNA with a known
sequence, allowing us to compare it with the other samples we run. The
negative sample is typically just water and contains no DNA, allowing us
to ensure there were no contaminants. The next step was adding the DNA
samples to the master mix. The next couple of steps are run in a lab,
which we weren’t able to see in class. These steps include denaturation,
which occurs when the DNA is separated into single strands. Annealing,
which is when the primers are inserted, this targets the specific
sequence. Extension, where the polymerase adds new nucleotides to the
target strands, creating new strands of DNA. The reagents used are
template DNA, which is the sample inserted that we aim to replicate.
Primers are used to mark where the copying should start. The following
reagent would be loose nucleotides, which is what allow the new strands
to be made. Some enzymes are also used to help build the new DNA
strands. After the PCR was successfully ran it was put into A’s, T’s,
C’s and G’s. These sequences for each sample was then ran in BLASTn. The
types of organisms that the blood meals came from were inserted into the
datasheet.

The final step was analyzing the data to determine if the house finch
acted as the main host for WNV in the Salt Lake City area. Once the data
was collected it was inserted into a data sheet and analyzed using a bar
plot (for visual analysis) then also inserted into a statistical test to
determine if the observations we observed in the bar plot were
statistically significant. The bar plot was created in R using the
built-in statistical package. The tools used for making this bar plot
were the blood meal and WNV data sheets. The statistical models that
were used were descriptive statistics that allows us to see the summed
counts, and also the data visualization to create the bar plot. The
significance test that was run to analyze the data was summary(glm1) and
summary(glm2). The test for summary(glm1) was to look at the number of
House Finches and see if this can be used to predict the number of
positive tests for WNV. To test this a generalized linear model function
was used with the model type being a logistic regression. The test for
summary(glm2) was to look at the total House Finch rates and whether
this has an impact on the positivity rate. To test this a generalized
linear model function was used with the model type being a linear
regression.

## 1st Analysis - Barplot

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

![](Warm-up-mosquitoes-Knowles_files/figure-gfm/first-analysis-1.png)<!-- -->

``` r
par(op)

## Keep the colors mapping for reuse elsewhere
host_species_colors <- species_colors
```

This analysis was run to compare the number of mosquito blood meals that
came from locations without any WNV-positive pools and the locations
that had one or more WNV-positive pools for each host species that were
tested. The left bar plot shows the locations where WNV was not detected
and organizes them by the host species and the number of counts for each
of these species. The right bar plot shows the same information but for
the locations where WNV was detected and was also organized by host
species and the counts for each. We can see from this visual analysis
that in both WNV positive and WNV negative areas that House Finches were
the predominate host species chosen by the mosquitoes. The WNV positive
side had an even larger margin of distinction between themselves and the
rest of the host species.

## 2nd Analysis - Generalized Linear Model

``` r
#glm with house finch alone against binary +/_
glm1 <- glm(loc_positives ~ host_House_finch,
            data = counts_matrix,
            family = binomial)
summary(glm1)
```

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

This test allows us to formally determine if the visibly seen
relationship in the bar plots is statistically significant or not. This
test looked at whether or not the number of House Finch blood meals
correlates to the sites with WNV with binary values or just overall WNV
positivity rates with numeric values. The test for glm1 was to determine
whether the number of House Finches in an area was able to be used as a
predicted measure for areas that are more likely to test positive for
WNV. The results of this test showed that the number of House Finches
used as hosts has a statistically significant correlation to the rate of
WNV in those locations, given that the p-value is 0.0287. The test for
glm2 was to determine if the number of House Finches used as blood meals
was correlated to the positivity rate. The results of this test showed
that when the House Finch blood meal counts increased the rate for
positivity also increased and this was statistically significant with a
p-value of 4.54e-05.

# DISCUSSION

Given the results for both visual and numerical analyses ran, we can see
that there is a statistically significant correction between House Finch
populations and the WNV postitivity rate. This overall supports the idea
that House Finches act as a amplifying host for the WNV in the Salt Lake
City area.

## Interpretation of 1st Analysis (Barplot)

The bar plot that was created from the data collected shows us a visual
representation of the data. The column on the left shows the species
that the blood meals were collected from that tested negative for WNV.
The columns on the right show the blood meal results of the host species
that tested positive for WNV. Based on the bar plot created from the
data collect from the mosquito blood meals, we can see that there is a
strong visual correlation between the number of WNV cases and the
locations with the highest number of House Finches. In both the positive
and negative columns we see a high percentage of the blood meals were
taken from House Finches and an even higher percentage of these tests
came back positive for WNV. This visual representation of the data helps
to support the hypothesis that House Finches act as amplifying hosts for
the WNV and that the areas with the highest number of House Finches will
have the highest number of WNV positive cases. A limitation of this bar
plot is that it doesn’t give you a map of the locations to see exactly
where these cases of WNV occur and where the House Finches have the
highest population size.

## Interpretation of 2nd Analysis (Generalized Linear Model)

The first statistical analysis run gives us the relationship of the
tests taken with House Finch hosts and the ratio of positive tests for
WNV. Given that the p-value is 0.0287 then this is a very significant
relationship. The second statistical analysis run gives us the
relationship of the overall positivity rate with the data collected from
House Finches. The p-value for this test was 4.54e-05 which indicates a
very high correlation. These two statistical analyses support what was
seen the bar plot and what was predicted in the hypothesis. The overall
rate of WNV is higher in areas with a higher number of House Finches.

# CONCLUSION

Based on all the evidence collected and the statistical analyses run it
is possible to confirm the hypothesis stated. There is a correlation
between the host being a House Finch and the rate of positivity of WNV.
There is also a correlation between locations with higher numbers of WNV
and the same locations having higher numbers of House Finches. These are
all supported by the visual representations and the statistical analyses
run. All of these tests and visuals seem to support the idea that House
Finches act as amplification hosts for the WNV in the Salt Lake City
area.

# REFERENCES

1.  Komar N, Langevin S, Hinten S, Nemeth N, Edwards E, Hettler D, Davis
    B, Bowen R, Bunning M. Experimental infection of North American
    birds with the New York 1999 strain of West Nile virus. Emerg Infect
    Dis. 2003 Mar;9(3):311-22. <https://doi.org/10.3201/eid0903.020628>

2.  ChatGPT. OpenAI, version Jan 2025. Used as a reference for functions
    such as plot() and to correct syntax errors. Accessed 2025-10-11.

3.  Taieb, L., Ludwig, A., Ogden, N. H., Lindsay, R. L., Iranpour, M.,
    Gagnon, C. A., & Bicout, D. J. (2020). Bird Species Involved in West
    Nile Virus Epidemiological Cycle in Southern Québec. International
    journal of environmental research and public health, 17(12), 4517.
    <https://doi.org/10.3390/ijerph17124517>
