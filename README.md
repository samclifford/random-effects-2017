Visualisation of random effects in Bayesian hierarchical linear models
======================================================================

Exploring alternative ways to visualise the random effect means and
intercepts in a hierarchical linear model of the rats data from Gelfand
et al. (1990).

This poster was presented at the 2017 Bayes on the Beach workshop.

You may need to check for fonts on your system for LaTeX and you'll need
JAGS installed to fit the model.

Results
=======

The results at the conference for each plot were

<table>
<thead>
<tr class="header">
<th align="right">Plot</th>
<th align="right">Like</th>
<th align="right">Meh</th>
<th align="right">Dislike</th>
<th align="right">Total</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">Caterpillar</td>
<td align="right">12</td>
<td align="right">11</td>
<td align="right">3</td>
<td align="right">26</td>
</tr>
<tr class="even">
<td align="right">STAN</td>
<td align="right">18</td>
<td align="right">7</td>
<td align="right">2</td>
<td align="right">27</td>
</tr>
<tr class="odd">
<td align="right">Transparent density</td>
<td align="right">10</td>
<td align="right">10</td>
<td align="right">2</td>
<td align="right">26</td>
</tr>
<tr class="even">
<td align="right">Cumulative density</td>
<td align="right">3</td>
<td align="right">9</td>
<td align="right">11</td>
<td align="right">23</td>
</tr>
<tr class="odd">
<td align="right">Faceted density</td>
<td align="right">1</td>
<td align="right">7</td>
<td align="right">19</td>
<td align="right">27</td>
</tr>
</tbody>
</table>

![Final poster](final.jpg%20=360x)

Comments about the caterpillar (raterpillar?) plot were that it's a
common way they see it presented but they don't normally sort by one of
the parameter levels' medians. One coauthor indicated that they hated
the ordering as it implies a pattern that isn't "real" and that it
misled them into thinking they were looking at posterior predictions of
the rats' growth. The STAN-style visualisation was most popular, with a
five number summary deemed more informative and reading bars
horizontally rather than vertically was easier on the eyes as people
scan left to right more easily than up and down. Another coauthor
indicated that they wanted to know the overall mean, the posterior mean
of *β*<sub>00</sub> and *β*<sub>10</sub> for context to indicate whether
a given rat was higher or lower than the mean and this annotation was
added to the plot.

Transparent densities were described as being pretty and informative, as
the more dark the grey in the plot the more rats have interecepts/slopes
in that region. The use of colour was suggested by one coauthor. The
majority opinion, with only a few dissenting coauthors, was that this
would scale well to 100 rats as we'd be focussing on the distribution of
the distributions overall. One coauthor indicated that the transparent
shading was actually misleading, as it gave more visual attention to
overlapping tails when it was the means and variances of the parameters
that were the important values. This dissenting coauthor indicated a
preference for the cumulative density plots, as the curves are
approximately parallel and do not produce any interference patterns.
This graph was least popular, with many coauthors indicating that a lack
of experience with cumulative densities and difficulty interpreting them
were obstacles to being able to extract any meaningful information.

The overwhelming majority of coauthors described the faceted densities
as awful (with one coauthor going as far to say that they would produce
it for themselves on a large sheet to look at but would *never* publish
such a graph), as all small multiples show an approximately normal
distribution and the values on the *x* axis are the only avenue for
contrast but that these were too small. One coauthor indicated that a
table of small figures is less interpretable than a table of many
numbers (means and credible intervals for all 30 rats' intercepts and
slopes provides 180 numbers). Two coauthors indicated that there are
times when this style is useful - namely detecting one parameter which
is different to the others, whether skewed or bimodal (and a bimodal
small multiple was added to the plot on the poster). Coauthors also
indicated that using a common *x* axis would help understand where each
rat's parameters lay in the parameter space. Another coauthor extended
this idea to showing *all* density plots behind each small multiple and
then that small multiple's density with a darker line in order to show
not just where the parameter lays in the parameter space but also in
reference to the other rats.

Alternative visualisations were suggested, such as putting jittered dots
representing the MCMC samples behind the median and credible intervals
to help show bimodality and tail behaviour. Another coauthor suggested
that small multiples of posterior predictions would allow for an
understanding of where the aberrant rats (fast or slow growers) lay in
the pack. The data, model and visualisation code are all available on
this repository.
