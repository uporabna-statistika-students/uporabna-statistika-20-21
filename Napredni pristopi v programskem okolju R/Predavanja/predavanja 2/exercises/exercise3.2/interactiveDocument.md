---
title: "Demo app"
output: html_document
runtime: shiny
---

This R Markdown document is made interactive using Shiny. Unlike the more traditional workflow of creating static reports, you can now create documents that allow your readers to change the assumptions underlying your analysis and see the results immediately. 

To learn more, see [Interactive Documents](http://rmarkdown.rstudio.com/authoring_shiny.html).

## Inputs and Outputs

You can embed Shiny inputs and outputs in your document. Outputs are automatically updated whenever inputs change.  This demonstrates how a standard R plot can be made interactive by wrapping it in the Shiny `renderPlot` function. The `selectInput` and `sliderInput` functions create the input widgets used to drive the plot.

<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<label class="control-label" for="n_breaks">Number of bins:</label>
<select id="n_breaks"><option value="10">10</option>
<option value="20" selected>20</option>
<option value="35">35</option>
<option value="50">50</option></select>
<script type="application/json" data-for="n_breaks" data-nonempty="">{}</script>
</div>
<div>
<div>
<label class="control-label" for="bw_adjust">Bandwidth adjustment:</label>
<input id="bw_adjust" type="slider" name="bw_adjust" value="1" class="jslider" data-from="0.2" data-to="2" data-step="0.2" data-skin="plastic" data-round="FALSE" data-locale="us" data-format="#,##0.#####" data-scale="|;|;|;|;|;|;|;|;|;|" data-smooth="FALSE"/>
</div>
</div>
</div>
</div><!--/html_preserve--><!--html_preserve--><div id="out2ab0b551ca5b82ae" class="shiny-plot-output" style="width: 100% ; height: 400px"></div><!--/html_preserve-->

## Scatterplot of the eruptions data
<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<label for="xlabel">X axis label</label>
<input id="xlabel" type="text" value="Eruption time in minutes"/>
</div>
<div>
<label for="ylabel">Y axis label</label>
<input id="ylabel" type="text" value="Waiting time to next eruption in minutes"/>
</div>
</div>
</div><!--/html_preserve--><!--html_preserve--><div id="outf34f6b860b58b2dc" class="shiny-plot-output" style="width: 100% ; height: 400px"></div><!--/html_preserve-->

## Embedded Application

It's also possible to embed an entire Shiny application within an R Markdown document using the `shinyAppDir` function. This example embeds a Shiny application located in another directory:

<!--html_preserve--><iframe src="app9289a2618f03b69f77736e9634b709fb/?w=&amp;__subapp__=1" width="100%" height="700" class="shiny-frame"></iframe><!--/html_preserve-->

Note the use of the `height` parameter to determine how much vertical space the embedded application should occupy.

You can also use the `shinyApp` function to define an application inline rather then in an external directory.

In all of R code chunks above the `echo = FALSE` attribute is used. This is to prevent the R code within the chunk from rendering in the document alongside the Shiny components.



