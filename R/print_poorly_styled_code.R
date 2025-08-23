#' Print a deliberately poorly styled tidyverse code example
#'
#' This function prints a long chunk of R/tidyverse code that has intentionally
#' inconsistent formatting, spacing, and indentation. It is designed for use in
#' demonstrations of code style tools such as [styler::style_text()] or
#' [styler::style_file()].
#'
#' @details
#' The printed code:
#' - Loads {dplyr}, {ggplot2}, and {stringr}.
#' - Performs grouping, summarisation, filtering, joining, and plotting on
#'   the built-in `mtcars` dataset.
#' - Is deliberately badly formatted (inconsistent spacing, misaligned pipes,
#'   poor indentation, inconsistent line breaks).
#'
#' The output is meant to provide an example input for automatic code
#' formatting tools, so that users can compare the *before* (poor style) and
#' *after* (styled) versions.
#'
#' @return
#' Invisibly returns `NULL`. The function is called for its side effect of
#' printing the poorly styled code to the console.
#'
#' @examples
#' \dontrun{
#' # Print the messy code to the console
#' print_poorly_styled_code()
#'
#' # Pass it to styler to see how formatting improves
#' styled <- styler::style_text(capture.output(print_poorly_styled_code()))
#' cat(styled, sep = "\n")
#' }
#'
#' @seealso [styler::style_text()], [styler::style_file()]
#' @export
print_poorly_styled_code <- function() {
  cat(r"(
library(dplyr)
library(ggplot2)
library(stringr)

mtcars %>% mutate(am=ifelse(am==1,
"manual","automatic"),gear=factor(
  gear
))%>%
group_by(
  cyl,am)%>%
summarise(avg_mpg=mean(mpg,na.rm=TRUE),
                       med_hp=median(hp),
                       count=n(),
                       .groups="drop"
                       )%>%
filter(avg_mpg>18|med_hp>100)%>%
mutate(flag=ifelse(avg_mpg>20&med_hp>110,TRUE,
                   FALSE),hp_per_cyl=med_hp/cyl)%>%arrange(desc(avg_mpg))%>%
left_join(
  mtcars%>%count(cyl, gear),by="cyl"  )%>%
mutate(label=str_c("cyl: ",cyl,"; am: ",am))%>%
ggplot(aes(x=reorder(
  label,
  avg_mpg
),y=avg_mpg,fill=
am))+geom_col(width  =   0.7,
              color  =   "black")+geom_text( aes( label = round(avg_mpg,1)),vjust = -0.5,size = 3)+
facet_wrap(~gear,scales="free_x")+theme_minimal(
)+theme(
axis.text.x=element_text(angle=45,hjust=1)
)+labs(
x="Group",y="Average MPG",title="Badly Styled Tidyverse Example")+
guides(fill=guide_legend(title="Transmission"))+coord_flip()
)")
}