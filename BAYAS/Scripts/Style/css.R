#tabset with equal sized headers. Just for known number of tabs
tabsetEqualHeader <- function(inputId, nTabs){
  paste0("#",
         inputId,
         ">div>ul>li {width: ",
         100/nTabs,
         "%;
          font-weight:bold;}")
}

