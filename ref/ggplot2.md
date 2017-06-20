# Reference on ggplot2 ([ggplot2 @ tidyverse](ggplot2.tidyverse.org))

## general

* general plotting:
      ggplot(data = <DATA>) +
      <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

* assigning aesthetics by different value
      aes(..., <parameter> = <df_column>)

      aes(..., <parameter> = <bool_vector>)

## statistics

* basic histogram
      + geom_box()

* 
