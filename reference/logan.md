# Logan airport dataset

Median delay times for passenger carriers flying out of Logan
International Airport (Boston, USA) by month and am/pm scheduled
departure for the 2023 year.

## Usage

``` r
data(logan)
```

## Format

A data frame with the following variables:

- am_pm:

  Morning (`am`) or afternoon (`pm`) scheduled flight

- carrier:

  Passenger carrier. `AA` = American Airlines, `DL` = Delta Airlines,
  `B6` = JetBlue Airways, `UA` = United Airlines, `WN` = Southwest
  Airlines.

- month:

  Month

- delay:

  Delay time in minutes. Negative values indicate an earlier than
  scheduled departure.
