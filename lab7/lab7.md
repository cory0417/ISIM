Lab 7: Pulse measurement with light - Oximeter
================
Daeyoung Kim
2023-04-14

- <a href="#introduction" id="toc-introduction">Introduction</a>
- <a href="#results" id="toc-results">Results</a>

``` r
library(tidyverse)
```

### Introduction

#### Circuit Design

![](./images/circuit_schematic.jpg)<!-- -->

![](./images/circuit.jpg)<!-- -->

#### Filters and Gain

To process the input voltage signal from the photo diode, a low pass
filter with 1 MΩ resistor and 10 nF capacitor is used with a cutoff
frequency of about 16 Hz. Then, a high pass filter with 10 µF capacitor
and 15.8 kΩ resistor were used to filter out frequencies that are much
slower than human pulse. The cutoff frequency was about 0.16 Hz. For the
connecting op amp, 1 MΩ and 115 kΩ resistors were used to create a gain
of about 9.7. To again filter higher frequency noise, an RC filter with
15.8 kΩ resistor and 1 µF capacitor were used with a cutoff frequency of
24 Hz. Again, an op amp with 1 MΩ and 115 kΩ resistors were used to
create a gain of about 9.7.

``` r
# First RC filter:
# Resistor: 1 MΩ
# Capacitor: 10 nF
#
# CR filter:
# Capacitor: 10 µF
# Resistor: 15.8 kΩ
# 
# Second RC filter:
# Resistor: 15.8 kΩ
# Capacitor: 1 µF
# 
# Op Amp:
# R1: 1 MΩ
# R2: 115 kΩ

RC1 <- 1e6*10e-9
CR <- 1e-6*1e6
RC2 <- 2e3*3.3e-6


df_cutoff <- 
  tibble(
    "filter_type" = c("low pass", "high pass", "low pass"),
    "RC_value" = c(RC1, CR, RC2)
  ) %>% 
  mutate(
    cutoff_frequency = 1/(2*pi*RC_value)
  )
  
df_cutoff
```

    ## # A tibble: 3 × 3
    ##   filter_type RC_value cutoff_frequency
    ##   <chr>          <dbl>            <dbl>
    ## 1 low pass      0.01             15.9  
    ## 2 high pass     1                 0.159
    ## 3 low pass      0.0066           24.1

#### Bode Plot

``` r
df_bode <- read_csv('data/bode.csv', show_col_types = FALSE)

df_bode %>% 
  ggplot(aes(freq, gain)) +
  geom_line() +
  scale_x_log10() +
  labs(
    title = "Bode Plot",
    x = "Frequency (Hz)",
    y = "Gain (dB)"
  ) +
  theme_minimal()
```

![](lab7_files/figure-gfm/Bode%20Plot-1.png)<!-- -->

### Results

To test the pulse detector, two cases were tested:

1.  Resting state
2.  Immediately after running up 4 flights of stairs.

``` r
df_rest_raw <- read_csv("data/rest.csv", show_col_types = FALSE)
df_run_raw <- read_csv("data/run.csv", show_col_types = FALSE)

df_rest <- 
  df_rest_raw %>% 
  select(t1, ch1) %>% 
  mutate(
    t1 = t1 - t1[1],
    state = "Rest"
    ) %>% 
  rename(
    time = t1,
    voltage = ch1
  )

df_run <- 
  df_run_raw %>% 
  select(t1, ch1) %>% 
  mutate(
    t1 = t1 - t1[1],
    state = "Run"
    ) %>% 
  rename(
    time = t1,
    voltage = ch1
  )

df_pulse <- 
  rbind(df_rest, df_run)

df_pulse %>% 
  ggplot(aes(time, voltage, col = state)) +
  geom_line() +
  facet_wrap(~state, ncol =1) +
  labs(
    title = "Voltage vs Time",
    x = "Time (s)",
    y = "Voltage (V)",
    caption = str_wrap("The plot shows the pulse measured at resting state and right after running. The frequency of the pulse in the lower plot is much greater than that of the higher one. The approximate pulse rates are 60 bpm and 150 bpm at resting state and at after running state, respectively.", 100),
    color = "State"
  ) +
  theme(
    plot.caption = element_text(hjust = 0, size = 10)
  )
```

![](lab7_files/figure-gfm/Results-1.png)<!-- -->
