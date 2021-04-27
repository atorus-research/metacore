metacore
========================================================
author: Christina Fillmore
date: 14 April 2021
autosize: true

Goal of the Metacore Package
========================================================

*The goal of metacore is to create a structured non-modifiable object to contain dataset metadata.*

The metadata this object contained is the same as the information usually held in a
specification document or the define.

- Formal structure: allows for leverage metadata in a consistent manner
- Regulated:
- Bullet 3

Understanding the Structure
========================================================

The metadata is held in a series of tables, with keys to connect them, much like a traditional
SQL database. Data is normalized where possible to reduce the size of the object.
***
![alt text](vignettes/schema-colors.png)

Slide With Plot
========================================================

![plot of chunk unnamed-chunk-1](metacore-figure/unnamed-chunk-1-1.png)
