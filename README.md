
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dhis2\_pi\_generator

The goal of `dhis2_pi_generator` is to help [DHIS2](dhis2.org) system
admins to develop new program indicators in bulk.

The Shiny app will allow you to pass in DHIS2 credentials and download
all data elements, attributes, and option sets associated with Tracker
programs in your instance. You can then build basic `event` or
`enrollment` program indicators with filters for definable values of any
data element or attribute.

After you select the basis for program indicators filters, click
“download PI metadata” for a json file of metadata. These you can import
into DHIS2 through the import/export app. The Shiny app logic is
designed so different PI names and codes are unique for your export, and
within the character limit. Both event and enrollment type indicators
are available, with congruous defaults for period boundaries,
expression, and filter expressions.

Demo here: <https://iambodo.shinyapps.io/pi_generator/>

## How It Works

For example, an infant health program has a data element for
`Feeding Type` which is associated with an option set, and those options
(values) could be “Exclusive” or “Mixed” or “Replacement”.

| Data Element | Value       |
|:-------------|:------------|
| Feeding Type | Exclusive   |
| Feeding Type | Mixed       |
| Feeding Type | Replacement |

But a tracked entity attribute for `Infant Weight` would have numeric
values. You want to break these up into ranges, from 0-2kg, 2-4kg, and
4kg+

| Data Element      | Value       |
|:------------------|:------------|
| Feeding Type      | Exclusive   |
| Feeding Type      | Mixed       |
| Feeding Type      | Replacement |
| Infant Weight (g) | 0-2000      |
| Infant Weight (g) | 2000-4000   |
| Infant Weight (g) | 4000+       |

You can select any or all of these values and ranges, building Program
Indicator filters in the Shiny app (go to “Filter options” to select the
numeric ranges).

This will save you building separate 6 program indicators in the DHIS2
UI, which can be difficult to do with consistent syntax.

| pi\_filter                                                                    | pi\_name                          | pi\_code                          |
|:------------------------------------------------------------------------------|:----------------------------------|:----------------------------------|
| \#{ZzYYXq4fJie.GQY2lXrypjO} &gt;=0 && \#{ZzYYXq4fJie.GQY2lXrypjO} &lt;2000    | Mch infant weight (g): 0\_2000    | MCH INFANT WEIGHT (G): 0\_2000    |
| \#{ZzYYXq4fJie.GQY2lXrypjO} &gt;=2000 && \#{ZzYYXq4fJie.GQY2lXrypjO} &lt;4000 | Mch infant weight (g): 2000\_4000 | MCH INFANT WEIGHT (G): 2000\_4000 |
| \#{ZzYYXq4fJie.GQY2lXrypjO} &gt;=4000 && \#{ZzYYXq4fJie.GQY2lXrypjO} &lt;6000 | Mch infant weight (g): 4000\_6000 | MCH INFANT WEIGHT (G): 4000\_6000 |
| \#{ZzYYXq4fJie.X8zyunlgUfM} == ‘Exclusive’                                    | Mch infant feeding: exclusive     | MCH INFANT FEEDING: EXCLUSIVE     |
| \#{ZzYYXq4fJie.X8zyunlgUfM} == ‘Mixed’                                        | Mch infant feeding: mixed         | MCH INFANT FEEDING: MIXED         |
| \#{ZzYYXq4fJie.X8zyunlgUfM} == ‘Replacement’                                  | Mch infant feeding: replacement   | MCH INFANT FEEDING: REPLACEMENT   |

Now say you want program indicator filters for each possible
**combination** of Feeding Type and Weight range.

Alternatively, filter for events with two possible values for the same
data element, e.g. “Exclusive” OR “Mixed”.

There are 15 possibilities for such combinations.

| V1          | V2          |
|:------------|:------------|
| Exclusive   | Mixed       |
| Exclusive   | Replacement |
| Exclusive   | 0-2000      |
| Exclusive   | 2000-4000   |
| Exclusive   | 4000+       |
| Mixed       | Replacement |
| Mixed       | 0-2000      |
| Mixed       | 2000-4000   |
| Mixed       | 4000+       |
| Replacement | 0-2000      |
| Replacement | 2000-4000   |
| Replacement | 4000+       |
| 0-2000      | 2000-4000   |
| 0-2000      | 4000+       |
| 2000-4000   | 4000+       |

Use the option for *Combination Filters* and you can build all possible
combinations of the data values. Change the “Combo Filter Junction” to
`||` to join each filter condition with `OR` instead of the default
`AND`.

You can add more up to five filter values for the “choice without
replacement” operation.

| pi\_name                                                              | pi\_code                                          | pi\_filter                                                                                                                    |
|:----------------------------------------------------------------------|:--------------------------------------------------|:------------------------------------------------------------------------------------------------------------------------------|
| MCH Infant Weight (g): 0\_2000 + MCH Infant Weight (g): 2000\_4000    | MCH INFANT WEIGHT (G): 0\_2000 + MCH …\_COMBO\_1  | \#{ZzYYXq4fJie.GQY2lXrypjO} &gt;=0 && \#{ZzYYXq4fJie.GQY2lXrypjO} =2000 && \#{ZzYYXq4fJie.GQY2lXrypjO} &lt;4000               |
| MCH Infant Weight (g): 0\_2000 + MCH Infant Weight (g): 4000\_6000    | MCH INFANT WEIGHT (G): 0\_2000 + MCH …\_COMBO\_2  | \#{ZzYYXq4fJie.GQY2lXrypjO} &gt;=0 && \#{ZzYYXq4fJie.GQY2lXrypjO} =4000 && \#{ZzYYXq4fJie.GQY2lXrypjO} &lt;6000               |
| MCH Infant Weight (g): 0\_2000 + MCH Infant Feeding: Exclusive        | MCH INFANT WEIGHT (G): 0\_2000 + MCH …\_COMBO\_3  | \#{ZzYYXq4fJie.GQY2lXrypjO} &gt;=0 && \#{ZzYYXq4fJie.GQY2lXrypjO} &lt;2000 && \#{ZzYYXq4fJie.X8zyunlgUfM} == ‘Exclusive’      |
| MCH Infant Weight (g): 0\_2000 + MCH Infant Feeding: Mixed            | MCH INFANT WEIGHT (G): 0\_2000 + MCH …\_COMBO\_4  | \#{ZzYYXq4fJie.GQY2lXrypjO} &gt;=0 && \#{ZzYYXq4fJie.GQY2lXrypjO} &lt;2000 && \#{ZzYYXq4fJie.X8zyunlgUfM} == ‘Mixed’          |
| MCH Infant Weight (g): 0\_2000 + MCH Infant Feeding: Replacement      | MCH INFANT WEIGHT (G): 0\_2000 + MCH …\_COMBO\_5  | \#{ZzYYXq4fJie.GQY2lXrypjO} &gt;=0 && \#{ZzYYXq4fJie.GQY2lXrypjO} &lt;2000 && \#{ZzYYXq4fJie.X8zyunlgUfM} == ‘Replacement’    |
| MCH Infant Weight (g): 2000\_4000 + MCH Infant Weight (g): 4000\_6000 | MCH INFANT WEIGHT (G): 2000\_4000 + M…\_COMBO\_6  | \#{ZzYYXq4fJie.GQY2lXrypjO} &gt;=2000 && \#{ZzYYXq4fJie.GQY2lXrypjO} =4000 && \#{ZzYYXq4fJie.GQY2lXrypjO} &lt;6000            |
| MCH Infant Weight (g): 2000\_4000 + MCH Infant Feeding: Exclusive     | MCH INFANT WEIGHT (G): 2000\_4000 + M…\_COMBO\_7  | \#{ZzYYXq4fJie.GQY2lXrypjO} &gt;=2000 && \#{ZzYYXq4fJie.GQY2lXrypjO} &lt;4000 && \#{ZzYYXq4fJie.X8zyunlgUfM} == ‘Exclusive’   |
| MCH Infant Weight (g): 2000\_4000 + MCH Infant Feeding: Mixed         | MCH INFANT WEIGHT (G): 2000\_4000 + M…\_COMBO\_8  | \#{ZzYYXq4fJie.GQY2lXrypjO} &gt;=2000 && \#{ZzYYXq4fJie.GQY2lXrypjO} &lt;4000 && \#{ZzYYXq4fJie.X8zyunlgUfM} == ‘Mixed’       |
| MCH Infant Weight (g): 2000\_4000 + MCH Infant Feeding: Replacement   | MCH INFANT WEIGHT (G): 2000\_4000 + M…\_COMBO\_9  | \#{ZzYYXq4fJie.GQY2lXrypjO} &gt;=2000 && \#{ZzYYXq4fJie.GQY2lXrypjO} &lt;4000 && \#{ZzYYXq4fJie.X8zyunlgUfM} == ‘Replacement’ |
| MCH Infant Weight (g): 4000\_6000 + MCH Infant Feeding: Exclusive     | MCH INFANT WEIGHT (G): 4000\_6000 + M…\_COMBO\_10 | \#{ZzYYXq4fJie.GQY2lXrypjO} &gt;=4000 && \#{ZzYYXq4fJie.GQY2lXrypjO} &lt;6000 && \#{ZzYYXq4fJie.X8zyunlgUfM} == ‘Exclusive’   |
| MCH Infant Weight (g): 4000\_6000 + MCH Infant Feeding: Mixed         | MCH INFANT WEIGHT (G): 4000\_6000 + M…\_COMBO\_11 | \#{ZzYYXq4fJie.GQY2lXrypjO} &gt;=4000 && \#{ZzYYXq4fJie.GQY2lXrypjO} &lt;6000 && \#{ZzYYXq4fJie.X8zyunlgUfM} == ‘Mixed’       |
| MCH Infant Weight (g): 4000\_6000 + MCH Infant Feeding: Replacement   | MCH INFANT WEIGHT (G): 4000\_6000 + M…\_COMBO\_12 | \#{ZzYYXq4fJie.GQY2lXrypjO} &gt;=4000 && \#{ZzYYXq4fJie.GQY2lXrypjO} &lt;6000 && \#{ZzYYXq4fJie.X8zyunlgUfM} == ‘Replacement’ |
| MCH Infant Feeding: Exclusive + MCH Infant Feeding: Mixed             | MCH INFANT FEEDING: EXCLUSIVE + MCH I…\_COMBO\_13 | \#{ZzYYXq4fJie.X8zyunlgUfM} == ‘Exclusive’ && \#{ZzYYXq4fJie.X8zyunlgUfM} == ‘Mixed’                                          |
| MCH Infant Feeding: Exclusive + MCH Infant Feeding: Replacement       | MCH INFANT FEEDING: EXCLUSIVE + MCH I…\_COMBO\_14 | \#{ZzYYXq4fJie.X8zyunlgUfM} == ‘Exclusive’ && \#{ZzYYXq4fJie.X8zyunlgUfM} == ‘Replacement’                                    |
| MCH Infant Feeding: Mixed + MCH Infant Feeding: Replacement           | MCH INFANT FEEDING: MIXED + MCH INFAN…\_COMBO\_15 | \#{ZzYYXq4fJie.X8zyunlgUfM} == ‘Mixed’ && \#{ZzYYXq4fJie.X8zyunlgUfM} == ‘Replacement’                                        |
