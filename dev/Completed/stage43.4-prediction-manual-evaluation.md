# Stage 43.4 prediction manual evaluation set

## Purpose

This document defines a small, repeatable set of prediction questions for manual evaluation in WMFM developer mode. These are not automated tests and they do not prescribe exact numerical answers. Run each labelled prompt in the corresponding packaged example, download the developer-mode JSON, and retain the label in the question text so that the returned output can be matched to the prompt unambiguously.

Do not remove or alter the label at the start of a prompt. The labels use the form `WMFM-PRED-<DATASET>-<NUMBER>`.

## What to inspect

For each result, inspect whether WMFM:

- identifies the requested predictor values correctly;
- distinguishes an average or expected response from an individual future outcome;
- leads with a prediction interval when the wording explicitly asks about an individual;
- presents both mean-response and individual-outcome uncertainty when the wording is personally framed but ambiguous;
- avoids inventing a threshold when words such as "well" or "high" are not numerically defined;
- uses language appropriate to linear, logistic, or Poisson models;
- avoids duplicated deterministic text;
- reports unsupported requests cautiously rather than fabricating a calculation.

## Course

Use the packaged `Course` example with model `Exam ~ Attend + Test`.

### WMFM-PRED-COURSE-01 - explicit individual prediction

`[WMFM-PRED-COURSE-01] What mark would you predict I would get if I attended class regularly and scored 15 out of 20 on the test?`

Expected intent to inspect: individual outcome should lead; the average response may be added as a secondary teaching comparison.

### WMFM-PRED-COURSE-02 - ambiguous personal outcome

`[WMFM-PRED-COURSE-02] Will I do well on the final exam if I attend class regularly and score 15 out of 20 on the test?`

Expected intent to inspect: personal but ambiguous; both the expected mark and individual prediction should be explained, without defining "do well" as an unstated numerical threshold.

### WMFM-PRED-COURSE-03 - explicit mean response

`[WMFM-PRED-COURSE-03] What is the expected final exam mark for students who attend class regularly and score 15 out of 20 on the test?`

Expected intent to inspect: mean response should lead; an individual prediction interval should not displace the requested confidence interval.

## Oysters

Use the packaged `Oysters` example with model `Oysters ~ Site`. Replace `<SITE>` with one exact site level shown in the app before running each prompt, and use the same site for all three prompts.

### WMFM-PRED-OYSTERS-01 - explicit future count

`[WMFM-PRED-OYSTERS-01] If I take one new sample at <SITE>, how many oysters would you predict I will count?`

Expected intent to inspect: an individual future count should lead, with Poisson-appropriate wording and uncertainty.

### WMFM-PRED-OYSTERS-02 - ambiguous practical wording

`[WMFM-PRED-OYSTERS-02] If I sample at <SITE>, am I likely to find many oysters?`

Expected intent to inspect: the phrase "many oysters" is undefined; WMFM should provide the expected and future-count information it can support without inventing a threshold.

### WMFM-PRED-OYSTERS-03 - explicit expected count

`[WMFM-PRED-OYSTERS-03] What is the expected number of oysters in a sample taken at <SITE>?`

Expected intent to inspect: the expected count should lead and should be distinguished from the variability of a new observed count.

## Quakes

Use the packaged `Quakes` example with model `Freq ~ Magnitude * Locn`.

### WMFM-PRED-QUAKES-01 - explicit future count

`[WMFM-PRED-QUAKES-01] How many earthquakes would you predict in a new frequency interval at magnitude 5.0 in Southern California?`

Expected intent to inspect: future count should lead, with the location and magnitude extracted correctly.

### WMFM-PRED-QUAKES-02 - ambiguous practical wording

`[WMFM-PRED-QUAKES-02] At magnitude 5.0 in Southern California, should I expect many earthquakes?`

Expected intent to inspect: "many" is undefined; WMFM should not manufacture a cutoff and should explain expected versus observed count variation where supported.

### WMFM-PRED-QUAKES-03 - explicit expected count

`[WMFM-PRED-QUAKES-03] What is the expected earthquake frequency at magnitude 5.0 in Southern California?`

Expected intent to inspect: expected count should lead, with no unnecessary replacement by an individual-outcome answer.

## Diamonds

Use the packaged `Diamonds IV` example with model `log(price) ~ log(carat) + cut + color + clarity`. Replace each placeholder with an exact level displayed in the app before running the prompts, and use the same settings for all three prompts.

### WMFM-PRED-DIAMONDS-01 - explicit individual prediction

`[WMFM-PRED-DIAMONDS-01] What price would you predict for a 1.0 carat diamond with cut <CUT>, color <COLOR>, and clarity <CLARITY>?`

Expected intent to inspect: individual diamond price should lead; back-transformation and interval wording should be handled correctly.

### WMFM-PRED-DIAMONDS-02 - ambiguous personal purchase wording

`[WMFM-PRED-DIAMONDS-02] If I buy a 1.0 carat diamond with cut <CUT>, color <COLOR>, and clarity <CLARITY>, what price should I expect to pay?`

Expected intent to inspect: wording may support both a typical or expected price and an individual purchase price; WMFM should explain the distinction clearly.

### WMFM-PRED-DIAMONDS-03 - explicit mean response

`[WMFM-PRED-DIAMONDS-03] What is the expected price of diamonds weighing 1.0 carat with cut <CUT>, color <COLOR>, and clarity <CLARITY>?`

Expected intent to inspect: expected price should lead; the answer should clearly identify the response scale after fitting the model to log price.

## Returning results

Run one prompt at a time and download the developer-mode JSON after each run. The prompt label should appear in the captured question text. Keep the generated filename, or rename it to begin with the matching label, for example:

```text
WMFM-PRED-COURSE-01.json
```

Return the JSON files together so behaviour can be compared across datasets and intent types.
