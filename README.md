# LDA-Hotel-review

# Hotel Review Topic Modelling (Assignment Part 3)

This repository contains the workflow and code for analysing customer satisfaction drivers through topic modelling on hotel reviews.

---

## 📂 Data

* **File:** `HotelData.csv`
* **Fields (excerpt):**
  * `review_text` – full customer review
  * `rating` – Likert scale **1 (low)** to **5 (high)**

---

## 🎯 Objectives

1. **Reproducible Sampling**  
   * Randomly select **2 000** reviews:
     ```r
     library(dplyr)
     set.seed(<XXX>)          # XXX = last 3 digits of your student ID
     sample_reviews <- sample_n(hotel_reviews, 2000)
     ```
2. **Separate Corpora**  
   * **Positive reviews** vs. **Negative reviews** (define criterion explicitly).
3. **Topic Modelling**  
   * Discover latent **factors** in each corpus (positive & negative).
4. **Insights**  
   * Identify and label topics.  
   * Discuss **top 3 satisfaction** factors and **top 3 dissatisfaction** factors.

---

## 📝 Report Checklist (`.docx`)

| Section | Must Include |
|---------|--------------|
| **Classification Rule** | How ratings map to *positive* / *negative* sets |
| **Pre-processing Steps** | Tokenisation, stop-word removal, stemming/lemmatisation, n-grams, etc. |
| **Topic-Count Selection** | Metrics (e.g., coherence, perplexity) & visualisations |
| **Topic Labelling** | Top terms ⟶ human-readable labels |
| **Key Findings** | 3 main positive factors & 3 main negative factors with discussion |

---

## 🛠 Suggested Workflow

1. **Clone** or download this repo.  
2. Place `HotelData.csv` in `/data`.  
3. Run `/analysis/topic_model.Rmd` (or `.R`) which:
   * Loads and samples the data
   * Splits into positive / negative corpora
   * Pre-processes text
   * Fits topic models (e.g., LDA, STM)
   * Exports visualisations to `/figures`
4. Compile findings into `report.docx` located in `/report`.

---


