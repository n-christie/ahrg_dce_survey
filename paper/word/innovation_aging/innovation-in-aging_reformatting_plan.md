# Reformatting Plan: "The Value of Location" for *Innovation in Aging*

Manuscript type: **Original Research Article** (250-word structured abstract; 6,000-word text cap; 50-reference cap; 5-element cap).

## STATUS UPDATE 4 — class-selection justification documented (fills a real gap)

Audited `scripts/paper/latent_class_analysis.R` line by line against the raw data and the four saved model objects (`output/models/lc_mnl_base.rds`, `lc_2class.rds`, `lc_3class.rds`, `lc_4class.rds`) to confirm the 3-class solution is actually justified, not just asserted. Found that **the choice of Q=3 was never documented anywhere** — not in code comments, not in the paper — only implicit in which model `lc_plot.R` happens to load. That's the gap this update closes.

**Recomputed AIC/BIC/CAIC/entropy independently** (matches the script's own `model_fit` table exactly):

| Model | Q | LogLik | K | AIC | BIC | CAIC | Entropy |
|---|---|---|---|---|---|---|---|
| MNL | 1 | -4344.3 | 9 | 8706.7 | 8750.5 | 8759.5 | — |
| LC-2 | 2 | -4083.5 | 19 | 8205.0 | 8297.4 | 8316.4 | 0.635 |
| LC-3 | 3 | -3898.9 | 29 | 7855.9 | 7996.9 | 8025.9 | 0.682 |
| LC-4 | 4 | -3856.9 | 39 | 7791.8 | 7981.5 | 8020.5 | 0.732 |

(N = 957 respondents used as sample size for BIC/CAIC, per the script's own footnote convention.)

**Why 3, not 4, despite BIC technically still falling at Q=4:**
- BIC improves by 453 points going 1→2 classes, another 300 points going 2→3, but only **~15 points** going 3→4 — an order-of-magnitude smaller gain, the classic diminishing-returns "elbow" at Q=3.
- Entropy is modest throughout (0.64–0.73) and never reaches the conventional "good separation" threshold (~0.8) even at Q=4, so the 4-class solution doesn't buy materially cleaner class assignment either.
- Substantively, LC-4's fourth class is small (9–14% of the sample under both the model-implied prior share and the modal/posterior assignment) and has a price coefficient (-9.42) more than double the magnitude of any other class-price estimate across all four models — consistent with a small, statistically unstable split rather than a genuine, well-populated fourth segment.

**Suggested language for the manuscript** (fills the `[to finalize: one sentence on class selection criteria]` placeholder in the Abstract draft, Section 5 below, and can extend the Methods paragraph from Status Update 2):

> Model selection favored a 3-class solution: relative fit (AIC, BIC, CAIC) improved sharply from one to two and two to three classes but only marginally from three to four, and the four-class solution's additional segment was small (9–14% of respondents) and less stable, so the 3-class model was retained as the most parsimonious solution with substantively interpretable, adequately sized segments.

**Still open**: this sentence needs to actually be inserted into the docx Methods paragraph and the Abstract placeholder — this status update documents the justification and recommended text but doesn't touch the .docx files. Also worth double-checking against your own judgment on the entropy interpretation, since 0.68 (LC-3) is "moderate," not "good," separation — if a reviewer pushes on this, the honest answer is that classes are reasonably but not sharply separated, which is worth a sentence in the Limitations subsection rather than glossing over.

---

## STATUS UPDATE 3 — class profiles added + a structural bug fixed

Pulled `paper/word/tables/lc_class_profiles.html` and added the demographic-by-class breakdown as **Supplementary Table S5**. Also adopted your own class labels from that file — Shop-seekers, Nature-seekers, Car-centred — throughout Table 4, the Results text, and the Discussion, instead of generic "Class 1/2/3." Added one sentence to the Results latent-class paragraph summarizing the modal-class demographics (Car-centred slightly older, more likely homeowners, less likely female) with a pointer to Table S5.

**Important fix**: while adding these tables I found and corrected a real structural bug in both docx files — some table-formatting XML (cell margins, border ordering, and a `tblLayout` element) was in the wrong sequence per the OOXML schema. This didn't show up visually in Word necessarily, but it was invalid enough that LibreOffice's PDF export was silently dropping the last few tables in the supplement during my own verification renders. Both files now pass full OOXML schema validation — safe to open in Word without a "needs repair" prompt.

**Final numbers**: body text 4,638 / 6,000 words; elements 5 / 5; references 49 / 50; supplement now has 5 tables (S1–S5) + 1 figure placeholder (S1).

---

## STATUS UPDATE 2 — latent class analysis now integrated

Pulled the latent class results from your project repo (`scripts/paper/latent_class_analysis.R`, `output/models/lc_3class.rds`, and the `.tex` tables) and wrote them into main_document_innovation-in-aging.docx and supplementary_material.docx. The 3-class solution was used (selected in your scripts over 2- and 4-class alternatives). Added:

- **Methods**: a paragraph describing the latent class estimation approach (gmnl package, model selection via AIC/BIC/CAIC/entropy).
- **Results**: a new "Latent class results" subsection summarizing the three segments — a price-sensitive shop-oriented class (38.8%), a green-space-oriented class (24.7%), and a less price-sensitive parking-oriented class (36.5%) — plus **Table 4** (coefficients + MWTP by class, the 5th and final main-text element).
- **Discussion**: a paragraph tying the latent class findings back to the existing "selective heterogeneity" narrative.
- **Abstract, Innovation and Translational Significance**: both updated to reflect the latent class contribution, still within their word caps.
- **Supplementary Table S4**: full model comparison (MNL, LC-2, LC-3, LC-4) with AIC/BIC/CAIC/entropy.
- **New reference**: Sarrias & Daziano (2017) for the gmnl package — verified via web search, not fabricated.

**Final verified numbers**: body text 4,565 / 6,000 words; elements 5 / 5 (Tables 1–4 + Figure 1 — at the cap, no more room without cutting something else); references 49 / 50 (1 slot left); abstract 243 / 250 words; innovation statement 95 / 100 words.

**One gap**: your R scripts also compute demographic profiles by class (age/tenure/gender/health composition of each segment), but that output is only printed to console in `latent_class_analysis.R`, not saved to a table — I don't have R available in this environment to regenerate it. If you want that breakdown in the paper, run that script section yourself and send me the `profiles_lc3` output, or paste the numbers directly.

---

## STATUS UPDATE — reformatting applied to your latest draft (main_document.docx)

All of the mechanical/structural reformatting below has now been applied to the draft you uploaded and saved as two files: **main_document_innovation-in-aging.docx** (anonymized main document) and **supplementary_material.docx**. Verified final numbers:

- **Elements: 4 of 5 used** in the main text (Table 1 attributes, Table 2 sample characteristics, Table 3 — merged coefficients + MWTP, Figure 1 — example choice set). Tables 5–7 (age/gender/health interactions) and the location map moved to Supplementary Material as Tables S1–S3 and Figure S1. **One slot remains for your latent class table.**
- **Words: 4,112 of 6,000** in the main text body. That leaves **~1,900 words of headroom** for the full latent class section (methods + results + discussion integration) — much more than the ~700–900 words a focused subsection typically needs.
- **References: 48 of 50.** Removed two unused entries (Berg & Liljedal — its only citation was in a paragraph that got condensed out; Earnhart — was already uncited in the draft you sent). 2 slots free for latent-class methodology citations.
- **Abstract: 231 of 250 words**, restructured into the four required headings.
- **Innovation and Translational Significance: 79 of 100 words**, newly added.

Also done: renamed section headers, merged Tables 3+4 into one table, fixed all identified first-person ("we/our") instances to third person, trimmed age/gender/health prose in Results and Discussion to brief summaries pointing to the supplement, fixed keywords (dropped "older people," trimmed to 5), fixed a subject-verb agreement slip introduced by the anonymization edit.

**What's still open — same three items as before, still waiting on your latent class analysis:**
1. The actual latent class methods/results/discussion content isn't written yet — nothing below can be finalized without it.
2. Once added, the Results/Abstract/Discussion will need the two bracketed placeholders `[to finalize...]` filled in with your real findings (see Section 5 below).
3. Word and element budgets above assume the latent class section fits in the remaining 1 table slot and ~1,900 words — flag if your analysis needs more than one table.

---

## Original plan (for reference — mostly superseded by the Status Update above)

## 1. Element allocation (hard cap: 5 tables/figures in main text)

Current draft has 9 elements (2 figures, 7 tables) — over cap even before the new section. Proposed split:

**Main text (5 elements):**
1. Table 1 — Attributes and levels (was Table 1)
2. Table 2 — Sample characteristics by tenure (was Table 2)
3. Table 3 — Mixed logit coefficients + MWTP, owners/renters, combined into one table (was Tables 3 & 4)
4. Table 4 — **Latent class results** (class enumeration/fit stats, class profiles, class-specific WTP) — NEW, pending your analysis
5. Figure 1 — Example choice set (was Figure 2; kept because it's the fastest way for an interdisciplinary reviewer to understand the DCE task)

**Supplementary Material (no cap, still peer-reviewed):**
- Figure S1 — Location map of participants (was Figure 1)
- Table S1 — Age interaction model (was Table 5)
- Table S2 — Gender interaction model (was Table 6)
- Table S3 — Health interaction model (was Table 7)

Each moved element needs one in-text citation, e.g. "see Supplementary Table S1," and the Results/Discussion prose describing age/gender/health interactions should shrink to a 1–2 sentence headline summary rather than a full paragraph per subgroup (see word budget below) — this is also where most of your word-count room comes from.

Note: merging Tables 3+4 means one table with both coefficients and MWTP columns side by side, following the same APA note/footnote conventions already used.

## 2. Word budget (hard cap: 6,000 words, text only)

Current Introduction–Conclusion body: **~6,400 words** — already over, before adding anything.

| Section | Current (approx.) | Target after edit | Notes |
|---|---|---|---|
| Background and Objectives (was Introduction) | ~1,450 | ~1,200 | Light trim; tighten literature paragraph |
| Research Design and Methods | ~1,800 | ~1,600 | Trim experiment-design prose slightly; add ~150–200 words for latent class estimation approach |
| Results | ~1,550 | ~1,050 | Cut age/gender/health subsections to brief summaries (detail moves to Supplement); add ~350–400 words for latent class results |
| Discussion and Implications (was Discussion + Limitations + Conclusion) | ~1,600 | ~1,600 | Cut per-subgroup paragraphs to the existing synthesis paragraph; add ~250–300 words on what latent class adds beyond the interaction models |
| **Total** | **~6,400** | **~5,450–5,700** | Leaves ~300–550 words of headroom depending on how much the latent class section needs |

This only works if the age/gender/health *tables* move to Supplement — if you want to keep all three in the main text as tables, the word math doesn't close no matter how much prose is cut.

## 3. Reference count (hard cap: 50)

Currently 46 — only 4 slots free. Latent class methodology typically needs 2–4 new citations (e.g., a seminal latent-class choice model paper, a class-enumeration/fit-statistic reference, and possibly a housing-specific latent class precedent). Recommend pruning 1–2 more marginal references now to create buffer, rather than finding out at 51 after the section is written.

## 4. Structural / heading changes (required regardless of new section)

Journal requires exactly these four sections in the Main Document text: **Background and Objectives, Research Design and Methods, Results, Discussion and Implications.**

- Introduction → **Background and Objectives**
- Materials and Method → **Research Design and Methods** (Ethics, Experiment design, Statistical analysis become subheadings within it)
- Results → unchanged name; Sample characteristics / Standard model results / latent class results / (trimmed) heterogeneity summary become subheadings
- Discussion + Limitations + Conclusion → merge into **Discussion and Implications**, with Limitations as a subheading; Conclusion content folds into the closing paragraphs rather than standing alone

## 5. Abstract — must become structured, ≤250 words

Same four headings as above. Draft below is built from your existing abstract; **the Results and Discussion paragraphs need your latent class numbers before this is final** — treat this as scaffolding, not copy-ready text:

> **Background and Objectives:** Relocation decisions among older adults involve trade-offs across housing and neighbourhood attributes, yet few stated-preference studies quantify these trade-offs or test for heterogeneity in how they are valued. This study examined preferences for housing and locational attributes among older adults in Sweden considering relocation, testing for variation by tenure, age, gender, and health.
>
> **Research Design and Methods:** A discrete choice experiment embedded in the Prospective RELOC-AGE cohort (N = 957; mean age = 72; 55.3% women) asked participants to choose between housing alternatives varying in proximity to green space, shops, and public transport, parking availability, and monthly cost. Mixed logit models, estimated separately for homeowners and renters with interaction terms for age, gender, and health, were used to derive marginal willingness-to-pay (MWTP) estimates. Latent class models were estimated to identify unobserved preference segments. **[to finalize: one sentence on class selection criteria]**
>
> **Results:** Homeowners consistently expressed higher MWTP than renters across all attributes, with the largest gap for parking (approximately twice). Age, gender, and health explained only modest, attribute-specific variation in preferences. **[insert latent class finding — e.g., number of classes identified and what distinguished them]**
>
> **Discussion and Implications:** Tenure differences in willingness to pay largely reflect price sensitivity rather than weaker underlying preferences among renters. **[insert how latent class results change or reinforce this policy conclusion]** Findings support integrating housing, transport, and land-use planning to protect equitable access to well-located housing as populations age.

(Current draft of the four sections above: ~230 words, leaving room for the two latent-class insertions.)

## 6. New requirement: Innovation and Translational Significance (100 words, below abstract)

This section doesn't exist in the current draft. Draft:

> This study quantifies how older adults in Sweden weigh neighbourhood accessibility, parking, and housing cost when considering relocation, using discrete choice methods that move beyond descriptive or qualitative housing research. By comparing homeowners and renters and testing both observable subgroup differences and latent, unobserved preference segments, the analysis shows that market-based willingness-to-pay gaps partly reflect financial constraint rather than weaker underlying preference among renters. These findings can inform age-inclusive housing and transport policy by clarifying which residential attributes warrant universal versus targeted provision as populations age. *(~95 words — adjust once latent class framing is final.)*

## 7. Double-anonymization — main document must have zero identifying info or first-person language

Two separate files required at submission:
- **Title page**: title, author names/affiliations/degrees/emails, corresponding author, Funding, Conflict of Interest, Data Availability, Acknowledgments (optional), CRediT roles. **None of these back-matter sections (Funding/COI/Data Availability) exist yet in your draft — need to be written from scratch**, even if just "None reported" for COI/funding.
- **Main document**: fully anonymized, third person throughout.

The current draft uses first-person ("We examined...", "We estimated...", "We followed...") extensively in the Abstract, Methods, Results, and Discussion. This needs a systematic pass, e.g.:
- "We examined heterogeneity in housing preferences..." → "This study examined heterogeneity in housing preferences..."
- "We estimated mixed logit models to recover..." → "Mixed logit models were estimated to recover..."
- "We followed (Caplan et al., 2021) to estimate..." → "Following the approach of (Caplan et al., 2021), willingness-to-pay values were estimated..."

This is a global find-and-fix, not a one-line change — every section has multiple instances.

## 8. Keywords

Current 5 keywords (stated choice; discrete choice; ageing in place; housing; housing preferences) satisfy the 3–5 count and don't duplicate title terms, but "housing" and "housing preferences" overlap — consider swapping one for something more specific to the new analysis, e.g. "latent class analysis" or "preference heterogeneity," which also better signals the paper's expanded contribution.

## Open items needing your input before final drafting
1. The actual latent class analysis (methods, number of classes, class profiles, fit statistics) — nothing above can be finalized without it.
2. Confirm the element plan in Section 1, especially: OK to move the location map (Figure 1) to Supplementary and combine Tables 3+4?
3. Decide which reference(s) to prune to create buffer under the 50-cap before adding latent class methodology citations.
