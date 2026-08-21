# Editorial notes — companion to manuscript.md

Working notes from the section-by-section revision, kept separate so
`manuscript.md` reads as clean, submission-formatted prose. Not part of the
manuscript itself.

## Open items — need your input before this is submission-ready

1. **D-optimal vs. orthogonal design.** `manuscript.md` still says "a
   D-optimal fractional factorial design," per your instruction to keep it
   as-is pending your own check. For the record: the archived design
   scripts (`scripts/archive/create_choice_questions.R`, `magnus_levels.R`,
   `cbctools.R`) all call `cbc_design(..., method = 'orthogonal')` for the
   variable that looks like the one actually fielded, despite being named
   `design_dopt`. cbcTools' actual D-optimal-type option is `method =
   'Modfed'`, used elsewhere in the same files for a different,
   apparently-unused design object. Worth confirming against your own
   records which design was actually used before submission — a DCE-literate
   reviewer may check this.

2. **Response rate: 73% vs. 76.7%.** `manuscript.md` still says 957/1,247 =
   73%, per your instruction to keep it pending your own check. The actual
   arithmetic is 76.7%. If 73% comes from a different denominator, that
   denominator needs to be stated explicitly; otherwise this should become
   77% (or the nearest accurate rounding).

3. **`interaction_regs.R` may be dead code.** Worth checking: it builds a
   three-category age interaction model (55–64/65–74/75+) that doesn't
   appear to feed any table actually used in the paper (the real
   age-interaction results, Supplementary Table S1, come from
   `interaction_regs_age.R`'s binary age≥72 specification instead). If
   `interaction_regs.R`'s output isn't used anywhere, it's worth removing
   or clearly marking as superseded, so a future reader of the codebase
   doesn't assume it's the source of the reported results.

4. ~~manuscript.md still needs to be merged into the actual docx.~~
   **Done.** `main_document_innovation-in-aging.docx` now matches
   `manuscript.md` throughout: Abstract, Introduction, Methods (including
   removing Equation 3's derivation, which was intentional per the
   simplification — see changelog), Results, Discussion, Limitations,
   Conclusion, and References (39 entries) were all replaced or
   restructured in place. Embedded equations 1/2/4/5/6 and all four tables
   verified intact afterward. Backup of the pre-merge state saved as
   `main_document_innovation-in-aging.PRE_MERGE.bak.docx`.

5. **Scale-comparability caveat added to Limitations — read it once.** New
   substantive point, not just wording: owner and renter mixed logit models
   are estimated *separately*, so their coefficients are each identified
   only up to that model's own scale (a standard property of discrete
   choice/logit models — coefficients are confounded with the variance of
   the unobserved error term). This means raw coefficient magnitudes
   ("renters' coefficient of 4.32 vs owners' 3.04") are not strictly
   cardinally comparable across the two models, even though the
   within-model MWTP ratio (coefficient ÷ cost coefficient) is scale-free
   and remains valid for the owner/renter comparison. Every place that
   leaned on "renters' coefficients are as large as or larger than owners',
   therefore not weaker preferences" has been reworded to the safer,
   scale-agnostic framing: the MWTP gap "arises primarily through the
   estimated cost trade-off." A caveat paragraph explaining this was also
   added to Limitations (extending the existing MWTP-mixing point). Since
   the renter/owner argument is now central to the paper, it's worth a
   second pair of eyes on this — I'm confident the reasoning is right, but
   it changes language across the Abstract, Results, Discussion, and
   Conclusion, so it's the one item on this list I'd actually call
   substantive rather than cosmetic.

## Resolved since the last note

- **Class labels propagated everywhere.** Table 4 and Supplementary Table
  S5 in the live docx, and the hardcoded labels in `scripts/paper/figures.R`
  and `scripts/paper/lc_plot.R` (driving Figures 5–6), now all say
  Shop-oriented / Green-space-oriented / Price-sensitive shop-oriented /
  Parking-oriented, matching `manuscript.md`. Figures regenerated and
  spot-checked.
- **Schulz, Gross, & Teti (2025) reference removed.** It was incomplete (no
  journal, volume, pages, or DOI) and I have no way to verify or complete
  it. Removed from the in-text citation (Introduction, in the "latent class
  approaches" cluster alongside Caplan and Aitken, which still stands
  without it) and from the reference list (39 references now, was 40). If
  you have the actual publication details, it can go back in.
- **Methods "Overview" subsection removed**, per your clarification of item
  25. Its paragraph now sits directly under "Research Design and Methods"
  with no subheading, and its closing "contribution" sentence (which
  repeated the Introduction) was cut. Methods now opens straight into
  "Using data from a DCE embedded in the Prospective RELOC-AGE project...
  the analysis proceeded in two stages."
- **"Willingness to pay" hyphenation and neighbourhood spelling
  standardized.** Unhyphenated when used as a noun ("estimated willingness
  to pay"), hyphenated only where it's a genuine compound adjective before
  a noun ("willingness-to-pay estimates," "willingness-to-pay gaps"). One
  stray "neighborhood" (US spelling) in the Innovation and Translational
  Significance paragraph fixed to "neighbourhood," matching the British
  spelling used throughout the rest of the manuscript (ageing, modelling,
  favouring, etc.).

## Changelog — major substantive corrections made earlier in this revision

- **MWTP scaling reverted from mean back to median.** An earlier same-session
  edit had switched the latent-class WTP calculations from median to mean,
  based on mistakenly checking `interaction_regs_table.R` (a different
  table) instead of `baseline_regs.R` (the actual source of Table 3, which
  uses tenure-specific medians: owners 10,000 SEK, renters 9,000 SEK).
  Reverted across all affected scripts (`latent_class_analysis.R`,
  `lc_results.R`, `_save_lc_word_tables.R`, `figures.R`, `lc_plot.R`) and
  the live docx Table 4 + abstract text.
- **Interaction-effect misinterpretation found and fixed in Results.** The
  original manuscript's interaction-model paragraph ("136 vs 15 SEK," "164
  vs 47 SEK," "624 vs 550 SEK," etc.) was reporting each interaction
  coefficient's own MWTP-equivalent value as if it were the second
  subgroup's total MWTP. It isn't: the interaction term is only the
  incremental effect on top of the base/reference coefficient, and
  combining them isn't a simple sum because MWTP is a nonlinear ratio. The
  Results paragraph now reports interaction coefficients and their
  significance directly instead.
- **Age-interaction variable corrected.** Methods originally (mistakenly)
  described the age interaction as a three-category grouping
  (55–64/65–74/75+), sourced from `interaction_regs.R`. The actual results
  in Supplementary Table S1 come from `interaction_regs_age.R`, which uses
  a binary age≥72 split. Methods now describes the binary split; the
  three-category grouping is still correctly used elsewhere (the
  latent-class demographic profiles in Table S5).
- **9 references dropped as unused** (49 → 39 after the Schulz removal
  above): Cohen-Cline et al. (2015), Landby & Godtman Kling (2025), Lucas
  et al. (2016), Maas (2006), Nielsen & Hansen (2007), Paquet et al.
  (2013), Ricciardi et al. (2015), Scheiner & Holz-Rau (2017), Sturm &
  Cohen (2014). Checked against every citation actually used in the
  finished draft, not just the ones flagged along the way.
- **Tenure/income conflation fixed** in multiple places (Discussion,
  Conclusion, and earlier in the live docx): language like "lower-income
  older adults" and "income gradients" replaced with framing around price
  sensitivity, since the tenure model doesn't directly identify income.
- **"Budget-constrained" class label renamed** to "Price-sensitive
  shop-oriented" (the model identifies price sensitivity, not financial
  constraint per se).
- **Class count: adopted LC-4 over LC-3.** A multi-start refit found the
  original single-start BFGS fits had converged to meaningfully worse local
  optima, especially for the 4-class model; once refit, LC-4 beat LC-3 on
  AIC/BIC/CAIC. Original single-start fits kept as
  `output/models/lc_*class_ORIGINAL_localopt.rds` for reference.
