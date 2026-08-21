# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

R-based discrete choice experiment (DCE) analysis for a paper on housing/relocation preferences among older adults in Sweden (RELOC-AGE cohort), targeting *Innovation in Aging*. This is a data-analysis project, not a software package — there is no build system, linter, or test suite. Correctness is verified by running scripts end-to-end in R (RStudio, `Rscript`, or the `r-studio` MCP tools) and inspecting the resulting tables/figures.

## Running the analysis

- Open via `ahrg_dce_survey.Rproj` so `here::here()` resolves to the repo root; scripts use `here()` for all paths, not paths relative to the script's own location.
- Package management is via `pacman::p_load(...)` at the top of each script (installs-if-missing + loads).
- No single pipeline runner — each script in `scripts/paper/` is run independently with `source()`, in dependency order: model-fitting scripts (`latent_class_analysis.R`, `baseline_regs.R`, `interaction_regs*.R`) save to `output/models/*.rds`, then downstream scripts (`lc_results.R`, `figures.R`, `lc_plot.R`, `_save_*_table.R`) load those `.rds` files to build tables/figures. Re-run the fitting scripts first if a model's coefficients or the WTP scaling logic changes.
- `data/` is gitignored (real respondent data). The canonical analysis dataset is `data/formr/df_model.rds`; `data/formr/` also holds many intermediate/legacy survey-collection snapshots not used by the paper scripts.

## Architecture: script → output mapping

- `scripts/paper/latent_class_analysis.R` — fits MNL + latent class (LC-2/3/4) models via `gmnl`, saves to `output/models/lc_*.rds`. Uses `fit_lc_multistart()` (multi-start BFGS, keeps the best converged log-likelihood) for LC-2/3/4 — **do not revert to single-start fitting**; an audit found single-start BFGS converges to meaningfully worse local optima for Q=3 and especially Q=4 (see below).
- `scripts/paper/lc_results.R` — loads the saved LC models, produces console diagnostics and LaTeX tables (`paper/tex/tables/lc_*.tex`).
- `scripts/paper/_save_lc_word_tables.R` / `scripts/paper/_save_profiles_table.R` — Word-ready HTML versions of the same LC tables (`paper/word/tables/lc_*.html`) for pasting into the manuscript. `_save_profiles_table.R` auto-derives class labels from each class's own coefficients rather than hardcoding persona names, so labels can't silently go stale if the models are refit.
- `scripts/paper/figures.R` — all 6 main-text figures (`output/figures/`, mirrored to `paper/tex/figures/`). `scripts/paper/lc_plot.R` is a standalone version of the LC WTP figure.
- `scripts/paper/baseline_regs.R`, `interaction_regs*.R`, `interaction_regs_table.R` — tenure-stratified mixed logit (MXL) models and demographic-interaction models; source of Table 3 and the interaction results.
- `scripts/paper/descriptive_table.R` — sample characteristics (Table 2).
- `scripts/formr/` and `scripts/archive/` — data collection/cleaning for the original formr survey platform, upstream of `df_model.rds`; not part of the paper-generation pipeline.
- `paper/tex/` — LaTeX tables/figures, kept in sync but not the primary submission target (see below).
- `paper/word/innovation_aging/` — the actual manuscript docx files (`main_document_innovation-in-aging.docx`, `supplementary_material.docx`) plus the Word-ready HTML tables in `paper/word/tables/`. **This is the primary deliverable format**; the author does not intend to submit in LaTeX.
- `paper/word/innovation_aging_revision/manuscript.md` — active markdown working draft for a peer-review-driven revision, edited section by section before being ported back into the docx.

## Conventions specific to this paper (load-bearing — do not silently change)

- **MWTP scaling**: always 10% of the sample **mean** planned monthly housing cost, not median. This was inconsistent across scripts until an audit fixed it (2026-08-21); mean is correct because it matches the Methods text and `interaction_regs_table.R`'s Table 3 convention.
- **Class count**: the paper uses the **4-class** (LC-4) latent class solution, not 3-class, after a multi-start refit showed LC-4 wins on AIC/BIC/CAIC. The original single-start (locally-optimal, inferior) fits are kept as `output/models/lc_3class_ORIGINAL_localopt.rds` / `lc_4class_ORIGINAL_localopt.rds` for reference only.
- **Class labels** (order = the model's internal `class.1..4` numbering): 1 = Shop-seekers, 2 = Nature-seekers, 3 = Price-sensitive shop-seekers, 4 = Car-centred. Class 3 was renamed from "Budget-constrained" — the model identifies price sensitivity, not financial constraint, and that overclaim was flagged in peer review.
- **Terminology in manuscript prose**: "underlying preference profiles", not "unobserved preference segments" or "latent preference segments".
- **Tense**: past tense for describing this study's own methods/aims (standard for *Innovation in Aging* and gerontology journals generally); general background claims and literature-gap statements stay present tense.
- **Punctuation**: no em-dashes in manuscript prose.

## Current paper-revision state

The manuscript is being revised section by section against reviewer feedback, in `paper/word/innovation_aging_revision/manuscript.md` (not yet merged back into the docx). Status: the Introduction ("Background and Objectives") has been rewritten and iterated with the author — done. Methods is next. The reviewer's central critique was that the latent-class analysis read as an afterthought rather than a stated research aim; the Introduction was restructured around a two-step heterogeneity question (how much do observable characteristics explain, then do underlying preference profiles explain more) to fix this. When Methods is rewritten, it should make that same two-step logic explicit in how the analysis strategy is described, not just report the models in sequence.

Backup copies of the docx files from before the LC-4 pivot and before the review-fix pass exist as `*.PRE_LC4.bak.docx` / `*.PRE_REVIEWFIXES.bak.docx` in `paper/word/innovation_aging/` — local safety copies only, not alternate canonical versions.
