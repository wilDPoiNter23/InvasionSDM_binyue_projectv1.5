"""Recalculate descriptive SDM evaluation summaries from archived exports."""

import argparse
from pathlib import Path

import numpy as np
import pandas as pd


def describe(values):
    values = pd.Series(values).dropna()
    return {
        "n": len(values),
        "median": values.median(),
        "q25": values.quantile(0.25),
        "q75": values.quantile(0.75),
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--all-models", required=True, type=Path)
    parser.add_argument("--selected-models", required=True, type=Path)
    parser.add_argument("--species-summary", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()

    all_models = pd.read_csv(args.all_models)
    selected = pd.read_csv(args.selected_models)
    species = pd.read_csv(args.species_summary)
    assert len(species) == species.accepted_taxon.nunique() == 813
    assert set(species.selection_tier.unique()) == {"PRIMARY", "FALLBACK", "BELOW_THRESHOLD"}

    individual = all_models[
        all_models.Type.eq("individual")
        & all_models["metric.eval"].isin(["TSS", "ROC"])
    ].copy()
    raw_validation = individual[individual.validation.notna()]
    paired = individual.pivot_table(
        index=["Species", "full.name"],
        columns="metric.eval",
        values="validation",
        aggfunc="max",
    ).dropna(subset=["TSS", "ROC"])
    assert paired.index.get_level_values("Species").nunique() == 784
    per_species_median = paired.groupby(level="Species")[["TSS", "ROC"]].median()

    raw_max = paired.groupby(level="Species")[["TSS", "ROC"]].max()
    from_comprehensive = species[species.data_source.eq("comprehensive_eval_CSVs")]
    comparable = from_comprehensive.set_index("accepted_taxon").join(raw_max)
    assert len(comparable) == 719
    assert np.allclose(comparable.best_TSS, comparable.TSS)
    assert np.allclose(comparable.best_ROC, comparable.ROC)
    same_max_model = 0
    for _, group in paired.loc[comparable.index].groupby(level="Species"):
        tss_models = set(group.index[group.TSS.eq(group.TSS.max())])
        roc_models = set(group.index[group.ROC.eq(group.ROC.max())])
        same_max_model += bool(tss_models & roc_models)

    cv = all_models[
        ~all_models.run.isin(["allRun", "allData"])
        & all_models["metric.eval"].isin(["TSS", "ROC"])
        & all_models.validation.notna()
    ]
    proxy = cv.groupby(["Species", "metric.eval"]).validation.mean().rename("proxy")
    plot = selected[selected["metric.eval"].isin(["TSS", "ROC"])].join(
        proxy, on=["Species", "metric.eval"]
    )
    plot["score"] = plot.validation.fillna(plot.proxy).fillna(plot.calibration)
    plot_positive = plot[plot.score.gt(0)]

    lines = [
        "# FULL model-performance recheck",
        "",
        "The archived exports were read without modifying models or figures. ROC is the archived area-under-curve metric. All summaries below are descriptive and retain their stated evaluation unit.",
        "",
        "| Source and unit | TSS median [Q1, Q3] | ROC/AUC median [Q1, Q3] |",
        "| --- | ---: | ---: |",
    ]
    summaries = [
        ("All archived candidate validation rows (17,513 rows per metric; repeated model names)",
         describe(raw_validation.loc[raw_validation["metric.eval"].eq("TSS"), "validation"]),
         describe(raw_validation.loc[raw_validation["metric.eval"].eq("ROC"), "validation"])),
        ("Unique individual model names with paired validation metrics (8,277 models)",
         describe(paired.TSS), describe(paired.ROC)),
        ("Median candidate validation score per species (784 species)",
         describe(per_species_median.TSS), describe(per_species_median.ROC)),
        ("Maximum available candidate validation score per species (813 species; separate maxima)",
         describe(species.best_TSS), describe(species.best_ROC)),
        ("Original figure-b proxy after excluding nonpositive scores (869 selected-output rows before filtering)",
         describe(plot_positive.loc[plot_positive["metric.eval"].eq("TSS"), "score"]),
         describe(plot_positive.loc[plot_positive["metric.eval"].eq("ROC"), "score"])),
    ]
    for label, tss, roc in summaries:
        lines.append(
            f"| {label} | {tss['median']:.3f} [{tss['q25']:.3f}, {tss['q75']:.3f}], n={tss['n']} | "
            f"{roc['median']:.3f} [{roc['q25']:.3f}, {roc['q75']:.3f}], n={roc['n']} |"
        )
    lines += [
        "",
        "Archived candidate-run validation by algorithm (row-weighted):",
        "",
        "| Algorithm | TSS median (n) | ROC/AUC median (n) |",
        "| --- | ---: | ---: |",
    ]
    for algorithm in ["GLM", "MAXNET", "RFd"]:
        group = raw_validation[raw_validation.algo.eq(algorithm)]
        tss = describe(group.loc[group["metric.eval"].eq("TSS"), "validation"])
        roc = describe(group.loc[group["metric.eval"].eq("ROC"), "validation"])
        lines.append(f"| {algorithm} | {tss['median']:.3f} ({tss['n']}) | {roc['median']:.3f} ({roc['n']}) |")
    lines += [
        "",
        f"The 813-species table has {species.selection_tier.value_counts()['PRIMARY']} PRIMARY, "
        f"{species.selection_tier.value_counts()['FALLBACK']} FALLBACK and "
        f"{species.selection_tier.value_counts()['BELOW_THRESHOLD']} BELOW_THRESHOLD labels.",
        f"This final 813-species set combines {species.source_generation.value_counts()['ORIGINAL_FROZEN']} original frozen entries and {len(species) - species.source_generation.value_counts()['ORIGINAL_FROZEN']} later direct or repaired entries; it is not one homogeneous fitting batch.",
        f"For all {len(comparable)} comprehensive-export species, the staged `best_TSS` and `best_ROC` exactly match the separate maximum validation scores among candidate single models. Their maxima come from the same model for {same_max_model} species and from different models for {len(comparable) - same_max_model}. They are not paired evaluations of a final projected model.",
        f"In the selected-output export, {selected[selected['metric.eval'].eq('TSS')].validation.notna().sum()} TSS rows have direct validation values, all from individual models; the remaining {selected[selected['metric.eval'].eq('TSS')].validation.isna().sum()} use the original figure script's species-wide candidate average as a proxy. Ensemble validation is absent in this export.",
        "The original figure-b median of approximately TSS 0.652 and ROC/AUC 0.909 is reproduced by this proxy-and-positive-filter procedure. It must not be labelled cross-validated performance of final selected ensembles.",
        "The original figure-a panel contains only GLM, MAXNET and RFd validation distributions; the accompanying caption's claim that it compares ensemble performance is unsupported by the displayed panel.",
        "Recalculation from archived scores cannot replace de novo ROC/TSS evaluation from held-out labels and predictions; those observations and prediction vectors are not contained in these exports.",
        "",
        "Inputs: `03_All_Models_evaluationCombined.csv`, `04_All_Best_Models_Combined.csv`, `species_model_performance_813_final.csv`. Script: `code/09_figures/recheck_full_model_performance.py`.",
        "",
    ]
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text("\n".join(lines), encoding="utf-8")
    print(args.output)


if __name__ == "__main__":
    main()
