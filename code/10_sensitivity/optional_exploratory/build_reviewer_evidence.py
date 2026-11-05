#!/usr/bin/env python3
"""Build evidence-level reports from saved sensitivity results and reviewer IDs."""
import argparse
import csv
import hashlib
import json
from datetime import datetime, timezone
from pathlib import Path

parser = argparse.ArgumentParser()
parser.add_argument("output_root", type=Path)
args = parser.parse_args()
root = args.output_root.resolve()
if root.name != "reviewer_evidence_v1":
    raise SystemExit("The declared article evidence root is required")
writer = "code/10_sensitivity/optional_exploratory/build_reviewer_evidence.py"
sources = {}
outputs = []


def read(rel, optional=False):
    path = root / rel
    if not path.exists():
        if optional:
            return []
        raise FileNotFoundError(path)
    sources[rel] = hashlib.sha256(path.read_bytes()).hexdigest()
    with path.open(encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def num(row, field, digits=4):
    try:
        value = float(row[field])
        return f"{value:.{digits}g}"
    except (ValueError, KeyError, TypeError):
        return "not available"


def mdtable(rows, fields):
    return "| " + " | ".join(fields) + " |\n| " + " | ".join("---" for _ in fields) + " |\n" + "\n".join("| " + " | ".join(str(r.get(f, "")).replace("|", "/").replace("\n", " ") for f in fields) + " |" for r in rows)


def cite(*paths):
    return "<!-- Evidence: " + "; ".join(paths) + " -->"


def write_md(rel, body):
    path = root / rel
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(body.strip() + "\n\n<!-- Report writer: " + writer + "; inputs and hashes: 07_integrated_evidence/report_build_inputs.csv; report is an author-review draft, not a manuscript edit. -->\n", encoding="utf-8")
    outputs.append(rel)


def write_csv(rel, rows, fields=None):
    path = root / rel
    path.parent.mkdir(parents=True, exist_ok=True)
    fields = fields or list(rows[0])
    with path.open("w", encoding="utf-8", newline="") as handle:
        w = csv.DictWriter(handle, fieldnames=fields, extrasaction="ignore")
        w.writeheader()
        w.writerows(rows)
    outputs.append(rel)


rp = "01_environment_only/tables/raster_comparisons.csv"
hp = "01_environment_only/tables/historical_raster_comparisons.csv"
cp = "01_environment_only/tables/three_response_city_extraction.csv"
ap = "01_environment_only/tables/paired_excluded_attrition.csv"
gp = "02_gam_robustness/tables/model_comparison.csv"
sp = "02_gam_robustness/tables/sign_change_intervals.csv"
bp = "02_gam_robustness/tables/bootstrap_summary.csv"
np = "04_nested_spatial_contrasts/tables/city_background_statistics.csv"
mp = "05_spatial_diagnostics/tables/moran_permutation_results.csv"
rr = read(rp)[0]
historical = read(hp)
cities = read(cp)
attrition = read(ap)
models = read(gp)
signs = read(sp)
boot = read(bp)
nested = read(np)
moran = read(mp)
review = read("00_project_control/reviewer_analysis_map.csv")
if len(review) != 52 or len({r["comment_id"] for r in review}) != 52:
    raise SystemExit("Expected exactly 52 unique authoritative reviewer IDs")
formulas = read("02_gam_robustness/qc/model_definitions.csv")
reproduction = read("02_gam_robustness/qc/historical_reproduction.csv")
roster = read("03_sampling_selection/qc/roster_identity_check.csv")
geo_path="03_sampling_selection/tables/leave_continent_out_models_geography_verified.csv"
geo_scan_path="03_sampling_selection/tables/leave_continent_out_endpoint_scans_geography_verified.csv"
geo = read(geo_path, True)
geo_scan = read(geo_scan_path, True)
geo_receipt = read("03_sampling_selection/qc/geographic_grouping_receipt.csv", True)
country_crosswalk=read("03_sampling_selection/qc/geographic_crosswalk_verified.csv",True)
sem = read("06_optional_exploratory/tables/sem_input_provenance.csv")
age = read("06_optional_exploratory/tables/reclamation_age_availability.csv")
age_attributes = read("06_optional_exploratory/tables/reclamation_age_attribute_inspection.csv", True)
zone = read("04_nested_spatial_contrasts/qc/final_city_zone_union_equivalence.csv", True)
performance = read("01_environment_only/tables/paired_selected_performance_summary.csv", True)
accepted_performance = read("01_environment_only/tables/accepted_model_performance_v3.csv", True)
performance_coverage=read("01_environment_only/qc/paired_performance_coverage.csv",True)
concordance = read("01_environment_only/qc/historical_city_response_concordance_summary.csv", True)
g0 = next(r for r in models if r["response"] == "HISTORICAL_MAIN" and r["model"] == "G0")
interval = next(r for r in signs if r["response"] == "HISTORICAL_MAIN" and r["model"] == "G0" and r["support"] == "whole108_range")
rings = [r for r in nested if r["baseline"] == "ring_background_mean"]
unmatched = [r["city"] for r in roster if r["matched"].lower() != "true"]
paired_flips = sum(int(r["flips"]) for r in signs if r["response"].startswith("PAIRED_") and r["support"] == "whole108_range")
def geographic_bracket(response,continent):
    rows=sorted([r for r in geo_scan if r['response']==response and r['model']=='G0' and r['excluded_continent']==continent and r['support']=='whole108_range'],key=lambda r:float(r['quantile']))
    flips=[]
    for left,right in zip(rows,rows[1:]):
        if float(left['endpoint_difference'])*float(right['endpoint_difference'])<0:
            flips.append((left['city_area_km2'],right['city_area_km2']))
    return flips

fact = {}
fact["environment_only_rasters"] = f"""# Frozen paired raster comparison

The frozen set contains 684 paired taxa and 130 exclusions. Each paired product is a single-band global 4320 by 8640 WGS84 raster at 2.5 arcminutes. Only cells with finite values in both means enter the comparison: n = {int(rr['finite_cells']):,}. Suitability uses the recorded BIOMOD divisor of 1000; the shared species-cell count determines both means. Input SHA256 values and actual comparisons are recorded in `01_environment_only/qc/verified_source_checksums.csv`.

Pearson r = {num(rr,'pearson',10)}; Spearman rho = {num(rr,'spearman',10)}; MAE = {num(rr,'MAE',10)}; RMSE = {num(rr,'RMSE',10)}. FULL mean = {num(rr,'reference_mean',10)}, ENV mean = {num(rr,'comparison_mean',10)}, and mean ENV minus FULL = {num(rr,'comparison_minus_reference',10)}.

Sources: `{rp}`; `01_environment_only/tables/paired_difference_quantiles.csv`; `01_environment_only/model_objects/paired_env_minus_full.tif`. Direct checks of the saved difference raster and reproduction of the accepted paired summary are in `01_environment_only/qc/independent_saved_product_checks.csv`.
"""
fact["historical_and_city_responses"] = f"""# Historical and final-city responses

Final city extraction contains {len(cities)} unique cities and three separate response labels. Exact covered-cell fractions were used for the native-grid city means. Two invalid polygons were repaired in memory; the original city files were preserved. The maximum absolute error against the accepted paired city means was 2.31314967180651e-13. Sources: `{cp}` and `01_environment_only/qc/paired_acceptance_checks.csv`.

{mdtable(historical,['reference','comparison','finite_cells','pearson','spearman','MAE','RMSE'])}

Source: `{hp}`. These are raw-scale descriptive comparisons; the historical normalization, species set and domain differ from the paired products.
"""
fact["species_attrition_and_performance"] = f"""# Species attrition and evaluation availability

The attrition table contains 684 paired and 130 excluded taxa. No excluded taxon was retried, substituted or assigned a zero prediction. The frozen manifest records 676 ordinary FULL/ENV pairs and eight CLEAN_FULL/CLEAN_ENV pairs.

{mdtable(attrition,['metric','paired_available','excluded_available','paired_mean','excluded_mean','standardized_mean_difference'])}

Source: `{ap}`. Standardized differences use the mean difference divided by the square root of the mean group variances. Historical FULL TSS/ROC summaries are used only for the attrition diagnostic, not as matched CLEAN_FULL or ENV evaluations. Accepted selected-route performance export is {'present at `01_environment_only/tables/paired_selected_performance_summary.csv` and needs its route/partition coverage read with the export QC' if performance else 'pending; no paired evaluation test is claimed in this report snapshot'}.
"""
fact["gam_robustness"] = f"""# GAM reproduction and parameterisations

Each of the twelve main fits uses the same 84 complete cities and REML. Historical G0 reproduced adjusted R2 = {num(g0,'adj_r2',8)}, deviance explained = {num(g0,'deviance_explained',8)}, AIC = {num(g0,'AIC',8)}, focal edf = {num(g0,'focal_edf',8)}, F = {num(g0,'focal_F',8)}, and P = {num(g0,'focal_p',8)}.

{mdtable(formulas,['model','formula'])}

{mdtable(models,['response','model','n','adj_r2','deviance_explained','AIC','maximum_estimated_concurvity'])}

Sources: `{gp}`; `02_gam_robustness/qc/model_definitions.csv`; `02_gam_robustness/qc/historical_reproduction.csv`. Diagnostic and influence tables are retained in `02_gam_robustness/tables/`.
"""
fact["sign_change_and_bootstrap"] = f"""# Endpoint sign scans and bootstrap

Historical G0 has a sign change bracket from {num(interval,'lower_km2',9)} to {num(interval,'upper_km2',9)} km2, corresponding to the reported P85-P90 scan interval. The lower endpoint is not an estimated breakpoint. Historical G1-G3 and all paired FULL/ENV formulations have no whole-range endpoint sign change in the fixed scan. The paired sign-change count is {paired_flips}.

{mdtable(boot,['model','replicates','successful','any_flip','fraction_any_flip','flip_fraction_lower','flip_fraction_upper'])}

Sources: `{sp}`; `{bp}`; `02_gam_robustness/tables/endpoint_scan.csv`. Bootstrap results use only the historical response, 30 replicates and the pre-recorded seed; their Monte Carlo precision is limited.
"""
fact["sampling_and_geography"] = f"""# City selection and geographic sensitivity

The original-pool name reconciliation has {len(unmatched)} unmatched final names: {', '.join(unmatched)}. Retained-versus-excluded covariate comparisons and sampling-effort adjustment remain uncomputed. Source: `03_sampling_selection/qc/roster_identity_check.csv`.

The derived geographic grouping corrects Los Angeles from South America to North America using independent Country=USA, Regions=Northern America metadata and matching coordinates. Original input files are unchanged. The authoritative geographic fit table is `03_sampling_selection/tables/leave_continent_out_models_geography_corrected.csv`, containing {len(geo)} fits. North America has two omitted cities and falls below the minimum three-city omitted-group rule; excluding Asia leaves 32 complete cities and falls below the 50-city fit rule. These contrasts were not fitted. Source: `03_sampling_selection/qc/module_status_geography_corrected.csv`.

{mdtable(geo,['response','model','subset','n','adj_r2','deviance_explained','AIC'])}
"""
fact["city_background_and_zone_lineage"] = f"""# Boundary-updated city/background contrasts

The final 108 cities were projected to EPSG:6933. Each response raster was reprojected with bilinear interpolation using the existing framework. Full buffers extend 50 km from each city; rings remove all city interiors. Paired t tests, Wilcoxon tests, standardized paired effects and percentile city-bootstrap intervals were calculated. Bootstrap replicates = 1999; seed = 20261014.

{mdtable(rings,['response','n_cities','mean_difference','percent_difference','percent_CI_low','percent_CI_high','paired_t','t_p','cohens_dz'])}

Source: `{np}`. This is a final-boundary sensitivity; the current original city vector has 107 cities and lacks Tripoli, so exact reproduction of the historic footprint is not claimed. The full-buffer baseline is retained in the same table.

The legacy reclaimed/non-reclaimed geometry union does not consistently match the final city boundaries. Major footprint differences and topology exceptions are recorded in `04_nested_spatial_contrasts/qc/final_city_zone_union_equivalence.csv`. The five-city patch is not a full-city zone dataset. No within-city reclaimed/non-reclaimed estimate was regenerated.
"""
fact["spatial_and_optional_checks"] = f"""# Spatial diagnostics and optional inputs

The residual diagnostics contain {len(moran)} response/formulation/neighbourhood tests with 499 permutations and a recorded seed. They are exploratory and unadjusted for multiple testing. At the G0 symmetric four-neighbour definition:

{mdtable([r for r in moran if r['model']=='G0' and r['neighbourhood']=='symmetric_knn_4'],['response','n','Moran_I','permutation_p','permutations'])}

Source: `{mp}`. These tests do not establish absence of spatial dependence under other neighbourhoods.

Two archived SEM input candidates were inspected; neither is verified as the original fit input and preprocessing chain. No replacement SEM was fitted. The available period-named reclamation layers have no verified per-polygon age provenance; no age model was fitted. Sources: `06_optional_exploratory/tables/sem_input_provenance.csv`; `06_optional_exploratory/tables/reclamation_age_availability.csv`; `06_optional_exploratory/qc/optional_analysis_status.csv`.
"""
if accepted_performance:
    fact['species_attrition_and_performance'] += '\nAuthoritative selected-route metadata: `01_environment_only/tables/accepted_model_performance_v3.csv`. Superseded initial exports are not used. Across the fixed species set, 1,267 of 1,368 species/group units map exactly; 101 are partial. Calibration, validation and external evaluation labels remain separate. The eight CLEAN_FULL units have no projection-level evaluation metrics and are not substituted with historical FULL summaries.\n'
if age_attributes:
    fact['spatial_and_optional_checks'] += '\n\n' + mdtable(age_attributes, ['n_features', 'attribute_columns', 'recorded_codes', 'verified_calendar_year_dictionary', 'verified_global_age_scope']) + '\n\nExisting attribute tables contain PERIOD codes; their calendar-year dictionary, temporal source and final spatial scope remain unverified. These codes were not converted into invented ages. Source: `06_optional_exploratory/tables/reclamation_age_attribute_inspection.csv`.\n'
if performance:
    fact['species_attrition_and_performance'] += '\n\n' + mdtable(performance,['metric','evaluation_stage','route_stratum','n_pairs','mean_env_minus_full','difference_CI_low','difference_CI_high','paired_t_p','wilcoxon_p','cohens_dz']) + '\n\nSource: `01_environment_only/tables/paired_selected_performance_summary.csv`; bootstrap replicates 1999, seed 20261014. Same-unit ensemble calibration pairs number 404; only 280 also match the ensemble algorithm label. Same-unit individual-model pairs number 19 in calibration and validation, with 11 also matching the algorithm label. No ensemble validation or external-evaluation pairs are available. Calibration results are descriptive training-partition summaries; selected algorithms and CV runs can differ, so these tests do not establish general predictive superiority or removal of sampling bias. Numerical reproduction is in `01_environment_only/qc/independent_performance_summary_checks.csv`.\n'
if concordance:
    fact['historical_and_city_responses'] += '\nArchived GAM response concordance:\n\n' + mdtable(concordance,['subset','extraction','n','max_abs_difference','mean_abs_difference','correlation','status']) + '\n\nSource: `01_environment_only/qc/historical_city_response_concordance_summary.csv`. The archived GAM risk is preserved. Fresh historical city extraction and the archived response are not numerically concordant; comparisons across archived historical and fresh paired GAM responses have a response-lineage warning.\n'
fact['gam_robustness'] += '\nThe historical fits reproduce the archived city-risk table; they do not demonstrate concordance with fresh historical raster extraction. Matched paired FULL versus ENV models use the same final geometry and common city cohort. Historical-versus-paired GAM interpretation remains source-concordance blocked.\n'
fact['sign_change_and_bootstrap'] += '\nG1 and G2 are additive in the focal reclamation term: absence of a city-area-dependent sign flip follows their model construction and is not an independent test against interaction. Tensor G0 versus tensor G3, matched paired responses, bootstrap and geography provide the relevant transition sensitivity checks. A no-flip scan does not imply no reclamation association.\n'
europe=[r for r in geo if r['subset']=='exclude_Europe' and r['model']=='G0']
if europe:
    fact['sampling_and_geography'] += '\nEurope-excluded G0 fits:\n\n' + mdtable(europe,['response','n','focal_F','focal_p']) + '\n\nHistorical and paired FULL G0 sign changes occur after Europe exclusion, whereas paired ENV does not exhibit a sign change in the recorded scan. Joint tensor support remains distinct from endpoint sign-change behaviour. The pooled paired FULL result therefore does not establish geographic invariance.\n'
if geo:
    changed=[r['city'] for r in country_crosswalk if r['continent_changed'].lower()=='true']
    fact['sampling_and_geography']='# City selection and verified geography\n\nThe original-pool identity reconciliation has '+str(len(unmatched))+' unmatched final names: '+', '.join(unmatched)+'. Retained/excluded covariate and effort comparisons remain blocked. Separately, the final-country crosswalk covers '+str(len(country_crosswalk))+' cities and corrects derived continent assignments for '+', '.join(changed)+'. Original city-response/covariate inputs and model coordinates were preserved. Crosswalk identity evidence, coordinate warnings and source checksums are recorded in `03_sampling_selection/qc/geographic_crosswalk_verified.csv`.\n\nThe country-verified targeted geographic fit table contains '+str(len(geo))+' fits:\n\n'+mdtable(geo,['response','model','subset','n','adj_r2','deviance_explained','focal_p'])+'\n\nSource: `'+geo_path+'`; endpoint scans: `'+geo_scan_path+'`. Superseded initial and first-correction geographic tables are excluded from quantitative interpretation. The pre-specified support rules and affected subsets are recorded with the geographic module QC.\n'
if not geo:
    fact['sampling_and_geography']='# City selection and geographic verification\n\nThe original-pool identity reconciliation has '+str(len(unmatched))+' unmatched final names: '+', '.join(unmatched)+'. Retained/excluded covariate and effort comparisons remain blocked. Country verification found erroneous continent assignments for Los Angeles and Caribbean cities in the archived covariate table. Original inputs were preserved. The first corrected geographic tables remain superseded/provisional until all city countries are verified and the affected Europe/North America subsets are rerun. No prior Europe-exclusion sample size or sign-change conclusion is treated as final here. Sources: `03_sampling_selection/qc/roster_identity_check.csv` and the geographic grouping receipts.\n'
if geo:
    for continent in sorted({r['excluded_continent'] for r in geo_scan}):
        for response in ['HISTORICAL_MAIN','PAIRED_FULL_684','PAIRED_ENV_684']:
            fact['sampling_and_geography']+='\n'+continent+'-excluded '+response+' G0 point-estimate sign-change brackets (km2): '+str(geographic_bracket(response,continent))+'.\n'
    fact['sampling_and_geography']+='\nEndpoint scans retain their uncertainty intervals and extrapolation flags. A point-estimate sign change does not imply an interval excluding zero or a fitted ecological breakpoint. Paired ENV has an Africa-excluded sign change despite none in its pooled scan.\n'
if performance:
    fact['species_attrition_and_performance'] += '\nThe paired tests are exploratory and unadjusted for the number of route/partition/metric summaries. Bootstrap intervals describe selected-metric differences across species, not uncertainty in future geographic transfer.\n'
for name, body in fact.items():
    write_md(f"text_results/01_verified_facts/{name}.md",body)
write_md("text_results/01_verified_facts/00_all_verified_results.md","# Verified numerical results\n\n"+"\n\n".join(fact.values()))

interpretations = {
"environment_only_interpretation.md": "Direct support: the matched paired FULL city/ring excess is attenuated under ENV despite high global rank agreement. This sensitivity does not by itself decompose the historical 122% contrast or isolate predictor removal from recorded model-route differences. Alternatives remain: spatial sampling, model route, aggregation/domain and covariate dependence. ENV suitability is not an independent observation of invasion, propagule supply or a causal reclamation effect.",
"gam_robustness_interpretation.md": "Direct support: historical G0 is numerically reproduced, but its sign change is absent from the alternative formulations and both paired responses. The historical bootstrap recovers a G0 sign change in 21 of 30 replicates; this is uncertainty, not evidence for an exact ecological breakpoint. Interpretation: the scale-transition narrative depends on parameterisation and response construction. Its absence in paired FULL as well as paired ENV prevents attributing the change solely to human-predictor removal. Mathematical coupling and sparse high-area support remain plausible explanations.",
"sampling_interpretation.md": "Direct support: geographic exclusion fits are available for supported groups after the evidence-backed derived Los Angeles correction. The original 135-city selection comparison and effort control remain blocked. Geographic exclusion cannot eliminate unequal GBIF sampling or response-dependent city selection. Small omitted or remaining samples prevent stable North America and Asia exclusion claims. Geographic patterns should be presented with their subset sizes and uncertainty rather than as proof of global generality.",
"nested_contrasts_interpretation.md": "Direct support: the final-boundary city/background association remains positive under paired ENV, with marked attenuation relative to paired FULL. The historical final-boundary estimate is a new footprint sensitivity, not exact reproduction of the original 122%. The 39% within-city claim has not been regenerated because the reclaimed/non-reclaimed geometry versions are unresolved. These are different estimands; their percentages cannot be combined, subtracted or treated as opposing estimates of one common effect.",
"integrated_interpretation.md": "The evidence supports a bounded description of predicted-suitability associations across spatial baselines. It requires weaker claims about an invariant scale transition, mechanisms, causal pathways and management leverage. Paired responses retain residual spatial dependence in several tested neighbourhoods. Sampling, timing, response construction and unmeasured development variables remain unresolved alternatives. Technical reproduction is distinct from scientific acceptance; the resubmission story requires author judgement."}
interpretations['gam_robustness_interpretation.md'] += ' G1/G2 cannot exhibit city-size modulation of an additive reclamation smooth by construction; their no-flip results should not be described as independent refutation of an interaction. Geographic exclusion findings require the final country-verified grouping after source continent errors were found. Joint tensor association and endpoint sign-change behaviour are distinct: no-flip is not absence of an association. Historical-versus-paired GAM response interpretation is further restricted by the archived-risk versus fresh-extraction mismatch.'
if not geo:
    interpretations['sampling_interpretation.md']='Direct support: source-country checks identify continent errors for Los Angeles and Caribbean cities. Country-verified geographic exclusion reruns are pending; superseded Europe/North America contrasts must not be interpreted as final. The original 135-city comparison and effort adjustment remain blocked. Correcting grouping metadata cannot eliminate GBIF sampling bias or response-dependent selection. Original input tables are preserved.'
else:
    interpretations['sampling_interpretation.md']='Direct support: country-verified geographic reruns correct Los Angeles and Caribbean continent assignments without changing original model inputs. Europe exclusion retains the archived historical G0 sign change and introduces a paired FULL G0 sign change at a higher area bracket; paired ENV has no Europe-excluded sign change but does have an Africa-excluded sign change. North America exclusion yields no G0 endpoint sign change for any response. Interpretation: sign-transition behaviour is geographically sensitive, alongside response construction and parameterisation. The scans retain uncertainty intervals and extrapolation warnings, so point-estimate signs are not threshold evidence. The original 135-city retained/excluded comparison and effort control remain blocked, and geographic exclusion does not resolve sampling or selection bias. Historical-versus-paired comparisons retain the response-concordance warning.'
interpretations['integrated_interpretation.md'] += ' The archived historical city-risk response is not concordant with fresh final-boundary historical raster extraction. Historical G0 numeric reproduction remains valid for its archived table, but the historical-versus-paired GAM comparison cannot presently be assigned solely to a response-product change. The author must freeze and document the response/table provenance before publication.'
interpretations['environment_only_interpretation.md'] += ' Removal of human predictors does not remove reuse of retained environmental information across the species-level and city-level models. The ENV analysis therefore addresses the specified human-predictor sensitivity and leaves broader response-independence concerns only partly addressed.'
if performance:
    interpretations['environment_only_interpretation.md'] += ' Selected-unit calibration comparisons are largely available for ensemble routes and show descriptive metric differences; matched validation is available only for a small individual-model subset. Calibration, unit type, selected algorithm and evaluation partition remain separate. These summaries do not establish general predictive superiority or eliminate sampling bias.'
for name,body in interpretations.items():
    write_md("text_results/02_scientific_interpretation/"+name,"# Scientific interpretation\n\n"+body+"\n\nSupporting facts: `text_results/01_verified_facts/00_all_verified_results.md`.")

ring_sentence = "; ".join(f"{r['response']}: {num(r,'percent_difference',4)}% (95% city-bootstrap interval {num(r,'percent_CI_low',4)}-{num(r,'percent_CI_high',4)}%)" for r in rings)
methods = f"""We evaluated predictor-removal sensitivity using the accepted paired set of 684 taxa. FULL and environment-only continuous predictions retained their recorded model scales, and each grid cell used only taxa with finite predictions in both groups. The two mean surfaces therefore shared a cell-specific species denominator; missing predictions were not replaced with zeros or FULL predictions. The historical manuscript surface was retained as a separate response. {cite(rp,'01_environment_only/qc/paired_acceptance_checks.csv')}

We extracted the three responses using the final city boundaries and exact covered-cell fractions. For city/background sensitivity, we retained the 50-km buffering framework in an equal-area coordinate system and defined the background ring by excluding city interiors. This analysis uses the final boundary version and is not an exact reproduction of the historic footprint. City-level uncertainty was summarized by percentile bootstrap intervals and paired tests. {cite(cp,np,'04_nested_spatial_contrasts/qc/spatial_definition.csv')}

We compared four pre-specified GAM parameterisations fitted by REML on the same 84 complete cities. These included the reported tensor of log reclamation area and city area, additive log-area smooths, reclamation-fraction and log-city-area smooths, and their tensor alternative. Covariate smooths were retained. We evaluated fit, concurvity, basis-dimension diagnostics, influence, endpoint sign scans and residual spatial dependence. {cite(gp,'02_gam_robustness/qc/model_definitions.csv',mp)}"""
results = f"""The paired FULL and environment-only surfaces were strongly associated across {int(rr['finite_cells']):,} shared finite cells (Pearson r = {num(rr,'pearson',6)}; Spearman rho = {num(rr,'spearman',6)}), although their mean absolute difference was {num(rr,'MAE',6)}. {cite(rp)}

Using the final city boundaries, city means exceeded their surrounding ring means for each response: {ring_sentence}. These percentages are ratios of the aggregate city and background means, and the historical comparison uses a revised footprint. {cite(np)}

The historical tensor GAM was numerically reproduced on 84 complete cities (adjusted R2 = {num(g0,'adj_r2',5)}; deviance explained = {num(g0,'deviance_explained',5)}). Its endpoint scan changed sign between {num(interval,'lower_km2',7)} and {num(interval,'upper_km2',7)} km2. The historical tensor G3 and pooled paired tensor fits did not retain this transition. G1 and G2 impose additivity, so their absent city-area-dependent focal sign transitions follow the model structure and do not independently refute an interaction. The interval endpoints are point-estimate contrasts whose uncertainty must be retained; the lower endpoint is not an exact ecological breakpoint. {cite(gp,sp,'02_gam_robustness/tables/endpoint_scan.csv')}

In the exploratory historical bootstrap, G0 exhibited a sign change in 21 of 30 replicates, compared with zero for G1 and G2 and three for G3. {cite(bp)}"""
discussion = "The paired comparison indicates that the city-scale predicted-suitability contrast is sensitive to anthropogenic predictor inclusion. Its attenuation does not by itself identify a reclamation mechanism, and an environment-only response still reflects species-level modelling and observation coverage. The failure of both paired FULL and ENV formulations to reproduce the historical sign transition also points to response aggregation and species-set differences, alongside parameterisation, rather than a single predictor-removal explanation. "+cite(rp,np,sp)+"\n\nThe city/background and within-city habitat contrasts should be interpreted against their respective baselines. A city may differ from its surroundings while newly reclaimed parcels differ from other land inside that city. The present package verifies the boundary-updated city/background comparison; the within-city numerical claim remains pending geometry reconciliation. "+cite(np,'04_nested_spatial_contrasts/qc/final_city_zone_union_equivalence.csv')
limitations = "These analyses are cross-sectional associations of predicted suitability. They do not resolve the timing of invasion relative to reclamation, unequal occurrence sampling, response-dependent city inclusion or confounding by wider development and transport processes. The retained-versus-excluded city comparison remains incomplete because some identities and excluded-city covariates are unresolved, and no verified effort measure was available. "+cite('03_sampling_selection/qc/roster_identity_check.csv','06_optional_exploratory/tables/reclamation_age_availability.csv')+"\n\nSeveral paired-response residual tests indicate spatial dependence; conventional GAM and city-bootstrap uncertainty should therefore be interpreted alongside the neighbourhood-specific diagnostics. The SEM input and preprocessing lineage are unresolved, and no replacement SEM or age sensitivity was fitted. "+cite(mp,'06_optional_exploratory/qc/optional_analysis_status.csv')
results += '\n\nThe historical GAM reproduction refers to the archived city-response table, which is not concordant with a fresh extraction of the historical raster using the final city boundaries. Accordingly, historical-versus-paired GAM differences cannot yet be interpreted as an isolated change in response construction. The matched paired FULL-versus-ENV comparison retains a common geometry and cohort. '+cite('01_environment_only/qc/historical_city_response_concordance_summary.csv',gp)
if geo:
    results += '\n\nGeographic exclusion fits were evaluated only after country-verified continent assignments; their sample sizes and endpoint patterns are reported separately. Joint tensor associations and endpoint sign transitions are distinct quantities. '+cite(geo_path,geo_scan_path)
    europe_full=geographic_bracket('PAIRED_FULL_684','Europe')
    if europe_full:
        lo,hi=europe_full[0]
        en=next(r['n'] for r in geo if r['response']=='PAIRED_FULL_684' and r['model']=='G0' and r['subset']=='exclude_Europe')
        results+='\n\nAfter Europe exclusion, the paired FULL G0 endpoint scan changed sign between '+f'{float(lo):,.2f} and {float(hi):,.2f}'+' km2 on '+en+' complete cities, whereas the paired ENV scan did not change sign. Thus, the pooled paired FULL no-flip result was not geographically invariant. '+cite(geo_path,geo_scan_path)
if performance:
    results += '\n\nRoute-compatible selected ensemble calibration metrics were available for 404 paired taxa, whereas matched individual-model validation metrics were available for only 19 taxa. Ensemble validation and external-evaluation pairs were unavailable. These incomplete, selected-unit evaluations were kept separate from the historical attrition summaries and do not establish elimination of sampling bias. '+cite('01_environment_only/qc/paired_performance_coverage.csv','01_environment_only/tables/paired_selected_performance_summary.csv')
limitations += '\n\nThe archived historical city responses and fresh final-boundary raster extractions differ. Until their extraction and version lineage is reconciled, reproduction of the archived GAM should be distinguished from response-product concordance. Additive G1/G2 formulations cannot produce city-size-dependent modulation of their reclamation smooth, so their absent sign transitions are not independent evidence against an interaction. '+cite('01_environment_only/qc/historical_city_response_concordance_summary.csv','02_gam_robustness/qc/model_definitions.csv')
limitations += '\n\nRemoving the specified human predictors does not establish an independent observed response. Retained environmental information remains represented in the SDMs and in the city-level covariate models, so broader predictor-reuse and response-independence concerns remain. '+cite(rp,'02_gam_robustness/qc/model_definitions.csv')
for name,body in [('methods_additions.md',methods),('results_additions.md',results),('discussion_additions.md',discussion),('limitations_additions.md',limitations)]:
    write_md('text_results/03_manuscript_ready/'+name,'# Proposed manuscript text\n\nAuthor-review draft; no DOCX edits were made.\n\n'+body)
write_md('text_results/03_manuscript_ready/supplementary_methods.md','# Proposed supplementary methods\n\n'+methods+'\n\nThe historical sign-change bootstrap used 30 replicates and reports its limited Monte Carlo precision. Residual Moran tests used 499 permutations across pre-specified neighbourhood definitions; P values are exploratory and unadjusted for multiple comparisons. Geographic exclusions were fitted only where the omitted group and remaining sample met the recorded support rules. '+cite(bp,mp,'03_sampling_selection/qc/module_status_geography_corrected.csv'))

captions = {
'01_environment_only/figures/selected_performance_differences':('Selected-unit evaluation differences stratified by metric, evaluation partition and projection-unit route. Only exactly mapped finite pairs contribute. Calibration panels summarize training-partition values and are not held-out predictive comparisons. Available individual validation metrics represent a limited selected subset.', '01_environment_only/tables/paired_selected_performance_source.csv'),
'01_environment_only/figures/paired_cell_density':('All finite cells in the paired FULL and ENV responses. Colour gives log10 cell counts in equal-width suitability bins; the diagonal marks equal predictions. Both responses use the same accepted taxa and cell-specific denominator.', '01_environment_only/tables/paired_density_source.csv'),
'01_environment_only/figures/paired_common_scale_maps':('Paired FULL and ENV mean suitability on a common zero-to-one colour scale. Maps show display means at 25 arcminutes; numerical analyses use the original 2.5-arcminute grid.', '01_environment_only/tables/paired_map_source_25arcmin.csv'),
'01_environment_only/figures/paired_difference_map':('Environment-only minus FULL suitability. Positive values denote greater ENV suitability. Display aggregation does not alter the native-grid statistical comparison.', '01_environment_only/tables/paired_map_source_25arcmin.csv'),
'02_gam_robustness/figures/model_comparison':('Fit comparisons for the pre-specified GAM formulations and three response definitions, using the same complete cities. AIC is compared within response definitions and does not independently select a scientific interpretation.',gp),
'02_gam_robustness/figures/endpoint_scan':('Endpoint association scans across city-area quantiles and response/formulation combinations. Sign changes are scan brackets, not fitted ecological breakpoints. Whole-range and local-support scans are retained.', '02_gam_robustness/tables/endpoint_scan.csv'),
'02_gam_robustness/figures/bootstrap_stability':('Frequency of historical-response endpoint sign changes in the recorded bootstrap. Limited replicate counts quantify uncertainty and do not validate a threshold.',bp),
'02_gam_robustness/figures/G0_conditional_curves':('Conditional fitted response profiles for the reported tensor model across the three response definitions. Curves use the fixed covariate settings recorded with their source data.', '02_gam_robustness/tables/G0_conditional_curve_source.csv'),
'02_gam_robustness/figures/influence':('Influence diagnostics across the fitted response/formulation combinations. High-influence observations were retained unless a documented data error justified a derived correction.', '02_gam_robustness/tables/residuals_and_influence.csv'),
'02_gam_robustness/figures/residual_qq':('Residual quantile diagnostics for the fitted GAMs. This check supplements basis-dimension and spatial-residual diagnostics.', '02_gam_robustness/tables/residuals_and_influence.csv'),
'03_sampling_selection/figures/geographic_endpoint_scans_geography_verified':('Country-verified geographic endpoint sensitivity for G0. Curves retain uncertainty and extrapolation qualifications; the archived historical response retains its concordance warning. Derived continent corrections preserve original model coordinates and covariates. Superseded initial geographic plots are excluded.', '03_sampling_selection/tables/geographic_endpoint_plot_source_geography_verified.csv'),
'04_nested_spatial_contrasts/figures/city_background_difference':('City minus surrounding-ring mean suitability using the final city boundaries and a 50-km buffer. Points are cities; the three responses retain their respective scales. This is a boundary-updated sensitivity rather than exact historical-footprint reproduction.', '04_nested_spatial_contrasts/tables/three_response_city_background_source.csv'),
'05_spatial_diagnostics/figures/residual_maps':('Spatial distribution of model residuals. Maps accompany neighbourhood-specific Moran diagnostics and should not be used to infer absence of spatial dependence.', '05_spatial_diagnostics/tables/residual_map_source.csv')}
figtext='# Figure and table recommendations\n\nMain text: use the paired city/background attenuation and parameterisation dependence to bound the claim. Supplement: full raster maps, bootstrap, influence, geographic and spatial diagnostics. Do not present an unreproduced within-city percentage or an exact ecological breakpoint as a new sensitivity finding.\n\n'
for stem,(caption,source) in captions.items():
    if stem.startswith('03_sampling_selection/') and not geo:
        continue
    if (root/(stem+'.png')).exists() and (root/(stem+'.pdf')).exists():
        if (root/source).exists():
            sources[source]=hashlib.sha256((root/source).read_bytes()).hexdigest()
        if stem.startswith('02_gam_robustness/'):
            caption+=' Historical fits reproduce the archived response table; clean historical-versus-paired interpretation requires response-concordance reconciliation.'
        figtext+='## '+Path(stem).name+'\n\n'+caption+' '+cite(source)+'\n\nPNG: `'+stem+'.png`; PDF: `'+stem+'.pdf`; source CSV: `'+source+'`.\n\n'
write_md('text_results/03_manuscript_ready/figure_and_table_recommendations.md',figtext)

evidence_groups = {
'response_independence_environment_only':(rp+'; '+np+'; '+sp,'PARTIALLY_ADDRESSED','Paired response sensitivity is completed; predictor-removal sensitivity does not establish independent observed invasion or eliminate confounding.','Define the response as model-derived suitability; separate historical, paired FULL and paired ENV; weaken causal and propagule-pressure language.'),
'gam_parameterisation_coupling':(gp+'; '+sp+'; '+bp,'PARTIALLY_ADDRESSED','Historical G0 reproduced, but sign-change behaviour is formulation and response dependent.','Report the P85-P90 interval as a scan bracket; remove an exact threshold claim; retain MarinePC and identify GAM correctly.'),
'nested_spatial_contrasts_and_baselines':(np+'; 04_nested_spatial_contrasts/qc/final_city_zone_union_equivalence.csv','PARTIALLY_ADDRESSED','Final-boundary city/background sensitivity completed; within-city geometry and 39% reproduction remain blocked.','Separate spatial baselines; label 115.4% as a final-boundary historical-response sensitivity rather than exact 122% reproduction.'),
'sampling_city_selection_and_region_exclusion':('03_sampling_selection/qc/roster_identity_check.csv; 03_sampling_selection/tables/leave_continent_out_models_geography_corrected.csv','PARTIALLY_ADDRESSED','Supported geographic exclusions completed; 135-pool reconciliation, excluded covariates and effort adjustment remain unresolved.','Describe selection and sampling limitations; report supported geographic subsets with their sizes.'),
'temporal_alignment_and_optional_reclamation_age':('06_optional_exploratory/tables/reclamation_age_availability.csv','NOT_ADDRESSED','No verified per-polygon age or post-reclamation response design was available; temporal direction was not tested.','Use association-based language and state temporal limitations; seek verified timing before a lag or age analysis.'),
'sem_specification_and_descriptive_scores':('06_optional_exploratory/tables/sem_input_provenance.csv','NOT_ADDRESSED','Two archived SEM inputs are not uniquely bound to the original fit/preprocessing; no replacement SEM fitted.','Reconcile SEM input/path-selection provenance; describe composite allocations as model-derived scores rather than causal city effects.'),
'sdm_lineage_settings_and_attrition':(ap+'; 01_environment_only/tables/paired_excluded_source.csv','PARTIALLY_ADDRESSED','Frozen 684/130 attrition and historical performance metadata inspected; complete SDM-method questions require the original production provenance.','Explain ensemble/fallback routes, evaluation partitions and exclusion handling without changing accepted thresholds.'),
'text_and_cross_reference_consistency':('text_results/03_manuscript_ready/figure_and_table_recommendations.md','NOT_ADDRESSED','Recommended wording/caption edits are drafted; this task did not edit manuscripts.','Apply and inspect the registered wording, punctuation, repetition and cross-reference corrections.'),
'scope_and_contribution_review':('text_results/02_scientific_interpretation/integrated_interpretation.md','AUTHOR_DECISION_REQUIRED','Sensitivity results narrow the supported claim; contribution, venue fit and final narrative require author judgement.','Reframe the contribution around bounded spatial associations and tested sensitivities; make the transfer/venue decision separately.')}
evidence_groups['sampling_city_selection_and_region_exclusion']=('03_sampling_selection/qc/roster_identity_check.csv; text_results/01_verified_facts/sampling_and_geography.md','PARTIALLY_ADDRESSED','Country verification identified source continent errors; only verified geographic reruns may be interpreted. Original-pool and effort comparisons remain blocked.','Document derived geographic corrections, supported subsets and unresolved selection/effort limitations.')
if geo:
    evidence_groups['sampling_city_selection_and_region_exclusion']=('03_sampling_selection/qc/roster_identity_check.csv; '+geo_path+'; 03_sampling_selection/qc/geographic_crosswalk_verified.csv','PARTIALLY_ADDRESSED','Country-verified geographic reruns completed; point-estimate sign transitions vary across exclusions. Original-pool and effort comparisons remain blocked.','Report verified geographic sample sizes and uncertainty/extrapolation warnings; retain sampling and selection limitations.')
if performance:
    evidence_groups['sdm_lineage_settings_and_attrition']=(ap+'; 01_environment_only/tables/paired_selected_performance_summary.csv; 01_environment_only/qc/paired_performance_coverage.csv','PARTIALLY_ADDRESSED','Frozen 684/130 attrition and selected-unit evaluation summaries completed; calibration is descriptive, validation covers a limited route-compatible subset and partial units remain missing.','Explain accepted route and partition coverage; do not infer eliminated bias or general predictive superiority from selected metrics.')
matrix=[]
for r in review:
    cid=r['comment_id'];module=r['proposed_analysis_modules'].split(';')[0]
    paths,status,result,action=evidence_groups.get(module,evidence_groups['scope_and_contribution_review'])
    if cid=='R2-C06':
        result='Accepted paired FULL/ENV sensitivity quantifies specified human-predictor removal. Retained environmental information can still be reused across modelling stages; independent observed-invasion validation and causal identification are not established.'
    if cid=='R2-C24':
        status='PARTIALLY_ADDRESSED';result='Submitted text inspection identifies 2000-2020 plus superscript reference 4; verified age/timing data remain unavailable.';action='Clarify year range and citation styling; do not infer a temporal correction from a typography clarification.'
    if cid in {'E-C01','E-C02','R3-C01','R3-C02','R3-C03'}:
        status='AUTHOR_DECISION_REQUIRED'
    row={'reviewer':cid.split('-')[0],'comment_id':cid,'concern':r['registered_core_concern'],'analysis':r['proposed_analysis_modules'],'status':status,'principal_result':result,'supports_original_claim':'QUALIFIED_OR_UNRESOLVED','recommended_manuscript_action':action,'recommended_rebuttal_action':'Describe completed evidence and remaining limits; manuscript edits are proposed, not claimed completed.','output_paths':paths,'remaining_limitation':r['registered_evidence_requirement']}
    if module=='gam_parameterisation_coupling':
        row['supports_original_claim']='HISTORICAL_FIT_REPRODUCED_TRANSITION_REQUIRES_WEAKER_CLAIM'
        row['remaining_limitation']+='; archived historical response differs from fresh final-boundary raster extraction; additive alternatives cannot modulate a focal smooth by city area.'
    elif module=='nested_spatial_contrasts_and_baselines':
        row['supports_original_claim']='CITY_BACKGROUND_SUPPORTED_WITHIN_CITY_NOT_REGENERATED'
    elif module=='temporal_alignment_and_optional_reclamation_age':
        row['supports_original_claim']='TEMPORAL_DIRECTION_NOT_TESTED'
    elif module=='response_independence_environment_only':
        row['supports_original_claim']='PAIRED_SENSITIVITY_COMPLETED_CAUSALITY_NOT_IDENTIFIED'
    matrix.append(row)
allowed={'ADDRESSED','PARTIALLY_ADDRESSED','NOT_ADDRESSED','NOT_APPLICABLE','AUTHOR_DECISION_REQUIRED'}
assert {r['comment_id'] for r in matrix}=={r['comment_id'] for r in review}
assert all(r['status'] in allowed for r in matrix)
write_csv('07_integrated_evidence/reviewer_evidence_matrix.csv',matrix)
write_csv('text_results/04_rebuttal_ready/reviewer_evidence_matrix.csv',matrix)
drafts='# Reviewer-response style drafts\n\nReadiness: needs author input. The original decision was rejection; these are reusable evidence drafts, not a claim of an invited revision or of changes already made.\n\n'
for r,m in zip(review,matrix):
    drafts+='## '+m['comment_id']+'\n\n**Original comment:**\n\n'+r['exact_review_comment']+'\n\n**Draft response:** Thank you for highlighting this issue. '+m['principal_result']+' We propose the following manuscript action: '+m['recommended_manuscript_action']+' The remaining requirement is '+m['remaining_limitation']+' No manuscript line references are assigned because this task did not edit the DOCX files.\n\n**Status:** '+m['status']+'. Evidence: `'+m['output_paths'].replace('; ','`; `')+'`.\n\n'
write_md('text_results/04_rebuttal_ready/reviewer_response_drafts.md',drafts)
write_md('text_results/04_rebuttal_ready/editor_summary.md','# Editor-facing evidence summary\n\nThe completed paired analysis quantifies sensitivity to the specified human-predictor removal on a frozen species set. It retains a positive but attenuated final-boundary city/background contrast. The historical tensor GAM is numerically reproduced for its archived city-risk table, while tensor alternatives, paired response constructions and geographic exclusions reveal unstable endpoint sign-transition behaviour. Additive no-flip results follow model construction and are not independent tests against interaction. These findings support a more limited association-based account.\n\nThe archived historical city risk is not concordant with fresh extraction of the historical response raster using the final boundaries. That input-lineage discrepancy prevents a clean historical-versus-paired GAM interpretation until the response/table chain is frozen. Matched paired FULL-versus-ENV models retain common geometry and cohort. Selected-unit performance summaries have limited evaluation coverage and do not establish eliminated sampling bias.\n\nThe package does not resolve temporal direction, unequal observation effort, complete city-selection bias, retained environmental predictor reuse, SEM pathway provenance or final within-city zone geometry. The proposed response and manuscript text acknowledge these open issues. Contribution and venue fit require author judgement; the package is not ready to submit.\n\nAll 52 source-registered comments are mapped in `07_integrated_evidence/reviewer_evidence_matrix.csv`. No submitted or working DOCX was edited.')

decisions='1. Freeze and document the historical response/table provenance: archived GAM risk differs from fresh exact-fraction and cell-centre extraction; do not refit the archived table simply to force agreement.\n2. Approve a narrative centred on model-derived suitability and distinct spatial baselines.\n3. Withdraw the exact ecological-threshold interpretation or frame it as a response/formulation/geography-specific sign-scan interval.\n4. Reconcile final reclaimed/non-reclaimed city footprints before repeating the within-city estimate.\n5. Resolve the original-pool city identities, excluded covariates and effort-data availability.\n6. Bind the original SEM fit input and preprocessing or omit unverified pathway sensitivity.\n7. Verify selected-route evaluation partitions before interpreting paired model-performance changes.\n8. Choose the venue/contribution framing and apply proposed manuscript edits in a separate authorized task.'
write_md('text_results/05_internal_decisions/author_decisions_required.md','# Author decisions required\n\n'+decisions)
write_md('text_results/05_internal_decisions/results_that_change_the_story.md','# Results that change the story\n\nThe paired ENV city/background contrast is much smaller than paired FULL, although still positive under the final-boundary ring comparison. Historical G0 reproduces its archived city-risk table and has a scan sign change, but tensor G3 and the pooled paired tensor fits do not reproduce that transition. Additive G1/G2 no-flip results follow their construction and cannot independently refute an interaction. Geographic subset results require the final country-verified assignments after archived continent errors were found. No-flip does not mean no reclamation association.\n\nThe archived historical city risk differs from fresh raster extraction using the final boundaries. That discrepancy blocks clean historical-versus-paired GAM interpretation until the author freezes response provenance. The paired FULL-versus-ENV comparison uses the same geometry and cohort. Several paired residual tests retain spatial structure.\n\nThese results require weaker claims about stable thresholds, causal pathways and management leverage. The within-city claim remains unverified in this package; the final-boundary historical city/ring sensitivity must not silently replace the original percentage. Keep global agreement, model fit, response concordance and causal interpretation distinct.')
write_md('text_results/05_internal_decisions/analyses_not_completed.md','# Analyses not completed\n\n- Exact historical city/background footprint reproduction: current original city vector has 107 cities, whereas the final version has 108.\n- Within-city reclaimed/non-reclaimed reproduction and a complete nested four-zone table: full-zone final-version provenance and footprint mismatches.\n- Retained 108 versus excluded 27 covariate/effort comparison: eight unmatched names plus missing excluded covariates and effort.\n- North America and Asia geographic exclusions: pre-specified sample support rules fail.\n- Original SEM reproduction and ENV SEM sensitivity: original fit/preprocessing lineage unresolved.\n- Reclamation age/time-lag analysis: no verified per-polygon timing input.\n- Independent causal or sampling-bias correction: not achieved by the sensitivity analyses.\n- Manuscript edits, scientific approval, publication and copying to FinalResults: outside this task execution.\n\nAccepted selected-route performance export status should be checked in its latest QC before release.')
write_md('text_results/05_internal_decisions/recommended_revision_priority.md','# Recommended revision priority\n\nFirst, freeze and document the historical response/table provenance before publishing the archived G0 reproduction or interpreting historical-versus-paired GAM differences. The concordance discrepancy is not explained by switching from exact-fraction to cell-centre means. Preserve the archived response; do not refit it to force agreement. Next, align the central claim with paired attenuation and formulation/geography-dependent transition behaviour. Separate city/background from within-city habitat estimands and reconcile final zone footprints before quoting a regenerated within-city percentage. Then document residual spatial dependence, selection and timing limitations, finalize route-matched performance metadata, and bind the SEM fit-input chain before pathway claims. Apply and verify proposed text/caption changes and make the venue decision last.\n\nMain text should carry the claim-changing paired sensitivity findings and the qualified GAM checks. Extended model diagnostics, geographic exclusions and the low-replicate bootstrap belong in supplementary material with explicit limitations.')

findings=[{'finding':'paired_global_agreement','result':f"Pearson {rr['pearson']}; Spearman {rr['spearman']}; n={rr['finite_cells']}",'status':'NUMERICALLY_VERIFIED','scope':'Frozen684 sharedfinitecellmeans','output_paths':rp},
{'finding':'city_background_attenuation','result':ring_sentence,'status':'NUMERICALLY_VERIFIED_BOUNDARY_UPDATED','scope':'Final108city50kmring; separate response scales','output_paths':np},
{'finding':'transition_parameterisation_dependence','result':'Historical G0 P85-P90 bracket; tensor G3 and pooled paired FULL/ENV have no whole-range flip; geographic sensitivity requires final country verification','status':'NUMERICALLY_VERIFIED_WITH_RESPONSE_LINEAGE_WARNING','scope':'Endpoint scans; additive no-flip is structural; not an exact breakpoint','output_paths':sp},
{'finding':'historical_bootstrap','result':'G0 21/30; G1/G2 0/30; G3 3/30','status':'LIMITED_MONTE_CARLO_PRECISION','scope':'Historicalresponseonly','output_paths':bp},
{'finding':'residual_spatial_dependence','result':'Pairedresponses retain neighbourhood-specific residual structure','status':'EXPLORATORY_UNADJUSTED','scope':'499permutations; multiple neighbourhoods','output_paths':mp},
{'finding':'within_city_reproduction','result':'Not regenerated; final-zone lineage unresolved','status':'BLOCKED','scope':'39percentclaim','output_paths':'04_nested_spatial_contrasts/qc/final_city_zone_union_equivalence.csv'},
{'finding':'selection_bias','result':'Eight unmatched135-pool names; geographic sensitivity does not replace effort control','status':'PARTIAL','scope':'Selection/effortunresolved','output_paths':'03_sampling_selection/qc/roster_identity_check.csv'},
{'finding':'optional_SEM_and_age','result':'Inputs not verified; no substitute fit','status':'BLOCKED','scope':'Provenance/timing','output_paths':'06_optional_exploratory/qc/optional_analysis_status.csv'}]
findings.append({'finding':'historical_city_response_concordance','result':'Archived risk differs from fresh final-boundary exact-fraction and cell-centre extraction','status':'WARNING_RESPONSE_LINEAGE_BLOCKER','scope':'Archived historical G0 reproduction remains table-specific','output_paths':'01_environment_only/qc/historical_city_response_concordance_summary.csv'})
if performance:
    findings.append({'finding':'selected_unit_evaluation','result':'Same-unit ensemble calibration 404 pairs; algorithm-matched ensemble 280; individual validation 19; external evaluation 0','status':'NUMERICALLY_VERIFIED_LIMITED_COVERAGE','scope':'Calibration descriptive; route, algorithm and evaluation partition separate','output_paths':'01_environment_only/tables/paired_selected_performance_summary.csv; 01_environment_only/qc/paired_performance_coverage.csv'})
write_csv('07_integrated_evidence/main_findings_summary.csv',findings)
comparisons=[{'manuscript_claim':'City/background 122%','new_result':next(r['percent_difference'] for r in rings if r['response']=='HISTORICAL_MAIN'),'relationship':'Boundary-updated sensitivity; cannot claim same historicfootprint','recommendation':'Retain historicalversionrecord; decidehow toreportfinalboundaryanalysis','output_paths':np},
{'manuscript_claim':'Within-city39%','new_result':'NOT_REGENERATED','relationship':'Blockedbyfinalzonegeometry','recommendation':'Do not presentasnewverifiedsensitivityresult','output_paths':'04_nested_spatial_contrasts/qc/final_city_zone_union_equivalence.csv'},
{'manuscript_claim':'Exact5621.5km2threshold','new_result':interval['lower_km2']+'-'+interval['upper_km2'],'relationship':'HistoricalG0 scanbracketonly; pairedandotherformulationsno flip','recommendation':'Weaken thresholdclaim; reportintervalandformulationdependence','output_paths':sp},
{'manuscript_claim':'Causal indirectpathways','new_result':'Not identified','relationship':'SEMininputchainunresolved; cross-sectionalassociation','recommendation':'Use associational/modelderivedwording','output_paths':'06_optional_exploratory/tables/sem_input_provenance.csv'}]
comparisons.append({'manuscript_claim':'Historical city response underlying reported GAM','new_result':'Fresh raster extraction is not numerically concordant with archived risk','relationship':'Historical G0 reproduces archived table; historical-versus-paired response-product comparison is lineage restricted','recommendation':'Freeze and document response/table provenance before publishing this comparison','output_paths':'01_environment_only/qc/historical_city_response_concordance_summary.csv'})
write_csv('07_integrated_evidence/manuscript_result_comparison.csv',comparisons)
placement=[{'component':'Paired predictor-removal and final city/background sensitivity','placement':'MAIN_TEXT','reason':'Directly changes centralclaim','output_paths':rp+'; '+np}, {'component':'GAM parameterisation and sign-change uncertainty','placement':'MAIN_TEXT_PLUS_SUPPLEMENT','reason':'Mainqualification; full12fitdiagnosticsin supplement','output_paths':gp+'; '+sp}, {'component':'Rastermaps/bootstrap/influence/geographic/spatial','placement':'SUPPLEMENT','reason':'Traceablefullrobustnessdetail','output_paths':bp+'; '+mp}, {'component':'Within-city reanalysis/SEM age fits','placement':'NOT_REPORTABLE_AS_COMPLETED','reason':'Inputprovenance blockers','output_paths':'06_optional_exploratory/qc/optional_analysis_status.csv'}]
write_csv('07_integrated_evidence/recommended_main_vs_supplement.csv',placement)
write_csv('07_integrated_evidence/report_build_inputs.csv',[{'input_id':f'REPORT_INPUT_{i:03d}','relative_path':p,'sha256':h} for i,(p,h) in enumerate(sorted(sources.items()),1)])
write_md('RELEASE_ANALYSIS_SUMMARY.md',f"""# Reviewer evidence release summary

Readiness: author-review draft with explicit scientific and input-provenance limitations. The analyses are not a declaration of scientific acceptance or submission readiness.

The frozen paired products, exact cell comparison and final-city extraction are numerically verified. The boundary-updated city/background contrast remains positive but is attenuated under ENV. The historical GAM is reproduced, while its sign transition is not stable across parameterisations or the paired response constructions. Geographic and spatial diagnostics are retained with their support rules and residual-dependence qualifications.

The archived historical GAM risk differs from fresh final-boundary historical raster extraction. Historical G0 reproduction is valid for its archived table, but clean historical-versus-paired GAM interpretation is blocked by response concordance. Matched paired FULL-versus-ENV models use a common geometry and cohort. Additive no-flip results are structural. Country-verified geographic reruns are completed after derived continent corrections; paired FULL exhibits a Europe-excluded sign change and ENV exhibits an Africa-excluded sign change despite their pooled no-flip scans. No-flip is not absence of an association. Older geographic tables and plots remain superseded.

The package retains blockers for exact historical geometry, within-city zones, full city-selection/effort comparison, SEM lineage and reclamation timing. No SDM was refitted, no excluded taxon retried, no manuscript edited, and no result copied to FinalResults.

Runtime warning: several R graphics runs wrote the reviewed PNG/PDF files and then terminated with a Windows graphics teardown error and a nonzero exit status. Separate numeric-only verifiers passed with exit zero; these checks do not erase the graphics-runtime failure or establish a blanket end-to-end PASS. The execution receipts retain the actual statuses.

- Verified facts: `text_results/01_verified_facts/00_all_verified_results.md`.
- Scientific interpretation: `text_results/02_scientific_interpretation/integrated_interpretation.md`.
- Proposed English text and captions: `text_results/03_manuscript_ready/`.
- Exact 52-record reviewer matrix: `07_integrated_evidence/reviewer_evidence_matrix.csv`.
- Manuscript comparison: `07_integrated_evidence/manuscript_result_comparison.csv`.
- Reviewer drafts: `text_results/04_rebuttal_ready/reviewer_response_drafts.md`.
- Author priorities: `text_results/05_internal_decisions/recommended_revision_priority.md`.

Report input hashes and writer provenance are in `07_integrated_evidence/report_build_inputs.csv` and `07_integrated_evidence/report_output_provenance.csv`. The parent output manifest supplies the complete release file hashes. Completed analysis files are retained; text reports may be rebuilt when outstanding metadata exports settle.
""")
write_csv('07_integrated_evidence/report_output_provenance.csv',[{'output_path':p,'writer_script':writer,'writer_sha256':hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),'input_manifest':'07_integrated_evidence/report_build_inputs.csv','input_ids':';'.join(f'REPORT_INPUT_{i:03d}' for i in range(1,len(sources)+1)),'build_utc':datetime.now(timezone.utc).isoformat()} for p in outputs])
index=root/'07_integrated_evidence/analysis_output_index.csv'
if not index.exists():
    write_csv('07_integrated_evidence/analysis_output_index.csv',[{'relative_path':str(p.relative_to(root)),'size_bytes':p.stat().st_size,'sha256':hashlib.sha256(p.read_bytes()).hexdigest(),'scope':'Preliminaryreportbuildindex; finalreleaseindexwrittenbyorchestrator'} for p in sorted(root.rglob('*')) if p.is_file() and p.suffix.lower() in {'.csv','.md','.png','.pdf'}])
print(json.dumps({'status':'REPORTS_BUILT_WITH_EXPLICIT_LIMITATIONS','reviewer_rows':len(matrix),'source_files':len(sources),'report_outputs':len(outputs),'matrix_status_counts':{s:sum(r['status']==s for r in matrix) for s in sorted(allowed)}},indent=2))
