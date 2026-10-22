#!/usr/bin/env bash
set -euo pipefail

: "${ENSEMBLE_CV_MANIFEST:?Set ENSEMBLE_CV_MANIFEST}"
: "${ENSEMBLE_CV_OUTPUT:?Set ENSEMBLE_CV_OUTPUT}"
: "${SLURM_ARRAY_TASK_ID:?Run as a Slurm array task}"

repo_root="${ENSEMBLE_CV_REPO_ROOT:-$PWD}"
script_path="${repo_root}/code/04_sdm/evaluate_foldmatched_ensembles.R"
test -f "${script_path}"
rscript_bin="${RSCRIPT_BIN:-Rscript}"
offset="${ENSEMBLE_CV_OFFSET:-0}"
stride="${ENSEMBLE_CV_STRIDE:-1}"
start=$((SLURM_ARRAY_TASK_ID + offset))
last="${ENSEMBLE_CV_LAST_INDEX:-$start}"
failures=0

if (( stride < 1 || start < 1 || last < start )); then
  echo "Invalid array stride or index bounds" >&2
  exit 2
fi

for ((index = start; index <= last; index += stride)); do
  output_file="${ENSEMBLE_CV_OUTPUT}/$(printf '%04d' "${index}").csv"
  if [[ -f "${output_file}" ]]; then
    echo "Existing result: ${index}"
    continue
  fi
  if ! "${rscript_bin}" --vanilla \
    "${script_path}" \
    "${ENSEMBLE_CV_MANIFEST}" \
    "${index}" \
    "${ENSEMBLE_CV_OUTPUT}"; then
    echo "Evaluation failed: ${index}" >&2
    failures=$((failures + 1))
  fi
done

if (( failures > 0 )); then
  echo "Species evaluations failed: ${failures}" >&2
  exit 1
fi
