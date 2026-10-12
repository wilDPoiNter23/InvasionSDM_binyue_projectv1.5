# Original source: collect_hpc_inventory.sh
# Original source location: ${NON_NATIVE_ROOT}/04_hpc_inventory/collect_hpc_inventory.sh
# Original source SHA-256: 543d2bb54d11e3c9356543e0c69017ec4a6915b26fea62e717304cb5535fb904
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/bin/bash
# collect_hpc_inventory.sh
# Read-only inventory of the invasionSDM_binyue HPC tree.
# Phase 1B Track A (R1-R10) evidence collector for the Scientific Data descriptor.
#
# SAFETY CONTRACT
#   This script only READS the tree under $ROOT. It writes text files to one new
#   directory under $HOME. It never deletes, moves, renames, recompresses or
#   modifies anything inside $ROOT. No compute job needed; login node is fine.
#
# USAGE
#   bash collect_hpc_inventory.sh                      # default root
#   ROOT=/some/other/tree bash collect_hpc_inventory.sh
#   HASH_BIG=1 bash collect_hpc_inventory.sh           # also sha256 big files (SLOW)
#
# OUTPUT
#   $HOME/sdinv_<stamp>/           text reports
#   $HOME/sdinv_<stamp>.tar.gz     the one file to bring back

set -u

ROOT="${ROOT:-<WORK_ROOT>}"
STAMP="$(date +%Y%m%d_%H%M)"
OUT="${OUT:-$HOME/sdinv_${STAMP}}"
HASH_BIG="${HASH_BIG:-0}"
HASH_MAX_BYTES="${HASH_MAX_BYTES:-52428800}"   # 50 MB cap for automatic checksums

mkdir -p "$OUT" || { echo "FATAL: cannot create $OUT"; exit 1; }
exec > >(tee "$OUT/00_console.log") 2>&1

say()  { printf '\n=========== %s ===========\n' "$*"; }
note() { printf -- '-- %s\n' "$*"; }

say "S1 context"
{
  echo "collected_at_iso: $(date -Is 2>/dev/null || date)"
  echo "hostname: $(hostname)"
  echo "whoami: $(whoami)"
  echo "root: $ROOT"
  echo "root_exists: $([ -d "$ROOT" ] && echo yes || echo NO)"
  echo "uname: $(uname -a)"
  echo "pwd: $(pwd)"
  echo "script_sha256: $(sha256sum "$0" 2>/dev/null | cut -d' ' -f1)"
} | tee "$OUT/01_context.txt"

if [ ! -d "$ROOT" ]; then
  echo "FATAL: ROOT does not exist or is not readable: $ROOT"
  echo "Fix: pass the correct path, e.g. ROOT=/thfs1/home/<acct>/<dir> bash $0"
  exit 2
fi

# ---------------------------------------------------------------- helpers
# Full manifest line: relpath<TAB>bytes<TAB>mtime_epoch<TAB>mtime_iso
# GNU find -printf is preferred; fall back to stat if unavailable.
have_find_printf=0
if find "$ROOT" -maxdepth 0 -printf '' 2>/dev/null; then have_find_printf=1; fi
note "find -printf available: $have_find_printf"

manifest_of() {  # $1 = dir to walk
  local d="$1"
  [ -d "$d" ] || return 0
  if [ "$have_find_printf" = "1" ]; then
    find "$d" -type f -printf '%p\t%s\t%T@\t%TY-%Tm-%Td %TH:%TM\n' 2>/dev/null
  else
    find "$d" -type f 2>/dev/null | while IFS= read -r f; do
      printf '%s\t%s\t%s\t%s\n' "$f" \
        "$(stat -c %s  "$f" 2>/dev/null)" \
        "$(stat -c %Y  "$f" 2>/dev/null)" \
        "$(stat -c %y  "$f" 2>/dev/null | cut -c1-16)"
    done
  fi
}

say "S2 top-level layout and sizes"
ls -la "$ROOT"/ | tee "$OUT/02_toplevel_ls.txt"
note "du -sh per top-level entry (may take a minute on Lustre)"
du -sh "$ROOT"/*/ 2>/dev/null | tee "$OUT/03_toplevel_du.txt"
find "$ROOT" -maxdepth 2 -type d 2>/dev/null | sort | tee "$OUT/04_dirs_depth2.txt"
find "$ROOT" -maxdepth 3 -type d 2>/dev/null | sort > "$OUT/05_dirs_depth3.txt"
note "depth-3 dir count: $(wc -l < "$OUT/05_dirs_depth3.txt")"

say "S3 full file manifest"
printf 'path\tbytes\tmtime_epoch\tmtime_iso\n' > "$OUT/10_file_manifest.tsv"
manifest_of "$ROOT" >> "$OUT/10_file_manifest.tsv"
note "manifest rows: $(( $(wc -l < "$OUT/10_file_manifest.tsv") - 1 ))"

note "extension histogram"
awk -F'\t' 'NR>1{n=split($1,a,"/"); f=a[n];
             if (f ~ /\./) {m=split(f,b,"."); e=tolower(b[m])} else {e="(none)"}
             c[e]++; s[e]+=$2}
        END {for (k in c) printf "%-12s %8d files %14.1f MB\n", k, c[k], s[k]/1048576}' \
    "$OUT/10_file_manifest.tsv" | sort -k2 -nr | tee "$OUT/11_extension_histogram.txt"

note "10 largest files"
awk -F'\t' 'NR>1{printf "%14.1f MB  %s  %s\n", $2/1048576, $4, $1}' "$OUT/10_file_manifest.tsv" \
  | sort -nr | head -20 | tee "$OUT/12_largest_files.txt"

note "mtime span per top-level dir (R3 provenance timeline)"
awk -F'\t' -v root="$ROOT" 'NR>1{
      rel=$1; sub("^"root"/","",rel); n=split(rel,a,"/"); top=(n>1?a[1]:"(rootfile)");
      if (min[top]==""||$3<min[top]) {min[top]=$3; minf[top]=$4}
      if (max[top]==""||$3>max[top]) {max[top]=$3; maxf[top]=$4}
      c[top]++}
   END {for (k in c) printf "%-14s %6d files   first=%s   last=%s\n", k, c[k], minf[k], maxf[k]}' \
    "$OUT/10_file_manifest.tsv" | sort | tee "$OUT/13_mtime_span_by_dir.txt"

say "S4 R1 models/ inventory"
MODELS="$ROOT/models"
if [ -d "$MODELS" ]; then
  find "$MODELS" -maxdepth 1 -mindepth 1 -type d 2>/dev/null | sed "s|.*/||" | sort \
    > "$OUT/20_models_species_dirs.txt"
  note "species dirs under models/: $(wc -l < "$OUT/20_models_species_dirs.txt")"
  manifest_of "$MODELS" > "$OUT/21_models_manifest.tsv"
  note "model files: $(wc -l < "$OUT/21_models_manifest.tsv")"
  find "$MODELS" -maxdepth 2 -name '*.models.out' -o -maxdepth 2 -name '*.out' 2>/dev/null \
    | head -50 > "$OUT/22_models_out_objects.txt"
  find "$MODELS" -type f \( -name '*.rds' -o -name '*.RDS' \) 2>/dev/null \
    | sed "s|$MODELS/||" | sort > "$OUT/23_models_rds_list.txt"
  note "model .rds files: $(wc -l < "$OUT/23_models_rds_list.txt")"
  ls -la "$MODELS"/ 2>/dev/null | head -30 > "$OUT/24_models_head_ls.txt"
else
  echo "MISSING: $MODELS" | tee "$OUT/20_models_MISSING.txt"
fi

say "S5 R2 results/projections inventory"
PROJ="$ROOT/results"
if [ -d "$PROJ" ]; then
  find "$PROJ" -maxdepth 3 -type d 2>/dev/null | sort > "$OUT/30_results_dirs.txt"
  manifest_of "$PROJ" > "$OUT/31_results_manifest.tsv"
  note "result files: $(wc -l < "$OUT/31_results_manifest.tsv")"
  # raster families
  for ext in grd gri tif tiff img nc rds RDS; do
    n=$(awk -F'\t' -v e=".$ext" 'index($1,e)==length($1)-length(e)+1' "$OUT/31_results_manifest.tsv" | wc -l)
    printf '%-6s %6d\n' "$ext" "$n"
  done | tee "$OUT/32_results_raster_counts.txt"
  # species names inferred from ensemble projection filenames
  awk -F'\t' '{n=split($1,a,"/"); print a[n]}' "$OUT/31_results_manifest.tsv" \
    | grep -Ei '\.(grd|tif|rds)$' | sort -u > "$OUT/33_results_basenames.txt"
  note "distinct result basenames: $(wc -l < "$OUT/33_results_basenames.txt")"
  # candidate final aggregate rasters
  grep -Ei 'hotspot|normalized|mean_suit|pressure|combine' "$OUT/31_results_manifest.tsv" \
    > "$OUT/34_aggregate_raster_candidates.tsv"
  note "aggregate-raster candidate rows: $(wc -l < "$OUT/34_aggregate_raster_candidates.tsv")"
else
  echo "MISSING: $PROJ" | tee "$OUT/30_results_MISSING.txt"
fi

say "S6 R3 logs and job provenance"
LOGS="$ROOT/logs"
if [ -d "$LOGS" ]; then
  ls -la "$LOGS"/ 2>/dev/null | tee "$OUT/40_logs_ls.txt"
  manifest_of "$LOGS" > "$OUT/41_logs_manifest.tsv"
else
  echo "MISSING: $LOGS" > "$OUT/40_logs_MISSING.txt"
fi
note "search whole root for job scripts and stdout"
find "$ROOT" -maxdepth 3 -type f \
  \( -name '*.out' -o -name '*.err' -o -name '*.log' -o -name '*.sh' \
     -o -name '*slurm*' -o -name '*sbatch*' -o -name '*.yhbatch' -o -name '*.pbs' \) \
  2>/dev/null | sort > "$OUT/42_job_files.txt"
note "job/log/script files found: $(wc -l < "$OUT/42_job_files.txt")"
# head+tail of every REcombine-style log, small enough to read offline
mkdir -p "$OUT/43_log_excerpts"
grep -Ei 'recombine|combine|aggregate|hotspot' "$OUT/42_job_files.txt" | head -20 | while IFS= read -r f; do
  b=$(echo "$f" | sed 's|/|_|g')
  { echo "### FILE: $f"
    echo "### BYTES: $(stat -c %s "$f" 2>/dev/null)  MTIME: $(stat -c %y "$f" 2>/dev/null)"
    echo "### ---- HEAD 120 ----"; head -120 "$f" 2>/dev/null
    echo "### ---- TAIL 120 ----"; tail -120 "$f" 2>/dev/null
  } > "$OUT/43_log_excerpts/$b.txt"
done
note "log excerpts written: $(ls -1 "$OUT/43_log_excerpts" 2>/dev/null | wc -l)"

say "S7 R7 environment provenance"
find "$ROOT" -maxdepth 4 -type f \
  \( -name 'renv.lock' -o -name 'sessionInfo*' -o -name '*.Rprofile' -o -name '.Rprofile' \
     -o -name 'DESCRIPTION' -o -name 'renv' -o -name '*.Rhistory' \) \
  2>/dev/null | sort | tee "$OUT/50_env_provenance_files.txt"
for f in $(grep -E 'renv\.lock|sessionInfo' "$OUT/50_env_provenance_files.txt" 2>/dev/null | head -5); do
  b=$(echo "$f" | sed 's|/|_|g')
  cp -p "$f" "$OUT/51_copy_$b" 2>/dev/null && note "copied $f"
done
note "R version available on login node"
{ command -v R && R --version 2>/dev/null | head -3; } > "$OUT/52_login_node_R.txt" 2>&1

say "S8 R4 R5 native-range mask evidence"
find "$ROOT" -maxdepth 4 -type f -iname '*mask*' 2>/dev/null | sort > "$OUT/60_mask_files.txt"
find "$ROOT" -maxdepth 4 -type d -iname '*mask*' 2>/dev/null | sort >> "$OUT/60_mask_files.txt"
find "$ROOT" -maxdepth 4 -type f \
  \( -iname '*.shp' -o -iname '*.gpkg' -o -iname '*glonaf*' -o -iname '*wcvp*' \
     -o -iname '*native*' -o -iname '*range*' \) 2>/dev/null | sort > "$OUT/61_polygon_candidates.txt"
note "mask hits: $(wc -l < "$OUT/60_mask_files.txt")  polygon hits: $(wc -l < "$OUT/61_polygon_candidates.txt")"

say "S9 R code inventory"
find "$ROOT" -maxdepth 4 -type f \( -name '*.[Rr]' -o -name '*.[Rr]md' -o -name '*.py' \) \
  2>/dev/null | sort | tee "$OUT/70_code_files.txt"
mkdir -p "$OUT/71_code_copies"
# R scripts are small; copy them so the exact executed code is auditable offline
while IFS= read -r f; do
  sz=$(stat -c %s "$f" 2>/dev/null || echo 0)
  if [ "${sz:-0}" -le 1048576 ]; then
    cp -p "$f" "$OUT/71_code_copies/$(echo "$f" | sed 's|/|_|g')" 2>/dev/null
  fi
done < "$OUT/70_code_files.txt"
note "code files copied: $(ls -1 "$OUT/71_code_copies" 2>/dev/null | wc -l)"

say "S10 R10 predictor stack candidates"
awk -F'\t' 'NR>1 && $2>104857600 {printf "%14.1f MB  %s  %s\n", $2/1048576, $4, $1}' \
  "$OUT/10_file_manifest.tsv" | sort -nr | tee "$OUT/80_files_over_100MB.txt"
grep -Ei 'stack|predictor|bioclim|worldclim|soil|env' "$OUT/10_file_manifest.tsv" \
  | awk -F'\t' '{printf "%12.1f MB  %s  %s\n", $2/1048576, $4, $1}' | sort -nr | head -40 \
  > "$OUT/81_stack_candidates.txt"

say "S11 checksums"
note "sha256 for files <= $HASH_MAX_BYTES bytes (HASH_BIG=$HASH_BIG for the rest)"
printf 'sha256\tbytes\tpath\n' > "$OUT/90_checksums.tsv"
awk -F'\t' -v cap="$HASH_MAX_BYTES" -v big="$HASH_BIG" 'NR>1 && (big==1 || $2<=cap) {print $1}' \
  "$OUT/10_file_manifest.tsv" > "$OUT/.hashlist"
note "files to hash: $(wc -l < "$OUT/.hashlist")"
while IFS= read -r f; do
  h=$(sha256sum "$f" 2>/dev/null | cut -d' ' -f1)
  printf '%s\t%s\t%s\n' "${h:-HASH_FAILED}" "$(stat -c %s "$f" 2>/dev/null)" "$f"
done < "$OUT/.hashlist" >> "$OUT/90_checksums.tsv"
rm -f "$OUT/.hashlist"
note "checksum rows: $(( $(wc -l < "$OUT/90_checksums.tsv") - 1 ))"

say "S12 disk quota and package"
{ lfs quota -h -u "$(whoami)" /thfs1 2>/dev/null || df -h "$ROOT" 2>/dev/null; } | tee "$OUT/95_quota.txt"

say "S13 done"
{
  echo "output_dir: $OUT"
  echo "files_written: $(find "$OUT" -type f | wc -l)"
  echo "output_size: $(du -sh "$OUT" | cut -f1)"
  echo "finished_at: $(date -Is 2>/dev/null || date)"
} | tee "$OUT/99_summary.txt"

tar -czf "${OUT}.tar.gz" -C "$(dirname "$OUT")" "$(basename "$OUT")" 2>/dev/null
echo
echo "==================================================================="
echo "BRING THIS ONE FILE BACK:"
echo "  ${OUT}.tar.gz   ($(du -sh "${OUT}.tar.gz" 2>/dev/null | cut -f1))"
echo "==================================================================="
