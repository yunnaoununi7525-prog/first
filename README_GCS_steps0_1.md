# GCS steps 0–1: source recovery and validation snapshot

Code snapshot: local commit 432d7ea, 2026-11-09. Unzip code_only_backup.zip to obtain R/, sql/, tests/, tools/, docs/, environment lock and example configuration. The original repository main branch is unchanged.

This revision adds reproducible raw eICU extraction and MIMIC CAM, medication-order, SOFA and admission/death sources. It fixes 64-bit nursing-record identifier overflow and preserves GCS observations for stays with unknown ICU discharge time without inventing follow-up duration.

Validated MIMIC output: 2,001,331 assessments; 1,591,242 valid assessments. Unknown-discharge handling restored 593 assessments. Raw eICU neurological extract: 13,796,460 records with complete unique primary identifiers. Ten source extracts, original and new regression tests, full GCS acceptance and fixed 20-subject independent checks passed. 132 original local files retain their SHA256 checksums.

This is NOT a claim that every research-plan requirement is complete: old final-dataset/classification provenance remains unavailable; human source-review signoff and group presentation remain pending. New final cohorts and trajectory models have not been built in this revision. The detailed docs inside the archive describe the state immediately before this publication; this outer README identifies the uploaded snapshot.

Only code, documentation and aggregate counts are included. No patient-level records, fitted models, private configuration or login tokens are uploaded.
