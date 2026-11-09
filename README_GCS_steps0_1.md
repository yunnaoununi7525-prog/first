# GCS steps 0 and 1 code snapshot

The code_only_backup.zip archive contains the independent runnable project, including R and SQL scripts, tests, README, source selection notes and renv.lock. Extract it and open gcs_early_sepsis.Rproj. The folder hierarchy is preserved inside the archive.

No patient-level data, fitted models, source archives, credentials or private BigQuery configuration are included. The original main branch is unchanged. The fully populated local project is maintained separately on the research computer.

Verified locally: 2,000,738 reconstructed GCS assessments; 1,591,050 valid assessments; official Jointlcmm example converged; helper and real-output checks passed; 20 subjects independently checked against raw source records. These are broad extraction-frame counts before the step-2 sepsis cohort selection. Some historical generator scripts remain unavailable; see the selection report.

Local code commit: 9551909. Database versions: MIMIC-IV 3.1 and eICU-CRD 2.0. R 4.5.3; lcmm 2.2.2.
