source('scripts/smoke.R')
render_risk_report(s, 'reports/sample-risk-report.pdf', 'pdf')
cat('PDF rendered.\n')
