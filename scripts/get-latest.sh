touch public/latestX.csv
curl https://docs.google.com/spreadsheets/d/1wZhPLMCHKJvwOkP4juclhjFgqIY8fQFMemwKL2c64vk/gviz/tq?tqx=out:csv > public/latest.csv
python scripts/csv-to-tsv.py