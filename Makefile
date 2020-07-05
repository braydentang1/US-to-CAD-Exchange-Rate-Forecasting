all: excaus_forecast/results/quantile-training-data.rds excaus_forecast/results/excaus.rds excaus_forecast/results/predictions.rds upload

# Validate individual methods

excaus_forecast/results/individual_validation.csv excaus_forecast/results/excaus.rds: src/0_evaluate_methods.R src/all_functions.R
	Rscript src/0_evaluate_methods.R --exclude_vector "Bagged ETS" --results_out excaus_forecast/results

# Validate ensemble

excaus_forecast/results/ensemble_validation.csv: src/1_ensemble_main.R src/all_functions.R excaus_forecast/results/excaus.rds
	Rscript src/1_ensemble_main.R --iterations 60 --results_out excaus_forecast/results --excaus_in excaus_forecast/results/excaus.rds
	
# Get training data for quantile regression predictino intervals

excaus_forecast/results/quantile-training-data.rds excaus_forecast/results/predictions.rds: src/2_quantile_pi.R src/all_functions.R excaus_forecast/results/excaus.rds
	Rscript src/2_quantile_pi.R --excaus_in excaus_forecast/results/excaus.rds --results_out excaus_forecast/results 

# Build the app and upload

.PHONY: upload
upload: excaus_forecast/app.R excaus_forecast/results/quantile-training-data.rds excaus_forecast/results/excaus.rds excaus_forecast/results/predictions.rds
	Rscript -e "rsconnect::deployApp('excaus_forecast', forceUpdate = TRUE, launch.browser = FALSE)"

clean:
	rm -rf excaus_forecast/results/*
	
