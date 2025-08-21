# ------- Project variables -------
PYTHON ?= python
PIP ?= pip
DATA ?= data/articles.csv
TARGET ?=
TEST_SIZE ?= 0.2
SEED ?= 42
OUTDIR ?= models

# ------- Phony targets -------
.PHONY: help install train test clean r_install r_train r_summary

help:
	@echo "Make targets:"
	@echo "  make install                Install Python dependencies"
	@echo "  make train DATA=path.csv    Train & compare models (Python)"
	@echo "  make test                   Run unit tests (pytest)"
	@echo "  make clean                  Remove caches and built artifacts"
	@echo "  make r_install              Install R packages (dependencies)"
	@echo "  make r_train DATA=path.csv  Train models with R scripts"
	@echo "  make r_summary DATA=path.csv  Run R summary/EDA"
	@echo "  make renv_restore           Restore R environment using renv"
	@echo "  make renv_snapshot          Snapshot R environment using renv"
	@echo "  make renv_clean             Clean R environment using renv"

install:
	$(PIP) install -r requirements.txt

train:
	@if [ ! -f "$(DATA)" ]; then \
		echo "ERROR: DATA file '$(DATA)' not found. Pass DATA=./data/articles.csv"; \
		exit 1; \
	fi
	$(PYTHON) main.py --data "$(DATA)" $(if $(TARGET),--target $(TARGET),) --test_size $(TEST_SIZE) --seed $(SEED) --outdir "$(OUTDIR)"

test:
	$(PYTHON) -m pytest -q

clean:
	find . -type d -name "__pycache__" -prune -exec rm -rf {} \; || true
	find . -type d -name ".pytest_cache" -prune -exec rm -rf {} \; || true
	rm -rf models/*.joblib || true


# ------- R targets -------
r_train:
	Rscript R/train.R --data "$(DATA)"

r_install:
	Rscript -e " source('R/setup_renv.R')"

# r_test:
# 	Rscript -e "if (!requireNamespace('testthat', quietly=TRUE)) install.packages('testthat'); source('tests/testthat/test_load_functions.R') ;testthat::test_dir('tests/testthat')"


# ------- renv targets -------

renv_restore:
	Rscript -e "if (!requireNamespace('renv', quietly=TRUE)) install.packages('renv', repos='$(CRAN)'); renv::restore(prompt = FALSE)"

renv_snapshot:
	Rscript -e "renv::snapshot(prompt = FALSE)"


renv_clean:
	Rscript -e "renv::clean(confirm = FALSE)"
