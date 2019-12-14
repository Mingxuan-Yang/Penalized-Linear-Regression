all: choose-port 523-Final-Project.html monthly_stock.csv iris.csv swiss.csv launch

choose-port:
	rm -rf port.txt
	Rscript getPort.R

523-Final-Project.html: 523-Final-Project.Rmd
	Rscript -e "library(rmarkdown); render('523-Final-Project.Rmd')"

monthly_stock.csv iris.csv swiss.csv: Data/getData.R
	Rscript Data/getData.R

cat := $(if $(filter $(OS),Windows_NT),type,cat)
variable = $(shell $(cat) port.txt)

launch: app.R AppFiles/about.md AppFiles/glossary.md port.txt
	
	Rscript app.R &
	open http\://127.0.0.1\:$(variable);
	
clean_data:
	rm -rf Data/*.csv

clean_html:
	rm -rf *.html

.PHONY: all clean_html clean_data