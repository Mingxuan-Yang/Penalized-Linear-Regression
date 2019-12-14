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
ifeq ($(OS),Windows_NT)
    OPEN := start
else
    UNAME := $(shell uname -s)
    ifeq ($(UNAME),Linux)
        OPEN := xdg-open
    else
    		OPEN := open
    endif
endif

launch: app.R AppFiles/about.md AppFiles/glossary.md port.txt
	
	Rscript app.R &
	$(OPEN) http\://127.0.0.1\:$(variable);
	
clean_data:
	rm -rf Data/*.csv

clean_html:
	rm -rf *.html

.PHONY: all clean_html clean_data