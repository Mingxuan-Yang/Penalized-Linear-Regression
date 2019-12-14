# STA 523 :: Project

This is our group final project on **Penalized Linear Regression**. 

Group members: Bo Liu, Mingxuan Yang, Jiawei Chen, Linlin Li.

### How to evaluate

You can use `make` to generate files you will need and launch the app.

There might be an error "This site can’t be reached. 127.0.0.1 refused to connect.", which is because the webpage is opened before the set-up of the shiny app. You can refresh the webpage or just wait a couple of seconds.

Three sample data files are under `/Data` folder. One of them is `monthly_stock.csv`, which we use API to download from online source.

### Attention

- For regression part, when you use the automatic formula-generating system, please delete what you've written in the formula input. Otherwise, it will still use your formula input.

- If there is an error "address already in use" when executing `make`, it is because the port selected is currently occupied. Since we choose one out of 5000 port numbers randomly each time, the problem will likely to disappear for the next port number.

- If the webpage fails to pop out after `make`, you can open the browser and log in onto http://127.0.0.1:xxxx, where xxxx is the port number stored in port.txt. The webpage may fail to pop out because of security settings.

### Topics covered

- Basic R calculation, functions and plots;

- Advanced plots using `ggplot2`;

- S3 objects and generic programming;

- Useful data manipulating packages like `dplyr` and `purrr`;

- Efficient data I/O packages like `readr`;

- Web scraping and API interface, and manipulating scraped texts using `jsonlite`;

- Platform accustomed Makefile with variables;

- RShiny app;

- HighChart object for interactive plots.