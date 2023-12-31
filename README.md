# ENPOL Project


## Objective

This project aims to analyze the Criminal Justice System (SJPA, for its acronym in Spanish) in Mexico using data from the Encuesta Nacional de Población Privada de Libertad (ENPOL) recolected by the Instituto Nacional de Estadística y Geografía (INEGI). The ENPOL survey provides valuable insights into the experiences and perspectives of individuals within the Mexican SJPA, including their interactions with law enforcement, the judicial process, and prison conditions. The present repository contains the R code the World Justice Project used to produce the ENPOL project's data inputs. 

## Files description

The present repository is structured based on the following filing system:

<img width="1500" alt="FILE" src="https://github.com/aspardog/ENPOL/assets/85714147/3cb8f7f4-4e77-4e0f-bb05-1141138535ac">

Under this system, you will find three main structures:

- Directorio 

- Data Analysis Process 
  - Data_cleaning
  - Descriptives
  - Exploration
  - Hypothesis
  - Data_check
  - Visualization 
  
- Data Generation Process
  - Input
  - Code
  - Output

The Directory file provides an overview of the data generated by the codes and the theoretical variables.

The Data Analysis Process files comprehensively go through all the stages of the data workflow.

The Data Generation files show only the Code output, which contains the R commands data processed the data.

## Workflow

Codes with several functions have the following structure:

1. Upper level: A single file controls all the programs, including the RunMe.R script. This is a "single-call" file, designed just to be called along with a single argument containing the abbreviated name of the country group that we would like to generate the figure. Once it is called, the script will start developing the outputs of the activated functions, such as in the hypothesis.

2. Defining Functions: The RunMe file will call several defining functions. Depending on the stage of the data process.

For codes with a single function, it is established and run in the same code file. 

## How to use

In this repository, the code is thoughtfully provided, offering a transparent and accessible glimpse into the analytical processes undertaken. However, it's important to note that the databases integral to these analyses are not directly accessible within the repository. To faithfully replicate and engage with the presented analyses, interested parties are encouraged to establish direct communication with the authors. This intentional separation between code and databases underscores the authors' commitment to maintaining the integrity of their data sources while facilitating collaboration and further exploration. 

By reaching out to the authors, individuals can not only gain insights into the intricacies of the datasets but also foster a collaborative environment where knowledge exchange and methodological discussions can thrive. This approach not only promotes transparency but also emphasizes the collaborative nature of scientific inquiry, encouraging a dynamic exchange of ideas within the research community.

## Contact

For inqueries please contact Santiago Pardo (spardo@worldjusticeproject.org), Marcelo Torres (mtorres@worldjusticeproject.org),  Cristina Alvarez (calvarez@worldjusticeproject.org).
