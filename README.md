# ⚽ tidyWorldCupR

Welcome to `tidyWorldCupR` – a comprehensive and tidy toolkit for exploring historical FIFA World Cup data in R! From team statistics to player performance, stadium mapping, and goal visualizations, this package makes World Cup data exploration intuitive, insightful, and fun.

---

## 📦 What This Package Does

- 📁 **Loads** cleaned datasets for World Cup Matches, Players, and Tournament Summaries (1930–2014)
- 🏆 **Analyzes** tournament winners and player achievements
- 🗺️ **Maps** stadiums and country performances using `leaflet`
- 📊 **Visualizes** top goal scorers and national team scoring history
- 🌍 **Converts** country names to ISO codes and emoji flags

---

## 📂 Package Structure

The functions in this package are organized across three main R scripts:

| File Name            | Description |
|----------------------|-------------|
| `getData.R`          | Loads and returns tidy World Cup datasets (`Matches`, `Players`, and `Summarised`) to be used throughout the package. |
| `summaryFunctions.R` | Houses logic for summarizing World Cup results such as `winningTeam()`, `yearsWon()`, and `getMatchInfo()`. |
| `visualFunctions.R`  | Handles all data visualizations and mapping, including `topGoalScorers()`, `stadiumsMap()`, and `countryGoalsMap()`. |

Each script is modularly structured to keep the package maintainable, clean, and easy to expand.


## 🔧 Installation

This package is currently local/developmental. You can install it using:

```r
# devtools or remotes is required for local installation
devtools::install_github("yourusername/tidyWorldCupR")
