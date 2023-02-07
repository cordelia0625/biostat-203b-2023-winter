*Cordelia Lee*

### Overall Grade: 95/120

### Quality of report: 10/10

- Is the homework submitted (git tag time) before deadline? Take 10 pts per day for late submission. 

    Yes

- Is the final report in a human readable format html? 

    Yes

- Is the report prepared as a dynamic document (R markdown/Quarto) for better reproducibility?

    Yes

- Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how results are produced by just reading the report? Take some points off if the solutions are too succinct to grasp, or there are too many typos/grammar. 

    Yes

### Completeness, correctness and efficiency of solution: 54/70

- Q1 (10/10)

- Q2 (10/20)

    No completion certificate is included. `-10`
    
- Q3 (20/20)

- Q4 (9/10)

    #3: We should explain the meaning of `$1` - `$3` specifically in 'this' shell script. `-1.0`

- Q5 (5/10)
	    
    No interpretation is included. `-5.0`

### Usage of Git: 4/10

- Are branches (`main` and `develop`) correctly set up? Is the hw submission put into the `main` branch?

    Yes

- Are there enough commits (>=5) in develop branch? Are commit messages clear? The commits should span out not clustered the day before deadline.

    Not enough commits in develop branch. `-5.0`
          
- Is the hw1 submission tagged? 

    Yes

- Are the folders (`hw1`, `hw2`, ...) created correctly?

    No `hw1` folder. `-1.0`
  
- Do not put a lot auxiliary files into version control. 

    Yes
    
### Reproducibility: 7/10

- Are the materials (files and instructions) submitted to the `main` branch sufficient for reproducing all the results? Just click the `render` button will produce the final `html`? 

    Q4.1: We should run the wget command to download  `pg42671.txt` in the background by `#| eval: true`. `-1.0`
    
    Q4.3: We should include `middle.sh` in Git or run a code to make this. `-1.0`

    Q4.3: We expect you to display the output on html by `#| eval: true`. `-1.0`
    
- If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

### R code style: 20/20

Take 2 pts off for each violation in R chunk.

- [Rule 2.5](https://style.tidyverse.org/syntax.html#long-lines) The maximum line length is 80 characters.

- [Rule 2.4.1](https://style.tidyverse.org/syntax.html#indenting) When indenting your code, use two spaces.

- [Rule 2.2.4](https://style.tidyverse.org/syntax.html#infix-operators) Place spaces around all infix operators (=, +, -, &lt;-, etc.).

- [Rule 2.2.1.](https://style.tidyverse.org/syntax.html#commas) Do not place a space before a comma, but always place one after a comma.

- [Rule 2.2.2](https://style.tidyverse.org/syntax.html#parentheses) Do not place spaces around code in parentheses or square brackets. Place a space before left parenthesis, except in a function call.
