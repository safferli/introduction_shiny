---
title       : Game Analytics
subtitle    : 
author      : Christoph Safferling
job         : Head of Game Analytics
url         : {lib: "."}    # this is important for reveal.js
framework   : revealjs      # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
revealjs: 
    theme: default
--- ds:intro

<!--
2015-06-02
FU Berlin

-->

<!-- adjust background for "intro" slides -->
<style>
html.intro body {
background:url("./assets/img/ubi-background.jpg");
background-position:center;
background-size: 100%;
} 
</style>


<p style="color: #13DAEC; font-family: 'Lato', sans-serif; font-size: 150%; margin: 0 0 10% 0;">
  Game Analytics <br />
  <span style="font-size: 80%;">a practical introduction</span>
</p>

<p style="font-size: 100%; color: #000000;">
  Christoph Safferling
</p>
<p style="font-size: 80%; margin: 3% 0 0 0; color: #222222;">
  Head of Game Analytics <br/ > 
  Ubisoft Blue Byte
</p>

<!-- fuck CSS... why can't I include this in a custom CSS? -->

<style>
.reveal {color: #231F20;}
.reveal h2, .reveal h3 {color: #222222;}
</style>

--- ds:ubi

## Ubisoft Blue Byte

- founded in 1988, best known for the *Settlers* and *Anno* brand
- fully acquired by Ubisoft in 2001
- online games since 2010
- newest title: [Assassin's Creed: Identity]
  (https://itunes.apple.com/nz/app/assassins-creed-identity/id880971164?mt=8) 
  (mobile)


---

## Christoph Safferling

- since 2012: Head of Game Analytics at Blue Byte
- PhD in economics: [Three Essays on the Economics of Online Games]
  (http://kops.uni-konstanz.de/handle/123456789/17259?locale-attribute=en)
- research specialties: personnel economics, incentive theory, industrial organisation
- academic papers available at [repec.org](https://ideas.repec.org/f/psa961.html)
- contact:
  - mail: christoph.safferling@ubisoft.com
  - Twitter: [@safferli](https://twitter.com/safferli) 
  - LinkedIn: https://www.linkedin.com/in/safferling 
  

--- &vertical

## Live Operation Games

- large online component
- not only f2p games: hybrid and all (future) AAA games
- continuous updating and balancing of the game
- need for data

***

<img src="assets/img/dontworry.jpg" />

***

### Free to Play 

> freemium: the '-mium' is Latin for 'not really'

Southpark, [Season 18, Episode 6](http://southpark.cc.com/full-episodes/s18e06-freemium-isnt-free)

<img src="assets/img/southpark_f2p.gif" />

***

- f2p is a *business model*, not a game type
- time vs money: opens game to a larger audience
- gives customers more impact on gameplay
- *Extra Credits* on 
  - Microtransactions https://www.youtube.com/watch?v=WXA559KNopI&hd=1
  - Doing Free to Play Wrong https://www.youtube.com/watch?v=Mhz9OXy86a0&hd=1


---

## data collection

<!--
- tracking, Kafka, Hadoop
- realtime Storm, Samza
-->

<!-- <img src="assets/img/server-setup.png" /> -->

<img src="assets/img/dwh20-chengdu.png" />


--- &vertical

## Data Scientists

> [A data scientist is] a high-ranking professional with the training and 
> curiosity to make discoveries in the world of big data.

Data Scientist: the sexiest job of the 21st century, 
[Harvard Business Review, Oct 2012]
(https://hbr.org/2012/10/data-scientist-the-sexiest-job-of-the-21st-century/)

***

<img src="assets/img/barycentric-triangle.png" />

***

<img src="assets/img/data-scientist-triangle.png" />

***

### Skills sought after

- domain knowledge and experience
- *"mitdenken"*: think and act independently, and ahead
- statistics and mathematics
- Linux and scripting, mostly R and Python
- database (SQL and NoSQL, Hadoop) knowledge
- communication and presentation skills

<!-- you specifically do *not* need Excel! -->

***

<img src="assets/img/all-the-things.jpg" />


--- &vertical

## Game Analytics

- provide data, data support, and insights to all
  - managers
  - game designers
  - marketing
  - CRM/support
  - LiveOps team (including programmers)
- educate on data and data usage

***

### fancy shit!

- Shiny R: [ACID mission dashboard](http://127.0.0.1:7471/)
- iPython data notebooks: https://github.com/ipython/ipython/wiki/A-gallery-of-interesting-IPython-Notebooks


---

## Conclusion

- modern games rely on data for game design and monetisation
- game analytics provides these data (with interpretations)
- cross-section of all game development departments


---

## ...questions?

<img src="assets/img/cat-menacing.gif" />


---

## Thank you

<img src="assets/img/dankeschoen.jpg" />

<!-- &hellip;and we're hiring! &#9786; 
 &#128522; &#128512; -->


