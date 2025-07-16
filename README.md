# Interconnections-Yale
This is the project I worked for Professor Nicholas Ryan as a Pre-Doc research fellow at Yale during 2024-25
 
This is the project I was working on for Professor Nicholas Ryan at Yale as a pre-doc research fellow during 2024-2025.
The project involved estimating a structural model of dynamic entry of energy firms/power plants joining the California Grid (California Independent System Operator - CAISO). Energy powerplants apply to join the "interconnection" queue and recieve a cost draw from CAISO, which involves a connection cost at the particular location (POI cost) and a network cost which reflects the marginal burned imposed by the power plant on the energy grid via the need to upgrade grid capacity. The cost studies are updates periodically as new firms join and existing firm drop out from the queue. Thus cost studies are in phases - Phase I, Phase II etc.


This repo contains code (software in parenthesis) on the following task I have worked on for the project: 
1. Crawling the individual power plant webpages on CAISO website (restricted access) to download **pdf files** **(2000+)** containing the cost studies given to firms (PYTHON).
2. _Scraping the pdf cost studies for textual data on project charecteristics_ **(PYTHON)** :
    a. Static firm charecteristics - proposed MW, point of connection, latitude, longitude etc. **(PYTHON)**
    b. Dynamic cost data- the tables which provide the break down of various network costs quotes given to the firms. Multiple tables (4-5) were scraped per pdf, usually spread over multiple          pages and with not well justified row and column boundaries. **(PYTHON)**
3. Cleaning the scraped raw data to create a dynamic panel data set of firms static charecteristics and evolving cost draws. **(PYTHON)**
4. Running basic data cleaning, graphical analysis, reduced form regressions of the scraped dynamic panel data. **(R)**
5. Estimating the strctural model. **(Julia)**
