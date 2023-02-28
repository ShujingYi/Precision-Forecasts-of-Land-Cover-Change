# Use case
**Clients' Work**:
The client work for the southeastern planning association on water policy, landscape planning, adaption issues.
Try to develop data tools to provide information to their locality so that they can make informed land cover and other planning decisions.
The client don’t make decision, instead, they provide information.

**What they care**:
Climate adaption, conserved land, green infrastructure ( forests, other types of natural landscapes, ecosystem services)_ How those areas are going to be affected by climate change so that they can plan for adaption to maintain the resources into the future.

**How things done now**:
Right now, a lot of regulatory framework doesn’t really take into account climate change. If conditions change, if we lose a lot of these resources that’s presumably going to have a lot of impacts on the lands when it comes to nitrogen phosphorous.

**Project Purpose**:
**Help with future manuscripts the localities, could also help with land trusts and other sorts of non-governmental organizations that are looking at land conservation to potentially prioritize acquisition of those space that might allow for those natural spaces to migrate areas that are more climate resilient.**

Know how land will change and then better plan.
Understanding the quality and quantity of types of impervious services & Differences in these three localities.


**Prediction model**: Predict land cover change (from pervious to impervious)

**Web App**: To create an app to visualize which lands are likely to switch from pervious to impervious.⇒ know the impacts of land cover change, the potential climate impacts and recognize the differences among counties.

### Some questions
1. They said they want to find some lands that are more climate resilient, since pervious lands are more resilient to climate change, so why should we predict the change from pervious to impervious? What factors driven land to change from pervious to impervious? 

# Exploratory Analysis
**How to work with big data?**
- R can't plot the dataset
- What's the alternative? ArcGIS, Python (dask), other suggestions?

**Which are the social, political and economic factors associated with land cover changes**

- economic: underlying driving forces (e.g. influence of markets, distances to a road or a town)
- social: population                  
- political: land-use policies/planning regoin

**Which are the biophysical attributes associated with land cover changes**

- elevation, slope, soil type
- Climate change (temperature, precipitation)
- Flood
- greenhouse gas/carbon emission
- pattern of vegetation change

**Which are the land-cover or land-use modifications associated with land cover changes**

- changes in cropping patterns, input use or tree density of forests
- the distribution and frequency of disturbances (e.g., hurricanes, tornadoes, fires, and insects)                  
- Features derived from original LU/LC data


**What size cell is the appropriate analysis cell**

- Possible cell range: 10m


# Features
1. `originallc` originallandcover

2. `lcp` permeable/impermeable:
< 6 - 0
>=6 - 1

3. `lcchange` whether landcover has changed

4. `lc` 3*3的permeable rate:
0-1（0.1，0.2， 0.3， 0.4， 0.5， 0.6， 0.7， 0.8）

5. `popchange`` per m^2

6. `pctwhitechange`

7. `unitchange` per m^2

8. `medhhincchange` per m^2

9. `road` road

10. `water` water

11. `canopy` tree canopy：
tree canopy - 1
tree canopy over structure -0.5
tree canopy over other -0.5
tree canopy over road - 0.5
other - 0

12. `perm` permeable:
2 -shrub - 0.3
4 - herb - 0.3
5 - wetand - 0.4
other -0

13. `barren` barren:
6 - barren - 1
not barren - 0

14. `struct` structure:
7 - structure - 1
not structure - 0

15. `other` other:
8 - other - 1
not other - 0

16. `terrain`dem

17. `slope`slope

18. `area` m^2 per block group

19. `geoid`-- blockgroup id

20. `soil type`
# Team roles

- Project Management: Shujing Yi

- R Markdown development: Xinge Zhang

- App Development: Yuewen Dai

# Reference
Dale, Virginia H. “THE RELATIONSHIP BETWEEN LAND-USE CHANGE AND CLIMATE CHANGE.” Ecological Applications 7, no. 3 (August 1997): 753–69. https://doi.org/10.1890/1051-0761(1997)007[0753:TRBLUC]2.0.CO;2.

Veldkamp, A, and E.F Lambin. “Predicting Land-Use Change.” Agriculture, Ecosystems & Environment 85, no. 1–3 (June 2001): 1–6. https://doi.org/10.1016/S0167-8809(01)00199-2.

