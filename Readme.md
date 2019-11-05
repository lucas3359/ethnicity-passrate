
* # Goal 

## Trying to find out if three different ethnic groups' pass rates differ among different clusters (especially for lower performance ones)  


##### *[Ethnicity pass rate by cluster.Rmd](https://github.com/lucas3359/ethnicity-passrate/blob/master/Ethnicity%20pass%20rate%20by%20cluster.Rmd)*

---


* ## Process - R code
### Separate data into defined clusters

### For each cluster:  
  
  
#### Assumption checks:

* ##### Normality Check:	Shapiro-Wilk normality test (small size sample <500)

```text
shapiro.test()
Lilliefors (Kolmogorov-Smirnov) #normality test
lillie.test()
```
							
- ##### Outliers:	if so, remove 

```text
outliersx5<-boxplot(c5$PASSRATE~c5$ETHNICITY, data = c5,
				ylab="Pass Rate",xlab="Ethnicity",
			main = "compare", col = c("#00AFBB", "#E7B800", "#FC4E07")
						)$out
c5<-c5[-which(c5$PASSRATE %in% outliersx5),]
```
		
- ##### Homogenity Check: 	

```text
leveneTest() 
```


#### If the results are significant(the Homogeneity assumption is not met), do ANOVA by accepting assumption that variances not the same and choose Tamhane’s T2

```text
tamhaneT2Test()
```

#### Otherwise use Tukey multiple pairwise-comparisons

```text
TukeyHSD() 
```
<br/><br/><br/>

* ## For those Ethnic group - Pacific Islanders  

#### Trying to match manually entered home addresses with the CSV files of formal NZ road, suburb and city data with corresponding longtitude & latitude information from government website 



##### *[address-cleansing.R](https://github.com/lucas3359/ethnicity-passrate/blob/master/addressI(new).R)*

***


### Overall Schema of the - *address cleansing R code*:  

#### - This describes the logic of the address cleaning
  
<br/><br/>

![alt text](https://github.com/lucas3359/ethnicity-passrate/blob/master/address-schema.png?raw=true)

<br/><br/>	
#### - This is the end user dashboard where the raw data can be compared to the validated data

<br/><br/>

![alt text](https://github.com/lucas3359/ethnicity-passrate/blob/master/map%20visualization.jpg)