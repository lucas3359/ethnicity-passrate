
# Goal 

## Trying to find out if three different ethnic groups' pass rates differ among different clusters, especially for lower performance ones  


##### [Ethnicity pass rate by cluster.Rmd](https://github.com/lucas3359/ethnicity-passrate/blob/master/Ethnicity%20pass%20rate%20by%20cluster.Rmd)

---



### Seperate data into defined clusters,
### for each cluster:  
  
  
#### Assumption checks:

- Normality Check:	Shapiro-Wilk normality test (small size sample <500)

```text
shapiro.test()
Lilliefors (Kolmogorov-Smirnov) #normality test
lillie.test()
```
							
- Outliers:	if so, remove 

```text
outliersx5<-boxplot(c5$PASSRATE~c5$ETHNICITY, data = c5,
				ylab="Pass Rate",xlab="Ethnicity",
			main = "compare", col = c("#00AFBB", "#E7B800", "#FC4E07")
						)$out
c5<-c5[-which(c5$PASSRATE %in% outliersx5),]
```
		
- Homogenity Check: 	

```text
leveneTest() 
```


##### If the results are significant(the Homogeneity assumption is not met) by accepting assumption that variances not the same and choose Tamhane's Test 

```text
tamhaneT2Test()
```

##### otherwise 

```text
TukeyHSD() 
```


  


## For those Ethnic group - Pacific Islanders  

### (lower pass rates groups)

#### --- Trying to match maseey students (Pacific students) high school`s home address with the CVS files of formal nz road, suburb and city data with corresponding longtitude& latitude information from government website, trying to match  



##### [addressI(new).R](https://github.com/lucas3359/ethnicity-passrate/blob/master/addressI(new).R)

***


### Overall Schema of the address(new) R code:  
  
  

![alt text](https://github.com/lucas3359/ethnicity-passrate/blob/master/address-schema.png?raw=true)
	