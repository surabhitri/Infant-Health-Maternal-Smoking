# Infant-Health-Maternal-Smoking
MATERNAL SMOKING AND BIRTH WEIGHTS. These days, it is widely understood that mothers who smoke during pregnancy risk exposing their babies to many health problems. This was not common knowledge fifty years ago. One of the first studies that addressed the issue of pregnancy and smoking was the Child Health and Development Studies, a comprehensive study of all babies born between 1960 and 1967 at the Kaiser Foundation Hospital in Oakland, CA. The original reference for the study is Yerushalmy (1964, American Journal of Obstetrics and Gynecology, pp. 505-518). The data and a summary of the study are in Nolan and Speed (2000, Stat Labs, Chapter 10) and can be found at the book’s website.
The data for this question can be found in the file “babiesdata.csv” on Sakai.

There were about 15,000 families in the study. We will only analyze a subset of the data, in particular 1236 male single births where the baby lived at least 28 days. The researchers interviewed mothers early in their pregnancy to collect information on socioeconomic and demographic characteristics, including an indicator of whether the mother smoked during pregnancy. The variables in the dataset are described in the code book at the end of this document.
Note that this is an observational study, because mothers decided whether or not to smoke during pregnancy; there was no random assignment to smoke or not to smoke. Thus, we cannot make causal inference statements from the results of a standard regression model.

In 1989, the Surgeon General asserted that mothers who smoke have increased rates of premature delivery (before 270 days) and low birth weights. We will analyze the data to see if there is an association between smoking and birth weight. To simplify analyses, we’ll compare babies whose mothers smoke to babies whose mothers have never smoked. The data file you have access to has only these people, although there were other types of smokers in the original dataset.
Our questions of interest include the following.

Do mothers who smoke tend to give birth to babies with lower weights than mothers who do not smoke?
What is a likely range for the difference in birth weights for smokers and non-smokers?
Is there any evidence that the association between smoking and birth weight differs by mother’s race? If so, characterize those differences.
Are there other interesting associations with birth weight that are worth mentioning?
