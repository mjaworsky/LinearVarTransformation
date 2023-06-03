###R Version 4.1.2 (64bit)

library(dplyr)
library(janeaustenr)
library(tidytext)
library(ggplot2)
library(tm)
library(rlist)

library(wordcloud)
library(RColorBrewer)
library(wordcloud2)

#'male female diabetes cancer pregnant injury teeth tooth disease obese overweight decay gum smoked myocardial infarction heart drinks cigarettes tobacco asthma coronary alcohol considering hepatitis confusion cannabis vaccine flu zoster fatigue injected myalgic encephalomyelitis pneumonia pneumococcal eye dilated dementia mammogram pap screening cervical cancer hpv hysterectomy antigen prostate psa colon stool hiv blood fluid month sexually insulin sugar glucose feet'


lm_corpus <- Corpus(VectorSource('male female diabetes cancer pregnant injury teeth tooth disease obese overweight decay gum smoked myocardial infarction heart drinks cigarettes tobacco asthma coronary alcohol considering hepatitis confusion cannabis vaccine flu zoster fatigue injected myalgic encephalomyelitis pneumonia pneumococcal eye dilated dementia mammogram pap screening cervical cancer hpv hysterectomy antigen prostate psa colon stool hiv blood fluid month sexually insulin sugar glucose feet'))
lm_corpus_00 = tm_map(lm_corpus, content_transformer(tolower))
lm_corpus_00 = tm_map(lm_corpus_00, removePunctuation)
lm_corpus_00 = tm_map(lm_corpus_00, removeWords, stopwords("english"))
LM_00 <- lm_corpus_00$content
LM_00 <- as.list(LM_00)

df_LM_00 <- as.data.frame(do.call(rbind, LM_00))
colnames(df_LM_00) <- c("text")

LM_words <- df_LM_00 %>%
  unnest_tokens(word, text) 
  

#Define BRFSS Survey questions as individual text vectors
Q1 <- tolower(c('Is','this','PHONE','NUMBER'))
Q2 <- tolower(c('Is','this','a','private','residence'))							
Q3 <- tolower(c('Do','you','live','in','college','housing'))							
Q4 <- tolower(c('Do','you','currently','live','in','state'))							
Q5 <- tolower(c('Is','this','a','cell','phone'))							
Q6 <- tolower(c('Are','you','18','years','of','age','or','older'))							
Q7 <- tolower(c('Are','you','male','or','female'))							
Q8 <- tolower(c('I','need','to','randomly','select','one','adult','who','lives','in','your','household','to','be','interviewed.','Excluding','adults','living','away','from','home','such','as','students','away','at','college','how','many','members','of','your','household','including','yourself','are','18','years','of','age','or','older'))
Q9 <- tolower(c('Are','you','male','or','female'))							
Q10 <- tolower(c('How','many','of','these','adults','are','men'))							
Q11 <- tolower(c('So','the','number','of','women','in','the','household','is','X','Is','that','correct'))							
Q12 <- tolower(c('The','person','in','your','household','that','I','need','to','speak','with','is','Oldest','Youngest','Middle','Male','Female','Are','you','the','Oldest','Youngest','Middle','Male','Female','in','this','household'))							
Q13 <- tolower(c('Is','this','a','safe','time','to','talk','with','you'))							
Q14 <- tolower(c('Is','this','PHONE','NUMBER'))							
Q15 <- tolower(c('Is','this','a','cell','phone'))							
Q16 <- tolower(c('Are','you','18','years','of','age','or','older'))							
Q17 <- tolower(c('Are','you','male','or','female'))							
Q18 <- tolower(c('Do','you','live','in','a','private','residence'))							
Q19 <- tolower(c('Do','you','live','in','college','housing'))							
Q20 <- tolower(c('Do','you','currently','live','in','state'))							
Q21 <- tolower(c('In','what','state','do','you','currently','live'))							
Q22 <- tolower(c('Do','you','also','have','a','landline','telephone','in','your','home','that','is','used','to','make','and','receive','calls'))						
Q23 <- tolower(c('How','many','members','of','your','household','including','yourself','are','18','years','of','age','or','older'))							
Q24 <- tolower(c('Would','you','say','that','in','general','your','health','is','Excellent','Very','Good','Good','Fair','Poor'))	
Q25 <- tolower(c('Now','thinking','about','your','physical','health','which','includes','physical','illness','and','injury','for','how','many','days','during','the','past','30','days','was','your','physical','health','not','good'))		
Q26 <- tolower(c('Now','thinking','about','your','mental','health','which','includes','stress','depression','and','problems','with','emotions','for','how','many','days','during','the','past','30','days','was','your','mental','health','not','good'))					
Q27 <- tolower(c('During','the','past','30','days','for','about','how','many','days','did','poor','physical','or','mental','health','keep','you','from','doing	your','usual','activities','such','as','self','care','work','or','recreation'))		
Q28 <- tolower(c('What','is','the','current','primary','source','of','your','health','insurance'))			
Q29 <- tolower(c('Do','you','have','one','person','or','a','group','of','doctors','that','you','think','of','as','your','personal','health','care','provider'))			
Q30 <- tolower(c('Was','there','a','time','in','the','past','12','months','when','you','needed','to','see','a','doctor','but','could','not','because','you','could','not','afford','it'))
Q31 <- tolower(c('About','how','long','has','it','been','since','you','last','visited','a','doctor','for','a','routine','checkup'))			
Q32 <- tolower(c('During','the','past','month','other','than','your','regular','job','did','you','participate','in','any','physical','activities','or','exercises','such','as','running','calisthenics','golf','gardening','or','walking','for','exercise'))						
Q33 <- tolower(c('Have','you','ever','been','told','by','a','doctor','nurse','or','other','health','professional','that','you','have','high','blood','pressure'))						
Q34 <- tolower(c('Are','you	currently','taking','prescription','medicine','for','your','high','blood','pressure'))						
Q35 <- tolower(c('Cholesterol','is','a','fatty','substance','found','in','the','blood.','About','how','long','has','it','been','since','you','last','had','your','cholesterol','checked'))			
Q36 <- tolower(c('Have','you','ever','been','told','by','a','doctor','nurse','or','other','health','professional','that','your','cholesterol','is','high'))					
Q37 <- tolower(c('Are','you	currently','taking','medicine','prescribed','by','your','doctor','or','other','health','professional','for','your','cholesterol'))					
Q38 <- tolower(c('Has','a','doctor','nurse','or','other','health','professional','ever','told','you','that','you','had','any','of','the','following','For','each','tell','me','Yes','No','Or','You','are','Not','Sure'))						
Q39 <- tolower(c('Ever','told','you','that','you','had','a','heart','attack','also','called','a','myocardial','infarction'))
Q40 <- tolower(c('Ever','told','you','had','angina','or','coronary','heart','disease'))
Q41 <- tolower(c('Ever','told','you','had','a','stroke'))
Q42 <- tolower(c('Ever','told','you','had','asthma'))
Q43 <- tolower(c('Do','you','still','have','asthma'))						
Q44 <- tolower(c('Ever','told','you','had','skin','cancer'))						
Q45 <- tolower(c('Ever','told','you','had','any','other','types','of','cancer'))
Q46 <- tolower(c('Ever','told','you','had','COPD','chronic','obstructive','pulmonary','disease','emphysema','or','chronic','bronchitis'))
Q47 <- tolower(c('Ever','told','you','had','a','depressive','disorder','including','depression','major','depression','dysthymia','or','minor','depression'))
Q48 <- tolower(c('Not','including','kidney','stones','bladder','infection','or','incontinence','were','you','ever','told','you','had','kidney','disease'))
Q49 <- tolower(c('Ever','told','you','had','diabetes'))
Q50 <- tolower(c('How','old','were','you','when','you','were','told','you','had','diabetes'))						
Q51 <- tolower(c('Has','a','doctor','nurse','or','other','health','professional','ever','told','you','that','you','had','some','form','of','arthritis','rheumatoid','arthritis','gout','lupus','or','fibromyalgia'))						
Q52 <- tolower(c('Has','a','doctor','or','other','health','professional','ever','suggested','physical','activity','or','exercise','to','help','your','arthritis','or','joint','symptoms'))	
Q53 <- tolower(c('Have','you','ever','taken','an','educational','course','or','class','to','teach','you','how','to','manage','problems','related','to','your','arthritis','or','joint','symptoms'))	
Q54 <- tolower(c('Are','you','now','limited','in','any','way','in','any','of','your','usual','activities','because','of','arthritis','or','joint','symptoms'))	
Q55 <- tolower(c('In','the','next','question','we','are','referring','to','work','for','pay','Do','arthritis','or','joint','symptoms','now','affect','whether','you','work','the','type','of','work','you','do','or','the','amount','of','work','you','do'))						
Q56 <- tolower(c('Please','think','about','the','past','30','days','keeping','in','mind','all','of','your','joint','pain','or','aching','and','whether','or','not','you','have','taken','medication'))				
Q57 <- tolower(c('During','the','past','30','days','how','bad','was','your','joint','pain','on','average','on','a','scale','of','0','to','10','where','0','is','no','pain','and','10','is','pain','or','aching','as','bad','as','it','can','be'))						
Q58 <- tolower(c('What','is','your','age'))		
Q59 <- tolower(c('Are','you','Hispanic','Latino','a','or','Spanish','origin'))		
Q60 <- tolower(c('Which','one','or','more','of','the','following','would','you','say','is','your','race','White','Black','or','African','American','American','Indian','or','Alaska','Native','Asian','Asian','Indian','Chinese','Filipino','Japanese','Korean','Vietnamese','Other','Asian','Pacific','Islander','Native','Hawaiian','Guamanian','or','Chamorro','Samoan','Other','Pacific','Islander'))
Q61 <- tolower(c('Are','you','Married','Divorced','Widowed','Separated','Never','married','Or','A','member','of','an','unmarried'))
Q62 <- tolower(c('What','is','the','highest','grade','or','year','of','school','you','completed'))
Q63 <- tolower(c('Do','you','own','or','rent','your','home'))		
Q64 <- tolower(c('In','what','county','do','you','currently','live'))		
Q65 <- tolower(c('What','is','the','ZIP','Code','where','you','currently','live'))		
Q66 <- tolower(c('Not','including','cell','phones','or','numbers','used','for','computers','fax','machines','or','security','systems','do','you','have','more','than','one','telephone','number','in','your','household'))
Q67 <- tolower(c('How','many','of','these','telephone','numbers','are','residential','numbers'))		
Q68 <- tolower(c('How','many','cell','phones','do','you','have','for','personal','use'))		
Q69 <- tolower(c('Have','you','ever','served','on','active','duty','in','the','United','States','Armed','Forces','either','in','the','regular','military','or','in','a','National','Guard','or','military','reserve','unit'))
Q70 <- tolower(c('Are','you','currently','Employed','for','wages','Self','employed','Out','of','work','for','1','year','or','more','Out','of','work','for','less','than','1','year','A','Homemaker','A','Student','Retired','Or','Unable','to','work'))
Q71 <- tolower(c('How','many','children','less','than','18','years','of','age','live','in','your','household'))
Q72 <- tolower(c('Is','your','annual','household','income','from','all','sources'))
Q73 <- tolower(c('To','your','knowledge','are','you','now','pregnant'))
Q74 <- tolower(c('About','how','much','do','you','weigh','without','shoes'))		
Q75 <- tolower(c('About','how','tall','are','you','without','shoes'))
Q76 <- tolower(c('Some','people','who','are','deaf','or','have','serious','difficulty','hearing','use','assistive','devices','to','communicate','by','phone.','Are','you','deaf','or','do','you','have','serious','difficulty','hearing'))
Q77 <- tolower(c('Are','you','blind','or','do','you','have','serious','difficulty','seeing','even','when','wearing','glasses'))						
Q78 <- tolower(c('Because','of','a','physical','mental','or','emotional','condition','do','you','have','serious','difficulty','concentrating','remembering','or','making','decisions'))
Q79 <- tolower(c('Do','you','have','serious','difficulty','walking','or','climbing','stairs'))						
Q80 <- tolower(c('Do','you','have','difficulty','dressing','or','bathing'))						
Q81 <- tolower(c('Because','of','a','physical','mental','or','emotional','condition','do','you','have','difficulty','doing','errands','alone','such','as','visiting','a','doctors','office','or','shopping'))
Q82 <- tolower(c('Have','you','smoked','at','least','100','cigarettes','in','your','entire','life'))	
Q83 <- tolower(c('Do','you','now','smoke','cigarettes','every','day','some','days','or','not','at','all'))					
Q84 <- tolower(c('Do','you','currently','use','chewing','tobacco','snuff','or','snus','every','day','some','days','or','not','at','all'))
Q85 <- tolower(c('Do','you','now','use','ecigarettes','or','other','electronic','vaping','products','every','day','some','days','or','not','at','all'))					
Q86 <- tolower(c('During','the','past','30','days','how','many','days','per','week','or','per','month','did','you','have','at','least','one','drink','of','any','alcoholic','beverage','such','as','beer','wine','a','malt','beverage','or','liquor'))						
Q87 <- tolower(c('One','drink','is','equivalent','to','a','12','ounce','beer','a','5','ounce','glass','of','wine','or','a','drink','with','one','shot','of','liquor'))
Q88 <- tolower(c('During','the','past','30','days','on','the','days','when','you','drank','about','how','many','drinks','did','you','drink','on','the','average'))
Q89 <- tolower(c('During','the','past	30','days','what','is','the','largest','number','of','drinks','you','had','on','any','occasion'))
Q90 <- tolower(c('During','the','past','12','months','have','you','had','either','a','flu','vaccine','that','was','sprayed','in','your','nose','or','a','flu','shot','injected','into','your','arm'))					
Q91 <- tolower(c('During','what','month','and','year','did','you','receive','your','most','recent','flu','vaccine','that','was','sprayed','in','your','nose','or','flu','shot','injected','into','your','arm'))
Q92 <- tolower(c('At','what','kind','of	place','did','you','get','your','last','flu	shot','or','vaccine'))						
Q93 <- tolower(c('Have','you','ever','had','a','pneumonia','shot','also','known','as','a','pneumococcal','vaccine'))						
Q94 <- tolower(c('Including','fluid','testing','from','your','mouth','but','not','including','tests','you','may','have','had','for','blood','donation','have','you','ever','been','tested','for','HIV'))				
Q95 <- tolower(c('Not','including','blood','donations','in','what','month','and','year','was','your','last','HIV','test'))
Q96 <- tolower(c('Now','think','about','the','foods','you','ate','or','drank','during','the','past','month','that','is','the','past','30','days','including','meals','and','snacks','Not','including','juices','how','often','did','you','eat','fruit','You','can','tell','me','times','per','day','times','per','week','or','times','per','month'))	
Q97 <- tolower(c('Not','including','fruit','flavored','drinks','or','fruit','juices','with','added','sugar','how','often','did','you','drink','100','percent','fruit','juice','such','as','apple','or','orange','juice'))
Q98 <- tolower(c('How','often','did','you','eat','a','green','leafy','or','lettuce','salad','with','or','without','other','vegetables'))
Q99 <- tolower(c('How','often','did','you','eat','any','kind','of','fried','potatoes','including','French','fries','home','fries','or','hash','browns'))		
Q100 <- tolower(c('How','often','did','you','eat','any','other','kind','of','potatoes','or','sweet','potatoes','such','as','baked','boiled','mashed','potatoes','or','potato','salad'))						
Q101 <- tolower(c('Not','including','lettuce','salads','and','potatoes','how','often','did','you','eat','other','vegetables'))						
Q102 <- tolower(c('Have','you','had','a','test','for','high','blood','sugar','or','diabetes','within','the','past','three','years'))
Q103 <- tolower(c('Have','you','ever','been','told','by','a','doctor','or','other','health','professional','that','you','have','pre','diabetes','or','borderline','diabetes'))
Q104 <- tolower(c('Are','you','now','taking','insulin'))					
Q105 <- tolower(c('About','how','often','do','you','check','your','blood','for','glucose','or','sugar'))					
Q106 <- tolower(c('Including','times','when','checked','by','a','family','member','or','friend','about','how','often','do','you','check','your','feet','for','any','sores','or','irritations'))
Q107 <- tolower(c('About','how','many','times','in','the','past','12','months','have','you','seen','a','doctor','nurse','or','other','health','professional','for','your','diabetes'))
Q108 <- tolower(c('About','how','many','times','in','the','past','12','months','has','a','doctor','nurse','or','other','health	professional','checked','you','for','A','one','C'))
Q109 <- tolower(c('About','how','many','times','in','the','past','12','months','has','a','health','professional','checked','your','feet','for','any','sores','or','irritations'))
Q110 <- tolower(c('When','was','the','last','time','you','had','an','eye','exam','in','which','the','pupils','were','dilated','making','you','temporarily','sensitive','to','bright','light'))
Q111 <- tolower(c('Has','a','doctor','ever','told','you','that','diabetes','has','affected','your','eyes','or','that','you','had','retinopathy'))	
Q112 <- tolower(c('Have','you','ever','taken','a','course','or','class','in','how','to','manage','your','diabetes','yourself'))
Q113 <- tolower(c('Have','you','ever','been','told','by','a','doctor','or','other','health','professional','that','you','had','Chronic','Fatigue','Syndrome','CFS','or','Myalgic','Encephalomyelitis','ME'))
Q114 <- tolower(c('Do','you','still','have','Chronic','Fatigue','Syndrome','CFS','or','Myalgic','Encephalomyelitis','ME'))
Q115 <- tolower(c('Thinking','about','your','CFS','or','ME','during','the','past','6','months','how','many','hours','a','week','on','average','have','you','been','able','to','work','at','a','job','or','business','for','pay'))								
Q116 <- tolower(c('Have','you','ever','been','told','by','a','doctor','or','other','health','professional','that','you','had','Hepatitis','C'))						
Q117 <- tolower(c('Were','you','treated','for','Hepatitis','C','in','2015','or','after'))						
Q118 <- tolower(c('Were','you','treated','for','Hepatitis','C','prior','to','2015'))						
Q119 <- tolower(c('Do','you','still','have','Hepatitis','C'))						
Q120 <- tolower(c('The','next','question','is','about','Hepatitis','B.','Has','a','doctor','nurse','or','other','health','professional','ever','told','you','that','you','had','hepatitis','B'))		
Q121 <- tolower(c('Are','you','currently','taking','medicine','to','treat','hepatitis','B'))						
Q122 <- tolower(c('Have','you','ever','had','an','HPV','vaccination'))
Q123 <- tolower(c('How','many','HPV','shots','did','you','receive'))			
Q124 <- tolower(c('Have','you','received','a','tetanus','shot','in','the','past','10','years'))				
Q125 <- tolower(c('Have','you','ever','had','the','shingles','or','zoster','vaccine'))				
Q126 <- tolower(c('Since','DATE','OF','VACCINE','AVAILABILITY','have','you','had','a','COVID','19','vaccination'))				
Q127 <- tolower(c('How','many','COVID','19','vaccinations','have','you','received'))	
Q128 <- tolower(c('During','what','month','and','year','did','you','receive','your','first','COVID','19','vaccination'))		
Q129 <- tolower(c('At','what','kind','of','place','did','you','get','your','first','COVID','19','vaccination'))
Q130 <- tolower(c('During','what','month','and','year','did','you','receive','your','second','COVID','19','vaccination'))
Q131 <- tolower(c('At','what','kind','of','place','did','you','get','your','second','COVID','19','vaccination'))
Q132 <- tolower(c('You','have','told','us','that','you','have','smoked','in','the','past','or','are','currently','smoking','The','next','questions','are','about','screening','for','lung','cancer','How','old','were','you','when','you','first','started','to','smoke','cigarettes','regularly'))		
Q133 <- tolower(c('How','old','were','you','when','you','last','smoked','cigarettes','regularly'))				
Q134 <- tolower(c('On','average','when','you','smoke','smoked','regularly','about','how','many','cigarettes','do','did','you','usually','smoke','each','day'))
Q135 <- tolower(c('The','next','question','is','about','CT','or','CAT','scans.','During','this','test','you','lie','flat','on','your','back','on','a','table.','While','you','hold','your','breath','the','table','moves','through','a','donut','shaped','xray','machine','while','the','scan','is','done','In','the','last','12','months','did','you','have','a','CT','or','CAT','scan'))
Q136 <- tolower(c('The','next','questions','are','about','breast','and','cervical','cancer','Have','you','ever','had','a','mammogram'))					
Q137 <- tolower(c('How','long','has','it','been','since','you','had','your','last','mammogram'))					
Q138 <- tolower(c('Have','you','ever','had','a','cervical','cancer','screening','test'))					
Q139 <- tolower(c('How','long','has','it','been','since','you','had','your','last','cervical','cancer','screening','test'))					
Q140 <- tolower(c('At','your','most','recent','cervical','cancer','screening','did','you','have','a','Pap','test'))
Q141 <- tolower(c('At','your','most','recent','cervical','cancer','screening','did','you','have','an','HPV','test'))
Q142 <- tolower(c('Have','you','had','a','hysterectomy'))					
Q143 <- tolower(c('Have','you','ever','had','a','PSA','test'))
Q144 <- tolower(c('About','how','long','has','it','been','since','your','most','recent','PSA','test'))
Q145 <- tolower(c('What','was','the','main','reason','you','had','this','PSA','test','was','it'))
Q146 <- tolower(c('Did','a','doctor','nurse','or','other','health','professional','EVER','talk','with','you','about','the','advantages','the','disadvantages','or','both','advantages','and','disadvantages','of','the','Prostate','Specific','Antigen','or','PSA','test'))				
Q147 <- tolower(c('Colonoscopy','and','sigmoidoscopy','are','exams','to','check','for','colon','cancer','Have','you','ever','had','either','of','these','exams'))
Q148 <- tolower(c('Have','you','had','a','colonoscopy','a','sigmoidoscopy','or','both'))						
Q149 <- tolower(c('How','long','has','it','been','since','your','most','recent','colonoscopy'))						
Q150 <- tolower(c('How','long','has','it','been','since','your','most','recent','sigmoidoscopy'))						
Q151 <- tolower(c('How','long','has','it','been','since','your','most','recent','colonoscopy','or','sigmoidoscopy'))						
Q152 <- tolower(c('Have','you','ever','had','any','other','kind','of','test','for','colorectal','cancer','such','as','virtual','colonoscopy','CT','colonography','blood','stool','test','FIT','DNA','or','Cologuard','test'))
Q153 <- tolower(c('A','virtual','colonoscopy','uses','a','series','of','Xrays','to','take','pictures','of','inside','the','colon','Have','you','ever','had','a','virtual','colonoscopy'))
Q154 <- tolower(c('When','was','your','most','recent','CT','colonography','or','virtual','colonoscopy'))
Q155 <- tolower(c('One','stool','test','uses','a','special','kit','to','obtain','a','small','amount','of','stool','at','home','and','returns','the','kit','to','the','doctor','or','the','lab','Have','you','ever','had','this','test'))						
Q156 <- tolower(c('How','long','has','it','been','since','you','had','this','test'))						
Q157 <- tolower(c('Another','stool','test','uses','a','special','kit','to','obtain','an','entire','bowel','movement','at','home','and','returns','the','kit','to','a','lab.','Have','you','ever','had','this','Cologuard','test'))
Q158 <- tolower(c('Was','the','blood','stool','or','FIT','you','reported','earlier','conducted','as','part','of','a','Cologuard','test'))
Q159 <- tolower(c('How','long','has','it','been','since','you','had','this','test'))						
Q160 <- tolower(c('You','have','told','us','that','you','have','had','cancer','I','would','like','to','ask','you','a','few','more','questions','about','your','cancer','How','many','different','types','of','cancer','have','you','had'))	
Q161 <- tolower(c('At','what','age','were','you','told','that','you','had','cancer'))	
Q162 <- tolower(c('What','type','of','cancer','was','it'))	
Q163 <- tolower(c('Are','you','currently','receiving','treatment','for','cancer'))				
Q164 <- tolower(c('What','type','of','doctor','provides','the','majority','of','your','health','care','Is','it','a','Cancer','Surgeon','Family','Practitioner','General','Surgeon','Gynecologic','Oncologist','General','Practitioner','Internist','Plastic','Surgeon','Reconstructive','Surgeon','Medical','Oncologist','Radiation','Oncologist','Urologist','Other'))
Q165 <- tolower(c('Did','any','doctor','nurse','or','other','health','professional','ever','give','you','a','written','summary','of','all','the','cancer','treatments','that','you','received'))		
Q166 <- tolower(c('Have','you','ever','received','instructions','from','a','doctor','nurse','or','other','health','professional','about','where','you','should','return','or','who','you','should','see','for','routine','cancer','check','ups','after','completing','your','treatment','for','cancer'))
Q167 <- tolower(c('Were','these','instructions','written','down','or','printed','on','paper','for','you'))				
Q168 <- tolower(c('With','your','most','recent','diagnosis','of','cancer','did','you','have','health','insurance','that','paid','for','all','or','part','of','your','cancer','treatment'))
Q169 <- tolower(c('Were','you','ever','denied','health','insurance','or','life','insurance','coverage','because','of','your','cancer'))
Q170 <- tolower(c('Did','you','participate','in','a','clinical','trial','as','part','of','your','cancer','treatment'))
Q171 <- tolower(c('Do','you','currently','have','physical','pain','caused','by','your','cancer','or','cancer','treatment'))
Q172 <- tolower(c('Would','you','say','your','pain','is','currently','under','control','With','medication','or','treatment','Without','medication','or','treatment','Not','under','control','with','medication','or','treatment','Not','under','control','without','medication','or','treatment'))
Q173<- tolower(c('Has','your','doctor','nurse','or','other','health','professional','recommended','you','check','your','blood','pressure','outside','of','the','office','or','at','home'))
Q174 <- tolower(c('Do','you','regularly','check','your','blood','pressure','outside','of','your','healthcare','professional','s','office','or','at','home'))
Q175 <- tolower(c('Do','you','take','it','mostly','at','home','or','on','a','machine','at','a','pharmacy','grocery','or','similar','location'))						
Q176<- tolower(c('How','do','you','share','your','blood','pressure','numbers','that','you','collected','with','your','health','professional','Is','it','mostly','by','telephone','other','methods','such','as','emails','internet','portal','or','fax','or','in','person'))	
Q177 <- tolower(c('Are','you','currently','watching','or','reducing','your	sodium','or','salt','intake'))
Q178 <- tolower(c('Has','a','doctor','or','other','health','professional','ever','advised','you','to','reduce','sodium','or','salt','intake'))
Q179 <- tolower(c('During','the','past','12','months','have','you','experienced','confusion','or','memory','loss','that','is','happening','more','often','or','is','getting','worse'))
Q180 <- tolower(c('During','the','past','12','months','as','a','result','of','confusion','or','memory','loss','how','often','have','you','given','up','day','to','day','household','activities','or','chores','you','used','to','do','such','as','cooking','cleaning','taking','medications','driving','or','paying','bills','Would','you','say','it','is','Always','Usually','Sometimes','Rarely','Never'))
Q181 <- tolower(c('When','you','need','help','with','these','day','to','day','activities','how','often','are','you','able','to','get','the','help','that','you','need','Would','you','say','it','is','Always','Usually','Sometimes','Rarely','Never'))
Q182 <- tolower(c('During','the','past','12','months','how','often','has','confusion','or','memory','loss','interfered','with','your','ability','to','work','volunteer','or','engage','in','social','activities','outside','the','home','Would','you','say','it','is','Always','Usually','Sometimes','Rarely','Never'))
Q183 <- tolower(c('Have','you','or','anyone','else','discussed','your','confusion','or','memory','loss','with','a','health','care','professional'))
Q184 <- tolower(c('During','the','past','30','days','did','you','provide','regular','care','or','assistance','to','a','friend','or','family','member','who','has','a','health','problem','or','disability','What','is','his','or','her','relationship','to','you'))				
Q185 <- tolower(c('For','how','long','have','you','provided','care','for','that','person'))				
Q186 <- tolower(c('In','an','average','week','how','many','hours','do','you','provide','care','or','assistance'))				
Q187 <- tolower(c('What','is','the','main','health','problem','long','term','illness','or','disability','that','the','person','you','care','for','has'))	
Q188 <- tolower(c('Does','the','person','you','care','for','also','have','Alzheimer','disease','dementia','or','other','cognitive','impairment','disorder'))	
Q189 <- tolower(c('In','the','past','30','days','did','you','provide','care','for','this','person','by','managing','personal','care','such','as','giving','medications','feeding','dressing','or','bathing'))
Q190 <- tolower(c('In','the','past','30','days','did','you','provide','care','for','this','person','by','managing','household','tasks','such','as','cleaning','managing','money','or','preparing','meals'))				
Q191 <- tolower(c('In','the','next','2','years','do','you','expect','to','provide','care','or','assistance','to','a','friend','or','family','member','who','has','a','health','problem','or','disability'))
Q192 <- tolower(c('Now','looking','back','before','you','were','18','years','of','age','Did','you','live','with','anyone','who','was','depressed','mentally','ill','or','suicidal'))
Q193 <- tolower(c('Did','you','live','with','anyone','who','was','a','problem','drinker','or','alcoholic'))												
Q194 <- tolower(c('Did','you','live','with','anyone','who','used','illegal','street','drugs','or','who','abused','prescription','medications'))
Q195 <- tolower(c('Did','you','live','with','anyone','who','served','time','or','was','sentenced','to','serve','time','in','a','prison','jail','or','other','correctional','facility'))		
Q196 <- tolower(c('Were','your','parents','separated','or','divorced'))												
Q197 <- tolower(c('How','often','did','your','parents','or','adults','in','your','home','ever','slap','hit','kick','punch','or','beat','each','other','up','Was','it','Never','Once','More','than','once'))
Q198 <- tolower(c('Not','including','spanking','before','age','18','how','often','did','a','parent','or','adult','in','your','home','ever','hit','beat','kick','or','physically','hurt','you','in','any','way','Was','it','Never','Once','More','than','once'))									
Q199 <- tolower(c('How','often','did','a','parent','or','adult','in','your','home','ever','swear','at','you','insult','you','or','put','you','down','Was','it','Never','Once','More','than','once'))
Q200 <- tolower(c('How','often','did','anyone','at','least','5','years','older','than','you','or','an','adult','ever','touch','you','sexually','Was','it','Never','Once','More','than','once'))
Q201 <- tolower(c('How','often','did','anyone','at','least','5','years','older','than','you','or','an','adult','try','to','make','you','touch','them','sexually','Was','it','Never','Once','More','than','once'))
Q202 <- tolower(c('How','often','did','anyone','at','least','5','years','older','than','you','or','an','adult','force','you','to','have','sex','Was','it','Never','Once','More','than','once'))
Q203 <- tolower(c('For','how','much','of','your','childhood','was','there','an','adult','in','your','household','who','made','you','feel','safe','and','protected','Would','you','say','never','a','little','of','the','time','some','of','the','time','most','of','the','time','or','all','of','the','time'))												
Q204 <- tolower(c('For','how','much','of','your','childhood','was','there','an','adult','in','your','household','who','tried','hard','to','make','sure','your','basic','needs','were','met','Would','you','say','never','a','little','of','the','time','some','of','the','time','most','of','the','time','or','all','of','the','time'))												
Q205 <- tolower(c('During','the','past','30','days','on','how','many','days','did','you','use','marijuana','or','cannabis'))				
Q206 <- tolower(c('During','the','past','30','days','which','one','of','the','following','ways','did','you','use','marijuana','the','most','often','Did','you','usually','Smoke','it','for','example','in','a','joint','bong','pipe','or','blunt','Eat','it','for','example','in','brownies','cakes','cookies','or','candy','Drink','it','for','example','in','tea','cola','or','alcohol','Vaporize','it','for','example','in','an','ecigarette','like','vaporizer','or','another','vaporizing','device','Dab','it','for','example','using','a','dabbing','rig','knife','or','dab','pen','or','Use','it','some','other','way'))
Q207 <- tolower(c('When','you','used','marijuana','or','cannabis','during','the','past','30','days','was','it','usually','For','medical','reasons','For','non','medical','reasons','or','For','both','medical','and','non','medical','reasons'))
Q208 <- tolower(c('How','long','has','it','been','since','you','last','smoked','a','cigarette','even','one','or','two','puffs'))					
Q209 <- tolower(c('During','the','past','12','months','have','you','stopped','smoking','for','one','day','or','longer','because','you','were','trying','to','quit','smoking'))
Q210 <- tolower(c('Are','any','firearms','now','kept','in','or','around','your','home'))						
Q211 <- tolower(c('Are','any','of','these','firearms','now','loaded'))						
Q212 <- tolower(c('Are','any','of','these','loaded','firearms','also','unlocked'))						
Q213 <- tolower(c('What','kind','of','work','do','you','do','For','example','registered','nurse','janitor','cashier','auto','mechanic'))	
Q214 <- tolower(c('What','kind','of','business','or','industry','do','you','work','in','For','example','hospital','elementary','school','clothing','manufacturing','restaurant'))	
Q215 <- tolower(c('What','is','the','birth','month','and','year','of','the','Xth','child'))			
Q216 <- tolower(c('Is','the','child','a','boy','or','a','girl'))				
Q217 <- tolower(c('Is','the','child','Hispanic','Latino','a','or','Spanish','origin'))				
Q218 <- tolower(c('Which','one','or','more','of','the','following','would','you','say','is','the','race','of','the','child','White','Black','or','African','American','American','Indian','or','Alaska','Native','Asian','Asian','Indian','Chinese','Filipino','Japanese','Korean','Vietnamese','Other','Asian','Pacific','Islander','Native','Hawaiian','Guamanian','or','Chamorro','Samoan','Other','Pacific','Islander'))
Q219 <- tolower(c('Which','one','of','these','groups','would','you','say','best','represents','the','child’s','race','White','Black','or','African','American','American','Indian','or','Alaska','Native','Asian','Asian','Indian','Chinese','Filipino','Japanese','Korean','Vietnamese','Other','Asian','Pacific','Islander','Native','Hawaiian','Guamanian','or','Chamorro','Samoan','Other','Pacific','Islander'))
Q220 <- tolower(c('The','next','two','questions','are','about','the','Xth','child','Has','a','doctor','nurse','or','other','health','professional','EVER','said','that','the','child','has','asthma'))
Q221 <- tolower(c('Does','the','child','still','have','asthma'))
Q222 <- tolower(c('What','was','your','sex','at','birth','Was','it','male','or','female'))
Q223 <- tolower(c('Which','of','the','following','best','represents','how','you','think','of','yourself','Gay','Straight','that','is','not','gay','Bisexual','Something','else','I','do','not','know','the','answer'))
Q224 <- tolower(c('Which','of','the','following','best','represents','how','you','think','of','yourself','Lesbian','or','Gay','Straight','that','is','not','gay','Bisexual','Something','else','I','do','not','know','the','answer'))
Q225 <- tolower(c('Do','you','consider','yourself','to','be','transgender'))		
Q226 <- tolower(c('Would','it','be','okay','if','we','called','you','back','to','ask','additional','asthma','related','questions','at','a','later','time'))
Q227 <- tolower(c('Which','person','in','the','household','was','selected','as','the','focus','of','the','asthma','call','back'))
Q228 <- tolower(c('Can','I','please','have','either','your','your','child','first','name','or','initials','so','we','will','know','who','to','ask','for','when','we','call','back'))

  


LM_Q1 <- LM_words %>% filter(word %in% Q1) 
LM_Q2 <- LM_words %>% filter(word %in% Q2) 
LM_Q3 <- LM_words %>% filter(word %in% Q3) 
LM_Q4 <- LM_words %>% filter(word %in% Q4) 
LM_Q5 <- LM_words %>% filter(word %in% Q5) 
LM_Q6 <- LM_words %>% filter(word %in% Q6) 
LM_Q6 <- LM_words %>% filter(word %in% Q7) 
LM_Q7 <- LM_words %>% filter(word %in% Q7) 
LM_Q8 <- LM_words %>% filter(word %in% Q8) 
LM_Q9 <- LM_words %>% filter(word %in% Q9) 
LM_Q10 <- LM_words %>% filter(word %in% Q10) 
LM_Q11 <- LM_words %>% filter(word %in% Q11) 
LM_Q12 <- LM_words %>% filter(word %in% Q12) 
LM_Q13 <- LM_words %>% filter(word %in% Q13) 
LM_Q14 <- LM_words %>% filter(word %in% Q14) 
LM_Q15 <- LM_words %>% filter(word %in% Q15) 
LM_Q16 <- LM_words %>% filter(word %in% Q16) 
LM_Q17 <- LM_words %>% filter(word %in% Q17) 
LM_Q18 <- LM_words %>% filter(word %in% Q18) 
LM_Q19 <- LM_words %>% filter(word %in% Q19) 
LM_Q20 <- LM_words %>% filter(word %in% Q20) 
LM_Q21 <- LM_words %>% filter(word %in% Q21) 
LM_Q22 <- LM_words %>% filter(word %in% Q22) 
LM_Q23 <- LM_words %>% filter(word %in% Q23) 
LM_Q24 <- LM_words %>% filter(word %in% Q24) 
LM_Q25 <- LM_words %>% filter(word %in% Q25) 
LM_Q26 <- LM_words %>% filter(word %in% Q26) 
LM_Q27 <- LM_words %>% filter(word %in% Q27) 
LM_Q28 <- LM_words %>% filter(word %in% Q28) 
LM_Q29 <- LM_words %>% filter(word %in% Q29) 
LM_Q30 <- LM_words %>% filter(word %in% Q30) 
LM_Q31 <- LM_words %>% filter(word %in% Q31) 
LM_Q32 <- LM_words %>% filter(word %in% Q32) 
LM_Q33 <- LM_words %>% filter(word %in% Q33) 
LM_Q34 <- LM_words %>% filter(word %in% Q34) 
LM_Q35 <- LM_words %>% filter(word %in% Q35) 
LM_Q36 <- LM_words %>% filter(word %in% Q36) 
LM_Q37 <- LM_words %>% filter(word %in% Q37) 
LM_Q38 <- LM_words %>% filter(word %in% Q38) 
LM_Q39 <- LM_words %>% filter(word %in% Q39) 
LM_Q40 <- LM_words %>% filter(word %in% Q40) 
LM_Q41 <- LM_words %>% filter(word %in% Q41) 
LM_Q42 <- LM_words %>% filter(word %in% Q42) 
LM_Q43 <- LM_words %>% filter(word %in% Q43) 
LM_Q44 <- LM_words %>% filter(word %in% Q44) 
LM_Q45 <- LM_words %>% filter(word %in% Q45) 
LM_Q46 <- LM_words %>% filter(word %in% Q46) 
LM_Q47 <- LM_words %>% filter(word %in% Q47) 
LM_Q48 <- LM_words %>% filter(word %in% Q48) 
LM_Q49 <- LM_words %>% filter(word %in% Q49) 
LM_Q50 <- LM_words %>% filter(word %in% Q50) 
LM_Q51 <- LM_words %>% filter(word %in% Q51) 
LM_Q52 <- LM_words %>% filter(word %in% Q52) 
LM_Q53 <- LM_words %>% filter(word %in% Q53) 
LM_Q54 <- LM_words %>% filter(word %in% Q54) 
LM_Q55 <- LM_words %>% filter(word %in% Q55) 
LM_Q56 <- LM_words %>% filter(word %in% Q56) 
LM_Q57 <- LM_words %>% filter(word %in% Q57) 
LM_Q58 <- LM_words %>% filter(word %in% Q58) 
LM_Q59 <- LM_words %>% filter(word %in% Q59) 
LM_Q60 <- LM_words %>% filter(word %in% Q60) 
LM_Q61 <- LM_words %>% filter(word %in% Q61)  
LM_Q62 <- LM_words %>% filter(word %in% Q62)  
LM_Q63 <- LM_words %>% filter(word %in% Q63)  
LM_Q64 <- LM_words %>% filter(word %in% Q64)  
LM_Q65 <- LM_words %>% filter(word %in% Q65)  
LM_Q66 <- LM_words %>% filter(word %in% Q66)  
LM_Q67 <- LM_words %>% filter(word %in% Q67)  
LM_Q68 <- LM_words %>% filter(word %in% Q68)  
LM_Q69 <- LM_words %>% filter(word %in% Q69)  
LM_Q70 <- LM_words %>% filter(word %in% Q70)  
LM_Q71 <- LM_words %>% filter(word %in% Q71)  
LM_Q72 <- LM_words %>% filter(word %in% Q72)  
LM_Q73 <- LM_words %>% filter(word %in% Q73)  
LM_Q74 <- LM_words %>% filter(word %in% Q74)  
LM_Q75 <- LM_words %>% filter(word %in% Q75)  
LM_Q76 <- LM_words %>% filter(word %in% Q76)  
LM_Q77 <- LM_words %>% filter(word %in% Q77)  
LM_Q78 <- LM_words %>% filter(word %in% Q78)  
LM_Q79 <- LM_words %>% filter(word %in% Q79)  
LM_Q80 <- LM_words %>% filter(word %in% Q80)  
LM_Q81 <- LM_words %>% filter(word %in% Q81)  
LM_Q82 <- LM_words %>% filter(word %in% Q82)  
LM_Q83 <- LM_words %>% filter(word %in% Q83)  
LM_Q84 <- LM_words %>% filter(word %in% Q84)  
LM_Q85 <- LM_words %>% filter(word %in% Q85)  
LM_Q86 <- LM_words %>% filter(word %in% Q86)  
LM_Q87 <- LM_words %>% filter(word %in% Q87)  
LM_Q88 <- LM_words %>% filter(word %in% Q88)  
LM_Q89 <- LM_words %>% filter(word %in% Q89)  
LM_Q90 <- LM_words %>% filter(word %in% Q90)  
LM_Q91 <- LM_words %>% filter(word %in% Q91)  
LM_Q92 <- LM_words %>% filter(word %in% Q92)  
LM_Q93 <- LM_words %>% filter(word %in% Q93)  
LM_Q94 <- LM_words %>% filter(word %in% Q94)  
LM_Q95 <- LM_words %>% filter(word %in% Q95)  
LM_Q96 <- LM_words %>% filter(word %in% Q96)  
LM_Q97 <- LM_words %>% filter(word %in% Q97)  
LM_Q98 <- LM_words %>% filter(word %in% Q98)  
LM_Q99 <- LM_words %>% filter(word %in% Q99)  
LM_Q100 <- LM_words %>% filter(word %in% Q100)  
LM_Q101 <- LM_words %>% filter(word %in% Q101)  
LM_Q102 <- LM_words %>% filter(word %in% Q102)  
LM_Q103 <- LM_words %>% filter(word %in% Q103)  
LM_Q104 <- LM_words %>% filter(word %in% Q104)  
LM_Q105 <- LM_words %>% filter(word %in% Q105)  
LM_Q106 <- LM_words %>% filter(word %in% Q106)  
LM_Q107 <- LM_words %>% filter(word %in% Q107)  
LM_Q108 <- LM_words %>% filter(word %in% Q108)  
LM_Q109 <- LM_words %>% filter(word %in% Q109)  
LM_Q110 <- LM_words %>% filter(word %in% Q110)  
LM_Q111 <- LM_words %>% filter(word %in% Q111)  
LM_Q112 <- LM_words %>% filter(word %in% Q112)  
LM_Q113 <- LM_words %>% filter(word %in% Q113)  
LM_Q114 <- LM_words %>% filter(word %in% Q114)  
LM_Q115 <- LM_words %>% filter(word %in% Q115)  
LM_Q116 <- LM_words %>% filter(word %in% Q116)  
LM_Q117 <- LM_words %>% filter(word %in% Q117)  
LM_Q118 <- LM_words %>% filter(word %in% Q118)  
LM_Q119 <- LM_words %>% filter(word %in% Q119)  
LM_Q120 <- LM_words %>% filter(word %in% Q120)  
LM_Q121 <- LM_words %>% filter(word %in% Q121)  
LM_Q122 <- LM_words %>% filter(word %in% Q122)  
LM_Q123 <- LM_words %>% filter(word %in% Q123)  
LM_Q124 <- LM_words %>% filter(word %in% Q124)  
LM_Q125 <- LM_words %>% filter(word %in% Q125)  
LM_Q126 <- LM_words %>% filter(word %in% Q126)  
LM_Q127 <- LM_words %>% filter(word %in% Q127)  
LM_Q128 <- LM_words %>% filter(word %in% Q128)  
LM_Q129 <- LM_words %>% filter(word %in% Q129)  
LM_Q130 <- LM_words %>% filter(word %in% Q130)  
LM_Q131 <- LM_words %>% filter(word %in% Q131)  
LM_Q132 <- LM_words %>% filter(word %in% Q132)  
LM_Q133 <- LM_words %>% filter(word %in% Q133)  
LM_Q134 <- LM_words %>% filter(word %in% Q134)  
LM_Q135 <- LM_words %>% filter(word %in% Q135)  
LM_Q136 <- LM_words %>% filter(word %in% Q136)  
LM_Q137 <- LM_words %>% filter(word %in% Q137)  
LM_Q138 <- LM_words %>% filter(word %in% Q138)  
LM_Q139 <- LM_words %>% filter(word %in% Q139)  
LM_Q140 <- LM_words %>% filter(word %in% Q140)  
LM_Q141 <- LM_words %>% filter(word %in% Q141)  
LM_Q142 <- LM_words %>% filter(word %in% Q142)  
LM_Q143 <- LM_words %>% filter(word %in% Q143)  
LM_Q144 <- LM_words %>% filter(word %in% Q144)  
LM_Q145 <- LM_words %>% filter(word %in% Q145)  
LM_Q146 <- LM_words %>% filter(word %in% Q146)  
LM_Q147 <- LM_words %>% filter(word %in% Q147)  
LM_Q148 <- LM_words %>% filter(word %in% Q148)  
LM_Q149 <- LM_words %>% filter(word %in% Q149)  
LM_Q150 <- LM_words %>% filter(word %in% Q150)  
LM_Q151 <- LM_words %>% filter(word %in% Q151)  
LM_Q152 <- LM_words %>% filter(word %in% Q152)  
LM_Q153 <- LM_words %>% filter(word %in% Q153)  
LM_Q154 <- LM_words %>% filter(word %in% Q154)  
LM_Q155 <- LM_words %>% filter(word %in% Q155)  
LM_Q156 <- LM_words %>% filter(word %in% Q156)  
LM_Q157 <- LM_words %>% filter(word %in% Q157)  
LM_Q158 <- LM_words %>% filter(word %in% Q158)  
LM_Q159 <- LM_words %>% filter(word %in% Q159)  
LM_Q160 <- LM_words %>% filter(word %in% Q160)  
LM_Q161 <- LM_words %>% filter(word %in% Q161)  
LM_Q162 <- LM_words %>% filter(word %in% Q162)  
LM_Q163 <- LM_words %>% filter(word %in% Q163)  
LM_Q164 <- LM_words %>% filter(word %in% Q164)  
LM_Q165 <- LM_words %>% filter(word %in% Q165)  
LM_Q166 <- LM_words %>% filter(word %in% Q166)  
LM_Q167 <- LM_words %>% filter(word %in% Q167)  
LM_Q168 <- LM_words %>% filter(word %in% Q168)  
LM_Q169 <- LM_words %>% filter(word %in% Q169)  
LM_Q170 <- LM_words %>% filter(word %in% Q170)  
LM_Q171 <- LM_words %>% filter(word %in% Q171)  
LM_Q172 <- LM_words %>% filter(word %in% Q172)  
LM_Q173 <- LM_words %>% filter(word %in% Q173)  
LM_Q174 <- LM_words %>% filter(word %in% Q174)  
LM_Q175 <- LM_words %>% filter(word %in% Q175)  
LM_Q176 <- LM_words %>% filter(word %in% Q176)  
LM_Q177 <- LM_words %>% filter(word %in% Q177)  
LM_Q178 <- LM_words %>% filter(word %in% Q178)  
LM_Q179 <- LM_words %>% filter(word %in% Q179)  
LM_Q180 <- LM_words %>% filter(word %in% Q180)  
LM_Q181 <- LM_words %>% filter(word %in% Q181)  
LM_Q182 <- LM_words %>% filter(word %in% Q182)  
LM_Q183 <- LM_words %>% filter(word %in% Q183)  
LM_Q184 <- LM_words %>% filter(word %in% Q184)  
LM_Q185 <- LM_words %>% filter(word %in% Q185)  
LM_Q186 <- LM_words %>% filter(word %in% Q186)  
LM_Q187 <- LM_words %>% filter(word %in% Q187)  
LM_Q188 <- LM_words %>% filter(word %in% Q188)  
LM_Q189 <- LM_words %>% filter(word %in% Q189)  
LM_Q190 <- LM_words %>% filter(word %in% Q190)  
LM_Q191 <- LM_words %>% filter(word %in% Q191)  
LM_Q192 <- LM_words %>% filter(word %in% Q192)  
LM_Q193 <- LM_words %>% filter(word %in% Q193)  
LM_Q194 <- LM_words %>% filter(word %in% Q194)  
LM_Q195 <- LM_words %>% filter(word %in% Q195)  
LM_Q196 <- LM_words %>% filter(word %in% Q196)  
LM_Q197 <- LM_words %>% filter(word %in% Q197)  
LM_Q198 <- LM_words %>% filter(word %in% Q198)  
LM_Q199 <- LM_words %>% filter(word %in% Q199)  
LM_Q200 <- LM_words %>% filter(word %in% Q200)  
LM_Q201 <- LM_words %>% filter(word %in% Q201)  
LM_Q202 <- LM_words %>% filter(word %in% Q202)  
LM_Q203 <- LM_words %>% filter(word %in% Q203)  
LM_Q204 <- LM_words %>% filter(word %in% Q204)  
LM_Q205 <- LM_words %>% filter(word %in% Q205)  
LM_Q206 <- LM_words %>% filter(word %in% Q206)  
LM_Q207 <- LM_words %>% filter(word %in% Q207)  
LM_Q208 <- LM_words %>% filter(word %in% Q208)  
LM_Q209 <- LM_words %>% filter(word %in% Q209)  
LM_Q210 <- LM_words %>% filter(word %in% Q210)  
LM_Q211 <- LM_words %>% filter(word %in% Q211)  
LM_Q212 <- LM_words %>% filter(word %in% Q212)  
LM_Q213 <- LM_words %>% filter(word %in% Q213)  
LM_Q214 <- LM_words %>% filter(word %in% Q214)  
LM_Q215 <- LM_words %>% filter(word %in% Q215)  
LM_Q216 <- LM_words %>% filter(word %in% Q216)  
LM_Q217 <- LM_words %>% filter(word %in% Q217)  
LM_Q218 <- LM_words %>% filter(word %in% Q218)  
LM_Q219 <- LM_words %>% filter(word %in% Q219)  
LM_Q220 <- LM_words %>% filter(word %in% Q220)  
LM_Q221 <- LM_words %>% filter(word %in% Q221)  
LM_Q222 <- LM_words %>% filter(word %in% Q222)  
LM_Q223 <- LM_words %>% filter(word %in% Q223)  
LM_Q224 <- LM_words %>% filter(word %in% Q224)  
LM_Q225 <- LM_words %>% filter(word %in% Q225)  
LM_Q226 <- LM_words %>% filter(word %in% Q226)  
LM_Q227 <- LM_words %>% filter(word %in% Q227)  
LM_Q228 <- LM_words %>% filter(word %in% Q228)  



setwd('C:/Users/mdjaw/OneDrive/Documents/Markian Training/Research_USQ/WHO_ICD_Codes')

WHO_CH_00 <- as.list(readLines("WHO_CH_00_List.txt", encoding = "UTF-8"))
WHO_CH_01 <- as.list(readLines("WHO_CH_01.txt", encoding = "UTF-8"))
WHO_CH_02 <- as.list(readLines("WHO_CH_02.txt", encoding = "UTF-8"))
WHO_CH_03 <- as.list(readLines("WHO_CH_03.txt", encoding = "UTF-8"))
WHO_CH_04 <- as.list(readLines("WHO_CH_04.txt", encoding = "UTF-8"))
WHO_CH_05 <- as.list(readLines("WHO_CH_05.txt", encoding = "UTF-8"))
WHO_CH_06 <- as.list(readLines("WHO_CH_06.txt", encoding = "UTF-8"))
WHO_CH_07 <- as.list(readLines("WHO_CH_07.txt", encoding = "UTF-8"))
WHO_CH_08 <- as.list(readLines("WHO_CH_08.txt", encoding = "UTF-8"))
WHO_CH_09 <- as.list(readLines("WHO_CH_09.txt", encoding = "UTF-8"))
WHO_CH_10 <- as.list(readLines("WHO_CH_10.txt", encoding = "UTF-8"))
WHO_CH_11 <- as.list(readLines("WHO_CH_11.txt", encoding = "UTF-8"))
WHO_CH_12 <- as.list(readLines("WHO_CH_12.txt", encoding = "UTF-8"))
WHO_CH_13 <- as.list(readLines("WHO_CH_13.txt", encoding = "UTF-8"))
WHO_CH_14 <- as.list(readLines("WHO_CH_14.txt", encoding = "UTF-8"))
WHO_CH_15 <- as.list(readLines("WHO_CH_15.txt", encoding = "UTF-8"))
WHO_CH_16 <- as.list(readLines("WHO_CH_16.txt", encoding = "UTF-8"))
WHO_CH_17 <- as.list(readLines("WHO_CH_17.txt", encoding = "UTF-8"))
WHO_CH_18 <- as.list(readLines("WHO_CH_18.txt", encoding = "UTF-8"))
WHO_CH_19 <- as.list(readLines("WHO_CH_19.txt", encoding = "UTF-8"))
WHO_CH_20 <- as.list(readLines("WHO_CH_20.txt", encoding = "UTF-8"))
WHO_CH_21 <- as.list(readLines("WHO_CH_21.txt", encoding = "UTF-8"))
WHO_CH_22 <- as.list(readLines("WHO_CH_22.txt", encoding = "UTF-8"))
WHO_CH_23 <- as.list(readLines("WHO_CH_23.txt", encoding = "UTF-8"))
WHO_CH_24 <- as.list(readLines("WHO_CH_24.txt", encoding = "UTF-8"))
WHO_CH_25 <- as.list(readLines("WHO_CH_25.txt", encoding = "UTF-8"))
WHO_CH_26 <- as.list(readLines("WHO_CH_26.txt", encoding = "UTF-8"))
#WHO_SUPP <- as.list(readLines("WHO_SUPP.txt", encoding = "UTF-8"))
#WHO_EXT <- as.list(readLines("WHO_EXT.txt", encoding = "UTF-8"))
#WHO_ICD <- as.list(readLines("WHO_ICD.txt", encoding = "UTF-8"))

doc_CH_00 <- Corpus(VectorSource(WHO_CH_00))
doc_CH_00 = tm_map(doc_CH_00, content_transformer(tolower))
doc_CH_00 = tm_map(doc_CH_00, removePunctuation)
doc_CH_00 = tm_map(doc_CH_00, removeWords, stopwords("english"))
WHO_CH_00 <- doc_CH_00$content
WHO_CH_00 <- as.list(doc_CH_00$content)

doc_CH_01 <- Corpus(VectorSource(WHO_CH_01))
doc_CH_01 = tm_map(doc_CH_01, content_transformer(tolower))
doc_CH_01 = tm_map(doc_CH_01, removePunctuation)
doc_CH_01 = tm_map(doc_CH_01, removeWords, stopwords("english"))
WHO_CH_01 <- doc_CH_01$content
WHO_CH_01 <- as.list(doc_CH_01$content)

doc_CH_02 <- Corpus(VectorSource(WHO_CH_02))
doc_CH_02 = tm_map(doc_CH_02, content_transformer(tolower))
doc_CH_02 = tm_map(doc_CH_02, removePunctuation)
doc_CH_02 = tm_map(doc_CH_02, removeWords, stopwords("english"))
WHO_CH_02 <- doc_CH_02$content
WHO_CH_02 <- as.list(doc_CH_02$content)

doc_CH_03 <- Corpus(VectorSource(WHO_CH_03))
doc_CH_03 = tm_map(doc_CH_03, content_transformer(tolower))
doc_CH_03 = tm_map(doc_CH_03, removePunctuation)
doc_CH_03 = tm_map(doc_CH_03, removeWords, stopwords("english"))
WHO_CH_03 <- doc_CH_03$content
WHO_CH_03 <- as.list(doc_CH_03$content)

doc_CH_04 <- Corpus(VectorSource(WHO_CH_04))
doc_CH_04 = tm_map(doc_CH_04, content_transformer(tolower))
doc_CH_04 = tm_map(doc_CH_04, removePunctuation)
doc_CH_04 = tm_map(doc_CH_04, removeWords, stopwords("english"))
WHO_CH_04 <- doc_CH_04$content
WHO_CH_04 <- as.list(doc_CH_04$content)

doc_CH_05 <- Corpus(VectorSource(WHO_CH_05))
doc_CH_05 = tm_map(doc_CH_05, content_transformer(tolower))
doc_CH_05 = tm_map(doc_CH_05, removePunctuation)
doc_CH_05 = tm_map(doc_CH_05, removeWords, stopwords("english"))
WHO_CH_05 <- doc_CH_05$content
WHO_CH_05 <- as.list(doc_CH_05$content)

doc_CH_06 <- Corpus(VectorSource(WHO_CH_06))
doc_CH_06 = tm_map(doc_CH_06, content_transformer(tolower))
doc_CH_06 = tm_map(doc_CH_06, removePunctuation)
doc_CH_06 = tm_map(doc_CH_06, removeWords, stopwords("english"))
WHO_CH_06 <- doc_CH_06$content
WHO_CH_06 <- as.list(doc_CH_06$content)

doc_CH_07 <- Corpus(VectorSource(WHO_CH_07))
doc_CH_07 = tm_map(doc_CH_07, content_transformer(tolower))
doc_CH_07 = tm_map(doc_CH_07, removePunctuation)
doc_CH_07 = tm_map(doc_CH_07, removeWords, stopwords("english"))
WHO_CH_07 <- doc_CH_07$content
WHO_CH_07 <- as.list(doc_CH_07$content)

doc_CH_08 <- Corpus(VectorSource(WHO_CH_08))
doc_CH_08 = tm_map(doc_CH_08, content_transformer(tolower))
doc_CH_08 = tm_map(doc_CH_08, removePunctuation)
doc_CH_08 = tm_map(doc_CH_08, removeWords, stopwords("english"))
WHO_CH_08 <- doc_CH_08$content
WHO_CH_08 <- as.list(doc_CH_08$content)

doc_CH_09 <- Corpus(VectorSource(WHO_CH_09))
doc_CH_09 = tm_map(doc_CH_09, content_transformer(tolower))
doc_CH_09 = tm_map(doc_CH_09, removePunctuation)
doc_CH_09 = tm_map(doc_CH_09, removeWords, stopwords("english"))
WHO_CH_09 <- doc_CH_09$content
WHO_CH_09 <- as.list(doc_CH_09$content)

doc_CH_10 <- Corpus(VectorSource(WHO_CH_10))
doc_CH_10 = tm_map(doc_CH_10, content_transformer(tolower))
doc_CH_10 = tm_map(doc_CH_10, removePunctuation)
doc_CH_10 = tm_map(doc_CH_10, removeWords, stopwords("english"))
WHO_CH_10 <- doc_CH_10$content
WHO_CH_10 <- as.list(doc_CH_10$content)

doc_CH_11 <- Corpus(VectorSource(WHO_CH_11))
doc_CH_11 = tm_map(doc_CH_11, content_transformer(tolower))
doc_CH_11 = tm_map(doc_CH_11, removePunctuation)
doc_CH_11 = tm_map(doc_CH_11, removeWords, stopwords("english"))
WHO_CH_11 <- doc_CH_11$content
WHO_CH_11 <- as.list(doc_CH_11$content)

doc_CH_12 <- Corpus(VectorSource(WHO_CH_12))
doc_CH_12 = tm_map(doc_CH_12, content_transformer(tolower))
doc_CH_12 = tm_map(doc_CH_12, removePunctuation)
doc_CH_12 = tm_map(doc_CH_12, removeWords, stopwords("english"))
WHO_CH_12 <- doc_CH_12$content
WHO_CH_12 <- as.list(doc_CH_12$content)

doc_CH_13 <- Corpus(VectorSource(WHO_CH_13))
doc_CH_13 = tm_map(doc_CH_13, content_transformer(tolower))
doc_CH_13 = tm_map(doc_CH_13, removePunctuation)
doc_CH_13 = tm_map(doc_CH_13, removeWords, stopwords("english"))
WHO_CH_13 <- doc_CH_13$content
WHO_CH_13 <- as.list(doc_CH_13$content)

doc_CH_14 <- Corpus(VectorSource(WHO_CH_14))
doc_CH_14 = tm_map(doc_CH_14, content_transformer(tolower))
doc_CH_14 = tm_map(doc_CH_14, removePunctuation)
doc_CH_14 = tm_map(doc_CH_14, removeWords, stopwords("english"))
WHO_CH_14 <- doc_CH_14$content
WHO_CH_14 <- as.list(doc_CH_14$content)

doc_CH_15 <- Corpus(VectorSource(WHO_CH_15))
doc_CH_15 = tm_map(doc_CH_15, content_transformer(tolower))
doc_CH_15 = tm_map(doc_CH_15, removePunctuation)
doc_CH_15 = tm_map(doc_CH_15, removeWords, stopwords("english"))
WHO_CH_15 <- doc_CH_15$content
WHO_CH_15 <- as.list(doc_CH_15$content)

doc_CH_16 <- Corpus(VectorSource(WHO_CH_16))
doc_CH_16 = tm_map(doc_CH_16, content_transformer(tolower))
doc_CH_16 = tm_map(doc_CH_16, removePunctuation)
doc_CH_16 = tm_map(doc_CH_16, removeWords, stopwords("english"))
WHO_CH_16 <- doc_CH_16$content
WHO_CH_16 <- as.list(doc_CH_16$content)

doc_CH_17 <- Corpus(VectorSource(WHO_CH_17))
doc_CH_17 = tm_map(doc_CH_17, content_transformer(tolower))
doc_CH_17 = tm_map(doc_CH_17, removePunctuation)
doc_CH_17 = tm_map(doc_CH_17, removeWords, stopwords("english"))
WHO_CH_17 <- doc_CH_17$content
WHO_CH_17 <- as.list(doc_CH_17$content)

doc_CH_18 <- Corpus(VectorSource(WHO_CH_18))
doc_CH_18 = tm_map(doc_CH_18, content_transformer(tolower))
doc_CH_18 = tm_map(doc_CH_18, removePunctuation)
doc_CH_18 = tm_map(doc_CH_18, removeWords, stopwords("english"))
WHO_CH_18 <- doc_CH_18$content
WHO_CH_18 <- as.list(doc_CH_18$content)

doc_CH_19 <- Corpus(VectorSource(WHO_CH_19))
doc_CH_19 = tm_map(doc_CH_19, content_transformer(tolower))
doc_CH_19 = tm_map(doc_CH_19, removePunctuation)
doc_CH_19 = tm_map(doc_CH_19, removeWords, stopwords("english"))
WHO_CH_19 <- doc_CH_19$content
WHO_CH_19 <- as.list(doc_CH_19$content)

doc_CH_20 <- Corpus(VectorSource(WHO_CH_20))
doc_CH_20 = tm_map(doc_CH_20, content_transformer(tolower))
doc_CH_20 = tm_map(doc_CH_20, removePunctuation)
doc_CH_20 = tm_map(doc_CH_20, removeWords, stopwords("english"))
WHO_CH_20 <- doc_CH_20$content
WHO_CH_20 <- as.list(doc_CH_20$content)

doc_CH_21 <- Corpus(VectorSource(WHO_CH_21))
doc_CH_21 = tm_map(doc_CH_21, content_transformer(tolower))
doc_CH_21 = tm_map(doc_CH_21, removePunctuation)
doc_CH_21 = tm_map(doc_CH_21, removeWords, stopwords("english"))
WHO_CH_21 <- doc_CH_21$content
WHO_CH_21 <- as.list(doc_CH_21$content)

doc_CH_22 <- Corpus(VectorSource(WHO_CH_22))
doc_CH_22 = tm_map(doc_CH_22, content_transformer(tolower))
doc_CH_22 = tm_map(doc_CH_22, removePunctuation)
doc_CH_22 = tm_map(doc_CH_22, removeWords, stopwords("english"))
WHO_CH_22 <- doc_CH_22$content
WHO_CH_22 <- as.list(doc_CH_22$content)

doc_CH_23 <- Corpus(VectorSource(WHO_CH_23))
doc_CH_23 = tm_map(doc_CH_23, content_transformer(tolower))
doc_CH_23 = tm_map(doc_CH_23, removePunctuation)
doc_CH_23 = tm_map(doc_CH_23, removeWords, stopwords("english"))
WHO_CH_23 <- doc_CH_23$content
WHO_CH_23 <- as.list(doc_CH_23$content)

doc_CH_24 <- Corpus(VectorSource(WHO_CH_24))
doc_CH_24 = tm_map(doc_CH_24, content_transformer(tolower))
doc_CH_24 = tm_map(doc_CH_24, removePunctuation)
doc_CH_24 = tm_map(doc_CH_24, removeWords, stopwords("english"))
WHO_CH_24 <- doc_CH_24$content
WHO_CH_24 <- as.list(doc_CH_24$content)

doc_CH_25 <- Corpus(VectorSource(WHO_CH_25))
doc_CH_25 = tm_map(doc_CH_25, content_transformer(tolower))
doc_CH_25 = tm_map(doc_CH_25, removePunctuation)
doc_CH_25 = tm_map(doc_CH_25, removeWords, stopwords("english"))
WHO_CH_25 <- doc_CH_25$content
WHO_CH_25 <- as.list(doc_CH_25$content)

doc_CH_26 <- Corpus(VectorSource(WHO_CH_26))
doc_CH_26 = tm_map(doc_CH_26, content_transformer(tolower))
doc_CH_26 = tm_map(doc_CH_26, removePunctuation)
doc_CH_26 = tm_map(doc_CH_26, removeWords, stopwords("english"))
WHO_CH_26 <- doc_CH_26$content
WHO_CH_26 <- as.list(doc_CH_26$content)

#doc_SUPP <- Corpus(VectorSource(WHO_SUPP))
#doc_SUPP = tm_map(doc_SUPP, content_transformer(tolower))
#doc_SUPP = tm_map(doc_SUPP, removePunctuation)
#doc_SUPP = tm_map(doc_SUPP, removeWords, stopwords("english"))
#WHO_SUPP <- doc_SUPP$content
#WHO_SUPP <- as.list(doc_SUPP$content)
#
#doc_EXT <- Corpus(VectorSource(WHO_EXT))
#doc_EXT = tm_map(doc_EXT, content_transformer(tolower))
#doc_EXT = tm_map(doc_EXT, removePunctuation)
#doc_EXT = tm_map(doc_EXT, removeWords, stopwords("english"))
#WHO_EXT <- doc_EXT$content
#WHO_EXT <- as.list(doc_EXT$content)


df_WHO_00 <- as.data.frame(do.call(rbind, WHO_CH_00))
df_WHO_01 <- as.data.frame(do.call(rbind, WHO_CH_01))
df_WHO_02 <- as.data.frame(do.call(rbind, WHO_CH_02))
df_WHO_03 <- as.data.frame(do.call(rbind, WHO_CH_03))
df_WHO_04 <- as.data.frame(do.call(rbind, WHO_CH_04))
df_WHO_05 <- as.data.frame(do.call(rbind, WHO_CH_05))
df_WHO_06 <- as.data.frame(do.call(rbind, WHO_CH_06))
df_WHO_07 <- as.data.frame(do.call(rbind, WHO_CH_07))
df_WHO_08 <- as.data.frame(do.call(rbind, WHO_CH_08))
df_WHO_09 <- as.data.frame(do.call(rbind, WHO_CH_09))
df_WHO_10 <- as.data.frame(do.call(rbind, WHO_CH_10))
df_WHO_11 <- as.data.frame(do.call(rbind, WHO_CH_11))
df_WHO_12 <- as.data.frame(do.call(rbind, WHO_CH_12))
df_WHO_13 <- as.data.frame(do.call(rbind, WHO_CH_13))
df_WHO_14 <- as.data.frame(do.call(rbind, WHO_CH_14))
df_WHO_15 <- as.data.frame(do.call(rbind, WHO_CH_15))
df_WHO_16 <- as.data.frame(do.call(rbind, WHO_CH_16))
df_WHO_17 <- as.data.frame(do.call(rbind, WHO_CH_17))
df_WHO_18 <- as.data.frame(do.call(rbind, WHO_CH_18))
df_WHO_19 <- as.data.frame(do.call(rbind, WHO_CH_19))
df_WHO_20 <- as.data.frame(do.call(rbind, WHO_CH_20))
df_WHO_21 <- as.data.frame(do.call(rbind, WHO_CH_21))
df_WHO_22 <- as.data.frame(do.call(rbind, WHO_CH_22))
df_WHO_23 <- as.data.frame(do.call(rbind, WHO_CH_23))
df_WHO_24 <- as.data.frame(do.call(rbind, WHO_CH_24))
df_WHO_25 <- as.data.frame(do.call(rbind, WHO_CH_25))
df_WHO_26 <- as.data.frame(do.call(rbind, WHO_CH_26))
#df_WHO_SUPP <- as.data.frame(do.call(rbind, WHO_SUPP))
#df_WHO_EXT <- as.data.frame(do.call(rbind, WHO_EXT))
#df_WHO_ICD <- as.data.frame(do.call(rbind, WHO_ICD))

df_WHO_00$Chapter = c("CH_00: List of top level categories")
df_WHO_01$Chapter = c("CH_01: Certain infectious or parasitic diseases")
df_WHO_02$Chapter = c("CH_02: Neoplasms")
df_WHO_03$Chapter = c("CH_03: Diseases of the blood or blood-forming organs")
df_WHO_04$Chapter = c("CH_04: Diseases of the immune system")
df_WHO_05$Chapter = c("CH_05: Endocrine, nutritional or metabolic diseases")
df_WHO_06$Chapter = c("CH_06: Mental, behavioural or neurodevelopmental disorders")
df_WHO_07$Chapter = c("CH_07: Sleep-wake disorders")
df_WHO_08$Chapter = c("CH_08: Diseases of the nervous system")
df_WHO_09$Chapter = c("CH_09: Diseases of the visual system")
df_WHO_10$Chapter = c("CH_10: Diseases of the ear or mastoid process")
df_WHO_11$Chapter = c("CH_11: Diseases of the circulatory system")
df_WHO_12$Chapter = c("CH_12: Diseases of the respiratory system")
df_WHO_13$Chapter = c("CH_13: Diseases of the digestive system")
df_WHO_14$Chapter = c("CH_14: Diseases of the skin")
df_WHO_15$Chapter = c("CH_15: Diseases of the musculoskeletal system or connective tissue")
df_WHO_16$Chapter = c("CH_16: Diseases of the genitourinary system")
df_WHO_17$Chapter = c("CH_17: Conditions related to sexual health")
df_WHO_18$Chapter = c("CH_18: Pregnancy, childbirth or the puerperium")
df_WHO_19$Chapter = c("CH_19: Certain conditions originating in the perinatal period")
df_WHO_20$Chapter = c("CH_20: Developmental anomalies")
df_WHO_21$Chapter = c("CH_21: Symptoms, signs or clinical findings, not elsewhere classified")
df_WHO_22$Chapter = c("CH_22: Injury, poisoning or certain other consequences of external causes")
df_WHO_23$Chapter = c("CH_23: External causes of morbidity or mortality")
df_WHO_24$Chapter = c("CH_24: Factors influencing health status or contact with health services")
df_WHO_25$Chapter = c("CH_25: Codes for special purposes")
df_WHO_26$Chapter = c("CH_26: Supplementary Chapter Traditional Medicine Conditions - Module I")
#df_WHO_SUPP$Chapter = c("CH_SUPP: Supplementary section for functioning assessment")
#df_WHO_EXT$Chapter = c("CH_EXT: Extension Codes")
#df_WHO_ICD$Chapter = c("CH_ICD: Certain infectious or parasitic diseases")

df_WHO <- rbind(df_WHO_00, df_WHO_01)
df_WHO <- rbind(df_WHO, df_WHO_02)
df_WHO <- rbind(df_WHO, df_WHO_03)
df_WHO <- rbind(df_WHO, df_WHO_04)
df_WHO <- rbind(df_WHO, df_WHO_05)
df_WHO <- rbind(df_WHO, df_WHO_06)
df_WHO <- rbind(df_WHO, df_WHO_07)
df_WHO <- rbind(df_WHO, df_WHO_08)
df_WHO <- rbind(df_WHO, df_WHO_09)
df_WHO <- rbind(df_WHO, df_WHO_10)
df_WHO <- rbind(df_WHO, df_WHO_11)
df_WHO <- rbind(df_WHO, df_WHO_12)
df_WHO <- rbind(df_WHO, df_WHO_13)
df_WHO <- rbind(df_WHO, df_WHO_14)
df_WHO <- rbind(df_WHO, df_WHO_15)
df_WHO <- rbind(df_WHO, df_WHO_16)
df_WHO <- rbind(df_WHO, df_WHO_17)
df_WHO <- rbind(df_WHO, df_WHO_18)
df_WHO <- rbind(df_WHO, df_WHO_19)
df_WHO <- rbind(df_WHO, df_WHO_20)
df_WHO <- rbind(df_WHO, df_WHO_21)
df_WHO <- rbind(df_WHO, df_WHO_22)
df_WHO <- rbind(df_WHO, df_WHO_23)
df_WHO <- rbind(df_WHO, df_WHO_24)
df_WHO <- rbind(df_WHO, df_WHO_25)
df_WHO <- rbind(df_WHO, df_WHO_26)
#df_WHO <- rbind(df_WHO, df_WHO_SUPP)
#df_WHO <- rbind(df_WHO, df_WHO_EXT)

colnames(df_WHO) <- c("text","chapter")

WHO_words <- df_WHO %>%
  unnest_tokens(word, text) %>%
  count(chapter, word, sort = TRUE)

total_words <- WHO_words %>% group_by(chapter) %>% summarize(total = sum(n))

WHO_words <- left_join(WHO_words, total_words)
WHO_words
  
WHO_words <- WHO_words %>%
  bind_tf_idf(word, chapter, n)
WHO_words



lm_list <- tolower(c('male','female','diabetes','cancer','pregnant','injury','teeth','tooth','disease','obese','overweight','decay','gum','smoked','myocardial','infarction','heart','drinks','cigarettes','tobacco','asthma','coronary','alcohol','considering','hepatitis','confusion','cannabis','vaccine','flu','zoster','fatigue','injected','myalgic','encephalomyelitis','pneumonia','pneumococcal','eye','dilated','dementia','mammogram','pap','screening','cervical','hpv','hysterectomy','antigen','prostate','psa','colon','stool','hiv','blood','fluid','month','sexually','insulin','sugar','glucose','feet'))


LM_Chapter_Scores <- WHO_words %>% filter(word %in% lm_list) %>% select(-total) %>% arrange(desc(tf_idf))



                                                                      chapter              word    n           tf        idf       tf_idf
#1                            CH_23: External causes of morbidity or mortality            injury 1226 6.815655e-02 0.20479441 1.395808e-02
#2                                   CH_11: Diseases of the circulatory system        myocardial   82 7.481752e-03 1.68639895 1.261722e-02
#3                                   CH_12: Diseases of the respiratory system         pneumonia   79 8.713876e-03 1.34992672 1.176309e-02
#4                                  CH_17: Conditions related to sexual health              male   30 1.564129e-02 0.65677954 1.027288e-02
#5   CH_22: Injury, poisoning or certain other consequences of external causes            injury  912 4.870234e-02 0.20479441 9.973967e-03
#6                                   CH_11: Diseases of the circulatory system          coronary   77 7.025547e-03 1.21639532 8.545843e-03
#7                                   CH_12: Diseases of the respiratory system            asthma   45 4.963600e-03 1.50407740 7.465639e-03
#8                                   CH_11: Diseases of the circulatory system        infarction   78 7.116788e-03 0.89794159 6.390460e-03
#9    CH_24: Factors influencing health status or contact with health services         screening   54 5.064716e-03 0.99325177 5.030538e-03
#10                             CH_18: Pregnancy, childbirth or the puerperium          pregnant   41 4.948702e-03 0.99325177 4.915308e-03
#11                 CH_06: Mental, behavioural or neurodevelopmental disorders          cannabis  122 2.387055e-03 1.90954250 4.558183e-03
#12                                    CH_13: Diseases of the digestive system             tooth   42 2.307566e-03 1.68639895 3.891476e-03
#13                                CH_16: Diseases of the genitourinary system          prostate   27 2.190314e-03 1.68639895 3.693743e-03
#14                                CH_16: Diseases of the genitourinary system            female   86 6.976556e-03 0.52324814 3.650470e-03
#15                                 CH_17: Conditions related to sexual health            female   13 6.777894e-03 0.52324814 3.546520e-03
#16                 CH_06: Mental, behavioural or neurodevelopmental disorders          dementia  157 3.071866e-03 1.09861229 3.374790e-03
#17                        CH_05: Endocrine, nutritional or metabolic diseases        overweight   25 1.724019e-03 1.90954250 3.292088e-03
#18                                       CH_09: Diseases of the visual system               eye   98 1.088284e-02 0.30010459 3.265991e-03
#19                                          CH_25: Codes for special purposes         screening    1 3.164557e-03 0.99325177 3.143202e-03
#20                       CH_03: Diseases of the blood or blood-forming organs           disease  266 3.835063e-02 0.07696104 2.951505e-03
#21                            CH_01: Certain infectious or parasitic diseases         hepatitis   70 2.895314e-03 0.99325177 2.875775e-03
#22                                    CH_13: Diseases of the digestive system         hepatitis   50 2.747102e-03 0.99325177 2.728564e-03
#23                                    CH_13: Diseases of the digestive system             teeth   44 2.417450e-03 1.09861229 2.655840e-03
#24                       CH_03: Diseases of the blood or blood-forming organs           fatigue   38 5.478662e-03 0.46262352 2.534558e-03
#25                                  CH_11: Diseases of the circulatory system             heart  109 9.945255e-03 0.25131443 2.499386e-03
#26   CH_24: Factors influencing health status or contact with health services            injury  130 1.219283e-02 0.20479441 2.497024e-03
#27                        CH_05: Endocrine, nutritional or metabolic diseases          diabetes   89 6.137508e-03 0.40546511 2.488545e-03
#28                       CH_03: Diseases of the blood or blood-forming organs             blood  219 3.157439e-02 0.07696104 2.429998e-03
#29                                  CH_11: Diseases of the circulatory system           dilated   24 2.189781e-03 1.09861229 2.405720e-03
#30                            CH_01: Certain infectious or parasitic diseases           disease  748 3.093850e-02 0.07696104 2.381059e-03
#31                                                           CH_02: Neoplasms             colon   43 2.157985e-03 1.09861229 2.370788e-03
#32                                        CH_00: List of top level categories            injury  338 1.103818e-02 0.20479441 2.260557e-03
#33                            CH_01: Certain infectious or parasitic diseases               hiv   55 2.274889e-03 0.99325177 2.259538e-03
#34                                             CH_20: Developmental anomalies          coronary   43 1.816415e-03 1.21639532 2.209479e-03
#35                 CH_06: Mental, behavioural or neurodevelopmental disorders             month   81 1.584848e-03 1.34992672 2.139429e-03
#36    CH_26: Supplementary Chapter Traditional Medicine Conditions - Module I             heart   28 8.218374e-03 0.25131443 2.065396e-03
#37                             CH_18: Pregnancy, childbirth or the puerperium         screening   16 1.931201e-03 0.99325177 1.918169e-03
#38                                    CH_13: Diseases of the digestive system             colon   30 1.648261e-03 1.09861229 1.810800e-03
#39              CH_19: Certain conditions originating in the perinatal period            injury  117 8.729389e-03 0.20479441 1.787730e-03
#40                        CH_05: Endocrine, nutritional or metabolic diseases           glucose   21 1.448176e-03 1.21639532 1.761555e-03
#41                            CH_01: Certain infectious or parasitic diseases          sexually   28 1.158125e-03 1.50407740 1.741910e-03
#42                 CH_06: Mental, behavioural or neurodevelopmental disorders           alcohol  164 3.208828e-03 0.52324814 1.679013e-03
#43   CH_24: Factors influencing health status or contact with health services          cannabis    8 7.503283e-04 1.90954250 1.432784e-03
#44                            CH_01: Certain infectious or parasitic diseases            zoster   25 1.034041e-03 1.34992672 1.395879e-03
#45                                CH_16: Diseases of the genitourinary system              male   26 2.109191e-03 0.65677954 1.385274e-03
#46                                             CH_20: Developmental anomalies             teeth   28 1.182782e-03 1.09861229 1.299419e-03
#47                                  CH_12: Diseases of the respiratory system           disease  152 1.676594e-02 0.07696104 1.290324e-03
#48  CH_22: Injury, poisoning or certain other consequences of external causes          cervical   41 2.189469e-03 0.58778666 1.286941e-03
#49   CH_24: Factors influencing health status or contact with health services           tobacco   10 9.379103e-04 1.34992672 1.266110e-03
#50                                      CH_08: Diseases of the nervous system          dementia   19 1.084723e-03 1.09861229 1.191690e-03
#51                                                           CH_02: Neoplasms          prostate   14 7.025996e-04 1.68639895 1.184863e-03
#52                                       CH_04: Diseases of the immune system            asthma    4 7.293946e-04 1.50407740 1.097066e-03
#53                        CH_05: Endocrine, nutritional or metabolic diseases           insulin   16 1.103372e-03 0.99325177 1.095926e-03
#54    CH_26: Supplementary Chapter Traditional Medicine Conditions - Module I            female    7 2.054593e-03 0.52324814 1.075062e-03
#55                                CH_16: Diseases of the genitourinary system      hysterectomy    6 4.867364e-04 2.19722458 1.069469e-03
#56                            CH_01: Certain infectious or parasitic diseases         pneumonia   19 7.858709e-04 1.34992672 1.060868e-03
#57                                       CH_04: Diseases of the immune system           disease   74 1.349380e-02 0.07696104 1.038497e-03
#58                             CH_18: Pregnancy, childbirth or the puerperium           glucose    7 8.449004e-04 1.21639532 1.027733e-03
#59                                                CH_07: Sleep-wake disorders           fatigue   10 2.214839e-03 0.46262352 1.024637e-03
#60                             CH_18: Pregnancy, childbirth or the puerperium          cervical   13 1.569101e-03 0.58778666 9.222965e-04
#61              CH_19: Certain conditions originating in the perinatal period             month    9 6.714915e-04 1.34992672 9.064643e-04
#62                                        CH_00: List of top level categories            female   53 1.730838e-03 0.52324814 9.056579e-04
#63                                                           CH_02: Neoplasms            cancer   39 1.957242e-03 0.46262352 9.054661e-04
#64   CH_24: Factors influencing health status or contact with health services           alcohol   18 1.688239e-03 0.52324814 8.833677e-04
#65                                                CH_14: Diseases of the skin              feet   17 8.879139e-04 0.99325177 8.819221e-04
#66                                    CH_13: Diseases of the digestive system           disease  208 1.142794e-02 0.07696104 8.795064e-04
#67         CH_15: Diseases of the musculoskeletal system or connective tissue          cervical    9 1.463177e-03 0.58778666 8.600358e-04
#68      CH_21: Symptoms, signs or clinical findings, not elsewhere classified              male   28 1.252236e-03 0.65677954 8.224431e-04
#69                                                CH_07: Sleep-wake disorders         confusion    3 6.644518e-04 1.21639532 8.082361e-04
#70         CH_15: Diseases of the musculoskeletal system or connective tissue            injury   24 3.901805e-03 0.20479441 7.990678e-04
#71                                        CH_00: List of top level categories              male   37 1.208321e-03 0.65677954 7.936006e-04
#72         CH_15: Diseases of the musculoskeletal system or connective tissue           disease   63 1.024224e-02 0.07696104 7.882532e-04
#73                                 CH_17: Conditions related to sexual health          sexually    1 5.213764e-04 1.50407740 7.841905e-04
#74      CH_21: Symptoms, signs or clinical findings, not elsewhere classified            cancer   36 1.610018e-03 0.46262352 7.448321e-04
#75                                  CH_11: Diseases of the circulatory system           disease  106 9.671533e-03 0.07696104 7.443312e-04
#76              CH_19: Certain conditions originating in the perinatal period             colon    9 6.714915e-04 1.09861229 7.377088e-04
#77    CH_26: Supplementary Chapter Traditional Medicine Conditions - Module I             blood   32 9.392427e-03 0.07696104 7.228510e-04
#78                                      CH_08: Diseases of the nervous system           disease  163 9.305778e-03 0.07696104 7.161823e-04
#79                        CH_05: Endocrine, nutritional or metabolic diseases          dementia    9 6.206469e-04 1.09861229 6.818503e-04
#80      CH_21: Symptoms, signs or clinical findings, not elsewhere classified            female   29 1.296959e-03 0.52324814 6.786313e-04
#81                                             CH_20: Developmental anomalies              feet   16 6.758755e-04 0.99325177 6.713145e-04
#82                                    CH_13: Diseases of the digestive system           dilated   11 6.043624e-04 1.09861229 6.639600e-04
#83    CH_26: Supplementary Chapter Traditional Medicine Conditions - Module I             fluid   11 3.228647e-03 0.20479441 6.612088e-04
#84                                CH_16: Diseases of the genitourinary system           disease  104 8.436765e-03 0.07696104 6.493022e-04
#85    CH_26: Supplementary Chapter Traditional Medicine Conditions - Module I          dementia    2 5.870267e-04 1.09861229 6.449148e-04
#86                                             CH_20: Developmental anomalies             tooth    9 3.801800e-04 1.68639895 6.411351e-04
#87                            CH_01: Certain infectious or parasitic diseases               hpv    7 2.895314e-04 2.19722458 6.361654e-04
#88                                      CH_08: Diseases of the nervous system             month    8 4.567253e-04 1.34992672 6.165457e-04
#89                                CH_16: Diseases of the genitourinary system          sexually    5 4.056137e-04 1.50407740 6.100744e-04
#90              CH_19: Certain conditions originating in the perinatal period          diabetes   20 1.492203e-03 0.40546511 6.050363e-04
#91              CH_19: Certain conditions originating in the perinatal period         pneumonia    6 4.476610e-04 1.34992672 6.043095e-04
#92              CH_19: Certain conditions originating in the perinatal period           tobacco    6 4.476610e-04 1.34992672 6.043095e-04
#93                                       CH_09: Diseases of the visual system            zoster    4 4.441977e-04 1.34992672 5.996343e-04
#94                        CH_05: Endocrine, nutritional or metabolic diseases           disease  112 7.723605e-03 0.07696104 5.944167e-04
#95                             CH_18: Pregnancy, childbirth or the puerperium            injury   24 2.896801e-03 0.20479441 5.932488e-04
#96              CH_19: Certain conditions originating in the perinatal period           insulin    8 5.968813e-04 0.99325177 5.928534e-04
#97                                      CH_08: Diseases of the nervous system            injury   50 2.854533e-03 0.20479441 5.845924e-04
#98                                      CH_08: Diseases of the nervous system        infarction   11 6.279973e-04 0.89794159 5.639049e-04
#99                                                           CH_02: Neoplasms              male   17 8.531567e-04 0.65677954 5.603358e-04
#100                                       CH_00: List of top level categories          coronary   14 4.572026e-04 1.21639532 5.561391e-04
#101                                                          CH_02: Neoplasms            female   21 1.053899e-03 0.52324814 5.514509e-04
#102                                                          CH_02: Neoplasms               hpv    5 2.509284e-04 2.19722458 5.513461e-04
#103                       CH_05: Endocrine, nutritional or metabolic diseases           alcohol   15 1.034411e-03 0.52324814 5.412539e-04
#104 CH_22: Injury, poisoning or certain other consequences of external causes             tooth    6 3.204101e-04 1.68639895 5.403393e-04
#105             CH_19: Certain conditions originating in the perinatal period             blood   94 7.013355e-03 0.07696104 5.397551e-04
#106                           CH_01: Certain infectious or parasitic diseases           fatigue   28 1.158125e-03 0.46262352 5.357761e-04
#107                                   CH_13: Diseases of the digestive system           alcohol   18 9.889567e-04 0.52324814 5.174697e-04
#108 CH_22: Injury, poisoning or certain other consequences of external causes               eye   32 1.708854e-03 0.30010459 5.128349e-04
#109                                       CH_00: List of top level categories             heart   61 1.992097e-03 0.25131443 5.006427e-04
#110                                            CH_20: Developmental anomalies               eye   39 1.647446e-03 0.30010459 4.944062e-04
#111                                      CH_04: Diseases of the immune system         pneumonia    2 3.646973e-04 1.34992672 4.923146e-04
#112                            CH_18: Pregnancy, childbirth or the puerperium          diabetes   10 1.207001e-03 0.40546511 4.893966e-04
#113        CH_15: Diseases of the musculoskeletal system or connective tissue              feet    3 4.877256e-04 0.99325177 4.844343e-04
#114             CH_19: Certain conditions originating in the perinatal period          cervical   11 8.207118e-04 0.58778666 4.824034e-04
#115                                     CH_08: Diseases of the nervous system encephalomyelitis    5 2.854533e-04 1.68639895 4.813881e-04
#116                               CH_16: Diseases of the genitourinary system          cervical   10 8.112274e-04 0.58778666 4.768286e-04
#117                                                          CH_02: Neoplasms           tobacco    7 3.512998e-04 1.34992672 4.742290e-04
#118                                                          CH_02: Neoplasms          cervical   16 8.029710e-04 0.58778666 4.719756e-04
#119                                            CH_20: Developmental anomalies              male   17 7.181177e-04 0.65677954 4.716450e-04
#120 CH_22: Injury, poisoning or certain other consequences of external causes             colon    8 4.272135e-04 1.09861229 4.693420e-04
#121                                            CH_20: Developmental anomalies             heart   44 1.858658e-03 0.25131443 4.671075e-04
#122                             CH_10: Diseases of the ear or mastoid process            zoster    1 3.447087e-04 1.34992672 4.653315e-04
#123                           CH_01: Certain infectious or parasitic diseases         confusion    9 3.722546e-04 1.21639532 4.528088e-04
#124                                      CH_09: Diseases of the visual system          diabetes   10 1.110494e-03 0.40546511 4.502666e-04
#125        CH_15: Diseases of the musculoskeletal system or connective tissue        infarction    3 4.877256e-04 0.89794159 4.379491e-04
#126                                   CH_13: Diseases of the digestive system               gum    5 2.747102e-04 1.50407740 4.131854e-04
#127                                               CH_14: Diseases of the skin          injected    3 1.566907e-04 2.60268969 4.078172e-04
#128                           CH_01: Certain infectious or parasitic diseases             fluid   48 1.985358e-03 0.20479441 4.065902e-04
#129                             CH_10: Diseases of the ear or mastoid process           disease   15 5.170631e-03 0.07696104 3.979371e-04
#130   CH_26: Supplementary Chapter Traditional Medicine Conditions - Module I            zoster    1 2.935134e-04 1.34992672 3.962215e-04
#131                            CH_18: Pregnancy, childbirth or the puerperium             fluid   16 1.931201e-03 0.20479441 3.954992e-04
#132                       CH_05: Endocrine, nutritional or metabolic diseases             sugar    3 2.068823e-04 1.90954250 3.950505e-04
#133                                       CH_00: List of top level categories          dementia   11 3.592306e-04 1.09861229 3.946551e-04
#134                                       CH_00: List of top level categories          sexually    8 2.612586e-04 1.50407740 3.929532e-04
#135   CH_26: Supplementary Chapter Traditional Medicine Conditions - Module I              male    2 5.870267e-04 0.65677954 3.855471e-04
#136     CH_21: Symptoms, signs or clinical findings, not elsewhere classified            injury   42 1.878354e-03 0.20479441 3.846764e-04
#137 CH_22: Injury, poisoning or certain other consequences of external causes             blood   93 4.966357e-03 0.07696104 3.822160e-04
#138                               CH_16: Diseases of the genitourinary system            injury   23 1.865823e-03 0.20479441 3.821101e-04
#139                                               CH_14: Diseases of the skin              male   11 5.745325e-04 0.65677954 3.773412e-04
#140 CH_22: Injury, poisoning or certain other consequences of external causes             heart   28 1.495247e-03 0.25131443 3.757772e-04
#141                                       CH_00: List of top level categories        overweight    6 1.959440e-04 1.90954250 3.741633e-04
#142                                            CH_20: Developmental anomalies           disease  112 4.731128e-03 0.07696104 3.641126e-04
#143             CH_19: Certain conditions originating in the perinatal period           glucose    4 2.984406e-04 1.21639532 3.630218e-04
#144        CH_15: Diseases of the musculoskeletal system or connective tissue           antigen    2 3.251504e-04 1.09861229 3.572142e-04
#145                CH_06: Mental, behavioural or neurodevelopmental disorders         confusion   15 2.934904e-04 1.21639532 3.570003e-04
#146                                 CH_11: Diseases of the circulatory system            injury   19 1.733577e-03 0.20479441 3.550268e-04
#147                                       CH_00: List of top level categories               eye   36 1.175664e-03 0.30010459 3.528221e-04
#148                                   CH_13: Diseases of the digestive system            injury   31 1.703203e-03 0.20479441 3.488065e-04
#149                CH_06: Mental, behavioural or neurodevelopmental disorders           fatigue   37 7.239429e-04 0.46262352 3.349130e-04
#150                               CH_16: Diseases of the genitourinary system             fluid   20 1.622455e-03 0.20479441 3.322697e-04
#151                                                          CH_02: Neoplasms               psa    2 1.003714e-04 3.29583687 3.308077e-04
#152                                       CH_00: List of top level categories        myocardial    6 1.959440e-04 1.68639895 3.304397e-04
#153                                 CH_11: Diseases of the circulatory system             blood   47 4.288321e-03 0.07696104 3.300337e-04
#154                           CH_01: Certain infectious or parasitic diseases              feet    8 3.308930e-04 0.99325177 3.286601e-04
#155             CH_19: Certain conditions originating in the perinatal period           disease   57 4.252779e-03 0.07696104 3.272983e-04
#156  CH_24: Factors influencing health status or contact with health services          prostate    2 1.875821e-04 1.68639895 3.163382e-04
#157                            CH_18: Pregnancy, childbirth or the puerperium            female    5 6.035003e-04 0.52324814 3.157804e-04
#158  CH_24: Factors influencing health status or contact with health services         mammogram    1 9.379103e-05 3.29583687 3.091199e-04
#159                                      CH_04: Diseases of the immune system        myocardial    1 1.823487e-04 1.68639895 3.075126e-04
#160                                       CH_00: List of top level categories           disease  121 3.951537e-03 0.07696104 3.041144e-04
#161                                     CH_08: Diseases of the nervous system             fluid   26 1.484357e-03 0.20479441 3.039881e-04
#162                            CH_18: Pregnancy, childbirth or the puerperium             heart   10 1.207001e-03 0.25131443 3.033367e-04
#163                                                          CH_02: Neoplasms               gum    4 2.007427e-04 1.50407740 3.019326e-04
#164     CH_21: Symptoms, signs or clinical findings, not elsewhere classified          prostate    4 1.788909e-04 1.68639895 3.016814e-04
#165                               CH_16: Diseases of the genitourinary system           glucose    3 2.433682e-04 1.21639532 2.960320e-04
#166     CH_21: Symptoms, signs or clinical findings, not elsewhere classified             blood   85 3.801431e-03 0.07696104 2.925621e-04
#167                                            CH_20: Developmental anomalies            female   13 5.491488e-04 0.52324814 2.873411e-04
#168                                 CH_12: Diseases of the respiratory system               pap    1 1.103022e-04 2.60268969 2.870825e-04
#169                           CH_01: Certain infectious or parasitic diseases               eye   23 9.513174e-04 0.30010459 2.854947e-04
#170                             CH_10: Diseases of the ear or mastoid process             fluid    4 1.378835e-03 0.20479441 2.823777e-04
#171                             CH_10: Diseases of the ear or mastoid process            injury    4 1.378835e-03 0.20479441 2.823777e-04
#172  CH_24: Factors influencing health status or contact with health services         hepatitis    3 2.813731e-04 0.99325177 2.794743e-04
#173                            CH_18: Pregnancy, childbirth or the puerperium             blood   30 3.621002e-03 0.07696104 2.786761e-04
#174                            CH_18: Pregnancy, childbirth or the puerperium           disease   30 3.621002e-03 0.07696104 2.786761e-04
#175                                            CH_20: Developmental anomalies           dilated    6 2.534533e-04 1.09861229 2.784469e-04
#176                                               CH_14: Diseases of the skin            injury   26 1.357986e-03 0.20479441 2.781079e-04
#177             CH_19: Certain conditions originating in the perinatal period             fluid   18 1.342983e-03 0.20479441 2.750354e-04
#178                                      CH_09: Diseases of the visual system            injury   12 1.332593e-03 0.20479441 2.729076e-04
#179                                CH_17: Conditions related to sexual health           alcohol    1 5.213764e-04 0.52324814 2.728093e-04
#180     CH_21: Symptoms, signs or clinical findings, not elsewhere classified           glucose    5 2.236136e-04 1.21639532 2.720025e-04
#181                                                          CH_02: Neoplasms               eye   18 9.033424e-04 0.30010459 2.710972e-04
#182                                 CH_12: Diseases of the respiratory system            injury   12 1.323627e-03 0.20479441 2.710714e-04
#183                                               CH_07: Sleep-wake disorders          coronary    1 2.214839e-04 1.21639532 2.694120e-04
#184     CH_21: Symptoms, signs or clinical findings, not elsewhere classified          sexually    4 1.788909e-04 1.50407740 2.690657e-04
#185                           CH_01: Certain infectious or parasitic diseases             blood   84 3.474376e-03 0.07696104 2.673916e-04
#186        CH_15: Diseases of the musculoskeletal system or connective tissue             fluid    8 1.300602e-03 0.20479441 2.663559e-04
#187     CH_21: Symptoms, signs or clinical findings, not elsewhere classified             fluid   29 1.296959e-03 0.20479441 2.656099e-04
#188                            CH_18: Pregnancy, childbirth or the puerperium           dilated    2 2.414001e-04 1.09861229 2.652051e-04
#189                            CH_18: Pregnancy, childbirth or the puerperium      hysterectomy    1 1.207001e-04 2.19722458 2.652051e-04
#190   CH_26: Supplementary Chapter Traditional Medicine Conditions - Module I               eye    3 8.805401e-04 0.30010459 2.642541e-04
#191                                     CH_08: Diseases of the nervous system            cancer   10 5.709066e-04 0.46262352 2.641148e-04
#192                               CH_16: Diseases of the genitourinary system            cancer    7 5.678592e-04 0.46262352 2.627050e-04
#193                                                          CH_02: Neoplasms           disease   68 3.412627e-03 0.07696104 2.626393e-04
#194                                               CH_14: Diseases of the skin           insulin    5 2.611512e-04 0.99325177 2.593888e-04
#195                                 CH_12: Diseases of the respiratory system          cervical    4 4.412089e-04 0.58778666 2.593367e-04
#196                      CH_03: Diseases of the blood or blood-forming organs        infarction    2 2.883506e-04 0.89794159 2.589220e-04
#197                CH_06: Mental, behavioural or neurodevelopmental disorders            drinks    4 7.826410e-05 3.29583687 2.579457e-04
#198     CH_21: Symptoms, signs or clinical findings, not elsewhere classified             stool    3 1.341682e-04 1.90954250 2.561998e-04
#199                       CH_05: Endocrine, nutritional or metabolic diseases           fatigue    8 5.516861e-04 0.46262352 2.552230e-04
#200                                               CH_14: Diseases of the skin           disease   63 3.290505e-03 0.07696104 2.532407e-04
#201     CH_21: Symptoms, signs or clinical findings, not elsewhere classified           disease   73 3.264758e-03 0.07696104 2.512592e-04
#202                                 CH_12: Diseases of the respiratory system             fluid   11 1.213325e-03 0.20479441 2.484821e-04
#203                                      CH_04: Diseases of the immune system           tobacco    1 1.823487e-04 1.34992672 2.461573e-04
#204  CH_24: Factors influencing health status or contact with health services           vaccine    1 9.379103e-05 2.60268969 2.441090e-04
#205                                      CH_09: Diseases of the visual system             obese    1 1.110494e-04 2.19722458 2.440005e-04
#206             CH_19: Certain conditions originating in the perinatal period             heart   13 9.699321e-04 0.25131443 2.437579e-04
#207                                         CH_25: Codes for special purposes           disease    1 3.164557e-03 0.07696104 2.435476e-04
#208                                               CH_07: Sleep-wake disorders             teeth    1 2.214839e-04 1.09861229 2.433250e-04
#209                                 CH_12: Diseases of the respiratory system           antigen    2 2.206045e-04 1.09861229 2.423588e-04
#210                            CH_18: Pregnancy, childbirth or the puerperium         hepatitis    2 2.414001e-04 0.99325177 2.397711e-04
#211                            CH_18: Pregnancy, childbirth or the puerperium           insulin    2 2.414001e-04 0.99325177 2.397711e-04
#212                                 CH_11: Diseases of the circulatory system               pap    1 9.124088e-05 2.60268969 2.374717e-04
#213  CH_24: Factors influencing health status or contact with health services             heart   10 9.379103e-04 0.25131443 2.357104e-04
#214                               CH_16: Diseases of the genitourinary system          diabetes    7 5.678592e-04 0.40546511 2.302471e-04
#215                                               CH_14: Diseases of the skin             obese    2 1.044605e-04 2.19722458 2.295231e-04
#216                                               CH_07: Sleep-wake disorders            injury    5 1.107420e-03 0.20479441 2.267934e-04
#217                       CH_05: Endocrine, nutritional or metabolic diseases             blood   42 2.896352e-03 0.07696104 2.229063e-04
#218             CH_19: Certain conditions originating in the perinatal period         hepatitis    3 2.238305e-04 0.99325177 2.223200e-04
#219                                      CH_04: Diseases of the immune system          coronary    1 1.823487e-04 1.21639532 2.218080e-04
#220                                      CH_09: Diseases of the visual system           insulin    2 2.220988e-04 0.99325177 2.206001e-04
#221                                       CH_00: List of top level categories          prostate    4 1.306293e-04 1.68639895 2.202931e-04
#222        CH_15: Diseases of the musculoskeletal system or connective tissue             month    1 1.625752e-04 1.34992672 2.194646e-04
#223                               CH_16: Diseases of the genitourinary system        infarction    3 2.433682e-04 0.89794159 2.185304e-04
#224                                       CH_00: List of top level categories             teeth    6 1.959440e-04 1.09861229 2.152664e-04
#225                                      CH_04: Diseases of the immune system          cervical    2 3.646973e-04 0.58778666 2.143642e-04
#226                                      CH_09: Diseases of the visual system           disease   25 2.776235e-03 0.07696104 2.136620e-04
#227                                      CH_09: Diseases of the visual system             sugar    1 1.110494e-04 1.90954250 2.120536e-04
#228                                               CH_14: Diseases of the skin          diabetes   10 5.223023e-04 0.40546511 2.117754e-04
#229                                 CH_12: Diseases of the respiratory system          cannabis    1 1.103022e-04 1.90954250 2.106268e-04
#230                                 CH_12: Diseases of the respiratory system             sugar    1 1.103022e-04 1.90954250 2.106268e-04
#231                           CH_01: Certain infectious or parasitic diseases encephalomyelitis    3 1.240849e-04 1.68639895 2.092566e-04
#232                                     CH_08: Diseases of the nervous system          diabetes    9 5.138159e-04 0.40546511 2.083344e-04
#233                       CH_05: Endocrine, nutritional or metabolic diseases              feet    3 2.068823e-04 0.99325177 2.054862e-04
#234                                 CH_12: Diseases of the respiratory system             blood   24 2.647253e-03 0.07696104 2.037354e-04
#235             CH_19: Certain conditions originating in the perinatal period            zoster    2 1.492203e-04 1.34992672 2.014365e-04
#236     CH_21: Symptoms, signs or clinical findings, not elsewhere classified               eye   15 6.708408e-04 0.30010459 2.013224e-04
#237             CH_19: Certain conditions originating in the perinatal period        infarction    3 2.238305e-04 0.89794159 2.009867e-04
#238                                                          CH_02: Neoplasms               hiv    4 2.007427e-04 0.99325177 1.993881e-04
#239                                                          CH_02: Neoplasms         screening    4 2.007427e-04 0.99325177 1.993881e-04
#240                                            CH_20: Developmental anomalies          cervical    8 3.379377e-04 0.58778666 1.986353e-04
#241                                   CH_13: Diseases of the digestive system        infarction    4 2.197681e-04 0.89794159 1.973390e-04
#242     CH_21: Symptoms, signs or clinical findings, not elsewhere classified           antigen    4 1.788909e-04 1.09861229 1.965317e-04
#243     CH_21: Symptoms, signs or clinical findings, not elsewhere classified             teeth    4 1.788909e-04 1.09861229 1.965317e-04
#244                CH_06: Mental, behavioural or neurodevelopmental disorders            smoked    3 5.869808e-05 3.29583687 1.934593e-04
#245                       CH_05: Endocrine, nutritional or metabolic diseases            cancer    6 4.137646e-04 0.46262352 1.914172e-04
#246     CH_21: Symptoms, signs or clinical findings, not elsewhere classified             heart   17 7.602862e-04 0.25131443 1.910709e-04
#247                                     CH_08: Diseases of the nervous system               flu    1 5.709066e-05 3.29583687 1.881615e-04
#248  CH_24: Factors influencing health status or contact with health services               hiv    2 1.875821e-04 0.99325177 1.863162e-04
#249                       CH_05: Endocrine, nutritional or metabolic diseases             month    2 1.379215e-04 1.34992672 1.861839e-04
#250                                     CH_08: Diseases of the nervous system           fatigue    7 3.996346e-04 0.46262352 1.848804e-04
#251                CH_06: Mental, behavioural or neurodevelopmental disorders           disease  122 2.387055e-03 0.07696104 1.837102e-04
#252                           CH_01: Certain infectious or parasitic diseases           antigen    4 1.654465e-04 1.09861229 1.817616e-04
#253                           CH_01: Certain infectious or parasitic diseases             colon    4 1.654465e-04 1.09861229 1.817616e-04
#254                           CH_01: Certain infectious or parasitic diseases          dementia    4 1.654465e-04 1.09861229 1.817616e-04
#255                                 CH_11: Diseases of the circulatory system               hiv    2 1.824818e-04 0.99325177 1.812503e-04
#256                                 CH_11: Diseases of the circulatory system           insulin    2 1.824818e-04 0.99325177 1.812503e-04
#257                       CH_05: Endocrine, nutritional or metabolic diseases              male    4 2.758430e-04 0.65677954 1.811681e-04
#258                                      CH_04: Diseases of the immune system              feet    1 1.823487e-04 0.99325177 1.811181e-04
#259                                   CH_13: Diseases of the digestive system             decay    1 5.494204e-05 3.29583687 1.810800e-04
#260                                               CH_07: Sleep-wake disorders          diabetes    2 4.429679e-04 0.40546511 1.796080e-04
#261                               CH_16: Diseases of the genitourinary system               hpv    1 8.112274e-05 2.19722458 1.782449e-04
#262     CH_21: Symptoms, signs or clinical findings, not elsewhere classified         screening    4 1.788909e-04 0.99325177 1.776837e-04
#263                                       CH_00: List of top level categories        infarction    6 1.959440e-04 0.89794159 1.759462e-04
#264                      CH_03: Diseases of the blood or blood-forming organs         confusion    1 1.441753e-04 1.21639532 1.753742e-04
#265                                               CH_14: Diseases of the skin           dilated    3 1.566907e-04 1.09861229 1.721423e-04
#266                                      CH_04: Diseases of the immune system            cancer    2 3.646973e-04 0.46262352 1.687175e-04
#267                               CH_16: Diseases of the genitourinary system             blood   27 2.190314e-03 0.07696104 1.685688e-04
#268                       CH_05: Endocrine, nutritional or metabolic diseases         confusion    2 1.379215e-04 1.21639532 1.677671e-04
#269                                               CH_07: Sleep-wake disorders             heart    3 6.644518e-04 0.25131443 1.669863e-04
#270                           CH_01: Certain infectious or parasitic diseases             heart   16 6.617860e-04 0.25131443 1.663164e-04
#271     CH_21: Symptoms, signs or clinical findings, not elsewhere classified           fatigue    8 3.577818e-04 0.46262352 1.655183e-04
#272                                      CH_04: Diseases of the immune system               eye    3 5.470460e-04 0.30010459 1.641710e-04
#273                                               CH_14: Diseases of the skin            female    6 3.133814e-04 0.52324814 1.639762e-04
#274                                       CH_00: List of top level categories         screening    5 1.632866e-04 0.99325177 1.621847e-04
#275        CH_15: Diseases of the musculoskeletal system or connective tissue               hiv    1 1.625752e-04 0.99325177 1.614781e-04
#276                                       CH_00: List of top level categories             blood   64 2.090069e-03 0.07696104 1.608539e-04
#277 CH_22: Injury, poisoning or certain other consequences of external causes               gum    2 1.068034e-04 1.50407740 1.606405e-04
#278                                       CH_00: List of top level categories          diabetes   12 3.918879e-04 0.40546511 1.588969e-04
#279                      CH_03: Diseases of the blood or blood-forming organs           antigen    1 1.441753e-04 1.09861229 1.583928e-04
#280                      CH_03: Diseases of the blood or blood-forming organs             colon    1 1.441753e-04 1.09861229 1.583928e-04
#281                      CH_03: Diseases of the blood or blood-forming organs          dementia    1 1.441753e-04 1.09861229 1.583928e-04
#282     CH_21: Symptoms, signs or clinical findings, not elsewhere classified          cervical    6 2.683363e-04 0.58778666 1.577245e-04
#283                                   CH_13: Diseases of the digestive system             blood   37 2.032855e-03 0.07696104 1.564507e-04
#284             CH_19: Certain conditions originating in the perinatal period           alcohol    4 2.984406e-04 0.52324814 1.561585e-04
#285                                               CH_14: Diseases of the skin         hepatitis    3 1.566907e-04 0.99325177 1.556333e-04
#286                       CH_05: Endocrine, nutritional or metabolic diseases             fluid   11 7.585684e-04 0.20479441 1.553506e-04
#287                                      CH_09: Diseases of the visual system            cancer    3 3.331483e-04 0.46262352 1.541222e-04
#288                                       CH_00: List of top level categories             fluid   23 7.511185e-04 0.20479441 1.538249e-04
#289     CH_21: Symptoms, signs or clinical findings, not elsewhere classified             tooth    2 8.944544e-05 1.68639895 1.508407e-04
#290        CH_15: Diseases of the musculoskeletal system or connective tissue            cancer    2 3.251504e-04 0.46262352 1.504222e-04
#291                                     CH_08: Diseases of the nervous system       considering    1 5.709066e-05 2.60268969 1.485893e-04
#292             CH_19: Certain conditions originating in the perinatal period              feet    2 1.492203e-04 0.99325177 1.482134e-04
#293             CH_19: Certain conditions originating in the perinatal period         screening    2 1.492203e-04 0.99325177 1.482134e-04
#294  CH_24: Factors influencing health status or contact with health services            female    3 2.813731e-04 0.52324814 1.472280e-04
#295  CH_24: Factors influencing health status or contact with health services             blood   20 1.875821e-03 0.07696104 1.443651e-04
#296                       CH_05: Endocrine, nutritional or metabolic diseases            female    4 2.758430e-04 0.52324814 1.443344e-04
#297                      CH_03: Diseases of the blood or blood-forming organs         hepatitis    1 1.441753e-04 0.99325177 1.432024e-04
#298                      CH_03: Diseases of the blood or blood-forming organs          pregnant    1 1.441753e-04 0.99325177 1.432024e-04
#299                                                          CH_02: Neoplasms             blood   37 1.856870e-03 0.07696104 1.429067e-04
#300             CH_19: Certain conditions originating in the perinatal period        overweight    1 7.461016e-05 1.90954250 1.424713e-04
#301             CH_19: Certain conditions originating in the perinatal period             stool    1 7.461016e-05 1.90954250 1.424713e-04
#302  CH_24: Factors influencing health status or contact with health services            asthma    1 9.379103e-05 1.50407740 1.410690e-04
#303  CH_24: Factors influencing health status or contact with health services               eye    5 4.689552e-04 0.30010459 1.407356e-04
#304                             CH_10: Diseases of the ear or mastoid process          diabetes    1 3.447087e-04 0.40546511 1.397674e-04
#305                                            CH_20: Developmental anomalies      pneumococcal    1 4.224222e-05 3.29583687 1.392235e-04
#306                                      CH_04: Diseases of the immune system             heart    3 5.470460e-04 0.25131443 1.374805e-04
#307                                     CH_08: Diseases of the nervous system               eye    8 4.567253e-04 0.30010459 1.370654e-04
#308   CH_26: Supplementary Chapter Traditional Medicine Conditions - Module I           fatigue    1 2.935134e-04 0.46262352 1.357862e-04
#309                                      CH_09: Diseases of the visual system          coronary    1 1.110494e-04 1.21639532 1.350800e-04
#310                                      CH_09: Diseases of the visual system           glucose    1 1.110494e-04 1.21639532 1.350800e-04
#311                                     CH_08: Diseases of the nervous system          cervical    4 2.283626e-04 0.58778666 1.342285e-04
#312                                 CH_12: Diseases of the respiratory system         confusion    1 1.103022e-04 1.21639532 1.341711e-04
#313                      CH_03: Diseases of the blood or blood-forming organs            cancer    2 2.883506e-04 0.46262352 1.333978e-04
#314     CH_21: Symptoms, signs or clinical findings, not elsewhere classified              feet    3 1.341682e-04 0.99325177 1.332628e-04
#315                                               CH_07: Sleep-wake disorders               eye    2 4.429679e-04 0.30010459 1.329367e-04
#316                                       CH_00: List of top level categories           tobacco    3 9.797198e-05 1.34992672 1.322550e-04
#317                                       CH_00: List of top level categories            zoster    3 9.797198e-05 1.34992672 1.322550e-04
#318                CH_06: Mental, behavioural or neurodevelopmental disorders           tobacco    5 9.783013e-05 1.34992672 1.320635e-04
#319        CH_15: Diseases of the musculoskeletal system or connective tissue          diabetes    2 3.251504e-04 0.40546511 1.318371e-04
#320                                                          CH_02: Neoplasms           alcohol    5 2.509284e-04 0.52324814 1.312978e-04
#321                                CH_17: Conditions related to sexual health             heart    1 5.213764e-04 0.25131443 1.310294e-04
#322                                       CH_00: List of top level categories         hepatitis    4 1.306293e-04 0.99325177 1.297478e-04
#323                                   CH_13: Diseases of the digestive system          cervical    4 2.197681e-04 0.58778666 1.291768e-04
#324                CH_06: Mental, behavioural or neurodevelopmental disorders        cigarettes    2 3.913205e-05 3.29583687 1.289729e-04
#325             CH_19: Certain conditions originating in the perinatal period        myocardial    1 7.461016e-05 1.68639895 1.258225e-04
#326                                     CH_08: Diseases of the nervous system           myalgic    1 5.709066e-05 2.19722458 1.254410e-04
#327                       CH_05: Endocrine, nutritional or metabolic diseases             heart    7 4.827253e-04 0.25131443 1.213158e-04
#328                                 CH_12: Diseases of the respiratory system             teeth    1 1.103022e-04 1.09861229 1.211794e-04
#329                                               CH_14: Diseases of the skin            cancer    5 2.611512e-04 0.46262352 1.208147e-04
#330                                      CH_04: Diseases of the immune system              male    1 1.823487e-04 0.65677954 1.197629e-04
#331                                               CH_07: Sleep-wake disorders           disease    7 1.550388e-03 0.07696104 1.193194e-04
#332                                       CH_00: List of top level categories           glucose    3 9.797198e-05 1.21639532 1.191727e-04
#333                                               CH_07: Sleep-wake disorders           alcohol    1 2.214839e-04 0.52324814 1.158911e-04
#334  CH_24: Factors influencing health status or contact with health services         confusion    1 9.379103e-05 1.21639532 1.140870e-04
#335  CH_24: Factors influencing health status or contact with health services          coronary    1 9.379103e-05 1.21639532 1.140870e-04
#336                                      CH_09: Diseases of the visual system             fluid    5 5.552471e-04 0.20479441 1.137115e-04
#337 CH_22: Injury, poisoning or certain other consequences of external causes            female    4 2.136067e-04 0.52324814 1.117693e-04
#338                                 CH_12: Diseases of the respiratory system             heart    4 4.412089e-04 0.25131443 1.108822e-04
#339                                      CH_09: Diseases of the visual system               hiv    1 1.110494e-04 0.99325177 1.103000e-04
#340                                     CH_08: Diseases of the nervous system        overweight    1 5.709066e-05 1.90954250 1.090170e-04
#341                      CH_03: Diseases of the blood or blood-forming organs             heart    3 4.325260e-04 0.25131443 1.087000e-04
#342                           CH_01: Certain infectious or parasitic diseases           vaccine    1 4.136162e-05 2.60268969 1.076515e-04
#343                                   CH_13: Diseases of the digestive system             stool    1 5.494204e-05 1.90954250 1.049142e-04
#344                       CH_05: Endocrine, nutritional or metabolic diseases               eye    5 3.448038e-04 0.30010459 1.034772e-04
#345                             CH_10: Diseases of the ear or mastoid process               eye    1 3.447087e-04 0.30010459 1.034487e-04
#346  CH_24: Factors influencing health status or contact with health services           antigen    1 9.379103e-05 1.09861229 1.030400e-04
#347                                 CH_12: Diseases of the respiratory system           fatigue    2 2.206045e-04 0.46262352 1.020568e-04
#348                CH_06: Mental, behavioural or neurodevelopmental disorders       considering    2 3.913205e-05 2.60268969 1.018486e-04
#349                                     CH_08: Diseases of the nervous system             blood   23 1.313085e-03 0.07696104 1.010564e-04
#350                                 CH_11: Diseases of the circulatory system           antigen    1 9.124088e-05 1.09861229 1.002383e-04
#351                                                          CH_02: Neoplasms         hepatitis    2 1.003714e-04 0.99325177 9.969405e-05
#352     CH_21: Symptoms, signs or clinical findings, not elsewhere classified             colon    2 8.944544e-05 1.09861229 9.826586e-05
#353     CH_21: Symptoms, signs or clinical findings, not elsewhere classified           myalgic    1 4.472272e-05 2.19722458 9.826586e-05
#354     CH_21: Symptoms, signs or clinical findings, not elsewhere classified             obese    1 4.472272e-05 2.19722458 9.826586e-05
#355                                       CH_00: List of top level categories              feet    3 9.797198e-05 0.99325177 9.731084e-05
#356                                       CH_00: List of top level categories          cervical    5 1.632866e-04 0.58778666 9.597771e-05
#357                                 CH_11: Diseases of the circulatory system           alcohol    2 1.824818e-04 0.52324814 9.548324e-05
#358                                      CH_04: Diseases of the immune system            female    1 1.823487e-04 0.52324814 9.541359e-05
#359                                            CH_20: Developmental anomalies             fluid   11 4.646644e-04 0.20479441 9.516067e-05
#360                                               CH_14: Diseases of the skin        infarction    2 1.044605e-04 0.89794159 9.379939e-05
#361  CH_24: Factors influencing health status or contact with health services           insulin    1 9.379103e-05 0.99325177 9.315811e-05
#362  CH_24: Factors influencing health status or contact with health services          pregnant    1 9.379103e-05 0.99325177 9.315811e-05
#363                                            CH_20: Developmental anomalies             colon    2 8.448443e-05 1.09861229 9.281564e-05
#364                                     CH_08: Diseases of the nervous system           alcohol    3 1.712720e-04 0.52324814 8.961775e-05
#365                                 CH_12: Diseases of the respiratory system          diabetes    2 2.206045e-04 0.40546511 8.944741e-05
#366                               CH_16: Diseases of the genitourinary system           dilated    1 8.112274e-05 1.09861229 8.912244e-05
#367                      CH_03: Diseases of the blood or blood-forming organs            injury    3 4.325260e-04 0.20479441 8.857890e-05
#368                                            CH_20: Developmental anomalies           alcohol    4 1.689689e-04 0.52324814 8.841265e-05
#369                                       CH_00: List of top level categories         pneumonia    2 6.531465e-05 1.34992672 8.817000e-05
#370  CH_24: Factors influencing health status or contact with health services           disease   12 1.125492e-03 0.07696104 8.661907e-05
#371     CH_21: Symptoms, signs or clinical findings, not elsewhere classified             sugar    1 4.472272e-05 1.90954250 8.539993e-05
#372        CH_15: Diseases of the musculoskeletal system or connective tissue            female    1 1.625752e-04 0.52324814 8.506717e-05
#373                                 CH_11: Diseases of the circulatory system           fatigue    2 1.824818e-04 0.46262352 8.442035e-05
#374                                      CH_04: Diseases of the immune system           fatigue    1 1.823487e-04 0.46262352 8.435877e-05
#375                                      CH_04: Diseases of the immune system             blood    6 1.094092e-03 0.07696104 8.420245e-05
#376                CH_06: Mental, behavioural or neurodevelopmental disorders            injury   21 4.108865e-04 0.20479441 8.414727e-05
#377                                            CH_20: Developmental anomalies          pregnant    2 8.448443e-05 0.99325177 8.391431e-05
#378                           CH_01: Certain infectious or parasitic diseases          pregnant    2 8.272325e-05 0.99325177 8.216501e-05
#379                               CH_16: Diseases of the genitourinary system          pregnant    1 8.112274e-05 0.99325177 8.057530e-05
#380                               CH_16: Diseases of the genitourinary system         screening    1 8.112274e-05 0.99325177 8.057530e-05
#381                           CH_01: Certain infectious or parasitic diseases             stool    1 4.136162e-05 1.90954250 7.898178e-05
#382                                               CH_14: Diseases of the skin            asthma    1 5.223023e-05 1.50407740 7.855831e-05
#383                                               CH_14: Diseases of the skin          sexually    1 5.223023e-05 1.50407740 7.855831e-05
#384                                     CH_08: Diseases of the nervous system            zoster    1 5.709066e-05 1.34992672 7.706821e-05
#385                                      CH_09: Diseases of the visual system             blood    9 9.994448e-04 0.07696104 7.691831e-05
#386  CH_24: Factors influencing health status or contact with health services          diabetes    2 1.875821e-04 0.40546511 7.605798e-05
#387                       CH_05: Endocrine, nutritional or metabolic diseases             teeth    1 6.896076e-05 1.09861229 7.576114e-05
#388     CH_21: Symptoms, signs or clinical findings, not elsewhere classified encephalomyelitis    1 4.472272e-05 1.68639895 7.542035e-05
#389     CH_21: Symptoms, signs or clinical findings, not elsewhere classified        myocardial    1 4.472272e-05 1.68639895 7.542035e-05
#390        CH_15: Diseases of the musculoskeletal system or connective tissue           fatigue    1 1.625752e-04 0.46262352 7.521111e-05
#391                                 CH_11: Diseases of the circulatory system             fluid    4 3.649635e-04 0.20479441 7.474249e-05
#392             CH_19: Certain conditions originating in the perinatal period               hiv    1 7.461016e-05 0.99325177 7.410668e-05
#393             CH_19: Certain conditions originating in the perinatal period          pregnant    1 7.461016e-05 0.99325177 7.410668e-05
#394                                 CH_11: Diseases of the circulatory system          diabetes    2 1.824818e-04 0.40546511 7.398998e-05
#395                           CH_01: Certain infectious or parasitic diseases          cervical    3 1.240849e-04 0.58778666 7.293543e-05
#396     CH_21: Symptoms, signs or clinical findings, not elsewhere classified          diabetes    4 1.788909e-04 0.40546511 7.253401e-05
#397                                       CH_00: List of top level categories      hysterectomy    1 3.265733e-05 2.19722458 7.175548e-05
#398                                               CH_14: Diseases of the skin             month    1 5.223023e-05 1.34992672 7.050698e-05
#399     CH_21: Symptoms, signs or clinical findings, not elsewhere classified           alcohol    3 1.341682e-04 0.52324814 7.020324e-05
#400                                     CH_08: Diseases of the nervous system         confusion    1 5.709066e-05 1.21639532 6.944481e-05
#401                       CH_05: Endocrine, nutritional or metabolic diseases          pregnant    1 6.896076e-05 0.99325177 6.849540e-05
#402                       CH_05: Endocrine, nutritional or metabolic diseases         screening    1 6.896076e-05 0.99325177 6.849540e-05
#403                                                          CH_02: Neoplasms         pneumonia    1 5.018569e-05 1.34992672 6.774700e-05
#404                                   CH_13: Diseases of the digestive system             fluid    6 3.296522e-04 0.20479441 6.751093e-05
#405     CH_21: Symptoms, signs or clinical findings, not elsewhere classified               gum    1 4.472272e-05 1.50407740 6.726643e-05
#406                           CH_01: Certain infectious or parasitic diseases            female    3 1.240849e-04 0.52324814 6.492718e-05
#407                                                          CH_02: Neoplasms             heart    5 2.509284e-04 0.25131443 6.306194e-05
#408                                     CH_08: Diseases of the nervous system           dilated    1 5.709066e-05 1.09861229 6.272050e-05
#409        CH_15: Diseases of the musculoskeletal system or connective tissue             blood    5 8.128760e-04 0.07696104 6.255978e-05
#410                                       CH_00: List of top level categories          cannabis    1 3.265733e-05 1.90954250 6.236055e-05
#411                           CH_01: Certain infectious or parasitic diseases            asthma    1 4.136162e-05 1.50407740 6.221108e-05
#412                           CH_01: Certain infectious or parasitic diseases               gum    1 4.136162e-05 1.50407740 6.221108e-05
#413  CH_24: Factors influencing health status or contact with health services              male    1 9.379103e-05 0.65677954 6.160003e-05
#414     CH_21: Symptoms, signs or clinical findings, not elsewhere classified             month    1 4.472272e-05 1.34992672 6.037239e-05
#415     CH_21: Symptoms, signs or clinical findings, not elsewhere classified           tobacco    1 4.472272e-05 1.34992672 6.037239e-05
#416                                   CH_13: Diseases of the digestive system           antigen    1 5.494204e-05 1.09861229 6.036000e-05
#417                                               CH_14: Diseases of the skin             blood   15 7.834535e-04 0.07696104 6.029539e-05
#418                                 CH_11: Diseases of the circulatory system              male    1 9.124088e-05 0.65677954 5.992514e-05
#419                           CH_01: Certain infectious or parasitic diseases            injury    7 2.895314e-04 0.20479441 5.929441e-05
#420 CH_22: Injury, poisoning or certain other consequences of external causes             teeth    1 5.340169e-05 1.09861229 5.866775e-05
#421                                            CH_20: Developmental anomalies            cancer    3 1.267267e-04 0.46262352 5.862673e-05
#422                CH_06: Mental, behavioural or neurodevelopmental disorders               hiv    3 5.869808e-05 0.99325177 5.830197e-05
#423  CH_24: Factors influencing health status or contact with health services             fluid    3 2.813731e-04 0.20479441 5.762364e-05
#424                                   CH_13: Diseases of the digestive system            female    2 1.098841e-04 0.52324814 5.749664e-05
#425                           CH_01: Certain infectious or parasitic diseases            cancer    3 1.240849e-04 0.46262352 5.740458e-05
#426                                     CH_08: Diseases of the nervous system             heart    4 2.283626e-04 0.25131443 5.739083e-05
#427                                            CH_20: Developmental anomalies         pneumonia    1 4.224222e-05 1.34992672 5.702390e-05
#428                                     CH_08: Diseases of the nervous system         screening    1 5.709066e-05 0.99325177 5.670540e-05
#429                                   CH_13: Diseases of the digestive system             heart    4 2.197681e-04 0.25131443 5.523091e-05
#430                          CH_23: External causes of morbidity or mortality          pregnant    1 5.559262e-05 0.99325177 5.521747e-05
#431                                                          CH_02: Neoplasms           antigen    1 5.018569e-05 1.09861229 5.513461e-05
#432                                       CH_00: List of top level categories encephalomyelitis    1 3.265733e-05 1.68639895 5.507328e-05
#433                                       CH_00: List of top level categories             tooth    1 3.265733e-05 1.68639895 5.507328e-05
#434                                   CH_13: Diseases of the digestive system               hiv    1 5.494204e-05 0.99325177 5.457127e-05
#435                                   CH_13: Diseases of the digestive system           insulin    1 5.494204e-05 0.99325177 5.457127e-05
#436     CH_21: Symptoms, signs or clinical findings, not elsewhere classified          coronary    1 4.472272e-05 1.21639532 5.440051e-05
#437                                 CH_11: Diseases of the circulatory system          cervical    1 9.124088e-05 0.58778666 5.363017e-05
#438 CH_22: Injury, poisoning or certain other consequences of external causes              feet    1 5.340169e-05 0.99325177 5.304132e-05
#439 CH_22: Injury, poisoning or certain other consequences of external causes         hepatitis    1 5.340169e-05 0.99325177 5.304132e-05
#440                                               CH_14: Diseases of the skin               hiv    1 5.223023e-05 0.99325177 5.187777e-05
#441                                       CH_00: List of top level categories           alcohol    3 9.797198e-05 0.52324814 5.126366e-05
#442                CH_06: Mental, behavioural or neurodevelopmental disorders          injected    1 1.956603e-05 2.60268969 5.092429e-05
#443                                   CH_13: Diseases of the digestive system            cancer    2 1.098841e-04 0.46262352 5.083496e-05
#444                           CH_01: Certain infectious or parasitic diseases           glucose    1 4.136162e-05 1.21639532 5.031209e-05
#445     CH_21: Symptoms, signs or clinical findings, not elsewhere classified          dementia    1 4.472272e-05 1.09861229 4.913293e-05
#446     CH_21: Symptoms, signs or clinical findings, not elsewhere classified           dilated    1 4.472272e-05 1.09861229 4.913293e-05
#447                                       CH_00: List of top level categories            asthma    1 3.265733e-05 1.50407740 4.911915e-05
#448                                       CH_00: List of top level categories               gum    1 3.265733e-05 1.50407740 4.911915e-05
#449                                            CH_20: Developmental anomalies          dementia    1 4.224222e-05 1.09861229 4.640782e-05
#450                           CH_01: Certain infectious or parasitic diseases           dilated    1 4.136162e-05 1.09861229 4.544039e-05
#451                           CH_01: Certain infectious or parasitic diseases             teeth    1 4.136162e-05 1.09861229 4.544039e-05
#452                                       CH_00: List of top level categories           fatigue    3 9.797198e-05 0.46262352 4.532414e-05
#453             CH_19: Certain conditions originating in the perinatal period               eye    2 1.492203e-04 0.30010459 4.478170e-05
#454     CH_21: Symptoms, signs or clinical findings, not elsewhere classified           insulin    1 4.472272e-05 0.99325177 4.442092e-05
#455  CH_24: Factors influencing health status or contact with health services            cancer    1 9.379103e-05 0.46262352 4.338994e-05
#456                CH_06: Mental, behavioural or neurodevelopmental disorders           myalgic    1 1.956603e-05 2.19722458 4.299095e-05
#457                       CH_05: Endocrine, nutritional or metabolic diseases            injury    3 2.068823e-04 0.20479441 4.236834e-05
#458                                 CH_11: Diseases of the circulatory system            cancer    1 9.124088e-05 0.46262352 4.221018e-05
#459     CH_21: Symptoms, signs or clinical findings, not elsewhere classified        infarction    1 4.472272e-05 0.89794159 4.015839e-05
#460                                CH_17: Conditions related to sexual health             blood    1 5.213764e-04 0.07696104 4.012567e-05
#461                                CH_17: Conditions related to sexual health           disease    1 5.213764e-04 0.07696104 4.012567e-05
#462                               CH_16: Diseases of the genitourinary system           fatigue    1 8.112274e-05 0.46262352 3.752929e-05
#463                                     CH_08: Diseases of the nervous system              male    1 5.709066e-05 0.65677954 3.749598e-05
#464                           CH_01: Certain infectious or parasitic diseases        infarction    1 4.136162e-05 0.89794159 3.714032e-05
#465                                       CH_00: List of top level categories             colon    1 3.265733e-05 1.09861229 3.587774e-05
#466 CH_22: Injury, poisoning or certain other consequences of external causes              male    1 5.340169e-05 0.65677954 3.507314e-05
#467             CH_19: Certain conditions originating in the perinatal period           fatigue    1 7.461016e-05 0.46262352 3.451642e-05
#468                           CH_01: Certain infectious or parasitic diseases          diabetes    2 8.272325e-05 0.40546511 3.354139e-05
#469                          CH_23: External causes of morbidity or mortality               eye    2 1.111852e-04 0.30010459 3.336720e-05
#470                                 CH_12: Diseases of the respiratory system               eye    1 1.103022e-04 0.30010459 3.310221e-05
#471                CH_06: Mental, behavioural or neurodevelopmental disorders encephalomyelitis    1 1.956603e-05 1.68639895 3.299613e-05
#472 CH_22: Injury, poisoning or certain other consequences of external causes           disease    8 4.272135e-04 0.07696104 3.287880e-05
#473                                       CH_00: List of top level categories          pregnant    1 3.265733e-05 0.99325177 3.243695e-05
#474                                               CH_14: Diseases of the skin               eye    2 1.044605e-04 0.30010459 3.134906e-05
#475                          CH_23: External causes of morbidity or mortality           alcohol    1 5.559262e-05 0.52324814 2.908873e-05
#476 CH_22: Injury, poisoning or certain other consequences of external causes           alcohol    1 5.340169e-05 0.52324814 2.794233e-05
#477                                      CH_09: Diseases of the visual system             heart    1 1.110494e-04 0.25131443 2.790832e-05
#478                                 CH_11: Diseases of the circulatory system               eye    1 9.124088e-05 0.30010459 2.738181e-05
#479                                               CH_14: Diseases of the skin           alcohol    1 5.223023e-05 0.52324814 2.732937e-05
#480                CH_06: Mental, behavioural or neurodevelopmental disorders            cancer    3 5.869808e-05 0.46262352 2.715511e-05
#481                                            CH_20: Developmental anomalies             blood    8 3.379377e-04 0.07696104 2.600804e-05
#482                                   CH_13: Diseases of the digestive system           fatigue    1 5.494204e-05 0.46262352 2.541748e-05
#483 CH_22: Injury, poisoning or certain other consequences of external causes           fatigue    1 5.340169e-05 0.46262352 2.470488e-05
#484                CH_06: Mental, behavioural or neurodevelopmental disorders               eye    4 7.826410e-05 0.30010459 2.348742e-05
#485 CH_22: Injury, poisoning or certain other consequences of external causes             fluid    2 1.068034e-04 0.20479441 2.187273e-05
#486                CH_06: Mental, behavioural or neurodevelopmental disorders           insulin    1 1.956603e-05 0.99325177 1.943399e-05
#487                                            CH_20: Developmental anomalies          diabetes    1 4.224222e-05 0.40546511 1.712775e-05
#488                          CH_23: External causes of morbidity or mortality             blood    4 2.223705e-04 0.07696104 1.711386e-05
#489                                               CH_07: Sleep-wake disorders             blood    1 2.214839e-04 0.07696104 1.704563e-05
#490                                       CH_00: List of top level categories            cancer    1 3.265733e-05 0.46262352 1.510805e-05
#491                          CH_23: External causes of morbidity or mortality             fluid    1 5.559262e-05 0.20479441 1.138506e-05
#492                                               CH_14: Diseases of the skin             fluid    1 5.223023e-05 0.20479441 1.069646e-05
#493                                                          CH_02: Neoplasms             fluid    1 5.018569e-05 0.20479441 1.027775e-05
#494                CH_06: Mental, behavioural or neurodevelopmental disorders             heart    2 3.913205e-05 0.25131443 9.834449e-06
#495                CH_06: Mental, behavioural or neurodevelopmental disorders             blood    6 1.173962e-04 0.07696104 9.034930e-06
#496                                            CH_20: Developmental anomalies            injury    1 4.224222e-05 0.20479441 8.650970e-06
#497                CH_06: Mental, behavioural or neurodevelopmental disorders             fluid    2 3.913205e-05 0.20479441 8.014025e-06
#498                CH_06: Mental, behavioural or neurodevelopmental disorders          diabetes    1 1.956603e-05 0.40546511 7.933341e-06




setwd('C:\\Users\\mdjaw\\OneDrive\\Documents\\Markian Training\\Research_USQ\\USCenterDiseaseControlPrevention\\LLCP2021XPT')
data <- read.csv('LLCP2021XPT.csv')
state_data <- subset(data, X_STATE == 4 | X_STATE == 9 | X_STATE == 10 | X_STATE == 13 | X_STATE == 15 | X_STATE == 18 | X_STATE == 22 | X_STATE == 25 | X_STATE == 26 | X_STATE == 28 | X_STATE == 29 | X_STATE == 30 | X_STATE == 34 | X_STATE == 35 | X_STATE == 37 | X_STATE == 44 | X_STATE == 46 | X_STATE == 49 | X_STATE == 50 | X_STATE == 51 | X_STATE == 55 | X_STATE == 56 | X_STATE == 66 | X_STATE == 31 | X_STATE == 16 | X_STATE == 19 | X_STATE == 2 | X_STATE == 31 | X_STATE == 1 | X_STATE == 40 | X_STATE == 39) 




LM_WHOICD_KG <- subset(LM_Chapter_Scores,select=c('chapter','word','n'))

state_data['GENDER_00'] <- 0
state_data['GENDER_01'] <- 0
state_data['GENDER_02'] <- 0
state_data['GENDER_03'] <- 0
state_data['GENDER_04'] <- 0                                  
state_data['GENDER_05'] <- 0
state_data['GENDER_06'] <- 0
state_data['GENDER_07'] <- 0
state_data['GENDER_08'] <- 0
state_data['GENDER_09'] <- 0
state_data['GENDER_10'] <- 0
state_data['GENDER_11'] <- 0
state_data['GENDER_12'] <- 0
state_data['GENDER_13'] <- 0
state_data['GENDER_14'] <- 0
state_data['GENDER_15'] <- 0
state_data['GENDER_16'] <- 0
state_data['GENDER_17'] <- 0
state_data['GENDER_18'] <- 0
state_data['GENDER_19'] <- 0
state_data['GENDER_20'] <- 0
state_data['GENDER_21'] <- 0
state_data['GENDER_22'] <- 0
state_data['GENDER_23'] <- 0
state_data['GENDER_24'] <- 0
state_data['GENDER_25'] <- 0
state_data['GENDER_26'] <- 0

state_data['LABEL_00'] <- 0
state_data['LABEL_01'] <- 0
state_data['LABEL_02'] <- 0
state_data['LABEL_03'] <- 0
state_data['LABEL_04'] <- 0                                  
state_data['LABEL_05'] <- 0
state_data['LABEL_06'] <- 0
state_data['LABEL_07'] <- 0
state_data['LABEL_08'] <- 0
state_data['LABEL_09'] <- 0
state_data['LABEL_10'] <- 0
state_data['LABEL_11'] <- 0
state_data['LABEL_12'] <- 0
state_data['LABEL_13'] <- 0
state_data['LABEL_14'] <- 0
state_data['LABEL_15'] <- 0
state_data['LABEL_16'] <- 0
state_data['LABEL_17'] <- 0
state_data['LABEL_18'] <- 0
state_data['LABEL_19'] <- 0
state_data['LABEL_20'] <- 0
state_data['LABEL_21'] <- 0
state_data['LABEL_22'] <- 0
state_data['LABEL_23'] <- 0
state_data['LABEL_24'] <- 0
state_data['LABEL_25'] <- 0
state_data['LABEL_26'] <- 0

state_data['WEIGHT_00'] <- 0
state_data['WEIGHT_01'] <- 0
state_data['WEIGHT_02'] <- 0
state_data['WEIGHT_03'] <- 0
state_data['WEIGHT_04'] <- 0                                  
state_data['WEIGHT_05'] <- 0
state_data['WEIGHT_06'] <- 0
state_data['WEIGHT_07'] <- 0
state_data['WEIGHT_08'] <- 0
state_data['WEIGHT_09'] <- 0
state_data['WEIGHT_10'] <- 0
state_data['WEIGHT_11'] <- 0
state_data['WEIGHT_12'] <- 0
state_data['WEIGHT_13'] <- 0
state_data['WEIGHT_14'] <- 0
state_data['WEIGHT_15'] <- 0
state_data['WEIGHT_16'] <- 0
state_data['WEIGHT_17'] <- 0
state_data['WEIGHT_18'] <- 0
state_data['WEIGHT_19'] <- 0
state_data['WEIGHT_20'] <- 0
state_data['WEIGHT_21'] <- 0
state_data['WEIGHT_22'] <- 0
state_data['WEIGHT_23'] <- 0
state_data['WEIGHT_24'] <- 0
state_data['WEIGHT_25'] <- 0
state_data['WEIGHT_26'] <- 0

state_data['SMOKE_00'] <- 0
state_data['SMOKE_01'] <- 0
state_data['SMOKE_02'] <- 0
state_data['SMOKE_03'] <- 0
state_data['SMOKE_04'] <- 0                                  
state_data['SMOKE_05'] <- 0
state_data['SMOKE_06'] <- 0
state_data['SMOKE_07'] <- 0
state_data['SMOKE_08'] <- 0
state_data['SMOKE_09'] <- 0
state_data['SMOKE_10'] <- 0
state_data['SMOKE_11'] <- 0
state_data['SMOKE_12'] <- 0
state_data['SMOKE_13'] <- 0
state_data['SMOKE_14'] <- 0
state_data['SMOKE_15'] <- 0
state_data['SMOKE_16'] <- 0
state_data['SMOKE_17'] <- 0
state_data['SMOKE_18'] <- 0
state_data['SMOKE_19'] <- 0
state_data['SMOKE_20'] <- 0
state_data['SMOKE_21'] <- 0
state_data['SMOKE_22'] <- 0
state_data['SMOKE_23'] <- 0
state_data['SMOKE_24'] <- 0
state_data['SMOKE_25'] <- 0
state_data['SMOKE_26'] <- 0

state_data['DRINK_00'] <- 0
state_data['DRINK_01'] <- 0
state_data['DRINK_02'] <- 0
state_data['DRINK_03'] <- 0
state_data['DRINK_04'] <- 0                                  
state_data['DRINK_05'] <- 0
state_data['DRINK_06'] <- 0
state_data['DRINK_07'] <- 0
state_data['DRINK_08'] <- 0
state_data['DRINK_09'] <- 0
state_data['DRINK_10'] <- 0
state_data['DRINK_11'] <- 0
state_data['DRINK_12'] <- 0
state_data['DRINK_13'] <- 0
state_data['DRINK_14'] <- 0
state_data['DRINK_15'] <- 0
state_data['DRINK_16'] <- 0
state_data['DRINK_17'] <- 0
state_data['DRINK_18'] <- 0
state_data['DRINK_19'] <- 0
state_data['DRINK_20'] <- 0
state_data['DRINK_21'] <- 0
state_data['DRINK_22'] <- 0
state_data['DRINK_23'] <- 0
state_data['DRINK_24'] <- 0
state_data['DRINK_25'] <- 0
state_data['DRINK_26'] <- 0

state_data['PDTEST_00'] <- 0
state_data['PDTEST_01'] <- 0
state_data['PDTEST_02'] <- 0
state_data['PDTEST_03'] <- 0
state_data['PDTEST_04'] <- 0                                  
state_data['PDTEST_05'] <- 0
state_data['PDTEST_06'] <- 0
state_data['PDTEST_07'] <- 0
state_data['PDTEST_08'] <- 0
state_data['PDTEST_09'] <- 0
state_data['PDTEST_10'] <- 0
state_data['PDTEST_11'] <- 0
state_data['PDTEST_12'] <- 0
state_data['PDTEST_13'] <- 0
state_data['PDTEST_14'] <- 0
state_data['PDTEST_15'] <- 0
state_data['PDTEST_16'] <- 0
state_data['PDTEST_17'] <- 0
state_data['PDTEST_18'] <- 0
state_data['PDTEST_19'] <- 0
state_data['PDTEST_20'] <- 0
state_data['PDTEST_21'] <- 0
state_data['PDTEST_22'] <- 0
state_data['PDTEST_23'] <- 0
state_data['PDTEST_24'] <- 0
state_data['PDTEST_25'] <- 0
state_data['PDTEST_26'] <- 0

LM_WHOICD_KG_SORTED <- LM_WHOICD_KG[order(LM_WHOICD_KG$chapter),]

substr(LM_WHOICD_KG_SORTED[,1][LM_WHOICD_KG_SORTED$word == 'male'],1,5)
# [1] "CH_00" "CH_02" "CH_04" "CH_05" "CH_08" "CH_11" "CH_14" "CH_16" "CH_17" "CH_20" "CH_21" "CH_22" "CH_24" "CH_26"
# 
LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'male']
# [1] 37 17  1  4  1  1 11 26 30 17 28  1  1  2 
 
state_data$GENDER_00[state_data$X_SEX == 1] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'male'][1]
state_data$GENDER_02[state_data$X_SEX == 1] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'male'][2]
state_data$GENDER_04[state_data$X_SEX == 1] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'male'][3] 
state_data$GENDER_05[state_data$X_SEX == 1] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'male'][4]
state_data$GENDER_08[state_data$X_SEX == 1] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'male'][5] 
state_data$GENDER_11[state_data$X_SEX == 1] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'male'][6]
state_data$GENDER_14[state_data$X_SEX == 1] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'male'][7]
state_data$GENDER_16[state_data$X_SEX == 1] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'male'][8] 
state_data$GENDER_17[state_data$X_SEX == 1] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'male'][9]
state_data$GENDER_20[state_data$X_SEX == 1] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'male'][10]
state_data$GENDER_21[state_data$X_SEX == 1] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'male'][11] 
state_data$GENDER_22[state_data$X_SEX == 1] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'male'][12]
state_data$GENDER_24[state_data$X_SEX == 1] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'male'][13]
state_data$GENDER_26[state_data$X_SEX == 1] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'male'][14] 
  
#substr(LM_WHOICD_KG_SORTED[,1][LM_WHOICD_KG_SORTED$word == 'female'],1,5)
# [1] "CH_00" "CH_01" "CH_02" "CH_04" "CH_05" "CH_13" "CH_14" "CH_15" "CH_16" "CH_17" "CH_18" "CH_20" "CH_21" "CH_22" "CH_24" "CH_26"
#
# [1] 53  3 21  1  4  2  6  1 86 13  5 13 29  4  3  7 

state_data$GENDER_00[state_data$X_SEX == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'female'][1]
state_data$GENDER_01[state_data$X_SEX == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'female'][2]
state_data$GENDER_02[state_data$X_SEX == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'female'][3]
state_data$GENDER_04[state_data$X_SEX == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'female'][4]
state_data$GENDER_05[state_data$X_SEX == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'female'][5]
state_data$GENDER_13[state_data$X_SEX == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'female'][6]
state_data$GENDER_14[state_data$X_SEX == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'female'][7]
state_data$GENDER_15[state_data$X_SEX == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'female'][8]
state_data$GENDER_16[state_data$X_SEX == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'female'][9]
state_data$GENDER_17[state_data$X_SEX == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'female'][10]
state_data$GENDER_18[state_data$X_SEX == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'female'][11]
state_data$GENDER_20[state_data$X_SEX == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'female'][12]
state_data$GENDER_21[state_data$X_SEX == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'female'][13]
state_data$GENDER_22[state_data$X_SEX == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'female'][14]
state_data$GENDER_24[state_data$X_SEX == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'female'][15]
state_data$GENDER_26[state_data$X_SEX == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'female'][16]


state_data["DIABETE4"][is.na(state_data["DIABETE4"])] <- 9

#substr(LM_WHOICD_KG_SORTED[,1][LM_WHOICD_KG_SORTED$word == 'diabetes'],1,5)
# [1] "CH_00" "CH_01" "CH_05" "CH_06" "CH_07" "CH_08" "CH_09" "CH_10" "CH_11" "CH_12" "CH_14" "CH_15" "CH_16" "CH_18" "CH_19" "CH_20" "CH_21" "CH_24"
# 
# LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'diabetes']
# [1] 12  2 89  1  2  9 10  1  2  2 10  2  7 10 20  1  4  2
 
state_data$LABEL_00[state_data$DIABETE4 == 1 | state_data$DIABETE4 == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'diabetes'][1]
state_data$LABEL_01[state_data$DIABETE4 == 1 | state_data$DIABETE4 == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'diabetes'][2]
state_data$LABEL_05[state_data$DIABETE4 == 1 | state_data$DIABETE4 == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'diabetes'][3]
state_data$LABEL_06[state_data$DIABETE4 == 1 | state_data$DIABETE4 == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'diabetes'][4]
state_data$LABEL_07[state_data$DIABETE4 == 1 | state_data$DIABETE4 == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'diabetes'][5]
state_data$LABEL_08[state_data$DIABETE4 == 1 | state_data$DIABETE4 == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'diabetes'][6]
state_data$LABEL_09[state_data$DIABETE4 == 1 | state_data$DIABETE4 == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'diabetes'][7]
state_data$LABEL_10[state_data$DIABETE4 == 1 | state_data$DIABETE4 == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'diabetes'][8]
state_data$LABEL_11[state_data$DIABETE4 == 1 | state_data$DIABETE4 == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'diabetes'][9]
state_data$LABEL_12[state_data$DIABETE4 == 1 | state_data$DIABETE4 == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'diabetes'][10]
state_data$LABEL_14[state_data$DIABETE4 == 1 | state_data$DIABETE4 == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'diabetes'][11]
state_data$LABEL_15[state_data$DIABETE4 == 1 | state_data$DIABETE4 == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'diabetes'][12]
state_data$LABEL_16[state_data$DIABETE4 == 1 | state_data$DIABETE4 == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'diabetes'][13]
state_data$LABEL_18[state_data$DIABETE4 == 1 | state_data$DIABETE4 == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'diabetes'][14]
state_data$LABEL_19[state_data$DIABETE4 == 1 | state_data$DIABETE4 == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'diabetes'][15]
state_data$LABEL_20[state_data$DIABETE4 == 1 | state_data$DIABETE4 == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'diabetes'][16]
state_data$LABEL_21[state_data$DIABETE4 == 1 | state_data$DIABETE4 == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'diabetes'][17]
state_data$LABEL_24[state_data$DIABETE4 == 1 | state_data$DIABETE4 == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'diabetes'][18]


#substr(LM_WHOICD_KG_SORTED[,1][LM_WHOICD_KG_SORTED$word == 'cancer'],1,5)
# [1] "CH_00" "CH_01" "CH_02" "CH_03" "CH_04" "CH_05" "CH_06" "CH_08" "CH_09" "CH_11" "CH_13" "CH_14" "CH_15" "CH_16" "CH_20" "CH_21" "CH_24"
# 
#LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'cancer']
# [1]  1  3 39  2  2  6  3 10  3  1  2  5  2  7  3 36  1
 

state_data["CNCRDIFF"][is.na(state_data["CNCRDIFF"])] <- 9

state_data$LABEL_00[state_data$CNCRDIFF == 1 | state_data$CNCRDIFF == 2 | state_data$CNCRDIFF == 3] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'cancer'][1]
state_data$LABEL_01[state_data$CNCRDIFF == 1 | state_data$CNCRDIFF == 2 | state_data$CNCRDIFF == 3] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'cancer'][2]
state_data$LABEL_05[state_data$CNCRDIFF == 1 | state_data$CNCRDIFF == 2 | state_data$CNCRDIFF == 3] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'cancer'][3]
state_data$LABEL_06[state_data$CNCRDIFF == 1 | state_data$CNCRDIFF == 2 | state_data$CNCRDIFF == 3] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'cancer'][4]
state_data$LABEL_07[state_data$CNCRDIFF == 1 | state_data$CNCRDIFF == 2 | state_data$CNCRDIFF == 3] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'cancer'][5]
state_data$LABEL_08[state_data$CNCRDIFF == 1 | state_data$CNCRDIFF == 2 | state_data$CNCRDIFF == 3] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'cancer'][6]
state_data$LABEL_09[state_data$CNCRDIFF == 1 | state_data$CNCRDIFF == 2 | state_data$CNCRDIFF == 3] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'cancer'][7]
state_data$LABEL_10[state_data$CNCRDIFF == 1 | state_data$CNCRDIFF == 2 | state_data$CNCRDIFF == 3] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'cancer'][8]
state_data$LABEL_11[state_data$CNCRDIFF == 1 | state_data$CNCRDIFF == 2 | state_data$CNCRDIFF == 3] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'cancer'][9]
state_data$LABEL_12[state_data$CNCRDIFF == 1 | state_data$CNCRDIFF == 2 | state_data$CNCRDIFF == 3] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'cancer'][10]
state_data$LABEL_14[state_data$CNCRDIFF == 1 | state_data$CNCRDIFF == 2 | state_data$CNCRDIFF == 3] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'cancer'][11]
state_data$LABEL_15[state_data$CNCRDIFF == 1 | state_data$CNCRDIFF == 2 | state_data$CNCRDIFF == 3] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'cancer'][12]
state_data$LABEL_16[state_data$CNCRDIFF == 1 | state_data$CNCRDIFF == 2 | state_data$CNCRDIFF == 3] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'cancer'][13]
state_data$LABEL_18[state_data$CNCRDIFF == 1 | state_data$CNCRDIFF == 2 | state_data$CNCRDIFF == 3] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'cancer'][14]
state_data$LABEL_19[state_data$CNCRDIFF == 1 | state_data$CNCRDIFF == 2 | state_data$CNCRDIFF == 3] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'cancer'][15]
state_data$LABEL_20[state_data$CNCRDIFF == 1 | state_data$CNCRDIFF == 2 | state_data$CNCRDIFF == 3] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'cancer'][16]
state_data$LABEL_21[state_data$CNCRDIFF == 1 | state_data$CNCRDIFF == 2 | state_data$CNCRDIFF == 3] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'cancer'][17]
state_data$LABEL_24[state_data$CNCRDIFF == 1 | state_data$CNCRDIFF == 2 | state_data$CNCRDIFF == 3] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'cancer'][18]
																									

state_data["X_BMI5CAT"][is.na(state_data["X_BMI5CAT"])] <- 5

#substr(LM_WHOICD_KG_SORTED[,1][LM_WHOICD_KG_SORTED$word == 'obese'],1,5)
#[1] "CH_09" "CH_14" "CH_21"
#
#LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'obese']
#[1] 1 2 1

state_data$WEIGHT_09[state_data$X_BMI5CAT == 4] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'obese'][1]
state_data$WEIGHT_14[state_data$X_BMI5CAT == 4] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'obese'][2]
state_data$WEIGHT_21[state_data$X_BMI5CAT == 4] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'obese'][3]


#substr(LM_WHOICD_KG_SORTED[,1][LM_WHOICD_KG_SORTED$word == 'overweight'],1,5)
#[1] "CH_00" "CH_05" "CH_08" "CH_19"
#
#LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'overweight']
#[1]  6 25  1  1

state_data$WEIGHT_00[state_data$X_BMI5CAT == 4] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'overweight'][1]
state_data$WEIGHT_05[state_data$X_BMI5CAT == 4] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'overweight'][2]
state_data$WEIGHT_08[state_data$X_BMI5CAT == 4] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'overweight'][3]
state_data$WEIGHT_19[state_data$X_BMI5CAT == 4] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'overweight'][4]


#SMOKE100
#SMOKDAY2


state_data["SMOKDAY2"][is.na(state_data["SMOKDAY2"])] <- 7

#substr(LM_WHOICD_KG_SORTED[,1][LM_WHOICD_KG_SORTED$word == 'smoked' | LM_WHOICD_KG_SORTED$word == 'cigarettes' | LM_WHOICD_KG_SORTED$word == 'tabacco'],1,5)
##[1] "CH_06" "CH_06"
#
#substr(LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'smoked' | LM_WHOICD_KG_SORTED$word == 'cigarettes' | LM_WHOICD_KG_SORTED$word == 'tabacco'],1,5)
##[1] "3" "2"

state_data$SMOKE_06[state_data$SMOKDAY2 == 1 | state_data$SMOKDAY2 == 2] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'smoked' | LM_WHOICD_KG_SORTED$word == 'cigarettes' | LM_WHOICD_KG_SORTED$word == 'tabacco'][1] + LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'smoked' | LM_WHOICD_KG_SORTED$word == 'cigarettes' | LM_WHOICD_KG_SORTED$word == 'tabacco'][2]


#load the BRFSS survey into memory, apply geograhpical state filter
#setwd('C:\\Users\\mdjaw\\OneDrive\\Documents\\Markian Training\\Research_USQ\\USCenterDiseaseControlPrevention\\LLCP2020XPT')
#data <- read.csv('LLCP2020XPT_LINEAR.csv')
#state_data <- subset(data, X_STATE == 4 | X_STATE == 9 | X_STATE == 10 | X_STATE == 13 | X_STATE == 15 | X_STATE == 18 | X_STATE == 22 | X_STATE == 25 | X_STATE == 26 | X_STATE == 28 | X_STATE == 29 | X_STATE == 30 | X_STATE == 34 | X_STATE == 35 | X_STATE == 37 | X_STATE == 44 | X_STATE == 46 | X_STATE == 49 | X_STATE == 50 | X_STATE == 51 | X_STATE == 55 | X_STATE == 56 | X_STATE == 66 | X_STATE == 31 | X_STATE == 16 | X_STATE == 19 | X_STATE == 2 | X_STATE == 31 | X_STATE == 1 | X_STATE == 40 | X_STATE == 39) 

#state_data = subset(state_data, select = -c(X) )

state_data["PDIABTST"][is.na(state_data["PDIABTST"])] <- 7

#substr(LM_WHOICD_KG_SORTED[,1][LM_WHOICD_KG_SORTED$word == 'sugar'],1,5)
#[1] "CH_05" "CH_09" "CH_12" "CH_21"
#
#LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'sugar']
#[1] 3 1 1 1

state_data$PDTEST_05[state_data$PDIABTST == 1] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'sugar'][1]
state_data$PDTEST_09[state_data$PDIABTST == 1] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'sugar'][2]
state_data$PDTEST_12[state_data$PDIABTST == 1] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'sugar'][3]
state_data$PDTEST_21[state_data$PDIABTST == 1] <- LM_WHOICD_KG_SORTED[,3][LM_WHOICD_KG_SORTED$word == 'sugar'][4]



state_data = subset(state_data, select = c(CNCRTYP1,DIABETE4,GENDER_00,GENDER_01,GENDER_02,GENDER_03,GENDER_04,GENDER_05,GENDER_06,GENDER_07,GENDER_08,GENDER_09,GENDER_10,GENDER_11,GENDER_12,GENDER_13,GENDER_14,GENDER_15,GENDER_16,GENDER_17,GENDER_18,GENDER_19,GENDER_20,GENDER_21,GENDER_22,GENDER_23,GENDER_24,GENDER_25,GENDER_26,LABEL_00,LABEL_01,LABEL_02,LABEL_03,LABEL_04,LABEL_05,LABEL_06,LABEL_07,LABEL_08,LABEL_09,LABEL_10,LABEL_11,LABEL_12,LABEL_13,LABEL_14,LABEL_15,LABEL_16,LABEL_17,LABEL_18,LABEL_19,LABEL_20,LABEL_21,LABEL_22,LABEL_23,LABEL_24,LABEL_25,LABEL_26,WEIGHT_00,WEIGHT_01,WEIGHT_02,WEIGHT_03,WEIGHT_04,WEIGHT_05,WEIGHT_06,WEIGHT_07,WEIGHT_08,WEIGHT_09,WEIGHT_10,WEIGHT_11,WEIGHT_12,WEIGHT_13,WEIGHT_14,WEIGHT_15,WEIGHT_16,WEIGHT_17,WEIGHT_18,WEIGHT_19,WEIGHT_20,WEIGHT_21,WEIGHT_22,WEIGHT_23,WEIGHT_24,WEIGHT_25,WEIGHT_26,SMOKE_00,SMOKE_01,SMOKE_02,SMOKE_03,SMOKE_04,SMOKE_05,SMOKE_06,SMOKE_07,SMOKE_08,SMOKE_09,SMOKE_10,SMOKE_11,SMOKE_12,SMOKE_13,SMOKE_14,SMOKE_15,SMOKE_16,SMOKE_17,SMOKE_18,SMOKE_19,SMOKE_20,SMOKE_21,SMOKE_22,SMOKE_23,SMOKE_24,SMOKE_25,SMOKE_26,PDTEST_01,PDTEST_02,PDTEST_03,PDTEST_04,PDTEST_05,PDTEST_06,PDTEST_07,PDTEST_08,PDTEST_09,PDTEST_10,PDTEST_11,PDTEST_12,PDTEST_13,PDTEST_14,PDTEST_15,PDTEST_16,PDTEST_17,PDTEST_18,PDTEST_19,PDTEST_20,PDTEST_21,PDTEST_22,PDTEST_23,PDTEST_24,PDTEST_25,PDTEST_26))

state_data[is.na(state_data)] <- 0

#convert diabetes and cancer types into a multi-label response variable
state_data$DIABETE4[state_data$DIABETE4 == 1] <- 1 
state_data$DIABETE4[state_data$DIABETE4 == 2] <- 1
state_data$DIABETE4[state_data$DIABETE4 == 3] <- 0
state_data$DIABETE4[state_data$DIABETE4 == 4] <- 0
state_data$DIABETE4[state_data$DIABETE4 == 7] <- 0
state_data$DIABETE4[state_data$DIABETE4 == 9] <- 0
state_data$DIABETE4[is.na(state_data$DIABETE4)] <- 0
state_data$CNCRTYP1[is.na(state_data$CNCRTYP1)] <- 0
state_data$CNCRTYP1[state_data$CNCRTYP1 >= 1] <- 1

write.csv(state_data, "LLCP2021XPT_LINEAR_WHOICD_V2B.csv")
