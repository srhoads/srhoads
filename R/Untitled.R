mtcars$ID <- 1:nrow(mtcars)
library(tidyverse)
# women$ID <- 1:nrow(women)
iris$ID <- 1:nrow(iris)
MASS::Animals

dim(professor <- MASS::painters %>% 
      sample_n(., nrow(.)) %>% 
      mutate(ID = nrow(.)))
dim(course <- iris %>% #sample_n(., 54) %>% 
      mutate(ID = nrow(.)))
dim(department <- MASS::oats %>% #sample_n(., 54) %>% 
      mutate(ID = nrow(.)))
dim(schedule <- MASS::Aids2 %>% #sample_n(., 54) %>% 
      mutate(ID = nrow(.)))

total <- list.files('~/GitHub/slu', pattern='5.*csv', full.names = T) %>%
  read.csv() %>%
  distinct() %>%
  filter(!is.na(Employee.ID), !is.na(First.Name) | !is.na(Last.Name), !is.na(Job.Code) | !is.na(New.Job.Code), !is.na(Dept.Code) | !is.na(New.Dept.Code)) %>%
  select(matches('.id|code|salary|date|t.name')) %>%
  as_tibble() #%>% summary_factor()

professor <- sample_n(total, 3000) %>% 
  transmute(ID = Employee.ID,
            NAME = substr(gsub('a|t|l', '_', paste0(Last.Name, First.Name)), start=1, stop=25),
            DEPARTMENT_ID = Dept.Code,
            SALARY = Annual.Salary) %>%
  filter(!is.na(DEPARTMENT_ID), !is.na(ID))

course <- sample_n(total, 3000) %>% 
  transmute(ID = as.numeric(Job.Code),
            NAME = as.character(Job.Code),
            DEPARTMENT_ID = gsub('8|0', '--', Dept.Code)) %>% 
  filter(!is.na(DEPARTMENT_ID), !is.na(ID))

department <- sample_n(total, 3000) %>% 
  transmute(ID = Dept.Code,
            NAME = paste0('deptnameblah', ID)) %>% 
  filter(!is.na(ID))

schedule <- sample_n(total, 3000) %>% 
  transmute(PROFESSOR_ID = Employee.ID,
            COURSE_ID = as.numeric(Job.Code),
            SEMESTER = round(sqrt(sqrt(as.numeric(as.factor(Term.Date))))),
            YEAR = lubridate::year(as.Date(Term.Date, origin = "1899-12-30"))
  ) %>%
  na.omit()


# head(professor <- list.files('~/GitHub/slu', pattern='workforce.*csv', full.names = T)[1] %>%
#        read.csv() %>%
#         transmute(ID = Employee.ID,
#                NAME = substr(gsub('a|t|l', '_', paste0(Last.Name, First.Name)), start=1, stop=25),
#                DEPARTMENT_ID = Dept.Code,
#                SALARY = Annual.Salary) %>%
#        filter(!is.na(DEPARTMENT_ID), !is.na(ID))
# )
# 
# course2 <- rev(list.files('~/GitHub/slu', pattern='appl.*csv', full.names = T))[1] %>% read.csv() %>% 
#   transmute(ID = as.numeric(Job.Code),
#             NAME = as.character(Job.Code),
#             DEPARTMENT_ID = gsub('8|0', '--', Dept.Code)) %>% 
#   filter(!is.na(DEPARTMENT_ID), !is.na(ID))
# 
# head(course <- list.files('~/GitHub/slu', pattern='promo.*csv', full.names = T)[1] %>%
#        read.csv() %>%
#        transmute(ID = as.numeric(New.Job.Code),
#                  NAME = as.character(New.Job.Code),
#                  DEPARTMENT_ID = gsub('8|0', '--', New.Dept.Code),
#                  CREDITS = round(sqrt(sqrt(as.numeric(as.factor(Promotion.Date)))))
#        ) %>% 
#        filter(!is.na(DEPARTMENT_ID), !is.na(ID)) %>%
#        full_join(., course2, type='all')
#        
# )
# head(department <- list.files('~/GitHub/slu', pattern='newhire.*csv', full.names = T)[1] %>%
#         read.csv() %>%
#         transmute(ID = Dept.Code,
#                   NAME = paste0('deptnameblah', ID)) %>% 
#         filter(!is.na(ID))
#       )
# head(schedule <- list.files('~/GitHub/slu', pattern='termin.*csv', full.names = T)[1] %>%
#        read.csv() %>%
#         transmute(PROFESSOR_ID = Employee.ID,
#                   COURSE_ID = as.numeric(Job.Code),
#                   SEMESTER = round(sqrt(sqrt(as.numeric(as.factor(Term.Date))))),
#                   YEAR = lubridate::year(as.Date(Term.Date, origin = "1899-12-30"))
#                   ) %>%
#        na.omit()
#      )


library(magrittr)
PROFESSOR <- professor %>% 
  rename(PROFESSOR_ID = ID,
         PROFESSOR_NAME = NAME) %>%
  dplyr::distinct() %>% 
  na.omit() %>%
  as_tibble()

DEPARTMENT <- department %>% 
  rename(DEPARTMENT_ID = ID,
         DEPARTMENT_NAME = NAME) %>%
  dplyr::distinct() %>% na.omit() %>%
  as_tibble()

COURSE <- course %>% 
  rename(COURSE_ID = ID,
         DEPARTMENT_ID_COURSE = DEPARTMENT_ID,
         COURSE_NAME = NAME) %>%
  dplyr::distinct() %>% na.omit() %>%
  as_tibble()

SCHEDULE <- schedule %>% 
  dplyr::distinct() %>% na.omit() %>%
  as_tibble()

sc <- full_join(SCHEDULE, COURSE) %>% na.omit() # joining data by COURSE_ID
scp <- full_join(sc, PROFESSOR) %>% na.omit() # joining data by PROFESSOR_ID
scpd <- full_join(scp, DEPARTMENT) %>% na.omit() # joining data by DEPARTMENT_ID

scp %>% filter(DEPARTMENT_ID != DEPARTMENT_ID_COURSE) %>%
  select(PROFESSOR_NAME, COURSE_NAME) %>% distinct()







tables <- list(PROFESSOR, 
               DEPARTMENT, 
               COURSE, 
               SCHEDULE)


PROFESSOR %>% names() # "PROFESSOR_ID"   "PROFESSOR_NAME" "DEPARTMENT_ID"  "SALARY"
DEPARTMENT %>% names() # "DEPARTMENT_ID"   "DEPARTMENT_NAME"
COURSE %>% names() # "COURSE_ID"            "COURSE_NAME"          "DEPARTMENT_ID_COURSE" "CREDITS"
SCHEDULE %>% names() # "PROFESSOR_ID" "COURSE_ID"    "SEMESTER"     "YEAR"

joined <- plyr::join_all(tables, type='full') %>% as_tibble()

joined %>% 
  # mutate_all(., as.character) %>%
  filter(DEPARTMENT_ID != DEPARTMENT_ID_COURSE)

joined %>% 
  mutate_all(., as.character) %>%
  filter(!is.na(DEPARTMENT_ID_COURSE) | !is.na(DEPARTMENT_ID))






# dfincasef <- dfincaser <- dfsampler <- function(which='long', tibble=F){
#   if(which=='short') dfincase <- data.frame(name=c('charlene teters', 'sandra sunrising osawa'), 
#                                             firstname=c('charlene', 'sandra sunrising'), 
#                                             lastname=c('teters', 'osawa'), 
#                                             gender=c('female', 'female'), 
#                                             race=c('american indian or alaska native', 'american indian or alaska native'), 
#                                             stringsAsFactors = F)
#   if(which=='long') dfincase <- data.frame(gender = c("male", "female", "female", 
#                                                      "female", "female", "female", 
#                                                      "male", "female", "male", 
#                                                      "male", "male", "male", 
#                                                      "female", "female", "female",
#                                                      "female", "female", "female", 
#                                                      "female", "female", "male",
#                                                      "male"), 
#                                           race = c("white", "white", 
#                                                    "native hawaiian or other pacific islander", 
#                                                    "black or african american", 
#                                                    "american indian or alaska native", 
#                                                    "hispanic or latino", 
#                                                    "asian",
#                                                    "black or african american", 
#                                                    "american indian or alaska native", 
#                                                    "white", 
#                                                    "two or more races", "two or more races", 
#                                                    "american indian or alaska native",
#                                                    "american indian or alaska native", 
#                                                    "native hawaiian or other pacific islander",
#                                                    "hispanic or latino", 
#                                                    "hispanic or latino",
#                                                    "white", "white", 
#                                                    "hispanic or latino", 
#                                                    "hispanic or latino",
#                                                    "native hawaiian or other pacific islander"), 
#                                           name = c("jason o'rawe", "samantha karlaina rhoads", "keisha castle-hughes", 
#                                                    "oprah winfrey", "shoni schimmel", "alexandria ocasio-cortez", 
#                                                    "kendrick kang-joh jeong","purdie greenaway, valerie", "silverheels, jay", 
#                                                    "jadrian charles guy", "jordan peele", "keegan-michael key", 
#                                                    "davids, sharice", "deb haaland", "dinah jane hansen",
#                                                    "ochoa, ellen", "sonia sotomayor", "ruth bader ginsburg", 
#                                                    "natalia nikolaevna zakharenko", "kahlo, frida", "diego rivera",
#                                                    "momoa, jason"), stringsAsFactors = F)
#   if(tibble) dfincase <- as.tibble(dfincase)
#   dfincase
# }


dfincasef()



library(ggnet)
library(GGally)
library(network)
library(sna)
library(ggplot2)
library(magrittr)
library(visNetwork)
library(imager)


file <- system.file('samnet/sam1.jpg',package='imager')
list.files('samnet', full.names=T)
#system.file gives the full path for a file that ships with a R package
#if you already have the full path to the file you want to load just run:
#im <- load.image("/somedirectory/myfile.png")
im <- load.image('samnet/sam1.jpg')

plot(im) #Parrots!

image <- "https://res.cloudinary.com/campus-job/image/upload/t_student-public-page/v1/profile_pictures/ACWf2ittID_20170205.jpg"
image1 <- "https://scontent-lga3-1.cdninstagram.com/vp/9e8ff0e799f055c7f771997c4d00af28/5CF580B5/t51.2885-19/s320x320/51287988_551040821971916_4613039624615362560_n.jpg?_nc_ht=scontent-lga3-1.cdninstagram.com"
image2 <- "https://pbs.twimg.com/profile_images/1091560298316787712/SlXBNHNj_400x400.jpg"
image3 <- "https://scontent-lga3-1.cdninstagram.com/vp/41564eb5635836b20bba9da908a73673/5CE37A98/t51.2885-19/s320x320/50725988_631941440599074_6549096231795163136_n.jpg?_nc_ht=scontent-lga3-1.cdninstagram.com"
image4 <- "https://scontent-lga3-1.cdninstagram.com/vp/7a61c90e41de8b440a931a557404bcdd/5CF8DDEF/t51.2885-19/s320x320/49998204_337816310396180_7327404482617147392_n.jpg?_nc_ht=scontent-lga3-1.cdninstagram.com"
image5 <- "https://scontent-lga3-1.cdninstagram.com/vp/11d2030618f9b64238e9ced2ba8aca71/5CFD2FD8/t51.2885-19/s320x320/51447075_785824928450613_2546429381870354432_n.jpg?_nc_ht=scontent-lga3-1.cdninstagram.com"
nodes <- data.frame(id = 1:7, 
                    shape = c("circularImage"),
                    image = image,#paste0(image, 1:4),
                    label = "I'm Sam",
                    size = 10)

edges <- data.frame(from = c(2,4,1,5,8,7,3,3,6,3,5,2,1), 
                    to = c(7,1,4,3,2,1,1,2,4,2,6,4,5))

edges <- data.frame(from = c(1,2,4,1,5,8,7,3,6,3,5,2,1), 
                    to = c(3,1,1,7,1,4,3,2,4,2,6,4,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)



nodes <- data.frame(id = 1:6, 
                    shape = c("circularImage"),
                    image = c(image5, image2, image3, image4, image, image1),
                    label = c("Jo & Is", "Sam!!", "Sam!?", "Sam", "Sam!", "Sam..."),
                    size = 30)
edges <- data.frame(from = c(1,2,3,4,5,6,0,  1,2,3,4,5,6,0, 1,2,3,4,5,6,0, 1,2,3,4,5,6,0, 1,2,3,4,5,6,0, 1,2,3,4,5,6,0, 1,2,3,4,5,6,0), 
                    to =   c(0,1,1,1,1,1,1,  2,0,0,0,2,0,0, 0,0,0,0,0,3,0, 0,0,0,0,0,0,0, 0,0,0,5,0,0,0, 0,0,0,0,0,0,0, 0,0,0,0,0,0,0),
                    size = 2,
                    length = .1)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))




nodes <- data.frame(id = 1:5, 
                    shape = c("circularImage"),
                    image = c(image5, image2, image3, image4, image),
                    label = c("Jo & Is", "Sam!!", "Sam!?", "Sam", "Sam!"),
                    size = 30)
edges <- data.frame(from = c(1,2,3,4,5,6,0,  1,2,3,4,5,6,0, 1,2,3,4,5,6,0, 1,2,3,4,5,6,0, 1,2,3,4,5,6,0, 1,2,3,4,5,6,0, 1,2,3,4,5,6,0), 
                    to =   c(0,1,1,1,1,1,1,  2,0,0,0,2,0,0, 0,0,0,0,0,3,0, 0,0,0,0,0,0,0, 0,0,0,3,0,0,0, 0,0,0,0,0,0,0, 0,0,0,0,0,0,0),
                    size = 2,
                    length = .1)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))


nodes <- data.frame(id = 1:5, 
                    shape = c("circularImage"),
                    image = c(image5, image2, image3, image, image4),
                    label = c("Jo & Is", "Sam!!", "Sam!?", "Sam", "Sam!"),
                    size = 60,
                    size = 90)
edges <- data.frame(from = c(1,2,3,4,5,6,0,8,2,  1,2,4,5,6,0, 1,2,3,4,5,6,0, 1,2,3,4,6,0, 2,3,4,6,0, 1,2,4,5,6,0, 1,2,4,5,6,0), 
                    to =   c(0,1,1,1,1,0,1,4,8,  2,0,0,2,0,0, 0,0,0,0,0,3,0, 0,0,5,0,0,0, 0,0,3,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0),
                    arrows = "from",
                    length = 280,
                    size = 350,
                    dashes = c(TRUE))                                    # dashes
(visNetwork(nodes, edges, width = "100%") %>% 
    visEdges(arrows = 'from', scaling = list(min = .00002, max = .2)) %>%
    visNodes(shapeProperties = list(useBorderWithImage = TRUE))-> keeper)






nodes <- data.frame(id = 1:6, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam",
                    size = 30)
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,1,2,1,6,5,1), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5,1,6,4))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))




nodes <- data.frame(id = 1:4, 
                    shape = c("circularImage"),
                    image = "https://pbs.twimg.com/profile_images/920140713161043968/un91zBzJ_400x400.jpg",
                    label = "I'm Sam",
                    size = 40)
edges <- data.frame(from = c(1,4,3,2,1,3,7,7,9,7,7,2), 
                    to = c(3,2,3,4,4,4,7,4,2,5,1,4),
                    size = 2,
                    width = 2,
                    length = 2,
                    height = 2)
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))













nodes <- data.frame(id = 1:6, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam",
                    size = 30)


visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,6,4,5,8,7,3,6,3,1,2,1), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,1,2,1,6), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5,1))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,1,2,1,6,5), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5,1,1))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,1,2,1,6,5), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5,1,2))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,1,2,1,6,5), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5,1,6))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,1,2,1,6,5,6), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5,1,6,3))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,1,2,1,6,5,6), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5,1,6,4))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,1,2,1,6,5,1), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5,1,6,4))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9), 
                    to = c(3,1,7,1,4,3,2,4,2))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))


edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9), 
                    to = c(3,3,7,1,4,3,2,4,2))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9), 
                    to = c(3,3,4,1,4,3,2,4,2))
(visNetwork(nodes, edges, width = "100%") %>% 
    visNodes(shapeProperties = list(useBorderWithImage = TRUE)) -> keeper1)

edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1), 
                    to = c(3,3,4,1,4,3,2,4,2,2))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1), 
                    to = c(3,3,4,1,4,3,2,4,2,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1), 
                    to = c(3,3,4,1,4,3,2,4,2,5))
visNetwork(nodes, edges, width = "200%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1), 
                    to = c(3,3,4,1,4,3,2,4,2,5))
visNetwork(nodes, edges, width = "200%", height = "200%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
visNetwork(nodes, edges, width = "200", height = "200") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))


visNetwork(nodes, edges, width = "2000", height = "2000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
visNetwork(nodes, edges, width = "2", height = "2") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
visNetwork(nodes, edges, width = "20", height = "20") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
visNetwork(nodes, edges, width = "200", height = "200") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
visNetwork(nodes, edges, width = "900", height = "900") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
visNetwork(nodes, edges, width = "110%", height = "110%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
visNetwork(nodes, edges, width = "190%", height = "190%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
visNetwork(nodes, edges, width = "1500", height = "1500") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1), 
                    to = c(3,3,4,1,4,3,2,4,2,5))
visNetwork(nodes, edges, width = "1500", height = "1500") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
nodes <- data.frame(id = 1:4, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam",
                    size = 40)
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1), 
                    to = c(3,3,4,1,4,3,2,4,2,5))
visNetwork(nodes, edges, width = "1500", height = "1500") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1,4), 
                    to = c(3,3,4,1,4,3,2,4,2,5,1))
visNetwork(nodes, edges, width = "1500", height = "1500") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1,4,1), 
                    to = c(3,3,4,1,4,3,2,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1,4,1,2), 
                    to = c(3,3,4,1,4,3,2,4,2,5,1,4,3))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1,4,1,2), 
                    to = c(3,3,4,1,4,3,2,4,2,5,1,4,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1,4,2), 
                    to = c(3,3,4,1,4,3,2,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,6,7,8,9,1,4,2), 
                    to = c(3,4,1,4,3,2,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1,4,2), 
                    to = c(3,3,4,1,4,3,2,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,2,5,6,7,8,9,1,4,2), 
                    to = c(3,3,4,1,4,3,2,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,7,3,2,5,6,7,8,9,1,4,2), 
                    to = c(3,3,4,1,4,3,2,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,7,3,2,5,6,7,8,9,7,4,2), 
                    to = c(3,3,4,1,4,3,2,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,7,3,2,6,6,7,7,9,7,7,2), 
                    to = c(3,3,4,1,4,3,2,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
nodes <- data.frame(id = 1:4, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam",
                    size = 40)
edges <- data.frame(from = c(1,7,3,2,6,6,7,7,9,7,7,2), 
                    to = c(1,2,3,4,5,6,7,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,7,3,2,6,6,7,7,9,7,7,2), 
                    to = c(3,2,3,4,5,6,7,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,4,3,2,6,6,7,7,9,7,7,2), 
                    to = c(3,2,3,4,5,6,7,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,4,3,2,1,6,7,7,9,7,7,2), 
                    to = c(3,2,3,4,4,6,7,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,4,3,2,1,3,7,7,9,7,7,2), 
                    to = c(3,2,3,4,4,4,7,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
nodes <- data.frame(id = 1:4, 
                    shape = c("circularImage"),
                    image = "https://pbs.twimg.com/profile_images/920140713161043968/un91zBzJ_400x400.jpg",
                    label = "I'm Sam",
                    size = 40)
edges <- data.frame(from = c(1,4,3,2,1,3,7,7,9,7,7,2), 
                    to = c(3,2,3,4,4,4,7,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
nodes <- data.frame(id = 1:4, 
                    shape = c("circularImage"),
                    image = "https://pbs.twimg.com/profile_images/920140713161043968/un91zBzJ_400x400.jpg",
                    label = "I'm Sam",
                    size = 40)
edges <- data.frame(from = c(1,4,3,2,1,3,7,7,9,7,7,2), 
                    to = c(3,2,3,4,4,4,7,4,2,5,1,4),
                    size = 2)
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
nodes <- data.frame(id = 1:6, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam",
                    size = 30)
edges <- data.frame(from = c(1,2,4,1,5,8,7,3,6,3,5,2,1), 
                    to = c(3,1,1,7,1,4,3,2,4,2,6,4,5),
                    size = 2)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
nodes <- data.frame(id = 1:6, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam",
                    size = 30)
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,1,2,1,6,5,1), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5,1,6,4))
(visNetwork(nodes, edges, width = "100%") %>% 
    visNodes(shapeProperties = list(useBorderWithImage = TRUE))-> keeper2)
nodes <- data.frame(id = 1:7, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam",
                    size = 10)
edges <- data.frame(from = c(2,4,1,5,8,7,3,3,6,3,5,2,1), 
                    to = c(7,1,4,3,2,1,1,2,4,2,6,4,5))
edges <- data.frame(from = c(1,2,4,1,5,8,7,3,6,3,5,2,1), 
                    to = c(3,1,1,7,1,4,3,2,4,2,6,4,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
nodes <- data.frame(id = 1:6, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam",
                    size = 30)
edges <- data.frame(from = c(1,2,4,1,5,8,7,3,6,3,5,2,1), 
                    to = c(3,1,1,7,1,4,3,2,4,2,6,4,5),
                    size = 2)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
nodes <- data.frame(id = 1:6, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam",
                    size = 30)
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,1,2,1,6,5,1), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5,1,6,4))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
nodes <- data.frame(id = 1:5, 
                    shape = c("circularImage"),
                    image = c(image5, image2, image3, image4, image),
                    label = c("Sam!", "Sam!!", "Sam!?", "Sam", "Jo & Is"),
                    size = 30)
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,1,2,1,6,5,1), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5,1,6,4))























edges <- data.frame(from = c(2,4,1,5,8,7,3,3, 2,1), to = c(7,1, 4, 3, 2, 1,1,2,4,2))
library(visNetwork)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
nodes <- data.frame(id = 1:6, 
                    shape = c("image", "circularImage"),
                    image = image,
                    label = "I'm Sam")
edges <- data.frame(from = c(2,4,1,5,8,7,3,3, 2,1), to = c(7,1, 4, 3, 2, 1,1,2,4,2))
library(visNetwork)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
edges <- data.frame(from = c(2,4,1,5,8,7,3,3, 6, 3,5,2,1), to = c(7,1, 4, 3, 2, 1,1,2,4,2,6,4,5))
library(visNetwork)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
visNetwork(nodes, edges, width = "80%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
nodes <- data.frame(id = 1:6, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam")
edges <- data.frame(from = c(2,4,1,5,8,7,3,3, 6, 3,5,2,1), to = c(7,1, 4, 3, 2, 1,1,2,4,2,6,4,5))
library(visNetwork)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
nodes <- data.frame(id = 1:6, 
                    shape = c(1, 2, 3, 4, 5,"circularImage"),
                    image = image,
                    label = "I'm Sam")
edges <- data.frame(from = c(2,4,1,5,8,7,3,3, 6, 3,5,2,1), to = c(7,1, 4, 3, 2, 1,1,2,4,2,6,4,5))
library(visNetwork)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
nodes <- data.frame(id = 1:6, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam")
edges <- data.frame(from = c(2,4,1,5,8,7,3,3, 6, 3,5,2,1), to = c(7,1, 4, 3, 2, 1,1,2,4,2,6,4,5))
library(visNetwork)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
edges <- data.frame(from = c(2,4,1,5,8,7,3,3, 6, 3,5,2), to = c(7,1, 4, 3, 2, 1,1,2,4,2,6,4))
library(visNetwork)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
edges <- data.frame(from = c(2,8,7,3,3, 6, 3,5,2,1), to = c(7, 2, 1,1,2,4,2,6,4,5))
library(visNetwork)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
edges <- data.frame(from = c(2,4,1,5,8, 6, 3,5,2,1), to = c(7,1, 4, 3, 2,4,2,6,4,5))
library(visNetwork)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
edges <- data.frame(from = c(2,4,1,5,8,7,3,3, 6, 3), to = c(7,1, 4, 3, 2,4,2,6,4,5))
library(visNetwork)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
edges <- data.frame(from = c(2,4,1,5,8,7,3,3, 6, 3,1), to = c(4, 3, 2, 1,1,2,4,2,6,4,5))
library(visNetwork)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
edges <- data.frame(from = c(2,4,1,5,8,7,3,3, 6, 3,5), to = c( 4, 3, 2, 1,1,2,4,2,6,4,5))
library(visNetwork)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
edges <- data.frame(from = c(2,4,1,5,8,7,3,3, 6, 3,5,2,1), to = c(7,1, 4, 3, 2, 1,1,2,4,2,6,4,5))
library(visNetwork)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
edges <- data.frame(from = c(2,4,1,5,8,7,3,3,6,3,5,1), 
                    to = c(7,1,4,3,2,1,1,2,4,2,6,5))
library(visNetwork)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
edges <- data.frame(from = c(2,4,1,5,8,7,3,6,3,5,2,1), 
                    to = c(7,1,4,3,2,1,2,4,2,6,4,5))
library(visNetwork)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
edges <- data.frame(from = c(11,1,1,2,4,1,5,8,7,3,6,3,5,2,1), 
                    to = c(1,1,1,7,1,4,3,2,1,2,4,2,6,4,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)


edges <- data.frame(from = c(1,2,4,1,5,8,7,3,6,3,5,2,1), 
                    to = c(1,1,1,7,1,4,3,2,4,2,6,4,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
edges <- data.frame(from = c(2,4,1,5,8,7,3,3,6,3,5,2,1), 
                    to = c(7,1,4,3,2,1,1,2,4,2,6,4,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
nodes <- data.frame(id = 1:7, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam")
edges <- data.frame(from = c(2,4,1,5,8,7,3,3,6,3,5,2,1), 
                    to = c(7,1,4,3,2,1,1,2,4,2,6,4,5))
(visNetwork(nodes, edges, width = "100%") %>% 
    visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
    visLayout(randomSeed = 2)-> keeper3)
edges <- data.frame(from = c(1,2,4,1,5,8,7,3,6,3,5,2,1), 
                    to = c(1,1,1,7,1,4,3,2,4,2,6,4,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
edges <- data.frame(from = c(1,2,4,1,5,8,7,3,6,3,5,2,1), 
                    to = c(3,1,1,7,1,4,3,2,4,2,6,4,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)


visNetwork(nodes, edges, width = "190%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
visNetwork(nodes, edges, width = "70%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
visNetwork(nodes, edges, width = "90%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
edges <- data.frame(from = c(1,2,4,1,5,8,7,3,6,3,5,2,1), 
                    to = c(3,1,1,7,1,4,3,2,4,2,6,4,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
visNetwork(nodes, edges, width = "200%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
nodes <- data.frame(id = 1:7, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam",
                    size = 10)
nodes <- data.frame(id = 1:7, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam",
                    size = 10)
edges <- data.frame(from = c(1,2,4,1,5,8,7,3,6,3,5,2,1), 
                    to = c(3,1,1,7,1,4,3,2,4,2,6,4,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
nodes <- data.frame(id = 1:7, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam",
                    size = 100)
edges <- data.frame(from = c(1,2,4,1,5,8,7,3,6,3,5,2,1), 
                    to = c(3,1,1,7,1,4,3,2,4,2,6,4,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
nodes <- data.frame(id = 1:7, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam",
                    size = 70)
edges <- data.frame(from = c(1,2,4,1,5,8,7,3,6,3,5,2,1), 
                    to = c(3,1,1,7,1,4,3,2,4,2,6,4,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
nodes <- data.frame(id = 1:7, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam",
                    size = 30)
edges <- data.frame(from = c(1,2,4,1,5,8,7,3,6,3,5,2,1), 
                    to = c(3,1,1,7,1,4,3,2,4,2,6,4,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
nodes <- data.frame(id = 1:6, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam",
                    size = 30)
edges <- data.frame(from = c(1,2,4,1,5,8,7,3,6,3,5,2,1), 
                    to = c(3,1,1,7,1,4,3,2,4,2,6,4,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,5,2,1), 
                    to = c(3,1,1,7,1,4,3,2,4,2,6,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,5,2,1), 
                    to = c(3,1,1,1,4,3,2,4,2,6,4,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,5,2,1), 
                    to = c(3,1,1,1,4,3,2,4,2,6,4,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,5,2,1), 
                    to = c(3,1,1,1,4,3,2,4,2,6,4,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,7,2,1), 
                    to = c(3,1,7,1,4,3,2,4,2,6,4,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,5,2,1), 
                    to = c(3,1,7,1,4,3,2,4,2,6,4,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,5,2,1), 
                    to = c(3,1,7,1,4,3,2,4,2,6,1,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,1,2,1), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,6,4,5,8,7,3,6,3,1,2,1), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,1,2,1,6), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5,1))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,1,2,1,6,5), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5,1,1))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,1,2,1,6,5), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5,1,2))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,1,2,1,6,5), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5,1,6))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,1,2,1,6,5,6), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5,1,6,3))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,1,2,1,6,5,6), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5,1,6,4))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,8,7,3,6,3,1,2,1,6,5,1), 
                    to = c(3,1,7,1,4,3,2,4,2,6,5,5,1,6,4))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9), 
                    to = c(3,1,7,1,4,3,2,4,2))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))


edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9), 
                    to = c(3,3,7,1,4,3,2,4,2))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9), 
                    to = c(3,3,4,1,4,3,2,4,2))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1), 
                    to = c(3,3,4,1,4,3,2,4,2,2))
visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))










nodes <- data.frame(id = 1:6, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam",
                    size = 35)
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1), 
                    to = c(3,3,4,1,4,3,2,4,2,5),
                    size = .01,
                    length = .01)
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))






edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1), 
                    to = c(3,3,4,1,4,3,2,4,2,5))
visNetwork(nodes, edges, width = "200%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1), 
                    to = c(3,3,4,1,4,3,2,4,2,5))
visNetwork(nodes, edges, width = "200%", height = "200%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
visNetwork(nodes, edges, width = "200", height = "200") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))


visNetwork(nodes, edges, width = "2000", height = "2000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
visNetwork(nodes, edges, width = "2", height = "2") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
visNetwork(nodes, edges, width = "20", height = "20") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
visNetwork(nodes, edges, width = "200", height = "200") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
visNetwork(nodes, edges, width = "900", height = "900") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
visNetwork(nodes, edges, width = "110%", height = "110%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
visNetwork(nodes, edges, width = "190%", height = "190%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
visNetwork(nodes, edges, width = "1500", height = "1500") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1), 
                    to = c(3,3,4,1,4,3,2,4,2,5))
visNetwork(nodes, edges, width = "1500", height = "1500") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
nodes <- data.frame(id = 1:4, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam",
                    size = 40)
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1), 
                    to = c(3,3,4,1,4,3,2,4,2,5))
visNetwork(nodes, edges, width = "1500", height = "1500") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1,4), 
                    to = c(3,3,4,1,4,3,2,4,2,5,1))
visNetwork(nodes, edges, width = "1500", height = "1500") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1,4,1), 
                    to = c(3,3,4,1,4,3,2,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1,4,1,2), 
                    to = c(3,3,4,1,4,3,2,4,2,5,1,4,3))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1,4,1,2), 
                    to = c(3,3,4,1,4,3,2,4,2,5,1,4,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1,4,2), 
                    to = c(3,3,4,1,4,3,2,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,4,5,6,7,8,9,1,4,2), 
                    to = c(3,4,1,4,3,2,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,4,5,6,7,8,9,1,4,2), 
                    to = c(3,3,4,1,4,3,2,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,2,3,2,5,6,7,8,9,1,4,2), 
                    to = c(3,3,4,1,4,3,2,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,7,3,2,5,6,7,8,9,1,4,2), 
                    to = c(3,3,4,1,4,3,2,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,7,3,2,5,6,7,8,9,7,4,2), 
                    to = c(3,3,4,1,4,3,2,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
edges <- data.frame(from = c(1,7,3,2,6,6,7,7,9,7,7,2), 
                    to = c(3,3,4,1,4,3,2,4,2,5,1,4))
visNetwork(nodes, edges, width = "1000", height = "1000") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))
nodes <- data.frame(id = 1:4, 
                    shape = c("circularImage"),
                    image = image,
                    label = "I'm Sam",
                    size = 40)
