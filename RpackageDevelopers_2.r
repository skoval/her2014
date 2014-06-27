# Script to calculate gender information of R-packages

library(XML)

# Names of R packages on CRAN 
PackageURL <-
  "http://cran.r-project.org/web/packages/available_packages_by_name.html"

allPackage <- readHTMLTable(PackageURL)[[1]][, c("V1")]

# Extract package information, store in a list - this takes a while
PP <- list()
for (i in 1:length(allPackage)){
  print(i)
  packageName <- allPackage[i]
  packageURL <- paste("http://cran.r-project.org/web/packages/", packageName,
                      "/index.html", sep="")
  y <- readHTMLTable(packageURL)
  y$L1 <- packageName
  PP[[i]] <- y
} 
# save it once, so that the previous code can be avoided
save(file = "packages_27May2014.rda", PP)


# Once saved, can start from here...
load(file = "packages_27May2014.rda")

# Extract author information ... a bit crude
auth <- first <- dub <- maint <- allmaint <- maintComp <- maintvign <- num <- NULL

for (ii in 1:length(PP)) {
  L <- PP[[ii]][[1]]
  if (is.data.frame(L)) {
    num <- c(num, ii)   # genuine

    rr <- L[which (L[,1] == "Author:"), 2]
    Author <- as.character(rr)
    auth <- c(auth, format(as.person(Author), include = c("given", "family")))
    first <- c(first, format(as.person(Author), include = c("given")))
    maintvign <- c(maintvign, 'Vignettes:' %in% PP[[ii]][[2]][1][1,])

    mm <- L[which (L[,1] == "Maintainer:"),2]
    MM <- as.character(mm)
    # first maintainer in 'maint' - sometimes there is more than one - in allmaint
    maint <- c(maint, format(as.person(MM), include = c("given", "family"))[1])
    allmaint <- c(allmaint, format(as.person(MM), include = c("given", "family")))

    cc <- L[which (L[,1] == "NeedsCompilation:"),2]
    if (cc == "yes") 
      maintComp <- c(maintComp, format(as.person(MM), include = c("given", "family"))) 
  } else 
    dub <- c(dub, ii)    # dubious
}

# clean up and summaries
remove <- c("ORPHANED", "", "R Core Team", "Revolution Analytics")

# maintainers of packages with vignettes
maint_vign <- maint[maintvign]
maint_vign <- maint_vign[!maint_vign %in% remove] 
MV <- sort(table(maint_vign))
print(tail(MV, n = 20))

# authors    - cannot check for women here...
auth <- auth[!auth %in% remove]
AA <- sort(table(auth))
print(tail (AA, n = 20))

# maintainers of packages with compiled code (high maintenance)
maintComp <- maintComp[!maintComp %in% remove]
AC <- sort(table(maintComp))
print(tail (AC, n = 20))

# maintainers
maint <- maint[!maint %in% remove]  
MM <- sort(table(maint))
print(tail(MM, n = 20))

# I Don't do anything with this - as no similar information from men
Same <- c("Katharine Mullen", "Katharine M. Mullen")
Same2 <- c("Mary C Meyer", "Mary Meyer")
Same3 <- c("Daniela Witten", "Daniela M. Witten", "Daniela M Witten")

# manually selected female R-developers - if in doubt: used google picture inspection
is.female <- c("Agnes Fussl", "Agnieszka Listwon-Krol", "Aida Eslami", "Aiora Zabala",  
 "Aleksandra Maj", "Alessandra R. Brazzale", "Alessia Pini", "Alexis Dinno",  
 "Alice Cleynen", "Alice Parodi", "Alina Matei", "Allison Irvine",  
 "Alysha M De Livera", "Amanda Ellis", "Ameline Crida", "Ana Belen Nieto Librero",          
 "Ana C. Braga", "Ana C. Cebrian", "Ana Ines Vazquez", "Anabel Forte",  
 "Andrea Benedetti", "Andrea Dal Pozzolo", "Andrea Rau", "Andrea Wiencierz",   
 "Angela Fan", "Aniko Szabo", "Anita Lindmark", "Anita M. Thieler",   
 "Anja Lange", "Anja von Heydebreck", "Anna Bonnet", "Anna Kiriliouk",  
 "Anna Klimova",  "Anna Kuparinen", "Anna L. Tyler", "Anne-Laure Boulesteix", 
 "Anne Bichteler", "Anne Chao", "Anne Cori", "Anne Parkhurst",  
 "Anne Sabourin", "Annegret Grimm", "Annette Molinaro", "Annie Bouvier", 
 "Audrey Q. Fu", "Aurelie Bertrand", "Aurelie Siberchicot", "Aurora Torrente", 
 "Beatriz Goitisolo", "Beatriz Pateiro-Lopez",  "Benedetta Bigio",  "Bettina Gruen", 
 "Bettina Grün", "Birgit Erni", "Brigitte Mangin", "Camille Brunet",  
 "Carla Freitas", "Carla Moreira", "Caroline Berard", "Catherine Hurley",   
 "Cecile Chauvel", "Cecile Proust-Lima", "Celine Helbert", "Celine Levy-Leduc",  
 "Charlotte Wickham", "Chiara Gigliarano", "Chloe Bracis", "Christiana Kartsonaki", 
 "Christiane Fuchs", "Christina Riedel", "Christina Stahl", "Christine Hohensinn", 
 "Christine Porzelius", "Chuang Ma", "Claire Gormley", "Claudia Beleites",   
 "Claudia Cabrera", "Claudia Koellmann", "Claudia van Borkulo", "Cornelia Froemke",   
 "Cristina Tortora", "Daniela Dunkler",  "Daniela M Witten", "Daniela M. Witten", 
 "Daniela Witten", "Daniela Vicente", "Daniela Wenzel", "Deborah Burr", "Delia Bailey",  
 "Di Cook", "Dianne Cook",    "Edith Gabriel",  "Elea McDonnell Feit", 
 "Elena Bertossi", "Elisa Frutos Bernal", "Elisa Loza",  "Elise Dusseldorp",   
 "Elizabeth Crane", "Elizabeth Freeman", "Elizabeth Gonzalez-Estrada", "Elizabeth Gonzalez Estrada",       
 "Elizabeth Holmes - NOAA Federal",  "Elizabeth McClellan", "Elizabeth Purdom", "Elizabeth Slate", 
 "Emily Goren", "Emily Weiser",   "Emma Granqvist", "Emma Huang",    
 "Emmanuelle Comets",  "Erika Cule",     "Eva Janousova",  "Evelyne Vigneau", 
 "Fernanda De Bastiani", "Francesca Marta Lilja Di Lascio", "Gabriela Cohen-Freue", "Gabriele Sales",  
 "Gaelle LELANDAIS", "Genevera I. Allen", "Geraldine Van der Auwera", "Gerda Claeskens", 
 "Giselle Perdomo",  "Glenna Nightingale", "Hana Sevcikova", "Hanna Jankowski", 
 "Hannah Frick", "Heather Stoll",  "Heather Turner", "Heidi Seibold", 
 "Heike Hofmann", "Irene Castro Conde", "Irene Zeng", "Isabelle Charlier",  
 "Ivonne Solis-Trapala", "Jana Fruth", "Jelena Kovacic", "Jenine K. Harris",   
 "Jenise Swall", "Jenny Häggström", "Juliane Manitz", "Julie Carreau", 
 "Julie Eaton", "Karen R. Ryberg", "Karline Soetaert", "Karoly Antal",  
 "Katarina Domijan", "Kate Cowles", "Katharine M. Mullen", "Katharine Mullen",   
 "Kellie J. Archer", "Kim-Anh Le Cao", "Kim Nimon", "Kimberly F. McManus", 
 "Kitty Lo", "Kristen Zygmunt",  "Kristin A. Linn", "Laetitia Marisa", 
 "Larissa Matos", "Laura Marshall", "Laura Villanova", "Lauren Hanna",  
 "Laurence Kell", "Laurin Mueller", "Leena Choi", "Leslie Perez",  
 "Lindsay P. Scheef", "Lindsay V. Clark", "Line Clemmensen", "Lisa McFerrin", 
 "Lise Bardet", "Lora Murphy", "Lori A. Shepherd", "Luana Cecilia Meireles",           
 "Lysiane Charest", "M. Catherine Duryea", "M. Helena Gonçalves", "M. Suzette Blanchard",  
 "Madeleine Thompson", "Magdalena Chrapek", "Maja Pohar Perme", "Malika Charrad",  
 "Margarita Marin", "Margret Oelker", "Maria Dolores Martinez-Miranda", "Maria Mercedes Gregorio-Dominguez",
 "María Oliveira", "Maria Ortiz-Estevez", "Maria Rizzo", "Marie-Helene Ouellette",           
 "Marie-Pierre Sylvestre", "Marie Chabbert", "Marie Chavent", "Marie Laure Delignette-Muller",    
 "Marie Vendettuoli",   "Marijke Welvaert", "Marina Evangelou", "Marlene Mueller", 
 "Marlene Silva Marchena", "Marlies Vervloet", "Marloes Maathuis", "Martina Fischer", 
 "Martina Mincheva", "Mary C Meyer", "Mary Meyer", "Maxime Wack",   
 "Melanie Prague", "Melanie Quintana", "Melissa Hubisz", "Meredith Ray",  
 "Merete K Hansen", "Merlise Clyde", "Michela Battauz", "Michela Cameletti",  
 "Michele B. Nuijten", "Michele De Meo", "Michele Filosi", "Michele Grassi",  
 "Michele Morara", "Miriam Marusiakova", "Monica Fernanda Rinaldi", "Monica Lopez Raton", 
 "Monique Bettio Massuia", "Na Li", "Nadine Grambauer", "Nadine Schoene",  
 "Natacha Nikolic", "Natalia Becker", "Natalia Novoselova", "Natalia Vilor", 
 "Natalie Koziol", "Natalya Pya", "Nicole Kraemer", "Nicole Mee-Hyaang Jinn",           
 "Nikola Kasprikova", "Noemi Andor", "Noemie Robil", "Raquel Iniesta",  
 "Rebecca Killick", "Rebecca Sela", "Ronja Foraita", "Rosaria Lombardo",   
 "Ruth Heller", "Sabine Zinn", "Samantha Cook", "Sandra Barragán", 
 "Sandra Plancade", "Sara Varela", "Sara Varela González", "Sarah Brockhaus", 
 "Sarah Colby", "Sarah Gelper", "Sarah Gerster", "Sarah Goslee",  
 "Sarah Reese", "Shannon M. Bell", "Sharon X. Lee", "Silvia Liverani", 
 "Simone Giannerini", "Simone Padoan", "Sophie Achard", "Sophie Baillargeon", 
 "Sophie Lebre", "Stacey Winham", "Stefanie Hieke", "Stefanie Kalus",  
 "Stephanie Kovalchik", "Stephanie Locke", "Susan Gruber", "Susan Holmes",  
 "Susana Barbosa", "Susanne Heim", "Susanne Stampf", "Susie Jentoft", 
 "Talita Perciano", "Tanja Stadler", "Tanya Cashorali", "Tatjana Kinsvater",  
 "Tatsiana Khamiakova", "Tatyana Krivobokova", "Teresa Buglielli", "Teresa Henriques",   
 "Theresa Scharl", "Tracy Ke", "Ulrike Groemping", "Veronica Andrea Gonzalez-Lopez",   
 "Veronica J. Berrocal", "Vicky Qian Wu", "Viktoria Spaiser", "Virginie Rondeau",   
 "Wei E. Liang", "Wei Jiang", "Wei Yang", "Wiebke Werft",  
 "Wioletta Wojtowicz", "Xuefei Mi")
      

# first names of (chinese) females - asked a chinese student - based on his experience:
is.female.first <- c("Xia", "Xianhong", "Xiaohong", "Xiaoyue", "Xiaoyun", "Xinyu", "Xuan", "Xuefei", "Yanchun", "Yanhong",     
"Ying", "Ying-Ying", "Weihong",  "Qiong", "Jie")            

     
GenderInfo <- function (Data = MM, main = "maintainers", ...) {     
  Maintainers <- data.frame(Gender = "M", Number = Data, stringsAsFactors = FALSE)
  Maintainers[which (rownames(Maintainers) %in% is.female),"Gender"] <- "F" 
  Maintainers[which (as.person(rownames(Maintainers))$given %in% is.female.first),"Gender"] <- "F" 

  TM <- table (Maintainers)
  par (mfrow = c(2, 2))
  
  with (Maintainers, 
    plot(Number, pch = ".", ylab = "Number of packages",
      cex = 3, col = c("red", "blue")[as.factor(Gender)], log = "y"), ...)
  Fem <- which(Maintainers$Gender == "F")
  rug (Fem, col = "red")
  legend(ncol = 2, x = "topleft", text.col = c("red", "blue"), 
    c("Female", "Male",rowSums(TM, na.rm = TRUE)))
  
  Maint5 <- Maintainers[Maintainers$Number >= 5,]
  Fem <- which(Maint5$Gender == "F")
  with (Maint5, 
    plot(Number, pch = ".", cex = c(5, 3)[as.factor(Gender)], 
      ylab = "Number of packages",
      col = c("red", "blue")[as.factor(Gender)]), ...)
  abline (v = Fem, col = "red")
        
  plot(TM[2,], type = "b", log = "y", col = "blue", 
   ylab = "number of persons", xlab = "number of packages")
  lines(TM[1,], type = "b", col = "red")

  plot(0, axes = FALSE, xlab="", ylab="", type = "n")
  legend (fill = c("red", "blue"), legend = c("female", "male"), x = "center") 
  
  mtext(side = 3, line = -1.5, cex = 1.5, main, outer = TRUE)   
  invisible (TM)
}
pdf(file = "gendeR.pdf")      

GenderInfo()  # all packages
GenderInfo(MV, "Maintainers of packages with vignettes") 
GenderInfo(AC, "Maintainers of packages with compiled code") 

dev.off()
