# some convenience functions for dealing with IPUMS imports in SPSS format

#install.packages("foreign");
library(foreign);

# import the SPSS file from IPUMS and check for any problems with factors
# posed to StackOverflow here: http://stackoverflow.com/questions/40987639/how-to-diagnose-duplicated-levels-in-an-r-import
ipums_load <- function(filepath) {
  print("Loading IPUMS file. This may take a few minutes since it's a large extracts, but you only have to do it once.")
  ipums <- read.spss(filepath, to.data.frame = TRUE)
  print(paste("Loaded", NROW(ipums), "rows."));
  
  # loop through each column that's a factor to see if there are duplicates,
  # which produce the red "duplicated levels in factors are deprecated" warning
  # but typically don't present any problems
  
  for (name in names(ipums)) {
    type <- class(ipums[[name]]);
    if (type == "factor") {
      dups <- anyDuplicated(levels(ipums[[name]]))
      if (dups != 0) {
        print(paste("Duplicate factor in", name, "at index", dups))
        fac <- levels(ipums[[name]])
        culprit <- fac[dups]
        matches <- subset(ipums, ipums[[name]]==culprit)
        print(paste("The offending value is", culprit, "which shows up", NROW(matches), "times in the data."))
        if (NROW(matches) == 0) {
          print("Since that value never occurs, I wouldn't worry about this.");
        }
      }
    }
  }
  return (ipums);
}

# convert a factors to their correct types
ipums_convert_factors <- function(ipums) {
  ipums$YEAR <- as.numeric(ipums$YEAR)
  
  print("de-factorizing remainder of factored columns into characters")
  types <- lapply(ipums, class)
  factor_columns <- names(types[types=="factor"])
  
  print("Converting factors to the appropriate types")
  for (column in factor_columns) {
    print(paste("Converting", column))
    ipums[[column]] <- as.character(ipums[[column]])
  }
  
  return(ipums);
}

ipums_convert_age <- function(data) {
  data$AGE <- as.character(data$AGE);
  data[data$AGE == "Less than 1 year old","AGE"] <- "0"
  data[data$AGE == "90 (90+ in 1980 and 1990)","AGE"] <- "90"
  data$AGE <- as.numeric(data$AGE)
  return(data)
}

# STATES
ipums_field_STATEFIP <- function(data) {
  if (!("STATEFIP" %in% colnames(data))) {
    print("Skipping `ipums_field_STATEFIP` since 'STATEFIP' isn't present.")  
    return(data);
  }
  
  print("Adding state FIPs values and abbreviations")
  
  # hand-crafted file that converts the state FIPs values to state names and abbreviations
  canonical <- as.data.frame(read.csv("ipums.R/variables/states.csv",
    colClasses=c("character","character","character","logical")
  ))
  
  data$STATE_NAME <- data$STATEFIP
  data$STATE_ABBR <- NA
  data$STATE_FIPS <- NA
  data <- subset(data, select = -STATEFIP )
  
  convertField <- function(row) {
    fips <- row$FIPS
    abbr <- row$ABBR
    name <- row$NAME
    
    data$STATE_FIPS[data$STATE_NAME==name] <- fips
    data$STATE_ABBR[data$STATE_NAME==name] <- abbr
    return (data);
  }
  
  for (i in 1:NROW(canonical)) {
    data <- convertField(canonical[i,])
  }
  
  return(data);
}

# add names of PUMAs
ipums_field_PUMA <- function(data) {
  if (!("PUMA" %in% colnames(data))) {
    print("Skipping `ipums_field_PUMA` since 'PUMA' isn't present.");
    return(data);
  }

  if (!("STATE_FIPS" %in% colnames(data))) {
    print("converting states to FIPS")
    data <- ipums_field_States(data);
  }
  
  # function to add leading zeroes where needed
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }

  data$PUMA_CODE <- paste(data$STATE_FIPS, substrRight(paste("00", data$PUMA, sep=""), 5), sep="")

  puma_list <- read.csv(
    "ipums.R/variables/pumas.csv", header=TRUE,
    colClasses=c(rep("character", 3), rep("integer", 2), rep("numeric", 6))
  )
  
  #puma_list$PUMA_DENSITY <- puma_list$POP10 / puma_list$ALAND_SQMI

  puma_names <- setNames(puma_list[,c("PUMA_CODE", "PUMA_NAME", "ALAND_SQMI")], c("PUMA_CODE", "PUMA_NAME", "PUMA_SQMI"))
  
  data <- merge(data, puma_names, by="PUMA_CODE")
    
  return(data);
}

# EDUCATION
ipums_field_EDUC <- function(data) {
  if (!("EDUC" %in% colnames(data))) {
    print("Skipping `makefield_Education` since 'EDUC' isn't present.")  
    return(data);
  }
  
  print("Simplifying EDUC into fewer categories in 'EDUC_SIMPLIFIED' and adding a 'EDUC_DEGREE' field")
  
  # hand-crafted file that converts the many EDUCD values to more general categories
  canonical <- as.data.frame(read.csv("ipums.R/variables/educd.csv", stringsAsFactors = F))
  
  data$EDUC_SIMPLIFIED <- "";
  data$EDUC_HAS_DEGREE <- "";
  
  convertField <- function(row) {
    original <- as.character(row[1]);
    data$EDUC_HAS_DEGREE[data$EDUCD==original] <- as.character(row[3])
    data$EDUC_SIMPLIFIED[data$EDUCD==original] <- as.character(row[2])
    return (data);
  }
  
  for (i in 1:nrow(canonical)) {
    data <- convertField(canonical[i,])
  }
  
  return(data);
}

# Match occupation names to the OCCSOC variable
ipums_field_OCCSOC <- function(data) {
  if (!("OCCSOC" %in% colnames(data))) {
    print("Skipping `ipums_field_OCCSOC` since 'OCCSOC' isn't present.")  
    return(data);
  }
  
  print("Adding OCCSOC occupation names")
  
  # hand-crafted file that converts the OCCSOC codes to descriptions, including condensed categories
  canonical <- as.data.frame(read.csv("ipums.R/variables/occsoc.csv",
    colClasses=rep("character", 4)
  ))

  data$OCCSOC_TITLE <- ""
  
  convertField <- function(row) {
    title <- row$OCCSOC_TITLE
    occsoc <- row$OCCSOC
    data$OCCSOC_TITLE[data$OCCSOC==occsoc] <- title
    return (data);
  }
  
  for (i in 1:NROW(canonical)) {
    print(paste(i, canonical[i,"OCCSOC_TITLE"]));
    data <- convertField(canonical[i,])
  }
  
  return(data)  
}