# code source: https://stackoverflow.com/questions/50798934/send-sql-string-through-post-with-httr-package-in-r

#install.packages(httr)
library(httr)

month_list <- c("January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", "November", "December")

sqlstrings <- "SELECT Year,Month,Day_of_Month,Day_Of_Week,Dep_Time,CRS_Dep_Time,Arr_Time,CRS_Arr_Time,Op_Unique_Carrier,OP_Carrier_FL_Num,Tail_Num,Actual_Elapsed_Time,CRS_Elapsed_Time,Air_Time,Arr_Delay,Dep_Delay,Origin,Dest,Distance,Taxi_In,Taxi_Out,Cancelled,Cancellation_Code,Diverted,Carrier_Delay,Weather_Delay,NAS_Delay,Security_Delay,Late_Aircraft_Delay FROM T_ONTIME_REPORTING WHERE Month=%d AND YEAR=%d"

for (i in 2001:2018) {
  
  for (j in 1:12) {
    
    print(paste("Download of ", month_list[j], ", ", i, " start", sep = ""))
    
    POST(
      url = "https://www.transtats.bts.gov/DownLoad_Table.asp",
      httr::add_headers(
        Referer = "http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236"
      ), 
      body = list(
        UserTableName = "Reporting_Carrier_On_Time_Performance_1987_present",
        DBShortName = "On_Time",
        RawDataTable = "T_ONTIME_REPORTING",
        sqlstr = sprintf(sqlstrings, j, i),
        grouplist = "", suml = "", sumRegion = "",
        filter1 = "title=", filter2 = "title=", geo = "All\xa0",
        time = month_list[j], timename = "Month", GEOGRAPHY = "All", XYEAR = as.character(i), FREQUENCY = "1"
      ),
      encode = "form",
      query = list(
        Table_ID = "236", Has_Group = "0", Is_Zipped = "0"
      )
    ) -> res
    
    (save_to <- file.path("./", paste(i, sprintf("%02d", j), ".zip", sep = "")))
    
    writeBin(httr::content(res, as="raw"), save_to)
    
    print(paste("Download of ", month_list[j], ", ", i, " complete", sep = ""))
  }
}


