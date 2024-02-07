list(APPEND ush_files
  make_ntc_bull.pl
  make_NTC_file.pl
  make_tif.sh
  month_name.sh
  )

install(PROGRAMS ${ush_files}
        DESTINATION "ush")
