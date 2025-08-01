FUNCTION Z_SDSFI_GEN_TIMESTAMP.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(EX_DATE) TYPE  DATS
*"     REFERENCE(EX_TIME) TYPE  NUMC10
*"----------------------------------------------------------------------

  DATA: lv_ts2(25) TYPE c,
        lv_ts      TYPE timestampl.

  DATA: lv_dat    TYPE date,
        lv_tim    TYPE time,
        lv_ex_tim TYPE numc10,
        lv_tz     TYPE timezone.

  DATA: lv_t1(10) TYPE c,
        lv_t2(13) TYPE c.

  GET TIME STAMP FIELD lv_ts.

  MOVE lv_ts TO lv_ts2.
  CONDENSE lv_ts2.

  CONVERT TIME STAMP lv_ts
           TIME ZONE sy-zonlo
           INTO DATE lv_dat
                TIME lv_tim.

  CONCATENATE lv_tim+0(2)  "hh
              lv_tim+2(2)  "mm
              lv_tim+4(2)  "ss
              lv_ts2+15(3) "SSS
         INTO lv_ex_tim.

  ex_date = lv_dat.
  ex_time = lv_ex_tim.



ENDFUNCTION.
