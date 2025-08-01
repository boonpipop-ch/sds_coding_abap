*&---------------------------------------------------------------------*
*& Include          ZSDSCAR0050_F05
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_process_data_tmp11
*&---------------------------------------------------------------------*
*&  BP Master Template 11 Vendor General
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA_TMP11 CHANGING PT_RESULT TYPE GTTY_RESULT11
                                     PS_SUM     TYPE  GTY_SUM.

  DATA:
    LT_RAW  TYPE  GTTY_RAW,
    LT_DATA TYPE  GTTY_DATA11.

  DATA:
    LS_MESSG  TYPE  GTY_MESSG,
    LS_RESULT TYPE  GTY_RESULT11.


* --------------------------------
* Step1: Read Input file data
* --------------------------------
  PERFORM F_READ_INPUT_FILE CHANGING LT_RAW
                                     LS_MESSG.
  IF LS_MESSG IS NOT INITIAL.
*   Assign to result
    CLEAR LS_RESULT.
    LS_RESULT-STATU = ICON_LED_RED.
    LS_RESULT-MSGTY = LS_MESSG-MSGTY.
    LS_RESULT-MSGID = LS_MESSG-MSGID.
    LS_RESULT-MSGTX = LS_MESSG-MSGTX.
    INSERT LS_RESULT INTO TABLE PT_RESULT.
    RETURN.
  ENDIF.

* --------------------------------
* Step2: Validate Input file
* --------------------------------
  PERFORM F_VALIDATE_FILE_TMPL11  USING LT_RAW
                               CHANGING LT_DATA.

* --------------------------------
* Step3: Upload data from file
* --------------------------------
* Upload file
  PERFORM F_UPLOAD_FILE_TMPL11  USING LT_DATA
                                      CB_TEST
                             CHANGING PT_RESULT
                                      PS_SUM.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_file_tmpl11
*&---------------------------------------------------------------------*
*& Validate File data for Template 1
*&---------------------------------------------------------------------*
FORM F_VALIDATE_FILE_TMPL11 USING PT_RAW TYPE GTTY_RAW
                          CHANGING PT_DATA  TYPE  GTTY_DATA11.

  DATA:
    LS_KEY   TYPE  GTY_KEY11,
    LS_MAIN  TYPE  GTY_MAIN11,
    LS_MAINX TYPE  GTY_MAIN11X,
    LS_MESSG TYPE  GTY_MESSG.

  DATA:
    LV_ROWNO   TYPE  GTY_RESULT11-ROWNO.

  FIELD-SYMBOLS:
    <LFS_RAW>   TYPE  GTY_RAW.


* Initialize Output
  CLEAR: PT_DATA.

* Show Progress
* Text-p02 : Validating file data. . .
  MC_SHOW_PROGRESS 20 TEXT-P02.
*
*  IF p_begrow GE gs_config-beg_row.
*    lv_rowno = p_begrow - 1.
*  ELSE.
*    lv_rowno = gs_config-beg_row.
*  ENDIF.

  LV_ROWNO = P_BEGROW - 1.

  LOOP AT PT_RAW ASSIGNING <LFS_RAW>.

    LV_ROWNO = LV_ROWNO + 1.

*   Translate Raw line into variables
    PERFORM F_TRANSLATE_RAW11  USING  <LFS_RAW>
                            CHANGING LS_KEY
                                     LS_MAIN
                                     LS_MAINX
                                     LS_MESSG.

*   Validate and Append data into table
    PERFORM F_VALIDATE_AND_APPEND11  USING LV_ROWNO
                                           LS_KEY
                                           LS_MAIN
                                           LS_MAINX
                                           LS_MESSG
                                  CHANGING PT_DATA.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_upload_file_tmpl11
*&---------------------------------------------------------------------*
*& Upload File Template 1
*&---------------------------------------------------------------------*
FORM F_UPLOAD_FILE_TMPL11 USING PT_DATA TYPE GTTY_DATA11
                                 PV_TEST   TYPE  FLAG
                        CHANGING PT_RESULT TYPE  GTTY_RESULT11
                                 PS_SUM    TYPE  GTY_SUM.

*  TYPES: BEGIN OF lty_lfb1 ,
*           lifnr TYPE lfb1-lifnr,
*           bukrs TYPE lfb1-bukrs,
*           pernr TYPE lfb1-pernr,
*         END   OF lty_lfb1 .

  DATA:
    LT_MESSG TYPE  GTTY_MESSG.

  DATA:
    LS_DATA  TYPE  GTY_DATA11.

  DATA:
    LV_PARTNER TYPE  BUT000-PARTNER,
    LV_ERROR   TYPE  FLAG.

*  DATA lt_lfb1 TYPE TABLE OF lty_lfb1 .
  DATA:
    LV_BUKRS TYPE LFB1-BUKRS ##NEEDED,
    LV_LIFNR TYPE LFB1-LIFNR ##NEEDED,
*    ls_lfb1  TYPE lty_lfb1,
    LV_GUID  TYPE BAPIBUS1006_HEAD-PARTNGUID.

* Initialize Output
  CLEAR: PT_RESULT.
  CLEAR:   PS_SUM.

* Show Progress
* Text-p03 : Uploading file data. . .
  MC_SHOW_PROGRESS 45 TEXT-P03.

  IF PT_DATA IS NOT INITIAL.
    SELECT LIFNR, BUKRS INTO TABLE @GT_LOG_EXTEND
      FROM LFB1
       FOR ALL ENTRIES IN @PT_DATA
     WHERE LIFNR = @PT_DATA-KEY-PARTNER .
    IF SY-SUBRC = 0 .
      SORT GT_LOG_EXTEND BY PARTNER BUKRS .
    ENDIF.
  ENDIF.

* Processing each record
  LOOP AT PT_DATA INTO LS_DATA.

    CLEAR LS_DATA-KEY-EXTEND_COMP.

*   Only non validation error
    IF LS_DATA-MESSG[] IS INITIAL.

*       Implement suitable error handling here
      CALL FUNCTION 'BAPI_BUPA_GET_NUMBERS'
        EXPORTING
          BUSINESSPARTNER        = LS_DATA-KEY-PARTNER
        IMPORTING
*         BUSINESSPARTNEROUT     =
          BUSINESSPARTNERGUIDOUT = LV_GUID.

      IF GV_MODE = GC_MODE_CREATE .
        SORT GT_LOG_EXTEND BY PARTNER BUKRS .
        DELETE ADJACENT DUPLICATES FROM GT_LOG_EXTEND
                        COMPARING PARTNER BUKRS.

        READ TABLE GT_LOG_EXTEND TRANSPORTING NO FIELDS
          WITH KEY PARTNER = LS_DATA-KEY-PARTNER
                   BUKRS = LS_DATA-MAIN-COMP-BUKRS.
        IF SY-SUBRC = 0.
          CLEAR LS_DATA-KEY-EXTEND_COMP.
        ELSE.
          READ TABLE GT_LOG_EXTEND TRANSPORTING NO FIELDS
          WITH KEY PARTNER = LS_DATA-KEY-PARTNER   .
          IF SY-SUBRC = 0 .
            LS_DATA-KEY-EXTEND_COMP = GC_TRUE .
            LS_DATA-KEY-PARTNER_GUID = LV_GUID.
          ELSE.
            CLEAR LS_DATA-KEY-EXTEND_COMP.
          ENDIF.
        ENDIF.
      ENDIF.

      IF  LS_DATA-KEY-EXTEND_COMP IS NOT INITIAL .
        "Extend company
        PERFORM F_MAINTAIN_EXTEND_COMP11  USING  LS_DATA
                                                 PV_TEST
                                                 LV_ERROR
                                        CHANGING LV_PARTNER
                                                 LT_MESSG .
        IF LV_PARTNER IS NOT INITIAL.
          LS_DATA-KEY-PARTNER = LV_PARTNER.
        ENDIF.
        LS_DATA-MESSG = LT_MESSG.

      ELSE.
        PERFORM F_MAINTAIN_PARTNER11  USING  PV_TEST
                                    CHANGING LS_DATA
                                             LV_PARTNER
                                             LT_MESSG
                                             LV_ERROR.
        IF LV_PARTNER IS NOT INITIAL.
          LS_DATA-KEY-PARTNER = LV_PARTNER.
        ENDIF.
        LS_DATA-MESSG = LT_MESSG.

      ENDIF.
    ELSE.
      LV_ERROR = GC_TRUE.
    ENDIF.

    PS_SUM-TOTAL = PS_SUM-TOTAL + 1.

    IF LV_ERROR EQ GC_TRUE.
      PS_SUM-ERROR = PS_SUM-ERROR + 1.
    ELSE.
      PS_SUM-SUCCS = PS_SUM-SUCCS + 1.
*      IF ls_data-key-extend_comp IS INITIAL .
*        CLEAR ls_lfb1 .
*        ls_lfb1-lifnr = ls_data-key-partner .
*        ls_lfb1-bukrs = ls_data-main-comp-bukrs .
*        APPEND ls_lfb1 TO lt_lfb1 .
*      ENDIF.

    ENDIF.

*   Collect Result
    PERFORM F_COLLECT_RESULT11  USING  LS_DATA
                              CHANGING PT_RESULT.

    CLEAR: LT_MESSG[], LV_ERROR .
  ENDLOOP.

* Show Final Message for Processing completed
* Message: Processing completed.
  MESSAGE S582(1FA).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_translate_raw11
*&---------------------------------------------------------------------*
*& Convert Raw to Data1
*&---------------------------------------------------------------------*
FORM F_TRANSLATE_RAW11 USING PS_RAW TYPE GTY_RAW
                     CHANGING PS_KEY       TYPE  GTY_KEY11
                              PS_MAIN      TYPE  GTY_MAIN11
                              PS_MAINX     TYPE  GTY_MAIN11X
                              PS_MESSG     TYPE  GTY_MESSG.

  DATA:
    LT_SPLIT  TYPE  STANDARD TABLE OF STRING.

  DATA:
    LV_INDEX    TYPE  I,
    LV_FIELD    TYPE  GTY_FIELD_NAME,
    LV_STRING   TYPE  STRING,
    LV_MSGTX    TYPE  GTY_MESSG-MSGTX,
    LV_REQUIRED TYPE  CHAR1,
    LV_LEN      TYPE  I.


* Initialize Output
  CLEAR: PS_KEY,
         PS_MAIN,
         PS_MAINX,
         PS_MESSG.

* Split Into Fields
  SPLIT PS_RAW-TLINE AT GC_SPLIT INTO TABLE LT_SPLIT.

  LOOP AT LT_SPLIT INTO LV_STRING.

    LV_INDEX = SY-TABIX.

*   Initialize Variables
    CLEAR: LV_MSGTX.

*   Ignore Dummy Value
    CHECK LV_STRING NE GC_DUMMY_FLD ##BLANK_OK.

*   Replace Blank Filling
    IF LV_STRING EQ GC_BLANK_FILL.
      CLEAR LV_STRING.
    ENDIF.

    IF LV_STRING CP '"*"'.
      LV_LEN = STRLEN( LV_STRING ) - 2.
      LV_STRING = LV_STRING+1(LV_LEN).
    ENDIF.

*   Remove Special Char
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>NEWLINE IN LV_STRING WITH SPACE.

    PERFORM F_REMOVE_SPECIAL_CHAR CHANGING LV_STRING.

*   Get Target Field name to assign value
    PERFORM F_GET_TARGET_FIELD11  USING  LV_INDEX
                               CHANGING LV_FIELD.

    CASE LV_FIELD.
      WHEN 'ALTKN' .
        PS_MAIN-COMP-ALTKN  = LV_STRING.
        PS_MAINX-COMP-ALTKN = GC_TRUE.

      WHEN 'PARTNER'.
        PERFORM F_VALIDATE_PARTNER  USING  LV_STRING
                                           GV_MODE
                                  CHANGING PS_KEY-PARTNER
                                           PS_KEY-PARTNER_GUID
                                           LV_MSGTX.

      WHEN 'BU_GROUP'.
        PERFORM F_VALIDATE_BUGROUP  USING  LV_STRING
                                  CHANGING PS_KEY-BU_GROUP
                                           LV_MSGTX.
        PS_KEY-TYPE =  GC_PERSON.  "1 Person
      WHEN 'TYPE'.
        PERFORM F_VALIDATE_BPTYPE  USING  LV_STRING
                                 CHANGING PS_KEY-TYPE
                                          LV_MSGTX.

      WHEN 'ADDR_NAME1'.
        PS_MAIN-ADDR-NAME1  = LV_STRING.
        PS_MAINX-ADDR-NAME1 = GC_TRUE.

      WHEN 'ADDR_NAME2'.
        PS_MAIN-ADDR-NAME2  = LV_STRING.
        PS_MAINX-ADDR-NAME2 = GC_TRUE.

      WHEN 'ADDR_NAME3'.
        PS_MAIN-ADDR-NAME3  = LV_STRING.
        PS_MAINX-ADDR-NAME3 = GC_TRUE.

      WHEN 'ADDR_NAME4'.
        PS_MAIN-ADDR-NAME4  = LV_STRING.
        PS_MAINX-ADDR-NAME4 = GC_TRUE.

      WHEN 'ADDR_SORT1'.
        PS_MAIN-ADDR-SORT1  = LV_STRING.
        PS_MAINX-ADDR-SORT1 = GC_TRUE.

      WHEN 'ADDR_SORT2'.
        PS_MAIN-ADDR-SORT2  = LV_STRING.
        PS_MAINX-ADDR-SORT2 = GC_TRUE.

      WHEN 'ADDR_STREET'.
        PS_MAIN-ADDR-STREET  = LV_STRING.
        PS_MAINX-ADDR-STREET = GC_TRUE.

      WHEN 'ADDR_STR_SUPPL1'.
        PS_MAIN-ADDR-STR_SUPPL1  = LV_STRING.
        PS_MAINX-ADDR-STR_SUPPL1 = GC_TRUE.

      WHEN 'ADDR_STR_SUPPL2'.
        PS_MAIN-ADDR-STR_SUPPL2  = LV_STRING.
        PS_MAINX-ADDR-STR_SUPPL2 = GC_TRUE.

      WHEN 'ADDR_STR_SUPPL3'.
        PS_MAIN-ADDR-STR_SUPPL3  = LV_STRING.
        PS_MAINX-ADDR-STR_SUPPL3 = GC_TRUE.

      WHEN 'ADDR_LOCATION'.
        PS_MAIN-ADDR-LOCATION  = LV_STRING.
        PS_MAINX-ADDR-LOCATION = GC_TRUE.

      WHEN 'ADDR_CITY2'.
        PS_MAIN-ADDR-CITY2  = LV_STRING.
        PS_MAINX-ADDR-CITY2 = GC_TRUE.

      WHEN 'ADDR_CITY1'.
        PS_MAIN-ADDR-CITY1  = LV_STRING.
        PS_MAINX-ADDR-CITY1 = GC_TRUE.

      WHEN 'ADDR_POST_CODE1'.
        PS_MAIN-ADDR-POST_CODE1  = LV_STRING.
        PS_MAINX-ADDR-POST_CODE1 = GC_TRUE.

      WHEN 'ADDR_COUNTRY'.
        PS_MAIN-ADDR-COUNTRY  = LV_STRING.
        PS_MAINX-ADDR-COUNTRY = GC_TRUE.

      WHEN 'ADDR_LANGU'.
        PERFORM F_VALIDATE_LANGUAGE  USING  LV_STRING
                                   CHANGING PS_MAIN-ADDR-LANGU
                                            LV_MSGTX.
        PS_MAINX-ADDR-LANGU = GC_TRUE.

      WHEN 'ADDR_TELNO1'.
        PS_MAIN-ADDR-PHONE-TELNO1  = LV_STRING.
        PS_MAINX-ADDR-PHONE-TELNO1 = GC_TRUE.

      WHEN 'ADDR_TELEXT1'.
        PS_MAIN-ADDR-PHONE-TELEXT1  = LV_STRING.
        PS_MAINX-ADDR-PHONE-TELEXT1 = GC_TRUE.

      WHEN 'ADDR_TELNO2'.
        PS_MAIN-ADDR-PHONE-TELNO2  = LV_STRING.
        PS_MAINX-ADDR-PHONE-TELNO2 = GC_TRUE.

      WHEN 'ADDR_TELEXT2'.
        PS_MAIN-ADDR-PHONE-TELEXT2  = LV_STRING.
        PS_MAINX-ADDR-PHONE-TELEXT2 = GC_TRUE.

      WHEN 'ADDR_MOBILE1'.
        PS_MAIN-ADDR-PHONE-MOBILE  = LV_STRING.
        PS_MAINX-ADDR-PHONE-MOBILE = GC_TRUE.

      WHEN 'ADDR_FAXNO'.
        PS_MAIN-ADDR-FAX-FAXNO  = LV_STRING.
        PS_MAINX-ADDR-FAX-FAXNO = GC_TRUE.

      WHEN 'ADDR_FAXEXT'.
        PS_MAIN-ADDR-FAX-FAXEXT  = LV_STRING.
        PS_MAINX-ADDR-FAX-FAXEXT = GC_TRUE.

      WHEN 'ADDR_EMAIL01'.
        PS_MAIN-ADDR-SMTP-EMAIL1  = LV_STRING.
        PS_MAINX-ADDR-SMTP-EMAIL1 = GC_TRUE.

      WHEN 'ADDR_TELNO_AP'.
        PS_MAIN-ADDR_IND-TELNO  = LV_STRING.
        PS_MAINX-ADDR_IND-TELNO = GC_TRUE.

      WHEN 'ADDR_MOBILE_AP'.
        PS_MAIN-ADDR_IND-MOBILE  = LV_STRING.
        PS_MAINX-ADDR_IND-MOBILE = GC_TRUE.

      WHEN 'ADDR_EMAIL_AP'.
        PS_MAIN-ADDR_IND-EMAIL  = LV_STRING.
        PS_MAINX-ADDR_IND-EMAIL = GC_TRUE.


*      WHEN 'ADDRINT_ACTIVE'.
*        PERFORM f_validate_flag  USING  lv_string
*                               CHANGING ps_main-addr_int-active
*                                        lv_msgtx.

      WHEN 'ADDRINT_NAME1'.
        PS_MAIN-ADDR_INT-NAME1 = LV_STRING.
        PS_MAINX-ADDR_INT-NAME1 = GC_TRUE.

        PS_MAIN-ADDR_INT-ACTIVE = GC_TRUE.
      WHEN 'ADDRINT_NAME2'.
        PS_MAIN-ADDR_INT-NAME2 = LV_STRING.
        PS_MAINX-ADDR_INT-NAME2 = GC_TRUE.

      WHEN 'ADDRINT_NAME3'.
        PS_MAIN-ADDR_INT-NAME3 = LV_STRING.
        PS_MAINX-ADDR_INT-NAME3 = GC_TRUE.

      WHEN 'ADDRINT_NAME4'.
        PS_MAIN-ADDR_INT-NAME4 = LV_STRING.
        PS_MAINX-ADDR_INT-NAME4 = GC_TRUE.

      WHEN 'ADDRINT_SORT1'.
        PS_MAIN-ADDR_INT-SORT1 = LV_STRING.
        PS_MAINX-ADDR_INT-SORT1 = GC_TRUE.

      WHEN 'ADDRINT_STREET'.
        PS_MAIN-ADDR_INT-STREET = LV_STRING.
        PS_MAINX-ADDR_INT-STREET = GC_TRUE.

      WHEN 'ADDRINT_STR_SUPPL1'.
        PS_MAIN-ADDR_INT-STR_SUPPL1 = LV_STRING.
        PS_MAINX-ADDR_INT-STR_SUPPL1 = GC_TRUE.

      WHEN 'ADDRINT_STR_SUPPL2'.
        PS_MAIN-ADDR_INT-STR_SUPPL2 = LV_STRING.
        PS_MAINX-ADDR_INT-STR_SUPPL3 = GC_TRUE.


      WHEN 'ADDRINT_STR_SUPPL3'.
        PS_MAIN-ADDR_INT-STR_SUPPL3 = LV_STRING.
        PS_MAINX-ADDR_INT-STR_SUPPL3 = GC_TRUE.

      WHEN 'ADDRINT_LOCATION'.
        PS_MAIN-ADDR_INT-LOCATION = LV_STRING.
        PS_MAINX-ADDR_INT-LOCATION = GC_TRUE.

      WHEN 'ADDRINT_CITY2'.
        PS_MAIN-ADDR_INT-CITY2 = LV_STRING.
        PS_MAINX-ADDR_INT-CITY2 = GC_TRUE.

      WHEN 'ADDRINT_CITY1'.
        PS_MAIN-ADDR_INT-CITY1 = LV_STRING.
        PS_MAINX-ADDR_INT-CITY1 = GC_TRUE.

      WHEN 'ADDRINT_POST_CODE1'.
        PS_MAIN-ADDR_INT-POST_CODE1 = LV_STRING.
        PS_MAINX-ADDR_INT-POST_CODE1 = GC_TRUE.

      WHEN 'ADDRINT_COUNTRY'.
        PS_MAIN-ADDR_INT-COUNTRY = LV_STRING.
        PS_MAINX-ADDR_INT-COUNTRY = GC_TRUE.

      WHEN 'ADDRINT_LANGU'.
        PERFORM F_VALIDATE_LANGUAGE  USING  LV_STRING
                                   CHANGING PS_MAIN-ADDR_INT-LANGU
                                            LV_MSGTX.
        PS_MAINX-ADDR_INT-LANGU = GC_TRUE.
      WHEN 'ADEXT'.

        PS_MAIN-ADDR-ADEXT  = LV_STRING.
        PS_MAINX-ADDR-ADEXT = GC_TRUE.
      WHEN 'ADDR_ADEXT'.
        PS_MAIN-ADDR-ADEXT  = LV_STRING.
        PS_MAINX-ADDR-ADEXT = GC_TRUE.

      WHEN 'BPEXT'.
        PS_MAIN-BPEXT  = LV_STRING.
        PS_MAINX-BPEXT = GC_TRUE.

      WHEN 'TAX_TAXTYPE'.
        PS_MAIN-TAX-TAXTYPE  = LV_STRING.
        PS_MAINX-TAX-TAXTYPE = GC_TRUE.

      WHEN 'TAX_TAXNUM'.
        PS_MAIN-TAX-TAXNUM  = LV_STRING.
        PS_MAINX-TAX-TAXNUM = GC_TRUE.

      WHEN 'BNK01_BKVID'.
        PS_MAIN-BANK01-BKVID  = LV_STRING.
        PS_MAINX-BANK01-BKVID = GC_TRUE.

      WHEN 'BNK01_BANKS'.
        PS_MAIN-BANK01-BANKS  = LV_STRING.
        PS_MAINX-BANK01-BANKS = GC_TRUE.

      WHEN 'BNK01_BANKL'.
        PS_MAIN-BANK01-BANKL  = LV_STRING.
        PS_MAINX-BANK01-BANKL = GC_TRUE.

      WHEN 'BNK01_BANKN'.
        PS_MAIN-BANK01-BANKN  = LV_STRING.
        PS_MAINX-BANK01-BANKN = GC_TRUE.

      WHEN 'BNK01_BKONT'.
        PS_MAIN-BANK01-BKONT  = LV_STRING.
        PS_MAINX-BANK01-BKONT = GC_TRUE.

      WHEN 'BNK01_BKREF'.
        PS_MAIN-BANK01-BKREF  = LV_STRING.
        PS_MAINX-BANK01-BKREF = GC_TRUE.

      WHEN 'BNK01_BKEXT'.
        PS_MAIN-BANK01-BKEXT  = LV_STRING.
        PS_MAINX-BANK01-BKEXT = GC_TRUE.

      WHEN 'BNK01_KOINH'.
        PS_MAIN-BANK01-KOINH  = LV_STRING.
        PS_MAINX-BANK01-KOINH = GC_TRUE.


      WHEN 'BNK02_BKVID'.
        PS_MAIN-BANK02-BKVID  = LV_STRING.
        PS_MAINX-BANK02-BKVID = GC_TRUE.

      WHEN 'BNK02_BANKS'.
        PS_MAIN-BANK02-BANKS  = LV_STRING.
        PS_MAINX-BANK02-BANKS = GC_TRUE.

      WHEN 'BNK02_BANKL'.
        PS_MAIN-BANK02-BANKL  = LV_STRING.
        PS_MAINX-BANK02-BANKL = GC_TRUE.

      WHEN 'BNK02_BANKN'.
        PS_MAIN-BANK02-BANKN  = LV_STRING.
        PS_MAINX-BANK02-BANKN = GC_TRUE.

      WHEN 'BNK02_BKONT'.
        PS_MAIN-BANK02-BKONT  = LV_STRING.
        PS_MAINX-BANK02-BKONT = GC_TRUE.

      WHEN 'BNK02_BKREF'.
        PS_MAIN-BANK02-BKREF  = LV_STRING.
        PS_MAINX-BANK02-BKREF = GC_TRUE.

      WHEN 'BNK02_BKEXT'.
        PS_MAIN-BANK02-BKEXT  = LV_STRING.
        PS_MAINX-BANK02-BKEXT = GC_TRUE.


      WHEN 'BNK03_BKVID'.
        PS_MAIN-BANK03-BKVID  = LV_STRING.
        PS_MAINX-BANK03-BKVID = GC_TRUE.

      WHEN 'BNK03_BANKS'.
        PS_MAIN-BANK03-BANKS  = LV_STRING.
        PS_MAINX-BANK03-BANKS = GC_TRUE.

      WHEN 'BNK03_BANKL'.
        PS_MAIN-BANK03-BANKL  = LV_STRING.
        PS_MAINX-BANK03-BANKL = GC_TRUE.

      WHEN 'BNK03_BANKN'.
        PS_MAIN-BANK03-BANKN  = LV_STRING.
        PS_MAINX-BANK03-BANKN = GC_TRUE.

      WHEN 'BNK03_BKONT'.
        PS_MAIN-BANK03-BKONT  = LV_STRING.
        PS_MAINX-BANK03-BKONT = GC_TRUE.

      WHEN 'BNK03_BKREF'.
        PS_MAIN-BANK03-BKREF  = LV_STRING.
        PS_MAINX-BANK03-BKREF = GC_TRUE.

      WHEN 'BNK03_BKEXT'.
        PS_MAIN-BANK03-BKEXT  = LV_STRING.
        PS_MAINX-BANK03-BKEXT = GC_TRUE.

      WHEN 'BNK03_KOINH'.
        PS_MAIN-BANK03-KOINH  = LV_STRING.
        PS_MAINX-BANK03-KOINH = GC_TRUE.


      WHEN 'BNK04_BKVID'.
        PS_MAIN-BANK04-BKVID  = LV_STRING.
        PS_MAINX-BANK04-BKVID = GC_TRUE.

      WHEN 'BNK04_BANKS'.
        PS_MAIN-BANK04-BANKS  = LV_STRING.
        PS_MAINX-BANK04-BANKS = GC_TRUE.

      WHEN 'BNK04_BANKL'.
        PS_MAIN-BANK04-BANKL  = LV_STRING.
        PS_MAINX-BANK04-BANKL = GC_TRUE.

      WHEN 'BNK04_BANKN'.
        PS_MAIN-BANK04-BANKN  = LV_STRING.
        PS_MAINX-BANK04-BANKN = GC_TRUE.

      WHEN 'BNK04_BKONT'.
        PS_MAIN-BANK04-BKONT  = LV_STRING.
        PS_MAINX-BANK04-BKONT = GC_TRUE.

      WHEN 'BNK04_BKREF'.
        PS_MAIN-BANK04-BKREF  = LV_STRING.
        PS_MAINX-BANK04-BKREF = GC_TRUE.

      WHEN 'BNK04_BKEXT'.
        PS_MAIN-BANK04-BKEXT  = LV_STRING.
        PS_MAINX-BANK04-BKEXT = GC_TRUE.

      WHEN 'BNK04_KOINH'.
        PS_MAIN-BANK04-KOINH  = LV_STRING.
        PS_MAINX-BANK04-KOINH = GC_TRUE.

      WHEN 'BNK05_BKVID'.
        PS_MAIN-BANK05-BKVID  = LV_STRING.
        PS_MAINX-BANK05-BKVID = GC_TRUE.

      WHEN 'BNK05_BANKS'.
        PS_MAIN-BANK05-BANKS  = LV_STRING.
        PS_MAINX-BANK05-BANKS = GC_TRUE.

      WHEN 'BNK05_BANKL'.
        PS_MAIN-BANK05-BANKL  = LV_STRING.
        PS_MAINX-BANK05-BANKL = GC_TRUE.

      WHEN 'BNK05_BANKN'.
        PS_MAIN-BANK05-BANKN  = LV_STRING.
        PS_MAINX-BANK05-BANKN = GC_TRUE.

      WHEN 'BNK05_BKONT'.
        PS_MAIN-BANK05-BKONT  = LV_STRING.
        PS_MAINX-BANK05-BKONT = GC_TRUE.

      WHEN 'BNK05_BKREF'.
        PS_MAIN-BANK05-BKREF  = LV_STRING.
        PS_MAINX-BANK05-BKREF = GC_TRUE.

      WHEN 'BNK05_BKEXT'.
        PS_MAIN-BANK05-BKEXT  = LV_STRING.
        PS_MAINX-BANK05-BKEXT = GC_TRUE.

      WHEN 'BNK05_KOINH'.
        PS_MAIN-BANK05-KOINH  = LV_STRING.
        PS_MAINX-BANK05-KOINH = GC_TRUE.

      WHEN 'BNK06_BKVID'.
        PS_MAIN-BANK06-BKVID  = LV_STRING.
        PS_MAINX-BANK06-BKVID = GC_TRUE.

      WHEN 'BNK06_BANKS'.
        PS_MAIN-BANK06-BANKS  = LV_STRING.
        PS_MAINX-BANK06-BANKS = GC_TRUE.

      WHEN 'BNK06_BANKL'.
        PS_MAIN-BANK06-BANKL  = LV_STRING.
        PS_MAINX-BANK06-BANKL = GC_TRUE.

      WHEN 'BNK06_BANKN'.
        PS_MAIN-BANK06-BANKN  = LV_STRING.
        PS_MAINX-BANK06-BANKN = GC_TRUE.

      WHEN 'BNK06_BKONT'.
        PS_MAIN-BANK06-BKONT  = LV_STRING.
        PS_MAINX-BANK06-BKONT = GC_TRUE.

      WHEN 'BNK06_BKREF'.
        PS_MAIN-BANK06-BKREF  = LV_STRING.
        PS_MAINX-BANK06-BKREF = GC_TRUE.

      WHEN 'BNK06_BKEXT'.
        PS_MAIN-BANK06-BKEXT  = LV_STRING.
        PS_MAINX-BANK06-BKEXT = GC_TRUE.

      WHEN 'BNK06_KOINH'.
        PS_MAIN-BANK06-KOINH  = LV_STRING.
        PS_MAINX-BANK06-KOINH = GC_TRUE.

      WHEN 'BPKIND'.
        PS_MAIN-BPKIND   = LV_STRING.
        PS_MAINX-BPKIND  = GC_TRUE.

      WHEN 'VEND_VBUND'.
        PERFORM F_VALIDATE_TRADEPARTNR  USING  LV_STRING
                                      CHANGING PS_MAIN-VEND-VBUND
                                               LV_MSGTX.
        PS_MAINX-VEND-VBUND = GC_TRUE.


      WHEN 'VEND_EMPFK'.
        PS_MAIN-VEND-EMPFK  = LV_STRING.
        PS_MAINX-VEND-EMPFK = GC_TRUE.



      WHEN 'VEND_KUNNR'.
        PERFORM F_VALIDATE_CUSTOMER  USING  LV_STRING
                                   CHANGING PS_MAIN-VEND-KUNNR
                                            LV_MSGTX.
        PS_MAINX-VEND-KUNNR = GC_TRUE.

      WHEN 'VEND_XZEMP'.
        PERFORM F_VALIDATE_FLAG  USING  LV_STRING
                               CHANGING PS_MAIN-VEND-XZEMP
                                        LV_MSGTX.
        PS_MAINX-VEND-XZEMP = GC_TRUE.


      WHEN 'KNVK_NAMEV'.

        PS_MAIN-VEND-NAMEV = LV_STRING.

      WHEN 'KNVK_NAME1'.
        PS_MAIN-VEND-NAME1 = LV_STRING.


      WHEN 'COMP_BUKRS'.
        PERFORM F_VALIDATE_COMPCODE  USING  LV_STRING
                                   CHANGING PS_MAIN-COMP-BUKRS
                                            LV_MSGTX.

      WHEN 'COMP_AKONT'.
        IF PS_MAIN-COMP-BUKRS IS NOT INITIAL .
          PERFORM F_VALIDATE_GLACCOUNT  USING  LV_STRING
                                               PS_MAIN-COMP-BUKRS
                                      CHANGING PS_MAIN-COMP-AKONT
                                               LV_MSGTX.
          PS_MAINX-COMP-AKONT = GC_TRUE.
        ENDIF.


      WHEN 'COMP_ZUAWA'.
        PS_MAIN-COMP-ZUAWA  = LV_STRING.
        PS_MAINX-COMP-ZUAWA = GC_TRUE.

      WHEN 'COMP_FDGRV'.
        PERFORM F_VALIDATE_PLAN_GRP  USING  LV_STRING
                                   CHANGING PS_MAIN-COMP-FDGRV
                                            LV_MSGTX.
        PS_MAINX-COMP-FDGRV = GC_TRUE.

      WHEN 'COMP_ALTKN'.
        PS_MAIN-COMP-ALTKN  = LV_STRING.
        PS_MAINX-COMP-ALTKN = GC_TRUE.

      WHEN 'COMP_ZTERM'.
        PS_MAIN-COMP-ZTERM  = LV_STRING.
        PS_MAINX-COMP-ZTERM = GC_TRUE.

      WHEN 'COMP_REPRF'.
        PERFORM F_VALIDATE_FLAG  USING  LV_STRING
                               CHANGING PS_MAIN-COMP-REPRF
                                        LV_MSGTX.
        PS_MAINX-COMP-REPRF = GC_TRUE.

      WHEN 'COMP_ZWELS'.
        PS_MAIN-COMP-ZWELS  = LV_STRING.
        PS_MAINX-COMP-ZWELS = GC_TRUE.

      WHEN 'COMP_HBKID'.
        PS_MAIN-COMP-HBKID  = LV_STRING.
        PS_MAINX-COMP-HBKID = GC_TRUE.

      WHEN 'WTAX_WITHT1'.
        PS_MAIN-COMP-WTAX1-WITHT  = LV_STRING.

      WHEN 'WTAX_WITHCD1'.
        PS_MAIN-COMP-WTAX1-WITHCD  = LV_STRING.
        PS_MAINX-COMP-WTAX1-WITHCD = GC_TRUE.

      WHEN 'WTAX_SUBJCT1'.
        PS_MAIN-COMP-WTAX1-SUBJCT  = LV_STRING.
        PS_MAINX-COMP-WTAX1-SUBJCT = GC_TRUE.

      WHEN 'WTAX_QSREC1'.
        PS_MAIN-COMP-WTAX1-QSREC  = LV_STRING.
        PS_MAINX-COMP-WTAX1-QSREC = GC_TRUE.

      WHEN 'WTAX_WITHT2'.
        PS_MAIN-COMP-WTAX2-WITHT  = LV_STRING.

      WHEN 'WTAX_WITHCD2'.
        PS_MAIN-COMP-WTAX2-WITHCD  = LV_STRING.
        PS_MAINX-COMP-WTAX2-WITHCD = GC_TRUE.

      WHEN 'WTAX_SUBJCT2'.
        PS_MAIN-COMP-WTAX2-SUBJCT  = LV_STRING.
        PS_MAINX-COMP-WTAX2-SUBJCT = GC_TRUE.

      WHEN 'WTAX_QSREC2'.
        PS_MAIN-COMP-WTAX2-QSREC  = LV_STRING.
        PS_MAINX-COMP-WTAX2-QSREC = GC_TRUE.

      WHEN 'BCODE'.
        PS_MAIN-COMP-BRANCH-BCODE  = LV_STRING.
        PS_MAINX-COMP-BRANCH-BCODE = GC_TRUE.

      WHEN 'BDESC'.
        PS_MAIN-COMP-BRANCH-BDESC  = LV_STRING.
        PS_MAINX-COMP-BRANCH-BDESC = GC_TRUE.

      WHEN 'BDEFT'.
        PS_MAIN-COMP-BRANCH-BDEFT  = LV_STRING.
        PS_MAINX-COMP-BRANCH-BDEFT = GC_TRUE.

      WHEN 'PURC_EKORG'.
        PERFORM F_VALIDATE_PURCH_ORG  USING  LV_STRING
                                    CHANGING PS_MAIN-PURCH-EKORG
                                             LV_MSGTX.

      WHEN 'PURC_WAERS'.
        PERFORM F_VALIDATE_CURRKEY  USING  LV_STRING
                                  CHANGING PS_MAIN-PURCH-WAERS
                                           LV_MSGTX.
        PS_MAINX-PURCH-WAERS = GC_TRUE.

      WHEN 'PURC_ZTERM'.
        PS_MAIN-PURCH-ZTERM  = LV_STRING.
        PS_MAINX-PURCH-ZTERM = GC_TRUE.

      WHEN 'PURC_INCO1'.
        PS_MAIN-PURCH-INCO1  = LV_STRING.
        PS_MAINX-PURCH-INCO1 = GC_TRUE.

      WHEN 'PURC_INCO2_L'.
        PS_MAIN-PURCH-INCO2_L  = LV_STRING.
        PS_MAINX-PURCH-INCO2_L = GC_TRUE.

      WHEN 'PURC_VERKF'.
        PS_MAIN-PURCH-VERKF  = LV_STRING.
        PS_MAINX-PURCH-VERKF = GC_TRUE.

      WHEN 'PURC_TELF1'.
        PS_MAIN-PURCH-TELF1  = LV_STRING.
        PS_MAINX-PURCH-TELF1 = GC_TRUE.

      WHEN 'PURC_WEBRE'.
        PS_MAIN-PURCH-WEBRE  = LV_STRING.
        PS_MAINX-PURCH-WEBRE = GC_TRUE.

      WHEN 'PURC_EKGRP'.
        PS_MAIN-PURCH-EKGRP  = LV_STRING.
        PS_MAINX-PURCH-EKGRP = GC_TRUE.

      WHEN 'PURC_KZAUT'.
        PERFORM F_VALIDATE_FLAG  USING  LV_STRING
                               CHANGING PS_MAIN-PURCH-KZAUT
                                        LV_MSGTX.
        PS_MAINX-PURCH-KZAUT = GC_TRUE.



      WHEN OTHERS.
        CONTINUE.

    ENDCASE.

*   If error, assign message and ignore the rest
    IF LV_MSGTX IS NOT INITIAL.
*     Do not override previous error
      IF PS_MESSG-MSGTY NE 'E'.
        PS_MESSG-MSGTY = 'E'.
        PS_MESSG-MSGID = 'ZTEC'.
        PS_MESSG-MSGNO = '000'.
        PS_MESSG-MSGTX = LV_MSGTX.
      ENDIF.
*     Processing until all required fields
      IF LV_REQUIRED EQ GC_TRUE.
        EXIT.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_target_field11
*&---------------------------------------------------------------------*
*& Get Target Field name based on column sequence
*&---------------------------------------------------------------------*
FORM F_GET_TARGET_FIELD11 USING PV_INDEX TYPE I
                        CHANGING PV_FIELD  TYPE  GTY_FIELD_NAME.

  STATICS:
    LT_FIELDS   TYPE  ABAP_COMPDESCR_TAB.

  DATA:
    LREF_DESCR TYPE  REF TO CL_ABAP_TYPEDESCR,
    LREF_STRUC TYPE  REF TO CL_ABAP_STRUCTDESCR.

  FIELD-SYMBOLS:
    <LFS_FIELD>  TYPE  ABAP_COMPDESCR.


* Initialize Output
  CLEAR: PV_FIELD.

  IF LT_FIELDS IS INITIAL.
*   Get Template Fields Sequence
    LREF_DESCR = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( 'GTY_TEMPLATE11' ).
    LREF_STRUC ?= LREF_DESCR.
    LT_FIELDS = LREF_STRUC->COMPONENTS.
  ENDIF.

  READ TABLE LT_FIELDS ASSIGNING <LFS_FIELD>
                       INDEX PV_INDEX.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Output
  PV_FIELD = <LFS_FIELD>-NAME.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_remove_special_char
*&---------------------------------------------------------------------*
*&  Remove Special Character
*&---------------------------------------------------------------------*
FORM F_REMOVE_SPECIAL_CHAR CHANGING PV_STRING TYPE STRING.

  CONSTANTS:
    LC_SP01(2) TYPE X VALUE 'A0'.

  FIELD-SYMBOLS:
    <LFS_FIELD> TYPE CHAR1.

  DATA:
    LV_SP01 TYPE CHAR1.


  ASSIGN LC_SP01 TO <LFS_FIELD> CASTING.
  LV_SP01 = <LFS_FIELD>.

  REPLACE ALL OCCURRENCES OF LV_SP01 IN PV_STRING WITH ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_and_append11
*&---------------------------------------------------------------------*
*& Validate Whole Line data and collect into internal table for posting
*&---------------------------------------------------------------------*
FORM F_VALIDATE_AND_APPEND11 USING PV_ROWNO TYPE GTY_RESULT11-ROWNO
                                    PS_KEY    TYPE  GTY_KEY11
                                    PS_MAIN   TYPE  GTY_MAIN11
                                    PS_MAINX  TYPE  GTY_MAIN11X
                                    PS_MESSG  TYPE  GTY_MESSG
                           CHANGING PT_DATA   TYPE  GTTY_DATA11.

  DATA:
    LS_KEY   TYPE  GTY_KEY11,
    LS_MAIN  TYPE  GTY_MAIN11,
    LS_MAINX TYPE  GTY_MAIN11X,
    LS_DATA  TYPE  GTY_DATA11,
    LS_MESSG TYPE  GTY_MESSG.


* Initial Data
  LS_KEY   = PS_KEY.
  LS_MAIN  = PS_MAIN.
  LS_MAINX = PS_MAINX.

  DO 1 TIMES.
*  ---------------------------------
*   Validate Key
*  ---------------------------------
*   Validate Key
    PERFORM F_VALIDATE_KEY11 CHANGING LS_KEY
                                     LS_MESSG.
    IF LS_MESSG IS NOT INITIAL.
      EXIT.
    ENDIF.

*  ---------------------------------
*   Validate Main data
*  ---------------------------------
    PERFORM F_VALIDATE_MAIN11  USING  LS_KEY
                            CHANGING LS_MAIN
                                     LS_MAINX
                                     LS_MESSG.
    IF LS_MESSG IS NOT INITIAL.
      EXIT.
    ENDIF.

*  Only for Vendor Role
    READ TABLE LS_KEY-ROLE TRANSPORTING NO FIELDS
                           WITH KEY RLTYP = GC_ROLE1_12.
    IF SY-SUBRC EQ 0.
*     ---------------------------------
*      Validate Vendor
*     ---------------------------------
      PERFORM F_VALIDATE_VEND  USING  LS_KEY
                                      LS_MAIN-COMP-BUKRS
                             CHANGING LS_MAIN-VEND
                                      LS_MAINX-VEND
                                      LS_MESSG.
      IF LS_MESSG IS NOT INITIAL.
        EXIT.
      ENDIF.

*     ---------------------------------
*      Validate Company
*     ---------------------------------
      IF LS_MAIN-COMP-BUKRS IS NOT INITIAL .
        PERFORM F_VALIDATE_COMP  USING  LS_KEY
                                        LS_MAIN-VEND
                               CHANGING LS_MAIN-COMP
                                        LS_MAINX-COMP
                                        LS_MESSG.
        IF LS_MESSG IS NOT INITIAL.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

*  Only for Purchasing Role
    READ TABLE LS_KEY-ROLE TRANSPORTING NO FIELDS
                           WITH KEY RLTYP = GC_ROLE1_13.
    IF SY-SUBRC EQ 0.
*     ---------------------------------
*      Validate Purchasing View
*     ---------------------------------
      PERFORM F_VALIDATE_PURCH  USING  LS_KEY
                                       LS_MAIN-VEND
                              CHANGING LS_MAIN-PURCH
                                       LS_MAINX-PURCH
                                       LS_MESSG.
      IF LS_MESSG IS NOT INITIAL.
        EXIT.
      ENDIF.

    ENDIF.

  ENDDO.

* Generate new key
  CLEAR LS_DATA.
  LS_DATA-ROWNO = PV_ROWNO.
  LS_DATA-KEY   = LS_KEY.
  LS_DATA-MAIN  = LS_MAIN.
  LS_DATA-MAINX = LS_MAINX.

* Collect Message
  IF PS_MESSG IS NOT INITIAL.
    APPEND PS_MESSG TO LS_DATA-MESSG.
  ELSEIF LS_MESSG IS NOT INITIAL.
    APPEND LS_MESSG TO LS_DATA-MESSG.
  ENDIF.

* Generate GUID
  IF LS_DATA-MESSG IS INITIAL AND
     LS_DATA-KEY-PARTNER_GUID IS INITIAL.
    TRY.
        CALL METHOD CL_SYSTEM_UUID=>IF_SYSTEM_UUID_STATIC~CREATE_UUID_C32
          RECEIVING
            UUID = LS_DATA-KEY-PARTNER_GUID.
      CATCH CX_UUID_ERROR.
        CLEAR LS_DATA-KEY-PARTNER_GUID.
    ENDTRY.
  ENDIF.

  INSERT LS_DATA INTO TABLE PT_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_key11
*&---------------------------------------------------------------------*
*& Validate Key
*&---------------------------------------------------------------------*
FORM F_VALIDATE_KEY11 CHANGING PS_KEY TYPE GTY_KEY11
                              PS_MESSG   TYPE GTY_MESSG.

  DATA:
    LS_ROLE     TYPE  GTY_V_ROLE.

  DATA:
    LV_PARTNER  TYPE  BUT000-PARTNER ##NEEDED.


* Initialize Output
  CLEAR: PS_MESSG.

  IF PS_KEY-BU_GROUP IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e15 : Missing BP Group in the file.
    PS_MESSG-MSGTX = TEXT-E15.
    RETURN.
  ENDIF.

  IF PS_KEY-TYPE IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e16 : Missing BP Category in the file.
    PS_MESSG-MSGTX = TEXT-E16.
    RETURN.
  ELSEIF PS_KEY-TYPE NE GC_TYPE_ORG.
*    PS_MESSG-MSGTY = 'E'.
*    PS_MESSG-MSGID = 'ZTEC'.
*    PS_MESSG-MSGNO = '000'.
**   Text-e34 : BP Category must be organization only.
*    PS_MESSG-MSGTX = TEXT-E34.
*    RETURN.
  ENDIF.

* Check Number on Creation
  CASE GV_MODE.
    WHEN GC_MODE_CREATE.
*      IF CB_TEST = space.
      CALL FUNCTION 'BUP_NUMBER_CHECK'
        EXPORTING
          I_PARTNER     = PS_KEY-PARTNER
          I_GROUP       = PS_KEY-BU_GROUP
          I_AKTYP       = '01'
          I_AKTDB       = '01'
        EXCEPTIONS
          ERROR_OCCURED = 1
          OTHERS        = 2.
      IF SY-SUBRC <> 0.
        PS_MESSG-MSGTY = 'E'.
        PS_MESSG-MSGID = SY-MSGID.
        PS_MESSG-MSGNO = SY-MSGNO.
        MESSAGE ID SY-MSGID
                TYPE  SY-MSGTY
                NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2
                     SY-MSGV3 SY-MSGV4
                INTO PS_MESSG-MSGTX.
        RETURN.
      ENDIF.
*      ENDIF.
    WHEN GC_MODE_CHANGE.
      IF PS_KEY-PARTNER IS INITIAL.
        PS_MESSG-MSGTY = 'E'.
        PS_MESSG-MSGID = 'ZTEC'.
        PS_MESSG-MSGNO = '000'.
*       Text-e17 : Missing BP Number in the file.
        PS_MESSG-MSGTX = TEXT-E17.
        RETURN.
      ENDIF.

  ENDCASE.

  DO 2 TIMES.
    CLEAR LS_ROLE.

    IF SY-INDEX EQ 1.
*      IF cb_role1 IS INITIAL.
*        CONTINUE.
*      ENDIF.
      LS_ROLE-RLTYP = GC_ROLE1_11.
    ELSEIF SY-INDEX EQ 2.
*      IF cb_role2 IS INITIAL.
*        CONTINUE.
*      ENDIF.
      LS_ROLE-RLTYP = GC_ROLE1_12.
*    ELSEIF sy-index EQ 3.
*      IF cb_role3 IS INITIAL.
*        CONTINUE.
*      ENDIF.
*      ls_role-rltyp = gc_role1_13.
    ENDIF.
    SELECT PARTNER
        UP TO 1 ROWS
      INTO LV_PARTNER
      FROM BUT100
     WHERE PARTNER EQ PS_KEY-PARTNER
       AND RLTYP   EQ LS_ROLE-RLTYP
     ORDER BY PRIMARY KEY.                           "#EC CI_SEL_NESTED
    ENDSELECT.
    IF SY-SUBRC EQ 0.
      LS_ROLE-EXIST = GC_TRUE.
    ENDIF.
    INSERT LS_ROLE INTO TABLE PS_KEY-ROLE.
  ENDDO.

* Check Existing Key
  IF PS_KEY-PARTNER IS NOT INITIAL.

    SELECT SINGLE PARTNER
      INTO LV_PARTNER
      FROM BUT000
     WHERE PARTNER EQ PS_KEY-PARTNER.                "#EC CI_SEL_NESTED
    IF SY-SUBRC EQ 0.
      PS_KEY-PARTNER_EXIST = GC_TRUE.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_main11
*&---------------------------------------------------------------------*
*& Validate Main Data
*&---------------------------------------------------------------------*
FORM F_VALIDATE_MAIN11 USING PS_KEY TYPE GTY_KEY11
                       CHANGING PS_MAIN TYPE  GTY_MAIN11
                                PS_MAINX TYPE GTY_MAIN11X
                                PS_MESSG TYPE GTY_MESSG.

*-Check One-time (No need to validate address)
  CLEAR GV_ONETIME .
  IF PS_KEY-BU_GROUP = 'Z304' .
    GV_ONETIME = GC_TRUE .
  ENDIF.

* ---------------------------------
*  Validate Address
* ---------------------------------
*  Validate Address
  PERFORM F_VALIDATE_ADDRESS11  USING  PS_KEY
                              CHANGING PS_MAIN-ADDR
                                       PS_MAINX-ADDR
                                       PS_MESSG.
  IF PS_MESSG IS NOT INITIAL.
*    EXIT.
    RETURN.
  ENDIF.

* ---------------------------------
*  Validate Address International
* ---------------------------------
*  Validate Address
  PERFORM F_VALIDATE_ADDRESS_INT11  USING  PS_KEY
                                  CHANGING PS_MAIN-ADDR_INT
                                           PS_MAINX-ADDR_INT
                                           PS_MESSG.
  IF PS_MESSG IS NOT INITIAL.
*    EXIT.
    RETURN.
  ENDIF.

* ---------------------------------
* Validate Bank
* ---------------------------------
* Validate Bank
  PERFORM F_VALIDATE_BANK  USING  PS_KEY
                         CHANGING PS_MAIN-BANK01
                                  PS_MAINX-BANK01
                                  PS_MESSG.
  IF PS_MESSG IS NOT INITIAL.
*    EXIT.
    RETURN.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_address11
*&---------------------------------------------------------------------*
*& Validate Address
*&---------------------------------------------------------------------*
FORM F_VALIDATE_ADDRESS11 USING PS_KEY TYPE GTY_KEY11
                         CHANGING PS_ADDR  TYPE  GTY_V_ADDR
                                  PS_ADDRX TYPE  GTY_V_ADDRX  ##NEEDED
                                  PS_MESSG TYPE  GTY_MESSG.

* Initialize Output
  CLEAR: PS_MESSG.

* Get GUID for Address if Exists
  CALL FUNCTION 'BAPI_BUPA_ADDRESS_GET_NUMBERS'
    EXPORTING
      BUSINESSPARTNER = PS_KEY-PARTNER
    IMPORTING
      ADDRESSGUIDOUT  = PS_ADDR-GUID.

  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
       PS_ADDRX-NAME1 EQ GC_TRUE ) AND
      PS_ADDR-NAME1 IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e44 : Missing Name 1 in the file.
    PS_MESSG-MSGTX = TEXT-E44.
    RETURN.
  ENDIF.

*  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
*       PS_ADDRX-SORT1 EQ GC_TRUE ) AND
*      PS_ADDR-SORT1 IS INITIAL.
*    PS_MESSG-MSGTY = 'E'.
*    PS_MESSG-MSGID = 'ZTEC'.
*    PS_MESSG-MSGNO = '000'.
**   Text-e45 : Missing Search Term 1 in the file.
*    PS_MESSG-MSGTX = TEXT-E45.
*    RETURN.
*  ENDIF.

  IF GV_ONETIME IS INITIAL. "Case one time no need to validate
    IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
         PS_ADDRX-CITY1 EQ GC_TRUE ) AND
        PS_ADDR-CITY1 IS INITIAL.
      PS_MESSG-MSGTY = 'E'.
      PS_MESSG-MSGID = 'ZTEC'.
      PS_MESSG-MSGNO = '000'.
*   Text-e47 : Missing City in the file.
      PS_MESSG-MSGTX = TEXT-E47.
      RETURN.
    ENDIF.

*  IF ( ps_key-partner_exist IS INITIAL OR
*       ps_addrx-post_code1 EQ gc_true ) AND
*      ps_addr-post_code1 IS INITIAL.
*    ps_messg-msgty = 'E'.
*    ps_messg-msgid = 'ZTEC'.
*    ps_messg-msgno = '000'.
**   Text-e19 : Missing Postal Code in the file.
*    ps_messg-msgtx = TEXT-e19.
*    RETURN.
*  ENDIF.

    IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
         PS_ADDRX-COUNTRY EQ GC_TRUE ) AND
        PS_ADDR-COUNTRY IS INITIAL.
      PS_MESSG-MSGTY = 'E'.
      PS_MESSG-MSGID = 'ZTEC'.
      PS_MESSG-MSGNO = '000'.
*   Text-e49 : Missing Country in the file.
      PS_MESSG-MSGTX = TEXT-E49.
      RETURN.
    ENDIF.
  ENDIF.

  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
       PS_ADDRX-LANGU EQ GC_TRUE ) AND
      PS_ADDR-LANGU IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e21 : Missing Language in the file.
    PS_MESSG-MSGTX = TEXT-E21.
    RETURN.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_address_int11
*&---------------------------------------------------------------------*
*& Validate Address International
*&---------------------------------------------------------------------*
FORM F_VALIDATE_ADDRESS_INT11 USING PS_KEY TYPE GTY_KEY11
                             CHANGING PS_ADDR  TYPE  GTY_V_ADDR_INT  ##NEEDED
                                      PS_ADDRX TYPE  GTY_V_ADDR_INTX ##NEEDED
                                      PS_MESSG TYPE  GTY_MESSG.

  CONSTANTS:
    LC_DEFAULT_NATION  TYPE  ADRC-NATION VALUE 'I'.


* Initialize Output
  CLEAR: PS_MESSG.

* Only when Active
  IF PS_ADDR-ACTIVE IS INITIAL.
    RETURN.
  ENDIF.

* Default Version
  IF PS_ADDR-NATION IS INITIAL.
    PS_ADDR-NATION = LC_DEFAULT_NATION.
  ENDIF.

* Case one time no need to validate address
  IF GV_ONETIME IS INITIAL .
    RETURN .
  ENDIF .

  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
       PS_ADDRX-NAME1 EQ GC_TRUE ) AND
      PS_ADDR-NAME1 IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e44 : Missing Name 1 in the file.
    PS_MESSG-MSGTX = TEXT-E44.
    RETURN.
  ENDIF.

*  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
*       PS_ADDRX-SORT1 EQ GC_TRUE ) AND
*      PS_ADDR-SORT1 IS INITIAL.
*    PS_MESSG-MSGTY = 'E'.
*    PS_MESSG-MSGID = 'ZTEC'.
*    PS_MESSG-MSGNO = '000'.
**   Text-e45 : Missing Search Term 1 in the file.
*    PS_MESSG-MSGTX = TEXT-E45.
*    RETURN.
*  ENDIF.

  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
       PS_ADDRX-CITY1 EQ GC_TRUE ) AND
      PS_ADDR-CITY1 IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e47 : Missing City in the file.
    PS_MESSG-MSGTX = TEXT-E47.
    RETURN.
  ENDIF.

*  IF ( ps_key-partner_exist IS INITIAL OR
*       ps_addrx-post_code1 EQ gc_true ) AND
*      ps_addr-post_code1 IS INITIAL.
*    ps_messg-msgty = 'E'.
*    ps_messg-msgid = 'ZTEC'.
*    ps_messg-msgno = '000'.
**   Text-e19 : Missing Postal Code in the file.
*    ps_messg-msgtx = TEXT-e19.
*    RETURN.
*  ENDIF.

  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
       PS_ADDRX-COUNTRY EQ GC_TRUE ) AND
      PS_ADDR-COUNTRY IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e49 : Missing Country in the file.
    PS_MESSG-MSGTX = TEXT-E49.
    RETURN.
  ENDIF.

  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
       PS_ADDRX-LANGU EQ GC_TRUE ) AND
      PS_ADDR-LANGU IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e51 : Missing Language in the file.
    PS_MESSG-MSGTX = TEXT-E51.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_bank
*&---------------------------------------------------------------------*
*& Validate Bank Data
*&---------------------------------------------------------------------*
FORM F_VALIDATE_BANK USING PS_KEY TYPE GTY_KEY11
                    CHANGING PS_BANK  TYPE  GTY_V_BANK
                             PS_BANKX TYPE  GTY_V_BANKX ##NEEDED
                             PS_MESSG TYPE  GTY_MESSG.

  DATA:
    LV_BKVID  TYPE  BUT0BK-BKVID.


* Only when data is filled
  IF PS_BANKX IS INITIAL.
    RETURN.
  ENDIF.

  IF PS_BANK-BKVID IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e50 : Missing Bank ID in the file.
    PS_MESSG-MSGTX = TEXT-E89.
    RETURN.
  ENDIF.

  IF PS_KEY-PARTNER_EXIST EQ GC_TRUE.
    SELECT SINGLE BKVID                              "#EC CI_SEL_NESTED
      INTO LV_BKVID
      FROM BUT0BK
     WHERE PARTNER  EQ  PS_KEY-PARTNER
       AND BKVID    EQ  PS_BANK-BKVID.
    IF SY-SUBRC EQ 0.
      PS_BANK-BANK_EXIST = GC_TRUE.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_comp
*&---------------------------------------------------------------------*
*& Validate Company view data
*&---------------------------------------------------------------------*
FORM F_VALIDATE_COMP USING PS_KEY TYPE GTY_KEY11
                             PS_VEND  TYPE  GTY_V_VEND
                    CHANGING PS_COMP  TYPE  GTY_V_COMP
                             PS_COMPX TYPE  GTY_V_COMPX ##NEEDED
                             PS_MESSG TYPE  GTY_MESSG.

  TYPES: BEGIN OF LTY_LFBW,
           WITHT TYPE  LFBW-WITHT,
         END OF LTY_LFBW.
  TYPES: LTTY_LFBW  TYPE  SORTED TABLE OF LTY_LFBW
                          WITH UNIQUE KEY WITHT.

  DATA:
    LT_LFBW   TYPE  LTTY_LFBW.

  DATA:
    LV_TEMP  TYPE  LFB1-LIFNR  ##NEEDED,
    LV_LIFNR TYPE  LFB1-LIFNR.


* Initialize Output
  CLEAR: PS_MESSG.

  IF PS_COMP-BUKRS IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e54 : Missing Company Code in the file.
    PS_MESSG-MSGTX = TEXT-E54.
    RETURN.
  ENDIF.

  IF PS_VEND-LIFNR IS NOT INITIAL.
    LV_LIFNR = PS_VEND-LIFNR.
  ELSE.
    LV_LIFNR = PS_KEY-PARTNER.
  ENDIF.

  SELECT SINGLE LIFNR
    INTO LV_TEMP
    FROM LFB1
   WHERE LIFNR EQ LV_LIFNR
     AND BUKRS EQ PS_COMP-BUKRS.                     "#EC CI_SEL_NESTED
  IF SY-SUBRC EQ 0.
    PS_COMP-EXIST = GC_TRUE.
  ENDIF.

  IF ( PS_COMP-EXIST IS INITIAL OR
       PS_COMPX-AKONT EQ GC_TRUE ) AND
     PS_COMP-AKONT IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e55 : Missing Reconciliation acct. in the file.
    PS_MESSG-MSGTX = TEXT-E55.
    RETURN.
  ENDIF.

  IF ( PS_COMP-EXIST IS INITIAL OR
       PS_COMPX-ZUAWA EQ GC_TRUE ) AND
     PS_COMP-ZUAWA IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e27 : Missing Sort Key in the file.
    PS_MESSG-MSGTX = TEXT-E27.
    RETURN.
  ENDIF.

*  IF ( ps_comp-exist IS INITIAL OR
*       ps_compx-fdgrv EQ gc_true ) AND
*     ps_comp-fdgrv IS INITIAL.
*    ps_messg-msgty = 'E'.
*    ps_messg-msgid = 'ZTEC'.
*    ps_messg-msgno = '000'.
**   Text-e28 : Missing Planning Group in the file.
*    ps_messg-msgtx = TEXT-e28.
*    RETURN.
*  ENDIF.

*  IF ( PS_COMP-EXIST IS INITIAL OR
*       PS_COMPX-REPRF IS NOT INITIAL ) AND
*     PS_COMP-REPRF IS INITIAL.
*    PS_MESSG-MSGTY = 'E'.
*    PS_MESSG-MSGID = 'ZTEC'.
*    PS_MESSG-MSGNO = '000'.
**   Text-e52: Missing Check Double Inv in the file.
**    PS_MESSG-MSGTX = TEXT-E52.
*    PS_MESSG-MSGTX = TEXT-E91.
*    RETURN.
*  ENDIF.

  IF ( PS_COMP-EXIST IS INITIAL OR
       PS_COMPX-BRANCH IS NOT INITIAL ) AND
     PS_COMP-BRANCH-BCODE IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e81: Missing Branch Code in the file.
    PS_MESSG-MSGTX = TEXT-E81.
    RETURN.
  ENDIF.

  IF ( PS_COMP-EXIST IS INITIAL OR
       PS_COMPX-BRANCH IS NOT INITIAL ) AND
     PS_COMP-BRANCH-BDESC IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e90: Missing Branch Desc. in the file.
    PS_MESSG-MSGTX = TEXT-E90.
    RETURN.
  ENDIF.

* Check Withhold Exists?
  IF PS_COMP-EXIST EQ GC_TRUE.
    SELECT WITHT
      INTO TABLE LT_LFBW
      FROM LFBW
     WHERE LIFNR  EQ  LV_LIFNR
       AND BUKRS  EQ  PS_COMP-BUKRS.                 "#EC CI_SEL_NESTED
    IF SY-SUBRC NE 0.
      CLEAR: LT_LFBW.
    ENDIF.

    READ TABLE LT_LFBW TRANSPORTING NO FIELDS
                       WITH KEY WITHT = PS_COMP-WTAX1-WITHT
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      PS_COMP-WTAX1-EXIST = GC_TRUE.
    ENDIF.
    READ TABLE LT_LFBW TRANSPORTING NO FIELDS
                       WITH KEY WITHT = PS_COMP-WTAX2-WITHT
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      PS_COMP-WTAX2-EXIST = GC_TRUE.
    ENDIF.

  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_purch
*&---------------------------------------------------------------------*
*& Validate Purchasing View
*&---------------------------------------------------------------------*
FORM F_VALIDATE_PURCH USING PS_KEY TYPE GTY_KEY11
                              PS_VEND   TYPE  GTY_V_VEND
                     CHANGING PS_PURCH  TYPE  GTY_V_PURCH
                              PS_PURCHX TYPE  GTY_V_PURCHX ##NEEDED
                              PS_MESSG  TYPE  GTY_MESSG.

  DATA:
    LV_TEMP  TYPE  LFM1-LIFNR  ##NEEDED,
    LV_LIFNR TYPE  LFM1-LIFNR.


* Initialize Output
  CLEAR: PS_MESSG.

  IF PS_PURCH-EKORG IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e31 : Missing Purchasing Org. in the file.
    PS_MESSG-MSGTX = TEXT-E31.
    RETURN.
  ENDIF.

  IF PS_VEND-LIFNR IS NOT INITIAL.
    LV_LIFNR = PS_VEND-LIFNR.
  ELSE.
    LV_LIFNR = PS_KEY-PARTNER.
  ENDIF.

  SELECT SINGLE LIFNR
    INTO LV_TEMP
    FROM LFM1
   WHERE LIFNR EQ LV_LIFNR
     AND EKORG EQ PS_PURCH-EKORG.                    "#EC CI_SEL_NESTED
  IF SY-SUBRC EQ 0.
    PS_PURCH-EXIST = GC_TRUE.
  ENDIF.

  IF ( PS_PURCH-EXIST IS INITIAL OR
       PS_PURCHX-WAERS EQ GC_TRUE ) AND
     PS_PURCH-WAERS IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e32 : Missing Order Currency in the file.
    PS_MESSG-MSGTX = TEXT-E32.
    RETURN.
  ENDIF.

  IF ( PS_PURCH-EXIST IS INITIAL OR
       PS_PURCHX-ZTERM EQ GC_TRUE ) AND
     PS_PURCH-ZTERM IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e33 : Missing Term of Payment in the file.
    PS_MESSG-MSGTX = TEXT-E33.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_currkey
*&---------------------------------------------------------------------*
*& Validate Currency Key
*&---------------------------------------------------------------------*
FORM F_VALIDATE_CURRKEY USING PV_STRING TYPE STRING
                       CHANGING PV_WAERS   TYPE  TCURC-WAERS
                                PV_MSGTX   TYPE  CLIKE.

  TYPES: BEGIN OF LTY_TCURC,
           WAERS TYPE  TCURC-WAERS,
         END OF LTY_TCURC.
  TYPES: LTTY_TCURC  TYPE  SORTED TABLE OF LTY_TCURC
                           WITH UNIQUE KEY WAERS.

  STATICS:
    LS_TCURC TYPE  LTY_TCURC,
    LT_TCURC TYPE  LTTY_TCURC.

  DATA:
    LV_WAERS TYPE  LTY_TCURC-WAERS.


* Initialize Output
  CLEAR: PV_WAERS,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 5
  IF STRLEN( PV_STRING ) GT 5.
*   Text-e06 : Invalid Currency:
    CONCATENATE TEXT-E06 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LV_WAERS = PV_STRING.

* Check Buffer
  IF LS_TCURC-WAERS NE LV_WAERS.
*   Validate with Memory
    READ TABLE LT_TCURC INTO LS_TCURC
                        WITH KEY WAERS = LV_WAERS
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TCURC.
*     Validate with Database
      SELECT SINGLE WAERS
        INTO LS_TCURC
        FROM TCURC
       WHERE WAERS EQ LV_WAERS.
      IF SY-SUBRC NE 0.
*       Text-e06 : Invalid Currency:
        CONCATENATE TEXT-E06 PV_STRING
               INTO PV_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TCURC INTO TABLE LT_TCURC.
    ENDIF.

  ENDIF.

* Assign Output
  PV_WAERS = LS_TCURC-WAERS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_plan_grp
*&---------------------------------------------------------------------*
*& Validate Planning Group
*&---------------------------------------------------------------------*
FORM F_VALIDATE_PLAN_GRP USING PV_STRING TYPE STRING
                        CHANGING PV_GRUPP   TYPE  T035-GRUPP
                                 PV_MSGTX   TYPE  CLIKE.

  TYPES: BEGIN OF LTY_T035,
           GRUPP TYPE  T035-GRUPP,
         END OF LTY_T035.
  TYPES: LTTY_T035  TYPE  SORTED TABLE OF LTY_T035
                           WITH UNIQUE KEY GRUPP.

  STATICS:
    LS_T035 TYPE  LTY_T035,
    LT_T035 TYPE  LTTY_T035.

  DATA:
    LV_LEN   TYPE  I,
    LV_GRUPP TYPE  LTY_T035-GRUPP.


* Initialize Output
  CLEAR: PV_GRUPP,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 10
  LV_LEN = STRLEN( PV_STRING ).
  IF LV_LEN GT 10.
*   Text-e11 : Invalid Planning Group:
    CONCATENATE TEXT-E11 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = PV_STRING
    IMPORTING
      OUTPUT = LV_GRUPP.

* Check Buffer
  IF LS_T035-GRUPP NE LV_GRUPP.

*   Validate with Memory
    READ TABLE LT_T035 INTO LS_T035
                        WITH KEY GRUPP = LV_GRUPP
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_T035.
*     Validate with Database
      SELECT SINGLE GRUPP
        INTO LS_T035
        FROM T035
       WHERE GRUPP  EQ LV_GRUPP.
      IF SY-SUBRC NE 0.
*       Text-e11 : Invalid Planning Group:
        CONCATENATE TEXT-E11 PV_STRING
               INTO PV_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_T035 INTO TABLE LT_T035.
    ENDIF.

  ENDIF.

* Assign Output
  PV_GRUPP = LS_T035-GRUPP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_purch_org
*&---------------------------------------------------------------------*
*& Validate Purchasing Org
*&---------------------------------------------------------------------*
FORM F_VALIDATE_PURCH_ORG USING PV_STRING TYPE STRING
                         CHANGING PV_EKORG   TYPE  T024E-EKORG
                                  PV_MSGTX   TYPE  CLIKE.

  TYPES: BEGIN OF LTY_T024E,
           EKORG TYPE  T024E-EKORG,
         END OF LTY_T024E.
  TYPES: LTTY_T024E  TYPE  SORTED TABLE OF LTY_T024E
                           WITH UNIQUE KEY EKORG.

  STATICS:
    LS_T024E TYPE  LTY_T024E,
    LT_T024E TYPE  LTTY_T024E.

  DATA:
    LV_LEN   TYPE  I,
    LV_EKORG TYPE  LTY_T024E-EKORG.


* Initialize Output
  CLEAR: PV_EKORG,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 10
  LV_LEN = STRLEN( PV_STRING ).
  IF LV_LEN GT 10.
*   Text-e12 : Invalid Purchasing Org.:
    CONCATENATE TEXT-E12 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = PV_STRING
    IMPORTING
      OUTPUT = LV_EKORG.

* Check Buffer
  IF LS_T024E-EKORG NE LV_EKORG.

*   Validate with Memory
    READ TABLE LT_T024E INTO LS_T024E
                        WITH KEY EKORG = LV_EKORG
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_T024E.
*     Validate with Database
      SELECT SINGLE EKORG
        INTO LS_T024E
        FROM T024E
       WHERE EKORG  EQ LV_EKORG.
      IF SY-SUBRC NE 0.
*       Text-e12 : Invalid Purchasing Org.:
        CONCATENATE TEXT-E12 PV_STRING
               INTO PV_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_T024E INTO TABLE LT_T024E.
    ENDIF.

  ENDIF.

* Assign Output
  PV_EKORG = LS_T024E-EKORG.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_vend
*&---------------------------------------------------------------------*
*& Validate Vendor view data
*&---------------------------------------------------------------------*
FORM F_VALIDATE_VEND USING PS_KEY TYPE GTY_KEY11
                              PV_BUKRS TYPE  GTY_V_COMP-BUKRS
                    CHANGING  PS_VEND  TYPE  GTY_V_VEND
                              PS_VENDX TYPE  GTY_V_VENDX ##NEEDED
                              PS_MESSG TYPE  GTY_MESSG.

  DATA:
    LT_RETURN TYPE  BAPIRET2_T.

  FIELD-SYMBOLS:
    <LFS_RETURN>  TYPE  BAPIRET2.


* Initialize Output
  CLEAR: PS_MESSG.

* Validate Grouping for Customer
  CALL METHOD FSBP_BO_CVI=>VALIDATE_GROUPING_FOR_VEND
    EXPORTING
      I_GROUPING = PS_KEY-BU_GROUP
    RECEIVING
      R_MESSAGES = LT_RETURN.
  IF LT_RETURN IS NOT INITIAL.
    LOOP AT LT_RETURN ASSIGNING <LFS_RETURN>
                      WHERE TYPE EQ 'E' OR
                            TYPE EQ 'A'.
      PS_MESSG-MSGTY = 'E'.
      PS_MESSG-MSGID = <LFS_RETURN>-ID.
      PS_MESSG-MSGNO = <LFS_RETURN>-NUMBER.
      PS_MESSG-MSGTX = <LFS_RETURN>-MESSAGE.
      EXIT.
    ENDLOOP.
    IF SY-SUBRC EQ 0.
      RETURN.
    ENDIF.
  ENDIF.

* Get Vendor from Partner
  PERFORM F_GET_VENDOR_FROM_PARTNER  USING  PS_KEY-PARTNER
                                   CHANGING PS_VEND-LIFNR.
  IF PS_VEND-LIFNR IS NOT INITIAL.
    PS_VEND-EXIST = GC_TRUE.

    "Check Alt Payee
    SELECT EMPFK INTO @DATA(LV_EMPFK) UP TO 1 ROWS ##NEEDED
      FROM LFZA
     WHERE LIFNR  EQ @PS_VEND-LIFNR
       AND BUKRS  EQ @PV_BUKRS.
    ENDSELECT.
    IF SY-SUBRC EQ 0 .
      PS_VEND-PAYEE_EXIST = GC_TRUE.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_vendor_from_partner
*&---------------------------------------------------------------------*
*& Get Vendor code from partner
*&---------------------------------------------------------------------*
*&      --> PS_KEY_PARTNER
*&      <-- PS_VEND_LIFNR
*&---------------------------------------------------------------------*
FORM F_GET_VENDOR_FROM_PARTNER USING PV_PARTNER TYPE BUT000-PARTNER
                              CHANGING PV_LIFNR    TYPE  LFA1-LIFNR.

* Initlialize Output
  CLEAR: PV_LIFNR.

  SELECT SINGLE LIFNR
    INTO PV_LIFNR
    FROM LFA1
   WHERE LIFNR EQ PV_PARTNER.                        "#EC CI_SEL_NESTED
  IF SY-SUBRC NE 0.
*   Try to find Vendor Code
    SELECT A~VENDOR
        UP TO 1 ROWS
      INTO PV_LIFNR
      FROM CVI_VEND_LINK AS A
             INNER JOIN BUT000 AS B
               ON  B~PARTNER_GUID = A~PARTNER_GUID
     WHERE B~PARTNER EQ PV_PARTNER
     ORDER BY A~VENDOR ASCENDING.                    "#EC CI_SEL_NESTED
    ENDSELECT.
    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_maintain_partner11
*&---------------------------------------------------------------------*
*& Maintain Business Partner Vendor
*&---------------------------------------------------------------------*
FORM F_MAINTAIN_PARTNER11 USING PV_TEST TYPE FLAG
                       CHANGING PS_DATA  TYPE  GTY_DATA11
                                PV_PARTNER  TYPE  GTY_DATA11-KEY-PARTNER
                                PT_MESSG TYPE  GTTY_MESSG
                                PV_ERROR TYPE  FLAG.

  CONSTANTS:
    LC_DUMMY_PARTNER  TYPE  GTY_DATA11-KEY-PARTNER VALUE '1'.

  DATA:
    LT_BAPI_DATA TYPE  CVIS_EI_EXTERN_T,
    LT_MSGMAP    TYPE  MDG_BS_BP_MSGMAP_T,
    LT_RETURN    TYPE  BAPIRETM.

  DATA:
    LS_BAPI_DATA TYPE  CVIS_EI_EXTERN,
    LS_BAPI_ROLE TYPE  BUS_EI_BUPA_ROLES.

  DATA:
    LV_RLTYP_EXIST TYPE FLAG,
    LV_VEND_EXIST  TYPE FLAG,
    LV_VEND        TYPE FLAG,
    LV_PURCH       TYPE FLAG.

  FIELD-SYMBOLS:
    <LFS_ROLE>   TYPE  GTY_V_ROLE.


* Initialize Output
  CLEAR: PT_MESSG.
  CLEAR:   PV_PARTNER,
           PV_ERROR.

  CLEAR LS_BAPI_DATA.

  DATA: LS_LOG_PARTNER TYPE GTYP_LOG_PARTNER,
        LS_LOG_BANK    TYPE GTYP_LOG_BANK,
        LS_LOG_COMP    TYPE GTYP_LOG_COMP,
        LS_LOG_PAYEE   TYPE GTYP_LOG_PAYEE,
        LS_LOG_EXTEND  TYPE GTYP_LOG_EXTEND.

  SORT: GT_LOG_PARTNER BY PARTNER ,
        GT_LOG_BANK    BY PARTNER BKVID,
        GT_LOG_COMP    BY PARTNER BUKRS WITHT1 WITHT2 ,
        GT_LOG_PAYEE   BY PARTNER BUKRS EMPFK ,
        GT_LOG_EXTEND  BY PARTNER BUKRS .

  DELETE ADJACENT DUPLICATES FROM: GT_LOG_PARTNER COMPARING PARTNER ,
                                   GT_LOG_BANK    COMPARING PARTNER BKVID,
                                   GT_LOG_COMP    COMPARING PARTNER BUKRS WITHT1 WITHT2 ,
                                   GT_LOG_PAYEE   COMPARING PARTNER BUKRS EMPFK ,
                                   GT_LOG_EXTEND  COMPARING PARTNER BUKRS .

  READ TABLE GT_LOG_PARTNER INTO LS_LOG_PARTNER
    WITH KEY PARTNER = PS_DATA-KEY-PARTNER
              BINARY SEARCH .
  IF SY-SUBRC = 0 .
    PS_DATA-KEY-PARTNER_EXIST = GC_TRUE.
    PS_DATA-KEY-PARTNER_GUID = LS_LOG_PARTNER-GUID.
    PS_DATA-MAIN-ADDR-GUID   = LS_LOG_PARTNER-GUID_ADDR .
  ENDIF.

  READ TABLE GT_LOG_BANK INTO LS_LOG_BANK
  WITH KEY PARTNER = PS_DATA-KEY-PARTNER
           BKVID   = PS_DATA-MAIN-BANK01-BKVID
              BINARY SEARCH .
  IF SY-SUBRC = 0 .
    PS_DATA-MAIN-BANK01-BANK_EXIST = GC_TRUE.
  ENDIF.

  READ TABLE GT_LOG_COMP INTO LS_LOG_COMP
  WITH KEY PARTNER = PS_DATA-KEY-PARTNER
           BUKRS = PS_DATA-MAIN-COMP-BUKRS
              BINARY SEARCH .
  IF SY-SUBRC = 0 .
    PS_DATA-MAIN-COMP-EXIST = GC_TRUE.

    IF PS_DATA-MAIN-COMP-WTAX1-WITHT = LS_LOG_COMP-WITHT1 .
      PS_DATA-MAIN-COMP-WTAX1-EXIST = GC_TRUE.
    ENDIF.

    IF PS_DATA-MAIN-COMP-WTAX2-WITHT = LS_LOG_COMP-WITHT2 .
      PS_DATA-MAIN-COMP-WTAX2-EXIST = GC_TRUE.
    ENDIF.
  ENDIF.

  READ TABLE GT_LOG_PAYEE INTO LS_LOG_PAYEE
  WITH KEY PARTNER = PS_DATA-KEY-PARTNER
           BUKRS = PS_DATA-MAIN-COMP-BUKRS
           EMPFK = PS_DATA-MAIN-VEND-EMPFK
              BINARY SEARCH .
  IF SY-SUBRC = 0 .
    PS_DATA-MAIN-VEND-PAYEE_EXIST = GC_TRUE.
  ENDIF.

* ---------------------------
* Assign Header Data
* ---------------------------
  CASE GV_MODE.
    WHEN GC_MODE_CREATE.
      IF PS_DATA-KEY-PARTNER_EXIST IS INITIAL .
        LS_BAPI_DATA-PARTNER-HEADER-OBJECT_TASK = 'I'.
      ELSE.
        LS_BAPI_DATA-PARTNER-HEADER-OBJECT_TASK = 'U'.
      ENDIF.
    WHEN GC_MODE_CHANGE.
      LS_BAPI_DATA-PARTNER-HEADER-OBJECT_TASK = 'U'.
  ENDCASE.
  IF PS_DATA-KEY-PARTNER IS NOT INITIAL.
    LS_BAPI_DATA-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNER = PS_DATA-KEY-PARTNER.
  ELSE.
    LS_BAPI_DATA-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNER = LC_DUMMY_PARTNER.
  ENDIF.
  LS_BAPI_DATA-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNERGUID = PS_DATA-KEY-PARTNER_GUID.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CONTROL-CATEGORY = PS_DATA-KEY-TYPE.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CONTROL-GROUPING = PS_DATA-KEY-BU_GROUP.

* ---------------------------
* Assign Role
* ---------------------------
  CLEAR: LV_VEND,
         LV_PURCH.
  LOOP AT PS_DATA-KEY-ROLE ASSIGNING <LFS_ROLE>
                           WHERE RLTYP NE GC_ROLE1_11.

    IF <LFS_ROLE>-RLTYP EQ GC_ROLE1_12.
      LV_VEND = GC_TRUE.
    ELSEIF <LFS_ROLE>-RLTYP EQ GC_ROLE1_13.
      LV_PURCH = GC_TRUE.
    ENDIF.

*   Re-check again since previous post may create role already
    PERFORM F_CHECK_ROLE_EXIST  USING  PS_DATA-KEY-PARTNER
                                       <LFS_ROLE>-RLTYP
                              CHANGING LV_RLTYP_EXIST.
    IF LV_RLTYP_EXIST IS INITIAL.
      LS_BAPI_ROLE-TASK = 'I'.
    ELSE.
      LS_BAPI_ROLE-TASK = 'U'.
    ENDIF.
    LS_BAPI_ROLE-DATA_KEY          = <LFS_ROLE>-RLTYP.
    LS_BAPI_ROLE-DATA-ROLECATEGORY = <LFS_ROLE>-RLTYP.
    IF <LFS_ROLE>-RLTYP IN GR_RLTYP .
      LS_BAPI_ROLE-DATA-VALID_FROM   = GV_VALIDFROM .
    ELSE.
      LS_BAPI_ROLE-DATA-VALID_FROM   = SY-DATUM.
    ENDIF.
    LS_BAPI_ROLE-DATA-VALID_TO     = '99991231'.
    LS_BAPI_ROLE-DATAX-VALID_FROM  = GC_TRUE.
    LS_BAPI_ROLE-DATAX-VALID_TO    = GC_TRUE.
    INSERT LS_BAPI_ROLE INTO TABLE LS_BAPI_DATA-PARTNER-CENTRAL_DATA-ROLE-ROLES.

  ENDLOOP.

* ---------------------------
* Assign Common Data
* ---------------------------
* Name + Sort Key
*  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-NAME1  = PS_DATA-MAIN-ADDR-NAME1.
*  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-NAME1 = PS_DATA-MAINX-ADDR-NAME1.
*  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-NAME2  = PS_DATA-MAIN-ADDR-NAME2.
*  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-NAME2 = PS_DATA-MAINX-ADDR-NAME2.
*  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-NAME3  = PS_DATA-MAIN-ADDR-NAME3.
*  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-NAME3 = PS_DATA-MAINX-ADDR-NAME3.
*  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-NAME4  = PS_DATA-MAIN-ADDR-NAME4.
*  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-NAME4 = PS_DATA-MAINX-ADDR-NAME4.

  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-SEARCHTERM1  = PS_DATA-MAIN-ADDR-SORT1.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-SEARCHTERM1 = PS_DATA-MAINX-ADDR-SORT1.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-SEARCHTERM2  = PS_DATA-MAIN-ADDR-SORT2.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-SEARCHTERM2 = PS_DATA-MAINX-ADDR-SORT2.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-PARTNERLANGUAGE  = PS_DATA-MAIN-ADDR-LANGU.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-PARTNERLANGUAGE = PS_DATA-MAINX-ADDR-LANGU.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-PARTNEREXTERNAL  = PS_DATA-MAIN-BPEXT.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-PARTNEREXTERNAL = PS_DATA-MAINX-BPEXT.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-PARTNERTYPE  = PS_DATA-MAIN-BPKIND.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-PARTNERTYPE = PS_DATA-MAINX-BPKIND.
* ---------------------------
* Assign Address Data
* ---------------------------
*  PERFORM F_ASSIGN_BAPI_ADDRESS11  USING  PS_DATA-KEY-TYPE
*                                          PS_DATA-MAIN-ADDR
*                                          PS_DATA-MAINX-ADDR
*                                          PS_DATA-MAIN-ADDR_INT
*                                          PS_DATA-MAINX-ADDR_INT
*                                          PS_DATA-MAIN-BPEXT
*                                 CHANGING LS_BAPI_DATA-PARTNER-CENTRAL_DATA-ADDRESS
*                                          GV_ZZCONTEXT.

* ---------------------------
* Assign Tax Data
* ---------------------------
  PERFORM F_ASSIGN_BAPI_TAX11  USING  PS_DATA-MAIN-TAX
                                    PS_DATA-MAINX-TAX
                           CHANGING LS_BAPI_DATA-PARTNER-CENTRAL_DATA-TAXNUMBER.

* ---------------------------
* Assign Bank Detail Data
* ---------------------------
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-BANKDETAIL-TIME_DEPENDENT = GC_TRUE.
*  PERFORM F_ASSIGN_BAPI_BANKDETAIL11  USING PS_DATA-MAIN-BANK01
*                                            PS_DATA-MAINX-BANK01
*                                   CHANGING LS_BAPI_DATA-PARTNER-CENTRAL_DATA-BANKDETAIL-BANKDETAILS.

*  PERFORM f_assign_bapi_bankdetail11  USING ps_data-main-bank02
*                                            ps_data-mainx-bank02
*                                   CHANGING ls_bapi_data-partner-central_data-bankdetail-bankdetails.
*
*  PERFORM f_assign_bapi_bankdetail11  USING ps_data-main-bank03
*                                            ps_data-mainx-bank03
*                                   CHANGING ls_bapi_data-partner-central_data-bankdetail-bankdetails.
*
*  PERFORM f_assign_bapi_bankdetail11  USING ps_data-main-bank04
*                                            ps_data-mainx-bank04
*                                   CHANGING ls_bapi_data-partner-central_data-bankdetail-bankdetails.
*
*  PERFORM f_assign_bapi_bankdetail11  USING ps_data-main-bank05
*                                            ps_data-mainx-bank05
*                                   CHANGING ls_bapi_data-partner-central_data-bankdetail-bankdetails.
*
*  PERFORM f_assign_bapi_bankdetail11  USING ps_data-main-bank06
*                                            ps_data-mainx-bank06
*                                   CHANGING ls_bapi_data-partner-central_data-bankdetail-bankdetails.

  CLEAR LV_VEND_EXIST.
  IF LV_VEND EQ GC_TRUE OR
     LV_PURCH EQ GC_TRUE.
*   Re-check again since previous post may create role already
    PERFORM F_CHECK_VENDOR_EXIST  USING  PS_DATA-KEY-PARTNER
                                CHANGING LV_VEND_EXIST.
  ENDIF.

* ---------------------------
* Assign Vendor Data
* ---------------------------
  IF LV_VEND EQ GC_TRUE.

    LS_BAPI_DATA-PARTNER-FINSERV_DATA-COMMON-DATA-FSBP_CENTRL-VBUND  = PS_DATA-MAIN-VEND-VBUND.
    LS_BAPI_DATA-PARTNER-FINSERV_DATA-COMMON-DATAX-FSBP_CENTRL-VBUND = PS_DATA-MAINX-VEND-VBUND.

*   Not assign vendor code for Some Group
    IF NOT ( PS_DATA-KEY-BU_GROUP EQ GC_BU_GROUP_1_V OR
             PS_DATA-KEY-BU_GROUP EQ GC_BU_GROUP_2_V ).
      LS_BAPI_DATA-VENDOR-HEADER-OBJECT_INSTANCE = PS_DATA-KEY-PARTNER.
    ENDIF.

    IF LV_VEND_EXIST IS INITIAL .
      LS_BAPI_DATA-VENDOR-HEADER-OBJECT_TASK = 'I'.
    ELSE.
      LS_BAPI_DATA-VENDOR-HEADER-OBJECT_TASK = 'U'.
      LS_BAPI_DATA-VENDOR-HEADER-OBJECT_INSTANCE = PS_DATA-MAIN-VEND-LIFNR.
    ENDIF.
*    PERFORM F_ASSIGN_BAPI_VENDOR  USING  PS_DATA-MAIN-VEND
*                                         PS_DATA-MAINX-VEND
*                                CHANGING PS_DATA-MAIN-COMP
*                                         PS_DATA-MAINX-COMP
*                                         LS_BAPI_DATA-VENDOR.
    PERFORM F_ASSIGN_BAPI_VENDOR  USING  PS_DATA-MAIN-VEND
                                         PS_DATA-MAINX-VEND
                                         PS_DATA-MAIN-COMP
                                         PS_DATA-MAINX-COMP
                                CHANGING LS_BAPI_DATA-VENDOR.
  ENDIF.

* ---------------------------
* Assign Purchasing Data
* ---------------------------
  IF LV_PURCH EQ GC_TRUE.
*   Not assign vendor code for Some Group
    IF NOT ( PS_DATA-KEY-BU_GROUP EQ GC_BU_GROUP_1_V OR
             PS_DATA-KEY-BU_GROUP EQ GC_BU_GROUP_2_V ).
      LS_BAPI_DATA-VENDOR-HEADER-OBJECT_INSTANCE = PS_DATA-KEY-PARTNER.
    ENDIF.

    IF LV_VEND_EXIST IS INITIAL .
      LS_BAPI_DATA-VENDOR-HEADER-OBJECT_TASK = 'I'.
    ELSE.
      LS_BAPI_DATA-VENDOR-HEADER-OBJECT_TASK = 'U'.
      LS_BAPI_DATA-VENDOR-HEADER-OBJECT_INSTANCE = PS_DATA-MAIN-VEND-LIFNR.
    ENDIF.
    PERFORM F_ASSIGN_BAPI_PURCH11  USING  PS_DATA-MAIN-PURCH
                                        PS_DATA-MAINX-PURCH
                               CHANGING LS_BAPI_DATA-VENDOR.
  ENDIF.

* Validate Data
  CALL METHOD CL_MD_BP_MAINTAIN=>VALIDATE_SINGLE
    EXPORTING
      I_DATA        = LS_BAPI_DATA
    IMPORTING
      ET_RETURN_MAP = LT_MSGMAP.
* To ensure, all update has been reverted.
* - This is to fix issue vendor number skip
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  IF LT_MSGMAP IS INITIAL.

    INSERT LS_BAPI_DATA INTO TABLE LT_BAPI_DATA.

*   Post Data
    CALL METHOD CL_MD_BP_MAINTAIN=>MAINTAIN
      EXPORTING
        I_DATA     = LT_BAPI_DATA
        I_TEST_RUN = PV_TEST
      IMPORTING
        E_RETURN   = LT_RETURN.
    PERFORM F_CHECK_BAPI_RETURN  USING  LT_RETURN
                                        PV_TEST
                               CHANGING PV_ERROR
                                        PT_MESSG.
    IF PV_TEST IS INITIAL AND
       PV_ERROR IS INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = GC_TRUE.

*     Get Result Partner no.
      IMPORT LV_PARTNER TO PV_PARTNER
                        FROM MEMORY ID 'BUP_MEMORY_PARTNER'.
*     Update Additional Data which Method does not support
      PERFORM F_UPDATE_ADDITIONAL11  USING  PV_PARTNER
                                          PS_DATA
                                 CHANGING PV_ERROR
                                          PT_MESSG.

*---------------Keep log data--------------------------
      CLEAR: LS_LOG_PARTNER , LS_LOG_COMP ,
             LS_LOG_BANK .

      LS_LOG_PARTNER-PARTNER   = PS_DATA-KEY-PARTNER .

**  Implement suitable error handling here
      CALL FUNCTION 'BAPI_BUPA_GET_NUMBERS'
        EXPORTING
          BUSINESSPARTNER        = PS_DATA-KEY-PARTNER
        IMPORTING
*         BUSINESSPARTNEROUT     =
          BUSINESSPARTNERGUIDOUT = LS_LOG_PARTNER-GUID.


      CALL FUNCTION 'BAPI_BUPA_ADDRESS_GET_NUMBERS'
        EXPORTING
          BUSINESSPARTNER = PS_DATA-KEY-PARTNER
        IMPORTING
          ADDRESSGUIDOUT  = LS_LOG_PARTNER-GUID_ADDR.

      APPEND LS_LOG_PARTNER TO GT_LOG_PARTNER .

      LS_LOG_BANK-PARTNER =  PS_DATA-KEY-PARTNER .
      LS_LOG_BANK-BKVID   =  PS_DATA-MAIN-BANK01-BKVID .
      APPEND LS_LOG_BANK TO GT_LOG_BANK .

      LS_LOG_COMP-PARTNER =  PS_DATA-KEY-PARTNER  .
      LS_LOG_COMP-BUKRS   =  PS_DATA-MAIN-COMP-BUKRS .
      LS_LOG_COMP-WITHT1  =  PS_DATA-MAIN-COMP-WTAX1-WITHT.
      LS_LOG_COMP-WITHT2  =  PS_DATA-MAIN-COMP-WTAX2-WITHT .
      APPEND LS_LOG_COMP TO GT_LOG_COMP .

      LS_LOG_PAYEE-PARTNER =  PS_DATA-KEY-PARTNER  .
      LS_LOG_PAYEE-BUKRS   =  PS_DATA-MAIN-COMP-BUKRS .
      LS_LOG_PAYEE-EMPFK   =  PS_DATA-MAIN-VEND-EMPFK .
      APPEND LS_LOG_PAYEE TO GT_LOG_PAYEE .

      CLEAR LS_LOG_EXTEND .
      LS_LOG_EXTEND-PARTNER = PS_DATA-KEY-PARTNER.
      LS_LOG_EXTEND-BUKRS   = PS_DATA-MAIN-COMP-BUKRS.
      APPEND LS_LOG_EXTEND TO GT_LOG_EXTEND .

      SORT: GT_LOG_PARTNER BY PARTNER ,
            GT_LOG_BANK    BY PARTNER BKVID,
            GT_LOG_COMP    BY PARTNER BUKRS WITHT1 WITHT2 ,
            GT_LOG_PAYEE   BY PARTNER BUKRS EMPFK ,
            GT_LOG_EXTEND  BY PARTNER BUKRS .


    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ELSE.
    PERFORM F_CHECK_BAPI_RETURNMAP11  USING LT_MSGMAP
                                   CHANGING PV_ERROR
                                            PT_MESSG.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_additional11
*&---------------------------------------------------------------------*
*& Update Additional Data which method does not support
*&---------------------------------------------------------------------*
FORM F_UPDATE_ADDITIONAL11 USING PV_PARTNER TYPE BUT000-PARTNER
                                   PS_DATA     TYPE  GTY_DATA11
                          CHANGING PV_ERROR    TYPE  FLAG
                                   PT_MESSG    TYPE  GTTY_MESSG.

  DATA:
    LT_MESSG  TYPE  GTTY_MESSG.

  DATA:
    LV_ERROR  TYPE  FLAG.


* Only when Independent Address exist
  IF PS_DATA-MAINX-ADDR_IND IS NOT INITIAL.
    PERFORM F_UPDATE_INDEPENDENT_ADDRESS  USING  PV_PARTNER
                                                 PS_DATA-MAIN-ADDR
                                                 PS_DATA-MAIN-ADDR_IND
                                                 PS_DATA-MAINX-ADDR_IND
                                        CHANGING LV_ERROR
                                                 LT_MESSG.
    IF LV_ERROR IS NOT INITIAL.
      PV_ERROR = LV_ERROR.
      APPEND LINES OF LT_MESSG TO PT_MESSG.
    ENDIF.
  ENDIF.

* Only for Vendor View
  READ TABLE PS_DATA-KEY-ROLE TRANSPORTING NO FIELDS
                              WITH KEY RLTYP = GC_ROLE1_12.
  IF SY-SUBRC EQ 0.
*   -----------------------------------
*   Update Brach Code
*   -----------------------------------
    PERFORM F_UPDATE_VEND_BRANNCH  USING  PV_PARTNER
                                          PS_DATA-MAIN-COMP-BRANCH
                                          PS_DATA-MAINX-COMP-BRANCH
                                 CHANGING LV_ERROR
                                          LT_MESSG.
    IF LV_ERROR IS NOT INITIAL.
      PV_ERROR = LV_ERROR.
      APPEND LINES OF LT_MESSG TO PT_MESSG.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_vend_brannch
*&---------------------------------------------------------------------*
*&   Update Vendor Branch Data
*&---------------------------------------------------------------------*
FORM F_UPDATE_VEND_BRANNCH USING PV_PARTNER TYPE BUT000-PARTNER
                                   PS_BRANCH   TYPE  GTY_V_BRANCH
                                   PS_BRANCHX  TYPE  GTY_V_BRANCHX
                          CHANGING PV_ERROR    TYPE  FLAG
                                   PT_MESSG    TYPE  GTTY_MESSG.

  DATA:
    LT_CODE     TYPE  CL_FINLOC_CVI_PERSIST_DATA=>TT_FITHA_PBUPL_K,
    LT_CODE_DEL TYPE  CL_FINLOC_CVI_PERSIST_DATA=>TT_FITHA_PBUPL_K,
    LT_DESC     TYPE  CL_FINLOC_CVI_PERSIST_DATA=>TT_FITHA_PBUPL_K_T,
    LT_DESC_DEL TYPE  CL_FINLOC_CVI_PERSIST_DATA=>TT_FITHA_PBUPL_K_T,
    LT_BRANCH   TYPE  TABLE OF FITHA_PBUPL_K , "OCCURS 0 WITH HEADER LINE,
    LT_BRANCH_T TYPE  TABLE OF FITHA_PBUPL_K_T. " OCCURS 0 WITH HEADER LINE.

  DATA:
    LS_CODE TYPE  FITHA_PBUPL_K,
    LS_DESC TYPE  FITHA_PBUPL_K_T.

  DATA:
    LV_LIFNR  TYPE  LFA1-LIFNR.


* Initialize Output
  CLEAR: PV_ERROR.
  CLEAR: PT_MESSG.

* Only when Branch is fill
  IF PS_BRANCHX IS INITIAL.
    RETURN.
  ENDIF.

* Get Vendor from Partner
  PERFORM F_GET_VENDOR_FROM_PARTNER  USING  PV_PARTNER
                                   CHANGING LV_LIFNR.
  IF LV_LIFNR IS INITIAL.
    RETURN.
  ENDIF.

  SELECT * FROM FITHA_PBUPL_K INTO TABLE LT_BRANCH
    WHERE LIFNR EQ LV_LIFNR.

  SELECT * FROM FITHA_PBUPL_K_T INTO TABLE LT_BRANCH_T
    WHERE LIFNR EQ LV_LIFNR
    AND   SPRAS EQ SY-LANGU.

  READ TABLE LT_BRANCH INTO DATA(LS_BRANCH) WITH KEY LIFNR = LV_LIFNR
                                                    J_1TPBUPL = PS_BRANCH-BCODE.
  IF SY-SUBRC EQ 0.
    RETURN.

  ELSE.

    IF LT_BRANCH[] IS INITIAL.

* Assign Code
      CLEAR LS_CODE.
      LS_CODE-LIFNR     = LV_LIFNR.
      LS_CODE-J_1TPBUPL = PS_BRANCH-BCODE.
      IF PS_BRANCHX-BDEFT EQ GC_TRUE.
        LS_CODE-DEFAULT_BRANCH = PS_BRANCH-BDEFT.
      ENDIF.
      INSERT LS_CODE INTO TABLE LT_CODE.

    ELSE.

      LOOP AT LT_BRANCH INTO LS_BRANCH ##INTO_OK.
        CLEAR LS_CODE.
        LS_CODE-LIFNR           = LV_LIFNR.
        LS_CODE-J_1TPBUPL       = LS_BRANCH-J_1TPBUPL.
        LS_CODE-DEFAULT_BRANCH  = LS_BRANCH-DEFAULT_BRANCH.
        INSERT LS_CODE INTO TABLE LT_CODE.

      ENDLOOP.

      CLEAR LS_CODE.
      LS_CODE-LIFNR     = LV_LIFNR.
      LS_CODE-J_1TPBUPL = PS_BRANCH-BCODE.
      INSERT LS_CODE INTO TABLE LT_CODE.

    ENDIF.

  ENDIF.


  CALL FUNCTION 'LVTH_BUPA_UPDATE_BRANCH_CODE'
    EXPORTING
      IT_FITHA_PBUPL_K        = LT_CODE
      IT_FITHA_PBUPL_K_DELETE = LT_CODE_DEL.

  LOOP AT LT_BRANCH_T INTO DATA(LS_BRANCH_T) ##INTO_OK.

    CLEAR LS_DESC.
    LS_DESC-SPRAS       = SY-LANGU.
    LS_DESC-LIFNR       = LV_LIFNR.
    LS_DESC-J_1TPBUPL   = LS_BRANCH_T-J_1TPBUPL.
    LS_DESC-DESCRIPTION = LS_BRANCH_T-DESCRIPTION.
    INSERT LS_DESC INTO TABLE LT_DESC.

  ENDLOOP.

* Assign Desc
  CLEAR LS_DESC.
  LS_DESC-SPRAS       = SY-LANGU.
  LS_DESC-LIFNR       = LV_LIFNR.
  LS_DESC-J_1TPBUPL   = PS_BRANCH-BCODE.
  LS_DESC-DESCRIPTION = PS_BRANCH-BDESC.
  INSERT LS_DESC INTO TABLE LT_DESC.

  CALL FUNCTION 'LVTH_BUPA_UPDATE_BRANCH_DESC'
    EXPORTING
      IT_FITHA_PBUPL_K_T        = LT_DESC
      IT_FITHA_PBUPL_K_T_DELETE = LT_DESC_DEL.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT = GC_TRUE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_assign_bapi_address11
*&---------------------------------------------------------------------*
*& Assign BAPI Address data
*&---------------------------------------------------------------------*
FORM F_ASSIGN_BAPI_ADDRESS11 USING PV_TYPE TYPE BUT000-TYPE
                                    PS_ADDR      TYPE  GTY_V_ADDR
                                    PS_ADDRX     TYPE  GTY_V_ADDRX
                                    PS_ADDR_INT  TYPE  GTY_V_ADDR_INT
                                    PS_ADDR_INTX TYPE  GTY_V_ADDR_INTX
                                    PV_BPEXT     TYPE BUT000-BPEXT      ##NEEDED
                          CHANGING  PS_BAPI_ADDRESS  TYPE  BUS_EI_ADDRESS
                                    PV_ZZCONTEXT     TYPE  CHAR10.

  DATA:
    LS_ADDRESS TYPE  BUS_EI_BUPA_ADDRESS,
    LS_REMARK  TYPE  BUS_EI_BUPA_ADDRESSREMARK.


* Initialize Output
  CLEAR: PV_ZZCONTEXT.

  CLEAR LS_ADDRESS.

  IF PS_ADDR-GUID IS INITIAL.
    LS_ADDRESS-TASK = 'I'.
  ELSE.
    LS_ADDRESS-TASK = 'U'.
  ENDIF.

  LS_ADDRESS-DATA_KEY-GUID = PS_ADDR-GUID.
* Set HR Context to be able to update HR Business Partner
  IF PS_ADDR-HCM_EXIST EQ GC_TRUE.
    PV_ZZCONTEXT = 'HR'.
  ENDIF.

  LS_ADDRESS-DATA-POSTAL-DATA-STR_SUPPL1  = PS_ADDR-STR_SUPPL1.
  LS_ADDRESS-DATA-POSTAL-DATAX-STR_SUPPL1 = PS_ADDRX-STR_SUPPL1.

  LS_ADDRESS-DATA-POSTAL-DATA-STR_SUPPL2  = PS_ADDR-STR_SUPPL2.
  LS_ADDRESS-DATA-POSTAL-DATAX-STR_SUPPL2 = PS_ADDRX-STR_SUPPL2.

  LS_ADDRESS-DATA-POSTAL-DATA-STREET  = PS_ADDR-STREET.
  LS_ADDRESS-DATA-POSTAL-DATAX-STREET = PS_ADDRX-STREET.

  LS_ADDRESS-DATA-POSTAL-DATA-STR_SUPPL3  = PS_ADDR-STR_SUPPL3.
  LS_ADDRESS-DATA-POSTAL-DATAX-STR_SUPPL3 = PS_ADDRX-STR_SUPPL3.

  LS_ADDRESS-DATA-POSTAL-DATA-LOCATION  = PS_ADDR-LOCATION.
  LS_ADDRESS-DATA-POSTAL-DATAX-LOCATION = PS_ADDRX-LOCATION.

  LS_ADDRESS-DATA-POSTAL-DATA-DISTRICT  = PS_ADDR-CITY2.
  LS_ADDRESS-DATA-POSTAL-DATAX-DISTRICT = PS_ADDRX-CITY2.

  LS_ADDRESS-DATA-POSTAL-DATA-CITY  = PS_ADDR-CITY1.
  LS_ADDRESS-DATA-POSTAL-DATAX-CITY = PS_ADDRX-CITY1.

  LS_ADDRESS-DATA-POSTAL-DATA-POSTL_COD1  = PS_ADDR-POST_CODE1.
  LS_ADDRESS-DATA-POSTAL-DATAX-POSTL_COD1 = PS_ADDRX-POST_CODE1.

* Force Clear Region which cause error if changing country
  LS_ADDRESS-DATA-POSTAL-DATA-REGION   = SPACE.
  LS_ADDRESS-DATA-POSTAL-DATAX-REGION  = PS_ADDRX-COUNTRY.

  LS_ADDRESS-DATA-POSTAL-DATA-COUNTRY  = PS_ADDR-COUNTRY.
  LS_ADDRESS-DATA-POSTAL-DATAX-COUNTRY = PS_ADDRX-COUNTRY.


  LS_ADDRESS-DATA-POSTAL-DATA-LANGU  = PS_ADDR-LANGU.
  LS_ADDRESS-DATA-POSTAL-DATAX-LANGU = PS_ADDRX-LANGU.

  LS_ADDRESS-DATA-POSTAL-DATA-VALIDFROMDATE  = PS_ADDR-DATE_FROM.
  LS_ADDRESS-DATA-POSTAL-DATAX-VALIDFROMDATE = PS_ADDRX-DATE_FROM.

  LS_ADDRESS-DATA-POSTAL-DATA-VALIDTODATE  = PS_ADDR-DATE_TO.
  LS_ADDRESS-DATA-POSTAL-DATAX-VALIDTODATE = PS_ADDRX-DATE_TO.


  PERFORM F_ASSIGN_BAPI_PHONE  USING  PS_ADDR-PHONE
                                      PS_ADDRX-PHONE
                             CHANGING LS_ADDRESS-DATA-COMMUNICATION-PHONE.

  PERFORM F_ASSIGN_BAPI_FAX   USING  PS_ADDR-FAX
                                     PS_ADDRX-FAX
                            CHANGING LS_ADDRESS-DATA-COMMUNICATION-FAX.

  PERFORM F_ASSIGN_BAPI_SMTP  USING  PS_ADDR-SMTP
                                     PS_ADDRX-SMTP
                            CHANGING LS_ADDRESS-DATA-COMMUNICATION-SMTP.

  CLEAR LS_REMARK.
  LS_REMARK-DATA-LANGU      = SY-LANGU.
  LS_REMARK-DATA-ADR_NOTES  = PS_ADDR-REMARK.
  LS_REMARK-DATAX-LANGU     = GC_TRUE.
  LS_REMARK-DATAX-ADR_NOTES = GC_TRUE.
  INSERT LS_REMARK INTO TABLE LS_ADDRESS-DATA-REMARK-REMARKS.

  IF PS_ADDR_INT-ACTIVE EQ GC_TRUE.
    PERFORM F_ASSIGN_BAPI_VERSION11  USING  PV_TYPE
                                            PS_ADDR_INT
                                            PS_ADDR_INTX
                                   CHANGING LS_ADDRESS-DATA-VERSION-VERSIONS.
  ENDIF.

* Insert Address data
  INSERT LS_ADDRESS INTO TABLE PS_BAPI_ADDRESS-ADDRESSES.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_assign_bapi_bankdetail11
*&---------------------------------------------------------------------*
*& Assign Bank Detail Data
*&---------------------------------------------------------------------*
*FORM F_ASSIGN_BAPI_BANKDETAIL11 USING PS_BANK TYPE GTY_V_BANK
*                                       PS_BANKX  TYPE  GTY_V_BANKX
*                             CHANGING PT_BAPI_BANK  TYPE BUS_EI_BUPA_BANKDETAIL_T.
*
*  DATA:
*    LS_BANK  TYPE  BUS_EI_BUPA_BANKDETAIL.
*
*
*  IF PS_BANKX IS INITIAL.
*    RETURN.
*  ENDIF.
*
*  CLEAR LS_BANK.
*  IF PS_BANK-BANKS IS INITIAL AND
*     PS_BANK-BANKL IS INITIAL AND
*     PS_BANK-BANKN IS INITIAL AND
*     PS_BANK-BKONT IS INITIAL AND
*     PS_BANK-BKREF IS INITIAL AND
*     PS_BANK-BKEXT IS INITIAL.
*    LS_BANK-TASK           = 'D'. "Delete
*  ELSEIF PS_BANK-BANK_EXIST EQ GC_TRUE.
*    LS_BANK-TASK           = 'U'.
*  ELSE.
*    LS_BANK-TASK           = 'I'.
*  ENDIF.
*
*  LS_BANK-DATA_KEY       = PS_BANK-BKVID.
*
*  LS_BANK-DATA-BANK_CTRY        = PS_BANK-BANKS.
*  LS_BANK-DATAX-BANK_CTRY       = PS_BANKX-BANKS.
*  LS_BANK-DATA-BANK_KEY         = PS_BANK-BANKL.
*  LS_BANK-DATAX-BANK_KEY        = PS_BANKX-BANKL.
*  LS_BANK-DATA-BANK_ACCT        = PS_BANK-BANKN.
*  LS_BANK-DATAX-BANK_ACCT       = PS_BANKX-BANKN.
*  LS_BANK-DATA-CTRL_KEY         = PS_BANK-BKONT.
*  LS_BANK-DATAX-CTRL_KEY        = PS_BANKX-BKONT.
*  LS_BANK-DATA-BANK_REF         = PS_BANK-BKREF.
*  LS_BANK-DATAX-BANK_REF        = PS_BANKX-BKREF.
*  LS_BANK-DATA-EXTERNALBANKID   = PS_BANK-BKEXT.
*  LS_BANK-DATAX-EXTERNALBANKID  = PS_BANKX-BKEXT.
*  LS_BANK-DATA-ACCOUNTHOLDER    = PS_BANK-KOINH.
*  LS_BANK-DATAX-ACCOUNTHOLDER   = PS_BANKX-KOINH.
*  LS_BANK-DATA-BANKDETAILVALIDFROM  = '18000101'.
*  LS_BANK-DATAX-BANKDETAILVALIDFROM = GC_TRUE.
*  LS_BANK-DATA-BANKDETAILVALIDTO    = '99991231'.
*  LS_BANK-DATAX-BANKDETAILVALIDTO   = GC_TRUE.
*
*  INSERT LS_BANK INTO TABLE PT_BAPI_BANK.
*
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_assign_bapi_purch11
*&---------------------------------------------------------------------*
*& Assign BAPI Purchasing view data
*&---------------------------------------------------------------------*
FORM F_ASSIGN_BAPI_PURCH11 USING PS_PURCH TYPE GTY_V_PURCH
                                 PS_PURCHX   TYPE  GTY_V_PURCHX
                        CHANGING PS_BAPI_VEND TYPE VMDS_EI_EXTERN.

  DATA:
    LS_PURCH  TYPE  VMDS_EI_PURCHASING.

  CLEAR: LS_PURCH.
  IF PS_PURCH-EXIST EQ GC_TRUE.
    LS_PURCH-TASK = 'U'.
  ELSE.
    LS_PURCH-TASK = 'I'.
  ENDIF.
  LS_PURCH-DATA_KEY-EKORG = PS_PURCH-EKORG.

  LS_PURCH-DATA-WAERS    = PS_PURCH-WAERS.
  LS_PURCH-DATAX-WAERS   = PS_PURCHX-WAERS.
  LS_PURCH-DATA-ZTERM    = PS_PURCH-ZTERM.
  LS_PURCH-DATAX-ZTERM   = PS_PURCHX-ZTERM.
  LS_PURCH-DATA-INCO1    = PS_PURCH-INCO1.
  LS_PURCH-DATAX-INCO1   = PS_PURCHX-INCO1.
  LS_PURCH-DATA-INCO2_L  = PS_PURCH-INCO2_L.
  LS_PURCH-DATAX-INCO2_L = PS_PURCHX-INCO2_L.
  LS_PURCH-DATA-INCO3_L  = PS_PURCH-INCO3_L.
  LS_PURCH-DATAX-INCO3_L = PS_PURCHX-INCO3_L.
  LS_PURCH-DATA-VERKF    = PS_PURCH-VERKF.
  LS_PURCH-DATAX-VERKF   = PS_PURCHX-VERKF.
  LS_PURCH-DATA-TELF1    = PS_PURCH-TELF1.
  LS_PURCH-DATAX-TELF1   = PS_PURCHX-TELF1.
  LS_PURCH-DATA-WEBRE    = PS_PURCH-WEBRE.
  LS_PURCH-DATAX-WEBRE   = PS_PURCHX-WEBRE.
  LS_PURCH-DATA-LEBRE    = PS_PURCH-WEBRE.
  LS_PURCH-DATAX-LEBRE   = PS_PURCHX-WEBRE.
  LS_PURCH-DATA-BOLRE    = PS_PURCH-BOLRE.
  LS_PURCH-DATAX-BOLRE   = PS_PURCHX-BOLRE.
  LS_PURCH-DATA-UMSAE    = PS_PURCH-UMSAE.
  LS_PURCH-DATAX-UMSAE   = PS_PURCHX-UMSAE.
  LS_PURCH-DATA-BOIND    = PS_PURCH-BOIND.
  LS_PURCH-DATAX-BOIND   = PS_PURCHX-BOIND.
  LS_PURCH-DATA-EKGRP    = PS_PURCH-EKGRP.
  LS_PURCH-DATAX-EKGRP   = PS_PURCHX-EKGRP.
  LS_PURCH-DATA-KZAUT    = PS_PURCH-KZAUT.
  LS_PURCH-DATAX-KZAUT   = PS_PURCHX-KZAUT.

  INSERT LS_PURCH INTO TABLE PS_BAPI_VEND-PURCHASING_DATA-PURCHASING.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_assign_bapi_tax11
*&---------------------------------------------------------------------*
*& Assign BAPI Tax Number Data
*&---------------------------------------------------------------------*
FORM F_ASSIGN_BAPI_TAX11 USING PS_TAX TYPE GTY_V_TAX
                                 PS_TAXX TYPE GTY_V_TAXX
                        CHANGING PS_BAPI_TAX  TYPE  BUS_EI_TAXNUMBER.

  DATA:
    LS_TAX  TYPE  BUS_EI_BUPA_TAXNUMBER.


  IF PS_TAXX-TAXTYPE EQ GC_TRUE OR
     PS_TAXX-TAXNUM  EQ GC_TRUE .
    CLEAR LS_TAX.
    LS_TAX-DATA_KEY-TAXTYPE   = PS_TAX-TAXTYPE.
    LS_TAX-DATA_KEY-TAXNUMBER = PS_TAX-TAXNUM.
    INSERT LS_TAX INTO TABLE PS_BAPI_TAX-TAXNUMBERS.
    PS_BAPI_TAX-CURRENT_STATE = GC_TRUE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_assign_bapi_vendor
*&---------------------------------------------------------------------*
*& Assign BAPI Vendor data
*&---------------------------------------------------------------------*
FORM F_ASSIGN_BAPI_VENDOR USING  PS_VEND TYPE GTY_V_VEND
                                  PS_VENDX    TYPE  GTY_V_VENDX
                                  PS_COMP     TYPE  GTY_V_COMP
                                  PS_COMPX    TYPE  GTY_V_COMPX
                           CHANGING  PS_BAPI_VEND TYPE VMDS_EI_EXTERN.

  DATA:
    LS_COMPANY TYPE  VMDS_EI_COMPANY.

* -----------------
* Central Data
* -----------------
  PS_BAPI_VEND-CENTRAL_DATA-CENTRAL-DATA-VBUND  = PS_VEND-VBUND.
  PS_BAPI_VEND-CENTRAL_DATA-CENTRAL-DATAX-VBUND = PS_VENDX-VBUND.
  PS_BAPI_VEND-CENTRAL_DATA-CENTRAL-DATA-KUNNR  = PS_VEND-KUNNR.
  PS_BAPI_VEND-CENTRAL_DATA-CENTRAL-DATAX-KUNNR = PS_VENDX-KUNNR.
  PS_BAPI_VEND-CENTRAL_DATA-CENTRAL-DATA-XZEMP  = PS_VEND-XZEMP.
  PS_BAPI_VEND-CENTRAL_DATA-CENTRAL-DATAX-XZEMP = PS_VENDX-XZEMP.

* -----------------
* Company Data
* -----------------
  IF PS_COMP-BUKRS IS NOT INITIAL.

    CLEAR LS_COMPANY.

    IF PS_COMP-EXIST IS INITIAL.
      LS_COMPANY-TASK = 'I'.
    ELSE.
      LS_COMPANY-TASK = 'U'.
    ENDIF.

    LS_COMPANY-DATA_KEY-BUKRS = PS_COMP-BUKRS.
    LS_COMPANY-DATA-AKONT  = PS_COMP-AKONT.
    LS_COMPANY-DATAX-AKONT = PS_COMPX-AKONT.
    LS_COMPANY-DATA-ZUAWA  = PS_COMP-ZUAWA.
    LS_COMPANY-DATAX-ZUAWA = PS_COMPX-ZUAWA.
    LS_COMPANY-DATA-FDGRV  = PS_COMP-FDGRV.
    LS_COMPANY-DATAX-FDGRV = PS_COMPX-FDGRV.
    LS_COMPANY-DATA-ALTKN  = PS_COMP-ALTKN.
    LS_COMPANY-DATAX-ALTKN = PS_COMPX-ALTKN.
    LS_COMPANY-DATA-ZTERM  = PS_COMP-ZTERM.
    LS_COMPANY-DATAX-ZTERM = PS_COMPX-ZTERM.
    LS_COMPANY-DATA-GUZTE  = PS_COMP-ZTERM.  "credit memo payment
    LS_COMPANY-DATAX-GUZTE = PS_COMPX-ZTERM.
    LS_COMPANY-DATA-REPRF  = GC_TRUE. "Check double invoice
    LS_COMPANY-DATAX-REPRF = GC_TRUE.
    LS_COMPANY-DATA-REPRF  = PS_COMP-REPRF.
    LS_COMPANY-DATAX-REPRF = PS_COMPX-REPRF.
    LS_COMPANY-DATA-ZWELS  = PS_COMP-ZWELS.
    LS_COMPANY-DATAX-ZWELS = PS_COMPX-ZWELS.
    LS_COMPANY-DATA-HBKID  = PS_COMP-HBKID.
    LS_COMPANY-DATAX-HBKID = PS_COMPX-HBKID.

    PERFORM F_ASSIGN_BAPI_COMP_WTAX11  USING  PS_COMP-WTAX1
                                              PS_COMPX-WTAX1
                                     CHANGING LS_COMPANY-WTAX_TYPE-WTAX_TYPE.

    PERFORM F_ASSIGN_BAPI_COMP_WTAX11  USING  PS_COMP-WTAX2
                                              PS_COMPX-WTAX2
                                     CHANGING LS_COMPANY-WTAX_TYPE-WTAX_TYPE.

    PERFORM F_ASSIGN_BAPI_ALT_PAYEE   USING  PS_VEND
                                             PS_VENDX
                                    CHANGING LS_COMPANY-ALT_PAYEE-ALT_PAYEE .

    INSERT LS_COMPANY INTO TABLE PS_BAPI_VEND-COMPANY_DATA-COMPANY.

  ENDIF.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_assign_bapi_comp_wtax11
*&---------------------------------------------------------------------*
*& Assign Company Withholding Tax data
*&---------------------------------------------------------------------*
FORM F_ASSIGN_BAPI_COMP_WTAX11 USING PS_WTAX TYPE GTY_V_WTAX
                                       PS_WTAXX TYPE  GTY_V_WTAXX
                             CHANGING  PT_BAPI_WTAX TYPE VMDS_EI_WTAX_TYPE_T.

  DATA:
    LS_WTAX  TYPE  VMDS_EI_WTAX_TYPE.


* Only when data exist
  IF PS_WTAXX IS INITIAL.
    RETURN.
  ENDIF.

  CLEAR LS_WTAX.
  IF PS_WTAX-EXIST EQ GC_TRUE.
    IF PS_WTAX-WITHCD IS INITIAL AND
       PS_WTAX-SUBJCT IS INITIAL AND
       PS_WTAX-QSREC  IS INITIAL .
      LS_WTAX-TASK            = 'D'.
    ELSE.
      LS_WTAX-TASK            = 'U'.
    ENDIF.
  ELSE.
    LS_WTAX-TASK            = 'I'.
  ENDIF.
  LS_WTAX-DATA_KEY-WITHT  = PS_WTAX-WITHT.

  LS_WTAX-DATA-WT_WITHCD  = PS_WTAX-WITHCD.
  LS_WTAX-DATAX-WT_WITHCD = PS_WTAXX-WITHCD.
  LS_WTAX-DATA-WT_SUBJCT  = PS_WTAX-SUBJCT.
  LS_WTAX-DATAX-WT_SUBJCT = PS_WTAXX-SUBJCT.
  LS_WTAX-DATA-QSREC  = PS_WTAX-QSREC.
  LS_WTAX-DATAX-QSREC = PS_WTAXX-QSREC.

  INSERT LS_WTAX INTO TABLE PT_BAPI_WTAX.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_assign_bapi_version11
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PV_TYPE
*&      --> PS_ADDR_INT
*&      --> PS_ADDR_INTX
*&      <-- LS_ADDRESS_DATA_VERSION_VERSIO
*&---------------------------------------------------------------------*
FORM F_ASSIGN_BAPI_VERSION11 USING PV_TYPE TYPE GTY_DATA11-KEY-TYPE
                                   PS_ADDR  TYPE  GTY_V_ADDR_INT
                                   PS_ADDRX TYPE  GTY_V_ADDR_INTX
                          CHANGING PT_VERSION  TYPE  BUS_EI_BUPA_VERSION_T.

  DATA:
    LS_VERSION TYPE  BUS_EI_BUPA_VERSION.


  CLEAR LS_VERSION.

  CASE PV_TYPE.
    WHEN GC_TYPE_PERSON.
      LS_VERSION-DATA-PERSON-ADDR_VERS   = PS_ADDR-NATION.
      LS_VERSION-DATAX-PERSON-ADDR_VERS  = GC_TRUE.
      LS_VERSION-DATA-PERSON-FIRSTNAME   = PS_ADDR-NAME_FIRST.
      LS_VERSION-DATAX-PERSON-FIRSTNAME  = PS_ADDRX-NAME_FIRST.
      LS_VERSION-DATA-PERSON-LASTNAME    = PS_ADDR-NAME_LAST.
      LS_VERSION-DATAX-PERSON-LASTNAME   = PS_ADDRX-NAME_LAST.
      LS_VERSION-DATA-PERSON-SORT1_P     = PS_ADDR-SORT1.
      LS_VERSION-DATAX-PERSON-SORT1_P    = PS_ADDRX-SORT1.
      LS_VERSION-DATA-PERSON-SORT2_P     = PS_ADDR-SORT2.
      LS_VERSION-DATAX-PERSON-SORT2_P    = PS_ADDRX-SORT2.
      LS_VERSION-DATA-PERSON-STREET      = PS_ADDR-STREET.
      LS_VERSION-DATAX-PERSON-STREET     = PS_ADDRX-STREET.
      LS_VERSION-DATA-ORGANIZATION-STR_SUPPL1  = PS_ADDR-STR_SUPPL1.
      LS_VERSION-DATAX-ORGANIZATION-STR_SUPPL1 = PS_ADDRX-STR_SUPPL1.
      LS_VERSION-DATA-ORGANIZATION-STR_SUPPL2 = PS_ADDR-STR_SUPPL2.
      LS_VERSION-DATAX-ORGANIZATION-STR_SUPPL2 = PS_ADDRX-STR_SUPPL2.
      LS_VERSION-DATA-PERSON-STR_SUPPL3  = PS_ADDR-STR_SUPPL3.
      LS_VERSION-DATAX-PERSON-STR_SUPPL3 = PS_ADDRX-STR_SUPPL3.
      LS_VERSION-DATA-PERSON-LOCATION    = PS_ADDR-LOCATION.
      LS_VERSION-DATAX-PERSON-LOCATION   = PS_ADDRX-LOCATION.
      LS_VERSION-DATA-PERSON-DISTRICT    = PS_ADDR-CITY2.
      LS_VERSION-DATAX-PERSON-DISTRICT   = PS_ADDRX-CITY2.
      LS_VERSION-DATA-PERSON-CITY        = PS_ADDR-CITY1.
      LS_VERSION-DATAX-PERSON-CITY       = PS_ADDRX-CITY1.
*      ls_version-data-person-po_box_cit  = ps_addr-post_code1.
*      ls_version-datax-person-po_box_cit = ps_addrx-post_code1.
      LS_VERSION-DATAX-PERSON-COUNTY     = PS_ADDR-COUNTRY.
      LS_VERSION-DATAX-PERSON-COUNTY     = PS_ADDRX-COUNTRY.

    WHEN GC_TYPE_ORG.
      LS_VERSION-DATA-ORGANIZATION-ADDR_VERS   = PS_ADDR-NATION.
      LS_VERSION-DATAX-ORGANIZATION-ADDR_VERS  = GC_TRUE.
      LS_VERSION-DATA-ORGANIZATION-NAME        = PS_ADDR-NAME1.
      LS_VERSION-DATAX-ORGANIZATION-NAME       = PS_ADDRX-NAME1.
      LS_VERSION-DATA-ORGANIZATION-NAME_2      = PS_ADDR-NAME2.
      LS_VERSION-DATAX-ORGANIZATION-NAME_2     = PS_ADDRX-NAME2.
      LS_VERSION-DATA-ORGANIZATION-NAME_3      = PS_ADDR-NAME3.
      LS_VERSION-DATAX-ORGANIZATION-NAME_3     = PS_ADDRX-NAME3.
      LS_VERSION-DATA-ORGANIZATION-NAME_4      = PS_ADDR-NAME4.
      LS_VERSION-DATAX-ORGANIZATION-NAME_4     = PS_ADDRX-NAME4.
      LS_VERSION-DATA-ORGANIZATION-SORT1       = PS_ADDR-SORT1.
      LS_VERSION-DATAX-ORGANIZATION-SORT1      = PS_ADDRX-SORT1.
      LS_VERSION-DATA-ORGANIZATION-SORT2       = PS_ADDR-SORT2.
      LS_VERSION-DATAX-ORGANIZATION-SORT2      = PS_ADDRX-SORT2.
      LS_VERSION-DATA-ORGANIZATION-STREET      = PS_ADDR-STREET.
      LS_VERSION-DATAX-ORGANIZATION-STREET     = PS_ADDRX-STREET.
      LS_VERSION-DATA-ORGANIZATION-STR_SUPPL1  = PS_ADDR-STR_SUPPL1.
      LS_VERSION-DATAX-ORGANIZATION-STR_SUPPL1 = PS_ADDRX-STR_SUPPL1.
      LS_VERSION-DATA-ORGANIZATION-STR_SUPPL2 = PS_ADDR-STR_SUPPL2.
      LS_VERSION-DATAX-ORGANIZATION-STR_SUPPL2 = PS_ADDRX-STR_SUPPL2.
      LS_VERSION-DATA-ORGANIZATION-STR_SUPPL3  = PS_ADDR-STR_SUPPL3.
      LS_VERSION-DATAX-ORGANIZATION-STR_SUPPL3 = PS_ADDRX-STR_SUPPL3.
      LS_VERSION-DATA-ORGANIZATION-LOCATION    = PS_ADDR-LOCATION.
      LS_VERSION-DATAX-ORGANIZATION-LOCATION   = PS_ADDRX-LOCATION.
      LS_VERSION-DATA-ORGANIZATION-DISTRICT    = PS_ADDR-CITY2.
      LS_VERSION-DATAX-ORGANIZATION-DISTRICT   = PS_ADDRX-CITY2.
      LS_VERSION-DATA-ORGANIZATION-CITY        = PS_ADDR-CITY1.
      LS_VERSION-DATAX-ORGANIZATION-CITY       = PS_ADDRX-CITY1.
      LS_VERSION-DATA-ORGANIZATION-PO_BOX_CIT  = PS_ADDR-POST_CODE1.
      LS_VERSION-DATAX-ORGANIZATION-PO_BOX_CIT = PS_ADDRX-POST_CODE1.
      LS_VERSION-DATA-ORGANIZATION-COUNTY      = PS_ADDR-COUNTRY.
      LS_VERSION-DATAX-ORGANIZATION-COUNTY     = PS_ADDRX-COUNTRY.

  ENDCASE.

  INSERT LS_VERSION INTO TABLE PT_VERSION.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_bapi_returnmap11
*&---------------------------------------------------------------------*
*& Check BAPI Return result
*&---------------------------------------------------------------------*
FORM F_CHECK_BAPI_RETURNMAP11 USING PT_RETURN TYPE MDG_BS_BP_MSGMAP_T
                           CHANGING PV_ERROR    TYPE  FLAG
                                    PT_MESSG    TYPE  GTTY_MESSG.

  DATA:
    LS_MESSG      TYPE  GTY_MESSG.

  FIELD-SYMBOLS:
    <LFS_RETURN>  TYPE  MDG_BS_BP_MSGMAP.


* Initialize Output
  CLEAR PV_ERROR.
  CLEAR: PT_MESSG.

  LOOP AT PT_RETURN ASSIGNING <LFS_RETURN>
                    WHERE TYPE  EQ  'A' OR
                          TYPE  EQ  'E'.
    PV_ERROR = GC_TRUE.
    CLEAR LS_MESSG.
    LS_MESSG-MSGTY = 'E'.
    LS_MESSG-MSGID = <LFS_RETURN>-ID.
    LS_MESSG-MSGNO = <LFS_RETURN>-NUMBER.
    MESSAGE ID <LFS_RETURN>-ID
            TYPE <LFS_RETURN>-TYPE
            NUMBER <LFS_RETURN>-NUMBER
            WITH <LFS_RETURN>-MESSAGE_V1 <LFS_RETURN>-MESSAGE_V2
                 <LFS_RETURN>-MESSAGE_V3 <LFS_RETURN>-MESSAGE_V4
            INTO LS_MESSG-MSGTX.
    INSERT LS_MESSG INTO TABLE PT_MESSG.
    EXIT.
  ENDLOOP.
  IF PT_MESSG IS INITIAL.
*   Get 1st Message
    LOOP AT PT_RETURN ASSIGNING <LFS_RETURN>.
      PV_ERROR = GC_TRUE.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'E'.
      LS_MESSG-MSGID = <LFS_RETURN>-ID.
      LS_MESSG-MSGNO = <LFS_RETURN>-NUMBER.
      MESSAGE ID <LFS_RETURN>-ID
              TYPE <LFS_RETURN>-TYPE
              NUMBER <LFS_RETURN>-NUMBER
              WITH <LFS_RETURN>-MESSAGE_V1 <LFS_RETURN>-MESSAGE_V2
                   <LFS_RETURN>-MESSAGE_V3 <LFS_RETURN>-MESSAGE_V4
              INTO LS_MESSG-MSGTX.
      INSERT LS_MESSG INTO TABLE PT_MESSG.
      EXIT.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_role_exist
*&---------------------------------------------------------------------*
*& Check Partner role already exist?
*&---------------------------------------------------------------------*

FORM F_CHECK_ROLE_EXIST USING PV_PARTNER TYPE BUT000-PARTNER
                                PV_RLTYP    TYPE  BUT100-RLTYP
                       CHANGING PV_EXIST    TYPE  FLAG.

  DATA:
    LV_PARTNER  TYPE  BUT100-PARTNER ##NEEDED.


* Initialize Output
  CLEAR: PV_EXIST.

  SELECT PARTNER
      UP TO 1 ROWS
    INTO LV_PARTNER
    FROM BUT100
   WHERE PARTNER EQ PV_PARTNER
     AND RLTYP   EQ PV_RLTYP
   ORDER BY PRIMARY KEY.                             "#EC CI_SEL_NESTED
  ENDSELECT.
  IF SY-SUBRC EQ 0.
    PV_EXIST = GC_TRUE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_vendor_exist
*&---------------------------------------------------------------------*
*& Check Vendor Exist
*&---------------------------------------------------------------------*
FORM F_CHECK_VENDOR_EXIST USING PV_PARTNER TYPE BUT000-PARTNER
                         CHANGING PV_EXIST  TYPE  FLAG.

  DATA:
    LV_LIFNR  TYPE  LFA1-LIFNR ##NEEDED.


* Initlialize Output
  CLEAR: PV_EXIST.

* Get Vendor from Partner
  PERFORM F_GET_VENDOR_FROM_PARTNER  USING  PV_PARTNER
                                   CHANGING LV_LIFNR.
  IF LV_LIFNR IS NOT INITIAL.
    PV_EXIST = GC_TRUE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_collect_result11
*&---------------------------------------------------------------------*
*& Collect Processing Result
*&---------------------------------------------------------------------*
FORM F_COLLECT_RESULT11 USING PS_DATA TYPE GTY_DATA11
                      CHANGING PT_RESULT TYPE  GTTY_RESULT11.

  DATA:
    LS_RESULT  TYPE  GTY_RESULT11.

  FIELD-SYMBOLS:
    <LFS_ROLE>  TYPE  GTY_V_ROLE,
    <LFS_MESSG> TYPE  GTY_MESSG.


  CLEAR: LS_RESULT.

  LOOP AT PS_DATA-KEY-ROLE ASSIGNING <LFS_ROLE>
                           WHERE RLTYP NE GC_ROLE1_11.
    LS_RESULT-RLTYP = <LFS_ROLE>-RLTYP.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC NE 0.
    LS_RESULT-RLTYP = GC_ROLE1_11.
  ENDIF.

  LS_RESULT-ROWNO   = PS_DATA-ROWNO.
  LS_RESULT-PARTNER = PS_DATA-KEY-PARTNER.
  LS_RESULT-BU_GROUP = PS_DATA-KEY-BU_GROUP.
  LS_RESULT-TYPE = PS_DATA-KEY-TYPE.
  LS_RESULT-ADDR_NAME1 = PS_DATA-MAIN-ADDR-NAME1.
  LS_RESULT-ADDR_NAME2 = PS_DATA-MAIN-ADDR-NAME2.
  LS_RESULT-ADDR_NAME3 = PS_DATA-MAIN-ADDR-NAME3.
  LS_RESULT-ADDR_NAME4 = PS_DATA-MAIN-ADDR-NAME4.
  LS_RESULT-ADDR_SORT1 = PS_DATA-MAIN-ADDR-SORT1.
  LS_RESULT-ADDR_SORT2 = PS_DATA-MAIN-ADDR-SORT2.
  LS_RESULT-ADDR_STREET = PS_DATA-MAIN-ADDR-STREET.
  LS_RESULT-ADDR_STR_SUPPL3 = PS_DATA-MAIN-ADDR-STR_SUPPL3.
  LS_RESULT-ADDR_LOCATION = PS_DATA-MAIN-ADDR-LOCATION.
  LS_RESULT-ADDR_CITY2 = PS_DATA-MAIN-ADDR-CITY2.
  LS_RESULT-ADDR_CITY1 = PS_DATA-MAIN-ADDR-CITY1.
  LS_RESULT-ADDR_POST_CODE1 = PS_DATA-MAIN-ADDR-POST_CODE1.
  LS_RESULT-ADDR_COUNTRY = PS_DATA-MAIN-ADDR-COUNTRY.
  LS_RESULT-ADDR_LANGU = PS_DATA-MAIN-ADDR-LANGU.
  LS_RESULT-ADDR_TELNO1 = PS_DATA-MAIN-ADDR-PHONE-TELNO1.
  LS_RESULT-ADDR_TELEXT1 = PS_DATA-MAIN-ADDR-PHONE-TELEXT1.
  LS_RESULT-ADDR_MOBILE1 = PS_DATA-MAIN-ADDR-PHONE-MOBILE.
  LS_RESULT-ADDR_FAXNO = PS_DATA-MAIN-ADDR-FAX-FAXNO.
  LS_RESULT-ADDR_FAXEXT = PS_DATA-MAIN-ADDR-FAX-FAXEXT.
  LS_RESULT-ADDR_EMAIL1 = PS_DATA-MAIN-ADDR-SMTP-EMAIL1.
  LS_RESULT-ADDRINT_NAME1 = PS_DATA-MAIN-ADDR_INT-NAME1.
  LS_RESULT-ADDRINT_NAME2 = PS_DATA-MAIN-ADDR_INT-NAME2.
  LS_RESULT-ADDRINT_NAME3 = PS_DATA-MAIN-ADDR_INT-NAME3.
  LS_RESULT-ADDRINT_NAME4 = PS_DATA-MAIN-ADDR_INT-NAME4.
  LS_RESULT-ADDRINT_SORT1 = PS_DATA-MAIN-ADDR_INT-SORT1.
  LS_RESULT-ADDRINT_STREET = PS_DATA-MAIN-ADDR_INT-STREET.
  LS_RESULT-ADDRINT_STR_SUPPL3 = PS_DATA-MAIN-ADDR_INT-STR_SUPPL3.
  LS_RESULT-ADDRINT_LOCATION = PS_DATA-MAIN-ADDR_INT-LOCATION.
  LS_RESULT-ADDRINT_CITY2 = PS_DATA-MAIN-ADDR_INT-CITY2.
  LS_RESULT-ADDRINT_CITY1 = PS_DATA-MAIN-ADDR_INT-CITY1.
  LS_RESULT-ADDRINT_POST_CODE1 = PS_DATA-MAIN-ADDR_INT-POST_CODE1.
  LS_RESULT-ADDRINT_COUNTRY = PS_DATA-MAIN-ADDR_INT-COUNTRY.
  LS_RESULT-ADDRINT_LANGU = PS_DATA-MAIN-ADDR_INT-LANGU.
  LS_RESULT-TAX_TAXTYPE = PS_DATA-MAIN-TAX-TAXTYPE.
  LS_RESULT-TAX_TAXNUM = PS_DATA-MAIN-TAX-TAXNUM.
  LS_RESULT-BNK01_BKVID = PS_DATA-MAIN-BANK01-BKVID.
  LS_RESULT-BNK01_BANKS = PS_DATA-MAIN-BANK01-BANKS.
  LS_RESULT-BNK01_BANKL = PS_DATA-MAIN-BANK01-BANKL.
  LS_RESULT-BNK01_BANKN = PS_DATA-MAIN-BANK01-BANKN.
  LS_RESULT-BNK01_BKONT = PS_DATA-MAIN-BANK01-BKONT.
  LS_RESULT-BNK01_BKREF = PS_DATA-MAIN-BANK01-BKREF.
  LS_RESULT-BNK01_BKEXT = PS_DATA-MAIN-BANK01-BKEXT.
  LS_RESULT-VEND_VBUND = PS_DATA-MAIN-VEND-VBUND.
  LS_RESULT-VEND_KUNNR = PS_DATA-MAIN-VEND-KUNNR.
  LS_RESULT-VEND_XZEMP = PS_DATA-MAIN-VEND-XZEMP.
  LS_RESULT-COMP_BUKRS = PS_DATA-MAIN-COMP-BUKRS.
  LS_RESULT-COMP_AKONT = PS_DATA-MAIN-COMP-AKONT.
  LS_RESULT-COMP_ZUAWA = PS_DATA-MAIN-COMP-ZUAWA.
  LS_RESULT-COMP_FDGRV = PS_DATA-MAIN-COMP-FDGRV.
  LS_RESULT-COMP_ALTKN = PS_DATA-MAIN-COMP-ALTKN.
  LS_RESULT-COMP_ZTERM = PS_DATA-MAIN-COMP-ZTERM.
  LS_RESULT-COMP_REPRF = PS_DATA-MAIN-COMP-REPRF.
  LS_RESULT-COMP_ZWELS = PS_DATA-MAIN-COMP-ZWELS.
  LS_RESULT-COMP_HBKID = PS_DATA-MAIN-COMP-HBKID.
  LS_RESULT-WTAX_WITHT1 = PS_DATA-MAIN-COMP-WTAX1-WITHT.
  LS_RESULT-WTAX_WITHCD1 = PS_DATA-MAIN-COMP-WTAX1-WITHCD.
  LS_RESULT-WTAX_SUBJCT1 = PS_DATA-MAIN-COMP-WTAX1-SUBJCT.
  LS_RESULT-WTAX_QSREC1 = PS_DATA-MAIN-COMP-WTAX1-QSREC.
  LS_RESULT-BCODE = PS_DATA-MAIN-COMP-BRANCH-BCODE.
  LS_RESULT-BDESC = PS_DATA-MAIN-COMP-BRANCH-BDESC.
  LS_RESULT-BDEFT = PS_DATA-MAIN-COMP-BRANCH-BDEFT.
  LS_RESULT-PURC_EKORG = PS_DATA-MAIN-PURCH-EKORG.
  LS_RESULT-PURC_WAERS = PS_DATA-MAIN-PURCH-WAERS.
  LS_RESULT-PURC_ZTERM = PS_DATA-MAIN-PURCH-ZTERM.
  LS_RESULT-PURC_INCO1 = PS_DATA-MAIN-PURCH-INCO1.
  LS_RESULT-PURC_INCO2_L = PS_DATA-MAIN-PURCH-INCO2_L.
  LS_RESULT-PURC_INCO3_L = PS_DATA-MAIN-PURCH-INCO3_L.
  LS_RESULT-PURC_VERKF = PS_DATA-MAIN-PURCH-VERKF.
  LS_RESULT-PURC_TELF1 = PS_DATA-MAIN-PURCH-TELF1.
  LS_RESULT-PURC_WEBRE = PS_DATA-MAIN-PURCH-WEBRE.
  LS_RESULT-PURC_EKGRP = PS_DATA-MAIN-PURCH-EKGRP.
  LS_RESULT-PURC_KZAUT = PS_DATA-MAIN-PURCH-KZAUT.

  LOOP AT PS_DATA-MESSG ASSIGNING <LFS_MESSG>.
    LS_RESULT-MSGTY = <LFS_MESSG>-MSGTY.
    LS_RESULT-MSGID = <LFS_MESSG>-MSGID.
    LS_RESULT-MSGNO = <LFS_MESSG>-MSGNO.
    LS_RESULT-MSGTX = <LFS_MESSG>-MSGTX.
    CASE LS_RESULT-MSGTY.
      WHEN 'S'.
        LS_RESULT-STATU = ICON_LED_GREEN.
      WHEN 'W'.
        LS_RESULT-STATU = ICON_LED_YELLOW.
      WHEN 'E' OR 'A'.
        LS_RESULT-STATU = ICON_LED_RED.
      WHEN OTHERS.
        LS_RESULT-STATU = ICON_LED_INACTIVE.
    ENDCASE.
    EXIT.
  ENDLOOP.

  INSERT LS_RESULT INTO TABLE PT_RESULT.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_independent_address
*&---------------------------------------------------------------------*
*&  Update Independent Address for Partner
*&---------------------------------------------------------------------*
FORM F_UPDATE_INDEPENDENT_ADDRESS USING PV_PARTNER TYPE BUT000-PARTNER
                                          PS_ADDR      TYPE  GTY_V_ADDR
                                          PS_ADDR_IND  TYPE  GTY_V_ADDR_IND
                                          PS_ADDR_INDX TYPE  GTY_V_ADDR_INDX
                                 CHANGING PV_ERROR     TYPE  FLAG
                                          PT_MESSG     TYPE  GTTY_MESSG.

  TYPES: BEGIN OF LTY_ADR2,
           DATE_FROM  TYPE  ADR2-DATE_FROM,
           CONSNUMBER TYPE  ADR2-CONSNUMBER,
           TEL_NUMBER TYPE  ADR2-TEL_NUMBER,
           R3_USER    TYPE  ADR2-R3_USER,
         END OF LTY_ADR2.
  TYPES: LTTY_ADR2 TYPE STANDARD TABLE OF LTY_ADR2.

  TYPES: BEGIN OF LTY_ADR6,
           DATE_FROM  TYPE  ADR6-DATE_FROM,
           CONSNUMBER TYPE  ADR6-CONSNUMBER,
           SMTP_ADDR  TYPE  ADR6-SMTP_ADDR,
         END OF LTY_ADR6.
  TYPES: LTTY_ADR6 TYPE STANDARD TABLE OF LTY_ADR6.

  DATA:
    LT_TEL    TYPE  STANDARD TABLE OF BAPIADTEL,
    LT_TELX   TYPE  STANDARD TABLE OF BAPIADTELX,
    LT_SMTP   TYPE  STANDARD TABLE OF BAPIADSMTP,
    LT_SMTPX  TYPE  STANDARD TABLE OF BAPIADSMTX,
    LT_RETURN TYPE  STANDARD TABLE OF BAPIRET2,
    LT_ADR2   TYPE  LTTY_ADR2,
    LT_ADR6   TYPE  LTTY_ADR6.

  DATA:
    LS_TEL   TYPE  BAPIADTEL,
    LS_TELX  TYPE  BAPIADTELX,
    LS_SMTP  TYPE  BAPIADSMTP,
    LS_SMTPX TYPE  BAPIADSMTX,
    LS_MESSG TYPE  GTY_MESSG.

  "beg+++ iPS136Oct2021 [Rollout Captain]
  DATA: LS_NOTE  TYPE BAPICOMREM,
        LS_NOTEX TYPE BAPICOMREX,
        LT_NOTE  TYPE STANDARD TABLE OF BAPICOMREM,
        LT_NOTEX TYPE STANDARD TABLE OF BAPICOMREX.
  "end+++ iPS136Oct2021 [Rollout Captain]

  FIELD-SYMBOLS:
    <LFS_ADR2>   TYPE  LTY_ADR2,
    <LFS_ADR6>   TYPE  LTY_ADR6,
    <LFS_RETURN> TYPE BAPIRET2.


* Initialize Output
  CLEAR: PV_ERROR.
  CLEAR: PT_MESSG.

  IF PS_ADDR_INDX-TELNO IS NOT INITIAL OR
     PS_ADDR_INDX-MOBILE IS NOT INITIAL.

*   Get Existing telno
    SELECT ADR2~DATE_FROM ADR2~CONSNUMBER
           ADR2~TEL_NUMBER ADR2~R3_USER
      INTO TABLE LT_ADR2
      FROM BUT000
             INNER JOIN ADR2
               ON  ADR2~ADDRNUMBER = BUT000~ADDRCOMM
     WHERE BUT000~PARTNER EQ PV_PARTNER
       AND ADR2~DATE_FROM LE SY-DATUM
     ORDER BY ADR2~DATE_FROM DESCENDING
              ADR2~CONSNUMBER ASCENDING.             "#EC CI_SEL_NESTED
    IF SY-SUBRC NE 0.
      CLEAR: LT_ADR2.
    ENDIF.

  ENDIF.

* Tel No
  IF PS_ADDR_INDX-TELNO IS NOT INITIAL.
    CLEAR: LS_TEL,
           LS_TELX.

    LOOP AT LT_ADR2 ASSIGNING <LFS_ADR2>
                    WHERE R3_USER EQ ' ' OR
                          R3_USER EQ '1' .
      LS_TEL-CONSNUMBER  = <LFS_ADR2>-CONSNUMBER.
      LS_TELX-CONSNUMBER = GC_TRUE.
      EXIT.
    ENDLOOP.
    IF SY-SUBRC EQ 0.
      LS_TELX-UPDATEFLAG = 'U'.
    ELSE.
      LS_TELX-UPDATEFLAG = 'I'.
    ENDIF.

    IF PS_ADDR-COUNTRY IS NOT INITIAL.
      LS_TEL-COUNTRY  = PS_ADDR-COUNTRY.
      LS_TELX-COUNTRY = GC_TRUE.
    ENDIF.
    LS_TEL-TELEPHONE  = PS_ADDR_IND-TELNO.
    LS_TELX-TELEPHONE = PS_ADDR_INDX-TELNO.

    INSERT LS_TEL INTO TABLE LT_TEL.
    INSERT LS_TELX INTO TABLE LT_TELX.
  ENDIF.

* Mobile no
  IF PS_ADDR_INDX-MOBILE IS NOT INITIAL.
    CLEAR: LS_TEL,
           LS_TELX.
    LOOP AT LT_ADR2 ASSIGNING <LFS_ADR2>
                    WHERE R3_USER EQ '2' OR
                          R3_USER EQ '3' .
      LS_TEL-CONSNUMBER  = <LFS_ADR2>-CONSNUMBER.
      LS_TELX-CONSNUMBER = GC_TRUE.
      EXIT.
    ENDLOOP.
    IF SY-SUBRC EQ 0.
      LS_TELX-UPDATEFLAG = 'U'.
    ELSE.
      LS_TELX-UPDATEFLAG = 'I'.
    ENDIF.

    IF PS_ADDR-COUNTRY IS NOT INITIAL.
      LS_TEL-COUNTRY  = PS_ADDR-COUNTRY.
      LS_TELX-COUNTRY = GC_TRUE.
    ENDIF.
    LS_TEL-TELEPHONE  = PS_ADDR_IND-MOBILE.
    LS_TELX-TELEPHONE = PS_ADDR_INDX-MOBILE.
    LS_TEL-R_3_USER   = '2'.
    LS_TELX-R_3_USER  = GC_TRUE.

    INSERT LS_TEL INTO TABLE LT_TEL.
    INSERT LS_TELX INTO TABLE LT_TELX.
  ENDIF.

* Email
  IF PS_ADDR_INDX-EMAIL IS NOT INITIAL.

*   Get Existing E-Mail
    SELECT ADR6~DATE_FROM ADR6~CONSNUMBER
           ADR6~SMTP_ADDR
      INTO TABLE LT_ADR6
      FROM BUT000
             INNER JOIN ADR6
               ON  ADR6~ADDRNUMBER = BUT000~ADDRCOMM
     WHERE BUT000~PARTNER EQ PV_PARTNER
       AND ADR6~DATE_FROM LE SY-DATUM
     ORDER BY ADR6~DATE_FROM DESCENDING
              ADR6~CONSNUMBER ASCENDING.             "#EC CI_SEL_NESTED
    IF SY-SUBRC NE 0.
      CLEAR: LT_ADR6.
    ENDIF.

    CLEAR: LS_SMTP,
           LS_SMTPX.
    LOOP AT LT_ADR6 ASSIGNING <LFS_ADR6>.
      LS_SMTP-CONSNUMBER  = <LFS_ADR6>-CONSNUMBER.
      LS_SMTPX-CONSNUMBER = GC_TRUE.
      EXIT.
    ENDLOOP.
    IF SY-SUBRC EQ 0.
      LS_SMTPX-UPDATEFLAG = 'U'.
    ELSE.
      LS_SMTPX-UPDATEFLAG = 'I'.
    ENDIF.
    LS_SMTP-E_MAIL    = PS_ADDR_IND-EMAIL.
    LS_SMTP-HOME_FLAG = GC_TRUE.            "iPS136Oct2021 [Rollout Captain]
    LS_SMTP-STD_RECIP = GC_TRUE.            "iPS136Oct2021 [Rollout Captain]
    LS_SMTPX-UPDATEFLAG = 'I'.              "iPS136Oct2021 [Rollout Captain]
    LS_SMTPX-E_MAIL     = PS_ADDR_INDX-EMAIL.
    INSERT LS_SMTP INTO TABLE LT_SMTP.
    INSERT LS_SMTPX INTO TABLE LT_SMTPX.

    LS_NOTE-COMM_TYPE    = 'INT'.
    LS_NOTE-LANGU        = SY-LANGU.
    LS_NOTE-COMM_NOTES   = PS_ADDR_IND-NOTE.
    LS_NOTEX-COMM_TYPE   = GC_TRUE.
    LS_NOTEX-LANGU       = GC_TRUE.
    LS_NOTEX-COMM_NOTES  = GC_TRUE.
    LS_NOTEX-CONSNUMBER  = GC_TRUE.
    LS_NOTEX-UPDATEFLAG  = LS_SMTPX-UPDATEFLAG.
    INSERT LS_NOTE  INTO TABLE LT_NOTE.
    INSERT LS_NOTEX INTO TABLE LT_NOTEX.


  ENDIF.

* Call Function update address
  CALL FUNCTION 'BUPA_CENTRAL_CHANGE'
    EXPORTING
      IV_PARTNER             = PV_PARTNER
      IV_X_SAVE              = GC_TRUE
      IV_CHANGE_BAS          = GC_TRUE
    TABLES
      IT_ADTEL_ADDR_IND      = LT_TEL
      IT_ADSMTP_ADDR_IND     = LT_SMTP
      IT_ADTEL_ADDR_IND_X    = LT_TELX
      IT_ADSMTP_ADDR_IND_X   = LT_SMTPX
      IT_ADCOMREM_ADDR_IND   = LT_NOTE    "iPS13Oct2021 [Rollout Captain]
      IT_ADCOMREM_ADDR_IND_X = LT_NOTEX   "iPS13Oct2021 [Rollout Captain]
      ET_RETURN              = LT_RETURN.

  LOOP AT LT_RETURN ASSIGNING <LFS_RETURN>
                    WHERE TYPE EQ 'E' OR
                          TYPE EQ 'A'.
    PV_ERROR = GC_TRUE.
    CLEAR LS_MESSG.
    LS_MESSG-MSGTY = 'E'.
    LS_MESSG-MSGID = <LFS_RETURN>-ID.
    LS_MESSG-MSGNO = <LFS_RETURN>-NUMBER.
    LS_MESSG-MSGTX = <LFS_RETURN>-MESSAGE.
    INSERT LS_MESSG INTO TABLE PT_MESSG.

  ENDLOOP.
  IF SY-SUBRC EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = GC_TRUE.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_display_result11
*&---------------------------------------------------------------------*
*&  Display Result
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT11 USING PT_RESULT TYPE GTTY_RESULT11.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
  GF_ALV_HEADER_1 = GC_TRUE.
* Soft refresh data
  GF_SOFT_REFRESH_1 = GC_TRUE.

* ALV Layout
  PERFORM F_ALV_LAYOUT11 CHANGING GS_LAYOUT_1
                                  GS_VARIANT_1
                                  GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_11.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_11.
  ASSIGN PT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2215424]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT11 CHANGING GT_FIELDCAT_1.
* Sort data
  PERFORM F_ALV_SORT_RESULT11 CHANGING GT_SORT_1.
* Call ALV Screen
  CALL SCREEN 9000.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_build_fieldcat11
*&---------------------------------------------------------------------*
*& ALV Field Catalog for Report11
*&---------------------------------------------------------------------*

FORM F_ALV_BUILD_FIELDCAT11 CHANGING PT_FIELDCAT TYPE LVC_T_FCAT.

  DATA:
    LV_TEXT         TYPE  TEXT50.

  FIELD-SYMBOLS:
    <LFS_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_11
                              CHANGING PT_FIELDCAT.

  LOOP AT PT_FIELDCAT ASSIGNING <LFS_FIELDCAT>.

*   Force show all by default
    <LFS_FIELDCAT>-NO_OUT = SPACE.

    CASE <LFS_FIELDCAT>-FIELDNAME.
      WHEN 'ROWNO'.
        <LFS_FIELDCAT>-KEY       = GC_TRUE.
        <LFS_FIELDCAT>-OUTPUTLEN = 6 ##NUMBER_OK.
      WHEN 'STATU'.
        <LFS_FIELDCAT>-KEY       = GC_TRUE.
*       Text-c01 : Status
        LV_TEXT                  = TEXT-C01.
        <LFS_FIELDCAT>-REPTEXT   = LV_TEXT.
        <LFS_FIELDCAT>-COLTEXT   = LV_TEXT.
        <LFS_FIELDCAT>-SCRTEXT_L = LV_TEXT.
        <LFS_FIELDCAT>-SCRTEXT_M = LV_TEXT.
        <LFS_FIELDCAT>-SCRTEXT_S = LV_TEXT.
        <LFS_FIELDCAT>-OUTPUTLEN = 6 ##NUMBER_OK.
      WHEN 'MSGTY'.
        <LFS_FIELDCAT>-NO_OUT = GC_TRUE.
      WHEN 'MSGID'.
        <LFS_FIELDCAT>-NO_OUT = GC_TRUE.
      WHEN 'MSGNO'.
        <LFS_FIELDCAT>-NO_OUT = GC_TRUE.
      WHEN 'MSGTX'.
        <LFS_FIELDCAT>-KEY       = GC_TRUE.
        <LFS_FIELDCAT>-OUTPUTLEN = 50 ##NUMBER_OK.
      WHEN 'RLTYP'.
        <LFS_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'PARTNER'.
        IF CB_TEST IS INITIAL.
          <LFS_FIELDCAT>-HOTSPOT   = GC_TRUE.
        ENDIF.
      WHEN 'BU_GROUP'.
      WHEN 'TYPE'.
      WHEN 'ADDR_NAME1'.
        <LFS_FIELDCAT>-OUTPUTLEN = 25 ##NUMBER_OK.
      WHEN 'ADDR_NAME2'.
        <LFS_FIELDCAT>-OUTPUTLEN = 25 ##NUMBER_OK.
      WHEN 'ADDR_NAME3'.
        <LFS_FIELDCAT>-OUTPUTLEN = 25 ##NUMBER_OK.
      WHEN 'ADDR_NAME4'.
        <LFS_FIELDCAT>-OUTPUTLEN = 25 ##NUMBER_OK.
      WHEN 'ADDR_SORT1'.
        <LFS_FIELDCAT>-OUTPUTLEN = 15 ##NUMBER_OK.
      WHEN 'ADDR_SORT2'.
        <LFS_FIELDCAT>-OUTPUTLEN = 15 ##NUMBER_OK.
      WHEN 'ADDR_STREET'.
        <LFS_FIELDCAT>-OUTPUTLEN = 30 ##NUMBER_OK.
      WHEN 'ADDR_STR_SUPPL3'.
        <LFS_FIELDCAT>-OUTPUTLEN = 30 ##NUMBER_OK.
      WHEN 'ADDR_LOCATION'.
        <LFS_FIELDCAT>-OUTPUTLEN = 30 ##NUMBER_OK.
      WHEN 'ADDR_CITY2'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_CITY1'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_POST_CODE1'.
      WHEN 'ADDR_COUNTRY'.
      WHEN 'ADDR_LANGU'.
      WHEN 'ADDR_TELNO1'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'ADDR_TELEXT1'.
        <LFS_FIELDCAT>-OUTPUTLEN = 5 ##NUMBER_OK.
      WHEN 'ADDR_TELNO2'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'ADDR_TELEXT2'.
        <LFS_FIELDCAT>-OUTPUTLEN = 5 ##NUMBER_OK.
      WHEN 'ADDR_TELNO3'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'ADDR_TELEXT3'.
        <LFS_FIELDCAT>-OUTPUTLEN = 5 ##NUMBER_OK.
      WHEN 'ADDR_TELNO4'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'ADDR_TELEXT4'.
        <LFS_FIELDCAT>-OUTPUTLEN = 5 ##NUMBER_OK.
      WHEN 'ADDR_TELNO5'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'ADDR_TELEXT5'.
        <LFS_FIELDCAT>-OUTPUTLEN = 5 ##NUMBER_OK.
      WHEN 'ADDR_MOBILE1'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'ADDR_MOBILE2'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'ADDR_MOBILE3'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'ADDR_MOBILE4'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'ADDR_MOBILE5'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'ADDR_FAXNO'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'ADDR_FAXEXT'.
        <LFS_FIELDCAT>-OUTPUTLEN = 5 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL01'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL02'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_TELNO_AP'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'ADDR_MOBILE_AP'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL_AP'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDRINT_ACTIVE'.
      WHEN 'ADDRINT_NAME1'.
        <LFS_FIELDCAT>-OUTPUTLEN = 25 ##NUMBER_OK.
      WHEN 'ADDRINT_NAME2'.
        <LFS_FIELDCAT>-OUTPUTLEN = 25 ##NUMBER_OK.
      WHEN 'ADDRINT_NAME3'.
        <LFS_FIELDCAT>-OUTPUTLEN = 25 ##NUMBER_OK.
      WHEN 'ADDRINT_NAME4'.
        <LFS_FIELDCAT>-OUTPUTLEN = 25 ##NUMBER_OK.
      WHEN 'ADDRINT_SORT1'.
        <LFS_FIELDCAT>-OUTPUTLEN = 15 ##NUMBER_OK.
      WHEN 'ADDRINT_STREET'.
        <LFS_FIELDCAT>-OUTPUTLEN = 30 ##NUMBER_OK.
      WHEN 'ADDRINT_STR_SUPPL3'.
        <LFS_FIELDCAT>-OUTPUTLEN = 30 ##NUMBER_OK.
      WHEN 'ADDRINT_LOCATION'.
        <LFS_FIELDCAT>-OUTPUTLEN = 30 ##NUMBER_OK.
      WHEN 'ADDRINT_CITY2'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDRINT_CITY1'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDRINT_POST_CODE1'.
      WHEN 'ADDRINT_COUNTRY'.
      WHEN 'ADDRINT_LANGU'.
      WHEN 'TAX_TAXTYPE'.
      WHEN 'TAX_TAXNUM'.
      WHEN 'BNK01_BKVID'.
      WHEN 'BNK01_BANKS'.
      WHEN 'BNK01_BANKL'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK01_BANKN'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK01_BKONT'.
      WHEN 'BNK01_BKREF'.
      WHEN 'BNK01_BKEXT'.
      WHEN 'BNK02_BKVID'.
      WHEN 'BNK02_BANKS'.
      WHEN 'BNK02_BANKL'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK02_BANKN'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK02_BKONT'.
      WHEN 'BNK02_BKREF'.
      WHEN 'BNK02_BKEXT'.
      WHEN 'VEND_VBUND'.
      WHEN 'VEND_KUNNR'.
      WHEN 'VEND_XZEMP'.
      WHEN 'COMP_BUKRS'.
      WHEN 'COMP_AKONT'.
      WHEN 'COMP_ZUAWA'.
      WHEN 'COMP_FDGRV'.
      WHEN 'COMP_ALTKN'.
      WHEN 'COMP_ZTERM'.
      WHEN 'COMP_REPRF'.
      WHEN 'COMP_ZWELS'.
      WHEN 'COMP_HBKID'.
      WHEN 'WTAX_WITHT1'.
      WHEN 'WTAX_WITHCD1'.
      WHEN 'WTAX_SUBJCT1'.
      WHEN 'WTAX_QSREC1'.
      WHEN 'WTAX_WITHT2'.
      WHEN 'WTAX_WITHCD2'.
      WHEN 'WTAX_SUBJCT2'.
      WHEN 'WTAX_QSREC2'.
      WHEN 'BCODE'.
      WHEN 'BDESC'.
        <LFS_FIELDCAT>-OUTPUTLEN = 15 ##NUMBER_OK.
      WHEN 'BDEFT'.
      WHEN 'PURC_EKORG'.
      WHEN 'PURC_WAERS'.
      WHEN 'PURC_ZTERM'.
      WHEN 'PURC_INCO1'.
      WHEN 'PURC_INCO2'.
      WHEN 'PURC_VERKF'.
      WHEN 'PURC_TELF1'.
      WHEN 'PURC_WEBRE'.
      WHEN 'PURC_EKGRP'.
      WHEN 'PURC_KZAUT'.

      WHEN OTHERS.
        <LFS_FIELDCAT>-TECH = GC_TRUE.

    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_layout11
*&---------------------------------------------------------------------*
*& ALV Layout for Report
*&---------------------------------------------------------------------*
FORM F_ALV_LAYOUT11 CHANGING PS_LAYOUT TYPE LVC_S_LAYO
                              PS_VARIANT TYPE  DISVARIANT
                              PS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR:  PS_LAYOUT, PS_VARIANT, PS_PRINT.

* determine layout
  PS_LAYOUT-SEL_MODE   = 'B'. "Multiple Selection with Push Box
  PS_LAYOUT-CWIDTH_OPT = SPACE.
  PS_LAYOUT-ZEBRA      = GC_TRUE.

* For Variant Saving
  PS_VARIANT-REPORT  = SY-REPID.

  PS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_sort_result11
*&---------------------------------------------------------------------*
*& Maintain SORT data for Result ALV
*&---------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT11 CHANGING PT_SORT TYPE LVC_T_SORT.

  CONSTANTS:
    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'ROWNO'.

  DATA:
    LS_SORT  TYPE  LVC_S_SORT.


* Initialize Output
  CLEAR: PT_SORT.

* Sort by Row Number
  CLEAR LS_SORT.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO PT_SORT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_maintain_extend_comp11
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_MAINTAIN_EXTEND_COMP11 USING PS_DATA TYPE GTY_DATA11
                                     PV_TEST  TYPE  FLAG
                                     PV_ERROR TYPE  FLAG
                            CHANGING PV_PARTNER TYPE GTY_DATA11-KEY-PARTNER ##NEEDED
                                     PT_MESSG TYPE  GTTY_MESSG .
  DATA: LS_RET2   TYPE BAPIRET2 ##NEEDED,
*        ls_logh   TYPE zret_inflogh,
*        lt_loge   TYPE TABLE OF zret_infloge WITH HEADER LINE,
        LT_RETURN TYPE BAPIRET2_T ##NEEDED.

  DATA: LV_LIFNR       TYPE LIFNR,
        LS_COMPANY     TYPE VMDS_EI_COMPANY,
        LT_COMPANY     TYPE VMDS_EI_COMPANY_T,
        LS_DUNNING     TYPE VMDS_EI_DUNNING ##NEEDED,
        LT_DUNNING     TYPE VMDS_EI_DUNNING_T ##NEEDED,
        LS_WTAX        TYPE VMDS_EI_WTAX_TYPE ##NEEDED,
        LT_WTAX        TYPE VMDS_EI_WTAX_TYPE_T ##NEEDED,

        LS_VENDOR      TYPE VMDS_EI_EXTERN,
        LS_MASTER_DATA TYPE VMDS_EI_MAIN ##NEEDED,
        LS_CORRECT     TYPE VMDS_EI_MAIN ##NEEDED,
        LS_MES_CORRECT TYPE CVIS_MESSAGE ##NEEDED,
        LS_DEFECTIVE   TYPE VMDS_EI_MAIN ##NEEDED,
        LS_MES_ERROR   TYPE CVIS_MESSAGE,

        LV_VALIDFROM   LIKE SY-DATUM ##NEEDED,
        LV_VALIDTO     LIKE SY-DATUM ##NEEDED.

  DATA LS_MESSG TYPE GTY_MESSG .
  DATA: LS_LOG_EXTEND TYPE GTYP_LOG_EXTEND,
        LS_LOG_COMP   TYPE GTYP_LOG_COMP.

  LV_LIFNR = PS_DATA-KEY-PARTNER.

  VMD_EI_API=>INITIALIZE( ).


  IF PS_DATA-MAIN-COMP-AKONT IS INITIAL.
    LS_COMPANY-DATA-AKONT = '1102111000'.  "Reconciliation Acct
  ENDIF.

  LS_COMPANY-DATA-AKONT = PS_DATA-MAIN-COMP-AKONT .

  LS_COMPANY-DATA-ZUAWA     = PS_DATA-MAIN-COMP-ZUAWA.
  IF LS_COMPANY-DATA-ZUAWA IS INITIAL .
    LS_COMPANY-DATA-ZUAWA = '035'.               "Sort Key
  ENDIF.

  LS_COMPANY-DATA-ZTERM     = PS_DATA-MAIN-COMP-ZTERM. "Terms of Payment
  LS_COMPANY-DATA-GUZTE     = PS_DATA-MAIN-COMP-ZTERM. "credit memo payment
  LS_COMPANY-DATA-REPRF     = GC_TRUE. "Check double invoice
*    ls_company-data-xausz     = ps_data-main-view_comp-xausz.  "Account Statement
*    ls_company-data-fdgrv      = ps_data-main-view_comp-fdgrv. "Planning Group (Cash Management Group)
*    ls_company-data-vzskz      = ps_data-main-view_comp-vzskz. "Interest Indicator

*    ls_company-data-mgrup     = '02'.              "Dunning notice grouping
  LS_COMPANY-DATA-ALTKN   = PS_DATA-MAIN-COMP-ALTKN.
  LS_COMPANY-DATA-ZUAWA   = PS_DATA-MAIN-COMP-ZUAWA .
  LS_COMPANY-DATA-FDGRV   = PS_DATA-MAIN-COMP-FDGRV .
  LS_COMPANY-DATA-ZTERM   = PS_DATA-MAIN-COMP-ZTERM .
  LS_COMPANY-DATA-REPRF   = PS_DATA-MAIN-COMP-REPRF .
  LS_COMPANY-DATA-ZWELS   = PS_DATA-MAIN-COMP-ZWELS .
  LS_COMPANY-DATA-HBKID   = PS_DATA-MAIN-COMP-HBKID .

*    "Dunning Procedure
*    ls_dunning-data-mahna     = ps_data-main-view_comp-mahna.
*    IF ls_dunning-data-mahna IS INITIAL.
*      ls_dunning-data-mahna = 'Z003'.
*    ENDIF.
*    ls_dunning-data_key-maber = ''.
*    ls_dunning-task           = 'I'.
*    APPEND ls_dunning TO lt_dunning.
*    ls_company-dunning-dunning = lt_dunning[].

  "Withholding Tax
  IF PS_DATA-MAIN-COMP-WTAX1-WITHT IS NOT INITIAL.
    LS_WTAX-DATA_KEY-WITHT = PS_DATA-MAIN-COMP-WTAX1-WITHT.
    LS_WTAX-DATA-WT_WITHCD = PS_DATA-MAIN-COMP-WTAX1-WITHCD.
    LS_WTAX-DATA-WT_SUBJCT = PS_DATA-MAIN-COMP-WTAX1-SUBJCT.
    LS_WTAX-DATA-QSREC     = PS_DATA-MAIN-COMP-WTAX1-QSREC.
    LS_WTAX-TASK           = 'I'.
    APPEND LS_WTAX TO LT_WTAX.
  ENDIF.

  IF PS_DATA-MAIN-COMP-WTAX2-WITHT IS NOT INITIAL.
    LS_WTAX-DATA_KEY-WITHT = PS_DATA-MAIN-COMP-WTAX2-WITHT.
    LS_WTAX-DATA-WT_WITHCD = PS_DATA-MAIN-COMP-WTAX2-WITHCD.
    LS_WTAX-DATA-WT_SUBJCT = PS_DATA-MAIN-COMP-WTAX2-SUBJCT.
    LS_WTAX-DATA-QSREC     = PS_DATA-MAIN-COMP-WTAX2-QSREC.
    LS_WTAX-TASK           = 'I'.
    APPEND LS_WTAX TO LT_WTAX.
  ENDIF.

  LS_COMPANY-WTAX_TYPE-WTAX_TYPE = LT_WTAX[].
  LS_COMPANY-DATA_KEY-BUKRS = PS_DATA-MAIN-COMP-BUKRS.
  LS_COMPANY-TASK           = 'I'.
  APPEND LS_COMPANY TO LT_COMPANY.

  LS_VENDOR-COMPANY_DATA-COMPANY = LT_COMPANY[].

  LS_VENDOR-HEADER-OBJECT_TASK = 'U'.  "Represents update
  LS_VENDOR-HEADER-OBJECT_INSTANCE-LIFNR = LV_LIFNR.
  APPEND LS_VENDOR TO LS_MASTER_DATA-VENDORS.

  VMD_EI_API=>LOCK( IV_LIFNR = LV_LIFNR ).

  CALL METHOD VMD_EI_API=>MAINTAIN_BAPI
    EXPORTING
      IV_TEST_RUN              = PV_TEST
      IV_COLLECT_MESSAGES      = 'X'
      IS_MASTER_DATA           = LS_MASTER_DATA
    IMPORTING
      ES_MASTER_DATA_CORRECT   = LS_CORRECT
      ES_MESSAGE_CORRECT       = LS_MES_CORRECT
      ES_MASTER_DATA_DEFECTIVE = LS_DEFECTIVE
      ES_MESSAGE_DEFECTIVE     = LS_MES_ERROR.

  IF LS_MES_ERROR IS INITIAL.
    IF PV_TEST IS INITIAL AND
       PV_ERROR IS INITIAL.

      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'S'.
      LS_MESSG-MSGID = 'ZTEC'.
      LS_MESSG-MSGNO = '000'.
*     Text-i02: Business Partner data has been created successfully.
      LS_MESSG-MSGTX = TEXT-I02.
      INSERT LS_MESSG INTO TABLE PT_MESSG.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = GC_TRUE.

      CLEAR LS_LOG_EXTEND .
      LS_LOG_EXTEND-PARTNER = PS_DATA-KEY-PARTNER.
      LS_LOG_EXTEND-BUKRS   = PS_DATA-MAIN-COMP-BUKRS.
      APPEND LS_LOG_EXTEND TO GT_LOG_EXTEND .

      CLEAR LS_LOG_COMP .
      LS_LOG_COMP-PARTNER = PS_DATA-KEY-PARTNER.
      LS_LOG_COMP-BUKRS   = PS_DATA-MAIN-COMP-BUKRS.
      LS_LOG_COMP-WITHT1  = PS_DATA-MAIN-COMP-WTAX1-WITHT.
      LS_LOG_COMP-WITHT2  = PS_DATA-MAIN-COMP-WTAX2-WITHT.
      APPEND LS_LOG_COMP TO GT_LOG_COMP .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'S'.
      LS_MESSG-MSGID = 'ZTEC'.
      LS_MESSG-MSGNO = '000'.
*     Text-i01: Test upload successfully.
      LS_MESSG-MSGTX = TEXT-I01.
      INSERT LS_MESSG INTO TABLE PT_MESSG.


    ENDIF.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    READ TABLE LS_MES_ERROR-MESSAGES INTO DATA(LS_MESSAGE) INDEX 1 .
    "Vendor 210002174 has not been created (skip)
    IF LS_MESSAGE-ID NE 'F2' AND LS_MESSAGE-NUMBER NE '163' .
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = LS_MESSAGE-TYPE.
      LS_MESSG-MSGID = LS_MESSAGE-ID .
      LS_MESSG-MSGNO = LS_MESSAGE-NUMBER.
      LS_MESSG-MSGTX = LS_MESSAGE-MESSAGE.
      INSERT LS_MESSG INTO TABLE PT_MESSG.
    ELSE.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'S'.
      LS_MESSG-MSGID = 'ZTEC'.
      LS_MESSG-MSGNO = '000'.
*     Text-i01: Test upload successfully.
      LS_MESSG-MSGTX = TEXT-I01.
      INSERT LS_MESSG INTO TABLE PT_MESSG.
    ENDIF.

  ENDIF.

  VMD_EI_API=>UNLOCK( IV_LIFNR = LV_LIFNR ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_assign_bapi_alt_payee
*&---------------------------------------------------------------------*
*& Assign Alt Payee
*&---------------------------------------------------------------------*
FORM F_ASSIGN_BAPI_ALT_PAYEE
                        USING  PS_VEND  TYPE  GTY_V_VEND
                               PS_VENDX TYPE  GTY_V_VENDX
                     CHANGING  PT_BAPI_ALTPAYEE TYPE VMDS_EI_ALT_PAYEE_T.

  DATA:
*    ls_wtax  TYPE  vmds_ei_wtax_type.
    LS_ALTPAYEE TYPE VMDS_EI_ALT_PAYEE .

* Only when data exist
  IF PS_VENDX IS INITIAL.
    RETURN.
  ENDIF.

  IF PS_VEND-EMPFK IS NOT INITIAL.
    CLEAR LS_ALTPAYEE.
    IF PS_VEND-PAYEE_EXIST EQ GC_TRUE.
      IF PS_VEND-EMPFK IS INITIAL.
        LS_ALTPAYEE-TASK            = 'D'.
      ELSE.
        LS_ALTPAYEE-TASK            = 'U'.
      ENDIF.
    ELSE.
      LS_ALTPAYEE-TASK            = 'I'.
    ENDIF.

    LS_ALTPAYEE-DATA_KEY-EMPFK   = PS_VEND-EMPFK.


    INSERT LS_ALTPAYEE INTO TABLE PT_BAPI_ALTPAYEE.

  ENDIF.
ENDFORM.
