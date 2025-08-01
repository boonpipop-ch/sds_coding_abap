*&---------------------------------------------------------------------*
*& Include          ZSDSCAR0050_F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_process_data_tmp21
*&---------------------------------------------------------------------*
*  Processing data for Template 1
*  General + Company Code View
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA_TMP21 CHANGING PT_RESULT TYPE GTTY_RESULT21
                                    PS_SUM     TYPE  GTY_SUM.

  DATA:
    LT_RAW  TYPE  GTTY_RAW,
    LT_DATA TYPE  GTTY_DATA21.

  DATA:
    LS_MESSG  TYPE  GTY_MESSG,
    LS_RESULT TYPE  GTY_RESULT21.

* Show Progress
* Text-p01 : Reading Input file. . .
  MC_SHOW_PROGRESS 10 TEXT-P01.


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
  PERFORM F_VALIDATE_FILE_TMP21   USING LT_RAW
                               CHANGING LT_DATA.

* --------------------------------
* Step3: Upload data from file
* --------------------------------
* Upload file
  PERFORM F_UPLOAD_FILE_TMP21   USING LT_DATA
                                      CB_TEST
                             CHANGING PT_RESULT
                                      PS_SUM.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_file_tmp21
*&---------------------------------------------------------------------*
*& Validate File data for Template 21 (Customer General ,company view)
*&---------------------------------------------------------------------*
FORM F_VALIDATE_FILE_TMP21 USING PT_RAW TYPE GTTY_RAW
                          CHANGING PT_DATA    TYPE  GTTY_DATA21.

  DATA:
    LS_KEY   TYPE  GTY_KEY21,
    LS_MAIN  TYPE  GTY_MAIN21,
    LS_MAINX TYPE  GTY_MAIN21X,
    LS_MESSG TYPE  GTY_MESSG.

  DATA:
    LV_ROWNO   TYPE  GTY_RESULT21-ROWNO.

  FIELD-SYMBOLS:
    <LFS_RAW>   TYPE  GTY_RAW.


* Initialize Output
  CLEAR: PT_DATA.

* Show Progress
* Text-p02 : Validating file data. . .
  MC_SHOW_PROGRESS 20 TEXT-P02.

  LV_ROWNO = P_BEGROW - 1.

  LOOP AT PT_RAW ASSIGNING <LFS_RAW>.

    LV_ROWNO = LV_ROWNO + 1.

*   Translate Raw line into variables
    PERFORM F_TRANSLATE_RAW21  USING <LFS_RAW>
                            CHANGING LS_KEY
                                     LS_MAIN
                                     LS_MAINX
                                     LS_MESSG.

*   Validate and Append data into table
    PERFORM F_VALIDATE_AND_APPEND21  USING LV_ROWNO
                                           LS_KEY
                                           LS_MAIN
                                           LS_MAINX
                                           LS_MESSG
                                  CHANGING PT_DATA.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_translate_raw21
*&---------------------------------------------------------------------*
*&  Convert Raw to Data 21 Customer
*&---------------------------------------------------------------------*
FORM F_TRANSLATE_RAW21 USING PS_RAW TYPE GTY_RAW
                     CHANGING PS_KEY       TYPE  GTY_KEY21
                              PS_MAIN      TYPE  GTY_MAIN21
                              PS_MAINX     TYPE  GTY_MAIN21X
                              PS_MESSG     TYPE  GTY_MESSG.

  DATA:
    LT_SPLIT  TYPE  STANDARD TABLE OF STRING.

  DATA:
    LV_INDEX    TYPE  I,
    LV_FIELD    TYPE  GTY_FIELD_NAME,
    LV_STRING   TYPE  STRING,
    LV_MSGTX    TYPE  GTY_MESSG-MSGTX,
    LV_REQUIRED TYPE  CHAR1.


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

*   Get Target Field name to assign value
    PERFORM F_GET_TARGET_FIELD21  USING  LV_INDEX
                               CHANGING LV_FIELD.

    CASE LV_FIELD.

      WHEN 'ALTKN' .
        PS_MAIN-VIEW_COMP-ALTKN  = LV_STRING.
        PS_MAINX-VIEW_COMP-ALTKN = GC_TRUE.

      WHEN 'RLTYP'.
        PERFORM F_VALIDATE_ROLE21  USING  LV_STRING
                                CHANGING PS_KEY-RLTYP
                                         LV_MSGTX.

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
        IF PS_KEY-BU_GROUP EQ 'Z070'.     "CH01+
          PS_KEY-TYPE =  GC_TYPE_PERSON.  "1 Person

          PS_MAIN-ADDR-COUNTRY = GC_COUNTRY_TH.
          PS_MAINX-ADDR-COUNTRY = GC_TRUE.
        ELSE.                             "CH01+
          PS_KEY-TYPE =  GC_BU_ORG.       "2 organization
        ENDIF.                            "CH01+
      WHEN 'BPKIND'.
        PERFORM F_VALIDATE_BPKIND  USING  LV_STRING
                                 CHANGING PS_MAIN-BPKIND
                                          LV_MSGTX.

        PS_MAINX-BPKIND = GC_TRUE.

      WHEN 'ADDR_TITLE'.
        PS_MAIN-ADDR-TITLE  = LV_STRING.
        PS_MAINX-ADDR-TITLE = GC_TRUE.

      WHEN 'ADDR_NAME_FIRST'.
        PS_MAIN-ADDR-NAME_FIRST  = LV_STRING.
        PS_MAINX-ADDR-NAME_FIRST = GC_TRUE.

      WHEN 'ADDR_NAME_LAST'.
        PS_MAIN-ADDR-NAME_LAST  = LV_STRING.
        PS_MAINX-ADDR-NAME_LAST = GC_TRUE.

      WHEN 'ADDR_NAME1'.
        PS_MAIN-ADDR-NAME1  = LV_STRING.
        PS_MAINX-ADDR-NAME1 = GC_TRUE.

* BOI - CH01
* For Group Z070, Move NAME1 to NAME_FIRST
        IF PS_KEY-BU_GROUP EQ 'Z070'.
          PS_MAIN-ADDR-NAME_FIRST = LV_STRING.
          PS_MAINX-ADDR-NAME_FIRST = GC_TRUE.

          CLEAR: PS_MAINX-ADDR-NAME1.
        ENDIF.
* EOI - CH01
      WHEN 'ADDR_NAME2'.
        PS_MAIN-ADDR-NAME2  = LV_STRING.
        PS_MAINX-ADDR-NAME2 = GC_TRUE.

* BOI - CH01
* For Group Z070, Move NAME2 to NAME_LAST
        IF PS_KEY-BU_GROUP EQ 'Z070'.
          PS_MAIN-ADDR-NAME_LAST = LV_STRING.
          PS_MAINX-ADDR-NAME_LAST = GC_TRUE.
          CLEAR: PS_MAINX-ADDR-NAME2.
        ENDIF.
* EOI - CH01

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

      WHEN 'ADDR_NAME_CO'.
        PS_MAIN-ADDR-NAME_CO  = LV_STRING.
        PS_MAINX-ADDR-NAME_CO = GC_TRUE.

      WHEN 'ADDR_STR_SUPPL1'.
        PS_MAIN-ADDR-STR_SUPPL1  = LV_STRING.
        PS_MAINX-ADDR-STR_SUPPL1 = GC_TRUE.

      WHEN 'ADDR_STR_SUPPL2'.
        PS_MAIN-ADDR-STR_SUPPL2  = LV_STRING.
        PS_MAINX-ADDR-STR_SUPPL2 = GC_TRUE.

      WHEN 'ADDR_STREET'.
        PS_MAIN-ADDR-STREET  = LV_STRING.
        PS_MAINX-ADDR-STREET = GC_TRUE.

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

      WHEN 'ADDR_MOBILE'.
        PS_MAIN-ADDR-PHONE-MOBILE  = LV_STRING.
        PS_MAINX-ADDR-PHONE-MOBILE = GC_TRUE.

      WHEN 'ADDR_FAXNO'.
        PS_MAIN-ADDR-FAX-FAXNO  = LV_STRING.
        PS_MAINX-ADDR-FAX-FAXNO = GC_TRUE.

      WHEN 'ADDR_FAXEXT'.
        PS_MAIN-ADDR-FAX-FAXEXT  = LV_STRING.
        PS_MAINX-ADDR-FAX-FAXEXT = GC_TRUE.

      WHEN 'ADDR_EMAIL1'.
        PS_MAIN-ADDR-SMTP-EMAIL1  = LV_STRING.
        PS_MAINX-ADDR-SMTP-EMAIL1 = GC_TRUE.

      WHEN 'ADDR_REMARK'.
        PS_MAIN-ADDR-REMARK  = LV_STRING.
        PS_MAINX-ADDR-REMARK = GC_TRUE.

      WHEN 'ADDR_ADEXT'.
        PS_MAIN-ADDR-ADEXT  = LV_STRING.
        PS_MAINX-ADDR-ADEXT = GC_TRUE.

      WHEN 'ADDR_DEFLT_COMM'.
        PS_MAIN-ADDR-DEFLT_COMM  = LV_STRING.
        PS_MAINX-ADDR-DEFLT_COMM = GC_TRUE.

      WHEN 'ADDR_VALID_FROM'.
        PERFORM F_VALIDATE_DATE  USING  LV_STRING
                               CHANGING PS_MAIN-ADDR-VALID_FROM
                                        LV_MSGTX.
        PS_MAINX-ADDR-VALID_FROM = GC_TRUE.
      WHEN 'ADDR_VALID_TO'.
        PERFORM F_VALIDATE_DATE  USING  LV_STRING
                               CHANGING PS_MAIN-ADDR-VALID_TO
                                        LV_MSGTX.
        PS_MAINX-ADDR-VALID_TO = GC_TRUE.

      WHEN 'ADDRINT_ACTIVE'.
        "Check at ADDRINT_NAME1
*        PERFORM f_validate_flag  USING  lv_string
*                               CHANGING ps_main-addr_int-active
*                                        lv_msgtx.

      WHEN 'ADDRINT_NATION'.
        PERFORM F_VALIDATE_NATION  USING  LV_STRING
                                 CHANGING PS_MAIN-ADDR_INT-NATION
                                          LV_MSGTX.

      WHEN 'ADDRINT_TITLE'.
        PS_MAIN-ADDR_INT-TITLE = LV_STRING.
        PS_MAINX-ADDR_INT-TITLE = GC_TRUE.

      WHEN 'ADDRINT_NAME_FIRST'.
        PS_MAIN-ADDR_INT-NAME_FIRST = LV_STRING.
        PS_MAINX-ADDR_INT-NAME_FIRST = GC_TRUE.

      WHEN 'ADDRINT_NAME_LAST'.
        PS_MAIN-ADDR_INT-NAME_LAST = LV_STRING.
        PS_MAINX-ADDR_INT-NAME_LAST = GC_TRUE.

      WHEN 'ADDRINT_NAME1'.
        PS_MAIN-ADDR_INT-NAME1 = LV_STRING.
        PS_MAINX-ADDR_INT-NAME1 = GC_TRUE.

        PS_MAIN-ADDR_INT-ACTIVE  = GC_TRUE .


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

      WHEN 'ADDRINT_SORT2'.
        PS_MAIN-ADDR_INT-SORT2 = LV_STRING.
        PS_MAINX-ADDR_INT-SORT2 = GC_TRUE.


      WHEN 'ADDRINT_NAME_CO'.
        PS_MAIN-ADDR_INT-NAME_CO = LV_STRING.
        PS_MAINX-ADDR_INT-NAME_CO = GC_TRUE.

      WHEN 'ADDRINT_STR_SUPPL1'.
        PS_MAIN-ADDR_INT-STR_SUPPL1 = LV_STRING.
        PS_MAINX-ADDR_INT-STR_SUPPL1 = GC_TRUE.

      WHEN 'ADDRINT_STR_SUPPL2'.
        PS_MAIN-ADDR_INT-STR_SUPPL2 = LV_STRING.
        PS_MAINX-ADDR_INT-STR_SUPPL2 = GC_TRUE.

      WHEN 'ADDRINT_STREET'.
        PS_MAIN-ADDR_INT-STREET = LV_STRING.
        PS_MAINX-ADDR_INT-STREET = GC_TRUE.

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

      WHEN 'ADDRINT_REMARK'.
        PS_MAIN-ADDR_INT-REMARK  = LV_STRING.
        PS_MAINX-ADDR_INT-REMARK = GC_TRUE.

      WHEN 'GENDR'.
        PERFORM F_VALIDATE_GENDER  USING  LV_STRING
                                 CHANGING PS_MAIN-GENDR
                                          LV_MSGTX.
        PS_MAINX-GENDR = GC_TRUE.

      WHEN 'DOBDT'.
        PERFORM F_VALIDATE_DATE  USING  LV_STRING
                               CHANGING PS_MAIN-DOBDT
                                        LV_MSGTX.
        PS_MAINX-DOBDT = GC_TRUE.

      WHEN 'FOUND_DAT'.
        PERFORM F_VALIDATE_DATE  USING  LV_STRING
                               CHANGING PS_MAIN-FOUND_DAT
                                        LV_MSGTX.
        PS_MAINX-FOUND_DAT = GC_TRUE.

      WHEN 'LIQUID_DAT'.
        PERFORM F_VALIDATE_DATE  USING  LV_STRING
                               CHANGING PS_MAIN-LIQUID_DAT
                                        LV_MSGTX.
        PS_MAINX-LIQUID_DAT = GC_TRUE.

      WHEN 'BPEXT'.
        PS_MAIN-BPEXT  = LV_STRING.
        PS_MAINX-BPEXT = GC_TRUE.

      WHEN 'NATPERS'.
        PERFORM F_VALIDATE_FLAG  USING  LV_STRING
                               CHANGING PS_MAIN-TAX-NATPERS
                                        LV_MSGTX.
        PS_MAINX-TAX-NATPERS = GC_TRUE.

      WHEN 'TAXTYPE'.
        PS_MAIN-TAX-TAXTYPE  = LV_STRING.
        PS_MAINX-TAX-TAXTYPE = GC_TRUE.

      WHEN 'TAXNUM'.
        PS_MAIN-TAX-TAXNUM  = LV_STRING.
        PS_MAINX-TAX-TAXNUM = GC_TRUE.

      WHEN 'VBUND'.
        PERFORM F_VALIDATE_TRADEPARTNR  USING  LV_STRING
                                      CHANGING PS_MAIN-VIEW_CUST-VBUND
                                               LV_MSGTX.
        PS_MAINX-VIEW_CUST-VBUND = GC_TRUE.

      WHEN 'LIFNR' .
        PS_MAIN-VIEW_CUST-LIFNR  = LV_STRING.
        PS_MAINX-VIEW_CUST-LIFNR = GC_TRUE.

        ##WHEN_DOUBLE_OK
      WHEN 'BPKIND'.
        PERFORM F_VALIDATE_BPKIND  USING  LV_STRING
                                 CHANGING PS_MAIN-BPKIND
                                          LV_MSGTX.
        PS_MAINX-BPKIND = GC_TRUE.

      WHEN 'TXT_C003'.
        PS_MAIN-LTEXT-TXT_C003  = LV_STRING.
        PS_MAINX-LTEXT-TXT_C003 = GC_TRUE.

      WHEN 'TXT_FLOG'.
        PS_MAIN-LTEXT-TXT_FLOG  = LV_STRING.
        PS_MAINX-LTEXT-TXT_FLOG = GC_TRUE.

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

      WHEN 'BNK01_KOINH'.
        PS_MAIN-BANK01-KOINH  = LV_STRING.
        PS_MAINX-BANK01-KOINH = GC_TRUE.

*      WHEN 'BNK02_BKVID'.
*        ps_main-bank02-bkvid  = lv_string.
*        ps_mainx-bank02-bkvid = gc_true.
*
*      WHEN 'BNK02_BANKS'.
*        ps_main-bank02-banks  = lv_string.
*        ps_mainx-bank02-banks = gc_true.
*
*      WHEN 'BNK02_BANKL'.
*        ps_main-bank02-bankl  = lv_string.
*        ps_mainx-bank02-bankl = gc_true.
*
*      WHEN 'BNK02_BANKN'.
*        ps_main-bank02-bankn  = lv_string.
*        ps_mainx-bank02-bankn = gc_true.
*
*      WHEN 'BNK02_KOINH'.
*        ps_main-bank02-koinh  = lv_string.
*        ps_mainx-bank02-koinh = gc_true.
*
*      WHEN 'BNK03_BKVID'.
*        ps_main-bank03-bkvid  = lv_string.
*        ps_mainx-bank03-bkvid = gc_true.
*
*      WHEN 'BNK03_BANKS'.
*        ps_main-bank03-banks  = lv_string.
*        ps_mainx-bank03-banks = gc_true.
*
*      WHEN 'BNK03_BANKL'.
*        ps_main-bank03-bankl  = lv_string.
*        ps_mainx-bank03-bankl = gc_true.
*
*      WHEN 'BNK03_BANKN'.
*        ps_main-bank03-bankn  = lv_string.
*        ps_mainx-bank03-bankn = gc_true.
*
*      WHEN 'BNK03_KOINH'.
*        ps_main-bank03-koinh  = lv_string.
*        ps_mainx-bank03-koinh = gc_true.
*
*      WHEN 'BNK04_BKVID'.
*        ps_main-bank04-bkvid  = lv_string.
*        ps_mainx-bank04-bkvid = gc_true.
*
*      WHEN 'BNK04_BANKS'.
*        ps_main-bank04-banks  = lv_string.
*        ps_mainx-bank04-banks = gc_true.
*
*      WHEN 'BNK04_BANKL'.
*        ps_main-bank04-bankl  = lv_string.
*        ps_mainx-bank04-bankl = gc_true.
*
*      WHEN 'BNK04_BANKN'.
*        ps_main-bank04-bankn  = lv_string.
*        ps_mainx-bank04-bankn = gc_true.
*
*      WHEN 'BNK04_KOINH'.
*        ps_main-bank04-koinh  = lv_string.
*        ps_mainx-bank04-koinh = gc_true.
*
*      WHEN 'BNK05_BKVID'.
*        ps_main-bank05-bkvid  = lv_string.
*        ps_mainx-bank05-bkvid = gc_true.
*
*      WHEN 'BNK05_BANKS'.
*        ps_main-bank05-banks  = lv_string.
*        ps_mainx-bank05-banks = gc_true.
*
*      WHEN 'BNK05_BANKL'.
*        ps_main-bank05-bankl  = lv_string.
*        ps_mainx-bank05-bankl = gc_true.
*
*      WHEN 'BNK05_BANKN'.
*        ps_main-bank05-bankn  = lv_string.
*        ps_mainx-bank05-bankn = gc_true.
*
*      WHEN 'BNK05_KOINH'.
*        ps_main-bank05-koinh  = lv_string.
*        ps_mainx-bank05-koinh = gc_true.
*
*      WHEN 'BNK06_BKVID'.
*        ps_main-bank06-bkvid  = lv_string.
*        ps_mainx-bank06-bkvid = gc_true.
*
*      WHEN 'BNK06_BANKS'.
*        ps_main-bank06-banks  = lv_string.
*        ps_mainx-bank06-banks = gc_true.
*
*      WHEN 'BNK06_BANKL'.
*        ps_main-bank06-bankl  = lv_string.
*        ps_mainx-bank06-bankl = gc_true.
*
*      WHEN 'BNK06_BANKN'.
*        ps_main-bank06-bankn  = lv_string.
*        ps_mainx-bank06-bankn = gc_true.
*
*      WHEN 'BNK06_KOINH'.
*        ps_main-bank06-koinh  = lv_string.
*        ps_mainx-bank06-koinh = gc_true.

      WHEN 'KUKLA'.
        PERFORM F_VALIDATE_CUST_CLASS  USING  LV_STRING
                                     CHANGING PS_MAIN-VIEW_CUST-KUKLA
                                              LV_MSGTX.
        PS_MAINX-VIEW_CUST-KUKLA = GC_TRUE.

      WHEN 'BRAN1'.
        PERFORM F_VALIDATE_INDUSTRY_CODE  USING  LV_STRING
                                        CHANGING PS_MAIN-VIEW_CUST-BRAN1
                                                 LV_MSGTX.
        PS_MAINX-VIEW_CUST-BRAN1 = GC_TRUE.

      WHEN 'KATR1'.
        PS_MAIN-VIEW_CUST-KATR1  = LV_STRING.
        PS_MAINX-VIEW_CUST-KATR1 = GC_TRUE.

      WHEN 'KDKG1'.
        PS_MAIN-VIEW_CUST-KDKG1  = LV_STRING.
        PS_MAINX-VIEW_CUST-KDKG1 = GC_TRUE.

      WHEN 'KDKG2'.
        PS_MAIN-VIEW_CUST-KDKG2  = LV_STRING.
        PS_MAINX-VIEW_CUST-KDKG2 = GC_TRUE.

      WHEN 'CLASS_VAL'.
        PS_MAIN-VIEW_CUST-CLASS_VAL  = LV_STRING.
        PS_MAINX-VIEW_CUST-CLASS_VAL = GC_TRUE.

*      WHEN 'VIEW_COMP_ACTIVE'.
*        ps_main-view_comp-active = gc_true.
*        PERFORM f_validate_flag  USING  lv_string
*                               CHANGING ps_main-view_comp-active
*                                        lv_msgtx.

      WHEN 'VIEW_COMP_BUKRS'.
        PERFORM F_VALIDATE_COMPCODE  USING  LV_STRING
                                   CHANGING PS_MAIN-VIEW_COMP-BUKRS
                                            LV_MSGTX.
        PS_MAIN-VIEW_COMP-ACTIVE = GC_TRUE.

      WHEN 'VIEW_COMP_AKONT'.
        PERFORM F_VALIDATE_GLACCOUNT  USING  LV_STRING
                                             PS_MAIN-VIEW_COMP-BUKRS
                                    CHANGING PS_MAIN-VIEW_COMP-AKONT
                                             LV_MSGTX.
        PS_MAINX-VIEW_COMP-AKONT = GC_TRUE.

      WHEN 'VIEW_COMP_ZUAWA'.
        PS_MAIN-VIEW_COMP-ZUAWA  = LV_STRING.
        PS_MAINX-VIEW_COMP-ZUAWA = GC_TRUE.

      WHEN 'VIEW_COMP_ZTERM'.
        PERFORM F_VALIDATE_PAYMENT_TERM  USING  LV_STRING
                                       CHANGING PS_MAIN-VIEW_COMP-ZTERM
                                                LV_MSGTX.
        PS_MAINX-VIEW_COMP-ZTERM = GC_TRUE.

      WHEN 'VIEW_COMP_XZVER'.
        PERFORM F_VALIDATE_FLAG  USING  LV_STRING
                               CHANGING PS_MAIN-VIEW_COMP-XZVER
                                        LV_MSGTX.
        PS_MAINX-VIEW_COMP-XZVER = GC_TRUE.

      WHEN 'VIEW_COMP_XVERR'.
        PERFORM F_VALIDATE_FLAG  USING  LV_STRING
                               CHANGING PS_MAIN-VIEW_COMP-XVERR
                                        LV_MSGTX.
        PS_MAINX-VIEW_COMP-XVERR = GC_TRUE.

      WHEN 'VIEW_COMP_KNRZB'.
        PERFORM F_VALIDATE_CUSTOMER  USING  LV_STRING
                                   CHANGING PS_MAIN-VIEW_COMP-KNRZB
                                            LV_MSGTX.
        PS_MAINX-VIEW_COMP-KNRZB = GC_TRUE.

      WHEN 'WT1_WITHT'.
        PS_MAIN-VIEW_COMP-WTAX01-WITHT  = LV_STRING.
        PS_MAINX-VIEW_COMP-WTAX01-WITHT = GC_TRUE.

      WHEN 'WT1_WITHCD'.
        PS_MAIN-VIEW_COMP-WTAX01-WITHCD  = LV_STRING.
        PS_MAINX-VIEW_COMP-WTAX01-WITHCD = GC_TRUE.

      WHEN 'WT1_AGENT'.
        PERFORM F_VALIDATE_FLAG  USING  LV_STRING
                               CHANGING PS_MAIN-VIEW_COMP-WTAX01-AGENT
                                        LV_MSGTX.
        PS_MAINX-VIEW_COMP-WTAX01-AGENT = GC_TRUE.

      WHEN 'WT1_WTSTCD'.
        PS_MAIN-VIEW_COMP-WTAX01-WTSTCD  = LV_STRING.
        PS_MAINX-VIEW_COMP-WTAX01-WTSTCD = GC_TRUE.

      WHEN 'WT1_AGTDF'.
        PERFORM F_VALIDATE_DATE  USING  LV_STRING
                               CHANGING PS_MAIN-VIEW_COMP-WTAX01-AGTDF
                                        LV_MSGTX.
        PS_MAINX-VIEW_COMP-WTAX01-AGTDF = GC_TRUE.

      WHEN 'WT1_AGTDT'.
        PERFORM F_VALIDATE_DATE  USING  LV_STRING
                               CHANGING PS_MAIN-VIEW_COMP-WTAX01-AGTDT
                                        LV_MSGTX.
        PS_MAINX-VIEW_COMP-WTAX01-AGTDT = GC_TRUE.

      WHEN 'WT2_WITHT'.
        PS_MAIN-VIEW_COMP-WTAX02-WITHT  = LV_STRING.
        PS_MAINX-VIEW_COMP-WTAX02-WITHT = GC_TRUE.

      WHEN 'WT2_WITHCD'.
        PS_MAIN-VIEW_COMP-WTAX02-WITHCD  = LV_STRING.
        PS_MAINX-VIEW_COMP-WTAX02-WITHCD = GC_TRUE.

      WHEN 'WT2_AGENT'.
        PERFORM F_VALIDATE_FLAG  USING  LV_STRING
                               CHANGING PS_MAIN-VIEW_COMP-WTAX02-AGENT
                                        LV_MSGTX.
        PS_MAINX-VIEW_COMP-WTAX02-AGENT = GC_TRUE.

      WHEN 'WT2_WTSTCD'.
        PS_MAIN-VIEW_COMP-WTAX02-WTSTCD  = LV_STRING.
        PS_MAINX-VIEW_COMP-WTAX02-WTSTCD = GC_TRUE.

      WHEN 'WT2_AGTDF'.
        PERFORM F_VALIDATE_DATE  USING  LV_STRING
                               CHANGING PS_MAIN-VIEW_COMP-WTAX02-AGTDF
                                        LV_MSGTX.
        PS_MAINX-VIEW_COMP-WTAX02-AGTDF = GC_TRUE.

      WHEN 'WT2_AGTDT'.
        PERFORM F_VALIDATE_DATE  USING  LV_STRING
                               CHANGING PS_MAIN-VIEW_COMP-WTAX02-AGTDT
                                        LV_MSGTX.
        PS_MAINX-VIEW_COMP-WTAX02-AGTDT = GC_TRUE.

      WHEN 'WT3_WITHT'.
        PS_MAIN-VIEW_COMP-WTAX03-WITHT  = LV_STRING.
        PS_MAINX-VIEW_COMP-WTAX03-WITHT = GC_TRUE.

      WHEN 'WT3_WITHCD'.
        PS_MAIN-VIEW_COMP-WTAX03-WITHCD  = LV_STRING.
        PS_MAINX-VIEW_COMP-WTAX03-WITHCD = GC_TRUE.

      WHEN 'WT3_AGENT'.
        PERFORM F_VALIDATE_FLAG  USING  LV_STRING
                               CHANGING PS_MAIN-VIEW_COMP-WTAX03-AGENT
                                        LV_MSGTX.
        PS_MAINX-VIEW_COMP-WTAX03-AGENT = GC_TRUE.

      WHEN 'WT3_WTSTCD'.
        PS_MAIN-VIEW_COMP-WTAX03-WTSTCD  = LV_STRING.
        PS_MAINX-VIEW_COMP-WTAX03-WTSTCD = GC_TRUE.

      WHEN 'WT3_AGTDF'.
        PERFORM F_VALIDATE_DATE  USING  LV_STRING
                               CHANGING PS_MAIN-VIEW_COMP-WTAX03-AGTDF
                                        LV_MSGTX.
        PS_MAINX-VIEW_COMP-WTAX03-AGTDF = GC_TRUE.

      WHEN 'WT3_AGTDT'.
        PERFORM F_VALIDATE_DATE  USING  LV_STRING
                               CHANGING PS_MAIN-VIEW_COMP-WTAX03-AGTDT
                                        LV_MSGTX.
        PS_MAINX-VIEW_COMP-WTAX03-AGTDT = GC_TRUE.


      WHEN 'WT4_WITHT'.
        PS_MAIN-VIEW_COMP-WTAX04-WITHT  = LV_STRING.
        PS_MAINX-VIEW_COMP-WTAX04-WITHT = GC_TRUE.

      WHEN 'WT4_WITHCD'.
        PS_MAIN-VIEW_COMP-WTAX04-WITHCD  = LV_STRING.
        PS_MAINX-VIEW_COMP-WTAX04-WITHCD = GC_TRUE.

      WHEN 'WT4_AGENT'.
        PERFORM F_VALIDATE_FLAG  USING  LV_STRING
                               CHANGING PS_MAIN-VIEW_COMP-WTAX04-AGENT
                                        LV_MSGTX.
        PS_MAINX-VIEW_COMP-WTAX04-AGENT = GC_TRUE.

      WHEN 'WT4_WTSTCD'.
        PS_MAIN-VIEW_COMP-WTAX04-WTSTCD  = LV_STRING.
        PS_MAINX-VIEW_COMP-WTAX04-WTSTCD = GC_TRUE.

      WHEN 'WT4_AGTDF'.
        PERFORM F_VALIDATE_DATE  USING  LV_STRING
                               CHANGING PS_MAIN-VIEW_COMP-WTAX04-AGTDF
                                        LV_MSGTX.
        PS_MAINX-VIEW_COMP-WTAX04-AGTDF = GC_TRUE.

      WHEN 'WT4_AGTDT'.
        PERFORM F_VALIDATE_DATE  USING  LV_STRING
                               CHANGING PS_MAIN-VIEW_COMP-WTAX04-AGTDT
                                        LV_MSGTX.
        PS_MAINX-VIEW_COMP-WTAX04-AGTDT = GC_TRUE.



      WHEN 'VIEW_COMP_J_1TPBUPL'.
        PS_MAIN-BRANCH-J_1TPBUPL  = LV_STRING.
        PS_MAINX-BRANCH-J_1TPBUPL = GC_TRUE.

      WHEN 'VIEW_COMP_DESCRIPTION'.
        PS_MAIN-BRANCH-DESCRIPTION  = LV_STRING.
        PS_MAINX-BRANCH-DESCRIPTION = GC_TRUE.

      WHEN 'LEGAL_ENTY'.
        PS_MAIN-LEGAL_ENTY  = LV_STRING.
        PS_MAINX-LEGAL_ENTY = GC_TRUE.

      WHEN 'LEGAL_ORG'.
        PS_MAIN-LEGAL_ORG  = LV_STRING.
        PS_MAINX-LEGAL_ORG = GC_TRUE.

* BOI - CH01
      WHEN 'RECNO'.
        PS_KEY-RECNO = LV_STRING.

      WHEN 'ADDR_TRANSPZONE'.
        PS_MAIN-ADDR-TRANSPZONE = LV_STRING.
        PS_MAINX-ADDR-TRANSPZONE = GC_TRUE.

      WHEN 'VIEW_CUST_KUKLA'.
        PERFORM F_VALIDATE_CUST_CLASS  USING  LV_STRING
                                     CHANGING PS_MAIN-VIEW_CUST-KUKLA
                                              LV_MSGTX.
        PS_MAINX-VIEW_CUST-KUKLA = GC_TRUE.

      WHEN 'BNK01_BVTYP'.
        PS_MAIN-BANK01-BKVID  = LV_STRING.
        PS_MAINX-BANK01-BKVID = GC_TRUE.

      WHEN 'VIEW_COMP_ALTKN'.
        PS_MAIN-VIEW_COMP-ALTKN = LV_STRING.
        PS_MAINX-VIEW_COMP-ALTKN = GC_TRUE.

      WHEN 'WT1_QSREC'.
        PS_MAIN-VIEW_COMP-WTAX01-QSREC  = LV_STRING.
        PS_MAINX-VIEW_COMP-WTAX01-QSREC = GC_TRUE.
      WHEN 'WT2_QSREC'.
        PS_MAIN-VIEW_COMP-WTAX02-QSREC  = LV_STRING.
        PS_MAINX-VIEW_COMP-WTAX02-QSREC = GC_TRUE.
      WHEN 'VIEW_COMP_FDGRV'.
        PS_MAIN-VIEW_COMP-FDGRV = LV_STRING.
        PS_MAINX-VIEW_COMP-FDGRV = GC_TRUE.
      WHEN 'VIEW_COMP_MAHNA'.
        PS_MAIN-VIEW_COMP-MAHNA = LV_STRING.
        PS_MAINX-VIEW_COMP-MAHNA = GC_TRUE.
      WHEN 'VIEW_COMP_BUSAB'.
        PS_MAIN-VIEW_COMP-BUSAB = LV_STRING.
        PS_MAINX-VIEW_COMP-BUSAB = GC_TRUE.
      WHEN 'VIEW_COMP_KVERM'.
        PS_MAIN-VIEW_COMP-KVERM = LV_STRING.
        PS_MAINX-VIEW_COMP-KVERM = GC_TRUE.
      WHEN 'VIEW_COMP_TLFXS'.
        PS_MAIN-VIEW_COMP-TLFXS = LV_STRING.
        PS_MAINX-VIEW_COMP-TLFXS = GC_TRUE.
      WHEN 'VIEW_COMP_INTAD'.
        PS_MAIN-VIEW_COMP-INTAD = LV_STRING.
        PS_MAINX-VIEW_COMP-INTAD = GC_TRUE.
      WHEN 'VIEW_COMP_XAUSZ'.
        PS_MAIN-VIEW_COMP-XAUSZ = LV_STRING.
        PS_MAINX-VIEW_COMP-XAUSZ = GC_TRUE.
      WHEN 'VIEW_COMP_SPERR'.
        PS_MAIN-VIEW_COMP-SPERR = LV_STRING.
        PS_MAINX-VIEW_COMP-SPERR = GC_TRUE.

*      WHEN 'ADDR_EMAIL1'.
*        PS_MAIN-ADDR-SMTP-EMAIL1 = LV_STRING.
*        PS_MAINX-ADDR-SMTP-EMAIL1 = GC_TRUE.
      WHEN 'ADDR_EMAIL2'.
        PS_MAIN-ADDR-SMTP-EMAIL2 = LV_STRING.
        PS_MAINX-ADDR-SMTP-EMAIL2 = GC_TRUE.
      WHEN 'ADDR_EMAIL3'.
        PS_MAIN-ADDR-SMTP-EMAIL3 = LV_STRING.
        PS_MAINX-ADDR-SMTP-EMAIL3 = GC_TRUE.
      WHEN 'ADDR_EMAIL4'.
        PS_MAIN-ADDR-SMTP-EMAIL4 = LV_STRING.
        PS_MAINX-ADDR-SMTP-EMAIL4 = GC_TRUE.
      WHEN 'ADDR_EMAIL5'.
        PS_MAIN-ADDR-SMTP-EMAIL5 = LV_STRING.
        PS_MAINX-ADDR-SMTP-EMAIL5 = GC_TRUE.
      WHEN 'ADDR_EMAIL6'.
        PS_MAIN-ADDR-SMTP-EMAIL6 = LV_STRING.
        PS_MAINX-ADDR-SMTP-EMAIL6 = GC_TRUE.
      WHEN 'ADDR_EMAIL7'.
        PS_MAIN-ADDR-SMTP-EMAIL7 = LV_STRING.
        PS_MAINX-ADDR-SMTP-EMAIL7 = GC_TRUE.
      WHEN 'ADDR_REGION'.
        PS_MAIN-ADDR-REGION = LV_STRING.
        PS_MAINX-ADDR-REGION = GC_TRUE.
      WHEN 'ADDR_HOME_CITY'.
        PS_MAIN-ADDR-HOME_CITY = LV_STRING.
        PS_MAINX-ADDR-HOME_CITY = GC_TRUE.
* EOI - CH01

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
*& Form f_validate_and_append21
*&---------------------------------------------------------------------*
*& Validate Whole Line data and collect into internal table for posting
*&---------------------------------------------------------------------*
FORM F_VALIDATE_AND_APPEND21 USING PV_ROWNO TYPE GTY_RESULT21-ROWNO
                                    PS_KEY    TYPE  GTY_KEY21
                                    PS_MAIN   TYPE  GTY_MAIN21
                                    PS_MAINX  TYPE  GTY_MAIN21X
                                    PS_MESSG  TYPE  GTY_MESSG
                           CHANGING PT_DATA   TYPE  GTTY_DATA21.

  DATA:
    LS_KEY   TYPE  GTY_KEY21,
    LS_MAIN  TYPE  GTY_MAIN21,
    LS_MAINX TYPE  GTY_MAIN21X,
    LS_DATA  TYPE  GTY_DATA21,
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
    PERFORM F_VALIDATE_KEY21 USING    LS_MAIN
                             CHANGING LS_KEY
                                     LS_MESSG.
    IF LS_MESSG IS NOT INITIAL.
      EXIT.
    ENDIF.

*  ---------------------------------
*   Validate Main data
*  ---------------------------------
    PERFORM F_VALIDATE_MAIN21  USING  LS_KEY
                            CHANGING LS_MAIN
                                     LS_MAINX
                                     LS_MESSG.
    IF LS_MESSG IS NOT INITIAL.
      EXIT.
    ENDIF.

*  Only for Customer Role
*    IF LS_KEY-RLTYP EQ GC_ROLE2_1.   "CH01-
    IF LS_KEY-RLTYP EQ GC_ROLE2_2.   "CH01+
*    ---------------------------------
*     Validate Customer
*    ---------------------------------
      PERFORM F_VALIDATE_VIEW_CUST  USING  LS_KEY
                                  CHANGING LS_MAIN-VIEW_CUST
                                           LS_MESSG.
      IF LS_MESSG IS NOT INITIAL.
        EXIT.
      ENDIF.

*    ---------------------------------
*     Validate Company
*    ---------------------------------
      PERFORM F_VALIDATE_VIEW_COMP  USING  LS_KEY
                                           LS_MAIN-VIEW_CUST
                                  CHANGING LS_MAIN-VIEW_COMP
                                           LS_MAINX-VIEW_COMP
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
*& Form f_validate_view_cust
*&---------------------------------------------------------------------*
*& Validate Customer View data
*&---------------------------------------------------------------------*
FORM F_VALIDATE_VIEW_CUST USING PS_KEY TYPE GTY_KEY21
                          CHANGING PS_VIEW_CUST TYPE  GTY_VIEW_CUST
                                   PS_MESSG     TYPE  GTY_MESSG.

  DATA:
    LT_RETURN TYPE  BAPIRET2_T.

  FIELD-SYMBOLS:
    <LFS_RETURN>  TYPE  BAPIRET2.


* Initialize Output
  CLEAR: PS_MESSG.

* Validate Grouping for Customer
  CALL METHOD FSBP_BO_CVI=>VALIDATE_GROUPING_FOR_CUST
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

* Get Customer from Partner
  PERFORM F_GET_CUSTOMER_FROM_PARTNER  USING  PS_KEY-PARTNER
                                     CHANGING PS_VIEW_CUST-KUNNR.
  IF PS_VIEW_CUST-KUNNR IS NOT INITIAL.
    PS_VIEW_CUST-EXIST = GC_TRUE.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_customer_from_partner
*&---------------------------------------------------------------------*
*& Get Customer From Partner
*&---------------------------------------------------------------------*
FORM F_GET_CUSTOMER_FROM_PARTNER
                           USING PV_PARTNER  TYPE  BUT000-PARTNER
                        CHANGING PV_KUNNR    TYPE KNA1-KUNNR.

* Initlialize Output
  CLEAR: PV_KUNNR.

  SELECT SINGLE KUNNR
    INTO PV_KUNNR
    FROM KNA1
   WHERE KUNNR EQ PV_PARTNER.                        "#EC CI_SEL_NESTED
  IF SY-SUBRC NE 0.
*   Try to find Customer Code
    SELECT A~CUSTOMER
        UP TO 1 ROWS
      INTO PV_KUNNR
      FROM CVI_CUST_LINK AS A
             INNER JOIN BUT000 AS B
               ON  B~PARTNER_GUID = A~PARTNER_GUID
     WHERE B~PARTNER EQ PV_PARTNER
     ORDER BY A~CUSTOMER ASCENDING.                  "#EC CI_SEL_NESTED
    ENDSELECT.
    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_role21
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_STRING
*&      <-- PS_KEY_RLTYP
*&      <-- LV_MSGTX
*&---------------------------------------------------------------------*
FORM F_VALIDATE_ROLE21 USING PV_STRING TYPE STRING
                     CHANGING PV_RLTYP   TYPE  BUT100-RLTYP
                              PV_MSGTX   TYPE  CLIKE.

* Initialize Output
  CLEAR: PV_RLTYP,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

  IF PV_STRING NE GC_ROLE2_1 AND
     PV_STRING NE GC_ROLE2_2 AND
     PV_STRING NE GC_ROLE2_3.         "CH01+
*   Text-e01 : Invalid BP Role:
    CONCATENATE TEXT-E01 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Assign Output
  PV_RLTYP = PV_STRING.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_target_field21
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_INDEX
*&      <-- LV_FIELD
*&---------------------------------------------------------------------*
FORM F_GET_TARGET_FIELD21 USING PV_INDEX TYPE I
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
    LREF_DESCR = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( 'GTY_TEMPLATE21' ).
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
*& Form f_upload_file_tmp21
*&---------------------------------------------------------------------*
*& Upload File Template 21
*&---------------------------------------------------------------------*

FORM F_UPLOAD_FILE_TMP21 USING PT_DATA TYPE GTTY_DATA21
                                 PV_TEST  TYPE  FLAG
                        CHANGING PT_RESULT TYPE  GTTY_RESULT21
                                 PS_SUM    TYPE  GTY_SUM.
*  TYPES: BEGIN OF lty_knb1 ,
*           kunnr TYPE knb1-kunnr,
*           bukrs TYPE knb1-bukrs,
*         END   OF lty_knb1 .

  DATA:
    LT_MESSG TYPE  GTTY_MESSG.

  DATA:
    LS_DATA  TYPE  GTY_DATA21.

  DATA:
    LV_PARTNER TYPE  BUT000-PARTNER,
    LV_ERROR   TYPE  FLAG.

  DATA:
*    lv_bukrs TYPE knb1-bukrs,
*    lv_kunnr TYPE knb1-kunnr,
*    ls_knb1 TYPE lty_knb1,
    LV_GUID TYPE BAPIBUS1006_HEAD-PARTNGUID.

*  DATA lt_knb1 TYPE TABLE OF lty_knb1 .

* Initialize Output
  CLEAR: PT_RESULT.
  CLEAR:   PS_SUM.

* Show Progress
* Text-p03 : Uploading file data. . .
  MC_SHOW_PROGRESS 45 TEXT-P03.

  IF PT_DATA IS NOT INITIAL.
    SELECT KUNNR, BUKRS INTO TABLE @GT_LOG_EXTEND
      FROM KNB1
       FOR ALL ENTRIES IN @PT_DATA
     WHERE KUNNR = @PT_DATA-KEY-PARTNER .
    IF SY-SUBRC = 0 .
      SORT GT_LOG_EXTEND BY PARTNER BUKRS .
    ENDIF.
  ENDIF.

* Processing each record
  LOOP AT PT_DATA INTO LS_DATA.
    CLEAR LV_ERROR.
*   Only non validation error
    IF LS_DATA-MESSG[] IS INITIAL.
*       Implement suitable error handling here
      CALL FUNCTION 'BAPI_BUPA_GET_NUMBERS'
        EXPORTING
          BUSINESSPARTNER        = LS_DATA-KEY-PARTNER
        IMPORTING
*         BUSINESSPARTNEROUT     =
          BUSINESSPARTNERGUIDOUT = LV_GUID.

*      IF gv_mode = gc_mode_create .
*        READ TABLE lt_knb1 TRANSPORTING NO FIELDS WITH KEY kunnr = ls_data-key-partner  .
*        IF sy-subrc = 0.
*          ls_data-key-extend_comp = gc_true .
*          MOVE lv_guid TO ls_data-key-partner_guid.
*        ELSE.
*          CLEAR ls_data-key-extend_comp.
*        ENDIF.
*      ENDIF.

      IF GV_MODE = GC_MODE_CREATE .
        SORT GT_LOG_EXTEND BY PARTNER BUKRS .
        DELETE ADJACENT DUPLICATES FROM GT_LOG_EXTEND
                        COMPARING PARTNER BUKRS.

        READ TABLE GT_LOG_EXTEND TRANSPORTING NO FIELDS
          WITH KEY PARTNER = LS_DATA-KEY-PARTNER
                   BUKRS = LS_DATA-MAIN-VIEW_COMP-BUKRS.
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

      IF LS_DATA-KEY-EXTEND_COMP IS NOT INITIAL .
        "Extend company
        PERFORM F_MAINTAIN_EXTEND_COMP21  USING  LS_DATA
                                                 PV_TEST
                                        CHANGING LV_PARTNER
                                                 LT_MESSG
                                                 LV_ERROR.
        IF LV_PARTNER IS NOT INITIAL.
          LS_DATA-KEY-PARTNER = LV_PARTNER.
        ENDIF.
        LS_DATA-MESSG = LT_MESSG.

      ELSE.
        PERFORM F_MAINTAIN_PARTNER  USING  PV_TEST
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
*        CLEAR ls_knb1 .
*        ls_knb1-kunnr = ls_data-key-partner .
*        ls_knb1-bukrs = ls_data-main-view_comp-bukrs .
*        APPEND ls_knb1 TO lt_knb1 .
*      ENDIF.
    ENDIF.

*   Collect Result
    PERFORM F_COLLECT_RESULT21  USING LS_DATA
                             CHANGING PT_RESULT.

    CLEAR: LT_MESSG[], LV_ERROR .
  ENDLOOP.

* Show Final Message for Processing completed
* Message: Processing completed.
  MESSAGE S582(1FA).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_partner
*&---------------------------------------------------------------------*
*& Validate Partner Number
*&---------------------------------------------------------------------*
FORM F_VALIDATE_PARTNER USING PV_STRING TYPE STRING
                                PV_MODE    TYPE  CHAR1
                       CHANGING PV_PARTNER TYPE  BUT000-PARTNER
                                PV_GUID    TYPE  BUT000-PARTNER_GUID
                                PV_MSGTX   TYPE  CLIKE.

  DATA:
    LV_PARTNER TYPE  BUT000-PARTNER,
    LV_EXIST   TYPE  FLAG.


* Initialize Output
  CLEAR: PV_PARTNER,
         PV_GUID,
         PV_MSGTX.

* Only value exists
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

  IF STRLEN( PV_STRING ) GT 10.
*   Text-e03 : Invalid BP Number:
    CONCATENATE TEXT-E03 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = PV_STRING
    IMPORTING
      OUTPUT = LV_PARTNER.

  CLEAR LV_EXIST.
  SELECT SINGLE PARTNER_GUID
    INTO PV_GUID
    FROM BUT000
   WHERE PARTNER EQ LV_PARTNER.                      "#EC CI_SEL_NESTED
  IF SY-SUBRC EQ 0.
    LV_EXIST = GC_TRUE.
  ENDIF.

* Create Mode
  IF PV_MODE EQ GC_MODE_CREATE.
*    IF lv_exist EQ gc_true.
**     Text-e02 : BP Number already exists:
*      CONCATENATE TEXT-e02 pv_string
*             INTO pv_msgtx
*        SEPARATED BY space.
*      RETURN.
*    ENDIF.

* Change Mode
  ELSEIF PV_MODE EQ GC_MODE_CHANGE.
    IF LV_EXIST IS INITIAL.
*     Text-e03 : Invalid BP Number:
      CONCATENATE TEXT-E03 PV_STRING
             INTO PV_MSGTX
        SEPARATED BY SPACE.
      RETURN.
    ENDIF.

  ENDIF.

* Assign Output
  PV_PARTNER = LV_PARTNER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_collect_result21
*&---------------------------------------------------------------------*
*& Collect Processing Result
*&---------------------------------------------------------------------*

FORM F_COLLECT_RESULT21 USING PS_DATA TYPE GTY_DATA21
                      CHANGING PT_RESULT TYPE  GTTY_RESULT21.

  DATA:
    LS_RESULT  TYPE  GTY_RESULT21.

  FIELD-SYMBOLS:
    <LFS_MESSG> TYPE  GTY_MESSG.


  CLEAR: LS_RESULT.

  LS_RESULT-ROWNO   = PS_DATA-ROWNO.
  LS_RESULT-RLTYP    = PS_DATA-KEY-RLTYP.
  LS_RESULT-PARTNER = PS_DATA-KEY-PARTNER.
  LS_RESULT-BU_GROUP = PS_DATA-KEY-BU_GROUP.
  LS_RESULT-TYPE = PS_DATA-KEY-TYPE.
  LS_RESULT-TITLE = PS_DATA-MAIN-ADDR-TITLE.
  LS_RESULT-ADDR_NAME1 = PS_DATA-MAIN-ADDR-NAME1.
  LS_RESULT-ADDR_NAME2 = PS_DATA-MAIN-ADDR-NAME2.
  LS_RESULT-ADDR_NAME3 = PS_DATA-MAIN-ADDR-NAME3.
  LS_RESULT-ADDR_NAME4 = PS_DATA-MAIN-ADDR-NAME4.
  LS_RESULT-ADDR_SORT1 = PS_DATA-MAIN-ADDR-SORT1.
  LS_RESULT-ADDR_SORT2 = PS_DATA-MAIN-ADDR-SORT2.
*  ls_result-addr_building = ps_data-main-addr-building.
  LS_RESULT-ADDR_NAME_CO = PS_DATA-MAIN-ADDR-NAME_CO.
*  ls_result-addr_house_num1 = ps_data-main-addr-house_num1.
  LS_RESULT-ADDR_STR_SUPPL1 = PS_DATA-MAIN-ADDR-STR_SUPPL1.
  LS_RESULT-ADDR_STR_SUPPL2 = PS_DATA-MAIN-ADDR-STR_SUPPL2.
  LS_RESULT-ADDR_STREET = PS_DATA-MAIN-ADDR-STREET.
  LS_RESULT-ADDR_STR_SUPPL3 = PS_DATA-MAIN-ADDR-STR_SUPPL3.
  LS_RESULT-ADDR_LOCATION = PS_DATA-MAIN-ADDR-LOCATION.
  LS_RESULT-ADDR_CITY2 = PS_DATA-MAIN-ADDR-CITY2.
  LS_RESULT-ADDR_CITY1 = PS_DATA-MAIN-ADDR-CITY1.
  LS_RESULT-ADDR_POST_CODE1 = PS_DATA-MAIN-ADDR-POST_CODE1.
  LS_RESULT-ADDR_COUNTRY = PS_DATA-MAIN-ADDR-COUNTRY.
*  ls_result-addr_region = ps_data-main-addr-region.
*  ls_result-addr_transpzone = ps_data-main-addr-transpzone.
  LS_RESULT-ADDR_LANGU = PS_DATA-MAIN-ADDR-LANGU.
  LS_RESULT-ADDR_TELNO1 = PS_DATA-MAIN-ADDR-PHONE-TELNO1.
  LS_RESULT-ADDR_TELEXT1 = PS_DATA-MAIN-ADDR-PHONE-TELEXT1.
  LS_RESULT-ADDR_TELNO2 = PS_DATA-MAIN-ADDR-PHONE-TELNO2.
  LS_RESULT-ADDR_TELEXT2 = PS_DATA-MAIN-ADDR-PHONE-TELEXT2.
  LS_RESULT-ADDR_MOBILE = PS_DATA-MAIN-ADDR-PHONE-MOBILE.
  LS_RESULT-ADDR_FAXNO = PS_DATA-MAIN-ADDR-FAX-FAXNO.
  LS_RESULT-ADDR_FAXEXT = PS_DATA-MAIN-ADDR-FAX-FAXEXT.
  LS_RESULT-ADDR_EMAIL1 = PS_DATA-MAIN-ADDR-SMTP-EMAIL1.
*  ls_result-addr_email2 = ps_data-main-addr-smtp-email2.
*  ls_result-addr_email3 = ps_data-main-addr-smtp-email3.
*  ls_result-addr_email4 = ps_data-main-addr-smtp-email4.
*  ls_result-addr_email5 = ps_data-main-addr-smtp-email5.
  LS_RESULT-ADDR_REMARK = PS_DATA-MAIN-ADDR-REMARK.
  LS_RESULT-ADDR_ADEXT = PS_DATA-MAIN-ADDR-ADEXT.
  LS_RESULT-ADDR_DEFLT_COMM = PS_DATA-MAIN-ADDR-DEFLT_COMM .
  LS_RESULT-ADDR_VALID_FROM = PS_DATA-MAIN-ADDR-VALID_FROM .
  LS_RESULT-ADDR_VALID_TO   = PS_DATA-MAIN-ADDR-VALID_TO .
  LS_RESULT-ADDRINT_ACTIVE = PS_DATA-MAIN-ADDR_INT-ACTIVE.
  LS_RESULT-ADDRINT_NATION = PS_DATA-MAIN-ADDR_INT-NATION.
  LS_RESULT-ADDRINT_TITLE  = PS_DATA-MAIN-ADDR_INT-TITLE.
  LS_RESULT-ADDRINT_NAME_FIRST = PS_DATA-MAIN-ADDR_INT-NAME_FIRST.
  LS_RESULT-ADDRINT_NAME_LAST = PS_DATA-MAIN-ADDR_INT-NAME_LAST.
  LS_RESULT-ADDRINT_NAME1 = PS_DATA-MAIN-ADDR_INT-NAME1.
  LS_RESULT-ADDRINT_NAME2 = PS_DATA-MAIN-ADDR_INT-NAME2.
  LS_RESULT-ADDRINT_NAME3 = PS_DATA-MAIN-ADDR_INT-NAME3.
  LS_RESULT-ADDRINT_NAME4 = PS_DATA-MAIN-ADDR_INT-NAME4.
  LS_RESULT-ADDRINT_SORT1 = PS_DATA-MAIN-ADDR_INT-SORT1.
  LS_RESULT-ADDRINT_SORT2 = PS_DATA-MAIN-ADDR_INT-SORT2.
  LS_RESULT-ADDRINT_STR_SUPPL1 = PS_DATA-MAIN-ADDR_INT-STR_SUPPL1.
  LS_RESULT-ADDRINT_STR_SUPPL2 = PS_DATA-MAIN-ADDR_INT-STR_SUPPL2.
  LS_RESULT-ADDRINT_STREET = PS_DATA-MAIN-ADDR_INT-STREET.
  LS_RESULT-ADDRINT_STR_SUPPL3 = PS_DATA-MAIN-ADDR_INT-STR_SUPPL3.
  LS_RESULT-ADDRINT_LOCATION = PS_DATA-MAIN-ADDR_INT-LOCATION.
  LS_RESULT-ADDRINT_CITY2 = PS_DATA-MAIN-ADDR_INT-CITY2.
  LS_RESULT-ADDRINT_CITY1 = PS_DATA-MAIN-ADDR_INT-CITY1.
  LS_RESULT-ADDRINT_POST_CODE1 = PS_DATA-MAIN-ADDR_INT-POST_CODE1.
  LS_RESULT-ADDRINT_COUNTRY = PS_DATA-MAIN-ADDR_INT-COUNTRY.
  LS_RESULT-FOUND_DAT = PS_DATA-MAIN-FOUND_DAT.
  LS_RESULT-LIQUID_DAT = PS_DATA-MAIN-LIQUID_DAT.
  LS_RESULT-BPEXT = PS_DATA-MAIN-BPEXT.
  LS_RESULT-NATPERS = PS_DATA-MAIN-TAX-NATPERS.
  LS_RESULT-TAXTYPE = PS_DATA-MAIN-TAX-TAXTYPE.
  LS_RESULT-TAXNUM = PS_DATA-MAIN-TAX-TAXNUM.
  LS_RESULT-VBUND = PS_DATA-MAIN-VIEW_CUST-VBUND.
  LS_RESULT-BPKIND = PS_DATA-MAIN-BPKIND.
  LS_RESULT-TXT_C003 = PS_DATA-MAIN-LTEXT-TXT_C003.
  LS_RESULT-TXT_FLOG = PS_DATA-MAIN-LTEXT-TXT_FLOG.
*  ls_result-bnk01_bkvid = ps_data-main-bank01-bkvid.
  LS_RESULT-BNK01_BANKS = PS_DATA-MAIN-BANK01-BANKS.
  LS_RESULT-BNK01_BANKL = PS_DATA-MAIN-BANK01-BANKL.
  LS_RESULT-BNK01_BANKN = PS_DATA-MAIN-BANK01-BANKN.
  LS_RESULT-BNK01_KOINH = PS_DATA-MAIN-BANK01-KOINH.
*  ls_result-bnk01_accname = ps_data-main-bank01-accname.
*  ls_result-bnk02_bkvid = ps_data-main-bank02-bkvid.
*  ls_result-bnk02_banks = ps_data-main-bank02-banks.
*  ls_result-bnk02_bankl = ps_data-main-bank02-bankl.
*  ls_result-bnk02_bankn = ps_data-main-bank02-bankn.
*  ls_result-bnk02_koinh = ps_data-main-bank02-koinh.
*  ls_result-bnk02_accname = ps_data-main-bank02-accname.
*  ls_result-bnk03_bkvid = ps_data-main-bank03-bkvid.
*  ls_result-bnk03_banks = ps_data-main-bank03-banks.
*  ls_result-bnk03_bankl = ps_data-main-bank03-bankl.
*  ls_result-bnk03_bankn = ps_data-main-bank03-bankn.
*  ls_result-bnk03_koinh = ps_data-main-bank03-koinh.
*  ls_result-bnk03_accname = ps_data-main-bank03-accname.
*  ls_result-bnk04_bkvid = ps_data-main-bank04-bkvid.
*  ls_result-bnk04_banks = ps_data-main-bank04-banks.
*  ls_result-bnk04_bankl = ps_data-main-bank04-bankl.
*  ls_result-bnk04_bankn = ps_data-main-bank04-bankn.
*  ls_result-bnk04_koinh = ps_data-main-bank04-koinh.
*  ls_result-bnk04_accname = ps_data-main-bank04-accname.
*  ls_result-bnk05_bkvid = ps_data-main-bank05-bkvid.
*  ls_result-bnk05_banks = ps_data-main-bank05-banks.
*  ls_result-bnk05_bankl = ps_data-main-bank05-bankl.
*  ls_result-bnk05_bankn = ps_data-main-bank05-bankn.
*  ls_result-bnk05_koinh = ps_data-main-bank05-koinh.
*  ls_result-bnk05_accname = ps_data-main-bank05-accname.
*  ls_result-bnk06_bkvid = ps_data-main-bank06-bkvid.
*  ls_result-bnk06_banks = ps_data-main-bank06-banks.
*  ls_result-bnk06_bankl = ps_data-main-bank06-bankl.
*  ls_result-bnk06_bankn = ps_data-main-bank06-bankn.
*  ls_result-bnk06_koinh = ps_data-main-bank06-koinh.
*  ls_result-bnk06_accname = ps_data-main-bank06-accname.
*  ls_result-bnk07_bkvid = ps_data-main-bank07-bkvid.
*  ls_result-bnk07_banks = ps_data-main-bank07-banks.
*  ls_result-bnk07_bankl = ps_data-main-bank07-bankl.
*  ls_result-bnk07_bankn = ps_data-main-bank07-bankn.
*  ls_result-bnk07_koinh = ps_data-main-bank07-koinh.
*  ls_result-bnk07_accname = ps_data-main-bank07-accname.
*  ls_result-bnk08_bkvid = ps_data-main-bank08-bkvid.
*  ls_result-bnk08_banks = ps_data-main-bank08-banks.
*  ls_result-bnk08_bankl = ps_data-main-bank08-bankl.
*  ls_result-bnk08_bankn = ps_data-main-bank08-bankn.
*  ls_result-bnk08_koinh = ps_data-main-bank08-koinh.
*  ls_result-bnk08_accname = ps_data-main-bank08-accname.
*  ls_result-bnk09_bkvid = ps_data-main-bank09-bkvid.
*  ls_result-bnk09_banks = ps_data-main-bank09-banks.
*  ls_result-bnk09_bankl = ps_data-main-bank09-bankl.
*  ls_result-bnk09_bankn = ps_data-main-bank09-bankn.
*  ls_result-bnk09_koinh = ps_data-main-bank09-koinh.
*  ls_result-bnk09_accname = ps_data-main-bank09-accname.
*  ls_result-bnk10_bkvid = ps_data-main-bank10-bkvid.
*  ls_result-bnk10_banks = ps_data-main-bank10-banks.
*  ls_result-bnk10_bankl = ps_data-main-bank10-bankl.
*  ls_result-bnk10_bankn = ps_data-main-bank10-bankn.
*  ls_result-bnk10_koinh = ps_data-main-bank10-koinh.
*  ls_result-bnk10_accname = ps_data-main-bank10-accname.
  LS_RESULT-KUKLA = PS_DATA-MAIN-VIEW_CUST-KUKLA.
  LS_RESULT-BRAN1 = PS_DATA-MAIN-VIEW_CUST-BRAN1.
  LS_RESULT-KATR1 = PS_DATA-MAIN-VIEW_CUST-KATR1.
  LS_RESULT-KDKG1 = PS_DATA-MAIN-VIEW_CUST-KDKG1.
  LS_RESULT-KDKG2 = PS_DATA-MAIN-VIEW_CUST-KDKG2.
  LS_RESULT-CLASS_VAL = PS_DATA-MAIN-VIEW_CUST-CLASS_VAL.
  LS_RESULT-VIEW_COMP_ACTIVE = PS_DATA-MAIN-VIEW_COMP-ACTIVE.
  LS_RESULT-VIEW_COMP_BUKRS = PS_DATA-MAIN-VIEW_COMP-BUKRS.
  LS_RESULT-VIEW_COMP_AKONT = PS_DATA-MAIN-VIEW_COMP-AKONT.
  LS_RESULT-VIEW_COMP_ZUAWA = PS_DATA-MAIN-VIEW_COMP-ZUAWA.
  LS_RESULT-VIEW_COMP_ZTERM = PS_DATA-MAIN-VIEW_COMP-ZTERM.
  LS_RESULT-VIEW_COMP_XZVER = PS_DATA-MAIN-VIEW_COMP-XZVER.
  LS_RESULT-VIEW_COMP_KNRZB = PS_DATA-MAIN-VIEW_COMP-KNRZB.
  LS_RESULT-WT1_WITHT = PS_DATA-MAIN-VIEW_COMP-WTAX01-WITHT.
  LS_RESULT-WT1_WITHCD = PS_DATA-MAIN-VIEW_COMP-WTAX01-WITHCD.
  LS_RESULT-WT1_AGENT = PS_DATA-MAIN-VIEW_COMP-WTAX01-AGENT.
  LS_RESULT-WT1_WTSTCD = PS_DATA-MAIN-VIEW_COMP-WTAX01-WTSTCD.
  LS_RESULT-WT1_AGTDF = PS_DATA-MAIN-VIEW_COMP-WTAX01-AGTDF.
  LS_RESULT-WT1_AGTDT = PS_DATA-MAIN-VIEW_COMP-WTAX01-AGTDT.
  LS_RESULT-WT2_WITHT = PS_DATA-MAIN-VIEW_COMP-WTAX02-WITHT.
  LS_RESULT-WT2_WITHCD = PS_DATA-MAIN-VIEW_COMP-WTAX02-WITHCD.
  LS_RESULT-WT2_AGENT = PS_DATA-MAIN-VIEW_COMP-WTAX02-AGENT.
  LS_RESULT-WT2_WTSTCD = PS_DATA-MAIN-VIEW_COMP-WTAX02-WTSTCD.
  LS_RESULT-WT2_AGTDF = PS_DATA-MAIN-VIEW_COMP-WTAX02-AGTDF.
  LS_RESULT-WT2_AGTDT = PS_DATA-MAIN-VIEW_COMP-WTAX02-AGTDT.
  LS_RESULT-WT3_WITHT = PS_DATA-MAIN-VIEW_COMP-WTAX03-WITHT.
  LS_RESULT-WT3_WITHCD = PS_DATA-MAIN-VIEW_COMP-WTAX03-WITHCD.
  LS_RESULT-WT3_AGENT = PS_DATA-MAIN-VIEW_COMP-WTAX03-AGENT.
  LS_RESULT-WT3_WTSTCD = PS_DATA-MAIN-VIEW_COMP-WTAX03-WTSTCD.
  LS_RESULT-WT3_AGTDF = PS_DATA-MAIN-VIEW_COMP-WTAX03-AGTDF.
  LS_RESULT-WT3_AGTDT = PS_DATA-MAIN-VIEW_COMP-WTAX03-AGTDT.
  LS_RESULT-VIEW_COMP_J_1TPBUPL = PS_DATA-MAIN-BRANCH-J_1TPBUPL.
  LS_RESULT-VIEW_COMP_DESCRIPTION = PS_DATA-MAIN-BRANCH-DESCRIPTION.

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
*& Form f_maintain_partner
*&---------------------------------------------------------------------*
*& Maintain Business Partner
*&---------------------------------------------------------------------*
FORM F_MAINTAIN_PARTNER USING PV_TEST TYPE FLAG
                       CHANGING PS_DATA  TYPE  GTY_DATA21
                                PV_PARTNER  TYPE  GTY_DATA21-KEY-PARTNER
                                PT_MESSG TYPE  GTTY_MESSG
                                PV_ERROR TYPE  FLAG.

  CONSTANTS:
    LC_DUMMY_PARTNER  TYPE  GTY_DATA21-KEY-PARTNER VALUE '1'.

  DATA:
    LT_BAPI_DATA TYPE  CVIS_EI_EXTERN_T,
    LT_MSGMAP    TYPE  MDG_BS_BP_MSGMAP_T,
    LT_RETURN    TYPE  BAPIRETM.

  DATA:
    LS_BAPI_DATA TYPE  CVIS_EI_EXTERN,
    LS_BAPI_ROLE TYPE  BUS_EI_BUPA_ROLES.

  DATA:
    LV_CUST_EXIST  TYPE FLAG.


* Initialize Output
  CLEAR:: PT_MESSG.
  CLEAR:   PV_PARTNER,
           PV_ERROR.

  CLEAR LS_BAPI_DATA.

  DATA: LS_LOG_PARTNER TYPE GTYP_LOG_PARTNER,
        LS_LOG_BANK    TYPE GTYP_LOG_BANK,
        LS_LOG_COMP    TYPE GTYP_LOG_COMP,
        LS_LOG_EXTEND  TYPE GTYP_LOG_EXTEND ##NEEDED.

  SORT: GT_LOG_PARTNER BY PARTNER ,
        GT_LOG_BANK    BY PARTNER BKVID,
        GT_LOG_COMP    BY PARTNER BUKRS WITHT1 WITHT2 ,
        GT_LOG_EXTEND  BY PARTNER BUKRS .

  DELETE ADJACENT DUPLICATES FROM: GT_LOG_PARTNER COMPARING PARTNER ,
                                   GT_LOG_BANK    COMPARING PARTNER BKVID,
                                   GT_LOG_COMP    COMPARING PARTNER BUKRS WITHT1 WITHT2 ,
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
           BUKRS = PS_DATA-MAIN-VIEW_COMP-BUKRS
              BINARY SEARCH .
  IF SY-SUBRC = 0 .
    PS_DATA-MAIN-VIEW_COMP-EXIST = GC_TRUE.

    IF PS_DATA-MAIN-VIEW_COMP-WTAX01-WITHT = LS_LOG_COMP-WITHT1 .
      PS_DATA-MAIN-VIEW_COMP-WTAX01-EXIST = GC_TRUE.
    ENDIF.

    IF PS_DATA-MAIN-VIEW_COMP-WTAX02-WITHT = LS_LOG_COMP-WITHT2 .
      PS_DATA-MAIN-VIEW_COMP-WTAX02-EXIST = GC_TRUE.
    ENDIF.
  ENDIF.



* ---------------------------
* Assign Header Data
* ---------------------------
  CASE GV_MODE.
    WHEN GC_MODE_CREATE.
      IF PS_DATA-KEY-PARTNER_EXIST IS INITIAL.
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
*  IF ps_data-key-rltyp NE gc_role2_1.

*   Re-check again since previous post may create role already
  PERFORM F_CHECK_ROLE_EXIST  USING  PS_DATA-KEY-PARTNER
                                     PS_DATA-KEY-RLTYP
                            CHANGING PS_DATA-KEY-RLTYP_EXIST.

  IF PS_DATA-KEY-RLTYP NE GC_ROLE0_0.
    IF PS_DATA-KEY-RLTYP_EXIST IS INITIAL.
      LS_BAPI_ROLE-TASK = 'I'.
    ELSE.
      LS_BAPI_ROLE-TASK = 'U'.
    ENDIF.
    LS_BAPI_ROLE-DATA_KEY          = PS_DATA-KEY-RLTYP.
    LS_BAPI_ROLE-DATA-ROLECATEGORY = PS_DATA-KEY-RLTYP.

    IF PS_DATA-KEY-RLTYP IN GR_RLTYP .
      LS_BAPI_ROLE-DATA-VALID_FROM   = GV_VALIDFROM .
    ELSE.
      LS_BAPI_ROLE-DATA-VALID_FROM   = PS_DATA-MAIN-ADDR-VALID_FROM .
    ENDIF.
    LS_BAPI_ROLE-DATA-VALID_TO     = PS_DATA-MAIN-ADDR-VALID_TO .

    LS_BAPI_ROLE-DATAX-VALID_FROM  = GC_TRUE.
    LS_BAPI_ROLE-DATAX-VALID_TO    = GC_TRUE.
    INSERT LS_BAPI_ROLE INTO TABLE LS_BAPI_DATA-PARTNER-CENTRAL_DATA-ROLE-ROLES.

  ENDIF.

* ---------------------------
* Assign Common Data
* ---------------------------
* Name + Sort Key
  CASE PS_DATA-KEY-TYPE.
    WHEN GC_TYPE_PERSON.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_PERSON-FIRSTNAME  = PS_DATA-MAIN-ADDR-NAME_FIRST.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_PERSON-FIRSTNAME = PS_DATA-MAINX-ADDR-NAME_FIRST.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_PERSON-LASTNAME  = PS_DATA-MAIN-ADDR-NAME_LAST.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_PERSON-LASTNAME = PS_DATA-MAINX-ADDR-NAME_LAST.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_PERSON-SEX  = PS_DATA-MAIN-GENDR.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_PERSON-SEX = PS_DATA-MAINX-GENDR.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_PERSON-BIRTHDATE  = PS_DATA-MAIN-DOBDT.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_PERSON-BIRTHDATE = PS_DATA-MAINX-DOBDT.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_PERSON-CORRESPONDLANGUAGE = PS_DATA-MAIN-ADDR-LANGU.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_PERSON-CORRESPONDLANGUAGE = GC_TRUE.
    WHEN GC_TYPE_ORG.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-NAME1  = PS_DATA-MAIN-ADDR-NAME1.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-NAME1 = PS_DATA-MAINX-ADDR-NAME1.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-NAME2  = PS_DATA-MAIN-ADDR-NAME2.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-NAME2 = PS_DATA-MAINX-ADDR-NAME2.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-NAME3  = PS_DATA-MAIN-ADDR-NAME3.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-NAME3 = PS_DATA-MAINX-ADDR-NAME3.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-NAME4  = PS_DATA-MAIN-ADDR-NAME4.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-NAME4 = PS_DATA-MAINX-ADDR-NAME4.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-FOUNDATIONDATE   = PS_DATA-MAIN-FOUND_DAT.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-FOUNDATIONDATE  = PS_DATA-MAINX-FOUND_DAT.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-LIQUIDATIONDATE  = PS_DATA-MAIN-LIQUID_DAT.
      LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-LIQUIDATIONDATE = PS_DATA-MAINX-LIQUID_DAT.
  ENDCASE.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-TITLE_KEY  = PS_DATA-MAIN-ADDR-TITLE.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-TITLE_KEY = PS_DATA-MAINX-ADDR-TITLE.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-SEARCHTERM1  = PS_DATA-MAIN-ADDR-SORT1.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-SEARCHTERM1 = PS_DATA-MAINX-ADDR-SORT1.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-SEARCHTERM2  = PS_DATA-MAIN-ADDR-SORT2.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-SEARCHTERM2 = PS_DATA-MAINX-ADDR-SORT2.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-PARTNEREXTERNAL  = PS_DATA-MAIN-BPEXT.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-PARTNEREXTERNAL = PS_DATA-MAINX-BPEXT.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-PARTNERTYPE  = PS_DATA-MAIN-BPKIND.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-PARTNERTYPE = PS_DATA-MAINX-BPKIND.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-PARTNERLANGUAGE  = PS_DATA-MAIN-ADDR-LANGU.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-PARTNERLANGUAGE = PS_DATA-MAINX-ADDR-LANGU.

  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-LEGALFORM  = PS_DATA-MAIN-LEGAL_ENTY.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-LEGALFORM = PS_DATA-MAIN-LEGAL_ENTY.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-LEGALORG   = PS_DATA-MAIN-LEGAL_ORG.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-LEGALORG  = PS_DATA-MAIN-LEGAL_ORG.
* ---------------------------
* Assign Address Data
* ---------------------------
  PERFORM F_ASSIGN_BAPI_ADDRESS  USING  PS_DATA-KEY-TYPE
                                        PS_DATA-MAIN-ADDR
                                        PS_DATA-MAINX-ADDR
                                        PS_DATA-MAIN-ADDR_INT
                                        PS_DATA-MAINX-ADDR_INT
                               CHANGING LS_BAPI_DATA-PARTNER-CENTRAL_DATA-ADDRESS.

* ---------------------------
* Assign Tax Data
* ---------------------------
  PERFORM F_ASSIGN_BAPI_TAX  USING  PS_DATA-MAIN-TAX
                                    PS_DATA-MAINX-TAX
                           CHANGING LS_BAPI_DATA-PARTNER-CENTRAL_DATA-TAXNUMBER.

** ---------------------------
** Assign Long Text Data
** ---------------------------
*  PERFORM f_assign_bapi_longtxt  USING  ps_data-main-ltext
*                                        ps_data-mainx-ltext
*                               CHANGING ls_bapi_data-partner-central_data-longtext.

* ---------------------------
* Assign Bank Detail Data
* ---------------------------
  PERFORM F_ASSIGN_BAPI_BANKDETAIL  USING  PS_DATA-MAIN-BANK01
                                           PS_DATA-MAINX-BANK01
                                  CHANGING LS_BAPI_DATA-PARTNER-CENTRAL_DATA-BANKDETAIL-BANKDETAILS.
*  PERFORM f_assign_bapi_bankdetail  USING  ps_data-main-bank02
*                                           ps_data-mainx-bank02
*                                  CHANGING ls_bapi_data-partner-central_data-bankdetail-bankdetails.
*  PERFORM f_assign_bapi_bankdetail  USING  ps_data-main-bank03
*                                           ps_data-mainx-bank03
*                                  CHANGING ls_bapi_data-partner-central_data-bankdetail-bankdetails.
*  PERFORM f_assign_bapi_bankdetail  USING  ps_data-main-bank04
*                                           ps_data-mainx-bank04
*                                  CHANGING ls_bapi_data-partner-central_data-bankdetail-bankdetails.
*  PERFORM f_assign_bapi_bankdetail  USING  ps_data-main-bank05
*                                           ps_data-mainx-bank05
*                                  CHANGING ls_bapi_data-partner-central_data-bankdetail-bankdetails.
*  PERFORM f_assign_bapi_bankdetail  USING  ps_data-main-bank06
*                                           ps_data-mainx-bank06
*                                  CHANGING ls_bapi_data-partner-central_data-bankdetail-bankdetails.

* ---------------------------
* Assign Customer Data
* ---------------------------
*  IF PS_DATA-KEY-RLTYP EQ GC_ROLE2_1.   "CH01-
  IF PS_DATA-KEY-RLTYP EQ GC_ROLE2_2.    "CH02+

    CLEAR LV_CUST_EXIST.
*   Re-check again since previous post may create role already
    PERFORM F_CHECK_CUSTOMER_EXIST  USING  PS_DATA-KEY-PARTNER
                                  CHANGING LV_CUST_EXIST.

    LS_BAPI_DATA-PARTNER-FINSERV_DATA-COMMON-DATA-FSBP_CENTRL-VBUND  = PS_DATA-MAIN-VIEW_CUST-VBUND.
    LS_BAPI_DATA-PARTNER-FINSERV_DATA-COMMON-DATAX-FSBP_CENTRL-VBUND = PS_DATA-MAINX-VIEW_CUST-VBUND.

* BOD - CH01
**   Not assign customer code for Some Group
*    IF PS_DATA-KEY-BU_GROUP EQ GC_BU_GROUP_1 .
*
*      LS_BAPI_DATA-CUSTOMER-HEADER-OBJECT_INSTANCE = PS_DATA-KEY-PARTNER.
*    ENDIF.
* EOD - CH01

    IF LV_CUST_EXIST IS INITIAL.
      LS_BAPI_DATA-CUSTOMER-HEADER-OBJECT_TASK = 'I'.
    ELSE.
      LS_BAPI_DATA-CUSTOMER-HEADER-OBJECT_TASK = 'U'.
      LS_BAPI_DATA-CUSTOMER-HEADER-OBJECT_INSTANCE = PS_DATA-MAIN-VIEW_CUST-KUNNR.
    ENDIF.

    PERFORM F_ASSIGN_BAPI_CUSTOMER  USING  PS_DATA-KEY-PARTNER
                                           PS_DATA-MAIN-VIEW_CUST
                                           PS_DATA-MAINX-VIEW_CUST
                                           PS_DATA-MAIN-VIEW_COMP
                                           PS_DATA-MAINX-VIEW_COMP
                                  CHANGING LS_BAPI_DATA-CUSTOMER.
  ENDIF.

* Validate Data
  CALL METHOD CL_MD_BP_MAINTAIN=>VALIDATE_SINGLE
    EXPORTING
      I_DATA        = LS_BAPI_DATA
    IMPORTING
      ET_RETURN_MAP = LT_MSGMAP.
* To ensure, all update has been reverted.
* - This is to fix issue customer number skip
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
      PERFORM F_UPDATE_ADDITIONAL  USING  PV_PARTNER
                                          PS_DATA
                                 CHANGING PV_ERROR
                                          PT_MESSG.




*---------------Keep log data--------------------------
      CLEAR: LS_LOG_PARTNER , LS_LOG_COMP ,
             LS_LOG_BANK .
      PS_DATA-KEY-PARTNER = PV_PARTNER.

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
      LS_LOG_COMP-BUKRS   =  PS_DATA-MAIN-VIEW_COMP-BUKRS .
      LS_LOG_COMP-WITHT1  =  PS_DATA-MAIN-VIEW_COMP-WTAX01-WITHT.
      LS_LOG_COMP-WITHT2  =  PS_DATA-MAIN-VIEW_COMP-WTAX02-WITHT .
      APPEND LS_LOG_COMP TO GT_LOG_COMP .

      SORT: GT_LOG_PARTNER BY PARTNER ,
            GT_LOG_BANK    BY PARTNER BKVID,
            GT_LOG_COMP    BY PARTNER BUKRS WITHT1 WITHT2 .

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ELSE.
    PERFORM F_CHECK_BAPI_RETURNMAP  USING  LT_MSGMAP
                                  CHANGING PV_ERROR
                                           PT_MESSG.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_assign_bapi_address
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_ASSIGN_BAPI_ADDRESS USING PV_TYPE TYPE BUT000-TYPE
                                   PS_ADDR   TYPE  GTY_ADDR
                                   PS_ADDRX  TYPE  GTY_ADDRX
                                   PS_ADDR_INT  TYPE  GTY_ADDR_INT
                                   PS_ADDR_INTX TYPE  GTY_ADDR_INTX
                          CHANGING PS_BAPI_ADDRESS  TYPE  BUS_EI_ADDRESS.

  DATA:
    LS_ADDRESS TYPE  BUS_EI_BUPA_ADDRESS,
    LS_REMARK  TYPE  BUS_EI_BUPA_ADDRESSREMARK.


  CLEAR LS_ADDRESS.

  IF PS_ADDR-GUID IS INITIAL.
    LS_ADDRESS-TASK = 'I'.
  ELSE.
    LS_ADDRESS-TASK = 'U'.
  ENDIF.

  LS_ADDRESS-DATA_KEY-GUID = PS_ADDR-GUID.

  LS_ADDRESS-DATA-POSTAL-DATA-C_O_NAME  = PS_ADDR-NAME_CO.
  LS_ADDRESS-DATA-POSTAL-DATAX-C_O_NAME = PS_ADDRX-NAME_CO.

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

  LS_ADDRESS-DATA-POSTAL-DATA-COUNTRY  = PS_ADDR-COUNTRY.
  LS_ADDRESS-DATA-POSTAL-DATAX-COUNTRY = PS_ADDRX-COUNTRY.

  LS_ADDRESS-DATA-POSTAL-DATA-LANGU  = PS_ADDR-LANGU.
  LS_ADDRESS-DATA-POSTAL-DATAX-LANGU = PS_ADDRX-LANGU.

  LS_ADDRESS-DATA-POSTAL-DATA-EXTADDRESSNUMBER  = PS_ADDR-ADEXT.
  LS_ADDRESS-DATA-POSTAL-DATAX-EXTADDRESSNUMBER = PS_ADDRX-ADEXT.

  LS_ADDRESS-DATA-POSTAL-DATA-VALIDFROMDATE    = PS_ADDR-VALID_FROM.
  LS_ADDRESS-DATA-POSTAL-DATAX-VALIDFROMDATE   = PS_ADDRX-VALID_FROM.

  LS_ADDRESS-DATA-POSTAL-DATA-VALIDTODATE    = PS_ADDR-VALID_TO.
  LS_ADDRESS-DATA-POSTAL-DATAX-VALIDTODATE   = PS_ADDRX-VALID_TO.

  LS_ADDRESS-DATA-POSTAL-DATA-COMM_TYPE  = PS_ADDR-DEFLT_COMM.
  LS_ADDRESS-DATA-POSTAL-DATAX-COMM_TYPE = PS_ADDRX-DEFLT_COMM.

* BOI - CH01
  LS_ADDRESS-DATA-POSTAL-DATA-TRANSPZONE  = PS_ADDR-TRANSPZONE.
  LS_ADDRESS-DATA-POSTAL-DATAX-TRANSPZONE = PS_ADDRX-TRANSPZONE.

  LS_ADDRESS-DATA-POSTAL-DATA-REGION  = PS_ADDR-REGION.
  LS_ADDRESS-DATA-POSTAL-DATAX-REGION = PS_ADDRX-REGION.

  LS_ADDRESS-DATA-POSTAL-DATA-HOME_CITY  = PS_ADDR-HOME_CITY.
  LS_ADDRESS-DATA-POSTAL-DATAX-HOME_CITY = PS_ADDRX-HOME_CITY.
* EOI - CH01

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

* BOD - CH01
*  LS_REMARK-DATA-LANGU = SY-LANGU.
*  LS_REMARK-DATA-ADR_NOTES = PS_ADDR-REMARK.
*  LS_REMARK-DATAX-LANGU = GC_TRUE.
*  LS_REMARK-DATAX-ADR_NOTES = GC_TRUE.
*  INSERT LS_REMARK INTO TABLE LS_ADDRESS-DATA-REMARK-REMARKS.
* EOD - CH01

  IF PS_ADDR_INT-ACTIVE EQ GC_TRUE.
    PERFORM F_ASSIGN_BAPI_VERSION  USING  PV_TYPE
                                          PS_ADDR_INT
                                          PS_ADDR_INTX
                                 CHANGING LS_ADDRESS-DATA-VERSION-VERSIONS.
    "Remark Inter.
    IF PS_ADDR_INT-REMARK IS NOT INITIAL.
      CLEAR LS_REMARK.
      LS_REMARK-DATA-ADDR_VERS = 'I' .
      LS_REMARK-DATA-LANGU = SY-LANGU.
      LS_REMARK-DATA-ADR_NOTES = PS_ADDR_INT-REMARK.
      LS_REMARK-DATAX-LANGU = GC_TRUE.
      LS_REMARK-DATAX-ADR_NOTES = GC_TRUE.
      INSERT LS_REMARK INTO TABLE LS_ADDRESS-DATA-REMARK-REMARKS.
    ENDIF.
  ENDIF.

* Insert Address data
  INSERT LS_ADDRESS INTO TABLE PS_BAPI_ADDRESS-ADDRESSES.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_assign_bapi_bankdetail
*&---------------------------------------------------------------------*
*& Assign Bank Detail Data
*&---------------------------------------------------------------------*
FORM F_ASSIGN_BAPI_BANKDETAIL USING PS_BANK TYPE GTY_BANK
                                      PS_BANKX  TYPE  GTY_BANKX
                             CHANGING PT_BAPI_BANK  TYPE BUS_EI_BUPA_BANKDETAIL_T.

  DATA:
    LS_BANK  TYPE  BUS_EI_BUPA_BANKDETAIL.


  IF PS_BANKX IS INITIAL.
    RETURN.
  ENDIF.

  CLEAR LS_BANK.
  IF PS_BANK-BANKS IS INITIAL AND
     PS_BANK-BANKL IS INITIAL AND
     PS_BANK-BANKN IS INITIAL AND
     PS_BANK-KOINH IS INITIAL .
*     AND     ps_bank-accname IS INITIAL.
    LS_BANK-TASK           = 'D'. "Delete
  ELSE.
    LS_BANK-TASK           = 'I'.
  ENDIF.
  LS_BANK-DATA_KEY       = PS_BANK-BKVID.

  LS_BANK-DATA-BANK_CTRY        = PS_BANK-BANKS.
  LS_BANK-DATAX-BANK_CTRY       = PS_BANKX-BANKS.
  LS_BANK-DATA-BANK_KEY         = PS_BANK-BANKL.
  LS_BANK-DATAX-BANK_KEY        = PS_BANKX-BANKL.
  LS_BANK-DATA-BANK_ACCT        = PS_BANK-BANKN.
  LS_BANK-DATAX-BANK_ACCT       = PS_BANKX-BANKN.
  LS_BANK-DATA-ACCOUNTHOLDER    = PS_BANK-KOINH.
  LS_BANK-DATAX-ACCOUNTHOLDER   = PS_BANKX-KOINH.
*  ls_bank-data-bankaccountname  = ps_bank-accname.
*  ls_bank-datax-bankaccountname = ps_bankx-accname.

  INSERT LS_BANK INTO TABLE PT_BAPI_BANK.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_assign_bapi_customer
*&---------------------------------------------------------------------*
*& Assign Customer Data
*&---------------------------------------------------------------------*
FORM F_ASSIGN_BAPI_CUSTOMER USING PV_PARTNER TYPE GTY_KEY21-PARTNER ##NEEDED
                                    PS_VIEW_CUST  TYPE GTY_VIEW_CUST
                                    PS_VIEW_CUSTX TYPE GTY_VIEW_CUSTX
                                    PS_VIEW_COMP  TYPE GTY_VIEW_COMP
                                    PS_VIEW_COMPX TYPE GTY_VIEW_COMPX
                           CHANGING PS_BAPI_CUST  TYPE CMDS_EI_EXTERN.

  DATA:
    LS_COMPANY TYPE  CMDS_EI_COMPANY,
    LS_DUNNING TYPE  CMDS_EI_DUNNING ##NEEDED.

  DATA: LT_DUNNING TYPE CMDS_EI_DUNNING_T.    "CH01+

* -----------------
* Central Data
* -----------------
  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATA-KUKLA  = PS_VIEW_CUST-KUKLA.
  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATAX-KUKLA = PS_VIEW_CUSTX-KUKLA.

  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATA-BRAN1  = PS_VIEW_CUST-BRAN1.
  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATAX-BRAN1 = PS_VIEW_CUSTX-BRAN1.

  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATA-KATR1  = PS_VIEW_CUST-KATR1.
  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATAX-KATR1 = PS_VIEW_CUSTX-KATR1.

  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATA-KDKG1  = PS_VIEW_CUST-KDKG1.
  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATAX-KDKG1 = PS_VIEW_CUSTX-KDKG1.

  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATA-KDKG2  = PS_VIEW_CUST-KDKG2.
  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATAX-KDKG2 = PS_VIEW_CUSTX-KDKG2.

  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATA-VBUND  = PS_VIEW_CUST-VBUND.
  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATAX-VBUND = PS_VIEW_CUSTX-VBUND.

  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATA-LIFNR  = PS_VIEW_CUST-LIFNR.
  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATAX-LIFNR = PS_VIEW_CUSTX-LIFNR.

* -----------------
* Classification
* -----------------
*  PERFORM f_assign_bapi_cust_class  USING  pv_partner
*                                           ps_view_cust-class_val
*                                           ps_view_custx-class_val
*                                  CHANGING ps_bapi_cust-central_data-classification.

* -----------------
* Company Data
* -----------------
  IF PS_VIEW_COMP-ACTIVE EQ GC_TRUE.
    CLEAR LS_COMPANY.

    IF PS_VIEW_COMP-EXIST IS INITIAL.
      LS_COMPANY-TASK = 'I'.
    ELSE.
      LS_COMPANY-TASK = 'U'.
    ENDIF.

    LS_COMPANY-DATA_KEY-BUKRS = PS_VIEW_COMP-BUKRS.

    LS_COMPANY-DATA-AKONT  = PS_VIEW_COMP-AKONT.
    LS_COMPANY-DATAX-AKONT = PS_VIEW_COMPX-AKONT.

    LS_COMPANY-DATA-ZUAWA  = PS_VIEW_COMP-ZUAWA.
    LS_COMPANY-DATAX-ZUAWA = PS_VIEW_COMPX-ZUAWA.

    LS_COMPANY-DATA-ALTKN  = PS_VIEW_COMP-ALTKN.
    LS_COMPANY-DATAX-ALTKN = PS_VIEW_COMPX-ALTKN.

    LS_COMPANY-DATA-ZTERM  = PS_VIEW_COMP-ZTERM.
    LS_COMPANY-DATAX-ZTERM = PS_VIEW_COMPX-ZTERM.

    LS_COMPANY-DATA-XZVER  = PS_VIEW_COMP-XZVER.
    LS_COMPANY-DATAX-XZVER = PS_VIEW_COMPX-XZVER.

    LS_COMPANY-DATA-XVERR  = PS_VIEW_COMP-XVERR.
    LS_COMPANY-DATAX-XVERR = PS_VIEW_COMPX-XVERR.

    LS_COMPANY-DATA-KNRZB  = PS_VIEW_COMP-KNRZB.
    LS_COMPANY-DATAX-KNRZB = PS_VIEW_COMPX-KNRZB.

* BOI - CH01
    LS_COMPANY-DATA-FDGRV  = PS_VIEW_COMP-FDGRV.
    LS_COMPANY-DATAX-FDGRV = PS_VIEW_COMPX-FDGRV.

    LS_COMPANY-DATA-BUSAB  = PS_VIEW_COMP-BUSAB.
    LS_COMPANY-DATAX-BUSAB = PS_VIEW_COMPX-BUSAB.

    LS_COMPANY-DATA-KVERM  = PS_VIEW_COMP-KVERM.
    LS_COMPANY-DATAX-KVERM = PS_VIEW_COMPX-KVERM.

    LS_COMPANY-DATA-TLFXS  = PS_VIEW_COMP-TLFXS.
    LS_COMPANY-DATAX-TLFXS = PS_VIEW_COMPX-TLFXS.

    LS_COMPANY-DATA-INTAD  = PS_VIEW_COMP-INTAD.
    LS_COMPANY-DATAX-INTAD = PS_VIEW_COMPX-INTAD.

    LS_COMPANY-DATA-XAUSZ  = PS_VIEW_COMP-XAUSZ.
    LS_COMPANY-DATAX-XAUSZ = PS_VIEW_COMPX-XAUSZ.

    LS_COMPANY-DATA-SPERR  = PS_VIEW_COMP-SPERR.
    LS_COMPANY-DATAX-SPERR = PS_VIEW_COMPX-SPERR.

* Dunning Procedure
    LS_DUNNING-DATA_KEY-MABER = ''.
    LS_DUNNING-DATA-MAHNA     = PS_VIEW_COMP-MAHNA.
    CASE GV_MODE.
      WHEN GC_MODE_CREATE.
        LS_DUNNING-TASK           = 'I'.
      WHEN GC_MODE_CHANGE.
        LS_DUNNING-TASK           = 'U'.
    ENDCASE.
    APPEND LS_DUNNING TO LT_DUNNING.
    LS_COMPANY-DUNNING-DUNNING = LT_DUNNING[].
* EOI - CH01

    PERFORM F_ASSIGN_BAPI_COMP_WTAX  USING  PS_VIEW_COMP-WTAX01
                                            PS_VIEW_COMPX-WTAX01
                                   CHANGING LS_COMPANY-WTAX_TYPE-WTAX_TYPE.
    PERFORM F_ASSIGN_BAPI_COMP_WTAX  USING  PS_VIEW_COMP-WTAX02
                                            PS_VIEW_COMPX-WTAX02
                                   CHANGING LS_COMPANY-WTAX_TYPE-WTAX_TYPE.
    PERFORM F_ASSIGN_BAPI_COMP_WTAX  USING  PS_VIEW_COMP-WTAX03
                                            PS_VIEW_COMPX-WTAX03
                                   CHANGING LS_COMPANY-WTAX_TYPE-WTAX_TYPE.
    PERFORM F_ASSIGN_BAPI_COMP_WTAX  USING  PS_VIEW_COMP-WTAX04
                                            PS_VIEW_COMPX-WTAX04
                                   CHANGING LS_COMPANY-WTAX_TYPE-WTAX_TYPE.

    INSERT LS_COMPANY INTO TABLE PS_BAPI_CUST-COMPANY_DATA-COMPANY.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_assign_bapi_comp_wtax
*&---------------------------------------------------------------------*
*& Assign Company Withholding Tax data
*&---------------------------------------------------------------------*
*&      --> PS_VIEW_COMP_WTAX01
*&      --> PS_VIEW_COMPX_WTAX01
*&      <-- LS_COMPANY_WTAX_TYPE_WTAX_TYPE
*&---------------------------------------------------------------------*
FORM F_ASSIGN_BAPI_COMP_WTAX USING PS_WTAX TYPE GTY_WITHTAX
                                     PS_WTAXX TYPE  GTY_WITHTAXX
                            CHANGING PT_BAPI_WTAX TYPE CMDS_EI_WTAX_TYPE_T.

  DATA:
    LS_WTAX  TYPE  CMDS_EI_WTAX_TYPE.


* Only when data exist
  IF PS_WTAXX IS INITIAL.
    RETURN.
  ENDIF.

  CLEAR LS_WTAX.
  IF PS_WTAX-EXIST EQ GC_TRUE.
    IF PS_WTAX-WITHCD IS INITIAL AND
       PS_WTAX-AGTDF  IS INITIAL AND
       PS_WTAX-AGTDT  IS INITIAL .
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
  LS_WTAX-DATA-WT_AGENT   = PS_WTAX-AGENT.
  LS_WTAX-DATAX-WT_AGENT  = PS_WTAXX-AGENT.
  LS_WTAX-DATA-WT_WTSTCD  = PS_WTAX-WTSTCD.
  LS_WTAX-DATAX-WT_WTSTCD = PS_WTAXX-WTSTCD.
  LS_WTAX-DATA-WT_AGTDF  = PS_WTAX-AGTDF.
  LS_WTAX-DATAX-WT_AGTDF = PS_WTAXX-AGTDF.
  LS_WTAX-DATA-WT_AGTDT  = PS_WTAX-AGTDT.
  LS_WTAX-DATAX-WT_AGTDT = PS_WTAXX-AGTDT.

* BOI - CH01
  LS_WTAX-DATA-QSREC  = PS_WTAX-QSREC.
  LS_WTAX-DATAX-QSREC = PS_WTAXX-QSREC.
* EOI - CH01

  INSERT LS_WTAX INTO TABLE PT_BAPI_WTAX.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_assign_bapi_fax
*&---------------------------------------------------------------------*
*& Assign BAPI Fax data
*&---------------------------------------------------------------------*
FORM F_ASSIGN_BAPI_FAX USING PS_FAX TYPE GTY_FAX
                               PS_FAXX     TYPE  GTY_FAXX
                      CHANGING PS_BAPI_FAX TYPE BUS_EI_FAX_DATA.

  DATA:
    LS_FAX  TYPE  BUS_EI_BUPA_FAX.


  IF PS_FAXX-FAXNO EQ GC_TRUE OR
     PS_FAXX-FAXEXT EQ GC_TRUE.
    CLEAR LS_FAX.
    LS_FAX-CONTACT-TASK      = 'I'.
    LS_FAX-CONTACT-DATA-FAX  = PS_FAX-FAXNO.
    LS_FAX-CONTACT-DATAX-FAX = PS_FAXX-FAXNO.
    LS_FAX-CONTACT-DATA-EXTENSION  = PS_FAX-FAXEXT.
    LS_FAX-CONTACT-DATAX-EXTENSION = PS_FAXX-FAXEXT.
    INSERT LS_FAX INTO TABLE PS_BAPI_FAX-FAX.
    PS_BAPI_FAX-CURRENT_STATE = GC_TRUE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_assign_bapi_phone
*&---------------------------------------------------------------------*
*& Assign BAPI Phone data
*&---------------------------------------------------------------------*
FORM F_ASSIGN_BAPI_PHONE USING PS_PHONE TYPE GTY_PHONE
                                 PS_PHONEX TYPE  GTY_PHONEX
                        CHANGING PS_BAPI_PHONE  TYPE  BUS_EI_TEL_DATA.

  DATA:
    LS_PHONE  TYPE  BUS_EI_BUPA_TELEPHONE.


  IF PS_PHONEX-TELNO1 EQ GC_TRUE OR
     PS_PHONEX-TELEXT1 EQ GC_TRUE.
    CLEAR LS_PHONE.
    LS_PHONE-CONTACT-TASK            = 'I'.
    LS_PHONE-CONTACT-DATA-TELEPHONE  = PS_PHONE-TELNO1.
    LS_PHONE-CONTACT-DATAX-TELEPHONE = PS_PHONEX-TELNO1 .
    LS_PHONE-CONTACT-DATA-EXTENSION  = PS_PHONE-TELEXT1.
    LS_PHONE-CONTACT-DATAX-EXTENSION = PS_PHONEX-TELEXT1.
    INSERT LS_PHONE INTO TABLE PS_BAPI_PHONE-PHONE.
    PS_BAPI_PHONE-CURRENT_STATE = GC_TRUE.
  ENDIF.

  IF PS_PHONEX-TELNO2 EQ GC_TRUE OR
     PS_PHONEX-TELEXT2 EQ GC_TRUE.
    CLEAR LS_PHONE.
    LS_PHONE-CONTACT-TASK            = 'I'.
    LS_PHONE-CONTACT-DATA-TELEPHONE  = PS_PHONE-TELNO2.
    LS_PHONE-CONTACT-DATAX-TELEPHONE = PS_PHONEX-TELNO2.
    LS_PHONE-CONTACT-DATA-EXTENSION  = PS_PHONE-TELEXT2.
    LS_PHONE-CONTACT-DATAX-EXTENSION = PS_PHONEX-TELEXT2.
    INSERT LS_PHONE INTO TABLE PS_BAPI_PHONE-PHONE.
    PS_BAPI_PHONE-CURRENT_STATE = GC_TRUE.
  ENDIF.

  IF PS_PHONEX-MOBILE EQ GC_TRUE.
    CLEAR LS_PHONE.
    LS_PHONE-CONTACT-TASK            = 'I'.
    LS_PHONE-CONTACT-DATA-TELEPHONE  = PS_PHONE-MOBILE.
    LS_PHONE-CONTACT-DATAX-TELEPHONE = GC_TRUE.
    LS_PHONE-CONTACT-DATA-R_3_USER   = '2'.
    LS_PHONE-CONTACT-DATAX-R_3_USER  = GC_TRUE.
    INSERT LS_PHONE INTO TABLE PS_BAPI_PHONE-PHONE.
    PS_BAPI_PHONE-CURRENT_STATE = GC_TRUE.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_assign_bapi_smtp
*&---------------------------------------------------------------------*
*& Assign BAPI SMTP data
*&---------------------------------------------------------------------*

FORM F_ASSIGN_BAPI_SMTP USING PS_SMTP TYPE GTY_SMTP
                                 PS_SMTPX TYPE GTY_SMTPX
                        CHANGING PS_BAPI_SMTP TYPE BUS_EI_SMTP_DATA.

  DATA:
    LS_SMTP TYPE  BUS_EI_BUPA_SMTP,
    LT_NOTE TYPE  BUS_EI_BUPA_COMREM_T ##NEEDED,
    LS_NOTE TYPE  BUS_EI_BUPA_COMREM ##NEEDED.


  IF PS_SMTPX-EMAIL1 EQ GC_TRUE.
    CLEAR LS_SMTP.
    LS_SMTP-CONTACT-TASK         = 'I'.
    LS_SMTP-CONTACT-DATA-E_MAIL  = PS_SMTP-EMAIL1.
    LS_SMTP-CONTACT-DATAX-E_MAIL = GC_TRUE.

    INSERT LS_SMTP INTO TABLE PS_BAPI_SMTP-SMTP.
  ENDIF.

* BOI - CH01 - 19.08.2024
  IF PS_SMTPX-EMAIL2 EQ GC_TRUE.
    CLEAR LS_SMTP.
    LS_SMTP-CONTACT-TASK         = 'I'.
    LS_SMTP-CONTACT-DATA-E_MAIL  = PS_SMTP-EMAIL2.
    LS_SMTP-CONTACT-DATAX-E_MAIL = GC_TRUE.

    INSERT LS_SMTP INTO TABLE PS_BAPI_SMTP-SMTP.
  ENDIF.

  IF PS_SMTPX-EMAIL3 EQ GC_TRUE.
    CLEAR LS_SMTP.
    LS_SMTP-CONTACT-TASK         = 'I'.
    LS_SMTP-CONTACT-DATA-E_MAIL  = PS_SMTP-EMAIL3.
    LS_SMTP-CONTACT-DATAX-E_MAIL = GC_TRUE.

    INSERT LS_SMTP INTO TABLE PS_BAPI_SMTP-SMTP.
  ENDIF.

  IF PS_SMTPX-EMAIL4 EQ GC_TRUE.
    CLEAR LS_SMTP.
    LS_SMTP-CONTACT-DATA-E_MAIL  = PS_SMTP-EMAIL4.
    LS_SMTP-CONTACT-DATAX-E_MAIL = GC_TRUE.

    INSERT LS_SMTP INTO TABLE PS_BAPI_SMTP-SMTP.
  ENDIF.

  IF PS_SMTPX-EMAIL5 EQ GC_TRUE.
    CLEAR LS_SMTP.
    LS_SMTP-CONTACT-DATA-E_MAIL  = PS_SMTP-EMAIL5.
    LS_SMTP-CONTACT-DATAX-E_MAIL = GC_TRUE.

    INSERT LS_SMTP INTO TABLE PS_BAPI_SMTP-SMTP.
  ENDIF.

  IF PS_SMTPX-EMAIL6 EQ GC_TRUE.
    CLEAR LS_SMTP.
    LS_SMTP-CONTACT-DATA-E_MAIL  = PS_SMTP-EMAIL6.
    LS_SMTP-CONTACT-DATAX-E_MAIL = GC_TRUE.

    INSERT LS_SMTP INTO TABLE PS_BAPI_SMTP-SMTP.
  ENDIF.

  IF PS_SMTPX-EMAIL7 EQ GC_TRUE.
    CLEAR LS_SMTP.
    LS_SMTP-CONTACT-DATA-E_MAIL  = PS_SMTP-EMAIL7.
    LS_SMTP-CONTACT-DATAX-E_MAIL = GC_TRUE.

    INSERT LS_SMTP INTO TABLE PS_BAPI_SMTP-SMTP.
  ENDIF.

* EOI - CH01 - 19.08.2024
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_assign_bapi_tax
*&---------------------------------------------------------------------*
*& Assign BAPI Tax Number Data
*&---------------------------------------------------------------------*

FORM F_ASSIGN_BAPI_TAX USING PS_TAX TYPE GTY_TAX
                               PS_TAXX TYPE GTY_TAXX
                      CHANGING PS_BAPI_TAX  TYPE  BUS_EI_TAXNUMBER.

  DATA:
    LS_TAX  TYPE  BUS_EI_BUPA_TAXNUMBER.


  PS_BAPI_TAX-COMMON-DATA-NAT_PERSON  = PS_TAX-NATPERS.
  PS_BAPI_TAX-COMMON-DATAX-NAT_PERSON = PS_TAXX-NATPERS.

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
*& Form f_assign_bapi_version
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_ASSIGN_BAPI_VERSION USING PV_TYPE TYPE GTY_DATA21-KEY-TYPE
                                   PS_ADDR  TYPE  GTY_ADDR_INT
                                   PS_ADDRX TYPE  GTY_ADDR_INTX
                          CHANGING PT_VERSION  TYPE  BUS_EI_BUPA_VERSION_T.

  DATA:
    LS_VERSION TYPE  BUS_EI_BUPA_VERSION.


  CLEAR LS_VERSION.
  LS_VERSION-TASK = 'M'.                                    "+450000630

  CASE PV_TYPE.
    WHEN GC_TYPE_PERSON.
      LS_VERSION-DATA-PERSON-ADDR_VERS   = PS_ADDR-NATION.
      LS_VERSION-DATAX-PERSON-ADDR_VERS  = GC_TRUE.
      LS_VERSION-DATA-PERSON-TITLE_P     = PS_ADDR-TITLE.
      LS_VERSION-DATAX-PERSON-TITLE_P    = PS_ADDRX-TITLE.
      LS_VERSION-DATA-PERSON-FIRSTNAME   = PS_ADDR-NAME_FIRST.
      LS_VERSION-DATAX-PERSON-FIRSTNAME  = PS_ADDRX-NAME_FIRST.
      LS_VERSION-DATA-PERSON-LASTNAME    = PS_ADDR-NAME_LAST.
      LS_VERSION-DATAX-PERSON-LASTNAME   = PS_ADDRX-NAME_LAST.
      LS_VERSION-DATA-PERSON-SORT1_P     = PS_ADDR-SORT1.
      LS_VERSION-DATAX-PERSON-SORT1_P    = PS_ADDRX-SORT1.
      LS_VERSION-DATA-PERSON-SORT2_P     = PS_ADDR-SORT2.
      LS_VERSION-DATAX-PERSON-SORT2_P    = PS_ADDRX-SORT2.
      LS_VERSION-DATA-PERSON-C_O_NAME    = PS_ADDR-NAME_CO.
      LS_VERSION-DATAX-PERSON-C_O_NAME   = PS_ADDRX-NAME_CO.

      LS_VERSION-DATA-PERSON-STR_SUPPL1  = PS_ADDR-STR_SUPPL1.
      LS_VERSION-DATAX-PERSON-STR_SUPPL1 = PS_ADDRX-STR_SUPPL1.
      LS_VERSION-DATA-PERSON-STR_SUPPL2  = PS_ADDR-STR_SUPPL2.
      LS_VERSION-DATAX-PERSON-STR_SUPPL2 = PS_ADDRX-STR_SUPPL2.
      LS_VERSION-DATA-PERSON-STREET      = PS_ADDR-STREET.
      LS_VERSION-DATAX-PERSON-STREET     = PS_ADDRX-STREET.
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
      LS_VERSION-DATA-PERSON-COUNTY     = PS_ADDR-COUNTRY.
      LS_VERSION-DATAX-PERSON-COUNTY     = PS_ADDRX-COUNTRY.

    WHEN GC_TYPE_ORG.
*      ls_version-data-organization-addr_vers   = ps_addr-nation. "JJ
      LS_VERSION-DATA-ORGANIZATION-ADDR_VERS   = 'I'.
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
      LS_VERSION-DATA-ORGANIZATION-C_O_NAME    = PS_ADDR-NAME_CO.
      LS_VERSION-DATAX-ORGANIZATION-C_O_NAME   = PS_ADDRX-NAME_CO.
      LS_VERSION-DATA-ORGANIZATION-STREET      = PS_ADDR-STREET.
      LS_VERSION-DATAX-ORGANIZATION-STREET     = PS_ADDRX-STREET.
      LS_VERSION-DATA-ORGANIZATION-STR_SUPPL1  = PS_ADDR-STR_SUPPL1.
      LS_VERSION-DATAX-ORGANIZATION-STR_SUPPL1 = PS_ADDRX-STR_SUPPL1.
      LS_VERSION-DATA-ORGANIZATION-STR_SUPPL2  = PS_ADDR-STR_SUPPL2.
      LS_VERSION-DATAX-ORGANIZATION-STR_SUPPL2 = PS_ADDRX-STR_SUPPL2.
      LS_VERSION-DATA-ORGANIZATION-STR_SUPPL3  = PS_ADDR-STR_SUPPL3.
      LS_VERSION-DATAX-ORGANIZATION-STR_SUPPL3 = PS_ADDRX-STR_SUPPL3.
      LS_VERSION-DATA-ORGANIZATION-LOCATION    = PS_ADDR-LOCATION.
      LS_VERSION-DATAX-ORGANIZATION-LOCATION   = PS_ADDRX-LOCATION.
      LS_VERSION-DATA-ORGANIZATION-DISTRICT    = PS_ADDR-CITY2.
      LS_VERSION-DATAX-ORGANIZATION-DISTRICT   = PS_ADDRX-CITY2.
      LS_VERSION-DATA-ORGANIZATION-CITY        = PS_ADDR-CITY1.
      LS_VERSION-DATAX-ORGANIZATION-CITY       = PS_ADDRX-CITY1.
*      ls_version-data-organization-po_box_cit  = ps_addr-post_code1.
*      ls_version-datax-organization-po_box_cit = ps_addrx-post_code1.
      LS_VERSION-DATA-ORGANIZATION-COUNTY      = PS_ADDR-COUNTRY.
      LS_VERSION-DATAX-ORGANIZATION-COUNTY     = PS_ADDRX-COUNTRY.

  ENDCASE.

  INSERT LS_VERSION INTO TABLE PT_VERSION.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_bapi_return
*&---------------------------------------------------------------------*
*& Check BAPI Return result
*&---------------------------------------------------------------------*
FORM F_CHECK_BAPI_RETURN USING PT_RETURN TYPE BAPIRETM
                                 PV_TEST   TYPE  FLAG
                        CHANGING PV_ERROR  TYPE  FLAG
                                 PT_MESSG  TYPE  GTTY_MESSG.

  DATA:
    LS_MESSG     TYPE  GTY_MESSG.

  FIELD-SYMBOLS:
    <LFS_RETURN> TYPE BAPIRETI,
    <LFS_MSG>    TYPE BAPIRETC.


* Initialize Output
  CLEAR PV_ERROR.
  CLEAR: PT_MESSG.


  LOOP AT PT_RETURN ASSIGNING <LFS_RETURN>.

    LOOP AT <LFS_RETURN>-OBJECT_MSG ASSIGNING <LFS_MSG>
                                    WHERE TYPE EQ 'A' OR
                                          TYPE EQ 'E'.
      PV_ERROR = GC_TRUE.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'E'.
      LS_MESSG-MSGID = <LFS_MSG>-ID.
      LS_MESSG-MSGNO = <LFS_MSG>-NUMBER.
      MESSAGE ID <LFS_MSG>-ID
              TYPE <LFS_MSG>-TYPE
              NUMBER <LFS_MSG>-NUMBER
              WITH <LFS_MSG>-MESSAGE_V1 <LFS_MSG>-MESSAGE_V2
                   <LFS_MSG>-MESSAGE_V3 <LFS_MSG>-MESSAGE_V4
              INTO LS_MESSG-MSGTX.
      INSERT LS_MESSG INTO TABLE PT_MESSG.
    ENDLOOP.
    IF SY-SUBRC EQ 0.
      EXIT.
    ENDIF.

  ENDLOOP.

  IF PT_MESSG IS INITIAL.
    IF PV_TEST EQ GC_TRUE.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'S'.
      LS_MESSG-MSGID = 'ZTEC'.
      LS_MESSG-MSGNO = '000'.
*     Text-i01: Test upload successfully.
      LS_MESSG-MSGTX = TEXT-I01.
      INSERT LS_MESSG INTO TABLE PT_MESSG.
    ELSE.
      CASE GV_MODE.
        WHEN GC_MODE_CREATE.
          CLEAR LS_MESSG.
          LS_MESSG-MSGTY = 'S'.
          LS_MESSG-MSGID = 'ZTEC'.
          LS_MESSG-MSGNO = '000'.
*         Text-i02: Business Partner data has been created successfully.
          LS_MESSG-MSGTX = TEXT-I02.
          INSERT LS_MESSG INTO TABLE PT_MESSG.
        WHEN GC_MODE_CHANGE.
          CLEAR LS_MESSG.
          LS_MESSG-MSGTY = 'S'.
          LS_MESSG-MSGID = 'ZTEC'.
          LS_MESSG-MSGNO = '000'.
*         Text-i03: Business Partner data has been updated successfully.
          LS_MESSG-MSGTX = TEXT-I03.
          INSERT LS_MESSG INTO TABLE PT_MESSG.
      ENDCASE.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_bapi_returnmap
*&---------------------------------------------------------------------*
*& Check BAPI Return result
*&---------------------------------------------------------------------*
FORM F_CHECK_BAPI_RETURNMAP USING PT_RETURN TYPE MDG_BS_BP_MSGMAP_T
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
*& Form f_check_customer_exist
*&---------------------------------------------------------------------*
*& Check Customer Exist
*&---------------------------------------------------------------------*
FORM F_CHECK_CUSTOMER_EXIST USING PV_PARTNER TYPE BUT000-PARTNER
                          CHANGING PV_EXIST    TYPE FLAG.

  DATA:
    LV_KUNNR  TYPE  KNA1-KUNNR ##NEEDED.


* Initlialize Output
  CLEAR: PV_EXIST.

* Get Customer from Partner
  PERFORM F_GET_CUSTOMER_FROM_PARTNER  USING  PV_PARTNER
                                     CHANGING LV_KUNNR.
  IF LV_KUNNR IS NOT INITIAL.
    PV_EXIST = GC_TRUE.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_additional
*&---------------------------------------------------------------------*
*& Update Additional Data which method does not support
*&---------------------------------------------------------------------*
FORM F_UPDATE_ADDITIONAL USING PV_PARTNER TYPE BUT000-PARTNER
                                 PS_DATA     TYPE  GTY_DATA21
                        CHANGING PV_ERROR    TYPE  FLAG
                                 PT_MESSG    TYPE  GTTY_MESSG.

  DATA:
    LT_MESSG  TYPE  GTTY_MESSG.

  DATA:
    LV_ERROR  TYPE  FLAG.


* -----------------------------------
* Update Partner Long Text
* -----------------------------------
  PERFORM F_UPDATE_PARTNER_LTEXT  USING  PV_PARTNER
                                         PS_DATA-MAIN-LTEXT
                                         PS_DATA-MAINX-LTEXT
                               CHANGING LV_ERROR
                                        LT_MESSG.
  IF LV_ERROR IS NOT INITIAL.
    PV_ERROR = LV_ERROR.
    PT_MESSG = LT_MESSG.
  ENDIF.

* Only for Customer View
*  IF PS_DATA-KEY-RLTYP EQ GC_ROLE2_1.    "CH01-
  IF PS_DATA-KEY-RLTYP EQ GC_ROLE2_2.     "CH01+
**   -----------------------------------
**   Update Customer Classification
**   -----------------------------------
*    PERFORM F_UPDATE_CUST_CLASS    USING  PV_PARTNER
*                                          PS_DATA-MAIN-VIEW_CUST-CLASS_VAL
*                                          PS_DATA-MAINX-VIEW_CUST-CLASS_VAL
*                                          PS_DATA-MAIN-VIEW_COMP-BUKRS
*                                 CHANGING LV_ERROR
*                                          LT_MESSG.
*    IF LV_ERROR IS NOT INITIAL.
*      PV_ERROR = LV_ERROR.
*      PT_MESSG = LT_MESSG.
*    ENDIF.

*   -----------------------------------
*   Update Branch Code
*   -----------------------------------
    PERFORM F_UPDATE_CUST_BRANCH  USING  PV_PARTNER
                                         PS_DATA-MAIN-BRANCH
                                         PS_DATA-MAINX-BRANCH
                                CHANGING LV_ERROR
                                         LT_MESSG.
    IF LV_ERROR IS NOT INITIAL.
      PV_ERROR = LV_ERROR.
      PT_MESSG = LT_MESSG.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_cust_branch
*&---------------------------------------------------------------------*
*& Update Customer Branch Data
*&---------------------------------------------------------------------*
FORM F_UPDATE_CUST_BRANCH USING PV_PARTNER TYPE BUT000-PARTNER
                                  PS_BRANCH   TYPE  GTY_BRANCH
                                  PS_BRANCHX  TYPE  GTY_BRANCHX
                         CHANGING PV_ERROR    TYPE  FLAG
                                  PT_MESSG    TYPE  GTTY_MESSG.

  DATA:
    LT_CODE     TYPE  CL_FINLOC_CVI_PERSIST_DATA=>TT_FITHA_PBUPL_D,
    LT_CODE_DEL TYPE  CL_FINLOC_CVI_PERSIST_DATA=>TT_FITHA_PBUPL_D,
    LT_DESC     TYPE  CL_FINLOC_CVI_PERSIST_DATA=>TT_FITHA_PBUPL_D_T,
    LT_DESC_DEL TYPE  CL_FINLOC_CVI_PERSIST_DATA=>TT_FITHA_PBUPL_D_T.

  DATA:
    LS_CODE TYPE  FITHA_PBUPL_D,
    LS_DESC TYPE  FITHA_PBUPL_D_T.

  DATA:
    LV_KUNNR  TYPE  KNA1-KUNNR.


* Initialize Output
  CLEAR: PV_ERROR.
  CLEAR: PT_MESSG.

* Only when Branch is fill
  IF PS_BRANCHX IS INITIAL.
    RETURN.
  ENDIF.

* Get Customer from Partner
  PERFORM F_GET_CUSTOMER_FROM_PARTNER  USING  PV_PARTNER
                                     CHANGING LV_KUNNR.
  IF LV_KUNNR IS INITIAL.
    RETURN.
  ENDIF.

* Assign Code
  CLEAR LS_CODE.
  LS_CODE-KUNNR     = LV_KUNNR.
  LS_CODE-J_1TPBUPL = PS_BRANCH-J_1TPBUPL.
  LS_CODE-DEFAULT_BRANCH = PS_BRANCH-DEFAULT_BRANCH.  "CH01+
  INSERT LS_CODE INTO TABLE LT_CODE.

  CALL FUNCTION 'LCTH_BUPA_UPDATE_BRANCH_CODE'
    EXPORTING
      IT_FITHA_PBUPL_D        = LT_CODE
      IT_FITHA_PBUPL_D_DELETE = LT_CODE_DEL.

* Assign Desc
  CLEAR LS_DESC.
  LS_DESC-SPRAS       = SY-LANGU.
  LS_DESC-KUNNR       = LV_KUNNR.
  LS_DESC-J_1TPBUPL   = PS_BRANCH-J_1TPBUPL.
  LS_DESC-DESCRIPTION = PS_BRANCH-DESCRIPTION.
  INSERT LS_DESC INTO TABLE LT_DESC.

  CALL FUNCTION 'LCTH_BUPA_UPDATE_BRANCH_DESC'
    EXPORTING
      IT_FITHA_PBUPL_D_T        = LT_DESC
      IT_FITHA_PBUPL_D_T_DELETE = LT_DESC_DEL.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT = GC_TRUE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_cust_class
*&---------------------------------------------------------------------*
*& Update Customer Classification
*&---------------------------------------------------------------------*
FORM F_UPDATE_CUST_CLASS USING PV_PARTNER TYPE BUT000-PARTNER  ##CALLED
                                 PV_CLASS_VAL  TYPE  GTY_VIEW_CUST-CLASS_VAL
                                 PV_CLASS_VALX TYPE  GTY_VIEW_CUSTX-CLASS_VAL
                                 PV_BUKRS      TYPE  BUKRS    "iPS14.09.2021 [Rollout Captain]
                        CHANGING PV_ERROR  TYPE FLAG
                                 PT_MESSG  TYPE GTTY_MESSG.

  CONSTANTS:
    LC_KNA1   TYPE BAPI1003_KEY-OBJECTTABLE VALUE 'KNA1',
    LC_CLASS  TYPE BAPI1003_KEY-CLASSNUM VALUE 'ZFISDCUST',
    LC_011    TYPE BAPI1003_KEY-CLASSTYPE VALUE '011',
    LC_CHAR01 TYPE BAPI1003_ALLOC_VALUES_CHAR-CHARACT VALUE 'ZFISDCUST'.

  DATA:
    LT_NUM    TYPE  STANDARD TABLE OF BAPI1003_ALLOC_VALUES_NUM,
    LT_CHAR   TYPE  STANDARD TABLE OF BAPI1003_ALLOC_VALUES_CHAR,
    LT_CURR   TYPE  STANDARD TABLE OF BAPI1003_ALLOC_VALUES_CURR,
    LT_RETURN TYPE STANDARD TABLE OF BAPIRET2.

  DATA:
    LS_CHAR  TYPE  BAPI1003_ALLOC_VALUES_CHAR,
    LS_MESSG TYPE  GTY_MESSG.

  DATA:
    LV_KEY  TYPE  BAPI1003_KEY-OBJECT.

  FIELD-SYMBOLS:
    <LFS_RETURN>  TYPE  BAPIRET2.


  "beg+++ iPS14.09.2021 [Rollout Captain]
  DATA: "LS_CABN          TYPE CABN,
    LV_CLASS         TYPE BAPI1003_KEY-CLASSNUM ##NEEDED,
    LV_CHAR01        TYPE BAPI1003_ALLOC_VALUES_CHAR-CHARACT ##NEEDED,
    LV_ATNAM_COMPANY TYPE CABN-ATNAM.

  "Try finding Characteristic name ZFISDCUST_<company>
  CONCATENATE LC_CHAR01 '_' PV_BUKRS  INTO LV_ATNAM_COMPANY.
  SELECT  ATNAM   ##NEEDED
    INTO @DATA(LS_CABN)  ##NEEDED
    FROM CABN  UP TO 1 ROWS
    WHERE  ATNAM  = @LV_ATNAM_COMPANY.
  ENDSELECT.
  IF SY-SUBRC EQ 0.
    LV_CLASS  = LV_ATNAM_COMPANY.
    LV_CHAR01 = LV_ATNAM_COMPANY.
  ELSE.
    "// If not found use ZFISDCUST for TOA
    LV_CLASS  = LC_CLASS.
    LV_CHAR01 = LC_CHAR01.
  ENDIF.
  "end+++ iPS14.09.2021 [Rollout Captain]

* Initialize Output
  CLEAR: PV_ERROR.
  CLEAR: PT_MESSG.

* Only when value filled
  IF PV_CLASS_VALX IS INITIAL.
    RETURN.
  ENDIF.

  LV_KEY = PV_PARTNER.

* Assign Value
  CLEAR: LS_CHAR.
*  ls_char-charact    = lc_char01.    "dPS14.09.2021 [Rollout Captain]
  LS_CHAR-CHARACT    = LV_CHAR01.     "iPS14.09.2021 [Rollout Captain]
  LS_CHAR-VALUE_CHAR = PV_CLASS_VAL.         "#EC CI_FLDEXT_OK[2215424]
  INSERT LS_CHAR INTO TABLE LT_CHAR.

  CALL FUNCTION 'BAPI_OBJCL_CHANGE'  "#EC CI_USAGE_OK[2438131]
    EXPORTING
      OBJECTKEY          = LV_KEY
      OBJECTTABLE        = LC_KNA1
      CLASSNUM           = LC_CLASS
      CLASSTYPE          = LC_011
    TABLES
      ALLOCVALUESNUMNEW  = LT_NUM
      ALLOCVALUESCHARNEW = LT_CHAR
      ALLOCVALUESCURRNEW = LT_CURR
      RETURN             = LT_RETURN.

  LOOP AT LT_RETURN ASSIGNING <LFS_RETURN>
                    WHERE TYPE = 'E' OR
                          TYPE = 'A'.
    PV_ERROR = GC_TRUE.
    CLEAR: LS_MESSG.
    LS_MESSG-MSGTY  = 'E'.
    LS_MESSG-MSGID  = <LFS_RETURN>-ID.
    LS_MESSG-MSGNO  = <LFS_RETURN>-NUMBER.
    LS_MESSG-MSGTX  = <LFS_RETURN>-MESSAGE.
    INSERT LS_MESSG INTO TABLE PT_MESSG.

  ENDLOOP.

  IF PV_ERROR IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = GC_TRUE.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_partner_ltext
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PV_PARTNER
*&      --> PS_DATA_MAIN_LTEXT
*&      --> PS_DATA_MAINX_LTEXT
*&      <-- LV_ERROR
*&      <-- LT_MESSG
*&---------------------------------------------------------------------*
FORM F_UPDATE_PARTNER_LTEXT USING PV_PARTNER TYPE BUT000-PARTNER
                                    PS_LTEXT    TYPE  GTY_LTEXT
                                    PS_LTEXTX   TYPE  GTY_LTEXTX
                           CHANGING PV_ERROR    TYPE  FLAG
                                    PT_MESSG    TYPE  GTTY_MESSG.

  CONSTANTS:
    LC_BUT000 TYPE  THEAD-TDOBJECT VALUE 'BUT000',
    LC_C003   TYPE  THEAD-TDID     VALUE 'C003',
    LC_FLOG   TYPE  THEAD-TDID     VALUE 'FLOG'.

  DATA:
    LT_LINE  TYPE  STANDARD TABLE OF TLINE,
    LT_MESSG TYPE  GTTY_MESSG.

  DATA:
    LS_LINE   TYPE  TLINE.

  DATA:
    LV_NAME   TYPE  THEAD-TDNAME.


  LV_NAME = PV_PARTNER.

  IF PS_LTEXTX-TXT_C003 IS NOT INITIAL.
    CLEAR: LT_LINE.
    CLEAR LS_LINE.
    LS_LINE-TDLINE = PS_LTEXT-TXT_C003.
    INSERT LS_LINE INTO TABLE LT_LINE.
    PERFORM F_SAVE_TEXT  USING  LC_BUT000
                                LV_NAME
                                LC_C003
                                SY-LANGU
                                LT_LINE
                       CHANGING LT_MESSG.
    IF LT_MESSG IS NOT INITIAL.
      PV_ERROR = GC_TRUE.
      APPEND LINES OF LT_MESSG TO PT_MESSG.
    ENDIF.
  ENDIF.

  IF PS_LTEXTX-TXT_FLOG IS NOT INITIAL.
    CLEAR: LT_LINE.
    CLEAR LS_LINE.
    LS_LINE-TDLINE = PS_LTEXT-TXT_FLOG.
    INSERT LS_LINE INTO TABLE LT_LINE.
    PERFORM F_SAVE_TEXT  USING  LC_BUT000
                                LV_NAME
                                LC_FLOG
                                SY-LANGU
                                LT_LINE
                       CHANGING LT_MESSG.
    IF LT_MESSG IS NOT INITIAL.
      PV_ERROR = GC_TRUE.
      APPEND LINES OF LT_MESSG TO PT_MESSG.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_save_text
*&---------------------------------------------------------------------*
*& Save Text
*&---------------------------------------------------------------------*
FORM F_SAVE_TEXT USING PV_OBJECT TYPE THEAD-TDOBJECT
                         PV_NAME    TYPE  THEAD-TDNAME
                         PV_ID      TYPE  THEAD-TDID
                         PV_SPRAS   TYPE  THEAD-TDSPRAS
                         PT_LINE    TYPE  TLINE_TAB
                CHANGING PT_MESSG   TYPE  GTTY_MESSG.

  DATA:
    LS_HEAD  TYPE  THEAD,
    LS_MESSG TYPE  GTY_MESSG.


* Initialize Output
  CLEAR: PT_MESSG.

* Assign Header
  CLEAR: LS_HEAD.
  LS_HEAD-TDOBJECT = PV_OBJECT.
  LS_HEAD-TDNAME   = PV_NAME.
  LS_HEAD-TDID     = PV_ID.
  LS_HEAD-TDSPRAS  = PV_SPRAS.

  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      HEADER          = LS_HEAD
      SAVEMODE_DIRECT = GC_TRUE
    TABLES
      LINES           = PT_LINE
    EXCEPTIONS
      ID              = 1
      LANGUAGE        = 2
      NAME            = 3
      OBJECT          = 4
      OTHERS          = 5.
  IF SY-SUBRC <> 0.
    CLEAR LS_MESSG.
    LS_MESSG-MSGTY = 'E'.
    LS_MESSG-MSGID = 'ZTEC'.
    LS_MESSG-MSGNO = '000'.
*   Text-e18: Error during update Partner long text.
    LS_MESSG-MSGTX = TEXT-E18.
    INSERT LS_MESSG INTO TABLE PT_MESSG.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_bpkind
*&---------------------------------------------------------------------*
*& Validate Business Partner Type
*&---------------------------------------------------------------------*
FORM F_VALIDATE_BPKIND USING PV_STRING TYPE STRING
                      CHANGING PV_BPKIND  TYPE  TB004-BPKIND
                               PV_MSGTX   TYPE  CLIKE.

  TYPES: BEGIN OF LTY_TB004,
           BPKIND TYPE  TB004-BPKIND,
         END OF LTY_TB004.
  TYPES: LTTY_TB004  TYPE  SORTED TABLE OF LTY_TB004
                           WITH UNIQUE KEY BPKIND.

  STATICS:
    LS_TB004 TYPE  LTY_TB004,
    LT_TB004 TYPE  LTTY_TB004.

  DATA:
    LV_LEN    TYPE  I,
    LV_BPKIND TYPE  LTY_TB004-BPKIND.


* Initialize Output
  CLEAR: PV_BPKIND,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 4
  LV_LEN = STRLEN( PV_STRING ).
  IF LV_LEN GT 4.
*   Text-e59 : Invalid BP Type:
    CONCATENATE TEXT-E59 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LV_BPKIND = PV_STRING.

* Check Buffer
  IF LS_TB004-BPKIND NE LV_BPKIND.
*   Validate with Memory
    READ TABLE LT_TB004 INTO LS_TB004
                        WITH KEY BPKIND = LV_BPKIND
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TB004.
*     Validate with Database
      SELECT SINGLE BPKIND
        INTO LS_TB004
        FROM TB004
       WHERE BPKIND EQ LV_BPKIND.
      IF SY-SUBRC NE 0.
*       Text-e59 : Invalid BP Type:
        CONCATENATE TEXT-E59 PV_STRING
               INTO PV_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TB004 INTO TABLE LT_TB004.
    ENDIF.

  ENDIF.

* Assign Output
  PV_BPKIND = LS_TB004-BPKIND.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_bptype
*&---------------------------------------------------------------------*
*& Validate BP Type
*&---------------------------------------------------------------------*
FORM F_VALIDATE_BPTYPE USING PV_STRING TYPE STRING
                      CHANGING PV_BU_TYPE  TYPE  BUT000-TYPE
                               PV_MSGTX    TYPE  CLIKE.

  TYPES: BEGIN OF LTY_DD07L,
           BU_TYPE TYPE  DD07L-DOMVALUE_L,
         END OF LTY_DD07L.
  TYPES: LTTY_DD07L  TYPE  SORTED TABLE OF LTY_DD07L
                             WITH UNIQUE KEY BU_TYPE.

  STATICS:
    LS_DD07L TYPE  LTY_DD07L,
    LT_DD07L TYPE  LTTY_DD07L.

  DATA:
    LV_LEN     TYPE  I,
    LV_BU_TYPE TYPE  LTY_DD07L-BU_TYPE.


* Initialize Output
  CLEAR: PV_BU_TYPE,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 1
  LV_LEN = STRLEN( PV_STRING ).
  IF LV_LEN GT 1.
*   Text-e05 : Invalid BP Category:
    CONCATENATE TEXT-E05 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Convert to input format
  LV_BU_TYPE = PV_STRING.

* Check Buffer
  IF LS_DD07L-BU_TYPE NE LV_BU_TYPE.
*   Validate with Memory
    READ TABLE LT_DD07L INTO LS_DD07L
                        WITH KEY BU_TYPE = LV_BU_TYPE
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.

*     Validate with Database
      SELECT DOMVALUE_L
          UP TO 1 ROWS
        INTO LS_DD07L-BU_TYPE
        FROM DD07L
       WHERE DOMNAME EQ 'BU_TYPE'
         AND AS4LOCAL EQ 'A'
         AND DOMVALUE_L EQ LV_BU_TYPE
       ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF SY-SUBRC NE 0.
*       Text-e05 : Invalid BP Category:
        CONCATENATE TEXT-E05 PV_STRING
               INTO PV_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_DD07L INTO TABLE LT_DD07L.
    ENDIF.

  ENDIF.

* Assign Output
  PV_BU_TYPE = LS_DD07L-BU_TYPE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_bugroup
*&---------------------------------------------------------------------*
*& Validate BP Group
*&---------------------------------------------------------------------*
FORM F_VALIDATE_BUGROUP USING PV_STRING TYPE STRING
                       CHANGING PV_BU_GROUP TYPE BUT000-BU_GROUP
                                PV_MSGTX    TYPE CLIKE.

  TYPES: BEGIN OF LTY_TB001,
           BU_GROUP TYPE  TB001-BU_GROUP,
         END OF LTY_TB001.
  TYPES: LTTY_TB001  TYPE  SORTED TABLE OF LTY_TB001
                           WITH UNIQUE KEY BU_GROUP.

  STATICS:
    LS_TB001 TYPE  LTY_TB001,
    LT_TB001 TYPE  LTTY_TB001.

  DATA:
    LV_LEN      TYPE  I,
    LV_BU_GROUP TYPE  LTY_TB001-BU_GROUP.


* Initialize Output
  CLEAR: PV_BU_GROUP,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 4
  LV_LEN = STRLEN( PV_STRING ).
  IF LV_LEN GT 4.
*   Text-e04 : Invalid BP Group:
    CONCATENATE TEXT-E04 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LV_BU_GROUP = PV_STRING.

* Check Buffer
  IF LS_TB001-BU_GROUP NE LV_BU_GROUP.
*   Validate with Memory
    READ TABLE LT_TB001 INTO LS_TB001
                        WITH KEY BU_GROUP = LV_BU_GROUP
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TB001.
*     Validate with Database
      SELECT SINGLE BU_GROUP
        INTO LS_TB001
        FROM TB001
       WHERE BU_GROUP EQ LV_BU_GROUP.
      IF SY-SUBRC NE 0.
*       Text-e04 : Invalid BP Group:
        CONCATENATE TEXT-E04 PV_STRING
               INTO PV_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TB001 INTO TABLE LT_TB001.
    ENDIF.

  ENDIF.

* Assign Output
  PV_BU_GROUP = LS_TB001-BU_GROUP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_compcode
*&---------------------------------------------------------------------*
*& Validate Company Code
*&---------------------------------------------------------------------*
FORM F_VALIDATE_COMPCODE USING PV_STRING TYPE STRING
                        CHANGING PV_BUKRS   TYPE  T001-BUKRS
                                 PV_MSGTX   TYPE  CLIKE.

  TYPES: BEGIN OF LTY_T001,
           BUKRS TYPE  T001-BUKRS,
         END OF LTY_T001.
  TYPES: LTTY_T001  TYPE  SORTED TABLE OF LTY_T001
                           WITH UNIQUE KEY BUKRS.

  STATICS:
    LS_T001 TYPE  LTY_T001,
    LT_T001 TYPE  LTTY_T001.

  DATA:
    LV_LEN   TYPE  I,
    LV_BUKRS TYPE  LTY_T001-BUKRS.


* Initialize Output
  CLEAR: PV_BUKRS,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 4
  LV_LEN = STRLEN( PV_STRING ).
  IF LV_LEN GT 4.
*   Text-e10 : Invalid Company code:
    CONCATENATE TEXT-E10 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LV_BUKRS = PV_STRING.

* Check Buffer
  IF LS_T001-BUKRS NE LV_BUKRS.
*   Validate with Memory
    READ TABLE LT_T001 INTO LS_T001
                        WITH KEY BUKRS = LV_BUKRS
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_T001.
*     Validate with Database
      SELECT SINGLE BUKRS
        INTO LS_T001
        FROM T001
       WHERE BUKRS EQ LV_BUKRS.
      IF SY-SUBRC NE 0.
*       Text-e10 : Invalid Company code:
        CONCATENATE TEXT-E10 PV_STRING
               INTO PV_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_T001 INTO TABLE LT_T001.
    ENDIF.

  ENDIF.

* Assign Output
  PV_BUKRS = LS_T001-BUKRS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_glaccount
*&---------------------------------------------------------------------*
*& Validate GL Account
*&---------------------------------------------------------------------*
FORM F_VALIDATE_GLACCOUNT USING PV_STRING TYPE STRING
                                  PV_BUKRS   TYPE T001-BUKRS
                         CHANGING PV_SAKNR   TYPE SKA1-SAKNR
                                  PV_MSGTX   TYPE CLIKE.

  TYPES: BEGIN OF LTY_SKA1,
           BUKRS TYPE  T001-BUKRS,
           SAKNR TYPE  SKA1-SAKNR,
         END OF LTY_SKA1.
  TYPES: LTTY_SKA1  TYPE  SORTED TABLE OF LTY_SKA1
                           WITH UNIQUE KEY BUKRS
                                           SAKNR.

  STATICS:
    LS_SKA1 TYPE  LTY_SKA1,
    LT_SKA1 TYPE  LTTY_SKA1.

  DATA:
    LV_LEN   TYPE  I,
    LV_SAKNR TYPE  LTY_SKA1-SAKNR.


* Initialize Output
  CLEAR: PV_SAKNR,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 10
  LV_LEN = STRLEN( PV_STRING ).
  IF LV_LEN GT 10.
*   Text-e11 : Invalid GL Account:
    CONCATENATE TEXT-E11 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = PV_STRING
    IMPORTING
      OUTPUT = LV_SAKNR.

* Check Buffer
  IF LS_SKA1-BUKRS NE PV_BUKRS OR
     LS_SKA1-SAKNR NE LV_SAKNR.

*   Validate with Memory
    READ TABLE LT_SKA1 INTO LS_SKA1
                        WITH KEY BUKRS = PV_BUKRS
                                 SAKNR = LV_SAKNR
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_SKA1.
*     Validate with Database
      SELECT T001~BUKRS SKA1~SAKNR
          UP TO 1 ROWS
        INTO LS_SKA1
        FROM T001                      "#EC CI_DB_OPERATION_OK[2431747]
               INNER JOIN SKA1         "#EC CI_DB_OPERATION_OK[2389136]
                 ON  SKA1~KTOPL = T001~KTOPL           "#EC CI_BUFFJOIN
       WHERE T001~BUKRS  EQ  PV_BUKRS
         AND SKA1~SAKNR  EQ LV_SAKNR
       ORDER BY T001~BUKRS ASCENDING
                SKA1~SAKNR ASCENDING.                "#EC CI_SEL_NESTED
      ENDSELECT.
      IF SY-SUBRC NE 0.
*       Text-e11 : Invalid GL Account:
        CONCATENATE TEXT-E11 PV_STRING
               INTO PV_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_SKA1 INTO TABLE LT_SKA1.
    ENDIF.

  ENDIF.

* Assign Output
  PV_SAKNR = LS_SKA1-SAKNR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_payment_term
*&---------------------------------------------------------------------*
*& Validate Payment Term
*&---------------------------------------------------------------------*
FORM F_VALIDATE_PAYMENT_TERM USING PV_STRING TYPE STRING
                            CHANGING PV_ZTERM   TYPE  T052-ZTERM
                                     PV_MSGTX   TYPE  CLIKE.

  TYPES: BEGIN OF LTY_T052,
           ZTERM TYPE  T052-ZTERM,
         END OF LTY_T052.
  TYPES: LTTY_T052  TYPE  SORTED TABLE OF LTY_T052
                           WITH UNIQUE KEY ZTERM.

  STATICS:
    LS_T052 TYPE  LTY_T052,
    LT_T052 TYPE  LTTY_T052.

  DATA:
    LV_ZTERM TYPE  LTY_T052-ZTERM.


* Initialize Output
  CLEAR: PV_ZTERM,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 4
  IF STRLEN( PV_STRING ) GT 4.
*   Text-e88 : Invalid Payment Terms:
    CONCATENATE TEXT-E88 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LV_ZTERM = PV_STRING.

* Check Buffer
  IF LS_T052-ZTERM NE LV_ZTERM.
*   Validate with Memory
    READ TABLE LT_T052 INTO LS_T052
                        WITH KEY ZTERM = LV_ZTERM
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_T052.
*     Validate with Database
      SELECT  ZTERM                                     "#EC CI_NOORDER
        INTO  LS_T052 UP TO 1 ROWS
        FROM  T052
        WHERE ZTERM EQ LV_ZTERM.
      ENDSELECT.
      IF SY-SUBRC NE 0.
*       Text-e88 : Invalid Payment Terms:
        CONCATENATE TEXT-E88 PV_STRING
               INTO PV_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_T052 INTO TABLE LT_T052.
    ENDIF.

  ENDIF.

* Assign Output
  PV_ZTERM = LS_T052-ZTERM.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_flag
*&---------------------------------------------------------------------*
*& Validate Flag
*&---------------------------------------------------------------------*
FORM F_VALIDATE_FLAG USING PV_STRING TYPE STRING
                    CHANGING PV_FLAG    TYPE  FLAG
                             PV_MSGTX   TYPE  CLIKE.

  DATA:
    LV_FLAG  TYPE  CHAR1.


* Initialize Output
  CLEAR: PV_FLAG,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

  IF STRLEN( PV_STRING ) GT 1.
*   Text-e07 : Invalid Flag value:
    CONCATENATE TEXT-E07 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LV_FLAG  = PV_STRING.
  TRANSLATE LV_FLAG TO UPPER CASE.

  IF NOT LV_FLAG CO ' X'.
*   Text-e07 : Invalid Flag value:
    CONCATENATE TEXT-E07 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Assign Output
  PV_FLAG = LV_FLAG.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_customer
*&---------------------------------------------------------------------*
*& Validate Customer Number
*&---------------------------------------------------------------------*
FORM F_VALIDATE_CUSTOMER USING PV_STRING TYPE STRING
                        CHANGING PV_KUNNR   TYPE KNA1-KUNNR
                                 PV_MSGTX   TYPE CLIKE.

  TYPES: BEGIN OF LTY_KNA1,
           KUNNR TYPE  KNA1-KUNNR,
         END OF LTY_KNA1.
  TYPES: LTTY_KNA1  TYPE  SORTED TABLE OF LTY_KNA1
                           WITH UNIQUE KEY KUNNR.

  STATICS:
    LS_KNA1 TYPE  LTY_KNA1,
    LT_KNA1 TYPE  LTTY_KNA1.

  DATA:
    LV_LEN   TYPE  I,
    LV_KUNNR TYPE  LTY_KNA1-KUNNR.


* Initialize Output
  CLEAR: PV_KUNNR,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 10
  LV_LEN = STRLEN( PV_STRING ).
  IF LV_LEN GT 10.
*   Text-e12 : Invalid Customer no.:
    CONCATENATE TEXT-E12 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = PV_STRING
    IMPORTING
      OUTPUT = LV_KUNNR.

* Check Buffer
  IF LS_KNA1-KUNNR NE LV_KUNNR.

*   Validate with Memory
    READ TABLE LT_KNA1 INTO LS_KNA1
                        WITH KEY KUNNR = LV_KUNNR
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_KNA1.
*     Validate with Database
      SELECT SINGLE KUNNR
        INTO LS_KNA1
        FROM KNA1
       WHERE KUNNR  EQ LV_KUNNR.                     "#EC CI_SEL_NESTED
      IF SY-SUBRC NE 0.
*       Text-e12 : Invalid Customer no.:
        CONCATENATE TEXT-E12 PV_STRING
               INTO PV_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_KNA1 INTO TABLE LT_KNA1.
    ENDIF.

  ENDIF.

* Assign Output
  PV_KUNNR = LS_KNA1-KUNNR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_date
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_STRING
*&      <-- PS_MAIN_VIEW_COMP_WTAX01_AGTDF
*&      <-- LV_MSGTX
*&---------------------------------------------------------------------*
FORM F_VALIDATE_DATE USING PV_STRING TYPE STRING
                    CHANGING PV_DATUM   TYPE  SY-DATUM
                             PV_MSGTX   TYPE  CLIKE.

  DATA:
    LV_LENGTH  TYPE  I.


* Initialize Output
  CLEAR: PV_DATUM,
         PV_MSGTX.

* Not initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Only number and .?
  IF NOT PV_STRING CO '0123456789.'.
*   Text-e06 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E06 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Length?
  LV_LENGTH = STRLEN( PV_STRING ).
  IF LV_LENGTH NE 10.
*   Text-e06 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E06 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* First 2 Digit is date
  IF NOT PV_STRING(2) BETWEEN 01 AND 31 ##NUMBER_OK.
*   Text-e06 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E06 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* 4-5th Digit is month
  IF NOT PV_STRING+3(2) BETWEEN 01 AND 12 ##NUMBER_OK.
*   Text-e06 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E06 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

** 7-8th digit is year
*  IF NOT pv_string+6(4) BETWEEN 1900 AND 2200 AND
*     pv_string+6(4) NE '9999'.
**   Text-e06 : Wrong Date format. Please use format DD.MM.YYYY:
*    CONCATENATE TEXT-e06 pv_string
*           INTO pv_msgtx
*      SEPARATED BY space.
*    RETURN.
*  ENDIF.

* Assign Output
  CONCATENATE PV_STRING+6(4) PV_STRING+3(2) PV_STRING(2)
         INTO PV_DATUM.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_cust_class
*&---------------------------------------------------------------------*
*& Validate Customer Classification
*&---------------------------------------------------------------------*
FORM F_VALIDATE_CUST_CLASS USING PV_STRING TYPE STRING
                          CHANGING PV_KUKLA   TYPE  TKUKL-KUKLA
                                   PV_MSGTX   TYPE  CLIKE.

  TYPES: BEGIN OF LTY_TKUKL,
           KUKLA TYPE  TKUKL-KUKLA,
         END OF LTY_TKUKL.
  TYPES: LTTY_TKUKL  TYPE  SORTED TABLE OF LTY_TKUKL
                           WITH UNIQUE KEY KUKLA.

  STATICS:
    LS_TKUKL TYPE  LTY_TKUKL,
    LT_TKUKL TYPE  LTTY_TKUKL.

  DATA:
    LV_KUKLA TYPE  LTY_TKUKL-KUKLA.


* Initialize Output
  CLEAR: PV_KUKLA,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 2
  IF STRLEN( PV_STRING ) GT 2.
*   Text-e86 : Invalid Customer Classification:
    CONCATENATE TEXT-E86 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LV_KUKLA = PV_STRING.

* Check Buffer
  IF LS_TKUKL-KUKLA NE LV_KUKLA.
*   Validate with Memory
    READ TABLE LT_TKUKL INTO LS_TKUKL
                        WITH KEY KUKLA = LV_KUKLA
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TKUKL.
*     Validate with Database
      SELECT SINGLE KUKLA
        INTO LS_TKUKL
        FROM TKUKL
       WHERE KUKLA EQ LV_KUKLA.
      IF SY-SUBRC NE 0.
*       Text-e86 : Invalid Customer Classification:
        CONCATENATE TEXT-E86 PV_STRING
               INTO PV_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TKUKL INTO TABLE LT_TKUKL.
    ENDIF.

  ENDIF.

* Assign Output
  PV_KUKLA = LS_TKUKL-KUKLA.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_industry_code
*&---------------------------------------------------------------------*
*& Validate Industry Code
*&---------------------------------------------------------------------*
FORM F_VALIDATE_INDUSTRY_CODE USING PV_STRING TYPE STRING
                             CHANGING PV_BRACO   TYPE  TBRC-BRACO
                                      PV_MSGTX   TYPE  CLIKE.

  TYPES: BEGIN OF LTY_TBRC,
           BRACO TYPE  TBRC-BRACO,
         END OF LTY_TBRC.
  TYPES: LTTY_TBRC  TYPE  SORTED TABLE OF LTY_TBRC
                           WITH UNIQUE KEY BRACO.

  STATICS:
    LS_TBRC TYPE  LTY_TBRC,
    LT_TBRC TYPE  LTTY_TBRC.

  DATA:
    LV_BRACO TYPE  LTY_TBRC-BRACO.


* Initialize Output
  CLEAR: PV_BRACO,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 10
  IF STRLEN( PV_STRING ) GT 10.
*   Text-e87 : Invalid Industry Code:
    CONCATENATE TEXT-E87 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LV_BRACO = PV_STRING.

* Check Buffer
  IF LS_TBRC-BRACO NE LV_BRACO.
*   Validate with Memory
    READ TABLE LT_TBRC INTO LS_TBRC
                        WITH KEY BRACO = LV_BRACO
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TBRC.
*     Validate with Database
      SELECT SINGLE BRACO
        INTO LS_TBRC
        FROM TBRC
       WHERE BRACO EQ LV_BRACO.
      IF SY-SUBRC NE 0.
*       Text-e87 : Invalid Industry Code:
        CONCATENATE TEXT-E87 PV_STRING
               INTO PV_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TBRC INTO TABLE LT_TBRC.
    ENDIF.

  ENDIF.

* Assign Output
  PV_BRACO = LS_TBRC-BRACO.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_gender
*&---------------------------------------------------------------------*
*& Validate Gender
*&---------------------------------------------------------------------*
FORM F_VALIDATE_GENDER USING PV_STRING TYPE STRING
                      CHANGING PV_GENDR  TYPE  GTY_MAIN21-GENDR
                               PV_MSGTX  TYPE  CLIKE.

* Initialize Output
  CLEAR: PV_GENDR,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

  IF PV_STRING NE GC_GENDR_MALE AND
     PV_STRING NE GC_GENDR_FEMALE AND
     PV_STRING NE GC_GENDR_UNKNW .

*   Text-e09 : Invalid Gender:
    CONCATENATE TEXT-E09 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Assign Output
  PV_GENDR = PV_STRING.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_key21
*&---------------------------------------------------------------------*
*& Validate Key
*&---------------------------------------------------------------------*
FORM F_VALIDATE_KEY21 USING PS_MAIN TYPE GTY_MAIN21 ##NEEDED
                      CHANGING PS_KEY   TYPE GTY_KEY21
                              PS_MESSG  TYPE GTY_MESSG.

  DATA:
    LV_PARTNER TYPE  BUT000-PARTNER ##NEEDED,
*    lv_guid    TYPE  but000-partner_guid,
    LV_BUKRS   TYPE  T001-BUKRS ##NEEDED.

* Initialize Output
  CLEAR: PS_MESSG.

  IF PS_KEY-RLTYP IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e14 : Missing BP Role in the file.
    PS_MESSG-MSGTX = TEXT-E14.
    RETURN.
  ENDIF.

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
  ENDIF.

* Check Number on Creation
  CASE GV_MODE.
    WHEN GC_MODE_CREATE.
*        "Check partner and bu group
*        CALL FUNCTION 'BUP_NUMBER_CHECK'
*          EXPORTING
*            i_partner     = ps_key-partner
*            i_group       = ps_key-bu_group
*            i_aktyp       = '01'
*            i_aktdb       = '01'
*          EXCEPTIONS
*            error_occured = 1
*            OTHERS        = 2.
*        IF sy-subrc <> 0 .
*          ps_messg-msgty = 'E'.
*          ps_messg-msgid = sy-msgid.
*          ps_messg-msgno = sy-msgno.
*          MESSAGE ID sy-msgid
*                  TYPE  sy-msgty
*                  NUMBER sy-msgno
*                  WITH sy-msgv1 sy-msgv2
*                       sy-msgv3 sy-msgv4
*                  INTO ps_messg-msgtx.
*          RETURN.
*        ENDIF.
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

* Check Roles Existing
  IF PS_KEY-PARTNER IS NOT INITIAL.
    SELECT SINGLE PARTNER
      INTO LV_PARTNER
      FROM BUT000
     WHERE PARTNER EQ PS_KEY-PARTNER.                "#EC CI_SEL_NESTED
    IF SY-SUBRC EQ 0.
      PS_KEY-PARTNER_EXIST = GC_TRUE.
    ENDIF.
    SELECT PARTNER
        UP TO 1 ROWS
      INTO LV_PARTNER
      FROM BUT100
     WHERE PARTNER EQ PS_KEY-PARTNER
       AND RLTYP   EQ PS_KEY-RLTYP
     ORDER BY PRIMARY KEY.                           "#EC CI_SEL_NESTED
    ENDSELECT.
    IF SY-SUBRC EQ 0.
      PS_KEY-RLTYP_EXIST = GC_TRUE.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_language
*&---------------------------------------------------------------------*
*& Validate Language
*&---------------------------------------------------------------------*
FORM F_VALIDATE_LANGUAGE USING PV_STRING TYPE STRING
                        CHANGING PV_SPRAS   TYPE  T002-SPRAS
                                 PV_MSGTX   TYPE  CLIKE.

  TYPES: BEGIN OF LTY_T002,
           SPRAS TYPE  T002-SPRAS,
         END OF LTY_T002.
  TYPES: LTTY_T002  TYPE  SORTED TABLE OF LTY_T002
                           WITH UNIQUE KEY SPRAS.

  STATICS:
    LS_T002 TYPE  LTY_T002,
    LT_T002 TYPE  LTTY_T002.

  DATA:
    LV_LEN   TYPE  I,
    LV_SPRAS TYPE  LTY_T002-SPRAS.


* Initialize Output
  CLEAR: PV_SPRAS,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 2
  LV_LEN = STRLEN( PV_STRING ).
  IF LV_LEN GT 2.
*   Text-e35 : Invalid Language:
    CONCATENATE TEXT-E35 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
    EXPORTING
      INPUT            = PV_STRING
    IMPORTING
      OUTPUT           = LV_SPRAS
    EXCEPTIONS
      UNKNOWN_LANGUAGE = 1
      OTHERS           = 2.
  IF SY-SUBRC <> 0.
*   Text-e35 : Invalid Language:
    CONCATENATE TEXT-E35 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Check Buffer
  IF LS_T002-SPRAS NE LV_SPRAS.

*   Validate with Memory
    READ TABLE LT_T002 INTO LS_T002
                        WITH KEY SPRAS = LV_SPRAS
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_T002.
*     Validate with Database
      SELECT SINGLE SPRAS
        INTO LS_T002
        FROM T002
       WHERE SPRAS  EQ LV_SPRAS.
      IF SY-SUBRC NE 0.
*       Text-e35 : Invalid Language:
        CONCATENATE TEXT-E35 PV_STRING
               INTO PV_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_T002 INTO TABLE LT_T002.
    ENDIF.

  ENDIF.

* Assign Output
  PV_SPRAS = LS_T002-SPRAS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_main21
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_KEY
*&      <-- LS_MAIN
*&      <-- LS_MAINX
*&      <-- LS_MESSG
*&---------------------------------------------------------------------*
FORM F_VALIDATE_MAIN21 USING PS_KEY TYPE GTY_KEY21
                     CHANGING PS_MAIN  TYPE  GTY_MAIN21
                              PS_MAINX TYPE  GTY_MAIN21X
                              PS_MESSG TYPE  GTY_MESSG.

*-Check One-time (No need to validate address)
  CLEAR GV_ONETIME .
* BOD - CH01
*  IF PS_KEY-BU_GROUP = 'Z304' .
*    GV_ONETIME = GC_TRUE .
*  ENDIF.
* EOD - CH01

* BOI - CH01
* ---------------------------------
*  Validate Required Fields
* ---------------------------------
  PERFORM F_VALIDATE_REQUIRED_FLD_MAIN21 USING PS_KEY
                                      CHANGING PS_MAIN
                                               PS_MAINX
                                               PS_MESSG.
  IF PS_MESSG IS NOT INITIAL.
*    EXIT.
    RETURN.
  ENDIF.
* EOI - CH01

* ---------------------------------
*  Validate Address
* ---------------------------------
*  Validate Address
  PERFORM F_VALIDATE_ADDRESS  USING  PS_KEY
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
  PERFORM F_VALIDATE_ADDRESS_INT  USING  PS_KEY
                                CHANGING PS_MAIN-ADDR_INT
                                         PS_MAINX-ADDR_INT
                                         PS_MESSG.
  IF PS_MESSG IS NOT INITIAL.
*    EXIT.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_address
*&---------------------------------------------------------------------*
*& Validate Address
*&---------------------------------------------------------------------*
FORM F_VALIDATE_ADDRESS USING PS_KEY TYPE GTY_KEY21
                       CHANGING PS_ADDR    TYPE  GTY_ADDR
                                PS_ADDRX   TYPE  GTY_ADDRX  ##NEEDED
                                PS_MESSG   TYPE  GTY_MESSG.

* Initialize Output
  CLEAR: PS_MESSG.

* Get GUID for Address if Exists
  CALL FUNCTION 'BAPI_BUPA_ADDRESS_GET_NUMBERS'
    EXPORTING
      BUSINESSPARTNER = PS_KEY-PARTNER
    IMPORTING
      ADDRESSGUIDOUT  = PS_ADDR-GUID.

  CASE PS_KEY-TYPE.
    WHEN GC_TYPE_ORG.
      IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
           PS_ADDRX-NAME1 EQ GC_TRUE ) AND
          PS_ADDR-NAME1 IS INITIAL.
        PS_MESSG-MSGTY = 'E'.
        PS_MESSG-MSGID = 'ZTEC'.
        PS_MESSG-MSGNO = '000'.
*       Text-e44 : Missing Name 1 in the file.
        PS_MESSG-MSGTX = TEXT-E44.
        RETURN.
      ENDIF.
    WHEN GC_TYPE_PERSON.
      IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
           PS_ADDRX-NAME_LAST EQ GC_TRUE ) AND
          PS_ADDR-NAME_LAST IS INITIAL.
        PS_MESSG-MSGTY = 'E'.
        PS_MESSG-MSGID = 'ZTEC'.
        PS_MESSG-MSGNO = '000'.
*       Text-e58 : Missing Last name in the file.
        PS_MESSG-MSGTX = TEXT-E58.
        RETURN.
      ENDIF.
  ENDCASE.

* BOI - CH01
* Skip validation for address when BP Type = Person
  IF PS_KEY-TYPE EQ GC_TYPE_PERSON.
    RETURN.
  ENDIF.
* EOI - CH01

*  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
*       PS_ADDRX-SORT1 EQ GC_TRUE ) AND
*      PS_ADDR-SORT1 IS INITIAL.
*    PS_MESSG-MSGTY = 'E'.
*    PS_MESSG-MSGID = 'ZTEC'.
*    PS_MESSG-MSGNO = '000'.
**   Text-e45 : Missing SearchTerm1 in the file.
*    PS_MESSG-MSGTX = TEXT-E45.
*    RETURN.
*  ENDIF.

  IF GV_ONETIME IS INITIAL. "Case one time no need to validate
    IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
         PS_ADDRX-STREET EQ GC_TRUE ) AND
        PS_ADDR-STREET IS INITIAL.
      PS_MESSG-MSGTY = 'E'.
      PS_MESSG-MSGID = 'ZTEC'.
      PS_MESSG-MSGNO = '000'.
*   Text-e46 : Missing Street/House number in the file.
      PS_MESSG-MSGTX = TEXT-E46.
      RETURN.
    ENDIF.

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

*    IF ( ps_key-partner_exist IS INITIAL OR
*         ps_addrx-post_code1 EQ gc_true ) AND
*        ps_addr-post_code1 IS INITIAL.
*      ps_messg-msgty = 'E'.
*      ps_messg-msgid = 'ZTEC'.
*      ps_messg-msgno = '000'.
**     Text-e19 : Missing Postal Code in the file.
*      ps_messg-msgtx = TEXT-e19.
*      RETURN.
*    ENDIF.
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
*& Form f_validate_address_int
*&---------------------------------------------------------------------*
*& Validate Address International
*&---------------------------------------------------------------------*
FORM F_VALIDATE_ADDRESS_INT USING PS_KEY TYPE GTY_KEY21
                           CHANGING PS_ADDR  TYPE  GTY_ADDR_INT  ##NEEDED
                                    PS_ADDRX TYPE  GTY_ADDR_INTX ##NEEDED
                                    PS_MESSG TYPE  GTY_MESSG.

* Initialize Output
  CLEAR: PS_MESSG.

* Only when Active
  IF PS_ADDR-ACTIVE IS INITIAL.
    RETURN.
  ENDIF.

* Case one time no need to validate address
  IF GV_ONETIME IS INITIAL .
    RETURN .
  ENDIF .

  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
       PS_ADDRX-STREET EQ GC_TRUE ) AND
      PS_ADDR-STREET IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e46 : Missing Street/House number in the file.
    PS_MESSG-MSGTX = TEXT-E46.
    RETURN.
  ENDIF.

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

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_nation
*&---------------------------------------------------------------------*
*& Validate Nation
*&---------------------------------------------------------------------*
FORM F_VALIDATE_NATION USING PV_STRING TYPE STRING
                      CHANGING PV_NATION  TYPE  ADRC-NATION
                               PV_MSGTX   TYPE  CLIKE.

  TYPES: BEGIN OF LTY_TSADV,
           NATION TYPE  TSADV-NATION,
         END OF LTY_TSADV.
  TYPES: LTTY_TSADV  TYPE  SORTED TABLE OF LTY_TSADV
                           WITH UNIQUE KEY NATION.

  STATICS:
    LS_TSADV TYPE  LTY_TSADV,
    LT_TSADV TYPE  LTTY_TSADV.

  DATA:
    LV_LEN    TYPE  I,
    LV_NATION TYPE  LTY_TSADV-NATION.


* Initialize Output
  CLEAR: PV_NATION,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 1
  LV_LEN = STRLEN( PV_STRING ).
  IF LV_LEN GT 1.
*   Text-e08 : Invalid Address Version:
    CONCATENATE TEXT-E08 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LV_NATION = PV_STRING.

* Check Buffer
  IF LS_TSADV-NATION NE LV_NATION.
*   Validate with Memory
    READ TABLE LT_TSADV INTO LS_TSADV
                        WITH KEY NATION = LV_NATION
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TSADV.
*     Validate with Database
      SELECT SINGLE NATION
        INTO LS_TSADV
        FROM TSADV
       WHERE NATION EQ LV_NATION.
      IF SY-SUBRC NE 0.
*       Text-e08 : Invalid Address Version:
        CONCATENATE TEXT-E08 PV_STRING
               INTO PV_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TSADV INTO TABLE LT_TSADV.
    ENDIF.

  ENDIF.

* Assign Output
  PV_NATION = LS_TSADV-NATION.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_region
*&---------------------------------------------------------------------*
*& Validate Region
*&---------------------------------------------------------------------*
*FORM F_VALIDATE_REGION USING PV_LAND1 TYPE T005S-LAND1
*                               PV_STRING TYPE  STRING
*                      CHANGING PV_BLAND  TYPE  T005S-BLAND
*                               PV_MSGTX  TYPE  CLIKE.
*
*  TYPES: BEGIN OF LTY_T005S,
*           LAND1 TYPE  T005S-LAND1,
*           BLAND TYPE  T005S-BLAND,
*         END OF LTY_T005S.
*  TYPES: LTTY_T005S  TYPE  SORTED TABLE OF LTY_T005S
*                           WITH UNIQUE KEY LAND1
*                                           BLAND.
*
*  STATICS:
*    LS_T005S TYPE  LTY_T005S,
*    LT_T005S TYPE  LTTY_T005S.
*
*  DATA:
*    LV_LEN   TYPE  I,
*    LV_BLAND TYPE  LTY_T005S-BLAND.
*
*
** Initialize Output
*  CLEAR: PV_BLAND,
*         PV_MSGTX.
*
** Check Not Initial
*  IF PV_STRING IS INITIAL.
*    RETURN.
*  ENDIF.
*
** Length = 3
*  LV_LEN = STRLEN( PV_STRING ).
*  IF LV_LEN GT 3.
**   Text-e84 : Invalid Region:
*    CONCATENATE TEXT-E84 PV_STRING
*           INTO PV_MSGTX
*      SEPARATED BY SPACE.
*    RETURN.
*  ENDIF.
*
*  LV_BLAND = PV_STRING.
*
** Check Buffer
*  IF LS_T005S-LAND1 NE PV_LAND1 OR
*     LS_T005S-BLAND NE LV_BLAND.
*
**   Validate with Memory
*    READ TABLE LT_T005S INTO LS_T005S
*                        WITH KEY LAND1 = PV_LAND1
*                                 BLAND = LV_BLAND
*                        BINARY SEARCH.
*    IF SY-SUBRC NE 0.
*      CLEAR LS_T005S.
**     Validate with Database
*      SELECT SINGLE LAND1 BLAND
*        INTO LS_T005S
*        FROM T005S
*       WHERE LAND1  EQ PV_LAND1
*         AND BLAND  EQ LV_BLAND.
*      IF SY-SUBRC NE 0.
**       Text-e84 : Invalid Region:
*        CONCATENATE TEXT-E84 PV_STRING
*               INTO PV_MSGTX
*          SEPARATED BY SPACE.
*        RETURN.
*      ENDIF.
*      INSERT LS_T005S INTO TABLE LT_T005S.
*    ENDIF.
*
*  ENDIF.
*
** Assign Output
*  PV_BLAND = LS_T005S-BLAND.
*
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_transzone
*&---------------------------------------------------------------------*
*&  Validate Transport Zone
*&---------------------------------------------------------------------*
*FORM F_VALIDATE_TRANSZONE USING PV_LAND1 TYPE TZONE-LAND1
*                                  PV_STRING TYPE  STRING
*                         CHANGING PV_ZONE1  TYPE  TZONE-ZONE1
*                                  PV_MSGTX  TYPE  CLIKE.
*
*  TYPES: BEGIN OF LTY_TZONE,
*           LAND1 TYPE  TZONE-LAND1,
*           ZONE1 TYPE  TZONE-ZONE1,
*         END OF LTY_TZONE.
*  TYPES: LTTY_TZONE  TYPE  SORTED TABLE OF LTY_TZONE
*                           WITH UNIQUE KEY LAND1
*                                           ZONE1.
*
*  STATICS:
*    LS_TZONE TYPE  LTY_TZONE,
*    LT_TZONE TYPE  LTTY_TZONE.
*
*  DATA:
*    LV_LEN   TYPE  I,
*    LV_ZONE1 TYPE  LTY_TZONE-ZONE1.
*
*
** Initialize Output
*  CLEAR: PV_ZONE1,
*         PV_MSGTX.
*
** Check Not Initial
*  IF PV_STRING IS INITIAL.
*    RETURN.
*  ENDIF.
*
** Length = 10
*  LV_LEN = STRLEN( PV_STRING ).
*  IF LV_LEN GT 10.
**   Text-e85 : Invalid Transportation Zone:
*    CONCATENATE TEXT-E85 PV_STRING
*           INTO PV_MSGTX
*      SEPARATED BY SPACE.
*    RETURN.
*  ENDIF.
*
*  LV_ZONE1 = PV_STRING.
*
** Check Buffer
*  IF LS_TZONE-LAND1 NE PV_LAND1 OR
*     LS_TZONE-ZONE1 NE LV_ZONE1.
*
**   Validate with Memory
*    READ TABLE LT_TZONE INTO LS_TZONE
*                        WITH KEY LAND1 = PV_LAND1
*                                 ZONE1 = LV_ZONE1
*                        BINARY SEARCH.
*    IF SY-SUBRC NE 0.
*      CLEAR LS_TZONE.
**     Validate with Database
*      SELECT SINGLE LAND1 ZONE1
*        INTO LS_TZONE
*        FROM TZONE
*       WHERE LAND1  EQ PV_LAND1
*         AND ZONE1  EQ LV_ZONE1.
*      IF SY-SUBRC NE 0.
**       Text-e85 : Invalid Transportation Zone:
*        CONCATENATE TEXT-E85 PV_STRING
*               INTO PV_MSGTX
*          SEPARATED BY SPACE.
*        RETURN.
*      ENDIF.
*      INSERT LS_TZONE INTO TABLE LT_TZONE.
*    ENDIF.
*
*  ENDIF.
*
** Assign Output
*  PV_ZONE1 = LS_TZONE-ZONE1.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_tradepartnr
*&---------------------------------------------------------------------*
*& Validate Trading Partner
*&---------------------------------------------------------------------*
FORM F_VALIDATE_TRADEPARTNR USING PV_STRING TYPE STRING
                           CHANGING PV_RCOMP  TYPE  T880-RCOMP
                                    PV_MSGTX  TYPE  CLIKE.

  TYPES: BEGIN OF LTY_T880,
           RCOMP TYPE  T880-RCOMP,
         END OF LTY_T880.
  TYPES: LTTY_T880  TYPE  SORTED TABLE OF LTY_T880
                           WITH UNIQUE KEY RCOMP.

  STATICS:
    LS_T880 TYPE  LTY_T880,
    LT_T880 TYPE  LTTY_T880.

  DATA:
    LV_LEN   TYPE  I,
    LV_RCOMP TYPE  LTY_T880-RCOMP.


* Initialize Output
  CLEAR: PV_RCOMP,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 6
  LV_LEN = STRLEN( PV_STRING ).
  IF LV_LEN GT 6.
*   Text-e19 : Invalid Trading Partner:
    CONCATENATE TEXT-E19 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = PV_STRING
    IMPORTING
      OUTPUT = LV_RCOMP.

* Check Buffer
  IF LS_T880-RCOMP NE LV_RCOMP.
*   Validate with Memory
    READ TABLE LT_T880 INTO LS_T880
                        WITH KEY RCOMP = LV_RCOMP
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_T880.
*     Validate with Database
      SELECT SINGLE RCOMP
        INTO LS_T880
        FROM T880
       WHERE RCOMP EQ LV_RCOMP.
      IF SY-SUBRC NE 0.
*       Text-e19 : Invalid Trading Partner:
        CONCATENATE TEXT-E19 PV_STRING
               INTO PV_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_T880 INTO TABLE LT_T880.
    ENDIF.

  ENDIF.

* Assign Output
  PV_RCOMP = LS_T880-RCOMP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_view_comp
*&---------------------------------------------------------------------*
*& Validate Company view data
*&---------------------------------------------------------------------*
FORM F_VALIDATE_VIEW_COMP USING PS_KEY TYPE GTY_KEY21 ##NEEDED
                                  PS_VIEW_CUST  TYPE  GTY_VIEW_CUST
                         CHANGING PS_VIEW_COMP  TYPE  GTY_VIEW_COMP
                                  PS_VIEW_COMPX TYPE GTY_VIEW_COMPX ##NEEDED
                                  PS_MESSG      TYPE  GTY_MESSG.
  TYPES: BEGIN OF LTY_KNBW,
           WITHT TYPE  KNBW-WITHT,
         END OF LTY_KNBW.
  TYPES: LTTY_KNBW  TYPE  SORTED TABLE OF LTY_KNBW
                          WITH UNIQUE KEY WITHT.

  DATA:
    LT_KNBW   TYPE  LTTY_KNBW.

  DATA:
    LV_KUNNR  TYPE  KNB1-KUNNR.


* Initialize Output
  CLEAR: PS_MESSG.

  IF PS_VIEW_COMP-ACTIVE IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-t03 : Require Extra (Company Code View).
    PS_MESSG-MSGTX = TEXT-T03.
    RETURN.
  ENDIF.

  SELECT SINGLE KUNNR
    INTO LV_KUNNR
    FROM KNB1
   WHERE KUNNR EQ PS_VIEW_CUST-KUNNR
     AND BUKRS EQ PS_VIEW_COMP-BUKRS.                "#EC CI_SEL_NESTED
  IF SY-SUBRC EQ 0.
    PS_VIEW_COMP-EXIST = GC_TRUE.
  ENDIF.

  IF PS_VIEW_COMP-EXIST IS INITIAL AND
     PS_VIEW_COMP-BUKRS IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e54 : Missing Company Code in the file.
    PS_MESSG-MSGTX = TEXT-E54.
    RETURN.
  ENDIF.

  IF ( PS_VIEW_COMP-EXIST IS INITIAL OR
       PS_VIEW_COMPX-AKONT EQ GC_TRUE ) AND
     PS_VIEW_COMP-AKONT IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e55 : Missing Reconciliation acct. in the file.
    PS_MESSG-MSGTX = TEXT-E55.
    RETURN.
  ENDIF.


*  IF ( ps_view_comp-exist IS INITIAL OR
*       ps_view_compx-xzver EQ gc_true ) AND
*     ps_view_comp-xzver IS INITIAL.
*    ps_messg-msgty = 'E'.
*    ps_messg-msgid = 'ZTEC'.
*    ps_messg-msgno = '000'.
**   Text-e57 : Missing Record Pmnt History in the file.
*    ps_messg-msgtx = TEXT-e57.
*    RETURN.
*  ENDIF.
*
*  IF ( ps_view_comp-exist IS INITIAL OR
*       ps_view_compx-xverr EQ gc_true ) AND
*     ps_view_comp-xverr IS INITIAL.
*    ps_messg-msgty = 'E'.
*    ps_messg-msgid = 'ZTEC'.
*    ps_messg-msgno = '000'.
**   Text-e57 : Missing Record Pmnt History in the file.
*    ps_messg-msgtx = TEXT-e57.
*    RETURN.
*  ENDIF.

* Check Withhold Exists?
  IF PS_VIEW_COMP-EXIST EQ GC_TRUE.
    SELECT WITHT
      INTO TABLE LT_KNBW
      FROM KNBW
     WHERE KUNNR  EQ  PS_VIEW_CUST-KUNNR
       AND BUKRS  EQ  PS_VIEW_COMP-BUKRS.            "#EC CI_SEL_NESTED
    IF SY-SUBRC NE 0.
      CLEAR: LT_KNBW.
    ENDIF.

    READ TABLE LT_KNBW TRANSPORTING NO FIELDS
                       WITH KEY WITHT = PS_VIEW_COMP-WTAX01-WITHT
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      PS_VIEW_COMP-WTAX01-EXIST = GC_TRUE.
    ENDIF.
    READ TABLE LT_KNBW TRANSPORTING NO FIELDS
                       WITH KEY WITHT = PS_VIEW_COMP-WTAX02-WITHT
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      PS_VIEW_COMP-WTAX02-EXIST = GC_TRUE.
    ENDIF.
    READ TABLE LT_KNBW TRANSPORTING NO FIELDS
                       WITH KEY WITHT = PS_VIEW_COMP-WTAX03-WITHT
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      PS_VIEW_COMP-WTAX03-EXIST = GC_TRUE.
    ENDIF.

    READ TABLE LT_KNBW TRANSPORTING NO FIELDS
                       WITH KEY WITHT = PS_VIEW_COMP-WTAX04-WITHT
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      PS_VIEW_COMP-WTAX04-EXIST = GC_TRUE.
    ENDIF.


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_display_result21
*&---------------------------------------------------------------------*
*& Display Result
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT21 USING PT_RESULT TYPE GTTY_RESULT21.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
*  gv_container_1 = 'CTL_ALV_1'.
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
*  gv_alv_header_1 = gc_true.
  GF_ALV_HEADER_1 = GC_TRUE.
* Soft refresh data
*  gv_soft_refresh_1 = gc_true.
  GF_SOFT_REFRESH_1 = GC_TRUE.

* ALV Layout
  PERFORM F_ALV_LAYOUT1 CHANGING GS_LAYOUT_1
                                 GS_VARIANT_1
                                 GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_21.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_21.
  ASSIGN PT_RESULT TO <G_LIST_1>. "<gfs_list_1>.          "#EC CI_FLDEXT_OK[2215424]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT21 CHANGING GT_FIELDCAT_1.
* Sort data
  PERFORM F_ALV_SORT_RESULT21 CHANGING GT_SORT_1.
* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_layout1
*&---------------------------------------------------------------------*
*& ALV Layout for Report
*&---------------------------------------------------------------------*
FORM F_ALV_LAYOUT1 CHANGING PS_LAYOUT TYPE LVC_S_LAYO
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
*& Form f_alv_build_fieldcat21
*&---------------------------------------------------------------------*
*& ALV Field Catalog for Report1
*&---------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT21 CHANGING PT_FIELDCAT TYPE LVC_T_FCAT.

  DATA:
    LV_TEXT         TYPE  TEXT50.

  FIELD-SYMBOLS:
    <LFS_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_21
                              CHANGING PT_FIELDCAT.

  LOOP AT PT_FIELDCAT ASSIGNING <LFS_FIELDCAT>.

*   Force show all by default
    <LFS_FIELDCAT>-NO_OUT = SPACE.

    CASE <LFS_FIELDCAT>-FIELDNAME.
      WHEN 'ROWNO'.
        <LFS_FIELDCAT>-KEY       = GC_TRUE.
        <LFS_FIELDCAT>-OUTPUTLEN = 6.
      WHEN 'STATU'.
        <LFS_FIELDCAT>-KEY       = GC_TRUE.
*       Text-c01 : Status
        LV_TEXT                  = TEXT-C01.
        <LFS_FIELDCAT>-REPTEXT   = LV_TEXT.
        <LFS_FIELDCAT>-COLTEXT   = LV_TEXT.
        <LFS_FIELDCAT>-SCRTEXT_L = LV_TEXT.
        <LFS_FIELDCAT>-SCRTEXT_M = LV_TEXT.
        <LFS_FIELDCAT>-SCRTEXT_S = LV_TEXT.
        <LFS_FIELDCAT>-OUTPUTLEN = 6.
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
      WHEN 'PARTNER'.
        IF CB_TEST IS INITIAL.
          <LFS_FIELDCAT>-HOTSPOT   = GC_TRUE.
        ENDIF.
      WHEN 'BU_GROUP'.
      WHEN 'TYPE'.
      WHEN 'ADDR_NAME_FIRST'.
        <LFS_FIELDCAT>-OUTPUTLEN = 25 ##NUMBER_OK.
      WHEN 'ADDR_NAME_LAST'.
        <LFS_FIELDCAT>-OUTPUTLEN = 25 ##NUMBER_OK.
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
      WHEN 'ADDR_BUILDING'.
      WHEN 'ADDR_NAME_CO'.
        <LFS_FIELDCAT>-OUTPUTLEN = 15 ##NUMBER_OK.
      WHEN 'ADDR_HOUSE_NUM1'.
        <LFS_FIELDCAT>-OUTPUTLEN = 5 ##NUMBER_OK.
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
      WHEN 'ADDR_REGION'.
      WHEN 'ADDR_TRANSPZONE'.
      WHEN 'ADDR_LANGU'.
      WHEN 'ADDR_TELNO1'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'ADDR_TELEXT1'.
        <LFS_FIELDCAT>-OUTPUTLEN = 5 ##NUMBER_OK.
      WHEN 'ADDR_TELNO2'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'ADDR_TELEXT2'.
        <LFS_FIELDCAT>-OUTPUTLEN = 5 ##NUMBER_OK.
      WHEN 'ADDR_MOBILE'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'ADDR_FAXNO'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'ADDR_FAXEXT'.
        <LFS_FIELDCAT>-OUTPUTLEN = 5 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL1'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL2'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL3'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL4'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL5'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_REMARK'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_ADEXT'.
      WHEN 'ADDRINT_ACTIVE'.
      WHEN 'ADDRINT_NATION'.
      WHEN 'ADDRINT_NAME_FIRST'.
        <LFS_FIELDCAT>-OUTPUTLEN = 25 ##NUMBER_OK.
      WHEN 'ADDRINT_NAME_LAST'.
        <LFS_FIELDCAT>-OUTPUTLEN = 25 ##NUMBER_OK.
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
      WHEN 'ADDRINT_SORT2'.
        <LFS_FIELDCAT>-OUTPUTLEN = 15 ##NUMBER_OK.
      WHEN 'ADDRINT_HOUSE_NUM1'.
        <LFS_FIELDCAT>-OUTPUTLEN = 5 ##NUMBER_OK.
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
      WHEN 'GENDR'.
      WHEN 'DOBDT'.
      WHEN 'FOUND_DAT'.
      WHEN 'LIQUID_DAT'.
      WHEN 'BPEXT'.
      WHEN 'NATPERS'.
      WHEN 'TAXTYPE'.
      WHEN 'TAXNUM'.
      WHEN 'VBUND'.
      WHEN 'BPKIND'.
      WHEN 'TXT_C003'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
*       Text-c02 : Business Hours
        LV_TEXT                  = TEXT-C02.
        <LFS_FIELDCAT>-REPTEXT   = LV_TEXT.
        <LFS_FIELDCAT>-COLTEXT   = LV_TEXT.
        <LFS_FIELDCAT>-SCRTEXT_L = LV_TEXT.
        <LFS_FIELDCAT>-SCRTEXT_M = LV_TEXT.
        <LFS_FIELDCAT>-SCRTEXT_S = LV_TEXT.
        <LFS_FIELDCAT>-OUTPUTLEN = 6.
      WHEN 'TXT_FLOG'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
*       Text-c03 : Note Log
        LV_TEXT                  = TEXT-C03.
        <LFS_FIELDCAT>-REPTEXT   = LV_TEXT.
        <LFS_FIELDCAT>-COLTEXT   = LV_TEXT.
        <LFS_FIELDCAT>-SCRTEXT_L = LV_TEXT.
        <LFS_FIELDCAT>-SCRTEXT_M = LV_TEXT.
        <LFS_FIELDCAT>-SCRTEXT_S = LV_TEXT.
        <LFS_FIELDCAT>-OUTPUTLEN = 6.
      WHEN 'BNK01_BKVID'.
      WHEN 'BNK01_BANKS'.
      WHEN 'BNK01_BANKL'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK01_BANKN'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK01_KOINH'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'BNK01_ACCNAME'.
      WHEN 'BNK02_BKVID'.
      WHEN 'BNK02_BANKS'.
      WHEN 'BNK02_BANKL'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK02_BANKN'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK02_KOINH'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'BNK02_ACCNAME'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'BNK03_BKVID'.
      WHEN 'BNK03_BANKS'.
      WHEN 'BNK03_BANKL'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK03_BANKN'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK03_KOINH'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'BNK03_ACCNAME'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'BNK04_BKVID'.
      WHEN 'BNK04_BANKS'.
      WHEN 'BNK04_BANKL'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK04_BANKN'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK04_KOINH'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'BNK04_ACCNAME'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK05_BKVID'.
      WHEN 'BNK05_BANKS'.
      WHEN 'BNK05_BANKL'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK05_BANKN'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK05_KOINH'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'BNK05_ACCNAME'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK06_BKVID'.
      WHEN 'BNK06_BANKS'.
      WHEN 'BNK06_BANKL'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK06_BANKN'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK06_KOINH'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'BNK06_ACCNAME'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'BNK07_BKVID'.
      WHEN 'BNK07_BANKS'.
      WHEN 'BNK07_BANKL'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK07_BANKN'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK07_KOINH'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'BNK07_ACCNAME'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'BNK08_BKVID'.
      WHEN 'BNK08_BANKS'.
      WHEN 'BNK08_BANKL'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK08_BANKN'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK08_KOINH'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'BNK08_ACCNAME'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK09_BKVID'.
      WHEN 'BNK09_BANKS'.
      WHEN 'BNK09_BANKL'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK09_BANKN'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK09_KOINH'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'BNK09_ACCNAME'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'BNK10_BKVID'.
      WHEN 'BNK10_BANKS'.
      WHEN 'BNK10_BANKL'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK10_BANKN'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'BNK10_KOINH'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'BNK10_ACCNAME'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'KUKLA'.
      WHEN 'BRAN1'.
      WHEN 'KATR1'.
      WHEN 'KDKG1'.
      WHEN 'KDKG2'.
      WHEN 'CLASS_VAL'.
      WHEN 'VIEW_COMP_ACTIVE'.
      WHEN 'VIEW_COMP_BUKRS'.
      WHEN 'VIEW_COMP_AKONT'.
      WHEN 'VIEW_COMP_ZUAWA'.
      WHEN 'VIEW_COMP_FDGRV'.
      WHEN 'VIEW_COMP_EKVBD'.
      WHEN 'VIEW_COMP_ZTERM'.
      WHEN 'VIEW_COMP_TOGRU'.
      WHEN 'VIEW_COMP_XZVER'.
      WHEN 'VIEW_COMP_KNRZB'.
      WHEN 'VIEW_COMP_MAHNA'.
      WHEN 'VIEW_COMP_BUSAB'.
      WHEN 'WT1_WITHT'.
      WHEN 'WT1_WITHCD'.
      WHEN 'WT1_AGENT'.
      WHEN 'WT1_WTSTCD'.
      WHEN 'WT1_AGTDF'.
      WHEN 'WT1_AGTDT'.
      WHEN 'WT2_WITHT'.
      WHEN 'WT2_WITHCD'.
      WHEN 'WT2_AGENT'.
      WHEN 'WT2_WTSTCD'.
      WHEN 'WT2_AGTDF'.
      WHEN 'WT2_AGTDT'.
      WHEN 'WT3_WITHT'.
      WHEN 'WT3_WITHCD'.
      WHEN 'WT3_AGENT'.
      WHEN 'WT3_WTSTCD'.
      WHEN 'WT3_AGTDF'.
      WHEN 'WT3_AGTDT'.
      WHEN 'VIEW_COMP_J_1TPBUPL'.
      WHEN 'VIEW_COMP_DESCRIPTION'.
        <LFS_FIELDCAT>-OUTPUTLEN = 15 ##NUMBER_OK.

      WHEN OTHERS.
        <LFS_FIELDCAT>-TECH = GC_TRUE.

    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_sort_result21
*&---------------------------------------------------------------------*
*& Maintain SORT data for Result ALV
*&---------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT21 CHANGING PT_SORT TYPE LVC_T_SORT.

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
*& Form f_process_data_tmp22
*&---------------------------------------------------------------------*
*& Processing data for Template Branch code(Customer)
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA_TMP22 CHANGING PT_RESULT TYPE GTTY_RESULT22
                                    PS_SUM     TYPE  GTY_SUM.

  DATA:
    LT_RAW  TYPE  GTTY_RAW,
    LT_DATA TYPE  GTTY_DATA22.

  DATA:
    LS_MESSG  TYPE  GTY_MESSG,
    LS_RESULT TYPE  GTY_RESULT22.


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
  PERFORM F_VALIDATE_FILE_TMP22  USING  LT_RAW
                               CHANGING LT_DATA.

* --------------------------------
* Step3: Upload data from file
* --------------------------------
* Upload file
  PERFORM F_UPLOAD_FILE_TMP22  USING  LT_DATA
                                      CB_TEST
                             CHANGING PT_RESULT
                                      PS_SUM.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_file_tmp22
*&---------------------------------------------------------------------*
*& Validate File data for Template Branch (Customer)
*&---------------------------------------------------------------------*
FORM F_VALIDATE_FILE_TMP22 USING PT_RAW TYPE GTTY_RAW
                          CHANGING PT_DATA    TYPE  GTTY_DATA22.

  DATA:
    LS_MAIN  TYPE  GTY_MAIN22,
    LS_MAINX TYPE  GTY_MAIN22X,
    LS_MESSG TYPE  GTY_MESSG.

  DATA:
    LV_ROWNO   TYPE  GTY_RESULT22-ROWNO.

  FIELD-SYMBOLS:
    <LFS_RAW>   TYPE  GTY_RAW.


* Initialize Output
  CLEAR: PT_DATA.

* Show Progress
* Text-p02 : Validating file data. . .
  MC_SHOW_PROGRESS 20 TEXT-P02.

*  IF p_begrow GE gs_config-beg_row.
*    lv_rowno = p_begrow - 1.
*  ELSE.
*    lv_rowno = gs_config-beg_row.
*  ENDIF.

  LV_ROWNO = P_BEGROW - 1.


  LOOP AT PT_RAW ASSIGNING <LFS_RAW>.

    LV_ROWNO = LV_ROWNO + 1.

*   Translate Raw line into variables
    PERFORM F_TRANSLATE_RAW22  USING  <LFS_RAW>
                            CHANGING LS_MAIN
                                     LS_MAINX
                                     LS_MESSG.

*   Validate and Append data into table
    PERFORM F_VALIDATE_AND_APPEND22  USING  LV_ROWNO
                                           LS_MAIN
                                           LS_MAINX
                                           LS_MESSG
                                  CHANGING PT_DATA.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_translate_raw22
*&---------------------------------------------------------------------*
*& Convert Raw to Data22
*&---------------------------------------------------------------------*
FORM F_TRANSLATE_RAW22 USING PS_RAW TYPE GTY_RAW
                     CHANGING PS_MAIN      TYPE  GTY_MAIN22
                              PS_MAINX     TYPE  GTY_MAIN22X
                              PS_MESSG     TYPE  GTY_MESSG.

  DATA:
    LT_SPLIT  TYPE  STANDARD TABLE OF STRING.

  DATA:
    LV_INDEX    TYPE  I,
    LV_FIELD    TYPE  GTY_FIELD_NAME,
    LV_STRING   TYPE  STRING,
    LV_MSGTX    TYPE  GTY_MESSG-MSGTX,
    LV_REQUIRED TYPE  CHAR1.


* Initialize Output
  CLEAR: PS_MAIN,
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

*   Get Target Field name to assign value
    PERFORM F_GET_TARGET_FIEL22  USING  LV_INDEX
                               CHANGING LV_FIELD.

    CASE LV_FIELD.

      WHEN 'RLTYP'.
        PERFORM F_VALIDATE_ROLE22  USING  LV_STRING
                                CHANGING PS_MAIN-RLTYP
                                         LV_MSGTX.

      WHEN 'PARTNER'.
        PERFORM F_VALIDATE_PARTNER  USING  LV_STRING
                                           GC_MODE_CHANGE
                                  CHANGING PS_MAIN-PARTNER
                                           PS_MAIN-PARTNER_GUID
                                           LV_MSGTX.

      WHEN 'J_1TPBUPL'.
        PS_MAIN-J_1TPBUPL  = LV_STRING.

      WHEN 'DESCRIPTION'.
        PS_MAIN-DESCRIPTION  = LV_STRING.
        PS_MAINX-DESCRIPTION = GC_TRUE.

      WHEN 'DEFAULT_BRANCH'.
        PS_MAIN-DEFAULT_BRANCH  = LV_STRING.
        PS_MAINX-DEFAULT_BRANCH = GC_TRUE.

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
*& Form f_get_target_fiel22
*&---------------------------------------------------------------------*
*& Get Target Field name based on column sequence
*&---------------------------------------------------------------------*
FORM F_GET_TARGET_FIEL22 USING PV_INDEX TYPE I
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
    LREF_DESCR = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( 'GTY_TEMPLATE22' ).
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
*& Form f_validate_role22
*&---------------------------------------------------------------------*
*& Validate BP Role for Template 22
*&---------------------------------------------------------------------*
FORM F_VALIDATE_ROLE22 USING PV_STRING TYPE STRING
                     CHANGING PV_RLTYP   TYPE  BUT100-RLTYP
                              PV_MSGTX   TYPE  CLIKE.

* Initialize Output
  CLEAR: PV_RLTYP,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

  IF PV_STRING NE GC_ROLE2_1 AND
     PV_STRING NE GC_ROLE2_2. "CH01+
*   Text-e01 : Invalid BP Role:
    CONCATENATE TEXT-E01 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Assign Output
  PV_RLTYP = PV_STRING.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_and_append22
*&---------------------------------------------------------------------*
*& Validate Whole Line data and collect into internal table for posting
*&---------------------------------------------------------------------*
FORM F_VALIDATE_AND_APPEND22 USING PV_ROWNO TYPE GTY_RESULT22-ROWNO
                                    PS_MAIN   TYPE  GTY_MAIN22
                                    PS_MAINX  TYPE  GTY_MAIN22X
                                    PS_MESSG  TYPE  GTY_MESSG
                           CHANGING PT_DATA   TYPE  GTTY_DATA22.

  DATA:
    LS_MAIN  TYPE  GTY_MAIN22,
    LS_MAINX TYPE  GTY_MAIN22X,
    LS_DATA  TYPE  GTY_DATA22,
    LS_MESSG TYPE  GTY_MESSG.


* Initial Data
  LS_MAIN  = PS_MAIN.
  LS_MAINX = PS_MAINX.

  DO 1 TIMES.

*  ---------------------------------
*   Validate Main data
*  ---------------------------------
    PERFORM F_VALIDATE_MAIN22 CHANGING LS_MAIN
                                       LS_MAINX
                                       LS_MESSG.
    IF LS_MESSG IS NOT INITIAL.
      EXIT.
    ENDIF.

  ENDDO.

* Generate new key
  CLEAR LS_DATA.
  LS_DATA-ROWNO = PV_ROWNO.
  LS_DATA-MAIN  = LS_MAIN.
  LS_DATA-MAINX = LS_MAINX.

* Collect Message
  IF PS_MESSG IS NOT INITIAL.
    APPEND PS_MESSG TO LS_DATA-MESSG.
  ELSEIF LS_MESSG IS NOT INITIAL.
    APPEND LS_MESSG TO LS_DATA-MESSG.
  ENDIF.

  INSERT LS_DATA INTO TABLE PT_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_main22
*&---------------------------------------------------------------------*
*& Validate Partner data
*&---------------------------------------------------------------------*
FORM F_VALIDATE_MAIN22 CHANGING PS_MAIN TYPE GTY_MAIN22
                                PS_MAINX TYPE  GTY_MAIN22X ##NEEDED
                                PS_MESSG TYPE  GTY_MESSG.

  DATA:
    LV_PARTNER TYPE  BUT100-PARTNER ##NEEDED.


* Initialize Output
  CLEAR: PS_MESSG.

  IF PS_MAIN-RLTYP IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e14 : Missing BP Role in the file.
    PS_MESSG-MSGTX = TEXT-E14.
    RETURN.
  ENDIF.

  IF PS_MAIN-PARTNER IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e17 : Missing BP Number in the file.
    PS_MESSG-MSGTX = TEXT-E17.
    RETURN.
  ENDIF.

* Check Roles Existing
  SELECT PARTNER
      UP TO 1 ROWS
    INTO LV_PARTNER
    FROM BUT100
   WHERE PARTNER EQ PS_MAIN-PARTNER
     AND RLTYP   EQ PS_MAIN-RLTYP
   ORDER BY PRIMARY KEY.                             "#EC CI_SEL_NESTED
  ENDSELECT.
  IF SY-SUBRC NE 0.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e80 : BP Role did not yet create for Partner.
    PS_MESSG-MSGTX = TEXT-E80.
    RETURN.
  ENDIF.

  IF PS_MAIN-J_1TPBUPL IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e81 : Missing Branch code in the file.
    PS_MESSG-MSGTX = TEXT-E81.
    RETURN.
  ENDIF.

* Check Customer Exist?
  PERFORM F_GET_CUSTOMER_FROM_PARTNER  USING  PS_MAIN-PARTNER
                                     CHANGING PS_MAIN-KUNNR.

* Get Branch data
  SELECT SINGLE ADDRNUMBER                           "#EC CI_SEL_NESTED
    INTO PS_MAIN-ADDR-ADDRNUMBER
    FROM FITHA_PBUPL_D
   WHERE KUNNR  EQ  PS_MAIN-KUNNR
     AND J_1TPBUPL EQ  PS_MAIN-J_1TPBUPL.
  IF SY-SUBRC EQ 0.
    PS_MAIN-BRANCH_EXIST = GC_TRUE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_upload_file_tmp22
*&---------------------------------------------------------------------*
*& Upload File Template 22
*&---------------------------------------------------------------------*
FORM F_UPLOAD_FILE_TMP22 USING PT_DATA TYPE GTTY_DATA22
                                 PV_TEST   TYPE  FLAG
                        CHANGING PT_RESULT TYPE  GTTY_RESULT22
                                 PS_SUM    TYPE  GTY_SUM.

  DATA:
    LT_MESSG TYPE  GTTY_MESSG.

  DATA:
    LS_DATA  TYPE  GTY_DATA22.

  DATA:
    LV_ERROR   TYPE  FLAG.


* Initialize Output
  CLEAR: PT_RESULT.
  CLEAR:   PS_SUM.

* Show Progress
* Text-p03 : Uploading file data. . .
  MC_SHOW_PROGRESS 45 TEXT-P03.

* Processing each record
  LOOP AT PT_DATA INTO LS_DATA.

*   Only non validation error
    IF LS_DATA-MESSG[] IS INITIAL.
      PERFORM F_MAINTAIN_BRANCH  USING  LS_DATA
                                        PV_TEST
                               CHANGING LT_MESSG
                                        LV_ERROR.
      LS_DATA-MESSG = LT_MESSG.
    ELSE.
      LV_ERROR = GC_TRUE.
    ENDIF.

    PS_SUM-TOTAL = PS_SUM-TOTAL + 1.

    IF LV_ERROR EQ GC_TRUE.
      PS_SUM-ERROR = PS_SUM-ERROR + 1.
    ELSE.
      PS_SUM-SUCCS = PS_SUM-SUCCS + 1.
    ENDIF.

*   Collect Result
    PERFORM F_COLLECT_RESULT22  USING  LS_DATA
                             CHANGING PT_RESULT.

    CLEAR: LT_MESSG[], LV_ERROR .
  ENDLOOP.

* Show Final Message for Processing completed
* Message: Processing completed.
  MESSAGE S582(1FA).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_maintain_branch
*&---------------------------------------------------------------------*
*& Maintain Branch Data
*&---------------------------------------------------------------------*
FORM F_MAINTAIN_BRANCH USING PS_DATA TYPE GTY_DATA22
                               PV_TEST   TYPE  FLAG       ##NEEDED
                      CHANGING PT_MESSG  TYPE  GTTY_MESSG ##NEEDED
                               PV_ERROR  TYPE  FLAG.

  DATA:
    LT_CODE     TYPE  CL_FINLOC_CVI_PERSIST_DATA=>TT_FITHA_PBUPL_D,
    LT_CODE_DEL TYPE  CL_FINLOC_CVI_PERSIST_DATA=>TT_FITHA_PBUPL_D,
    LT_DESC     TYPE  CL_FINLOC_CVI_PERSIST_DATA=>TT_FITHA_PBUPL_D_T,
    LT_DESC_DEL TYPE  CL_FINLOC_CVI_PERSIST_DATA=>TT_FITHA_PBUPL_D_T.

  DATA:
    LS_CODE  TYPE  FITHA_PBUPL_D,
    LS_DESC  TYPE  FITHA_PBUPL_D_T,
    LS_MESSG TYPE  GTY_MESSG.

  DATA:
    LV_ADDRNUMBER  TYPE  ADRC-ADDRNUMBER.

  FIELD-SYMBOLS:
    <LFS_CODE> TYPE  FITHA_PBUPL_D,
    <LFS_DESC> TYPE  FITHA_PBUPL_D_T.

* Get all Existing data
  SELECT *                                           "#EC CI_SEL_NESTED
    INTO TABLE LT_CODE
    FROM FITHA_PBUPL_D
   WHERE KUNNR  EQ   PS_DATA-MAIN-KUNNR.
  IF SY-SUBRC NE 0.
    CLEAR: LT_CODE.
  ENDIF.
  SORT LT_CODE BY KUNNR ASCENDING
                  J_1TPBUPL ASCENDING.

* Assign Code
  READ TABLE LT_CODE ASSIGNING <LFS_CODE>
                     WITH KEY KUNNR = PS_DATA-MAIN-KUNNR
                              J_1TPBUPL = PS_DATA-MAIN-J_1TPBUPL.
  IF SY-SUBRC NE 0.
    CLEAR LS_CODE.
    LS_CODE-KUNNR     = PS_DATA-MAIN-KUNNR.
    LS_CODE-J_1TPBUPL = PS_DATA-MAIN-J_1TPBUPL.
    LS_CODE-DEFAULT_BRANCH = PS_DATA-MAIN-DEFAULT_BRANCH. "CH01+
    INSERT LS_CODE INTO TABLE LT_CODE ASSIGNING <LFS_CODE>.
* BOI - CH01
  ELSE.
    <LFS_CODE>-DEFAULT_BRANCH = PS_DATA-MAIN-DEFAULT_BRANCH. "CH01+
* EOI - CH01
  ENDIF.
  <LFS_CODE>-ADDRNUMBER = LV_ADDRNUMBER.

  CALL FUNCTION 'LCTH_BUPA_UPDATE_BRANCH_CODE'
    EXPORTING
      IT_FITHA_PBUPL_D        = LT_CODE
      IT_FITHA_PBUPL_D_DELETE = LT_CODE_DEL.

* Assign Desc
  IF PS_DATA-MAINX-DESCRIPTION EQ GC_TRUE OR
     PS_DATA-MAIN-BRANCH_EXIST IS INITIAL.

*   Get all Existing data
    SELECT *                                         "#EC CI_SEL_NESTED
      INTO TABLE LT_DESC
      FROM FITHA_PBUPL_D_T
     WHERE KUNNR  EQ   PS_DATA-MAIN-KUNNR.
    IF SY-SUBRC NE 0.
      CLEAR: LT_DESC.
    ENDIF.
    SORT LT_DESC BY SPRAS ASCENDING
                    KUNNR ASCENDING
                    J_1TPBUPL ASCENDING.

    READ TABLE LT_DESC ASSIGNING <LFS_DESC>
                       WITH KEY SPRAS = SY-LANGU
                                KUNNR = PS_DATA-MAIN-KUNNR
                                J_1TPBUPL   = PS_DATA-MAIN-J_1TPBUPL
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_DESC.
      LS_DESC-SPRAS       = SY-LANGU.
      LS_DESC-KUNNR       = PS_DATA-MAIN-KUNNR.
      LS_DESC-J_1TPBUPL   = PS_DATA-MAIN-J_1TPBUPL.
      INSERT LS_DESC INTO TABLE LT_DESC ASSIGNING <LFS_DESC>.
    ENDIF.
    <LFS_DESC>-DESCRIPTION = PS_DATA-MAIN-DESCRIPTION.

    CALL FUNCTION 'LCTH_BUPA_UPDATE_BRANCH_DESC'
      EXPORTING
        IT_FITHA_PBUPL_D_T        = LT_DESC
        IT_FITHA_PBUPL_D_T_DELETE = LT_DESC_DEL.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT = GC_TRUE.

  CLEAR LS_MESSG.
  LS_MESSG-MSGTY = 'S'.
  LS_MESSG-MSGID = 'ZTEC'.
  LS_MESSG-MSGNO = '000'.
* Text-i03: Business Partner data has been updated successfully.
  LS_MESSG-MSGTX = TEXT-I03.
  INSERT LS_MESSG INTO TABLE PT_MESSG.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_collect_result22
*&---------------------------------------------------------------------*
*& Collect Result from Processing
*&---------------------------------------------------------------------*
FORM F_COLLECT_RESULT22 USING PS_DATA TYPE GTY_DATA22
                      CHANGING PT_RESULT TYPE  GTTY_RESULT22.

  DATA:
    LS_RESULT  TYPE  GTY_RESULT22.

  FIELD-SYMBOLS:
    <LFS_MESSG> TYPE  GTY_MESSG.


  CLEAR: LS_RESULT.

  LS_RESULT-ROWNO       =  PS_DATA-ROWNO.
  LS_RESULT-RLTYP       =  PS_DATA-MAIN-RLTYP.
  LS_RESULT-PARTNER     =  PS_DATA-MAIN-PARTNER.
  LS_RESULT-J_1TPBUPL   =  PS_DATA-MAIN-J_1TPBUPL.
  LS_RESULT-DESCRIPTION =  PS_DATA-MAIN-DESCRIPTION.
  LS_RESULT-DEFAULT_BRANCH = PS_DATA-MAIN-DEFAULT_BRANCH.

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
*& Form f_display_result22
*&---------------------------------------------------------------------*
*& Display Result
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT22 USING PT_RESULT TYPE GTTY_RESULT22.

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
  PERFORM F_ALV_LAYOUT22 CHANGING GS_LAYOUT_1
                                  GS_VARIANT_1
                                  GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_22.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_22.
  ASSIGN PT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2215424]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT22 CHANGING GT_FIELDCAT_1.
* Sort data
  PERFORM F_ALV_SORT_RESULT22 CHANGING GT_SORT_1.
* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_build_fieldcat22
*&---------------------------------------------------------------------*
*& ALV Field Catalog for Report22
*&---------------------------------------------------------------------*
*&      <-- GT_FIELDCAT_1
*&---------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT22 CHANGING PT_FIELDCAT TYPE LVC_T_FCAT.

  DATA:
    LV_TEXT         TYPE  TEXT50.

  FIELD-SYMBOLS:
    <LFS_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_22
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
        <LFS_FIELDCAT>-OUTPUTLEN = 6.
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
      WHEN 'PARTNER'.
        IF CB_TEST IS INITIAL.
          <LFS_FIELDCAT>-HOTSPOT   = GC_TRUE.
        ENDIF.
      WHEN 'J_1TPBUPL'.
      WHEN 'DESCRIPTION'.
        <LFS_FIELDCAT>-OUTPUTLEN = 15 ##NUMBER_OK.
      WHEN 'DEFAULT_BRANCH' .

      WHEN OTHERS.
        <LFS_FIELDCAT>-TECH = GC_TRUE.

    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_layout22
*&---------------------------------------------------------------------*
*& ALV Layout for Report
*&---------------------------------------------------------------------*
FORM F_ALV_LAYOUT22 CHANGING PS_LAYOUT TYPE LVC_S_LAYO
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
*& Form f_alv_sort_result22
*&---------------------------------------------------------------------*
*& Maintain SORT data for Result ALV
*&---------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT22 CHANGING PT_SORT TYPE LVC_T_SORT.

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
*& Form f_maintain_extend_comp21
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_MAINTAIN_EXTEND_COMP21 USING PS_DATA TYPE GTY_DATA21
                                     PV_TEST  TYPE  FLAG
                            CHANGING PV_PARTNER TYPE GTY_DATA11-KEY-PARTNER ##NEEDED
                                     PT_MESSG TYPE  GTTY_MESSG
                                     PV_ERROR TYPE  FLAG        ##NEEDED.
  DATA: LS_RET2   TYPE BAPIRET2 ##NEEDED,
*        ls_logh   TYPE zret_inflogh,
*        lt_loge   TYPE TABLE OF zret_infloge WITH HEADER LINE,
        LT_RETURN TYPE  BAPIRETM ##NEEDED.

  DATA: LV_KUNNR       TYPE KUNNR,
        LS_CENTRAL     TYPE CMDS_EI_CMD_CENTRAL ##NEEDED,
        LS_COMPANY     TYPE CMDS_EI_COMPANY,
        LT_COMPANY     TYPE CMDS_EI_COMPANY_T,
        LS_DUNNING     TYPE CMDS_EI_DUNNING ##NEEDED,
        LT_DUNNING     TYPE CMDS_EI_DUNNING_T ##NEEDED,
        LS_WTAX        TYPE CMDS_EI_WTAX_TYPE ##NEEDED,
        LT_WTAX        TYPE CMDS_EI_WTAX_TYPE_T ##NEEDED,

        LS_CUSTOMER    TYPE CMDS_EI_EXTERN,
        LS_MASTER_DATA TYPE CMDS_EI_MAIN,
        LS_CORRECT     TYPE CMDS_EI_MAIN ##NEEDED,
        LS_MES_CORRECT TYPE CVIS_MESSAGE ##NEEDED,
        LS_DEFECTIVE   TYPE CMDS_EI_MAIN ##NEEDED,
        LS_MES_ERROR   TYPE CVIS_MESSAGE ##NEEDED,

        LV_VALIDFROM   LIKE SY-DATUM ##NEEDED,
        LV_VALIDTO     LIKE SY-DATUM ##NEEDED.

  DATA: LS_LOG_EXTEND TYPE GTYP_LOG_EXTEND,
        LS_LOG_COMP   TYPE GTYP_LOG_COMP.

  DATA  LS_MESSG TYPE GTY_MESSG .
  FIELD-SYMBOLS:  <LFS_RETURN>  TYPE  BAPIRET2 ##NEEDED.

  LV_KUNNR = PS_DATA-KEY-PARTNER.

  CMD_EI_API=>INITIALIZE( ).

* BOD - CH01
*  IF PS_DATA-MAIN-VIEW_COMP-AKONT IS INITIAL.
*    LS_COMPANY-DATA-AKONT = '1102111000'.  "Reconciliation Acct
*  ENDIF.
* EOD - CH01

  LS_COMPANY-DATA-AKONT = PS_DATA-MAIN-VIEW_COMP-AKONT .

  LS_COMPANY-DATA-ZUAWA     = PS_DATA-MAIN-VIEW_COMP-ZUAWA.

* BOD - CH01
*  IF LS_COMPANY-DATA-ZUAWA IS INITIAL .
*    LS_COMPANY-DATA-ZUAWA = '035'.               "Sort Key
*  ENDIF.
* EOD - CH01

  LS_COMPANY-DATA-ZTERM     = PS_DATA-MAIN-VIEW_COMP-ZTERM. "Terms of Payment

*    ls_company-data-xausz     = ps_data-main-view_comp-xausz.  "Account Statement
  LS_COMPANY-DATA-FDGRV      = PS_DATA-MAIN-VIEW_COMP-FDGRV. "Planning Group (Cash Management Group)
*    ls_company-data-vzskz      = ps_data-main-view_comp-vzskz. "Interest Indicator

*    ls_company-data-mgrup     = '02'.              "Dunning notice grouping
  LS_COMPANY-DATA-ALTKN     = PS_DATA-MAIN-VIEW_COMP-ALTKN.
  LS_COMPANY-DATA-XZVER     = PS_DATA-MAIN-VIEW_COMP-XZVER.
  LS_COMPANY-DATA-XVERR     = PS_DATA-MAIN-VIEW_COMP-XVERR.
  LS_COMPANY-DATA-KNRZB     = PS_DATA-MAIN-VIEW_COMP-KNRZB.

* BOI - CH01
  LS_COMPANY-DATA-BUSAB     = PS_DATA-MAIN-VIEW_COMP-BUSAB.
  LS_COMPANY-DATA-KVERM     = PS_DATA-MAIN-VIEW_COMP-KVERM.
  LS_COMPANY-DATA-TLFXS     = PS_DATA-MAIN-VIEW_COMP-TLFXS.
  LS_COMPANY-DATA-INTAD     = PS_DATA-MAIN-VIEW_COMP-INTAD.
  LS_COMPANY-DATA-XAUSZ     = PS_DATA-MAIN-VIEW_COMP-XAUSZ.
  LS_COMPANY-DATA-SPERR     = PS_DATA-MAIN-VIEW_COMP-SPERR.

* Dunning Procedure
  LS_DUNNING-DATA_KEY-MABER = ''.
  LS_DUNNING-DATA-MAHNA     = PS_DATA-MAIN-VIEW_COMP-MAHNA.
  CASE GV_MODE.
    WHEN GC_MODE_CREATE.
      LS_DUNNING-TASK           = 'I'.
    WHEN GC_MODE_CHANGE.
      LS_DUNNING-TASK           = 'U'.
  ENDCASE.

  APPEND LS_DUNNING TO LT_DUNNING.
  LS_COMPANY-DUNNING-DUNNING = LT_DUNNING[].
* EOI - CH01

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
  IF PS_DATA-MAIN-VIEW_COMP-WTAX01-WITHT IS NOT INITIAL.
    LS_WTAX-DATA-WT_WITHCD = PS_DATA-MAIN-VIEW_COMP-WTAX01-WITHCD.
    LS_WTAX-DATA-WT_AGENT  = PS_DATA-MAIN-VIEW_COMP-WTAX01-AGENT.
    LS_WTAX-DATA-WT_AGTDF  = PS_DATA-MAIN-VIEW_COMP-WTAX01-AGTDF.
    LS_WTAX-DATA-WT_AGTDT  = PS_DATA-MAIN-VIEW_COMP-WTAX01-AGTDT.
    LS_WTAX-DATA_KEY-WITHT = PS_DATA-MAIN-VIEW_COMP-WTAX01-WITHT.
    LS_WTAX-DATA-QSREC = PS_DATA-MAIN-VIEW_COMP-WTAX01-QSREC.    "CH01+
    LS_WTAX-TASK           = 'I'.
    APPEND LS_WTAX TO LT_WTAX.
  ENDIF.

  IF PS_DATA-MAIN-VIEW_COMP-WTAX02-WITHT IS NOT INITIAL.
    LS_WTAX-DATA-WT_WITHCD = PS_DATA-MAIN-VIEW_COMP-WTAX02-WITHCD.
    LS_WTAX-DATA-WT_AGENT  = PS_DATA-MAIN-VIEW_COMP-WTAX02-AGENT.
    LS_WTAX-DATA-WT_AGTDF  = PS_DATA-MAIN-VIEW_COMP-WTAX02-AGTDF.
    LS_WTAX-DATA-WT_AGTDT  = PS_DATA-MAIN-VIEW_COMP-WTAX02-AGTDT.
    LS_WTAX-DATA_KEY-WITHT = PS_DATA-MAIN-VIEW_COMP-WTAX02-WITHT.
    LS_WTAX-DATA-QSREC = PS_DATA-MAIN-VIEW_COMP-WTAX02-QSREC.    "CH01+
    LS_WTAX-TASK           = 'I'.
    APPEND LS_WTAX TO LT_WTAX.
  ENDIF.

  IF PS_DATA-MAIN-VIEW_COMP-WTAX03-WITHT IS NOT INITIAL.
    LS_WTAX-DATA-WT_WITHCD = PS_DATA-MAIN-VIEW_COMP-WTAX03-WITHCD.
    LS_WTAX-DATA-WT_AGENT  = PS_DATA-MAIN-VIEW_COMP-WTAX03-AGENT.
    LS_WTAX-DATA-WT_AGTDF  = PS_DATA-MAIN-VIEW_COMP-WTAX03-AGTDF.
    LS_WTAX-DATA-WT_AGTDT  = PS_DATA-MAIN-VIEW_COMP-WTAX03-AGTDT.
    LS_WTAX-DATA_KEY-WITHT = PS_DATA-MAIN-VIEW_COMP-WTAX03-WITHT.
    LS_WTAX-TASK           = 'I'.
    APPEND LS_WTAX TO LT_WTAX.
  ENDIF.

  IF PS_DATA-MAIN-VIEW_COMP-WTAX04-WITHT IS NOT INITIAL.
    LS_WTAX-DATA-WT_WITHCD = PS_DATA-MAIN-VIEW_COMP-WTAX04-WITHCD.
    LS_WTAX-DATA-WT_AGENT  = PS_DATA-MAIN-VIEW_COMP-WTAX04-AGENT.
    LS_WTAX-DATA-WT_AGTDF  = PS_DATA-MAIN-VIEW_COMP-WTAX04-AGTDF.
    LS_WTAX-DATA-WT_AGTDT  = PS_DATA-MAIN-VIEW_COMP-WTAX04-AGTDT.
    LS_WTAX-DATA_KEY-WITHT = PS_DATA-MAIN-VIEW_COMP-WTAX04-WITHT.
    LS_WTAX-TASK           = 'I'.
    APPEND LS_WTAX TO LT_WTAX.
  ENDIF.


  LS_COMPANY-WTAX_TYPE-WTAX_TYPE = LT_WTAX[].

  LS_COMPANY-DATA_KEY-BUKRS = PS_DATA-MAIN-VIEW_COMP-BUKRS.
  LS_COMPANY-TASK           = 'I'.
  APPEND LS_COMPANY TO LT_COMPANY.

  LS_CUSTOMER-COMPANY_DATA-COMPANY = LT_COMPANY[].

  LS_CUSTOMER-HEADER-OBJECT_TASK = 'U'.  "Represents update
  LS_CUSTOMER-HEADER-OBJECT_INSTANCE-KUNNR = LV_KUNNR.
  APPEND LS_CUSTOMER TO LS_MASTER_DATA-CUSTOMERS.

  CMD_EI_API=>LOCK( IV_KUNNR = LV_KUNNR ).

  CALL METHOD CMD_EI_API=>MAINTAIN_BAPI
    EXPORTING
      IV_TEST_RUN              = PV_TEST
      IV_COLLECT_MESSAGES      = 'X'
      IS_MASTER_DATA           = LS_MASTER_DATA
    IMPORTING
      ES_MASTER_DATA_CORRECT   = LS_CORRECT
      ES_MESSAGE_CORRECT       = LS_MES_CORRECT
      ES_MASTER_DATA_DEFECTIVE = LS_DEFECTIVE
      ES_MESSAGE_DEFECTIVE     = LS_MES_ERROR.

  IF LS_MES_ERROR IS INITIAL .

    IF PV_TEST IS INITIAL .
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
      LS_LOG_EXTEND-BUKRS   = PS_DATA-MAIN-VIEW_COMP-BUKRS.
      APPEND LS_LOG_EXTEND TO GT_LOG_EXTEND .

      CLEAR LS_LOG_COMP .
      LS_LOG_COMP-PARTNER = PS_DATA-KEY-PARTNER.
      LS_LOG_COMP-BUKRS   = PS_DATA-MAIN-VIEW_COMP-BUKRS.
      LS_LOG_COMP-WITHT1  = PS_DATA-MAIN-VIEW_COMP-WTAX01-WITHT.
      LS_LOG_COMP-WITHT2  = PS_DATA-MAIN-VIEW_COMP-WTAX02-WITHT.
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

*    PERFORM f_check_bapi_return  USING  lt_return
*                                        pv_test
*                               CHANGING pv_error
*                                        pt_messg.
  ELSE.

*    LOOP AT ls_mes_error-messages ASSIGNING <lfs_return>
*                      WHERE type EQ 'E' OR
*                            type EQ 'A'.
*      ls_messg-msgty = 'E'.
*      ls_messg-msgid = <lfs_return>-id.
*      ls_messg-msgno = <lfs_return>-number.
*      ls_messg-msgtx = <lfs_return>-message.
*      APPEND ls_messg TO pt_messg .
*      EXIT.
*    ENDLOOP.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    READ TABLE LS_MES_ERROR-MESSAGES INTO DATA(LS_MESSAGE) INDEX 1 .
    "Customer xxxx has not been created (skip)
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


  CMD_EI_API=>UNLOCK( IV_KUNNR = LV_KUNNR ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_assign_bapi_extend_company
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*FORM F_ASSIGN_BAPI_EXTEND_COMPANY USING PV_PARTNER TYPE GTY_KEY21-PARTNER
*                                    PS_VIEW_CUST  TYPE GTY_VIEW_CUST
*                                    PS_VIEW_CUSTX TYPE GTY_VIEW_CUSTX
*                                    PS_VIEW_COMP  TYPE GTY_VIEW_COMP
*                                    PS_VIEW_COMPX TYPE GTY_VIEW_COMPX
*                           CHANGING PS_BAPI_CUST  TYPE CMDS_EI_EXTERN.
*
*  DATA:
*    LS_COMPANY TYPE  CMDS_EI_COMPANY,
*    LS_DUNNING TYPE  CMDS_EI_DUNNING.
*
*
** -----------------
** Central Data
** -----------------
*  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATA-KUKLA  = PS_VIEW_CUST-KUKLA.
*  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATAX-KUKLA = PS_VIEW_CUSTX-KUKLA.
*
*  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATA-BRAN1  = PS_VIEW_CUST-BRAN1.
*  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATAX-BRAN1 = PS_VIEW_CUSTX-BRAN1.
*
*  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATA-KATR1  = PS_VIEW_CUST-KATR1.
*  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATAX-KATR1 = PS_VIEW_CUSTX-KATR1.
*
*  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATA-KDKG1  = PS_VIEW_CUST-KDKG1.
*  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATAX-KDKG1 = PS_VIEW_CUSTX-KDKG1.
*
*  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATA-KDKG2  = PS_VIEW_CUST-KDKG2.
*  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATAX-KDKG2 = PS_VIEW_CUSTX-KDKG2.
*
*  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATA-VBUND  = PS_VIEW_CUST-VBUND.
*  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATAX-VBUND = PS_VIEW_CUSTX-VBUND.
*
*  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATA-LIFNR  = PS_VIEW_CUST-LIFNR.
*  PS_BAPI_CUST-CENTRAL_DATA-CENTRAL-DATAX-LIFNR = PS_VIEW_CUSTX-LIFNR.
*
** -----------------
** Classification
** -----------------
**  PERFORM f_assign_bapi_cust_class  USING  pv_partner
**                                           ps_view_cust-class_val
**                                           ps_view_custx-class_val
**                                  CHANGING ps_bapi_cust-central_data-classification.
*
** -----------------
** Company Data
** -----------------
**  IF ps_view_comp-active EQ gc_true.
*  CLEAR LS_COMPANY.
*
**    IF ps_view_comp-exist IS INITIAL.
**      ls_company-task = 'I'.
**    ELSE.
*  LS_COMPANY-TASK = 'U'.
**    ENDIF.
*
*  LS_COMPANY-DATA_KEY-BUKRS = PS_VIEW_COMP-BUKRS.
*
*  LS_COMPANY-DATA-AKONT  = PS_VIEW_COMP-AKONT.
*  LS_COMPANY-DATAX-AKONT = PS_VIEW_COMPX-AKONT.
*
*  LS_COMPANY-DATA-ZUAWA  = PS_VIEW_COMP-ZUAWA.
*  LS_COMPANY-DATAX-ZUAWA = PS_VIEW_COMPX-ZUAWA.
*
*  LS_COMPANY-DATA-ALTKN  = PS_VIEW_COMP-ALTKN.
*  LS_COMPANY-DATAX-ALTKN = PS_VIEW_COMPX-ALTKN.
*
*  LS_COMPANY-DATA-ZTERM  = PS_VIEW_COMP-ZTERM.
*  LS_COMPANY-DATAX-ZTERM = PS_VIEW_COMPX-ZTERM.
*
*  LS_COMPANY-DATA-XZVER  = PS_VIEW_COMP-XZVER.
*  LS_COMPANY-DATAX-XZVER = PS_VIEW_COMPX-XZVER.
*
*  LS_COMPANY-DATA-XVERR  = PS_VIEW_COMP-XZVER.
*  LS_COMPANY-DATAX-XVERR = PS_VIEW_COMPX-XZVER.
*
*  LS_COMPANY-DATA-KNRZB  = PS_VIEW_COMP-KNRZB.
*  LS_COMPANY-DATAX-KNRZB = PS_VIEW_COMPX-KNRZB.
*
*
*  PERFORM F_ASSIGN_BAPI_COMP_WTAX  USING  PS_VIEW_COMP-WTAX01
*                                          PS_VIEW_COMPX-WTAX01
*                                 CHANGING LS_COMPANY-WTAX_TYPE-WTAX_TYPE.
*  PERFORM F_ASSIGN_BAPI_COMP_WTAX  USING  PS_VIEW_COMP-WTAX02
*                                          PS_VIEW_COMPX-WTAX02
*                                 CHANGING LS_COMPANY-WTAX_TYPE-WTAX_TYPE.
*  PERFORM F_ASSIGN_BAPI_COMP_WTAX  USING  PS_VIEW_COMP-WTAX03
*                                          PS_VIEW_COMPX-WTAX03
*                                 CHANGING LS_COMPANY-WTAX_TYPE-WTAX_TYPE.
*  PERFORM F_ASSIGN_BAPI_COMP_WTAX  USING  PS_VIEW_COMP-WTAX04
*                                          PS_VIEW_COMPX-WTAX04
*                                 CHANGING LS_COMPANY-WTAX_TYPE-WTAX_TYPE.
*
*  INSERT LS_COMPANY INTO TABLE PS_BAPI_CUST-COMPANY_DATA-COMPANY.
*
**  ENDIF.
*
*ENDFORM.
* BOI - CH01
*---------------------------------------------------------------------*
* Form F_VALIDATE_REQUIRED_FLD_MAIN21
*---------------------------------------------------------------------*
* Validate required field option create/change BP Customer
*---------------------------------------------------------------------*
FORM F_VALIDATE_REQUIRED_FLD_MAIN21  USING PS_KEY TYPE GTY_KEY21
                                  CHANGING PS_MAIN  TYPE  GTY_MAIN21    ##NEEDED
                                           PS_MAINX TYPE  GTY_MAIN21X   ##NEEDED
                                           PS_MESSG TYPE  GTY_MESSG.

* Initialize Output
  CLEAR: PS_MESSG.

* Skip checking required field for BP group Z070 (Contact Person)
  IF PS_KEY-BU_GROUP EQ 'Z070'.
    RETURN.
  ENDIF.

  IF PS_KEY-PARTNER_EXIST IS INITIAL.
    IF PS_MAIN-VIEW_COMP-ALTKN IS INITIAL.
      PS_MESSG-MSGTY = 'E'.
      PS_MESSG-MSGID = 'ZSDSCA01'.
      PS_MESSG-MSGNO = '000'.
*   Text-e91 : Missing Previous account no. in the file.
      PS_MESSG-MSGTX = TEXT-E91.
      RETURN.
    ENDIF.
  ENDIF.

  IF PS_KEY-BU_GROUP IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZSDSCA01'.
    PS_MESSG-MSGNO = '000'.
*   Text-e92 : Missing Business Partner Group in the file.
    PS_MESSG-MSGTX = TEXT-E92.
    RETURN.
  ENDIF.

  IF PS_KEY-RLTYP IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZSDSCA01'.
    PS_MESSG-MSGNO = '000'.
*   Text-e93 : Missing BP Role in the file.
    PS_MESSG-MSGTX = TEXT-E93.
    RETURN.
  ENDIF.

*  IF PS_MAIN-BPKIND IS INITIAL.
*    PS_MESSG-MSGTY = 'E'.
*    PS_MESSG-MSGID = 'ZSDSCA01'.
*    PS_MESSG-MSGNO = '000'.
**   Text-e94 : Missing Business Partner Type in the file.
*    PS_MESSG-MSGTX = TEXT-E94.
*    RETURN.
*  ENDIF.

* -------------------------
* Address
* -------------------------
*  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
*       PS_MAINX-ADDR-LOCATION EQ GC_TRUE ) AND
*      PS_MAIN-ADDR-LOCATION IS INITIAL.
*    PS_MESSG-MSGTY = 'E'.
*    PS_MESSG-MSGID = 'ZSDSCA01'.
*    PS_MESSG-MSGNO = '000'.
**   Text-e95 : Missing Address 5 in the file.
*    PS_MESSG-MSGTX = TEXT-E95.
*    RETURN.
*  ENDIF.
*
*  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
*       PS_MAINX-ADDR-CITY2 EQ GC_TRUE ) AND
*      PS_MAIN-ADDR-CITY2 IS INITIAL.
*    PS_MESSG-MSGTY = 'E'.
*    PS_MESSG-MSGID = 'ZSDSCA01'.
*    PS_MESSG-MSGNO = '000'.
**   Text-e96 : Missing District in the file.
*    PS_MESSG-MSGTX = TEXT-E96.
*    RETURN.
*  ENDIF.
*
*  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
*       PS_MAINX-ADDR-CITY1 EQ GC_TRUE ) AND
*      PS_MAIN-ADDR-CITY1 IS INITIAL.
*    PS_MESSG-MSGTY = 'E'.
*    PS_MESSG-MSGID = 'ZSDSCA01'.
*    PS_MESSG-MSGNO = '000'.
**   Text-e97 : Missing City in the file.
*    PS_MESSG-MSGTX = TEXT-E97.
*    RETURN.
*  ENDIF.

  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
       PS_MAINX-ADDR-POST_CODE1 EQ GC_TRUE ) AND
      PS_MAIN-ADDR-POST_CODE1 IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZSDSCA01'.
    PS_MESSG-MSGNO = '000'.
*   Text-e98 : Missing Postal code in the file.
    PS_MESSG-MSGTX = TEXT-E98.
    RETURN.
  ENDIF.

  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
       PS_MAINX-ADDR-COUNTRY EQ GC_TRUE ) AND
      PS_MAIN-ADDR-COUNTRY IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZSDSCA01'.
    PS_MESSG-MSGNO = '000'.
*   Text-e99 : Missing Country Code in the file.
    PS_MESSG-MSGTX = TEXT-E99.
    RETURN.
  ENDIF.

  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
       PS_MAINX-ADDR-LANGU EQ GC_TRUE ) AND
      PS_MAIN-ADDR-LANGU IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZSDSCA01'.
    PS_MESSG-MSGNO = '000'.
*   Text-001 : Missing Commu. Language in the file.
    PS_MESSG-MSGTX = TEXT-001.
    RETURN.
  ENDIF.

  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
       PS_MAINX-ADDR-TRANSPZONE EQ GC_TRUE ) AND
      PS_MAIN-ADDR-TRANSPZONE IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZSDSCA01'.
    PS_MESSG-MSGNO = '000'.
*   Text-006 : Missing Transportation Zone in the file.
    PS_MESSG-MSGTX = TEXT-006.
    RETURN.
  ENDIF.


* -------------------------
* Address Int. Version
* -------------------------
*  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
*       PS_MAINX-ADDR_INT-LOCATION EQ GC_TRUE ) AND
*      PS_MAIN-ADDR_INT-LOCATION IS INITIAL.
*    PS_MESSG-MSGTY = 'E'.
*    PS_MESSG-MSGID = 'ZSDSCA01'.
*    PS_MESSG-MSGNO = '000'.
**   Text-e95 : Missing Address 5 in the file.
*    PS_MESSG-MSGTX = TEXT-E95.
*    RETURN.
*  ENDIF.
*
*  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
*       PS_MAINX-ADDR_INT-CITY2 EQ GC_TRUE ) AND
*      PS_MAIN-ADDR_INT-CITY2 IS INITIAL.
*    PS_MESSG-MSGTY = 'E'.
*    PS_MESSG-MSGID = 'ZSDSCA01'.
*    PS_MESSG-MSGNO = '000'.
**   Text-e96 : Missing District in the file.
*    PS_MESSG-MSGTX = TEXT-E96.
*    RETURN.
*  ENDIF.
*
*  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
*       PS_MAINX-ADDR_INT-CITY1 EQ GC_TRUE ) AND
*      PS_MAIN-ADDR_INT-CITY1 IS INITIAL.
*    PS_MESSG-MSGTY = 'E'.
*    PS_MESSG-MSGID = 'ZSDSCA01'.
*    PS_MESSG-MSGNO = '000'.
**   Text-e97 : Missing City in the file.
*    PS_MESSG-MSGTX = TEXT-E97.
*    RETURN.
*  ENDIF.

  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
       PS_MAINX-ADDR_INT-POST_CODE1 EQ GC_TRUE ) AND
      PS_MAIN-ADDR_INT-POST_CODE1 IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZSDSCA01'.
    PS_MESSG-MSGNO = '000'.
*   Text-e98 : Missing Postal code in the file.
    PS_MESSG-MSGTX = TEXT-E98.
    RETURN.
  ENDIF.

  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
       PS_MAINX-ADDR_INT-COUNTRY EQ GC_TRUE ) AND
      PS_MAIN-ADDR_INT-COUNTRY IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZSDSCA01'.
    PS_MESSG-MSGNO = '000'.
*   Text-e99 : Missing Country Code in the file.
    PS_MESSG-MSGTX = TEXT-E99.
    RETURN.
  ENDIF.

  IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
       PS_MAINX-ADDR_INT-LANGU EQ GC_TRUE ) AND
      PS_MAIN-ADDR_INT-LANGU IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZSDSCA01'.
    PS_MESSG-MSGNO = '000'.
*   Text-001 : Missing Commu. Language in the file.
    PS_MESSG-MSGTX = TEXT-001.
    RETURN.
  ENDIF.

* -------------------------
  IF PS_KEY-BU_GROUP NE 'Z040'.
    IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
         PS_MAINX-BPEXT EQ GC_TRUE ) AND
        PS_MAIN-BPEXT IS INITIAL.

      PS_MESSG-MSGTY = 'E'.
      PS_MESSG-MSGID = 'ZSDSCA01'.
      PS_MESSG-MSGNO = '000'.
*   Text-003 : Missing BP Number in External System in the file.
      PS_MESSG-MSGTX = TEXT-003.
      RETURN.
    ENDIF.

    IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
         PS_MAINX-TAX-TAXTYPE EQ GC_TRUE ) AND
        PS_MAIN-TAX-TAXTYPE IS INITIAL.
      PS_MESSG-MSGTY = 'E'.
      PS_MESSG-MSGID = 'ZSDSCA01'.
      PS_MESSG-MSGNO = '000'.
*   Text-004 : Missing Tax Category in the file.
      PS_MESSG-MSGTX = TEXT-004.
      RETURN.
    ENDIF.

* BP Group = Z040, no need to check require field for
* - TaxID, Branch code, Planning Group, Dunning Procedure,
*   Account Clerk, Account statement
    IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
         PS_MAINX-TAX-TAXNUM EQ GC_TRUE ) AND
        PS_MAIN-TAX-TAXNUM IS INITIAL.
      PS_MESSG-MSGTY = 'E'.
      PS_MESSG-MSGID = 'ZSDSCA01'.
      PS_MESSG-MSGNO = '000'.
*   Text-005 : Missing Tax Number in the file.
      PS_MESSG-MSGTX = TEXT-005.
      RETURN.
    ENDIF.

    IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
         PS_MAINX-BRANCH-J_1TPBUPL EQ GC_TRUE ) AND
        PS_MAIN-BRANCH-J_1TPBUPL IS INITIAL.
      PS_MESSG-MSGTY = 'E'.
      PS_MESSG-MSGID = 'ZSDSCA01'.
      PS_MESSG-MSGNO = '000'.
*   Text-007 : Missing Branch Code in the file.
      PS_MESSG-MSGTX = TEXT-007.
      RETURN.
    ENDIF.

    IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
         PS_MAINX-VIEW_COMP-FDGRV EQ GC_TRUE ) AND
        PS_MAIN-VIEW_COMP-FDGRV IS INITIAL.
      PS_MESSG-MSGTY = 'E'.
      PS_MESSG-MSGID = 'ZSDSCA01'.
      PS_MESSG-MSGNO = '000'.
*   Text-008 : Missing Planning Group in the file.
      PS_MESSG-MSGTX = TEXT-008.
      RETURN.
    ENDIF.

    IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
         PS_MAINX-VIEW_COMP-MAHNA EQ GC_TRUE ) AND
        PS_MAIN-VIEW_COMP-MAHNA IS INITIAL.
      PS_MESSG-MSGTY = 'E'.
      PS_MESSG-MSGID = 'ZSDSCA01'.
      PS_MESSG-MSGNO = '000'.
*   Text-009 : Missing Dunning Procedure in the file.
      PS_MESSG-MSGTX = TEXT-009.
      RETURN.
    ENDIF.

    IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
         PS_MAINX-VIEW_COMP-BUSAB EQ GC_TRUE ) AND
        PS_MAIN-VIEW_COMP-BUSAB IS INITIAL.
      PS_MESSG-MSGTY = 'E'.
      PS_MESSG-MSGID = 'ZSDSCA01'.
      PS_MESSG-MSGNO = '000'.
*   Text-010 : Missing Account Clerk in the file.
      PS_MESSG-MSGTX = TEXT-010.
      RETURN.
    ENDIF.

    IF ( PS_KEY-PARTNER_EXIST IS INITIAL OR
         PS_MAINX-VIEW_COMP-XAUSZ EQ GC_TRUE ) AND
        PS_MAIN-VIEW_COMP-XAUSZ IS INITIAL.
      PS_MESSG-MSGTY = 'E'.
      PS_MESSG-MSGID = 'ZSDSCA01'.
      PS_MESSG-MSGNO = '000'.
*   Text-011 : Missing Account Statement in the file.
      PS_MESSG-MSGTX = TEXT-011.
      RETURN.
    ENDIF.
  ENDIF.

ENDFORM.
* EOI - CH01
