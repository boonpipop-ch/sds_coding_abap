*&---------------------------------------------------------------------*
*& Include          ZSDSCAR0050_F06
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_process_data_tmp12
*&---------------------------------------------------------------------*
*& BP Master Template 12 Vendor General
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA_TMP12 CHANGING PT_RESULT TYPE GTTY_RESULT12
                                    PS_SUM     TYPE  GTY_SUM.

  DATA:
    LT_RAW  TYPE  GTTY_RAW,
    LT_DATA TYPE  GTTY_DATA12.

  DATA:
    LS_MESSG  TYPE  GTY_MESSG,
    LS_RESULT TYPE  GTY_RESULT12.


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
  PERFORM F_VALIDATE_FILE_TMPL12  USING LT_RAW
                               CHANGING LT_DATA.

* --------------------------------
* Step3: Upload data from file
* --------------------------------
* Upload file
  PERFORM F_UPLOAD_FILE_TMPL12  USING LT_DATA
                                      CB_TEST
                             CHANGING PT_RESULT
                                      PS_SUM.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_translate_raw12
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LFS_RAW>
*&      <-- LS_KEY
*&      <-- LS_MAIN
*&      <-- LS_MAINX
*&      <-- LS_MESSG
*&---------------------------------------------------------------------*
FORM F_TRANSLATE_RAW12 USING PS_RAW TYPE GTY_RAW
                     CHANGING PS_KEY       TYPE  GTY_KEY12
                              PS_MAIN      TYPE  GTY_MAIN12
                              PS_MAINX     TYPE  GTY_MAIN12X
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
    PERFORM F_GET_TARGET_FIELD12  USING  LV_INDEX
                               CHANGING LV_FIELD.
    IF  LV_STRING IS NOT INITIAL .
      CASE LV_FIELD.
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
          PS_KEY-TYPE =  GC_BU_ORG.  "2 organization
*        WHEN 'TYPE'.
*          PERFORM f_validate_bptype  USING  lv_string
*                                   CHANGING ps_key-type
*                                            lv_msgtx.

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

        WHEN 'VEND_VBUND'.
*          PERFORM f_validate_tradepartnr  USING  lv_string
*                                        CHANGING ps_main-vend-vbund
*                                                 lv_msgtx.
          PS_MAIN-VEND-VBUND  = LV_STRING.
          PS_MAINX-VEND-VBUND = GC_TRUE.

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


*        WHEN 'COMP_BUKRS'.
*          PERFORM f_validate_compcode  USING  lv_string
*                                     CHANGING ps_main-comp-bukrs
*                                              lv_msgtx.

*        WHEN 'COMP_AKONT'.
*          PERFORM f_validate_glaccount  USING  lv_string
*                                               ps_main-comp-bukrs
*                                      CHANGING ps_main-comp-akont
*                                               lv_msgtx.
*          ps_mainx-comp-akont = gc_true.

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

        WHEN 'PURC_INCO3_L'.
          PS_MAIN-PURCH-INCO3_L  = LV_STRING.
          PS_MAINX-PURCH-INCO3_L = GC_TRUE.

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


        WHEN 'PURC_MINBW'.
          PS_MAIN-PURCH-MINBW  = LV_STRING.
          PS_MAINX-PURCH-MINBW = GC_TRUE.

        WHEN 'PURC_LEBRE'.
          PS_MAIN-PURCH-LEBRE  = LV_STRING.
          PS_MAINX-PURCH-LEBRE = GC_TRUE.

        WHEN 'PURC_BOLRE'.
          PS_MAIN-PURCH-BOLRE  = LV_STRING.
          PS_MAINX-PURCH-BOLRE = GC_TRUE.

        WHEN 'PURC_UMSAE'.
          PS_MAIN-PURCH-UMSAE  = LV_STRING.
          PS_MAINX-PURCH-UMSAE = GC_TRUE.

        WHEN 'PURC_BOIND'.
          PS_MAIN-PURCH-BOIND  = LV_STRING.
          PS_MAINX-PURCH-BOIND = GC_TRUE.

        WHEN 'PURC_PLIFZ'.
          PS_MAIN-PURCH-PLIFZ  = LV_STRING.
          PS_MAINX-PURCH-PLIFZ = GC_TRUE.

        WHEN 'PURC_AGREL'.
          PS_MAIN-PURCH-AGREL  = LV_STRING.
          PS_MAINX-PURCH-AGREL = GC_TRUE.

        WHEN OTHERS.
          CONTINUE.

      ENDCASE.
    ENDIF.


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
*& Form f_validate_file_tmpl12
*&---------------------------------------------------------------------*
*& Validate File data for Template 12
*&---------------------------------------------------------------------*
FORM F_VALIDATE_FILE_TMPL12 USING PT_RAW TYPE GTTY_RAW
                          CHANGING PT_DATA  TYPE  GTTY_DATA12.

  DATA:
    LS_KEY   TYPE  GTY_KEY12,
    LS_MAIN  TYPE  GTY_MAIN12,
    LS_MAINX TYPE  GTY_MAIN12X,
    LS_MESSG TYPE  GTY_MESSG.

  DATA:
    LV_ROWNO   TYPE  GTY_RESULT12-ROWNO.

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
    PERFORM F_TRANSLATE_RAW12  USING  <LFS_RAW>
                            CHANGING LS_KEY
                                     LS_MAIN
                                     LS_MAINX
                                     LS_MESSG.

*   Validate and Append data into table
    PERFORM F_VALIDATE_AND_APPEND12  USING LV_ROWNO
                                           LS_KEY
                                           LS_MAIN
                                           LS_MAINX
                                           LS_MESSG
                                  CHANGING PT_DATA.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_and_append12
*&---------------------------------------------------------------------*
*& Validate Whole Line data and collect into internal table for posting
*&---------------------------------------------------------------------*
FORM F_VALIDATE_AND_APPEND12 USING PV_ROWNO TYPE GTY_RESULT12-ROWNO
                                    PS_KEY    TYPE  GTY_KEY12
                                    PS_MAIN   TYPE  GTY_MAIN12
                                    PS_MAINX  TYPE  GTY_MAIN12X
                                    PS_MESSG  TYPE  GTY_MESSG
                           CHANGING PT_DATA   TYPE  GTTY_DATA12.

  DATA:
    LS_KEY   TYPE  GTY_KEY12,
    LS_MAIN  TYPE  GTY_MAIN12,
    LS_MAINX TYPE  GTY_MAIN12X,
    LS_DATA  TYPE  GTY_DATA12,
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
    PERFORM F_VALIDATE_KEY12 CHANGING LS_KEY
                                     LS_MESSG.
    IF LS_MESSG IS NOT INITIAL.
      EXIT.
    ENDIF.

*  ---------------------------------
*   Validate Main data
*  ---------------------------------
    PERFORM F_VALIDATE_MAIN12 USING  LS_KEY
                            CHANGING LS_MAIN
                                     LS_MAINX
                                     LS_MESSG.
    IF LS_MESSG IS NOT INITIAL.
      EXIT.
    ENDIF.



*  Only for Purchasing Role
    READ TABLE LS_KEY-ROLE TRANSPORTING NO FIELDS
                           WITH KEY RLTYP = GC_ROLE1_13.
    IF SY-SUBRC EQ 0.
*     ---------------------------------
*      Validate Purchasing View
*     ---------------------------------
      PERFORM F_VALIDATE_PURCH12  USING  LS_KEY
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
*& Form f_upload_file_tmpl12
*&---------------------------------------------------------------------*
*& Upload File Template 12
*&---------------------------------------------------------------------*
FORM F_UPLOAD_FILE_TMPL12 USING PT_DATA TYPE GTTY_DATA12
                                 PV_TEST   TYPE  FLAG
                        CHANGING PT_RESULT TYPE  GTTY_RESULT12
                                 PS_SUM    TYPE  GTY_SUM.

  DATA:
    LT_MESSG TYPE  GTTY_MESSG.

  DATA:
    LS_DATA  TYPE  GTY_DATA12.

  DATA:
    LV_PARTNER TYPE  BUT000-PARTNER,
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
      PERFORM F_MAINTAIN_PARTNER12  USING  LS_DATA
                                           PV_TEST
                                  CHANGING LV_PARTNER
                                           LT_MESSG
                                           LV_ERROR.
      IF LV_PARTNER IS NOT INITIAL.
        LS_DATA-KEY-PARTNER = LV_PARTNER.
      ENDIF.
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
    PERFORM F_COLLECT_RESULT12  USING  LS_DATA
                              CHANGING PT_RESULT.

    CLEAR: LT_MESSG[], LV_ERROR .
  ENDLOOP.

* Show Final Message for Processing completed
* Message: Processing completed.
  MESSAGE S582(1FA).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_maintain_partner12
*&---------------------------------------------------------------------*
*& Maintain Business Partner Vendor
*&---------------------------------------------------------------------*
FORM F_MAINTAIN_PARTNER12 USING PS_DATA TYPE GTY_DATA12
                                PV_TEST  TYPE  FLAG
                       CHANGING PV_PARTNER TYPE GTY_DATA11-KEY-PARTNER
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
    LV_VEND        TYPE  FLAG,
    LV_PURCH       TYPE  FLAG.

  FIELD-SYMBOLS:
    <LFS_ROLE>   TYPE  GTY_V_ROLE.


* Initialize Output
  CLEAR: PT_MESSG.
  CLEAR:   PV_PARTNER,
           PV_ERROR.

  CLEAR LS_BAPI_DATA.

* ---------------------------
* Assign Header Data
* ---------------------------
  LS_BAPI_DATA-PARTNER-HEADER-OBJECT_TASK = 'U'.

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
    LS_BAPI_ROLE-DATA-VALID_FROM   = SY-DATUM.
    LS_BAPI_ROLE-DATA-VALID_TO     = '99991231'.
    LS_BAPI_ROLE-DATAX-VALID_FROM  = GC_TRUE.
    LS_BAPI_ROLE-DATAX-VALID_TO    = GC_TRUE.
    INSERT LS_BAPI_ROLE INTO TABLE LS_BAPI_DATA-PARTNER-CENTRAL_DATA-ROLE-ROLES.

  ENDLOOP.

* ---------------------------
* Assign Common Data
* ---------------------------
* Name + Sort Key
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-NAME1  = PS_DATA-MAIN-ADDR-NAME1.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-NAME1 = PS_DATA-MAINX-ADDR-NAME1.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-NAME2  = PS_DATA-MAIN-ADDR-NAME2.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-NAME2 = PS_DATA-MAINX-ADDR-NAME2.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-NAME3  = PS_DATA-MAIN-ADDR-NAME3.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-NAME3 = PS_DATA-MAINX-ADDR-NAME3.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-NAME4  = PS_DATA-MAIN-ADDR-NAME4.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-NAME4 = PS_DATA-MAINX-ADDR-NAME4.

  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-SEARCHTERM1  = PS_DATA-MAIN-ADDR-SORT1.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-SEARCHTERM1 = PS_DATA-MAINX-ADDR-SORT1.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-SEARCHTERM2  = PS_DATA-MAIN-ADDR-SORT2.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-SEARCHTERM2 = PS_DATA-MAINX-ADDR-SORT2.

  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-PARTNERLANGUAGE  = PS_DATA-MAIN-ADDR-LANGU.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-PARTNERLANGUAGE = PS_DATA-MAINX-ADDR-LANGU.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-PARTNERTYPE  = PS_DATA-MAIN-BPKIND.
  LS_BAPI_DATA-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-PARTNERTYPE = PS_DATA-MAINX-BPKIND.
* ---------------------------
* Assign Address Data
* ---------------------------
  PERFORM F_ASSIGN_BAPI_ADDRESS11  USING  PS_DATA-KEY-TYPE
                                          PS_DATA-MAIN-ADDR
                                          PS_DATA-MAINX-ADDR
                                          PS_DATA-MAIN-ADDR_INT
                                          PS_DATA-MAINX-ADDR_INT
                                          PS_DATA-MAIN-BPEXT
                                 CHANGING LS_BAPI_DATA-PARTNER-CENTRAL_DATA-ADDRESS
                                          GV_ZZCONTEXT.

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
*  IF lv_vend EQ gc_true.
*
*    ls_bapi_data-partner-finserv_data-common-data-fsbp_centrl-vbund  = ps_data-main-vend-vbund.
*    ls_bapi_data-partner-finserv_data-common-datax-fsbp_centrl-vbund = ps_data-mainx-vend-vbund.
*
**   Not assign vendor code for Some Group
*    IF NOT ( ps_data-key-bu_group EQ gc_bu_group_1_V OR
*             ps_data-key-bu_group EQ gc_bu_group_2_v ).
*      ls_bapi_data-vendor-header-object_instance = ps_data-key-partner.
*    ENDIF.
*
*    IF lv_vend_exist IS INITIAL.
*      ls_bapi_data-vendor-header-object_task = 'I'.
*    ELSE.
*      ls_bapi_data-vendor-header-object_task = 'U'.
*      ls_bapi_data-vendor-header-object_instance = ps_data-main-vend-lifnr.
*    ENDIF.
*    PERFORM f_assign_bapi_vendor  USING  ps_data-main-vend
*                                         ps_data-mainx-vend
*                                         ps_data-main-comp
*                                         ps_data-mainx-comp
*                                CHANGING ls_bapi_data-vendor.
*  ENDIF.

* ---------------------------
* Assign Purchasing Data
* ---------------------------
  IF LV_PURCH EQ GC_TRUE.
*   Not assign vendor code for Some Group
    IF NOT ( PS_DATA-KEY-BU_GROUP EQ GC_BU_GROUP_1_V OR
             PS_DATA-KEY-BU_GROUP EQ GC_BU_GROUP_2_V ).
      LS_BAPI_DATA-VENDOR-HEADER-OBJECT_INSTANCE = PS_DATA-KEY-PARTNER.
    ENDIF.

    IF LV_VEND_EXIST IS INITIAL.
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
      PERFORM F_UPDATE_ADDITIONAL12  USING  PV_PARTNER
                                          PS_DATA
                                 CHANGING PV_ERROR
                                          PT_MESSG.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ELSE.
    PERFORM F_CHECK_BAPI_RETURNMAP11  USING  LT_MSGMAP
                                    CHANGING PV_ERROR
                                             PT_MESSG.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_collect_result12
*&---------------------------------------------------------------------*
*& Collect Processing Result
*&---------------------------------------------------------------------*
FORM F_COLLECT_RESULT12 USING PS_DATA TYPE GTY_DATA12
                      CHANGING PT_RESULT TYPE  GTTY_RESULT12.

  DATA:
    LS_RESULT  TYPE  GTY_RESULT12.

  FIELD-SYMBOLS:
    <LFS_ROLE>  TYPE  GTY_V_ROLE,
    <LFS_MESSG> TYPE  GTY_MESSG.


  CLEAR: LS_RESULT.

  LOOP AT PS_DATA-KEY-ROLE ASSIGNING <LFS_ROLE>
                           WHERE RLTYP NE GC_ROLE1_13.
    LS_RESULT-RLTYP = <LFS_ROLE>-RLTYP.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC NE 0.
    LS_RESULT-RLTYP = GC_ROLE1_13.
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

  LS_RESULT-KNVK_NAMEV = PS_DATA-MAIN-VEND-NAMEV.
  LS_RESULT-KNVK_NAME1 = PS_DATA-MAIN-VEND-NAME1.


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
  LS_RESULT-PURC_INCO2 = PS_DATA-MAIN-PURCH-INCO2_L.
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
*& Form f_validate_key12
*&---------------------------------------------------------------------*
*& Validate Key
*&---------------------------------------------------------------------*
FORM F_VALIDATE_KEY12 CHANGING PS_KEY TYPE GTY_KEY12
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
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e34 : BP Category must be organization only.
    PS_MESSG-MSGTX = TEXT-E34.
    RETURN.
  ENDIF.

* Check Number on Creation
  CASE GV_MODE.
    WHEN GC_MODE_CREATE.
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

  DO 3 TIMES.
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
    ELSEIF SY-INDEX EQ 3.
*      IF cb_role3 IS INITIAL.
*        CONTINUE.
*      ENDIF.
      LS_ROLE-RLTYP = GC_ROLE1_13.
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
*& Form f_validate_main12
*&---------------------------------------------------------------------*
*& Validate Main Data
*&---------------------------------------------------------------------*
FORM F_VALIDATE_MAIN12 USING PS_KEY TYPE GTY_KEY12
                       CHANGING PS_MAIN TYPE  GTY_MAIN12
                                PS_MAINX TYPE GTY_MAIN12X
                                PS_MESSG TYPE GTY_MESSG.

* ---------------------------------
*  Validate Address
* ---------------------------------
*  Validate Address
  PERFORM F_VALIDATE_ADDRESS12  USING  PS_KEY
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
  PERFORM F_VALIDATE_ADDRESS_INT12  USING  PS_KEY
                                  CHANGING PS_MAIN-ADDR_INT
                                           PS_MAINX-ADDR_INT
                                           PS_MESSG.
  IF PS_MESSG IS NOT INITIAL.
*    EXIT.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_display_result12
*&---------------------------------------------------------------------*
*& Display Result
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT12 USING PT_RESULT TYPE GTTY_RESULT12.

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
  PERFORM F_ALV_LAYOUT12 CHANGING GS_LAYOUT_1
                                  GS_VARIANT_1
                                  GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_12.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_12.
  ASSIGN PT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2215424]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT12 CHANGING GT_FIELDCAT_1.
* Sort data
  PERFORM F_ALV_SORT_RESULT12 CHANGING GT_SORT_1.
* Call ALV Screen
  CALL SCREEN 9000.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_layout12
*&---------------------------------------------------------------------*
*& ALV Layout for Report
*&---------------------------------------------------------------------*
FORM F_ALV_LAYOUT12 CHANGING PS_LAYOUT TYPE LVC_S_LAYO
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
*& Form f_alv_build_fieldcat12
*&---------------------------------------------------------------------*
*& ALV Field Catalog for Report12
*&---------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT12 CHANGING PT_FIELDCAT TYPE LVC_T_FCAT.

  DATA:
    LV_TEXT         TYPE  TEXT50.

  FIELD-SYMBOLS:
    <LFS_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_12
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
      WHEN 'ADDR_EMAIL03'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL04'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL05'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL06'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL07'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL08'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL09'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL10'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL11'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL12'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL13'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL14'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL15'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
        "beg+++ iPS136Oct2021 [Rollout Captain]
      WHEN 'ADDR_EMAIL_NOTE01' OR 'ADDR_EMAIL_NOTE02' OR 'ADDR_EMAIL_NOTE03' OR 'ADDR_EMAIL_NOTE04' OR 'ADDR_EMAIL_NOTE05'
          OR 'ADDR_EMAIL_NOTE06' OR 'ADDR_EMAIL_NOTE07' OR 'ADDR_EMAIL_NOTE08' OR 'ADDR_EMAIL_NOTE09' OR 'ADDR_EMAIL_NOTE10'
          OR 'ADDR_EMAIL_NOTE11' OR 'ADDR_EMAIL_NOTE12' OR 'ADDR_EMAIL_NOTE13' OR 'ADDR_EMAIL_NOTE14' OR 'ADDR_EMAIL_NOTE15'
          OR 'ADDR_EMAIL_NOTEAP'.
        <LFS_FIELDCAT>-OUTPUTLEN = 5 ##NUMBER_OK.
        "end+++ iPS136Oct2021 [Rollout Captain]
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
      WHEN 'KNVK_NAMEV'.
      WHEN 'KNVK_NAME1'.
      WHEN OTHERS.
        <LFS_FIELDCAT>-TECH = GC_TRUE.

    ENDCASE.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_sort_result12
*&---------------------------------------------------------------------*
*& Maintain SORT data for Result ALV
*&---------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT12 CHANGING PT_SORT TYPE LVC_T_SORT.

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
*& Form f_get_target_field12
*&---------------------------------------------------------------------*
*& Get Target Field name based on column sequence
*&---------------------------------------------------------------------*
FORM F_GET_TARGET_FIELD12 USING PV_INDEX TYPE I
                        CHANGING  PV_FIELD  TYPE  GTY_FIELD_NAME.

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
    LREF_DESCR = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( 'GTY_TEMPLATE12' ).
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
*& Form f_validate_purch12
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_VALIDATE_PURCH12 USING PS_KEY TYPE GTY_KEY12
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
*& Form f_update_additional12
*&---------------------------------------------------------------------*
*& Update addition data
*&---------------------------------------------------------------------*
FORM F_UPDATE_ADDITIONAL12 USING PV_PARTNER TYPE BUT000-PARTNER
                                   PS_DATA     TYPE  GTY_DATA12
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
*& Form f_validate_address_int12
*&---------------------------------------------------------------------*
*& Validate address international
*&---------------------------------------------------------------------*

FORM F_VALIDATE_ADDRESS_INT12 USING PS_KEY TYPE GTY_KEY12
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
*& Form f_validate_address12
*&---------------------------------------------------------------------*
*& Validate address
*&---------------------------------------------------------------------*
FORM F_VALIDATE_ADDRESS12 USING PS_KEY TYPE GTY_KEY12
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
*   Text-e21 : Missing Language in the file.
    PS_MESSG-MSGTX = TEXT-E21.
    RETURN.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PROCESS_DATA_TMP13
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA_TMP13  CHANGING PT_RESULT TYPE GTTY_RESULT13
                                    PS_SUM     TYPE  GTY_SUM.

  DATA:
    LT_RAW  TYPE  GTTY_RAW,
    LT_DATA TYPE  GTTY_DATA13.

  DATA:
    LS_MESSG  TYPE  GTY_MESSG,
    LS_RESULT TYPE  GTY_RESULT13.


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
  PERFORM F_VALIDATE_FILE_TMPL13  USING LT_RAW
                               CHANGING LT_DATA.

* --------------------------------
* Step3: Upload data from file
* --------------------------------
* Upload file
  PERFORM F_UPLOAD_FILE_TMPL13  USING LT_DATA
                                      CB_TEST
                             CHANGING PT_RESULT
                                      PS_SUM.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_display_result13
*&---------------------------------------------------------------------*
*& Display Result
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT13 USING PT_RESULT TYPE GTTY_RESULT13.

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
  PERFORM F_ALV_LAYOUT13 CHANGING GS_LAYOUT_1
                                  GS_VARIANT_1
                                  GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_13.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_13.
  ASSIGN PT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2215424]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT13 CHANGING GT_FIELDCAT_1.
* Sort data
  PERFORM F_ALV_SORT_RESULT13 CHANGING GT_SORT_1.
* Call ALV Screen
  CALL SCREEN 9000.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_layout13
*&---------------------------------------------------------------------*
*& ALV Layout for Report
*&---------------------------------------------------------------------*
FORM F_ALV_LAYOUT13 CHANGING PS_LAYOUT TYPE LVC_S_LAYO
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
*& Form f_alv_build_fieldcat13
*&---------------------------------------------------------------------*
*& ALV Field Catalog for Report13
*&---------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT13 CHANGING PT_FIELDCAT TYPE LVC_T_FCAT.

  DATA:
    LV_TEXT         TYPE  TEXT50.

  FIELD-SYMBOLS:
    <LFS_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_13
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
        <LFS_FIELDCAT>-OUTPUTLEN = 50  ##NUMBER_OK.
      WHEN 'PARTNER'.
        IF CB_TEST IS INITIAL.
          <LFS_FIELDCAT>-HOTSPOT   = GC_TRUE.
        ENDIF.
      WHEN 'PARTNER2'.

      WHEN OTHERS.
        <LFS_FIELDCAT>-TECH = GC_TRUE.

    ENDCASE.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_sort_result13
*&---------------------------------------------------------------------*
*& Maintain SORT data for Result ALV
*&---------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT13 CHANGING PT_SORT TYPE LVC_T_SORT.

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
*& Form f_get_target_field13
*&---------------------------------------------------------------------*
*& Get Target Field name based on column sequence
*&---------------------------------------------------------------------*
FORM F_GET_TARGET_FIELD13 USING PV_INDEX TYPE I
                        CHANGING  PV_FIELD  TYPE  GTY_FIELD_NAME.

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
    LREF_DESCR = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( 'GTY_TEMPLATE13' ).
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
*& Form f_validate_file_tmpl13
*&---------------------------------------------------------------------*
*& Validate File data for Template 13
*&---------------------------------------------------------------------*
FORM F_VALIDATE_FILE_TMPL13 USING PT_RAW TYPE GTTY_RAW
                          CHANGING PT_DATA  TYPE  GTTY_DATA13.

  DATA:
    "LS_KEY   TYPE  GTY_KEY13,
    LS_MAIN  TYPE  GTY_MAIN3,
    LS_MESSG TYPE  GTY_MESSG.

  DATA:
    LV_ROWNO   TYPE  GTY_RESULT13-ROWNO.

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
    PERFORM F_TRANSLATE_RAW13  USING  <LFS_RAW>
                            CHANGING "LS_KEY
                                     LS_MAIN
                                     LS_MESSG.

*   Validate and Append data into table
    PERFORM F_VALIDATE_AND_APPEND13  USING LV_ROWNO
                                           "LS_KEY
                                           LS_MAIN
                                           LS_MESSG
                                  CHANGING PT_DATA.

  ENDLOOP.
ENDFORM.


*----------------------------------------------------------------------*
*  Form f_translate_raw13
*----------------------------------------------------------------------*
*  Convert Raw to Data13
*----------------------------------------------------------------------*
FORM F_TRANSLATE_RAW13  USING  PS_RAW       TYPE  GTY_RAW
                     CHANGING PS_MAIN      TYPE  GTY_MAIN3
                              PS_MESSG     TYPE  GTY_MESSG.

  DATA:
    LT_SPLIT  TYPE  STANDARD TABLE OF STRING.

  DATA:
    LV_INDEX        TYPE  I,
    LV_FIELD        TYPE  GTY_FIELD_NAME,
    LV_STRING       TYPE  STRING,
    LV_MSGTX        TYPE  GTY_MESSG-MSGTX,
    LV_PARTNER_GUID TYPE  BUT000-PARTNER_GUID,
    LV_REQUIRED     TYPE  CHAR1,
    LV_LEN          TYPE  I.


* Initialize Output
  CLEAR: PS_MAIN,

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
    REPLACE ALL OCCURRENCES OF  CL_ABAP_CHAR_UTILITIES=>NEWLINE IN LV_STRING WITH SPACE.

*   Get Target Field name to assign value
    PERFORM F_GET_TARGET_FIELD13  USING  LV_INDEX
                               CHANGING LV_FIELD.

    CASE LV_FIELD.
      WHEN 'PARTNER'.
        PERFORM F_VALIDATE_PARTNER  USING  LV_STRING
                                           GC_MODE_CHANGE
                                  CHANGING PS_MAIN-PARTNER
                                           PS_MAIN-PARTNER_GUID
                                           LV_MSGTX.

      WHEN 'PARTNER2'.
        PERFORM F_VALIDATE_PARTNER  USING  LV_STRING
                                           GC_MODE_CHANGE
                                  CHANGING PS_MAIN-PARTNER2
                                           LV_PARTNER_GUID
                                           LV_MSGTX.

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

*----------------------------------------------------------------------*
*  Form f_validate_and_append13
*----------------------------------------------------------------------*
*  Validate Whole Line data and collect into internal table for posting
*----------------------------------------------------------------------*
FORM F_VALIDATE_AND_APPEND13  USING PV_ROWNO  TYPE  GTY_RESULT13-ROWNO
                                    PS_MAIN   TYPE  GTY_MAIN3
                                    PS_MESSG  TYPE  GTY_MESSG
                           CHANGING PT_DATA   TYPE  GTTY_DATA13.

  DATA:
    LS_MAIN  TYPE  GTY_MAIN3,
    LS_DATA  TYPE  GTY_DATA13,
    LS_MESSG TYPE  GTY_MESSG.


* Initial Data
  LS_MAIN  = PS_MAIN.

  DO 1 TIMES.

*  ---------------------------------
*   Validate Main data
*  ---------------------------------
    PERFORM F_VALIDATE_MAIN13 CHANGING LS_MAIN
                                      LS_MESSG.
    IF LS_MESSG IS NOT INITIAL.
      EXIT.
    ENDIF.

  ENDDO.

* Generate new key
  CLEAR LS_DATA.
  LS_DATA-ROWNO = PV_ROWNO.
  LS_DATA-MAIN  = LS_MAIN.

* Collect Message
  IF PS_MESSG IS NOT INITIAL.
    APPEND PS_MESSG TO LS_DATA-MESSG.
  ELSEIF LS_MESSG IS NOT INITIAL.
    APPEND LS_MESSG TO LS_DATA-MESSG.
  ENDIF.

  INSERT LS_DATA INTO TABLE PT_DATA.

ENDFORM.


*----------------------------------------------------------------------*
*  Form f_validate_main13
*----------------------------------------------------------------------*
*  Validate Partner data
*----------------------------------------------------------------------*
FORM F_VALIDATE_MAIN13 CHANGING PS_MAIN  TYPE  GTY_MAIN3 ##NEEDED
                                PS_MESSG TYPE  GTY_MESSG.

  DATA:
    LV_PARTNER   TYPE  BUT000-PARTNER ##NEEDED.


* Initialize Output
  CLEAR: PS_MESSG.

  IF PS_MAIN-PARTNER IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e17 : Missing BP Number in the file.
    PS_MESSG-MSGTX = TEXT-E17.
    RETURN.
  ENDIF.

  IF PS_MAIN-PARTNER2 IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e13 : Missing Relationship BP Number in the file.
    PS_MESSG-MSGTX = TEXT-E13.
    RETURN.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_upload_file_tmpl3
*----------------------------------------------------------------------*
*  Upload File Template 3
*----------------------------------------------------------------------*
FORM F_UPLOAD_FILE_TMPL13  USING  PT_DATA  TYPE  GTTY_DATA13
                                 PV_TEST  TYPE  FLAG
                        CHANGING PT_RESULT TYPE  GTTY_RESULT13
                                 PS_SUM    TYPE  GTY_SUM.

  DATA:
    LT_MESSG TYPE  GTTY_MESSG.

  DATA:
    LS_DATA  TYPE  GTY_DATA13.

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
      PERFORM F_MAINTAIN_RELATION  USING  LS_DATA
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
    PERFORM F_COLLECT_RESULT13  USING  LS_DATA
                             CHANGING PT_RESULT.

  ENDLOOP.

* Show Final Message for Processing completed
* Message: Processing completed.
  MESSAGE S582(1FA).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_collect_result13
*&---------------------------------------------------------------------*
*& Collect Processing Result
*&---------------------------------------------------------------------*
FORM F_COLLECT_RESULT13 USING PS_DATA TYPE GTY_DATA13
                      CHANGING PT_RESULT TYPE  GTTY_RESULT13.

  DATA:
    LS_RESULT  TYPE  GTY_RESULT13.

  FIELD-SYMBOLS:
    <LFS_ROLE>  TYPE  GTY_V_ROLE ##NEEDED,
    <LFS_MESSG> TYPE  GTY_MESSG.


  CLEAR: LS_RESULT.

  LS_RESULT-ROWNO   = PS_DATA-ROWNO.
  LS_RESULT-PARTNER = PS_DATA-MAIN-PARTNER.
  LS_RESULT-PARTNER2 = PS_DATA-MAIN-PARTNER2.


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

*----------------------------------------------------------------------*
*  Form f_maintain_relation
*----------------------------------------------------------------------*
*  Maintain Relationship
*----------------------------------------------------------------------*
FORM F_MAINTAIN_RELATION  USING  PS_DATA  TYPE  GTY_DATA13
                                 PV_TEST  TYPE  FLAG
                        CHANGING PT_MESSG TYPE  GTTY_MESSG
                                 PV_ERROR TYPE  FLAG.

  DATA:
    LT_BAPI_DATA TYPE  CVIS_EI_EXTERN_T,
    LT_MSGMAP    TYPE  MDG_BS_BP_MSGMAP_T,
    LT_RETURN    TYPE  BAPIRETM.

  DATA:
    LS_BAPI_DATA TYPE  CVIS_EI_EXTERN.


* Initialize Output
  CLEAR: PT_MESSG.
  CLEAR:   PV_ERROR.

  CLEAR LS_BAPI_DATA.

* ---------------------------
* Assign Header Data
* ---------------------------
  LS_BAPI_DATA-PARTNER-HEADER-OBJECT_TASK = 'U'.
  LS_BAPI_DATA-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNER = PS_DATA-MAIN-PARTNER.
  LS_BAPI_DATA-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNERGUID = PS_DATA-MAIN-PARTNER_GUID.

* ---------------------------
* Assign Credit Control Managment
* ---------------------------
  PERFORM F_ASSIGN_BAPI_RELATION  USING  PS_DATA-MAIN
                                CHANGING LS_BAPI_DATA-PARTNER_RELATION.

* Validate Data
  CALL METHOD CL_MD_BP_MAINTAIN=>VALIDATE_SINGLE
    EXPORTING
      I_DATA        = LS_BAPI_DATA
    IMPORTING
      ET_RETURN_MAP = LT_MSGMAP.
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

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ELSE.
    PERFORM F_CHECK_BAPI_RETURNMAP  USING  LT_MSGMAP
                                  CHANGING PV_ERROR
                                           PT_MESSG.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_assign_bapi_relation
*----------------------------------------------------------------------*
*  Assign BAPI Relationship data
*----------------------------------------------------------------------*
FORM F_ASSIGN_BAPI_RELATION  USING  PS_MAIN   TYPE  GTY_MAIN3
                           CHANGING PT_RELATION TYPE  BURS_EI_EXTERN_T.

  DATA:
    LS_RELATION  TYPE  BURS_EI_EXTERN.


  CLEAR LS_RELATION.
  LS_RELATION-HEADER-OBJECT_TASK = 'I'.

  LS_RELATION-HEADER-OBJECT_INSTANCE-PARTNER1 = PS_MAIN-PARTNER.
  LS_RELATION-HEADER-OBJECT_INSTANCE-PARTNER2 = PS_MAIN-PARTNER2.


  LS_RELATION-HEADER-OBJECT_INSTANCE-RELAT_CATEGORY =  'BUR001'.
  LS_RELATION-HEADER-OBJECT_INSTANCE-DATE_TO        = '99991231'.

  INSERT LS_RELATION INTO TABLE PT_RELATION.

ENDFORM.
