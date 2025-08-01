*&---------------------------------------------------------------------*
*& Include          ZSDSCAR0050_F04
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_process_data_tmp23
*&---------------------------------------------------------------------*
*  Processing data for Template 23
*  Tax Address
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA_TMP23 CHANGING PT_RESULT TYPE GTTY_RESULT23
                                    PS_SUM     TYPE  GTY_SUM.

  DATA:
    LT_RAW  TYPE  GTTY_RAW,
    LT_DATA TYPE  GTTY_DATA23.

  DATA:
    LS_MESSG  TYPE  GTY_MESSG,
    LS_RESULT TYPE  GTY_RESULT23.


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
  PERFORM F_VALIDATE_FILE_TMP23  USING  LT_RAW
                                CHANGING LT_DATA.

* --------------------------------
* Step3: Upload data from file
* --------------------------------
* Upload file
  PERFORM F_UPLOAD_FILE_TMP23  USING  LT_DATA
                                      CB_TEST
                             CHANGING PT_RESULT
                                      PS_SUM.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_validate_file_tmp23
*&---------------------------------------------------------------------*
*& Validate File data for Template 23
*&---------------------------------------------------------------------*
FORM F_VALIDATE_FILE_TMP23 USING PT_RAW TYPE GTTY_RAW
                          CHANGING PT_DATA    TYPE  GTTY_DATA23.

  DATA:
    LS_MAIN  TYPE  GTY_MAIN23,
    LS_MAINX TYPE  GTY_MAIN23X,
    LS_MESSG TYPE  GTY_MESSG.

  DATA:
    LV_ROWNO   TYPE  GTY_RESULT23-ROWNO.

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
    PERFORM F_TRANSLATE_RAW23  USING  <LFS_RAW>
                             CHANGING LS_MAIN
                                      LS_MAINX
                                      LS_MESSG.

*   Validate and Append data into table
    PERFORM F_VALIDATE_AND_APPEND23  USING  LV_ROWNO
                                            LS_MAIN
                                            LS_MAINX
                                            LS_MESSG
                                   CHANGING PT_DATA.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_translate_raw23
*&---------------------------------------------------------------------*
*& Convert Raw to Data23
*&---------------------------------------------------------------------*
FORM F_TRANSLATE_RAW23 USING PS_RAW TYPE GTY_RAW
                      CHANGING PS_MAIN      TYPE  GTY_MAIN23
                               PS_MAINX     TYPE  GTY_MAIN23X
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
    PERFORM F_GET_TARGET_FIELD23  USING  LV_INDEX
                                CHANGING LV_FIELD.

    CASE LV_FIELD.

      WHEN 'RLTYP'.
        PERFORM F_VALIDATE_ROLE23  USING  LV_STRING
                                 CHANGING PS_MAIN-RLTYP
                                          LV_MSGTX.

      WHEN 'PARTNER'.
        PERFORM F_VALIDATE_PARTNER   USING  LV_STRING
                                            GC_MODE_CHANGE
                                   CHANGING PS_MAIN-PARTNER
                                            PS_MAIN-PARTNER_GUID
                                            LV_MSGTX.
      WHEN 'ADDR_ADR_KIND'.
        PS_MAIN-ADDR-ADR_KIND  = LV_STRING.

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

      WHEN 'ADDRINT_NAME_CO'.
        PS_MAIN-ADDR_INT-NAME_CO = LV_STRING.
        PS_MAINX-ADDR_INT-NAME_CO = GC_TRUE.

      WHEN 'ADDRINT_SORT1'.
        PS_MAIN-ADDR_INT-SORT1 = LV_STRING.
        PS_MAINX-ADDR_INT-SORT1 = GC_TRUE.

      WHEN 'ADDRINT_SORT2'.
        PS_MAIN-ADDR_INT-SORT2 = LV_STRING.
        PS_MAINX-ADDR_INT-SORT2 = GC_TRUE.

      WHEN 'ADDRINT_STREET'.
        PS_MAIN-ADDR_INT-STREET = LV_STRING.
        PS_MAINX-ADDR_INT-STREET = GC_TRUE.

      WHEN 'ADDRINT_STR_SUPPL1'.
        PS_MAIN-ADDR_INT-STR_SUPPL1 = LV_STRING.
        PS_MAINX-ADDR_INT-STR_SUPPL1 = GC_TRUE.

      WHEN 'ADDRINT_STR_SUPPL2'.
        PS_MAIN-ADDR_INT-STR_SUPPL2 = LV_STRING.
        PS_MAINX-ADDR_INT-STR_SUPPL2 = GC_TRUE.

*      WHEN 'ADDRINT_STREET'.
*        PS_MAIN-ADDR_INT-STREET = LV_STRING.
*        PS_MAINX-ADDR_INT-STREET = GC_TRUE.

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

      WHEN 'ADDRINT_REMARK'.
        PS_MAIN-ADDR_INT-REMARK  = LV_STRING.
        PS_MAINX-ADDR_INT-REMARK = GC_TRUE.

      WHEN 'ADDRINT_COUNTRY'.
        PS_MAIN-ADDR_INT-COUNTRY = LV_STRING.
        PS_MAINX-ADDR_INT-COUNTRY = GC_TRUE.

      WHEN 'BPEXT'.
        PS_MAIN-BPEXT  = LV_STRING.
        PS_MAINX-BPEXT = GC_TRUE.

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
*& Form f_get_target_field23
*&---------------------------------------------------------------------*
*& Get Target Field name based on column sequence
*&---------------------------------------------------------------------*
FORM F_GET_TARGET_FIELD23 USING PV_INDEX TYPE I
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
    LREF_DESCR = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( 'GTY_TEMPLATE23' ).
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
*& Form f_validate_role23
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_VALIDATE_ROLE23 USING PV_STRING TYPE STRING
                      CHANGING PV_RLTYP   TYPE  BUT100-RLTYP
                               PV_MSGTX   TYPE  CLIKE.

* Initialize Output
  CLEAR: PV_RLTYP,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

  IF PV_STRING NE GC_ROLE2_1 .
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
*& Form f_validate_and_append23
*&---------------------------------------------------------------------*
*& Validate Whole Line data and collect into internal table for posting
*&---------------------------------------------------------------------*
FORM F_VALIDATE_AND_APPEND23 USING PV_ROWNO TYPE GTY_RESULT23-ROWNO
                                    PS_MAIN   TYPE  GTY_MAIN23
                                    PS_MAINX  TYPE  GTY_MAIN23X
                                    PS_MESSG  TYPE  GTY_MESSG
                           CHANGING PT_DATA   TYPE  GTTY_DATA23.

  DATA:
    LS_MAIN  TYPE  GTY_MAIN23,
    LS_MAINX TYPE  GTY_MAIN23X,
    LS_DATA  TYPE  GTY_DATA23,
    LS_MESSG TYPE  GTY_MESSG.


* Initial Data
  LS_MAIN  = PS_MAIN.
  LS_MAINX = PS_MAINX.

  DO 1 TIMES.

*  ---------------------------------
*   Validate Main data
*  ---------------------------------
    PERFORM F_VALIDATE_MAIN23  CHANGING LS_MAIN
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
*& Form f_validate_main23
*&---------------------------------------------------------------------*
*& Validate Partner data
*&---------------------------------------------------------------------*
FORM F_VALIDATE_MAIN23 CHANGING PS_MAIN TYPE GTY_MAIN23
                                 PS_MAINX TYPE  GTY_MAIN23X ##NEEDED
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

* Check Customer Exist?
  PERFORM F_GET_CUSTOMER_FROM_PARTNER  USING  PS_MAIN-PARTNER
                                     CHANGING PS_MAIN-KUNNR.
  IF PS_MAIN-KUNNR IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e82 : Cannot find customer code for partner
    CONCATENATE TEXT-E82 PS_MAIN-PARTNER
           INTO PS_MESSG-MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Get address no.
  SELECT  ADDRNUMBER                "#EC CI_NOORDER  "#EC CI_SEL_NESTED
    INTO PS_MAIN-ADDR-ADDRNUMBER UP TO 1 ROWS
    FROM BUT020
   WHERE PARTNER  EQ  PS_MAIN-PARTNER .
  ENDSELECT .
  IF SY-SUBRC NE 0.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e : Missing Address no.
    PS_MESSG-MSGTX = TEXT-E80.
    RETURN.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_upload_file_tmp23
*&---------------------------------------------------------------------*
*&  Upload File Template 23
*&---------------------------------------------------------------------*
FORM F_UPLOAD_FILE_TMP23 USING PT_DATA TYPE GTTY_DATA23
                                 PV_TEST   TYPE  FLAG
                        CHANGING PT_RESULT TYPE  GTTY_RESULT23
                                 PS_SUM    TYPE  GTY_SUM.

  DATA:
    LT_MESSG TYPE  GTTY_MESSG.

  DATA:
    LS_DATA  TYPE  GTY_DATA23.

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
      PERFORM F_MAINTAIN_TAX_ADDRESS USING   LS_DATA
                                             PV_TEST
                                   CHANGING  LT_MESSG
                                             LV_ERROR.

*      perform f_maintain_tax_addr using ls_data
*                                        pv_test
*                               CHANGING lt_messg
*                                        lv_error .

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
    PERFORM F_COLLECT_RESULT23  USING  LS_DATA
                             CHANGING PT_RESULT.

    CLEAR: LT_MESSG[], LV_ERROR .
  ENDLOOP.

* Show Final Message for Processing completed
* Message: Processing completed.
  MESSAGE S582(1FA).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_maintain_tax_address
*&---------------------------------------------------------------------*
*& Maintain tax address
*&---------------------------------------------------------------------*
FORM F_MAINTAIN_TAX_ADDRESS USING PS_DATA TYPE GTY_DATA23
                                    PV_TEST   TYPE  FLAG
                           CHANGING PT_MESSG  TYPE  GTTY_MESSG
                                    PV_ERROR  TYPE  FLAG.

  DATA:
    LS_CODE  TYPE  FITHA_PBUPL_D ##NEEDED,
    LS_DESC  TYPE  FITHA_PBUPL_D_T ##NEEDED,
    LS_MESSG TYPE  GTY_MESSG.

  DATA:
    LV_ADDRNUMBER  TYPE  ADRC-ADDRNUMBER.

* Initialize Output
  CLEAR: PT_MESSG.
  CLEAR: PV_ERROR.

  IF PS_DATA-MAINX-ADDR IS NOT INITIAL OR
     PS_DATA-MAINX-ADDR_INT IS NOT INITIAL.
    PERFORM F_MAINTAIN_ADDRESS  USING  PS_DATA
                                       PV_TEST
                              CHANGING LV_ADDRNUMBER
                                       PT_MESSG
                                       PV_ERROR.
    IF PV_ERROR IS NOT INITIAL.
      RETURN.
    ENDIF.
  ELSE.
    LV_ADDRNUMBER = PS_DATA-MAIN-ADDR-ADDRNUMBER.
  ENDIF.

  IF PV_ERROR IS INITIAL.
    CLEAR LS_MESSG.
    LS_MESSG-MSGTY = 'S'.
    LS_MESSG-MSGID = 'ZTEC'.
    LS_MESSG-MSGNO = '000'.
*   Text-i03: Business Partner data has been updated successfully.
    LS_MESSG-MSGTX = TEXT-I03.
    INSERT LS_MESSG INTO TABLE PT_MESSG.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_maintain_address
*&---------------------------------------------------------------------*
*& Maintain Tax Address data
*&---------------------------------------------------------------------*
FORM F_MAINTAIN_ADDRESS USING PS_DATA TYPE GTY_DATA23
                                PV_TEST  TYPE  FLAG
                       CHANGING PV_ADDRNUMBER TYPE ADRC-ADDRNUMBER
                                PT_MESSG  TYPE GTTY_MESSG
                                PV_ERROR  TYPE FLAG.

  TYPES: BEGIN OF LTY_CONSNUMBER,
           CONSNUMBER TYPE  ADR2-CONSNUMBER,
         END OF LTY_CONSNUMBER.
  TYPES: LTTY_CONSNUMBER  TYPE  SORTED TABLE OF LTY_CONSNUMBER
                                WITH UNIQUE KEY CONSNUMBER.

  CONSTANTS:
    LC_BP     TYPE  ADRG-ADDR_GROUP VALUE 'BP' ##NEEDED,
    LC_HANDLE TYPE  SZADR_ADDR1_COMPLETE-ADDRHANDLE VALUE 'BRANCHADDR' ##NEEDED.

  DATA:
    LT_ERROR      TYPE  STANDARD TABLE OF ADDR_ERROR ##NEEDED,
    LT_CONSNUMBER TYPE  LTTY_CONSNUMBER ##NEEDED.

  DATA:
    LS_ADDR_COMPT TYPE  SZADR_ADDR1_COMPLETE ##NEEDED,
    LS_ADDR1      TYPE  SZADR_ADDR1_LINE ##NEEDED,
    LS_ADTEL      TYPE  SZADR_ADTEL_LINE ##NEEDED,
    LS_ADFAX      TYPE  SZADR_ADFAX_LINE ##NEEDED,
    LS_ADSMTP     TYPE  SZADR_ADSMTP_LINE ##NEEDED,
    LS_ADDR_REF   TYPE  ADDR_REF ##NEEDED,
    LS_MESSG      TYPE  GTY_MESSG ##NEEDED.

  DATA:
    LV_UPDATEFLAG  TYPE  SZAD_FIELD-UPDATEFLAG ##NEEDED,
    LV_UPDATEUSAGE TYPE  SZAD_FIELD-UPDATEFLAG ##NEEDED,
    LV_RETCODE     TYPE  SZAD_FIELD-RETURNCODE ##NEEDED.

  FIELD-SYMBOLS:
    <LFS_ERROR>   TYPE  ADDR_ERROR ##NEEDED.


* Initialize Output
  CLEAR: PV_ADDRNUMBER,
         PV_ERROR.
  CLEAR: PT_MESSG.

* -------------------
* Check address usage is exist or not.
* -------------------
  DATA: LV_ADDRNUMBER TYPE BUT020-ADDRNUMBER.
*        lv_addrguid   TYPE but020-address_guid.

*  SELECT but020~addrnumber , but020~address_guid
*    INTO (@lv_addrnumber , @lv_addrguid)
*    UP TO 1 ROWS
*    FROM but021_fs INNER JOIN but020
*                      ON but020~partner = but021_fs~partner
*   WHERE but021_fs~partner  = @ps_data-main-partner
*     AND but021_fs~adr_kind = @ps_data-main-addr-adr_kind  .
*  ENDSELECT.
  SELECT ADDRNUMBER INTO LV_ADDRNUMBER UP TO 1 ROWS     "#EC CI_NOORDER
    FROM BUT021_FS
   WHERE PARTNER  = PS_DATA-MAIN-PARTNER
     AND ADR_KIND = PS_DATA-MAIN-ADDR-ADR_KIND  .
  ENDSELECT.

  IF LV_ADDRNUMBER IS INITIAL .
    LV_UPDATEUSAGE = 'I'.
  ENDIF.

  IF  LV_UPDATEUSAGE IS INITIAL.
    PERFORM F_UPDATE_PARTNER24 USING PS_DATA
                                     PV_ADDRNUMBER
                                     PS_DATA-MAIN-ADDR-ADR_KIND
                                     PV_TEST
                            CHANGING PT_MESSG
                                     PV_ERROR .

  ELSE.
    PERFORM F_ADD_ADDRESSUSAGE USING PS_DATA
                                     PV_ADDRNUMBER
                                     PS_DATA-MAIN-ADDR-ADR_KIND
                                     PV_TEST
                            CHANGING PT_MESSG
                                     PV_ERROR .
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_collect_result23
*&---------------------------------------------------------------------*
*& Collect Result from Processing
*&---------------------------------------------------------------------*
FORM F_COLLECT_RESULT23 USING PS_DATA TYPE GTY_DATA23
                      CHANGING PT_RESULT TYPE  GTTY_RESULT23.

  DATA:
    LS_RESULT  TYPE  GTY_RESULT23.

  FIELD-SYMBOLS:
    <LFS_MESSG> TYPE  GTY_MESSG.


  CLEAR: LS_RESULT.

  LS_RESULT-ROWNO   = PS_DATA-ROWNO.
  LS_RESULT-RLTYP =  PS_DATA-MAIN-RLTYP.
  LS_RESULT-PARTNER =  PS_DATA-MAIN-PARTNER.
  LS_RESULT-ADDR_NAME1 =  PS_DATA-MAIN-ADDR-NAME1.
  LS_RESULT-ADDR_NAME2 =  PS_DATA-MAIN-ADDR-NAME2.
  LS_RESULT-ADDR_NAME3 =  PS_DATA-MAIN-ADDR-NAME3.
  LS_RESULT-ADDR_NAME4 =  PS_DATA-MAIN-ADDR-NAME4.
  LS_RESULT-ADDR_SORT1 =  PS_DATA-MAIN-ADDR-SORT1.
  LS_RESULT-ADDR_SORT2 =  PS_DATA-MAIN-ADDR-SORT2.
  LS_RESULT-ADDR_STR_SUPPL1 =  PS_DATA-MAIN-ADDR-STR_SUPPL1.
  LS_RESULT-ADDR_STR_SUPPL2 =  PS_DATA-MAIN-ADDR-STR_SUPPL2.
  LS_RESULT-ADDR_STREET =  PS_DATA-MAIN-ADDR-STREET.
  LS_RESULT-ADDR_STR_SUPPL3 =  PS_DATA-MAIN-ADDR-STR_SUPPL3.
  LS_RESULT-ADDR_LOCATION =  PS_DATA-MAIN-ADDR-LOCATION.
  LS_RESULT-ADDR_CITY2 =  PS_DATA-MAIN-ADDR-CITY2.
  LS_RESULT-ADDR_CITY1 =  PS_DATA-MAIN-ADDR-CITY1.
  LS_RESULT-ADDR_POST_CODE1 =  PS_DATA-MAIN-ADDR-POST_CODE1.
  LS_RESULT-ADDR_COUNTRY =  PS_DATA-MAIN-ADDR-COUNTRY.
  LS_RESULT-ADDR_LANGU =  PS_DATA-MAIN-ADDR-LANGU.
  LS_RESULT-ADDR_TELNO1 =  PS_DATA-MAIN-ADDR-PHONE-TELNO1.
  LS_RESULT-ADDR_TELEXT1 =  PS_DATA-MAIN-ADDR-PHONE-TELEXT1.
  LS_RESULT-ADDR_MOBILE =  PS_DATA-MAIN-ADDR-PHONE-MOBILE.
  LS_RESULT-ADDR_FAXNO =  PS_DATA-MAIN-ADDR-FAX-FAXNO.
  LS_RESULT-ADDR_FAXEXT =  PS_DATA-MAIN-ADDR-FAX-FAXEXT.
  LS_RESULT-ADDR_EMAIL1 =  PS_DATA-MAIN-ADDR-SMTP-EMAIL1.
  LS_RESULT-ADDR_REMARK =  PS_DATA-MAIN-ADDR-REMARK.
  LS_RESULT-ADDRINT_ACTIVE =  PS_DATA-MAIN-ADDR_INT-ACTIVE.
  LS_RESULT-ADDRINT_NATION =  PS_DATA-MAIN-ADDR_INT-NATION.
  LS_RESULT-ADDRINT_NAME1 =  PS_DATA-MAIN-ADDR_INT-NAME1.
  LS_RESULT-ADDRINT_NAME2 =  PS_DATA-MAIN-ADDR_INT-NAME2.
  LS_RESULT-ADDRINT_NAME3 =  PS_DATA-MAIN-ADDR_INT-NAME3.
  LS_RESULT-ADDRINT_NAME4 =  PS_DATA-MAIN-ADDR_INT-NAME4.
  LS_RESULT-ADDRINT_SORT1 =  PS_DATA-MAIN-ADDR_INT-SORT1.
  LS_RESULT-ADDRINT_SORT2 =  PS_DATA-MAIN-ADDR_INT-SORT2.
  LS_RESULT-ADDRINT_STR_SUPPL1 =  PS_DATA-MAIN-ADDR_INT-STR_SUPPL1.
  LS_RESULT-ADDRINT_STR_SUPPL2 =  PS_DATA-MAIN-ADDR_INT-STR_SUPPL2.
  LS_RESULT-ADDRINT_STREET =  PS_DATA-MAIN-ADDR_INT-STREET.
  LS_RESULT-ADDRINT_STR_SUPPL3 =  PS_DATA-MAIN-ADDR_INT-STR_SUPPL3.
  LS_RESULT-ADDRINT_LOCATION =  PS_DATA-MAIN-ADDR_INT-LOCATION.
  LS_RESULT-ADDRINT_CITY2 =  PS_DATA-MAIN-ADDR_INT-CITY2.
  LS_RESULT-ADDRINT_CITY1 =  PS_DATA-MAIN-ADDR_INT-CITY1.
  LS_RESULT-ADDRINT_POST_CODE1 =  PS_DATA-MAIN-ADDR_INT-POST_CODE1.
  LS_RESULT-ADDRINT_COUNTRY =  PS_DATA-MAIN-ADDR_INT-COUNTRY.
  LS_RESULT-ADDRINT_LANGU =  PS_DATA-MAIN-ADDR_INT-LANGU.

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
*& Form f_display_result23
*&---------------------------------------------------------------------*
*& Display Result
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT23 USING PT_RESULT TYPE GTTY_RESULT23.

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
  PERFORM F_ALV_LAYOUT23 CHANGING GS_LAYOUT_1
                                  GS_VARIANT_1
                                  GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_23.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_23.
  ASSIGN PT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2215424]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT23 CHANGING GT_FIELDCAT_1.
* Sort data
  PERFORM F_ALV_SORT_RESULT23 CHANGING GT_SORT_1.
* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_layout23
*&---------------------------------------------------------------------*
*& ALV Layout for Report
*&---------------------------------------------------------------------*
FORM F_ALV_LAYOUT23 CHANGING PS_LAYOUT TYPE LVC_S_LAYO
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
*& Form f_alv_build_fieldcat23
*&---------------------------------------------------------------------*
*& ALV Field Catalog for Report23
*&---------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT23 CHANGING PT_FIELDCAT TYPE LVC_T_FCAT.

  DATA:
    LV_TEXT         TYPE  TEXT50.

  FIELD-SYMBOLS:
    <LFS_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_23
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
      WHEN 'ADDR_STR_SUPPL1'.
        <LFS_FIELDCAT>-OUTPUTLEN = 30 ##NUMBER_OK.
      WHEN 'ADDR_STR_SUPPL2'.
        <LFS_FIELDCAT>-OUTPUTLEN = 30 ##NUMBER_OK.
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
      WHEN 'ADDR_MOBILE'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'ADDR_FAXNO'.
        <LFS_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'ADDR_FAXEXT'.
        <LFS_FIELDCAT>-OUTPUTLEN = 5 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL1'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_REMARK'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDRINT_ACTIVE'.
      WHEN 'ADDRINT_NATION'.
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
      WHEN 'ADDRINT_STR_SUPPL1'.
        <LFS_FIELDCAT>-OUTPUTLEN = 30 ##NUMBER_OK.
      WHEN 'ADDRINT_STR_SUPPL2'.
        <LFS_FIELDCAT>-OUTPUTLEN = 30 ##NUMBER_OK.
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

      WHEN OTHERS.
        <LFS_FIELDCAT>-TECH = GC_TRUE.

    ENDCASE.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_sort_result23
*&---------------------------------------------------------------------*
*& Maintain SORT data for Result ALV
*&---------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT23 CHANGING PT_SORT TYPE LVC_T_SORT.

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
*& Form f_maintain_tax_addr
*&---------------------------------------------------------------------*
*& Maintain Address
*&---------------------------------------------------------------------*
*FORM f_maintain_tax_addr  USING  ps_data   TYPE  gty_data23
*                                 pv_test   TYPE  flag
*                        CHANGING pt_messg  TYPE  gtty_messg
*                                 pv_error  TYPE  flag.
*
*  DATA:
*    ls_code  TYPE  fitha_pbupl_d,
*    ls_desc  TYPE  fitha_pbupl_d_t,
*    ls_messg TYPE  gty_messg.
*
*  DATA:
*    lv_addrnumber  TYPE  adrc-addrnumber.
*
*  FIELD-SYMBOLS:
*    <lfs_code> TYPE  fitha_pbupl_d,
*    <lfs_desc> TYPE  fitha_pbupl_d_t.
*
*
** Initialize Output
*  CLEAR: pt_messg.
*  CLEAR: pv_error.
*
*  IF ps_data-mainx-addr IS NOT INITIAL OR
*     ps_data-mainx-addr_int IS NOT INITIAL.
*    PERFORM f_maintain_address23  USING  ps_data
*                                         pv_test
*                                CHANGING lv_addrnumber
*                                         pt_messg
*                                         pv_error.
*    IF pv_error IS NOT INITIAL.
*      RETURN.
*    ENDIF.
*  ELSE.
*    lv_addrnumber = ps_data-main-addr-addrnumber.
*  ENDIF.
*
*  IF pv_test EQ gc_true.
*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*    CLEAR ls_messg.
*    ls_messg-msgty = 'S'.
*    ls_messg-msgid = 'ZTEC'.
*    ls_messg-msgno = '000'.
**   Text-i01: Test upload successfully.
*    ls_messg-msgtx = TEXT-i01.
*    INSERT ls_messg INTO TABLE pt_messg.
*    RETURN.
*  ENDIF.
*
*
*  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*    EXPORTING
*      wait = gc_true.
*
*  CLEAR ls_messg.
*  ls_messg-msgty = 'S'.
*  ls_messg-msgid = 'ZTEC'.
*  ls_messg-msgno = '000'.
** Text-i03: Business Partner data has been updated successfully.
*  ls_messg-msgtx = TEXT-i03.
*  INSERT ls_messg INTO TABLE pt_messg.
*
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_add_addressusage
*&---------------------------------------------------------------------*
*& Add address usge
*&---------------------------------------------------------------------*
FORM F_ADD_ADDRESSUSAGE USING PS_DATA TYPE GTY_DATA23
                                PV_ADDR     TYPE ADRC-ADDRNUMBER
                                PV_ADR_KIND TYPE BUT021_FS-ADR_KIND ##NEEDED
                                PV_TEST     TYPE FLAG
                       CHANGING PT_MESSG    TYPE GTTY_MESSG
                                PV_ERROR    TYPE FLAG.

  DATA: LT_ADRUSG TYPE TABLE OF BAPIBUS1006_ADDRESSUSAGE,
        LS_ADRUSG LIKE LINE OF LT_ADRUSG.

  DATA: LS_CENTRAL   TYPE BAPIBUS1006_CENTRAL,
        LS_ORG       TYPE BAPIBUS1006_CENTRAL_ORGAN,
        LS_CENTRAL_X TYPE BAPIBUS1006_CENTRAL_X,
        LS_ORG_X     TYPE BAPIBUS1006_CENTRAL_ORGAN_X,
        LS_ADDRESS_X TYPE BAPIBUS1006_ADDRESS_X,
        LS_ADDRESS   TYPE BAPIBUS1006_ADDRESS,
        LT_ADTEL     TYPE BAPIADTEL_T ##NEEDED,
        LT_ADFAX     TYPE BAPIADFAX_T ##NEEDED,
        LT_MAIL      TYPE BAPIADSMTP_T ##NEEDED,
        LT_ADTEL2    TYPE BAPIADTEL_T ##NEEDED,
        LT_ADFAX2    TYPE BAPIADFAX_T ##NEEDED,
        LT_MAIL2     TYPE BAPIADSMTP_T ##NEEDED,
        LT_ADTEL_X   TYPE BAPIADTELX_T ##NEEDED,
        LT_ADFAX_X   TYPE BAPIADFAXX_T ##NEEDED,
        LT_ADSMT_X   TYPE BAPIADSMTX_T ##NEEDED,
        LT_NOTE_X    TYPE BAPIAD_REX_T ##NEEDED,
        LS_ADTEL_X   TYPE BAPIADTELX ##NEEDED,
        LS_ADFAX_X   TYPE BAPIADFAXX ##NEEDED,
        LS_ADSMT_X   TYPE BAPIADSMTX ##NEEDED,
        LS_NOTE_X    TYPE BAPIAD_REX ##NEEDED,
        LT_RETURN    TYPE TABLE OF BAPIRET2,
        LS_RETURN    TYPE BAPIRET2,
        LT_NOTE      TYPE BP_AD_REM_TTY ##NEEDED,
        LT_NOTE2     TYPE BP_AD_REM_TTY ##NEEDED,
        LS_NOTE      TYPE BAPIAD_REM ##NEEDED,
        LS_ADTEL     TYPE BAPIADTEL ##NEEDED,
        LS_MAIL      TYPE BAPIADSMTP ##NEEDED,
        LS_ADFAX     TYPE BAPIADFAX ##NEEDED,
        LV_BP        TYPE BUT000-PARTNER ##NEEDED,
        LS_MESSG     TYPE  GTY_MESSG.

* --- Update existing BP ---
  PERFORM F_SET_UPDATE_VALUE USING 'SEARCHTERM1' PS_DATA-MAIN-ADDR-SORT1
        CHANGING LS_CENTRAL LS_CENTRAL_X.
  PERFORM F_SET_UPDATE_VALUE USING 'SEARCHTERM2' PS_DATA-MAIN-ADDR-SORT2
        CHANGING LS_CENTRAL LS_CENTRAL_X.
  PERFORM F_SET_UPDATE_VALUE USING 'TITLE_KEY' PS_DATA-MAIN-ADDR-TITLE
        CHANGING LS_CENTRAL LS_CENTRAL_X.

  PERFORM F_SET_UPDATE_VALUE USING 'NAME1' PS_DATA-MAIN-ADDR-NAME1
        CHANGING LS_ORG LS_ORG_X.
  PERFORM F_SET_UPDATE_VALUE USING 'NAME2' PS_DATA-MAIN-ADDR-NAME2
        CHANGING LS_ORG LS_ORG_X.
  PERFORM F_SET_UPDATE_VALUE USING 'NAME3' PS_DATA-MAIN-ADDR-NAME3
        CHANGING LS_ORG LS_ORG_X.
  PERFORM F_SET_UPDATE_VALUE USING 'NAME4' PS_DATA-MAIN-ADDR-NAME4
        CHANGING LS_ORG LS_ORG_X.

  CLEAR LT_RETURN.
  CALL FUNCTION 'BAPI_BUPA_CENTRAL_CHANGE'
    EXPORTING
      BUSINESSPARTNER           = PS_DATA-MAIN-PARTNER
      CENTRALDATA               = LS_CENTRAL
      CENTRALDATAORGANIZATION   = LS_ORG
      CENTRALDATA_X             = LS_CENTRAL_X
      CENTRALDATAORGANIZATION_X = LS_ORG_X
    TABLES
      RETURN                    = LT_RETURN.

  LOOP AT LT_RETURN INTO LS_RETURN WHERE TYPE CA 'AEX' ##INTO_OK.

    PV_ERROR = GC_TRUE.
    CLEAR LS_MESSG.
    LS_MESSG-MSGID = LS_RETURN-ID.
    LS_MESSG-MSGNO = LS_RETURN-NUMBER.
    LS_MESSG-MSGTY = LS_RETURN-TYPE.
    MESSAGE ID   LS_RETURN-ID
            TYPE LS_RETURN-TYPE
            NUMBER LS_RETURN-NUMBER
            WITH LS_RETURN-MESSAGE_V1
                 LS_RETURN-MESSAGE_V2
                 LS_RETURN-MESSAGE_V3
                 LS_RETURN-MESSAGE_V4
            INTO LS_MESSG-MSGTX.
    INSERT LS_MESSG INTO TABLE PT_MESSG.
    RETURN.
  ENDLOOP.

  IF PV_TEST EQ GC_TRUE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    CLEAR LS_MESSG.
    LS_MESSG-MSGTY = 'S'.
    LS_MESSG-MSGID = 'ZTEC'.
    LS_MESSG-MSGNO = '000'.
*   Text-i01: Test upload successfully.
    LS_MESSG-MSGTX = TEXT-I01.
    INSERT LS_MESSG INTO TABLE PT_MESSG.
    RETURN.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT = GC_TRUE.

* --- Add new Address ---
  PERFORM F_SET_UPDATE_VALUE USING 'CITY'
                                    PS_DATA-MAIN-ADDR-CITY1
                           CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'DISTRICT'
                                    PS_DATA-MAIN-ADDR-CITY2
                           CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'EXTADDRESSNUMBER'
                                    PS_DATA-MAIN-ADDR-ADEXT
                           CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'POSTL_COD1'
                                    PS_DATA-MAIN-ADDR-POST_CODE1
                           CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'STREET'
                                   PS_DATA-MAIN-ADDR-STREET
                           CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'STR_SUPPL1'
                                   PS_DATA-MAIN-ADDR-STR_SUPPL1
                           CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'STR_SUPPL2'
                                   PS_DATA-MAIN-ADDR-STR_SUPPL2
                           CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'STR_SUPPL3'
                                   PS_DATA-MAIN-ADDR-STR_SUPPL3
                          CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'LOCATION'
                                   PS_DATA-MAIN-ADDR-LOCATION
                          CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'COUNTRY'
                                   PS_DATA-MAIN-ADDR-COUNTRY
                          CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'TEL_NUMBER'
                                   PS_DATA-MAIN-ADDR-PHONE-TELNO1
                          CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'TEL_EXTENS'
                                   PS_DATA-MAIN-ADDR-PHONE-TELEXT1
                          CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'MOBILE'
                                   PS_DATA-MAIN-ADDR-PHONE-MOBILE
                          CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'FAX_NUMBER'
                                   PS_DATA-MAIN-ADDR-FAX-FAXNO
                          CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'FAX_EXTENS'
                                   PS_DATA-MAIN-ADDR-FAX-FAXEXT
                          CHANGING LS_ADDRESS LS_ADDRESS_X.

*  PERFORM f_set_update_value USING 'SMTP_ADDR'
*                                   ps_data-main-addr-smtp-email1
*                          CHANGING ls_address ls_address_x.

  PERFORM F_SET_UPDATE_VALUE USING 'REMARK'
                                   PS_DATA-MAIN-ADDR-REMARK
                          CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'COMM_TYPE'
                                   PS_DATA-MAIN-ADDR-DEFLT_COMM
                          CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'C_O_NAME'
                                   PS_DATA-MAIN-ADDR-NAME_CO
                          CHANGING LS_ADDRESS LS_ADDRESS_X.

  IF LS_ADDRESS-LANGU IS INITIAL.
    LS_ADDRESS-LANGU = '2'. "Default TH language
  ENDIF.

*  ls_adrusg-addresstype     = 'XXDEFAULT'.
*  ls_adrusg-standardaddressusage = '' .
**    ls_adrusg-usagevalidfrom  = ps_data-main-addr-valid_from.
**    ls_adrusg-usagevalidto    = ps_data-main-addr-valid_to.
*  APPEND ls_adrusg TO lt_adrusg.

  LS_ADRUSG-ADDRESSTYPE          = PS_DATA-MAIN-ADDR-ADR_KIND.
*  ls_adrusg-standardaddressusage = '' .
  LS_ADRUSG-USAGEVALIDFROM  = SY-DATUM.
  LS_ADRUSG-USAGEVALIDTO    = PS_DATA-MAIN-ADDR-VALID_TO.
  APPEND LS_ADRUSG TO LT_ADRUSG.
  DATA LT_SMTP TYPE TABLE OF BAPIADSMTP .
  DATA LV_GUID TYPE  BUT020-GUID .
  CALL FUNCTION 'BAPI_BUPA_ADDRESS_ADD'
    EXPORTING
      BUSINESSPARTNER = PS_DATA-MAIN-PARTNER
      ADDRESSDATA     = LS_ADDRESS
    IMPORTING
      ADDRESSGUID     = LV_GUID
    TABLES
      ADDRESSUSAGE    = LT_ADRUSG
      BAPIADSMTP      = LT_SMTP
      RETURN          = LT_RETURN.


  LOOP AT LT_RETURN INTO LS_RETURN WHERE TYPE CA 'AEX' ##INTO_OK.

    PV_ERROR = GC_TRUE.
    CLEAR LS_MESSG.
    LS_MESSG-MSGID = LS_RETURN-ID.
    LS_MESSG-MSGNO = LS_RETURN-NUMBER.
    LS_MESSG-MSGTY = LS_RETURN-TYPE.
    MESSAGE ID   LS_RETURN-ID
            TYPE LS_RETURN-TYPE
            NUMBER LS_RETURN-NUMBER
            WITH LS_RETURN-MESSAGE_V1
                 LS_RETURN-MESSAGE_V2
                 LS_RETURN-MESSAGE_V3
                 LS_RETURN-MESSAGE_V4
            INTO LS_MESSG-MSGTX.
    INSERT LS_MESSG INTO TABLE PT_MESSG.
    RETURN.
  ENDLOOP.

  IF PV_TEST EQ GC_TRUE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    CLEAR LS_MESSG.
    LS_MESSG-MSGTY = 'S'.
    LS_MESSG-MSGID = 'ZTEC'.
    LS_MESSG-MSGNO = '000'.
*   Text-i01: Test upload successfully.
    LS_MESSG-MSGTX = TEXT-I01.
    INSERT LS_MESSG INTO TABLE PT_MESSG.
    RETURN.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT = GC_TRUE.



*-Update address international
*  DATA: LV_ADDRNUMBER TYPE BUT020-ADDRNUMBER,
*        LV_ADDRGUID   TYPE BUT020-ADDRESS_GUID.

*  DO 100 TIMES.

*    SELECT but020~addrnumber , but020~address_guid
*      INTO (@lv_addrnumber , @lv_addrguid)
*      UP TO 1 ROWS
*      FROM but021_fs INNER JOIN but020
*                        ON but020~partner = but021_fs~partner
*     WHERE but021_fs~partner  = @ps_data-main-partner
*       AND but021_fs~adr_kind = @ps_data-main-addr-adr_kind  .
*    ENDSELECT.

*    SELECT addrnumber INTO lv_addrnumber UP TO 1 ROWS
*      FROM but021_fs
*     WHERE partner  = ps_data-main-partner
*       AND adr_kind = ps_data-main-addr-adr_kind  .
*    ENDSELECT.
*    IF sy-subrc = 0 .
*
*      EXIT.
*
*    ENDIF.
*  ENDDO.

  IF LV_GUID IS NOT INITIAL .
*    PERFORM f_addrusage_update_addr_inter  USING  ps_data
*                                                  lv_guid
*                                                  pv_adr_kind
*                                                  pv_test
*                                         CHANGING pt_messg
*                                                  pv_error    .

    DATA: LT_ADVER  TYPE BAPIAD1VD_T,
          LT_ADVERX TYPE BAPIAD1VDX_T,
          LS_ADVER  TYPE BAPIAD1VD,
          LS_ADVERX TYPE BAPIAD1VDX.

    IF PS_DATA-MAINX-ADDR_INT IS NOT INITIAL.

* Add international version
      LS_ADVER-ADDR_VERS = 'I'.
      LS_ADVER-TITLE      = PS_DATA-MAIN-ADDR_INT-TITLE.
      LS_ADVER-NAME       = PS_DATA-MAIN-ADDR_INT-NAME1.
      LS_ADVER-NAME_2     = PS_DATA-MAIN-ADDR_INT-NAME2.
      LS_ADVER-NAME_3     = PS_DATA-MAIN-ADDR_INT-NAME3.
      LS_ADVER-NAME_4     = PS_DATA-MAIN-ADDR_INT-NAME4.
      LS_ADVER-STR_SUPPL1 = PS_DATA-MAIN-ADDR_INT-STR_SUPPL1.
      LS_ADVER-STR_SUPPL2 = PS_DATA-MAIN-ADDR_INT-STR_SUPPL2.
      LS_ADVER-CITY       = PS_DATA-MAIN-ADDR_INT-CITY1.
      LS_ADVER-DISTRICT   = PS_DATA-MAIN-ADDR_INT-CITY2.
      LS_ADVER-STREET     = PS_DATA-MAIN-ADDR_INT-STREET.
      LS_ADVER-STR_SUPPL3 = PS_DATA-MAIN-ADDR_INT-STR_SUPPL3.
      LS_ADVER-LOCATION   = PS_DATA-MAIN-ADDR_INT-LOCATION.
      LS_ADVER-COUNTY     = PS_DATA-MAIN-ADDR_INT-COUNTRY.
      LS_ADVER-SORT1      = PS_DATA-MAIN-ADDR_INT-SORT1.
      LS_ADVER-SORT2      = PS_DATA-MAIN-ADDR_INT-SORT2.
      APPEND LS_ADVER TO LT_ADVER.

      LS_ADVERX-ADDR_VERS = 'X'.
      LS_ADVERX-TITLE = 'X'.
      LS_ADVERX-NAME = 'X'.
      LS_ADVERX-NAME_2 = 'X'.
      LS_ADVERX-NAME_3 = 'X'.
      LS_ADVERX-NAME_4 = 'X'.
      LS_ADVERX-STR_SUPPL1 = 'X'.
      LS_ADVERX-STR_SUPPL2 = 'X'.
      LS_ADVERX-STR_SUPPL3 = 'X'.
      LS_ADVERX-STREET = 'X'.
      LS_ADVERX-LOCATION = 'X'.
      LS_ADVERX-DISTRICT = 'X'.
      LS_ADVERX-CITY = 'X'.
      LS_ADVERX-COUNTY = 'X'.
      LS_ADVERX-UPDATEFLAG = 'I'.
      APPEND LS_ADVERX TO LT_ADVERX.

      CLEAR LT_RETURN[].
      CALL FUNCTION 'BAPI_BUPA_ADDRESS_CHANGE'
        EXPORTING
          BUSINESSPARTNER = PS_DATA-MAIN-PARTNER
          ADDRESSGUID     = LV_GUID
        TABLES
          BAPIADVERSORG   = LT_ADVER
          BAPIADVERSORG_X = LT_ADVERX
          RETURN          = LT_RETURN.

      LOOP AT LT_RETURN INTO LS_RETURN WHERE TYPE CA 'AEX' ##INTO_OK.

        PV_ERROR = GC_TRUE.
        CLEAR LS_MESSG.
        LS_MESSG-MSGID = LS_RETURN-ID.
        LS_MESSG-MSGNO = LS_RETURN-NUMBER.
        LS_MESSG-MSGTY = LS_RETURN-TYPE.
        MESSAGE ID   LS_RETURN-ID
                TYPE LS_RETURN-TYPE
                NUMBER LS_RETURN-NUMBER
                WITH LS_RETURN-MESSAGE_V1
                     LS_RETURN-MESSAGE_V2
                     LS_RETURN-MESSAGE_V3
                     LS_RETURN-MESSAGE_V4
                INTO LS_MESSG-MSGTX.
        INSERT LS_MESSG INTO TABLE PT_MESSG.
        RETURN.
      ENDLOOP.

      IF PV_TEST EQ GC_TRUE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CLEAR LS_MESSG.
        LS_MESSG-MSGTY = 'S'.
        LS_MESSG-MSGID = 'ZTEC'.
        LS_MESSG-MSGNO = '000'.
*     Text-i01: Test upload successfully.
        LS_MESSG-MSGTX = TEXT-I01.
        INSERT LS_MESSG INTO TABLE PT_MESSG.
        RETURN.
      ENDIF.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = GC_TRUE.

    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_update_value
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_SET_UPDATE_VALUE USING PV_FIELD TYPE DD03L-FIELDNAME
                                  PV_VALUE type ANY
                         CHANGING CS_DATA type ANY
                                  CS_DATA_X type ANY .

  FIELD-SYMBOLS: <LFS_FIELD>  TYPE ANY,
                 <LFS_FIELD2> TYPE ANY.

  CHECK PV_VALUE IS NOT INITIAL.

  ASSIGN COMPONENT PV_FIELD OF STRUCTURE CS_DATA TO <LFS_FIELD>.
  IF SY-SUBRC EQ 0.
    <LFS_FIELD> = PV_VALUE.

    ASSIGN COMPONENT PV_FIELD OF STRUCTURE CS_DATA_X TO <LFS_FIELD2>.
    IF SY-SUBRC EQ 0.
      <LFS_FIELD2> = ABAP_TRUE.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_addrusage_update_addr_inter
*&---------------------------------------------------------------------*
*& Update address international for add addressusage
*&---------------------------------------------------------------------*
*FORM f_addrusage_update_addr_inter  USING ps_data     TYPE gty_data23
*                                          pv_guid     TYPE but020-guid
*                                          pv_adr_kind TYPE but021_fs-adr_kind
*                                          pv_test     TYPE flag
*                                 CHANGING pt_messg    TYPE gtty_messg
*                                          pv_error    TYPE flag.
*  CONSTANTS:
*    lc_bp     TYPE  adrg-addr_group VALUE 'BP'.
*
*  DATA:
*    ls_addr_compt TYPE  szadr_addr1_complete,
*    ls_addr1      TYPE  szadr_addr1_line,
*    lt_error      TYPE  STANDARD TABLE OF addr_error,
*    ls_messg      TYPE  gty_messg,
*
*    lv_updateflag TYPE  szad_field-updateflag,
*    lv_retcode    TYPE  szad_field-returncode.
*
*  FIELD-SYMBOLS:
*    <lfs_error>   TYPE  addr_error.
*
*  lv_updateflag = 'U'.
*  ls_addr_compt-addrnumber = pv_addr.
*
*
*  IF ps_data-mainx-addr_int IS NOT INITIAL.
*    CLEAR ls_addr1.
**    ls_addr1-nation = ps_data-main-addr_int-nation.
*    ls_addr1-nation = 'I'.
*    ls_addr1-data-name1 = ps_data-main-addr_int-name1.
*    ls_addr1-data-name2 = ps_data-main-addr_int-name2.
*    ls_addr1-data-name3 = ps_data-main-addr_int-name3.
*    ls_addr1-data-name4 = ps_data-main-addr_int-name4.
*    ls_addr1-data-str_suppl1 = ps_data-main-addr_int-str_suppl1.
*    ls_addr1-data-str_suppl2 = ps_data-main-addr_int-str_suppl2.
*    ls_addr1-data-city1 = ps_data-main-addr_int-city1.
*    ls_addr1-data-city2 = ps_data-main-addr_int-city2.
*    ls_addr1-data-post_code1 = ps_data-main-addr_int-post_code1.
*    ls_addr1-data-street = ps_data-main-addr_int-street.
*    ls_addr1-data-str_suppl3 = ps_data-main-addr_int-str_suppl3.
*    ls_addr1-data-location = ps_data-main-addr_int-location.
*    ls_addr1-data-country = ps_data-main-addr_int-country.
*    ls_addr1-data-langu = ps_data-main-addr_int-langu.
*    ls_addr1-data-sort1 = ps_data-main-addr_int-sort1.
*    ls_addr1-data-sort2 = ps_data-main-addr_int-sort2.
*    INSERT ls_addr1 INTO TABLE ls_addr_compt-addr1_tab.
*  ENDIF.
*
*  CALL FUNCTION 'ADDR_MAINTAIN_COMPLETE'
*    EXPORTING
*      updateflag        = lv_updateflag
*      addr1_complete    = ls_addr_compt
*      address_group     = lc_bp
*      check_address     = space
*    IMPORTING
*      returncode        = lv_retcode
*    TABLES
*      error_table       = lt_error
*    EXCEPTIONS
*      parameter_error   = 1
*      address_not_exist = 2
*      handle_exist      = 3
*      internal_error    = 4
*      address_blocked   = 5
*      OTHERS            = 6.
*  IF sy-subrc <> 0 OR
*    lv_retcode IS NOT INITIAL OR
*    lt_error IS NOT INITIAL.
*    pv_error = gc_true.
*    LOOP AT lt_error ASSIGNING <lfs_error>.
*      CLEAR ls_messg.
*      ls_messg-msgid = <lfs_error>-msg_id.
*      ls_messg-msgty = <lfs_error>-msg_type.
*      ls_messg-msgno = <lfs_error>-msg_number.
*      MESSAGE ID <lfs_error>-msg_id
*              TYPE <lfs_error>-msg_type
*              NUMBER <lfs_error>-msg_number
*              WITH <lfs_error>-msg_var1
*                   <lfs_error>-msg_var2
*                   <lfs_error>-msg_var3
*                   <lfs_error>-msg_var4
*              INTO ls_messg-msgtx.
*      INSERT ls_messg INTO TABLE pt_messg.
*    ENDLOOP.
*    IF sy-subrc NE 0.
*      CLEAR ls_messg.
*      ls_messg-msgid = 'ZTEC'.
*      ls_messg-msgty = 'E'.
*      ls_messg-msgno = '000'.
**     Text-e83: Error during maintain branch address.
*      ls_messg-msgtx = TEXT-e83.
*    ENDIF.
*    RETURN.
*  ENDIF.
*
** Clear Memory For Test Mode
*  IF pv_test EQ gc_true.
*    CALL FUNCTION 'ADDR_MEMORY_CLEAR'
*      EXPORTING
*        force              = gc_true
*      EXCEPTIONS
*        unsaved_data_exist = 1
*        internal_error     = 2
*        OTHERS             = 3.
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.
*    RETURN.
*  ENDIF.
*
*  CALL FUNCTION 'ADDR_MEMORY_SAVE'
*    EXPORTING
*      execute_in_update_task = ' '
*    EXCEPTIONS
*      address_number_missing = 1
*      person_number_missing  = 2
*      internal_error         = 3
*      database_error         = 4
*      reference_missing      = 5
*      OTHERS                 = 6.
*  IF sy-subrc <> 0.
*    pv_error = gc_true.
*    CLEAR ls_messg.
*    ls_messg-msgid = sy-msgid.
*    ls_messg-msgno = sy-msgno.
*    ls_messg-msgty = sy-msgty.
*    MESSAGE ID sy-msgid
*            TYPE sy-msgty
*            NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2
*                 sy-msgv3 sy-msgv4
*            INTO ls_messg-msgtx.
*    INSERT ls_messg INTO TABLE pt_messg.
*    RETURN.
*  ENDIF.
*
*  IF pv_test EQ gc_true.
*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*    CLEAR ls_messg.
*    ls_messg-msgty = 'S'.
*    ls_messg-msgid = 'ZTEC'.
*    ls_messg-msgno = '000'.
**   Text-i01: Test upload successfully.
*    ls_messg-msgtx = TEXT-i01.
*    INSERT ls_messg INTO TABLE pt_messg.
*    RETURN.
*  ENDIF.
*
*  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*    EXPORTING
*      wait = gc_true.

*ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_partner24
*&---------------------------------------------------------------------*
*& Update Partner
*&---------------------------------------------------------------------*

FORM F_UPDATE_PARTNER24 USING PS_DATA TYPE GTY_DATA23
                                PV_ADDR     TYPE ADRC-ADDRNUMBER ##NEEDED
                                PV_ADR_KIND TYPE BUT021_FS-ADR_KIND
                                PV_TEST     TYPE FLAG
                       CHANGING PT_MESSG    TYPE GTTY_MESSG
                                PV_ERROR    TYPE FLAG.

  DATA: LS_CENTRAL   TYPE BAPIBUS1006_CENTRAL,
        LS_ORG       TYPE BAPIBUS1006_CENTRAL_ORGAN,
        LS_CENTRAL_X TYPE BAPIBUS1006_CENTRAL_X,
        LS_ORG_X     TYPE BAPIBUS1006_CENTRAL_ORGAN_X,
        LS_ADDRESS_X TYPE BAPIBUS1006_ADDRESS_X,
        LS_ADDRESS   TYPE BAPIBUS1006_ADDRESS,
        LT_ADTEL     TYPE BAPIADTEL_T,
        LT_ADFAX     TYPE BAPIADFAX_T,
        LT_MAIL      TYPE BAPIADSMTP_T,
        LT_ADTEL2    TYPE BAPIADTEL_T ##NEEDED,
        LT_ADFAX2    TYPE BAPIADFAX_T ##NEEDED,
        LT_MAIL2     TYPE BAPIADSMTP_T ##NEEDED,
        LT_ADTEL_X   TYPE BAPIADTELX_T ##NEEDED,
        LT_ADFAX_X   TYPE BAPIADFAXX_T ##NEEDED,
        LT_ADSMT_X   TYPE BAPIADSMTX_T ##NEEDED,
        LT_NOTE_X    TYPE BAPIAD_REX_T ##NEEDED,
        LS_ADTEL_X   TYPE BAPIADTELX ##NEEDED,
        LS_ADFAX_X   TYPE BAPIADFAXX ##NEEDED,
        LS_ADSMT_X   TYPE BAPIADSMTX ##NEEDED,
        LS_NOTE_X    TYPE BAPIAD_REX ##NEEDED,
        LT_RETURN    TYPE TABLE OF BAPIRET2,
        LS_RETURN    TYPE BAPIRET2,
        LT_NOTE      TYPE BP_AD_REM_TTY,
        LT_NOTE2     TYPE BP_AD_REM_TTY,
        LS_NOTE      TYPE BAPIAD_REM,
        LS_ADTEL     TYPE BAPIADTEL ##NEEDED,
        LS_MAIL      TYPE BAPIADSMTP ##NEEDED,
        LS_ADFAX     TYPE BAPIADFAX ##NEEDED,
*        lv_bp        TYPE but000-partner,
        LS_MESSG     TYPE GTY_MESSG.

*---Get BP GUID
  DATA: LV_ADDRNUMBER TYPE BAPIBUS1006_ADDRESSES_INT-ADDRNUMBER ##NEEDED,
        LV_ADDRGUID   TYPE BAPIBUS1006_ADDRESSES_INT-ADDRGUID,
        LV_GUID       TYPE BUT020-GUID.

  CALL FUNCTION 'BAPI_BUPA_ADDRESSES_GET'
    EXPORTING
      BUSINESSPARTNER       = PS_DATA-MAIN-PARTNER
      ADDRESSTYPE           = PV_ADR_KIND
    IMPORTING
      STANDARDADDRESSNUMBER = LV_ADDRNUMBER
      STANDARDADDRESSGUID   = LV_ADDRGUID.
  ##FM_SUBRC_OK
  IF SY-SUBRC = 0 .
    LV_GUID = LV_ADDRGUID.
  ENDIF.

* --- Update existing BP ---
  PERFORM F_SET_UPDATE_VALUE USING 'SEARCHTERM1' PS_DATA-MAIN-ADDR-SORT1
        CHANGING LS_CENTRAL LS_CENTRAL_X.
  PERFORM F_SET_UPDATE_VALUE USING 'SEARCHTERM2' PS_DATA-MAIN-ADDR-SORT2
        CHANGING LS_CENTRAL LS_CENTRAL_X.
  PERFORM F_SET_UPDATE_VALUE USING 'TITLE_KEY' PS_DATA-MAIN-ADDR-TITLE
        CHANGING LS_CENTRAL LS_CENTRAL_X.

  PERFORM F_SET_UPDATE_VALUE USING 'NAME1' PS_DATA-MAIN-ADDR-NAME1
        CHANGING LS_ORG LS_ORG_X.
  PERFORM F_SET_UPDATE_VALUE USING 'NAME2' PS_DATA-MAIN-ADDR-NAME2
        CHANGING LS_ORG LS_ORG_X.
  PERFORM F_SET_UPDATE_VALUE USING 'NAME3' PS_DATA-MAIN-ADDR-NAME3
        CHANGING LS_ORG LS_ORG_X.
  PERFORM F_SET_UPDATE_VALUE USING 'NAME4' PS_DATA-MAIN-ADDR-NAME4
        CHANGING LS_ORG LS_ORG_X.

  CLEAR LT_RETURN.
  CALL FUNCTION 'BAPI_BUPA_CENTRAL_CHANGE'
    EXPORTING
      BUSINESSPARTNER           = PS_DATA-MAIN-PARTNER
      CENTRALDATA               = LS_CENTRAL
      CENTRALDATAORGANIZATION   = LS_ORG
      CENTRALDATA_X             = LS_CENTRAL_X
      CENTRALDATAORGANIZATION_X = LS_ORG_X
    TABLES
      RETURN                    = LT_RETURN.

  LOOP AT LT_RETURN INTO LS_RETURN WHERE TYPE CA 'AEX' ##INTO_OK.

    PV_ERROR = GC_TRUE.
    CLEAR LS_MESSG.
    LS_MESSG-MSGID = LS_RETURN-ID.
    LS_MESSG-MSGNO = LS_RETURN-NUMBER.
    LS_MESSG-MSGTY = LS_RETURN-TYPE.
    MESSAGE ID   LS_RETURN-ID
            TYPE LS_RETURN-TYPE
            NUMBER LS_RETURN-NUMBER
            WITH LS_RETURN-MESSAGE_V1
                 LS_RETURN-MESSAGE_V2
                 LS_RETURN-MESSAGE_V3
                 LS_RETURN-MESSAGE_V4
            INTO LS_MESSG-MSGTX.
    INSERT LS_MESSG INTO TABLE PT_MESSG.
    RETURN.
  ENDLOOP.

  CLEAR LT_RETURN.
  CALL FUNCTION 'BAPI_BUPA_ADDRESS_GETDETAIL'
    EXPORTING
      BUSINESSPARTNER = PS_DATA-MAIN-PARTNER
      ADDRESSGUID     = LV_GUID
    TABLES
      BAPIADTEL       = LT_ADTEL
      BAPIADFAX       = LT_ADFAX
      BAPIADSMTP      = LT_MAIL
      BAPIAD_REM      = LT_NOTE
      RETURN          = LT_RETURN.


  PERFORM F_SET_UPDATE_VALUE USING 'CITY'
                                    PS_DATA-MAIN-ADDR-CITY1
                           CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'DISTRICT'
                                    PS_DATA-MAIN-ADDR-CITY2
                           CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'EXTADDRESSNUMBER'
                                    PS_DATA-MAIN-ADDR-ADEXT
                           CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'POSTL_COD1'
                                    PS_DATA-MAIN-ADDR-POST_CODE1
                           CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'STREET'
                                   PS_DATA-MAIN-ADDR-STREET
                           CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'STR_SUPPL1'
                                   PS_DATA-MAIN-ADDR-STR_SUPPL1
                           CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'STR_SUPPL2'
                                   PS_DATA-MAIN-ADDR-STR_SUPPL2
                           CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'STR_SUPPL3'
                                   PS_DATA-MAIN-ADDR-STR_SUPPL3
                          CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'LOCATION'
                                   PS_DATA-MAIN-ADDR-LOCATION
                          CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'COUNTRY'
                                   PS_DATA-MAIN-ADDR-COUNTRY
                          CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'LANGU'
                                   PS_DATA-MAIN-ADDR-LANGU
                          CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'COMM_TYPE'
                                   PS_DATA-MAIN-ADDR-DEFLT_COMM
                          CHANGING LS_ADDRESS LS_ADDRESS_X.

  PERFORM F_SET_UPDATE_VALUE USING 'C_O_NAME'
                                   PS_DATA-MAIN-ADDR-NAME_CO
                          CHANGING LS_ADDRESS LS_ADDRESS_X.

  IF LS_ADDRESS-LANGU IS INITIAL.
    LS_ADDRESS-LANGU = '2'. "Default TH language
  ENDIF.

  "Only change Address type upload space so we have to mark X at ls_address_x
  LS_ADDRESS_X-CITY              = GC_TRUE .
  LS_ADDRESS_X-DISTRICT          = GC_TRUE .
  LS_ADDRESS_X-EXTADDRESSNUMBER  = GC_TRUE .
  LS_ADDRESS_X-POSTL_COD1        = GC_TRUE .
  LS_ADDRESS_X-STREET            = GC_TRUE .
  LS_ADDRESS_X-STR_SUPPL1        = GC_TRUE .
  LS_ADDRESS_X-STR_SUPPL2        = GC_TRUE .
  LS_ADDRESS_X-STR_SUPPL3        = GC_TRUE .
  LS_ADDRESS_X-LOCATION          = GC_TRUE .
  LS_ADDRESS_X-COUNTRY           = GC_TRUE .
  LS_ADDRESS_X-COMM_TYPE         = GC_TRUE .
  LS_ADDRESS_X-C_O_NAME          = GC_TRUE .

* Phone number
  CLEAR LT_ADTEL2.
  IF PS_DATA-MAIN-ADDR-PHONE-TELNO1 IS NOT INITIAL.
    LS_ADTEL-R_3_USER = '1'.
    LS_ADTEL-HOME_FLAG = 'X'.
    LS_ADTEL-COUNTRY   = PS_DATA-MAIN-ADDR-COUNTRY.
    LS_ADTEL-TELEPHONE = PS_DATA-MAIN-ADDR-PHONE-TELNO1.
    LS_ADTEL-EXTENSION = PS_DATA-MAIN-ADDR-PHONE-TELEXT1 .
    APPEND LS_ADTEL TO LT_ADTEL2.

    LS_ADTEL_X-R_3_USER  = ABAP_TRUE.
    LS_ADTEL_X-HOME_FLAG = ABAP_TRUE.
    LS_ADTEL_X-COUNTRY   = ABAP_TRUE.
    LS_ADTEL_X-TELEPHONE = ABAP_TRUE.
    LS_ADTEL_X-EXTENSION = ABAP_TRUE.

    READ TABLE LT_ADTEL TRANSPORTING NO FIELDS WITH KEY R_3_USER = '1'
                                                        HOME_FLAG = 'X'.
    IF SY-SUBRC EQ 0.
      LS_ADTEL_X-UPDATEFLAG = 'U'.
    ELSE.
      LS_ADTEL_X-UPDATEFLAG = 'I'.
    ENDIF.
    APPEND LS_ADTEL_X TO LT_ADTEL_X.
  ENDIF.

* Mobile number
  CLEAR LS_ADTEL.
  IF PS_DATA-MAIN-ADDR-PHONE-MOBILE IS NOT INITIAL.
    LS_ADTEL-R_3_USER = '3'.
    LS_ADTEL-HOME_FLAG = 'X'.
    LS_ADTEL-COUNTRY   = PS_DATA-MAIN-ADDR-COUNTRY.
    LS_ADTEL-TELEPHONE = PS_DATA-MAIN-ADDR-PHONE-MOBILE.
    APPEND LS_ADTEL TO LT_ADTEL2.

    LS_ADTEL_X-R_3_USER  = ABAP_TRUE.
    LS_ADTEL_X-HOME_FLAG = ABAP_TRUE.
    LS_ADTEL_X-COUNTRY   = ABAP_TRUE.
    LS_ADTEL_X-TELEPHONE = ABAP_TRUE.

    READ TABLE LT_ADTEL TRANSPORTING NO FIELDS WITH KEY R_3_USER = '3'
                                                        HOME_FLAG = 'X'.
    IF SY-SUBRC EQ 0.
      LS_ADTEL_X-UPDATEFLAG = 'U'.
    ELSE.
      LS_ADTEL_X-UPDATEFLAG = 'I'.
    ENDIF.
    APPEND LS_ADTEL_X TO LT_ADTEL_X.
  ENDIF.

** Phone for inter version
*  CLEAR: ls_adtel, ls_adtel_x.
*  IF ps_data-main-addr_int-phone-telno1 IS NOT INITIAL.
*    CLEAR ls_adtel-r_3_user.
*    CLEAR ls_adtel-home_flag.
*    ls_adtel-country = cs_alv-country_i.
*    ls_adtel-telephone = cs_alv-tel_number_i.
*    ls_adtel-extension = cs_alv-tel_extens_i.
*    APPEND ls_adtel TO lt_adtel2.
*
*    ls_adtel_x-r_3_user = abap_true.
*    ls_adtel_x-home_flag = abap_true.
*    ls_adtel_x-country = abap_true.
*    ls_adtel_x-telephone = abap_true.
*    ls_adtel_x-extension = abap_true.
*    READ TABLE lt_adtel TRANSPORTING NO FIELDS
*    WITH KEY r_3_user = space
*    home_flag = space.
*    IF sy-subrc EQ 0.
*      ls_adtel_x-updateflag = 'U'.
*    ELSE.
*      ls_adtel_x-updateflag = 'I'.
*    ENDIF.
*    APPEND ls_adtel_x TO lt_adtel_x.
*  ENDIF.
*
*  CLEAR: ls_adtel, ls_adtel_x.
*  IF cs_alv-mobile_i IS NOT INITIAL.
*    ls_adtel-r_3_user = '2'.
*    ls_adtel-home_flag = space.
*    ls_adtel-country = cs_alv-country_i.
*    ls_adtel-telephone = cs_alv-mobile_i.
*    CLEAR ls_adtel-extension.
*    APPEND ls_adtel TO lt_adtel2.
*
*    ls_adtel_x-r_3_user = abap_true.
*    ls_adtel_x-home_flag = abap_true.
*    ls_adtel_x-country = abap_true.
*    ls_adtel_x-telephone = abap_true.
*    ls_adtel_x-extension = abap_true.
*    READ TABLE lt_adtel TRANSPORTING NO FIELDS
*    WITH KEY r_3_user = '2'.
*    IF sy-subrc EQ 0.
*      ls_adtel_x-updateflag = 'U'.
*    ELSE.
*      ls_adtel_x-updateflag = 'I'.
*    ENDIF.
*    APPEND ls_adtel_x TO lt_adtel_x.
*  ENDIF.

* Email
  IF PS_DATA-MAIN-ADDR-SMTP-EMAIL1 IS NOT INITIAL.
    LS_MAIL-E_MAIL = PS_DATA-MAIN-ADDR-SMTP-EMAIL1.
    LS_MAIL-HOME_FLAG = 'X'.
    LS_MAIL-STD_NO = 'X'.
    APPEND LS_MAIL TO LT_MAIL2.

    LS_ADSMT_X-HOME_FLAG = ABAP_TRUE.
    LS_ADSMT_X-E_MAIL = ABAP_TRUE.
    LS_ADSMT_X-STD_NO = ABAP_TRUE.
    READ TABLE LT_MAIL TRANSPORTING NO FIELDS  WITH KEY STD_NO = 'X'
                                                        HOME_FLAG = 'X'.
    IF SY-SUBRC EQ 0.
      LS_ADSMT_X-UPDATEFLAG = 'U'.
    ELSE.
      LS_ADSMT_X-UPDATEFLAG = 'I'.
    ENDIF.
    APPEND LS_ADSMT_X TO LT_ADSMT_X.
  ENDIF.

*  CLEAR: ls_mail, ls_adsmt_x.
*  IF cs_alv-smtp_addr_i IS NOT INITIAL.
*    ls_mail-e_mail = cs_alv-smtp_addr_i.
*    ls_mail-home_flag = space.
*    APPEND ls_mail TO lt_mail2.
*
*    ls_adsmt_x-home_flag = abap_true.
*    ls_adsmt_x-e_mail = abap_true.
*    ls_adsmt_x-std_no = abap_true.
*    READ TABLE lt_mail TRANSPORTING NO FIELDS
*    WITH KEY std_no = space
*             home_flag = space.
*    IF sy-subrc EQ 0.
*      ls_adsmt_x-updateflag = 'U'.
*    ELSE.
*      ls_adsmt_x-updateflag = 'I'.
*    ENDIF.
*    APPEND ls_adsmt_x TO lt_adsmt_x.
*  ENDIF.

* Fax number
  IF  PS_DATA-MAIN-ADDR-FAX-FAXNO IS NOT INITIAL.
    LS_ADFAX-COUNTRY = PS_DATA-MAIN-ADDR-COUNTRY.
    LS_ADFAX-FAX     = PS_DATA-MAIN-ADDR-FAX-FAXNO.
    LS_ADFAX-EXTENSION = PS_DATA-MAIN-ADDR-FAX-FAXEXT.
    LS_ADFAX-HOME_FLAG = 'X'.
    APPEND LS_ADFAX TO LT_ADFAX2.

    LS_ADFAX_X-HOME_FLAG = ABAP_TRUE.
    LS_ADFAX_X-COUNTRY = ABAP_TRUE.
    LS_ADFAX_X-FAX = ABAP_TRUE.
    LS_ADFAX_X-EXTENSION = ABAP_TRUE.
    READ TABLE LT_ADFAX TRANSPORTING NO FIELDS WITH KEY HOME_FLAG = 'X'.
    IF SY-SUBRC EQ 0.
      LS_ADFAX_X-UPDATEFLAG = 'U'.
    ELSE.
      LS_ADFAX_X-UPDATEFLAG = 'I'.
    ENDIF.
    APPEND LS_ADFAX_X TO LT_ADFAX_X.
  ENDIF.

** Fax for inter version
*  CLEAR: ls_adfax, ls_adfax_x.
*  IF cs_alv-fax_number_i IS NOT INITIAL.
*    ls_adfax-country = cs_alv-country_i.
*    ls_adfax-fax = cs_alv-fax_number_i.
*    ls_adfax-extension = cs_alv-fax_extens_i.
*    ls_adfax-home_flag = space.
*    APPEND ls_adfax TO lt_adfax2.
*
*    ls_adfax_x-home_flag = abap_true.
*    ls_adfax_x-country = abap_true.
*    ls_adfax_x-fax = abap_true.
*    ls_adfax_x-extension = abap_true.
*    READ TABLE lt_adfax TRANSPORTING NO FIELDS
*    WITH KEY home_flag = space.
*    IF sy-subrc EQ 0.
*      ls_adfax_x-updateflag = 'U'.
*    ELSE.
*      ls_adfax_x-updateflag = 'I'.
*    ENDIF.
*    APPEND ls_adfax_x TO lt_adfax_x.
*  ENDIF.

* Comment
  IF PS_DATA-MAIN-ADDR-REMARK IS NOT INITIAL.
    LS_NOTE-LANGU = SY-LANGU.
    LS_NOTE-ADR_NOTES = PS_DATA-MAIN-ADDR-REMARK.
    APPEND LS_NOTE TO LT_NOTE2.

    LS_NOTE_X-LANGU = ABAP_TRUE.
    LS_NOTE_X-ADR_NOTES = ABAP_TRUE.
    READ TABLE LT_NOTE TRANSPORTING NO FIELDS WITH KEY LANGU = SY-LANGU.
    IF SY-SUBRC EQ 0.
      LS_NOTE_X-UPDATEFLAG = 'U'.
    ELSE.
      LS_NOTE_X-UPDATEFLAG = 'I'.
    ENDIF.
    APPEND LS_NOTE_X TO LT_NOTE_X.
  ENDIF.

  CLEAR LT_RETURN.
  CALL FUNCTION 'BAPI_BUPA_ADDRESS_CHANGE'
    EXPORTING
      BUSINESSPARTNER = PS_DATA-MAIN-PARTNER
      ADDRESSGUID     = LV_GUID
      ADDRESSDATA     = LS_ADDRESS
      ADDRESSDATA_X   = LS_ADDRESS_X
*     DUPLICATE_MESSAGE_TYPE       =
*     ACCEPT_ERROR    = ' '
    TABLES
      BAPIADTEL       = LT_ADTEL2
      BAPIADFAX       = LT_ADFAX
      BAPIADSMTP      = LT_MAIL2
      BAPIAD_REM      = LT_NOTE2
      BAPIADTEL_X     = LT_ADTEL_X
      BAPIADFAX_X     = LT_ADFAX_X
      BAPIADSMT_X     = LT_ADSMT_X
      BAPIAD_RE_X     = LT_NOTE_X
      RETURN          = LT_RETURN.

  LOOP AT LT_RETURN INTO LS_RETURN WHERE TYPE CA 'AEX' ##INTO_OK.

    PV_ERROR = GC_TRUE.
    CLEAR LS_MESSG.
    LS_MESSG-MSGID = LS_RETURN-ID.
    LS_MESSG-MSGNO = LS_RETURN-NUMBER.
    LS_MESSG-MSGTY = LS_RETURN-TYPE.
    MESSAGE ID   LS_RETURN-ID
            TYPE LS_RETURN-TYPE
            NUMBER LS_RETURN-NUMBER
            WITH LS_RETURN-MESSAGE_V1
                 LS_RETURN-MESSAGE_V2
                 LS_RETURN-MESSAGE_V3
                 LS_RETURN-MESSAGE_V4
            INTO LS_MESSG-MSGTX.
    INSERT LS_MESSG INTO TABLE PT_MESSG.
    RETURN.
  ENDLOOP.


  DATA: LT_ADVER  TYPE BAPIAD1VD_T,
        LT_ADVERX TYPE BAPIAD1VDX_T,
        LS_ADVER  TYPE BAPIAD1VD,
        LS_ADVERX TYPE BAPIAD1VDX.

  IF PS_DATA-MAINX-ADDR_INT IS NOT INITIAL.

* Add international version
    LS_ADVER-ADDR_VERS = 'I'.
    LS_ADVER-TITLE      = PS_DATA-MAIN-ADDR_INT-TITLE.
    LS_ADVER-NAME       = PS_DATA-MAIN-ADDR_INT-NAME1.
    LS_ADVER-NAME_2     = PS_DATA-MAIN-ADDR_INT-NAME2.
    LS_ADVER-NAME_3     = PS_DATA-MAIN-ADDR_INT-NAME3.
    LS_ADVER-NAME_4     = PS_DATA-MAIN-ADDR_INT-NAME4.
    LS_ADVER-C_O_NAME   = PS_DATA-MAIN-ADDR_INT-NAME_CO.
    LS_ADVER-STR_SUPPL1 = PS_DATA-MAIN-ADDR_INT-STR_SUPPL1.
    LS_ADVER-STR_SUPPL2 = PS_DATA-MAIN-ADDR_INT-STR_SUPPL2.
    LS_ADVER-CITY       = PS_DATA-MAIN-ADDR_INT-CITY1.
    LS_ADVER-DISTRICT   = PS_DATA-MAIN-ADDR_INT-CITY2.
    LS_ADVER-STREET     = PS_DATA-MAIN-ADDR_INT-STREET.
    LS_ADVER-STR_SUPPL3 = PS_DATA-MAIN-ADDR_INT-STR_SUPPL3.
    LS_ADVER-LOCATION   = PS_DATA-MAIN-ADDR_INT-LOCATION.
    LS_ADVER-COUNTY     = PS_DATA-MAIN-ADDR_INT-COUNTRY.
    LS_ADVER-SORT1      = PS_DATA-MAIN-ADDR_INT-SORT1.
    LS_ADVER-SORT2      = PS_DATA-MAIN-ADDR_INT-SORT2.
    APPEND LS_ADVER TO LT_ADVER.

    LS_ADVERX-ADDR_VERS = 'X'.
    LS_ADVERX-TITLE = 'X'.
    LS_ADVERX-NAME = 'X'.
    LS_ADVERX-NAME_2 = 'X'.
    LS_ADVERX-NAME_3 = 'X'.
    LS_ADVERX-NAME_4 = 'X'.
    LS_ADVERX-C_O_NAME = 'X'.
    LS_ADVERX-STR_SUPPL1 = 'X'.
    LS_ADVERX-STR_SUPPL2 = 'X'.
    LS_ADVERX-STR_SUPPL3 = 'X'.
    LS_ADVERX-STREET = 'X'.
    LS_ADVERX-LOCATION = 'X'.
    LS_ADVERX-DISTRICT = 'X'.
    LS_ADVERX-CITY = 'X'.
    LS_ADVERX-COUNTY = 'X'.
    LS_ADVERX-UPDATEFLAG = 'I'.
    APPEND LS_ADVERX TO LT_ADVERX.

    CLEAR: LS_NOTE , LT_NOTE2[], LT_NOTE_X[] .
    IF PS_DATA-MAIN-ADDR_INT-REMARK IS NOT INITIAL.

      LS_NOTE-LANGU = SY-LANGU.
      LS_NOTE-ADR_NOTES = PS_DATA-MAIN-ADDR_INT-REMARK.
      APPEND LS_NOTE TO LT_NOTE2.

      LS_NOTE_X-LANGU = ABAP_TRUE.
      LS_NOTE_X-ADR_NOTES = ABAP_TRUE.
      READ TABLE LT_NOTE TRANSPORTING NO FIELDS WITH KEY LANGU = SY-LANGU
                                                         ADDR_VERS = 'I' .
      IF SY-SUBRC EQ 0.
        LS_NOTE_X-UPDATEFLAG = 'U'.
      ELSE.
        LS_NOTE_X-UPDATEFLAG = 'I'.
      ENDIF.
      APPEND LS_NOTE_X TO LT_NOTE_X.
    ENDIF.

    CLEAR LT_RETURN[].
    CALL FUNCTION 'BAPI_BUPA_ADDRESS_CHANGE'
      EXPORTING
        BUSINESSPARTNER = PS_DATA-MAIN-PARTNER
        ADDRESSGUID     = LV_GUID
      TABLES
        BAPIADVERSORG   = LT_ADVER
        BAPIADVERSORG_X = LT_ADVERX
        BAPIAD_REM      = LT_NOTE2
        BAPIAD_RE_X     = LT_NOTE_X
        RETURN          = LT_RETURN.

    LOOP AT LT_RETURN INTO LS_RETURN WHERE TYPE CA 'AEX' ##INTO_OK.

      PV_ERROR = GC_TRUE.
      CLEAR LS_MESSG.
      LS_MESSG-MSGID = LS_RETURN-ID.
      LS_MESSG-MSGNO = LS_RETURN-NUMBER.
      LS_MESSG-MSGTY = LS_RETURN-TYPE.
      MESSAGE ID   LS_RETURN-ID
              TYPE LS_RETURN-TYPE
              NUMBER LS_RETURN-NUMBER
              WITH LS_RETURN-MESSAGE_V1
                   LS_RETURN-MESSAGE_V2
                   LS_RETURN-MESSAGE_V3
                   LS_RETURN-MESSAGE_V4
              INTO LS_MESSG-MSGTX.
      INSERT LS_MESSG INTO TABLE PT_MESSG.
      RETURN.
    ENDLOOP.
  ENDIF.

  IF PV_ERROR IS INITIAL .

    IF PV_TEST EQ GC_TRUE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'S'.
      LS_MESSG-MSGID = 'ZTEC'.
      LS_MESSG-MSGNO = '000'.
*     Text-i01: Test upload successfully.
      LS_MESSG-MSGTX = TEXT-I01.
      INSERT LS_MESSG INTO TABLE PT_MESSG.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = GC_TRUE.
  ENDIF.
ENDFORM.
