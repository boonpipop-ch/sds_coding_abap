*&---------------------------------------------------------------------*
*& Include          ZSDSCAR0050_F03
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_process_data_tmp24
*&---------------------------------------------------------------------*
*& *  Processing data for Template 24 Mail(Customer)
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA_TMP24 CHANGING PT_RESULT TYPE GTTY_RESULT24
                                    PS_SUM     TYPE  GTY_SUM.

  DATA:
    LT_RAW  TYPE  GTTY_RAW,
    LT_DATA TYPE  GTTY_DATA24.

  DATA:
    LS_MESSG  TYPE  GTY_MESSG,
    LS_RESULT TYPE  GTY_RESULT24.


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
  PERFORM F_VALIDATE_FILE_TMP24  USING  LT_RAW
                               CHANGING LT_DATA.

* --------------------------------
* Step3: Upload data from file
* --------------------------------
* Upload file
  PERFORM F_UPLOAD_FILE_TMP24  USING  LT_DATA
                                      CB_TEST
                             CHANGING PT_RESULT
                                      PS_SUM.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_file_tmp24
*&---------------------------------------------------------------------*
*& Validate File data for Template 24
*&---------------------------------------------------------------------*
FORM F_VALIDATE_FILE_TMP24 USING PT_RAW TYPE GTTY_RAW
                          CHANGING PT_DATA    TYPE  GTTY_DATA24.

  DATA:
    LS_MAIN  TYPE  GTY_MAIN24,
    LS_MAINX TYPE  GTY_MAIN24X,
    LS_MESSG TYPE  GTY_MESSG.

  DATA:
    LV_ROWNO   TYPE  GTY_RESULT24-ROWNO.

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
    PERFORM F_TRANSLATE_RAW24  USING  <LFS_RAW>
                             CHANGING LS_MAIN
                                      LS_MAINX
                                      LS_MESSG.

*   Validate and Append data into table
    PERFORM F_VALIDATE_AND_APPEND24 USING  LV_ROWNO
                                           LS_MAIN
                                           LS_MAINX
                                           LS_MESSG
                                  CHANGING PT_DATA.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_translate_raw24
*&---------------------------------------------------------------------*
*& Convert Raw to Data24
*&---------------------------------------------------------------------*
FORM F_TRANSLATE_RAW24 USING PS_RAW TYPE GTY_RAW
                     CHANGING PS_MAIN      TYPE  GTY_MAIN24
                              PS_MAINX     TYPE  GTY_MAIN24X
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
    PERFORM F_GET_TARGET_FIELD24   USING LV_INDEX
                                CHANGING LV_FIELD.

    CASE LV_FIELD.

      WHEN 'RLTYP'.
        PERFORM F_VALIDATE_ROLE24   USING LV_STRING
                                 CHANGING PS_MAIN-RLTYP
                                          LV_MSGTX.

      WHEN 'PARTNER'.
        PERFORM F_VALIDATE_PARTNER  USING  LV_STRING
                                           GC_MODE_CHANGE
                                  CHANGING PS_MAIN-PARTNER
                                           PS_MAIN-PARTNER_GUID
                                           LV_MSGTX.
      WHEN 'ADDR_ADR_KIND'.
        PS_MAIN-ADDR_ADR_KIND  = LV_STRING.

      WHEN 'ADDR_ADEXT'.
        PS_MAIN-ADDR_ADEXT  = LV_STRING.
        PS_MAINX-ADDR_ADEXT = GC_TRUE.

      WHEN 'ADDR_REMARK'.
        PS_MAIN-ADDR_REMARK  = LV_STRING.
        PS_MAINX-ADDR_REMARK = GC_TRUE.

      WHEN 'ADDR_VALID_FROM'.
        PERFORM F_VALIDATE_DATE  USING  LV_STRING
                               CHANGING PS_MAIN-ADDR_VALID_FROM
                                        LV_MSGTX.
        PS_MAINX-ADDR_VALID_FROM = GC_TRUE.
      WHEN 'ADDR_VALID_TO'.
        PERFORM F_VALIDATE_DATE  USING  LV_STRING
                               CHANGING PS_MAIN-ADDR_VALID_TO
                                        LV_MSGTX.
        PS_MAINX-ADDR_VALID_TO = GC_TRUE.

      WHEN 'ADDR_EMAIL'.
        PS_MAIN-ADDR_EMAIL  = LV_STRING.
        PS_MAINX-ADDR_EMAIL = GC_TRUE.

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
*& Form f_get_target_field24
*&---------------------------------------------------------------------*
*& Get Target Field name based on column sequence
*&---------------------------------------------------------------------*
FORM F_GET_TARGET_FIELD24 USING PV_INDEX TYPE I
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
    LREF_DESCR = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( 'GTY_TEMPLATE24' ).
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
*& Form f_validate_role24
*&---------------------------------------------------------------------*
*& Validate BP Role for Template 24
*&---------------------------------------------------------------------*

FORM F_VALIDATE_ROLE24 USING PV_STRING TYPE STRING
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
*& Form f_validate_and_append24
*&---------------------------------------------------------------------*
*& Validate Whole Line data and collect into internal table for posting
*&---------------------------------------------------------------------*
FORM F_VALIDATE_AND_APPEND24 USING PV_ROWNO TYPE GTY_RESULT24-ROWNO
                                    PS_MAIN   TYPE  GTY_MAIN24
                                    PS_MAINX  TYPE  GTY_MAIN24X
                                    PS_MESSG  TYPE  GTY_MESSG
                           CHANGING PT_DATA   TYPE  GTTY_DATA24.

  DATA:
    LS_MAIN  TYPE  GTY_MAIN24,
    LS_MAINX TYPE  GTY_MAIN24X,
    LS_DATA  TYPE  GTY_DATA24,
    LS_MESSG TYPE  GTY_MESSG.


* Initial Data
  LS_MAIN  = PS_MAIN.
  LS_MAINX = PS_MAINX.

  DO 1 TIMES.

*  ---------------------------------
*   Validate Main data
*  ---------------------------------
    PERFORM F_VALIDATE_MAIN24  CHANGING LS_MAIN
                                        LS_MAINX
                                        LS_MESSG.
    IF LS_MESSG IS NOT INITIAL.
      EXIT.
    ENDIF.

  ENDDO.

* Generate new key
  CLEAR LS_DATA.
  LS_DATA-ROWNO = PV_ROWNO.
*  ls_data-main  = ls_main.
  LS_DATA-ALTKN           = LS_MAIN-ALTKN .
  LS_DATA-PARTNER         = LS_MAIN-PARTNER.
  LS_DATA-RLTYP           = LS_MAIN-RLTYP .
  LS_DATA-ADDR_ADR_KIND   = LS_MAIN-ADDR_ADR_KIND.
  LS_DATA-ADDR_ADEXT      = LS_MAIN-ADDR_ADEXT .
  LS_DATA-ADDR_REMARK     = LS_MAIN-ADDR_REMARK .
  LS_DATA-ADDR_VALID_FROM = LS_MAIN-ADDR_VALID_FROM .
  LS_DATA-ADDR_VALID_TO   = LS_MAIN-ADDR_VALID_TO .
  LS_DATA-ADDR_EMAIL      = LS_MAIN-ADDR_EMAIL .

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
*& Form f_validate_main24
*&---------------------------------------------------------------------*
*& Validate Partner data
*&---------------------------------------------------------------------*
FORM F_VALIDATE_MAIN24 CHANGING PS_MAIN TYPE GTY_MAIN24
                                 PS_MAINX TYPE  GTY_MAIN24X ##NEEDED
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

** Check Customer Exist?
*  PERFORM f_get_customer_from_partner  USING  ps_main-partner
*                                     CHANGING ps_main-kunnr.
*  IF ps_main-kunnr IS INITIAL.
*    ps_messg-msgty = 'E'.
*    ps_messg-msgid = 'ZTEC'.
*    ps_messg-msgno = '000'.
**   Text-e82 : Cannot find customer code for partner
*    CONCATENATE TEXT-e82 ps_main-partner
*           INTO ps_messg-msgtx
*      SEPARATED BY space.
*    RETURN.
*  ENDIF.

* Get address no.
  SELECT ADDRNUMBER                     "#EC CI_NOORDER  "#EC CI_SEL_NESTED
    INTO PS_MAIN-ADDR_ADDRNUMBER UP TO 1 ROWS
    FROM BUT020
   WHERE PARTNER  EQ  PS_MAIN-PARTNER .
  ENDSELECT.
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
*& Form f_upload_file_tmp24
*&---------------------------------------------------------------------*
*& Upload File Template 24
*&---------------------------------------------------------------------*
FORM F_UPLOAD_FILE_TMP24 USING PT_DATA TYPE GTTY_DATA24
                                 PV_TEST  TYPE  FLAG
                        CHANGING PT_RESULT TYPE  GTTY_RESULT24
                                 PS_SUM    TYPE  GTY_SUM.

  DATA:
    LT_MESSG TYPE  GTTY_MESSG.

  DATA:
    LS_DATA  TYPE  GTY_DATA24.

  DATA:
    LV_ERROR    TYPE FLAG,
    LV_ADDRGUID TYPE BAPIBUS1006_ADDRESSES_INT-ADDRGUID.

  DATA:
    LT_DATA     TYPE GTTY_DATA24,
    LT_ADR_KIND TYPE GTTY_DATA24,
    LS_ADR_KIND TYPE GTY_DATA24.


  LT_DATA[] = PT_DATA[] .


* Initialize Output
  CLEAR: PT_RESULT.
  CLEAR:   PS_SUM.

* Show Progress
* Text-p03 : Uploading file data. . .
  MC_SHOW_PROGRESS 45 TEXT-P03.


* Delete duplicate email
  SORT LT_DATA BY ROWNO
                  PARTNER
                  ADDR_ADR_KIND
                  ADDR_EMAIL  .

  DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING PARTNER
                                                    ADDR_ADR_KIND
                                                    ADDR_EMAIL .
*-Add email same partner and address type.
  LT_ADR_KIND[] = LT_DATA[] .
  DELETE ADJACENT DUPLICATES FROM LT_ADR_KIND COMPARING PARTNER
                                                        ADDR_ADR_KIND .

  LOOP AT LT_ADR_KIND INTO LS_ADR_KIND .

*  Get address guid
    CALL FUNCTION 'BAPI_BUPA_ADDRESSES_GET'
      EXPORTING
        BUSINESSPARTNER     = LS_ADR_KIND-PARTNER
*       OPERATION           =
        ADDRESSTYPE         = LS_ADR_KIND-ADDR_ADR_KIND
*       VALID_DATE          = SY-DATLO
      IMPORTING
        STANDARDADDRESSGUID = LV_ADDRGUID.

*   Only non validation error
    IF LS_ADR_KIND-MESSG[] IS INITIAL.

      PERFORM F_ADD_EMAIL USING  LS_ADR_KIND
                                 LV_ADDRGUID
                                 PV_TEST
                        CHANGING LT_DATA
                                 LT_MESSG
                                 LV_ERROR .


      " Processing each record
      LOOP AT LT_DATA INTO LS_DATA WHERE PARTNER       = LS_ADR_KIND-PARTNER
                                     AND ADDR_ADR_KIND = LS_ADR_KIND-ADDR_ADR_KIND.

        LS_DATA-MESSG = LT_MESSG .

        PS_SUM-TOTAL = PS_SUM-TOTAL + 1.

        IF LV_ERROR EQ GC_TRUE.
          PS_SUM-ERROR = PS_SUM-ERROR + 1.
        ELSE.
          PS_SUM-SUCCS = PS_SUM-SUCCS + 1.
        ENDIF.

*       Collect Result
        PERFORM F_COLLECT_RESULT24  USING  LS_DATA
                                  CHANGING PT_RESULT.

      ENDLOOP.


    ENDIF.
  ENDLOOP .



* Show Final Message for Processing completed
* Message: Processing completed.
  MESSAGE S582(1FA).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_collect_result24
*&---------------------------------------------------------------------*
*& Collect Result from Processing
*&---------------------------------------------------------------------*
FORM F_COLLECT_RESULT24 USING PS_DATA TYPE GTY_DATA24
                      CHANGING PT_RESULT TYPE  GTTY_RESULT24.

  DATA:
    LS_RESULT  TYPE  GTY_RESULT24.

  FIELD-SYMBOLS:
    <LFS_MESSG> TYPE  GTY_MESSG.


  CLEAR: LS_RESULT.

  LS_RESULT-ROWNO   = PS_DATA-ROWNO.
  LS_RESULT-RLTYP =  PS_DATA-RLTYP.
  LS_RESULT-PARTNER =  PS_DATA-PARTNER.
  LS_RESULT-ADDR_EMAIL1 =  PS_DATA-ADDR_EMAIL.
  LS_RESULT-ADDR_REMARK =  PS_DATA-ADDR_REMARK.
  LS_RESULT-ADDR_ADR_KIND   = PS_DATA-ADDR_ADR_KIND  .
  LS_RESULT-ADDR_ADEXT      = PS_DATA-ADDR_ADEXT  .
  LS_RESULT-ADDR_VALID_FROM = PS_DATA-ADDR_VALID_FROM   .
  LS_RESULT-ADDR_VALID_TO   = PS_DATA-ADDR_VALID_TO  .

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
*& Form f_display_result24
*&---------------------------------------------------------------------*
*& Display Result
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT24 USING PT_RESULT TYPE GTTY_RESULT24.

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
  PERFORM F_ALV_LAYOUT24 CHANGING GS_LAYOUT_1
                                  GS_VARIANT_1
                                  GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_24.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_24.
  ASSIGN PT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2215424]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT24 CHANGING GT_FIELDCAT_1.
* Sort data
  PERFORM F_ALV_SORT_RESULT24 CHANGING GT_SORT_1.
* Call ALV Screen
  CALL SCREEN 9000.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_layout24
*&---------------------------------------------------------------------*
*& ALV Layout for Report
*&---------------------------------------------------------------------*
FORM F_ALV_LAYOUT24 CHANGING PS_LAYOUT TYPE LVC_S_LAYO
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
*& Form f_alv_build_fieldcat24
*&---------------------------------------------------------------------*
*& ALV Field Catalog for Report email(Customer)
*&---------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT24 CHANGING PT_FIELDCAT TYPE LVC_T_FCAT.

  DATA:
    LV_TEXT         TYPE  TEXT50.

  FIELD-SYMBOLS:
    <LFS_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_24
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
      WHEN 'ADDR_ADR_KIND'.
      WHEN 'ADEXT'  .
      WHEN 'ADDR_VALID_FROM'.
      WHEN 'ADDR_VALID_TO'.

      WHEN 'ADDR_REMARK'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ADDR_EMAIL1'.
        <LFS_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN OTHERS.
        <LFS_FIELDCAT>-TECH = GC_TRUE.

    ENDCASE.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_sort_result24
*&---------------------------------------------------------------------*
*& Maintain SORT data for Result ALV
*&---------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT24 CHANGING PT_SORT TYPE LVC_T_SORT.

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
*& Form f_add_email
*&---------------------------------------------------------------------*
*& Add email
*&---------------------------------------------------------------------*
FORM F_ADD_EMAIL USING PS_ADR_KIND TYPE GTY_DATA24
                         PV_ADDRGUID TYPE BU_ADDRESS_GUID
                         PV_TEST     TYPE  FLAG
                CHANGING PT_DATA     TYPE  GTTY_DATA24
                         PT_MESSG    TYPE  GTTY_MESSG
                         PV_ERROR    TYPE  FLAG.

  DATA:
    LS_DATA    TYPE GTY_DATA24,
    LT_MAIL    TYPE BAPIADSMTP_T,
    LT_MAIL2   TYPE BAPIADSMTP_T,

    LT_NOTE    TYPE BP_AD_REM_TTY,
    LT_NOTE2   TYPE BP_AD_REM_TTY,

    LT_ADSMT_X TYPE BAPIADSMTX_T,
    LT_NOTE_X  TYPE BAPIAD_REX_T,
    LT_RETURN  TYPE TABLE OF BAPIRET2,

    LS_MAIL    TYPE BAPIADSMTP,
    LS_NOTE    TYPE BAPIAD_REM,
    LS_ADSMT_X TYPE BAPIADSMTX,
    LS_NOTE_X  TYPE BAPIAD_REX,
    LS_RETURN  TYPE BAPIRET2,
    LS_MESSG   TYPE GTY_MESSG.

  DATA :
    LV_ADDRGUID TYPE  BUT020-GUID .

 LV_ADDRGUID  = PV_ADDRGUID .

* Update Address
  CLEAR LT_RETURN.
  CALL FUNCTION 'BAPI_BUPA_ADDRESS_GETDETAIL'
    EXPORTING
      BUSINESSPARTNER = PS_ADR_KIND-PARTNER
      ADDRESSGUID     = LV_ADDRGUID
*     VALID_DATE      = SY-DATLO
*     RESET_BUFFER    =
* IMPORTING
*     ADDRESSDATA     =
    TABLES
*     BAPIADTTX       =
*     BAPIADTLX       =
      BAPIADSMTP      = LT_MAIL
*     BAPIADRML       =
*     BAPIADX400      =
*     BAPIADRFC       =
*     BAPIADPRT       =
*     BAPIADSSF       =
*     BAPIADURI       =
*     BAPIADPAG       =
      BAPIAD_REM      = LT_NOTE
*     BAPICOMREM      =
*     ADDRESSUSAGE    =
*     BAPIADVERSORG   =
*     BAPIADVERSPERS  =
*     BAPIADUSE       =
      RETURN          = LT_RETURN.

* Email
  LOOP AT PT_DATA INTO LS_DATA WHERE PARTNER  = PS_ADR_KIND-PARTNER ##INTO_OK
                               AND   ADDR_ADR_KIND = PS_ADR_KIND-ADDR_ADR_KIND .
    CLEAR: LS_MAIL, LS_ADSMT_X.

    LS_MAIL-E_MAIL = LS_DATA-ADDR_EMAIL.
    LS_MAIL-HOME_FLAG  = 'X'.
    LS_MAIL-STD_NO     = 'X'.
    LS_MAIL-VALID_FROM = LS_DATA-ADDR_VALID_FROM .
    LS_MAIL-VALID_TO   = LS_DATA-ADDR_VALID_TO .
    APPEND LS_MAIL TO LT_MAIL2.

    LS_ADSMT_X-HOME_FLAG  = ABAP_TRUE.
    LS_ADSMT_X-E_MAIL     = ABAP_TRUE.
    LS_ADSMT_X-STD_NO     = ABAP_TRUE.
    LS_ADSMT_X-VALID_FROM = ABAP_TRUE.
    LS_ADSMT_X-VALID_TO   = ABAP_TRUE.
    READ TABLE LT_MAIL TRANSPORTING NO FIELDS
*                WITH KEY std_no = 'X'
*                         home_flag = 'X'.
                       WITH KEY E_MAIL = LS_DATA-ADDR_EMAIL.
    IF SY-SUBRC EQ 0.
      LS_ADSMT_X-UPDATEFLAG = 'U'.
    ELSE.
      LS_ADSMT_X-UPDATEFLAG = 'I'.
    ENDIF.
    APPEND LS_ADSMT_X TO LT_ADSMT_X.

  ENDLOOP.

* Set Remark
  CLEAR LS_DATA .
  SORT PT_DATA BY PARTNER ADDR_ADR_KIND .
  READ TABLE PT_DATA INTO LS_DATA  WITH KEY PARTNER  = PS_ADR_KIND-PARTNER
                                            ADDR_ADR_KIND = PS_ADR_KIND-ADDR_ADR_KIND
                                   BINARY SEARCH.
  IF SY-SUBRC = 0  AND LS_DATA-ADDR_REMARK IS NOT INITIAL.
    LS_NOTE-LANGU = SY-LANGU.
    LS_NOTE-ADR_NOTES = LS_DATA-ADDR_REMARK.
    APPEND LS_NOTE TO LT_NOTE2.

    LS_NOTE_X-LANGU = ABAP_TRUE.
    LS_NOTE_X-ADR_NOTES = ABAP_TRUE.
    READ TABLE LT_NOTE TRANSPORTING NO FIELDS
    WITH KEY LANGU = SY-LANGU.
    IF SY-SUBRC EQ 0.
      LS_NOTE_X-UPDATEFLAG = 'U'.
    ELSE.
      LS_NOTE_X-UPDATEFLAG = 'I'.
    ENDIF.
    APPEND LS_NOTE_X TO LT_NOTE_X.
  ENDIF .

  IF LT_ADSMT_X[] IS NOT INITIAL.
    CLEAR LT_RETURN[].
    CALL FUNCTION 'BAPI_BUPA_ADDRESS_CHANGE'
      EXPORTING
        BUSINESSPARTNER = LS_DATA-PARTNER
        ADDRESSGUID     = LV_ADDRGUID
      TABLES
        BAPIADSMTP      = LT_MAIL2
        BAPIAD_REM      = LT_NOTE2
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
    CLEAR LS_MESSG.
    LS_MESSG-MSGTY = 'S'.
    LS_MESSG-MSGID = 'ZTEC'.
    LS_MESSG-MSGNO = '000'.
* Text-i03: Business Partner data has been updated successfully.
    LS_MESSG-MSGTX = TEXT-I03.
    INSERT LS_MESSG INTO TABLE PT_MESSG.

  ENDIF.



ENDFORM.
