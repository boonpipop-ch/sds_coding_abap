*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0480
*  Creation Date      : 04.02.2025
*  Author             : B.Chiewsarikij (SDS)
*  Add-on ID          : N/A
*  Description        : Open Sales Order (Overdue Delivery)
*  Purpose            : N/A
*  Copied from        : Refer from ZR_SFDC_OUTSTANDING_ORDER_TUNE (ECC)
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Report ZSDSSDR0480
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSSDR0480 MESSAGE-ID ZSDSSD01.
INCLUDE ZSDSSDR0480_TOP.
INCLUDE ZSDSSDR0480_CLASS.
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES: VBAK,
        VBAP,
        VBEP.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------


*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA:
*-> internal tables
  GT_RESULT           TYPE STANDARD TABLE OF TYP_ALV,
*-> range
  GR_MATGRP_TRANS     TYPE RANGE OF MARA-MATKL,
*-> work areas
*-> variables
  GV_OVR_UNALLO_LIMIT TYPE I,
  GV_MILESTONE        TYPE VBAK-ZZPOB.
*-> reference

*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS: GC_S_EMP TYPE PARVW VALUE 'VE'.

*-----------------------------------------------------------------------
* M A C R O S
*-----------------------------------------------------------------------
*DEFINE.
*END-OF-DEFINITION.

*-----------------------------------------------------------------------
* F I E L D – S Y M B O L S
*-----------------------------------------------------------------------
*FIELD-SYMBOLS:

*-----------------------------------------------------------------------
* F I E L D – G R O U P S
*-----------------------------------------------------------------------
*FIELD-GROUPS:

*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B01 WITH FRAME TITLE TEXT-B01.
  SELECT-OPTIONS : S_VBELN   FOR VBAK-VBELN,   "Document Number
                   S_AUART   FOR VBAK-AUART,   "Order type
                   S_KUNNR   FOR VBAK-KUNNR,   "Sold-To-Party
                   S_MATNR   FOR VBAP-MATNR.   "Material
SELECTION-SCREEN END OF BLOCK B01.

SELECTION-SCREEN BEGIN OF BLOCK B02 WITH FRAME TITLE TEXT-B02.
  SELECT-OPTIONS: S_AUDAT FOR VBAK-AUDAT,  "Document Date
                  S_EDATU FOR VBEP-EDATU.  "First delivery Date
SELECTION-SCREEN END OF BLOCK B02.

SELECTION-SCREEN BEGIN OF BLOCK B03 WITH FRAME TITLE TEXT-B03.
  SELECT-OPTIONS: S_VKORG FOR VBAK-VKORG,  "Sales Organization
                  S_VTWEG FOR VBAK-VTWEG,  "Distribution Channel
                  S_VKBUR FOR VBAK-VKBUR,  "Sales Office
                  S_VKGRP FOR VBAK-VKGRP.  "Sales Group
SELECTION-SCREEN END OF BLOCK B03.

SELECTION-SCREEN BEGIN OF BLOCK B04 WITH FRAME TITLE TEXT-B04.
  PARAMETERS: P_EMAIL TYPE CHAR01 AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B04.

SELECTION-SCREEN BEGIN OF BLOCK B05 WITH FRAME TITLE TEXT-B05.
  PARAMETERS: P_UNALO TYPE CHAR01 AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B05.

*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.
  PERFORM F_GET_CONSTANT.
*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
*AT SELECTION-SCREEN OUTPUT.
*AT SELECTION-SCREEN ON HELP-REQUEST FOR <field>
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR <field>
*AT SELECTION-SCREEN ON <field>
*AT SELECTION-SCREEN.

*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
*-> read data
  SELECT H~VBELN,
         I~POSNR,
         I~MATNR,
         I~KWMENG,
         I~VRKME,
         H~KUNNR,
         H~AUART,
         H~BESTK,
         H~ERDAT,
         H~ERZET,
*         P~PERNR,
         S~ETENR,     "Schedule line no.
         S~EDATU,     "Delivery Date
         H~VKBUR      AS OFFICE,
         TVKBT~BEZEI  AS OFFICE_TXT,
         I~PS_PSP_PNR,
         PRPS~POST1,
         I~PRODH
    FROM VBAK AS H
    INNER JOIN VBAP AS I ON H~VBELN = I~VBELN
    INNER JOIN VBEP AS S ON S~VBELN = I~VBELN
                        AND S~POSNR = I~POSNR
*                        AND S~ETENR = '0001'    "First Delivery date
    LEFT OUTER JOIN TVKBT ON TVKBT~VKBUR = H~VKBUR
                         AND SPRAS = 'E'
    LEFT OUTER JOIN PRPS ON PRPS~PSPNR = I~PS_PSP_PNR
    LEFT OUTER JOIN MARA ON MARA~MATNR = I~MATNR
    INTO TABLE @DATA(LT_DATA)
    WHERE H~VBELN IN @S_VBELN
      AND ( H~AUART IN @S_AUART AND
            H~AUART IN @GR_FOC )
      AND H~KUNNR IN @S_KUNNR
      AND H~AUDAT IN @S_AUDAT
      AND H~VKORG IN @S_VKORG
      AND H~VTWEG IN @S_VTWEG
      AND H~VKBUR IN @S_VKBUR
      AND H~VKGRP IN @S_VKGRP
      AND S~EDATU < @SY-DATUM
      AND H~ZZPOB <> @GV_MILESTONE
      AND H~GBSTK <> 'C'    "Overall Processing Status <> C
      AND I~LFSTA <> 'C'    "Not Complete delivery
      AND I~KOWRR <> 'X'
      AND S~BMENG <> 0      "Confirm QTY
      AND MARA~MATKL NOT IN @GR_MATGRP_TRANS.
  IF SY-SUBRC = 0.

    SELECT KUNNR,
           NAME1,
           NAME2,
           NAME3,
           NAME4
      INTO TABLE @DATA(LT_KNA1)
      FROM KNA1
      FOR ALL ENTRIES IN @LT_DATA
      WHERE KNA1~KUNNR = @LT_DATA-KUNNR.
    IF SY-SUBRC = 0.
      SORT LT_KNA1 BY KUNNR.
    ENDIF.

    SELECT
      VBELN,
      POSNR,
      PERNR
      FROM VBPA
      INTO TABLE @DATA(LT_VBPA)
      FOR ALL ENTRIES IN @LT_DATA
      WHERE VBPA~VBELN = @LT_DATA-VBELN
        AND VBPA~PARVW = 'VE'.
    IF SY-SUBRC = 0.
      SORT LT_VBPA BY VBELN.
    ENDIF.

    "Mapping to GT_RESULT
    LOOP AT LT_DATA INTO DATA(LS_DATA).

      APPEND INITIAL LINE TO GT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_RESULT>).
      MOVE-CORRESPONDING LS_DATA TO <LFS_RESULT>.

      <LFS_RESULT>-PSPNR    = LS_DATA-PS_PSP_PNR.
      <LFS_RESULT>-WBS_DESC = LS_DATA-POST1.

      READ TABLE LT_VBPA INTO DATA(LS_VBPA)
                         WITH KEY VBELN = LS_DATA-VBELN
                         BINARY SEARCH.
      IF SY-SUBRC = 0.
        <LFS_RESULT>-S_EMP    = LS_VBPA-PERNR.
      ENDIF.

      CONDENSE LS_DATA-PRODH.
      SPLIT LS_DATA-PRODH AT SPACE INTO: <LFS_RESULT>-PROD1
                                         <LFS_RESULT>-PROD2
                                         <LFS_RESULT>-PROD3.


      READ TABLE LT_KNA1 INTO DATA(LS_KNA1)
                         WITH KEY KUNNR = LS_DATA-KUNNR
                         BINARY SEARCH.
      IF SY-SUBRC = 0.
        CONCATENATE LS_KNA1-NAME1
                    LS_KNA1-NAME2
                    LS_KNA1-NAME3
                    LS_KNA1-NAME4
                    INTO <LFS_RESULT>-CUST_NAME.
      ENDIF.

    ENDLOOP.

    SORT GT_RESULT BY VBELN POSNR.
*    EXIT.   "FOR TEST
  ENDIF.

  IF GT_RESULT[] IS INITIAL.
    MESSAGE I003 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
*-> process data
  PERFORM F_ALV_DISPLAY.
  IF P_UNALO = ABAP_TRUE.
    PERFORM F_UNALLOCATE.
  ENDIF.
  IF P_EMAIL = ABAP_TRUE.
    PERFORM F_SENDING_EMAIL.
  ENDIF.

*-----------------------------------------------------------------------
* T O P - O F – P A G E
*-----------------------------------------------------------------------
*TOP-OF-PAGE.

*-----------------------------------------------------------------------
* AT LINE-SELECTION
*-----------------------------------------------------------------------
*AT LINE-SELECTION.

*-----------------------------------------------------------------------
* F O R M S
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Form F_ALV_DISPLAY
*&---------------------------------------------------------------------*
*& text ALV Display
*&---------------------------------------------------------------------*
FORM F_ALV_DISPLAY .
  DATA:
    ##NEEDED
    LR_TEXT             TYPE REF TO CL_SALV_FORM_TEXT,
    LS_ALVTABLE         TYPE REF TO CL_SALV_TABLE,
    LR_COLUMNS          TYPE REF TO CL_SALV_COLUMNS_TABLE,
    LR_CONTENT          TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
    LR_LABEL            TYPE REF TO CL_SALV_FORM_LABEL,
    LF_TEXT             TYPE STRING,
    LS_LAYOUT           TYPE REF TO CL_SALV_LAYOUT,
    LS_KEY              TYPE SALV_S_LAYOUT_KEY,
    LS_DISPLAY_SETTINGS TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
    LS_FUNCTIONS_LIST   TYPE REF TO CL_SALV_FUNCTIONS_LIST.

  FIELD-SYMBOLS <LFS_TABLE> TYPE ANY TABLE.
  ASSIGN GT_RESULT TO <LFS_TABLE>.

*Initialize ref to cl_salv_table
  TRY.
      CL_SALV_TABLE=>FACTORY(
        IMPORTING
          R_SALV_TABLE = LS_ALVTABLE
        CHANGING
          T_TABLE      = <LFS_TABLE> ).
    CATCH CX_SALV_MSG.                                  "#EC NO_HANDLER
      MESSAGE TEXT-E01 TYPE 'E'.
  ENDTRY.

  LR_COLUMNS = LS_ALVTABLE->GET_COLUMNS( ).
  LR_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ).

* Set column
  PERFORM F_DETAIL_COLUMN_SETTINGS_REPT USING LR_COLUMNS.

  LS_DISPLAY_SETTINGS = LS_ALVTABLE->GET_DISPLAY_SETTINGS( ).
  LS_DISPLAY_SETTINGS->SET_STRIPED_PATTERN( 'X' ).
  LS_DISPLAY_SETTINGS->SET_LIST_HEADER( SY-TITLE ).

  LS_FUNCTIONS_LIST = LS_ALVTABLE->GET_FUNCTIONS( ).
  LS_FUNCTIONS_LIST->SET_ALL( ).

  LS_KEY-REPORT = SY-CPROG.
  LS_KEY-HANDLE = '0001'.
  LS_LAYOUT = LS_ALVTABLE->GET_LAYOUT( ).
  LS_LAYOUT->SET_KEY( LS_KEY ).
  LS_LAYOUT->SET_SAVE_RESTRICTION( IF_SALV_C_LAYOUT=>RESTRICT_NONE ).
  LS_LAYOUT->SET_DEFAULT( 'X' ).

* Top_of_list
  CREATE OBJECT LR_CONTENT.
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 1
    COLUMN = 1
    TEXT   = SY-TITLE ).

  LF_TEXT = TEXT-T01.
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 2
    COLUMN = 1
    TEXT   = LF_TEXT ).
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 2
    COLUMN = 2
    TEXT   = SY-DATUM ).

  LF_TEXT = TEXT-T02.
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 3
    COLUMN = 1
    TEXT   = LF_TEXT ).
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 3
    COLUMN = 2
    TEXT   = SY-UNAME ).

  LR_LABEL->SET_LABEL_FOR( LR_TEXT ).
  LS_ALVTABLE->SET_TOP_OF_LIST( LR_CONTENT ).
*
* Display Table
  LS_ALVTABLE->DISPLAY( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_detail_column_settings_rept
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LR_COLUMNS
*&---------------------------------------------------------------------*
FORM F_DETAIL_COLUMN_SETTINGS_REPT  USING UR_COLUMNS TYPE REF TO CL_SALV_COLUMNS_TABLE.
  DATA: LR_COLUMN TYPE REF TO CL_SALV_COLUMN_TABLE,
        LF_S_TEXT TYPE SCRTEXT_S,
        LF_M_TEXT TYPE SCRTEXT_M,
        LF_L_TEXT TYPE SCRTEXT_L.

  TRY.
      LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( 'OBJNR' ).
      LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
    CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND. "#EC NO_HANDLER
  ENDTRY.

  LF_S_TEXT = LF_M_TEXT = LF_L_TEXT = TEXT-C01.
  TRY.
      LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( 'VBELN' ).
*      LR_COLUMN->SET_CURRENCY_COLUMN( VALUE = 'WAERS' ).

      LR_COLUMN->SET_SHORT_TEXT( VALUE = LF_S_TEXT ).
      LR_COLUMN->SET_MEDIUM_TEXT( VALUE = LF_M_TEXT ).
      LR_COLUMN->SET_LONG_TEXT( VALUE = LF_L_TEXT ).
    ##NO_HANDLER   CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND CX_SALV_DATA_ERROR.
  ENDTRY.

  LF_S_TEXT = LF_M_TEXT = LF_L_TEXT = TEXT-C01.
  TRY.
      LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( 'VBELN' ).
*      LR_COLUMN->SET_CURRENCY_COLUMN( VALUE = 'WAERS' ).

      LR_COLUMN->SET_SHORT_TEXT( VALUE = LF_S_TEXT ).
      LR_COLUMN->SET_MEDIUM_TEXT( VALUE = LF_M_TEXT ).
      LR_COLUMN->SET_LONG_TEXT( VALUE = LF_L_TEXT ).
    ##NO_HANDLER   CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND CX_SALV_DATA_ERROR.
  ENDTRY.

  "Customer Name
  LF_S_TEXT = LF_M_TEXT = LF_L_TEXT = TEXT-C02.
  TRY.
      LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( 'CUST_NAME' ).
*      LR_COLUMN->SET_CURRENCY_COLUMN( VALUE = 'WAERS' ).

      LR_COLUMN->SET_SHORT_TEXT( VALUE = LF_S_TEXT ).
      LR_COLUMN->SET_MEDIUM_TEXT( VALUE = LF_M_TEXT ).
      LR_COLUMN->SET_LONG_TEXT( VALUE = LF_L_TEXT ).
    ##NO_HANDLER   CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND CX_SALV_DATA_ERROR.
  ENDTRY.

  "Prod1
  LF_S_TEXT = LF_M_TEXT = LF_L_TEXT = TEXT-C03.
  TRY.
      LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( 'PROD1' ).
*      LR_COLUMN->SET_CURRENCY_COLUMN( VALUE = 'WAERS' ).

      LR_COLUMN->SET_SHORT_TEXT( VALUE = LF_S_TEXT ).
      LR_COLUMN->SET_MEDIUM_TEXT( VALUE = LF_M_TEXT ).
      LR_COLUMN->SET_LONG_TEXT( VALUE = LF_L_TEXT ).
    ##NO_HANDLER   CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND CX_SALV_DATA_ERROR.
  ENDTRY.

  "Prod2
  LF_S_TEXT = LF_M_TEXT = LF_L_TEXT = TEXT-C04.
  TRY.
      LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( 'PROD2' ).
      LR_COLUMN->SET_SHORT_TEXT( VALUE = LF_S_TEXT ).
      LR_COLUMN->SET_MEDIUM_TEXT( VALUE = LF_M_TEXT ).
      LR_COLUMN->SET_LONG_TEXT( VALUE = LF_L_TEXT ).
    ##NO_HANDLER   CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND CX_SALV_DATA_ERROR.
  ENDTRY.

  "Prod2
  LF_S_TEXT = LF_M_TEXT = LF_L_TEXT = TEXT-C05.
  TRY.
      LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( 'PROD3' ).
      LR_COLUMN->SET_SHORT_TEXT( VALUE = LF_S_TEXT ).
      LR_COLUMN->SET_MEDIUM_TEXT( VALUE = LF_M_TEXT ).
      LR_COLUMN->SET_LONG_TEXT( VALUE = LF_L_TEXT ).
    ##NO_HANDLER   CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND CX_SALV_DATA_ERROR.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SENDING_EMAIL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_SENDING_EMAIL.

  TYPES: BEGIN OF LTYP_EMP,
           S_EMP TYPE PA0001-PERNR,
         END OF LTYP_EMP.

  DATA: LV_SUBJECT      TYPE CHAR50,
        LV_SENDER_NAME  TYPE SO_RECNAME,
        LV_SENDER_EMAIL TYPE SO_RECNAME,
        LV_DATE         TYPE CHAR20.

  DATA: LCL_SDSUTIL   TYPE REF TO ZCL_SDSCA_UTIL_SDS.
  DATA: LV_MNG_EMAIL  TYPE BAPIADSMTP-E_MAIL.
  DATA: LT_PERNR TYPE STANDARD TABLE OF LTYP_EMP,
        LS_PERNR TYPE LTYP_EMP.

  DATA:
    LT_HTML_CON    TYPE SOLI_TAB,
    LS_ATTACH_FILE TYPE ZSDSCAS019,
    LT_ATTACH_FILE TYPE ZSDSCAS019_TT,
    LS_EMAIL       TYPE STRING,
    LT_REC         LIKE TABLE OF LS_EMAIL,
    LT_CC          LIKE TABLE OF LS_EMAIL.

  DATA:
    LT_DATA TYPE TTYP_ALV,
    LT_TMP  TYPE TTYP_ALV.

  LT_DATA[] = GT_RESULT[].
  SORT LT_DATA[] BY S_EMP ASCENDING.

*  APPEND LT_DATA[] TO LT_PERNR[].
  LOOP AT LT_DATA INTO DATA(LS_DATA).
    MOVE-CORRESPONDING LS_DATA TO LS_PERNR.
    APPEND LS_PERNR TO LT_PERNR.
  ENDLOOP.

  SORT LT_PERNR BY S_EMP ASCENDING.
  DELETE ADJACENT DUPLICATES FROM LT_PERNR COMPARING ALL FIELDS.

  LOOP AT LT_PERNR INTO LS_PERNR.
    REFRESH: LT_TMP[],
             LT_HTML_CON[],
             LT_ATTACH_FILE[],
             LT_REC[],
             LT_CC[].

    CLEAR: LV_MNG_EMAIL,
           LV_SENDER_EMAIL,
           LV_SENDER_NAME.


    READ TABLE LT_DATA TRANSPORTING NO FIELDS
                       WITH KEY S_EMP = LS_PERNR-S_EMP
                       BINARY SEARCH.
    IF SY-SUBRC = 0.
      DATA(LV_INDEX) = SY-TABIX.
      LOOP AT LT_DATA INTO LS_DATA FROM SY-TABIX.
        IF LS_DATA-S_EMP <> LS_PERNR-S_EMP.
          EXIT.
        ENDIF.

        APPEND LS_DATA TO LT_TMP[].
      ENDLOOP.
    ENDIF.

    "Get Sales&Manager (Email)
    SELECT SINGLE USRID_LONG
    FROM PA0105
    INTO @DATA(LV_SALES_EMAIL)
    WHERE PERNR = @LS_PERNR-S_EMP AND      "#1
          SUBTY = '0010'.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.

    PERFORM GET_MANAGER_DATA USING LS_PERNR-S_EMP     "#2
                             CHANGING LV_MNG_EMAIL.

    "Remind : Sales Order past due as of Date DD.MM.YYYY
    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING   I_REPID             = GC_CON-REPID
                                                    I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                    I_PARAM             = GC_CON-SUBJECT
                                          CHANGING  C_RETURN            = LV_SUBJECT ).

    WRITE SY-DATUM TO LV_DATE DD/MM/YYYY.
    LV_SUBJECT = |{ LV_SUBJECT } { LV_DATE }|.

    "SENDER NAME & EMAIL
    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING   I_REPID             = GC_CON-REPID
                                                  I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                  I_PARAM             = GC_CON-SENDER_EMAIL
                                        CHANGING  C_RETURN            = LV_SENDER_EMAIL ).
    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING   I_REPID             = GC_CON-REPID
                                                    I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                    I_PARAM             = GC_CON-SENDER_NAME
                                          CHANGING  C_RETURN            = LV_SENDER_NAME ).

    PERFORM GET_CONTENT USING LT_TMP
                        CHANGING LT_HTML_CON.

    CLEAR:LS_EMAIL.
    LS_EMAIL = LV_SALES_EMAIL.
    APPEND LS_EMAIL TO LT_REC.

    CLEAR:LS_EMAIL.
    LS_EMAIL = LV_MNG_EMAIL.
    APPEND LS_EMAIL TO LT_CC.

    IF LCL_SDSUTIL IS NOT BOUND.
      CREATE OBJECT LCL_SDSUTIL.
    ENDIF.

    LCL_SDSUTIL->SEND_MAIL(   I_SUBJECT      = LV_SUBJECT
                              IT_HTML_CON    = LT_HTML_CON
                              IT_ATTACH_FILE = LT_ATTACH_FILE
                              I_SENDER_EMAIL = LV_SENDER_EMAIL
                              I_SENDER_NAME  = LV_SENDER_NAME
                              IT_RECEIVER    = LT_REC
                              IT_CC          = LT_CC  ).

    COMMIT WORK AND WAIT.

    CLEAR: LT_TMP[],
           LV_SALES_EMAIL,
           LV_MNG_EMAIL.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_MANAGER_DATA
*&---------------------------------------------------------------------*
*& Get Manger Data
*&---------------------------------------------------------------------*
FORM GET_MANAGER_DATA   USING UV_SALESEMP TYPE PA0001-PERNR
                        CHANGING CV_MNG_EMAIL TYPE BAPIADSMTP-E_MAIL.

  DATA : LCL_ZCL_SDSCA_CAL_API TYPE REF TO ZCL_SDSCA_CAL_API.
  DATA: LO        TYPE REF TO LCL_DATA,
        LV_EMP_ID TYPE I.

  CLEAR: GV_EMP_ID,
         GS_K2_RES.

  IF LCL_ZCL_SDSCA_CAL_API IS INITIAL.
    CREATE OBJECT LCL_ZCL_SDSCA_CAL_API.
  ENDIF.

  IF LO IS INITIAL.
    CREATE OBJECT LO.
  ENDIF.
  GV_EMP_ID = LV_EMP_ID = UV_SALESEMP.
  CONDENSE GV_EMP_ID.

  LO->START_PROCESS( ).
  CV_MNG_EMAIL = GS_K2_RES-DATA-EMAIL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CONTENT
*&---------------------------------------------------------------------*
FORM GET_CONTENT  USING UT_TAB TYPE TTYP_ALV
                  CHANGING CT_HTML_CON TYPE SOLI_TAB.


  DATA : LS_CONTENT TYPE SOLI.
  DATA : LV_WEBNO TYPE ZSDSMMT006-WEBNO.
*  DATA : LT_ATTACH TYPE TABLE OF GY_ATTACH.
  DATA : LV_ENDPOINT TYPE STRING.
  DATA : LV_DOWNLOAD_PATH TYPE STRING.
  DATA : LV_DELIVERY_DATE TYPE CHAR15,
         LV_KWMENG        TYPE CHAR20,
         LV_DATE          TYPE CHAR20.

  READ TABLE UT_TAB INTO DATA(LS_DATA) INDEX 1.
  IF SY-SUBRC = 0.
    SELECT SINGLE ENAME INTO @DATA(LV_ENAME) FROM PA0001 WHERE PERNR = @LS_DATA-S_EMP.
    IF SY-SUBRC <> 0.
      LV_ENAME = 'SALES PERSON'.
    ENDIF.
  ENDIF.

  CLEAR : LS_CONTENT.
  LS_CONTENT-LINE = |<B>DEAR</B>  { LV_ENAME }|.
  APPEND LS_CONTENT TO CT_HTML_CON.

  CLEAR : LS_CONTENT.
  LS_CONTENT-LINE = '<BR><BR>'.
  APPEND LS_CONTENT TO CT_HTML_CON.

  CLEAR : LS_CONTENT, LV_DATE.
  WRITE: SY-DATUM TO LV_DATE DD/MM/YYYY.
  CONCATENATE '<dd> Reminder Sales Order past due as of'  LV_DATE  ', Detail as below </dd>' INTO LS_CONTENT-LINE SEPARATED BY SPACE ##NO_TEXT.
  APPEND LS_CONTENT TO CT_HTML_CON.

  CLEAR : LS_CONTENT.
  LS_CONTENT-LINE = '<br />'.
  APPEND LS_CONTENT TO CT_HTML_CON.

  CLEAR : LS_CONTENT.
  LS_CONTENT-LINE = '<TABLE  width= "90%" >'.
  APPEND LS_CONTENT TO CT_HTML_CON.

  "Sales Team
*  CLEAR: LS_CONTENT.
*  CONCATENATE '<TR><td style="width: 30px; height: 4px; background-color: deepskyblue;" bgcolor="#990000"> '
*       '<FONT COLOR = "BLUE">  <CENTER> <B> Sales Team </B> </CENTER> </FONT>'
*       '</td>'  INTO LS_CONTENT-LINE.
*  APPEND LS_CONTENT TO CT_HTML_CON.

  "Sales Name
  CLEAR: LS_CONTENT.
  CONCATENATE '<td style="width: 74px; height: 4px; background-color: deepskyblue;" bgcolor="#CCCCFF"> '
       '<FONT COLOR = "BLUE">  <CENTER> <B> Sales Name </B> </CENTER> </FONT>'
       '</td>'  INTO LS_CONTENT-LINE.
  APPEND LS_CONTENT TO CT_HTML_CON.

  "Sales Order
  CLEAR: LS_CONTENT.
  CONCATENATE '<td style="width: 74px; height: 4px; background-color: deepskyblue;" bgcolor="#CCCCFF"> '
       '<FONT COLOR = "BLUE">  <CENTER> <B> Sales Order </B> </CENTER> </FONT>'
       '</td>'  INTO LS_CONTENT-LINE.
  APPEND LS_CONTENT TO CT_HTML_CON.

  "Item
  CLEAR: LS_CONTENT.
  CONCATENATE '<td style="width: 74px; height: 4px; background-color: deepskyblue;" bgcolor="#CCCCFF"> '
      '<FONT COLOR = "BLUE">  <CENTER> <B> Item </B> </CENTER> </FONT>'
      '</td>'  INTO LS_CONTENT-LINE.
  APPEND LS_CONTENT TO CT_HTML_CON.

  "First Delivery date
  CLEAR: LS_CONTENT.
  CONCATENATE '<td style="width: 74px; height: 4px; background-color: deepskyblue;" bgcolor="#CCCCFF">'
       '<FONT COLOR = "BLUE"> <CENTER> <B> First Delivery date </B> </CENTER> </FONT>'
       '</td>' INTO LS_CONTENT-LINE.
  APPEND LS_CONTENT TO CT_HTML_CON.

  "SO Hold date
*  CLEAR: LS_CONTENT.
*  CONCATENATE '<td style="width: 74px; height: 4px; background-color: deepskyblue;" bgcolor="#990000">'
*      '<FONT COLOR = "BLUE"> <CENTER> <B> SO Hold date </B> </CENTER> </FONT>'
*      '</td>'  INTO LS_CONTENT-LINE.
*  APPEND LS_CONTENT TO CT_HTML_CON.

  "Project Name
  CLEAR: LS_CONTENT.
  CONCATENATE '<td style="width: 74px; height: 4px; background-color: deepskyblue;" bgcolor="#CCCCFF">'
      '<FONT COLOR = "BLUE"> <CENTER> <B>Project Name</B> </CENTER> </FONT>'
      '</td>' INTO LS_CONTENT-LINE.
  APPEND LS_CONTENT TO CT_HTML_CON.

  "Customer
  CLEAR: LS_CONTENT.
  CONCATENATE '<td style="width: 74px; height: 4px; background-color: deepskyblue;" bgcolor="#CCCCFF">'
       '<FONT COLOR = "BLUE"> <CENTER> <B> Customer </B> </CENTER> </FONT>'
       '</td>'  INTO LS_CONTENT-LINE.
  APPEND LS_CONTENT TO CT_HTML_CON.

  "PH1
  CLEAR: LS_CONTENT.
  CONCATENATE '<td style="width: 74px; height: 4px; background-color: deepskyblue;" bgcolor="#CCCCFF">'
       '<FONT COLOR = "BLUE"> <CENTER> <B> PH1 </B> </CENTER> </FONT>'
       '</td>'  INTO LS_CONTENT-LINE.
  APPEND LS_CONTENT TO CT_HTML_CON.

  "PH2
  CLEAR: LS_CONTENT.
  CONCATENATE '<td style="width: 74px; height: 4px; background-color: deepskyblue;" bgcolor="#CCCCFF">'
       '<FONT COLOR = "BLUE"> <CENTER> <B> PH2 </B> </CENTER> </FONT>'
       '</td>'  INTO LS_CONTENT-LINE.
  APPEND LS_CONTENT TO CT_HTML_CON.

*  "BOM
*  CLEAR: LS_CONTENT.
*  CONCATENATE '<td style="width: 74px; height: 4px; background-color: deepskyblue;" bgcolor="#990000">'
*         '<FONT COLOR = "BLUE"> <CENTER> <B> BOM </B> </CENTER> </FONT>'
*         '</td>'  INTO LS_CONTENT-LINE.
*  APPEND LS_CONTENT TO CT_HTML_CON.

  "Model
  CLEAR: LS_CONTENT.
  CONCATENATE '<td style="width: 100px; height: 4px; background-color: deepskyblue;" bgcolor="#CCCCFF">'
         '<FONT COLOR = "BLUE"> <CENTER> <B> Model </B> </CENTER> </FONT>'
         '</td>'  INTO LS_CONTENT-LINE.
  APPEND LS_CONTENT TO CT_HTML_CON.

  "Qty
  CLEAR: LS_CONTENT.
  CONCATENATE '<td style="width: 74px; height: 4px; background-color: deepskyblue;" bgcolor="#CCCCFF">'
         '<FONT COLOR = "BLUE"> <CENTER> <B> QTY </B> </CENTER> </FONT>'
         '</td></tr>' INTO LS_CONTENT-LINE.
  APPEND LS_CONTENT TO CT_HTML_CON.


  "Remark
*  Clear: Ls_content.
*  Concatenate '<Td Style="width: 74Px; Height: 4Px; Background-color: Deepskyblue;" Bgcolor="#990000">'
*         '<Font Color = "Blue"> <Center> <B> Remark </B> </Center> </Font>'
*         '</Td></tr>'  Into Ls_content-line.
*  Append Ls_content To Ct_html_con.

  DATA: LV_COLOR_CODE TYPE CHAR10.

  LOOP AT UT_TAB INTO DATA(LS_TAB).

    DATA(LV_MOD) = SY-TABIX MOD 2.

    IF LV_MOD = 1.
      LV_COLOR_CODE = '#FDFEFE'.
    ELSE.
      LV_COLOR_CODE = '#F4F6F6'.
    ENDIF.

*    "Sales Office
*    CLEAR: LS_CONTENT.
*    CONCATENATE '<TR><td style= "font-family:Tahoma;font-size:10pt" align = "LEFT" BGCOLOR = "#ffe4e1" >'
*         '<FONT COLOR = "BLUE">  <LEFT> ' LS_TAB-OFFICE_TXT  '</LEFT>  </FONT>'
*         '</td>'  INTO LS_CONTENT-LINE.
*    APPEND LS_CONTENT TO CT_HTML_CON.

    "Sales Emp name
    CLEAR: LS_CONTENT.
    CONCATENATE '<TR><td style= "font-family:Tahoma;font-size:10pt" align = "LEFT" BGCOLOR = "' LV_COLOR_CODE '" >'
         '<FONT COLOR = "BLUE"> <LEFT>' LV_ENAME '</LEFT> </FONT>'
         '</td>'  INTO LS_CONTENT-LINE.
    APPEND LS_CONTENT TO CT_HTML_CON.

    "Sales Order
    CLEAR: LS_CONTENT.
    CONCATENATE '<td style= "font-family:Tahoma;font-size:10pt" align = "LEFT" BGCOLOR = "' LV_COLOR_CODE '" >'
         '<FONT COLOR = "BLUE"> <LEFT>' LS_TAB-VBELN '</LEFT> </FONT>'
         '</td>'  INTO LS_CONTENT-LINE.
    APPEND LS_CONTENT TO CT_HTML_CON.

    "Item no.
*    CLEAR: LV_REPLACE.
*    LV_COUNT = WA_DETAIL-POSNR.
*    WRITE LV_COUNT TO LV_REPLACE.
*    CONDENSE LV_REPLACE NO-GAPS.
    CLEAR: LS_CONTENT.
    CONCATENATE '<td style= "font-family:Tahoma;font-size:10pt" align = "LEFT" BGCOLOR = "' LV_COLOR_CODE '" >'
         '<FONT COLOR = "BLUE"> <LEFT>' LS_TAB-POSNR '</LEFT> </FONT>'
         '</td>'  INTO LS_CONTENT-LINE.
    APPEND LS_CONTENT TO CT_HTML_CON.

    "First Delivery DAte
    CLEAR: LS_CONTENT, LV_DATE.
    WRITE: LS_TAB-EDATU TO LV_DATE DD/MM/YYYY.
    CONCATENATE '<td style= "font-family:Tahoma;font-size:10pt" align = "LEFT" BGCOLOR = "' LV_COLOR_CODE '"  >'
         '<FONT COLOR = "BLUE"> <LEFT>' LV_DATE '</LEFT> </FONT>'
         '</td>'  INTO LS_CONTENT-LINE.
    APPEND LS_CONTENT TO CT_HTML_CON.

    "reject Date
*    CLEAR: LS_CONTENT.
*    CONCATENATE '<td style= "font-family:Tahoma;font-size:10pt" align = "LEFT" BGCOLOR = "#ffe4e1"  >'
*         '<FONT COLOR = "BLUE"> <LEFT>' 'LS_TAB-REJ_DATE' '</LEFT> </FONT>'
*         '</td>'  INTO LS_CONTENT-LINE.
*    APPEND LS_CONTENT TO CT_HTML_CON.

    CLEAR: LS_CONTENT.
    CONCATENATE '<td style= "font-family:Tahoma;font-size:10pt" align = "LEFT" BGCOLOR = "' LV_COLOR_CODE '" >'
         '<FONT COLOR = "BLUE"> <LEFT>' LS_TAB-WBS_DESC '</LEFT> </FONT>'
         '</td>'  INTO LS_CONTENT-LINE.
    APPEND LS_CONTENT TO CT_HTML_CON.

    CLEAR: LS_CONTENT.
    CONCATENATE '<td style= "font-family:Tahoma;font-size:10pt" align = "LEFT" BGCOLOR = "' LV_COLOR_CODE '"  >'
         '<FONT COLOR = "BLUE"> <LEFT>' LS_TAB-CUST_NAME '</LEFT> </FONT>'
         '</td>'  INTO LS_CONTENT-LINE.
    APPEND LS_CONTENT TO CT_HTML_CON.

    CLEAR: LS_CONTENT.
    CONCATENATE '<td style= "font-family:Tahoma;font-size:10pt" align = "LEFT" BGCOLOR = "' LV_COLOR_CODE '"  >'
         '<FONT COLOR = "BLUE"> <LEFT>' LS_TAB-PROD1 '</LEFT> </FONT>'
         '</td>'  INTO LS_CONTENT-LINE.
    APPEND LS_CONTENT TO CT_HTML_CON.

    CLEAR: LS_CONTENT.
    CONCATENATE '<td style= "font-family:Tahoma;font-size:10pt" align = "LEFT" BGCOLOR = "' LV_COLOR_CODE '"  >'
         '<FONT COLOR = "BLUE"> <LEFT>' LS_TAB-PROD2 '</LEFT> </FONT>'
         '</td>'  INTO LS_CONTENT-LINE.
    APPEND LS_CONTENT TO CT_HTML_CON.

*    CLEAR: LS_CONTENT.
*    CONCATENATE '<td style= "font-family:Tahoma;font-size:10pt" align = "LEFT" BGCOLOR = "#ffe4e1" >'
*         '<FONT COLOR = "BLUE"> <LEFT>' 'WA_DETAIL-UPMAT' ' </LEFT></FONT>'
*         '</td>'  INTO LS_CONTENT-LINE.
*    APPEND LS_CONTENT TO CT_HTML_CON.

    "Material
    CLEAR: LS_CONTENT.
    CONCATENATE '<td style= "font-family:Tahoma;font-size:10pt" align = "LEFT" BGCOLOR = "' LV_COLOR_CODE '"  >'
         '<FONT COLOR = "BLUE"> <LEFT>'  LS_TAB-MATNR '</LEFT> </FONT>'
         '</td>' INTO LS_CONTENT-LINE.
    APPEND LS_CONTENT TO CT_HTML_CON.

    "Qty
    CLEAR: LS_CONTENT, LV_KWMENG.
    WRITE LS_TAB-KWMENG TO LV_KWMENG ##UOM_IN_MES.
    CONDENSE LV_KWMENG.
    CONCATENATE '<td style= "font-family:Tahoma;font-size:10pt" align = "LEFT" BGCOLOR = "' LV_COLOR_CODE '"  >'
         '<FONT COLOR = "BLUE"> <CENTER>' LV_KWMENG  '</CENTER> </FONT>'
         '</td>' INTO LS_CONTENT-LINE.
    APPEND LS_CONTENT TO CT_HTML_CON.

*    "Aging
*    CLEAR: LS_CONTENT.
*    CONCATENATE '<td style= "font-family:Tahoma;font-size:10pt" align = "LEFT" BGCOLOR = "#ffe4e1"  >'
*         '<FONT COLOR = "BLUE"> <CENTER>' 'WA_DETAIL-ZSPA_AGING' '</CENTER> </FONT>'
*         '</td></tr>'  INTO LS_CONTENT-LINE.
*    APPEND LS_CONTENT TO CT_HTML_CON.

  ENDLOOP.

  CLEAR: LS_CONTENT.
  LS_CONTENT-LINE = '</TABLE>'.
  APPEND LS_CONTENT TO CT_HTML_CON.

  CLEAR LS_CONTENT.
  LS_CONTENT-LINE = '<br/><b>If no revise of delivery date, So will be automatic hold in next 2 weeks.,</b>'.
  APPEND LS_CONTENT TO CT_HTML_CON.

*  CLEAR LS_CONTENT.
*  LS_CONTENT-LINE = '<br/><b>If your SO have “Aging Model”, please kindly focus and try to push sell out asap.,</b>'.
*  APPEND LS_CONTENT TO CT_HTML_CON.

  CLEAR LS_CONTENT.
  LS_CONTENT-LINE = '<br />'.
  APPEND LS_CONTENT TO CT_HTML_CON.

  CLEAR LS_CONTENT.
  LS_CONTENT-LINE = '<br/><b>Best Regards,</b>'.
  APPEND LS_CONTENT TO CT_HTML_CON.

  CLEAR LS_CONTENT.
  LS_CONTENT-LINE = '<br/><b>Automatic e-mail from SAP System</b><br>'.
  APPEND LS_CONTENT TO CT_HTML_CON.

  CLEAR LS_CONTENT.
  LS_CONTENT-LINE = '</span></p></body>'.
  APPEND LS_CONTENT TO CT_HTML_CON.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_UNALLOCATE
*&---------------------------------------------------------------------*
*& CANCEL_CONFIRM_QTY
*&---------------------------------------------------------------------*
FORM F_UNALLOCATE .

  DATA LCL_SO TYPE REF TO ZCL_SDSSD_SALES_DOCUMENT.
  "Order Type "ZZPOB
  DATA LV_DIFF TYPE I.
  DATA(LT_DATA) = GT_RESULT[].
  DATA LT_DOC TYPE ZSDSSDS127_TT.

  IF LCL_SO IS NOT BOUND.
    CREATE OBJECT LCL_SO.
  ENDIF.

  LOOP AT LT_DATA INTO DATA(LS_DATA).
    LV_DIFF = SY-DATUM - LS_DATA-EDATU.
    IF LV_DIFF >= GV_OVR_UNALLO_LIMIT.
      APPEND INITIAL LINE TO LT_DOC ASSIGNING FIELD-SYMBOL(<LFS_DOC>).
      <LFS_DOC>-VBELN = LS_DATA-VBELN.
      <LFS_DOC>-POSNR = LS_DATA-POSNR.
      UNASSIGN <LFS_DOC>.

    ENDIF.
  ENDLOOP.

  IF LT_DOC IS NOT INITIAL.
    SORT LT_DOC.
    DELETE ADJACENT DUPLICATES FROM LT_DOC.
    DATA(LT_RETURN) = LCL_SO->CANCEL_CONFIRM_QTY( LT_DOC ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_CONSTANT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_CONSTANT .

  DATA: BEGIN OF LS_GEN_C,
          REPID        TYPE  ZSDSCAC001-REPID,
          PARAM        TYPE  ZSDSCAC001-PARAM,
          PARAM_EXT    TYPE  ZSDSCAC001-PARAM_EXT,
          SEQUENCE     TYPE  ZSDSCAC001-SEQUENCE,
          PARAM_SIGN   TYPE  ZSDSCAC001-PARAM_SIGN,
          PARAM_OPTION TYPE  ZSDSCAC001-PARAM_OPTION,
          VALUE_LOW    TYPE  ZSDSCAC001-VALUE_LOW,
          VALUE_HIGH   TYPE  ZSDSCAC001-VALUE_HIGH,
          VDESC        TYPE  ZSDSCAC001-VDESC,
        END OF LS_GEN_C .
  DATA: LT_GEN_C  LIKE STANDARD TABLE OF LS_GEN_C .

  ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = GC_CON-REPID
                                                  IF_PARAM = 'AUART_FOC'
                                        IMPORTING ET_RANGE = GR_FOC ).

  ZCL_SDSCA_UTILITIES=>GET_GEN_C( EXPORTING IF_REPID  = GC_CON-REPID
                                  IMPORTING ET_GEN_C  = LT_GEN_C ).

  IF LT_GEN_C IS NOT INITIAL.
    LOOP AT LT_GEN_C INTO LS_GEN_C.
      CASE LS_GEN_C-PARAM.
        WHEN 'UNALLOCATE_DAY'.
          GV_OVR_UNALLO_LIMIT = LS_GEN_C-VALUE_LOW.
        WHEN 'POB_MILESTONE'.
          GV_MILESTONE        = LS_GEN_C-VALUE_LOW.
        WHEN 'MATKL_TRANS'.
          APPEND VALUE #( SIGN   = LS_GEN_C-PARAM_SIGN
                          OPTION = LS_GEN_C-PARAM_OPTION
                          LOW    = LS_GEN_C-VALUE_LOW )
                          TO GR_MATGRP_TRANS.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ENDLOOP.
  ENDIF.



ENDFORM.
