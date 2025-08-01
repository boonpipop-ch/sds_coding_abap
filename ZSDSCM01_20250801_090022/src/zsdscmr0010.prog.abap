*-----------------------------------------------------------------------
*  Program ID         : ZSDSCMR0010
*  Creation Date      : 16.08.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : CME017
*  Description        : Update Warranty Master
*  Purpose            : To update warranty master table ZSDSCMT003 on
*                       installation and commission date from based
*                       on service order data
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSCMR0010.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  SSCRFIELDS,
  CRMS4D_SERV_H.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: TT_PROC_TYPE_RANGE  TYPE  RANGE OF CRMS4D_SERV_H-PROCESS_TYPE.

TYPES: BEGIN OF TS_RESULT.
         INCLUDE TYPE ZSDSCMS014.
TYPES: END OF TS_RESULT.
TYPES: TT_RESULT  TYPE STANDARD TABLE OF TS_RESULT.

TYPES: BEGIN OF TS_LIST,
         EQUNR      TYPE  EQUI-EQUNR,
         UDATE      TYPE  CRM_JCDS-UDATE,
         INACT      TYPE  CRM_JCDS-INACT,
         OBJECT_ID  TYPE  CRMS4D_SERV_I-OBJECT_ID,
         NUMBER_INT TYPE  CRMS4D_SERV_I-NUMBER_INT,
         UTIME      TYPE  CRM_JCDS-UTIME,
       END OF TS_LIST.
TYPES: TT_LIST  TYPE  STANDARD TABLE OF TS_LIST
                        WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE      TYPE  CHAR1       VALUE 'X',
  GC_TCODE     TYPE  SY-TCODE    VALUE 'ZSDSCM003',

  GC_SVO       TYPE  CRMT_SUBOBJECT_CATEGORY_DB VALUE 'BUS2000116',
  GC_CMPD      TYPE  CRM_JCDS-STAT              VALUE 'I1005',

  GC_LED_GREEN TYPE  ICON-ID VALUE '@5B@',
  GC_LED_RED   TYPE  ICON-ID VALUE '@5C@'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT TYPE  TT_RESULT                                   ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*
DATA:
  GR_COMM TYPE  TT_PROC_TYPE_RANGE                             ##NEEDED,
  GR_INST TYPE  TT_PROC_TYPE_RANGE                             ##NEEDED.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSCMS014'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 24,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 76.

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*
DEFINE MC_SHOW_PROGRESS.
  IF sy-batch IS INITIAL.
*   Show progress online
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = &1 ##NUMBER_OK
        text       = &2.
  ELSE.
*   Show message step in background
    MESSAGE i000(38) WITH &2 space space space.
  ENDIF.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* Text-s01: Selection Criteria
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01 NO INTERVALS.
  SELECT-OPTIONS:
    S_OBJID  FOR CRMS4D_SERV_H-OBJECT_ID OBLIGATORY.
  PARAMETERS:
    CB_TEST  TYPE  CHAR1 AS CHECKBOX DEFAULT 'X'.
  PARAMETERS:
    P_DATUM TYPE  SY-DATUM NO-DISPLAY,
    P_UZEIT TYPE  SY-UZEIT NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
  PERFORM F_GET_CONSTANTS.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM EQ 'ONLI'.
    PERFORM F_VALIDATE_SELECTION_SCREEN.
  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_PROCESS_DATA CHANGING GT_RESULT.
  IF GT_RESULT IS INITIAL.
*   Message: No data found.
    MESSAGE S001(ZSDSCA01).
    RETURN.
  ENDIF.


*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Display Processing Result
  PERFORM F_DISPLAY_RESULT USING GT_RESULT.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE ZSDSCAI9990 ##INCL_OK.

*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Form F_AUTHORIZE_CHECK
*----------------------------------------------------------------------*
*  Check Authorization on t-code
*----------------------------------------------------------------------*
FORM F_AUTHORIZE_CHECK USING UF_TCODE  TYPE  SY-TCODE.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
    ID 'TCD' FIELD UF_TCODE.

  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE S172(00) WITH UF_TCODE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_CONSTANTS
*----------------------------------------------------------------------*
*  Get GenC Constants
*----------------------------------------------------------------------*
FORM F_GET_CONSTANTS .

  CONSTANTS:
    LC_COMM TYPE  ZSDSDE_PARAM_NAME VALUE 'COMMISSION_PROC_TYPE',
    LC_INST TYPE  ZSDSDE_PARAM_NAME VALUE 'INSTALLATION_PROC_TYPE'.

  STATICS:
    LF_READ       TYPE  FLAG.

  DATA:
    LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA:
    LF_REPID   TYPE  PROGRAMM.


* Check Already Read?
  IF LF_READ EQ GC_TRUE.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GR_COMM,
         GR_INST.

* Assign REPID
  LF_REPID = SY-REPID.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = LF_REPID
    IMPORTING
      ET_GEN_C = LT_GENC.

* Mark Read Flag
  LF_READ = GC_TRUE.

* Assign GenC Constants
  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).

    CASE <L_GENC>-PARAM.
*     ------------------------------------
*     Process Type for Commission
*     ------------------------------------
      WHEN LC_COMM.
        INSERT VALUE #( SIGN = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GR_COMM.

*     ------------------------------------
*     Process Type for Installation
*     ------------------------------------
      WHEN LC_INST.
        INSERT VALUE #( SIGN = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GR_INST.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_VALIDATE_SELECTION_SCREEN
*----------------------------------------------------------------------*
*  Validate Selection Screen
*----------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN  ##NEEDED.
* Validate Selection Screen here....
ENDFORM.

*----------------------------------------------------------------------*
*  Form F_PROCESS_DATA
*----------------------------------------------------------------------*
*  Process Data
*----------------------------------------------------------------------*
FORM F_PROCESS_DATA CHANGING CT_RESULT TYPE TT_RESULT.

  DATA:
    LT_LIST   TYPE TT_LIST.

  DATA:
    LS_INPUT  TYPE ZCL_SDSCM_WARRANTY_UTIL=>TS_INST_INFO,
    LS_RETURN TYPE BAPIRET1,
    LS_RESULT TYPE TS_RESULT.

  DATA:
    LF_SUBRC  TYPE  SY-SUBRC.


* Initialize Output
  CLEAR: CT_RESULT.

* Get Order List
  SELECT OBJTYPE_H,
         OBJECT_ID,
         HEADER_GUID,
         PROCESS_TYPE,
         ZZ1_DELIVERY_ORD
    FROM CRMS4D_SERV_H
   WHERE OBJTYPE_H EQ @GC_SVO
     AND OBJECT_ID IN @S_OBJID
    INTO TABLE @DATA(LT_SERV_H).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  LOOP AT LT_SERV_H ASSIGNING FIELD-SYMBOL(<L_SERV_H>).

    CLEAR LT_LIST.

    DO 1 TIMES.

*     Check Order Lock?
      PERFORM F_LOCK_ORDER  USING  <L_SERV_H>-HEADER_GUID
                         CHANGING LF_SUBRC.
      IF LF_SUBRC NE 0.
*       Text-e01: Cannot lock order for processing.
        MESSAGE I000(ZSDSCM01) DISPLAY LIKE 'E'
                               WITH TEXT-E01 SPACE SPACE SPACE.
        EXIT.
      ENDIF.

*     ************************************************
*     ************************************************
*     Try Read Status Up to 1 Minutes from starting time
*     - Since entries in table CRM_JCDS updated delay,
*       the program has to wait until new data found.
*     ************************************************
*     ************************************************
      DO 60 TIMES ##NUMBER_OK.

*       -----------------------------
*       Commissioning
*       -----------------------------
        IF GR_COMM IS NOT INITIAL AND
           <L_SERV_H>-PROCESS_TYPE IN GR_COMM.

          PERFORM F_GET_COMMISSION  USING  <L_SERV_H>-HEADER_GUID
                                           P_DATUM
                                           P_UZEIT
                                  CHANGING LT_LIST
                                           LF_SUBRC.

*       -----------------------------
*       Installation
*       -----------------------------
        ELSEIF GR_INST IS NOT INITIAL AND
           <L_SERV_H>-PROCESS_TYPE IN GR_INST.

*         Ignore if no reference DO
          IF <L_SERV_H>-ZZ1_DELIVERY_ORD IS INITIAL.
            EXIT.
          ENDIF.

          PERFORM F_GET_INSTALLATION  USING  <L_SERV_H>-HEADER_GUID
                                             <L_SERV_H>-ZZ1_DELIVERY_ORD
                                             P_DATUM
                                             P_UZEIT
                                    CHANGING LT_LIST
                                             LF_SUBRC.

        ENDIF.
        IF LF_SUBRC NE 0.
          IF P_DATUM IS INITIAL.
            EXIT.
          ELSE.
            WAIT UP TO 1 SECONDS.
          ENDIF.
        ELSE.
          EXIT.
        ENDIF.

      ENDDO.


      LOOP AT LT_LIST ASSIGNING FIELD-SYMBOL(<L_LIST>).
        CLEAR LS_INPUT.
        LS_INPUT-EQUNR      = <L_LIST>-EQUNR.
        IF <L_SERV_H>-PROCESS_TYPE IN GR_INST.
          LS_INPUT-DTYPE      = ZCL_SDSCM_WARRANTY_UTIL=>GC_INSTALL.
        ELSEIF <L_SERV_H>-PROCESS_TYPE IN GR_COMM.
          LS_INPUT-DTYPE      = ZCL_SDSCM_WARRANTY_UTIL=>GC_COMMISSION.
        ENDIF.
        LS_INPUT-DATUM        = <L_LIST>-UDATE.
        LS_INPUT-INACT        = <L_LIST>-INACT.
        LS_INPUT-OBJECT_ID    = <L_LIST>-OBJECT_ID.
        LS_INPUT-NUMBER_INT   = <L_LIST>-NUMBER_INT.

*       Update Warranty Master
        ZCL_SDSCM_WARRANTY_UTIL=>INST_COMM_WARRANTY_PRODUCT(
          EXPORTING
            IS_INPUT  = LS_INPUT
            IF_TEST   = CB_TEST
            IF_COMMIT = GC_TRUE
          IMPORTING
            ES_RETURN = LS_RETURN ).

*       Collect Result
        CLEAR LS_RESULT.
        LS_RESULT-EQUNR       = LS_INPUT-EQUNR.
        CASE LS_INPUT-DTYPE.
          WHEN ZCL_SDSCM_WARRANTY_UTIL=>GC_INSTALL.
*           Text-a01: Installation
            LS_RESULT-DTYPE = TEXT-A01.
          WHEN ZCL_SDSCM_WARRANTY_UTIL=>GC_COMMISSION.
*           Text-a02: Commissioning
            LS_RESULT-DTYPE = TEXT-A02.
        ENDCASE.
        LS_RESULT-OBJECT_ID  = LS_INPUT-OBJECT_ID.
        LS_RESULT-NUMBER_INT = LS_INPUT-NUMBER_INT.
        LS_RESULT-DATUM      = LS_INPUT-DATUM.
        LS_RESULT-UZEIT      = <L_LIST>-UTIME.
        LS_RESULT-INACT      = LS_INPUT-INACT.
        IF LS_RETURN-TYPE EQ 'S'.
          LS_RESULT-STATU    = GC_LED_GREEN.
        ELSE.
          LS_RESULT-STATU    = GC_LED_RED.
        ENDIF.
        LS_RESULT-MSGTY      = LS_RETURN-TYPE.
        LS_RESULT-MSGTX      = LS_RETURN-MESSAGE.

        INSERT LS_RESULT INTO TABLE CT_RESULT.

      ENDLOOP.

    ENDDO.

*   Unlock Order
    PERFORM F_UNLOCK_ORDER USING <L_SERV_H>-HEADER_GUID.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_LOCK_ORDER
*----------------------------------------------------------------------*
*  Lock Order for processing
*----------------------------------------------------------------------*
FORM F_LOCK_ORDER  USING  UF_GUID TYPE CRMT_OBJECT_GUID
                 CHANGING CF_SUBRC TYPE SY-SUBRC.

* Initialize Output
  CF_SUBRC = 4.

* Wait up to 1 minute
  DO 60 TIMES ##NUMBER_OK.
    CALL FUNCTION 'ENQUEUE_E_CRM_ORDER'
      EXPORTING
        MODE_CRMD_ORDERADM_H = 'E'
        GUID                 = UF_GUID
      EXCEPTIONS
        FOREIGN_LOCK         = 1
        SYSTEM_FAILURE       = 2
        OTHERS               = 3.
    IF SY-SUBRC <> 0.
      WAIT UP TO 1 SECONDS.
    ELSE.
      CF_SUBRC = SY-SUBRC.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_UNLOCK_ORDER
*----------------------------------------------------------------------*
*  Unlock Order
*----------------------------------------------------------------------*
FORM F_UNLOCK_ORDER  USING UF_GUID TYPE CRMT_OBJECT_GUID.

  CALL FUNCTION 'DEQUEUE_E_CRM_ORDER'
    EXPORTING
      MODE_CRMD_ORDERADM_H = 'E'
      GUID                 = UF_GUID.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_DISPLAY_RESULT
*----------------------------------------------------------------------*
*  Display Processing Result
*----------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING  UT_RESULT TYPE TT_RESULT.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
  GF_ALV_HEADER_1 = SPACE.
* Enable Soft Refresh only in display mode
  GF_SOFT_REFRESH_1 = GC_TRUE.

* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
  ASSIGN UT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2610650]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT CHANGING GT_FIELDCAT_1.
* Sort data
  PERFORM F_ALV_SORT_RESULT CHANGING GT_SORT_1.
* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_LAYOUT
*----------------------------------------------------------------------*
*  ALV Layout for Report
*----------------------------------------------------------------------*
FORM F_ALV_LAYOUT CHANGING CS_LAYOUT  TYPE  LVC_S_LAYO
                           CS_VARIANT TYPE  DISVARIANT
                           CS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR:  CS_LAYOUT, CS_VARIANT, CS_PRINT.

* determine layout
  CS_LAYOUT-SEL_MODE   = 'B'. "Multiple Selection with Push Box
  CS_LAYOUT-CWIDTH_OPT = SPACE.
  CS_LAYOUT-ZEBRA      = GC_TRUE.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.

  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.

  DATA:
    LF_TEXT         TYPE  TEXT50.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'EQUNR'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
      WHEN 'DTYPE'.
        <L_FIELDCAT>-OUTPUTLEN = 15 ##NUMBER_OK.
      WHEN 'OBJECT_ID'.
*       Text-c00 : Service Order
        LF_TEXT                = TEXT-C00.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'NUMBER_INT'.
      WHEN 'INACT'.
*       Text-c01 : Cancelled
        LF_TEXT                = TEXT-C01.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = 10.
        <L_FIELDCAT>-CHECKBOX  = GC_TRUE.
        <L_FIELDCAT>-JUST      = 'C'.
      WHEN 'DATUM'.
      WHEN 'UZEIT'.
      WHEN 'STATU'.
*       Text-c02 : Status
        LF_TEXT                = TEXT-C02.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = 6.
        <L_FIELDCAT>-JUST      = 'C'.
        <L_FIELDCAT>-ICON      = GC_TRUE.
      WHEN 'MSGTY'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'MSGTX'.
        <L_FIELDCAT>-OUTPUTLEN = 50 ##NUMBER_OK.
      WHEN OTHERS.
        <L_FIELDCAT>-TECH = GC_TRUE.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT  CHANGING CT_SORT TYPE LVC_T_SORT.

  CONSTANTS:
    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'EQUNR'.

  DATA:
    LS_SORT  TYPE  LVC_S_SORT.


* Initialize Output
  CLEAR: CT_SORT.

* Sort by Equipment Number
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 1.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_COMMISSION
*----------------------------------------------------------------------*
*  Get Commissioning Equipments
*----------------------------------------------------------------------*
FORM F_GET_COMMISSION  USING  UF_GUID  TYPE  CRMS4D_SERV_H-HEADER_GUID
                              UF_DATUM TYPE  SY-DATUM
                              UF_UZEIT TYPE  SY-UZEIT
                     CHANGING CT_LIST  TYPE  TT_LIST
                              CF_SUBRC TYPE  SY-SUBRC.

* Initialize output
  CLEAR: CT_LIST,
         CF_SUBRC.

* Get SVO item Completed Updating date (Change History)
  SELECT A~OBJTYPE_H,
         A~OBJECT_ID,
         A~NUMBER_INT,
         C~COUNTER,
         C~EQUIPMENT_ID AS EQUNR,
         B~STAT,
         B~CHGNR,
         B~USNAM,
         B~UDATE,
         B~UTIME,
         B~INACT,
         B~CHIND
    FROM CRMS4D_SERV_I AS A
           INNER JOIN CRM_JCDS AS B
             ON  B~OBJNR = A~ITEM_GUID
             AND B~STAT  = @GC_CMPD
           INNER JOIN CRMS4D_REFOBJ AS C
             ON  C~OBJTYPE_H = A~OBJTYPE_H
             AND C~OBJECT_ID = A~OBJECT_ID
             AND C~NUMBER_INT = A~NUMBER_INT
   WHERE A~HEADER_GUID  EQ @UF_GUID
     AND C~EQUIPMENT_ID NE @SPACE
     AND ( B~UDATE GT @UF_DATUM OR
           ( B~UDATE EQ @UF_DATUM AND
             B~UTIME GE @UF_UZEIT ) )
   ORDER BY A~OBJTYPE_H  ASCENDING,
            A~OBJECT_ID  ASCENDING,
            A~NUMBER_INT ASCENDING,
            C~COUNTER    ASCENDING,
            B~STAT       ASCENDING,
            B~UDATE      DESCENDING,
            B~UTIME      DESCENDING
    INTO TABLE @DATA(LT_TMP).
  IF SY-SUBRC NE 0.
    CF_SUBRC = SY-SUBRC.
    RETURN.
  ENDIF.

* Get Latest for each service item
  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING OBJTYPE_H
                                                    OBJECT_ID
                                                    NUMBER_INT.
* Get Latest for each equipment
  SORT LT_TMP BY EQUNR ASCENDING
                 UDATE DESCENDING
                 UTIME DESCENDING.
  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING EQUNR.

* Assign Output
  CT_LIST = CORRESPONDING TT_LIST( LT_TMP ).

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_INSTALLATION
*----------------------------------------------------------------------*
*  text
*----------------------------------------------------------------------*
FORM F_GET_INSTALLATION  USING  UF_GUID  TYPE  CRMS4D_SERV_H-HEADER_GUID
                                UF_VBELN TYPE  LIKP-VBELN
                                UF_DATUM TYPE  SY-DATUM
                                UF_UZEIT TYPE  SY-UZEIT
                       CHANGING CT_LIST  TYPE  TT_LIST
                                CF_SUBRC TYPE  SY-SUBRC.

* Initialize output
  CLEAR: CT_LIST,
         CF_SUBRC.

* Get SVO item Completed Updating date (Change History)
  SELECT A~OBJTYPE_H,
         A~OBJECT_ID,
         A~NUMBER_INT,
         B~STAT,
         B~CHGNR,
         B~USNAM,
         B~UDATE,
         B~UTIME,
         B~INACT,
         B~CHIND
    FROM CRMS4D_SERV_I AS A
           INNER JOIN CRM_JCDS AS B
             ON  B~OBJNR = A~ITEM_GUID
             AND B~STAT  = @GC_CMPD
   WHERE A~HEADER_GUID  EQ @UF_GUID
     AND ( B~UDATE GT @UF_DATUM OR
           ( B~UDATE EQ @UF_DATUM AND
             B~UTIME GE @UF_UZEIT ) )
   ORDER BY A~OBJTYPE_H  ASCENDING,
            A~OBJECT_ID  ASCENDING,
            A~NUMBER_INT ASCENDING,
            B~STAT       ASCENDING,
            B~UDATE      DESCENDING,
            B~UTIME      DESCENDING
    INTO TABLE @DATA(LT_TMP). "LT_SERV_I.
  IF SY-SUBRC NE 0.
    CF_SUBRC = SY-SUBRC.
    RETURN.
  ENDIF.

* Get Latest for each service item
  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING OBJTYPE_H
                                                    OBJECT_ID
                                                    NUMBER_INT.
  READ TABLE LT_TMP ASSIGNING FIELD-SYMBOL(<L_TMP>)
                    INDEX 1.
  IF SY-SUBRC NE 0.
    CF_SUBRC = SY-SUBRC.
    RETURN.
  ENDIF.

* Get List of Equipment
  SELECT D~OBKNR,
         D~OBZAE,
         C~LIEF_NR,
         C~POSNR,
         D~EQUNR
    FROM SER01 AS C
           INNER JOIN OBJK AS D
             ON  D~OBKNR = C~OBKNR
   WHERE C~LIEF_NR EQ @UF_VBELN
    INTO TABLE @DATA(LT_SER01).
  IF SY-SUBRC NE 0.
    CF_SUBRC = SY-SUBRC.
    RETURN.
  ENDIF.

* Assign Output
  LOOP AT LT_SER01 ASSIGNING FIELD-SYMBOL(<L_SER01>).

    INSERT VALUE #(
         EQUNR      = <L_SER01>-EQUNR
         UDATE      = <L_TMP>-UDATE
         INACT      = <L_TMP>-INACT
         OBJECT_ID  = <L_TMP>-OBJECT_ID
         NUMBER_INT = <L_TMP>-NUMBER_INT
         UTIME      = <L_TMP>-UTIME )
           INTO TABLE CT_LIST.

  ENDLOOP.

ENDFORM.
