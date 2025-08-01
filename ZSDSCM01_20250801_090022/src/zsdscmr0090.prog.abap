*-----------------------------------------------------------------------
*  Program ID         : ZSDSCMR0020
*  Creation Date      : 22.08.2024
*  Author             : Jutamas Y.(Eviden)
*  Add-on ID          : ZCMI016
*  Description        : Out-Bound Warranty to Salesforce
*  Purpose            : Get data Warranty and gen Interface file to
*                       salesforce
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSCMR0090.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  ZSDSCMT003,MARA.


*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_RESULT.
         INCLUDE TYPE ZSDSCMS016.
TYPES:
         LINECOLOR TYPE  CHAR4,
       END OF TS_RESULT.
TYPES: TT_RESULT TYPE STANDARD TABLE OF TS_RESULT.

TYPES: BEGIN OF TS_ZSDSCMT003 ,
         EQUNR       TYPE ZSDSCMT003-EQUNR,
         MATNR       TYPE ZSDSCMT003-MATNR,
         INSDT       TYPE ZSDSCMT003-INSDT,
         COMDT       TYPE ZSDSCMT003-COMDT,
         MAT_POSID   TYPE ZSDSCMT003-MAT_POSID,
         STD_POSID   TYPE ZSDSCMT003-STD_POSID,
         EXT_POSID   TYPE ZSDSCMT003-EXT_POSID,
         STD_WRT_BEG TYPE ZSDSCMT003-STD_WRT_BEG,
         STD_WRT_END TYPE ZSDSCMT003-STD_WRT_END,
         EXT_WRT_BEG TYPE ZSDSCMT003-EXT_WRT_BEG,
         EXT_WRT_END TYPE ZSDSCMT003-EXT_WRT_END,
         WRTLT_FLAG  TYPE ZSDSCMT003-WRTLT_FLAG,
         WRTLT       TYPE ZSDSCMT003-WRTLT,
         WRTPK       TYPE ZSDSCMT003-WRTPK,
         CUS_REG_BEG TYPE ZSDSCMT003-CUS_REG_BEG,
         CUS_REG_END TYPE ZSDSCMT003-CUS_REG_END,
         GI_WRT_BEG  TYPE ZSDSCMT003-GI_WRT_BEG,
         GI_WRT_END  TYPE ZSDSCMT003-GI_WRT_END,
         MIGFLG      TYPE ZSDSCMT003-MIGFLG,
         WADAT_IST   TYPE ZSDSCMT003-WADAT_IST,
         UPDFLG      TYPE ZSDSCMT003-UPDFLG,
         SERNR       TYPE EQUI-SERNR,
         HERST       TYPE EQUI-HERST,
         SERGE       TYPE EQUI-SERGE,
         AULDT       TYPE EQUI-AULDT,
         PRDHA       TYPE MARA-PRDHA,
       END  OF TS_ZSDSCMT003 .
TYPES TT_ZSDSCMT003 TYPE STANDARD TABLE OF TS_ZSDSCMT003 .

TYPES: BEGIN OF TS_INTF_TXT,
         EQUNR(18)             TYPE C,
         EQUID(35)             TYPE C,
         MATNR(40)             TYPE C,
         SERNR(18)             TYPE C,
         HERST(30)             TYPE C,
         SERGE(30)             TYPE C,
         DEL_INDICATOR(1)      TYPE C,
         DELIVERY_DATE(10)     TYPE C,
         NAME(255)             TYPE C,
         PHONE(30)             TYPE C,
         ADDRESS(255)          TYPE C,
         DISTRICT(255)         TYPE C,
         SUB_DISTRICT(255)     TYPE C,
         PROVINCE(255)         TYPE C,
         ZIPCODE(5)            TYPE C,
         STATUS(30)            TYPE C,
         GRDT(10)              TYPE C,
         INSDT(10)             TYPE C,
         COMDT(10)             TYPE C,
         MAT_POSID(24)         TYPE C,
         STD_POSID(24)         TYPE C,
         EXT_POSID(24)         TYPE C,
         STD_WRT_BEG(10)       TYPE C,
         STD_WRT_END(10)       TYPE C,
         EXT_WRT_BEG(10)       TYPE C,
         EXT_WRT_END(10)       TYPE C,
         WRTLT_FLAG(1)         TYPE C,
         WRTLT(10)             TYPE C,
         CUS_REG_BEG(10)       TYPE C,
         CUS_REG_END(10)       TYPE C,
         WRT_ENDDAT(10)        TYPE C,
         GI_WRT_BEG(10)        TYPE C,
         GI_WRT_END(10)        TYPE C,
         MIGFLG(10)            TYPE C,
         SP_COMP_STD_BEG(10)   TYPE C,
         SP_COMP_STD_END(10)   TYPE C,
         SV_COMP_STD_BEG(10)   TYPE C,
         SV_COMP_STD_END(10)   TYPE C,
         SP_COMP_EXT_BEG(10)   TYPE C,
         SP_COMP_EXT_END(10)   TYPE C,
         SV_COMP_EXT_BEG(10)   TYPE C,
         SV_COMP_EXT_END(10)   TYPE C,
         SP_COND_STD_BEG(10)   TYPE C,
         SP_COND_STD_END(10)   TYPE C,
         SV_COND_STD_BEG(10)   TYPE C,
         SV_COND_STD_END(10)   TYPE C,
         SP_COND_EXT_BEG(10)   TYPE C,
         SP_COND_EXT_END(10)   TYPE C,
         SV_COND_EXT_BEG(10)   TYPE C,
         SV_COND_EXT_END(10)   TYPE C,
         SP_EVAP_STD_BEG(10)   TYPE C,
         SP_EVAP_STD_END(10)   TYPE C,
         SV_EVAP_STD_BEG(10)   TYPE C,
         SV_EVAP_STD_END(10)   TYPE C,
         SP_EVAP_EXT_BEG(10)   TYPE C,
         SP_EVAP_EXT_END(10)   TYPE C,
         SV_EVAP_EXT_BEG(10)   TYPE C,
         SV_EVAP_EXT_END(10)   TYPE C,
         SP_PCBIN_STD_BEG(10)  TYPE C,
         SP_PCBIN_STD_END(10)  TYPE C,
         SV_PCBIN_STD_BEG(10)  TYPE C,
         SV_PCBIN_STD_END(10)  TYPE C,
         SP_PCBIN_EXT_BEG(10)  TYPE C,
         SP_PCBIN_EXT_END(10)  TYPE C,
         SV_PCBIN_EXT_BEG(10)  TYPE C,
         SV_PCBIN_EXT_END(10)  TYPE C,
         SP_PCBOUT_STD_BEG(10) TYPE C,
         SP_PCBOUT_STD_END(10) TYPE C,
         SV_PCBOUT_STD_BEG(10) TYPE C,
         SV_PCBOUT_STD_END(10) TYPE C,
         SP_PCBOUT_EXT_BEG(10) TYPE C,
         SP_PCBOUT_EXT_END(10) TYPE C,
         SV_PCBOUT_EXT_BEG(10) TYPE C,
         SV_PCBOUT_EXT_END(10) TYPE C,
         SP_FAN_STD_BEG(10)    TYPE C,
         SP_FAN_STD_END(10)    TYPE C,
         SV_FAN_STD_BEG(10)    TYPE C,
         SV_FAN_STD_END(10)    TYPE C,
         SP_FAN_EXT_BEG(10)    TYPE C,
         SP_FAN_EXT_END(10)    TYPE C,
         SV_FAN_EXT_BEG(10)    TYPE C,
         SV_FAN_EXT_END(10)    TYPE C,
         SP_CASING_STD_BEG(10) TYPE C,
         SP_CASING_STD_END(10) TYPE C,
         SV_CASING_STD_BEG(10) TYPE C,
         SV_CASING_STD_END(10) TYPE C,
         SP_CASING_EXT_BEG(10) TYPE C,
         SP_CASING_EXT_END(10) TYPE C,
         SV_CASING_EXT_BEG(10) TYPE C,
         SV_CASING_EXT_END(10) TYPE C,
         SP_OTHER_STD_BEG(10)  TYPE C,
         SP_OTHER_STD_END(10)  TYPE C,
         SV_OTHER_STD_BEG(10)  TYPE C,
         SV_OTHER_STD_END(10)  TYPE C,
         SP_OTHER_EXT_BEG(10)  TYPE C,
         SP_OTHER_EXT_END(10)  TYPE C,
         SV_OTHER_EXT_BEG(10)  TYPE C,
         SV_OTHER_EXT_END(10)  TYPE C,
       END OF TS_INTF_TXT.


TYPES: BEGIN OF TS_MAPFIELD,
         FIELDNAME(15) TYPE  C,
         MATKL         TYPE  MARA-MATKL,
       END OF TS_MAPFIELD .
TYPES TT_MAPFIELD TYPE STANDARD TABLE OF TS_MAPFIELD .

TYPES: BEGIN OF TS_PROD_WRTTYP,
         PRDHA     TYPE MARA-PRDHA,
         MATWLTYPE TYPE ZSDSCMT006-MATWLTYPE,
       END OF TS_PROD_WRTTYP .
TYPES TT_PROD_WRTTYP TYPE STANDARD TABLE OF TS_PROD_WRTTYP .

TYPES:
  TT_ZSDSCMC002 TYPE STANDARD TABLE OF ZSDSCMC002,
  TT_ZSDSCMT004 TYPE STANDARD TABLE OF ZSDSCMT004,
  TT_ZSDSCMT006 TYPE STANDARD TABLE OF ZSDSCMT006,
  TT_ZSDSCMT010 TYPE HASHED TABLE OF ZSDSCMT010 WITH UNIQUE KEY MATNR SERNR.
*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
##NEEDED
CONSTANTS:
  GC_TRUE        TYPE  CHAR1     VALUE 'X',
  GC_TCODE       TYPE  SY-TCODE  VALUE 'ZSDSCM005',
  GC_INTFNO_SFDC TYPE  ZSDSCAC004-INTFNO VALUE 'CMI016'. "SFDC
*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
##NEEDED
DATA:
  GT_RESULT     TYPE TT_RESULT.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
##NEEDED
DATA: GT_MAPFIELD TYPE TT_MAPFIELD,
      GT_PROD     TYPE TT_PROD_WRTTYP.

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSCMS016'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 0,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 100.
*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*
DEFINE MC_SHOW_PROGRESS.
  IF SY-BATCH IS INITIAL.
*   Show progress online
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = &1 ##NUMBER_OK
        TEXT       = &2.
  ELSE.
*   Show message step in background
    MESSAGE I000(38) WITH &2 SPACE SPACE SPACE.
  ENDIF.
END-OF-DEFINITION.

DEFINE CONVERT_ALPHA_OUTPUT.
  IF &1 IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = &1
      IMPORTING
        OUTPUT = &2.
  ENDIF.
END-OF-DEFINITION.

DEFINE CONVERT_DATE_OUTPUT.
  IF &1 IS NOT INITIAL AND
     &1 NE '0000'.
    CONCATENATE &1 '-' &2 '-' &3
      INTO &4.
  ENDIF.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  SELECT-OPTIONS:
    S_EQUNR  FOR ZSDSCMT003-EQUNR,
    S_GI     FOR ZSDSCMT003-WADAT_IST,
    S_BEG    FOR ZSDSCMT003-STD_WRT_BEG,
    S_END    FOR ZSDSCMT003-STD_WRT_END,
    S_PRDHA  FOR MARA-PRDHA,
    S_ERNAM  FOR ZSDSCMT003-ERNAM,
    S_ERDAT  FOR ZSDSCMT003-ERDAT,
    S_ERZET  FOR ZSDSCMT003-ERZET,
    S_AENAM  FOR ZSDSCMT003-AENAM,
    S_AEDAT  FOR ZSDSCMT003-AEDAT,
    S_AEZET  FOR ZSDSCMT003-AEZET.
  PARAMETERS:
    P_UPDT TYPE  ZSDSCMT003-UPDFLG.
SELECTION-SCREEN END OF BLOCK B1.

PARAMETERS:
  P_SPECI AS CHECKBOX,
  P_WATRG AS CHECKBOX,
  P_TEST  AS CHECKBOX.


*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
  PERFORM F_INITAIL_DATA .
  PERFORM F_GET_CONSTANTS.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*



*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_PROCESS_DATA .
  IF GT_RESULT IS INITIAL.
*   Message: No data found.
    MESSAGE S001(ZSDSCA01) DISPLAY LIKE 'E'.
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


*&---------------------------------------------------------------------*
*& Form F_GET_DATA
*&---------------------------------------------------------------------*
FORM F_GET_DATA CHANGING CT_ZSDSCMT003 TYPE TT_ZSDSCMT003
                         CT_ZSDSCMC002 TYPE TT_ZSDSCMC002
                         CT_ZSDSCMT004 TYPE TT_ZSDSCMT004
                         CT_ZSDSCMT006 TYPE TT_ZSDSCMT006
                         CT_ZSDSCMT010 TYPE TT_ZSDSCMT010 .

  DATA: LR_WL_ID TYPE RANGE OF ZSDSCMT006-WL_ID .

* ZSDSCMT003 SDS: Warranty Master
  IF P_WATRG EQ ABAP_TRUE.
    SELECT DISTINCT
      ZSDSCMT003~EQUNR,
      ZSDSCMT003~MATNR,
      ZSDSCMT003~INSDT ,
      ZSDSCMT003~COMDT,
      ZSDSCMT003~MAT_POSID,
      ZSDSCMT003~STD_POSID,
      ZSDSCMT003~EXT_POSID,
      ZSDSCMT003~STD_WRT_BEG,
      ZSDSCMT003~STD_WRT_END,
      ZSDSCMT003~EXT_WRT_BEG,
      ZSDSCMT003~EXT_WRT_END,
      ZSDSCMT003~WRTLT_FLAG,
      ZSDSCMT003~WRTLT,
      ZSDSCMT003~WRTPK,
      ZSDSCMT003~CUS_REG_BEG,
      ZSDSCMT003~CUS_REG_END,
      ZSDSCMT003~GI_WRT_BEG,
      ZSDSCMT003~GI_WRT_END,
      ZSDSCMT003~MIGFLG ,
      ZSDSCMT003~WADAT_IST,
      ZSDSCMT003~UPDFLG,
      EQUI~SERNR ,
      EQUI~HERST ,
      EQUI~SERGE ,
      EQUI~AULDT ,
      MARA~PRDHA
     FROM ZSDSCMT003 INNER JOIN EQUI       ON ZSDSCMT003~EQUNR = EQUI~EQUNR
                     INNER JOIN MARA       ON ZSDSCMT003~MATNR = MARA~MATNR
                     INNER JOIN ZSDSCMT004 ON ZSDSCMT003~MATNR = ZSDSCMT004~MATNR AND
                                              ZSDSCMT003~SERNR = ZSDSCMT004~SERNR
     WHERE ZSDSCMT003~EQUNR       IN @S_EQUNR
       AND ZSDSCMT003~WADAT_IST   IN @S_GI
       AND MARA~PRDHA             IN @S_PRDHA
       AND ZSDSCMT003~STD_WRT_BEG IN @S_BEG
       AND ZSDSCMT003~STD_WRT_END IN @S_END
       AND ZSDSCMT003~ERNAM       IN @S_ERNAM
       AND ZSDSCMT003~ERDAT       IN @S_ERDAT
       AND ZSDSCMT003~ERZET       IN @S_ERZET
       AND ZSDSCMT003~AENAM       IN @S_AENAM
       AND ZSDSCMT003~AEDAT       IN @S_AEDAT
       AND ZSDSCMT003~AEZET       IN @S_AEZET
       AND ZSDSCMT003~UPDFLG      EQ @P_UPDT
      INTO TABLE @CT_ZSDSCMT003.
  ELSE.
    SELECT
      ZSDSCMT003~EQUNR,
      ZSDSCMT003~MATNR,
      ZSDSCMT003~INSDT ,
      ZSDSCMT003~COMDT,
      ZSDSCMT003~MAT_POSID,
      ZSDSCMT003~STD_POSID,
      ZSDSCMT003~EXT_POSID,
      ZSDSCMT003~STD_WRT_BEG,
      ZSDSCMT003~STD_WRT_END,
      ZSDSCMT003~EXT_WRT_BEG,
      ZSDSCMT003~EXT_WRT_END,
      ZSDSCMT003~WRTLT_FLAG,
      ZSDSCMT003~WRTLT,
      ZSDSCMT003~WRTPK,
      ZSDSCMT003~CUS_REG_BEG,
      ZSDSCMT003~CUS_REG_END,
      ZSDSCMT003~GI_WRT_BEG,
      ZSDSCMT003~GI_WRT_END,
      ZSDSCMT003~MIGFLG ,
      ZSDSCMT003~WADAT_IST,
      ZSDSCMT003~UPDFLG,
      EQUI~SERNR ,
      EQUI~HERST ,
      EQUI~SERGE ,
      EQUI~AULDT ,
      MARA~PRDHA
     FROM ZSDSCMT003 INNER JOIN EQUI ON ZSDSCMT003~EQUNR = EQUI~EQUNR
                     INNER JOIN MARA ON ZSDSCMT003~MATNR = MARA~MATNR
     WHERE ZSDSCMT003~EQUNR       IN @S_EQUNR
       AND ZSDSCMT003~WADAT_IST   IN @S_GI
       AND MARA~PRDHA             IN @S_PRDHA
       AND ZSDSCMT003~STD_WRT_BEG IN @S_BEG
       AND ZSDSCMT003~STD_WRT_END IN @S_END
       AND ZSDSCMT003~ERNAM       IN @S_ERNAM
       AND ZSDSCMT003~ERDAT       IN @S_ERDAT
       AND ZSDSCMT003~ERZET       IN @S_ERZET
       AND ZSDSCMT003~AENAM       IN @S_AENAM
       AND ZSDSCMT003~AEDAT       IN @S_AEDAT
       AND ZSDSCMT003~AEZET       IN @S_AEZET
       AND ZSDSCMT003~UPDFLG      EQ @P_UPDT
      INTO TABLE @CT_ZSDSCMT003.
  ENDIF.

  IF CT_ZSDSCMT003[] IS NOT INITIAL .
    SORT CT_ZSDSCMT003 BY EQUNR .

    LOOP AT CT_ZSDSCMT003 ASSIGNING FIELD-SYMBOL(<LF_T003>).
      APPEND VALUE #( SIGN   = 'I'
                      OPTION = 'EQ'
                      LOW    = <LF_T003>-WRTLT
                      HIGH   = '' )
          TO LR_WL_ID.

    ENDLOOP .
    SORT LR_WL_ID BY LOW .
    DELETE ADJACENT DUPLICATES FROM LR_WL_ID COMPARING LOW .

* ZSDSCMC002 SDS: Warranty Packages
    SELECT *
      FROM ZSDSCMC002
       FOR ALL ENTRIES IN @CT_ZSDSCMT003
     WHERE WRTPK = @CT_ZSDSCMT003-WRTPK
      INTO TABLE @CT_ZSDSCMC002 .
    IF SY-SUBRC = 0 .
      SORT CT_ZSDSCMC002 BY WRTPK MATKL .
    ENDIF.

* ZSDSCMT004 Register Warranty
    SELECT *
      FROM ZSDSCMT004
       FOR ALL ENTRIES IN @CT_ZSDSCMT003
     WHERE MATNR = @CT_ZSDSCMT003-MATNR
       AND SERNR = @CT_ZSDSCMT003-SERNR
      INTO TABLE @CT_ZSDSCMT004.
    IF SY-SUBRC = 0 .
      SORT CT_ZSDSCMT004 BY MATNR SERNR .
    ENDIF.

    SELECT *
      FROM ZSDSCMT006
      INTO TABLE @CT_ZSDSCMT006
      WHERE WL_ID IN @LR_WL_ID .
    IF SY-SUBRC = 0 .
      SORT CT_ZSDSCMT006 BY WL_ID MATWLTYPE .
    ENDIF.

    SELECT *
      FROM ZSDSCMT010
       FOR ALL ENTRIES IN @CT_ZSDSCMT003
     WHERE MATNR = @CT_ZSDSCMT003-MATNR
       AND SERNR = @CT_ZSDSCMT003-SERNR
      INTO TABLE @CT_ZSDSCMT010.
    IF SY-SUBRC = 0 .
      SORT CT_ZSDSCMT010 BY MATNR SERNR .
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_process_data
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA .
  DATA:
    LT_ZSDSCMC002 TYPE TT_ZSDSCMC002,
    LT_ZSDSCMT003 TYPE TT_ZSDSCMT003,
    LT_ZSDSCMT004 TYPE TT_ZSDSCMT004,
    LT_ZSDSCMT006 TYPE TT_ZSDSCMT006,
    LT_ZSDSCMT010 TYPE TT_ZSDSCMT010.

  PERFORM F_GET_DATA CHANGING LT_ZSDSCMT003
                              LT_ZSDSCMC002
                              LT_ZSDSCMT004
                              LT_ZSDSCMT006
                              LT_ZSDSCMT010.

  PERFORM F_SET_DATA USING LT_ZSDSCMT003
                           LT_ZSDSCMC002
                           LT_ZSDSCMT004
                           LT_ZSDSCMT006
                           LT_ZSDSCMT010
                  CHANGING GT_RESULT.

  IF P_TEST EQ SPACE.
    PERFORM F_CREATE_INTF_FILE  USING  GT_RESULT
                                       LT_ZSDSCMT003 .
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_INITAIL_DATA
*&---------------------------------------------------------------------*
*& Initial data
*&---------------------------------------------------------------------*
FORM F_INITAIL_DATA .
  CLEAR :
    GT_RESULT[] .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DISPLAY_RESULT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_RESULT
*&---------------------------------------------------------------------*
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
** No auto refresh in edit mode
*  GF_NO_AUTO_REFRESH_1 = GC_TRUE.

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
*&---------------------------------------------------------------------*
*& Form F_ALV_LAYOUT
*&---------------------------------------------------------------------*
*& ALV Layout for Report
*&---------------------------------------------------------------------*
FORM F_ALV_LAYOUT  CHANGING CS_LAYOUT  TYPE  LVC_S_LAYO
                            CS_VARIANT TYPE  DISVARIANT
                            CS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR:  CS_LAYOUT, CS_VARIANT, CS_PRINT.

* determine layout
  CS_LAYOUT-SEL_MODE   = 'A'.
  CS_LAYOUT-CWIDTH_OPT = GC_TRUE.
  CS_LAYOUT-ZEBRA      = SPACE.
  CS_LAYOUT-INFO_FNAME = 'LINECOLOR'.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.
*  CS_VARIANT-VARIANT = P_VARI.
  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*& ALV Field Catalog for Report
*&---------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT  CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.

  DATA:
    LF_TEXT         TYPE  TEXT50.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.



* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT> .
    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'EQUID'.
        "Text-A01 : Mat No. & Serial No.
        LF_TEXT                = TEXT-A01.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SERNR' .
        "Text-A02 : Serial No.
        LF_TEXT                = TEXT-A02.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'DEL_INDICATOR' .
        "Text-A03 : Del Int.
        LF_TEXT                = TEXT-A03.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_COMP_STD_BEG' .
        LF_TEXT                = TEXT-H01.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.

      WHEN 'SP_COMP_STD_END'  .
        LF_TEXT                = TEXT-H02.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_COMP_STD_BEG'  .
        LF_TEXT                = TEXT-H03.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_COMP_STD_END'  .
        LF_TEXT                = TEXT-H04.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_COMP_EXT_BEG'  .
        LF_TEXT                = TEXT-H05.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_COMP_EXT_END'  .
        LF_TEXT                = TEXT-H06.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_COMP_EXT_BEG'  .
        LF_TEXT                = TEXT-H07.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_COMP_EXT_END'  .
        LF_TEXT                = TEXT-H08.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_COND_STD_BEG'  .
        LF_TEXT                = TEXT-H09.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_COND_STD_END'  .
        LF_TEXT                = TEXT-H10.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_COND_STD_BEG'  .
        LF_TEXT                = TEXT-H11.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_COND_STD_END'  .
        LF_TEXT                = TEXT-H12.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_COND_EXT_BEG'  .
        LF_TEXT                = TEXT-H13.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_COND_EXT_END'  .
        LF_TEXT                = TEXT-H14.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_COND_EXT_BEG'  .
        LF_TEXT                = TEXT-H15.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_COND_EXT_END'  .
        LF_TEXT                = TEXT-H16.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_EVAP_STD_BEG'  .
        LF_TEXT                = TEXT-H17.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_EVAP_STD_END'  .
        LF_TEXT                = TEXT-H18.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_EVAP_STD_BEG'  .
        LF_TEXT                = TEXT-H19.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_EVAP_STD_END'  .
        LF_TEXT                = TEXT-H20.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_EVAP_EXT_BEG'  .
        LF_TEXT                = TEXT-H21.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_EVAP_EXT_END'  .
        LF_TEXT                = TEXT-H22.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_EVAP_EXT_BEG'  .
        LF_TEXT                = TEXT-H23.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_EVAP_EXT_END'  .
        LF_TEXT                = TEXT-H24.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_PCBIN_STD_BEG' .
        LF_TEXT                = TEXT-H25.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_PCBIN_STD_END' .
        LF_TEXT                = TEXT-H26.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_PCBIN_STD_BEG' .
        LF_TEXT                = TEXT-H27.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_PCBIN_STD_END' .
        LF_TEXT                = TEXT-H28.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_PCBIN_EXT_BEG' .
        LF_TEXT                = TEXT-H29.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_PCBIN_EXT_END' .
        LF_TEXT                = TEXT-H30.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_PCBIN_EXT_BEG' .
        LF_TEXT                = TEXT-H31.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_PCBIN_EXT_END' .
        LF_TEXT                = TEXT-H32.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_PCBOUT_STD_BEG'.
        LF_TEXT                = TEXT-H33.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_PCBOUT_STD_END'.
        LF_TEXT                = TEXT-H34.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_PCBOUT_STD_BEG'.
        LF_TEXT                = TEXT-H35.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_PCBOUT_STD_END'.
        LF_TEXT                = TEXT-H36.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_PCBOUT_EXT_BEG'.
        LF_TEXT                = TEXT-H37.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_PCBOUT_EXT_END'.
        LF_TEXT                = TEXT-H38.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_PCBOUT_EXT_BEG'.
        LF_TEXT                = TEXT-H39.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_PCBOUT_EXT_END'.
        LF_TEXT                = TEXT-H40.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_FAN_STD_BEG'   .
        LF_TEXT                = TEXT-H41.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_FAN_STD_END'   .
        LF_TEXT                = TEXT-H42.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_FAN_STD_BEG'   .
        LF_TEXT                = TEXT-H43.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_FAN_STD_END'   .
        LF_TEXT                = TEXT-H44.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_FAN_EXT_BEG'   .
        LF_TEXT                = TEXT-H45.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_FAN_EXT_END'   .
        LF_TEXT                = TEXT-H46.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_FAN_EXT_BEG'   .
        LF_TEXT                = TEXT-H47.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_FAN_EXT_END'   .
        LF_TEXT                = TEXT-H48.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_CASING_STD_BEG'.
        LF_TEXT                = TEXT-H49.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_CASING_STD_END'.
        LF_TEXT                = TEXT-H50.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_CASING_STD_BEG'.
        LF_TEXT               = TEXT-H51.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_CASING_STD_END'.
        LF_TEXT               = TEXT-H52.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_CASING_EXT_BEG'.
        LF_TEXT               = TEXT-H53.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_CASING_EXT_END'.
        LF_TEXT               = TEXT-H54.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_CASING_EXT_BEG'.
        LF_TEXT               = TEXT-H55.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_CASING_EXT_END'.
        LF_TEXT               = TEXT-H56.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_OTHER_STD_BEG' .
        LF_TEXT               = TEXT-H57.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_OTHER_STD_END' .
        LF_TEXT               = TEXT-H58.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_OTHER_STD_BEG' .
        LF_TEXT               = TEXT-H59.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_OTHER_STD_END' .
        LF_TEXT               = TEXT-H60.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_OTHER_EXT_BEG' .
        LF_TEXT               = TEXT-H61.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SP_OTHER_EXT_END' .
        LF_TEXT               = TEXT-H62.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_OTHER_EXT_BEG' .
        LF_TEXT               = TEXT-H63.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SV_OTHER_EXT_END' .
        LF_TEXT               = TEXT-H64.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'GRDT'.
        "Text-H65 : GR Date
        LF_TEXT                = TEXT-H65.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
    ENDCASE.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_SORT_RESULT
*&---------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT  CHANGING CT_SORT TYPE LVC_T_SORT.

* Initialize Output
  CLEAR: CT_SORT.
*  CLEAR LS_SORT.
*  LS_SORT-SPOS      = 1.
*  LS_SORT-FIELDNAME = LC_SORT1.
*  LS_SORT-UP        = GC_TRUE.
*  LS_SORT-SUBTOT    = SPACE.
*  APPEND LS_SORT TO CT_SORT.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_DATA
*&---------------------------------------------------------------------*
*& Set data to Result
*&---------------------------------------------------------------------*
FORM F_SET_DATA  USING UT_ZSDSCMT003 TYPE TT_ZSDSCMT003
                       UT_ZSDSCMC002 TYPE TT_ZSDSCMC002
                       UT_ZSDSCMT004 TYPE TT_ZSDSCMT004
                       UT_ZSDSCMT006 TYPE TT_ZSDSCMT006
                       UT_ZSDSCMT010 TYPE TT_ZSDSCMT010
              CHANGING CT_RESULT     TYPE TT_RESULT.

  DATA: LS_RESULT  TYPE TS_RESULT,
        LF_STD_BEG TYPE ZSDSCMT003-EXT_WRT_BEG.

  LOOP AT UT_ZSDSCMT003 ASSIGNING FIELD-SYMBOL(<F_ZSDSCMT003>).
    CLEAR: LS_RESULT , LF_STD_BEG .
    MOVE-CORRESPONDING <F_ZSDSCMT003> TO LS_RESULT .
    CLEAR <F_ZSDSCMT003>-UPDFLG  .

    IF <F_ZSDSCMT003>-WADAT_IST LT '20241101'.
      READ TABLE UT_ZSDSCMT010 INTO DATA(LS_ZSDSCMT010)
      WITH TABLE KEY MATNR = <F_ZSDSCMT003>-MATNR
                     SERNR = <F_ZSDSCMT003>-SERNR.
      IF SY-SUBRC EQ 0.
        <F_ZSDSCMT003>-STD_WRT_BEG = LS_ZSDSCMT010-GWLDT.
        <F_ZSDSCMT003>-STD_WRT_END = LS_ZSDSCMT010-GWLEN.
      ENDIF.
    ENDIF.

    CONVERT_ALPHA_OUTPUT  <F_ZSDSCMT003>-EQUNR  LS_RESULT-EQUNR.
    CONVERT_ALPHA_OUTPUT  <F_ZSDSCMT003>-MATNR  LS_RESULT-MATNR.

    CALL FUNCTION 'CONVERSION_EXIT_GERNR_OUTPUT'
      EXPORTING
        INPUT  = <F_ZSDSCMT003>-SERNR
      IMPORTING
        OUTPUT = LS_RESULT-SERNR.

    CONCATENATE <F_ZSDSCMT003>-MATNR LS_RESULT-SERNR
           INTO LS_RESULT-EQUID.

    READ TABLE UT_ZSDSCMT004 ASSIGNING FIELD-SYMBOL(<F_ZSDSCMT004>)
       WITH KEY  MATNR = <F_ZSDSCMT003>-MATNR
                 SERNR = <F_ZSDSCMT003>-SERNR
                 BINARY SEARCH.
    IF SY-SUBRC = 0 .
      LS_RESULT-NAME         =  <F_ZSDSCMT004>-NAME .
      LS_RESULT-PHONE        =  <F_ZSDSCMT004>-PHONE.
      LS_RESULT-ADDRESS      =  <F_ZSDSCMT004>-ADDR1.
      LS_RESULT-DISTRICT     =  <F_ZSDSCMT004>-ADDR2.
      LS_RESULT-SUB_DISTRICT =  <F_ZSDSCMT004>-ADDR3.
      LS_RESULT-PROVINCE     =  <F_ZSDSCMT004>-PROVINCE.
      IF <F_ZSDSCMT004>-ZIPCODE IS INITIAL  .
        LS_RESULT-ZIPCODE    =  <F_ZSDSCMT004>-ZIPCODE.
      ENDIF.
    ENDIF.

    LS_RESULT-DEL_INDICATOR = '' .
    LS_RESULT-STATUS = '' .
    LS_RESULT-AULDT   = <F_ZSDSCMT003>-WADAT_IST .

    IF <F_ZSDSCMT003>-CUS_REG_BEG IS NOT INITIAL.
      LS_RESULT-STD_WRT_BEG = <F_ZSDSCMT003>-CUS_REG_BEG.
    ELSE.
      IF <F_ZSDSCMT003>-GI_WRT_BEG LE LS_RESULT-STD_WRT_BEG.
        LS_RESULT-STD_WRT_BEG = <F_ZSDSCMT003>-STD_WRT_BEG.
      ELSE.
        IF P_SPECI EQ ABAP_TRUE.
          CONTINUE.
        ENDIF.
        LS_RESULT-STD_WRT_BEG = <F_ZSDSCMT003>-GI_WRT_BEG.
      ENDIF.
    ENDIF.

    IF <F_ZSDSCMT003>-CUS_REG_END IS NOT INITIAL.
      LS_RESULT-STD_WRT_END = <F_ZSDSCMT003>-CUS_REG_END.
    ELSE.
      LS_RESULT-STD_WRT_END = <F_ZSDSCMT003>-STD_WRT_END .
    ENDIF.
    "Set Standard Start Date
    IF LS_RESULT-CUS_REG_BEG IS NOT INITIAL AND
       LS_RESULT-WRTLT_FLAG  IS NOT INITIAL .
      LF_STD_BEG  = LS_RESULT-CUS_REG_BEG .
    ELSE.
      LF_STD_BEG  = LS_RESULT-STD_WRT_BEG .
    ENDIF.

    LS_RESULT-SP_COMP_STD_BEG   = LF_STD_BEG .
    LS_RESULT-SV_COMP_STD_BEG   = LF_STD_BEG .
    LS_RESULT-SP_COND_STD_BEG   = LF_STD_BEG .
    LS_RESULT-SV_COND_STD_BEG   = LF_STD_BEG .
    LS_RESULT-SP_EVAP_STD_BEG   = LF_STD_BEG .
    LS_RESULT-SV_EVAP_STD_BEG   = LF_STD_BEG .
    LS_RESULT-SP_PCBIN_STD_BEG  = LF_STD_BEG .
    LS_RESULT-SV_PCBIN_STD_BEG  = LF_STD_BEG .
    LS_RESULT-SP_PCBOUT_STD_BEG = LF_STD_BEG .
    LS_RESULT-SV_PCBOUT_STD_BEG = LF_STD_BEG .
    LS_RESULT-SP_FAN_STD_BEG    = LF_STD_BEG .
    LS_RESULT-SV_FAN_STD_BEG    = LF_STD_BEG .
    LS_RESULT-SP_CASING_STD_BEG = LF_STD_BEG .
    LS_RESULT-SV_CASING_STD_BEG = LF_STD_BEG .
    LS_RESULT-SP_OTHER_STD_BEG  = LF_STD_BEG .
    LS_RESULT-SV_OTHER_STD_BEG  = LF_STD_BEG .


    "WarrantyLetterEndDate
    IF <F_ZSDSCMT003>-WRTLT_FLAG IS NOT INITIAL .  "IsRequiredWarrantyL
      IF LS_RESULT-EXT_WRT_END IS NOT INITIAL .
        LS_RESULT-WRT_ENDDAT = LS_RESULT-EXT_WRT_END .
      ELSE.
        LS_RESULT-WRT_ENDDAT = LS_RESULT-STD_WRT_END .
      ENDIF.
    ENDIF.

*- Set End date for Standard Warranty
    PERFORM F_SET_STD_ENDDATE USING UT_ZSDSCMC002
                                    <F_ZSDSCMT003>
                           CHANGING LS_RESULT .

*- Set Beg date for Extended Warranty
    LS_RESULT-SP_COMP_EXT_BEG   = LS_RESULT-SP_COMP_STD_END + 1 .
    LS_RESULT-SV_COMP_EXT_BEG   = LS_RESULT-SV_COMP_STD_END + 1 .
    LS_RESULT-SP_COND_EXT_BEG   = LS_RESULT-SP_COND_STD_END + 1 .
    LS_RESULT-SV_COND_EXT_BEG   = LS_RESULT-SV_COND_STD_END + 1 .
    LS_RESULT-SP_EVAP_EXT_BEG   = LS_RESULT-SP_EVAP_STD_END  + 1 .
    LS_RESULT-SV_EVAP_EXT_BEG   = LS_RESULT-SV_EVAP_STD_END  + 1 .
    LS_RESULT-SP_PCBIN_EXT_BEG  = LS_RESULT-SP_PCBIN_STD_END + 1 .
    LS_RESULT-SV_PCBIN_EXT_BEG  = LS_RESULT-SV_PCBIN_STD_END + 1 .
    LS_RESULT-SP_PCBOUT_EXT_BEG = LS_RESULT-SP_PCBOUT_STD_END + 1 .
    LS_RESULT-SV_PCBOUT_EXT_BEG = LS_RESULT-SV_PCBOUT_STD_END + 1 .
    LS_RESULT-SP_CASING_EXT_BEG = LS_RESULT-SP_CASING_STD_END + 1 .
    LS_RESULT-SV_CASING_EXT_BEG = LS_RESULT-SV_CASING_STD_END + 1 .
    LS_RESULT-SP_FAN_EXT_BEG    = LS_RESULT-SP_FAN_STD_END + 1 .
    LS_RESULT-SV_FAN_EXT_BEG    = LS_RESULT-SV_FAN_STD_END + 1 .
    LS_RESULT-SV_OTHER_EXT_BEG  = LS_RESULT-SV_OTHER_STD_END + 1 .
    LS_RESULT-SP_OTHER_EXT_BEG  = LS_RESULT-SP_OTHER_STD_END + 1 .


*- Set End date for Extended Warranty
    PERFORM F_SET_EXT_ENDDATE USING UT_ZSDSCMC002
                                    UT_ZSDSCMT006
                                    <F_ZSDSCMT003>
                           CHANGING LS_RESULT .

*    IF P_SPECI EQ ABAP_TRUE.
*      LS_RESULT-SP_OTHER_STD_END = LS_RESULT-STD_WRT_END.
*      LS_RESULT-SV_OTHER_STD_END = LS_RESULT-STD_WRT_END.
*    ENDIF.

    IF LS_RESULT-WRTLT_FLAG IS NOT INITIAL .
      LS_RESULT-WRTLT_FLAG = GC_TRUE .
    ENDIF.

    CLEAR LS_RESULT-GRDT .
    APPEND LS_RESULT TO CT_RESULT .
  ENDLOOP.

  IF CT_RESULT IS NOT INITIAL .
    SORT CT_RESULT BY EQUNR .
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CAL_WARRANTY_DATE
*&---------------------------------------------------------------------*
FORM F_CAL_WARRANTY_DATE  USING UF_DATE           TYPE ANY
                                UF_STD_WARRANTY   TYPE ZSDSDE_CM_STWRT
                                UF_SIGNUM         TYPE T5A4A-SPLIT
                                UF_DAY            TYPE T5A4A-DLYDY
                       CHANGING CF_DATE_WARRANTY  TYPE ANY .

  DATA: LF_YEAR TYPE T5A4A-DLYYR .

  LF_YEAR = UF_STD_WARRANTY .

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = UF_DATE
      DAYS      = 00
      MONTHS    = 00
*     SIGNUM    = '+'
      YEARS     = LF_YEAR
    IMPORTING
      CALC_DATE = CF_DATE_WARRANTY.
  IF SY-SUBRC = 0 AND CF_DATE_WARRANTY IS NOT INITIAL ##FM_SUBRC_OK.
    IF UF_SIGNUM = '+' .
      CF_DATE_WARRANTY = CF_DATE_WARRANTY + UF_DAY .
    ELSE.
      CF_DATE_WARRANTY = CF_DATE_WARRANTY - UF_DAY .
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_CONSTANTS
*&---------------------------------------------------------------------*
*&   Get GenC Constants
*&---------------------------------------------------------------------*
FORM F_GET_CONSTANTS .
  CONSTANTS:
    LC_MAPFIELD TYPE  ZSDSDE_PARAM_NAME  VALUE 'MAPFIELD_MAPGROUP',
    LC_MAPPROD  TYPE  ZSDSDE_PARAM_NAME  VALUE 'PROD_WRTTYP'.

  STATICS:
    LF_READ       TYPE  FLAG.

  DATA:
    LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA:
    LF_REPID  TYPE PROGRAMM.

* Check Already Read?
  IF LF_READ EQ GC_TRUE.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GT_MAPFIELD,
         GT_PROD .

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

      WHEN LC_MAPFIELD.

        APPEND VALUE #( FIELDNAME =  <L_GENC>-VALUE_LOW
                        MATKL     = <L_GENC>-VALUE_HIGH )

               TO GT_MAPFIELD.

      WHEN LC_MAPPROD.

        APPEND VALUE #( PRDHA      = <L_GENC>-PARAM_EXT
                        MATWLTYPE  = <L_GENC>-VALUE_LOW )
               TO GT_PROD .
    ENDCASE .
  ENDLOOP.

  SORT: GT_MAPFIELD BY FIELDNAME MATKL ,
        GT_PROD  BY PRDHA MATWLTYPE .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_std_enddate
*&---------------------------------------------------------------------*
*& Set Standard End Date by get Warranty Year from table ZSDSCMC002
*&---------------------------------------------------------------------*
FORM F_SET_STD_ENDDATE  USING US_ZSDSCMC002 TYPE TT_ZSDSCMC002
                              US_ZSDSCMT003 TYPE TS_ZSDSCMT003
                     CHANGING CS_RESULT     TYPE TS_RESULT ##NEEDED.

  FIELD-SYMBOLS:
    <LF_VALUE>         TYPE ANY,
    <LF_VALUE_STD_BEG> TYPE ANY.
  DATA: LF_STRING  TYPE STRING,
        LF_STD_BEG TYPE STRING.

  LOOP AT GT_MAPFIELD ASSIGNING FIELD-SYMBOL(<LF_MAPFIELD>) .

    READ TABLE US_ZSDSCMC002 ASSIGNING FIELD-SYMBOL(<LF_CMC002>)
      WITH KEY WRTPK = US_ZSDSCMT003-WRTPK
               MATKL = <LF_MAPFIELD>-MATKL
       BINARY SEARCH .

    IF <LF_CMC002> IS ASSIGNED AND <LF_CMC002>-STWRT > 0 .
      CONCATENATE 'CS_RESULT-' <LF_MAPFIELD>-FIELDNAME '_' 'STD_END' INTO LF_STRING.
      ASSIGN (LF_STRING) TO <LF_VALUE>.
      IF <LF_VALUE> IS ASSIGNED.

        CONCATENATE 'CS_RESULT-' <LF_MAPFIELD>-FIELDNAME '_' 'STD_BEG' INTO LF_STD_BEG.
        ASSIGN  (LF_STD_BEG) TO <LF_VALUE_STD_BEG> .

        IF <LF_VALUE_STD_BEG> IS ASSIGNED  .

          "Calculate Standard End Date
          PERFORM F_CAL_WARRANTY_DATE USING <LF_VALUE_STD_BEG>
                                            <LF_CMC002>-STWRT
                                            '-'
                                            '1'
                                   CHANGING  <LF_VALUE>.
          IF CS_RESULT-STD_WRT_END GT <LF_VALUE>.
            <LF_VALUE> = CS_RESULT-STD_WRT_END.
          ENDIF.

        ENDIF.
      ENDIF.
      UNASSIGN <LF_CMC002>.
    ELSE.
      CONCATENATE 'CS_RESULT-' <LF_MAPFIELD>-FIELDNAME '_' 'STD_BEG' INTO LF_STD_BEG.
      ASSIGN  (LF_STD_BEG) TO <LF_VALUE_STD_BEG> .

      IF <LF_VALUE_STD_BEG> IS ASSIGNED .
        CLEAR <LF_VALUE_STD_BEG> .
        UNASSIGN <LF_VALUE_STD_BEG> .
      ENDIF.
    ENDIF.
  ENDLOOP .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CREATE_INTF_FILE
*&---------------------------------------------------------------------*
*& Create Interface File
*&---------------------------------------------------------------------*
FORM F_CREATE_INTF_FILE  USING  UT_RESULT TYPE TT_RESULT
                                UT_ZSDSCMT003 TYPE TT_ZSDSCMT003 .

  DATA:
    LT_INTF_CSV TYPE  ZCL_SDSCA_FILE_INTERFACE=>TT_DATATXT.

  DATA:
    LS_INTF_TXT TYPE  TS_INTF_TXT,
    LS_RETURN   TYPE  BAPIRET2.

  DATA:
    LF_DATUM      TYPE  SY-DATUM,
    LF_UZEIT      TYPE  SY-UZEIT,
    LF_FILENAME_I TYPE  STRING,
    LF_FILEPATH_O TYPE  STRING   ##NEEDED,
    LF_FILENAME_O TYPE  STRING   ##NEEDED.

* Processing Log
  LF_DATUM = SY-DATUM.
  LF_UZEIT = SY-UZEIT.

  CLEAR: LT_INTF_CSV .

  LOOP AT UT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>) .
*   Interface File data
    CLEAR LS_INTF_TXT.

    LS_INTF_TXT-EQUNR         =  <L_RESULT>-EQUNR.
    LS_INTF_TXT-EQUID         =  <L_RESULT>-EQUID.
    LS_INTF_TXT-MATNR         =  <L_RESULT>-MATNR.
    LS_INTF_TXT-SERNR         =  <L_RESULT>-SERNR.
    LS_INTF_TXT-HERST         =  <L_RESULT>-HERST.
    LS_INTF_TXT-SERGE         =  <L_RESULT>-SERGE.
    LS_INTF_TXT-DEL_INDICATOR = <L_RESULT>-DEL_INDICATOR .
    LS_INTF_TXT-WRTLT         =  <L_RESULT>-WRTLT.
    LS_INTF_TXT-NAME          =  <L_RESULT>-NAME .
    LS_INTF_TXT-PHONE         =  <L_RESULT>-PHONE.
    LS_INTF_TXT-ADDRESS       = <L_RESULT>-ADDRESS.
    LS_INTF_TXT-DISTRICT      = <L_RESULT>-DISTRICT.
    LS_INTF_TXT-SUB_DISTRICT  = <L_RESULT>-SUB_DISTRICT.
    LS_INTF_TXT-PROVINCE      = <L_RESULT>-PROVINCE.
    LS_INTF_TXT-ZIPCODE       = <L_RESULT>-ZIPCODE.
    LS_INTF_TXT-STATUS        = <L_RESULT>-STATUS.
    LS_INTF_TXT-MAT_POSID     = <L_RESULT>-MAT_POSID.
    LS_INTF_TXT-STD_POSID     = <L_RESULT>-STD_POSID.
    LS_INTF_TXT-EXT_POSID     = <L_RESULT>-EXT_POSID.
    LS_INTF_TXT-WRTLT_FLAG    = <L_RESULT>-WRTLT_FLAG.
    LS_INTF_TXT-MIGFLG        = <L_RESULT>-MIGFLG.

    CONVERT_DATE_OUTPUT:
    <L_RESULT>-AULDT+0(4)    <L_RESULT>-AULDT+4(2)
    <L_RESULT>-AULDT+6(2)    LS_INTF_TXT-DELIVERY_DATE,

*    <L_RESULT>-GRDT+0(4)     <L_RESULT>-GRDT+4(2)
*    <L_RESULT>-GRDT+6(2)     LS_INTF_TXT-GRDT  ,

    <L_RESULT>-INSDT+0(4)    <L_RESULT>-INSDT+4(2)
    <L_RESULT>-INSDT+6(2)   LS_INTF_TXT-INSDT ,

    <L_RESULT>-COMDT+0(4)    <L_RESULT>-COMDT+4(2)
    <L_RESULT>-COMDT+6(2)   LS_INTF_TXT-COMDT ,

    <L_RESULT>-STD_WRT_BEG+0(4)    <L_RESULT>-STD_WRT_BEG+4(2)
    <L_RESULT>-STD_WRT_BEG+6(2)    LS_INTF_TXT-STD_WRT_BEG ,

    <L_RESULT>-STD_WRT_END+0(4)    <L_RESULT>-STD_WRT_END+4(2)
    <L_RESULT>-STD_WRT_END+6(2)    LS_INTF_TXT-STD_WRT_END,

    <L_RESULT>-EXT_WRT_BEG+0(4)    <L_RESULT>-EXT_WRT_BEG+4(2)
    <L_RESULT>-EXT_WRT_BEG+6(2)    LS_INTF_TXT-EXT_WRT_BEG ,

    <L_RESULT>-EXT_WRT_END+0(4)    <L_RESULT>-EXT_WRT_END+4(2)
    <L_RESULT>-EXT_WRT_END+6(2)    LS_INTF_TXT-EXT_WRT_END ,

    <L_RESULT>-CUS_REG_BEG+0(4)    <L_RESULT>-CUS_REG_BEG+4(2)
    <L_RESULT>-CUS_REG_BEG+6(2)    LS_INTF_TXT-CUS_REG_BEG,

    <L_RESULT>-CUS_REG_END+0(4)    <L_RESULT>-CUS_REG_END+4(2)
    <L_RESULT>-CUS_REG_END+6(2)    LS_INTF_TXT-CUS_REG_END,

    <L_RESULT>-WRT_ENDDAT+0(4)     <L_RESULT>-WRT_ENDDAT+4(2)
    <L_RESULT>-WRT_ENDDAT+6(2)     LS_INTF_TXT-WRT_ENDDAT,

    <L_RESULT>-GI_WRT_BEG+0(4)     <L_RESULT>-GI_WRT_BEG+4(2)
    <L_RESULT>-GI_WRT_BEG+6(2)     LS_INTF_TXT-GI_WRT_BEG,

    <L_RESULT>-GI_WRT_END+0(4)     <L_RESULT>-GI_WRT_END+4(2)
    <L_RESULT>-GI_WRT_END+6(2)     LS_INTF_TXT-GI_WRT_END,

    <L_RESULT>-SP_COMP_STD_BEG+0(4)    <L_RESULT>-SP_COMP_STD_BEG+4(2)
    <L_RESULT>-SP_COMP_STD_BEG+6(2)    LS_INTF_TXT-SP_COMP_STD_BEG,

    <L_RESULT>-SP_COMP_STD_END+0(4)    <L_RESULT>-SP_COMP_STD_END+4(2)
    <L_RESULT>-SP_COMP_STD_END+6(2)    LS_INTF_TXT-SP_COMP_STD_END,

    <L_RESULT>-SV_COMP_STD_BEG+0(4)    <L_RESULT>-SV_COMP_STD_BEG+4(2)
    <L_RESULT>-SV_COMP_STD_BEG+6(2)    LS_INTF_TXT-SV_COMP_STD_BEG,

    <L_RESULT>-SV_COMP_STD_END+0(4)    <L_RESULT>-SV_COMP_STD_END+4(2)
    <L_RESULT>-SV_COMP_STD_END+6(2)    LS_INTF_TXT-SV_COMP_STD_END,

    <L_RESULT>-SP_COMP_EXT_BEG+0(4)    <L_RESULT>-SP_COMP_EXT_BEG+4(2)
    <L_RESULT>-SP_COMP_EXT_BEG+6(2)    LS_INTF_TXT-SP_COMP_EXT_BEG,

    <L_RESULT>-SP_COMP_EXT_END+0(4)    <L_RESULT>-SP_COMP_EXT_END+4(2)
    <L_RESULT>-SP_COMP_EXT_END+6(2)    LS_INTF_TXT-SP_COMP_EXT_END,

    <L_RESULT>-SV_COMP_EXT_BEG+0(4)    <L_RESULT>-SV_COMP_EXT_BEG+4(2)
    <L_RESULT>-SV_COMP_EXT_BEG+6(2)    LS_INTF_TXT-SV_COMP_EXT_BEG,

    <L_RESULT>-SV_COMP_EXT_END+0(4)    <L_RESULT>-SV_COMP_EXT_END+4(2)
    <L_RESULT>-SV_COMP_EXT_END+6(2)    LS_INTF_TXT-SV_COMP_EXT_END,

    <L_RESULT>-SP_COND_STD_BEG+0(4)    <L_RESULT>-SP_COND_STD_BEG+4(2)
    <L_RESULT>-SP_COND_STD_BEG+6(2)    LS_INTF_TXT-SP_COND_STD_BEG,

    <L_RESULT>-SP_COND_STD_END+0(4)    <L_RESULT>-SP_COND_STD_END+4(2)
    <L_RESULT>-SP_COND_STD_END+6(2)    LS_INTF_TXT-SP_COND_STD_END,

    <L_RESULT>-SV_COND_STD_BEG+0(4)    <L_RESULT>-SV_COND_STD_BEG+4(2)
    <L_RESULT>-SV_COND_STD_BEG+6(2)    LS_INTF_TXT-SV_COND_STD_BEG,

    <L_RESULT>-SV_COND_STD_END+0(4)    <L_RESULT>-SV_COND_STD_END+4(2)
    <L_RESULT>-SV_COND_STD_END+6(2)    LS_INTF_TXT-SV_COND_STD_END,

    <L_RESULT>-SP_COND_EXT_BEG+0(4)    <L_RESULT>-SP_COND_EXT_BEG+4(2)
    <L_RESULT>-SP_COND_EXT_BEG+6(2)    LS_INTF_TXT-SP_COND_EXT_BEG,

    <L_RESULT>-SP_COND_EXT_END+0(4)    <L_RESULT>-SP_COND_EXT_END+4(2)
    <L_RESULT>-SP_COND_EXT_END+6(2)    LS_INTF_TXT-SP_COND_EXT_END,

    <L_RESULT>-SV_COND_EXT_BEG+0(4)    <L_RESULT>-SV_COND_EXT_BEG+4(2)
    <L_RESULT>-SV_COND_EXT_BEG+6(2)    LS_INTF_TXT-SV_COND_EXT_BEG,

    <L_RESULT>-SV_COND_EXT_END+0(4)    <L_RESULT>-SV_COND_EXT_END+4(2)
    <L_RESULT>-SV_COND_EXT_END+6(2)    LS_INTF_TXT-SV_COND_EXT_END,

    <L_RESULT>-SP_EVAP_STD_BEG+0(4)    <L_RESULT>-SP_EVAP_STD_BEG+4(2)
    <L_RESULT>-SP_EVAP_STD_BEG+6(2)    LS_INTF_TXT-SP_EVAP_STD_BEG,

    <L_RESULT>-SP_EVAP_STD_END+0(4)    <L_RESULT>-SP_EVAP_STD_END+4(2)
    <L_RESULT>-SP_EVAP_STD_END+6(2)    LS_INTF_TXT-SP_EVAP_STD_END,

    <L_RESULT>-SV_EVAP_STD_BEG+0(4)    <L_RESULT>-SV_EVAP_STD_BEG+4(2)
    <L_RESULT>-SV_EVAP_STD_BEG+6(2)    LS_INTF_TXT-SV_EVAP_STD_BEG,

    <L_RESULT>-SV_EVAP_STD_END+0(4)    <L_RESULT>-SV_EVAP_STD_END+4(2)
    <L_RESULT>-SV_EVAP_STD_END+6(2)    LS_INTF_TXT-SV_EVAP_STD_END,

    <L_RESULT>-SP_EVAP_EXT_BEG+0(4)    <L_RESULT>-SP_EVAP_EXT_BEG+4(2)
    <L_RESULT>-SP_EVAP_EXT_BEG+6(2)    LS_INTF_TXT-SP_EVAP_EXT_BEG,

    <L_RESULT>-SP_EVAP_EXT_END+0(4)    <L_RESULT>-SP_EVAP_EXT_END+4(2)
    <L_RESULT>-SP_EVAP_EXT_END+6(2)    LS_INTF_TXT-SP_EVAP_EXT_END,

    <L_RESULT>-SV_EVAP_EXT_BEG+0(4)    <L_RESULT>-SV_EVAP_EXT_BEG+4(2)
    <L_RESULT>-SV_EVAP_EXT_BEG+6(2)    LS_INTF_TXT-SV_EVAP_EXT_BEG,

    <L_RESULT>-SV_EVAP_EXT_END+0(4)    <L_RESULT>-SV_EVAP_EXT_END+4(2)
    <L_RESULT>-SV_EVAP_EXT_END+6(2)    LS_INTF_TXT-SV_EVAP_EXT_END,

    <L_RESULT>-SP_PCBIN_STD_BEG+0(4)   <L_RESULT>-SP_PCBIN_STD_BEG+4(2)
    <L_RESULT>-SP_PCBIN_STD_BEG+6(2)   LS_INTF_TXT-SP_PCBIN_STD_BEG,

    <L_RESULT>-SP_PCBIN_STD_END+0(4)   <L_RESULT>-SP_PCBIN_STD_END+4(2)
    <L_RESULT>-SP_PCBIN_STD_END+6(2)   LS_INTF_TXT-SP_PCBIN_STD_END,

    <L_RESULT>-SV_PCBIN_STD_BEG+0(4)   <L_RESULT>-SV_PCBIN_STD_BEG+4(2)
    <L_RESULT>-SV_PCBIN_STD_BEG+6(2)   LS_INTF_TXT-SV_PCBIN_STD_BEG,

    <L_RESULT>-SV_PCBIN_STD_END+0(4)   <L_RESULT>-SV_PCBIN_STD_END+4(2)
    <L_RESULT>-SV_PCBIN_STD_END+6(2)   LS_INTF_TXT-SV_PCBIN_STD_END,

    <L_RESULT>-SP_PCBIN_EXT_BEG+0(4)   <L_RESULT>-SP_PCBIN_EXT_BEG+4(2)
    <L_RESULT>-SP_PCBIN_EXT_BEG+6(2)   LS_INTF_TXT-SP_PCBIN_EXT_BEG,

    <L_RESULT>-SP_PCBIN_EXT_END+0(4)   <L_RESULT>-SP_PCBIN_EXT_END+4(2)
    <L_RESULT>-SP_PCBIN_EXT_END+6(2)   LS_INTF_TXT-SP_PCBIN_EXT_END,

    <L_RESULT>-SV_PCBIN_EXT_BEG+0(4)   <L_RESULT>-SV_PCBIN_EXT_BEG+4(2)
    <L_RESULT>-SV_PCBIN_EXT_BEG+6(2)   LS_INTF_TXT-SV_PCBIN_EXT_BEG,

    <L_RESULT>-SV_PCBIN_EXT_END+0(4)   <L_RESULT>-SV_PCBIN_EXT_END+4(2)
    <L_RESULT>-SV_PCBIN_EXT_END+6(2)   LS_INTF_TXT-SV_PCBIN_EXT_END,

    <L_RESULT>-SP_PCBOUT_STD_BEG+0(4)  <L_RESULT>-SP_PCBOUT_STD_BEG+4(2)
    <L_RESULT>-SP_PCBOUT_STD_BEG+6(2)  LS_INTF_TXT-SP_PCBOUT_STD_BEG,

    <L_RESULT>-SP_PCBOUT_STD_END+0(4)  <L_RESULT>-SP_PCBOUT_STD_END+4(2)
    <L_RESULT>-SP_PCBOUT_STD_END+6(2)  LS_INTF_TXT-SP_PCBOUT_STD_END,

    <L_RESULT>-SV_PCBOUT_STD_BEG+0(4)  <L_RESULT>-SV_PCBOUT_STD_BEG+4(2)
    <L_RESULT>-SV_PCBOUT_STD_BEG+6(2)  LS_INTF_TXT-SV_PCBOUT_STD_BEG,

    <L_RESULT>-SV_PCBOUT_STD_END+0(4)  <L_RESULT>-SV_PCBOUT_STD_END+4(2)
    <L_RESULT>-SV_PCBOUT_STD_END+6(2)  LS_INTF_TXT-SV_PCBOUT_STD_END,

    <L_RESULT>-SP_PCBOUT_EXT_BEG+0(4)  <L_RESULT>-SP_PCBOUT_EXT_BEG+4(2)
    <L_RESULT>-SP_PCBOUT_EXT_BEG+6(2)  LS_INTF_TXT-SP_PCBOUT_EXT_BEG,

    <L_RESULT>-SP_PCBOUT_EXT_END+0(4)  <L_RESULT>-SP_PCBOUT_EXT_END+4(2)
    <L_RESULT>-SP_PCBOUT_EXT_END+6(2)  LS_INTF_TXT-SP_PCBOUT_EXT_END,

    <L_RESULT>-SV_PCBOUT_EXT_BEG+0(4)  <L_RESULT>-SV_PCBOUT_EXT_BEG+4(2)
    <L_RESULT>-SV_PCBOUT_EXT_BEG+6(2)  LS_INTF_TXT-SV_PCBOUT_EXT_BEG,

    <L_RESULT>-SV_PCBOUT_EXT_END+0(4)  <L_RESULT>-SV_PCBOUT_EXT_END+4(2)
    <L_RESULT>-SV_PCBOUT_EXT_END+6(2)  LS_INTF_TXT-SV_PCBOUT_EXT_END,

    <L_RESULT>-SP_FAN_STD_BEG+0(4)     <L_RESULT>-SP_FAN_STD_BEG+4(2)
    <L_RESULT>-SP_FAN_STD_BEG+6(2)     LS_INTF_TXT-SP_FAN_STD_BEG,

    <L_RESULT>-SP_FAN_STD_END+0(4)     <L_RESULT>-SP_FAN_STD_END+4(2)
    <L_RESULT>-SP_FAN_STD_END+6(2)     LS_INTF_TXT-SP_FAN_STD_END,

    <L_RESULT>-SV_FAN_STD_BEG+0(4)     <L_RESULT>-SV_FAN_STD_BEG+4(2)
    <L_RESULT>-SV_FAN_STD_BEG+6(2)     LS_INTF_TXT-SV_FAN_STD_BEG,

    <L_RESULT>-SV_FAN_STD_END+0(4)     <L_RESULT>-SV_FAN_STD_END+4(2)
    <L_RESULT>-SV_FAN_STD_END+6(2)     LS_INTF_TXT-SV_FAN_STD_END,

    <L_RESULT>-SP_FAN_EXT_BEG+0(4)     <L_RESULT>-SP_FAN_EXT_BEG+4(2)
    <L_RESULT>-SP_FAN_EXT_BEG+6(2)     LS_INTF_TXT-SP_FAN_EXT_BEG,

    <L_RESULT>-SP_FAN_EXT_END+0(4)     <L_RESULT>-SP_FAN_EXT_END+4(2)
    <L_RESULT>-SP_FAN_EXT_END+6(2)     LS_INTF_TXT-SP_FAN_EXT_END,

    <L_RESULT>-SV_FAN_EXT_BEG+0(4)     <L_RESULT>-SV_FAN_EXT_BEG+4(2)
    <L_RESULT>-SV_FAN_EXT_BEG+6(2)     LS_INTF_TXT-SV_FAN_EXT_BEG,

    <L_RESULT>-SV_FAN_EXT_END+0(4)     <L_RESULT>-SV_FAN_EXT_END+4(2)
    <L_RESULT>-SV_FAN_EXT_END+6(2)     LS_INTF_TXT-SV_FAN_EXT_END,

    <L_RESULT>-SP_CASING_STD_BEG+0(4)  <L_RESULT>-SP_CASING_STD_BEG+4(2)
    <L_RESULT>-SP_CASING_STD_BEG+6(2)  LS_INTF_TXT-SP_CASING_STD_BEG,

    <L_RESULT>-SP_CASING_STD_END+0(4)  <L_RESULT>-SP_CASING_STD_END+4(2)
    <L_RESULT>-SP_CASING_STD_END+6(2)  LS_INTF_TXT-SP_CASING_STD_END,

    <L_RESULT>-SV_CASING_STD_BEG+0(4)  <L_RESULT>-SV_CASING_STD_BEG+4(2)
    <L_RESULT>-SV_CASING_STD_BEG+6(2)  LS_INTF_TXT-SV_CASING_STD_BEG,

    <L_RESULT>-SV_CASING_STD_END+0(4)  <L_RESULT>-SV_CASING_STD_END+4(2)
    <L_RESULT>-SV_CASING_STD_END+6(2)  LS_INTF_TXT-SV_CASING_STD_END,

    <L_RESULT>-SP_CASING_EXT_BEG+0(4)  <L_RESULT>-SP_CASING_EXT_BEG+4(2)
    <L_RESULT>-SP_CASING_EXT_BEG+6(2)  LS_INTF_TXT-SP_CASING_EXT_BEG,

    <L_RESULT>-SP_CASING_EXT_END+0(4)  <L_RESULT>-SP_CASING_EXT_END+4(2)
    <L_RESULT>-SP_CASING_EXT_END+6(2)  LS_INTF_TXT-SP_CASING_EXT_END,

    <L_RESULT>-SV_CASING_EXT_BEG+0(4)  <L_RESULT>-SV_CASING_EXT_BEG+4(2)
    <L_RESULT>-SV_CASING_EXT_BEG+6(2)  LS_INTF_TXT-SV_CASING_EXT_BEG,

    <L_RESULT>-SV_CASING_EXT_END+0(4)  <L_RESULT>-SV_CASING_EXT_END+4(2)
    <L_RESULT>-SV_CASING_EXT_END+6(2)  LS_INTF_TXT-SV_CASING_EXT_END,

    <L_RESULT>-SP_OTHER_STD_BEG+0(4)   <L_RESULT>-SP_OTHER_STD_BEG+4(2)
    <L_RESULT>-SP_OTHER_STD_BEG+6(2)   LS_INTF_TXT-SP_OTHER_STD_BEG,

    <L_RESULT>-SP_OTHER_STD_END+0(4)   <L_RESULT>-SP_OTHER_STD_END+4(2)
    <L_RESULT>-SP_OTHER_STD_END+6(2)   LS_INTF_TXT-SP_OTHER_STD_END,

    <L_RESULT>-SV_OTHER_STD_BEG+0(4)   <L_RESULT>-SV_OTHER_STD_BEG+4(2)
    <L_RESULT>-SV_OTHER_STD_BEG+6(2)   LS_INTF_TXT-SV_OTHER_STD_BEG,

    <L_RESULT>-SV_OTHER_STD_END+0(4)   <L_RESULT>-SV_OTHER_STD_END+4(2)
    <L_RESULT>-SV_OTHER_STD_END+6(2)   LS_INTF_TXT-SV_OTHER_STD_END,

    <L_RESULT>-SP_OTHER_EXT_BEG+0(4)   <L_RESULT>-SP_OTHER_EXT_BEG+4(2)
    <L_RESULT>-SP_OTHER_EXT_BEG+6(2)   LS_INTF_TXT-SP_OTHER_EXT_BEG,

    <L_RESULT>-SP_OTHER_EXT_END+0(4)   <L_RESULT>-SP_OTHER_EXT_END+4(2)
    <L_RESULT>-SP_OTHER_EXT_END+6(2)   LS_INTF_TXT-SP_OTHER_EXT_END,

    <L_RESULT>-SV_OTHER_EXT_BEG+0(4)   <L_RESULT>-SV_OTHER_EXT_BEG+4(2)
    <L_RESULT>-SV_OTHER_EXT_BEG+6(2)   LS_INTF_TXT-SV_OTHER_EXT_BEG,

    <L_RESULT>-SV_OTHER_EXT_END+0(4)   <L_RESULT>-SV_OTHER_EXT_END+4(2)
    <L_RESULT>-SV_OTHER_EXT_END+6(2)   LS_INTF_TXT-SV_OTHER_EXT_END.

    IF  LT_INTF_CSV IS INITIAL .
      "Add Header
      PERFORM F_ADD_HEADER_CSV CHANGING LT_INTF_CSV.
      "Add Item
      PERFORM F_ADD_ITEM_CSV USING LS_INTF_TXT
                          CHANGING LT_INTF_CSV.
    ELSE.
      "Add Item
      PERFORM F_ADD_ITEM_CSV USING LS_INTF_TXT
                          CHANGING LT_INTF_CSV.
    ENDIF.
  ENDLOOP.

*  Assign Filename
  "Text-F01 : Equipment
  LF_FILENAME_I = TEXT-F01 && '_'  &&
                  LF_DATUM  && LF_UZEIT && '.csv' ##NO_TEXT.

  CALL METHOD ZCL_SDSCA_FILE_INTERFACE=>CREATE_INTERFACE_FILE
    EXPORTING
      IF_INTFNO   = GC_INTFNO_SFDC
      IF_FILENAME = LF_FILENAME_I
      IT_DATATXT  = LT_INTF_CSV
    IMPORTING
      ES_RETURN   = LS_RETURN
      EF_FILEPATH = LF_FILEPATH_O
      EF_FILENAME = LF_FILENAME_O.
  IF LS_RETURN-TYPE NE 'S'.
*     Error
    MESSAGE ID LS_RETURN-ID TYPE 'I'
            NUMBER LS_RETURN-NUMBER
            DISPLAY LIKE LS_RETURN-TYPE
            WITH LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2
                 LS_RETURN-MESSAGE_V3 LS_RETURN-MESSAGE_V4.
    RETURN.
  ELSE.
    LOOP AT UT_ZSDSCMT003 ASSIGNING FIELD-SYMBOL(<L_ZSDSCMT003>) .
      "Update status
      UPDATE ZSDSCMT003 SET UPDFLG = ''
                            AENAM = SY-UNAME
                            AEDAT = LF_DATUM
                            AEZET = LF_UZEIT
                      WHERE EQUNR = <L_ZSDSCMT003>-EQUNR .
      IF SY-SUBRC NE 0 .
        RETURN.
      ENDIF.

    ENDLOOP.

    IF SY-SUBRC = 0 .
      COMMIT WORK.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_HEADER_CSV
*&---------------------------------------------------------------------*
*& Add Header text to CSV file
*&---------------------------------------------------------------------*
FORM F_ADD_HEADER_CSV
  CHANGING CT_INTF_TXT TYPE  ZCL_SDSCA_FILE_INTERFACE=>TT_DATATXT.

  DATA LS_INTF_TXT  TYPE  ZCL_SDSCA_FILE_INTERFACE=>TS_DATATXT.

  CONCATENATE:   TEXT-T01
                 TEXT-T02
                 TEXT-T03
                 TEXT-T04
                 TEXT-T05
                 TEXT-T06
                 TEXT-T07
                 TEXT-T08
                 TEXT-T09
                 TEXT-T10
                 TEXT-T11
                 TEXT-T12
                 TEXT-T13
                 TEXT-T14
                 TEXT-T15
                 TEXT-T16
                 TEXT-T17
                 TEXT-T18
                 TEXT-T19
                 TEXT-T20
                 TEXT-T21
                 TEXT-T22
                 TEXT-T23
                 TEXT-T24
                 TEXT-T25
                 TEXT-T26
                 TEXT-T27
                 TEXT-T28
                 TEXT-T29
                 TEXT-T30
                 TEXT-T31
                 TEXT-T32
                 TEXT-T33
                 TEXT-T34
                 TEXT-T35
                 TEXT-T36
                 TEXT-T37
                 TEXT-T38
                 TEXT-T39
                 TEXT-T40
                 TEXT-T41
                 TEXT-T42
                 TEXT-T43
                 TEXT-T44
                 TEXT-T45
                 TEXT-T46
                 TEXT-T47
                 TEXT-T48
                 TEXT-T49
                 TEXT-T50
                 TEXT-T51
                 TEXT-T52
                 TEXT-T53
                 TEXT-T54
                 TEXT-T55
                 TEXT-T56
                 TEXT-T57
                 TEXT-T58
                 TEXT-T59
                 TEXT-T60
                 TEXT-T61
                 TEXT-T62
                 TEXT-T63
                 TEXT-T64
                 TEXT-T65
                 TEXT-T66
                 TEXT-T67
                 TEXT-T68
                 TEXT-T69
                 TEXT-T70
                 TEXT-T71
                 TEXT-T72
                 TEXT-T73
                 TEXT-T74
                 TEXT-T75
                 TEXT-T76
                 TEXT-T77
                 TEXT-T78
                 TEXT-T79
                 TEXT-T80
                 TEXT-T81
                 TEXT-T82
                 TEXT-T83
                 TEXT-T84
                 TEXT-T85
                 TEXT-T86
                 TEXT-T87
                 TEXT-T88
                 TEXT-T89
                 TEXT-T90
                 TEXT-T91
                 TEXT-T92
                 TEXT-T93
                 TEXT-T94
                 TEXT-T95
                 TEXT-T96
                 TEXT-T97
                 TEXT-T98
       INTO LS_INTF_TXT
       SEPARATED BY ',' .

  APPEND LS_INTF_TXT TO CT_INTF_TXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_ITEM_CSV
*&---------------------------------------------------------------------*
*& Add Item text to CSV file
*&---------------------------------------------------------------------*
FORM F_ADD_ITEM_CSV
       USING US_INTF_TXT  TYPE  TS_INTF_TXT
    CHANGING CT_INTF_TXT  TYPE  ZCL_SDSCA_FILE_INTERFACE=>TT_DATATXT.

  DATA LS_INTF_TXT  TYPE  TS_INTF_TXT .

  CONCATENATE:
         US_INTF_TXT-EQUNR
         US_INTF_TXT-EQUID
         US_INTF_TXT-MATNR
         US_INTF_TXT-SERNR
         US_INTF_TXT-HERST
         US_INTF_TXT-SERGE
         US_INTF_TXT-DEL_INDICATOR
         US_INTF_TXT-DELIVERY_DATE
         US_INTF_TXT-NAME
         US_INTF_TXT-PHONE
         US_INTF_TXT-ADDRESS
         US_INTF_TXT-DISTRICT
         US_INTF_TXT-SUB_DISTRICT
         US_INTF_TXT-PROVINCE
         US_INTF_TXT-ZIPCODE
         US_INTF_TXT-STATUS
         US_INTF_TXT-GRDT
         US_INTF_TXT-INSDT
         US_INTF_TXT-COMDT
         US_INTF_TXT-MAT_POSID
         US_INTF_TXT-STD_POSID
         US_INTF_TXT-EXT_POSID
         US_INTF_TXT-STD_WRT_BEG
         US_INTF_TXT-STD_WRT_END
         US_INTF_TXT-EXT_WRT_BEG
         US_INTF_TXT-EXT_WRT_END
         US_INTF_TXT-WRTLT_FLAG
         US_INTF_TXT-WRTLT
         US_INTF_TXT-CUS_REG_BEG
         US_INTF_TXT-CUS_REG_END
         US_INTF_TXT-WRT_ENDDAT
         US_INTF_TXT-GI_WRT_BEG
         US_INTF_TXT-GI_WRT_END
         US_INTF_TXT-MIGFLG
         US_INTF_TXT-SP_COMP_STD_BEG
         US_INTF_TXT-SP_COMP_STD_END
         US_INTF_TXT-SV_COMP_STD_BEG
         US_INTF_TXT-SV_COMP_STD_END
         US_INTF_TXT-SP_COMP_EXT_BEG
         US_INTF_TXT-SP_COMP_EXT_END
         US_INTF_TXT-SV_COMP_EXT_BEG
         US_INTF_TXT-SV_COMP_EXT_END
         US_INTF_TXT-SP_COND_STD_BEG
         US_INTF_TXT-SP_COND_STD_END
         US_INTF_TXT-SV_COND_STD_BEG
         US_INTF_TXT-SV_COND_STD_END
         US_INTF_TXT-SP_COND_EXT_BEG
         US_INTF_TXT-SP_COND_EXT_END
         US_INTF_TXT-SV_COND_EXT_BEG
         US_INTF_TXT-SV_COND_EXT_END
         US_INTF_TXT-SP_EVAP_STD_BEG
         US_INTF_TXT-SP_EVAP_STD_END
         US_INTF_TXT-SV_EVAP_STD_BEG
         US_INTF_TXT-SV_EVAP_STD_END
         US_INTF_TXT-SP_EVAP_EXT_BEG
         US_INTF_TXT-SP_EVAP_EXT_END
         US_INTF_TXT-SV_EVAP_EXT_BEG
         US_INTF_TXT-SV_EVAP_EXT_END
         US_INTF_TXT-SP_PCBIN_STD_BEG
         US_INTF_TXT-SP_PCBIN_STD_END
         US_INTF_TXT-SV_PCBIN_STD_BEG
         US_INTF_TXT-SV_PCBIN_STD_END
         US_INTF_TXT-SP_PCBIN_EXT_BEG
         US_INTF_TXT-SP_PCBIN_EXT_END
         US_INTF_TXT-SV_PCBIN_EXT_BEG
         US_INTF_TXT-SV_PCBIN_EXT_END
         US_INTF_TXT-SP_PCBOUT_STD_BEG
         US_INTF_TXT-SP_PCBOUT_STD_END
         US_INTF_TXT-SV_PCBOUT_STD_BEG
         US_INTF_TXT-SV_PCBOUT_STD_END
         US_INTF_TXT-SP_PCBOUT_EXT_BEG
         US_INTF_TXT-SP_PCBOUT_EXT_END
         US_INTF_TXT-SV_PCBOUT_EXT_BEG
         US_INTF_TXT-SV_PCBOUT_EXT_END
         US_INTF_TXT-SP_FAN_STD_BEG
         US_INTF_TXT-SP_FAN_STD_END
         US_INTF_TXT-SV_FAN_STD_BEG
         US_INTF_TXT-SV_FAN_STD_END
         US_INTF_TXT-SP_FAN_EXT_BEG
         US_INTF_TXT-SP_FAN_EXT_END
         US_INTF_TXT-SV_FAN_EXT_BEG
         US_INTF_TXT-SV_FAN_EXT_END
         US_INTF_TXT-SP_CASING_STD_BEG
         US_INTF_TXT-SP_CASING_STD_END
         US_INTF_TXT-SV_CASING_STD_BEG
         US_INTF_TXT-SV_CASING_STD_END
         US_INTF_TXT-SP_CASING_EXT_BEG
         US_INTF_TXT-SP_CASING_EXT_END
         US_INTF_TXT-SV_CASING_EXT_BEG
         US_INTF_TXT-SV_CASING_EXT_END
         US_INTF_TXT-SP_OTHER_STD_BEG
         US_INTF_TXT-SP_OTHER_STD_END
         US_INTF_TXT-SV_OTHER_STD_BEG
         US_INTF_TXT-SV_OTHER_STD_END
         US_INTF_TXT-SP_OTHER_EXT_BEG
         US_INTF_TXT-SP_OTHER_EXT_END
         US_INTF_TXT-SV_OTHER_EXT_BEG
         US_INTF_TXT-SV_OTHER_EXT_END
    INTO LS_INTF_TXT
*    SEPARATED BY ',' .
       SEPARATED BY '","' .
  CONCATENATE '"' LS_INTF_TXT  '"' INTO LS_INTF_TXT .
  APPEND LS_INTF_TXT TO CT_INTF_TXT.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_EXT_ENDDATE
*&---------------------------------------------------------------------*
*& Set Extension End date
*&---------------------------------------------------------------------*
FORM F_SET_EXT_ENDDATE  USING   UT_ZSDSCMC002  TYPE TT_ZSDSCMC002
                                UT_ZSDSCMT006  TYPE TT_ZSDSCMT006
                                US_ZSDSCMT003  TYPE TS_ZSDSCMT003
                      CHANGING  CS_RESULT      TYPE TS_RESULT ##NEEDED.

  FIELD-SYMBOLS:
    <LF_VALUE_EXT_BEG> TYPE ANY,
    <LF_VALUE_EXT_END> TYPE ANY,
    <LF_VALUE_STD_BEG> TYPE ANY,
    <LF_VALUE_STD_END> TYPE ANY,
    <LF_FIELDS>        TYPE ANY.

  DATA: LF_EXT_BEG        TYPE STRING,
        LF_EXT_END        TYPE STRING,
        LF_STD_BEG        TYPE STRING,
        LF_STD_END        TYPE STRING,
        LF_FIELDS         TYPE STRING,

        LF_PRDHA_1        TYPE MARA-PRDHA,
        LF_STD_WRTTY_YEAR TYPE ZSDSCMC002-STWRT,
        LF_EXT_WRTTY_YEAR TYPE ZSDSCMC002-STWRT,
        LF_WRTTY_YEAR     TYPE ZSDSCMC002-STWRT,
        LF_MATWLTYPE      TYPE ZSDSCMT006-MATWLTYPE.

* Get Mat WarrantyType from Prod Hierarchy Level 1 of Equipment
  "Set Product Hierachy Level 1
  SPLIT US_ZSDSCMT003-PRDHA AT SPACE INTO: DATA(LF_STR1)
                                           DATA(LF_STR2) ##NEEDED
                                           DATA(LF_STR3) ##NEEDED.
  IF LF_STR1 IS NOT INITIAL .
    LF_PRDHA_1 = LF_STR1 .

    "Get Product Warranty type
    READ TABLE GT_PROD ASSIGNING FIELD-SYMBOL(<LF_PROD>)
      WITH KEY PRDHA = LF_PRDHA_1 .
    IF <LF_PROD> IS ASSIGNED .
      LF_MATWLTYPE = <LF_PROD>-MATWLTYPE .
      UNASSIGN <LF_PROD> .
    ENDIF.

  ENDIF.

  LOOP AT GT_MAPFIELD ASSIGNING FIELD-SYMBOL(<LF_MAPFIELD>) .
    CLEAR: LF_STD_WRTTY_YEAR, LF_EXT_WRTTY_YEAR ,
           LF_STD_BEG, LF_STD_END, LF_EXT_BEG, LF_EXT_END, LF_FIELDS.

    READ TABLE UT_ZSDSCMT006  ASSIGNING FIELD-SYMBOL(<LF_CMT006>)
      WITH KEY WL_ID     = US_ZSDSCMT003-WRTLT
               MATWLTYPE = LF_MATWLTYPE
       BINARY SEARCH .
    IF <LF_CMT006> IS ASSIGNED .
      "Set Value from ZSDSCMT006
      CONCATENATE 'CS_RESULT-' <LF_MAPFIELD>-FIELDNAME '_' 'EXT_END' INTO LF_EXT_END.
      ASSIGN (LF_EXT_END) TO <LF_VALUE_EXT_END>.
      IF <LF_VALUE_EXT_END> IS ASSIGNED.

        "Get Standard warrantly(YEARS) and Matgroup of EquipmentID FROM ZSDSCMC002
        READ TABLE UT_ZSDSCMC002 ASSIGNING FIELD-SYMBOL(<LF_CMC002>)
          WITH KEY WRTPK = US_ZSDSCMT003-WRTPK
                   MATKL = <LF_MAPFIELD>-MATKL
           BINARY SEARCH .
        IF <LF_CMC002> IS ASSIGNED .
          LF_STD_WRTTY_YEAR = <LF_CMC002>-STWRT .
        ENDIF.

        "Get Extended warrantly(YEARS) FROM ZSDSCMT006
        CONCATENATE '<LF_CMT006>-' <LF_MAPFIELD>-FIELDNAME INTO LF_FIELDS.
        ASSIGN  (LF_FIELDS) TO <LF_FIELDS> .
        IF <LF_FIELDS> IS ASSIGNED .
          LF_EXT_WRTTY_YEAR  =  <LF_FIELDS> .
          UNASSIGN  <LF_FIELDS> .
        ENDIF.

        LF_WRTTY_YEAR  = LF_EXT_WRTTY_YEAR - LF_STD_WRTTY_YEAR .

        CLEAR LF_EXT_BEG .

        CONCATENATE 'CS_RESULT-' <LF_MAPFIELD>-FIELDNAME '_' 'EXT_BEG' INTO LF_EXT_BEG.
        ASSIGN  (LF_EXT_BEG) TO <LF_VALUE_EXT_BEG> .

        IF <LF_VALUE_EXT_BEG> IS ASSIGNED .

          IF LF_WRTTY_YEAR <= 0 .
            "Set Ext. Start Date = Std. Start Date
            CONCATENATE 'CS_RESULT-' <LF_MAPFIELD>-FIELDNAME '_' 'STD_BEG' INTO LF_STD_BEG.
            ASSIGN  (LF_STD_BEG) TO <LF_VALUE_STD_BEG> .
            IF <LF_VALUE_STD_BEG> IS ASSIGNED .
              <LF_VALUE_EXT_BEG> = <LF_VALUE_STD_BEG> .
              UNASSIGN <LF_VALUE_STD_BEG> .
            ENDIF.

            "Set Ext. End date = Std. End date
            CONCATENATE 'CS_RESULT-' <LF_MAPFIELD>-FIELDNAME '_' 'STD_END' INTO LF_STD_END.
            ASSIGN  (LF_STD_END) TO <LF_VALUE_STD_END> .
            IF <LF_VALUE_STD_END> IS ASSIGNED .
              <LF_VALUE_EXT_END> = <LF_VALUE_STD_END> .
              UNASSIGN <LF_VALUE_STD_END> .
            ENDIF.

          ELSE.
            "Calculate Standard End Date
            PERFORM F_CAL_WARRANTY_DATE USING <LF_VALUE_EXT_BEG>
                                              LF_WRTTY_YEAR
                                              '-'
                                              '1'
                                     CHANGING  <LF_VALUE_EXT_END>.
          ENDIF.

          UNASSIGN <LF_VALUE_EXT_BEG> .
        ENDIF.

        UNASSIGN  <LF_VALUE_EXT_END> .
      ENDIF.
      UNASSIGN <LF_CMT006>.

    ELSE.
      CONCATENATE 'CS_RESULT-' <LF_MAPFIELD>-FIELDNAME '_' 'EXT_BEG' INTO LF_EXT_BEG.
      ASSIGN  (LF_EXT_BEG) TO <LF_VALUE_EXT_BEG> .

      IF <LF_VALUE_EXT_BEG> IS ASSIGNED .
        CLEAR  <LF_VALUE_EXT_BEG> .
        UNASSIGN <LF_VALUE_EXT_BEG> .
      ENDIF.
    ENDIF.
  ENDLOOP .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_AUTHORIZE_CHECK
*&---------------------------------------------------------------------*
*& Check Authorization on t-code
*&---------------------------------------------------------------------*
FORM F_AUTHORIZE_CHECK  USING UF_TCODE  TYPE  SY-TCODE.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
  ID 'TCD' FIELD UF_TCODE.

  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE S172(00) WITH UF_TCODE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.
