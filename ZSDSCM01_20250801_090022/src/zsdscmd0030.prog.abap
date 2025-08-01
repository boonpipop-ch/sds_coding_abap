*-----------------------------------------------------------------------
*  Program ID         : ZSDSCMD0030
*  Creation Date      : 18.03.2025
*  Author             : Boonpipop Ch.
*  Add-on ID          :
*  Description        : This is a program to create/change warranty letter
*  Purpose            :
*  Copied from        : ZSDSCMD0010
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSCMD0030.


*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
##NEEDED
TABLES:
  SSCRFIELDS,ZSDSCMT005.

*----------------------------------------------------------------------*
* Class
*----------------------------------------------------------------------*
*CLASS:
*  LCL_EVENT_RECEIVER DEFINITION DEFERRED.  "for event handling
*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF GTY_DF_WARR_COMP,
         SP_COMP1   TYPE ZSDSDE_WL_SP_COMP,
         SP_COND1   TYPE ZSDSDE_WL_SP_COND,
         SP_EVAP1   TYPE ZSDSDE_WL_SP_EVAP,
         SP_PCBIN1  TYPE ZSDSDE_WL_SP_PCBIN,
         SP_PCBOUT1 TYPE ZSDSDE_WL_SP_PCBOUT,
         SP_FAN1    TYPE ZSDSDE_WL_SP_FAN,
         SP_CASING1 TYPE ZSDSDE_WL_SP_CASING,
         SP_OTHER1  TYPE ZSDSDE_WL_SP_OTHER,
         SV_COMP1   TYPE ZSDSDE_WL_SV_COMP,
         SV_COND1   TYPE ZSDSDE_WL_SV_COND,
         SV_EVAP1   TYPE ZSDSDE_WL_SV_EVAP,
         SV_PCBIN1  TYPE ZSDSDE_WL_SV_PCBIN,
         SV_PCBOUT1 TYPE ZSDSDE_WL_SV_PCBOUT,
         SV_FAN1    TYPE ZSDSDE_WL_SV_FAN,
         SV_CASING1 TYPE ZSDSDE_WL_SV_CASING,
         SV_OTHER1  TYPE ZSDSDE_WL_SV_OTHER,
         SP_COMP2   TYPE ZSDSDE_WL_SP_COMP,
         SP_COND2   TYPE ZSDSDE_WL_SP_COND,
         SP_EVAP2   TYPE ZSDSDE_WL_SP_EVAP,
         SP_PCBIN2  TYPE ZSDSDE_WL_SP_PCBIN,
         SP_PCBOUT2 TYPE ZSDSDE_WL_SP_PCBOUT,
         SP_FAN2    TYPE ZSDSDE_WL_SP_FAN,
         SP_CASING2 TYPE ZSDSDE_WL_SP_CASING,
         SP_OTHER2  TYPE ZSDSDE_WL_SP_OTHER,
         SV_COMP2   TYPE ZSDSDE_WL_SV_COMP,
         SV_COND2   TYPE ZSDSDE_WL_SV_COND,
         SV_EVAP2   TYPE ZSDSDE_WL_SV_EVAP,
         SV_PCBIN2  TYPE ZSDSDE_WL_SV_PCBIN,
         SV_PCBOUT2 TYPE ZSDSDE_WL_SV_PCBOUT,
         SV_FAN2    TYPE ZSDSDE_WL_SV_FAN,
         SV_CASING2 TYPE ZSDSDE_WL_SV_CASING,
         SV_OTHER2  TYPE ZSDSDE_WL_SV_OTHER,
         SP_COMP3   TYPE ZSDSDE_WL_SP_COMP,
         SP_COND3   TYPE ZSDSDE_WL_SP_COND,
         SP_EVAP3   TYPE ZSDSDE_WL_SP_EVAP,
         SP_PCBIN3  TYPE ZSDSDE_WL_SP_PCBIN,
         SP_PCBOUT3 TYPE ZSDSDE_WL_SP_PCBOUT,
         SP_FAN3    TYPE ZSDSDE_WL_SP_FAN,
         SP_CASING3 TYPE ZSDSDE_WL_SP_CASING,
         SP_OTHER3  TYPE ZSDSDE_WL_SP_OTHER,
         SV_COMP3   TYPE ZSDSDE_WL_SV_COMP,
         SV_COND3   TYPE ZSDSDE_WL_SV_COND,
         SV_EVAP3   TYPE ZSDSDE_WL_SV_EVAP,
         SV_PCBIN3  TYPE ZSDSDE_WL_SV_PCBIN,
         SV_PCBOUT3 TYPE ZSDSDE_WL_SV_PCBOUT,
         SV_FAN3    TYPE ZSDSDE_WL_SV_FAN,
         SV_CASING3 TYPE ZSDSDE_WL_SV_CASING,
         SV_OTHER3  TYPE ZSDSDE_WL_SV_OTHER,
       END OF GTY_DF_WARR_COMP.

TYPES : BEGIN OF GTY_ITEMS,
          MATNR TYPE VBRP-MATNR,
          FKIMG TYPE CHAR19,
          SERNR TYPE STRING,
          VBELN TYPE VBRP-VBELN,
          INSTD TYPE DATS,
          WRTF1 TYPE DATS,
          WRTE1 TYPE DATS,
          WRTF2 TYPE DATS,
          WRTE2 TYPE DATS,
          BUKRS TYPE BUKRS,
        END OF GTY_ITEMS,
        GTTY_ITEMS TYPE STANDARD TABLE OF GTY_ITEMS.

TYPES : BEGIN OF GTY_HEADER,
          NUMBER     TYPE CHAR50,
          DOCDATE    TYPE CHAR50,
          ATTENTION  TYPE CHAR50,
          SUBJECT    TYPE CHAR255,
          PROJECT    TYPE CHAR255,
          ADDRESS    TYPE CHAR255,
          INSTD      TYPE DATS,
          APNAM      TYPE CHAR30,
          POS        TYPE CHAR50,
          PROJ       TYPE CHAR100,
          ACCORDING  TYPE CHAR1024,
          ACCORDING2 TYPE CHAR1024, "420000361++
          COMP_EN    TYPE TEXT255,
          ADDR1_EN   TYPE TEXT255,
          ADDR2_EN   TYPE TEXT255,
          COMP_TH    TYPE TEXT255,
          ADDR1_TH   TYPE TEXT255,
          ADDR2_TH   TYPE TEXT255,
          REMARK     TYPE TEXT255,
        END OF GTY_HEADER,
        GTTY_HEADER TYPE STANDARD TABLE OF GTY_HEADER.

TYPES : BEGIN OF GTY_COND,
          LINE(255) TYPE C,
        END OF GTY_COND,
        GTTY_COND TYPE STANDARD TABLE OF GTY_COND.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE  TYPE  CHAR1     VALUE 'X' ##NEEDED.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_INV_CRE             TYPE STANDARD TABLE OF ZSDSCMS015 INITIAL SIZE 0 ##NEEDED,
  GT_INV_CHGDIS          TYPE STANDARD TABLE OF ZSDSCMS015 INITIAL SIZE 0 ##NEEDED,
  GT_INV_CHGDIS_MASS_UPD TYPE STANDARD TABLE OF ZSDSCMS015 INITIAL SIZE 0 ##NEEDED,
  GT_INV_100             TYPE STANDARD TABLE OF ZSDSCMS015 INITIAL SIZE 0 ##NEEDED,
  GT_EXCLUDE             TYPE TABLE OF SYUCOMM ##NEEDED,
  GT_ZSDSCMC006          TYPE TABLE OF ZSDSCMC006 ##NEEDED,
  GT_ZSDSCMC007          TYPE TABLE OF ZSDSCMC007 ##NEEDED,
  GT_ZSDSCMT005          TYPE TABLE OF ZSDSCMT005 ##NEEDED,
  GT_ZSDSCMT006          TYPE TABLE OF ZSDSCMT006 ##NEEDED,
  GT_ZSDSCMT007          TYPE TABLE OF ZSDSCMT007 ##NEEDED,
  GT_ZSDSCMT008          TYPE TABLE OF ZSDSCMT008 ##NEEDED,
  GT_ZSDSCMC010          TYPE TABLE OF ZSDSCMC010 ##NEEDED.
*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA:
  GS_DF_WARR_COMP TYPE GTY_DF_WARR_COMP ##NEEDED,
  GS_INV_100      TYPE ZSDSCMS015 ##NEEDED,
  GS_ZSDSCMC007   TYPE ZSDSCMC007 ##NEEDED,
  GS_ZSDSCMT005   TYPE ZSDSCMT005 ##NEEDED,
  GS_ZSDSCMT006   TYPE ZSDSCMT006 ##NEEDED,
  GS_ZSDSCMT007   TYPE ZSDSCMT007 ##NEEDED,
  GS_ZSDSCMT008   TYPE ZSDSCMT008 ##NEEDED,
  GS_ZSDSCMC010   TYPE ZSDSCMC010 ##NEEDED.
*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
DATA:
  OK_CODE             TYPE SY-UCOMM,
  SAVE_OK             TYPE SY-UCOMM ##NEEDED,
  GV_PS_PSP_PNR       TYPE PS_POSID ##NEEDED,
  GV_PS_PSP_PNR_OLD   TYPE PS_POSID ##NEEDED,
  GV_LANG_TH_EN       TYPE ZSDSDE_LANG_TH_EN,
  GV_INV_PRINT        TYPE CHAR1,
  GV_LOGO             TYPE CHAR1 ##NEEDED,
  GV_WARRANTY_NO_TEXT TYPE ZSDSDE_WL_ID ##NEEDED,
  GT_GENC_PROD_WRTTYP TYPE STANDARD TABLE OF ZSDSCAC001 ##NEEDED,
  R_INV100            TYPE CHAR1,
  R_INV_EX100         TYPE CHAR1,
  R_MANUAL100         TYPE CHAR1,
  GV_DAYS100          TYPE NUMC4,
  GV_WLDATE100        TYPE DATUM,
  GV_WL_ID            TYPE ZSDSDE_WL_ID ##NEEDED,
  GV_SUBRC_ENQUEUE    TYPE SUBRC ##NEEDED,
  GV_STATUS_TXT       TYPE TEXT15 ##NEEDED,
  GRT_ALLOW_USR       TYPE RANGE OF SY-UNAME.

CONTROLS:  TC_REF TYPE TABLEVIEW USING SCREEN '0100'.

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_WARRANTY    TYPE ZSDSDE_PARAM_NAME VALUE 'WARRANTY',
  GC_ALLOW_USER  TYPE ZSDSDE_PARAM_NAME VALUE 'ALLOW_USER',
  GC_PROD_WRTTYP TYPE ZSDSDE_PARAM_NAME VALUE 'PROD_WRTTYP',
  GC_ZSDSCMR0020 TYPE PROGRAMM VALUE 'ZSDSCMR0020',
  GC_FORM_EN     TYPE TDSFNAME VALUE 'ZSDSCM002_EN',
  GC_FORM_TH     TYPE TDSFNAME VALUE 'ZSDSCM002_TH' ##NEEDED,
  GC_INTFNO      TYPE ZSDSDE_INTFNO VALUE 'SDE031_1'.

CONSTANTS:
  BEGIN OF GC_NUMBER_RANGE,
    NR  TYPE INRI-NRRANGENR VALUE '1',
    OBJ TYPE INRI-OBJECT    VALUE 'ZCM001',
  END OF GC_NUMBER_RANGE.
*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*

CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSCMS015' ##NEEDED.

CONSTANTS:
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 65 ##NEEDED.

DATA:
  OREF_DOCK TYPE REF TO CL_GUI_DOCKING_CONTAINER ##NEEDED,
  OREF_ALV  TYPE REF TO CL_GUI_ALV_GRID ##NEEDED,
  GT_FCAT   TYPE LVC_T_FCAT ##NEEDED,
  GS_FCAT   TYPE LVC_S_FCAT ##NEEDED.

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*
DEFINE MC_SHOW_PROGRESS ##NEEDED.
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

DEFINE CONVERT_ALPHA_INPUT ##NEEDED.

  if &1 is not initial.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = &1
      importing
        output = &2.
  endif.

END-OF-DEFINITION.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN:
FUNCTION KEY 1,
FUNCTION KEY 2,
FUNCTION KEY 3.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-B01.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS : R_CRE RADIOBUTTON GROUP RDO1 MODIF ID RDO USER-COMMAND UCOMM DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 3(23) TEXT-M01 MODIF ID RDO.

    PARAMETERS : R_CHGDIS RADIOBUTTON GROUP RDO1 MODIF ID RDO.
    SELECTION-SCREEN COMMENT 31(23) TEXT-M02 MODIF ID RDO.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-B02.
*  PARAMETERS: P_POSID TYPE PRPS-POSID.
  PARAMETERS: P_POSID TYPE ZSDSCMT005-PROJWBS NO-DISPLAY.
  SELECT-OPTIONS: S_POSID FOR ZSDSCMT005-PROJWBS NO-EXTENSION NO INTERVALS.

  PARAMETERS: P_WL_ID TYPE ZSDSCMT005-WL_ID MODIF ID CHG MATCHCODE OBJECT ZSDSH_WARRANTY_INV.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(23) TEXT-M03 MODIF ID CRE.
    PARAMETERS: P_SPLIT TYPE C AS CHECKBOX MODIF ID CRE.
    SELECTION-SCREEN COMMENT 27(45) TEXT-M04 MODIF ID CRE.
  SELECTION-SCREEN END OF LINE.


  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS : R_INV RADIOBUTTON GROUP RDO2 DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 3(26) TEXT-M05 MODIF ID RDO.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS : R_INV_EX RADIOBUTTON GROUP RDO2.
    SELECTION-SCREEN COMMENT 3(25) TEXT-M06.
    PARAMETERS : P_DAYS TYPE NUMC4.
    SELECTION-SCREEN COMMENT 34(5) TEXT-M07.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS : R_MANUAL RADIOBUTTON GROUP RDO2 .
    SELECTION-SCREEN COMMENT 3(25) TEXT-M08 MODIF ID RDO.
    PARAMETERS : P_START TYPE DATUM.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN:PUSHBUTTON 3(11) PB1 USER-COMMAND MASS MODIF ID CHG.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PB1 = TEXT-T01."Mass Update.

  CONCATENATE '@15@' TEXT-T05 INTO SSCRFIELDS-FUNCTXT_01.
  SSCRFIELDS-FUNCTXT_02 = TEXT-T02. "Warranty Condition.
  SSCRFIELDS-FUNCTXT_03 = TEXT-T03. "Set default component Yrs.


  IF GV_WARRANTY_NO_TEXT IS INITIAL.
    PERFORM VALIDATE_GENC.
  ENDIF.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*

AT SELECTION-SCREEN OUTPUT.

  CLEAR: GT_EXCLUDE.
  APPEND 'SJOB' TO GT_EXCLUDE.
  APPEND 'PRIN' TO GT_EXCLUDE.
  APPEND 'ONLI' TO GT_EXCLUDE.
  IF SY-UNAME NOT IN GRT_ALLOW_USR.
    APPEND 'FC02' TO GT_EXCLUDE.
  ENDIF.


  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      P_STATUS  = SY-PFKEY
      P_PROGRAM = SY-REPID
    TABLES
      P_EXCLUDE = GT_EXCLUDE.


  CASE 'X'.
    WHEN R_CRE.
      LOOP AT SCREEN.
        CASE SCREEN-GROUP1.
          WHEN 'CRE'.
            SCREEN-ACTIVE = 1.
          WHEN 'CHG'.
            SCREEN-ACTIVE = 0.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.

    WHEN R_CHGDIS.
      LOOP AT SCREEN.
        CASE SCREEN-GROUP1.
          WHEN 'CRE'.
            SCREEN-ACTIVE = 0.
          WHEN 'CHG'.
            SCREEN-ACTIVE = 1.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
  ENDCASE.

AT SELECTION-SCREEN.

*Set Warranty Component Default and Warranty Condition Text
  CASE SSCRFIELDS-UCOMM.
    WHEN 'FC02'.
      PERFORM GET_WARR_CONDTEXT_DF_DISPLAY.
      CALL SCREEN 9100.
    WHEN 'FC03'.
      PERFORM GET_WARR_COMP_DF.
      PERFORM DISPLAY_WARR_COMP_DF.
      CALL SCREEN 9200.
  ENDCASE.

*Select and display invoice
  IF SY-UCOMM = 'MASS' OR SY-UCOMM = SPACE.
    CASE 'X'.
      WHEN R_CRE.
        IF OREF_ALV IS BOUND.
          CALL METHOD OREF_DOCK->FREE.
          CLEAR: OREF_DOCK,OREF_ALV.

        ENDIF.
        IF SY-UCOMM = SPACE AND S_POSID[] IS NOT INITIAL.
          "Display screen
          PERFORM F_DISPLAY_INVOICE_CREATE.
        ENDIF.
      WHEN R_CHGDIS.
        IF P_SPLIT = SPACE.
          IF OREF_ALV IS BOUND.
            CALL METHOD OREF_DOCK->FREE.
            CLEAR: OREF_DOCK,OREF_ALV.
          ENDIF.
*          IF ( SY-UCOMM = SPACE ) AND ( P_POSID IS NOT INITIAL OR P_WL_ID IS NOT INITIAL ).
          IF ( SY-UCOMM = SPACE ) AND ( S_POSID[] IS NOT INITIAL OR P_WL_ID IS NOT INITIAL ).
            "Display screen
            PERFORM F_DISPLAY_INVOICE_CHANGE.
          ENDIF.
*Save Mass start warranty date
          IF SY-UCOMM = 'MASS' AND ( ( R_INV = 'X' ) OR
                                     ( R_INV_EX = 'X' AND P_DAYS <> 0 ) OR
                                     ( R_MANUAL = 'X' AND P_START IS NOT INITIAL ) ).
            CLEAR: SY-UCOMM.
            GT_INV_CHGDIS_MASS_UPD  = GT_INV_CHGDIS.
            DELETE GT_INV_CHGDIS_MASS_UPD WHERE CHK = SPACE.
            IF GT_INV_CHGDIS_MASS_UPD IS NOT INITIAL.
              PERFORM MASS_CHANGE_WARRANTY_DATE.
              PERFORM F_DISPLAY_INVOICE_CHANGE.
            ENDIF.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDIF.


*Process creating and changing warranty lettter
  IF SY-UCOMM  = 'FC01'.

    CLEAR:  SY-UCOMM,GS_DF_WARR_COMP,
            GT_ZSDSCMC006, GT_ZSDSCMC007, GT_ZSDSCMT005, GT_ZSDSCMT006, GT_ZSDSCMT007, GT_ZSDSCMT008,
            GS_INV_100,
            GS_ZSDSCMC007, GS_ZSDSCMT005, GS_ZSDSCMT006, GS_ZSDSCMT007, GS_ZSDSCMT008,
            GV_DAYS100, GV_WLDATE100, GV_WL_ID.

    PERFORM VALIDATE_GENC.

    CASE 'X'.
      WHEN R_CRE.
        DELETE GT_INV_CRE WHERE CHK = SPACE.
        IF GT_INV_CRE IS NOT INITIAL.
          DATA(LT_INV_CRE) = GT_INV_CRE.
          SORT LT_INV_CRE BY PROJWBS.
          DELETE ADJACENT DUPLICATES FROM LT_INV_CRE COMPARING PROJWBS.
          IF LINES( LT_INV_CRE ) > 1.
            "Please select the same Project WBS
            MESSAGE TEXT-E09 TYPE 'E'.
          ENDIF.

          IF P_SPLIT = SPACE.
            PERFORM DEFAULT_CREATE_100.

            MOVE-CORRESPONDING GT_INV_CRE TO GT_INV_100.

            CALL SCREEN 100.
          ELSE.
            CLEAR: SY-UCOMM,OK_CODE.
            R_CRE = SPACE.
            R_CHGDIS = 'X'.
            "Split 1 Warranty Letter for 1 Inv no.
            PERFORM CREATE_WARRANTY_LETTER_SPLIT.
            IF OREF_ALV IS BOUND.
              CALL METHOD OREF_DOCK->FREE.
              CLEAR: OREF_DOCK,OREF_ALV.
            ENDIF.
            PERFORM F_DISPLAY_INVOICE_SPLIT.
          ENDIF.
        ENDIF.

      WHEN R_CHGDIS.
        DATA(LT_INV_CHGDIS) = GT_INV_CHGDIS.
        DELETE LT_INV_CHGDIS WHERE CHK = SPACE.

        DATA(LV_LINES) = LINES( LT_INV_CHGDIS ).
        IF LV_LINES > 1.
          MESSAGE E755(06). "Choose one line only
        ELSE.
          IF LT_INV_CHGDIS IS NOT INITIAL.
            PERFORM GET_DATA_CHGDIS_100.
            CALL SCREEN 100.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*

INCLUDE ZSDSCMD0030_O01.
*  INCLUDE ZSDSCMD0010_O01.

INCLUDE ZSDSCMD0030_I01.
*  INCLUDE ZSDSCMD0010_I01.

INCLUDE ZSDSCMD0030_F01.
*  INCLUDE ZSDSCMD0010_F01.
