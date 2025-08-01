*-----------------------------------------------------------------------
*  Program ID         : ZSDSCOR0050
*  Creation Date      : 18.11.2024
*  Author             : Atitep B.
*  Add-on ID          : ZCOPAR001
*  Description        : Management Profit & Loss Report
*  Purpose            :
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSCOR0050.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: ACDOCA.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: BEGIN OF GS_OUTPUT ##NEEDED,
        NO            TYPE CHAR10,
        PL            TYPE SETHEADERT-DESCRIPT,
        PRCTR         TYPE ACDOCA-PRCTR,
        VKBUR         TYPE ACDOCA-VKBUR_PA,
        VKGRP         TYPE ACDOCA-VKGRP_PA,
        WWLO          TYPE ACDOCA-WWLO_PA,
        PRODH         TYPE ACDOCA-PRODH_PA,
        MSE           TYPE ACDOCA-ZZ1_SDSDIST_MSE,
        KUNNR         TYPE ACDOCA-KUNNR,
        KUKLA         TYPE ACDOCA-ZZ1_SHOPTYPE,
        RS_LY         TYPE ACDOCA-HSL,
        RS_LY_PER     TYPE P DECIMALS 2,
        RB_YY         TYPE ACDOCA-HSL,
        RB_YY_PER     TYPE P DECIMALS 2,
        RS_YY         TYPE ACDOCA-HSL,
        RS_YY_PER     TYPE P DECIMALS 2,
        RB_YY_VAR     TYPE ACDOCA-HSL,
        RB_YY_VAR_PER TYPE P DECIMALS 2,
        FY_LY_VAR     TYPE ACDOCA-HSL,
        FY_LY_VAR_PER TYPE P DECIMALS 2,
        STYLE         TYPE  LVC_T_STYL,
        COLOR         TYPE  LVC_T_SCOL,
      END OF GS_OUTPUT,
      GT_OUTPUT LIKE TABLE OF GS_OUTPUT ##NEEDED,

      BEGIN OF GS_DATA ##NEEDED,
        NO            TYPE CHAR10,
        PL            TYPE SETHEADERT-DESCRIPT,
        PRCTR         TYPE ACDOCA-PRCTR,
        VKBUR         TYPE ACDOCA-VKBUR_PA,
        VKGRP         TYPE ACDOCA-VKGRP_PA,
        WWLO          TYPE ACDOCA-WWLO_PA,
        PRODH         TYPE ACDOCA-PRODH_PA,
        MSE           TYPE ACDOCA-ZZ1_SDSDIST_MSE,
        KUNNR         TYPE ACDOCA-KUNNR,
        KUKLA         TYPE ACDOCA-ZZ1_SHOPTYPE,
        RS_LY         TYPE ACDOCA-HSL,
        RS_LY_PER     TYPE P DECIMALS 2,
        RB_YY         TYPE ACDOCA-HSL,
        RB_YY_PER     TYPE P DECIMALS 2,
        RS_YY         TYPE ACDOCA-HSL,
        RS_YY_PER     TYPE P DECIMALS 2,
        RB_YY_VAR     TYPE ACDOCA-HSL,
        RB_YY_VAR_PER TYPE P DECIMALS 2,
        FY_LY_VAR     TYPE ACDOCA-HSL,
        FY_LY_VAR_PER TYPE P DECIMALS 2,
      END OF GS_DATA,
      GT_DATA  LIKE TABLE OF GS_DATA ##NEEDED,
      GT_TOTAL LIKE TABLE OF GS_DATA ##NEEDED,

      BEGIN OF GS_GL_GROUP ##NEEDED,
        SETCLASS  TYPE SETLEAF-SETCLASS,
        SUBCLASS  TYPE SETLEAF-SUBCLASS,
        SETNAME   TYPE SETLEAF-SETNAME,
        LINEID    TYPE SETLEAF-LINEID,
        VALSIGN   TYPE SETLEAF-VALSIGN,
        VALOPTION TYPE SETLEAF-VALOPTION,
        VALFROM   TYPE SETLEAF-VALFROM,
        VALTO     TYPE SETLEAF-VALTO,
        SEQNR     TYPE SETLEAF-SEQNR,
        DESCRIPT  TYPE SETHEADERT-DESCRIPT,
      END OF GS_GL_GROUP,
      GT_GL_GROUP LIKE TABLE OF GS_GL_GROUP ##NEEDED,

      BEGIN OF GS_ACDOCA ##NEEDED,
        RLDNR  TYPE ACDOCA-RLDNR,
        RBUKRS TYPE ACDOCA-RBUKRS,
        GJAHR  TYPE ACDOCA-GJAHR,
        BELNR  TYPE ACDOCA-BELNR,
        DOCLN  TYPE ACDOCA-DOCLN,
        RYEAR  TYPE ACDOCA-RYEAR,
        POPER  TYPE  ACDOCA-POPER,
        RACCT  TYPE ACDOCA-RACCT,
        PRCTR  TYPE ACDOCA-PRCTR,
        VKBUR  TYPE ACDOCA-VKBUR_PA,
        VKGRP  TYPE ACDOCA-VKGRP_PA,
        WWLO   TYPE ACDOCA-WWLO_PA,
        PRODH  TYPE ACDOCA-PRODH_PA,
        MSE    TYPE ACDOCA-ZZ1_SDSDIST_MSE,
        KUNNR  TYPE ACDOCA-KUNNR,
        KUKLA  TYPE ACDOCA-ZZ1_SHOPTYPE,
        HSL    TYPE ACDOCA-HSL,
        XPAOBJNR_CO_REL TYPE ACDOCA-XPAOBJNR_CO_REL,
      END OF GS_ACDOCA,
      GT_ACDOCA    LIKE TABLE OF GS_ACDOCA ##NEEDED,
      GT_ACDOCA_RS LIKE TABLE OF GS_ACDOCA ##NEEDED,

      BEGIN OF GS_ACDOCP ##NEEDED,
        REQTSN    TYPE ACDOCP-REQTSN,
        DATAPAKID TYPE ACDOCP-DATAPAKID,
        RECORD    TYPE ACDOCP-RECORD,
        RYEAR     TYPE ACDOCP-RYEAR,
        POPER     TYPE  ACDOCP-POPER,
        RACCT     TYPE ACDOCP-RACCT,
        PRCTR     TYPE ACDOCP-PRCTR,
        VKBUR     TYPE ACDOCP-VKBUR_PA,
        VKGRP     TYPE ACDOCP-VKGRP_PA,
        WWLO      TYPE ACDOCP-WWLO_PA,
        PRODH     TYPE ACDOCP-PRODH_PA,
        MSE       TYPE ACDOCP-ZZ1_SDSDIST_MSE,
        KUNNR     TYPE ACDOCP-KUNNR,
        KUKLA     TYPE ACDOCP-ZZ1_SHOPTYPE,
        HSL       TYPE ACDOCP-HSL,
      END OF GS_ACDOCP,
      GT_ACDOCP LIKE TABLE OF GS_ACDOCP ##NEEDED.

*      BEGIN OF GS_CSKB ##NEEDED,
*        KOKRS TYPE CSKB-KOKRS,
*        KSTAR TYPE CSKB-KSTAR,
*        DATBI TYPE CSKB-DATBI,
*        DATAB TYPE CSKB-DATAB,
*        KATYP TYPE CSKB-KATYP,
*      END OF GS_CSKB,
*      GT_CSKB LIKE TABLE OF GS_CSKB ##NEEDED.

DATA: GT_FIELDCAT TYPE LVC_T_FCAT ##NEEDED.
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

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*
*DEFINE MC_SHOW_PROGRESS.
*  IF SY-BATCH IS INITIAL.
**   Show progress online
*    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*      EXPORTING
*        PERCENTAGE = &1 ##NUMBER_OK
*        TEXT       = &2.
*  ELSE.
**   Show message step in background
*    MESSAGE I000(38) WITH &2 SPACE SPACE SPACE.
*  ENDIF.
*END-OF-DEFINITION.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-B01.
  PARAMETERS: P_BUKRS TYPE ACDOCA-RBUKRS OBLIGATORY DEFAULT '1000'.
  SELECT-OPTIONS: S_POPER FOR ACDOCA-POPER OBLIGATORY NO-EXTENSION .
  PARAMETERS: P_RYEAR TYPE ACDOCA-RYEAR OBLIGATORY.

  SELECT-OPTIONS: S_PRCTR FOR ACDOCA-PRCTR OBLIGATORY,
                  S_VKBUR FOR ACDOCA-VKBUR_PA,
                  S_VKGRP FOR ACDOCA-VKGRP_PA,
                  S_WWLO  FOR ACDOCA-WWLO_PA,
                  S_PRODH FOR ACDOCA-PRODH_PA,
                  S_MSE   FOR ACDOCA-ZZ1_SDSDIST_MSE,
                  S_KUNNR FOR ACDOCA-KUNNR,
                  S_KUKLA FOR ACDOCA-ZZ1_SHOPTYPE.

  PARAMETERS: CB_TOT AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INIT.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM GET_DATA.
  PERFORM PREPARE_DATA.
*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF GT_OUTPUT[] IS NOT INITIAL.
    PERFORM DISPLAY_ALV.
  ELSE.
    MESSAGE 'No data' ##NO_TEXT
         TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
*  INCLUDE ZSDSCAI9990 ##INCL_OK.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
FORM GET_DATA .
  DATA: LF_RYEAR TYPE ACDOCA-RYEAR.

  DATA: LRT_RACCT    TYPE RANGE OF ACDOCA-RACCT,
        LRT_POPER    TYPE RANGE OF ACDOCA-POPER,
        LRT_POPER_RS TYPE RANGE OF ACDOCA-POPER,
        LRT_XPAOBJNR TYPE RANGE OF ACDOCA-XPAOBJNR_CO_REL.

  SELECT A~SETCLASS,
         A~SUBCLASS,
         A~SETNAME,
         A~LINEID,
         A~VALSIGN,
         A~VALOPTION,
         A~VALFROM,
         A~VALTO,
         A~SEQNR,
         B~DESCRIPT
    FROM SETLEAF AS A
    LEFT OUTER JOIN SETHEADERT AS B                    "#EC CI_BUFFJOIN
    ON  A~SETCLASS = B~SETCLASS
    AND A~SUBCLASS = B~SUBCLASS
    AND A~SETNAME  = B~SETNAME
    WHERE A~SETCLASS = '0109'
      AND A~SUBCLASS = 'RCOA'
    INTO TABLE @GT_GL_GROUP.

  IF GT_GL_GROUP[] IS NOT INITIAL.
    LRT_RACCT = VALUE #( FOR RECORD IN GT_GL_GROUP
                        ( SIGN = RECORD-VALSIGN
                          OPTION = RECORD-VALOPTION
                          LOW = RECORD-VALFROM
                          HIGH = RECORD-VALTO )
                      ).
    IF LRT_RACCT[] IS NOT INITIAL.
      LRT_POPER[] = S_POPER[].
      IF S_POPER-LOW = '012' OR S_POPER-HIGH = '012'.
        APPEND VALUE #( SIGN = 'I'
                        OPTION = 'EQ'
                        LOW = '013' )
                  TO LRT_POPER.
      ENDIF.

      IF S_VKBUR[] IS NOT INITIAL OR S_VKGRP[] IS NOT INITIAL
        OR S_WWLO[] IS NOT INITIAL OR S_PRODH[] IS NOT INITIAL
        OR S_MSE[] IS NOT INITIAL OR S_KUNNR[] IS NOT INITIAL
        OR S_KUKLA[] IS NOT INITIAL.
        APPEND VALUE #( SIGN = 'I'
                        OPTION = 'EQ'
                        LOW = 'X' )
                  TO LRT_XPAOBJNR.
      ENDIF.

      SELECT RLDNR
             RBUKRS
             GJAHR
             BELNR
             DOCLN
             RYEAR
             POPER
             RACCT
             PRCTR
             VKBUR_PA
             VKGRP_PA
             WWLO_PA
             PRODH_PA
             ZZ1_SDSDIST_MSE
             KUNNR
             ZZ1_SHOPTYPE
             HSL
             XPAOBJNR_CO_REL
        FROM ACDOCA
        INTO TABLE GT_ACDOCA
        WHERE RLDNR = '0L'
         AND RBUKRS = P_BUKRS
         AND RYEAR  = P_RYEAR
         AND POPER  IN LRT_POPER
         AND RACCT  IN LRT_RACCT
         AND PRCTR  IN S_PRCTR
         AND VKBUR_PA  IN S_VKBUR
         AND VKGRP_PA  IN S_VKGRP
         AND WWLO_PA   IN S_WWLO
         AND PRODH_PA  IN S_PRODH
         AND ZZ1_SDSDIST_MSE    IN S_MSE
         AND KUNNR  IN S_KUNNR
         AND ZZ1_SHOPTYPE  IN S_KUKLA
         AND XPAOBJNR_CO_REL IN LRT_XPAOBJNR.
      SORT GT_ACDOCA BY PRCTR VKBUR VKGRP WWLO PRODH MSE KUNNR KUKLA.



      LF_RYEAR = P_RYEAR - 1.
      LRT_POPER_RS = VALUE #( ( SIGN = 'I'
                                OPTION = 'EQ'
                                LOW = '012' )
                              ( SIGN = 'I'
                                OPTION = 'EQ'
                                LOW = '013' ) ).
      SELECT RLDNR
             RBUKRS
             GJAHR
             BELNR
             DOCLN
             RYEAR
             POPER
             RACCT
             PRCTR
             VKBUR_PA
             VKGRP_PA
             WWLO_PA
             PRODH_PA
             ZZ1_SDSDIST_MSE
             KUNNR
             ZZ1_SHOPTYPE
             HSL
             XPAOBJNR_CO_REL
        FROM ACDOCA
        INTO TABLE GT_ACDOCA_RS
        WHERE RLDNR = '0L'
         AND RBUKRS = P_BUKRS
         AND RYEAR  = LF_RYEAR
         AND POPER  IN LRT_POPER_RS
         AND RACCT  IN LRT_RACCT
         AND PRCTR  IN S_PRCTR
         AND VKBUR_PA  IN S_VKBUR
         AND VKGRP_PA  IN S_VKGRP
         AND WWLO_PA   IN S_WWLO
         AND PRODH_PA  IN S_PRODH
         AND ZZ1_SDSDIST_MSE    IN S_MSE
         AND KUNNR  IN S_KUNNR
         AND ZZ1_SHOPTYPE  IN S_KUKLA
        AND XPAOBJNR_CO_REL IN LRT_XPAOBJNR.

      SORT GT_ACDOCA_RS BY PRCTR VKBUR VKGRP WWLO PRODH MSE KUNNR KUKLA.


      SELECT REQTSN
             DATAPAKID
             RECORD
             RYEAR
             POPER
             RACCT
             PRCTR
             VKBUR_PA
             VKGRP_PA
             WWLO_PA
             PRODH_PA
             ZZ1_SDSDIST_MSE
             KUNNR
             ZZ1_SHOPTYPE
             HSL
        FROM ACDOCP
        INTO TABLE GT_ACDOCP
        WHERE RLDNR = '0L'
         AND RBUKRS = P_BUKRS
         AND RYEAR  = P_RYEAR
         AND POPER  IN S_POPER
         AND RACCT  IN LRT_RACCT
         AND PRCTR  IN S_PRCTR
         AND VKBUR_PA  IN S_VKBUR
         AND VKGRP_PA  IN S_VKGRP
         AND WWLO_PA   IN S_WWLO
         AND PRODH_PA  IN S_PRODH
         AND ZZ1_SDSDIST_MSE    IN S_MSE
         AND KUNNR  IN S_KUNNR
         AND ZZ1_SHOPTYPE  IN S_KUKLA
         AND CATEGORY = 'Z01'.

      SORT GT_ACDOCP BY PRCTR VKBUR VKGRP WWLO PRODH MSE KUNNR KUKLA.

*      SELECT KOKRS
*             KSTAR
*             DATBI
*             DATAB
*             KATYP
*        FROM CSKB
*        INTO TABLE GT_CSKB                            "#EC CI_SGLSELECT
*        WHERE KOKRS = '1000'
*          AND KSTAR IN LRT_RACCT
*          AND DATBI >= SY-DATUM
*          AND DATAB <= SY-DATUM
*          AND KATYP = '11'.

    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM DISPLAY_ALV .
  DATA: LS_LAYOUT   TYPE LVC_S_LAYO.

  LS_LAYOUT-CWIDTH_OPT  = ABAP_TRUE.
*  lw_layout-box_fname   = 'SEL'.
  LS_LAYOUT-STYLEFNAME  = 'STYLE'.
  LS_LAYOUT-CTAB_FNAME  = 'COLOR'.

  PERFORM CREATE_FIELDCAT.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC' ##FM_SUBRC_OK
    EXPORTING
      I_CALLBACK_PROGRAM      = SY-REPID
      IS_LAYOUT_LVC           = LS_LAYOUT
      IT_FIELDCAT_LVC         = GT_FIELDCAT
      I_CALLBACK_TOP_OF_PAGE  = 'TOP_OF_PAGE'
*     i_callback_pf_status_set = 'SET_PF_STATUS'
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
    TABLES
      T_OUTTAB                = GT_OUTPUT
    EXCEPTIONS
      PROGRAM_ERROR           = 1
      OTHERS                  = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE ##CALLED.
  DATA:
    LT_HEADER TYPE SLIS_T_LISTHEADER,
    LS_HEADER TYPE SLIS_LISTHEADER,
    LV_NUM    TYPE I.

  LS_HEADER-TYP = 'S'.
  LS_HEADER-KEY = 'Company Code:' ##NO_TEXT.
  LS_HEADER-INFO = P_BUKRS.
  APPEND LS_HEADER TO LT_HEADER.

  LS_HEADER-TYP = 'S'.
  LS_HEADER-KEY = 'Year:' ##NO_TEXT.
  LS_HEADER-INFO = P_RYEAR.
  APPEND LS_HEADER TO LT_HEADER.

  LS_HEADER-TYP = 'S'.
  LS_HEADER-KEY = 'Period:' ##NO_TEXT.
  IF S_POPER-HIGH IS INITIAL.
    LS_HEADER-INFO = |{ S_POPER-LOW+1(2) } To { S_POPER-LOW+1(2) }|.
  ELSE.
    LS_HEADER-INFO = |{ S_POPER-LOW+1(2) } To { S_POPER-HIGH+1(2) }|.
  ENDIF.
  APPEND LS_HEADER TO LT_HEADER.

  IF CB_TOT IS NOT INITIAL.
    LV_NUM = LINES( S_PRCTR ).
    READ TABLE S_PRCTR ASSIGNING FIELD-SYMBOL(<LFS_PRCTR>) INDEX 1.
    LS_HEADER-TYP = 'S'.
    LS_HEADER-KEY = 'Profit Center:' ##NO_TEXT.
    IF LV_NUM = 1 AND <LFS_PRCTR>-SIGN = 'I' AND ( <LFS_PRCTR>-OPTION = 'EQ' OR <LFS_PRCTR>-OPTION = 'BT' ).
      IF <LFS_PRCTR>-HIGH IS INITIAL.
        LS_HEADER-INFO = |{ <LFS_PRCTR>-LOW ALPHA = OUT }|.
      ELSE.
        LS_HEADER-INFO = |{ <LFS_PRCTR>-LOW ALPHA = OUT } To { <LFS_PRCTR>-HIGH ALPHA = OUT }|.
      ENDIF.
    ELSE.
      LS_HEADER-INFO = 'Multiple selection' ##NO_TEXT.
    ENDIF.
    APPEND LS_HEADER TO LT_HEADER.
  ENDIF.

  LS_HEADER-TYP = 'S'.
  LS_HEADER-KEY = ''.
  LS_HEADER-INFO = ''.
  APPEND LS_HEADER TO LT_HEADER.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LT_HEADER. "it_listheader.
ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*& Form create_fieldcat
*&---------------------------------------------------------------------*
FORM CREATE_FIELDCAT .
  DATA: LF_TXT(50).

  PERFORM ADD_FCAT USING 'PL           ' 'PL (THB)' ##NO_TEXT
        '' ''.
  IF CB_TOT IS INITIAL.
    PERFORM ADD_FCAT USING 'PRCTR        ' 'Profit Center' ##NO_TEXT
          'ACDOCA' 'PRCTR'.
    PERFORM ADD_FCAT USING 'VKBUR        ' 'Sale Office' ##NO_TEXT
          'ACDOCA' 'VKBUR_PA'.
    PERFORM ADD_FCAT USING 'VKGRP        ' 'Sale Group' ##NO_TEXT
          'ACDOCA' 'VKGRP_PA'.
    PERFORM ADD_FCAT USING 'WWLO         ' 'LOB' ##NO_TEXT
          'ACDOCA' 'WWLO_PA'.
    PERFORM ADD_FCAT USING 'PRODH        ' 'Prod. Hierarchy01-1' ##NO_TEXT
          'ACDOCA' 'PRODH_PA'.
    PERFORM ADD_FCAT USING 'MSE          ' 'Distribution Channel (SDS)' ##NO_TEXT
          'ACDOCA' 'ZZ1_SDSDIST_MSE'.
    PERFORM ADD_FCAT USING 'KUNNR        ' 'Customer' ##NO_TEXT
          'ACDOCA' 'KUNNR'.
    PERFORM ADD_FCAT USING 'KUKLA        ' 'Customer Classification' ##NO_TEXT
          'ACDOCA' 'ZZ1_SHOPTYPE'.
  ENDIF.
  LF_TXT = |RS{ P_RYEAR+2(2) - 1 }|.
  PERFORM ADD_FCAT USING 'RS_LY        ' LF_TXT
        'ACDOCA' 'HSL'.

  LF_TXT = |{ LF_TXT }(%)|.
  PERFORM ADD_FCAT USING 'RS_LY_PER    ' LF_TXT
        '' ''.

  LF_TXT = |RB{ P_RYEAR+2(2) }|.
  PERFORM ADD_FCAT USING 'RB_YY        ' LF_TXT
        '' ''.

  LF_TXT = |{ LF_TXT }(%)|.
  PERFORM ADD_FCAT USING 'RB_YY_PER    ' LF_TXT
        '' ''.

  LF_TXT = |RS{ P_RYEAR+2(2) }|.
  PERFORM ADD_FCAT USING 'RS_YY        ' LF_TXT
        '' ''.

  LF_TXT = |{ LF_TXT }(%)|.
  PERFORM ADD_FCAT USING 'RS_YY_PER    ' LF_TXT
        '' ''.

  LF_TXT = |vs RB{ P_RYEAR+2(2) }| ##NO_TEXT.
  PERFORM ADD_FCAT USING 'RB_YY_VAR    ' LF_TXT
        '' ''.

  LF_TXT = |{ LF_TXT }(%)|.
  PERFORM ADD_FCAT USING 'RB_YY_VAR_PER' LF_TXT
        '' ''.

  LF_TXT = |vs FY{ P_RYEAR+2(2) - 1 }| ##NO_TEXT.
  PERFORM ADD_FCAT USING 'FY_LY_VAR    ' LF_TXT
        '' ''.

  LF_TXT = |{ LF_TXT }(%)|.
  PERFORM ADD_FCAT USING 'FY_LY_VAR_PER' LF_TXT
        '' ''.
*  PERFORM add_fcat USING '' '' '' ''.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_FCAT
*&---------------------------------------------------------------------*
FORM ADD_FCAT  USING UF_FNAME ##PERF_NO_TYPE
                     UF_TEXT ##PERF_NO_TYPE
                     UF_REF_TABLE ##PERF_NO_TYPE
                     UF_REF_FIELD ##PERF_NO_TYPE.

  DATA LS_FCAT   TYPE LINE OF LVC_T_FCAT.

*  CLEAR gs_fcat.
  LS_FCAT-FIELDNAME = UF_FNAME.
  LS_FCAT-REPTEXT   = UF_TEXT.
  LS_FCAT-COLTEXT   = UF_TEXT.
  LS_FCAT-REF_TABLE = UF_REF_TABLE.
  LS_FCAT-REF_FIELD = UF_REF_FIELD.
  APPEND LS_FCAT TO GT_FIELDCAT.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form PREPARE_DATA
*&---------------------------------------------------------------------*
FORM PREPARE_DATA .

*  "1 SDS_Price List Sales_FG
*  PERFORM APPEND_DATA USING '1'
*                            'Z10_40100'.
*
*  "2 SDS_Price List Sales_SV
*  PERFORM APPEND_DATA USING '2'
*                            'Z10_40110'.
*
*  "3 SDS_Price List
*  PERFORM CALCULATE_DATA USING '1'
*                               '2'
*                               '+'
*                               '3'
*                               'SDS_Price List' ##NO_TEXT.
*
*  "4 Discount FG
*  PERFORM APPEND_DATA USING '4'
*                            'Z10_40200'.
*
*  "5 Discount SV
*  PERFORM APPEND_DATA USING '5'
*                            'Z10_40210'.
*
*  "6 Discount
*  PERFORM CALCULATE_DATA USING '4'
*                               '5'
*                               '+'
*                               '6'
*                               'Discount' ##NO_TEXT.
*
*  "7 Gross Sales FG
*  PERFORM CALCULATE_DATA USING '1'
*                               '4'
*                               '-'
*                               '7'
*                               'Gross Sales FG' ##NO_TEXT.
*
*  "8 Gross Sales SV
*  PERFORM CALCULATE_DATA USING '2'
*                               '5'
*                               '-'
*                               '8'
*                               'Gross Sales SV' ##NO_TEXT.

  "7 Gross Sales FG
  PERFORM APPEND_DATA USING '7'
                            'Z10_40100'
                            'X'.

  "8 Gross Sales SV
  PERFORM APPEND_DATA USING '8'
                            'Z10_40110'
                            'X'.

  "9 Gross Sales
  PERFORM CALCULATE_DATA USING '7'
                               '8'
                               '+'
                               '9'
                               'Gross Sales' ##NO_TEXT.

  "10 Sales Deduction FG
  PERFORM APPEND_DATA USING '10'
                            'Z10_40300'
                            ''.

  "11 Sales Deduction SV
  PERFORM APPEND_DATA USING '11'
                            'Z10_40310'
                            ''.

  "12 Sales Deduction
  PERFORM CALCULATE_DATA USING '10'
                               '11'
                               '+'
                               '12'
                               'Sales Deduction' ##NO_TEXT.

  "13 Net Sales  FG
  PERFORM CALCULATE_DATA USING '7'
                               '10'
                               '-'
                               '13'
                               'Net Sales  FG' ##NO_TEXT.

  "14 Net Sales SV
  PERFORM CALCULATE_DATA USING '8'
                               '11'
                               '-'
                               '14'
                               'Net Sales SV' ##NO_TEXT.

  "15 Net Sales
  PERFORM CALCULATE_DATA USING '13'
                               '14'
                               '+'
                               '15'
                               'Net Sales' ##NO_TEXT.

  "16 COGS FG
  PERFORM APPEND_DATA USING '16'
                            'Z10_41000'
                            ''.

  "17 COS SV
  PERFORM APPEND_DATA USING '17'
                            'Z10_41010'
                            ''.

  "18 COGS
  PERFORM CALCULATE_DATA USING '16'
                               '17'
                               '+'
                               '18'
                               'COGS'.

  "19 GP FG
  PERFORM CALCULATE_DATA USING '13'
                               '16'
                               '-'
                               '19'
                               'GP FG'.

  "20 GP SV
  PERFORM CALCULATE_DATA USING '14'
                               '17'
                               '-'
                               '20'
                               'GP SV'.

  "21 GP
  PERFORM CALCULATE_DATA USING '19'
                               '20'
                               '+'
                               '21'
                               'GP'.

  "22 Sales Promotion
  PERFORM APPEND_DATA USING '22'
                            'Z10_51010'
                            ''.

  "23 Logistics cost
  PERFORM APPEND_DATA USING '23'
                            'Z10_51020'
                            ''.

  "24 Warranty
  PERFORM APPEND_DATA USING '24'
                            'Z10_51030'
                            ''.

  "25 Commission
  PERFORM APPEND_DATA USING '25'
                            'Z10_51040'
                            ''.

  "26 SGA - Variable
  PERFORM SUM_DATA USING '22'
                         '25'
                         '26'
                         'SGA - Variable' ##NO_TEXT.

  "27 Labor cost
  PERFORM APPEND_DATA USING '27'
                            'Z10_52010'
                            ''.

  "28 Advertising
  PERFORM APPEND_DATA USING '28'
                            'Z10_52020'
                            ''.

  "29 Depreciation
  PERFORM APPEND_DATA USING '29'
                            'Z10_52030'
                            ''.

  "30 Maintenance & Repair
  PERFORM APPEND_DATA USING '30'
                            'Z10_52040'
                            ''.

  "31 Consultant & Professional Fee
  PERFORM APPEND_DATA USING '31'
                            'Z10_52050'
                            ''.

  "32 Rental & Lease
  PERFORM APPEND_DATA USING '32'
                            'Z10_52060'
                            ''.

  "33 Travelling Expenses
  PERFORM APPEND_DATA USING '33'
                            'Z10_52070'
                            ''.

  "34 Management Fee
  PERFORM APPEND_DATA USING '34'
                            'Z10_52080'
                            ''.

  "35 Utility
  PERFORM APPEND_DATA USING '35'
                            'Z10_52090'
                            ''.

  "36 Call center fee
  PERFORM APPEND_DATA USING '36'
                            'Z10_52100'
                            ''.

  "37 Communication Expense
  PERFORM APPEND_DATA USING '37'
                            'Z10_52105'
                            ''.

  "38 Membership Expense
  PERFORM APPEND_DATA USING '38'
                            'Z10_52110'
                            ''.

  "39 Training & Seminar
  PERFORM APPEND_DATA USING '39'
                            'Z10_52120'
                            ''.

  "40 Entertainment & Gift
  PERFORM APPEND_DATA USING '40'
                            'Z10_52130'
                            ''.

  "41 Office&Service Supply
  PERFORM APPEND_DATA USING '41'
                            'Z10_52140'
                            ''.

  "42 Miscellanous Expense
  PERFORM APPEND_DATA USING '42'
                            'Z10_52150'
                            ''.

  "43 Tax Expenses
  PERFORM APPEND_DATA USING '43'
                            'Z10_52160'
                            ''.

  "44 Insurance Expenses
  PERFORM APPEND_DATA USING '44'
                            'Z10_52170'
                            ''.

  "45 Bad Debt
  PERFORM APPEND_DATA USING '45'
                            'Z10_52180'
                            ''.

  "46 CSR &Donation
  PERFORM APPEND_DATA USING '46'
                            'Z10_52190'
                            ''.

  "47 Royalty Expenses
  PERFORM APPEND_DATA USING '47'
                            'Z10_52200'
                            ''.

  "48 SGA - Fixed
  PERFORM SUM_DATA USING '27'
                         '47'
                         '48'
                         'SGA - Fixed' ##NO_TEXT.

  "49 SGA
  PERFORM CALCULATE_DATA USING '26'
                               '48'
                               '+'
                               '49'
                               'SGA'.

  "50 OP
  PERFORM CALCULATE_DATA USING '21'
                               '49'
                               '-'
                               '50'
                               'OP'.

  "51 FX Gain/Loss
  PERFORM APPEND_DATA USING '51'
                            'Z10_61010'
                            ''.

  "52 Interest Income
  PERFORM APPEND_DATA USING '52'
                            'Z10_61020'
                            ''.

  "53 Others Income
  PERFORM APPEND_DATA USING '53'
                            'Z10_61030'
                            ''.

  "54 Other Expense
  PERFORM APPEND_DATA USING '54'
                            'Z10_61040'
                            ''.

  "55 Non-OP
  PERFORM SUM_DATA USING '51'
                         '54'
                         '55'
                         'Non-OP' ##NO_TEXT.

  "56 NP
  PERFORM CALCULATE_DATA USING '50'
                               '55'
                               '-'
                               '56'
                               'NP'.

  "57 Corporate Tax
  PERFORM APPEND_DATA USING '57'
                            'Z10_62010'
                            ''.

  "58 NP after Tax
  PERFORM CALCULATE_DATA USING '56'
                               '57'
                               '-'
                               '58'
                               'NP after Tax' ##NO_TEXT.

  "59 EBIT
  PERFORM CALCULATE_DATA USING '56'
                               ''
                               ''
                               '59'
                               'EBIT'.

  "60 EBITDA
  PERFORM CALCULATE_DATA USING '59'
                               '29'
                               '+'
                               '60'
                               'EBITDA'.

  READ TABLE GT_TOTAL INTO DATA(LS_TOTAL) WITH KEY NO = '15'."INDEX 15.
  IF CB_TOT IS INITIAL .
    LOOP AT GT_DATA INTO DATA(LS_OUTPUT) ##INTO_OK.
      APPEND INITIAL LINE TO GT_OUTPUT ASSIGNING FIELD-SYMBOL(<L_OUTPUT>).
      MOVE-CORRESPONDING LS_OUTPUT TO <L_OUTPUT>.
      IF LS_TOTAL-RS_LY IS NOT INITIAL.
        <L_OUTPUT>-RS_LY_PER =  ( <L_OUTPUT>-RS_LY / LS_TOTAL-RS_LY ) * 100.
      ENDIF.

      IF LS_TOTAL-RB_YY IS NOT INITIAL.
        <L_OUTPUT>-RB_YY_PER =  ( <L_OUTPUT>-RB_YY / LS_TOTAL-RB_YY ) * 100.
      ENDIF.

      IF LS_TOTAL-RS_YY IS NOT INITIAL.
        <L_OUTPUT>-RS_YY_PER =  ( <L_OUTPUT>-RS_YY / LS_TOTAL-RS_YY ) * 100.
      ENDIF.

      <L_OUTPUT>-RB_YY_VAR = <L_OUTPUT>-RS_YY - <L_OUTPUT>-RB_YY.

      IF <L_OUTPUT>-RB_YY IS NOT INITIAL.
        <L_OUTPUT>-RB_YY_VAR_PER = ( <L_OUTPUT>-RS_YY / <L_OUTPUT>-RB_YY ) * 100.
      ENDIF.

      <L_OUTPUT>-FY_LY_VAR = <L_OUTPUT>-RS_YY - <L_OUTPUT>-RS_LY.

      IF <L_OUTPUT>-RS_LY IS NOT INITIAL.
        <L_OUTPUT>-FY_LY_VAR_PER = ( <L_OUTPUT>-RS_YY / <L_OUTPUT>-RS_LY ) * 100.
      ENDIF.

      CASE <L_OUTPUT>-NO.
        WHEN '3' OR '6' OR '9' OR '12' OR '18'.
          PERFORM ADD_COLOR_ALL TABLES <L_OUTPUT>-COLOR
                                 USING '3' '1' '0'.
        WHEN '13' OR '14' OR '15' OR '21' OR '26' OR '48' OR '49' OR '50' OR '55'
          OR '56' OR '58' OR '59' OR '60'.
          PERFORM ADD_COLOR_ALL TABLES <L_OUTPUT>-COLOR
                                 USING '5' '1' '0'.
        WHEN '19' OR '20'.
          PERFORM ADD_COLOR_ALL TABLES <L_OUTPUT>-COLOR
                                 USING '5' '0' '0'.
      ENDCASE.
    ENDLOOP.
  ELSE.
    LOOP AT GT_TOTAL INTO LS_OUTPUT ##INTO_OK.
      APPEND INITIAL LINE TO GT_OUTPUT ASSIGNING <L_OUTPUT>.
      MOVE-CORRESPONDING LS_OUTPUT TO <L_OUTPUT>.
      IF LS_TOTAL-RS_LY IS NOT INITIAL.
        <L_OUTPUT>-RS_LY_PER =  ( <L_OUTPUT>-RS_LY / LS_TOTAL-RS_LY ) * 100.
      ENDIF.

      IF LS_TOTAL-RB_YY IS NOT INITIAL.
        <L_OUTPUT>-RB_YY_PER =  ( <L_OUTPUT>-RB_YY / LS_TOTAL-RB_YY ) * 100.
      ENDIF.

      IF LS_TOTAL-RS_YY IS NOT INITIAL.
        <L_OUTPUT>-RS_YY_PER =  ( <L_OUTPUT>-RS_YY / LS_TOTAL-RS_YY ) * 100.
      ENDIF.

      <L_OUTPUT>-RB_YY_VAR = <L_OUTPUT>-RS_YY - <L_OUTPUT>-RB_YY.

      IF <L_OUTPUT>-RB_YY IS NOT INITIAL.
        <L_OUTPUT>-RB_YY_VAR_PER = ( <L_OUTPUT>-RS_YY / <L_OUTPUT>-RB_YY ) * 100.
      ENDIF.

      <L_OUTPUT>-FY_LY_VAR = <L_OUTPUT>-RS_YY - <L_OUTPUT>-RS_LY.

      IF <L_OUTPUT>-RS_LY IS NOT INITIAL.
        <L_OUTPUT>-FY_LY_VAR_PER = ( <L_OUTPUT>-RS_YY / <L_OUTPUT>-RS_LY ) * 100.
      ENDIF.

      CASE <L_OUTPUT>-NO.
        WHEN '3' OR '6' OR '9' OR '12' OR '18'.
          PERFORM ADD_COLOR_ALL TABLES <L_OUTPUT>-COLOR
                                 USING '3' '1' '0'.
        WHEN '13' OR '14' OR '15' OR '21' OR '26' OR '48' OR '49' OR '50' OR '55'
          OR '56' OR '58' OR '59' OR '60'.
          PERFORM ADD_COLOR_ALL TABLES <L_OUTPUT>-COLOR
                                 USING '5' '1' '0'.
        WHEN '19' OR '20'.
          PERFORM ADD_COLOR_ALL TABLES <L_OUTPUT>-COLOR
                                 USING '5' '0' '0'.
      ENDCASE.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_COLOR_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ADD_COLOR_ALL  TABLES T_COLOR ##PERF_NO_TYPE
                    USING  UF_COLOR ##PERF_NO_TYPE
                           UF_INT ##PERF_NO_TYPE
                           UF_INV ##PERF_NO_TYPE.

  DATA: LS_COLOR TYPE LVC_S_SCOL.

  LS_COLOR-FNAME      = 'PL'.
  LS_COLOR-COLOR-COL  = UF_COLOR.
  LS_COLOR-COLOR-INT  = UF_INT.
  LS_COLOR-COLOR-INV  = UF_INV.
  APPEND LS_COLOR TO T_COLOR.

  LS_COLOR-FNAME      = 'PRCTR'.
  LS_COLOR-COLOR-COL  = UF_COLOR.
  LS_COLOR-COLOR-INT  = UF_INT.
  LS_COLOR-COLOR-INV  = UF_INV.
  APPEND LS_COLOR TO T_COLOR.

  LS_COLOR-FNAME      = 'VKBUR'.
  LS_COLOR-COLOR-COL  = UF_COLOR.
  LS_COLOR-COLOR-INT  = UF_INT.
  LS_COLOR-COLOR-INV  = UF_INV.
  APPEND LS_COLOR TO T_COLOR.

  LS_COLOR-FNAME      = 'VKGRP'.
  LS_COLOR-COLOR-COL  = UF_COLOR.
  LS_COLOR-COLOR-INT  = UF_INT.
  LS_COLOR-COLOR-INV  = UF_INV.
  APPEND LS_COLOR TO T_COLOR.

  LS_COLOR-FNAME      = 'WWLO'.
  LS_COLOR-COLOR-COL  = UF_COLOR.
  LS_COLOR-COLOR-INT  = UF_INT.
  LS_COLOR-COLOR-INV  = UF_INV.
  APPEND LS_COLOR TO T_COLOR.

  LS_COLOR-FNAME      = 'PRODH'.
  LS_COLOR-COLOR-COL  = UF_COLOR.
  LS_COLOR-COLOR-INT  = UF_INT.
  LS_COLOR-COLOR-INV  = UF_INV.
  APPEND LS_COLOR TO T_COLOR.

  LS_COLOR-FNAME      = 'MSE'.
  LS_COLOR-COLOR-COL  = UF_COLOR.
  LS_COLOR-COLOR-INT  = UF_INT.
  LS_COLOR-COLOR-INV  = UF_INV.
  APPEND LS_COLOR TO T_COLOR.

  LS_COLOR-FNAME      = 'KUNNR'.
  LS_COLOR-COLOR-COL  = UF_COLOR.
  LS_COLOR-COLOR-INT  = UF_INT.
  LS_COLOR-COLOR-INV  = UF_INV.
  APPEND LS_COLOR TO T_COLOR.

  LS_COLOR-FNAME      = 'KUKLA'.
  LS_COLOR-COLOR-COL  = UF_COLOR.
  LS_COLOR-COLOR-INT  = UF_INT.
  LS_COLOR-COLOR-INV  = UF_INV.
  APPEND LS_COLOR TO T_COLOR.

  LS_COLOR-FNAME      = 'RS_LY'.
  LS_COLOR-COLOR-COL  = UF_COLOR.
  LS_COLOR-COLOR-INT  = UF_INT.
  LS_COLOR-COLOR-INV  = UF_INV.
  APPEND LS_COLOR TO T_COLOR.

  LS_COLOR-FNAME      = 'RS_LY_PER'.
  LS_COLOR-COLOR-COL  = UF_COLOR.
  LS_COLOR-COLOR-INT  = UF_INT.
  LS_COLOR-COLOR-INV  = UF_INV.
  APPEND LS_COLOR TO T_COLOR.

  LS_COLOR-FNAME      = 'RB_YY'.
  LS_COLOR-COLOR-COL  = UF_COLOR.
  LS_COLOR-COLOR-INT  = UF_INT.
  LS_COLOR-COLOR-INV  = UF_INV.
  APPEND LS_COLOR TO T_COLOR.

  LS_COLOR-FNAME      = 'RB_YY_PER'.
  LS_COLOR-COLOR-COL  = UF_COLOR.
  LS_COLOR-COLOR-INT  = UF_INT.
  LS_COLOR-COLOR-INV  = UF_INV.
  APPEND LS_COLOR TO T_COLOR.

  LS_COLOR-FNAME      = 'RS_YY'.
  LS_COLOR-COLOR-COL  = UF_COLOR.
  LS_COLOR-COLOR-INT  = UF_INT.
  LS_COLOR-COLOR-INV  = UF_INV.
  APPEND LS_COLOR TO T_COLOR.

  LS_COLOR-FNAME      = 'RS_YY_PER'.
  LS_COLOR-COLOR-COL  = UF_COLOR.
  LS_COLOR-COLOR-INT  = UF_INT.
  LS_COLOR-COLOR-INV  = UF_INV.
  APPEND LS_COLOR TO T_COLOR.

  LS_COLOR-FNAME      = 'RB_YY_VAR'.
  LS_COLOR-COLOR-COL  = UF_COLOR.
  LS_COLOR-COLOR-INT  = UF_INT.
  LS_COLOR-COLOR-INV  = UF_INV.
  APPEND LS_COLOR TO T_COLOR.

  LS_COLOR-FNAME      = 'RB_YY_VAR_PER'.
  LS_COLOR-COLOR-COL  = UF_COLOR.
  LS_COLOR-COLOR-INT  = UF_INT.
  LS_COLOR-COLOR-INV  = UF_INV.
  APPEND LS_COLOR TO T_COLOR.

  LS_COLOR-FNAME      = 'FY_LY_VAR'.
  LS_COLOR-COLOR-COL  = UF_COLOR.
  LS_COLOR-COLOR-INT  = UF_INT.
  LS_COLOR-COLOR-INV  = UF_INV.
  APPEND LS_COLOR TO T_COLOR.

  LS_COLOR-FNAME      = 'FY_LY_VAR_PER'.
  LS_COLOR-COLOR-COL  = UF_COLOR.
  LS_COLOR-COLOR-INT  = UF_INT.
  LS_COLOR-COLOR-INV  = UF_INV.
  APPEND LS_COLOR TO T_COLOR.

ENDFORM.                    " ADD_COLOR_ALL
*&---------------------------------------------------------------------*
*& Form APPEND_DATA
*&---------------------------------------------------------------------*
FORM APPEND_DATA  USING UF_NO ##PERF_NO_TYPE
                        UF_GL_GROUP ##PERF_NO_TYPE
                        UF_RE_SIGN  ##PERF_NO_TYPE.

  DATA: "LT_OUTPUT LIKE TABLE OF GS_DATA,
    LS_OUTPUT LIKE GS_DATA,
    LF_CK(1).

  DATA: LRT_RACCT    TYPE RANGE OF ACDOCA-RACCT.

  LRT_RACCT = VALUE #( FOR RECORD IN GT_GL_GROUP WHERE ( SETNAME = UF_GL_GROUP )
                      ( SIGN = RECORD-VALSIGN
                        OPTION = RECORD-VALOPTION
                        LOW = RECORD-VALFROM
                        HIGH = RECORD-VALTO )
                    ).
  READ TABLE GT_GL_GROUP INTO DATA(LS_GL_GROUP) WITH KEY SETNAME = UF_GL_GROUP.
  LS_OUTPUT-NO = UF_NO.
  LS_OUTPUT-PL = LS_GL_GROUP-DESCRIPT.
  APPEND LS_OUTPUT TO GT_TOTAL.

  IF LRT_RACCT[] IS NOT INITIAL.
    LOOP AT GT_ACDOCA_RS ASSIGNING FIELD-SYMBOL(<L_ACDOCA_RS>) WHERE RACCT IN LRT_RACCT.
      LF_CK = 'X'.
      LS_OUTPUT-NO = UF_NO.
      LS_OUTPUT-PL = LS_GL_GROUP-DESCRIPT.
      LS_OUTPUT-RS_LY = <L_ACDOCA_RS>-HSL.
*      READ TABLE GT_CSKB WITH KEY KSTAR = <L_ACDOCA_RS>-RACCT
*                         TRANSPORTING NO FIELDS.
*      IF SY-SUBRC = 0.
*        LS_OUTPUT-RS_LY = LS_OUTPUT-RS_LY * -1.
*      ENDIF.
      IF UF_RE_SIGN = 'X'.
        LS_OUTPUT-RS_LY = LS_OUTPUT-RS_LY * -1.
      ENDIF.
      COLLECT LS_OUTPUT INTO GT_TOTAL.

      LS_OUTPUT-PRCTR = <L_ACDOCA_RS>-PRCTR.
      IF <L_ACDOCA_RS>-XPAOBJNR_CO_REL IS NOT INITIAL.
        LS_OUTPUT-VKBUR = <L_ACDOCA_RS>-VKBUR.
        LS_OUTPUT-VKGRP = <L_ACDOCA_RS>-VKGRP.
        LS_OUTPUT-WWLO  = <L_ACDOCA_RS>-WWLO.
        LS_OUTPUT-PRODH = <L_ACDOCA_RS>-PRODH.
        LS_OUTPUT-MSE   = <L_ACDOCA_RS>-MSE.
        LS_OUTPUT-KUNNR = <L_ACDOCA_RS>-KUNNR.
        LS_OUTPUT-KUKLA = <L_ACDOCA_RS>-KUKLA.
      ENDIF.
      COLLECT LS_OUTPUT INTO GT_DATA.

      CLEAR: LS_OUTPUT.
    ENDLOOP.

    LOOP AT GT_ACDOCP ASSIGNING FIELD-SYMBOL(<L_ACDOCP>) WHERE RACCT IN LRT_RACCT.
      LF_CK = 'X'.
      LS_OUTPUT-NO = UF_NO.
      LS_OUTPUT-PL = LS_GL_GROUP-DESCRIPT.
      LS_OUTPUT-RB_YY = <L_ACDOCP>-HSL.
*      READ TABLE GT_CSKB WITH KEY KSTAR = <L_ACDOCP>-RACCT
*                         TRANSPORTING NO FIELDS.
*      IF SY-SUBRC = 0.
*        LS_OUTPUT-RB_YY = LS_OUTPUT-RB_YY * -1.
*      ENDIF.
      IF UF_RE_SIGN = 'X'.
        LS_OUTPUT-RB_YY = LS_OUTPUT-RB_YY * -1.
      ENDIF.
      COLLECT LS_OUTPUT INTO GT_TOTAL.

      LS_OUTPUT-PRCTR = <L_ACDOCP>-PRCTR.
      LS_OUTPUT-VKBUR = <L_ACDOCP>-VKBUR.
      LS_OUTPUT-VKGRP = <L_ACDOCP>-VKGRP.
      LS_OUTPUT-WWLO  = <L_ACDOCP>-WWLO.
      LS_OUTPUT-PRODH = <L_ACDOCP>-PRODH.
      LS_OUTPUT-MSE   = <L_ACDOCP>-MSE.
      LS_OUTPUT-KUNNR = <L_ACDOCP>-KUNNR.
      LS_OUTPUT-KUKLA = <L_ACDOCP>-KUKLA.
      COLLECT LS_OUTPUT INTO GT_DATA.

      CLEAR: LS_OUTPUT.
    ENDLOOP.

    LOOP AT GT_ACDOCA ASSIGNING FIELD-SYMBOL(<L_ACDOCA>) WHERE RACCT IN LRT_RACCT.
      LF_CK = 'X'.
      LS_OUTPUT-NO = UF_NO.
      LS_OUTPUT-PL = LS_GL_GROUP-DESCRIPT.
      LS_OUTPUT-RS_YY = <L_ACDOCA>-HSL.
*      READ TABLE GT_CSKB WITH KEY KSTAR = <L_ACDOCA>-RACCT
*                         TRANSPORTING NO FIELDS.
*      IF SY-SUBRC = 0.
*        LS_OUTPUT-RS_YY = LS_OUTPUT-RS_YY * -1.
*      ENDIF.
      IF UF_RE_SIGN = 'X'.
        LS_OUTPUT-RS_YY = LS_OUTPUT-RS_YY * -1.
      ENDIF.
      COLLECT LS_OUTPUT INTO GT_TOTAL.

      LS_OUTPUT-PRCTR = <L_ACDOCA>-PRCTR.
      IF <L_ACDOCA>-XPAOBJNR_CO_REL IS NOT INITIAL.
        LS_OUTPUT-VKBUR = <L_ACDOCA>-VKBUR.
        LS_OUTPUT-VKGRP = <L_ACDOCA>-VKGRP.
        LS_OUTPUT-WWLO  = <L_ACDOCA>-WWLO.
        LS_OUTPUT-PRODH = <L_ACDOCA>-PRODH.
        LS_OUTPUT-MSE   = <L_ACDOCA>-MSE.
        LS_OUTPUT-KUNNR = <L_ACDOCA>-KUNNR.
        LS_OUTPUT-KUKLA = <L_ACDOCA>-KUKLA.
      ENDIF.
      COLLECT LS_OUTPUT INTO GT_DATA.

      CLEAR: LS_OUTPUT.
    ENDLOOP.
  ENDIF.

  IF LF_CK IS INITIAL.
    APPEND LS_OUTPUT TO GT_DATA.
  ENDIF.

*  IF UF_RE_SIGN = 'X'.
*    READ TABLE GT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>) WITH KEY NO = UF_NO.
*    IF sy-subrc = 0.
*      <L_DATA>-RS_LY = <L_DATA>-RS_LY * -1.
*      <L_DATA>-RB_YY = <L_DATA>-RB_YY * -1.
*      <L_DATA>-RS_YY = <L_DATA>-RS_YY * -1.
*    ENDIF.
*
*    READ TABLE GT_TOTAL ASSIGNING FIELD-SYMBOL(<L_TOTAL>) WITH KEY NO = UF_NO.
*    IF sy-subrc = 0.
*      <L_TOTAL>-RS_LY = <L_TOTAL>-RS_LY * -1.
*      <L_TOTAL>-RB_YY = <L_TOTAL>-RB_YY * -1.
*      <L_TOTAL>-RS_YY = <L_TOTAL>-RS_YY * -1.
*    ENDIF.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form calculate_data
*&---------------------------------------------------------------------*
FORM CALCULATE_DATA  USING UF_INDEX1 ##PERF_NO_TYPE
                           UF_INDEX2 ##PERF_NO_TYPE
                           UF_SIGN ##PERF_NO_TYPE
                           UF_NO ##PERF_NO_TYPE
                           UF_DES ##PERF_NO_TYPE.

  DATA: LS_TOTAL LIKE GS_DATA.

  READ TABLE GT_TOTAL INTO LS_TOTAL WITH KEY NO = UF_INDEX1.
  LS_TOTAL-NO = UF_NO.
  LS_TOTAL-PL = UF_DES.
  COLLECT LS_TOTAL INTO GT_TOTAL.
  COLLECT LS_TOTAL INTO GT_DATA.
  CLEAR: LS_TOTAL.

  IF UF_INDEX2 IS NOT INITIAL.
    READ TABLE GT_TOTAL INTO LS_TOTAL WITH KEY NO = UF_INDEX2.
    LS_TOTAL-NO = UF_NO.
    LS_TOTAL-PL = UF_DES.
    IF UF_SIGN = '-'.
      LS_TOTAL-RS_LY = LS_TOTAL-RS_LY * -1.
      LS_TOTAL-RB_YY = LS_TOTAL-RB_YY * -1.
      LS_TOTAL-RS_YY = LS_TOTAL-RS_YY * -1.
    ENDIF.
    COLLECT LS_TOTAL INTO GT_TOTAL.
    COLLECT LS_TOTAL INTO GT_DATA.
    CLEAR: LS_TOTAL.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUM_DATA
*&---------------------------------------------------------------------*
FORM SUM_DATA  USING UF_FROM ##PERF_NO_TYPE
                     UF_TO ##PERF_NO_TYPE
                     UF_NO ##PERF_NO_TYPE
                     UF_DES ##PERF_NO_TYPE.

  DATA: LF_START(10).
*        lf_end TYPE i.

  DATA: LS_TOTAL LIKE GS_DATA.

  LF_START = UF_FROM.
*  lf_end = UF_TO.
  DO.
    READ TABLE GT_TOTAL INTO LS_TOTAL WITH KEY NO = LF_START.
    LS_TOTAL-NO = UF_NO.
    LS_TOTAL-PL = UF_DES.
    COLLECT LS_TOTAL INTO GT_TOTAL.
    COLLECT LS_TOTAL INTO GT_DATA.
    CLEAR: LS_TOTAL.

    IF LF_START = UF_TO.
      EXIT.
    ENDIF.

    LF_START = LF_START + 1.
    CONDENSE LF_START.
  ENDDO.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form init
*&---------------------------------------------------------------------*
FORM INIT .
  DATA: LF_YEAR   TYPE BAPI0002_4-FISCAL_YEAR,
        LF_PERIOD TYPE BAPI0002_4-FISCAL_PERIOD,
        LS_RETURN TYPE BAPIRETURN1 ##NEEDED.

  CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
    EXPORTING
      COMPANYCODEID = P_BUKRS
      POSTING_DATE  = SY-DATUM
    IMPORTING
      FISCAL_YEAR   = LF_YEAR
      FISCAL_PERIOD = LF_PERIOD
      RETURN        = LS_RETURN.

  P_RYEAR = LF_YEAR.

  S_POPER[] = VALUE #( ( SIGN = 'I'
                         OPTION = 'BT'
                         LOW = LF_PERIOD
                         HIGH = LF_PERIOD ) ).
ENDFORM.
