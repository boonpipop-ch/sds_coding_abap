*-----------------------------------------------------------------------
*  Program ID         : ZSDSCOR0030
*  Creation Date      : 09.07.2024
*  Author             : Nadtaya T.(Eviden)
*  Add-on ID          : N/A
*  Description        : Program to adjust profit center for service project
*  Purpose            :
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  25.11.2024  F36K908494  Jutamas Y.  set sales office and sales group
*-----------------------------------------------------------------------
REPORT ZSDSCOR0030.

*----------------------------------------------------------------------*
*   I N C L U D E                                                      *
*----------------------------------------------------------------------*
INCLUDE ZSDSCOR0030_TOP.

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* §5.1 define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
CLASS LCL_HANDLE_EVENTS DEFINITION ##CLASS_FINAL .
  PUBLIC SECTION.
    METHODS:
      ON_USER_COMMAND FOR EVENT ADDED_FUNCTION OF CL_SALV_EVENTS
        IMPORTING E_SALV_FUNCTION,

      ON_TOP_OF_PAGE FOR EVENT TOP_OF_PAGE OF CL_SALV_EVENTS_HIERSEQ
        IMPORTING R_TOP_OF_PAGE.
ENDCLASS.                    "lcl_handle_events DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
* §5.2 implement the events for handling the events of cl_salv_table
*---------------------------------------------------------------------*
CLASS LCL_HANDLE_EVENTS IMPLEMENTATION.
  METHOD ON_USER_COMMAND.

    PERFORM HANDLE_USER_COMMAND USING E_SALV_FUNCTION.

  ENDMETHOD.                    "on_user_command
  METHOD ON_TOP_OF_PAGE.
    DATA: LR_CONTENT TYPE REF TO CL_SALV_FORM_ELEMENT.

**... create the content
    PERFORM CREATE_ALV_FORM_CONTENT_TOP
      CHANGING LR_CONTENT.

*... set the content
    R_TOP_OF_PAGE->SET_CONTENT( LR_CONTENT ).
  ENDMETHOD.                    "on_top_of_page
ENDCLASS.                    "lcl_handle_events IMPLEMENTATION.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B01 WITH FRAME TITLE TEXT-001.
  PARAMETERS: P_RLDNR TYPE RLDNR DEFAULT '0L'.
  PARAMETERS: P_BUKRS TYPE T001-BUKRS DEFAULT '1000' OBLIGATORY.
  PARAMETERS: P_GJAHR TYPE GJAHR DEFAULT SY-DATUM(4) OBLIGATORY.
  SELECT-OPTIONS: S_BUDAT FOR BKPF-BUDAT OBLIGATORY.
  SELECT-OPTIONS: S_BELNR FOR BKPF-BELNR.
  SELECT-OPTIONS: S_POSID FOR PRPS-POSID.
*-Beg of INS by  Jutamas Y.
  SELECT-OPTIONS: S_ACTTYP FOR ACDOCA-ZZ1_ACTTYPE_MSE .
*-End of INS by Jutamas Y.
  SELECTION-SCREEN BEGIN OF BLOCK B02 WITH FRAME TITLE TEXT-002.
    PARAMETERS: P_BLART TYPE BKPF-BLART DEFAULT 'SA' OBLIGATORY.
  SELECTION-SCREEN END   OF BLOCK B02.
SELECTION-SCREEN END   OF BLOCK B01.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
*AT SELECTION-SCREEN.
*  PERFORM f_screen_check.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_cegrp-low.
*  PERFORM f4_ordgrp.


AT SELECTION-SCREEN OUTPUT.
* Set Screen format

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
*  PERFORM F_GET_CONSTANTS.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_GET_DATA.


*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM F_DISPLAY_HIERSEQ.

*&---------------------------------------------------------------------*
*& Form f_get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_DATA.

  DATA: LT_ACDOCA       TYPE STANDARD TABLE OF ACDOCA,
        LF_SELECT_QUERY TYPE STRING,
        LR_AWTYP        TYPE STANDARD TABLE OF FAGL_R_S_SELOPT,
        LR_VRGNG        TYPE STANDARD TABLE OF FAGL_R_S_SELOPT,
        LR_ACCTY        TYPE STANDARD TABLE OF FAGL_R_S_SELOPT.


  ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZSDSCOR0030'
                                                  IF_PARAM = 'AWTYP'
                                        IMPORTING ET_RANGE = LR_AWTYP ).

  ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZSDSCOR0030'
                                                  IF_PARAM = 'VRGNG'
                                        IMPORTING ET_RANGE = LR_VRGNG ).

  ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZSDSCOR0030'
                                                  IF_PARAM = 'ZZ1_ACTTYPE_MSE'
                                        IMPORTING ET_RANGE = LR_ACCTY ).

  IF LINES( LR_AWTYP ) EQ 0 OR LINES( LR_VRGNG ) EQ 0 OR LINES( LR_ACCTY ) EQ 0.
    MESSAGE TEXT-E03 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  PERFORM F_SET_QUERY CHANGING LF_SELECT_QUERY.

  PERFORM F_GET_DBTAB TABLES LT_ACDOCA LR_AWTYP LR_VRGNG LR_ACCTY USING LF_SELECT_QUERY.

  DELETE LT_ACDOCA WHERE PS_POSID IS INITIAL.

  IF LINES( LT_ACDOCA ) EQ 0.
    MESSAGE TEXT-E04 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  PERFORM F_PROCESS_DATA TABLES LT_ACDOCA.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_selections
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_SELECTIONS .

  DATA: LR_SELECTIONS TYPE REF TO CL_SALV_SELECTIONS,
        LT_ROWS       TYPE SALV_T_ROW.

  TRY.
      LR_SELECTIONS = GR_HIERSEQ->GET_SELECTIONS( 1 ).
    ##NO_HANDLER    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

  LT_ROWS = LR_SELECTIONS->GET_SELECTED_ROWS( ).

  IF LINES( LT_ROWS ) EQ 0.
    MESSAGE TEXT-006 TYPE 'E'.
  ENDIF.

  PERFORM F_POSTING TABLES LT_ROWS.

  GR_HIERSEQ->REFRESH( REFRESH_MODE = IF_SALV_C_REFRESH=>FULL
*    REFRESH_MODE = IF_SALV_C_REFRESH=>SOFT " ALV: Data Element for Constants
*    S_STABLE     =                         " ALV Control: Refresh Stability
  ).

  TRY.
      LR_SELECTIONS->SET_SELECTED_ROWS( LT_ROWS ).
    ##NO_HANDLER    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

ENDFORM.                    " get_selections
*&---------------------------------------------------------------------*
*&      Form  handle_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND USING I_UCOMM TYPE SALV_DE_FUNCTION.

  CASE I_UCOMM.
    WHEN 'POST'.
      PERFORM GET_SELECTIONS.
  ENDCASE.

ENDFORM.                    " handle_user_command
*&---------------------------------------------------------------------*
*&      Form  create_alv_form_content_top
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CREATE_ALV_FORM_CONTENT_TOP
  CHANGING CR_CONTENT     TYPE REF TO CL_SALV_FORM_ELEMENT.

  DATA: LR_GRID   TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
        LR_GRID_1 TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
        LR_LABEL  TYPE REF TO CL_SALV_FORM_LABEL,
        LR_TEXT   TYPE REF TO CL_SALV_FORM_TEXT,
        L_TEXT    TYPE STRING.

*... create a grid
  CREATE OBJECT LR_GRID.

  L_TEXT = SY-TITLE.
  LR_GRID->CREATE_HEADER_INFORMATION(
    ROW     = 1
    COLUMN  = 1
    TEXT    = L_TEXT
    TOOLTIP = L_TEXT ).

*... add a row to the grid -> row 2
  LR_GRID->ADD_ROW( ).

*... in the cell [3,1] create a grid
  LR_GRID_1 = LR_GRID->CREATE_GRID(
    ROW    = 3
    COLUMN = 1 ).

  L_TEXT = |{ TEXT-004 }:|.
  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW     = 1
    COLUMN  = 1
    TEXT    = L_TEXT
    TOOLTIP = SPACE ).

  L_TEXT = P_RLDNR.
  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW     = 1
    COLUMN  = 2
    TEXT    = L_TEXT
    TOOLTIP = SPACE ).

  L_TEXT = |{ TEXT-005 }:|.
  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW     = 2
    COLUMN  = 1
    TEXT    = L_TEXT
    TOOLTIP = SPACE ).

  L_TEXT = P_BUKRS.
  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW     = 2
    COLUMN  = 2
    TEXT    = L_TEXT
    TOOLTIP = SPACE ).

  L_TEXT = |{ TEXT-007 }:|.
  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW     = 3
    COLUMN  = 1
    TEXT    = L_TEXT
    TOOLTIP = SPACE ).

  L_TEXT = P_GJAHR.
  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW     = 3
    COLUMN  = 2
    TEXT    = L_TEXT
    TOOLTIP = SPACE ).


  LR_LABEL->SET_LABEL_FOR( LR_TEXT ).

*... content is the top grid
  CR_CONTENT = LR_GRID.

ENDFORM.                    " create_alv_form_content_top
*&---------------------------------------------------------------------*
*& Form f_authorize_check
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GC_TCODE
*&---------------------------------------------------------------------*
FORM F_AUTHORIZE_CHECK USING UF_TCODE  TYPE  SY-TCODE.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
    ID 'TCD' FIELD UF_TCODE.

  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE S172(00) WITH UF_TCODE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_QUERY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LF_SELECT_QUERY
*&---------------------------------------------------------------------*
FORM F_SET_QUERY  CHANGING UF_SELECT_QUERY TYPE STRING.

  CLEAR UF_SELECT_QUERY.

  UF_SELECT_QUERY = 'A~RLDNR, A~RBUKRS, A~GJAHR, A~BELNR, A~DOCLN, A~RYEAR, A~DOCNR_LD, A~BSCHL, A~RTCUR'.
  UF_SELECT_QUERY = |{ UF_SELECT_QUERY }, A~RACCT, A~DRCRK, A~TSL, A~SGTXT, A~AWREF, A~PS_POSID, A~PAOBJNR,A~POPER|.

*...Account Assignment Fields
  PERFORM F_SET_QUERY_FIELDS USING 'ACDOC_SI_GL_ACCAS'
                             CHANGING UF_SELECT_QUERY.

*...COPA Fields
  PERFORM F_SET_QUERY_FIELDS USING 'ACDOC_SI_COPA'
                             CHANGING UF_SELECT_QUERY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_DBTAB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_ACDOCA
*&      --> LF_SELECT_QUERY
*&---------------------------------------------------------------------*
FORM F_GET_DBTAB  TABLES UT_ACDOCA TYPE STANDARD TABLE  ##PERF_NO_TYPE
                         UR_AWTYP  TYPE STANDARD TABLE  ##PERF_NO_TYPE
                         UR_VRGNG  TYPE STANDARD TABLE  ##PERF_NO_TYPE
                         UR_ACCTY  TYPE STANDARD TABLE  ##PERF_NO_TYPE
                   USING UF_SELECT_QUERY TYPE STRING.

  SELECT (UF_SELECT_QUERY)
    INTO CORRESPONDING FIELDS OF TABLE @UT_ACDOCA
    FROM ACDOCA AS A INNER JOIN BKPF
    ON A~RBUKRS EQ BKPF~BUKRS
    AND A~BELNR EQ BKPF~BELNR
    AND A~GJAHR EQ BKPF~GJAHR
    WHERE A~AWTYP IN @UR_AWTYP
      AND A~VRGNG IN @UR_VRGNG
      AND A~XTRUEREV EQ @SPACE
      AND A~RBUKRS EQ @P_BUKRS
      AND A~GJAHR EQ @P_GJAHR
      AND A~BELNR IN @S_BELNR
      AND A~BUDAT IN @S_BUDAT
      AND A~RLDNR EQ @P_RLDNR
*      AND A~ACCASTY EQ 'PR'
      AND A~XPAOBJNR_CO_REL EQ @ABAP_TRUE
      AND A~PS_POSID IN @S_POSID
      AND BKPF~STBLG EQ ''
      AND A~ZZ1_ACTTYPE_MSE IN @UR_ACCTY
      AND A~ZZ1_ACTTYPE_MSE IN @S_ACTTYP
      AND
    NOT EXISTS ( SELECT B~RBUKRS, B~BELNR, B~GJAHR, T~BKTXT, T~AWKEY
    FROM ACDOCA AS B INNER JOIN BKPF AS T
    ON B~RBUKRS EQ T~BUKRS
    AND B~BELNR EQ T~BELNR
    AND B~GJAHR EQ T~GJAHR
    WHERE B~AWTYP EQ 'BKPF'
      AND B~RLDNR EQ @P_RLDNR
      AND T~BKTXT EQ A~AWREF  "Co Document no.
      AND B~RBUKRS EQ A~RBUKRS
      AND T~STBLG EQ ''
    ) ORDER BY A~RBUKRS, A~BELNR, A~GJAHR, A~DOCLN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PROCESS_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_ACDOCA
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA  TABLES UT_ACDOCA TYPE FINST_ACDOCA.

  DATA: LT_ACDOCA TYPE STANDARD TABLE OF ACDOCA,
        LF_BUZEI  TYPE ACDOCA-BUZEI.
  DATA: BEGIN OF LS_CRMS4D_SERV_I,
          PROCESS_TYPE    TYPE VKORG,
          SALES_ORG_SD    TYPE AUART,
          SALES_OFFICE_SD TYPE VKBUR,
          SALES_GROUP_SD  TYPE VKGRP,
        END OF LS_CRMS4D_SERV_I.
  DATA: LT_DOC_LINKS TYPE CRMT_DOC_FLOW_DB_WRKT,
        LS_SLAVE     TYPE G_TYPE_S_SLAVE,
        LV_PRCTR     TYPE PRCTR.

*-Beg of INS by Jutamas
  DATA: LF_VKBUR_PA TYPE ACDOCA-VKBUR_PA,
        LF_VKGRP_PA TYPE ACDOCA-VKGRP_PA.
*-End of INS by Jutamas

  APPEND LINES OF UT_ACDOCA TO LT_ACDOCA.
  DELETE ADJACENT DUPLICATES FROM LT_ACDOCA COMPARING RBUKRS BELNR GJAHR.

  LOOP AT LT_ACDOCA ASSIGNING FIELD-SYMBOL(<L_HEADER>).

    APPEND INITIAL LINE TO GT_MASTER ASSIGNING FIELD-SYMBOL(<L_MASTER>).
*    MOVE-CORRESPONDING <L_HEADER> TO <L_MASTER>.
    <L_MASTER>-REF_BUKRS = <L_HEADER>-RBUKRS.
    <L_MASTER>-REF_BELNR = <L_HEADER>-BELNR.
    <L_MASTER>-REF_GJAHR = <L_HEADER>-GJAHR.
    <L_MASTER>-REF_AWKEY = <L_HEADER>-AWREF.
    <L_MASTER>-WAERS     = <L_HEADER>-RTCUR.

    LF_BUZEI = 0.

    LOOP AT UT_ACDOCA ASSIGNING FIELD-SYMBOL(<L_ITEM>) WHERE RBUKRS EQ <L_HEADER>-RBUKRS
                                                         AND BELNR  EQ <L_HEADER>-BELNR
                                                         AND GJAHR  EQ <L_HEADER>-GJAHR.
      LF_BUZEI = LF_BUZEI + 1.
      APPEND INITIAL LINE TO GT_SLAVE ASSIGNING FIELD-SYMBOL(<L_SLAVE>).
      MOVE-CORRESPONDING <L_MASTER> TO <L_SLAVE> ##ENH_OK.
      MOVE-CORRESPONDING <L_ITEM> TO <L_SLAVE>   ##ENH_OK.

      CASE <L_SLAVE>-BSCHL.
        WHEN '40'.
          <L_SLAVE>-BSCHL = '50'.
        WHEN '50'.
          <L_SLAVE>-BSCHL = '40'.
      ENDCASE.

      <L_SLAVE>-BUZEI = LF_BUZEI.
      <L_SLAVE>-WRBTR = ABS( <L_ITEM>-TSL ).

      "Get data from CRMS4D_SERV_I
      CLEAR:LS_CRMS4D_SERV_I.
      SELECT PROCESS_TYPE,HEADER_GUID,SALES_ORG_SD,SALES_OFFICE_SD,SALES_GROUP_SD
*-Beg of INS by Jutamas
             , CAST( ' ' AS CHAR( 1 ) ) AS USED
*-End of INS by Jutamas
        FROM CRMS4D_SERV_I
       WHERE OBJTYPE_H = 'BUS2000116'
         AND AC_OBJECT_TYPE = '03'
         AND AC_ASSIGNMENT = @<L_SLAVE>-PS_POSID
        ORDER BY PROCESS_TYPE
        INTO TABLE @DATA(LT_CRMS4D_SERV_I).
      IF SY-SUBRC <> 0.
        SELECT PROCESS_TYPE,HEADER_GUID,SALES_ORG_SD,SALES_OFFICE_SD,SALES_GROUP_SD
*-Beg of INS by Jutamas
               , CAST( ' ' AS CHAR( 1 ) ) AS USED
*-End of INS by Jutamas
          FROM CRMS4D_SERV_I
         WHERE OBJTYPE_H = 'BUS2000116'
           AND AC_OBJECT_TYPE = '01'
           AND ZZ1_POSID = @<L_SLAVE>-PS_POSID
          ORDER BY PROCESS_TYPE
          INTO TABLE @LT_CRMS4D_SERV_I.
      ENDIF.
*-Beg of INS by Jutamas
      SORT LT_CRMS4D_SERV_I BY PROCESS_TYPE SALES_ORG_SD SALES_OFFICE_SD SALES_GROUP_SD .
*-End of INS by Jutamas

      CASE <L_SLAVE>-BSCHL.
        WHEN '40'.
          APPEND INITIAL LINE TO GT_SLAVE ASSIGNING <L_SLAVE>.
          MOVE-CORRESPONDING <L_MASTER> TO <L_SLAVE> ##ENH_OK.
          MOVE-CORRESPONDING <L_ITEM>   TO <L_SLAVE> ##ENH_OK.
          LF_BUZEI = LF_BUZEI + 1.
          <L_SLAVE>-BUZEI = LF_BUZEI.
          <L_SLAVE>-BSCHL = '50'.
          <L_SLAVE>-WRBTR = ABS( <L_ITEM>-TSL ).

          IF <L_SLAVE>-RACCT = '4031000030'. "Revenue
            <L_SLAVE>-WRBTR = ABS( <L_ITEM>-TSL ).

          ELSEIF <L_SLAVE>-RACCT = '4131000300'.  "Cost

            IF LT_CRMS4D_SERV_I IS NOT INITIAL.
              DATA(LT_SERV_I_TMP) = LT_CRMS4D_SERV_I.
              DELETE ADJACENT DUPLICATES FROM LT_SERV_I_TMP COMPARING PROCESS_TYPE.
              DATA(LV_LINES) = LINES( LT_SERV_I_TMP ).
              "Case 1: Only one Service "Transaction Type"
              IF LV_LINES = 1.
                READ TABLE LT_SERV_I_TMP ASSIGNING FIELD-SYMBOL(<LFS_SERV_I>) INDEX 1.
                IF SY-SUBRC = 0.
                  LS_CRMS4D_SERV_I-PROCESS_TYPE    = <LFS_SERV_I>-PROCESS_TYPE.
                  LS_CRMS4D_SERV_I-SALES_ORG_SD    = <LFS_SERV_I>-SALES_ORG_SD.
                  LS_CRMS4D_SERV_I-SALES_OFFICE_SD = <LFS_SERV_I>-SALES_OFFICE_SD.
                  LS_CRMS4D_SERV_I-SALES_GROUP_SD  = <LFS_SERV_I>-SALES_GROUP_SD.
*-Beg of INS by Jutamas
                  LF_VKBUR_PA = LS_CRMS4D_SERV_I-SALES_OFFICE_SD.
                  LF_VKGRP_PA = LS_CRMS4D_SERV_I-SALES_GROUP_SD .
*-End of INS by Jutamas
                ENDIF.

                <L_SLAVE>-WRBTR = ABS( <L_ITEM>-TSL ).  "*********************************
              ELSE.
                "Case 2: More than one Service "Transaction Type"
                SELECT KOKRS,BELNR,BUZEI,PERIO,PSPNR,KSTAR,AWTYP,REFBN,REFGJ,REFBK,WKGBTR
                  FROM COVP
                 WHERE KOKRS = '1000'
                   AND PERIO = @<L_ITEM>-POPER
                   AND PSPNR = @<L_SLAVE>-PS_POSID
                   AND KSTAR = '4131000300'        "Cost  @<L_SLAVE>-RACCT
                   AND AWTYP <> 'AUAK'
                  ORDER BY KOKRS,BELNR,BUZEI,PERIO
                  INTO TABLE @DATA(LT_COVP).
                IF SY-SUBRC = 0.

                  "Get 1st line and loop check document inside CRMS4D_SERV_I-HEADER_GUID
                  LOOP AT LT_COVP ASSIGNING FIELD-SYMBOL(<LFS_COVP>).
                    DATA(LV_TABIX) = SY-TABIX.

                    LOOP AT LT_CRMS4D_SERV_I ASSIGNING FIELD-SYMBOL(<LFS_CRMS4D_SERV_I>)
*-Beg of INS by Jutamas
                                            WHERE USED IS INITIAL .
*-End of INS by Jutamas
                      CALL FUNCTION 'CRM_DOC_FLOW_READ_DB'
                        EXPORTING
                          IV_HEADER_GUID = <LFS_CRMS4D_SERV_I>-HEADER_GUID
                        IMPORTING
                          ET_DOC_LINKS   = LT_DOC_LINKS
*                         ET_DOC_FLOW_PLNK_ATTR       =
*                         ET_DOC_FLOW_IPLK_ATTR       =
                        EXCEPTIONS
                          ERROR_OCCURRED = 1
                          OTHERS         = 2.
                      IF SY-SUBRC = 0.
                        READ TABLE LT_DOC_LINKS ASSIGNING FIELD-SYMBOL(<LFS_DOC_LINKS>) WITH KEY OBJTYPE_A = 'BUS2000116' "#EC CI_SORTSEQ
                                                                                                 OBJTYPE_B = 'BUS2017'.
                        IF SY-SUBRC = 0.
                          IF <LFS_DOC_LINKS>-OBJKEY_B+0(10) = <LFS_COVP>-REFBN AND <LFS_DOC_LINKS>-OBJKEY_B+10(4) = <LFS_COVP>-REFGJ.
                            CLEAR: LS_CRMS4D_SERV_I.
                            LS_CRMS4D_SERV_I-PROCESS_TYPE    = <LFS_CRMS4D_SERV_I>-PROCESS_TYPE.
                            LS_CRMS4D_SERV_I-SALES_ORG_SD    = <LFS_CRMS4D_SERV_I>-SALES_ORG_SD.
                            LS_CRMS4D_SERV_I-SALES_OFFICE_SD = <LFS_CRMS4D_SERV_I>-SALES_OFFICE_SD.
                            LS_CRMS4D_SERV_I-SALES_GROUP_SD  = <LFS_CRMS4D_SERV_I>-SALES_GROUP_SD.

*-Beg of INS by Jutamas
                            CLEAR : LF_VKBUR_PA, LF_VKGRP_PA .
                            LF_VKBUR_PA = LS_CRMS4D_SERV_I-SALES_OFFICE_SD.
                            LF_VKGRP_PA = LS_CRMS4D_SERV_I-SALES_GROUP_SD .
                            <LFS_CRMS4D_SERV_I>-USED = ABAP_TRUE.
*-End of INS by Jutamas

                            PERFORM F_SET_PRCTR USING LS_CRMS4D_SERV_I-SALES_ORG_SD
                                                      LS_CRMS4D_SERV_I-PROCESS_TYPE
                                                      LS_CRMS4D_SERV_I-SALES_OFFICE_SD
                                                      LS_CRMS4D_SERV_I-SALES_GROUP_SD
                                                CHANGING LV_PRCTR." <L_SLAVE>-PRCTR.
                            EXIT.
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ENDLOOP.

                    IF LV_TABIX = 1.
                      <L_SLAVE>-WRBTR = ABS( <LFS_COVP>-WKGBTR ).
                      <L_SLAVE>-PRCTR = LV_PRCTR.
                    ELSE.
                      CLEAR: LS_SLAVE.
                      LS_SLAVE = <L_SLAVE>.
*-Beg of INS by Jutamas
                      LS_SLAVE-VKBUR_PA = LF_VKBUR_PA .
                      LS_SLAVE-VKGRP_PA = LF_VKGRP_PA .
*-End of INS by Jutamas

                      LS_SLAVE-WRBTR = ABS( <LFS_COVP>-WKGBTR ).
                      LS_SLAVE-PRCTR = LV_PRCTR.

                      LF_BUZEI = LF_BUZEI + 1.

                      LS_SLAVE-BUZEI = LF_BUZEI.

                      APPEND LS_SLAVE TO GT_SLAVE.

                    ENDIF.

                  ENDLOOP.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.


          IF LS_CRMS4D_SERV_I IS INITIAL.
            READ TABLE LT_CRMS4D_SERV_I ASSIGNING <LFS_CRMS4D_SERV_I> INDEX 1.
            IF SY-SUBRC = 0.
              LS_CRMS4D_SERV_I-PROCESS_TYPE    = <LFS_CRMS4D_SERV_I>-PROCESS_TYPE.
              LS_CRMS4D_SERV_I-SALES_ORG_SD    = <LFS_CRMS4D_SERV_I>-SALES_ORG_SD.
              LS_CRMS4D_SERV_I-SALES_OFFICE_SD = <LFS_CRMS4D_SERV_I>-SALES_OFFICE_SD.
              LS_CRMS4D_SERV_I-SALES_GROUP_SD  = <LFS_CRMS4D_SERV_I>-SALES_GROUP_SD.

*-Beg of INS by Jutamas
              <L_SLAVE>-VKBUR_PA = <LFS_CRMS4D_SERV_I>-SALES_OFFICE_SD .
              <L_SLAVE>-VKGRP_PA = <LFS_CRMS4D_SERV_I>-SALES_GROUP_SD .
*-End of INS by Jutamas
            ENDIF.

            PERFORM F_SET_PRCTR USING LS_CRMS4D_SERV_I-SALES_ORG_SD
                                      LS_CRMS4D_SERV_I-PROCESS_TYPE
                                      LS_CRMS4D_SERV_I-SALES_OFFICE_SD
                                      LS_CRMS4D_SERV_I-SALES_GROUP_SD
                                CHANGING <L_SLAVE>-PRCTR.

          ENDIF.

        WHEN '50'.
          APPEND INITIAL LINE TO GT_SLAVE ASSIGNING <L_SLAVE>.
          MOVE-CORRESPONDING <L_MASTER> TO <L_SLAVE> ##ENH_OK.
          MOVE-CORRESPONDING <L_ITEM>   TO <L_SLAVE> ##ENH_OK.
          LF_BUZEI = LF_BUZEI + 1.
          <L_SLAVE>-BUZEI = LF_BUZEI.
          <L_SLAVE>-BSCHL = '40'.
          <L_SLAVE>-WRBTR = ABS( <L_ITEM>-TSL ).


          IF <L_SLAVE>-RACCT = '4031000030'. "Revenue
            <L_SLAVE>-WRBTR = ABS( <L_ITEM>-TSL ).

          ELSEIF <L_SLAVE>-RACCT = '4131000300'.  "Cost

            IF LT_CRMS4D_SERV_I IS NOT INITIAL.
              LT_SERV_I_TMP = LT_CRMS4D_SERV_I.
              DELETE ADJACENT DUPLICATES FROM LT_SERV_I_TMP COMPARING PROCESS_TYPE.
              LV_LINES = LINES( LT_SERV_I_TMP ).
              "Case 1: Only one Service "Transaction Type"
              IF LV_LINES = 1.
                READ TABLE LT_SERV_I_TMP ASSIGNING <LFS_SERV_I> INDEX 1.
                IF SY-SUBRC = 0.
                  LS_CRMS4D_SERV_I-PROCESS_TYPE    = <LFS_SERV_I>-PROCESS_TYPE.
                  LS_CRMS4D_SERV_I-SALES_ORG_SD    = <LFS_SERV_I>-SALES_ORG_SD.
                  LS_CRMS4D_SERV_I-SALES_OFFICE_SD = <LFS_SERV_I>-SALES_OFFICE_SD.
                  LS_CRMS4D_SERV_I-SALES_GROUP_SD  = <LFS_SERV_I>-SALES_GROUP_SD.
                ENDIF.

                <L_SLAVE>-WRBTR = ABS( <L_ITEM>-TSL ).  "*********************************
              ELSE.
                "Case 2: More than one Service "Transaction Type"
                SELECT KOKRS,BELNR,BUZEI,PERIO,PSPNR,KSTAR,AWTYP,REFBN,REFGJ,REFBK,WKGBTR
                  FROM COVP
                 WHERE KOKRS = '1000'
                   AND PERIO = @<L_ITEM>-POPER
                   AND PSPNR = @<L_SLAVE>-PS_POSID
                   AND KSTAR = '4131000300'        "Cost  @<L_SLAVE>-RACCT
                   AND AWTYP <> 'AUAK'
                  ORDER BY KOKRS,BELNR,BUZEI,PERIO
                  INTO TABLE @LT_COVP.
                IF SY-SUBRC = 0.

                  "Get 1st line and loop check document inside CRMS4D_SERV_I-HEADER_GUID
                  LOOP AT LT_COVP ASSIGNING <LFS_COVP>.
                    LV_TABIX = SY-TABIX.

                    LOOP AT LT_CRMS4D_SERV_I ASSIGNING <LFS_CRMS4D_SERV_I>
*-Beg of INS by Jutamas
                                                 WHERE USED IS INITIAL.
*-End of INS by Jutamas
                      CALL FUNCTION 'CRM_DOC_FLOW_READ_DB'
                        EXPORTING
                          IV_HEADER_GUID = <LFS_CRMS4D_SERV_I>-HEADER_GUID
                        IMPORTING
                          ET_DOC_LINKS   = LT_DOC_LINKS
*                         ET_DOC_FLOW_PLNK_ATTR       =
*                         ET_DOC_FLOW_IPLK_ATTR       =
                        EXCEPTIONS
                          ERROR_OCCURRED = 1
                          OTHERS         = 2.
                      IF SY-SUBRC = 0.
                        READ TABLE LT_DOC_LINKS ASSIGNING <LFS_DOC_LINKS> WITH KEY OBJTYPE_A = 'BUS2000116' "#EC CI_SORTSEQ
                                                                                   OBJTYPE_B = 'BUS2017'.
                        IF SY-SUBRC = 0.
                          IF <LFS_DOC_LINKS>-OBJKEY_B+0(10) = <LFS_COVP>-REFBN AND <LFS_DOC_LINKS>-OBJKEY_B+10(4) = <LFS_COVP>-REFGJ.
                            CLEAR: LS_CRMS4D_SERV_I.
                            LS_CRMS4D_SERV_I-PROCESS_TYPE    = <LFS_CRMS4D_SERV_I>-PROCESS_TYPE.
                            LS_CRMS4D_SERV_I-SALES_ORG_SD    = <LFS_CRMS4D_SERV_I>-SALES_ORG_SD.
                            LS_CRMS4D_SERV_I-SALES_OFFICE_SD = <LFS_CRMS4D_SERV_I>-SALES_OFFICE_SD.
                            LS_CRMS4D_SERV_I-SALES_GROUP_SD  = <LFS_CRMS4D_SERV_I>-SALES_GROUP_SD.
*-Beg of INS by Jutamas
                            LF_VKBUR_PA  =  <LFS_CRMS4D_SERV_I>-SALES_OFFICE_SD.
                            LF_VKGRP_PA  =  <LFS_CRMS4D_SERV_I>-SALES_GROUP_SD .
                            <LFS_CRMS4D_SERV_I>-USED = ABAP_TRUE .
*-End of INS by Jutamas
                            PERFORM F_SET_PRCTR USING LS_CRMS4D_SERV_I-SALES_ORG_SD
                                                      LS_CRMS4D_SERV_I-PROCESS_TYPE
                                                      LS_CRMS4D_SERV_I-SALES_OFFICE_SD
                                                      LS_CRMS4D_SERV_I-SALES_GROUP_SD
                                                CHANGING LV_PRCTR. "<L_SLAVE>-PRCTR.
                            EXIT.
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ENDLOOP.

                    IF LV_TABIX = 1.
                      <L_SLAVE>-WRBTR = ABS( <LFS_COVP>-WKGBTR ).
                      <L_SLAVE>-PRCTR = LV_PRCTR.

*-Beg of INS by Jutamas
                      <L_SLAVE>-VKBUR_PA = LF_VKBUR_PA .
                      <L_SLAVE>-VKGRP_PA = LF_VKGRP_PA .
                      LS_SLAVE = <L_SLAVE>.
*-End of INS by Jutamas
                    ELSE.
*-Beg of MOD by Jutamas
*                      CLEAR: LS_SLAVE.
*                      LS_SLAVE = <L_SLAVE>.
                      LS_SLAVE-VKBUR_PA = LF_VKBUR_PA .
                      LS_SLAVE-VKGRP_PA = LF_VKGRP_PA .
*-End of MOD by Jutamas

                      LS_SLAVE-WRBTR = ABS( <LFS_COVP>-WKGBTR ).
                      LS_SLAVE-PRCTR = LV_PRCTR.

                      LF_BUZEI = LF_BUZEI + 1.

                      LS_SLAVE-BUZEI = LF_BUZEI.

                      APPEND LS_SLAVE TO GT_SLAVE.

                    ENDIF.
                  ENDLOOP.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

          IF LS_CRMS4D_SERV_I IS INITIAL.
            READ TABLE LT_CRMS4D_SERV_I ASSIGNING <LFS_CRMS4D_SERV_I>  INDEX 1.
            IF SY-SUBRC = 0.
              LS_CRMS4D_SERV_I-PROCESS_TYPE    = <LFS_CRMS4D_SERV_I>-PROCESS_TYPE.
              LS_CRMS4D_SERV_I-SALES_ORG_SD    = <LFS_CRMS4D_SERV_I>-SALES_ORG_SD.
              LS_CRMS4D_SERV_I-SALES_OFFICE_SD = <LFS_CRMS4D_SERV_I>-SALES_OFFICE_SD.
              LS_CRMS4D_SERV_I-SALES_GROUP_SD  = <LFS_CRMS4D_SERV_I>-SALES_GROUP_SD.

*-Beg of INS by Jutamas
              <L_SLAVE>-VKBUR_PA = <LFS_CRMS4D_SERV_I>-SALES_OFFICE_SD .
              <L_SLAVE>-VKGRP_PA = <LFS_CRMS4D_SERV_I>-SALES_GROUP_SD .
*-End of INS by Jutamas

            ENDIF.

            PERFORM F_SET_PRCTR USING LS_CRMS4D_SERV_I-SALES_ORG_SD
                                      LS_CRMS4D_SERV_I-PROCESS_TYPE
                                      LS_CRMS4D_SERV_I-SALES_OFFICE_SD
                                      LS_CRMS4D_SERV_I-SALES_GROUP_SD
                                CHANGING <L_SLAVE>-PRCTR.
          ENDIF.
      ENDCASE.

      IF <L_SLAVE>-PRCTR IS INITIAL.
        <L_MASTER>-EXCEPTION = 1.
        <L_MASTER>-MSGTX     = TEXT-E02.
      ENDIF.

    ENDLOOP.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_display_hierseq
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_DISPLAY_HIERSEQ .
  DATA:
    LT_BINDING TYPE SALV_T_HIERSEQ_BINDING,
    LS_BINDING TYPE SALV_S_HIERSEQ_BINDING.

*  DATA:
*    lr_functions TYPE REF TO cl_salv_functions_list.

  DATA:
    LR_COLUMNS TYPE REF TO CL_SALV_COLUMNS_HIERSEQ,
    LR_COLUMN  TYPE REF TO CL_SALV_COLUMN_HIERSEQ.

  DATA:
    LR_LEVEL TYPE REF TO CL_SALV_HIERSEQ_LEVEL.

  DATA:
    LR_SELECTIONS TYPE REF TO CL_SALV_SELECTIONS.

  DATA: LR_EVENTS TYPE REF TO CL_SALV_EVENTS_HIERSEQ.
*... set list title
  DATA: LR_DISPLAY_SETTINGS TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
        L_TITLE             TYPE LVC_TITLE.


  DATA: LT_ROWS       TYPE SALV_T_ROW.
*... create the binding information between master and slave

  LS_BINDING-MASTER = 'REF_BUKRS'.
  LS_BINDING-SLAVE  = 'REF_BUKRS'.
  APPEND LS_BINDING TO LT_BINDING.
  LS_BINDING-MASTER = 'REF_BELNR'.
  LS_BINDING-SLAVE  = 'REF_BELNR'.
  APPEND LS_BINDING TO LT_BINDING.
  LS_BINDING-MASTER = 'REF_GJAHR'.
  LS_BINDING-SLAVE  = 'REF_GJAHR'.
  APPEND LS_BINDING TO LT_BINDING.

*... create an ALV hierseq table
  TRY.
      CL_SALV_HIERSEQ_TABLE=>FACTORY(
        EXPORTING
          T_BINDING_LEVEL1_LEVEL2 = LT_BINDING
        IMPORTING
          R_HIERSEQ               = GR_HIERSEQ
        CHANGING
          T_TABLE_LEVEL1          = GT_MASTER
          T_TABLE_LEVEL2          = GT_SLAVE ).
    CATCH CX_SALV_DATA_ERROR CX_SALV_NOT_FOUND.
      MESSAGE TEXT-E01 TYPE 'E'.
  ENDTRY.

*... §3 Functions
*... §3.2 include own functions by setting own status
  GR_HIERSEQ->SET_SCREEN_STATUS(
    PFSTATUS      = 'ZSTANDARD'
    REPORT        = SY-REPID
    SET_FUNCTIONS = CL_SALV_HIERSEQ_TABLE=>C_FUNCTIONS_ALL ).

*... §3.1 activate ALV generic Functions
*  lr_functions = gr_hierseq->get_functions( ).
*  lr_functions->set_all( abap_true ).

*
*... *** MASTER Settings ***
  TRY.
      LR_COLUMNS = GR_HIERSEQ->GET_COLUMNS( 1 ).
    ##NO_HANDLER    CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND.
  ENDTRY.

*... optimize the columns
  LR_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ).


*... set the columns technical
*  TRY.
*      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'MANDT' ).
*      LR_COLUMN->SET_TECHNICAL( IF_SALV_C_BOOL_SAP=>TRUE ).
*    ##NO_HANDLER   CATCH CX_SALV_DATA_ERROR CX_SALV_NOT_FOUND.
*  ENDTRY.


*... set expand column
  TRY.
      LR_COLUMNS->SET_EXPAND_COLUMN( 'EXPAND' ).
    ##NO_HANDLER    CATCH CX_SALV_DATA_ERROR CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND.
  ENDTRY.

*... set items expanded
  TRY.
      LR_LEVEL = GR_HIERSEQ->GET_LEVEL( 1 ).
    ##NO_HANDLER    CATCH CX_SALV_DATA_ERROR CX_SALV_NOT_FOUND.
  ENDTRY.

  LR_LEVEL->SET_ITEMS_EXPANDED( ).

*... §4.1 set exception column
  TRY.
      LR_COLUMNS->SET_EXCEPTION_COLUMN( 'EXCEPTION' ).
    CATCH CX_SALV_DATA_ERROR.                           "#EC NO_HANDLER
  ENDTRY.


*... *** SLAVE Settings ***
  TRY.
      LR_COLUMNS = GR_HIERSEQ->GET_COLUMNS( 2 ).
    ##NO_HANDLER    CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND.
  ENDTRY.

*... optimize the columns
  LR_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ).

* Set column
*  PERFORM F_DETAIL_COLUMN_SETTINGS USING LR_COLUMNS.

*  TRY.
*      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'OBJNR' ).
*      LR_COLUMN->SET_TECHNICAL( IF_SALV_C_BOOL_SAP=>TRUE ).
*    ##NO_HANDLER    CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND.
*  ENDTRY.
  TRY.
*      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'EXCEPTION' ).
*      LR_COLUMN->SET_TECHNICAL( IF_SALV_C_BOOL_SAP=>TRUE ).
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'REF_BUKRS' ).
      LR_COLUMN->SET_TECHNICAL( IF_SALV_C_BOOL_SAP=>TRUE ).
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'REF_BELNR' ).
      LR_COLUMN->SET_TECHNICAL( IF_SALV_C_BOOL_SAP=>TRUE ).
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'REF_GJAHR' ).
      LR_COLUMN->SET_TECHNICAL( IF_SALV_C_BOOL_SAP=>TRUE ).
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'REF_AWKEY' ).
      LR_COLUMN->SET_TECHNICAL( IF_SALV_C_BOOL_SAP=>TRUE ).
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'WAERS' ).
      LR_COLUMN->SET_TECHNICAL( IF_SALV_C_BOOL_SAP=>TRUE ).
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'BUKRS' ).
      LR_COLUMN->SET_TECHNICAL( IF_SALV_C_BOOL_SAP=>TRUE ).
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'BELNR' ).
      LR_COLUMN->SET_TECHNICAL( IF_SALV_C_BOOL_SAP=>TRUE ).
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'GJAHR' ).
      LR_COLUMN->SET_TECHNICAL( IF_SALV_C_BOOL_SAP=>TRUE ).
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'REF_BUZEI' ).
      LR_COLUMN->SET_TECHNICAL( IF_SALV_C_BOOL_SAP=>TRUE ).
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'BUZEI' ).
      LR_COLUMN->SET_TECHNICAL( IF_SALV_C_BOOL_SAP=>TRUE ).
    ##NO_HANDLER    CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND.
  ENDTRY.

*... §6 register to the events of cl_salv_hierseq_table

  LR_EVENTS = GR_HIERSEQ->GET_EVENT( ).

  CREATE OBJECT GR_EVENTS.

*... §6.1 register to the event USER_COMMAND
  SET HANDLER GR_EVENTS->ON_USER_COMMAND FOR LR_EVENTS.
  SET HANDLER GR_EVENTS->ON_TOP_OF_PAGE FOR LR_EVENTS.

  TRY.
      LR_SELECTIONS = GR_HIERSEQ->GET_SELECTIONS( 1 ).
    ##NO_HANDLER   CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND.
  ENDTRY.

*... §7.1 set selection mode
  LR_SELECTIONS->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>MULTIPLE ).

*... set selected rows.
  CLEAR LT_ROWS.
  DO LINES( GT_MASTER ) TIMES.

    DATA(LV_INDEX) = SY-INDEX.

    IF SY-INDEX GT LINES( GT_MASTER ).
      EXIT.
    ENDIF.

    READ TABLE GT_MASTER INTO DATA(LS_MASTER) INDEX LV_INDEX.
    IF SY-SUBRC EQ 0 AND LS_MASTER-EXCEPTION IS NOT INITIAL.
      CONTINUE.
    ENDIF.
    APPEND LV_INDEX TO LT_ROWS.
  ENDDO.

  LR_SELECTIONS->SET_SELECTED_ROWS( VALUE = LT_ROWS ).

*  L_TITLE = TEXT-T01.
  LR_DISPLAY_SETTINGS = GR_HIERSEQ->GET_DISPLAY_SETTINGS( ).
  LR_DISPLAY_SETTINGS->SET_LIST_HEADER( L_TITLE ).
  LR_DISPLAY_SETTINGS->SET_STRIPED_PATTERN( VALUE = ABAP_TRUE ).

*... §7 display the table
  GR_HIERSEQ->DISPLAY( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_QUERY_FIELDS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- UF_SELECT_QUERY
*&---------------------------------------------------------------------*
FORM F_SET_QUERY_FIELDS  USING    VALUE(UF_STRUCTURE) TYPE DDOBJNAME
                         CHANGING UF_SELECT_QUERY TYPE STRING.

  DATA: LT_DFIES TYPE DFIES_TAB.

*..
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME        = UF_STRUCTURE
      LANGU          = SY-LANGU
    TABLES
      DFIES_TAB      = LT_DFIES
    EXCEPTIONS
      NOT_FOUND      = 1
      INTERNAL_ERROR = 2
      OTHERS         = 3.
  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT LT_DFIES ASSIGNING FIELD-SYMBOL(<L_DFIES>).
    UF_SELECT_QUERY = |{ UF_SELECT_QUERY }, A~{ <L_DFIES>-FIELDNAME }|.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_PRCTR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <L_SLAVE>_VKORG
*&      --> <L_SLAVE>_AUART
*&      --> <L_SLAVE>_VKBUR
*&      --> <L_SLAVE>_VKGRP
*&---------------------------------------------------------------------*
FORM F_SET_PRCTR  USING    UF_VKORG TYPE VKORG
                           UF_AUART TYPE AUART
                           UF_VKBUR TYPE VKBUR
                           UF_VKGRP TYPE VKGRP
                  CHANGING CH_PRCTR TYPE PRCTR.

  DATA: LS_DETER_PRCTR TYPE ZCL_SDSCM_ENHANCEMENT=>TS_DETER_PRCTR.

  CLEAR CH_PRCTR.

  LS_DETER_PRCTR-SALES_ORG_SD    = UF_VKORG.
  LS_DETER_PRCTR-PROCESS_TYPE    = UF_AUART.
  LS_DETER_PRCTR-SALES_GROUP_SD  = UF_VKGRP.
  LS_DETER_PRCTR-SALES_OFFICE_SD = UF_VKBUR.

  ZCL_SDSCM_ENHANCEMENT=>DETERMINE_PROFIT_CENTER(
    EXPORTING
      IS_COND  = LS_DETER_PRCTR
    RECEIVING
      RF_PRCTR = CH_PRCTR               " Profit Center
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_posting
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_ROWS
*&---------------------------------------------------------------------*
FORM F_POSTING TABLES UT_ROW TYPE SALV_T_ROW.

  LOOP AT UT_ROW ASSIGNING FIELD-SYMBOL(<L_ROW>).
    READ TABLE GT_MASTER ASSIGNING FIELD-SYMBOL(<L_MASTER>) INDEX <L_ROW>.
    IF SY-SUBRC EQ 0.
      IF <L_MASTER>-EXCEPTION IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      PERFORM F_DOCUMENT_POSTING CHANGING <L_MASTER>.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DOCUMENT_POSTING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <L_MASTER>
*&---------------------------------------------------------------------*
FORM F_DOCUMENT_POSTING  CHANGING CF_MASTER TYPE G_TYPE_S_MASTER.
  CLEAR:  GT_BLNTAB[], GT_FTPOST[], GT_FTTAX[] .
  PERFORM F_POSTING_INTERFACE_START.

  PERFORM F_FILL_FTPOST_HEADER USING CF_MASTER.

  LOOP AT GT_SLAVE INTO DATA(LS_SLAVE) WHERE REF_BUKRS EQ CF_MASTER-REF_BUKRS
                                        AND REF_BELNR EQ CF_MASTER-REF_BELNR
                                        AND REF_GJAHR EQ CF_MASTER-REF_GJAHR.
    PERFORM F_FILL_FTPOST_ITEM USING LS_SLAVE.
  ENDLOOP.

*>pst interface document
  PERFORM F_POSTING_INTERFACE_DOCUMENT CHANGING CF_MASTER.
  PERFORM F_POSTING_INTERFACE_END.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_POSTING_INTERFACE_START
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_POSTING_INTERFACE_START .
  CALL FUNCTION 'POSTING_INTERFACE_START'
    EXPORTING
      I_FUNCTION         = 'C'
*     I_MODE             = 'A'
    EXCEPTIONS
      CLIENT_INCORRECT   = 1
      FUNCTION_INVALID   = 2
      GROUP_NAME_MISSING = 3
      MODE_INVALID       = 4
      UPDATE_INVALID     = 5
      OTHERS             = 6.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_FILL_FTPOST_HEADER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> CF_MASTER
*&---------------------------------------------------------------------*
FORM F_FILL_FTPOST_HEADER  USING US_MASTER  TYPE G_TYPE_S_MASTER.
* Post Document: Header Data

  DATA: LF_FVALUE TYPE FTPOST-FVAL.

  GF_COUNT = GF_COUNT + 1.

  GS_FTPOST-STYPE = 'K'.
  GS_FTPOST-COUNT =  GF_COUNT.
*
  WRITE P_BUKRS TO LF_FVALUE.
  PERFORM F_FTPOST_FIELD USING 'BKPF-BUKRS' LF_FVALUE.       "Company Code
*
  WRITE P_BLART TO LF_FVALUE.
  PERFORM F_FTPOST_FIELD USING 'BKPF-BLART' LF_FVALUE.       "Doc Type
*
  WRITE SY-DATUM TO LF_FVALUE.
  PERFORM F_FTPOST_FIELD USING 'BKPF-BLDAT' LF_FVALUE.       "Document Date

  WRITE SY-DATUM TO LF_FVALUE.
  PERFORM F_FTPOST_FIELD USING 'BKPF-BUDAT' LF_FVALUE.     "postin date

*..
  WRITE US_MASTER-REF_AWKEY TO LF_FVALUE.
  PERFORM F_FTPOST_FIELD USING 'BKPF-BKTXT' LF_FVALUE.       "Header Text

*  WRITE US_HEADER-WAERS TO LF_FVALUE.
  LF_FVALUE = 'THB'.
  PERFORM F_FTPOST_FIELD USING 'BKPF-WAERS' LF_FVALUE.       "curr.

*  LF_FVALUE = |{ US_HEADER-BUKRS }{ US_HEADER-BELNR }{ US_HEADER-GJAHR }{ US_HEADER-BUZEI }|.
*  PERFORM F_FTPOST_FIELD USING 'BKPF-BKTXT' LF_FVALUE.       "header text

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FTPOST_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0434   text
*      -->P_FVALUE  text
*----------------------------------------------------------------------*
FORM F_FTPOST_FIELD USING UF_FNAM TYPE FTPOST-FNAM
                          UF_FVAL TYPE FTPOST-FVAL.

  APPEND INITIAL LINE TO GT_FTPOST ASSIGNING FIELD-SYMBOL(<L_FTPOST>).
  <L_FTPOST>-STYPE = GS_FTPOST-STYPE.
  <L_FTPOST>-COUNT = GS_FTPOST-COUNT.
  <L_FTPOST>-FNAM = UF_FNAM.
  <L_FTPOST>-FVAL = UF_FVAL.

ENDFORM.                    " F_FTPOST_FIELD
*&---------------------------------------------------------------------*
*&      Form  F_FILL_FTPOST_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_FILL_FTPOST_ITEM USING US_SLAVE  TYPE G_TYPE_S_SLAVE.
  DATA: LF_FVALUE TYPE FTPOST-FVAL.

** Post Document: Fields of first line item

  GS_FTPOST-STYPE = 'P'.                  " Line Item
  GS_FTPOST-COUNT =  US_SLAVE-BUZEI.      " Nr Line Item

  WRITE US_SLAVE-BSCHL TO LF_FVALUE.
  PERFORM F_FTPOST_FIELD USING 'RF05A-NEWBS' LF_FVALUE.        " Post Key
*
  WRITE US_SLAVE-RACCT TO LF_FVALUE LEFT-JUSTIFIED.
  PERFORM F_FTPOST_FIELD USING 'RF05A-NEWKO' LF_FVALUE.        " Account

*    LF_FVALUE = 'VX'.
*    PERFORM F_FTPOST_FIELD USING 'BSEG-MWSKZ' LF_FVALUE.    "Vat code

*
  WRITE ABS( US_SLAVE-WRBTR ) TO LF_FVALUE NO-GROUPING LEFT-JUSTIFIED.
  PERFORM F_FTPOST_FIELD USING 'BSEG-WRBTR' LF_FVALUE.         " Amount

  WRITE US_SLAVE-SGTXT TO LF_FVALUE LEFT-JUSTIFIED.
  PERFORM F_FTPOST_FIELD USING 'BSEG-SGTXT' LF_FVALUE.         " Item text

*  IF US_SLAVE-PS_POSID IS NOT INITIAL.
*    WRITE US_SLAVE-PS_POSID TO LF_FVALUE NO-GROUPING LEFT-JUSTIFIED.
*    PERFORM F_FTPOST_FIELD USING 'COBL-PS_POSID' LF_FVALUE.
*  ENDIF.

  PERFORM F_APPEND_RKE_FIELDS USING US_SLAVE.


*  IF US_SLAVE-KOSTL IS NOT INITIAL.
*    WRITE US_SLAVE-KOSTL TO LF_FVALUE NO-GROUPING LEFT-JUSTIFIED.
*    PERFORM F_FTPOST_FIELD USING 'COBL-KOSTL' LF_FVALUE.
*  ENDIF.

ENDFORM.                    " F_FILL_FTPOST_ITEM
*&---------------------------------------------------------------------*
*& Form F_POSTING_INTERFACE_DOCUMENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CF_MASTER
*&---------------------------------------------------------------------*
FORM F_POSTING_INTERFACE_DOCUMENT  CHANGING CS_MASTER TYPE G_TYPE_S_MASTER.

  DATA: LF_SUBRC      TYPE SY-SUBRC ##NEEDED,
        LF_MSGID      TYPE SY-MSGID,
        LF_MSGNO      TYPE SY-MSGNO,
        LF_MSGTY      TYPE SY-MSGTY,
        LF_MSGV1      TYPE SY-MSGV1,
        LF_MSGV2      TYPE SY-MSGV2,
        LF_MSGV3      TYPE SY-MSGV3,
        LF_MSGV4      TYPE SY-MSGV4,
        LF_LINEP(110).

  CALL FUNCTION 'POSTING_INTERFACE_DOCUMENT'
    EXPORTING
      I_TCODE                  = 'FB01'
      I_XSIMU                  = ABAP_FALSE
*     I_SGFUNCT                = ' '
*     I_NO_AUTH                = ' '
    IMPORTING
      E_SUBRC                  = LF_SUBRC
      E_MSGID                  = LF_MSGID
      E_MSGTY                  = LF_MSGTY
      E_MSGNO                  = LF_MSGNO
      E_MSGV1                  = LF_MSGV1
      E_MSGV2                  = LF_MSGV2
      E_MSGV3                  = LF_MSGV3
      E_MSGV4                  = LF_MSGV4
    TABLES
      T_BLNTAB                 = GT_BLNTAB
      T_FTPOST                 = GT_FTPOST
      T_FTTAX                  = GT_FTTAX
    EXCEPTIONS
      ACCOUNT_MISSING          = 1
      COMPANY_CODE_MISSING     = 2
      POSTING_KEY_INVALID      = 3
      POSTING_KEY_MISSING      = 4
      RECORD_TYPE_INVALID      = 5
      TRANSACTION_CODE_INVALID = 6
      AMOUNT_FORMAT_ERROR      = 7
      TOO_MANY_LINE_ITEMS      = 8
      COMPANY_CODE_INVALID     = 9
      SCREEN_NOT_FOUND         = 10
      NO_AUTHORIZATION         = 11
      OTHERS                   = 12.
  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO CS_MASTER-MSGTX.
    CS_MASTER-EXCEPTION = SWITCH CHAR01( SY-MSGTY
                      WHEN 'I' OR 'S' THEN '3'
                      WHEN 'W' THEN '2'
                      ELSE '1' ).
  ELSE.
    READ TABLE GT_BLNTAB INTO DATA(LS_BLNTAB) INDEX 1.
    IF SY-SUBRC EQ 0.
      CS_MASTER-EXCEPTION = '3'.
      CS_MASTER-BUKRS = LS_BLNTAB-BUKRS.
      CS_MASTER-GJAHR = LS_BLNTAB-GJAHR.
      CS_MASTER-BELNR = LS_BLNTAB-BELNR.
    ELSE.
      IF LF_MSGID IS NOT INITIAL.
        CALL FUNCTION 'FORMAT_MESSAGE'
          EXPORTING
            ID        = LF_MSGID
            LANG      = SY-LANGU
            NO        = LF_MSGNO
            V1        = LF_MSGV1
            V2        = LF_MSGV2
            V3        = LF_MSGV3
            V4        = LF_MSGV4
          IMPORTING
            MSG       = LF_LINEP
          EXCEPTIONS ##FM_SUBRC_OK
            NOT_FOUND = 1
            OTHERS    = 2.
        CS_MASTER-MSGTX = LF_LINEP.
        CS_MASTER-EXCEPTION = SWITCH CHAR01( LF_MSGTY
                              WHEN 'I' OR 'S' THEN '3'
                              WHEN 'W' THEN '2'
                              ELSE '1' ).
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_POSTING_INTERFACE_END
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_POSTING_INTERFACE_END .

  CALL FUNCTION 'POSTING_INTERFACE_END'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_APPEND_RKE_FIELDS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> US_SLAVE
*&---------------------------------------------------------------------*
FORM F_APPEND_RKE_FIELDS  USING  US_SLAVE TYPE G_TYPE_S_SLAVE.

  DATA: LT_COPADATA TYPE COPADATA_TAB,
        LF_FNAM     TYPE FTPOST-FNAM,
        LF_PAOBJNR  TYPE CEST1-PAOBJNR.

  LF_PAOBJNR = US_SLAVE-PAOBJNR.

  CALL FUNCTION 'RKE_CONVERT_PAOBJNR_COPADATA'
    EXPORTING
      BUKRS          = US_SLAVE-REF_BUKRS
      KOKRS          = US_SLAVE-KOKRS
      VORGN          = 'RFBU'
      PAOBJNR        = LF_PAOBJNR
      SET_PREFIX     = ABAP_TRUE
    TABLES
      I_COPADATA     = LT_COPADATA
    EXCEPTIONS
      NO_ERKRS_FOUND = 1
      PAOBJNR_WRONG  = 2
      OTHERS         = 3.
  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    RETURN.
  ELSE.

    GS_FTPOST-STYPE = 'P'.                  " Line Item
    GS_FTPOST-COUNT =  US_SLAVE-BUZEI.      " Nr Line Item

    LOOP AT LT_COPADATA INTO DATA(LS_COPADATA).
      CHECK LS_COPADATA-FVAL IS NOT INITIAL.
      LF_FNAM = |BSEG-{ LS_COPADATA-FNAM }|.
      IF LF_FNAM = 'BSEG-RKE_PRCTR'.
        LS_COPADATA-FVAL = US_SLAVE-PRCTR.
      ENDIF.
      PERFORM F_FTPOST_FIELD USING LF_FNAM  LS_COPADATA-FVAL.
    ENDLOOP.

  ENDIF.

ENDFORM.
