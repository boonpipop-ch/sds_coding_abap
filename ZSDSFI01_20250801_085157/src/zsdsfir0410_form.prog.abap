*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0410_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form F_INIT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_INIT_DATA .

ENDFORM.                    "F_INIT_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_DATA.
  SELECT ANLA~ANLN1,
         ANLA~ANLN2,
         ANLA~MCOA1,
         ANLA~TXT50,
         ANLA~INVZU,
         ANLA~ZUGDT,
         ANLZ~RAUMN,
         ANLZ~PERNR,
         ( A~VORNA && ' ' && A~NACHN ) AS OWNNA,
         ( B~VORNA && ' ' && B~NACHN ) AS MGNNA
    FROM ANLA
    INNER JOIN ANLZ ON ANLA~BUKRS EQ ANLZ~BUKRS AND
                       ANLA~ANLN1 EQ ANLZ~ANLN1 AND
                       ANLA~ANLN2 EQ ANLZ~ANLN2 AND
                       ANLZ~BDATU GE @SY-DATUM  AND
                       ANLZ~ADATU LE @SY-DATUM
    LEFT JOIN PA0002 AS A ON ANLZ~RAUMN EQ A~PERNR   AND
                             A~ENDDA    GE @SY-DATUM AND
                             A~BEGDA    LE @SY-DATUM
    LEFT JOIN PA0002 AS B ON ANLZ~PERNR EQ B~PERNR   AND
                             B~ENDDA    GE @SY-DATUM AND
                             B~BEGDA    LE @SY-DATUM
    INTO TABLE @GT_RESULT
    WHERE ANLA~ANLN1 IN @S_ANLN1[]
      AND ANLA~ANLN2 IN @S_ANLN2[]
      AND ANLA~MCOA1 IN @S_MCOA1[].

ENDFORM.                    "f_get_data
*&---------------------------------------------------------------------*
*&      Form  pf_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_ALV_GRID.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_1'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     i_html_height_top        = 12
*     I_CALLBACK_HTML_TOP_OF_PAGE       = 'HTML_TOP_OF_PAGE'
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      IS_LAYOUT                = GS_LAYOUT
      IT_FIELDCAT              = GT_FCAT
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
      IT_SORT                  = GT_SORT
*     IT_FILTER                =
*     IS_SEL_HIDE              =
      I_DEFAULT                = GC_X
      I_SAVE                   = GC_A
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT                 =
*     IS_REPREP_ID             =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        = 0
*     I_HTML_HEIGHT_END        = 0
*     IT_ALV_GRAPHICS          =
*     IT_HYPERLINK             =
*     IT_ADD_FIELDCAT          =
*     IT_EXCEPT_QINFO          =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      T_OUTTAB                 = GT_RESULT
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    "pf_alv_grid
*&---------------------------------------------------------------------*
*&      Form  html_top_of_page
*&---------------------------------------------------------------------*

FORM HTML_TOP_OF_PAGE USING DOCUMENT TYPE REF TO CL_DD_DOCUMENT.

  DATA: TEXT TYPE SDYDO_TEXT_ELEMENT.

  CALL METHOD DOCUMENT->ADD_GAP
    EXPORTING
      WIDTH = 100.
  TEXT =  'Company Code Data'.
  CALL METHOD DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT      = TEXT
      SAP_STYLE = 'HEADING'.

  CALL METHOD DOCUMENT->NEW_LINE.
  CALL METHOD DOCUMENT->NEW_LINE.
  CALL METHOD DOCUMENT->NEW_LINE.

  TEXT = 'User Name : '.
  CALL METHOD DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = TEXT
      SAP_EMPHASIS = 'Strong'.

  CALL METHOD DOCUMENT->ADD_GAP
    EXPORTING
      WIDTH = 6.

  TEXT = SY-UNAME.
  CALL METHOD DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT      = TEXT
      SAP_STYLE = 'Key'.

  CALL METHOD DOCUMENT->ADD_GAP
    EXPORTING
      WIDTH = 50.


  TEXT = 'Date : '.
  CALL METHOD DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = TEXT
      SAP_EMPHASIS = 'Strong'.

  CALL METHOD DOCUMENT->ADD_GAP
    EXPORTING
      WIDTH = 6.

  TEXT = SY-DATUM.
  CALL METHOD DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT      = TEXT
      SAP_STYLE = 'Key'.

  CALL METHOD DOCUMENT->ADD_GAP
    EXPORTING
      WIDTH = 50.

  TEXT = 'Time : '.
  CALL METHOD DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = TEXT
      SAP_EMPHASIS = 'Strong'.

  CALL METHOD DOCUMENT->ADD_GAP
    EXPORTING
      WIDTH = 6.

  TEXT = SY-UZEIT.
  CALL METHOD DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT      = TEXT
      SAP_STYLE = 'Key'.

  CALL METHOD DOCUMENT->NEW_LINE.
  CALL METHOD DOCUMENT->NEW_LINE.

ENDFORM.                    "HTML_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  set_layout_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PS_LAYOUT  text
*----------------------------------------------------------------------*
FORM SET_LAYOUT_OUTPUT." CHANGING ps_layout TYPE slis_layout_alv.
  GS_LAYOUT-BOX_FIELDNAME     = 'CHECK'.
  GS_LAYOUT-ZEBRA             = GC_X.
  GS_LAYOUT-COLWIDTH_OPTIMIZE = GC_X.

ENDFORM.                    " set_layout_output
*&---------------------------------------------------------------------*
*&      Form  build_fcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BUILD_FCAT.

  DATA:
   LS_FCAT TYPE SLIS_FIELDCAT_ALV.

**  CLEAR ls_fcat.
**  ls_fcat-ref_tabname = 'LFA1'.
**  ls_fcat-fieldname   = 'LIFNR'.
**  APPEND ls_fcat TO gt_fcat.

*  CLEAR LS_FCAT.
**  ls_fcat-ref_tabname = 'GT_RESULT'.
*  LS_FCAT-FIELDNAME   = 'CHECK'.
*  LS_FCAT-SELTEXT_S   = 'Check'.
*  LS_FCAT-SELTEXT_M   = 'Check'.
*  LS_FCAT-SELTEXT_L   = 'Check'.
*  LS_FCAT-CHECKBOX    = 'X'.
*  LS_FCAT-INPUT       = 'X'.
*  LS_FCAT-EDIT        = 'X'.
*  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = TEXT-801.
  LS_FCAT-SELTEXT_S   = TEXT-701.
  LS_FCAT-SELTEXT_M   = TEXT-701.
  LS_FCAT-SELTEXT_L   = TEXT-701.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = TEXT-802.
  LS_FCAT-SELTEXT_S   = TEXT-702.
  LS_FCAT-SELTEXT_M   = TEXT-702.
  LS_FCAT-SELTEXT_L   = TEXT-702.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = TEXT-803.
  LS_FCAT-SELTEXT_S   = TEXT-703.
  LS_FCAT-SELTEXT_M   = TEXT-703.
  LS_FCAT-SELTEXT_L   = TEXT-703.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = TEXT-804.
  LS_FCAT-SELTEXT_S   = TEXT-704.
  LS_FCAT-SELTEXT_M   = TEXT-704.
  LS_FCAT-SELTEXT_L   = TEXT-704.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = TEXT-805.
  LS_FCAT-SELTEXT_S   = TEXT-705.
  LS_FCAT-SELTEXT_M   = TEXT-705.
  LS_FCAT-SELTEXT_L   = TEXT-705.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = TEXT-806.
  LS_FCAT-SELTEXT_S   = TEXT-706.
  LS_FCAT-SELTEXT_M   = TEXT-706.
  LS_FCAT-SELTEXT_L   = TEXT-706.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = TEXT-807.
  LS_FCAT-SELTEXT_S   = TEXT-707.
  LS_FCAT-SELTEXT_M   = TEXT-707.
  LS_FCAT-SELTEXT_L   = TEXT-707.
  APPEND LS_FCAT TO GT_FCAT.
*
*  CLEAR LS_FCAT.
*  LS_FCAT-FIELDNAME   = TEXT-808.
*  LS_FCAT-SELTEXT_S   = TEXT-708.
*  LS_FCAT-SELTEXT_M   = TEXT-708.
*  LS_FCAT-SELTEXT_L   = TEXT-708.
*  APPEND LS_FCAT TO GT_FCAT.
*
*  CLEAR LS_FCAT.
*  LS_FCAT-FIELDNAME   = TEXT-809.
*  LS_FCAT-SELTEXT_S   = TEXT-709.
*  LS_FCAT-SELTEXT_M   = TEXT-709.
*  LS_FCAT-SELTEXT_L   = TEXT-709.
*  APPEND LS_FCAT TO GT_FCAT.
*
*  CLEAR LS_FCAT.
*  LS_FCAT-FIELDNAME   = TEXT-810.
*  LS_FCAT-SELTEXT_S   = TEXT-710.
*  LS_FCAT-SELTEXT_M   = TEXT-710.
*  LS_FCAT-SELTEXT_L   = TEXT-710.
*  APPEND LS_FCAT TO GT_FCAT.
*
*  CLEAR LS_FCAT.
*  LS_FCAT-FIELDNAME   = TEXT-811.
*  LS_FCAT-SELTEXT_S   = TEXT-711.
*  LS_FCAT-SELTEXT_M   = TEXT-711.
*  LS_FCAT-SELTEXT_L   = TEXT-711.
*  LS_FCAT-NO_ZERO     = GC_X.
*  APPEND LS_FCAT TO GT_FCAT.
*
*  CLEAR LS_FCAT.
*  LS_FCAT-FIELDNAME   = TEXT-812.
*  LS_FCAT-SELTEXT_S   = TEXT-712.
*  LS_FCAT-SELTEXT_M   = TEXT-712.
*  LS_FCAT-SELTEXT_L   = TEXT-712.
*  APPEND LS_FCAT TO GT_FCAT.
*
*  CLEAR LS_FCAT.
*  LS_FCAT-FIELDNAME   = TEXT-813.
*  LS_FCAT-SELTEXT_S   = TEXT-713.
*  LS_FCAT-SELTEXT_M   = TEXT-713.
*  LS_FCAT-SELTEXT_L   = TEXT-713.
*  LS_FCAT-NO_ZERO     = GC_X.
*  APPEND LS_FCAT TO GT_FCAT.
*
*  CLEAR LS_FCAT.
*  LS_FCAT-FIELDNAME   = TEXT-814.
*  LS_FCAT-SELTEXT_S   = TEXT-714.
*  LS_FCAT-SELTEXT_M   = TEXT-714.
*  LS_FCAT-SELTEXT_L   = TEXT-714.
*  APPEND LS_FCAT TO GT_FCAT.
*
*
*  CLEAR LS_FCAT.
*  LS_FCAT-FIELDNAME   = TEXT-815.
*  LS_FCAT-SELTEXT_S   = TEXT-715.
*  LS_FCAT-SELTEXT_M   = TEXT-715.
*  LS_FCAT-SELTEXT_L   = TEXT-715.
*  LS_FCAT-NO_ZERO     = GC_X.
*  APPEND LS_FCAT TO GT_FCAT.
*
*  CLEAR LS_FCAT.
*  LS_FCAT-FIELDNAME   = TEXT-816.
*  LS_FCAT-SELTEXT_S   = TEXT-716.
*  LS_FCAT-SELTEXT_M   = TEXT-716.
*  LS_FCAT-SELTEXT_L   = TEXT-716.
*  APPEND LS_FCAT TO GT_FCAT.
*
*
*  CLEAR LS_FCAT.
*  LS_FCAT-FIELDNAME   = TEXT-817.
*  LS_FCAT-SELTEXT_S   = TEXT-717.
*  LS_FCAT-SELTEXT_M   = TEXT-717.
*  LS_FCAT-SELTEXT_L   = TEXT-717.
*  LS_FCAT-NO_ZERO     = GC_X.
*  APPEND LS_FCAT TO GT_FCAT.
*
*  CLEAR LS_FCAT.
*  LS_FCAT-FIELDNAME   = TEXT-818.
*  LS_FCAT-SELTEXT_S   = TEXT-718.
*  LS_FCAT-SELTEXT_M   = TEXT-718.
*  LS_FCAT-SELTEXT_L   = TEXT-718.
*  APPEND LS_FCAT TO GT_FCAT.
*
*  CLEAR LS_FCAT.
*  LS_FCAT-FIELDNAME   = TEXT-819.
*  LS_FCAT-SELTEXT_S   = TEXT-719.
*  LS_FCAT-SELTEXT_M   = TEXT-719.
*  LS_FCAT-SELTEXT_L   = TEXT-719.
*  APPEND LS_FCAT TO GT_FCAT.
*
*  CLEAR LS_FCAT.
*  LS_FCAT-FIELDNAME   = TEXT-820.
*  LS_FCAT-SELTEXT_S   = TEXT-720.
*  LS_FCAT-SELTEXT_M   = TEXT-720.
*  LS_FCAT-SELTEXT_L   = TEXT-720.
*  APPEND LS_FCAT TO GT_FCAT.
*
*  CLEAR LS_FCAT.
*  LS_FCAT-FIELDNAME   = TEXT-821.
*  LS_FCAT-SELTEXT_S   = TEXT-721.
*  LS_FCAT-SELTEXT_M   = TEXT-721.
*  LS_FCAT-SELTEXT_L   = TEXT-721.
*  APPEND LS_FCAT TO GT_FCAT.
*
*  CLEAR LS_FCAT.
*  LS_FCAT-FIELDNAME   = TEXT-822.
*  LS_FCAT-SELTEXT_S   = TEXT-722.
*  LS_FCAT-SELTEXT_M   = TEXT-722.
*  LS_FCAT-SELTEXT_L   = TEXT-722.
*  APPEND LS_FCAT TO GT_FCAT.
*
*  CLEAR LS_FCAT.
*  LS_FCAT-FIELDNAME   = TEXT-823.
*  LS_FCAT-SELTEXT_S   = TEXT-723.
*  LS_FCAT-SELTEXT_M   = TEXT-723.
*  LS_FCAT-SELTEXT_L   = TEXT-723.
*  APPEND LS_FCAT TO GT_FCAT.

ENDFORM.                    "build_fcat_1
*&---------------------------------------------------------------------*
*&      Form  F_SHOW_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SHOW_REPORT .
  PERFORM SET_LAYOUT_OUTPUT.
  PERFORM BUILD_FCAT.
  PERFORM SET_SORT.
  PERFORM SET_ALV_GRID.
ENDFORM.                    " F_SHOW_REPORT
*&---------------------------------------------------------------------*
*&      Form  F_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_SORT .
**  CLEAR gs_sort.
**  gs_sort-fieldname = 'LIFNR'.
**  gs_sort-spos = '1'.
**  gs_sort-up = 'X'.
***  gs_sort-subtot = 'X'.
**  APPEND gs_sort TO gt_sort.
ENDFORM.                    " F_SORT
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->US_EXTAB   text
*----------------------------------------------------------------------*
FORM PF_STATUS_1 USING US_EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'ZSTANDARD' EXCLUDING US_EXTAB.

ENDFORM.                    "PF_STATUS_1
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM    text
*      -->P_SELFLD   text
*----------------------------------------------------------------------*
FORM USER_COMMAND USING P_UCOMM TYPE SY-UCOMM
                        P_SELFLD TYPE SLIS_SELFIELD.
*&---------------------------------------------------------------------*
*&for Check = 'X' when tick Check Box
*&---------------------------------------------------------------------*
  DATA : REF_GRID TYPE REF TO CL_GUI_ALV_GRID.

  IF REF_GRID IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        E_GRID = REF_GRID.
    CALL METHOD REF_GRID->CHECK_CHANGED_DATA.
  ENDIF.
*&---------------------------------------------------------------------*

  CASE P_UCOMM.
    WHEN '&IC1'.
      PERFORM F_CALL_TRANSECTION.
    WHEN 'ALL'.
      PERFORM F_CHECK_BOX USING 'X'.
    WHEN 'SAL'.
      PERFORM F_CHECK_BOX USING SPACE.
    WHEN 'PRINT'.
      PERFORM F_PRINT_QR.
  ENDCASE.

  CALL METHOD REF_GRID->REFRESH_TABLE_DISPLAY.
  CLEAR : REF_GRID.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  F_CALL_TRANSECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CALL_TRANSECTION.
  DATA: LE_ROW     TYPE I,
        LE_VALUE   TYPE C,
        LE_COL     TYPE I,
        LES_ROW_ID TYPE LVC_S_ROW,
        LES_COL_ID TYPE LVC_S_COL,
        LES_ROW_NO TYPE LVC_S_ROID.

  DATA : REF_GRID TYPE REF TO CL_GUI_ALV_GRID.
  IF REF_GRID IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        E_GRID = REF_GRID.
  ENDIF.

  CALL METHOD REF_GRID->GET_CURRENT_CELL
    IMPORTING
      E_ROW     = LE_ROW
      E_VALUE   = LE_VALUE
      E_COL     = LE_COL
      ES_ROW_ID = LES_ROW_ID
      ES_COL_ID = LES_COL_ID.

  CLEAR : BDCDATA[],MESSTAB[].

  READ TABLE GT_RESULT INTO GS_RESULT INDEX LES_ROW_ID-INDEX.
  IF SY-SUBRC = 0.


  ENDIF.

ENDFORM.                    " F_CALL_TRANSECTION
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = GC_X.
  APPEND BDCDATA.
ENDFORM.                    "BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.                    "bdc_field
*&---------------------------------------------------------------------*
*&      Form  bdc_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TCODE      text
*----------------------------------------------------------------------*
FORM BDC_TRANSACTION  USING    TCODE.
  DATA: L_MSTRING(480).
  DATA: L_SUBRC LIKE SY-SUBRC.
  DATA: LV_OPT  TYPE CTU_PARAMS.

  LV_OPT-DISMODE  = GC_E."'A'
  LV_OPT-UPDMODE  = GC_L.
  LV_OPT-NOBINPT  = GC_X.
  "lv_opt-cattmode = 'A'.
  "lv_opt-defsize  = 'X'.

  CALL TRANSACTION TCODE USING BDCDATA
*                   MODE   'E'"N
*                   UPDATE 'L'
                   OPTIONS FROM LV_OPT
                   MESSAGES INTO MESSTAB.

ENDFORM.                    "bdc_transaction
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_BOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0645   text
*----------------------------------------------------------------------*
FORM F_CHECK_BOX  USING LV_CHECK.
  GS_RESULT-CHECK = LV_CHECK.
  MODIFY GT_RESULT FROM GS_RESULT TRANSPORTING CHECK
                                         WHERE CHECK NE GS_RESULT-CHECK.

ENDFORM.                    " F_CHECK_BOX
*&---------------------------------------------------------------------*
*&      Form  GET_ADDTIONAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ADDTIONAL_DATA.
  FIELD-SYMBOLS <LFS_RESULT> LIKE LINE OF GT_RESULT.

  DATA: BEGIN OF LS_PERNR,
          PERNR TYPE PA0002-PERNR,
        END OF LS_PERNR.
  DATA: LT_PERNR LIKE HASHED TABLE OF LS_PERNR
                      WITH UNIQUE KEY PERNR.

  DATA: LV_PERNR TYPE PA0002-PERNR.

  LT_PERNR =  CORRESPONDING #( BASE (  LT_PERNR )
                                      GT_RESULT  DISCARDING DUPLICATES
                                                 MAPPING PERNR = RAUMN ).

  SELECT A~PERNR,
         ( VORNA && ' ' && NACHN ) AS OWNNA
    FROM @LT_PERNR AS A
    INNER JOIN PA0002 ON A~PERNR EQ PA0002~PERNR
    INTO TABLE @DATA(LT_EMP).

  LOOP AT GT_RESULT ASSIGNING <LFS_RESULT>.
    IF <LFS_RESULT>-OWNNA IS INITIAL.
      LV_PERNR = <LFS_RESULT>-RAUMN.
      LV_PERNR = |{ LV_PERNR ALPHA = IN }|.
      <LFS_RESULT>-RAUMN = LV_PERNR.
      READ TABLE LT_EMP INTO DATA(LS_EMP)
      WITH KEY PERNR = LV_PERNR.
      IF SY-SUBRC EQ 0.
        <LFS_RESULT>-OWNNA = LS_EMP-OWNNA.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_ADDTIONAL_DATA
*&---------------------------------------------------------------------*
*&      Form  F_PRINT_QR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_PRINT_QR.
  DATA: FM_NAME TYPE RS38L_FNAM.

  DATA : BEGIN OF LS_QR,
           QR1   TYPE ANLA-ANLN1,
           SUB1  TYPE ANLA-ANLN2,
           NAME  TYPE STRING,
           INVZU TYPE ANLA-INVZU,
           ZUGDT TYPE ANLA-ZUGDT,
         END OF LS_QR.
  DATA LT_QR LIKE TABLE OF LS_QR.

  DATA LV_CHK_QR TYPE I.

  DATA : LS_RESULT LIKE GS_RESULT.

*  LOOP AT GT_RESULT INTO LS_RESULT WHERE CHECK EQ 'X'.
*    MOVE-CORRESPONDING LS_RESULT TO GS_RESULT.
*    ADD 1 TO LV_CHK_QR.
*
*    IF LV_CHK_QR EQ 1.
*      LS_QR-QR1  = GS_RESULT-ANLN1.
*      LS_QR-SUB1 = GS_RESULT-ANLN2.
*    ENDIF.
*
*    AT LAST.
*      IF LV_CHK_QR NE 2.
*        LS_QR-QR1  = GS_RESULT-ANLN1.
*        LS_QR-SUB1 = GS_RESULT-ANLN2.
*        APPEND LS_QR TO LT_QR.
*        EXIT.
*      ENDIF.
*    ENDAT.
*
*    IF LV_CHK_QR EQ 2.
*      LS_QR-QR2  = GS_RESULT-ANLN1.
*      LS_QR-SUB2 = GS_RESULT-ANLN2.
*      APPEND LS_QR TO LT_QR.
*      CLEAR : LS_QR,LV_CHK_QR.
*    ENDIF.
*  ENDLOOP.

  LOOP AT GT_RESULT INTO LS_RESULT WHERE CHECK EQ 'X'.
    MOVE-CORRESPONDING LS_RESULT TO GS_RESULT.
    LS_QR-QR1   = GS_RESULT-ANLN1.
    LS_QR-SUB1  = GS_RESULT-ANLN2.
    LS_QR-NAME  = GS_RESULT-TXT50.
    LS_QR-INVZU = GS_RESULT-INVZU.
    LS_QR-ZUGDT = GS_RESULT-ZUGDT.
    APPEND LS_QR TO LT_QR.
  ENDLOOP.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = 'ZSDSFI011'
    IMPORTING
      FM_NAME            = FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  DATA CP TYPE SSFCTRLOP.           "CONTROL PARAMETERS
  DATA LV_COUNT  TYPE I.


  DESCRIBE TABLE LT_QR LINES LV_COUNT.

  LOOP AT LT_QR INTO LS_QR.
    CLEAR : GS_DATA.
    IF SY-TABIX = 1.               "fisrt call
      CP-NO_OPEN   = SPACE .
      CP-NO_CLOSE  = 'X' .
    ELSEIF SY-TABIX = LV_COUNT.    "last call
      CP-NO_OPEN   = 'X' .
      CP-NO_CLOSE  = SPACE .
    ELSE.                          "other calls
      CP-NO_OPEN   = 'X' .
      CP-NO_CLOSE  = 'X' .
    ENDIF.

    GS_DATA-ASSET_NO      = |{ LS_QR-QR1 ALPHA = OUT }|.
    GS_DATA-ASSET_SUB     = |{ LS_QR-SUB1 ALPHA = OUT }|.
    GS_DATA-ASSET_NAME    = LS_QR-NAME.
    GS_DATA-OLD_ASSET_NUM = LS_QR-INVZU.
    WRITE LS_QR-ZUGDT TO GS_DATA-ACQUISITION_DATE.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN GS_DATA-ASSET_SUB WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN GS_DATA-ASSET_NO  WITH ''.

    PERFORM F_GEN_DATA_QR USING LS_QR.
*    PERFORM F_GEN_DATA_QR2.

*    AT LAST.
*      IF LV_CHK_QR NE 2.
*        LV_CHK_QR = 2.
*        CLEAR : GV_FIX2.
*      ENDIF.
*    ENDAT.

    CALL FUNCTION FM_NAME
      EXPORTING
        CONTROL_PARAMETERS = CP
      EXCEPTIONS
        FORMATTING_ERROR   = 1
        INTERNAL_ERROR     = 2
        SEND_ERROR         = 3
        USER_CANCELED      = 4
        OTHERS             = 5.


  ENDLOOP.

  IF LV_COUNT = 1.
    CALL FUNCTION 'SSF_CLOSE'.
  ENDIF.

ENDFORM.                    " F_PRINT_QR
*&---------------------------------------------------------------------*
*&      Form  f_get_data_form
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_GET_DATA_FORM CHANGING LS_DATA TYPE ZSDSFIS137.
*  LV_FIX1      = GV_FIX1.
*  LV_FIX2      = GV_FIX2.
*  LV_TEXT_FIX1 = GV_TEXT_FIX1.
*  LV_TEXT_FIX2 = GV_TEXT_FIX2.

  LS_DATA = GS_DATA.



*  LV_FIX1 = 'test1231'.
*  LV_FIX2 = 'test1232'.


ENDFORM.                    "f_get_data_form
*&---------------------------------------------------------------------*
*&      Form  F_GEN_DATA_QR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GEN_DATA_QR1.

  DATA : LV_URL TYPE STRING VALUE 'http://e-approve.daikin.com/Runtime/Runtime/Form/FixAssetInfo/'.
  IF GS_RESULT-ANLN1 IS NOT INITIAL.
    CONCATENATE LV_URL '?'
                'P_ASSET='
                GS_RESULT-ANLN1
                '&'
                'P_SUBASSET='
                GS_RESULT-ANLN2
           INTO LV_URL.

    CONCATENATE GS_RESULT-ANLN1 GS_RESULT-ANLN2 INTO GV_TEXT_FIX1 SEPARATED BY SPACE.

    GV_FIX1 = LV_URL.
  ELSE.
    CLEAR : GV_FIX1.
  ENDIF.

ENDFORM.                    " F_GEN_DATA_QR
*&---------------------------------------------------------------------*
*&      Form  F_GEN_DATA_QR2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GEN_DATA_QR2 .
  DATA : LV_URL TYPE STRING VALUE 'http://e-approve.daikin.com/Runtime/Runtime/Form/FixAssetInfo/'.

  IF GS_RESULT-ANLN1 IS NOT INITIAL.
    CONCATENATE LV_URL '?'
                'P_ASSET='
                GS_RESULT-ANLN1
                '&'
                'P_SUBASSET='
                GS_RESULT-ANLN2
           INTO LV_URL.

    CONCATENATE GS_RESULT-ANLN1 GS_RESULT-ANLN2 INTO GV_TEXT_FIX2 SEPARATED BY SPACE.

    GV_FIX2 = LV_URL.
  ELSE.
    CLEAR : GV_FIX2 .
  ENDIF.
ENDFORM.                    " F_GEN_DATA_QR2
*&---------------------------------------------------------------------*
*&      Form  F_GEN_DATA_QR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_QR  text
*----------------------------------------------------------------------*
FORM F_GEN_DATA_QR  USING  FS_QR.

  DATA : BEGIN OF LS_DATA,
           FIX_ASSET_NO  TYPE STRING,
           FIX_ASSET_SUB TYPE STRING,
*           FIX_ASSET_DESC TYPE STRING,
         END OF LS_DATA.

  DATA : BEGIN OF LS_QR,
           QR1   TYPE ANLA-ANLN1,
           SUB1  TYPE ANLA-ANLN2,
           NAME  TYPE STRING,
           INVZU TYPE ANLA-INVZU,
           ZUGDT TYPE ANLA-ZUGDT,
         END OF LS_QR.

  LS_QR = FS_QR.

  DATA : LV_FIX TYPE C LENGTH 50,
         LV_SUB TYPE C LENGTH 50.

  LV_FIX = |{ LS_QR-QR1  ALPHA = OUT }|.
  LV_SUB = |{ LS_QR-SUB1 ALPHA = OUT }|.

  REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_FIX WITH ''.
  REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_SUB WITH ''.

  CONCATENATE LV_FIX LV_SUB INTO GV_TEXT_FIX1 SEPARATED BY SPACE.

  LS_DATA-FIX_ASSET_NO   = LS_QR-QR1.
  LS_DATA-FIX_ASSET_SUB  = LS_QR-SUB1.
*  LS_DATA-FIX_ASSET_DESC = LS_QR-NAME.

  CALL METHOD /UI2/CL_JSON=>SERIALIZE
    EXPORTING
      DATA        = LS_DATA
      PRETTY_NAME = /UI2/CL_JSON=>PRETTY_MODE-CAMEL_CASE
*     assoc_arrays = abap_true
    RECEIVING
      R_JSON      = GS_DATA-QR_CODE
    EXCEPTIONS
      OTHERS      = 1.

*  DATA : LV_URL1 TYPE STRING VALUE 'http://e-approve.daikin.com/Runtime/Runtime/Form/FixAssetInfo/',
*         LV_URL2 TYPE STRING VALUE 'http://e-approve.daikin.com/Runtime/Runtime/Form/FixAssetInfo/'.
*
*  DATA : BEGIN OF LS_QR,
*           QR1  TYPE ANLA-ANLN1,
*           SUB1 TYPE ANLA-ANLN2,
*           QR2  TYPE ANLA-ANLN1,
*           SUB2 TYPE ANLA-ANLN2,
*         END OF LS_QR.
*
*  LS_QR = FS_QR.
*
*  IF LS_QR-QR1 IS NOT INITIAL.
*    CONCATENATE LV_URL1 '?'
*                'P_ASSET='
*                LS_QR-QR1
*                '&'
*                'P_SUBASSET='
*                LS_QR-SUB1
*           INTO LV_URL1.
*
*    CONCATENATE LS_QR-QR1 LS_QR-SUB1 INTO GV_TEXT_FIX1 SEPARATED BY SPACE.
*
*    GV_FIX1 = LV_URL1.
*  ELSE.
*    CLEAR : GV_FIX1,GV_TEXT_FIX1.
*  ENDIF.
*
*  IF LS_QR-QR2 IS NOT INITIAL.
*    CONCATENATE LV_URL2 '?'
*                'P_ASSET='
*                LS_QR-QR2
*                '&'
*                'P_SUBASSET='
*                LS_QR-SUB2
*           INTO LV_URL2.
*
*    CONCATENATE LS_QR-QR2 LS_QR-SUB2 INTO GV_TEXT_FIX2 SEPARATED BY SPACE.
*
*    GV_FIX2 = LV_URL2.
*  ELSE.
*    CLEAR : GV_FIX2,GV_TEXT_FIX2.
*  ENDIF.

ENDFORM.                    " F_GEN_DATA_QR
