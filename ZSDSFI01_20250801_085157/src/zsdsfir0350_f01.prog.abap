*&---------------------------------------------------------------------*
*& Include ZSDSFIR0350_F01
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
 FORM USER_OK_TC USING    P_TC_NAME TYPE DYNFNAM
                          P_TABLE_NAME ##PERF_NO_TYPE
                          P_MARK_NAME  ##PERF_NO_TYPE
                 CHANGING P_OK      LIKE SY-UCOMM.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA: L_OK     TYPE SY-UCOMM,
         L_OFFSET TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
   SEARCH P_OK FOR P_TC_NAME.
   IF SY-SUBRC <> 0.
     RETURN.
   ENDIF.
   L_OFFSET = STRLEN( P_TC_NAME ) + 1.
   L_OK = P_OK+L_OFFSET.
*&SPWIZARD: execute general and TC specific operations                 *
   CASE L_OK.
     WHEN 'INSR'.                      "insert row
       PERFORM FCODE_INSERT_ROW USING    P_TC_NAME
                                         P_TABLE_NAME.
       CLEAR P_OK.

     WHEN 'DELE'.                      "delete row
       PERFORM FCODE_DELETE_ROW USING    P_TC_NAME
                                         P_TABLE_NAME
                                         P_MARK_NAME.
       CLEAR P_OK.

     WHEN 'P--' OR                     "top of list
          'P-'  OR                     "previous page
          'P+'  OR                     "next page
          'P++'.                       "bottom of list
       PERFORM COMPUTE_SCROLLING_IN_TC USING P_TC_NAME
                                             L_OK.
       CLEAR P_OK.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
     WHEN 'MARK'.                      "mark all filled lines
       PERFORM FCODE_TC_MARK_LINES USING P_TC_NAME
                                         P_TABLE_NAME
                                         P_MARK_NAME   .
       CLEAR P_OK.

     WHEN 'DMRK'.                      "demark all filled lines
       PERFORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                           P_TABLE_NAME
                                           P_MARK_NAME .
       CLEAR P_OK.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

   ENDCASE.

 ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
 FORM FCODE_INSERT_ROW
               USING    P_TC_NAME    TYPE DYNFNAM
                        P_TABLE_NAME  ##PERF_NO_TYPE.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA L_LINES_NAME       TYPE FELD-NAME.
   DATA L_SELLINE          TYPE SY-STEPL.
   DATA L_LASTLINE         TYPE I ##NEEDED.
   DATA L_LINE             TYPE I.
   DATA L_TABLE_NAME       TYPE FELD-NAME.
   FIELD-SYMBOLS <TC>                 TYPE CXTAB_CONTROL.
   FIELD-SYMBOLS <TABLE>              TYPE STANDARD TABLE.
   FIELD-SYMBOLS <LINES>              TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
   CONCATENATE 'G_' P_TC_NAME '_LINES' INTO L_LINES_NAME.
   ASSIGN (L_LINES_NAME) TO <LINES>.

*&SPWIZARD: get current line                                           *
   GET CURSOR LINE L_SELLINE.
   IF SY-SUBRC <> 0.                   " append line to table
     L_SELLINE = <TC>-LINES + 1.
*&SPWIZARD: set top line                                               *
     IF L_SELLINE > <LINES>.
       <TC>-TOP_LINE = L_SELLINE - <LINES> + 1 .
     ELSE.
       <TC>-TOP_LINE = 1.
     ENDIF.
   ELSE.                               " insert line into table
     L_SELLINE = <TC>-TOP_LINE + L_SELLINE - 1.
     L_LASTLINE = <TC>-TOP_LINE + <LINES> - 1.
   ENDIF.
*&SPWIZARD: set new cursor line                                        *
   L_LINE = L_SELLINE - <TC>-TOP_LINE + 1.

*&SPWIZARD: insert initial line                                        *
   INSERT INITIAL LINE INTO <TABLE> INDEX L_SELLINE.
   <TC>-LINES = <TC>-LINES + 1.
*&SPWIZARD: set cursor                                                 *
   SET CURSOR 1 L_LINE.

 ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
 FORM FCODE_DELETE_ROW  USING  P_TC_NAME           TYPE DYNFNAM
                        P_TABLE_NAME  ##PERF_NO_TYPE
                        P_MARK_NAME   ##PERF_NO_TYPE.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA L_TABLE_NAME       TYPE FELD-NAME.

   FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
   FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
   FIELD-SYMBOLS <WA> TYPE ANY.
   FIELD-SYMBOLS <MARK_FIELD> TYPE ANY.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
   DESCRIBE TABLE <TABLE> LINES <TC>-LINES.

   LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

     IF <MARK_FIELD> = 'X'.
       DELETE <TABLE> INDEX SYST-TABIX.
       IF SY-SUBRC = 0.
         <TC>-LINES = <TC>-LINES - 1.
       ENDIF.
     ENDIF.
   ENDLOOP.

 ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
 FORM COMPUTE_SCROLLING_IN_TC USING    P_TC_NAME ##PERF_NO_TYPE
                                       P_OK      ##PERF_NO_TYPE.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA L_TC_NEW_TOP_LINE     TYPE I.
   DATA L_TC_NAME             TYPE FELD-NAME.
   DATA L_TC_LINES_NAME       TYPE FELD-NAME.
   DATA L_TC_FIELD_NAME       TYPE FELD-NAME.

   FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
   FIELD-SYMBOLS <LINES>      TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.
*&SPWIZARD: get looplines of TableControl                              *
   CONCATENATE 'G_' P_TC_NAME '_LINES' INTO L_TC_LINES_NAME.
   ASSIGN (L_TC_LINES_NAME) TO <LINES>.


*&SPWIZARD: is no line filled?                                         *
   IF <TC>-LINES = 0.
*&SPWIZARD: yes, ...                                                   *
     L_TC_NEW_TOP_LINE = 1.
   ELSE.
*&SPWIZARD: no, ...                                                    *
     CALL FUNCTION 'SCROLLING_IN_TABLE'
       EXPORTING
         ENTRY_ACT      = <TC>-TOP_LINE
         ENTRY_FROM     = 1
         ENTRY_TO       = <TC>-LINES
         LAST_PAGE_FULL = 'X'
         LOOPS          = <LINES>
         OK_CODE        = P_OK
         OVERLAPPING    = 'X'
       IMPORTING
         ENTRY_NEW      = L_TC_NEW_TOP_LINE
       EXCEPTIONS
*        NO_ENTRY_OR_PAGE_ACT  = 01
*        NO_ENTRY_TO    = 02
*        NO_OK_CODE_OR_PAGE_GO = 03
         OTHERS         = 0.
   ENDIF.

*&SPWIZARD: get actual tc and column                                   *
   GET CURSOR FIELD L_TC_FIELD_NAME
              AREA  L_TC_NAME.

   IF SYST-SUBRC = 0.
     IF L_TC_NAME = P_TC_NAME.
*&SPWIZARD: et actual column                                           *
       SET CURSOR FIELD L_TC_FIELD_NAME LINE 1.
     ENDIF.
   ENDIF.

*&SPWIZARD: set the new top line                                       *
   <TC>-TOP_LINE = L_TC_NEW_TOP_LINE.


 ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
 FORM FCODE_TC_MARK_LINES USING P_TC_NAME    ##PERF_NO_TYPE
                                P_TABLE_NAME ##PERF_NO_TYPE
                                P_MARK_NAME  ##PERF_NO_TYPE.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
   DATA L_TABLE_NAME       TYPE FELD-NAME.

   FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
   FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
   FIELD-SYMBOLS <WA> TYPE ANY.
   FIELD-SYMBOLS <MARK_FIELD> TYPE ANY.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
   LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

     <MARK_FIELD> = 'X'.
   ENDLOOP.
 ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
 FORM FCODE_TC_DEMARK_LINES USING P_TC_NAME    ##PERF_NO_TYPE
                                  P_TABLE_NAME ##PERF_NO_TYPE
                                  P_MARK_NAME  ##PERF_NO_TYPE.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA L_TABLE_NAME       TYPE FELD-NAME.

   FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
   FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
   FIELD-SYMBOLS <WA> TYPE ANY.
   FIELD-SYMBOLS <MARK_FIELD> TYPE ANY.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
   LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

     <MARK_FIELD> = SPACE.
   ENDLOOP.

 ENDFORM.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*& Form f_maintain_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM F_MAINTAIN_DATA .

   DATA: LR_SELECTIONS       TYPE REF TO CL_SALV_SELECTIONS,
         LR_EVENTS           TYPE REF TO CL_SALV_EVENTS_TABLE,
         LR_CONTENT          TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
         LR_LABEL            TYPE REF TO CL_SALV_FORM_LABEL,
         LR_DISPLAY_SETTINGS TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
         LR_COLUMNS          TYPE REF TO CL_SALV_COLUMNS_TABLE,
         LR_COLUMN           TYPE REF TO CL_SALV_COLUMN,
         LR_TEXT             TYPE REF TO CL_SALV_FORM_TEXT ##NEEDED,
         LS_LAYOUT           TYPE REF TO CL_SALV_LAYOUT,
         LS_KEY              TYPE SALV_S_LAYOUT_KEY.

   CHECK RB_FC1 EQ ABAP_TRUE.

   GF_MODE   = GC_CRET.
   GF_HEADER = TEXT-S03.

   SELECT BSEG~BUKRS, BSEG~BELNR, BSEG~GJAHR, BSEG~BUZEI, BKPF~BLDAT,
          BKPF~BUDAT, BKPF~BKTXT, BKPF~XBLNR, BKPF~WAERS, BSEG~WRBTR,
          BSEG~HKONT, BSEG~SGTXT,
          BSEG~PRCTR,   "CH01+
          SKAT~TXT20
    INTO CORRESPONDING FIELDS OF TABLE @GT_MAIN ##TOO_MANY_ITAB_FIELDS
       FROM BSEG INNER JOIN BKPF
     ON  BSEG~BUKRS EQ BKPF~BUKRS
     AND BSEG~BELNR EQ BKPF~BELNR
     AND BSEG~GJAHR EQ BKPF~GJAHR
     LEFT OUTER JOIN SKAT                              "#EC CI_BUFFJOIN
       ON BSEG~HKONT EQ SKAT~SAKNR
     WHERE BSEG~BUKRS EQ @P_BUKRS
       AND BSEG~GJAHR IN @S_GJAHR
       AND BSEG~HKONT IN @S_HKONT
       AND BSEG~BELNR IN @S_BELNR
       AND BSEG~SHKZG EQ 'S'
       AND BKPF~STBLG  EQ @SPACE
       AND BSEG~KOART EQ 'S'
       AND SKAT~SPRAS EQ @SY-LANGU
       AND SKAT~KTOPL EQ 'RCOA'
       AND BKPF~BUDAT IN @S_SELEC
       AND NOT EXISTS ( SELECT BELNR
                          FROM ZSDSFIT063 AS A
                         WHERE A~BUKRS EQ BKPF~BUKRS
                           AND A~BELNR EQ BKPF~BELNR
                           AND A~GJAHR EQ BKPF~GJAHR )
   ORDER BY BSEG~BUKRS, BSEG~BELNR, BSEG~GJAHR, BSEG~BUZEI,
   BKPF~BLDAT, BKPF~BUDAT.

   IF SY-SUBRC NE 0.
     MESSAGE 'No data found'(003) TYPE 'S' DISPLAY LIKE 'E'.
     LEAVE LIST-PROCESSING.
   ENDIF.

*..enable status field
   LOOP AT GT_MAIN ASSIGNING FIELD-SYMBOL(<L_MAIN>).
     PERFORM F_FIDOCUMENT_PROCEEDED USING <L_MAIN>-BUKRS
                                          <L_MAIN>-BELNR
                                          <L_MAIN>-GJAHR
                                          <L_MAIN>-BUZEI
                                          ABAP_FALSE   "msg alert
                                    CHANGING <L_MAIN>-GUID <L_MAIN>-STATUS_ICON.

   ENDLOOP.

   TRY.
       CALL METHOD CL_SALV_TABLE=>FACTORY
         IMPORTING
           R_SALV_TABLE = GR_TABLE
         CHANGING
           T_TABLE      = GT_MAIN.
     CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND. "#EC NO_HANDLER
       RETURN.
   ENDTRY.
*
   GR_TABLE->SET_SCREEN_STATUS(
     PFSTATUS      = 'MAIN'
     REPORT        = SY-REPID
     SET_FUNCTIONS = GR_TABLE->C_FUNCTIONS_ALL ).

   LR_COLUMNS = GR_TABLE->GET_COLUMNS( ).
   LR_COLUMNS->SET_OPTIMIZE( GC_TRUE ).

   TRY.
       LR_COLUMN = LR_COLUMNS->GET_COLUMN( 'GUID' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
       LR_COLUMN = LR_COLUMNS->GET_COLUMN( 'BEGDA' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
       LR_COLUMN = LR_COLUMNS->GET_COLUMN( 'ENDDA' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
       LR_COLUMN = LR_COLUMNS->GET_COLUMN( 'UNAME' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
       LR_COLUMN = LR_COLUMNS->GET_COLUMN( 'UDATE' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
       LR_COLUMN = LR_COLUMNS->GET_COLUMN( 'UTIME' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
       LR_COLUMN = LR_COLUMNS->GET_COLUMN( 'METHOD' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
       LR_COLUMN = LR_COLUMNS->GET_COLUMN( 'POPER' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
     CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND. "#EC NO_HANDLER
   ENDTRY.

   LR_SELECTIONS = GR_TABLE->GET_SELECTIONS( ).
   LR_SELECTIONS->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>ROW_COLUMN ).

   LR_EVENTS = GR_TABLE->GET_EVENT( ).

   CREATE OBJECT GR_EVENTS.

   SET HANDLER GR_EVENTS->ON_USER_COMMAND FOR LR_EVENTS.

* Top_of_list
   CREATE OBJECT LR_CONTENT.
   LR_LABEL = LR_CONTENT->CREATE_LABEL(
     ROW    = 1
     COLUMN = 1
     TEXT   = TEXT-S03 ).

   LR_LABEL->SET_LABEL_FOR( LR_TEXT ).
   GR_TABLE->SET_TOP_OF_LIST( LR_CONTENT ).

   LR_DISPLAY_SETTINGS = GR_TABLE->GET_DISPLAY_SETTINGS( ).
   LR_DISPLAY_SETTINGS->SET_STRIPED_PATTERN( VALUE = ABAP_TRUE ).

   LS_KEY-REPORT =  SY-CPROG.
   LS_KEY-HANDLE = '1000'.

   LS_LAYOUT = GR_TABLE->GET_LAYOUT( ).
   LS_LAYOUT->SET_KEY( LS_KEY ).
   LS_LAYOUT->SET_SAVE_RESTRICTION( IF_SALV_C_LAYOUT=>RESTRICT_NONE ).
   LS_LAYOUT->SET_DEFAULT( 'X' ).
   LS_LAYOUT->SET_INITIAL_LAYOUT( P_VAR ).


*... Display table
   GR_TABLE->DISPLAY( ).

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_fidocument_enqueue
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_MAIN_BUKRS
*&      --> LS_MAIN_BELNR
*&      --> LS_MAIN_GJAHR
*&      --> LS_MAIN_BUZEI
*&---------------------------------------------------------------------*
 FORM F_FIDOCUMENT_ENQUEUE  USING    UF_BUKRS TYPE BKPF-BUKRS
                                     UF_BELNR TYPE BKPF-BELNR
                                     UF_GJAHR TYPE BKPF-GJAHR.

   CALL FUNCTION 'ENQUEUE_EFBKPF'
     EXPORTING
*      MODE_BKPF      = 'E'
*      MANDT          = SY-MANDT
       BUKRS          = UF_BUKRS
       BELNR          = UF_BELNR
       GJAHR          = UF_GJAHR
*      X_BUKRS        = ' '
*      X_BELNR        = ' '
*      X_GJAHR        = ' '
*      _SCOPE         = '2'
*      _WAIT          = ' '
*      _COLLECT       = ' '
     EXCEPTIONS
       FOREIGN_LOCK   = 1
       SYSTEM_FAILURE = 2
       OTHERS         = 3.
   IF SY-SUBRC <> 0.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_fidocument_proceeded
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_MAIN_BUKRS
*&      --> LS_MAIN_BELNR
*&      --> LS_MAIN_GJAHR
*&      --> LS_MAIN_BUZEI
*&      <-- LS_MAIN_GUID
*&---------------------------------------------------------------------*
 FORM F_FIDOCUMENT_PROCEEDED USING UF_BUKRS TYPE BKPF-BUKRS
                                   UF_BELNR TYPE BKPF-BELNR
                                   UF_GJAHR TYPE BKPF-GJAHR
                                   UF_BUZEI TYPE BSEG-BUZEI
                                   UF_ISSUE_MSG TYPE ABAP_BOOLEAN
                              CHANGING CF_GUID TYPE /SEHS/BAE_UUID_32
                                       CF_STATUS_ICON TYPE ICON_NAME.

   CLEAR: CF_GUID, CF_STATUS_ICON.
   FREE: GT_MAINITEM, GT_ACCA, GT_POSTITEM.

   SELECT GUID INTO @DATA(LF_HEADER_GUID)
     FROM ZSDSFIT023 UP TO 1 ROWS
     WHERE BUKRS EQ @UF_BUKRS
       AND BELNR EQ @UF_BELNR
       AND GJAHR EQ @UF_GJAHR
       AND BUZEI EQ @UF_BUZEI ORDER BY PRIMARY KEY.
   ENDSELECT.
   IF SY-SUBRC EQ 0.
     SELECT GUID INTO @DATA(LF_GUID) FROM ZSDSFIT026 UP TO 1 ROWS ##NEEDED
       WHERE GUID EQ @LF_HEADER_GUID
         AND BUKRS_P NE ''.
     ENDSELECT.
     IF SY-SUBRC EQ 0. "data already posted, change not possible

       IF UF_ISSUE_MSG EQ ABAP_FALSE.
*...check completed document
         SELECT GUID INTO  LF_GUID FROM ZSDSFIT026 UP TO 1 ROWS ##NEEDED
                WHERE GUID EQ LF_HEADER_GUID
                  AND BUKRS_P EQ ''.
         ENDSELECT.
         IF SY-SUBRC EQ 0."COMPLETED
           CF_STATUS_ICON = ICON_LED_YELLOW.
         ELSE.

           CF_STATUS_ICON = ICON_LED_GREEN.
         ENDIF.
         RETURN.
       ENDIF.

       MESSAGE TEXT-E04 TYPE 'S' DISPLAY LIKE 'E'.
       GF_MODE = GC_DISP.
       CF_GUID = LF_HEADER_GUID.
     ELSE.
       IF UF_ISSUE_MSG EQ ABAP_FALSE.
         CF_STATUS_ICON = ICON_LED_RED.
         RETURN.
       ENDIF.
       MESSAGE TEXT-W01 TYPE 'S'.
       GF_MODE = GC_EDIT.
       CF_GUID = LF_HEADER_GUID.
     ENDIF.
   ELSE.
     IF UF_ISSUE_MSG EQ ABAP_FALSE.
       CF_STATUS_ICON = ICON_LED_INACTIVE.
       RETURN.
     ENDIF.
     GF_MODE = GC_CRET.
   ENDIF.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_document_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_MAIN_GUID
*&---------------------------------------------------------------------*
 FORM F_GET_DOCUMENT_DATA  USING  UF_GUID TYPE /SEHS/BAE_UUID_32.

   DATA: LS_HEADER TYPE  ZSDSFIT023.

   FREE: GT_MAINITEM, GT_ACCA, GT_POSTITEM.

   SELECT GUID, BUKRS, BELNR, GJAHR, BUZEI, BLDAT, BUDAT, BKTXT, XBLNR,
          WAERS, WRBTR, HKONT, POPER, BEGDA, ENDDA, METHOD, UNAME, UDATE, UTIME
     INTO CORRESPONDING FIELDS OF @LS_HEADER
     FROM ZSDSFIT023 UP TO 1 ROWS
     WHERE GUID EQ @UF_GUID ORDER BY PRIMARY KEY.
   ENDSELECT.

   SELECT GUID, RUNNO, BUKRS, BELNR, GJAHR, BUZEI, BLDAT,
          BUDAT, BKTXT, XBLNR, WAERS, WRBTR, HKONT_DR, HKONT_CR
     FROM ZSDSFIT024 INTO TABLE @DATA(LT_ITEM)
     WHERE GUID EQ @UF_GUID ORDER BY PRIMARY KEY.

   SELECT GUID, RUNNO, KOSTL, ALLKT, AUFNR, PS_POSID, PRCTR
     FROM ZSDSFIT025 INTO TABLE @DATA(LT_ACCA)
     WHERE GUID EQ @UF_GUID ORDER BY PRIMARY KEY.

   SELECT GUID, RUNNO, ITMNO, BUKRS, BELNR, GJAHR, BUZEI, BLDAT,
          BUDAT, BKTXT, XBLNR, KOSTL, AUFNR, PS_POSID, PRCTR, WAERS, WRBTR, HKONT_DR, HKONT_CR,
          BUKRS_P, BELNR_P, GJAHR_P, STATUS, MSGTX, UNAME, UDATE, UTIME
     FROM ZSDSFIT026 INTO TABLE @DATA(LT_POSTITEM)
     WHERE GUID EQ @UF_GUID ORDER BY PRIMARY KEY.

*..Set paramter
   MOVE-CORRESPONDING LS_HEADER TO ZSDSFIS074.
   GT_MAINITEM = CORRESPONDING #( LT_ITEM ).
   GT_ACCA     = CORRESPONDING #( LT_ACCA ).
   GT_POSTITEM = CORRESPONDING #( LT_POSTITEM ).

   PERFORM F_GET_ACC_DESC USING ZSDSFIS074-HKONT
                          CHANGING ZSDSFIS074-TXT20.

   LOOP AT GT_MAINITEM ASSIGNING FIELD-SYMBOL(<L_MAINITEM>).

     PERFORM F_GET_ACC_DESC USING <L_MAINITEM>-HKONT_DR
                            CHANGING <L_MAINITEM>-TXT20_DR.

     PERFORM F_GET_ACC_DESC USING <L_MAINITEM>-HKONT_CR
                       CHANGING <L_MAINITEM>-TXT20_CR.
   ENDLOOP.

   LOOP AT GT_POSTITEM ASSIGNING FIELD-SYMBOL(<L_POSTITEM>).

     PERFORM F_GET_ACC_DESC USING <L_POSTITEM>-HKONT_DR
                            CHANGING <L_POSTITEM>-TXT20_DR.

     PERFORM F_GET_ACC_DESC USING <L_POSTITEM>-HKONT_CR
                       CHANGING <L_POSTITEM>-TXT20_CR.

*transfer g/l to 1 field
     IF <L_POSTITEM>-HKONT_DR IS NOT INITIAL.
       <L_POSTITEM>-BSCHL = '40'.
       <L_POSTITEM>-HKONT = <L_POSTITEM>-HKONT_DR.
       <L_POSTITEM>-TXT20 = <L_POSTITEM>-TXT20_DR.

     ELSE.
       <L_POSTITEM>-BSCHL = '50'.
       <L_POSTITEM>-HKONT = <L_POSTITEM>-HKONT_CR.
       <L_POSTITEM>-TXT20 = <L_POSTITEM>-TXT20_CR.
       <L_POSTITEM>-WRBTR = - <L_POSTITEM>-WRBTR.
     ENDIF.

     PERFORM F_GET_CC_DESC USING <L_POSTITEM>-KOSTL
                        CHANGING <L_POSTITEM>-LTEXT.

   ENDLOOP.

   LOOP AT GT_ACCA ASSIGNING FIELD-SYMBOL(<L_ACCA>).
     PERFORM F_GET_CC_DESC USING <L_ACCA>-KOSTL
                        CHANGING <L_ACCA>-LTEXT.
   ENDLOOP.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_prepare_document_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM F_PREPARE_DOCUMENT_ITEM .

   DATA: LT_POSTITEM TYPE TT_POST,
         LF_WRBTR    TYPE WRBTR,
         LF_DIFF     TYPE WRBTR,
         LF_TOTAL    TYPE I.

   LOOP AT GT_MAINITEM INTO DATA(LS_MAINITEM) ##INTO_OK.
     FREE LT_POSTITEM.
     CLEAR: LF_WRBTR, LF_DIFF, LF_TOTAL.

     LOOP AT GT_ACCA INTO DATA(LS_ACCA) WHERE GUID EQ LS_MAINITEM-GUID ##INTO_OK.
       LF_TOTAL = LF_TOTAL + LS_ACCA-ALLKT.
     ENDLOOP.

*..Append Debit account
     LOOP AT GT_ACCA INTO LS_ACCA WHERE GUID EQ LS_MAINITEM-GUID ##INTO_OK.
       APPEND INITIAL LINE TO LT_POSTITEM ASSIGNING FIELD-SYMBOL(<L_POSTITEM>).

       MOVE-CORRESPONDING LS_MAINITEM TO <L_POSTITEM>.

       <L_POSTITEM>-GUID     = LS_ACCA-GUID.
       <L_POSTITEM>-RUNNO    = LS_ACCA-RUNNO.
       <L_POSTITEM>-KOSTL    = LS_ACCA-KOSTL.
       <L_POSTITEM>-LTEXT    = LS_ACCA-LTEXT.
       IF <L_POSTITEM>-AUFNR IS INITIAL.
         <L_POSTITEM>-AUFNR    = LS_ACCA-AUFNR.
       ENDIF.
       IF <L_POSTITEM>-PS_POSID IS INITIAL.
         <L_POSTITEM>-PS_POSID = LS_ACCA-PS_POSID.
       ENDIF.
       IF <L_POSTITEM>-PRCTR IS INITIAL.
         <L_POSTITEM>-PRCTR    = LS_ACCA-PRCTR.
       ENDIF.
*       MOVE-CORRESPONDING LS_ACCA TO <L_POSTITEM>.
       CLEAR <L_POSTITEM>-HKONT_CR.

       <L_POSTITEM>-BUKRS = ZSDSFIS074-BUKRS.
       <L_POSTITEM>-BELNR = ZSDSFIS074-BELNR.
       <L_POSTITEM>-GJAHR = ZSDSFIS074-GJAHR.
       <L_POSTITEM>-BUZEI = ZSDSFIS074-BUZEI.
       <L_POSTITEM>-WAERS = ZSDSFIS074-WAERS.
       <L_POSTITEM>-BUDAT = LS_MAINITEM-BUDAT.
       <L_POSTITEM>-RUNNO = LS_MAINITEM-RUNNO.
       <L_POSTITEM>-WRBTR = ( LS_MAINITEM-WRBTR * LS_ACCA-ALLKT ) / LF_TOTAL.
       LF_WRBTR = LF_WRBTR + <L_POSTITEM>-WRBTR.
       <L_POSTITEM>-ITMNO = LINES( LT_POSTITEM ).
     ENDLOOP.

*..append credit account and adjust diff to 1st item
     LF_DIFF = LS_MAINITEM-WRBTR - LF_WRBTR.
     IF LF_DIFF IS NOT INITIAL.
       READ TABLE LT_POSTITEM ASSIGNING <L_POSTITEM> INDEX 1.
       IF SY-SUBRC EQ 0.
         <L_POSTITEM>-WRBTR = <L_POSTITEM>-WRBTR + LF_DIFF.
       ENDIF.
     ENDIF.

     APPEND LINES OF LT_POSTITEM TO GT_POSTITEM.

     APPEND INITIAL LINE TO GT_POSTITEM ASSIGNING <L_POSTITEM>.
     MOVE-CORRESPONDING LS_MAINITEM TO <L_POSTITEM>.
     <L_POSTITEM>-BUKRS = ZSDSFIS074-BUKRS.
     <L_POSTITEM>-BELNR = ZSDSFIS074-BELNR.
     <L_POSTITEM>-GJAHR = ZSDSFIS074-GJAHR.
     <L_POSTITEM>-BUZEI = ZSDSFIS074-BUZEI.
     <L_POSTITEM>-WAERS = ZSDSFIS074-WAERS.
     <L_POSTITEM>-RUNNO = LS_MAINITEM-RUNNO.
     <L_POSTITEM>-ITMNO = LINES( LT_POSTITEM ) + 1.
     CLEAR <L_POSTITEM>-HKONT_DR.

   ENDLOOP.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_prepare_posting_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM F_PREPARE_POSTING_DATA .

   DATA: LR_COLUMNS          TYPE REF TO CL_SALV_COLUMNS_TABLE,
         LR_COLUMN           TYPE REF TO CL_SALV_COLUMN_TABLE,
         LR_SELECTIONS       TYPE REF TO CL_SALV_SELECTIONS,
         LR_EVENTS           TYPE REF TO CL_SALV_EVENTS_TABLE,
         LR_CONTENT          TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
         LR_LABEL            TYPE REF TO CL_SALV_FORM_LABEL,
         LR_TEXT             TYPE REF TO CL_SALV_FORM_TEXT ##NEEDED,
         LR_DISPLAY_SETTINGS TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
         LF_ANSWER           TYPE C,
         LF_QUESTION         TYPE STRING,
         LS_LAYOUT           TYPE REF TO CL_SALV_LAYOUT,
         LS_KEY              TYPE SALV_S_LAYOUT_KEY,
         LS_FUNCTIONS_LIST   TYPE REF TO CL_SALV_FUNCTIONS_LIST.

   CHECK RB_FC2 EQ ABAP_TRUE.

   GF_MODE   = GC_POST.

   CASE ABAP_TRUE.
     WHEN RB_M1.
       GF_HEADER = |{ TEXT-S04 } - { TEXT-S06 }|.
       LF_QUESTION = |{ TEXT-S06 }?|.
     WHEN OTHERS.
       GF_HEADER = |{ TEXT-S04 } - { TEXT-S07 }|.
       LF_QUESTION = |{ TEXT-S07 }?|.
   ENDCASE.

   SELECT GUID, RUNNO, ITMNO, BUKRS, BELNR, GJAHR, BUZEI, BUDAT, KOSTL, AUFNR,
          PS_POSID, PRCTR, WAERS, WRBTR, HKONT_DR, HKONT_CR,
          BUKRS_P, BELNR_P, GJAHR_P, SGTXT, STATUS, MSGTX, UNAME, UDATE, UTIME
     INTO CORRESPONDING FIELDS OF TABLE @GT_POSTITEM ##TOO_MANY_ITAB_FIELDS
     FROM ZSDSFIT026
*     WHERE BUDAT EQ @P_BUDAT
     WHERE BUKRS EQ @P_BUKRS
        AND BELNR IN @S_BELNR
        AND GJAHR IN @S_GJAHR
*        AND BUDAT EQ @P_BUDAT
        AND BUDAT IN @S_SELEC
        AND BUKRS_P EQ '' ORDER BY PRIMARY KEY.
   IF SY-SUBRC NE 0.
     MESSAGE 'No data found'(003) TYPE 'S' DISPLAY LIKE 'E'.
     LEAVE LIST-PROCESSING.
   ENDIF.

*...User's confirmation
   CALL FUNCTION 'POPUP_TO_CONFIRM'
     EXPORTING
       TITLEBAR              = TEXT-S04
       TEXT_QUESTION         = LF_QUESTION
       DISPLAY_CANCEL_BUTTON = SPACE
     IMPORTING
       ANSWER                = LF_ANSWER.

   CASE LF_ANSWER.
     WHEN '1'.
     WHEN OTHERS.
       LEAVE LIST-PROCESSING.
   ENDCASE.

*..................................................
* FI Document posting
*..................................................
   PERFORM F_POSTING_DATA.

*..................................................
* ALV DISPLAY
*..................................................
   TRY.
       CALL METHOD CL_SALV_TABLE=>FACTORY
         IMPORTING
           R_SALV_TABLE = GR_TABLE
         CHANGING
           T_TABLE      = GT_POSTITEM.
     CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND. "#EC NO_HANDLER
   ENDTRY.

   LS_FUNCTIONS_LIST = GR_TABLE->GET_FUNCTIONS( ).
   LS_FUNCTIONS_LIST->SET_ALL( ).

   LR_COLUMNS = GR_TABLE->GET_COLUMNS( ).
   LR_COLUMNS->SET_OPTIMIZE( GC_TRUE ).

   TRY.
       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'STATUS' ).
       LR_COLUMN->SET_ICON( IF_SALV_C_BOOL_SAP=>TRUE ).
       LR_COLUMN->SET_LONG_TEXT( VALUE = 'Status'(006) ).
       LR_COLUMN->SET_FIXED_HEADER_TEXT( 'L' ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'MSGTX' ).
       LR_COLUMN->SET_LONG_TEXT( VALUE = 'Message'(007) ).
       LR_COLUMN->SET_FIXED_HEADER_TEXT( 'L' ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'RUNNO' ).
       LR_COLUMN->SET_LONG_TEXT( VALUE = 'No.'(008) ).
       LR_COLUMN->SET_FIXED_HEADER_TEXT( 'L' ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'BUKRS' ).
       LR_COLUMN->SET_LONG_TEXT( VALUE = 'Ref.CoCode.'(009) ).
       LR_COLUMN->SET_FIXED_HEADER_TEXT( 'L' ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'BELNR' ).
       LR_COLUMN->SET_LONG_TEXT( VALUE = 'Ref.DocumentNo.'(010) ).
       LR_COLUMN->SET_FIXED_HEADER_TEXT( 'L' ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'GJAHR' ).
       LR_COLUMN->SET_LONG_TEXT( VALUE = 'Ref.FiscalYr'(011) ).
       LR_COLUMN->SET_FIXED_HEADER_TEXT( 'L' ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'BUZEI' ).
       LR_COLUMN->SET_LONG_TEXT( VALUE = 'Ref.Item'(012) ).
       LR_COLUMN->SET_FIXED_HEADER_TEXT( 'L' ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'GUID' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'RUNNO' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'HKONT_DR' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'TXT20_DR' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'HKONT_CR' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'TXT20_CR' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'UNAME' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'UTIME' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'UDATE' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'BUKRS_P' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'BELNR_P' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'GJAHR_P' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).

     CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND. "#EC NO_HANDLER
*        RETURN.
   ENDTRY.
*
   LR_SELECTIONS = GR_TABLE->GET_SELECTIONS( ).
   LR_SELECTIONS->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>ROW_COLUMN ).

   LR_EVENTS = GR_TABLE->GET_EVENT( ).

   CREATE OBJECT GR_EVENTS.

*... §6.1 register to the event USER_COMMAND
   SET HANDLER GR_EVENTS->ON_USER_COMMAND FOR LR_EVENTS.

* Top_of_list
   CREATE OBJECT LR_CONTENT.
   LR_LABEL = LR_CONTENT->CREATE_LABEL(
     ROW    = 1
     COLUMN = 1
     TEXT   = TEXT-S04 ).

   LR_LABEL = LR_CONTENT->CREATE_LABEL(
     ROW    = 2
     COLUMN = 1
     TEXT   = TEXT-S09 ).

   IF RB_M1 EQ ABAP_TRUE.
     LR_LABEL = LR_CONTENT->CREATE_LABEL(
       ROW    = 2
       COLUMN = 2
       TEXT   = TEXT-S06 ).
   ELSE.
     LR_LABEL = LR_CONTENT->CREATE_LABEL(
       ROW    = 2
       COLUMN = 2
       TEXT   = TEXT-S07 ).
   ENDIF.

   LR_LABEL->SET_LABEL_FOR( LR_TEXT ).
   GR_TABLE->SET_TOP_OF_LIST( LR_CONTENT ).
*
   LR_DISPLAY_SETTINGS = GR_TABLE->GET_DISPLAY_SETTINGS( ).
   LR_DISPLAY_SETTINGS->SET_STRIPED_PATTERN( VALUE = ABAP_TRUE ).
   LS_KEY-REPORT =  SY-CPROG.
   LS_KEY-HANDLE = '2000'.

   LS_LAYOUT = GR_TABLE->GET_LAYOUT( ).
   LS_LAYOUT->SET_KEY( LS_KEY ).
   LS_LAYOUT->SET_SAVE_RESTRICTION( IF_SALV_C_LAYOUT=>RESTRICT_NONE ).
   LS_LAYOUT->SET_DEFAULT( 'X' ).
   LS_LAYOUT->SET_INITIAL_LAYOUT( P_VAR ).

*... Display table
   GR_TABLE->DISPLAY( ).

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_posting_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM F_POSTING_DATA .

   DATA: LT_HEADER TYPE STANDARD TABLE OF ZSDSFIS074.

   IF LINES( GT_POSTITEM ) EQ 0.
     RETURN.
   ENDIF.

   SELECT GUID                                     "#EC CI_NO_TRANSFORM
          BUKRS BELNR GJAHR BUZEI BLDAT BUDAT BKTXT XBLNR WAERS
          WRBTR HKONT POPER BEGDA ENDDA METHOD SGTXT UNAME UDATE UTIME
     INTO CORRESPONDING FIELDS OF TABLE LT_HEADER ##TOO_MANY_ITAB_FIELDS
     FROM ZSDSFIT023
     FOR ALL ENTRIES IN GT_POSTITEM
     WHERE GUID EQ GT_POSTITEM-GUID.

   SORT LT_HEADER BY BUKRS BELNR GJAHR BUZEI.

   LOOP AT GT_POSTITEM ASSIGNING FIELD-SYMBOL(<L_POSTITEM>).
     IF <L_POSTITEM>-HKONT_CR IS NOT INITIAL.
       <L_POSTITEM>-WRBTR = - <L_POSTITEM>-WRBTR.
     ENDIF.

     PERFORM F_GET_ACC_DESC USING <L_POSTITEM>-HKONT_DR
                            CHANGING <L_POSTITEM>-TXT20_DR.

     PERFORM F_GET_ACC_DESC USING <L_POSTITEM>-HKONT_CR
                            CHANGING <L_POSTITEM>-TXT20_CR.

*transfer g/l to 1 field
     IF <L_POSTITEM>-HKONT_DR IS NOT INITIAL.
       <L_POSTITEM>-BSCHL = '40'.
       <L_POSTITEM>-HKONT = <L_POSTITEM>-HKONT_DR.
       <L_POSTITEM>-TXT20 = <L_POSTITEM>-TXT20_DR.

     ELSE.
       <L_POSTITEM>-BSCHL = '50'.
       <L_POSTITEM>-HKONT = <L_POSTITEM>-HKONT_CR.
       <L_POSTITEM>-TXT20 = <L_POSTITEM>-TXT20_CR.
     ENDIF.

     PERFORM F_GET_CC_DESC USING <L_POSTITEM>-KOSTL
                        CHANGING <L_POSTITEM>-LTEXT.

   ENDLOOP.

   LOOP AT LT_HEADER INTO DATA(LS_HEADER).
     PERFORM F_POSTING_INTERFACE_START.
     PERFORM F_FILL_FTPOST_HEADER USING LS_HEADER.

     LOOP AT GT_POSTITEM INTO DATA(LS_ITEM) WHERE GUID EQ LS_HEADER-GUID.
       PERFORM F_FILL_FTPOST_ITEM USING LS_HEADER LS_ITEM.
     ENDLOOP.

*>pst interface document
     PERFORM F_POSTING_INTERFACE_DOCUMENT USING LS_HEADER LS_ITEM.
     PERFORM F_POSTING_INTERFACE_END.
   ENDLOOP.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_posting_interface_start
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM F_POSTING_INTERFACE_START.

   CALL FUNCTION 'POSTING_INTERFACE_START'
     EXPORTING
       I_FUNCTION         = 'C'
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
*& Form f_fill_ftpost_header
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_HEADER
*&---------------------------------------------------------------------*
 FORM F_FILL_FTPOST_HEADER USING US_HEADER TYPE ZSDSFIS074. " ts_post.
* Post Document: Header Data

   DATA: LF_FVALUE TYPE FTPOST-FVAL.

   GF_COUNT = GF_COUNT + 1.

   GS_FTPOST-STYPE = 'K'.
   GS_FTPOST-COUNT =  GF_COUNT.
*
   WRITE US_HEADER-BUKRS TO LF_FVALUE.
   PERFORM F_FTPOST_FIELD USING 'BKPF-BUKRS' LF_FVALUE.       "Company Code
*
   WRITE P_BLART TO LF_FVALUE.
   PERFORM F_FTPOST_FIELD USING 'BKPF-BLART' LF_FVALUE.       "Doc Type
*
   WRITE P_BUDAT TO LF_FVALUE.
   PERFORM F_FTPOST_FIELD USING 'BKPF-BLDAT' LF_FVALUE.       "Document Date

   WRITE P_BUDAT TO LF_FVALUE.
   PERFORM F_FTPOST_FIELD USING 'BKPF-BUDAT' LF_FVALUE.     "postin date

*..G/L
   WRITE US_HEADER-HKONT TO LF_FVALUE.
   PERFORM F_FTPOST_FIELD USING 'BKPF-XBLNR' LF_FVALUE.       "reference

   WRITE US_HEADER-WAERS TO LF_FVALUE.
   PERFORM F_FTPOST_FIELD USING 'BKPF-WAERS' LF_FVALUE.       "curr.

   LF_FVALUE = |{ US_HEADER-BUKRS }{ US_HEADER-BELNR }{ US_HEADER-GJAHR }{ US_HEADER-BUZEI }|.
   PERFORM F_FTPOST_FIELD USING 'BKPF-BKTXT' LF_FVALUE.       "header text

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
 FORM F_FILL_FTPOST_ITEM USING US_HEADER TYPE ZSDSFIS074
                               US_ITEM   TYPE TS_POST.
   DATA: LF_FVALUE TYPE FTPOST-FVAL.

** Post Document: Fields of first line item

   GS_FTPOST-STYPE = 'P'.                  " Line Item
   GS_FTPOST-COUNT =  US_ITEM-ITMNO.                " Nr Line Item

   IF US_ITEM-HKONT_DR IS NOT INITIAL.
     LF_FVALUE = '40'.
     PERFORM F_FTPOST_FIELD USING 'RF05A-NEWBS' LF_FVALUE.        " Post Key
*
     WRITE US_ITEM-HKONT_DR TO LF_FVALUE LEFT-JUSTIFIED.
     PERFORM F_FTPOST_FIELD USING 'RF05A-NEWKO' LF_FVALUE.        " Account

     LF_FVALUE = 'VX'.
     PERFORM F_FTPOST_FIELD USING 'BSEG-MWSKZ' LF_FVALUE.    "Vat code
   ELSE.
     LF_FVALUE = '50'.
     PERFORM F_FTPOST_FIELD USING 'RF05A-NEWBS' LF_FVALUE.        " Post Key
*
     WRITE US_ITEM-HKONT_CR TO LF_FVALUE LEFT-JUSTIFIED.
     PERFORM F_FTPOST_FIELD USING 'RF05A-NEWKO' LF_FVALUE.        " Account
   ENDIF.
*
   WRITE ABS( US_ITEM-WRBTR ) TO LF_FVALUE NO-GROUPING LEFT-JUSTIFIED.
   PERFORM F_FTPOST_FIELD USING 'BSEG-WRBTR' LF_FVALUE.         " Amount

   LF_FVALUE = '0000'.
   PERFORM F_FTPOST_FIELD USING 'BSEG-BUPLA' LF_FVALUE.

   LF_FVALUE = US_HEADER-HKONT.
   PERFORM F_FTPOST_FIELD USING 'BSEG-ZUONR' LF_FVALUE.

*   LF_FVALUE = 'Pre-paid exp. Amort.'(015).
*   LF_FVALUE = |{ LF_FVALUE } { US_ITEM-BUDAT }|.
   LF_FVALUE = US_ITEM-SGTXT.
   PERFORM F_FTPOST_FIELD USING 'BSEG-SGTXT' LF_FVALUE.

   IF US_ITEM-KOSTL IS NOT INITIAL.
     WRITE US_ITEM-KOSTL TO LF_FVALUE NO-GROUPING LEFT-JUSTIFIED.
     PERFORM F_FTPOST_FIELD USING 'COBL-KOSTL' LF_FVALUE.
   ENDIF.

   IF US_ITEM-AUFNR IS NOT INITIAL.
     WRITE US_ITEM-AUFNR TO LF_FVALUE NO-GROUPING LEFT-JUSTIFIED.
     PERFORM F_FTPOST_FIELD USING 'COBL-AUFNR' LF_FVALUE.
   ENDIF.

   IF US_ITEM-PS_POSID IS NOT INITIAL.
     WRITE US_ITEM-PS_POSID TO LF_FVALUE NO-GROUPING LEFT-JUSTIFIED.
     PERFORM F_FTPOST_FIELD USING 'COBL-PS_POSID' LF_FVALUE.
   ENDIF.

   IF US_ITEM-PRCTR IS NOT INITIAL.
     WRITE US_ITEM-PRCTR TO LF_FVALUE NO-GROUPING LEFT-JUSTIFIED.
     PERFORM F_FTPOST_FIELD USING 'COBL-PRCTR' LF_FVALUE.
   ENDIF.

 ENDFORM.                    " F_FILL_FTPOST_ITEM
*&---------------------------------------------------------------------*
*&      Form  F_POSTING_INTERFACE_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM F_POSTING_INTERFACE_DOCUMENT USING US_HEADER TYPE ZSDSFIS074
                                         US_ITEM TYPE TS_POST.
   DATA: LF_SUBRC      TYPE SY-SUBRC ##NEEDED,
         LF_MSGID      TYPE SY-MSGID,
         LF_MSGNO      TYPE SY-MSGNO,
         LF_MSGTY      TYPE SY-MSGTY,
         LF_MSGV1      TYPE SY-MSGV1,
         LF_MSGV2      TYPE SY-MSGV2,
         LF_MSGV3      TYPE SY-MSGV3,
         LF_MSGV4      TYPE SY-MSGV4,
         LF_LINEP(110),
*         lf_index      TYPE i,
         LS_OUTTAB     TYPE TS_POST.

   DATA(LV_UNAME) = SY-UNAME.
   DATA(LV_DATUM) = SY-DATUM.
   DATA(LV_UZEIT) = SY-UZEIT.

   CALL FUNCTION 'POSTING_INTERFACE_DOCUMENT'
     EXPORTING
       I_TCODE                  = 'FB01'
       I_XSIMU                  = RB_M1
*      I_SGFUNCT                = ' '
*      I_NO_AUTH                = ' '
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
   IF SY-SUBRC EQ 0.

     CLEAR LS_OUTTAB.

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

     READ TABLE GT_BLNTAB INTO DATA(LS_BLNTAB) INDEX 1.
     IF SY-SUBRC EQ 0.
       LS_OUTTAB-STATUS  = ICON_GREEN_LIGHT.
       LS_OUTTAB-MSGTX   = LF_LINEP.
       LS_OUTTAB-BUKRS_P = LS_BLNTAB-BUKRS.
       LS_OUTTAB-GJAHR_P = LS_BLNTAB-GJAHR.
       LS_OUTTAB-BELNR_P = LS_BLNTAB-BELNR.
     ELSE. "Error or Simulation mode
       IF LF_MSGID EQ '00' AND LF_MSGTY  EQ 'S' AND LF_MSGNO EQ '344'.
         LS_OUTTAB-STATUS = ICON_GREEN_LIGHT.
         LS_OUTTAB-MSGTX  = SPACE.
       ELSE. "Error
         LS_OUTTAB-STATUS = ICON_RED_LIGHT.
         LS_OUTTAB-MSGTX  = LF_LINEP.
       ENDIF.
     ENDIF.
   ELSE.
     LS_OUTTAB-STATUS  = ICON_RED_LIGHT.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
                         INTO LS_OUTTAB-MSGTX.
   ENDIF.

   LS_OUTTAB-UNAME  = LV_UNAME.
   LS_OUTTAB-UDATE  = LV_DATUM.
   LS_OUTTAB-UTIME  = LV_UZEIT.

   IF RB_M2 EQ ABAP_TRUE.
     UPDATE ZSDSFIT026
     SET BUKRS_P = LS_OUTTAB-BUKRS_P
         GJAHR_P = LS_OUTTAB-GJAHR_P
         BELNR_P = LS_OUTTAB-BELNR_P
         STATUS  = LS_OUTTAB-STATUS
         MSGTX   = LS_OUTTAB-MSGTX
         UNAME   = LS_OUTTAB-UNAME
         UDATE   = LS_OUTTAB-UDATE
         UTIME   = LS_OUTTAB-UTIME
     WHERE GUID EQ US_HEADER-GUID
       AND RUNNO EQ US_ITEM-RUNNO.
     IF SY-SUBRC EQ 0.
       COMMIT WORK.
     ELSE.
       ROLLBACK WORK.                                  "#EC CI_ROLLBACK
     ENDIF.
   ENDIF.

*...
   MODIFY GT_POSTITEM FROM LS_OUTTAB TRANSPORTING STATUS MSGTX BUKRS_P BELNR_P GJAHR_P UNAME UDATE UTIME
                                     WHERE GUID  EQ US_HEADER-GUID
                                       AND RUNNO EQ US_ITEM-RUNNO.

   FREE: GT_FTPOST, GT_FTTAX, GT_BLNTAB.

 ENDFORM.                    " F_POSTING_INTERFACE_DOCUMENT
*&---------------------------------------------------------------------*
*& Form f_posting_interface_end
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
*& Form f_prepare_posting_report
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM F_PREPARE_POSTING_REPORT .

   DATA: LR_COLUMNS          TYPE REF TO CL_SALV_COLUMNS_TABLE,
         LR_COLUMN           TYPE REF TO CL_SALV_COLUMN_TABLE,
         LR_SELECTIONS       TYPE REF TO CL_SALV_SELECTIONS,
         LR_EVENTS           TYPE REF TO CL_SALV_EVENTS_TABLE,
         LR_CONTENT          TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
         LR_LABEL            TYPE REF TO CL_SALV_FORM_LABEL,
         LR_TEXT             TYPE REF TO CL_SALV_FORM_TEXT ##NEEDED,
         LR_DISPLAY_SETTINGS TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
         LS_LAYOUT           TYPE REF TO CL_SALV_LAYOUT,
         LS_KEY              TYPE SALV_S_LAYOUT_KEY,
         LS_FUNCTIONS_LIST   TYPE REF TO CL_SALV_FUNCTIONS_LIST.

   CHECK RB_FC3 EQ ABAP_TRUE.

   GF_MODE   = GC_DISP.
   GF_HEADER = TEXT-S05.

   SELECT GUID, RUNNO, ITMNO, BUKRS, BELNR, GJAHR, BUZEI,
          BUDAT, KOSTL, AUFNR, PS_POSID, PRCTR, WAERS, WRBTR, HKONT_DR, HKONT_CR,
          BUKRS_P, BELNR_P, GJAHR_P, SGTXT, STATUS, MSGTX, UNAME, UDATE, UTIME
     INTO CORRESPONDING FIELDS OF TABLE @GT_POSTITEM ##TOO_MANY_ITAB_FIELDS
      FROM ZSDSFIT026
      WHERE BUKRS EQ @P_BUKRS
        AND BELNR IN @S_BELNR
        AND GJAHR IN @S_GJAHR
        AND HKONT_CR IN @S_HKONT
        AND BUDAT IN @S_BUDAT
       ORDER BY PRIMARY KEY.
   IF SY-SUBRC NE 0.
     MESSAGE 'No data found'(003) TYPE 'S' DISPLAY LIKE 'E'.
     LEAVE LIST-PROCESSING.
   ENDIF.

   LOOP AT GT_POSTITEM ASSIGNING FIELD-SYMBOL(<L_POSTITEM>).
     PERFORM F_GET_ACC_DESC USING <L_POSTITEM>-HKONT_CR
                            CHANGING <L_POSTITEM>-TXT20_CR.
   ENDLOOP.

   TRY.
       CALL METHOD CL_SALV_TABLE=>FACTORY
         IMPORTING
           R_SALV_TABLE = GR_TABLE
         CHANGING
           T_TABLE      = GT_POSTITEM.

       LS_FUNCTIONS_LIST = GR_TABLE->GET_FUNCTIONS( ).
       LS_FUNCTIONS_LIST->SET_ALL( ).

       LR_COLUMNS = GR_TABLE->GET_COLUMNS( ).
       LR_COLUMNS->SET_OPTIMIZE( GC_TRUE ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'STATUS' ).
       LR_COLUMN->SET_ICON( IF_SALV_C_BOOL_SAP=>TRUE ).
       LR_COLUMN->SET_LONG_TEXT( VALUE = 'Status'(006) ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'MSGTX' ).
       LR_COLUMN->SET_LONG_TEXT( VALUE = 'Message'(007) ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'ITMNO' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'GUID' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'KOSTL' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'AUFNR' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'PS_POSID' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'PRCTR' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'HKONT_DR' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'TXT20_DR' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'BSCHL' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'LTEXT' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'HKONT' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'TXT20' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'UNAME' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'UDATE' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'UTIME' ).
       LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'RUNNO' ).
       LR_COLUMN->SET_LONG_TEXT( VALUE = 'Posting No.'(013) ).
       LR_COLUMN->SET_FIXED_HEADER_TEXT( 'L' ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'BUKRS' ).
       LR_COLUMN->SET_LONG_TEXT( VALUE = 'Ref.CoCode.'(009) ).
       LR_COLUMN->SET_FIXED_HEADER_TEXT( 'L' ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'BELNR' ).
       LR_COLUMN->SET_LONG_TEXT( VALUE = 'Ref.DocumentNo.'(010) ).
       LR_COLUMN->SET_FIXED_HEADER_TEXT( 'L' ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'GJAHR' ).
       LR_COLUMN->SET_LONG_TEXT( VALUE = 'Ref.FiscalYr'(011) ).
       LR_COLUMN->SET_FIXED_HEADER_TEXT( 'L' ).

       LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'BUZEI' ).
       LR_COLUMN->SET_LONG_TEXT( VALUE = 'Ref.Item'(012) ).
       LR_COLUMN->SET_FIXED_HEADER_TEXT( 'L' ).

     CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND. "#EC NO_HANDLER
   ENDTRY.

   LR_SELECTIONS = GR_TABLE->GET_SELECTIONS( ).
   LR_SELECTIONS->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>ROW_COLUMN ).

   LR_EVENTS = GR_TABLE->GET_EVENT( ).

   CREATE OBJECT GR_EVENTS.

*... §6.1 register to the event USER_COMMAND
   SET HANDLER GR_EVENTS->ON_USER_COMMAND FOR LR_EVENTS.

* Top_of_list
   CREATE OBJECT LR_CONTENT.
   LR_LABEL = LR_CONTENT->CREATE_LABEL(
     ROW    = 1
     COLUMN = 1
     TEXT   = GF_HEADER ).

   LR_LABEL->SET_LABEL_FOR( LR_TEXT ).
   GR_TABLE->SET_TOP_OF_LIST( LR_CONTENT ).

   LR_DISPLAY_SETTINGS = GR_TABLE->GET_DISPLAY_SETTINGS( ).
   LR_DISPLAY_SETTINGS->SET_STRIPED_PATTERN( VALUE = ABAP_TRUE ).

   LS_KEY-REPORT =  SY-CPROG.
   LS_KEY-HANDLE = '3000'.

   LS_LAYOUT = GR_TABLE->GET_LAYOUT( ).
   LS_LAYOUT->SET_KEY( LS_KEY ).
   LS_LAYOUT->SET_SAVE_RESTRICTION( IF_SALV_C_LAYOUT=>RESTRICT_NONE ).
   LS_LAYOUT->SET_DEFAULT( 'X' ).
   LS_LAYOUT->SET_INITIAL_LAYOUT( P_VAR ).

*... Display table
   GR_TABLE->DISPLAY( ).

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_prepare_document_save
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM F_PREPARE_DOCUMENT_SAVE .

   DATA: LT_HEADER   TYPE STANDARD TABLE OF ZSDSFIT023,
         LT_MAINITEM TYPE STANDARD TABLE OF ZSDSFIT024,
         LT_ACCA     TYPE STANDARD TABLE OF ZSDSFIT025,
         LT_POSTITEM TYPE STANDARD TABLE OF ZSDSFIT026,
         LF_UUID     TYPE CHAR32.

   IF LINES( GT_ACCA ) EQ 0.
     MESSAGE TEXT-E02 TYPE 'S' DISPLAY LIKE 'E'.
   ENDIF.

   TRY.
       LF_UUID = CL_SYSTEM_UUID=>IF_SYSTEM_UUID_STATIC~CREATE_UUID_C32( ).
     CATCH CX_UUID_ERROR.
       DATA LF_STRING TYPE STRING.
       CALL FUNCTION 'GENERAL_GET_RANDOM_STRING'
         EXPORTING
           NUMBER_CHARS  = 32
         IMPORTING
           RANDOM_STRING = LF_STRING.
       LF_UUID = LF_STRING.
   ENDTRY.

   IF LF_UUID IS INITIAL.
     MESSAGE 'No uuid created'(014) TYPE 'S' DISPLAY LIKE 'E'.
   ENDIF.

*prepare header
   APPEND INITIAL LINE TO LT_HEADER ASSIGNING FIELD-SYMBOL(<L_HEADER>).
   MOVE-CORRESPONDING ZSDSFIS074 TO <L_HEADER>.
   <L_HEADER>-GUID  = LF_UUID.
   <L_HEADER>-UNAME = SY-UNAME.
   <L_HEADER>-UDATE = SY-DATUM.
   <L_HEADER>-UTIME = SY-UZEIT.

*prepare item
   LOOP AT GT_MAINITEM INTO DATA(LS_MAINITEM) ##INTO_OK.
     APPEND INITIAL LINE TO LT_MAINITEM ASSIGNING FIELD-SYMBOL(<L_ITEM>).
     MOVE-CORRESPONDING LS_MAINITEM TO <L_ITEM>.
     <L_ITEM>-GUID  = LF_UUID.
     <L_ITEM>-BLDAT = LS_MAINITEM-BUDAT.
     <L_ITEM>-RUNNO = LINES( LT_MAINITEM ).
   ENDLOOP.

*prepare Account assignment
   LOOP AT GT_ACCA INTO DATA(LS_ACCA)  ##INTO_OK.
     APPEND INITIAL LINE TO LT_ACCA ASSIGNING FIELD-SYMBOL(<L_ACCA>).
     MOVE-CORRESPONDING LS_ACCA TO <L_ACCA>.
     <L_ACCA>-GUID = LF_UUID.
     <L_ACCA>-RUNNO = LINES( LT_ACCA ).
   ENDLOOP.

   LOOP AT GT_POSTITEM INTO DATA(LS_POSTITEM) ##INTO_OK.
     APPEND INITIAL LINE TO LT_POSTITEM ASSIGNING FIELD-SYMBOL(<L_POSTITEM>).
     MOVE-CORRESPONDING LS_POSTITEM TO <L_POSTITEM>.
     <L_POSTITEM>-GUID  = LF_UUID.
     <L_POSTITEM>-SGTXT = ZSDSFIS074-SGTXT.

*<<< Begin of insertion CH01
     IF <L_POSTITEM>-HKONT_CR EQ ZSDSFIS074-HKONT.
       <L_POSTITEM>-PRCTR = ZSDSFIS074-PRCTR.
     ENDIF.
*>>> End of insertion

   ENDLOOP.

   MODIFY ZSDSFIT023 FROM TABLE LT_HEADER.
   IF SY-SUBRC EQ 0.
     MODIFY ZSDSFIT024 FROM TABLE LT_MAINITEM.
     IF SY-SUBRC EQ 0.
       MODIFY ZSDSFIT025 FROM TABLE LT_ACCA.
       IF SY-SUBRC EQ 0.
         MODIFY ZSDSFIT026 FROM TABLE LT_POSTITEM.
         IF SY-SUBRC EQ 0.
           COMMIT WORK.
           MESSAGE 'Pre-paid Expense Amortization for posting has been saved'(004) TYPE 'S'.

           READ TABLE GT_MAIN ASSIGNING FIELD-SYMBOL(<L_MAIN>) WITH KEY BUKRS = <L_HEADER>-BUKRS
                                                                        BELNR = <L_HEADER>-BELNR
                                                                        GJAHR = <L_HEADER>-GJAHR
                                                                        BUZEI = <L_HEADER>-BUZEI.
           IF SY-SUBRC EQ 0.
             <L_MAIN>-STATUS_ICON = ICON_LED_YELLOW.
           ENDIF.

         ELSE.
           ROLLBACK WORK.                              "#EC CI_ROLLBACK
           MESSAGE 'Error during update custom table'(002) TYPE 'E'.
         ENDIF.
       ELSE.
         ROLLBACK WORK.                                "#EC CI_ROLLBACK
         MESSAGE 'Error during update custom table'(002) TYPE 'E'.
       ENDIF.

     ELSE.
       ROLLBACK WORK.                                  "#EC CI_ROLLBACK
       MESSAGE 'Error during update custom table'(002) TYPE 'E'.
     ENDIF.
   ELSE.
     ROLLBACK WORK.                                    "#EC CI_ROLLBACK
     MESSAGE 'Error during update custom table'(002) TYPE 'E'.
   ENDIF.

   CLEAR GF_OKCODE.
   GF_MODE = GC_DISP.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_period_tab
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_PERIOD
*&      <-- LF_ENDDA
*&---------------------------------------------------------------------*
 FORM  F_GET_PERIOD_TAB  TABLES   UT_PERIOD TYPE TT_PERIOD
                        CHANGING CF_ENDDA TYPE SYDATUM.

   DATA: LS_PERIOD TYPE TS_PERIOD,
         LF_BEGDA  TYPE SYDATUM,
         LF_ENDDA  TYPE SYDATUM,
         LF_TOTAL  TYPE I.

   FREE UT_PERIOD.
   CLEAR: CF_ENDDA, LF_TOTAL.

   LF_BEGDA      = ZSDSFIS074-BEGDA.

   DO ZSDSFIS074-POPER TIMES.

     APPEND INITIAL LINE TO UT_PERIOD ASSIGNING FIELD-SYMBOL(<L_PERIOD>).
     <L_PERIOD>-POPER = LINES( UT_PERIOD ).

     <L_PERIOD>-BEGDA = LF_BEGDA.

     CALL FUNCTION 'PS_LAST_DAY_OF_MONTH'
       EXPORTING
         DAY_IN            = LF_BEGDA
       IMPORTING
         LAST_DAY_OF_MONTH = LF_ENDDA
       EXCEPTIONS
         DAY_IN_NOT_VALID  = 1
         OTHERS            = 2.
     IF SY-SUBRC NE 0.
       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.

*...posting date  = end of month
     <L_PERIOD>-BUDAT = LF_ENDDA.
     <L_PERIOD>-BLDAT = LF_ENDDA.

*..check last period, set day to user input
     IF LINES( UT_PERIOD ) EQ ZSDSFIS074-POPER.
       LF_ENDDA+6(2) = ZSDSFIS074-ENDDA+6(2).

       CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
         EXPORTING
           DATE                      = LF_ENDDA
         EXCEPTIONS
           PLAUSIBILITY_CHECK_FAILED = 1
           OTHERS                    = 2.
       IF SY-SUBRC NE 0.
         LF_ENDDA = ZSDSFIS074-ENDDA.
       ENDIF.
     ENDIF.

     <L_PERIOD>-ENDDA = LF_ENDDA.

     CALL FUNCTION 'C14B_DIFF_BT_2_DATES'
       EXPORTING
         I_DATE_FROM               = LF_BEGDA
         I_DATE_TO                 = LF_ENDDA
       IMPORTING
         E_DAYS                    = <L_PERIOD>-DAY
       EXCEPTIONS
         PLAUSIBILITY_CHECK_FAILED = 1
         OTHERS                    = 2.
     IF SY-SUBRC NE 0.
       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.

     <L_PERIOD>-DAY = <L_PERIOD>-DAY + 1.        "No of day in month
     LF_TOTAL       = LF_TOTAL + <L_PERIOD>-DAY. "No of day in total
     CF_ENDDA       = LF_ENDDA.
*...
     LF_BEGDA+6(2) = '01'.            "set first day of month
     CALL FUNCTION 'HR_JP_ADD_MONTH_TO_DATE'
       EXPORTING
         IV_MONTHCOUNT = 1
         IV_DATE       = LF_BEGDA
       IMPORTING
         EV_DATE       = LF_BEGDA.
   ENDDO.

   CLEAR LS_PERIOD.
   LS_PERIOD-TOTAL = LF_TOTAL.
   MODIFY UT_PERIOD FROM LS_PERIOD TRANSPORTING TOTAL WHERE POPER IS NOT INITIAL.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_alv_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_PERIOD
*&---------------------------------------------------------------------*
 FORM F_UPDATE_ALV_ITEM  TABLES UT_PERIOD TYPE TT_PERIOD.

   FREE GT_MAINITEM.
   CLEAR:  GS_MAINITEM.

   REFRESH CONTROL 'TABLE_CTRL_MAIN' FROM SCREEN '9001'.

   LOOP AT UT_PERIOD INTO DATA(LS_PERIOD).
     APPEND INITIAL LINE TO GT_MAINITEM ASSIGNING FIELD-SYMBOL(<L_MAINITEM>).
     MOVE-CORRESPONDING ZSDSFIS074 TO <L_MAINITEM>.
     <L_MAINITEM>-BUDAT    = LS_PERIOD-BUDAT.
     <L_MAINITEM>-HKONT_CR = ZSDSFIS074-HKONT.
     <L_MAINITEM>-TXT20_CR = ZSDSFIS074-TXT20.
     <L_MAINITEM>-WRBTR    = ( ZSDSFIS074-WRBTR * LS_PERIOD-DAY ) / LS_PERIOD-TOTAL.
     <L_MAINITEM>-RUNNO    = LINES( GT_MAINITEM ).
   ENDLOOP.

   GF_MAINITEM_COPIED = 'X'.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_document_completion
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM F_CHECK_DOCUMENT_COMPLETION .

   DELETE GT_ACCA WHERE ALLKT IS INITIAL.

   IF LINES( GT_ACCA ) EQ 0.
     MESSAGE TEXT-E02 TYPE 'E'.
     RETURN.
   ENDIF.

   READ TABLE GT_MAINITEM WITH KEY HKONT_DR = '' TRANSPORTING NO FIELDS.
   IF SY-SUBRC EQ 0.
     MESSAGE TEXT-E05 TYPE 'E'.
     RETURN.
   ENDIF.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form exit_program
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM EXIT_PROGRAM.

   CLEAR GF_OKCODE.

   SET SCREEN 0.
   LEAVE SCREEN.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_data_init
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM F_DATA_INIT .

   CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
     EXPORTING
       DAY_IN            = SY-DATUM
     IMPORTING
       LAST_DAY_OF_MONTH = P_BUDAT
     EXCEPTIONS
       DAY_IN_NO_DATE    = 1
       OTHERS            = 2.
   IF SY-SUBRC NE 0.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_scr_modif
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM F_SCR_MODIF .

   LOOP AT SCREEN.
     IF SCREEN-NAME EQ 'S_HKONT-LOW'.
       SCREEN-INPUT = '0'.
       MODIFY SCREEN.
       EXIT.
     ENDIF.
   ENDLOOP.

 ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_VAR  text
*----------------------------------------------------------------------*
 FORM F4_VARIANT  CHANGING P_VAR TYPE DISVARIANT-VARIANT.

   DATA: LS_VARIANT TYPE DISVARIANT,
         LF_EXIT    TYPE CHAR1.

   LS_VARIANT-REPORT = SY-REPID.

   CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
     EXPORTING
       IS_VARIANT = LS_VARIANT
       I_SAVE     = 'A'
*      it_default_fieldcat =
     IMPORTING
       E_EXIT     = LF_EXIT
       ES_VARIANT = LS_VARIANT
     EXCEPTIONS
       NOT_FOUND  = 2.
   IF SY-SUBRC = 2.
     MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ELSE.
     IF LF_EXIT EQ SPACE.
       P_VAR = LS_VARIANT-VARIANT.
     ENDIF.
   ENDIF.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_constants
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM F_GET_CONSTANTS .

   CONSTANTS:
     LC_PARAM  TYPE  ZSDSDE_PARAM_NAME VALUE 'SAKNR'.

   DATA:
     LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

   DATA:
     LF_REPID TYPE  PROGRAMM,
     LS_HKONT LIKE LINE OF S_HKONT.

* Assign REPID
   LF_REPID = SY-REPID.

* Read All GenC constants for program
   CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
     EXPORTING
       IF_REPID = LF_REPID
     IMPORTING
       ET_GEN_C = LT_GENC.

* Assign GenC Constants
   LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).

     CASE <L_GENC>-PARAM.
*     ------------------------------------
*     Example Parameter
*     ------------------------------------
       WHEN LC_PARAM.
         LS_HKONT-SIGN = <L_GENC>-PARAM_SIGN.
         LS_HKONT-OPTION = <L_GENC>-PARAM_OPTION.
         LS_HKONT-LOW    = <L_GENC>-VALUE_LOW.
         LS_HKONT-HIGH    = <L_GENC>-VALUE_HIGH.
         APPEND LS_HKONT TO S_HKONT.

     ENDCASE.
   ENDLOOP.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_transform_postitem
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM F_TRANSFORM_POSTITEM.

   LOOP AT GT_POSTITEM ASSIGNING FIELD-SYMBOL(<L_POSTITEM>).
     IF  <L_POSTITEM>-HKONT_DR IS NOT INITIAL.
       SELECT  TXT20 INTO <L_POSTITEM>-TXT20_DR
             FROM SKAT UP TO 1 ROWS
         WHERE SPRAS EQ SY-LANGU
             AND KTOPL EQ 'RCOA'
             AND SAKNR EQ <L_POSTITEM>-HKONT_DR ORDER BY SAKNR.
       ENDSELECT.
     ENDIF.

     IF  <L_POSTITEM>-HKONT_CR IS NOT INITIAL.
       SELECT TXT20 INTO <L_POSTITEM>-TXT20_CR
              FROM SKAT UP TO 1 ROWS
          WHERE SPRAS EQ SY-LANGU
              AND KTOPL EQ 'RCOA'
              AND SAKNR EQ <L_POSTITEM>-HKONT_CR ORDER BY SAKNR.
       ENDSELECT.
     ENDIF.
*transfer g/l to 1 field
     IF <L_POSTITEM>-HKONT_DR IS NOT INITIAL.
       <L_POSTITEM>-BSCHL = '40'.
       <L_POSTITEM>-HKONT = <L_POSTITEM>-HKONT_DR.
       <L_POSTITEM>-TXT20 = <L_POSTITEM>-TXT20_DR.

     ELSE.
       <L_POSTITEM>-BSCHL = '50'.
       <L_POSTITEM>-HKONT = <L_POSTITEM>-HKONT_CR.
       <L_POSTITEM>-TXT20 = <L_POSTITEM>-TXT20_CR.
       <L_POSTITEM>-WRBTR = - <L_POSTITEM>-WRBTR.
     ENDIF.

     SELECT LTEXT FROM CSKT UP TO 1 ROWS INTO <L_POSTITEM>-LTEXT
       WHERE SPRAS EQ SY-LANGU
         AND KOKRS EQ '1000'
        AND KOSTL EQ <L_POSTITEM>-KOSTL
        AND DATBI GE SY-DATUM ORDER BY KOSTL.

     ENDSELECT.
   ENDLOOP.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_refresh_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM F_REFRESH_DATA .

   FREE: GT_ACCA, GT_BLNTAB, GT_FTPOST, GT_FTTAX, GT_MAIN,
         GT_MAINITEM, GT_POSTITEM.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_delete
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM F_DELETE .

   DATA: LF_ANSWER   TYPE C,
         LF_QUESTION TYPE STRING.

   GF_HEADER = |{ TEXT-S04 } - { TEXT-S10 }|.
   LF_QUESTION = |{ TEXT-S11 }?|.

*...User's confirmation
   CALL FUNCTION 'POPUP_TO_CONFIRM'
     EXPORTING
       TITLEBAR              = TEXT-S04
       TEXT_QUESTION         = LF_QUESTION
       DISPLAY_CANCEL_BUTTON = SPACE
     IMPORTING
       ANSWER                = LF_ANSWER.

   IF LF_ANSWER NE '1'.
     RETURN.
   ENDIF.

   DELETE FROM ZSDSFIT023 WHERE GUID EQ ZSDSFIS074-GUID.
   IF SY-SUBRC EQ 0.
     DELETE FROM ZSDSFIT024 WHERE GUID EQ ZSDSFIS074-GUID.
     IF SY-SUBRC EQ 0.
       DELETE FROM ZSDSFIT025 WHERE GUID EQ ZSDSFIS074-GUID.
       IF SY-SUBRC EQ 0.
         DELETE FROM ZSDSFIT026 WHERE GUID EQ ZSDSFIS074-GUID.
         IF SY-SUBRC EQ 0.
           COMMIT WORK.
           MESSAGE 'Pre-paid Expense Amortization for posting has been deleted'(001) TYPE 'S'.

           READ TABLE GT_MAIN ASSIGNING FIELD-SYMBOL(<L_MAIN>) WITH KEY BUKRS = ZSDSFIS074-BUKRS
                                                                        BELNR = ZSDSFIS074-BELNR
                                                                        GJAHR = ZSDSFIS074-GJAHR
                                                                        BUZEI = ZSDSFIS074-BUZEI.
           IF SY-SUBRC EQ 0.
             <L_MAIN>-STATUS_ICON = ICON_LED_INACTIVE.
           ENDIF.
         ELSE.
           ROLLBACK WORK.                              "#EC CI_ROLLBACK
           MESSAGE 'Error during update custom table'(002) TYPE 'E'.
         ENDIF.
       ELSE.
         ROLLBACK WORK.                                "#EC CI_ROLLBACK
         MESSAGE 'Error during update custom table'(002) TYPE 'E'.
       ENDIF.
     ELSE.
       ROLLBACK WORK.                                  "#EC CI_ROLLBACK
       MESSAGE 'Error during update custom table'(002) TYPE 'E'.
     ENDIF.
   ELSE.
     ROLLBACK WORK.                                    "#EC CI_ROLLBACK
     MESSAGE 'Error during update custom table'(002) TYPE 'E'.
   ENDIF.

   CLEAR GF_OKCODE.

   GF_MODE = GC_DISP.

 ENDFORM.
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
*& Form f_display_grid
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM F_DISPLAY_GRID .

   DATA: LR_FUNCTIONS TYPE REF TO CL_SALV_FUNCTIONS_LIST,
         LR_COLUMNS   TYPE REF TO CL_SALV_COLUMNS,
         LR_COLUMN    TYPE REF TO CL_SALV_COLUMN_TABLE,
         LR_LAYOUT    TYPE REF TO CL_SALV_LAYOUT,
         LS_KEY       TYPE SALV_S_LAYOUT_KEY.

   IF CONTAINER IS NOT BOUND.

     IF CL_SALV_TABLE=>IS_OFFLINE( ) EQ IF_SALV_C_BOOL_SAP=>FALSE.
       CREATE OBJECT CONTAINER
         EXPORTING
           CONTAINER_NAME = 'CONTAINER'.
     ENDIF.


     TRY.
         CL_SALV_TABLE=>FACTORY(
           EXPORTING
             R_CONTAINER    = CONTAINER
             CONTAINER_NAME = 'CONTAINER'
           IMPORTING
             R_SALV_TABLE   = GR_POSTITEM_TABLE
           CHANGING
             T_TABLE        = GT_POSTITEM ).
       CATCH CX_SALV_MSG.                               "#EC NO_HANDLER
     ENDTRY.

     GR_POSTITEM_TABLE->REFRESH(
*     S_STABLE     =
       REFRESH_MODE = IF_SALV_C_REFRESH=>SOFT
     ).

*   activate alv generic functions
     LR_FUNCTIONS = GR_POSTITEM_TABLE->GET_FUNCTIONS( ).
     LR_FUNCTIONS->SET_DEFAULT( ABAP_TRUE ).

     LR_COLUMNS = GR_POSTITEM_TABLE->GET_COLUMNS( ).
     LR_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ).

     TRY.
         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'RUNNO' ).
         LR_COLUMN->SET_SHORT_TEXT( VALUE = 'No.' ).
         LR_COLUMN->SET_MEDIUM_TEXT( VALUE = 'No.' ).
         LR_COLUMN->SET_LONG_TEXT( VALUE = 'No.' ).

         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'BUKRS' ).
         LR_COLUMN->SET_SHORT_TEXT( VALUE = 'Ref.CoCode' ).
         LR_COLUMN->SET_MEDIUM_TEXT( VALUE = 'Ref.CoCode.' ).
         LR_COLUMN->SET_LONG_TEXT( VALUE = 'Ref.CoCode.' ).
         LR_COLUMN->SET_FIXED_HEADER_TEXT( 'L' ).

         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'BELNR' ).
         LR_COLUMN->SET_SHORT_TEXT( VALUE = 'Ref.DocNo.' ).
         LR_COLUMN->SET_MEDIUM_TEXT( VALUE = 'Ref.DocumentNo.' ).
         LR_COLUMN->SET_LONG_TEXT( VALUE = 'Ref.DocumentNo.' ).

         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'GJAHR' ).
         LR_COLUMN->SET_SHORT_TEXT( VALUE = 'Ref.FYear' ).
         LR_COLUMN->SET_MEDIUM_TEXT( VALUE = 'Ref.FiscalYr' ).
         LR_COLUMN->SET_LONG_TEXT( VALUE = 'Ref.FiscalYr' ).

         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'BUZEI' ).
         LR_COLUMN->SET_SHORT_TEXT( VALUE = 'Ref.Item' ).
         LR_COLUMN->SET_MEDIUM_TEXT( VALUE = 'Ref.Item' ).
         LR_COLUMN->SET_LONG_TEXT( VALUE = 'Ref.Item' ).

         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'STATUS' ).
         LR_COLUMN->SET_ICON( IF_SALV_C_BOOL_SAP=>TRUE ).
*         LR_COLUMN->SET_SHORT_TEXT( VALUE = 'Status' ).
*         LR_COLUMN->SET_MEDIUM_TEXT( VALUE = 'Status' ).
         LR_COLUMN->SET_LONG_TEXT( VALUE = 'Status'(006) ).
         LR_COLUMN->SET_FIXED_HEADER_TEXT( 'L' ).

         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'MSGTX' ).
*         LR_COLUMN->SET_SHORT_TEXT( VALUE = 'Message' ).
*         LR_COLUMN->SET_MEDIUM_TEXT( VALUE = 'Message' ).
         LR_COLUMN->SET_LONG_TEXT( VALUE = 'Message'(007) ).
         LR_COLUMN->SET_FIXED_HEADER_TEXT( 'L' ).

         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'GUID' ).
         LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
*         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'STATUS' ).
*         LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
*         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'MSGTX' ).
*         LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
*         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'BUKRS_P' ).
*         LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
*         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'BELNR_P' ).
*         LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
*         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'GJAHR_P' ).
*         LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
*         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'UNAME' ).
*         LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
*         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'UTIME' ).
*         LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
*         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'UDATE' ).
         LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'HKONT_DR' ).
         LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'TXT20_DR' ).
         LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'HKONT_CR' ).
         LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'TXT20_CR' ).
         LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'UNAME' ).
         LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'UTIME' ).
         LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
         LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'UDATE' ).
         LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
       CATCH CX_SALV_NOT_FOUND ##NO_HANDLER.
     ENDTRY.

     LR_LAYOUT = GR_POSTITEM_TABLE->GET_LAYOUT( ).

     LS_KEY-REPORT = SY-REPID.
     LS_KEY-HANDLE = '00'.
     LR_LAYOUT->SET_KEY( LS_KEY ).

     LR_LAYOUT = GR_POSTITEM_TABLE->GET_LAYOUT( ).
     LR_LAYOUT->SET_KEY( LS_KEY ).
     LR_LAYOUT->SET_SAVE_RESTRICTION( IF_SALV_C_LAYOUT=>RESTRICT_NONE ).
     LR_LAYOUT->SET_DEFAULT( 'X' ).
     LR_LAYOUT->SET_INITIAL_LAYOUT( P_VAR ).

   ENDIF.

*... §4 display the table
   GR_POSTITEM_TABLE->DISPLAY( ).


 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_status_9001
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM F_STATUS_9001 .

   IF GF_MODE NE GC_CRET OR SY-UCOMM EQ 'SAVE'.
     LOOP AT TABLE_CTRL_MAIN-COLS ASSIGNING FIELD-SYMBOL(<L_COL>).
       <L_COL>-SCREEN-INPUT = '0'.
     ENDLOOP.
   ELSE.
     LOOP AT TABLE_CTRL_MAIN-COLS ASSIGNING <L_COL>.
       IF ZSDSFIS074-POPER IS INITIAL AND ZSDSFIS074-BEGDA IS INITIAL AND ZSDSFIS074-ENDDA IS INITIAL.
         IF <L_COL>-SCREEN-NAME EQ 'ZSDSFIS075-HKONT_DR'.
           <L_COL>-SCREEN-INPUT = '0'.
           <L_COL>-SCREEN-REQUIRED = '0'.
         ENDIF.
       ELSE.
         IF <L_COL>-SCREEN-NAME EQ 'ZSDSFIS075-HKONT_DR'.
           <L_COL>-SCREEN-INPUT = '1'.
           <L_COL>-SCREEN-REQUIRED = '1'.
         ENDIF.
       ENDIF.
     ENDLOOP.
   ENDIF.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_status_9002
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM F_STATUS_9002 .

   IF GF_MODE NE GC_CRET OR SY-UCOMM EQ 'SAVE'.
     LOOP AT TABLE_CTRL_ACCA-COLS ASSIGNING FIELD-SYMBOL(<L_ACC>).
       <L_ACC>-SCREEN-INPUT = '0'.
     ENDLOOP.
   ELSE.
     LOOP AT TABLE_CTRL_ACCA-COLS ASSIGNING <L_ACC>.
       IF ZSDSFIS074-POPER IS INITIAL AND ZSDSFIS074-BEGDA IS INITIAL AND ZSDSFIS074-ENDDA IS INITIAL.
         IF <L_ACC>-SCREEN-NAME EQ 'ZSDSFIS076-KOSTL' OR
           <L_ACC>-SCREEN-NAME  EQ 'ZSDSFIS076-ALLKT' OR
           <L_ACC>-SCREEN-NAME  EQ 'ZSDSFIS076-AUFNR' OR
           <L_ACC>-SCREEN-NAME  EQ 'ZSDSFIS076-PS_POSID' OR
           <L_ACC>-SCREEN-NAME  EQ 'ZSDSFIS076-PRCTR'.
           <L_ACC>-SCREEN-INPUT = '0'.
         ENDIF.
       ELSE.
         IF <L_ACC>-SCREEN-NAME EQ 'ZSDSFIS076-KOSTL' OR
            <L_ACC>-SCREEN-NAME EQ 'ZSDSFIS076-ALLKT' OR
           <L_ACC>-SCREEN-NAME  EQ 'ZSDSFIS076-AUFNR' OR
           <L_ACC>-SCREEN-NAME  EQ 'ZSDSFIS076-PS_POSID' OR
           <L_ACC>-SCREEN-NAME  EQ 'ZSDSFIS076-PRCTR'.
           <L_ACC>-SCREEN-INPUT = '1'.
         ENDIF.
       ENDIF.
     ENDLOOP.
   ENDIF.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_user_command_9000
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM F_USER_COMMAND_9000 .

   DATA: LF_ANSWER   TYPE C  ##NEEDED,
         LF_QUESTION TYPE STRING  ##NEEDED,
         LF_OKCODE   TYPE SY-UCOMM  ##NEEDED,
         LS_STBL     TYPE LVC_S_STBL.

   LF_OKCODE = GF_OKCODE.
   LS_STBL-COL = ABAP_TRUE.
   LS_STBL-ROW = ABAP_TRUE.

   CASE LF_OKCODE.
     WHEN '&F03' OR '&F15' OR '&F12'.

*       GR_TABLE->REFRESH(
*         S_STABLE     = LS_STBL
*         REFRESH_MODE = IF_SALV_C_REFRESH=>SOFT ).

       PERFORM EXIT_PROGRAM.

     WHEN 'DELE'.
*...User's confirmation
*       LF_QUESTION = TEXT-S13.
*       CALL FUNCTION 'POPUP_TO_CONFIRM'
*         EXPORTING
*           TITLEBAR              = TEXT-S03
*           TEXT_QUESTION         = LF_QUESTION
*           DISPLAY_CANCEL_BUTTON = SPACE
*         IMPORTING
*           ANSWER                = LF_ANSWER.
*
*       IF LF_ANSWER EQ '1'.
       PERFORM F_DELETE.

       GR_TABLE->REFRESH(
         S_STABLE     = LS_STBL
         REFRESH_MODE = IF_SALV_C_REFRESH=>SOFT ).
       PERFORM EXIT_PROGRAM.

     WHEN 'SAVE'.
       PERFORM F_CHECK_DOCUMENT_COMPLETION.
       IF LINES( GT_ACCA ) EQ 0.
         RETURN.
       ENDIF.

       LF_QUESTION = |{ TEXT-S12 }?|.
*...User's confirmation
       CALL FUNCTION 'POPUP_TO_CONFIRM'
         EXPORTING
           TITLEBAR              = TEXT-S03
           TEXT_QUESTION         = LF_QUESTION
           DISPLAY_CANCEL_BUTTON = SPACE
         IMPORTING
           ANSWER                = LF_ANSWER.

       IF LF_ANSWER EQ '1'.
         PERFORM F_PREPARE_DOCUMENT_ITEM.
         PERFORM F_PREPARE_DOCUMENT_SAVE.
         PERFORM F_TRANSFORM_POSTITEM.

         GR_TABLE->REFRESH(
           S_STABLE     = LS_STBL
           REFRESH_MODE = IF_SALV_C_REFRESH=>SOFT ).
*
         PERFORM EXIT_PROGRAM.

       ENDIF.

     WHEN OTHERS.
   ENDCASE.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_period_check
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM F_PERIOD_CHECK .

*..add duration(month) to start date
   DATA: LF_ENDDA  TYPE ENDDA      ##NEEDED,
         LT_PERIOD TYPE TT_PERIOD  ##NEEDED.

   CLEAR: LF_ENDDA.
   FREE : LT_PERIOD.

   IF ZSDSFIS074-BEGDA GT ZSDSFIS074-ENDDA.
     MESSAGE TEXT-M02 TYPE 'E'.
   ENDIF.

   PERFORM F_GET_PERIOD_TAB TABLES LT_PERIOD
                            CHANGING LF_ENDDA.

   IF ZSDSFIS074-ENDDA NE LF_ENDDA.
     ZSDSFIS074-ENDDA = LF_ENDDA.
   ENDIF.

   PERFORM F_UPDATE_ALV_ITEM TABLES LT_PERIOD.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_status_9000
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
 FORM F_STATUS_9000 .

   DATA: LT_EXCL_TAB  TYPE STANDARD TABLE OF SY-UCOMM ##NEEDED.
   FREE LT_EXCL_TAB.

   SET TITLEBAR '9000' OF PROGRAM SY-CPROG WITH GF_HEADER.

   IF GF_MODE EQ GC_DISP.
     APPEND 'UPD' TO LT_EXCL_TAB.
     APPEND 'SAVE' TO LT_EXCL_TAB.
     APPEND 'DELE' TO LT_EXCL_TAB.
   ELSEIF GF_MODE EQ GC_EDIT.
     APPEND 'UPD' TO LT_EXCL_TAB.
     APPEND 'SAVE' TO LT_EXCL_TAB.
   ELSE.
     APPEND 'UPD' TO LT_EXCL_TAB.
     APPEND 'DELE' TO LT_EXCL_TAB.
   ENDIF.

   SET PF-STATUS 'UPD' EXCLUDING LT_EXCL_TAB.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_acc_desc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <L_POSTITEM>_HKONT_DR
*&      <-- <L_POSTITEM>_TXT20_DR
*&---------------------------------------------------------------------*
 FORM F_GET_ACC_DESC  USING    UF_HKONT TYPE RACCT
                      CHANGING CH_TXT20 TYPE TXT20.
   CLEAR CH_TXT20.

   IF UF_HKONT IS INITIAL.
     RETURN.
   ENDIF.

   SELECT TXT20 INTO CH_TXT20
         FROM SKAT UP TO 1 ROWS
     WHERE SPRAS EQ SY-LANGU
         AND KTOPL EQ 'RCOA'
         AND SAKNR EQ UF_HKONT.
   ENDSELECT.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_cc_desc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <L_POSTITEM>_KOSTL
*&      <-- <L_POSTITEM>_LTEXT
*&---------------------------------------------------------------------*
 FORM F_GET_CC_DESC  USING    UF_KOSTL TYPE KOSTL
                     CHANGING CH_LTEXT TYPE LTEXT.

   CLEAR CH_LTEXT.

   IF UF_KOSTL IS INITIAL.
     RETURN.
   ENDIF.

   SELECT LTEXT FROM CSKT UP TO 1 ROWS
                 INTO CH_LTEXT
     WHERE SPRAS EQ SY-LANGU
       AND KOKRS EQ '1000'
      AND KOSTL EQ UF_KOSTL
      AND DATBI GE SY-DATUM ORDER BY PRIMARY KEY.
   ENDSELECT.

 ENDFORM.
