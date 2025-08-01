*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0330_F01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
 FORM USER_OK_TC USING    P_TC_NAME TYPE DYNFNAM
                          P_TABLE_NAME TYPE ANY
                          P_MARK_NAME TYPE ANY
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
     WHEN 'APPN'.
       PERFORM FCODE_APPEND_ROW USING    P_TC_NAME
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
               USING    P_TC_NAME           TYPE DYNFNAM
                        P_TABLE_NAME        TYPE ANY     .

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
*&      Form  FCODE_APPEND_ROW                                         *
*&---------------------------------------------------------------------*
 FORM FCODE_APPEND_ROW
               USING    P_TC_NAME           TYPE DYNFNAM
                        P_TABLE_NAME        TYPE ANY    .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA L_LINES_NAME       TYPE FELD-NAME.
   DATA L_SELLINE          TYPE SY-STEPL.
   DATA L_LASTLINE         TYPE I ##NEEDED.
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

*&SPWIZARD: insert initial line                                        *
   APPEND INITIAL LINE TO <TABLE> .
   <TC>-LINES = <TC>-LINES + 1.

 ENDFORM.                              " FCODE_APPEND_ROW
*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
 FORM FCODE_DELETE_ROW
               USING    P_TC_NAME     TYPE DYNFNAM
                        P_TABLE_NAME  TYPE ANY
                        P_MARK_NAME   TYPE ANY.

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
*   DESCRIBE TABLE <TABLE> LINES <TC>-LINES.
   <TC>-LINES = LINES( <TABLE> ).

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
 FORM COMPUTE_SCROLLING_IN_TC USING    P_TC_NAME TYPE ANY
                                       P_OK TYPE ANY.
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
*   <TC>-LINES = lines( gt_input ).

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
 FORM FCODE_TC_MARK_LINES USING P_TC_NAME TYPE ANY
                                P_TABLE_NAME TYPE ANY
                                P_MARK_NAME TYPE ANY.
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
 FORM FCODE_TC_DEMARK_LINES USING P_TC_NAME TYPE ANY
                                  P_TABLE_NAME TYPE ANY
                                  P_MARK_NAME TYPE ANY.
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
*---------------------END WIZARD GENERATED -----------------------------
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

*&---------------------------------------------------------------------*
*& Form F_SEL_SCRN_VALIDATE
*&---------------------------------------------------------------------*
 FORM F_SEL_SCRN_VALIDATE .
   IF P_IMPKEY IS INITIAL. "online execution to reprocess
     IF RB_SO = ABAP_TRUE
     AND S_VBELN[] IS INITIAL
     AND S_APVDT[] IS INITIAL .
       MESSAGE E000(ZSDSCA01) WITH 'Salses order/Approved date is required'(m01).
     ENDIF.
   ENDIF.
 ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DISPLAY_REPORT
*&---------------------------------------------------------------------*
 FORM F_DISPLAY_REPORT  USING  UT_REP TYPE TT_REP.
* Set Container name
   GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
   GF_ALV_HEADER_1 = SPACE.

* ALV Layout
   PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                 GS_VARIANT_1
                                 GS_PRINT_1.

* Assign Output Data
* Assign Size
   GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
   GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
   ASSIGN UT_REP TO <G_LIST_1>.              "#EC CI_FLDEXT_OK[2610650]

* Build Field cat
   PERFORM F_ALV_BUILD_FIELDCAT  USING UT_REP
                                 CHANGING GT_FIELDCAT_1.

* Call ALV Screen
   CALL SCREEN 9000.
 ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
 FORM F_ALV_BUILD_FIELDCAT  USING UT_DATA TYPE  TT_REP
                            CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.
   PERFORM F_ALV_GEN_FIELDCAT    USING UT_DATA
                                   CHANGING CT_FIELDCAT.

   PERFORM F_ALV_MODIFY_FC   CHANGING CT_FIELDCAT.
 ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_GEN_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
 FORM F_ALV_GEN_FIELDCAT USING UT_DATA TYPE ANY TABLE
                        CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.
   DATA: LF_COLUMNS      TYPE REF TO CL_SALV_COLUMNS_TABLE,
         LF_AGGREGATIONS TYPE REF TO CL_SALV_AGGREGATIONS,
         LF_SALV_TABLE   TYPE REF TO CL_SALV_TABLE,
         LF_TABLE        TYPE REF TO DATA.

   FIELD-SYMBOLS: <TABLE> TYPE ANY TABLE.

* create unprotected table from import data
   CREATE DATA LF_TABLE LIKE  UT_DATA.
   ASSIGN LF_TABLE->* TO <TABLE>.

*...New ALV Instance ...............................................
   TRY.
       CL_SALV_TABLE=>FACTORY(
         EXPORTING
           LIST_DISPLAY = ABAP_FALSE
         IMPORTING
           R_SALV_TABLE = LF_SALV_TABLE
         CHANGING
           T_TABLE      = <TABLE> ).
     CATCH CX_SALV_MSG ##NO_HANDLER.
   ENDTRY.
   LF_COLUMNS  = LF_SALV_TABLE->GET_COLUMNS( ).
   LF_AGGREGATIONS = LF_SALV_TABLE->GET_AGGREGATIONS( ).

   CALL METHOD CL_SALV_CONTROLLER_METADATA=>GET_LVC_FIELDCATALOG
     EXPORTING
       R_COLUMNS      = LF_COLUMNS
       R_AGGREGATIONS = LF_AGGREGATIONS
     RECEIVING
       T_FIELDCATALOG = CT_FIELDCAT[].

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_MODIFY_FC_NEW
*&---------------------------------------------------------------------*
 FORM F_ALV_MODIFY_FC  CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.

   LOOP AT CT_FIELDCAT ASSIGNING FIELD-SYMBOL(<FCAT>) .
     CASE <FCAT>-FIELDNAME.
       WHEN 'QT_VBELN'.
         %FCAT 'Quot no.'(c01).
       WHEN 'QT_POSNR'.
         %FCAT 'Quot item'(c02).
       WHEN 'SO_VBELN'.
         %FCAT 'SO No.'(c03).
       WHEN 'SO_POSNR'.
         %FCAT 'SO Item'(c04).
       WHEN 'SO_ETENR'.
         %FCAT 'Sched.Line'(c05).
       WHEN 'SCH_STATUS'.
         %FCAT 'Sched. Stat'(c06).
       WHEN 'MSGTY'.
         %FCAT 'Message type'(c07).
       WHEN 'MESSAGE_CHK'.
         %FCAT 'Message'(c08).
     ENDCASE.
   ENDLOOP.
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
   CLEAR: CS_LAYOUT, CS_VARIANT, CS_PRINT.

* determine layout
   CS_LAYOUT-SEL_MODE   = 'A'.
   CS_LAYOUT-CWIDTH_OPT = ABAP_TRUE.
   CS_LAYOUT-ZEBRA      = ABAP_TRUE.

* For Variant Saving
   CS_VARIANT-REPORT  = SY-REPID.

   CS_PRINT-NO_COLWOPT = SPACE.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_PROPERTIES
*&---------------------------------------------------------------------*
 FORM F_SET_PROPERTIES .
   IF P_IMPKEY IS INITIAL. "online execution to reprocess
     LOOP AT SCREEN.
       IF SCREEN-GROUP1 = 'BG'.
         SCREEN-ACTIVE = '0'.
         MODIFY SCREEN.
       ELSEIF SCREEN-GROUP1 = 'SO'.
         IF RB_SO = ABAP_FALSE.
           SCREEN-ACTIVE = '0'.
           MODIFY SCREEN.
         ELSE.
           SCREEN-ACTIVE = '1'.
           MODIFY SCREEN.
         ENDIF.
       ENDIF.
     ENDLOOP.
   ENDIF.
 ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SEND_INTERFACE
*&---------------------------------------------------------------------*
 FORM F_SEND_INTERFACE  CHANGING CT_INPUT TYPE TT_INPUT
                                  CT_REP TYPE TT_REP.
   DATA: LT_009        TYPE TT_009,
         LT_VBFA_SO_QT TYPE TT_VBFA_SO_QT.

   PERFORM F_VALIDATE_VBAK CHANGING CT_INPUT.
   IF CT_INPUT[] IS INITIAL.
     RETURN.
   ENDIF.
   PERFORM F_SELECT_ZSDSSDT009 USING CT_INPUT
                               CHANGING LT_009.

   PERFORM F_SELECT_VBFA_SO_QT USING CT_INPUT
                               CHANGING LT_VBFA_SO_QT.
   PERFORM F_PREPARE_AND_SEND_INTERFACE USING CT_INPUT
                                              LT_009
                                              LT_VBFA_SO_QT
                                         CHANGING CT_REP.

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_input_data
*&---------------------------------------------------------------------*
 FORM F_VALIDATE_INPUT_DATA  USING  US_INPUT TYPE ZSDSSDS102.
   IF GF_OKCODE <> 'ZSEND'.
     RETURN.
   ENDIF.
   IF US_INPUT-VBELN IS INITIAL
   AND US_INPUT-POSNR IS INITIAL
   AND US_INPUT-ETENR IS INITIAL.
     RETURN.
   ENDIF.
   IF US_INPUT-VBELN IS INITIAL.
     "&1 is required
     MESSAGE E028(ZSDSSD01) WITH 'Sales Doc'(m02).
   ENDIF.
   IF US_INPUT-POSNR IS INITIAL.
     "&1 is required
     MESSAGE E028(ZSDSSD01) WITH 'Item'(m03).
   ENDIF.
   IF US_INPUT-ETENR IS INITIAL.
     "&1 is required
     MESSAGE E028(ZSDSSD01) WITH 'Schedule line'(m04).
   ENDIF.
   SELECT A~VBELN,
          POSNR,
          ETENR
   UP TO 1 rows
   INTO @DATA(LS_009)
   FROM ZSDSSDT009 AS A INNER JOIN VBAK  AS B
   ON A~VBELN = B~VBELN
   WHERE A~VBELN = @US_INPUT-VBELN
   AND   POSNR = @US_INPUT-POSNR
   AND   ETENR = @US_INPUT-ETENR
   AND   MGR_APPV_STAT = @GF_MGR_APPV_STAT
   AND   AUART IN @GRT_AUART ##NEEDED.
   endselect.
   IF SY-SUBRC <> 0.
     " Invalid SO &1/ Item &2/ Schedule line &3
     MESSAGE E029(ZSDSSD01) WITH US_INPUT-VBELN US_INPUT-POSNR US_INPUT-ETENR.
   ENDIF.
 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_ZSDSSDT009
*&---------------------------------------------------------------------*
 FORM F_SELECT_ZSDSSDT009  USING    UT_INPUT TYPE TT_INPUT
                           CHANGING CT_009 TYPE TT_009.
   IF UT_INPUT IS INITIAL.
     RETURN.
   ENDIF.
   DATA: LR_APPV_STAT TYPE RANGE OF ZSDSSDT009-MGR_APPV_STAT.
   IF P_IMPKEY IS NOT INITIAL.  "submitted execution
     INSERT VALUE #( SIGN = 'I'
                    OPTION = 'EQ'
                    LOW = GF_MGR_APPV_STAT
                    HIGH = '' )
           INTO  TABLE LR_APPV_STAT.
   ENDIF.


   SELECT VBELN  ,
          POSNR  ,
          ETENR  ,
          MGR_APPV_STAT
   INTO TABLE @CT_009
   FROM ZSDSSDT009
   FOR ALL ENTRIES IN @UT_INPUT
   WHERE VBELN = @UT_INPUT-VBELN
   AND   POSNR = @UT_INPUT-POSNR
   AND   ETENR = @UT_INPUT-ETENR
   AND   MGR_APPV_STAT IN @LR_APPV_STAT.
 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_vbfa_so_qt
*&---------------------------------------------------------------------*
 FORM F_SELECT_VBFA_SO_QT  USING    UT_INPUT TYPE TT_INPUT
                           CHANGING CT_VBFA_SO_QT TYPE TT_VBFA_SO_QT.
   IF UT_INPUT[] IS INITIAL.
     RETURN.
   ENDIF.
   SELECT  VBFA~VBELN AS SO_VBELN,
           VBFA~POSNN AS SO_POSNR,
           VBFA~VBELV AS QT_VBELN,
           VBFA~POSNV AS QT_POSNR,
           VBAK~BNAME AS QT_BNAME
   INTO TABLE @CT_VBFA_SO_QT
   FROM VBFA INNER JOIN  VBAK
   ON VBFA~VBELV = VBAK~VBELN
   FOR ALL ENTRIES IN  @UT_INPUT
   WHERE VBFA~VBELN   = @UT_INPUT-VBELN
   AND   VBFA~POSNN   = @UT_INPUT-POSNR
   AND   VBFA~VBTYP_V = @GC_VBTYP_QT
   AND   VBFA~POSNV <> 0 .

 ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PREPARE_AND_SEND_INTERFACE
*&---------------------------------------------------------------------*
 FORM F_PREPARE_AND_SEND_INTERFACE  USING    UT_INPUT TYPE TT_INPUT
                                             UT_009 TYPE TT_009
                                             UT_VBFA_SO_QT TYPE TT_VBFA_SO_QT
                                    CHANGING  CT_REP TYPE TT_REP.
   DATA: LS_DATA        TYPE ZSDSSDS041,
         LS_QUOTITEM    TYPE ZSDSSDS042,
         LS_SCHEDULE    TYPE ZSDSSDS043,
         LS_VBFA_SO_QT  TYPE TS_VBFA_SO_QT,
         LS_REP         TYPE TS_REP,
         LS_REQUEST_KEY TYPE ZSDSCAS005,
         LF_MESSAGE_REP TYPE TS_REP-MESSAGE_CHK,
         LF_MESSAGE     TYPE ZSDSDE_REST_MESSAGE.


   DATA: LT_INPUT    TYPE TT_INPUT.

   LT_INPUT = UT_INPUT.
   SORT LT_INPUT BY VBELN POSNR ETENR .
   DELETE ADJACENT DUPLICATES FROM LT_INPUT COMPARING VBELN POSNR ETENR.

   LOOP AT LT_INPUT ASSIGNING FIELD-SYMBOL(<L_INPUT>).
     AT NEW VBELN.
       CLEAR: LS_DATA,
              LS_REP,
              LF_MESSAGE.
     ENDAT.
     AT NEW POSNR.
       CLEAR:  LS_QUOTITEM.
     ENDAT.
     CLEAR: LS_SCHEDULE,
            LF_MESSAGE_REP,
            LS_VBFA_SO_QT.
     READ TABLE UT_VBFA_SO_QT INTO LS_VBFA_SO_QT WITH KEY SO_VBELN = <L_INPUT>-VBELN
                                                          SO_POSNR = <L_INPUT>-POSNR.
     IF SY-SUBRC <> 0.
       LF_MESSAGE_REP = |Not found quotation related for SO/Item { <L_INPUT>-VBELN }/{ <L_INPUT>-POSNR }| ##NO_TEXT.
       PERFORM F_APPEND_LOG_REPORT USING <L_INPUT>
                                         LS_VBFA_SO_QT
                                         GC_MSGTY_ERROR
                                         LF_MESSAGE_REP
                                   CHANGING CT_REP.
       PERFORM F_CONCAT_MESSAGE  USING LF_MESSAGE_REP
                                 CHANGING LF_MESSAGE.
     ELSE.
       LS_DATA-SAPQUOTATIONNO  = LS_VBFA_SO_QT-QT_VBELN.
       LS_DATA-SFQUOTATIONNO   = LS_VBFA_SO_QT-QT_BNAME.

       LS_QUOTITEM-ITEMNO = LS_VBFA_SO_QT-QT_POSNR.
       READ TABLE UT_009 INTO DATA(LS_009) WITH KEY VBELN = <L_INPUT>-VBELN
                                                    POSNR = <L_INPUT>-POSNR
                                                    ETENR = <L_INPUT>-ETENR ##NEEDED.
       IF SY-SUBRC <> 0.
         IF P_IMPKEY IS INITIAL. "online execution to reprocess
           LF_MESSAGE_REP =
           |Not found schedule line { <L_INPUT>-VBELN }/{ <L_INPUT>-POSNR }/{ <L_INPUT>-ETENR }| ##NO_TEXT.
         ELSE.                  "submitted execution
           IF RB_APPR = ABAP_TRUE.
             LF_MESSAGE_REP =
             |Not found schedule line to be approved { <L_INPUT>-VBELN }/{ <L_INPUT>-POSNR }/{ <L_INPUT>-ETENR }| ##NO_TEXT.
           ELSEIF RB_CANC = ABAP_TRUE.
             LS_REP-MESSAGE_CHK =
             |Not found schedule line to be cancelled approval { <L_INPUT>-VBELN }/{ <L_INPUT>-POSNR }/{ <L_INPUT>-ETENR }| ##NO_TEXT.
           ENDIF.
         ENDIF.
         PERFORM F_APPEND_LOG_REPORT USING <L_INPUT>
                                           LS_VBFA_SO_QT
                                           GC_MSGTY_ERROR
                                           LF_MESSAGE_REP
                                     CHANGING CT_REP.
         PERFORM F_CONCAT_MESSAGE  USING LF_MESSAGE_REP
                                   CHANGING LF_MESSAGE.
       ELSE.
         LS_SCHEDULE-SCHEDULELINENUMBER = <L_INPUT>-ETENR.
         IF P_IMPKEY IS NOT INITIAL. "submitted execution
           LS_SCHEDULE-STATUS  = GF_SCHEDULE_STATUS.
         ELSE.                       "online execution to reprocess
           IF LS_009-MGR_APPV_STAT = 'Y'.
             LS_SCHEDULE-STATUS  = GC_APPROVED.
           ELSE.
             LS_SCHEDULE-STATUS  = GC_NOT_APPROVED.
           ENDIF.
         ENDIF.
         LS_SCHEDULE-SONUMBER  = <L_INPUT>-VBELN.
         LS_SCHEDULE-SOITEM  = <L_INPUT>-POSNR.
         APPEND LS_SCHEDULE TO LS_QUOTITEM-SCHEDULELINE.
         PERFORM F_APPEND_LOG_REPORT USING <L_INPUT>
                                           LS_VBFA_SO_QT
                                           ''
                                           LF_MESSAGE_REP
                                     CHANGING CT_REP.
       ENDIF.
     ENDIF.
     AT END OF POSNR.
       APPEND LS_QUOTITEM TO LS_DATA-QUOTEITEM.
     ENDAT.
     AT END OF VBELN.
       IF P_TEST = ABAP_FALSE.
         IF LF_MESSAGE IS NOT INITIAL.
           PERFORM F_ADD_LOG_REST_INTF USING <L_INPUT>-VBELN
                                             LF_MESSAGE
                                       CHANGING LS_REQUEST_KEY.
           LS_REP-REQNO = LS_REQUEST_KEY-REQNO.
           MODIFY CT_REP FROM LS_REP TRANSPORTING REQNO WHERE SO_VBELN = <L_INPUT>-VBELN .
         ELSE.
           PERFORM F_CALL_REST_INTF USING LS_DATA
                                          <L_INPUT>-VBELN
                                    CHANGING LS_REP.
           MODIFY CT_REP FROM LS_REP TRANSPORTING REQNO
                                                  RESP_STATUS
                                                  RESP_MESSAGE
                                                  HTTP_CODE
                                                  HTTP_REASON
           WHERE SO_VBELN = <L_INPUT>-VBELN .
         ENDIF.
       ENDIF.
     ENDAT.
   ENDLOOP.
 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_init_select
*&---------------------------------------------------------------------*
 FORM F_INIT_SELECT .
   IF P_IMPKEY IS NOT INITIAL. "submiited execution
     CASE ABAP_TRUE.
       WHEN  RB_APPR.
         GF_MGR_APPV_STAT = 'Y'.
         GF_SCHEDULE_STATUS = GC_APPROVED.
       WHEN  RB_CANC.
         GF_MGR_APPV_STAT = ''.
         GF_SCHEDULE_STATUS = GC_NOT_APPROVED.
     ENDCASE.
   ENDIF.
   CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
     EXPORTING
       IF_REPID = GC_REPID
       IF_PARAM = 'ORDER_TYPE'
     IMPORTING
       ET_RANGE = GRT_AUART.
 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_append_log_report
*&---------------------------------------------------------------------*
 FORM F_APPEND_LOG_REPORT  USING    US_INPUT TYPE ZSDSSDS102
                                    US_VBFA_SO_QT TYPE TS_VBFA_SO_QT
                                    US_MSGTY TYPE TS_REP-MSGTY
                                    US_MESSAGE TYPE TS_REP-MESSAGE_CHK
                           CHANGING CT_REP TYPE TT_REP.
   DATA: LS_REP TYPE TS_REP.
   LS_REP-QT_VBELN = US_VBFA_SO_QT-QT_VBELN.
   LS_REP-QT_POSNR = US_VBFA_SO_QT-QT_POSNR.
   LS_REP-SO_VBELN = US_INPUT-VBELN.
   LS_REP-SO_POSNR = US_INPUT-POSNR.
   LS_REP-SO_ETENR = US_INPUT-ETENR.
   LS_REP-SCH_STATUS = GF_SCHEDULE_STATUS.
   LS_REP-MSGTY = US_MSGTY.
   LS_REP-MESSAGE_CHK = US_MESSAGE.
   APPEND LS_REP TO CT_REP.
 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_concat_message
*&---------------------------------------------------------------------*
 FORM F_CONCAT_MESSAGE  USING    UF_MESSAGE TYPE ANY
                        CHANGING CF_MESSAGE TYPE ZSDSDE_REST_MESSAGE.
   IF CF_MESSAGE IS INITIAL.
     CF_MESSAGE = UF_MESSAGE.
   ELSE.
     CF_MESSAGE = CF_MESSAGE && |,{ UF_MESSAGE }|.
   ENDIF.
 ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_LOG_REST_INTF
*&---------------------------------------------------------------------*
 FORM F_ADD_LOG_REST_INTF  USING    UF_VBELN TYPE VBAK-VBELN
                                    UF_MESSAGE TYPE  ZSDSDE_REST_MESSAGE
                           CHANGING CS_REQUEST_KEY TYPE ZSDSCAS005.
   DATA: LF_KEYDATA       TYPE ZSDSDE_KEYDATA.
   CLEAR CS_REQUEST_KEY.
   LF_KEYDATA = UF_VBELN.
   CALL METHOD ZCL_SDSCA_REST_INTF_UTILITY=>ADD_LOG_REST_INTF
     EXPORTING
       IF_INTFNO          = GC_INTFNO
       IF_KEYDATA         = LF_KEYDATA
       IF_STATUS          = GC_MSGTY_ERROR
       IF_MESSAGE         = UF_MESSAGE
     IMPORTING
       ES_REQUEST_KEY     = CS_REQUEST_KEY
     EXCEPTIONS
       INVALID_STATUS     = 1
       INVALID_INTFNO     = 2
       ERROR_DATA_TO_JSON = 3
       REQNO_ERROR        = 4
       LOG_ERROR          = 5
       ERROR_JSON_TO_DATA = 6
       OTHERS             = 7.
   IF SY-SUBRC <> 0.
     RETURN.
   ENDIF.
 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_CALL_REST_INTF
*&---------------------------------------------------------------------*
 FORM F_CALL_REST_INTF  USING    US_DATA TYPE ZSDSSDS041
                                 UF_VBELN TYPE VBAK-VBELN
                        CHANGING CS_REP TYPE TS_REP.

   DATA: LF_KEYDATA  TYPE ZSDSDE_KEYDATA,
         LS_REQUEST  TYPE ZSDSSDS041,
         LS_RESPONSE TYPE ZSDSSDS041,
         LS_REQ_KEY  TYPE ZSDSCAS005.
   CLEAR CS_REP .


   LF_KEYDATA = UF_VBELN.
   LS_REQUEST = US_DATA.

   IF P_IMPKEY IS NOT INITIAL. "submitted execution
     IF RB_CANC = ABAP_TRUE.
       LF_KEYDATA = LF_KEYDATA && '|CANC'(D01).
     ENDIF.
   ELSE.                       "online execution to reprocess
     LF_KEYDATA = LF_KEYDATA && '|REPROCESS'(D02).
   ENDIF.

   ZCL_SDSCA_REST_INTF_UTILITY=>CALL_REST_INTF(
     EXPORTING
       IF_INTFNO           = GC_INTFNO
       IS_REQUEST          = LS_REQUEST
       IF_KEYDATA          = LF_KEYDATA
       IF_SHOW_PROCESS_LOG = ''
     IMPORTING
       ES_RESPONSE         = LS_RESPONSE
       ES_REQUEST_KEY      = LS_REQ_KEY
     EXCEPTIONS
       INVALID_INTFNO      = 1
       ERROR_DATA_TO_JSON  = 2
       URL_ERROR           = 3
       SEND_ERROR          = 4
       REQNO_ERROR         = 5
       LOG_ERROR           = 6
       ERROR_JSON_TO_DATA  = 7
       OTHERS              = 8 ).
   IF SY-SUBRC <> 0.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY
             NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2
                  SY-MSGV3 SY-MSGV4
     INTO CS_REP-RESP_MESSAGE.
     CS_REP-RESP_STATUS = GC_MSGTY_ERROR.
   ELSE.
     SELECT SINGLE REQNO,
                   HTTP_CODE,
                   HTTP_REASON
     FROM ZSDSCAT001
     WHERE INTFNO = @LS_REQ_KEY-INTFNO
     AND   REQNO  = @LS_REQ_KEY-REQNO
     AND   GJAHR  = @LS_REQ_KEY-GJAHR
     INTO  ( @CS_REP-REQNO,
             @CS_REP-HTTP_CODE,
             @CS_REP-HTTP_REASON ).

     CS_REP-RESP_STATUS = LS_RESPONSE-RESP_STATUS.
     CS_REP-RESP_MESSAGE = LS_RESPONSE-RESP_MESSAGE.
   ENDIF.
 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_rerun_so_data
*&---------------------------------------------------------------------*
 FORM F_RERUN_SO_DATA  CHANGING GT_INPUT TYPE TT_INPUT.
   SELECT VBELN
          POSNR
          ETENR
   INTO TABLE GT_INPUT
   FROM ZSDSSDT009
   WHERE VBELN IN S_VBELN
   AND   MGR_APPV_DATE IN S_APVDT
   AND   MGR_APPV_BY IN S_APVBY ##TOO_MANY_ITAB_FIELDS.
 ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_vbak
*&---------------------------------------------------------------------*
 FORM F_VALIDATE_VBAK  CHANGING CT_INPUT TYPE TT_INPUT.
   IF CT_INPUT[] IS INITIAL.
     RETURN.
   ENDIF.
   SELECT VBELN,
          AUART
   INTO TABLE @DATA(LT_VBAK)
   FROM VBAK
   FOR ALL ENTRIES IN @CT_INPUT
   WHERE VBELN = @CT_INPUT-VBELN
   AND   AUART IN @GRT_AUART.
   IF SY-SUBRC <> 0.
     CLEAR CT_INPUT[].
   ELSE.
     SORT LT_VBAK BY VBELN.
     LOOP AT CT_INPUT INTO DATA(LS_INPUT).
       READ TABLE LT_VBAK TRANSPORTING NO FIELDS WITH KEY VBELN = LS_INPUT-VBELN.
       IF SY-SUBRC <> 0.
         DELETE CT_INPUT.
       ENDIF.
     ENDLOOP.
   ENDIF.

 ENDFORM.
