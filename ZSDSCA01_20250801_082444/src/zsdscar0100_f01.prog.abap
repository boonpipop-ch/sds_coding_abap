*&---------------------------------------------------------------------*
*& Include          ZSDSCAR0050_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form F_LIST_IFILE
*&---------------------------------------------------------------------*
*& Popup for Input file selection
*&---------------------------------------------------------------------*
*&      <-- P_FILE
*&---------------------------------------------------------------------*
FORM F_LIST_IFILE CHANGING PV_FILENAME TYPE STRING.

  DATA:
    LT_FILE     TYPE  FILETABLE.

  DATA:
    LV_RC     TYPE  I,
    LV_ACTION TYPE  I.

  FIELD-SYMBOLS:
    <LFS_FILE>  TYPE  FILE_TABLE.


* List Local File
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      DEFAULT_EXTENSION       = '*'
*     File Filter format is '<Text>|<Filtering>;<Text>|<Filtering>'
      FILE_FILTER             = 'Excel Files (*.XLS;*.XLSX)|*.XLS;*.XLSX|' ##NO_TEXT
      MULTISELECTION          = SPACE
    CHANGING
      FILE_TABLE              = LT_FILE
      RC                      = LV_RC
      USER_ACTION             = LV_ACTION
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    RETURN.
  ENDIF.

* Read Selected
  IF NOT ( LV_ACTION IS INITIAL AND
           LV_RC     EQ 1 ).
    RETURN.
  ENDIF.

  READ TABLE LT_FILE ASSIGNING <LFS_FILE>
                     INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Output
  PV_FILENAME = <LFS_FILE>-FILENAME.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_read_input_file
*&---------------------------------------------------------------------*
*& Read input file into Raw internal table
*&---------------------------------------------------------------------*
FORM F_READ_INPUT_FILE CHANGING PT_RAW TYPE GTTY_RAW
                                 PS_MESSG TYPE  GTY_MESSG.

* Show Progress
* Text-p01 : Reading Input file. . .
  MC_SHOW_PROGRESS 10 TEXT-P01.

  PERFORM F_READ_EXCEL USING P_FILE
                             P_BEGROW
                             P_BEGCOL
                             P_ENDROW
                             P_ENDCOL
                    CHANGING PT_RAW
                             PS_MESSG.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_read_excel
*&---------------------------------------------------------------------*
*  Read Excel file into Raw internal table
*----------------------------------------------------------------------*
FORM F_READ_EXCEL USING PV_IFILE TYPE CLIKE
                          PV_BEGROW  TYPE I
                          PV_BEGCOL  TYPE I
                          PV_ENDROW  TYPE I
                          PV_ENDCOL  TYPE I
                 CHANGING PT_RAW     TYPE GTTY_RAW
                          PS_MESSG   TYPE GTY_MESSG.

  DATA:
    LT_TAB   TYPE  STANDARD TABLE OF ZSDSCAS016.

  DATA:
    LS_RAW       TYPE  GTY_RAW.

  DATA:
    LV_FILENAME TYPE  STRING,
    LV_BEGROW   TYPE  I,
    LV_BEGCOL   TYPE  I,
    LV_ROW      TYPE  I,
    LV_COL      TYPE  I.

  FIELD-SYMBOLS:
    <LFS_TAB>  TYPE  ZSDSCAS016.


* Initialize Output
  CLEAR: PT_RAW.
  CLEAR  : PS_MESSG.

* Assign File name
  LV_FILENAME = PV_IFILE.
  LV_BEGCOL = PV_BEGCOL.
  LV_BEGROW = PV_BEGROW.


  CALL FUNCTION 'Z_SDSCA_EXCEL_TO_ITAB'
    EXPORTING
      FILENAME                = LV_FILENAME
      I_BEGIN_COL             = LV_BEGCOL
      I_BEGIN_ROW             = LV_BEGROW
      I_END_COL               = PV_ENDCOL
      I_END_ROW               = PV_ENDROW
    TABLES
      INTERN                  = LT_TAB
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
*   Error: Error during read excel file.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '021'.
    MESSAGE ID PS_MESSG-MSGID
            TYPE PS_MESSG-MSGTY
            NUMBER PS_MESSG-MSGNO
            INTO   PS_MESSG-MSGTX.
    RETURN.
  ENDIF.

  SORT LT_TAB BY ROW ASCENDING
                 COL ASCENDING.

  LOOP AT LT_TAB ASSIGNING <LFS_TAB>.

*   -----------
*   On New Row
*   -----------
    IF <LFS_TAB>-ROW NE LV_ROW.

      IF LV_ROW IS NOT INITIAL.
        INSERT LS_RAW INTO TABLE PT_RAW.
      ENDIF.

*     Initialize New Line
      LV_ROW = <LFS_TAB>-ROW.
      LV_COL = 1.
      CLEAR LS_RAW.

    ENDIF.

*   -----------
*   Add Blank Cell
*   -----------
    WHILE LV_COL LT <LFS_TAB>-COL.
      IF LV_COL GT 1.
        CONCATENATE LS_RAW-TLINE GC_SPLIT
               INTO LS_RAW-TLINE.
      ENDIF.
      LV_COL = LV_COL + 1.
    ENDWHILE.

*   -----------
*   Assign column value
*   -----------
    IF LV_COL EQ 1.
      LS_RAW-TLINE = <LFS_TAB>-VALUE.
    ELSE.
      CONCATENATE LS_RAW-TLINE <LFS_TAB>-VALUE
             INTO LS_RAW-TLINE
        SEPARATED BY GC_SPLIT.
    ENDIF.

    LV_COL = LV_COL + 1.

*   Insert last line
    AT LAST.
      INSERT LS_RAW INTO TABLE PT_RAW.
    ENDAT.

  ENDLOOP.


ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_top_of_page_1
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_1 USING PV_DYNDOC_ID TYPE REF TO CL_DD_DOCUMENT. "#EC CALLED

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '30%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '70%'.

  DATA:
    LV_TITLE     TYPE  TEXT60,
    LV_TEXT      TYPE  SDYDO_TEXT_ELEMENT,
    LV_TEMP1     TYPE  TEXT50,
    LV_TEMP2     TYPE  TEXT50,
    LREF_TABLE   TYPE  REF TO CL_DD_TABLE_ELEMENT,
    LREF_COL_KEY TYPE  REF TO CL_DD_AREA,
    LREF_COL_VAL TYPE  REF TO CL_DD_AREA.

* Create table
  CALL METHOD PV_DYNDOC_ID->ADD_TABLE
    EXPORTING
      NO_OF_COLUMNS = 2
      BORDER        = '0'
      WIDTH         = '50%'
    IMPORTING
      TABLE         = LREF_TABLE.
* Get Column Element
  CALL METHOD LREF_TABLE->ADD_COLUMN
    EXPORTING
      WIDTH  = LC_SIZE_KEY
    IMPORTING
      COLUMN = LREF_COL_KEY.
* Get Column Element
  CALL METHOD LREF_TABLE->ADD_COLUMN
    EXPORTING
      WIDTH  = LC_SIZE_VAL
    IMPORTING
      COLUMN = LREF_COL_VAL.
* Set Key column style
  CALL METHOD LREF_TABLE->SET_COLUMN_STYLE
    EXPORTING
      COL_NO       = 1
      SAP_EMPHASIS = CL_DD_AREA=>STRONG.

*-----------------------
* Add value in Line1
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h01 : Program:
  LV_TEXT = TEXT-H01.
  PERFORM F_DETERMINE_TITLE CHANGING LV_TITLE.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.
  CONCATENATE SY-REPID LV_TITLE
         INTO LV_TEXT
    SEPARATED BY '-'.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.

*-----------------------
* Add value in Line2
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h02 : Processing Date/Time:
  LV_TEXT = TEXT-H02.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.
  WRITE SY-DATUM TO LV_TEMP1.
  WRITE SY-UZEIT TO LV_TEMP2.
  CONCATENATE LV_TEMP1 LV_TEMP2
         INTO LV_TEXT
    SEPARATED BY SPACE.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.

*-----------------------
* Add value in Line3
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h03 : System/Client:
  LV_TEXT = TEXT-H03.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.
  CONCATENATE SY-SYSID SY-MANDT
         INTO LV_TEXT
    SEPARATED BY '/'.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.

*-----------------------
* Add value in Line4
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h04 : Process By:
  LV_TEXT = TEXT-H04.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.
  LV_TEXT = SY-UNAME.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.

*-----------------------
* Add value in Line 5
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h05 : Total Records:
  LV_TEXT = TEXT-H05.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.
  WRITE GS_SUM-TOTAL TO LV_TEXT LEFT-JUSTIFIED.
  CONDENSE LV_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.

*-----------------------
* Add value in Line 6
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h06 : Success Records:
  LV_TEXT = TEXT-H06.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.
  WRITE GS_SUM-SUCCS TO LV_TEXT LEFT-JUSTIFIED.
  CONDENSE LV_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LV_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_POSITIVE.

*-----------------------
* Add value in Line 7
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h07 : Failed Records:
  LV_TEXT = TEXT-H07.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.
  WRITE GS_SUM-ERROR TO LV_TEXT LEFT-JUSTIFIED.
  CONDENSE LV_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LV_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_NEGATIVE.

*-----------------------
* Add value in Line 8
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
  IF CB_TEST IS INITIAL.
*   Text-h09 : Real Run
    LV_TEXT = TEXT-H09.
  ELSE.
*   Text-h08 : Test Run
    LV_TEXT = TEXT-H08.
  ENDIF.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT      = LV_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_TOTAL.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_print_top_of_page_1
*----------------------------------------------------------------------*
*       Print Top of Page
*----------------------------------------------------------------------*
FORM F_PRINT_TOP_OF_PAGE_1.                                 "#EC CALLED

  DATA:
    LV_TITLE TYPE  TEXT50,
    LV_COL01 TYPE  I VALUE 25,
    LV_COL02 TYPE  I VALUE 35,
    LV_TEXT  TYPE  TEXT50,
    LV_TEMP1 TYPE  TEXT50,
    LV_TEMP2 TYPE  TEXT50.


  PERFORM F_DETERMINE_TITLE CHANGING LV_TITLE.
  CONCATENATE SY-REPID LV_TITLE
         INTO LV_TEXT
    SEPARATED BY '-'.
* Text-h01 : Program:
  WRITE AT: /1(LV_COL01)  TEXT-H01 INTENSIFIED ON NO-GAP,
            (LV_COL02)    LV_TEXT NO-GAP.

  WRITE SY-DATUM TO LV_TEMP1.
  WRITE SY-UZEIT TO LV_TEMP2.
  CONCATENATE LV_TEMP1 LV_TEMP2
         INTO LV_TEXT
    SEPARATED BY SPACE.
* Text-h02 : Processing Date/Time:
  WRITE AT: /1(LV_COL01)  TEXT-H02 INTENSIFIED ON NO-GAP,
            (LV_COL02)    LV_TEXT NO-GAP.

  CONCATENATE SY-SYSID SY-MANDT
         INTO LV_TEXT
    SEPARATED BY '/'.
* Text-h03 : System/Client:
  WRITE AT: /1(LV_COL01)  TEXT-H03 INTENSIFIED ON NO-GAP,
            (LV_COL02)    LV_TEXT NO-GAP.

* Text-h04 : Process By:
  WRITE AT: /1(LV_COL01)  TEXT-H04 INTENSIFIED ON NO-GAP,
            (LV_COL02)    SY-UNAME NO-GAP.

* Text-h05 : Total Records:
  WRITE GS_SUM-TOTAL TO LV_TEXT LEFT-JUSTIFIED.
  CONDENSE LV_TEXT NO-GAPS.
  WRITE AT: /1(LV_COL01)  TEXT-H05 INTENSIFIED ON NO-GAP,
            (10)    LV_TEXT NO-GAP.

* Text-h06 : Success Records:
  WRITE GS_SUM-SUCCS TO LV_TEXT LEFT-JUSTIFIED.
  CONDENSE LV_TEXT NO-GAPS.
  WRITE AT: /1(LV_COL01)  TEXT-H06 INTENSIFIED ON NO-GAP,
            (10)    LV_TEXT NO-GAP COLOR COL_POSITIVE.

* Text-h07 : Failed Records:
  WRITE GS_SUM-ERROR TO LV_TEXT LEFT-JUSTIFIED.
  CONDENSE LV_TEXT NO-GAPS.
  WRITE AT: /1(LV_COL01)  TEXT-H07 INTENSIFIED ON NO-GAP,
            (10)    LV_TEXT NO-GAP COLOR COL_NEGATIVE.

  IF CB_TEST IS INITIAL.
*   Text-h09 : Real Run
    LV_TEXT = TEXT-H09.
  ELSE.
*   Text-h08 : Test Run
    LV_TEXT = TEXT-H08.
  ENDIF.
  WRITE AT: /1(20) LV_TEXT CENTERED COLOR COL_TOTAL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_determine_title
*&---------------------------------------------------------------------*
*& Determine Title
*&---------------------------------------------------------------------*
*&      <-- LV_TITLE
*&---------------------------------------------------------------------*
FORM F_DETERMINE_TITLE CHANGING PV_TITLE TYPE CLIKE.

  IF  RB_TYPE1 IS NOT INITIAL .
****** Vendor *****
*    PERFORM f_upload_excel.
  ELSE.

***** Customer *****
    CASE GC_TRUE.
      WHEN RB_TMP21.
*     Text-a01: Upload Customer General + Company Code View
        PV_TITLE = TEXT-A01.

      WHEN RB_TMP22.
*     Text-a02: Upload Customer Branch code
        PV_TITLE = TEXT-A02.

      WHEN RB_TMP23.
*     Text-a03: Upload Customer Tax and Address
        PV_TITLE = TEXT-A03.

      WHEN RB_TMP24.
*     Text-a04: Upload Customer Email
        PV_TITLE = TEXT-A04.
    ENDCASE.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_genc
*&---------------------------------------------------------------------*
*& Get program constants from ZTBC_GENC
*&---------------------------------------------------------------------*
*FORM F_GET_GENC .
*  DATA: lo_const TYPE REF TO zclbc_const.

*  IF lo_const IS NOT BOUND.
*    CREATE OBJECT lo_const
*      EXPORTING
*        im_repid = sy-repid.
*  ENDIF.
*
*  CALL METHOD lo_const->create_range
*    EXPORTING
*      im_const    = 'RLTYP'
*    CHANGING
*      ch_criteria = gr_rltyp.
*
*  CALL METHOD lo_const->get_single_value
*    EXPORTING
*      im_const = 'VALID_FROM'
*    CHANGING
*      ch_value = gv_validfrom.
*ENDFORM.
