FUNCTION Z_SDSFI_UPLOAD_EXCEL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(FILENAME) LIKE  RLGRAP-FILENAME
*"     VALUE(I_BEGIN_COL) TYPE  I
*"     VALUE(I_BEGIN_ROW) TYPE  I
*"     VALUE(I_END_COL) TYPE  I
*"     VALUE(I_END_ROW) TYPE  I
*"  TABLES
*"      INTERN STRUCTURE  ZSDSFIS190
*"----------------------------------------------------------------------

  DATA: EXCEL_TAB     TYPE  TY_T_SENDER.
  DATA: LD_SEPARATOR  TYPE  C.
  DATA: APPLICATION TYPE  OLE2_OBJECT,
        WORKBOOK    TYPE  OLE2_OBJECT,
        RANGE       TYPE  OLE2_OBJECT,
        WORKSHEET   TYPE  OLE2_OBJECT.
  DATA: H_CELL  TYPE  OLE2_OBJECT,
        H_CELL1 TYPE  OLE2_OBJECT.
  DATA:
    LD_RC             TYPE I.
*   Rückgabewert der Methode "clipboard_export     "

* Makro für Fehlerbehandlung der Methods
  DEFINE M_MESSAGE.
    CASE SY-SUBRC.
      WHEN 0.
      WHEN 1.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      WHEN OTHERS. RAISE UPLOAD_OLE.
    ENDCASE.
  END-OF-DEFINITION.


* check parameters
  IF I_BEGIN_ROW > I_END_ROW. RAISE INCONSISTENT_PARAMETERS. ENDIF.
  IF I_BEGIN_COL > I_END_COL. RAISE INCONSISTENT_PARAMETERS. ENDIF.

* Get TAB-sign for separation of fields
  CLASS CL_ABAP_CHAR_UTILITIES DEFINITION LOAD.
  LD_SEPARATOR = CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.

* open file in Excel
  IF APPLICATION-HEADER = SPACE OR APPLICATION-HANDLE = -1.
    CREATE OBJECT APPLICATION 'Excel.Application'.
    M_MESSAGE.
  ENDIF.
  CALL METHOD OF APPLICATION 'Workbooks' = WORKBOOK.
  M_MESSAGE.
  CALL METHOD OF WORKBOOK 'Open' EXPORTING #1 = FILENAME.
  M_MESSAGE.
*  set property of application 'Visible' = 1.
*  m_message.
  GET PROPERTY OF  APPLICATION 'ACTIVESHEET' = WORKSHEET.
  M_MESSAGE.

* mark whole spread sheet
  CALL METHOD OF WORKSHEET 'Cells' = H_CELL
      EXPORTING #1 = I_BEGIN_ROW #2 = I_BEGIN_COL.
  M_MESSAGE.
  CALL METHOD OF WORKSHEET 'Cells' = H_CELL1
      EXPORTING #1 = I_END_ROW #2 = I_END_COL.
  M_MESSAGE.

  CALL METHOD  OF WORKSHEET 'RANGE' = RANGE
                 EXPORTING #1 = H_CELL #2 = H_CELL1.
  M_MESSAGE.
  CALL METHOD OF RANGE 'SELECT'.
  M_MESSAGE.

* copy marked area (whole spread sheet) into Clippboard
  CALL METHOD OF RANGE 'COPY'.
  M_MESSAGE.

* read clipboard into ABAP
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>CLIPBOARD_IMPORT
    IMPORTING
      DATA       = EXCEL_TAB
    EXCEPTIONS
      CNTL_ERROR = 1
*     ERROR_NO_GUI         = 2
*     NOT_SUPPORTED_BY_GUI = 3
      OTHERS     = 4.
  IF SY-SUBRC <> 0.
    MESSAGE A037(ALSMEX).
  ENDIF.

  PERFORM SEPARATED_TO_INTERN_CONVERT TABLES EXCEL_TAB INTERN
                                      USING  LD_SEPARATOR.

* clear clipboard
  REFRESH EXCEL_TAB.
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>CLIPBOARD_EXPORT
    IMPORTING
      DATA       = EXCEL_TAB
    CHANGING
      RC         = LD_RC
    EXCEPTIONS
      CNTL_ERROR = 1
*     ERROR_NO_GUI         = 2
*     NOT_SUPPORTED_BY_GUI = 3
      OTHERS     = 4.

* quit Excel and free ABAP Object - unfortunately, this does not kill
* the Excel process
  CALL METHOD OF APPLICATION 'QUIT'.
  M_MESSAGE.

* >>>>> Begin of change note 575877
* to kill the Excel process it's necessary to free all used objects
  FREE OBJECT H_CELL.       M_MESSAGE.
  FREE OBJECT H_CELL1.      M_MESSAGE.
  FREE OBJECT RANGE.        M_MESSAGE.
  FREE OBJECT WORKSHEET.    M_MESSAGE.
  FREE OBJECT WORKBOOK.     M_MESSAGE.
  FREE OBJECT APPLICATION.  M_MESSAGE.
* <<<<< End of change note 575877




ENDFUNCTION.
