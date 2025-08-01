class ZCL_SDSCA_UTILITIES definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF TS_XLSX_DATA,
        WORKSHEET_NAME TYPE  STRING,
        DATA           TYPE  REF TO DATA,
      END OF TS_XLSX_DATA .
  types:
    TT_XLSX_DATA  TYPE  STANDARD TABLE OF TS_XLSX_DATA
                             WITH DEFAULT KEY .
  types:
    TRT_RANGE_PARAM TYPE RANGE OF ZSDSCAC001-PARAM .
  types:
    TRT_RANGE_EXT   TYPE RANGE OF ZSDSCAC001-PARAM_EXT .
  types:
    BEGIN OF TS_GEN_C,
        REPID        TYPE  ZSDSCAC001-REPID,
        PARAM        TYPE  ZSDSCAC001-PARAM,
        PARAM_EXT    TYPE  ZSDSCAC001-PARAM_EXT,
        SEQUENCE     TYPE  ZSDSCAC001-SEQUENCE,
        PARAM_SIGN   TYPE  ZSDSCAC001-PARAM_SIGN,
        PARAM_OPTION TYPE  ZSDSCAC001-PARAM_OPTION,
        VALUE_LOW    TYPE  ZSDSCAC001-VALUE_LOW,
        VALUE_HIGH   TYPE  ZSDSCAC001-VALUE_HIGH,
        VDESC        TYPE  ZSDSCAC001-VDESC,
      END OF TS_GEN_C .
  types:
    TT_GEN_C  TYPE STANDARD TABLE OF TS_GEN_C .

  class-methods GET_GEN_C
    importing
      !IF_REPID type PROGRAMM
      !IRT_PARAM type TRT_RANGE_PARAM optional
      !IRT_EXT type TRT_RANGE_EXT optional
    exporting
      !ET_GEN_C type TT_GEN_C .
  class-methods GET_GEN_C_RANGE
    importing
      value(IF_REPID) type PROGRAMM
      value(IF_PARAM) type ZSDSDE_PARAM_NAME
      value(IF_EXT) type ZSDSDE_PARAM_EXT optional
    exporting
      value(ET_RANGE) type TABLE .
  class-methods READ_FILE_TO_XSTRING
    importing
      !IF_FILENAME type STRING
    exporting
      !EF_XSTRING type XSTRING
    exceptions
      ERROR_READ_FILENAME .
  class-methods READ_APPSV_FILE_INTO_XSTRING
    importing
      !IF_FILENAME type STRING
    exporting
      !EF_XSTRING type XSTRING
    exceptions
      ERROR_READ_FILENAME .
  class-methods READ_XLSX_INTO_ITAB
    importing
      !IF_FILENAME type STRING
      !IF_APPSV_FLAG type XFLAG default ' '
      !IF_XSTRING type XSTRING optional
      !IF_READ_ACTIVE_WORKSHEET type XFLAG default ' '
      !IT_WORKSHEET type IF_FDT_DOC_SPREADSHEET=>T_WORKSHEET_NAMES optional
    exporting
      !ET_XLSX_DATA type TT_XLSX_DATA
    exceptions
      MISSING_FILENAME_OR_XSTRING
      ERROR_READ_FILENAME
      NO_DATA_FOUND .
  class-methods CREATE_XLSX_FROM_ITAB
    importing
      !IT_FIELDCAT type LVC_T_FCAT optional
      !IT_SORT type LVC_T_SORT optional
      !IT_FILT type LVC_T_FILT optional
      !IS_LAYOUT type LVC_S_LAYO optional
      !IT_HYPERLINKS type LVC_T_HYPE optional
      value(IT_DATA) type STANDARD TABLE                 "#EC CI_VALPAR
    returning
      value(RF_XSTRING) type XSTRING .
  class-methods GET_APPSV_FILE_LIST
    importing
      !IF_DIRNAME type CLIKE
      !IF_FILENAME type CLIKE
    exporting
      !ET_FILELIST type OIUXC_RSFILLST_ITAB .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SDSCA_UTILITIES IMPLEMENTATION.


METHOD CREATE_XLSX_FROM_ITAB.

  FIELD-SYMBOLS:
    <L_TAB>  TYPE STANDARD TABLE.


* Create Data reference
  DATA(LREF_DATA) = REF #( IT_DATA ).

* Generate Field category based on internal table
  IF IT_FIELDCAT IS INITIAL.

    ASSIGN LREF_DATA->* TO <L_TAB>.

    TRY.
        CL_SALV_TABLE=>FACTORY(
          EXPORTING
            LIST_DISPLAY = ABAP_FALSE
          IMPORTING
            R_SALV_TABLE = DATA(LREF_SALV_TABLE)
          CHANGING
            T_TABLE      = <L_TAB> ).

        DATA(LT_FCAT) = CL_SALV_CONTROLLER_METADATA=>GET_LVC_FIELDCATALOG(
                                 R_COLUMNS      = LREF_SALV_TABLE->GET_COLUMNS( )
                                 R_AGGREGATIONS = LREF_SALV_TABLE->GET_AGGREGATIONS( ) ).
      CATCH CX_SALV_MSG.
        RETURN.

    ENDTRY.

  ELSE.
    LT_FCAT = IT_FIELDCAT.
  ENDIF.

* Update Default Long Header text
  LOOP AT LT_FCAT TRANSPORTING NO FIELDS
                  WHERE COLDDICTXT IS NOT INITIAL.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC NE 0.
    MODIFY LT_FCAT FROM VALUE LVC_S_FCAT( COLDDICTXT = 'L'  ) TRANSPORTING COLDDICTXT
                   WHERE COLDDICTXT IS INITIAL.
  ENDIF.

* Call Method generate XLSX as XSTRING
  CALL METHOD CL_SALV_BS_TT_UTIL=>IF_SALV_BS_TT_UTIL~TRANSFORM
    EXPORTING
      XML_TYPE      = IF_SALV_BS_XML=>C_TYPE_XLSX
      XML_VERSION   = CL_SALV_BS_A_XML_BASE=>GET_VERSION( )
      R_RESULT_DATA = CL_SALV_EX_UTIL=>FACTORY_RESULT_DATA_TABLE(
                                              R_DATA                      = LREF_DATA
                                              S_LAYOUT                    = IS_LAYOUT
                                              T_FIELDCATALOG              = LT_FCAT
                                              T_SORT                      = IT_SORT
                                              T_FILTER                    = IT_FILT
                                              T_HYPERLINKS                = IT_HYPERLINKS )
      XML_FLAVOUR   = IF_SALV_BS_C_TT=>C_TT_XML_FLAVOUR_EXPORT
      GUI_TYPE      = IF_SALV_BS_XML=>C_GUI_TYPE_GUI
    IMPORTING
      XML           = RF_XSTRING.

*  TRY.
*      CALL METHOD CL_SALV_BS_LEX=>EXPORT_FROM_RESULT_DATA_TABLE
*        EXPORTING
*          IS_FORMAT            = IF_SALV_BS_LEX_FORMAT=>MC_FORMAT_XLSX
*          IR_RESULT_DATA_TABLE = CL_SALV_EX_UTIL=>FACTORY_RESULT_DATA_TABLE(
*                                         R_DATA                      = LREF_DATA
*                                         S_LAYOUT                    = IS_LAYOUT
*                                         T_FIELDCATALOG              = LT_FCAT
*                                         T_SORT                      = IT_SORT
*                                         T_FILTER                    = IT_FILT
*                                         T_HYPERLINKS                = IT_HYPERLINKS )
*        IMPORTING
*          ER_RESULT_FILE       = RF_XSTRING.
*    CATCH CX_SALV_UNEXPECTED_PARAM_VALUE.
*      RETURN.
*  ENDTRY.

ENDMETHOD.


METHOD GET_GEN_C.

* Initialize Output
  CLEAR ET_GEN_C.

* Get Data
  SELECT REPID,
         PARAM,
         PARAM_EXT,
         SEQUENCE,
         PARAM_SIGN,
         PARAM_OPTION,
         VALUE_LOW,
         VALUE_HIGH,
         VDESC
    INTO TABLE @ET_GEN_C
    FROM ZSDSCAC001
   WHERE REPID EQ @IF_REPID
     AND PARAM IN @IRT_PARAM
     AND PARAM_EXT IN @IRT_EXT
     AND ZDEL_FLG  EQ @SPACE.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD READ_FILE_TO_XSTRING.

  DATA:
    LF_FILELENGTH TYPE  I.

  DATA:
    LT_DATA  TYPE SOLIX_TAB.


* Initialize Output
  CLEAR: EF_XSTRING.

* Read File as Binary
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
    EXPORTING
      FILENAME                = IF_FILENAME
      FILETYPE                = 'BIN'
    IMPORTING
      FILELENGTH              = LF_FILELENGTH
    CHANGING
      DATA_TAB                = LT_DATA
    EXCEPTIONS
      FILE_OPEN_ERROR         = 1
      FILE_READ_ERROR         = 2
      NO_BATCH                = 3
      GUI_REFUSE_FILETRANSFER = 4
      INVALID_TYPE            = 5
      NO_AUTHORITY            = 6
      UNKNOWN_ERROR           = 7
      BAD_DATA_FORMAT         = 8
      HEADER_NOT_ALLOWED      = 9
      SEPARATOR_NOT_ALLOWED   = 10
      HEADER_TOO_LONG         = 11
      UNKNOWN_DP_ERROR        = 12
      ACCESS_DENIED           = 13
      DP_OUT_OF_MEMORY        = 14
      DISK_FULL               = 15
      DP_TIMEOUT              = 16
      NOT_SUPPORTED_BY_GUI    = 17
      ERROR_NO_GUI            = 18
      OTHERS                  = 19.
  IF SY-SUBRC <> 0.
    RAISE ERROR_READ_FILENAME.
  ENDIF.
* Convert to XString
  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      INPUT_LENGTH = LF_FILELENGTH
    IMPORTING
      BUFFER       = EF_XSTRING
    TABLES
      BINARY_TAB   = LT_DATA
    EXCEPTIONS
      FAILED       = 1
      OTHERS       = 2.
  IF SY-SUBRC <> 0.
    RAISE ERROR_READ_FILENAME.
  ENDIF.

ENDMETHOD.


METHOD READ_XLSX_INTO_ITAB.

  DATA:
    LS_XLSX_DATA  TYPE  TS_XLSX_DATA.

  DATA:
    LF_XSTRING    TYPE  XSTRING.


* Initialize Output
  CLEAR: ET_XLSX_DATA.

* -------------------------
* Get XLSX Binary data
* -------------------------
  CLEAR LF_XSTRING.
  IF IF_XSTRING IS NOT INITIAL.
    LF_XSTRING = IF_XSTRING.

* -------------------------
* Read Data from File if Binary string is not provided
* -------------------------
  ELSE.
    IF IF_FILENAME IS INITIAL.
      RAISE MISSING_FILENAME_OR_XSTRING.
    ENDIF.

    IF IF_APPSV_FLAG IS INITIAL.
*     Read File to XString
      READ_FILE_TO_XSTRING( EXPORTING IF_FILENAME = IF_FILENAME
                            IMPORTING EF_XSTRING = LF_XSTRING
                            EXCEPTIONS ERROR_READ_FILENAME = 1
                                       OTHERS              = 2 ).
    ELSE.
      READ_APPSV_FILE_INTO_XSTRING( EXPORTING IF_FILENAME = IF_FILENAME
                                    IMPORTING EF_XSTRING = LF_XSTRING
                                    EXCEPTIONS ERROR_READ_FILENAME = 1
                                               OTHERS              = 2 ).
    ENDIF.
    IF SY-SUBRC NE 0.
      RAISE ERROR_READ_FILENAME.
    ENDIF.
  ENDIF.
  IF LF_XSTRING IS INITIAL.
    RAISE NO_DATA_FOUND.
  ENDIF.

* -------------------------
* Processing Excel Data
* -------------------------
  TRY .
      DATA(LF_FILENAME) = IF_FILENAME.
      IF LF_FILENAME IS INITIAL.
        LF_FILENAME = 'EXCEL.XLSX'.
      ENDIF.
      DATA(LREF_EXCEL) = NEW CL_FDT_XL_SPREADSHEET(
                             DOCUMENT_NAME = LF_FILENAME
                             XDOCUMENT     = LF_XSTRING ) .
    CATCH CX_FDT_EXCEL_CORE.
      RAISE NO_DATA_FOUND.
  ENDTRY .

* Get List of Worksheets
  LREF_EXCEL->IF_FDT_DOC_SPREADSHEET~GET_WORKSHEET_NAMES(
                            IMPORTING
                              WORKSHEET_NAMES = DATA(LT_WORKSHEETS) ).

* Get and filter only Active Worksheet
  IF IF_READ_ACTIVE_WORKSHEET IS NOT INITIAL.
    DATA: LF_ACTIVE_INDX TYPE I.
    DATA: LF_ACTIVETAB TYPE STRING.
    TRY.
        DATA(LF_WORKBOOK) = LREF_EXCEL->IF_FDT_DOC_PKG~GET_FILE_AS_XSTRING( 'xl/workbook.xml' ).
        CALL TRANSFORMATION ZSDSCA_FDT_XL_GET_ACTIVETAB
            SOURCE XML LF_WORKBOOK
            RESULT ACTIVETAB = LF_ACTIVETAB.
        LF_ACTIVE_INDX = LF_ACTIVETAB + 1.
      CATCH CX_ROOT.
        LF_ACTIVE_INDX = 1.
    ENDTRY.
*   Remove non-active worksheet
    DELETE LT_WORKSHEETS WHERE TABLE_LINE NE LT_WORKSHEETS[ LF_ACTIVE_INDX ].
  ENDIF.

* Read Worksheet data
  LOOP AT LT_WORKSHEETS ASSIGNING FIELD-SYMBOL(<L_WORKSHEET>).

*   Check Specified Sheet
    IF IT_WORKSHEET IS NOT INITIAL AND
       NOT LINE_EXISTS( IT_WORKSHEET[ TABLE_LINE = <L_WORKSHEET> ] ).
      CONTINUE.
    ENDIF.

*   Read and Assign data
    CLEAR LS_XLSX_DATA.
    LS_XLSX_DATA-WORKSHEET_NAME = <L_WORKSHEET>.
    LS_XLSX_DATA-DATA = LREF_EXCEL->IF_FDT_DOC_SPREADSHEET~GET_ITAB_FROM_WORKSHEET(
                                             <L_WORKSHEET> ).
    INSERT LS_XLSX_DATA INTO TABLE ET_XLSX_DATA.
  ENDLOOP.

  IF ET_XLSX_DATA IS INITIAL.
    RAISE NO_DATA_FOUND.
  ENDIF.

ENDMETHOD.


  METHOD GET_GEN_C_RANGE.
    DATA: LRT_EXT TYPE RANGE OF ZSDSCAC001-PARAM_EXT.
    DATA: LF_REF TYPE REF TO DATA.
    FIELD-SYMBOLS: <L_WA> TYPE ANY.
    FIELD-SYMBOLS: <L_VAL> TYPE ANY.
    FIELD-SYMBOLS: <L_TAB> TYPE TABLE.

    IF IF_EXT IS NOT INITIAL.
      LRT_EXT =  VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = IF_EXT ) ).
    ENDIF.

    SELECT PARAM_SIGN,
           PARAM_OPTION,
           VALUE_LOW,
           VALUE_HIGH
      FROM ZSDSCAC001
      INTO TABLE @DATA(LT_GENC)
      WHERE REPID     = @IF_REPID
        AND PARAM     = @IF_PARAM
        AND PARAM_EXT IN @LRT_EXT.

    IF SY-SUBRC <> 0.
      RETURN.
    ENDIF.

    CREATE DATA LF_REF LIKE LINE OF ET_RANGE.
    ASSIGN LF_REF->* TO <L_WA>.

    LOOP AT LT_GENC INTO DATA(LS_GENC).

      CLEAR: <L_WA>.

      UNASSIGN <L_VAL>.
      ASSIGN COMPONENT 'SIGN' OF STRUCTURE <L_WA> TO <L_VAL>.
      IF  <L_VAL> IS ASSIGNED.
        <L_VAL> = LS_GENC-PARAM_SIGN.
      ENDIF.

      UNASSIGN <L_VAL>.
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <L_WA> TO <L_VAL>.
      IF  <L_VAL> IS ASSIGNED.
        <L_VAL> = LS_GENC-PARAM_OPTION.
      ENDIF.

      UNASSIGN <L_VAL>.
      ASSIGN COMPONENT 'LOW' OF STRUCTURE <L_WA> TO <L_VAL>.
      IF  <L_VAL> IS ASSIGNED.
        <L_VAL> = LS_GENC-VALUE_LOW.
        UNASSIGN <L_VAL>.
      ENDIF.

      ASSIGN COMPONENT 'HIGH' OF STRUCTURE <L_WA> TO <L_VAL>.
      IF  <L_VAL> IS ASSIGNED.
        <L_VAL> = LS_GENC-VALUE_HIGH.
      ENDIF.

      APPEND <L_WA> TO ET_RANGE.

    ENDLOOP.
  ENDMETHOD.


METHOD READ_APPSV_FILE_INTO_XSTRING.

* Initialize Output
  CLEAR: EF_XSTRING.

  TRY.
      OPEN DATASET IF_FILENAME FOR INPUT IN BINARY MODE.
      IF SY-SUBRC NE 0.
        RAISE ERROR_READ_FILENAME.
      ENDIF.

      READ DATASET IF_FILENAME INTO EF_XSTRING.
      IF SY-SUBRC NE 0.
        RAISE ERROR_READ_FILENAME.
      ENDIF.

      CLOSE DATASET IF_FILENAME.
      IF SY-SUBRC NE 0.
        RAISE ERROR_READ_FILENAME.
      ENDIF.

    CATCH CX_ROOT.
      RAISE ERROR_READ_FILENAME.

  ENDTRY.

ENDMETHOD.


METHOD GET_APPSV_FILE_LIST.

  TYPES: BEGIN OF TS_FILE,
           DIRNAME(75) TYPE C, " name of directory. (possibly truncated.)
           NAME(75)    TYPE C, " name of entry. (possibly truncated.)
           TYPE(10)    TYPE C,            " type of entry.
           LEN         TYPE I,            " length in bytes.
           OWNER(8)    TYPE C,            " owner of the entry.
           MTIME       TYPE I, " last modification date, seconds since 1970
           FMODE(9)    TYPE C, " like "rwx-r-x--x": protection mode.
           USEABLE(1)  TYPE C,
           SUBRC(4)    TYPE C,
           ERRNO(3)    TYPE C,
           ERRMSG(40)  TYPE C,
           MOD_DATE    TYPE D,
           MOD_TIME(8) TYPE C,            " hh:mm:ss
           SEEN(1)     TYPE C,
           CHANGED(1)  TYPE C,
         END OF TS_FILE.

  DATA:
    LS_FILELIST TYPE RSFILLST,
    LS_FILE     TYPE TS_FILE.

  DATA:
    LF_FILENAME TYPE STRING,
    LF_DIRNAME  TYPE TEXT1000,
    LF_PATTERN  TYPE TEXT100,
    LF_TEMP     TYPE TS_FILE-NAME,
    LF_ERRCNT   TYPE P LENGTH 2 VALUE 0.


* Initialize Output
  CLEAR ET_FILELIST.

  LF_DIRNAME = IF_DIRNAME.
  LF_PATTERN = IF_FILENAME.
  TRANSLATE LF_PATTERN TO UPPER CASE.

  CLEAR LS_FILELIST.
  CALL 'C_DIR_READ_FINISH'             " just to be sure
      ID 'ERRNO'  FIELD LS_FILELIST-ERRNO
      ID 'ERRMSG' FIELD LS_FILELIST-ERRMSG.

  CALL 'C_DIR_READ_START' ID 'DIR'    FIELD LF_DIRNAME
                          ID 'FILE'   FIELD LF_FILENAME
                          ID 'ERRNO'  FIELD LS_FILE-ERRNO
                          ID 'ERRMSG' FIELD LS_FILE-ERRMSG.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  DO.
    CLEAR LS_FILE.
    CALL 'C_DIR_READ_NEXT'
      ID 'TYPE'   FIELD LS_FILE-TYPE
      ID 'NAME'   FIELD LS_FILE-NAME
      ID 'LEN'    FIELD LS_FILE-LEN
      ID 'OWNER'  FIELD LS_FILE-OWNER
      ID 'MTIME'  FIELD LS_FILE-MTIME
      ID 'MODE'   FIELD LS_FILE-FMODE
      ID 'ERRNO'  FIELD LS_FILE-ERRNO
      ID 'ERRMSG' FIELD LS_FILE-ERRMSG.
    LS_FILE-DIRNAME = IF_DIRNAME.
    MOVE SY-SUBRC TO LS_FILE-SUBRC.
    CASE SY-SUBRC.
      WHEN 0.
        CLEAR: LS_FILE-ERRNO, LS_FILE-ERRMSG.
        CASE LS_FILE-TYPE(1).
          WHEN 'F'.                    " normal file.
          WHEN 'f'.                    " normal file.
          WHEN OTHERS. " directory, device, fifo, socket,...
            MOVE SPACE TO LS_FILE-USEABLE.
        ENDCASE.
        IF LS_FILE-LEN = 0.
          MOVE SPACE TO LS_FILE-USEABLE.
        ENDIF.
      WHEN 1.
        EXIT.
      WHEN OTHERS.                     " SY-SUBRC >= 2
        ADD 1 TO LF_ERRCNT.
        IF LF_ERRCNT > 10.
          EXIT.
        ENDIF.
        IF SY-SUBRC = 5.
          MOVE: '???' TO LS_FILE-TYPE,
                '???' TO LS_FILE-OWNER,
                '???' TO LS_FILE-FMODE.
        ELSE.
        ENDIF.
        MOVE SPACE TO LS_FILE-USEABLE.
    ENDCASE.

    PERFORM P6_TO_DATE_TIME_TZ IN PROGRAM RSTR0400
                                USING LS_FILE-MTIME
                                      LS_FILE-MOD_TIME
                                      LS_FILE-MOD_DATE.

*   * Does the filename contains the requested pattern?
*   * Then store it, else forget it.
    IF LF_PATTERN IS NOT INITIAL.

      LF_TEMP = LS_FILE-NAME.
      TRANSLATE LF_TEMP TO UPPER CASE.
      IF NOT LF_TEMP CP LF_PATTERN.
        CONTINUE.
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING LS_FILE TO LS_FILELIST.
    INSERT LS_FILELIST INTO TABLE ET_FILELIST.

  ENDDO.

  CALL 'C_DIR_READ_FINISH'
      ID 'ERRNO'  FIELD LS_FILELIST-ERRNO
      ID 'ERRMSG' FIELD LS_FILELIST-ERRMSG.
  IF SY-SUBRC <> 0.
  ENDIF.

ENDMETHOD.
ENDCLASS.
