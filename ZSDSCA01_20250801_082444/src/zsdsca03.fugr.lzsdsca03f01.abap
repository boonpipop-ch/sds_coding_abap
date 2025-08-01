*----------------------------------------------------------------------*
***INCLUDE LZSDSCA03F01.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Form F_LIST_IFILE
*----------------------------------------------------------------------*
*  Popup for Input file selection
*----------------------------------------------------------------------*
FORM F_LIST_IFILE  CHANGING PV_FILENAME  TYPE  STRING.

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
      FILE_FILTER             = 'Excel File|*.XLSX;*.XLS' ##NO_TEXT
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

*----------------------------------------------------------------------*
*  Form f_get_dest_fname
*----------------------------------------------------------------------*
*  Get Destination File name
*----------------------------------------------------------------------*
FORM F_GET_DEST_FNAME  USING  PV_FPATH TYPE PATHINTERN "zttec_conversion-fpath
                              PV_FNAME_I  TYPE  CLIKE
                              PV_CONFIRM_AFNAME TYPE FLAG
                     CHANGING PV_FNAME TYPE CLIKE.

  DATA:
    LT_FIELD   TYPE  STANDARD TABLE OF SVAL.

  DATA:
    LS_FIELD   TYPE  SVAL.

  DATA:
    LV_RET      TYPE  CHAR1,
    LV_FNAME    TYPE  STRING,
    LV_FILENAME TYPE  STRING,
    LV_ANS      TYPE  CHAR1,
    LV_FILE255  TYPE  BTCXPGPAR.

  FIELD-SYMBOLS:
    <LFS_FIELD>  TYPE  SVAL.


* Initialize Output
  CLEAR: PV_FNAME.

  IF PV_CONFIRM_AFNAME EQ 'X' AND
      PV_FNAME_I IS NOT INITIAL.
    LV_FNAME = PV_FNAME_I.
  ELSE.
*   Assign Field info
    CLEAR LS_FIELD.
    LS_FIELD-TABNAME   = 'FILENAMECI'.
    LS_FIELD-FIELDNAME = 'FILEEXTERN'.
*   Text-t02: Destination Filename
    LS_FIELD-FIELDTEXT = 'Destination Filename'.
    LS_FIELD-VALUE     = PV_FNAME_I.
    INSERT LS_FIELD INTO TABLE LT_FIELD.

*   Text-t01: Enter destination filename
    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        NO_VALUE_CHECK  = 'X'
        POPUP_TITLE     = TEXT-T01
        START_COLUMN    = '5'
        START_ROW       = '10'
      IMPORTING
        RETURNCODE      = LV_RET
      TABLES
        FIELDS          = LT_FIELD
      EXCEPTIONS
        ERROR_IN_FIELDS = 1
        OTHERS          = 2.
    IF SY-SUBRC <> 0.
      RETURN.
    ENDIF.

*   User presses cancel
    IF LV_RET EQ 'A'.
      RETURN.
    ENDIF.

*   Read Filename
    READ TABLE LT_FIELD ASSIGNING <LFS_FIELD>
                        INDEX 1.
    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.

*   Assign filename
    LV_FNAME = <LFS_FIELD>-VALUE.

  ENDIF.

* Assign output
  PV_FNAME = LV_FNAME.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_read_excel_to_text_tab
*----------------------------------------------------------------------*
*  Read Excel file into internal table of text
*----------------------------------------------------------------------*
FORM F_READ_EXCEL_TO_TEXT_TAB  USING  PV_IFILE    TYPE  STRING
                                      PV_BEG_COL  TYPE  I
                                      PV_BEG_ROW  TYPE  I
                                      PV_END_COL  TYPE  I
                                      PV_END_ROW  TYPE  I
                             CHANGING PT_RAW      TYPE  GTTY_RAW.

  CONSTANTS:
    LC_SPLIT         TYPE  CHAR1
                          VALUE CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.

  DATA:
    LT_TAB  TYPE  STANDARD TABLE OF ZSDSCAS016.

  DATA:
    LS_RAW       TYPE  GTY_RAW.

  DATA:
    LV_ROW TYPE  I,
    LV_COL TYPE  I.

  FIELD-SYMBOLS:
    <LFS_TAB>  TYPE  ZSDSCAS016.

* Initialize Output
  REFRESH: PT_RAW.

  CALL FUNCTION 'Z_SDSCA_EXCEL_TO_ITAB'
    EXPORTING
      FILENAME                = PV_IFILE
      I_BEGIN_COL             = PV_BEG_COL
      I_BEGIN_ROW             = PV_BEG_ROW
      I_END_COL               = PV_END_COL
      I_END_ROW               = PV_END_ROW
    TABLES
      INTERN                  = LT_TAB
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
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
        CONCATENATE LS_RAW-TLINE LC_SPLIT
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
        SEPARATED BY LC_SPLIT.
    ENDIF.

    LV_COL = LV_COL + 1.

*   Insert last line
    AT LAST.
      INSERT LS_RAW INTO TABLE PT_RAW.
    ENDAT.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_LIST_OFILE
*----------------------------------------------------------------------*
*  Popup for Output file selection
*----------------------------------------------------------------------*
FORM F_LIST_OFILE  CHANGING PV_FILENAME  TYPE  STRING.

  DATA:
    LV_PATH     TYPE  STRING,
    LV_FILENAME TYPE  STRING.


* Initialize Output
  CLEAR PV_FILENAME.

* Get File name dialog
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
      DEFAULT_EXTENSION    = '*'
*     File Filter format is '<Text>|<Filtering>;<Text>|<Filtering>'
      FILE_FILTER          = 'Excel File|*.XLSX;*.XLS' ##NO_TEXT
      PROMPT_ON_OVERWRITE  = 'X'
    CHANGING
      FILENAME             = LV_FILENAME
      PATH                 = LV_PATH
      FULLPATH             = PV_FILENAME
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_get_conv_config
*----------------------------------------------------------------------*
*  Get Conversion Configuration
*----------------------------------------------------------------------*
*FORM f_get_conv_config  USING  pv_repid  TYPE  gty_conv-repid
*                               pv_seqno  TYPE  gty_conv-seqno
*                      CHANGING ps_conv   TYPE  gty_conv.
*
** Initialize Output
*  CLEAR: ps_conv.
*
** Get Configuration
*  SELECT SINGLE *
*    INTO ps_conv
*    FROM zttec_conversion
*   WHERE repid  EQ  pv_repid
*     AND seqno  EQ  pv_seqno.
*  IF sy-subrc NE 0.
*    RETURN.
*  ENDIF.
*
*ENDFORM.

*----------------------------------------------------------------------*
*  Form f_get_physical_path
*----------------------------------------------------------------------*
*  Get Physical Path
*----------------------------------------------------------------------*
FORM F_GET_PHYSICAL_PATH  USING  PV_PATH      TYPE  PATHINTERN
                        CHANGING PV_PHYS_PATH TYPE  CLIKE.

  CONSTANTS:
    LC_DUMMY     TYPE  CHAR50 VALUE 'DUMMY'.
  DATA:
    LV_FULLNAME  TYPE  STRING.


* Initialize Output
  CLEAR PV_PHYS_PATH.

* Get Physical path
  CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
    EXPORTING
      LOGICAL_PATH               = PV_PATH
      FILE_NAME                  = LC_DUMMY
    IMPORTING
      FILE_NAME_WITH_PATH        = LV_FULLNAME
    EXCEPTIONS
      PATH_NOT_FOUND             = 1
      MISSING_PARAMETER          = 2
      OPERATING_SYSTEM_NOT_FOUND = 3
      FILE_SYSTEM_NOT_FOUND      = 4
      OTHERS                     = 5.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

* Remove Dummy Filename
  REPLACE ALL OCCURRENCES OF LC_DUMMY IN LV_FULLNAME
                                      WITH SPACE.

* Assign Output
  PV_PHYS_PATH = LV_FULLNAME.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_FILES_LIST
*----------------------------------------------------------------------*
*  Get Files List from Application Server Path
*----------------------------------------------------------------------*
FORM F_GET_FILES_LIST  USING  PV_DIR_NAME  TYPE  CLIKE
                     CHANGING PT_LIST      TYPE  GTTY_FILE_LIST.

  TYPES: BEGIN OF LTY_FILE,
           NAME     TYPE TEXT1000, " name of entry. (possibly truncated.)
           TYPE     TYPE C LENGTH 10,            " type of entry.
           LEN      TYPE P LENGTH 8 DECIMALS 0,  " length in bytes.
           OWNER    TYPE C LENGTH 8,             " owner of the entry.
           MTIME    TYPE P LENGTH 6 DECIMALS 0,  " last modification date, seconds since 1970
           MODE     TYPE C LENGTH 9,             " like "rwx-r-x--x": protection mode.
           MOD_DATE TYPE D,
           MOD_TIME TYPE C LENGTH 8,             " hh:mm:ss
           ERRNO    TYPE C LENGTH 3,
           ERRMSG   TYPE C LENGTH 40,
         END OF LTY_FILE.

  DATA:
    LS_FILE TYPE  LTY_FILE,
    LS_LIST TYPE  GTY_FILE_LIST.

  DATA:
    LV_MGR_USER            TYPE AS4USER.


* Initialize Output
  REFRESH: PT_LIST.

* Only when path exist
  IF PV_DIR_NAME IS INITIAL.
    RETURN.
  ENDIF.

* authority check
  CALL FUNCTION 'TMS_CI_GET_USER_INFO'
    IMPORTING
      EV_MGR_USER = LV_MGR_USER.
  CALL FUNCTION 'TR_AUTHORITY_CHECK_ADMIN'
    EXPORTING
      IV_USER          = LV_MGR_USER
      IV_ADMINFUNCTION = 'EPS1'
    EXCEPTIONS
      OTHERS           = 1.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

* get directory listing
  CALL 'C_DIR_READ_FINISH'                  " just to be sure
        ID 'ERRNO'  FIELD LS_FILE-ERRNO
        ID 'ERRMSG' FIELD LS_FILE-ERRMSG.                 "#EC CI_CCALL

  CALL 'C_DIR_READ_START'
        ID 'DIR'    FIELD PV_DIR_NAME
        ID 'FILE'   FIELD SPACE
        ID 'ERRNO'  FIELD LS_FILE-ERRNO
        ID 'ERRMSG' FIELD LS_FILE-ERRMSG.                 "#EC CI_CCALL
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  DO.
    CLEAR LS_FILE.
    CLEAR LS_LIST.
    CALL 'C_DIR_READ_NEXT'
          ID 'TYPE'   FIELD LS_FILE-TYPE
          ID 'NAME'   FIELD LS_FILE-NAME
          ID 'LEN'    FIELD LS_FILE-LEN
          ID 'OWNER'  FIELD LS_FILE-OWNER
          ID 'MTIME'  FIELD LS_FILE-MTIME
          ID 'MODE'   FIELD LS_FILE-MODE
          ID 'ERRNO'  FIELD LS_FILE-ERRNO
          ID 'ERRMSG' FIELD LS_FILE-ERRMSG.               "#EC CI_CCALL
    IF SY-SUBRC NE 0.
      EXIT.
    ENDIF.

*   Only File
    IF NOT ( LS_FILE-TYPE(1) EQ 'f' OR              " regular file
             LS_FILE-TYPE(1) EQ 'F' ).
      CONTINUE.
    ENDIF.

    PERFORM P6_TO_DATE_TIME_TZ IN PROGRAM RSTR0400
                               USING LS_FILE-MTIME
                                     LS_FILE-MOD_TIME
                                     LS_FILE-MOD_DATE.

*   handle files > 2147483647 byte (int 4) - hen 9.9.2004
    IF LS_FILE-LEN > 2147483647.
      LS_LIST-SIZE  = -99.
    ELSE.
      LS_LIST-SIZE  = LS_FILE-LEN.
    ENDIF.
    LS_LIST-NAME       = LS_FILE-NAME.
    LS_LIST-DATUM      = LS_FILE-MOD_DATE.
    LS_LIST-UZEIT(2)   = LS_FILE-MOD_TIME(2).
    LS_LIST-UZEIT+2(2) = LS_FILE-MOD_TIME+3(2).
    LS_LIST-UZEIT+4(2) = LS_FILE-MOD_TIME+6(2).
    APPEND LS_LIST TO PT_LIST.
  ENDDO.

  CALL 'C_DIR_READ_FINISH'
        ID 'ERRNO'  FIELD LS_FILE-ERRNO
        ID 'ERRMSG' FIELD LS_FILE-ERRMSG.                 "#EC CI_CCALL
  SORT PT_LIST BY NAME ASCENDING.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_list_files_for_select
*----------------------------------------------------------------------*
*  List Files for selection
*----------------------------------------------------------------------*
FORM F_LIST_FILES_FOR_SELECT  USING  PT_LIST  TYPE  GTTY_FILE_LIST
                            CHANGING PV_FNAME TYPE  CLIKE.

  DATA:
    LT_RETURN TYPE STANDARD TABLE OF DDSHRETVAL.

  DATA:
    LV_RETFIELD TYPE DFIES-FIELDNAME VALUE 'NAME'.

  FIELD-SYMBOLS:
    <LFS_RETURN>  TYPE  DDSHRETVAL.


* Text-i01: Please select file name
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      DDIC_STRUCTURE  = 'ZSTEC_FILES_LIST'
      RETFIELD        = LV_RETFIELD
      WINDOW_TITLE    = TEXT-I01
      VALUE_ORG       = 'S'
      MULTIPLE_CHOICE = SPACE
    TABLES
      VALUE_TAB       = PT_LIST
      RETURN_TAB      = LT_RETURN
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  READ TABLE LT_RETURN ASSIGNING <LFS_RETURN>
                       INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Output
  PV_FNAME = <LFS_RETURN>-FIELDVAL.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_read_file_to_xstring
*----------------------------------------------------------------------*
*  Read Input file into Xstring
*----------------------------------------------------------------------*
FORM F_READ_FILE_TO_XSTRING  USING  PV_FILENAME  TYPE  STRING
                           CHANGING PV_XSTRING   TYPE  XSTRING.

  DATA:
    LT_XTAB    TYPE  CPT_X255,
    LV_SIZE    TYPE  I,
    LV_XSTRING TYPE  XSTRING.


* Initialize Output
  CLEAR: PV_XSTRING.

* Read file as binary
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
    EXPORTING
      FILENAME                = PV_FILENAME
      FILETYPE                = 'BIN'
    IMPORTING
      FILELENGTH              = LV_SIZE
*     header                  =
    CHANGING
      DATA_TAB                = LT_XTAB
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
    MESSAGE ID SY-MSGID
            TYPE SY-MSGTY
            NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2
                 SY-MSGV3 SY-MSGV4
            RAISING ERROR_READ_FILE.
    RETURN.
  ENDIF.

* Convert to XString
  CL_SCP_CHANGE_DB=>XTAB_TO_XSTR( EXPORTING IM_XTAB    = LT_XTAB
                                            IM_SIZE    = LV_SIZE
                                  IMPORTING EX_XSTRING = LV_XSTRING ).

* Assign Output
  PV_XSTRING = LV_XSTRING.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  SEPARATED_TO_INTERN_CONVERT
*----------------------------------------------------------------------*
*       Subroutine is copeid from function group ALSMEX
*----------------------------------------------------------------------*
FORM SEPARATED_TO_INTERN_CONVERT TABLES I_TAB       TYPE TY_T_SENDER
                                        I_INTERN    TYPE TY_T_ITAB
                                 USING  I_SEPARATOR TYPE C.
DATA: L_SIC_TABIX LIKE SY-TABIX,
      L_SIC_COL   TYPE KCD_EX_COL.
DATA: L_FDPOS     LIKE SY-FDPOS.

REFRESH I_INTERN.

LOOP AT I_TAB.
  L_SIC_TABIX = SY-TABIX.
  L_SIC_COL = 0.
  WHILE I_TAB CA I_SEPARATOR.
    L_FDPOS = SY-FDPOS.
    L_SIC_COL = L_SIC_COL + 1.
    PERFORM LINE_TO_CELL_SEPARAT TABLES I_INTERN
                                 USING  I_TAB L_SIC_TABIX L_SIC_COL
                                        I_SEPARATOR L_FDPOS.
  ENDWHILE.
  IF I_TAB <> SPACE.
    CLEAR I_INTERN.
    I_INTERN-ROW = L_SIC_TABIX.
    I_INTERN-COL = L_SIC_COL + 1.
    I_INTERN-VALUE = I_TAB.
    APPEND I_INTERN.
  ENDIF.
ENDLOOP.
ENDFORM.                    " SEPARATED_TO_INTERN_CONVERT

*----------------------------------------------------------------------*
*       Form  line_to_cell_separat
*----------------------------------------------------------------------*
*       Subroutine is copeid from function group ALSMEX
*----------------------------------------------------------------------*
FORM LINE_TO_CELL_SEPARAT TABLES I_INTERN    TYPE TY_T_ITAB
                          USING  I_LINE
                                 I_ROW       LIKE SY-TABIX
                                 CH_CELL_COL TYPE KCD_EX_COL
                                 I_SEPARATOR TYPE C
                                 I_FDPOS     LIKE SY-FDPOS.
  DATA: L_STRING   TYPE TY_S_SENDERLINE.
  DATA  L_SIC_INT  TYPE I.

  CLEAR I_INTERN.
  L_SIC_INT = I_FDPOS.
  I_INTERN-ROW = I_ROW.
  L_STRING = I_LINE.
  I_INTERN-COL = CH_CELL_COL.
* csv Dateien mit separator in Zelle: --> ;"abc;cd";
  IF ( I_SEPARATOR = ';' OR  I_SEPARATOR = ',' ) AND
       L_STRING(1) = GC_ESC.
    PERFORM LINE_TO_CELL_ESC_SEP USING L_STRING
                                       L_SIC_INT
                                       I_SEPARATOR
                                       I_INTERN-VALUE.
  ELSE.
    IF L_SIC_INT > 0.
      I_INTERN-VALUE = I_LINE(L_SIC_INT).
    ENDIF.
  ENDIF.
  IF L_SIC_INT > 0.
    APPEND I_INTERN.
  ENDIF.
  L_SIC_INT = L_SIC_INT + 1.
  I_LINE = I_LINE+L_SIC_INT.
ENDFORM.

*----------------------------------------------------------------------*
*       Form  line_to_cell_esc_sep
*----------------------------------------------------------------------*
*       Subroutine is copeid from function group ALSMEX
*----------------------------------------------------------------------*
FORM LINE_TO_CELL_ESC_SEP USING I_STRING
                                I_SIC_INT      TYPE I
                                I_SEPARATOR    TYPE C
                                I_INTERN_VALUE TYPE TY_D_ITABVALUE.
  DATA: L_INT         TYPE I,
        L_CELL_END(2).
  FIELD-SYMBOLS: <L_CELL>.
  L_CELL_END = GC_ESC.
  L_CELL_END+1 = I_SEPARATOR .

  IF I_STRING CS GC_ESC.
    I_STRING = I_STRING+1.
    IF I_STRING CS L_CELL_END.
      L_INT = SY-FDPOS.
      ASSIGN I_STRING(L_INT) TO <L_CELL>.
      I_INTERN_VALUE = <L_CELL>.
      L_INT = L_INT + 2.
      I_SIC_INT = L_INT.
      I_STRING = I_STRING+L_INT.
    ELSEIF I_STRING CS GC_ESC.
*     letzte Celle
      L_INT = SY-FDPOS.
      ASSIGN I_STRING(L_INT) TO <L_CELL>.
      I_INTERN_VALUE = <L_CELL>.
      L_INT = L_INT + 1.
      I_SIC_INT = L_INT.
      I_STRING = I_STRING+L_INT.
      L_INT = STRLEN( I_STRING ).
      IF L_INT > 0 . MESSAGE X001(KX) . ENDIF.
    ELSE.
      MESSAGE X001(KX) . "was ist mit csv-Format
    ENDIF.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form download_html
*----------------------------------------------------------------------*
*  Download HTML - The subroutine is copied from subroutine in
*  function DOWNLOAD_WEB_OBJECT
*----------------------------------------------------------------------*
FORM DOWNLOAD_HTML USING P_OBJTYPE  LIKE WWWDATA-RELID
                         P_FSIZE    TYPE I
*                         p_filename TYPE c                     "-CH01
                         P_FILENAME TYPE STRING                "+CH01
                         P_TEMP     TYPE C.

DATA: FILEFILTER       TYPE STRING,
      FILENAME         TYPE STRING,
      PATH             TYPE STRING,
      FULLPATH         TYPE STRING,
      USER_ACTION      TYPE I,

      CUR_GUICOPDEPAGE TYPE TCP00-CPCODEPAGE ##NEEDED.

CLASS CL_GUI_FRONTEND_SERVICES DEFINITION LOAD.

USER_ACTION = CL_GUI_FRONTEND_SERVICES=>ACTION_OK.
IF P_TEMP EQ SPACE.              "process with user query
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
      FILE_FILTER = FILEFILTER
    CHANGING
      FILENAME    = FILENAME
      PATH        = PATH
      FULLPATH    = FULLPATH
      USER_ACTION = USER_ACTION.
ELSE.
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_TEMP_DIRECTORY
    CHANGING
      TEMP_DIR = PATH.
  CALL METHOD CL_GUI_CFW=>FLUSH.
  IF P_TEMP NE 'f'.
    CONCATENATE PATH '\' P_FILENAME INTO FULLPATH.
  ELSE.
    FULLPATH = P_FILENAME.
  ENDIF.

ENDIF.

CASE P_OBJTYPE.
  WHEN 'HT'.
    IF USER_ACTION = CL_GUI_FRONTEND_SERVICES=>ACTION_OK.

      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          FILENAME              = FULLPATH
          FILETYPE              = 'ASC'
          TRUNC_TRAILING_BLANKS = 'X'
          WRITE_LF              = 'X'
        TABLES
          DATA_TAB              = HTML.

    ENDIF.

  WHEN OTHERS.
    IF USER_ACTION = CL_GUI_FRONTEND_SERVICES=>ACTION_OK.

      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          FILENAME     = FULLPATH
          FILETYPE     = 'BIN'
          BIN_FILESIZE = P_FSIZE
        TABLES
          DATA_TAB     = MIME.

    ENDIF.
ENDCASE.
P_FILENAME = FULLPATH.

ENDFORM.
