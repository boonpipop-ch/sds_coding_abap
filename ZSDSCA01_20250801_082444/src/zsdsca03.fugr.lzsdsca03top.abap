FUNCTION-POOL ZSDSCA03.                     "MESSAGE-ID ..

* INCLUDE LZSDSCA03D...                      " Local class definition

*<-- Start Declaration copied from function group ALSMEX
*      value of excel-cell
TYPES: TY_D_ITABVALUE TYPE ZSDSCAS016-VALUE,
*      internal table containing the excel data
       TY_T_ITAB      TYPE ZSDSCAS016   OCCURS 0,

*      line type of sender table
       BEGIN OF TY_S_SENDERLINE,
         LINE(4096) TYPE C,
       END OF TY_S_SENDERLINE,
*      sender table
       TY_T_SENDER TYPE TY_S_SENDERLINE  OCCURS 0.

CONSTANTS:  GC_ESC              VALUE '"'.
*--> End of Declaratio copied from function group ALSMEX

*<-- Start of Declaration copeid from function group SHTM
*    in This section is Copeid for function DOWNLOAD_WEB_OBJECT
DATA  HTML LIKE W3HTML OCCURS 100 WITH HEADER LINE.
DATA  MIME LIKE W3MIME OCCURS 100 WITH HEADER LINE.
DATA  FILENAME LIKE RLGRAP-FILENAME ##NEEDED.

CONSTANTS: C_FILENAME  LIKE WWWPARAMS-NAME  VALUE 'filename'      ##NO_TEXT,
           C_FILESIZE  LIKE WWWPARAMS-NAME  VALUE 'filesize'      ##NO_TEXT,
           C_EXTENSION LIKE WWWPARAMS-NAME  VALUE 'fileextension' ##NO_TEXT,
           C_DOWNLOAD  LIKE WWWDATA-OBJID   VALUE 'download'      ##NO_TEXT.
*--> End of Declaration copeid from function group SHTM

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
*TYPES: gty_conv  TYPE  zttec_conversion.

TYPES: BEGIN OF GTY_RAW,
         TLINE TYPE  STRING,
       END OF GTY_RAW.
TYPES: GTTY_RAW  TYPE  STANDARD TABLE OF GTY_RAW.

TYPES: GTY_FILE_LIST TYPE ZSDSCAS017.
TYPES: GTTY_FILE_LIST  TYPE STANDARD TABLE OF GTY_FILE_LIST.

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*
DEFINE MC_SHOW_PROGRESS.
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
