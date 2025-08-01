*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0320_TOP
*  Creation Date      : 05.06.2024
*  Author             : Thanapong C.
*  Add-on ID          : N/A
*  Description        : This is include program for Global data
*  Purpose            : Include program for global data type and data object
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: UKMBP_CMS_SGM,
        ZSDSFIT036,
        UKMBP_CMS,
        KNB1.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES:
  TS_SUMMARY_PROJ     TYPE ZSDSFIS101,
  TS_SUMMARY_DOC_TYPE TYPE ZSDSFIS102,
  TS_DOC_DETAIL       TYPE ZSDSFIS103,
  TS_RESULT           TYPE ZSDSFIS104,
  TS_OUTPUT           TYPE ZSDSFIS105,
  TS_XLS              TYPE ZSDSFIS141,

  TT_PSPHI_RANGE      TYPE RANGE OF ZSDSFIT036-PSPHI,

  TT_XLS              TYPE STANDARD TABLE OF TS_XLS WITH EMPTY KEY,
  TT_RESULT           TYPE ZSDSFIS102_TT,
  TT_OUTPUT           TYPE STANDARD TABLE OF TS_OUTPUT WITH EMPTY KEY.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  BEGIN OF GC_FCODE,
    SEL_A    TYPE SY-UCOMM VALUE 'SEL_ALL',
    SEL_N    TYPE SY-UCOMM VALUE 'SEL_NONE',
    DOWNLOAD TYPE SY-UCOMM VALUE 'DOWNLOAD',
  END OF GC_FCODE,

  GC_TRUE  TYPE  CHAR1     VALUE 'X',
  GC_TCODE TYPE  SY-TCODE  VALUE 'ZSDSCM001'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT TYPE TT_RESULT                           ##NEEDED,
  GT_XLS    TYPE TT_XLS                              ##NEEDED,
  GT_OUTPUT TYPE TT_OUTPUT                           ##NEEDED.

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
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE     TYPE  TABNAME  VALUE 'ZSDSFIS105'.

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
