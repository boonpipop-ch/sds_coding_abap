*-----------------------------------------------------------------------
*  Program ID         : ZDSMMR0270
*  Creation Date      : 13.05.2024
*  Author             : Chanakarn T.(Eviden)
*  Add-on ID          : N/A
*  Description        : Program for read file and use data to create info record
*  Purpose            : Upload info record
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*& Report ZSDSMMR0270
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSMMR0270.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
* *****************************
* Types for File Processing
* *****************************
TYPES: TS_FIELD_NAME TYPE CHAR40.

TYPES: BEGIN OF TS_RAW,
         TLINE TYPE STRING,
       END OF TS_RAW.
TYPES: TT_RAW TYPE STANDARD TABLE OF TS_RAW.

TYPES: BEGIN OF TS_SUM,
         TOTAL TYPE I, "Total Entries
         SUCCS TYPE I, "Success Entries
         ERROR TYPE I, "Error Entries
       END OF TS_SUM.

* *****************************
* Types for Excel Template
* !!!! Changing field name impact to validation logic !!!
* *****************************
TYPES: BEGIN OF TS_TEMPLATE ##NEEDED,
*        Initial
         LIFNR       TYPE  EINA-LIFNR,
         MATNR       TYPE  EINA-MATNR,
         EKORG       TYPE  EINE-EKORG,
         WERKS       TYPE  EINE-WERKS,
         ESOKZ       TYPE  EINE-ESOKZ,
*        General Data
         MATKL       TYPE  EINA-MATKL,
         IDNLF       TYPE  EINA-IDNLF,
         VERKF       TYPE  EINA-VERKF,
         TELF1       TYPE  EINA-TELF1,
         MEINS       TYPE  EINA-MEINS,
         UMREN       TYPE  EINA-UMREN,
         MEIN1       TYPE  EINA-MEINS,
         UMREZ       TYPE  EINA-UMREZ,
         LMEIN       TYPE  EINA-LMEIN,
         VABME       TYPE  EINA-VABME,
*        Purchase Org data 1
         APLFZ       TYPE  EINE-APLFZ,
         EKGRP       TYPE  EINE-EKGRP,
         NORBM       TYPE  EINE-NORBM,
         MINBM       TYPE  EINE-MINBM,
         MHDRZ       TYPE  EINE-MHDRZ,
         IPRKZ       TYPE  EINE-IPRKZ,
         BSTMA       TYPE  EINE-BSTMA,
         UNTTO       TYPE  EINE-UNTTO,
         UEBTO       TYPE  EINE-UEBTO,
         UEBTK       TYPE  EINE-UEBTK,
         WEBRE       TYPE  EINE-WEBRE,
         MWSKZ       TYPE  EINE-MWSKZ,
         INCO1       TYPE  EINE-INCO1,
         INCO2       TYPE  EINE-INCO2,
         KZABS       TYPE  EINE-KZABS,
         BSTAE       TYPE  EINE-BSTAE,
*        Condition
         DATAB       TYPE  RV13A-DATAB,
         DATBI       TYPE  RV13A-DATBI,
         KSCHL       TYPE  KONP-KSCHL,
         KBETR       TYPE  KONP-KBETR,
         KONWA       TYPE  KONP-KONWA,
         KPEIN       TYPE  KONP-KPEIN,
         KMEIN       TYPE  KONP-KMEIN,
*        Text
         INTXT_SPRAS TYPE  SY-LANGU,
         INTXT_LINE1 TYPE  STRING,
         INTXT_LINE2 TYPE  STRING,
         INTXT_LINE3 TYPE  STRING,
         INTXT_LINE4 TYPE  STRING,
         INTXT_LINE5 TYPE  STRING,
         POTXT_SPRAS TYPE  SY-LANGU,
         POTXT_LINE1 TYPE  STRING,
         POTXT_LINE2 TYPE  STRING,
         POTXT_LINE3 TYPE  STRING,
         POTXT_LINE4 TYPE  STRING,
         POTXT_LINE5 TYPE  STRING,
*        Additional for processing fields
         KBETR_F     TYPE  F,
       END OF TS_TEMPLATE.

TYPES: BEGIN OF TS_LOG,
*        Result
         MSGTY       TYPE  TEXT1000,
         MSGTX       TYPE  TEXT1000,
*        Initial
         LIFNR       TYPE  TEXT100,
         MATNR       TYPE  TEXT100,
         EKORG       TYPE  TEXT100,
         WERKS       TYPE  TEXT100,
         ESOKZ       TYPE  TEXT100,
*        General Data
         MATKL       TYPE  TEXT100,
         IDNLF       TYPE  TEXT100,
         VERKF       TYPE  TEXT100,
         TELF1       TYPE  TEXT100,
         MEINS       TYPE  TEXT100,
         UMREN       TYPE  TEXT100,
         MEIN1       TYPE  TEXT100,
         UMREZ       TYPE  TEXT100,
         LMEIN       TYPE  TEXT100,
         VABME       TYPE  TEXT100,
*        Purchase Org data 1
         APLFZ       TYPE  TEXT100,
         EKGRP       TYPE  TEXT100,
         NORBM       TYPE  TEXT100,
         MINBM       TYPE  TEXT100,
         MHDRZ       TYPE  TEXT100,
         IPRKZ       TYPE  TEXT100,
         BSTMA       TYPE  TEXT100,
         UNTTO       TYPE  TEXT100,
         UEBTO       TYPE  TEXT100,
         UEBTK       TYPE  TEXT100,
         WEBRE       TYPE  TEXT100,
         MWSKZ       TYPE  TEXT100,
         INCO1       TYPE  TEXT100,
         INCO2       TYPE  TEXT100,
*        Condition
         DATAB       TYPE  TEXT100,
         DATBI       TYPE  TEXT100,
         KSCHL       TYPE  TEXT100,
         KBETR       TYPE  TEXT100,
         KONWA       TYPE  TEXT100,
         KPEIN       TYPE  TEXT100,
         KMEIN       TYPE  TEXT100,
*        Text
         INTXT_SPRAS TYPE  TEXT100,
         INTXT_LINE1 TYPE  TEXT250,
         INTXT_LINE2 TYPE  TEXT250,
         INTXT_LINE3 TYPE  TEXT250,
         INTXT_LINE4 TYPE  TEXT250,
         INTXT_LINE5 TYPE  TEXT250,
         POTXT_SPRAS TYPE  TEXT100,
         POTXT_LINE1 TYPE  TEXT250,
         POTXT_LINE2 TYPE  TEXT250,
         POTXT_LINE3 TYPE  TEXT250,
         POTXT_LINE4 TYPE  TEXT250,
         POTXT_LINE5 TYPE  TEXT250,
       END OF TS_LOG.

TYPES: TT_RESULT TYPE STANDARD TABLE OF ZSDSMMS027.

TYPES: TT_STRING_TAB TYPE STANDARD TABLE OF STRING
                            WITH DEFAULT KEY.

TYPES: BEGIN OF TS_TEMPLATE_TEXT,
         INTXT_SPRAS TYPE SY-LANGU,
         INTXT_LINE  TYPE TT_STRING_TAB,
         POTXT_SPRAS TYPE SY-LANGU,
         POTXT_LINE  TYPE TT_STRING_TAB,
       END OF TS_TEMPLATE_TEXT.

TYPES: BEGIN OF TS_FILE_INFO,
         DIRECTORY TYPE STRING,
         FILENAME  TYPE STRING,
         FULLNAME  TYPE STRING,
       END OF TS_FILE_INFO.


* *****************************
* Types for Inforecord Posting
* *****************************
TYPES: BEGIN OF TS_INFO,
         INFNR TYPE EINA-INFNR,
         MEINS TYPE EINA-MEINS,
         UMREN TYPE EINA-UMREN,
         UMREZ TYPE EINA-UMREZ,
       END OF TS_INFO.

TYPES: BEGIN OF TS_KEY,
         LIFNR TYPE EINA-LIFNR,
         MATNR TYPE EINA-MATNR,
         EKORG TYPE EINE-EKORG,
         WERKS TYPE EINE-WERKS,
         ESOKZ TYPE EINE-ESOKZ,
       END OF TS_KEY.

TYPES: BEGIN OF TS_EINA,
*      EINA fields
         MATKL TYPE  EINA-MATKL,
         LOEKZ TYPE  EINA-LOEKZ,
         TXZ01 TYPE  EINA-TXZ01,
         SORTL TYPE  EINA-SORTL,
         IDNLF TYPE  EINA-IDNLF,
         URZNR TYPE  EINA-URZNR,
         URZDT TYPE  EINA-URZDT,
         URZLA TYPE  EINA-URZLA,
         URZTP TYPE  EINA-URZTP,
         LMEIN TYPE  EINA-LMEIN,
         REGIO TYPE  EINA-REGIO,
         WGLIF TYPE  EINA-WGLIF,
         MEINS TYPE  EINA-MEINS,
         UMREZ TYPE  EINA-UMREZ,
         UMREN TYPE  EINA-UMREN,
         VERKF TYPE  EINA-VERKF,
         RUECK TYPE  EINA-RUECK,
         MAHN1 TYPE  EINA-MAHN1,
         MAHN2 TYPE  EINA-MAHN2,
         MAHN3 TYPE  EINA-MAHN3,
         TELF1 TYPE  EINA-TELF1,
         KOLIF TYPE  EINA-KOLIF,
         PUNEI TYPE  EINA-PUNEI,
         LTSSF TYPE  EINA-LTSSF,
         LTSNR TYPE  EINA-LTSNR,
         RELIF TYPE  EINA-RELIF,
         ANZPU TYPE  EINA-ANZPU,
         VABME TYPE  EINA-VABME,
         URZZT TYPE  EINA-URZZT,
         MFRNR TYPE  EINA-MFRNR,
         LIFAB TYPE  EINA-LIFAB,
         LIFBI TYPE  EINA-LIFBI,
       END OF TS_EINA.

TYPES: BEGIN OF TS_EINE,
*        EINE Fields
         APLFZ   TYPE  EINE-APLFZ,
         EKGRP   TYPE  EINE-EKGRP,
         NORBM   TYPE  EINE-NORBM,
         MINBM   TYPE  EINE-MINBM,
         BSTMA   TYPE  EINE-BSTMA,
         MHDRZ   TYPE  EINE-MHDRZ,
         INCO1   TYPE  EINE-INCO1,
         INCO2   TYPE  EINE-INCO2,
         UNTTO   TYPE  EINE-UNTTO,
         UEBTO   TYPE  EINE-UEBTO,
         WEBRE   TYPE  EINE-WEBRE,
         KZABS   TYPE  EINE-KZABS,
         BSTAE   TYPE  EINE-BSTAE,
         MWSKZ   TYPE  EINE-MWSKZ,
         MEPRF   TYPE  EINE-MEPRF,
         BWTAR   TYPE  EINE-BWTAR,
         EKKOL   TYPE  EINE-EKKOL,
         NETPR   TYPE  EINE-NETPR,
         NETPR_F TYPE  F,
         PEINH   TYPE  EINE-PEINH,
         UEBTK   TYPE  EINE-UEBTK,
         XERSN   TYPE  EINE-XERSN,
         EVERS   TYPE  EINE-EVERS,
         MTXNO   TYPE  EINE-MTXNO,
         EXPRF   TYPE  EINE-EXPRF,
         RDPRF   TYPE  EINE-RDPRF,
         MEGRU   TYPE  EINE-MEGRU,
         LOEKZ   TYPE  EINE-LOEKZ,
         IPRKZ   TYPE  EINE-IPRKZ,
         VERID   TYPE  EINE-VERID,
         WAERS   TYPE  EINE-WAERS,
         BPRME   TYPE  EINE-BPRME,
         SKTOF   TYPE  EINE-SKTOF,
         BPUMN   TYPE  EINE-BPUMN,
         BPUMZ   TYPE  EINE-BPUMZ,
       END OF TS_EINE.

TYPES: BEGIN OF TS_EINAX,
*        EINA fields
         MATKL TYPE  FLAG,
         LOEKZ TYPE  FLAG,
         TXZ01 TYPE  FLAG,
         SORTL TYPE  FLAG,
         IDNLF TYPE  FLAG,
         URZNR TYPE  FLAG,
         URZDT TYPE  FLAG,
         URZLA TYPE  FLAG,
         URZTP TYPE  FLAG,
         LMEIN TYPE  FLAG,
         REGIO TYPE  FLAG,
         WGLIF TYPE  FLAG,
         MEINS TYPE  FLAG,
         UMREZ TYPE  FLAG,
         UMREN TYPE  FLAG,
         VERKF TYPE  FLAG,
         RUECK TYPE  FLAG,
         MAHN1 TYPE  FLAG,
         MAHN2 TYPE  FLAG,
         MAHN3 TYPE  FLAG,
         TELF1 TYPE  FLAG,
         KOLIF TYPE  FLAG,
         PUNEI TYPE  FLAG,
         LTSSF TYPE  FLAG,
         LTSNR TYPE  FLAG,
         RELIF TYPE  FLAG,
         YYPUL TYPE  FLAG,
         ANZPU TYPE  FLAG,
         VABME TYPE  FLAG,
         URZZT TYPE  FLAG,
         MFRNR TYPE  FLAG,
         LIFAB TYPE  FLAG,
         LIFBI TYPE  FLAG,
       END OF TS_EINAX.

TYPES: BEGIN OF TS_EINEX,
*        EINE Fields
         APLFZ TYPE  FLAG,
         EKGRP TYPE  FLAG,
         NORBM TYPE  FLAG,
         MINBM TYPE  FLAG,
         BSTMA TYPE  FLAG,
         MHDRZ TYPE  FLAG,
         INCO1 TYPE  FLAG,
         INCO2 TYPE  FLAG,
         UNTTO TYPE  FLAG,
         UEBTO TYPE  FLAG,
         WEBRE TYPE  FLAG,
         KZABS TYPE  FLAG,
         BSTAE TYPE  FLAG,
         MWSKZ TYPE  FLAG,
         MEPRF TYPE  FLAG,
         BWTAR TYPE  FLAG,
         EKKOL TYPE  FLAG,
         NETPR TYPE  FLAG,
         PEINH TYPE  FLAG,
         UEBTK TYPE  FLAG,
         XERSN TYPE  FLAG,
         EVERS TYPE  FLAG,
         MTXNO TYPE  FLAG,
         EXPRF TYPE  FLAG,
         RDPRF TYPE  FLAG,
         MEGRU TYPE  FLAG,
         LOEKZ TYPE  FLAG,
         IPRKZ TYPE  FLAG,
         VERID TYPE  FLAG,
         WAERS TYPE  FLAG,
         BPRME TYPE  FLAG,
         SKTOF TYPE  FLAG,
         BPUMN TYPE  FLAG,
         BPUMZ TYPE  FLAG,
       END OF TS_EINEX.

TYPES: BEGIN OF TS_EXTRA,
         KALSM TYPE T685A-KALSM,
         PLIFZ TYPE MARC-PLIFZ,
       END OF TS_EXTRA.

TYPES: BEGIN OF TS_CONDDET,
*  Key
         KSCHL   TYPE KONP-KSCHL,
         DATAB   TYPE RV13A-DATAB,
         DATBI   TYPE RV13A-DATBI,
* Detail
         KOPOS   TYPE KONP-KOPOS,
         KBETR   TYPE KONP-KBETR,
         KBETR_F TYPE F,
         KONWA   TYPE KONP-KONWA,
         KPEIN   TYPE KONP-KPEIN,
         KMEIN   TYPE KONP-KMEIN,
         KZBZG   TYPE KONP-KZBZG,
       END OF TS_CONDDET.
TYPES: TT_CONDDET TYPE STANDARD TABLE OF TS_CONDDET.

TYPES: BEGIN OF TS_TEXT,
         TDOBJECT TYPE TDOBJECT,
         TDNAME   TYPE TDOBNAME,
         TDID     TYPE TDID,
         TDSPRAS  TYPE SPRAS,
         TDFORMAT TYPE TDTEXTTYPE,
         TDLINE   TYPE TDLINE,
       END OF TS_TEXT.
TYPES: TT_TEXT TYPE STANDARD TABLE OF TS_TEXT
                      WITH DEFAULT KEY.

TYPES: TS_MSGTX  TYPE  BAPI_MSG.

TYPES: BEGIN OF TS_MESSG,
         MSGTY TYPE  SY-MSGTY,
         MSGID TYPE  SY-MSGID,
         MSGNO TYPE  SY-MSGNO,
         MSGTX TYPE  TS_MSGTX,
       END OF TS_MESSG.
TYPES: TT_MESSG  TYPE  STANDARD TABLE OF TS_MESSG
                         WITH DEFAULT KEY.

TYPES: BEGIN OF TS_DATA,
         ROWNO   TYPE  ZSDSMMS027-ROWNO,
         INFO    TYPE  TS_INFO,
         KEY     TYPE  TS_KEY,
         EINA    TYPE  TS_EINA,
         EINAX   TYPE  TS_EINAX,
         EINE    TYPE  TS_EINE,
         EINEX   TYPE  TS_EINEX,
         EXTRA   TYPE  TS_EXTRA,
         CONDDET TYPE  TS_CONDDET,
         LTEXT   TYPE  TT_TEXT,
         MESSG   TYPE  TT_MESSG,
       END OF TS_DATA.
TYPES: TT_DATA  TYPE  STANDARD TABLE OF TS_DATA.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE        TYPE  CHAR1       VALUE 'X',

  GC_KVEWE       TYPE  T683S-KVEWE VALUE 'A',
  GC_KAPPL       TYPE  T683S-KAPPL VALUE 'M',

* Text ID
  GC_OBJEC_INTXT TYPE  TS_TEXT-TDOBJECT VALUE 'EINA',
  GC_OBJEC_POTXT TYPE  TS_TEXT-TDOBJECT VALUE 'EINE',

* Field value translated as not filled
  GC_DUMMY_FLD   TYPE  CHAR1       VALUE ' ',

* Splitter used in Raw data
  GC_SPLIT       TYPE  CHAR1
                 VALUE CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT        TYPE  TT_RESULT.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA:
  GS_LOGFILE TYPE  TS_FILE_INFO,
  GS_SUM     TYPE  TS_SUM.
*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSMMS027'.
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
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* Text-s01: Upload Info Record
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  PARAMETERS:
      P_LFILE  TYPE  STRING LOWER CASE MODIF ID LOC.
  SELECTION-SCREEN BEGIN OF LINE.
*     Text-s02: Start Row
    SELECTION-SCREEN COMMENT 1(29) TEXT-S02 FOR FIELD P_BEGROW.
    PARAMETERS:
      P_BEGROW TYPE I DEFAULT 15 MODIF ID LOC.
*     Text-s03: Start Column
    SELECTION-SCREEN COMMENT 50(15) TEXT-S03 FOR FIELD P_BEGCOL.
    PARAMETERS:
*      P_BEGCOL TYPE I DEFAULT 2 MODIF ID LOC. "CH01 DEL
      P_BEGCOL TYPE I DEFAULT 3 MODIF ID LOC. "CH01 ADD
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
*     Text-s04: End Row
    SELECTION-SCREEN COMMENT 1(29) TEXT-S04 FOR FIELD P_ENDROW.
    PARAMETERS:
      P_ENDROW TYPE I DEFAULT 9999 MODIF ID LOC.
*     Text-s05: End Column
    SELECTION-SCREEN COMMENT 50(15) TEXT-S05 FOR FIELD P_ENDCOL.
    PARAMETERS:
*      P_ENDCOL TYPE I DEFAULT 51 MODIF ID LOC.  "CH01 DEL
      P_ENDCOL TYPE I DEFAULT 9999 MODIF ID LOC.  "CH01 ADD
  SELECTION-SCREEN END OF LINE.
  PARAMETERS:
    P_TEST   TYPE FLAG AS CHECKBOX DEFAULT 'X',
    P_LOG    TYPE FLAG AS CHECKBOX DEFAULT 'X',
    P_LOGFIL TYPE STRING LOWER CASE DEFAULT 'C:\Temp\'.

SELECTION-SCREEN END OF BLOCK B1.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_LFILE.
* List Local input File
  PERFORM F_LIST_IFILE CHANGING P_LFILE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_LOGFIL.
* Get folder location for log file
  PERFORM F_GET_FOLDER_NAME CHANGING P_LOGFIL.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Processing data
  PERFORM F_PROCESS_DATA CHANGING GT_RESULT
                                  GS_SUM.
  IF GT_RESULT IS INITIAL.
*   Message: No data found.
    MESSAGE S001(ZSDSCA01).
    RETURN.
  ENDIF.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.

*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Form F_LIST_IFILE
*----------------------------------------------------------------------*
*  Popup for Input file selection
*----------------------------------------------------------------------*
FORM F_LIST_IFILE  CHANGING CF_FILENAME  TYPE  STRING.

  DATA:
    LT_FILE     TYPE  FILETABLE.

  DATA:
    LF_RC     TYPE  I,
    LF_ACTION TYPE  I.

  FIELD-SYMBOLS:
  <L_FILE>  TYPE  FILE_TABLE.

* List Local File
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      DEFAULT_EXTENSION       = '*'
*     File Filter format is '<Text>|<Filtering>;<Text>|<Filtering>'
      FILE_FILTER             = 'Excel File|*.XLSX;*.XLS' ##NO_TEXT
      MULTISELECTION          = SPACE
    CHANGING
      FILE_TABLE              = LT_FILE
      RC                      = LF_RC
      USER_ACTION             = LF_ACTION
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
  IF NOT ( LF_ACTION IS INITIAL AND
           LF_RC     EQ 1 ).
    RETURN.
  ENDIF.

  READ TABLE LT_FILE ASSIGNING <L_FILE>
                     INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Output
  CF_FILENAME = <L_FILE>-FILENAME.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_FOLDER_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_GET_FOLDER_NAME CHANGING CF_FILENAME  TYPE  STRING.

  DATA:
    LF_FOLDER TYPE STRING,
    LF_INIT   TYPE STRING.

  LF_INIT = P_LOGFIL.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE
    EXPORTING
      INITIAL_FOLDER       = LF_INIT
    CHANGING
      SELECTED_FOLDER      = LF_FOLDER
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC EQ 0 AND LF_FOLDER IS NOT INITIAL.
    CF_FILENAME = LF_FOLDER.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*  Form f_get_directory_of_file
*----------------------------------------------------------------------*
*  Get Directory of File
*----------------------------------------------------------------------*
FORM F_GET_DIRECTORY_OF_FILE  USING  UF_FILE  TYPE CLIKE
                            CHANGING CF_DIR   TYPE CLIKE.

  DATA:
    LF_SEP  TYPE  HCS_DIRECTORY_SEPARATOR,
    LF_OFFS TYPE  I.


* Initialize Output
  CLEAR: CF_DIR.

* Get Separator
  CALL METHOD CL_HCS_DIRECTORY_ACCESS=>GET_SEPARATOR
    RECEIVING
      R_SEPARATOR = LF_SEP.

* Find last separator
  FIND ALL OCCURRENCES OF LF_SEP IN UF_FILE
                                 MATCH OFFSET LF_OFFS.

  LF_OFFS = LF_OFFS + 1.

* Assign Output
  CF_DIR = UF_FILE(LF_OFFS).

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_assign_logfile
*----------------------------------------------------------------------*
*  Assign Log filename
*----------------------------------------------------------------------*
FORM F_ASSIGN_LOGFILE  USING  CF_DIR  TYPE  CLIKE
                     CHANGING CS_LOGFILE TYPE  TS_FILE_INFO.

* Initialize Output
  CLEAR: CS_LOGFILE.

  CS_LOGFILE-DIRECTORY = CF_DIR.

* File name format:  Upload_Info_Record_Log_YYYYMMDD_HHMMSS.txt
  CONCATENATE 'Upload_Info_Record_Log_'  ##NO_TEXT
               SY-DATLO
               '_'
               SY-TIMLO
               '.txt'
         INTO CS_LOGFILE-FILENAME.

* Assign Fullname
  CONCATENATE CS_LOGFILE-DIRECTORY
              '\'
              CS_LOGFILE-FILENAME
         INTO CS_LOGFILE-FULLNAME.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_process_data
*----------------------------------------------------------------------*
*  Processing data
*----------------------------------------------------------------------*
FORM F_PROCESS_DATA  CHANGING CT_RESULT TYPE TT_RESULT
                              CS_SUM    TYPE TS_SUM.

  DATA:
    LT_RESULT TYPE  TT_RESULT,
    LT_RAW    TYPE  TT_RAW,
    LT_DATA   TYPE  TT_DATA.

* --------------------------------
* Step1: Read Input file data
* --------------------------------
  PERFORM F_READ_INPUT_FILE CHANGING LT_RAW
                                     LT_RESULT.

* --------------------------------
* Step2: Validate Input file
* --------------------------------
  PERFORM F_VALIDATE_FILE USING  LT_RAW
                          CHANGING LT_DATA.

* --------------------------------
* Step3: Upload data from file
* --------------------------------
* Upload file
  PERFORM F_UPLOAD_FILE  USING  LT_DATA
                                P_TEST
                       CHANGING CT_RESULT
                                CS_SUM.

* --------------------------------
* Step4: Create Log File
* --------------------------------

  IF P_LOG EQ GC_TRUE .

    PERFORM F_ASSIGN_LOGFILE  USING  P_LOGFIL
                            CHANGING GS_LOGFILE.
*     Generate Log File
    PERFORM F_CREATE_LOGFILE  USING  CS_SUM
                                     GS_LOGFILE
                                     CT_RESULT.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_read_input_file
*----------------------------------------------------------------------*
*  Read input file into Raw internal table
*----------------------------------------------------------------------*
FORM F_READ_INPUT_FILE  CHANGING CT_RAW    TYPE  TT_RAW
                                 CT_RESULT TYPE  TT_RESULT.

* Show Progress
* Text-p01 : Reading Input file. . .
  MC_SHOW_PROGRESS 10 TEXT-P01.

  PERFORM F_READ_EXCEL USING   P_LFILE
                               P_BEGROW
                               P_BEGCOL
                               P_ENDROW
                               P_ENDCOL
                      CHANGING CT_RAW
                               CT_RESULT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_read_excel
*----------------------------------------------------------------------*
*  Read Excel file into Raw internal table
*----------------------------------------------------------------------*
FORM F_READ_EXCEL  USING  UF_IFILE   TYPE CLIKE
                          UF_BEGROW  TYPE I
                          UF_BEGCOL  TYPE I
                          UF_ENDROW  TYPE I
                          UF_ENDCOL  TYPE I
                 CHANGING CT_RAW     TYPE TT_RAW
                          CT_RESULT  TYPE TT_RESULT.

  DATA:
  LT_TAB TYPE STANDARD TABLE OF ZSDSCAS016.

  DATA:
  LS_RAW TYPE TS_RAW.

  DATA:
    LF_FILENAME TYPE  STRING,
    LF_ROW      TYPE  I,
    LF_COL      TYPE  I.

  FIELD-SYMBOLS:
  <L_TAB>  TYPE ZSDSCAS016.

* Initialize Output
  REFRESH: CT_RAW,
           CT_RESULT.

* Assign File name
  LF_FILENAME = UF_IFILE.

* Read xls File to Internal Table
  CALL FUNCTION 'Z_SDSCA_EXCEL_TO_ITAB'
    EXPORTING
      FILENAME                = LF_FILENAME
      I_BEGIN_COL             = UF_BEGCOL
      I_BEGIN_ROW             = UF_BEGROW
      I_END_COL               = UF_ENDCOL
      I_END_ROW               = UF_ENDROW
    TABLES
      INTERN                  = LT_TAB
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.

  ENDIF.

  SORT LT_TAB BY ROW ASCENDING
                 COL ASCENDING.

  LOOP AT LT_TAB ASSIGNING <L_TAB>.
*   -----------
*   On New Row
*   -----------
    IF <L_TAB>-ROW NE LF_ROW.

      IF LF_ROW IS NOT INITIAL.
        INSERT LS_RAW INTO TABLE CT_RAW.
      ENDIF.

*     Initialize New Line
      LF_ROW = <L_TAB>-ROW.
      LF_COL = 1.
      CLEAR LS_RAW.

    ENDIF.

*   -----------
*   Add Blank Cell
*   -----------
    WHILE LF_COL LT <L_TAB>-COL.
      IF LF_COL GT 1.
        CONCATENATE LS_RAW-TLINE GC_SPLIT
               INTO LS_RAW-TLINE.
      ENDIF.
      LF_COL = LF_COL + 1.
    ENDWHILE.

*   -----------
*   Assign column value
*   -----------
    IF LF_COL EQ 1.
      LS_RAW-TLINE = <L_TAB>-VALUE.
    ELSE.
      CONCATENATE LS_RAW-TLINE <L_TAB>-VALUE
             INTO LS_RAW-TLINE
        SEPARATED BY GC_SPLIT.
    ENDIF.

    LF_COL = LF_COL + 1.

*   Insert last line
    AT LAST.
      INSERT LS_RAW INTO TABLE CT_RAW.
    ENDAT.

  ENDLOOP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_file
*----------------------------------------------------------------------*
*  Validate File data
*----------------------------------------------------------------------*
FORM F_VALIDATE_FILE USING  UT_RAW     TYPE  TT_RAW
                     CHANGING CT_DATA  TYPE  TT_DATA.

  DATA:
    LT_LTEXT    TYPE  TT_TEXT.

  DATA:
    LS_KEY     TYPE  TS_KEY,
    LS_EINA    TYPE  TS_EINA,
    LS_EINAX   TYPE  TS_EINAX,
    LS_EINE    TYPE  TS_EINE,
    LS_EINEX   TYPE  TS_EINEX,
    LS_CONDDET TYPE  TS_CONDDET,
    LS_MESSG   TYPE  TS_MESSG.

  DATA:
  LF_ROWNO   TYPE  ZSDSMMS027-ROWNO.

  FIELD-SYMBOLS:
  <L_RAW>   TYPE  TS_RAW.


* Initialize Output
  REFRESH: CT_DATA.

* Show Progress
* Text-p02 : Validating file data. . .
  MC_SHOW_PROGRESS 20 TEXT-P02.

  LOOP AT UT_RAW ASSIGNING <L_RAW>.

    LF_ROWNO = LF_ROWNO + 1.

*   Translate Result into variables
    PERFORM F_TRANSLATE_RAW USING  <L_RAW>
                          CHANGING LS_KEY
                                   LS_EINA
                                   LS_EINAX
                                   LS_EINE
                                   LS_EINEX
                                   LS_CONDDET
                                   LT_LTEXT
                                   LS_MESSG.

*   Validate and Append data into table
    PERFORM F_VALIDATE_AND_APPEND  USING  LF_ROWNO
                                          LS_KEY
                                          LS_EINA
                                          LS_EINAX
                                          LS_EINE
                                          LS_EINEX
                                          LS_CONDDET
                                          LT_LTEXT
                                          LS_MESSG
                                 CHANGING CT_DATA.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_translate_raw
*----------------------------------------------------------------------*
*  Convert Uploaded data
*----------------------------------------------------------------------*
FORM F_TRANSLATE_RAW  USING  US_RAW       TYPE  TS_RAW
                    CHANGING CS_KEY       TYPE  TS_KEY
                             CS_EINA      TYPE  TS_EINA
                             CS_EINAX     TYPE  TS_EINAX
                             CS_EINE      TYPE  TS_EINE
                             CS_EINEX     TYPE  TS_EINEX
                             CS_CONDDET  TYPE  TS_CONDDET
                             CT_TEXT      TYPE  TT_TEXT
                             CS_MESSG     TYPE  TS_MESSG.

  DATA:
    LT_SPLIT  TYPE  STANDARD TABLE OF STRING.

  DATA:
    LS_TEXT     TYPE  TS_TEMPLATE_TEXT.

  DATA:
    LF_INDEX    TYPE  I,
    LF_FIELD    TYPE  TS_FIELD_NAME,
    LF_STRING   TYPE  STRING,
    LF_MSGTX    TYPE  TS_MESSG-MSGTX,
    LF_REQUIRED TYPE  CHAR1.


* Initialize Output
  CLEAR: CS_KEY,
         CS_EINA,
         CS_EINAX,
         CS_EINE,
         CS_EINEX,
         CS_CONDDET,
         CS_MESSG.
  REFRESH: CT_TEXT.

* Split Into Fields
  SPLIT US_RAW-TLINE AT GC_SPLIT INTO TABLE LT_SPLIT.

*   Initialize Variables
  CLEAR: LF_MSGTX.

  LOOP AT LT_SPLIT INTO LF_STRING.

    LF_INDEX = SY-TABIX.

*   Initialize Variables
    CLEAR: LF_MSGTX.

*   Ignore Dummy Value
    CHECK LF_STRING NE GC_DUMMY_FLD ##BLANK_OK.

*   Get Target Field name to assign value
    PERFORM F_GET_TARGET_FIELD  USING  LF_INDEX
                              CHANGING LF_FIELD.

    CASE LF_FIELD.
*     ---------------------------
*     Vendor
*     ---------------------------
      WHEN 'LIFNR'.
*       Validate Vendor
        PERFORM F_VALIDATE_VENDOR  USING  LF_STRING
                                 CHANGING CS_KEY-LIFNR
                                          LF_MSGTX.

*     ---------------------------
*     Material Number
*     ---------------------------
      WHEN 'MATNR'.
*       Validate Material number
        PERFORM F_VALIDATE_MATNR  USING  LF_STRING
                                CHANGING CS_KEY-MATNR
                                         LF_MSGTX.

*     ---------------------------
*     Purchasing Org
*     ---------------------------
      WHEN 'EKORG'.
*       Validate Purchasing Org.
        PERFORM F_VALIDATE_PURORG  USING  LF_STRING
                                 CHANGING CS_KEY-EKORG
                                          LF_MSGTX.
        IF LF_MSGTX IS INITIAL.
*         Validate Vendor + Org
          PERFORM F_VALIDATE_VENDOR_ORG  USING  CS_KEY-LIFNR
                                                CS_KEY-EKORG
                                       CHANGING LF_MSGTX.
        ENDIF.

*     ---------------------------
*     Plant
*     ---------------------------
      WHEN 'WERKS'.
*       Validate Plant
        PERFORM F_VALIDATE_PLANT  USING  LF_STRING
                                CHANGING CS_KEY-WERKS
                                         LF_MSGTX.

*     ---------------------------
*     Info record category
*     ---------------------------
      WHEN 'ESOKZ'.
*       Validate Info record category
        PERFORM F_VALIDATE_INFOCAT  USING  LF_STRING
                                  CHANGING CS_KEY-ESOKZ
                                           LF_MSGTX.

*     ---------------------------
*     Material class
*     ---------------------------
      WHEN 'MATKL'.
*       Validate Material class
        PERFORM F_VALIDATE_MATCLASS  USING  LF_STRING
                                   CHANGING CS_EINA-MATKL
                                            LF_MSGTX.
        CS_EINAX-MATKL = GC_TRUE.

*     ---------------------------
*     Venor Mat. No.
*     ---------------------------
      WHEN 'IDNLF'.
        CS_EINA-IDNLF  = LF_STRING.
        CS_EINAX-IDNLF = GC_TRUE.

*     ---------------------------
*     sales person
*     ---------------------------
      WHEN 'VERKF'.
        CS_EINA-VERKF  = LF_STRING.
        CS_EINAX-VERKF = GC_TRUE.

*     ---------------------------
*     vendor's telephone number
*     ---------------------------
      WHEN 'TELF1'.
        CS_EINA-TELF1  = LF_STRING.
        CS_EINAX-TELF1 = GC_TRUE.

*     ---------------------------
*     Order Unit
*     ---------------------------
      WHEN 'MEINS'.
*       Validate Unit
        PERFORM F_VALIDATE_UNIT  USING  LF_STRING
                               CHANGING CS_EINA-MEINS
                                        LF_MSGTX.
        CS_EINAX-MEINS = GC_TRUE.

*     ---------------------------
*     denominator for
*     ---------------------------
      WHEN 'UMREN'.
*       Validate Amount
        PERFORM F_VALIDATE_AMOUNT  USING  LF_STRING
                                 CHANGING CS_EINA-UMREN
                                          LF_MSGTX.
        CS_EINAX-UMREN = GC_TRUE.

*     ---------------------------
*     Conversion Unit
*     ---------------------------
      WHEN 'MEIN1'.
*       Field is not used for posting

*     ---------------------------
*     equal to
*     ---------------------------
      WHEN 'UMREZ'.
*       Validate Amount
        PERFORM F_VALIDATE_AMOUNT  USING  LF_STRING
                                 CHANGING CS_EINA-UMREZ
                                          LF_MSGTX.
        CS_EINAX-UMREZ = GC_TRUE.

*     ---------------------------
*     Base Unit of Measure
*     ---------------------------
      WHEN 'LMEIN'.
*       Validate Unit
        PERFORM F_VALIDATE_UNIT  USING  LF_STRING
                               CHANGING CS_EINA-LMEIN
                                        LF_MSGTX.
        CS_EINAX-LMEIN = GC_TRUE.

*     ---------------------------
*     Variable order unit active
*     ---------------------------
      WHEN 'VABME'.
        CS_EINA-VABME  = LF_STRING.
        CS_EINAX-VABME = GC_TRUE.

*     ---------------------------
*     Plnd dely time
*     ---------------------------
      WHEN 'APLFZ'.
*       Validate Amount
        PERFORM F_VALIDATE_AMOUNT  USING  LF_STRING
                                 CHANGING CS_EINE-APLFZ
                                          LF_MSGTX.
        CS_EINEX-APLFZ = GC_TRUE.

*     ---------------------------
*     purchase group
*     ---------------------------
      WHEN 'EKGRP'.
        CS_EINE-EKGRP  = LF_STRING.
        CS_EINEX-EKGRP = GC_TRUE.

*     ---------------------------
*     Standard qty.
*     ---------------------------
      WHEN 'NORBM'.
*       Validate Amount
        PERFORM F_VALIDATE_AMOUNT  USING  LF_STRING
                                 CHANGING CS_EINE-NORBM
                                          LF_MSGTX.
        CS_EINEX-NORBM = GC_TRUE.

*     ---------------------------
*     minimum qty
*     ---------------------------
      WHEN 'MINBM'.
*       Validate Amount
        PERFORM F_VALIDATE_AMOUNT  USING  LF_STRING
                                 CHANGING CS_EINE-MINBM
                                          LF_MSGTX.
        CS_EINEX-MINBM = GC_TRUE.

*     ---------------------------
*     rem. shelf life
*     ---------------------------
      WHEN 'MHDRZ'.
*       Validate Amount
        PERFORM F_VALIDATE_AMOUNT  USING  LF_STRING
                                 CHANGING CS_EINE-MHDRZ
                                          LF_MSGTX.
        CS_EINEX-MHDRZ = GC_TRUE.

*     ---------------------------
*     Period indicator
*     ---------------------------
      WHEN 'IPRKZ'.
*       Validate Period Indicator
        PERFORM F_VALIDATE_PERIOD_IND  USING  LF_STRING
                                     CHANGING CS_EINE-IPRKZ
                                              LF_MSGTX.
        CS_EINEX-IPRKZ = GC_TRUE.

*     ---------------------------
*     minimum qty
*     ---------------------------
      WHEN 'BSTMA'.
*       Validate Amount
        PERFORM F_VALIDATE_AMOUNT  USING  LF_STRING
                                 CHANGING CS_EINE-BSTMA
                                          LF_MSGTX.
        CS_EINEX-BSTMA = GC_TRUE.

*     ---------------------------
*     underdel. tol.
*     ---------------------------
      WHEN 'UNTTO'.
*       Validate Amount
        PERFORM F_VALIDATE_AMOUNT  USING  LF_STRING
                                 CHANGING CS_EINE-UNTTO
                                          LF_MSGTX.
        CS_EINEX-UNTTO = GC_TRUE.

*     ---------------------------
*     overdel. tol.
*     ---------------------------
      WHEN 'UEBTO'.
*       Validate Amount
        PERFORM F_VALIDATE_AMOUNT  USING  LF_STRING
                                 CHANGING CS_EINE-UEBTO
                                          LF_MSGTX.
        CS_EINEX-UEBTO = GC_TRUE.

*     ---------------------------
*     Indicator: Unlimited overdelivery allowed
*     ---------------------------
      WHEN 'UEBTK'.
        CS_EINE-UEBTK  = LF_STRING.
        CS_EINEX-UEBTK = GC_TRUE.

*     ---------------------------
*     GR-based IV
*     ---------------------------
      WHEN 'WEBRE'.
        CS_EINE-WEBRE  = LF_STRING.
        CS_EINEX-WEBRE = GC_TRUE.

*     ---------------------------
*     tax code
*     ---------------------------
      WHEN 'MWSKZ'.
        CS_EINE-MWSKZ  = LF_STRING.
        CS_EINEX-MWSKZ = GC_TRUE.

*     ---------------------------
*     Incoterms
*     ---------------------------
      WHEN 'INCO1'.
        CS_EINE-INCO1  = LF_STRING.
        CS_EINEX-INCO1 = GC_TRUE.

*     ---------------------------
*     Inco Term 2
*     ---------------------------
      WHEN 'INCO2'.
        CS_EINE-INCO2  = LF_STRING.
        CS_EINEX-INCO2 = GC_TRUE.

*     ---------------------------
*     Order Acknowledgment Requirement
*     ---------------------------
      WHEN 'KZABS'.
        CS_EINE-KZABS  = LF_STRING.
        CS_EINEX-KZABS = GC_TRUE.

*     ---------------------------
*     Confirmation Control Key
*     ---------------------------
      WHEN 'BSTAE'.
        CS_EINE-BSTAE  = LF_STRING.
        CS_EINEX-BSTAE = GC_TRUE.

*     ---------------------------
*     Validity start date of the condition record
*     ---------------------------
      WHEN 'DATAB'.
*       Validate Date
        PERFORM F_VALIDATE_DATE  USING  LF_STRING
                               CHANGING CS_CONDDET-DATAB
                                        LF_MSGTX.

*     ---------------------------
*     Validity end date of the condition record
*     ---------------------------
      WHEN 'DATBI'.
*       Validate Date
        PERFORM F_VALIDATE_DATE  USING  LF_STRING
                               CHANGING CS_CONDDET-DATBI
                                        LF_MSGTX.

*     ---------------------------
*     Condition type
*     ---------------------------
      WHEN 'KSCHL'.
        CS_CONDDET-KSCHL  = LF_STRING.

*     ---------------------------
*     condition rate
*     ---------------------------
      WHEN 'KBETR'.
*       Validate Amount
        PERFORM F_VALIDATE_AMOUNT  USING  LF_STRING
                                 CHANGING CS_CONDDET-KBETR_F
                                          LF_MSGTX.

*     ---------------------------
*     Rate unit (currency or percentage)
*     ---------------------------
      WHEN 'KONWA'.
*       Validate Currency
        PERFORM F_VALIDATE_CURENCY  USING  LF_STRING
                                  CHANGING CS_CONDDET-KONWA
                                           LF_MSGTX.

*     ---------------------------
*     Condition pricing unit
*     ---------------------------
      WHEN 'KPEIN'.
*       Validate Amount
        PERFORM F_VALIDATE_AMOUNT  USING  LF_STRING
                                 CHANGING CS_CONDDET-KPEIN
                                          LF_MSGTX.

*     ---------------------------
*     Condition unit
*     ---------------------------
      WHEN 'KMEIN'.
*       Validate Unit
        PERFORM F_VALIDATE_UNIT  USING  LF_STRING
                               CHANGING CS_CONDDET-KMEIN
                                        LF_MSGTX.

*     ---------------------------
*     Info Text Language
*     ---------------------------
      WHEN 'INTXT_SPRAS'.
*       Validate Language
        PERFORM F_VALIDATE_LANGUAGE  USING  LF_STRING
                                   CHANGING LS_TEXT-INTXT_SPRAS
                                            LF_MSGTX.

*     ---------------------------
*     Info Text Line 1
*     ---------------------------
      WHEN 'INTXT_LINE1'.
        APPEND LF_STRING TO LS_TEXT-INTXT_LINE.

*     ---------------------------
*     Info Text Line 2
*     ---------------------------
      WHEN 'INTXT_LINE2'.
        APPEND LF_STRING TO LS_TEXT-INTXT_LINE.

*     ---------------------------
*     Info Text Line 3
*     ---------------------------
      WHEN 'INTXT_LINE3'.
        APPEND LF_STRING TO LS_TEXT-INTXT_LINE.

*     ---------------------------
*     Info Text Line 4
*     ---------------------------
      WHEN 'INTXT_LINE4'.
        APPEND LF_STRING TO LS_TEXT-INTXT_LINE.

*     ---------------------------
*     Info Text Line 5
*     ---------------------------
      WHEN 'INTXT_LINE5'.
        APPEND LF_STRING TO LS_TEXT-INTXT_LINE.

*     ---------------------------
*     PO Text Language
*     ---------------------------
      WHEN 'POTXT_SPRAS'.
*       Validate Language
        PERFORM F_VALIDATE_LANGUAGE  USING  LF_STRING
                                   CHANGING LS_TEXT-POTXT_SPRAS
                                            LF_MSGTX.

*     ---------------------------
*     PO Text Line 1
*     ---------------------------
      WHEN 'POTXT_LINE1'.
        APPEND LF_STRING TO LS_TEXT-POTXT_LINE.

*     ---------------------------
*     PO Text Line 2
*     ---------------------------
      WHEN 'POTXT_LINE2'.
        APPEND LF_STRING TO LS_TEXT-POTXT_LINE.

*     ---------------------------
*     PO Text Line 3
*     ---------------------------
      WHEN 'POTXT_LINE3'.
        APPEND LF_STRING TO LS_TEXT-POTXT_LINE.

*     ---------------------------
*     PO Text Line 4
*     ---------------------------
      WHEN 'POTXT_LINE4'.
        APPEND LF_STRING TO LS_TEXT-POTXT_LINE.

*     ---------------------------
*     PO Text Line 5
*     ---------------------------
      WHEN 'POTXT_LINE5'.
        APPEND LF_STRING TO LS_TEXT-POTXT_LINE.

*       This is the last field that must be processed
*       even its previous fields are error
        LF_REQUIRED = GC_TRUE.

      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

*   If error, assign message and ignore the rest
    IF LF_MSGTX IS NOT INITIAL.
*     Do not override previous error
      IF CS_MESSG-MSGTY NE 'E'.
        CS_MESSG-MSGTY = 'E'.
        CS_MESSG-MSGTX = LF_MSGTX.
      ENDIF.
*     Processing until all required fields
      IF LF_REQUIRED EQ GC_TRUE.
        EXIT.
      ENDIF.
    ENDIF.

  ENDLOOP.

  IF LS_TEXT IS NOT INITIAL.
    PERFORM F_ASSIGN_TEXT  USING  LS_TEXT
                         CHANGING CT_TEXT.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_get_target_field
*----------------------------------------------------------------------*
*  Get Target Field name based on column sequence
*----------------------------------------------------------------------*
FORM F_GET_TARGET_FIELD  USING  UF_INDEX  TYPE  I
                       CHANGING UF_FIELD  TYPE  TS_FIELD_NAME.

  STATICS:
  LT_FIELDS   TYPE  ABAP_COMPDESCR_TAB.

  DATA:
    LREF_DESCR TYPE  REF TO CL_ABAP_TYPEDESCR,
    LREF_STRUC TYPE  REF TO CL_ABAP_STRUCTDESCR.

  FIELD-SYMBOLS:
  <L_FIELD>  TYPE  ABAP_COMPDESCR.


* Initialize Output
  CLEAR: UF_FIELD.

  IF LT_FIELDS IS INITIAL.
*   Get Template Fields Sequence
    LREF_DESCR = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( 'TS_TEMPLATE' ).
    LREF_STRUC ?= LREF_DESCR.
    LT_FIELDS = LREF_STRUC->COMPONENTS.
  ENDIF.

  READ TABLE LT_FIELDS ASSIGNING <L_FIELD>
                       INDEX UF_INDEX.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Output
  UF_FIELD = <L_FIELD>-NAME.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_vendor
*----------------------------------------------------------------------*
*  text
*----------------------------------------------------------------------*
FORM F_VALIDATE_VENDOR  USING  UF_STRING  TYPE  STRING
                      CHANGING CF_LIFNR   TYPE  LFA1-LIFNR
                               CF_MSGTX   TYPE  CLIKE.

  TYPES: BEGIN OF LTS_LFA1,
           LIFNR TYPE  LFA1-LIFNR,
         END OF LTS_LFA1.
  TYPES: LTT_LFA1  TYPE  SORTED TABLE OF LTS_LFA1
  WITH UNIQUE KEY LIFNR.

  STATICS:
    LT_LFA1 TYPE  LTT_LFA1,
    LS_LFA1 TYPE  LTS_LFA1.

  DATA:
    LF_LIFNR  TYPE  LFA1-LIFNR,
    LF_LENGTH TYPE  I.


* Initialize Output
  CLEAR: CF_LIFNR,
         CF_MSGTX.

* Only not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 10.
*   Text-e01 : Invalid Vendor:
    CONCATENATE TEXT-E01 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Convert to internal format
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = UF_STRING
    IMPORTING
      OUTPUT = LF_LIFNR.

* Check Buffer
  IF LS_LFA1-LIFNR NE LF_LIFNR.
*   Validate with Memory
    READ TABLE LT_LFA1 INTO LS_LFA1
                       WITH KEY LIFNR = LF_LIFNR
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_LFA1.
*     Validate with Database
      SELECT SINGLE LIFNR                            "#EC CI_SEL_NESTED
        INTO LS_LFA1
        FROM LFA1
       WHERE LIFNR  EQ  LF_LIFNR.
      IF SY-SUBRC NE 0.
*       Text-e01 : Invalid Vendor:
        CONCATENATE TEXT-E01 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_LFA1 INTO TABLE LT_LFA1.
    ENDIF.

  ENDIF.

* Assign Output
  CF_LIFNR = LS_LFA1-LIFNR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_matnr
*----------------------------------------------------------------------*
*  Validate Material Number
*----------------------------------------------------------------------*
FORM F_VALIDATE_MATNR  USING  UF_STRING  TYPE  STRING
                     CHANGING CF_MATNR   TYPE  MARA-MATNR
                              CF_MSGTX   TYPE  CLIKE.

  TYPES: BEGIN OF LTS_MARA,
           MATNR TYPE  MARA-MATNR,
         END OF LTS_MARA.
  TYPES: LTT_MARA  TYPE  SORTED TABLE OF LTS_MARA
  WITH UNIQUE KEY MATNR.

  STATICS:
    LT_MARA TYPE  LTT_MARA,
    LS_MARA TYPE  LTS_MARA.

  DATA:
  LF_MATNR  TYPE  MARA-MATNR.


* Initialize Output
  CLEAR: CF_MATNR,
         CF_MSGTX.

* Only when not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Convert to internal format
  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      INPUT        = UF_STRING
    IMPORTING
      OUTPUT       = LF_MATNR
    EXCEPTIONS
      LENGTH_ERROR = 1
      OTHERS       = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
            INTO CF_MSGTX.
    RETURN.
  ENDIF.

* Check Buffer
  IF LS_MARA-MATNR NE LF_MATNR.
*   Validate with Memory
    READ TABLE LT_MARA INTO LS_MARA
                       WITH KEY MATNR = LF_MATNR
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_MARA.
*     Validate with Database
      SELECT SINGLE MATNR                            "#EC CI_SEL_NESTED
        INTO LS_MARA
        FROM MARA
       WHERE MATNR  EQ  LF_MATNR.
      IF SY-SUBRC NE 0.
*       Text-e02 : Invalid Material number:
        CONCATENATE TEXT-E02 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_MARA INTO TABLE LT_MARA.
    ENDIF.

  ENDIF.

* Assign Output
  CF_MATNR = LF_MATNR.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_validate_purorg
*----------------------------------------------------------------------*
*       Validate Purchasing Org
*----------------------------------------------------------------------*
FORM F_VALIDATE_PURORG  USING  UF_STRING  TYPE  STRING
                      CHANGING CF_EKORG   TYPE  T024E-EKORG
                               CF_MSGTX   TYPE  CLIKE.

  TYPES: BEGIN OF LTS_T024E,
           EKORG TYPE  T024E-EKORG,
           KALSE TYPE  T024E-KALSE,
         END OF LTS_T024E.
  TYPES: LTT_T024E  TYPE  SORTED TABLE OF LTS_T024E
  WITH UNIQUE KEY EKORG.

  STATICS:
    LS_T024E TYPE  LTS_T024E,
    LT_T024E TYPE  LTT_T024E.

  DATA:
    LF_LEN   TYPE  I,
    LF_EKORG TYPE  LTS_T024E-EKORG.


* Initialize Output
  CLEAR: CF_EKORG,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 4
  LF_LEN = STRLEN( UF_STRING ).
  IF LF_LEN NE 4.
*   Text-e05 : Invalid purchasing org.:
    CONCATENATE TEXT-E05 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LF_EKORG = UF_STRING.

* Check Buffer
  IF LS_T024E-EKORG NE LF_EKORG.
*   Validate with Memory
    READ TABLE LT_T024E INTO LS_T024E
                        WITH KEY EKORG = LF_EKORG
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_T024E.
*     Validate with Database
      SELECT SINGLE EKORG KALSE
        INTO LS_T024E
        FROM T024E
       WHERE EKORG EQ LF_EKORG.
      IF SY-SUBRC NE 0.
*       Text-e05 : Invalid purchasing org.:
        CONCATENATE TEXT-E05 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_T024E INTO TABLE LT_T024E.
    ENDIF.

  ENDIF.

* Assign Output
  CF_EKORG = LS_T024E-EKORG.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_validate_vendor_org
*----------------------------------------------------------------------*
*       Validate Vendor + Purchasing Org
*----------------------------------------------------------------------*
FORM F_VALIDATE_VENDOR_ORG  USING  UF_LIFNR  TYPE  LFM1-LIFNR
                                   UF_EKORG  TYPE  LFM1-EKORG
                          CHANGING CF_MSGTX  TYPE  CLIKE.

  TYPES: BEGIN OF LTS_LFM1,
           LIFNR TYPE  LFM1-LIFNR,
           EKORG TYPE  LFM1-EKORG,
           KALSK TYPE  LFM1-KALSK,
         END OF LTS_LFM1.
  TYPES: LTT_LFM1  TYPE  SORTED TABLE OF LTS_LFM1
                          WITH UNIQUE KEY LIFNR
  EKORG.

  STATICS:
    LS_LFM1 TYPE  LTS_LFM1,
    LT_LFM1 TYPE  LTT_LFM1.


* Initialize Output
  CLEAR: CF_MSGTX.

* Check Buffer
  IF LS_LFM1-LIFNR NE UF_LIFNR OR
     LS_LFM1-EKORG NE UF_EKORG .
*   Validate with Memory
    READ TABLE LT_LFM1 INTO LS_LFM1
                       WITH KEY LIFNR = UF_LIFNR
                                EKORG = UF_EKORG
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_LFM1.
*     Validate with Database
      SELECT SINGLE LIFNR EKORG KALSK                "#EC CI_SEL_NESTED
        INTO LS_LFM1
        FROM LFM1
       WHERE LIFNR EQ UF_LIFNR
         AND EKORG EQ UF_EKORG.
      IF SY-SUBRC NE 0.
*       Error : Supplier & not yet created by purchasing organization &
        MESSAGE ID '06'
                TYPE 'E'
                NUMBER '321'
                WITH UF_LIFNR UF_EKORG
                INTO CF_MSGTX.
        RETURN.
      ENDIF.
      INSERT LS_LFM1 INTO TABLE LT_LFM1.
    ENDIF.

  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_validate_plant
*----------------------------------------------------------------------*
*       Validate Plant
*----------------------------------------------------------------------*
FORM F_VALIDATE_PLANT  USING  UF_STRING TYPE STRING
                     CHANGING CF_WERKS  TYPE T001W-WERKS
                              CF_MSGTX  TYPE CLIKE.

  TYPES: BEGIN OF LTS_WERKS,
           WERKS TYPE  T001W-WERKS,
         END OF LTS_WERKS.
  TYPES: LTT_WERKS  TYPE  SORTED TABLE OF LTS_WERKS
  WITH UNIQUE KEY WERKS.

  STATICS:
    LS_WERKS TYPE  LTS_WERKS,
    LT_WERKS TYPE  LTT_WERKS.

  DATA:
    LF_LEN   TYPE  I,
    LF_WERKS TYPE  LTS_WERKS-WERKS.


* Initialize Output
  CLEAR: CF_WERKS,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 4
  LF_LEN = STRLEN( UF_STRING ).
  IF LF_LEN NE 4.
*   Text-e03 : Invalid plant:
    CONCATENATE TEXT-E03 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LF_WERKS = UF_STRING.

* Check Buffer
  IF LS_WERKS-WERKS NE LF_WERKS.
*   Validate with Memory
    READ TABLE LT_WERKS INTO LS_WERKS
                        WITH KEY WERKS = LF_WERKS
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_WERKS.
*     Validate with Database
      SELECT SINGLE T001W~WERKS
        INTO LS_WERKS-WERKS
        FROM T001W
       WHERE T001W~WERKS = LF_WERKS.
      IF SY-SUBRC NE 0.
*       Text-e03 : Invalid plant:
        CONCATENATE TEXT-E03 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_WERKS INTO TABLE LT_WERKS.
    ENDIF.

  ENDIF.

* Assign Output
  CF_WERKS = LS_WERKS-WERKS.

ENDFORM.


*----------------------------------------------------------------------*
*       Form  f_validate_infocat
*----------------------------------------------------------------------*
*       Validate Info Record Category
*----------------------------------------------------------------------*
FORM F_VALIDATE_INFOCAT  USING  UF_STRING TYPE STRING
                       CHANGING CF_ESOKZ  TYPE EINE-ESOKZ
                                CF_MSGTX  TYPE CLIKE.

  TYPES: BEGIN OF LTS_INFOCAT,
           ESOKZ TYPE  DD07L-DOMVALUE_L,
         END OF LTS_INFOCAT.
  TYPES: LTT_INFOCAT  TYPE  SORTED TABLE OF LTS_INFOCAT
  WITH UNIQUE KEY ESOKZ.

  STATICS:
    LS_INFOCAT TYPE  LTS_INFOCAT,
    LT_INFOCAT TYPE  LTT_INFOCAT.

  DATA:
    LF_LEN   TYPE  I,
    LF_ESOKZ TYPE  LTS_INFOCAT-ESOKZ.


* Initialize Output
  CLEAR: CF_ESOKZ,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 1
  LF_LEN = STRLEN( UF_STRING ).
  IF LF_LEN NE 1.
*   Text-e06 : Invalid Info Record Category:
    CONCATENATE TEXT-E06 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LF_ESOKZ = UF_STRING.

* Check Buffer
  IF LS_INFOCAT-ESOKZ NE LF_ESOKZ.
*   Validate with Memory
    READ TABLE LT_INFOCAT INTO LS_INFOCAT
                        WITH KEY ESOKZ = LF_ESOKZ
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.

*     Validate with Database
      SELECT DOMVALUE_L
          UP TO 1 ROWS
        INTO LS_INFOCAT-ESOKZ
        FROM DD07L
       WHERE DOMNAME EQ 'ESOKZ'
         AND AS4LOCAL EQ 'A'
         AND DOMVALUE_L EQ LF_ESOKZ
         AND DOMVALUE_L NE '1'
       ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF SY-SUBRC NE 0.
*       Text-e06 : Invalid Info Record Category:
        CONCATENATE TEXT-E06 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_INFOCAT INTO TABLE LT_INFOCAT.
    ENDIF.

  ENDIF.

* Assign Output
  CF_ESOKZ = LS_INFOCAT-ESOKZ.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_validate_unit
*----------------------------------------------------------------------*
*       Validate Unit
*----------------------------------------------------------------------*
FORM F_VALIDATE_UNIT  USING  UF_STRING  TYPE  STRING
                    CHANGING CF_MEINS   TYPE  MEINS
                             CF_MSGTX   TYPE  CLIKE.

  DATA:
  LF_LENGTH  TYPE  I.


* Initialize Output
  CLEAR: CF_MEINS,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Check Length
  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 3.
*   Text-e12 : Invalid unit:
    CONCATENATE TEXT-E12 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Convert to input format
  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
    EXPORTING
      INPUT          = UF_STRING
      LANGUAGE       = SY-LANGU
    IMPORTING
      OUTPUT         = CF_MEINS
    EXCEPTIONS
      UNIT_NOT_FOUND = 1
      OTHERS         = 2.
  IF SY-SUBRC <> 0.
*   Text-e12 : Invalid unit:
    CONCATENATE TEXT-E12 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Validate in table
  SELECT SINGLE MSEHI
    INTO CF_MEINS
    FROM T006
   WHERE MSEHI EQ CF_MEINS.
  IF SY-SUBRC NE 0.
    CLEAR CF_MEINS.
*   Text-e12 : Invalid unit:
    CONCATENATE TEXT-E12 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_validate_amount
*----------------------------------------------------------------------*
*       Validate Amount
*----------------------------------------------------------------------*
FORM F_VALIDATE_AMOUNT  USING  UF_STRING  TYPE  STRING
                      CHANGING CF_AMOUNT  TYPE  ANY
                               CF_MSGTX   TYPE  CLIKE.

* Initialize Output
  CLEAR: CF_AMOUNT,
         CF_MSGTX.

* Only when value exist
  CHECK UF_STRING IS NOT INITIAL.

* Move to amount field
  TRY.
      CF_AMOUNT = UF_STRING.
    CATCH CX_ROOT.                                       "#EC CATCH_ALL
*     Text-e14 : Invalid amount value:
      CONCATENATE TEXT-E14 UF_STRING
             INTO CF_MSGTX
        SEPARATED BY SPACE.
      RETURN.
  ENDTRY.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_validate_curency
*----------------------------------------------------------------------*
*       Validate Currency Key
*----------------------------------------------------------------------*
FORM F_VALIDATE_CURENCY  USING  UF_STRING  TYPE  STRING
                       CHANGING CF_WAERS   TYPE  WAERS
                                CF_MSGTX   TYPE  CLIKE.

  TYPES: BEGIN OF LTS_TCURC,
           WAERS TYPE  TCURC-WAERS,
         END OF LTS_TCURC.
  TYPES: LTT_TCURC  TYPE  SORTED TABLE OF LTS_TCURC
  WITH UNIQUE KEY WAERS.

  STATICS:
    LT_TCURC TYPE  LTT_TCURC,
    LS_TCURC TYPE  LTS_TCURC.

  DATA:
    LF_LENGTH TYPE  I,
    LF_WAERS  TYPE  LTS_TCURC-WAERS.

* Initialize Output
  CLEAR: CF_WAERS,
         CF_MSGTX.

* Only not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Check Length
  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 5.
*   Text-e16 : Invalid currency:
    CONCATENATE TEXT-E16 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LF_WAERS = UF_STRING.
  TRANSLATE LF_WAERS TO UPPER CASE. "CH01 ADD

* Check %, no more validation
  IF LF_WAERS EQ '%'.
    CF_WAERS = LF_WAERS.
    RETURN.
  ENDIF.

* Check Buffer
  IF LF_WAERS NE LS_TCURC-WAERS.
*   Read from Memory
    READ TABLE LT_TCURC INTO LS_TCURC
                        WITH KEY WAERS = LF_WAERS
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TCURC.
*     Read from Database
      SELECT SINGLE WAERS
        INTO LS_TCURC-WAERS
        FROM TCURC
       WHERE WAERS EQ LF_WAERS.
      IF SY-SUBRC NE 0.
*       Text-e16 : Invalid currency:
        CONCATENATE TEXT-E16 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TCURC INTO TABLE LT_TCURC.
    ENDIF.
  ENDIF.

* Assign Output
  CF_WAERS = LS_TCURC-WAERS.

ENDFORM.
*----------------------------------------------------------------------*
*       Form  f_validate_date
*----------------------------------------------------------------------*
*       Validate Date
*----------------------------------------------------------------------*
FORM F_VALIDATE_DATE  USING  UF_STRING  TYPE  STRING
                    CHANGING CF_DATUM   TYPE  SY-DATUM
                             CF_MSGTX   TYPE  CLIKE.

  DATA:
  LF_LENGTH  TYPE  I.

* Initialize Output
  CLEAR: CF_DATUM,
         CF_MSGTX.

* Not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Only number and .?
  IF NOT UF_STRING CO '0123456789.'.
*   Text-e13 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E13 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Length?
  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH NE 10.
*   Text-e13 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E13 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* First 2 Digit is date
  IF NOT UF_STRING(2) BETWEEN 01 AND 31.
*   Text-e13 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E13 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* 4-5th Digit is month
  IF NOT UF_STRING+3(2) BETWEEN 01 AND 12.
*   Text-e13 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E13 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* 7-8th digit is year
  IF NOT UF_STRING+6(4) BETWEEN 1900 AND 2200 AND
     UF_STRING+6(4) NE '9999'.
*   Text-e13 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E13 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Assign Output
  CONCATENATE UF_STRING+6(4) UF_STRING+3(2) UF_STRING(2)
         INTO CF_DATUM.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_matclass
*----------------------------------------------------------------------*
*  Validate Material Class
*----------------------------------------------------------------------*
FORM F_VALIDATE_MATCLASS  USING  UF_STRING  TYPE  STRING
                        CHANGING CF_MATKL   TYPE  T023-MATKL
                                 CF_MSGTX   TYPE  CLIKE.

  TYPES: BEGIN OF LTS_T023,
           MATKL TYPE  T023-MATKL,
         END OF LTS_T023.
  TYPES: LTT_T023  TYPE  SORTED TABLE OF LTS_T023
  WITH UNIQUE KEY MATKL.

  STATICS:
    LS_T023 TYPE  LTS_T023,
    LT_T023 TYPE  LTT_T023.

  DATA:
    LF_LEN   TYPE  I,
    LF_MATKL TYPE  LTS_T023-MATKL.


* Initialize Output
  CLEAR: CF_MATKL,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 9
  LF_LEN = STRLEN( UF_STRING ).
  IF LF_LEN GT 9.
*   Text-e15 : Invalid Material Group:
    CONCATENATE TEXT-E15 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LF_MATKL = UF_STRING.

* Check Buffer
  IF LS_T023-MATKL NE LF_MATKL.
*   Validate with Memory
    READ TABLE LT_T023 INTO LS_T023
                        WITH KEY MATKL = LF_MATKL
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_T023.
*     Validate with Database
      SELECT SINGLE T023~MATKL
        INTO LS_T023-MATKL
        FROM T023
       WHERE T023~MATKL = LF_MATKL.
      IF SY-SUBRC NE 0.
*       Text-e15 : Invalid Material Group:
        CONCATENATE TEXT-E15 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_T023 INTO TABLE LT_T023.
    ENDIF.

  ENDIF.

* Assign Output
  CF_MATKL = LS_T023-MATKL.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_period_ind
*----------------------------------------------------------------------*
*  Validate Period Indicator
*----------------------------------------------------------------------*
FORM F_VALIDATE_PERIOD_IND  USING  UF_STRING  TYPE  STRING
                          CHANGING CF_IPRKZ  TYPE  DATTP
                                   CF_MSGTX  TYPE CLIKE.

  TYPES: BEGIN OF LTS_DD07L,
           IPRKZ TYPE  DD07L-DOMVALUE_L,
         END OF LTS_DD07L.
  TYPES: LTT_DD07L  TYPE  SORTED TABLE OF LTS_DD07L
  WITH UNIQUE KEY IPRKZ.

  STATICS:
    LS_DD07L TYPE  LTS_DD07L,
    LT_DD07L TYPE  LTT_DD07L.

  DATA:
    LF_LEN   TYPE  I,
    LF_IPRKZ TYPE  LTS_DD07L-IPRKZ.


* Initialize Output
  CLEAR: CF_IPRKZ,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 1
  LF_LEN = STRLEN( UF_STRING ).
  IF LF_LEN NE 1.
*   Text-e17 : Invalid Period Indicator:
    CONCATENATE TEXT-E17 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Convert to input format
  CALL FUNCTION 'CONVERSION_EXIT_PERKZ_INPUT'
    EXPORTING
      INPUT         = UF_STRING
    IMPORTING
      OUTPUT        = LF_IPRKZ
    EXCEPTIONS
      ERROR_MESSAGE = 1
      OTHERS        = 2.
  IF SY-SUBRC NE 0.
*   Text-e17 : Invalid Period Indicator:
    CONCATENATE TEXT-E17 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Check Buffer
  IF LS_DD07L-IPRKZ NE LF_IPRKZ.
*   Validate with Memory
    READ TABLE LT_DD07L INTO LS_DD07L
                        WITH KEY IPRKZ = LF_IPRKZ
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.

*     Validate with Database
      SELECT DOMVALUE_L
          UP TO 1 ROWS
        INTO LS_DD07L-IPRKZ
        FROM DD07L
       WHERE DOMNAME EQ 'DATTP'
         AND AS4LOCAL EQ 'A'
         AND DOMVALUE_L EQ LF_IPRKZ
       ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF SY-SUBRC NE 0.
*       Text-e17 : Invalid Period Indicator:
        CONCATENATE TEXT-E17 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_DD07L INTO TABLE LT_DD07L.
    ENDIF.

  ENDIF.

* Assign Output
  CF_IPRKZ = LS_DD07L-IPRKZ.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_language
*----------------------------------------------------------------------*
*  Validate Language
*----------------------------------------------------------------------*
FORM F_VALIDATE_LANGUAGE  USING  UF_STRING  TYPE  STRING
                        CHANGING CF_SPRAS   TYPE  T002-SPRAS
                                 CF_MSGTX   TYPE  CLIKE.

  DATA:
  LF_SPRAS TYPE  T002-SPRAS.


* Initialize Output
  CLEAR: CF_SPRAS,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Convert to Input format
  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
    EXPORTING
      INPUT            = UF_STRING
    IMPORTING
      OUTPUT           = LF_SPRAS
    EXCEPTIONS
      UNKNOWN_LANGUAGE = 1
      OTHERS           = 2.
  IF SY-SUBRC <> 0.
*   Text-e18 : Invalid Language:
    CONCATENATE TEXT-E18 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Assign Output
  CF_SPRAS = LF_SPRAS.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_assign_text
*----------------------------------------------------------------------*
*  Assign Text data from template
*----------------------------------------------------------------------*
FORM F_ASSIGN_TEXT  USING  PS_TEMP_TEXT  TYPE  TS_TEMPLATE_TEXT
                  CHANGING PT_TEXT       TYPE  TT_TEXT.

  DATA:
  LS_TEXT  TYPE  TS_TEXT.

  FIELD-SYMBOLS:
  <L_STRING>  TYPE  STRING.

* Initialize Output
  REFRESH: PT_TEXT.

  IF PS_TEMP_TEXT-INTXT_SPRAS IS NOT INITIAL.

    CLEAR LS_TEXT.
    LS_TEXT-TDOBJECT = GC_OBJEC_INTXT.
    LS_TEXT-TDSPRAS  = PS_TEMP_TEXT-INTXT_SPRAS.
    LOOP AT PS_TEMP_TEXT-INTXT_LINE ASSIGNING <L_STRING>.
      LS_TEXT-TDFORMAT = '*'.
      LS_TEXT-TDLINE   = <L_STRING>.
      APPEND LS_TEXT TO PT_TEXT.
    ENDLOOP.

  ENDIF.

  IF PS_TEMP_TEXT-POTXT_SPRAS IS NOT INITIAL.

    CLEAR LS_TEXT.
    LS_TEXT-TDOBJECT = GC_OBJEC_POTXT.
    LS_TEXT-TDSPRAS  = PS_TEMP_TEXT-POTXT_SPRAS.
    LOOP AT PS_TEMP_TEXT-POTXT_LINE ASSIGNING <L_STRING>.
      LS_TEXT-TDFORMAT = '*'.
      LS_TEXT-TDLINE   = <L_STRING>.
      APPEND LS_TEXT TO PT_TEXT.
    ENDLOOP.

  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_and_append
*----------------------------------------------------------------------*
*  Validate Whole Line data and collect into internal table for posting
*----------------------------------------------------------------------*
FORM F_VALIDATE_AND_APPEND  USING  CF_ROWNO    TYPE  ZSDSMMS027-ROWNO
                                   US_KEY      TYPE  TS_KEY
                                   US_EINA     TYPE  TS_EINA
                                   US_EINAX    TYPE  TS_EINAX
                                   US_EINE     TYPE  TS_EINE
                                   US_EINEX    TYPE  TS_EINEX
                                   US_CONDDET  TYPE  TS_CONDDET
                                   UT_LTEXT    TYPE  TT_TEXT
                                   US_MESSG    TYPE  TS_MESSG
                          CHANGING CT_DATA     TYPE  TT_DATA.

  DATA:
    LS_DATA    TYPE  TS_DATA,
    LS_CONDDET TYPE  TS_CONDDET,
    LS_INFO    TYPE  TS_INFO,
    LS_EXTRA   TYPE  TS_EXTRA,
    LS_MESSG   TYPE  TS_MESSG,
    LS_TEMP    TYPE  TS_MESSG,

    LS_EINA    TYPE  TS_EINA,
    LS_EINAX   TYPE  TS_EINAX,
    LS_EINE    TYPE  TS_EINE,
    LS_EINEX   TYPE  TS_EINEX.

  DATA:
  LF_EXIST   TYPE  FLAG.

  DO 1 TIMES.

*  ---------------------------------
*   Validate Info Record Key
*  ---------------------------------
*   Validate Key
    PERFORM F_VALIDATE_KEY  USING  US_KEY
                          CHANGING LS_INFO
                                   LS_EXTRA
                                   LS_MESSG.
    IF LS_MESSG IS NOT INITIAL.
      EXIT.
    ENDIF.

    CLEAR LF_EXIST.
    READ TABLE CT_DATA TRANSPORTING NO FIELDS
                       WITH KEY KEY = US_KEY.
    IF SY-SUBRC EQ 0.
      LF_EXIST = GC_TRUE.
    ENDIF.

*  ---------------------------------
*   Validate General data
*  ---------------------------------
*   Validate General data
    PERFORM F_VALIDATE_GENERAL  USING  US_EINA
                                       US_EINAX
                                       LS_INFO
                                       LF_EXIST
                              CHANGING LS_EINA
                                       LS_EINAX
                                       LS_MESSG.
    IF LS_MESSG IS NOT INITIAL.
      EXIT.
    ENDIF.

*  ---------------------------------
*   Validate Purch.Org. data
*  ---------------------------------
*   Validate Purch.Org. data
    PERFORM F_VALIDATE_PURCHORG  USING  US_EINE
                                        US_EINEX
                                        LS_INFO
                                        LS_EXTRA
                                        LF_EXIST
                               CHANGING LS_EINE
                                        LS_EINEX
                                        LS_MESSG.
    IF LS_MESSG IS NOT INITIAL.
      EXIT.
    ENDIF.

*  ---------------------------------
*   Validate Condition Detail
*  ---------------------------------
    IF US_CONDDET IS NOT INITIAL.

*     Validate Condition detail
      PERFORM F_VALIDATE_COND_DETAIL  USING  US_CONDDET
                                             LS_EXTRA
                                    CHANGING LS_CONDDET
                                             LS_TEMP.
      IF LS_MESSG IS INITIAL.
        LS_MESSG = LS_TEMP.
      ENDIF.
      IF LS_MESSG IS NOT INITIAL.
        EXIT.
      ENDIF.

    ENDIF.

  ENDDO.

* Generate new key
  CLEAR LS_DATA.
  LS_DATA-ROWNO   = CF_ROWNO.
  LS_DATA-KEY     = US_KEY.
  LS_DATA-INFO    = LS_INFO.
  LS_DATA-EXTRA   = LS_EXTRA.
  LS_DATA-EINA    = LS_EINA.
  LS_DATA-EINAX   = LS_EINAX.
  LS_DATA-EINE    = LS_EINE.
  LS_DATA-CONDDET = LS_CONDDET.
  LS_DATA-EINEX   = LS_EINEX.
  LS_DATA-LTEXT   = UT_LTEXT.

* Collect Message
  IF US_MESSG IS NOT INITIAL.
    APPEND US_MESSG TO LS_DATA-MESSG.
  ELSEIF LS_MESSG IS NOT INITIAL.
    APPEND LS_MESSG TO LS_DATA-MESSG.
  ENDIF.

  INSERT LS_DATA INTO TABLE CT_DATA.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_validate_key
*----------------------------------------------------------------------*
*       Validate Key
*----------------------------------------------------------------------*
FORM F_VALIDATE_KEY  USING  US_KEY     TYPE TS_KEY
                   CHANGING CS_INFO    TYPE TS_INFO
                            CS_EXTRA   TYPE TS_EXTRA
                            CS_MESSG   TYPE TS_MESSG.

  DATA:
    LF_PLIFZ TYPE  MARC-PLIFZ,
    LF_MSGTX TYPE  TS_MESSG-MSGTX.

* Initialize Output
  CLEAR: CS_INFO,
         CS_EXTRA,
         CS_MESSG.

* Vendor Required
  IF US_KEY-LIFNR IS INITIAL.
    CS_MESSG-MSGTY = 'E'.
*   Text-e07 : Missing Vendor in the file.
    CS_MESSG-MSGTX = TEXT-E07.
    RETURN.
  ENDIF.

* Material Required
  IF US_KEY-MATNR IS INITIAL.
    CS_MESSG-MSGTY = 'E'.
*   Text-e08 : Missing Material in the file.
    CS_MESSG-MSGTX = TEXT-E08.
    RETURN.
  ENDIF.

* Purchasing Org Required
  IF US_KEY-EKORG IS INITIAL.
    CS_MESSG-MSGTY = 'E'.
*   Text-e09 : Missing Purchasing Org in the file.
    CS_MESSG-MSGTX = TEXT-E09.
    RETURN.
  ENDIF.

* Plant Required
  IF US_KEY-WERKS IS INITIAL.
    CS_MESSG-MSGTY = 'E'.
*   Text-e10 : Missing Plant in the file.
    CS_MESSG-MSGTX = TEXT-E10.
    RETURN.
  ENDIF.

* Info Record Category Required
  IF US_KEY-ESOKZ IS INITIAL.
    CS_MESSG-MSGTY = 'E'.
*   Text-e11 : Missing Info record category in the file.
    CS_MESSG-MSGTX = TEXT-E11.
    RETURN.
  ENDIF.

* Validate Material + Plant
  PERFORM F_VALIDATE_MAT_PLANT  USING  US_KEY-MATNR
                                       US_KEY-WERKS
                              CHANGING LF_PLIFZ
                                       LF_MSGTX.
  IF LF_MSGTX IS NOT INITIAL.
    CS_MESSG-MSGTY = 'E'.
    CS_MESSG-MSGTX = LF_MSGTX.
    RETURN.
  ENDIF.

* Get Info record
  SELECT EINA~INFNR EINA~MEINS EINA~UMREN EINA~UMREZ "#EC CI_SEL_NESTED
      UP TO 1 ROWS
    INTO CS_INFO
    FROM EINA
         INNER JOIN EINE
           ON  EINA~INFNR = EINE~INFNR
           AND EINE~EKORG = US_KEY-EKORG
           AND EINE~ESOKZ = US_KEY-ESOKZ
           AND EINE~WERKS = US_KEY-WERKS
   WHERE EINA~LIFNR  EQ  US_KEY-LIFNR
     AND EINA~MATNR  EQ  US_KEY-MATNR
   ORDER BY EINA~INFNR ASCENDING.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    CLEAR CS_INFO.
  ENDIF.

* Read Info from EINA
  IF CS_INFO IS INITIAL.
*   Get Info record
    SELECT EINA~INFNR EINA~MEINS EINA~UMREN EINA~UMREZ "#EC CI_SEL_NESTED
        UP TO 1 ROWS
      INTO CS_INFO
      FROM EINA
     WHERE EINA~LIFNR  EQ  US_KEY-LIFNR
       AND EINA~MATNR  EQ  US_KEY-MATNR
    ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      CLEAR CS_INFO.
    ENDIF.
  ENDIF.

* Determine Extra data (Main condition list)
  PERFORM F_GET_MAIN_COND_LIST  USING  US_KEY
*                                       CS_INFO
                              CHANGING CS_EXTRA.

  CS_EXTRA-PLIFZ = LF_PLIFZ.

ENDFORM.                    " VALIDATE_KEY

*----------------------------------------------------------------------*
*  Form f_validate_mat_plant
*----------------------------------------------------------------------*
*  Validate Material + Plant
*----------------------------------------------------------------------*
FORM F_VALIDATE_MAT_PLANT  USING  UF_MATNR  TYPE  MARC-MATNR
                                  UF_WERKS  TYPE  MARC-WERKS
                         CHANGING CF_PLIFZ  TYPE  MARC-PLIFZ
                                  CF_MSGTX  TYPE  CLIKE.

  TYPES: BEGIN OF LTS_MARC,
           MATNR TYPE  MARC-MATNR,
           WERKS TYPE  MARC-WERKS,
           PLIFZ TYPE  MARC-PLIFZ,
         END OF LTS_MARC.
  TYPES: LTT_MARC  TYPE  SORTED TABLE OF LTS_MARC
                          WITH UNIQUE KEY MATNR
  WERKS.
  STATICS:
    LS_MARC TYPE  LTS_MARC,
    LT_MARC TYPE  LTT_MARC.


* Initialize Output
  CLEAR: CF_PLIFZ,
         CF_MSGTX.

* Check buffer
  IF LS_MARC-MATNR NE UF_MATNR OR
     LS_MARC-WERKS NE UF_WERKS .
*   Read from Memory
    READ TABLE LT_MARC INTO LS_MARC
                       WITH KEY MATNR = UF_MATNR
                                WERKS = UF_WERKS
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_MARC.
*     Read from database
      SELECT SINGLE MATNR WERKS PLIFZ                "#EC CI_SEL_NESTED
        INTO LS_MARC
        FROM MARC
       WHERE MATNR EQ UF_MATNR
         AND WERKS EQ UF_WERKS.
      IF SY-SUBRC NE 0.
*       Text-e04 : Invalid Material number on plant:
        CONCATENATE TEXT-E04 UF_MATNR UF_WERKS
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_MARC INTO TABLE LT_MARC.
    ENDIF.
  ENDIF.

* Assign Output
  CF_PLIFZ = LS_MARC-PLIFZ.

ENDFORM.
*----------------------------------------------------------------------*
*       Form  f_get_main_cond_list
*----------------------------------------------------------------------*
*       Get Main condition list
*       - This list must be the same list that popup during process
*         t-code ME11 and ME12
*       - The logic reference from subroutine KONDITIONSART_SUCHEN in
*         SAPLMEKO
*----------------------------------------------------------------------*
FORM F_GET_MAIN_COND_LIST  USING  US_KEY   TYPE TS_KEY
                         CHANGING CS_EXTRA TYPE TS_EXTRA.

  STATICS:
    LF_LIFNR TYPE  TS_KEY-LIFNR,
    LF_EKORG TYPE  TS_KEY-EKORG,
    LF_ESOKZ TYPE  TS_KEY-ESOKZ,
    LS_EXTRA TYPE  TS_EXTRA.


  DATA:
  LF_KALSM   TYPE  TMKS-KALSM.


* Initialize Output
  CLEAR CS_EXTRA.

* Check buffer
  IF LF_LIFNR  NE US_KEY-LIFNR OR
     LF_EKORG  NE US_KEY-EKORG OR
     LF_ESOKZ  NE US_KEY-ESOKZ .

*   Determine Procedure
    CALL FUNCTION 'MEX_SCHEMA_SUCHEN'
      EXPORTING
        PI_LIFNR = US_KEY-LIFNR
        PI_EKORG = US_KEY-EKORG
      IMPORTING
        PO_KALSM = LF_KALSM.
    IF US_KEY-ESOKZ = '1'.
      CALL FUNCTION 'ME_CHANGE_CHARGEABLE_SCHEMA'
        EXPORTING
          PI_LIFNR = US_KEY-LIFNR
          PI_EKORG = US_KEY-EKORG
        CHANGING
          PC_KALSM = LF_KALSM.
    ENDIF.

*   Save to Buffer
    CLEAR LS_EXTRA.
    LS_EXTRA-KALSM = LF_KALSM.

  ENDIF.

* Assign output
  CS_EXTRA = LS_EXTRA.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_general
*----------------------------------------------------------------------*
*  Validate General Data
*----------------------------------------------------------------------*
FORM F_VALIDATE_GENERAL  USING  US_EINA_I  TYPE  TS_EINA
                                US_EINAX_I TYPE  TS_EINAX
                                US_INFO    TYPE  TS_INFO  ##NEEDED
                                UF_EXIST   TYPE  FLAG      ##NEEDED
                       CHANGING CS_EINA    TYPE  TS_EINA
                                CS_EINAX   TYPE  TS_EINAX
                                CS_MESSG   TYPE  TS_MESSG.

* Initialize Output
  CLEAR: CS_EINA,
         CS_EINAX,
         CS_MESSG.

* Copy data to output
  CS_EINA  = US_EINA_I.
  CS_EINAX = US_EINAX_I.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_purchorg
*----------------------------------------------------------------------*
*  text
*----------------------------------------------------------------------*
FORM F_VALIDATE_PURCHORG  USING  US_EINE_I   TYPE  TS_EINE
                                 US_EINEX_I  TYPE  TS_EINEX
                                 US_INFO     TYPE  TS_INFO
                                 CS_EXTRA    TYPE  TS_EXTRA
                                 UF_EXIST    TYPE  FLAG
                        CHANGING CS_EINE     TYPE  TS_EINE
                                 CS_EINEX    TYPE  TS_EINEX
                                 CS_MESSG    TYPE  TS_MESSG.

* Copy data to output
  CS_EINE  = US_EINE_I.
  CS_EINEX = US_EINEX_I.

* Convert amount into internal format based on currency key
  IF CS_EINE-NETPR IS INITIAL AND
     CS_EINE-NETPR_F IS NOT INITIAL.
    PERFORM F_CONVERT_INT_AMOUNT  USING  CS_EINE-NETPR_F
                                         CS_EINE-WAERS
                                CHANGING CS_EINE-NETPR.
  ENDIF.

* Check required fields for non-existing
  IF US_INFO IS INITIAL AND
     UF_EXIST IS INITIAL.

    IF CS_EXTRA-PLIFZ IS INITIAL AND
       ( CS_EINE-APLFZ IS INITIAL OR
         CS_EINEX-APLFZ IS INITIAL ).
      CS_MESSG-MSGTY = 'E'.
*     Text-e19 : Missing Plnd dely time in the file.
      CS_MESSG-MSGTX = TEXT-E19.
      RETURN.
    ENDIF.
    IF CS_EINE-EKGRP IS INITIAL OR
       CS_EINEX-EKGRP IS INITIAL.
      CS_MESSG-MSGTY = 'E'.
*     Text-e20 : Missing purchase group for in the file.
      CS_MESSG-MSGTX = TEXT-E20.
      RETURN.
    ENDIF.
    IF CS_EINE-NORBM IS INITIAL OR
       CS_EINEX-NORBM IS INITIAL.
      CS_MESSG-MSGTY = 'E'.
*     Text-e21 : Missing Standard qty. in the file.
      CS_MESSG-MSGTX = TEXT-E21.
    ENDIF.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_convert_int_amount
*----------------------------------------------------------------------*
*       Convert amount to internal format based on currency key
*----------------------------------------------------------------------*
FORM F_CONVERT_INT_AMOUNT  USING  UF_AMOUNT_I  TYPE  ANY
                                  UF_WAERS     TYPE  TCURC-WAERS
                         CHANGING CF_AMOUNT_O  TYPE  ANY.

  STATICS:
    LT_TCURX TYPE  SORTED TABLE OF TCURX
                      WITH UNIQUE KEY CURRKEY,
    LS_TCURX TYPE  TCURX.

  DATA:
    LF_DEC TYPE  I,
    LF_MUL TYPE  I,
    LF_DIV TYPE  I.


* Initialize Output
  CLEAR CF_AMOUNT_O.

* Get Currency Decimals
* Check Buffer
  IF UF_WAERS NE LS_TCURX-CURRKEY.
*   Read from Memory
    READ TABLE LT_TCURX INTO LS_TCURX
                        WITH KEY CURRKEY = UF_WAERS
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
*     Read from Database
      SELECT SINGLE *
        INTO LS_TCURX
        FROM TCURX
       WHERE CURRKEY EQ UF_WAERS.
      IF SY-SUBRC NE 0.
        IF UF_WAERS EQ '%'.
*         Default is 3
          CLEAR LS_TCURX.
          LS_TCURX-CURRKEY = UF_WAERS.
          LS_TCURX-CURRDEC = 3.
        ELSE.
*         Default is 2
          CLEAR LS_TCURX.
          LS_TCURX-CURRKEY = UF_WAERS.
          LS_TCURX-CURRDEC = 2.
        ENDIF.
      ENDIF.
      INSERT LS_TCURX INTO TABLE LT_TCURX.
    ENDIF.
  ENDIF.

* Get Decimal of output
  DESCRIBE FIELD CF_AMOUNT_O DECIMALS LF_DEC.

* Calculate Multiply factors
  LF_MUL = 1.
  DO LS_TCURX-CURRDEC TIMES.
    LF_MUL = LF_MUL * 10.
  ENDDO.

* Calculate Divide factors
  LF_DIV = 1.
  DO LF_DEC TIMES.
    LF_DIV = LF_DIV * 10.
  ENDDO.

* Assign Output amount
  CF_AMOUNT_O = UF_AMOUNT_I * LF_MUL / LF_DIV.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_cond_detail
*----------------------------------------------------------------------*
*  Validate Condition Detail
*----------------------------------------------------------------------*
FORM F_VALIDATE_COND_DETAIL  USING  US_CONDDET_I  TYPE  TS_CONDDET
                                    US_EXTRA      TYPE  TS_EXTRA
                           CHANGING CS_CONDDET    TYPE  TS_CONDDET
                                    CS_MESSG      TYPE  TS_MESSG.

  DATA:
    LF_TEMP1 TYPE  TEXT10,
    LF_TEMP2 TYPE  TEXT10,
    LF_KALSM TYPE  T685A-KALSM.


* Copy data to output
  CS_CONDDET = US_CONDDET_I.

* Convert amount into internal format based on currency key
  IF CS_CONDDET-KBETR IS INITIAL AND
     CS_CONDDET-KBETR_F IS NOT INITIAL.
    PERFORM F_CONVERT_INT_AMOUNT  USING  CS_CONDDET-KBETR_F
                                         CS_CONDDET-KONWA
                                CHANGING CS_CONDDET-KBETR.
  ENDIF.

* Get Procedure
  SELECT SINGLE KALSM
    INTO LF_KALSM
    FROM T685A
   WHERE KAPPL EQ GC_KAPPL
     AND KSCHL EQ CS_CONDDET-KSCHL.
  IF SY-SUBRC NE 0 OR
     LF_KALSM IS INITIAL.
    LF_KALSM = US_EXTRA-KALSM.
  ENDIF.

* Check from to
  IF CS_CONDDET-DATAB GT CS_CONDDET-DATBI.
    CS_MESSG-MSGTY = 'E'.
    WRITE CS_CONDDET-DATAB TO LF_TEMP1.
    WRITE CS_CONDDET-DATBI TO LF_TEMP2.
*   Error : "From" date (&1) must come before "to" date (&2)
    MESSAGE ID 'VK'
            TYPE 'E'
            NUMBER '098'
            WITH LF_TEMP1 LF_TEMP2
            INTO CS_MESSG-MSGTX.
    RETURN.
  ENDIF.

* Check condition type
  PERFORM F_VALIDATE_CONDITION_TYPE  USING  CS_CONDDET-KSCHL
                                            LF_KALSM
                                   CHANGING CS_CONDDET-KZBZG
                                            CS_MESSG.
  IF CS_MESSG IS NOT INITIAL.
    RETURN.
  ENDIF.

* Only condition filled
  IF NOT ( CS_CONDDET-KBETR IS INITIAL AND
           CS_CONDDET-KONWA IS INITIAL AND
           CS_CONDDET-KPEIN IS INITIAL AND
           CS_CONDDET-KMEIN IS INITIAL ).
*   Condition rate is required
    IF CS_CONDDET-KBETR IS INITIAL.
      CS_MESSG-MSGTY = 'E'.
*     Text-e22 : Missing condition rate in the file.
      CS_MESSG-MSGTX = TEXT-E22.
      RETURN.
    ENDIF.

*   Rate unit is required
    IF CS_CONDDET-KONWA IS INITIAL.
      CS_MESSG-MSGTY = 'E'.
*     Text-e23 : Missing Rate unit in the file.
      CS_MESSG-MSGTX = TEXT-E23.
      RETURN.
    ENDIF.

    IF CS_CONDDET-KONWA NE '%'.
*     Condition pricing unit is required
      IF CS_CONDDET-KPEIN IS INITIAL.
        CS_MESSG-MSGTY = 'E'.
*       Text-e24 : Missing Condition pricing unit in the file.
        CS_MESSG-MSGTX = TEXT-E24.
        RETURN.
      ENDIF.

*     Condition unit is required
      IF CS_CONDDET-KMEIN IS INITIAL.
        CS_MESSG-MSGTY = 'E'.
*       Text-e25 : Missing Condition unit in the file.
        CS_MESSG-MSGTX = TEXT-E25.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_condition_type
*----------------------------------------------------------------------*
*  Validate Condition Types
*----------------------------------------------------------------------*
FORM F_VALIDATE_CONDITION_TYPE  USING  UF_KSCHL  TYPE  TS_CONDDET-KSCHL
                                       UF_KALSM  TYPE  TS_EXTRA-KALSM
                              CHANGING CF_KZBZG  TYPE  T685A-KZBZG
                                       CS_MESSG  TYPE  TS_MESSG.

  TYPES: BEGIN OF LTS_T683S,
           KSCHL TYPE  T683S-KSCHL,
           KZBZG TYPE  T685A-KZBZG,
         END OF LTS_T683S.
  TYPES: LTT_T683S TYPE SORTED TABLE OF LTS_T683S
  WITH UNIQUE KEY KSCHL.
  STATICS:
    LF_KALSM TYPE  TS_EXTRA-KALSM,
    LT_T683S TYPE  LTT_T683S.

  DATA:
  LS_T683S  TYPE  LTS_T683S.


* Initialize Output
  CLEAR: CF_KZBZG,
         CS_MESSG.

* Check Buffer
  IF LF_KALSM NE UF_KALSM.

    SELECT T683S~KSCHL T685A~KZBZG                   "#EC CI_SEL_NESTED
      INTO TABLE LT_T683S
      FROM T683S
           INNER JOIN T685
             ON  T683S~KVEWE = T685~KVEWE
             AND T683S~KAPPL = T685~KAPPL
             AND T683S~KSCHL = T685~KSCHL
           INNER JOIN T685A                            "#EC CI_BUFFJOIN
             ON  T683S~KAPPL = T685A~KAPPL
             AND T683S~KSCHL = T685A~KSCHL
     WHERE T683S~KVEWE  EQ  GC_KVEWE
       AND T683S~KAPPL  EQ  GC_KAPPL
       AND T683S~KALSM  EQ  UF_KALSM.
    IF SY-SUBRC NE 0.
      REFRESH: LT_T683S.
    ENDIF.

    LF_KALSM = UF_KALSM.

  ENDIF.

* Check condition valid?
  READ TABLE LT_T683S INTO LS_T683S
                      WITH KEY KSCHL = UF_KSCHL
                      BINARY SEARCH.
  IF SY-SUBRC NE 0.
    CS_MESSG-MSGTY = 'E'.
*   Text-e26 : Invalid condition type :
    CONCATENATE TEXT-E26 UF_KSCHL
           INTO CS_MESSG-MSGTX
      SEPARATED BY SPACE.
    EXIT.
  ENDIF.

* Assign Output
  CF_KZBZG = LS_T683S-KZBZG.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_upload_file
*----------------------------------------------------------------------*
*  Upload File
*----------------------------------------------------------------------*
FORM F_UPLOAD_FILE  USING  UT_DATA   TYPE  TT_DATA
                           UF_TEST   TYPE  FLAG
                  CHANGING CT_RESULT TYPE  TT_RESULT
                           CS_SUM    TYPE  TS_SUM.

  DATA:
  LT_MESSG TYPE  TT_MESSG.

  DATA:
  LS_DATA  TYPE  TS_DATA.

  DATA:
  LF_ERROR  TYPE  FLAG.

  DATA:
    LF_SUCC TYPE STRING,
    LF_ERR  TYPE STRING.


* Initialize Output
  REFRESH: CT_RESULT.
  CLEAR:   CS_SUM.

* Show Progress
* Text-p03 : Uploading file data. . .
  MC_SHOW_PROGRESS 45 TEXT-P03.

* Processing each record
  LOOP AT UT_DATA INTO LS_DATA.

*   Only non validation error
    IF LS_DATA-MESSG[] IS INITIAL.
      PERFORM F_CREATE_INFOREC  USING  LS_DATA
                                       UF_TEST
                              CHANGING LT_MESSG
                                       LF_ERROR.
      LS_DATA-MESSG = LT_MESSG.
    ELSE.
      LF_ERROR = GC_TRUE.
    ENDIF.

    CS_SUM-TOTAL = CS_SUM-TOTAL + 1.

    IF LF_ERROR EQ GC_TRUE.
      CS_SUM-ERROR = CS_SUM-ERROR + 1.
    ELSE.
      CS_SUM-SUCCS = CS_SUM-SUCCS + 1.
    ENDIF.

*   Collect Result
    PERFORM F_COLLECT_RESULT  USING  LS_DATA
                            CHANGING CT_RESULT.

  ENDLOOP.
  LF_SUCC = CS_SUM-SUCCS.
  LF_ERR = CS_SUM-ERROR.
  CONCATENATE 'Processing completed with' LF_SUCC 'success and' LF_ERR 'error' INTO DATA(LF_TEXT).
* Show Final Message for Processing completed
  MESSAGE LF_TEXT TYPE 'S'.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_create_inforec
*----------------------------------------------------------------------*
*  Create Info Record
*----------------------------------------------------------------------*
FORM F_CREATE_INFOREC  USING  US_DATA  TYPE  TS_DATA
                              UF_TEST  TYPE  FLAG
                     CHANGING CT_TEST TYPE  TT_MESSG
                              CF_ERROR TYPE  FLAG.

  DATA:
    LT_BAPI_TEXT    TYPE  MEWIPIRTEXT_T,
    LT_BAPI_CONDVAL TYPE  STANDARD TABLE OF MEWIVALIDITY,
    LT_BAPI_COND    TYPE  STANDARD TABLE OF MEWICONDITION,
    LT_RETURN       TYPE  MEWI_T_RETURN,
    LT_CONDDET      TYPE  TT_CONDDET.

  DATA:
    LS_BAPI_EINA    TYPE  MEWIEINA,
    LS_BAPI_EINAX   TYPE  MEWIEINAX,
    LS_BAPI_EINE    TYPE  MEWIEINE,
    LS_BAPI_EINEX   TYPE  MEWIEINEX,
    LS_BAPI_CONDVAL TYPE  MEWIVALIDITY,
    LS_BAPI_COND    TYPE  MEWICONDITION,
    LS_MESSG        TYPE  TS_MESSG.

  FIELD-SYMBOLS:
    <L_CONDDET> TYPE  TS_CONDDET,
    <L_RETURN>  TYPE  BAPIRETURN.


* Initialize Output
  REFRESH: CT_TEST.
  CLEAR:   CF_ERROR.

* --------------
* Assign EINA
* --------------
  CLEAR LS_BAPI_EINA.
  LS_BAPI_EINA-MATERIAL    =  US_DATA-KEY-MATNR.
  LS_BAPI_EINA-VENDOR      =  US_DATA-KEY-LIFNR.

  LS_BAPI_EINA-MAT_GRP     =  US_DATA-EINA-MATKL.
  LS_BAPI_EINA-DELETE_IND  =  US_DATA-EINA-LOEKZ.
  LS_BAPI_EINA-SHORT_TEXT  =  US_DATA-EINA-TXZ01.
  LS_BAPI_EINA-SORTED_BY   =  US_DATA-EINA-SORTL.
  LS_BAPI_EINA-VEND_MAT    =  US_DATA-EINA-IDNLF.
  LS_BAPI_EINA-CERT_NO     =  US_DATA-EINA-URZNR.
  LS_BAPI_EINA-CERT_VALID  =  US_DATA-EINA-URZDT.
  LS_BAPI_EINA-CERT_CTRY   =  US_DATA-EINA-URZLA.
  LS_BAPI_EINA-CERT_TYPE   =  US_DATA-EINA-URZTP.
  LS_BAPI_EINA-BASE_UOM    =  US_DATA-EINA-LMEIN.
  LS_BAPI_EINA-REGION      =  US_DATA-EINA-REGIO.
  LS_BAPI_EINA-VEND_MATG   =  US_DATA-EINA-WGLIF.
  LS_BAPI_EINA-PO_UNIT     =  US_DATA-EINA-MEINS.
  LS_BAPI_EINA-CONV_NUM1   =  US_DATA-EINA-UMREZ.
  LS_BAPI_EINA-CONV_DEN1   =  US_DATA-EINA-UMREN.
  LS_BAPI_EINA-SALES_PERS  =  US_DATA-EINA-VERKF.
  LS_BAPI_EINA-BACK_AGREE  =  US_DATA-EINA-RUECK.
  LS_BAPI_EINA-REMINDER1   =  US_DATA-EINA-MAHN1.
  LS_BAPI_EINA-REMINDER2   =  US_DATA-EINA-MAHN2.
  LS_BAPI_EINA-REMINDER3   =  US_DATA-EINA-MAHN3.
  LS_BAPI_EINA-TELEPHONE   =  US_DATA-EINA-TELF1.
  LS_BAPI_EINA-PRE_VENDOR  =  US_DATA-EINA-KOLIF.
  LS_BAPI_EINA-POINT_UNIT  =  US_DATA-EINA-PUNEI.
  LS_BAPI_EINA-SORT_NO     =  US_DATA-EINA-LTSSF.
  LS_BAPI_EINA-VEND_PART   =  US_DATA-EINA-LTSNR.
  LS_BAPI_EINA-NORM_VEND   =  US_DATA-EINA-RELIF.
  LS_BAPI_EINA-POINTS      =  US_DATA-EINA-ANZPU.
  LS_BAPI_EINA-VAR_ORD_UN  =  US_DATA-EINA-VABME.
  LS_BAPI_EINA-CUST_NO     =  US_DATA-EINA-URZZT.
  LS_BAPI_EINA-SUPPL_FROM  =  US_DATA-EINA-LIFAB.
  LS_BAPI_EINA-SUPPL_TO    =  US_DATA-EINA-LIFBI.

* --------------
* Assign EINAX
* --------------
  CLEAR LS_BAPI_EINAX.
  LS_BAPI_EINAX-MATERIAL    =  GC_TRUE.
  LS_BAPI_EINAX-VENDOR      =  GC_TRUE.

  LS_BAPI_EINAX-MAT_GRP     =  US_DATA-EINAX-MATKL.
  LS_BAPI_EINAX-DELETE_IND  =  US_DATA-EINAX-LOEKZ.
  LS_BAPI_EINAX-SHORT_TEXT  =  US_DATA-EINAX-TXZ01.
  LS_BAPI_EINAX-SORTED_BY   =  US_DATA-EINAX-SORTL.
  LS_BAPI_EINAX-VEND_MAT    =  US_DATA-EINAX-IDNLF.
  LS_BAPI_EINAX-CERT_NO     =  US_DATA-EINAX-URZNR.
  LS_BAPI_EINAX-CERT_VALID  =  US_DATA-EINAX-URZDT.
  LS_BAPI_EINAX-CERT_CTRY   =  US_DATA-EINAX-URZLA.
  LS_BAPI_EINAX-CERT_TYPE   =  US_DATA-EINAX-URZTP.
  LS_BAPI_EINAX-BASE_UOM    =  US_DATA-EINAX-LMEIN.
  LS_BAPI_EINAX-REGION      =  US_DATA-EINAX-REGIO.
  LS_BAPI_EINAX-VEND_MATG   =  US_DATA-EINAX-WGLIF.
  LS_BAPI_EINAX-PO_UNIT     =  US_DATA-EINAX-MEINS.
  LS_BAPI_EINAX-CONV_NUM1   =  US_DATA-EINAX-UMREZ.
  LS_BAPI_EINAX-CONV_DEN1   =  US_DATA-EINAX-UMREN.
  LS_BAPI_EINAX-SALES_PERS  =  US_DATA-EINAX-VERKF.
  LS_BAPI_EINAX-BACK_AGREE  =  US_DATA-EINAX-RUECK.
  LS_BAPI_EINAX-REMINDER1   =  US_DATA-EINAX-MAHN1.
  LS_BAPI_EINAX-REMINDER2   =  US_DATA-EINAX-MAHN2.
  LS_BAPI_EINAX-REMINDER3   =  US_DATA-EINAX-MAHN3.
  LS_BAPI_EINAX-TELEPHONE   =  US_DATA-EINAX-TELF1.
  LS_BAPI_EINAX-PRE_VENDOR  =  US_DATA-EINAX-KOLIF.
  LS_BAPI_EINAX-POINT_UNIT  =  US_DATA-EINAX-PUNEI.
  LS_BAPI_EINAX-SORT_NO     =  US_DATA-EINAX-LTSSF.
  LS_BAPI_EINAX-VEND_PART   =  US_DATA-EINAX-LTSNR.
  LS_BAPI_EINAX-NORM_VEND   =  US_DATA-EINAX-RELIF.
  LS_BAPI_EINAX-POINTS      =  US_DATA-EINAX-ANZPU.
  LS_BAPI_EINAX-VAR_ORD_UN  =  US_DATA-EINAX-VABME.
  LS_BAPI_EINAX-CUST_NO     =  US_DATA-EINAX-URZZT.
  LS_BAPI_EINAX-SUPPL_FROM  =  US_DATA-EINAX-LIFAB.
  LS_BAPI_EINAX-SUPPL_TO    =  US_DATA-EINAX-LIFBI.

* --------------
* Assign EINE
* --------------
  CLEAR LS_BAPI_EINE.
  LS_BAPI_EINE-PURCH_ORG   =  US_DATA-KEY-EKORG.
  LS_BAPI_EINE-INFO_TYPE   =  US_DATA-KEY-ESOKZ.
  LS_BAPI_EINE-PLANT       =  US_DATA-KEY-WERKS.

  IF US_DATA-EINEX-APLFZ IS INITIAL.
    LS_BAPI_EINE-PLND_DELRY  =  US_DATA-EXTRA-PLIFZ.
  ELSE.
    LS_BAPI_EINE-PLND_DELRY  =  US_DATA-EINE-APLFZ.
  ENDIF.
  LS_BAPI_EINE-PUR_GROUP   =  US_DATA-EINE-EKGRP.
  LS_BAPI_EINE-NRM_PO_QTY  =  US_DATA-EINE-NORBM.
  LS_BAPI_EINE-MIN_PO_QTY  =  US_DATA-EINE-MINBM.
  LS_BAPI_EINE-MAX_PO_QTY  =  US_DATA-EINE-BSTMA.
  LS_BAPI_EINE-MINREMLIFE  =  US_DATA-EINE-MHDRZ.
  LS_BAPI_EINE-INCOTERMS1  =  US_DATA-EINE-INCO1.
*  LS_BAPI_EINE-INCOTERMS2  =  US_DATA-EINE-INCO2.
  LS_BAPI_EINE-INCOTERMS2L =  US_DATA-EINE-INCO2.
  LS_BAPI_EINE-UNDER_TOL   =  US_DATA-EINE-UNTTO.
  LS_BAPI_EINE-OVERDELTOL  =  US_DATA-EINE-UEBTO.
  LS_BAPI_EINE-GR_BASEDIV  =  US_DATA-EINE-WEBRE.
  LS_BAPI_EINE-ACKN_REQD   =  US_DATA-EINE-KZABS.
  LS_BAPI_EINE-CONF_CTRL   =  US_DATA-EINE-BSTAE.
  LS_BAPI_EINE-TAX_CODE    =  US_DATA-EINE-MWSKZ.
  LS_BAPI_EINE-PRICEDATE   =  US_DATA-EINE-MEPRF.
  LS_BAPI_EINE-VAL_TYPE    =  US_DATA-EINE-BWTAR.
  LS_BAPI_EINE-COND_GROUP  =  US_DATA-EINE-EKKOL.
*  PERFORM F_ASSIGN_BAPI_AMOUNT  USING  US_DATA-EINE-NETPR
*                                       US_DATA-EINE-WAERS
*                              CHANGING LS_BAPI_EINE-NET_PRICE.
  LS_BAPI_EINE-PRICE_UNIT  =  US_DATA-EINE-PEINH.
  LS_BAPI_EINE-UNLIMITED   =  US_DATA-EINE-UEBTK.
  LS_BAPI_EINE-NO_AUT_GR   =  US_DATA-EINE-XERSN.
  LS_BAPI_EINE-SHIPPING    =  US_DATA-EINE-EVERS.
  LS_BAPI_EINE-NO_MATTEXT  =  US_DATA-EINE-MTXNO.
  LS_BAPI_EINE-EXP_IMP_P   =  US_DATA-EINE-EXPRF.
  LS_BAPI_EINE-ROUND_PROF  =  US_DATA-EINE-RDPRF.
  LS_BAPI_EINE-UNIT_GROUP  =  US_DATA-EINE-MEGRU.
  LS_BAPI_EINE-DELETE_IND  =  US_DATA-EINE-LOEKZ.
  LS_BAPI_EINE-PERIOD_IND_EXPIRATION_DATE
                           =  US_DATA-EINE-IPRKZ.
  LS_BAPI_EINE-VERSION     =  US_DATA-EINE-VERID.
  LS_BAPI_EINE-CURRENCY    =  US_DATA-EINE-WAERS.
  LS_BAPI_EINE-ORDERPR_UN  =  US_DATA-EINE-BPRME.
  LS_BAPI_EINE-NO_DISCT    =  US_DATA-EINE-SKTOF.
  LS_BAPI_EINE-CONV_DEN1   =  US_DATA-EINE-BPUMN.
  LS_BAPI_EINE-CONV_NUM1   =  US_DATA-EINE-BPUMZ.

* --------------
* Assign EINEX
* --------------
  CLEAR LS_BAPI_EINEX.
  LS_BAPI_EINEX-PURCH_ORG   =  GC_TRUE.
  LS_BAPI_EINEX-INFO_TYPE   =  GC_TRUE.
  LS_BAPI_EINEX-PLANT       =  GC_TRUE.

  IF US_DATA-EINEX-APLFZ IS NOT INITIAL OR
     US_DATA-EXTRA-PLIFZ IS NOT INITIAL.
    LS_BAPI_EINEX-PLND_DELRY  =  GC_TRUE.
  ENDIF.
  LS_BAPI_EINEX-PUR_GROUP   =  US_DATA-EINEX-EKGRP.
  LS_BAPI_EINEX-NRM_PO_QTY  =  US_DATA-EINEX-NORBM.
  LS_BAPI_EINEX-MIN_PO_QTY  =  US_DATA-EINEX-MINBM.
  LS_BAPI_EINEX-MAX_PO_QTY  =  US_DATA-EINEX-BSTMA.
  LS_BAPI_EINEX-MINREMLIFE  =  US_DATA-EINEX-MHDRZ.
  LS_BAPI_EINEX-INCOTERMS1  =  US_DATA-EINEX-INCO1.
*  LS_BAPI_EINEX-INCOTERMS2  =  US_DATA-EINEX-INCO2.
  LS_BAPI_EINEX-INCOTERMS2L =  US_DATA-EINEX-INCO2.
  LS_BAPI_EINEX-UNDER_TOL   =  US_DATA-EINEX-UNTTO.
  LS_BAPI_EINEX-OVERDELTOL  =  US_DATA-EINEX-UEBTO.
  LS_BAPI_EINEX-GR_BASEDIV  =  US_DATA-EINEX-WEBRE.
  LS_BAPI_EINEX-ACKN_REQD   =  US_DATA-EINEX-KZABS.
  LS_BAPI_EINEX-CONF_CTRL   =  US_DATA-EINEX-BSTAE.
  LS_BAPI_EINEX-TAX_CODE    =  US_DATA-EINEX-MWSKZ.
  LS_BAPI_EINEX-PRICEDATE   =  US_DATA-EINEX-MEPRF.
  LS_BAPI_EINEX-VAL_TYPE    =  US_DATA-EINEX-BWTAR.
  LS_BAPI_EINEX-COND_GROUP  =  US_DATA-EINEX-EKKOL.
*  LS_BAPI_EINEX-NET_PRICE   =  US_DATA-EINEX-NETPR.
  LS_BAPI_EINEX-PRICE_UNIT  =  US_DATA-EINEX-PEINH.
  LS_BAPI_EINEX-UNLIMITED   =  US_DATA-EINEX-UEBTK.
  LS_BAPI_EINEX-NO_AUT_GR   =  US_DATA-EINEX-XERSN.
  LS_BAPI_EINEX-SHIPPING    =  US_DATA-EINEX-EVERS.
  LS_BAPI_EINEX-NO_MATTEXT  =  US_DATA-EINEX-MTXNO.
  LS_BAPI_EINEX-EXP_IMP_P   =  US_DATA-EINEX-EXPRF.
  LS_BAPI_EINEX-ROUND_PROF  =  US_DATA-EINEX-RDPRF.
  LS_BAPI_EINEX-UNIT_GROUP  =  US_DATA-EINEX-MEGRU.
  LS_BAPI_EINEX-DELETE_IND  =  US_DATA-EINEX-LOEKZ.
  LS_BAPI_EINEX-PERIOD_IND_EXPIRATION_DATE
                           =  US_DATA-EINEX-IPRKZ.
  LS_BAPI_EINEX-VERSION     =  US_DATA-EINEX-VERID.
  LS_BAPI_EINEX-CURRENCY    =  US_DATA-EINEX-WAERS.
  LS_BAPI_EINEX-ORDERPR_UN  =  US_DATA-EINEX-BPRME.
  LS_BAPI_EINEX-NO_DISCT    =  US_DATA-EINEX-SKTOF.
  LS_BAPI_EINEX-CONV_DEN1   =  US_DATA-EINEX-BPUMN.
  LS_BAPI_EINEX-CONV_NUM1   =  US_DATA-EINEX-BPUMZ.

* --------------
* Assign TEXT
* --------------
  LT_BAPI_TEXT = US_DATA-LTEXT.

* --------------
* Assign Conditions
* --------------
  IF US_DATA-CONDDET IS NOT INITIAL.
    CLEAR LS_BAPI_CONDVAL.
    LS_BAPI_CONDVAL-PLANT      =  US_DATA-KEY-WERKS.
    LS_BAPI_CONDVAL-VALID_FROM =  US_DATA-CONDDET-DATAB.
    LS_BAPI_CONDVAL-VALID_TO   =  US_DATA-CONDDET-DATBI.
    INSERT LS_BAPI_CONDVAL INTO TABLE LT_BAPI_CONDVAL.

*   ***************************************
*   Add existing Condition on same key
*   - This is needed due to the function will mark delete flag
*     for all existing condition with same key
*   ***************************************
    PERFORM F_GET_EXISTING_COND  USING  US_DATA-KEY
                                        US_DATA-CONDDET-DATAB
                                        US_DATA-CONDDET-DATBI
                               CHANGING LT_CONDDET.
    LOOP AT LT_CONDDET ASSIGNING <L_CONDDET>
                       WHERE KSCHL NE US_DATA-CONDDET-KSCHL.
      CLEAR LS_BAPI_COND.
      LS_BAPI_COND-COND_COUNT =  <L_CONDDET>-KOPOS.
      LS_BAPI_COND-COND_TYPE  =  <L_CONDDET>-KSCHL.
      PERFORM F_ASSIGN_BAPI_AMOUNT  USING  <L_CONDDET>-KBETR
                                           <L_CONDDET>-KONWA
                                  CHANGING LS_BAPI_COND-COND_VALUE.
      LS_BAPI_COND-CURRENCY   =  <L_CONDDET>-KONWA.
      LS_BAPI_COND-COND_P_UNT =  <L_CONDDET>-KPEIN.
      LS_BAPI_COND-COND_UNIT  =  <L_CONDDET>-KMEIN.
      LS_BAPI_COND-CHANGE_ID  = 'U'.
      INSERT LS_BAPI_COND INTO TABLE LT_BAPI_COND.
    ENDLOOP.

*   Add new Condition Data from File
    CLEAR LS_BAPI_COND.
    LS_BAPI_COND-COND_TYPE  =  US_DATA-CONDDET-KSCHL.
    PERFORM F_ASSIGN_BAPI_AMOUNT  USING  US_DATA-CONDDET-KBETR
                                         US_DATA-CONDDET-KONWA
                                CHANGING LS_BAPI_COND-COND_VALUE.
    LS_BAPI_COND-CURRENCY   =  US_DATA-CONDDET-KONWA.
    LS_BAPI_COND-COND_P_UNT =  US_DATA-CONDDET-KPEIN.
    LS_BAPI_COND-COND_UNIT  =  US_DATA-CONDDET-KMEIN.
    READ TABLE LT_CONDDET ASSIGNING <L_CONDDET>
                          WITH KEY KSCHL = US_DATA-CONDDET-KSCHL
                          BINARY SEARCH.
    IF SY-SUBRC NE 0.
      LS_BAPI_COND-CHANGE_ID  = 'I'.
    ELSE.
      LS_BAPI_COND-COND_COUNT =  <L_CONDDET>-KOPOS.
      LS_BAPI_COND-CHANGE_ID  = 'U'.
    ENDIF.
    INSERT LS_BAPI_COND INTO TABLE LT_BAPI_COND.
  ENDIF.

* Call function to create info record
  CALL FUNCTION 'ME_INFORECORD_MAINTAIN'
    EXPORTING
      I_EINA        = LS_BAPI_EINA
      I_EINAX       = LS_BAPI_EINAX
      I_EINE        = LS_BAPI_EINE
      I_EINEX       = LS_BAPI_EINEX
      TESTRUN       = UF_TEST
    TABLES
      TXT_LINES     = LT_BAPI_TEXT
      COND_VALIDITY = LT_BAPI_CONDVAL
      CONDITION     = LT_BAPI_COND
      RETURN        = LT_RETURN.

  LOOP AT LT_RETURN ASSIGNING <L_RETURN>
                    WHERE TYPE = 'E' OR
                          TYPE = 'A'.
    CLEAR LS_MESSG.
    LS_MESSG-MSGTY = <L_RETURN>-TYPE.
    LS_MESSG-MSGID = <L_RETURN>-CODE(2).
    LS_MESSG-MSGNO = <L_RETURN>-CODE+2(3).
    LS_MESSG-MSGTX = <L_RETURN>-MESSAGE.
    INSERT LS_MESSG INTO TABLE CT_TEST.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC NE 0.

    IF UF_TEST EQ GC_TRUE.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'S'.
*     Text-i01: Test upload successfully.
      LS_MESSG-MSGTX = TEXT-I01.
      INSERT LS_MESSG INTO TABLE CT_TEST.
*     Rollback Work
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'S'.
*     Text-i02: Info Record posted successfully.
      LS_MESSG-MSGTX = TEXT-I02.
      INSERT LS_MESSG INTO TABLE CT_TEST.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = GC_TRUE.
    ENDIF.
  ELSE.
    CF_ERROR = GC_TRUE.
*   Rollback Work
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_collect_result
*----------------------------------------------------------------------*
*  Collect Result
*----------------------------------------------------------------------*
FORM F_COLLECT_RESULT  USING  US_DATA   TYPE  TS_DATA
                     CHANGING CT_RESULT TYPE  TT_RESULT.

  DATA:
  LS_RESULT  TYPE  ZSDSMMS027.

  DATA:
    LF_OBJEC TYPE  TS_TEXT-TDOBJECT,
    LF_SEQNO TYPE  I.

  FIELD-SYMBOLS:
    <L_MESSG> TYPE  TS_MESSG,
    <L_LTEXT> TYPE  TS_TEXT.


  CLEAR: LS_RESULT.

  LS_RESULT-ROWNO = US_DATA-ROWNO.
  LS_RESULT-LIFNR = US_DATA-KEY-LIFNR.
  LS_RESULT-MATNR = US_DATA-KEY-MATNR.
  LS_RESULT-EKORG = US_DATA-KEY-EKORG.
  LS_RESULT-WERKS = US_DATA-KEY-WERKS.
  LS_RESULT-ESOKZ = US_DATA-KEY-ESOKZ.
  LS_RESULT-MATKL = US_DATA-EINA-MATKL.
  LS_RESULT-IDNLF = US_DATA-EINA-IDNLF.
  LS_RESULT-VERKF = US_DATA-EINA-VERKF.
  LS_RESULT-TELF1 = US_DATA-EINA-TELF1.
  LS_RESULT-MEINS = US_DATA-EINA-MEINS.
  LS_RESULT-UMREN = US_DATA-EINA-UMREN.
  LS_RESULT-MEIN1 = US_DATA-EINA-MEINS.
  LS_RESULT-UMREZ = US_DATA-EINA-UMREZ.
  LS_RESULT-LMEIN = US_DATA-EINA-LMEIN.
  LS_RESULT-VABME = US_DATA-EINA-VABME.
  LS_RESULT-APLFZ = US_DATA-EINE-APLFZ.
  LS_RESULT-EKGRP = US_DATA-EINE-EKGRP.
  LS_RESULT-NORBM = US_DATA-EINE-NORBM.
  LS_RESULT-MINBM = US_DATA-EINE-MINBM.
  LS_RESULT-MHDRZ = US_DATA-EINE-MHDRZ.
  LS_RESULT-IPRKZ = US_DATA-EINE-IPRKZ.
  LS_RESULT-BSTMA = US_DATA-EINE-BSTMA.
  LS_RESULT-UNTTO = US_DATA-EINE-UNTTO.
  LS_RESULT-UEBTO = US_DATA-EINE-UEBTO.
  LS_RESULT-UEBTK = US_DATA-EINE-UEBTK.
  LS_RESULT-WEBRE = US_DATA-EINE-WEBRE.
  LS_RESULT-MWSKZ = US_DATA-EINE-MWSKZ.
  LS_RESULT-INCO1 = US_DATA-EINE-INCO1.
  LS_RESULT-INCO2 = US_DATA-EINE-INCO2.
  LS_RESULT-DATAB = US_DATA-CONDDET-DATAB.
  LS_RESULT-DATBI = US_DATA-CONDDET-DATBI.
  LS_RESULT-KSCHL = US_DATA-CONDDET-KSCHL.
  LS_RESULT-KBETR = US_DATA-CONDDET-KBETR.
  LS_RESULT-KONWA = US_DATA-CONDDET-KONWA.
  LS_RESULT-KPEIN = US_DATA-CONDDET-KPEIN.
  LS_RESULT-KMEIN = US_DATA-CONDDET-KMEIN.

  LOOP AT US_DATA-LTEXT ASSIGNING <L_LTEXT>.

*   AT New Object
    IF  <L_LTEXT>-TDOBJECT NE LF_OBJEC.
      LF_OBJEC = <L_LTEXT>-TDOBJECT.
      CLEAR LF_SEQNO.
    ENDIF.

    CASE LF_OBJEC.
      WHEN GC_OBJEC_INTXT.
        LS_RESULT-INTXT_SPRAS = <L_LTEXT>-TDSPRAS.
        LF_SEQNO = LF_SEQNO + 1.

        CASE LF_SEQNO.
          WHEN 1.
            LS_RESULT-INTXT_LINE1 = <L_LTEXT>-TDLINE.
          WHEN 2.
            LS_RESULT-INTXT_LINE2 = <L_LTEXT>-TDLINE.
          WHEN 3.
            LS_RESULT-INTXT_LINE3 = <L_LTEXT>-TDLINE.
          WHEN 4.
            LS_RESULT-INTXT_LINE4 = <L_LTEXT>-TDLINE.
          WHEN 5.
            LS_RESULT-INTXT_LINE5 = <L_LTEXT>-TDLINE.
        ENDCASE.

      WHEN GC_OBJEC_POTXT.
        LS_RESULT-POTXT_SPRAS = <L_LTEXT>-TDSPRAS.
        LF_SEQNO = LF_SEQNO + 1.
        CASE LF_SEQNO.
          WHEN 1.
            LS_RESULT-POTXT_LINE1 = <L_LTEXT>-TDLINE.
          WHEN 2.
            LS_RESULT-POTXT_LINE2 = <L_LTEXT>-TDLINE.
          WHEN 3.
            LS_RESULT-POTXT_LINE3 = <L_LTEXT>-TDLINE.
          WHEN 4.
            LS_RESULT-POTXT_LINE4 = <L_LTEXT>-TDLINE.
          WHEN 5.
            LS_RESULT-POTXT_LINE5 = <L_LTEXT>-TDLINE.
        ENDCASE.
    ENDCASE.
  ENDLOOP.

  LOOP AT US_DATA-MESSG ASSIGNING <L_MESSG>.
    LS_RESULT-MSGTY = <L_MESSG>-MSGTY.
    LS_RESULT-MSGTX = <L_MESSG>-MSGTX.
  ENDLOOP.

  INSERT LS_RESULT INTO TABLE CT_RESULT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_create_logfile
*----------------------------------------------------------------------*
*  Generate Log File
*----------------------------------------------------------------------*
FORM F_CREATE_LOGFILE  USING  US_SUM TYPE TS_SUM
                              CS_LOGFILE  TYPE  TS_FILE_INFO
                              UT_RESULT TYPE  TT_RESULT.

  DATA:
        LT_LOG   TYPE  TT_RAW.

  DATA:
        LF_TEXT TYPE STRING.

  DATA:
    LF_SUCC TYPE STRING,
    LF_ERR  TYPE STRING.


* Convert Result into Text table
  PERFORM F_CONVERT_RESULT_TO_TEXT  USING  UT_RESULT
                                    CHANGING LT_LOG.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME                = CS_LOGFILE-FULLNAME
      FILETYPE                = 'ASC'
      WRITE_FIELD_SEPARATOR   = 'X'
      TRUNC_TRAILING_BLANKS   = 'X'
    TABLES
      DATA_TAB                = LT_LOG
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      OTHERS                  = 22.
  IF SY-SUBRC EQ 0.
    LF_SUCC = US_SUM-SUCCS.
    LF_ERR = US_SUM-ERROR.
    CONCATENATE 'Log file' CS_LOGFILE-FILENAME 'has been created with' LF_SUCC 'success and' LF_ERR 'error' INTO LF_TEXT SEPARATED BY SPACE.
    MESSAGE LF_TEXT TYPE 'S'.
    RETURN.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_convert_result_to_text
*----------------------------------------------------------------------*
*  Convert Result into Text table
*----------------------------------------------------------------------*
FORM F_CONVERT_RESULT_TO_TEXT  USING  UT_RESULT  TYPE TT_RESULT
                             CHANGING CT_RAW  TYPE  TT_RAW.

  DATA:
    LS_LOG TYPE  TS_LOG,
    LS_RAW TYPE  TS_RAW.

  DATA:
  LF_INDEX      TYPE  I.

  FIELD-SYMBOLS:
    <L_RESULT> TYPE  ZSDSMMS027,
    <L_FIELD>  TYPE  CLIKE.


* Initialize Output
  REFRESH: CT_RAW.

* Insert Header Section of Log
  PERFORM F_INSERT_LOG_HEADER CHANGING CT_RAW.

  LOOP AT UT_RESULT ASSIGNING <L_RESULT>.



    PERFORM F_ASSIGN_LOG  USING  <L_RESULT>
                        CHANGING LS_LOG.

*   Change date format from YYYYMMDD to DD.MM.YYYY
    CONCATENATE LS_LOG-DATAB+6(2) LS_LOG-DATAB+4(2) LS_LOG-DATAB+0(4) INTO LS_LOG-DATAB SEPARATED BY '.'.
    CONCATENATE LS_LOG-DATBI+6(2) LS_LOG-DATBI+4(2) LS_LOG-DATBI+0(4) INTO LS_LOG-DATBI SEPARATED BY '.'.

    DO.
      LF_INDEX = SY-INDEX.
      ASSIGN COMPONENT LF_INDEX OF STRUCTURE LS_LOG TO <L_FIELD>.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
      CONDENSE <L_FIELD>.
      IF LF_INDEX EQ 1.
        LS_RAW-TLINE = <L_FIELD>.
      ELSE.
        CONCATENATE LS_RAW-TLINE
                    <L_FIELD>
               INTO LS_RAW-TLINE
          SEPARATED BY GC_SPLIT.
      ENDIF.
    ENDDO.

    INSERT LS_RAW INTO TABLE CT_RAW.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_assign_log
*----------------------------------------------------------------------*
*  Assign Data to Log Structure
*----------------------------------------------------------------------*
FORM F_ASSIGN_LOG  USING  US_RESULT  TYPE  ZSDSMMS027
                 CHANGING CS_LOG     TYPE  TS_LOG.

* Initialize Output
  CLEAR: CS_LOG.

  WRITE: US_RESULT-MSGTY TO CS_LOG-MSGTY,
         US_RESULT-MSGTX TO CS_LOG-MSGTX,
         US_RESULT-LIFNR TO CS_LOG-LIFNR,
         US_RESULT-MATNR TO CS_LOG-MATNR,
         US_RESULT-EKORG TO CS_LOG-EKORG,
         US_RESULT-WERKS TO CS_LOG-WERKS,
         US_RESULT-ESOKZ TO CS_LOG-ESOKZ,
         US_RESULT-MATKL TO CS_LOG-MATKL,
         US_RESULT-IDNLF TO CS_LOG-IDNLF,
         US_RESULT-VERKF TO CS_LOG-VERKF,
         US_RESULT-TELF1 TO CS_LOG-TELF1,
         US_RESULT-MEINS TO CS_LOG-MEINS,
         US_RESULT-UMREN TO CS_LOG-UMREN NO-ZERO,
         US_RESULT-MEIN1 TO CS_LOG-MEIN1,
         US_RESULT-UMREZ TO CS_LOG-UMREZ NO-ZERO,
         US_RESULT-LMEIN TO CS_LOG-LMEIN,
         US_RESULT-VABME TO CS_LOG-VABME,
         US_RESULT-APLFZ TO CS_LOG-APLFZ NO-ZERO,
         US_RESULT-EKGRP TO CS_LOG-EKGRP,
         US_RESULT-NORBM TO CS_LOG-NORBM UNIT US_RESULT-MEINS LEFT-JUSTIFIED,
         US_RESULT-MINBM TO CS_LOG-MINBM UNIT US_RESULT-MEINS LEFT-JUSTIFIED,
         US_RESULT-MHDRZ TO CS_LOG-MHDRZ NO-ZERO,
         US_RESULT-IPRKZ TO CS_LOG-IPRKZ,
         US_RESULT-BSTMA TO CS_LOG-BSTMA UNIT US_RESULT-MEINS LEFT-JUSTIFIED,
         US_RESULT-UNTTO TO CS_LOG-UNTTO NO-ZERO,
         US_RESULT-UEBTO TO CS_LOG-UEBTO NO-ZERO,
         US_RESULT-UEBTK TO CS_LOG-UEBTK,
         US_RESULT-WEBRE TO CS_LOG-WEBRE,
         US_RESULT-MWSKZ TO CS_LOG-MWSKZ,
         US_RESULT-INCO1 TO CS_LOG-INCO1,
         US_RESULT-INCO2 TO CS_LOG-INCO2,
         US_RESULT-DATAB TO CS_LOG-DATAB,
         US_RESULT-DATBI TO CS_LOG-DATBI,
         US_RESULT-KSCHL TO CS_LOG-KSCHL,
         US_RESULT-KBETR TO CS_LOG-KBETR CURRENCY US_RESULT-KONWA LEFT-JUSTIFIED,
         US_RESULT-KPEIN TO CS_LOG-KPEIN NO-ZERO,
         US_RESULT-KMEIN TO CS_LOG-KMEIN,
         US_RESULT-INTXT_SPRAS TO CS_LOG-INTXT_SPRAS,
         US_RESULT-INTXT_LINE1 TO CS_LOG-INTXT_LINE1,
         US_RESULT-INTXT_LINE2 TO CS_LOG-INTXT_LINE2,
         US_RESULT-INTXT_LINE3 TO CS_LOG-INTXT_LINE3,
         US_RESULT-INTXT_LINE4 TO CS_LOG-INTXT_LINE4,
         US_RESULT-INTXT_LINE5 TO CS_LOG-INTXT_LINE5,
         US_RESULT-POTXT_SPRAS TO CS_LOG-POTXT_SPRAS,
         US_RESULT-POTXT_LINE1 TO CS_LOG-POTXT_LINE1,
         US_RESULT-POTXT_LINE2 TO CS_LOG-POTXT_LINE2,
         US_RESULT-POTXT_LINE3 TO CS_LOG-POTXT_LINE3,
         US_RESULT-POTXT_LINE4 TO CS_LOG-POTXT_LINE4,
         US_RESULT-POTXT_LINE5 TO CS_LOG-POTXT_LINE5.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_insert_log_header
*----------------------------------------------------------------------*
*  Insert Header Section of Log file
*----------------------------------------------------------------------*
FORM F_INSERT_LOG_HEADER  CHANGING CT_RAW TYPE TT_RAW.

  DATA:
    LT_FIELDCAT TYPE  LVC_T_FCAT,
    LT_FIELDS   TYPE  ABAP_COMPDESCR_TAB.

  DATA:
    LREF_DESCR TYPE  REF TO CL_ABAP_TYPEDESCR,
    LREF_STRUC TYPE  REF TO CL_ABAP_STRUCTDESCR.

  DATA:
    LS_RAW  TYPE  TS_RAW.

  DATA:
    LF_TEXT  TYPE  TEXT50,
    LF_TEMP1 TYPE  TEXT50,
    LF_TEMP2 TYPE  TEXT50,
    LF_INDEX TYPE  I.

  FIELD-SYMBOLS:
    <L_FIELDCAT> TYPE  LVC_S_FCAT,
    <L_FIELD>    TYPE  ABAP_COMPDESCR.


* *********************
* File Header
* *********************
  CONCATENATE SY-REPID SY-TITLE
         INTO LF_TEXT
    SEPARATED BY '-'.
* Text-h01 : Program:
  CONCATENATE TEXT-H01
              LF_TEXT
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE CT_RAW.

  CONCATENATE SY-DATLO+6(2) SY-DATLO+4(2) SY-DATLO+0(4) INTO LF_TEMP1 SEPARATED BY '.'.
  WRITE SY-TIMLO TO LF_TEMP2.
  CONCATENATE LF_TEMP1 LF_TEMP2
         INTO LF_TEXT
    SEPARATED BY SPACE.
* Text-h02 : Processing Date/Time:
  CONCATENATE TEXT-H02
              LF_TEXT
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE CT_RAW.

  CONCATENATE SY-SYSID SY-MANDT
         INTO LF_TEXT
    SEPARATED BY '/'.
* Text-h03 : System/Client:
  CONCATENATE TEXT-H03
              LF_TEXT
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE CT_RAW.

* Text-h04 : Process By:
  CONCATENATE TEXT-H04
              SY-UNAME
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE CT_RAW.

  WRITE GS_SUM-TOTAL TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
* Text-h05 : Total Records:
  CONCATENATE TEXT-H05
              LF_TEXT
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE CT_RAW.

  WRITE GS_SUM-SUCCS TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
* Text-h06 : Success Records:
  CONCATENATE TEXT-H06
              LF_TEXT
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE CT_RAW.

  WRITE GS_SUM-ERROR TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
* Text-h07 : Failed Records:
  CONCATENATE TEXT-H07
              LF_TEXT
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE CT_RAW.

* *********************
* Generate Column header
* *********************
* Build Field cat
*  PERFORM f_aLF_build_fieldcat CHANGING lt_fieldcat.
* Build Field cat from Structure.

  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING LT_FIELDCAT.
  SORT LT_FIELDCAT BY FIELDNAME ASCENDING.

* Get List of LOG File fields
  LREF_DESCR = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( 'TS_LOG' ).
  LREF_STRUC ?= LREF_DESCR.
  LT_FIELDS = LREF_STRUC->COMPONENTS.

  CLEAR LS_RAW.
  CLEAR LF_INDEX.
  LOOP AT LT_FIELDS ASSIGNING <L_FIELD>.

    LF_INDEX = SY-TABIX.

    CLEAR LF_TEXT.
    READ TABLE LT_FIELDCAT ASSIGNING <L_FIELDCAT>
                           WITH KEY FIELDNAME = <L_FIELD>-NAME
                           BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      CASE <L_FIELD>-NAME.
        WHEN 'INTXT_LINE1'.
          LF_TEXT = 'Info Record Text1'.
        WHEN 'INTXT_LINE2'.
          LF_TEXT = 'Info Record Text2'.
        WHEN 'INTXT_LINE3'.
          LF_TEXT = 'Info Record Text3'.
        WHEN 'INTXT_LINE4'.
          LF_TEXT = 'Info Record Text4'.
        WHEN 'INTXT_LINE5'.
          LF_TEXT = 'Info Record Text5'.
        WHEN 'POTXT_LINE1'.
          LF_TEXT = 'Purchase Order Text1'.
        WHEN 'POTXT_LINE2'.
          LF_TEXT = 'Purchase Order Text2'.
        WHEN 'POTXT_LINE3'.
          LF_TEXT = 'Purchase Order Text3'.
        WHEN 'POTXT_LINE4'.
          LF_TEXT = 'Purchase Order Text4'.
        WHEN 'POTXT_LINE5'.
          LF_TEXT = 'Purchase Order Text5'.
        WHEN OTHERS.
          LF_TEXT = <L_FIELDCAT>-SCRTEXT_L.
*          IF <L_FIELDCAT>-REPTEXT IS NOT INITIAL.
*            LF_TEXT = <L_FIELDCAT>-REPTEXT.
*          ELSE.
*            LF_TEXT = <L_FIELDCAT>-SCRTEXT_M.
*          ENDIF.

      ENDCASE.

    ENDIF.

    IF LF_INDEX EQ 1.
      LS_RAW-TLINE = LF_TEXT.
    ELSE.
      CONCATENATE LS_RAW-TLINE
                  LF_TEXT
             INTO LS_RAW-TLINE
        SEPARATED BY GC_SPLIT.
    ENDIF.

  ENDLOOP.
  INSERT LS_RAW INTO TABLE CT_RAW.

ENDFORM.
*----------------------------------------------------------------------*
*       Form  f_prepare_fieldcat_o
*----------------------------------------------------------------------*
*       Prepare fieldcat method ALV used for online
*----------------------------------------------------------------------*
*  -->  PFD_STRUCTURE  Structure name of the list table
*  <--  PIT_FIELDCAT   Field catalog
*----------------------------------------------------------------------*
FORM F_PREPARE_FIELDCAT_O  USING  UF_STRUCTURE TYPE DD02L-TABNAME
                         CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT ##CALLED.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = UF_STRUCTURE
    CHANGING
      CT_FIELDCAT            = CT_FIELDCAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_get_existing_cond
*----------------------------------------------------------------------*
*  Get Existing Condition
*----------------------------------------------------------------------*
FORM F_GET_EXISTING_COND  USING  US_KEY     TYPE  TS_KEY
                                 UF_DATAB   TYPE  TS_CONDDET-DATAB
                                 UF_DATBI   TYPE  TS_CONDDET-DATBI
                        CHANGING CT_CONDDET TYPE  TT_CONDDET.

  TYPES: BEGIN OF LTS_KONP,
           KOPOS TYPE  KONP-KOPOS,
           KSCHL TYPE  KONP-KSCHL,
           KBETR TYPE  KONP-KBETR,
           KONWA TYPE  KONP-KONWA,
           KPEIN TYPE  KONP-KPEIN,
           KMEIN TYPE  KONP-KMEIN,
           KZBZG TYPE  KONP-KZBZG,
         END OF LTS_KONP.
  TYPES: LTT_KONP  TYPE  STANDARD TABLE OF LTS_KONP.

  DATA:
  LT_KONP  TYPE  LTT_KONP.

  DATA:
  LS_CONDDET  TYPE  TS_CONDDET.

  DATA:
  LF_KNUMH  TYPE  KONP-KNUMH.

  FIELD-SYMBOLS:
  <L_KONP>  TYPE  LTS_KONP.


* Initialize Output
  REFRESH: CT_CONDDET.

* Read Record number
  SELECT KNUMH
      UP TO 1 ROWS
    INTO LF_KNUMH
    FROM A017
   WHERE KAPPL  EQ  GC_KAPPL
     AND LIFNR  EQ  US_KEY-LIFNR
     AND MATNR  EQ  US_KEY-MATNR
     AND EKORG  EQ  US_KEY-EKORG
     AND WERKS  EQ  US_KEY-WERKS
     AND ESOKZ  EQ  US_KEY-ESOKZ
     AND DATBI  EQ  UF_DATBI
     AND DATAB  EQ  UF_DATAB
    ORDER BY PRIMARY KEY.
  ENDSELECT.
  IF SY-SUBRC NE 0.
*   Not Found
    RETURN.
  ENDIF.

* Select all conditions
  SELECT KOPOS KSCHL KBETR KONWA KPEIN KMEIN KZBZG   "#EC CI_SEL_NESTED
    INTO TABLE LT_KONP
    FROM KONP
   WHERE KNUMH     EQ  LF_KNUMH
     AND LOEVM_KO  EQ  SPACE.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Output
  LOOP AT LT_KONP ASSIGNING <L_KONP>.
    CLEAR: LS_CONDDET.
    LS_CONDDET-KOPOS = <L_KONP>-KOPOS.
    LS_CONDDET-KSCHL = <L_KONP>-KSCHL.
    LS_CONDDET-DATAB = UF_DATAB.
    LS_CONDDET-DATBI = UF_DATBI.
    LS_CONDDET-KBETR = <L_KONP>-KBETR.
    LS_CONDDET-KONWA = <L_KONP>-KONWA.
    LS_CONDDET-KPEIN = <L_KONP>-KPEIN.
    LS_CONDDET-KMEIN = <L_KONP>-KMEIN.
    LS_CONDDET-KZBZG = <L_KONP>-KZBZG.
    INSERT LS_CONDDET INTO TABLE CT_CONDDET.
  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_assign_bapi_amount
*----------------------------------------------------------------------*
*  Convert to BAPI Amount format
*----------------------------------------------------------------------*
FORM F_ASSIGN_BAPI_AMOUNT  USING  UF_KBETR  TYPE  KONP-KBETR
                                  UF_KONWA  TYPE  KONP-KONWA
                         CHANGING CF_AMT    TYPE  ANY.

  DATA:
  LF_TEMP  TYPE  BAPICURR-BAPICURR.


* Initialize Output
  CLEAR: CF_AMT.

  IF UF_KONWA EQ '%'.
    CF_AMT = UF_KBETR / 10.
  ELSE.
    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
      EXPORTING
        CURRENCY        = UF_KONWA
        AMOUNT_INTERNAL = UF_KBETR
      IMPORTING
        AMOUNT_EXTERNAL = LF_TEMP.
    CF_AMT = LF_TEMP.
  ENDIF.

ENDFORM.
