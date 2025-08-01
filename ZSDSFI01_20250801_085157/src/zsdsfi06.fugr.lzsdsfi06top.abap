FUNCTION-POOL ZSDSFI06.                     "MESSAGE-ID ..

* INCLUDE LZSDSFI06D...                      " Local class definition

* INCLUDE LZETAXD...                         " Local class definition

TYPE-POOLS TRUXS.

CONSTANTS:
  GC_ERROR        TYPE BAPI_MTYPE  VALUE 'E',
  GC_SUCCESS      TYPE BAPI_MTYPE  VALUE 'S',
  GC_TRUE         TYPE CHAR5       VALUE 'true',
  GC_FALSE        TYPE CHAR5       VALUE 'false',
  GC_HH           TYPE STRING      VALUE 'HH',  "Header
  GC_HR           TYPE STRING      VALUE 'HR',  "Header Doc reference
  GC_HV           TYPE STRING      VALUE 'HV',  "Header Doc VAT
  GC_HC           TYPE STRING      VALUE 'HC',  "Header Disc & Chagre
  GC_DD           TYPE STRING      VALUE 'DD',  "Item Details
  GC_DC           TYPE STRING      VALUE 'DC',  "Item Disc & Chagre per unit
  GC_DF           TYPE STRING      VALUE 'DF',  "Item Disc & Chagre per item

  GC_FTP_TXT      TYPE STRING      VALUE 'FTP-TXT',
  GC_FTP_PDF      TYPE STRING      VALUE 'FTP-PDF',
  GC_FTP_PDF_SKIP TYPE STRING      VALUE 'FTP-PDF-SKIP',
  GC_CALL_WS      TYPE STRING      VALUE 'CALL-WS',
  GC_TRANS_ITEM1  TYPE STRING      VALUE '0000001'.

CONSTANTS:
  GC_SAP_DIR_OUT  TYPE STRING      VALUE '/interface/EZTax/Out/',
  GC_SAP_DIR_ARCH TYPE STRING      VALUE '/interface/EZTax/Archived/',
  GC_FTP_DIR_IN   TYPE STRING      VALUE '/Dad/Eztax/Out/'.

CONSTANTS:
*  gc_fi_flnam_pdf TYPE string      VALUE 'EZTAX_$YYMMDD_$HHMMSS_$SAPOBJECTID_$OUTPUTTYPE.pdf',
  GC_FI_FLNAM_PDF TYPE STRING      VALUE 'EZTAX_$YYMMDD_$HHMMSS_$SAPOBJECTID_$COMP_$OUTPUTTYPE.pdf',
  GC_SD_FLNAM_PDF TYPE STRING      VALUE 'EZTAX_$YYMMDD_$HHMMSS_$SAPOBJECTID_$COMP_$OUTPUTTYPE.pdf',
  GC_FLNAM_TXT    TYPE STRING      VALUE 'EZTAX_$YYMMDD_$HHMMSS_$TRANSNO.txt'.

*Logical Port (maintained via SOAMANAGER)
CONSTANTS:
  GC_LOGICAL_PORT  TYPE PRX_LOGICAL_PORT_NAME VALUE 'ZETXSERVICE1'.

*--------------------------------------------------------------------*
*& STRUCTURE/WORK AREAS (gs_)
*--------------------------------------------------------------------*
DATA:
*  gs_file_list      TYPE gty_file_list,
  GS_FILE_PATH      TYPE ZSDSFIS028.
*  gs_t005u          TYPE t005u.

*--------------------------------------------------------------------*
*& BEGIN: COPY FROM AL11 ( REPORT: RSWATCH0 )
*--------------------------------------------------------------------*
CONSTANTS:
  C_LINE_SIZE        TYPE I VALUE 512,
  C_FILE_LIST_CREATE TYPE C VALUE 'I',
  C_FILE_LIST_UPDATE TYPE C VALUE 'U'.

*--------------------------------------------------------------------*
* GLOBAL VARIABLES (gv_)
*--------------------------------------------------------------------*
DATA: GV_LOGD            TYPE FLAG.

TYPES: NAME_OF_DIR(1024) TYPE C,
*      buffer for displaying file contents or downloading file
       T_BUFFER          TYPE STRING.
TYPES: BEGIN OF TS_FILE,
         DIRNAME     TYPE DIRNAME_AL11,   " name of directory
         NAME        TYPE FILENAME_AL11,   " name of entry
         TYPE(10)    TYPE C,   " type of entry.
         LEN(8)      TYPE P,   " length in bytes.
         OWNER(20)   TYPE C,   " owner of the entry.
         MTIME(6)    TYPE P,   " last mod.date, sec since 1970
         MODE(9)     TYPE C,   " like "rwx-r-x--x": prot. mode
         USEABLE(1)  TYPE C,
         SUBRC(4)    TYPE C,
         ERRNO(3)    TYPE C,
         ERRMSG(40)  TYPE C,
         MOD_DATE    TYPE D,
         MOD_TIME(8) TYPE C,   " hh:mm:ss
         SEEN(1)     TYPE C,
         CHANGED(1)  TYPE C,
         STATUS(1)   TYPE C,
       END OF TS_FILE.

TYPES: BEGIN OF TY_PARAM,
         ID           TYPE ZSDSFIC015-ID,
         NAME         TYPE ZSDSFIC015-NAME,
         PARAM_EXT    TYPE ZSDSFIC015-PARAM_EXT,
         SEQUENCE     TYPE ZSDSFIC015-SEQUENCE,
         ENDDA        TYPE ZSDSFIC015-ENDDA,
         BEGDA        TYPE ZSDSFIC015-BEGDA,
         PARAM_SIGN   TYPE ZSDSFIC015-PARAM_SIGN,
         PARAM_OPTION TYPE ZSDSFIC015-PARAM_OPTION,
         LOW_VALUE    TYPE ZSDSFIC015-LOW_VALUE,
         HIGH_VALUE   TYPE ZSDSFIC015-HIGH_VALUE,
         COMMENTS     TYPE ZSDSFIC015-COMMENTS,
       END OF TY_PARAM.

TYPES: TTY_FILE  TYPE TABLE OF TS_FILE,
       TTY_PARAM TYPE TABLE OF TY_PARAM.

DATA: FILE      TYPE TS_FILE.
"file_list type standard table of ts_file with header line." WITH NON-UNIQUE SORTED KEY k1 COMPONENTS name.

DATA: BEGIN OF DIRECTORY_STACK OCCURS 5,
        NAME TYPE NAME_OF_DIR,
        "filter(50) type c,
      END OF DIRECTORY_STACK.

FIELD-SYMBOLS:
      <FILE> TYPE TS_FILE.

DATA: SRT(1)      VALUE 'T'
    , NO_CS       VALUE ' '          " no MUST_ContainString
    , ALL_GEN     VALUE '*'          " generic filename shall select all
    , STRLEN      LIKE  SY-FDPOS
    , FCODE(4)    TYPE C.
*--------------------------------------------------------------------*
*& END: COPY FROM AL11 ( REPORT: RSWATCH0 )
*--------------------------------------------------------------------*
