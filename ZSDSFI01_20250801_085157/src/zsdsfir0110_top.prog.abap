*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0110_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : TCURR.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          TRKORR  TYPE E070-TRKORR,
          AS4USER TYPE E070-AS4USER,
          AS4TEXT TYPE E07T-AS4TEXT,
          CHECK   TYPE C,
        END OF GY_RESULT.

*TYPES:
TYPE-POOLS:SLIS.
TYPES: BEGIN OF TY_XML,
         CAL        TYPE I,
         RATE_TYPE  TYPE TCURR-KURST,
         RATE_DE    TYPE CHAR100,
         TO_CURR    TYPE TCURR-TCURR,
         FROM_CURR  TYPE TCURR-FCURR,
         EX_RATE    TYPE TCURR-UKURS,
         RATIO_FROM TYPE TCURR-FFACT,
         RATIO_TO   TYPE TCURR-TFACT,
         VALID_FROM TYPE SY-DATUM, "tcurr-gdatu,
*rate          TYPE char10,
       END OF TY_XML.

TYPES: BEGIN OF TY_DATA,
         KURST TYPE TCURR-KURST,
         TCURR TYPE TCURR-TCURR,
         FCURR TYPE TCURR-FCURR,
         UKURS TYPE TCURR-UKURS,
         FFACT TYPE TCURR-FFACT,
         TFACT TYPE TCURR-TFACT,
         GDATU TYPE TCURR-GDATU,
       END OF TY_DATA.

TYPES : BEGIN OF TY_RATIO,
          RATE_TYPE  TYPE TCURF-KURST,
          FROM_CURR  TYPE TCURF-FCURR,
          TO_CURR    TYPE TCURF-TCURR,
          RATIO_FROM TYPE TCURF-FFACT,
          RATIO_TO   TYPE TCURF-TFACT,
        END OF TY_RATIO.

TYPES: BEGIN OF TY_RATE,
*  in          TYPE char1,
         RATE TYPE TCURR-FCURR,
       END OF TY_RATE.

TYPES : GY_FIELDCAT TYPE SLIS_FIELDCAT_ALV,
        GY_EVENTS   TYPE SLIS_ALV_EVENT,
        GY_LAYOUT   TYPE SLIS_LAYOUT_ALV.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : GE_LAYOUT    TYPE SLIS_LAYOUT_ALV.
DATA : GW_DATA_XML  TYPE SRT_XML_DATA.
DATA : GW_DATA_HEAD TYPE SRT_XML_DATA.
DATA : GW_XML_DATA  TYPE SMUM_XMLTB.
DATA : GW_DATA      TYPE TY_XML.
DATA : GW_XML       TYPE TY_XML.
DATA : GW_RATE      TYPE TY_RATE.
DATA : GW_RATIO     TYPE TY_RATIO.
DATA : GW_FIELDCAT TYPE GY_FIELDCAT,
       GW_EVENTS   TYPE GY_EVENTS,
       GW_LAYOUT   TYPE GY_LAYOUT.

*-> internal tables
DATA : GT_DATA_XML  TYPE SRT_XML_DATA_TAB.
DATA : GT_DATA_HEAD TYPE SRT_XML_DATA_TAB.
DATA : GT_XML_DATA  TYPE TABLE OF SMUM_XMLTB.
DATA : GT_DATA      LIKE BAPI1093_0.
DATA : GT_XML       TYPE TABLE OF TY_XML.
DATA : GT_RATE      TYPE TABLE OF TY_RATE.
DATA : GT_RATIO     TYPE TABLE OF TY_RATIO.

DATA : GW_RETURN        LIKE BAPIRET2,
       GT_RETURN        LIKE TABLE OF BAPIRET2,
       GW_COMMIT_RETURN LIKE BAPIRET2,
       GT_COMMIT        LIKE TABLE OF BAPIRET2.

DATA : GT_FIELDCAT TYPE STANDARD TABLE OF GY_FIELDCAT,
       GT_EVENTS   TYPE STANDARD TABLE OF GY_EVENTS.

DATA : GT_EXCHANGE_RATE TYPE ZSDSFIS097_TT,
       GS_EXCHANGE_RATE TYPE ZSDSFIS097.

DATA : GV_DATE TYPE CHAR10.

DATA : LO_CLIENT     TYPE REF TO IF_HTTP_CLIENT.
DATA : LO_CONV       TYPE REF TO CL_ABAP_CONV_IN_CE.
DATA : LV_RESULT_URL TYPE STRING.
DATA : LV_BIN        TYPE XSTRING.
DATA : LV_RESPONSE   TYPE STRING.

DATA : GV_HCM_RFC TYPE STRING. "ADD by Ratchapol K. TISAD-3553

DATA : BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.

DATA : MESSTAB   LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       T_MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       S_MESSTAB LIKE BDCMSGCOLL.

DATA : DYNPFIELDS  LIKE DYNPREAD OCCURS 5 WITH HEADER LINE.

DATA : GT_FCAT   TYPE SLIS_T_FIELDCAT_ALV,
       GS_LAYOUT TYPE SLIS_LAYOUT_ALV,
       GT_SORT   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT   TYPE SLIS_SORTINFO_ALV.
*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS: GC_I  TYPE C LENGTH 1 VALUE 'I',
           GC_EQ TYPE C LENGTH 2 VALUE 'EQ',
           GC_S  TYPE C LENGTH 1 VALUE 'S',
           GC_E  TYPE C LENGTH 1 VALUE 'E',
           GC_X  TYPE C LENGTH 1 VALUE 'X',
           GC_A  TYPE C LENGTH 1 VALUE 'A',
           GC_L  TYPE C LENGTH 1 VALUE 'L'.

*-----------------------------------------------------------------------
* R A N G E S
*-----------------------------------------------------------------------
DATA R_RATE TYPE RANGE OF TCURR-FCURR.
*-----------------------------------------------------------------------
* M A C R O S
*-----------------------------------------------------------------------
*DEFINE.
*END-OF-DEFINITION.

*-----------------------------------------------------------------------
* F I E L D – S Y M B O L S
*-----------------------------------------------------------------------
FIELD-SYMBOLS: <FS_XML> TYPE TY_XML.
FIELD-SYMBOLS: <FS_RATE> TYPE TY_RATE.

*-----------------------------------------------------------------------
* F I E L D – G R O U P S
*-----------------------------------------------------------------------
*FIELD-GROUPS:
