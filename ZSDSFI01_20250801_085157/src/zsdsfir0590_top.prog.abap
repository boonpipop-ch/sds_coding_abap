*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0590_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : PAYR,
         ACDOCA,
         LFA1,
         LFBK,
         BNKA,
         BKPF.

TYPE-POOLS SLIS.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          GROUP(300) TYPE  C,
          BUKRS      LIKE  PAYR-ZBUKR,
          LIFNR      LIKE  PAYR-LIFNR,
          NAME1(250) TYPE  C,
          AUGBL      TYPE  BSAK_VIEW-AUGBL,
          AUGDT      TYPE  BSAK_VIEW-AUGDT,
          CHECT      TYPE  PAYR-CHECT,
          CHECF      TYPE  PAYR-CHECF,
          CHECV      TYPE  PAYR-CHECV,
          VOIDR      TYPE  PAYR-VOIDR,
          VBLNR      TYPE  PAYR-VBLNR,
          GJAHR_P    TYPE  PAYR-GJAHR,
          GJAHR_D    TYPE  BSAK_VIEW-GJAHR,
          BELNR      TYPE  BSAK_VIEW-BELNR,
          BUZEI      TYPE  BSAK_VIEW-BUZEI,
          BUDAT      TYPE  BSAK_VIEW-BUDAT,
          BLDAT      TYPE  BSAK_VIEW-BLDAT,
          WAERS      TYPE  PAYR-WAERS,
          XBLNR      TYPE  BSAK_VIEW-XBLNR,
          BLART      TYPE  BSAK_VIEW-BLART,
          SHKZG      TYPE  BSAK_VIEW-SHKZG,
          MWSKZ      TYPE  BSAK_VIEW-MWSKZ,
          RWBTR      TYPE  PAYR-RWBTR,
          DMBTR      TYPE  BSAK_VIEW-DMBTR,
          WRBTR      TYPE  BSAK_VIEW-WRBTR,
          MWSTS      TYPE  BSAK_VIEW-MWSTS,
          WMWST      TYPE  BSAK_VIEW-WMWST,
          HBKID      TYPE  BSAK_VIEW-HBKID,
          QSSKZ      TYPE  BSAK_VIEW-QSSKZ,
          QSSHB      TYPE  BSAK_VIEW-QSSHB,
          QBSHB      TYPE  BSAK_VIEW-QBSHB,
          QSZNR      TYPE  BSAK_VIEW-QSZNR,
          BSCHL      TYPE  BSAK_VIEW-BSCHL,
          ZUMSK      TYPE  BSAK_VIEW-ZUMSK,
          SGTXT      TYPE  BSAK_VIEW-SGTXT,
          BANKL      TYPE  LFBK-BANKL,
          BANKN      TYPE  LFBK-BANKN,
          BKREF      TYPE  LFBK-BKREF,
          BANKA      TYPE  BNKA-BANKA,
          DMBTR_BV   TYPE  BSAK_VIEW-DMBTR,
          DMBTR_V    TYPE  BSAK_VIEW-DMBTR,
          CHECK      TYPE C,
        END OF GY_RESULT.

TYPES: BEGIN OF TY_COL_STRUC,
         COL_POS   LIKE SY-CUCOL,
         FIELDNAME TYPE SLIS_FIELDNAME,
         TABNAME   TYPE SLIS_TABNAME,
       END OF TY_COL_STRUC.
TYPES: TY_COL_QUEUE TYPE TY_COL_STRUC OCCURS 1.

TYPES: BEGIN OF TYP_BSAK_CHECK,
         AUGBL TYPE BSAK_VIEW-AUGBL,
         AUGGJ TYPE BSAK_VIEW-AUGGJ,
       END OF TYP_BSAK_CHECK.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : GT_BSAK  TYPE STANDARD TABLE OF  BSAK_VIEW,
       GW_BSAK  TYPE STANDARD TABLE OF  BSAK_VIEW,
       GT_BSAK1 TYPE STANDARD TABLE OF  BSAK_VIEW,
       GT_BSEG  TYPE STANDARD TABLE OF  BSEG,
       GW_BSEG  TYPE BSEG,
       GT_BSAD  TYPE STANDARD TABLE OF  BSAD_VIEW,
       GW_BSAD  TYPE BSAD_VIEW,
       GW_BSAK1 TYPE BSAK_VIEW,
       GT_BKPF  TYPE STANDARD TABLE OF  BKPF,
       GW_BKPF  TYPE STANDARD TABLE OF  BKPF,
       GT_LFA1  TYPE STANDARD TABLE OF  LFA1,
       GT_LFBK  TYPE STANDARD TABLE OF  LFBK,
       GT_BANKA TYPE STANDARD TABLE OF  BNKA,
       GW_LFA1  TYPE LFA1,
       GW_LFBK  TYPE LFBK,
       GW_LFBK1 TYPE LFBK,
       GW_BANKA TYPE BNKA,
       GT_BSEC  TYPE STANDARD TABLE OF  BSEC,
       GW_BSEC  TYPE BSEC,
       GT_BSAS  TYPE STANDARD TABLE OF BSAS_VIEW,
       GW_BSAS  TYPE BSAS_VIEW,
       GT_BSIS  TYPE STANDARD TABLE OF BSIS_VIEW,
       GW_BSIS  TYPE BSIS_VIEW.

DATA : GT_IPAY    TYPE STANDARD TABLE OF GY_RESULT,
       GT_IPAY2   TYPE STANDARD TABLE OF GY_RESULT,
       GT_IPAY3   TYPE STANDARD TABLE OF GY_RESULT,
       GW_IPAY    TYPE GY_RESULT,
       GW_IPAY2   TYPE GY_RESULT,
       GW_IPAY3   TYPE GY_RESULT,
       GT_PAYM    TYPE STANDARD TABLE OF PAYR,
       GW_PAYM    TYPE PAYR,
       GWA_RESULT TYPE GY_RESULT,
       GW_RWBTR   TYPE P DECIMALS 2,
       I_LINES    LIKE SY-TABIX.

DATA: INT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      WA_FIELDCAT  TYPE SLIS_FIELDCAT_ALV.
DATA: GW_COL_QUEUE TYPE TY_COL_STRUC.
DATA: GT_COL_QUEUE LIKE TABLE OF GW_COL_QUEUE.
DATA: GW_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
DATA: GV_FILTER_AUART TYPE C.

DATA: GT_BSAK_CHECK TYPE HASHED TABLE OF TYP_BSAK_CHECK WITH UNIQUE KEY AUGBL
                                                                        AUGGJ.
DATA: WA_BSAK_CHECK TYPE TYP_BSAK_CHECK,
      WA_BSAK       TYPE BSAK_VIEW.

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
*DATA:
*-----------------------------------------------------------------------
* M A C R O S
*-----------------------------------------------------------------------
*DEFINE.
*END-OF-DEFINITION.

*-----------------------------------------------------------------------
* F I E L D – S Y M B O L S
*-----------------------------------------------------------------------
*FIELD-SYMBOLS:

*-----------------------------------------------------------------------
* F I E L D – G R O U P S
*-----------------------------------------------------------------------
*FIELD-GROUPS:
