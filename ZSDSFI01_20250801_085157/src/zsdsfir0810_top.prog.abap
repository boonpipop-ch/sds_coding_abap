*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0810_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : E070.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          RACCT       TYPE ACDOCA-RACCT,
          HSL_P       TYPE ACDOCA-HSL,
          MAT_PSPNR   TYPE ACDOCA-MAT_PSPNR,
          PRCTR       TYPE ACDOCA-PRCTR,
          MATNR       TYPE MAKT-MATNR,
          SGTXT       TYPE MAKT-MAKTX,
          ORDER       TYPE BSEG-AUFNR,
          COST_CENTER TYPE BSEG-KOSTL,
          ITEM_TEXT   TYPE BSEG-SGTXT,
          VENDOR      TYPE BSEG-LIFNR,
          NAME        TYPE CHAR255,
          TAXCODE     TYPE ACDOCA-MWSKZ,
          GROUP       TYPE CHAR5,
          OT01_NAME1  TYPE CHAR35,
          OT01_NAME2  TYPE CHAR35,
          OT01_NAME3  TYPE CHAR35,
          OT01_NAME4  TYPE CHAR35,
          ADDR_NUM    TYPE CHAR50,
          DISTRIC     TYPE CHAR50,
          SUB_DISTRIC TYPE CHAR50,
          POSTCODE    TYPE CHAR5,
          TAXID       TYPE CHAR18,
          PHONE       TYPE CHAR20,
          EMAIL       TYPE CHAR20,
          DOC_NUM     TYPE BKPF-BELNR,
          GJAHR       TYPE BKPF-GJAHR,
          REFERENCE   TYPE BKPF-XBLNR,
          TAX_AMT     TYPE ACDOCA-HSL,
          TOTAL       TYPE ACDOCA-HSL,
          CITY        TYPE CHAR50,
          COUNTRY     TYPE ADRC-COUNTRY,
          DOC_DATE    TYPE SY-DATUM,
          BKTXT       TYPE BKPF-BKTXT,
          CHECK       TYPE C,
        END OF GY_RESULT.

TYPES : BEGIN OF GY_HEADER,
          DOC_DATE     TYPE SY-DATUM,
          POST_DATE    TYPE SY-DATUM,
          TYPE         TYPE BKPF-BLART,
          TYPE_C       TYPE BKPF-BLART,
          PERIOD       TYPE CHAR20,
          COMPANY_CODE TYPE BSEG-BUKRS,
          CURRENT      TYPE CHAR3,
          RATE         TYPE BKPF-KURSF,
          REFERENCE    TYPE BKPF-XBLNR,
          DOC_HEADER   TYPE BKPF-BKTXT,
          SALES_VENDC  TYPE CHAR10,
          SALES_VENDN  TYPE CHAR255,
          DOC_CLEAR    TYPE CHAR10,
          DOC_CLEAR_Y  TYPE CHAR4,
          DOC_AP       TYPE CHAR10,
          DOC_AP_Y     TYPE CHAR4,
          DOC_ADVAN    TYPE CHAR10,
          DOC_ADVAN_Y  TYPE CHAR4,
          CLRDB        TYPE BSEG-HKONT,
          CLRCR        TYPE BSEG-HKONT,
          ITEM_TEXT    TYPE BSEG-SGTXT,
        END OF GY_HEADER.

TYPES  : BEGIN OF GY_T030K,
           MWSKZ TYPE T030K-MWSKZ,
           KONTS TYPE T030K-KONTS,
         END OF GY_T030K.

DATA : BEGIN OF GY_TAXRATE,
         MWSKZ TYPE T007A-MWSKZ,
         KBETR TYPE KONP-KBETR,
       END OF GY_TAXRATE.

*TYPES : BEGIN OF GY_RESULT2,
*          RACCT TYPE ACDOCA-RACCT,
*          HSL_P TYPE ACDOCA-HSL,
*          CHECK TYPE C,
*        END OF GY_RESULT2.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : GT_RESULT2 TYPE TABLE OF GY_RESULT,
       GS_RESULT2 TYPE GY_RESULT.

DATA : GS_HEADER TYPE GY_HEADER.

DATA : GV_ROW TYPE LVC_S_ROID-ROW_ID.

DATA : GV_HEADER_TEXT TYPE C LENGTH 255.

DATA : BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.

DATA : MESSTAB   LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       T_MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       S_MESSTAB LIKE BDCMSGCOLL.

DATA : GV_CURSOR TYPE CHAR255.

DATA : DYNPFIELDS  LIKE DYNPREAD OCCURS 5 WITH HEADER LINE.

DATA : GT_FCAT   TYPE SLIS_T_FIELDCAT_ALV,
       GS_LAYOUT TYPE SLIS_LAYOUT_ALV,
       GT_SORT   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT   TYPE SLIS_SORTINFO_ALV.
*--------------------------------------------------------------------*
DATA : GT_FCAT_OO   TYPE LVC_T_FCAT,
       GS_LAYOUT_OO TYPE LVC_S_LAYO,
       GT_SORT_OO   TYPE LVC_T_SORT,
       GS_SORT_OO   TYPE LVC_SORT.

DATA : GT_EVENTS TYPE SLIS_T_EVENT,
       GS_EVENTS TYPE SLIS_ALV_EVENT.

DATA : GCL_ALV  TYPE REF TO CL_GUI_ALV_GRID,
       GCL_CONT TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA : GT_TOOLBAR_EXCLUDING     TYPE UI_FUNCTIONS.
*--------------------------------------------------------------------*
DATA : GT_FCAT2_OO   TYPE LVC_T_FCAT,
       GS_LAYOUT2_OO TYPE LVC_S_LAYO,
       GT_SORT2_OO   TYPE LVC_T_SORT,
       GS_SORT2_OO   TYPE LVC_SORT.

DATA : GT_EVENTS2 TYPE SLIS_T_EVENT,
       GS_EVENTS2 TYPE SLIS_ALV_EVENT.

DATA : GCL_ALV2  TYPE REF TO CL_GUI_ALV_GRID,
       GCL_CONT2 TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA : GT_TOOLBAR_EXCLUDING2     TYPE UI_FUNCTIONS.
DATA : GC_ADDITIONAL_FILED TYPE C LENGTH 10 VALUE 'ZSDSFIS007'.

DATA : GV_HEAD   TYPE C LENGTH 4 VALUE 'SHOW'.
DATA : GV_DETIAL TYPE C LENGTH 4 VALUE 'SHOW'.

DATA : CB_ADVANCE TYPE C.

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


CONSTANTS: GC_CON_NAME TYPE C LENGTH 6  VALUE 'CC_ALV'.
CONSTANTS: GC_CON_NAME2 TYPE C LENGTH 7  VALUE 'CC_ALV2'.
CONSTANTS : BEGIN OF GC_CON,
              DOC_TYPE                TYPE BLART       VALUE 'K1',
              CREDIT                  TYPE C           VALUE 'H',
              DEBIT                   TYPE C           VALUE 'S',
              ADDITIONAL_FILED        TYPE C LENGTH 10 VALUE 'ZSDSFIS007',
              ADDITIONAL_FILED_VENDOR TYPE C LENGTH 10 VALUE 'ZSDSFIS015',
              ADDITIONAL_FILED_V2     TYPE C LENGTH 10 VALUE 'ZSDSFIS016',
              BRNCH                   TYPE C LENGTH 4  VALUE '0000',
              BUPLA                   TYPE C LENGTH 4  VALUE '0000',
              WT_TYPE                 TYPE C LENGTH 2  VALUE '11',
              E                       TYPE C           VALUE 'E',
              S                       TYPE C           VALUE 'S',
              M                       TYPE C           VALUE 'M',
              K                       TYPE C           VALUE 'K',
              IX                      TYPE C LENGTH 2  VALUE 'IX',
              STAR                    TYPE C           VALUE '*',
              MWVS                    TYPE C LENGTH 4  VALUE 'MWVS',
              VST                     TYPE C LENGTH 3  VALUE 'VST',
              40                      TYPE C LENGTH 2  VALUE '40',
              50                      TYPE C LENGTH 2  VALUE '50',
              31                      TYPE C LENGTH 2  VALUE '31',
              27                      TYPE C LENGTH 2  VALUE '27',
              21                      TYPE C LENGTH 2  VALUE '21',
              03                      TYPE C LENGTH 2  VALUE '03',
              29                      TYPE C LENGTH 2  VALUE '29',
              39                      TYPE C LENGTH 2  VALUE '39',
              EGK                     TYPE C LENGTH 3  VALUE 'EGK',
              THB                     TYPE C LENGTH 3  VALUE 'THB',
              DIT                     TYPE C LENGTH 10 VALUE '0000001001',
              MIRO                    TYPE C LENGTH 4  VALUE 'MIRO',
              TAXTH                   TYPE C LENGTH 5  VALUE 'TAXTH',
              TX                      TYPE C LENGTH 2  VALUE 'TX',
              TH                      TYPE C LENGTH 2  VALUE 'TH',
              BANGKOK                 TYPE C LENGTH 9  VALUE 'กรุงเทพฯ',
              P109999                 TYPE C LENGTH 10  VALUE '0019999999',
              F43                     TYPE C LENGTH 3  VALUE 'F43',
              APVI                    TYPE C LENGTH 4  VALUE 'APVI',
              KR                      TYPE C LENGTH 2  VALUE 'KR',
              DN                      TYPE C LENGTH 2  VALUE 'DN',
              OT03                    TYPE C LENGTH 4  VALUE 'OT03',
              BCOD                    TYPE C LENGTH 4  VALUE 'BCOD',
              TXID                    TYPE C LENGTH 4  VALUE 'TXID',
              103                     TYPE C LENGTH 3  VALUE '103',
              RE                      TYPE C LENGTH 2  VALUE 'RE',
              VX                      TYPE C LENGTH 2  VALUE 'VX',
              ADMIN_PROFIT            TYPE C LENGTH 10 VALUE '0010999900',
              RCOA                    TYPE C LENGTH 4 VALUE 'RCOA',
            END OF GC_CON.

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
