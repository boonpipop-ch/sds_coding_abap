*&---------------------------------------------------------------------*
*& Include          ZSDSFII0050_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
*TABLES : E070.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          CONTN TYPE ZSDSDE_LEASE_CONTRACT_NO,
          LIFNR TYPE LIFNR,
          RUNNG TYPE ZSDSDE_RUNNING3,
          MONIT TYPE ZSDSDE_MONTH,
          STDDT TYPE DATUM,
          ENDDT TYPE DATUM,
          USDAY TYPE I,
          REGMT TYPE P DECIMALS 2,
          INTER TYPE P DECIMALS 2,
          AMOZT TYPE P DECIMALS 2,
          VATMT TYPE P DECIMALS 2,
          OUTSC TYPE P DECIMALS 2,
          DEPRC TYPE P DECIMALS 2,
          TOTAD TYPE I,
          POSTD TYPE SY-DATUM,
          FIDOC TYPE BKPF-BELNR,
          FIYER TYPE BKPF-GJAHR,
          FIDZ1 TYPE BKPF-BELNR,
          FIDZ2 TYPE BKPF-BELNR,
          STATU TYPE ICON_D,
          MESSG TYPE C LENGTH 50,
          CHECK TYPE C,
          ALVED TYPE LVC_T_STYL,
        END OF GY_RESULT.

TYPES : BEGIN OF GY_AMT,
          STDDT TYPE DATUM,
          ENDDT TYPE DATUM,
          REGMT TYPE P DECIMALS 2,
          INTER TYPE P DECIMALS 4,
          CHECK TYPE C,
          ALVED TYPE LVC_T_STYL,
        END OF GY_AMT.

TYPES : BEGIN OF GY_HEADER,
          WAERS               TYPE WAERS,
          ANLN1               TYPE ANLN1,
          ANLN2               TYPE ANLN2,
          ASSNA               TYPE C LENGTH 50,
          INTER               TYPE P DECIMALS 4,
          INTEE               TYPE P DECIMALS 4,
          LIFNR               TYPE LFA1-LIFNR,
          LASOR               TYPE C LENGTH 140,
          LEASA               TYPE C LENGTH 70,
          CONTN               TYPE C LENGTH 25,
          COSEV               TYPE P DECIMALS 2,
          VATMT               TYPE P DECIMALS 2,
          RESID               TYPE P DECIMALS 2,
          LEAST               TYPE P DECIMALS 0,
          PAYMP               TYPE P DECIMALS 0,
          TIMES               TYPE P DECIMALS 0,
          RENEV               TYPE P DECIMALS 2,
          TOTAR               TYPE P DECIMALS 2,
          DEPOS               TYPE P DECIMALS 2,
          STADT               TYPE SY-DATUM,
          ENDDT               TYPE SY-DATUM,
          TODAY               TYPE P DECIMALS 0,
          PAYDT               TYPE SY-DATUM,
          FLAGE               TYPE FLAG,
          REMAK               TYPE C LENGTH 255,
          EXTED               TYPE FLAG,
          ERNAM               TYPE SY-UNAME,
          ERDAT               TYPE SY-DATUM,
          ERZET               TYPE SY-UZEIT,
          RUNNG               TYPE ZSDSDE_RUNNING3,
          STATU               TYPE ZSDSDE_STATUS,
          NPVFG               TYPE CHAR1,
          PVFAG               TYPE CHAR1,
          FISCO               TYPE BKPF-BELNR,
          FIECO               TYPE BKPF-BELNR,
          FIRCO               TYPE BKPF-BELNR,
          FIACO               TYPE BKPF-BELNR,
          GLREN               TYPE SAKNR,
          FLAGP               TYPE CHAR1,
          PRCTR               TYPE PRCTR,
          ACPVL               TYPE P DECIMALS 2,
          BUDGT               TYPE CHAR50,
          TEXT_FOR_RATE       TYPE C LENGTH 10,
          TEXT_FOR_RATE_EIR   TYPE C LENGTH 10,
          TEXT_LEASE_TERM     TYPE C LENGTH 10,
          TEXT_PAYMENT_PERIOD TYPE C LENGTH 10,
          TEXT_TIMES          TYPE C LENGTH 10,
          TEXT_CUR_COST       TYPE C LENGTH 10,
          TEXT_CUR_REST       TYPE C LENGTH 10,
          TEXT_CUR_RENT       TYPE C LENGTH 10,
          TEXT_CUR_DEPO       TYPE C LENGTH 10,
          TEXT_CUR_VAT        TYPE C LENGTH 10,
          SGTXT               TYPE SGTXT,
        END OF GY_HEADER.

TYPES : BEGIN OF GY_GL,
          INTER TYPE SAKNR,
          AMORT TYPE SAKNR,
          DEPRE TYPE SAKNR,
          MONPY TYPE SAKNR,
          ACCDP TYPE SAKNR,
          LESCL TYPE SAKNR,
          LESLI TYPE SAKNR,
          DEFER TYPE SAKNR,
          RENEP TYPE SAKNR,
          UNACP TYPE SAKNR,
        END OF GY_GL.

TYPES : BEGIN OF GY_ATTACH_FILE,
          ATTIT TYPE ZSDSDE_ATTACH_LINE_ITEM,
          FILEN TYPE ZSDSDE_FILE_NAME,
        END OF GY_ATTACH_FILE.

TYPES : BEGIN OF GY_REPORT,
          CONTN TYPE ZSDSDE_LEASE_CONTRACT_NO,
          LIFNR TYPE LIFNR,
          RUNNG TYPE ZSDSDE_RUNNING3,
          ASSNA TYPE ZSDSFIT002-ASSNA,
          WAERS TYPE ZSDSFIT002-WAERS,
          INTER TYPE ZSDSFIT002-INTER,
          LEASA TYPE ZSDSFIT002-LEASA,
          STADT TYPE ZSDSFIT002-STADT,
          ENDDT TYPE ZSDSFIT002-ENDDT,
          PAYDT TYPE ZSDSFIT002-PAYDT,
          COSEV TYPE ZSDSFIT002-COSEV,
          VATMT TYPE ZSDSFIT002-VATMT,
          RESID TYPE ZSDSFIT002-RESID,
          LEAST TYPE ZSDSFIT002-LEAST,
          PAYMP TYPE ZSDSFIT002-PAYMP,
          TIMES TYPE ZSDSFIT002-TIMES,
          RENEV TYPE ZSDSFIT002-RENEV,
          DEPOS TYPE ZSDSFIT002-DEPOS,
          TODAY TYPE ZSDSFIT002-TODAY,
          FLAGE TYPE ZSDSFIT002-FLAGE,
          EXTED TYPE ZSDSFIT002-EXTED,
          REMAK TYPE ZSDSFIT002-REMAK,
          STATU TYPE ZSDSFIT002-STATU,
          ERNAM TYPE ZSDSFIT002-ERNAM,
          ERDAT TYPE ZSDSFIT002-ERDAT,
          ERZET TYPE ZSDSFIT002-ERZET,
          AENAM TYPE ZSDSFIT002-AENAM,
          AEDAT TYPE ZSDSFIT002-AEDAT,
          AEZET TYPE ZSDSFIT002-AEZET,
        END OF GY_REPORT.

TYPES : BEGIN OF GY_FI,
          DEBIT       TYPE SKA1-SAKNR,
          CREDIT      TYPE SKA1-SAKNR,
          DEBIT_DESC  TYPE C LENGTH 50,
          CREDIT_DESC TYPE C LENGTH 50,
        END OF GY_FI.

TYPES : BEGIN OF GY_ZSDSFIT022,
          TYPES TYPE ZSDSFIT022-TYPES,
          MONIT TYPE ZSDSFIT022-MONIT,
          BELNR	TYPE BELNR_D,
          GJAHR	TYPE GJAHR,
        END OF GY_ZSDSFIT022.


TYPES : GYT_ZSDSFIT002 TYPE TABLE OF ZSDSFIT002.
*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF GY_RESULT WITH EMPTY KEY,
       GS_RESULT TYPE GY_RESULT.

DATA : GT_AMT TYPE TABLE OF GY_AMT,
       GS_AMT TYPE GY_AMT.

DATA : GT_HEADER TYPE TABLE OF GY_HEADER,
       GS_HEADER TYPE GY_HEADER.

DATA : GT_REPORT TYPE TABLE OF GY_REPORT,
       GS_REPORT TYPE GY_REPORT.

DATA : GS_FI TYPE GY_FI,
       GT_FI TYPE TABLE OF GY_FI.

DATA : GS_ZSDSFIT022 TYPE GY_ZSDSFIT022,
       GT_ZSDSFIT022 TYPE TABLE OF GY_ZSDSFIT022.

DATA : GS_GL TYPE GY_GL.

DATA : GV_CURSOR TYPE C LENGTH 50.

DATA : GS_REMARK TYPE C LENGTH 255,
       GT_REMARK LIKE TABLE OF GS_REMARK.

DATA : GT_ATTACH_FILE TYPE TABLE OF GY_ATTACH_FILE,
       GS_ATTACH_FILE TYPE GY_ATTACH_FILE.

DATA : R_PV  TYPE C LENGTH 1 VALUE ABAP_TRUE,
       R_NPV TYPE C LENGTH 1 VALUE SPACE.

DATA : GV_MODE         TYPE C LENGTH 3,
       GV_ASSET        TYPE FLAG,
       GV_DISABLE_END  TYPE FLAG,
       GV_POST_DATE_ST TYPE SY-DATUM,
       GV_COST_CENTER  TYPE ANLZ-KOSTL,
       GV_FUNC_AREA    TYPE CSKS-FUNC_AREA.

DATA : GV_SUM_INTER TYPE P DECIMALS 2.

DATA : BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.

DATA : MESSTAB   LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       T_MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       S_MESSTAB LIKE BDCMSGCOLL.

DATA : DYNPFIELDS  LIKE DYNPREAD OCCURS 5 WITH HEADER LINE.

DATA : GT_FCAT   TYPE SLIS_T_FIELDCAT_ALV,
       GS_LAYOUT TYPE SLIS_LAYOUT_ALV,
       GT_SORT   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT   TYPE SLIS_SORTINFO_ALV.

DATA : GT_FCAT_OO   TYPE LVC_T_FCAT,
       GS_LAYOUT_OO TYPE LVC_S_LAYO,
       GT_SORT_OO   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT_OO   TYPE SLIS_SORTINFO_ALV.

DATA : GT_FCAT_OO_AMT   TYPE LVC_T_FCAT,
       GS_LAYOUT_OO_AMT TYPE LVC_S_LAYO,
       GT_SORT_OO_AMT   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT_OO_AMT   TYPE SLIS_SORTINFO_ALV.

DATA : GT_EVENTS TYPE SLIS_T_EVENT,
       GS_EVENTS TYPE SLIS_ALV_EVENT.

DATA : GCL_ALV      TYPE REF TO CL_GUI_ALV_GRID,
       GCL_ALV_AMT  TYPE REF TO CL_GUI_ALV_GRID,
       GCL_CONT     TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       GCL_CONT_AMT TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA : GCL_REMARK TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       GCL_EDITOR TYPE REF TO CL_GUI_TEXTEDIT.

DATA : GT_TOOLBAR_EXCLUDING     TYPE UI_FUNCTIONS,
       GT_TOOLBAR_EXCLUDING_AMT TYPE UI_FUNCTIONS.

DATA : GT_EC_TOOL_101 TYPE TABLE OF SLIS_EXTAB.
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

CONSTANTS: GC_CON_NAME         TYPE C LENGTH 6  VALUE 'CC_ALV',
           GC_CON_NAME_AMT     TYPE C LENGTH 10 VALUE 'CC_ALV_AMT',
           GC_GEN              TYPE C LENGTH 6  VALUE 'BT_GEN',
           GC_BACK             TYPE C LENGTH 4  VALUE 'BACK',
           GC_CANC             TYPE C LENGTH 4  VALUE 'CANC',
           GC_EXIT             TYPE C LENGTH 4  VALUE 'EXIT',
           GC_SAVE             TYPE C LENGTH 4  VALUE 'SAVE',
           GC_ATTCH            TYPE C LENGTH 5  VALUE 'ATTCH',
           GC_REMARK           TYPE C LENGTH 9  VALUE 'CC_REMARK',
           GC_POST             TYPE C LENGTH 4  VALUE 'POST',
           GC_POST_T           TYPE C LENGTH 12  VALUE '++++Post++++',
           GC_SKIP             TYPE C LENGTH 4  VALUE 'SKIP',
           GC_SKIP_T           TYPE C LENGTH 12  VALUE 'Skip',
           GC_UNSKIP           TYPE C LENGTH 6  VALUE 'UNSKIP',
           GC_UNSKIP_T         TYPE C LENGTH 12  VALUE 'Cancel Skip',
           GC_DEL              TYPE C LENGTH 4  VALUE 'DELT',
           GC_CHECK            TYPE C LENGTH 5  VALUE 'CHECK',
           GC_POST_DATE        TYPE C LENGTH 5  VALUE 'POSTD',
           GC_EXE              TYPE C LENGTH 3  VALUE 'EXE',
           GC_DELETE           TYPE C LENGTH 6  VALUE 'DELETE',
           GC_ADDITIONAL_FILED TYPE C LENGTH 10 VALUE 'ZSDSFIS007',
           GC_ADDITIONAL_ASSET TYPE C LENGTH 10 VALUE 'ZSDSFIS117',
           GC_ADD_AMT          TYPE C LENGTH 7  VALUE 'ADD_AMT',
           GC_INS              TYPE C LENGTH 3  VALUE 'INS',
           GC_FI_START         TYPE C LENGTH 8  VALUE 'FI_START',
           GC_FI_END           TYPE C LENGTH 6  VALUE 'FI_END',
           GC_YES              TYPE C LENGTH 3  VALUE 'Yes',
           GC_NO               TYPE C LENGTH 2  VALUE 'No',
           GC_POST_ST          TYPE C LENGTH 7  VALUE 'POST_ST',
           GC_CANCEL_POST      TYPE C LENGTH 11 VALUE 'CANCEL_POST',
           GC_CHART_ACC        TYPE C LENGTH 4  VALUE 'RCOA',
           GC_COM_CODE         TYPE C LENGTH 4  VALUE '1000',
           GC_START            TYPE C LENGTH 5  VALUE 'START',
           GC_ASSET_POST       TYPE C LENGTH 5  VALUE 'ASSET',
           GC_END              TYPE C LENGTH 3  VALUE 'END',
           GC_ITEM             TYPE C LENGTH 4  VALUE 'ITEM',
           GC_ITEML            TYPE C LENGTH 5  VALUE 'ITEML',
           GC_ITEMT            TYPE C LENGTH 5  VALUE 'ITEMT',
           GC_RETIR            TYPE C LENGTH 5  VALUE 'RETIR',
           GC_RETIR_Z1         TYPE C LENGTH 5  VALUE 'RETZ1',
           GC_RETIR_Z2         TYPE C LENGTH 5  VALUE 'RETZ2',
           GC_DEFAULT_SUB      TYPE C LENGTH 4  VALUE '0000',
           GC_ONE              TYPE C LENGTH 1  VALUE '1',
           GC_GL_INTEREST      TYPE C LENGTH 10 VALUE '6313000010',
           GC_GL_DEFER_INS     TYPE C LENGTH 10 VALUE '1899000010', "'2899000010', "'2431000010',
           GC_GL_RENT_EXP      TYPE C LENGTH 10 VALUE '5232000010',
           GC_GL_UNACCEPT_VAL  TYPE C LENGTH 10 VALUE '5277000020',
           GC_GL_PREPAID       TYPE C LENGTH 10 VALUE '1429000080', "'1429000090',
           GC_GL_ADJ_ROU       TYPE C LENGTH 10 VALUE '5233009990',
           GC_SUCESS           TYPE ICON_D VALUE '@08@',
           GC_WARNNING         TYPE ICON_D VALUE '@09@',
           GC_ERROR            TYPE ICON_D VALUE '@0A@',
           GC_SAMPLE           TYPE C LENGTH 6 VALUE 'SAMPLE',
           GC_MIGRATION        TYPE C LENGTH 5 VALUE 'MIGRA',
           GC_100              TYPE C LENGTH 3 VALUE '100'.

CONSTANTS: GC_FILED_PAYDT TYPE C LENGTH 15  VALUE 'GS_HEADER-PAYDT',
           GC_BOT_BT_GEN  TYPE C LENGTH 6   VALUE 'BT_GEN',
           GC_BOT_BT_STA  TYPE C LENGTH 8   VALUE 'BT_START',
           GC_BOT_BT_END  TYPE C LENGTH 6   VALUE 'BT_END',
           GC_BOT_ANLN1   TYPE C LENGTH 15  VALUE 'GS_HEADER-ANLN1',
           GC_BOT_ANLN2   TYPE C LENGTH 15  VALUE 'GS_HEADER-ANLN2'.

CONSTANTS : BEGIN OF GC_STATUS,
              SAVE TYPE C LENGTH 3 VALUE 'SAV',
              DEL  TYPE C LENGTH 3 VALUE 'DEL',
              PAR  TYPE C LENGTH 3 VALUE 'PAR',
              COM  TYPE C LENGTH 3 VALUE 'COM',
              END  TYPE C LENGTH 3 VALUE 'END',
            END OF GC_STATUS.

CONSTANTS : BEGIN OF GC_MODE,
              CREATE TYPE C LENGTH 3 VALUE 'CRE',
              UPDATE TYPE C LENGTH 3 VALUE 'UPD',
              DELETE TYPE C LENGTH 3 VALUE 'DEL',
              REPORT TYPE C LENGTH 3 VALUE 'REP',
              REPOTU TYPE C LENGTH 3 VALUE 'REU',
            END OF GC_MODE.

CONSTANTS : BEGIN OF GC_BT_103,
              NEW    TYPE C LENGTH 6 VALUE 'BT_NEW',
              UPDATE TYPE C LENGTH 9 VALUE 'BT_UPDATE',
              DELETE TYPE C LENGTH 6 VALUE 'BT_DEL',
              REPORT TYPE C LENGTH 9 VALUE 'BT_REPORT',
            END OF GC_BT_103.

CONSTANTS : BEGIN OF GC_PARAID,
              CONTN TYPE C LENGTH 5 VALUE 'CONTN',
              LIFNR TYPE C LENGTH 5 VALUE 'LIFNR',
              RUNNG TYPE C LENGTH 5 VALUE 'RUNNG',
            END OF GC_PARAID.

CONSTANTS : GC_DEBIT  TYPE C LENGTH 2 VALUE '40',
            GC_CREDIT TYPE C LENGTH 2 VALUE '50',
            GC_ASSET  TYPE C LENGTH 2 VALUE '70'.
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
