*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0550
*  Creation Date      : 09.12.2024
*  Author             : B. CHIEWSRIKIJ (SDS)
*  Add-on ID          : N/A
*  Description        : Report for customer check
*  Purpose            : N/A
*  Copied from        : ZR_AR_CUSTOMER_CREDIT (ECC6)
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSFIR0550  MESSAGE-ID ZSDSFI01.
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES: KNA1,
        KNVP,
        KNVV,
        ADRC,
        KNKK,
        T077X,
        PA0182,
        PA0001,
        TVKBT,
        V_TVTW,
        V_TVKBZ_ASSIGN,
        V_TVKGR,
        TVKBZ.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPE-POOLS: SLIS.
TYPES: BEGIN OF TYP_OUT,
         KTOKD         TYPE KNA1-KTOKD,       "Account Group
         GRP_NAME(30)  TYPE C,                " Account Group name
         KUNNR         TYPE KNA1-KUNNR,       "Customer Code
         NAME_THA(255) TYPE C,                "Customer Name Thai
         NAME_ENG(255) TYPE C,                "Customer Name Eng.
         STREET        TYPE ADRC-STREET,     "Street
         STR_SUPPL3    TYPE ADRC-STR_SUPPL3, " street4
         LOCATION      TYPE ADRC-LOCATION, "Street 5
         CITY2         TYPE ADRC-CITY2,      "District
         CITY1         TYPE ADRC-CITY1,      "City
         POST_CODE1    TYPE ADRC-POST_CODE1, "post code
         TEL_NUMBER    TYPE ADRC-TEL_NUMBER, "telephone
         TELF1         TYPE KNA1-TELF1,       "Tel.
         TELF2         TYPE KNA1-TELF2,       " mobile phon
         TELFX         TYPE KNA1-TELFX,       " fax
         STCD1         TYPE KNA1-STCD1,       "Tax Number 1
         STCD2         TYPE KNA1-STCD2,       "Tax Number 2
         BRSCH         TYPE FITHA_PBUPL_D_T-J_1TPBUPL,       "Branch Code
         BRTXT         TYPE FITHA_PBUPL_D_T-DESCRIPTION,      "Branch Description
         WH_R1         TYPE KNBW-WITHT,                         " WHT R1
         WH_R1_TEXT    TYPE T059U-TEXT40,     "WHT R1 Desc.
         WH_R2         TYPE KNBW-WITHT,                         " WHT R2
         WH_R2_TEXT    TYPE T059U-TEXT40,     "WHT R2 Desc
         ERDAT         TYPE KNA1-ERDAT,       "Create date
         KVERM_1       TYPE KNB1-KVERM,       "Account memo for dealer contract date
         KVERM_2       TYPE KNB1-KVERM,       "Account memo for dealer contract date
         ZTERM         TYPE KNVV-ZTERM,       "Credit term for division 10
         KLIMK         TYPE KNKK-KLIMK,       "Credit limit
         KNKLI         TYPE KNKK-KNKLI,      "Customr Mothor
         CTLPC         TYPE KNKK-CTLPC,       "Risk category

         "Distribution Channel 00
         VTWEG_00      TYPE KNVV-VTWEG,       "Sale Area 10
         VTEXT_00      TYPE TVTWT-VTEXT,      "Distribution Chanal Detail
         VKBUR_00      TYPE KNVV-VKBUR,       "Sale office
         BEZEI_00      TYPE TVKBT-BEZEI,      "Sale Office detail
         VKGRP_00      TYPE KNVV-VKGRP,       "Sale Group Added Aphai 16.12.2014
         GBEZEI_00     TYPE TVGRT-BEZEI,      "Sale Group Description  Added Aphai 16.12.2014
         PERNR_00      TYPE KNVP-PERNR,       "Sale Code
         ALNAM_00      TYPE PA0182-ALNAM,     "Sale name thai
         ENAME_00      TYPE PA0001-ENAME,     "Sale name eng

         "Distribution Channel D0
         VTWEG_D0      TYPE KNVV-VTWEG,       "Sale Area 40
         VTEXT_D0      TYPE TVTWT-VTEXT,      "Distribution Chanal Detail
         VKBUR_D0      TYPE KNVV-VKBUR,       "Sale office 40
         BEZEI_D0      TYPE TVKBT-BEZEI,      "Sale Office detail 40
         VKGRP_D0      TYPE KNVV-VKGRP,       "Sale Group Added Aphai 16.12.2014
         GBEZEI_D0     TYPE TVGRT-BEZEI,      "Sale Group Description  Added Aphai 16.12.2014
         PERNR_D0      TYPE KNVP-PERNR,       "Sale Code
         ALNAM_D0      TYPE PA0182-ALNAM,     "Sale name thai
         ENAME_D0      TYPE PA0001-ENAME,     "Sale name eng

         "Distribution Channel S0
         VTWEG_S0      TYPE KNVV-VTWEG,       "Sale Area 40
         VTEXT_S0      TYPE TVTWT-VTEXT,      "Distribution Chanal Detail
         VKBUR_S0      TYPE KNVV-VKBUR,       "Sale office 40
         BEZEI_S0      TYPE TVKBT-BEZEI,      "Sale Office detail 40
         VKGRP_S0      TYPE KNVV-VKGRP,       "Sale Group Added Aphai 16.12.2014
         GBEZEI_S0     TYPE TVGRT-BEZEI,      "Sale Group Description  Added Aphai 16.12.2014
         PERNR_S0      TYPE KNVP-PERNR,       "Sale Code
         ALNAM_S0      TYPE PA0182-ALNAM,     "Sale name thai
         ENAME_S0      TYPE PA0001-ENAME,     "Sale name eng


         FAKSD         TYPE KNA1-FAKSD,       "Central billing block
         NODEL         TYPE KNA1-NODEL,       "Central del.block
         LOEVM         TYPE KNA1-LOEVM,       "Central deletion flag
         LIFSD         TYPE KNA1-LIFSD,       "Central delivery block
         AUFSD         TYPE KNA1-AUFSD,       "Central order block
         SPERR         TYPE KNA1-SPERR,       "Central posting block
         CASSD         TYPE KNA1-CASSD,       "Central sale block
         LZONE         TYPE KNA1-LZONE,        "Transportation Zone
         VSBED         TYPE KNVV-VSBED,       "Shipping Condition
         VTEXT         TYPE TVSBT-VTEXT,    "Shipping Condition Description
         ZTERM_40      TYPE KNVV-ZTERM,       "Credit term for division 40
         FAKSD_D       TYPE CDHDR-UDATE,     "Central billing block Date
         FAKSD_T       TYPE CDHDR-UTIME,     "Central billing block Time
         FAKSD_U       TYPE CDHDR-USERNAME,     "Central billing block user update
         LIFSD_D       TYPE CDHDR-UDATE,     "Central delivery block Date
         LIFSD_T       TYPE CDHDR-UTIME,     "Central delivery block Time
         LIFSD_U       TYPE CDHDR-USERNAME,     "Central delivery block user update
         AUFSD_D       TYPE CDHDR-UDATE,     "Central order block Date
         AUFSD_T       TYPE CDHDR-UTIME,     "Central order block Time
         AUFSD_U       TYPE CDHDR-USERNAME,     "Central order block user update
         SPERR_D       TYPE CDHDR-UDATE,     "Central posting block Date
         SPERR_T       TYPE CDHDR-UTIME,     "Central posting block Time
         SPERR_U       TYPE CDHDR-USERNAME,     "Central posting block user update
         CASSD_D       TYPE CDHDR-UDATE,     "Central sale block Date
         CASSD_T       TYPE CDHDR-UTIME,     "Central sale block Time
         CASSD_U       TYPE CDHDR-USERNAME,     "Central sale block user update
         KVERM         TYPE KNB1-KVERM,       "Reson for block
         STCD3         TYPE KNA1-STCD3,       "Tax Number 2
         J_1KFTIND     TYPE KNA1-J_1KFTIND,   "Branch No.
         USERNAME      TYPE CDHDR-USERNAME,
         UDATE         TYPE CDHDR-UDATE,
         UTIME         TYPE CDHDR-UTIME,
         NAME1_C1      TYPE KNVK-NAME1,
         VTEXT_C1      TYPE TSABT-VTEXT,
         NAME1_C2      TYPE KNVK-NAME1,
         VTEXT_C2      TYPE TSABT-VTEXT,
         NAME1_C3      TYPE KNVK-NAME1,
         VTEXT_C3      TYPE TSABT-VTEXT,
         DISPE         TYPE KBETR_KOND,
         KONWA         TYPE KONWA,
         KONDA         TYPE KNVV-KONDA,
         KONDA_D       TYPE T188T-VTEXT,
       END OF TYP_OUT.

TYPES: BEGIN OF TYP_KNA1,
         KUNNR      TYPE KNA1-KUNNR,      "Customer Code
         NAME_THA_1 TYPE KNA1-NAME1,      "Customer name thai
         NAME_THA_2 TYPE KNA1-NAME2,      "Customer name thai
         NAME_ENG_1 TYPE ADRC-NAME1,      "Customer name eng
         NAME_ENG_2 TYPE ADRC-NAME2,      "Customer name eng
         STRAS      TYPE KNA1-STRAS,      "Address street
         ORT02      TYPE KNA1-ORT02,     "Address
         ORT01      TYPE KNA1-ORT01,     "city
         PSTLZ      TYPE KNA1-PSTLZ,     "Post code
         TELF1      TYPE KNA1-TELF1,     "Tel.
         ADRNR      TYPE KNA1-ADRNR,     "Address number
         KTOKD      TYPE KNA1-KTOKD,     "Account Group
         FAKSD      TYPE KNA1-FAKSD,     "Central billing block
         NODEL      TYPE KNA1-NODEL,     "Central del.block
         LOEVM      TYPE KNA1-LOEVM,     "Central deletion flag
         LIFSD      TYPE KNA1-LIFSD,     "Central delivery block
         AUFSD      TYPE KNA1-AUFSD,     "Central order block
         SPERR      TYPE KNA1-SPERR,     "Central posting block
         CASSD      TYPE KNA1-CASSD,     "Central sale block
         STCD1      TYPE KNA1-STCD1,       "Tax Number 1
         STCD2      TYPE KNA1-STCD2,       "Tax Number 2
         BRSCH      TYPE T016T-BRSCH,      "Industry
         STCD3      TYPE KNA1-STCD3,       "Tax Number 2
         J_1KFTIND  TYPE KNA1-J_1KFTIND,   "Branch No.
         ERDAT      TYPE KNA1-ERDAT,       "Create date
         KVERM      TYPE KNB1-KVERM,       "Account memo for dealer contract date
         TELF2      TYPE KNA1-TELF2,       " mobile phon
         TELFX      TYPE KNA1-TELFX,       " fax
         LZONE      TYPE KNA1-LZONE,        "Transportation Zone
       END OF TYP_KNA1.

TYPES: BEGIN OF TYP_T077X,
         KTOKD TYPE KNA1-KTOKD,    "Account Group
         TXT30 TYPE T077X-TXT30,   "Group name
       END OF TYP_T077X.

TYPES: BEGIN OF TYP_KNVV,
         KUNNR   TYPE KNA1-KUNNR,    "Customer Code
         VTWEG   TYPE KNVV-VTWEG,    "Distribution Chanal
         VKGRP   TYPE KNVV-VKGRP,    "Sale Group
         BEZEI_G TYPE TVGRT-BEZEI,   "Sale group detail
         VKBUR   TYPE KNVV-VKBUR,    "Sale office
         BEZEI_O TYPE TVKBT-BEZEI,   "Sale Office detail
         ZTERM   TYPE KNVV-ZTERM,    "Credit term
         VSBED   TYPE KNVV-VSBED,       "Shipping Condition
         VTEXT   TYPE TVSBT-VTEXT,    "Shipping Condition Description
       END OF TYP_KNVV.

TYPES: BEGIN OF TYP_KNKK,
         KUNNR TYPE KNA1-KUNNR,    "Customer Code
         KLIMK TYPE KNKK-KLIMK,    "Credit limit
         KNKLI TYPE KNKK-KNKLI,   "Customr Mothor
         CTLPC TYPE KNKK-CTLPC,    "Risk category
         RTEXT TYPE T691T-RTEXT,   "Rist Detail

       END OF TYP_KNKK.

TYPES: BEGIN OF TYP_KNVP,
         KUNNR TYPE KNA1-KUNNR,    "Cutomer Code
         VTWEG TYPE KNVP-VTWEG,    "Distribution Chanal
         PARVW TYPE KNVP-PARVW,    "T
         PERNR TYPE KNVP-PERNR,    "Sale Code
         ALNAM TYPE PA0182-ALNAM,  "Sale name thai
         ENAME TYPE PA0001-ENAME,  "Sale name eng

       END OF TYP_KNVP.

TYPES: BEGIN OF TYP_TVTWT,
         VTWEG TYPE TVTWT-VTWEG,    "Distribution Chanal
         VTEXT TYPE TVTWT-VTEXT,  "Distribution Chanal Detail
         VKBUR TYPE TVKBZ-VKBUR,   "Sale Office
       END OF TYP_TVTWT.

TYPES: BEGIN OF TYP_T691T,
         CTLPC TYPE KNKK-CTLPC,    "Risk category
         RTEXT TYPE T691T-RTEXT,   "Rist Detail

       END OF TYP_T691T.

TYPES: BEGIN OF TYP_T016T,
         BRSCH TYPE KNA1-BRSCH,    "Industry
         BRTXT TYPE T691T-RTEXT,   "Industry Description
       END OF TYP_T016T.

TYPES: BEGIN OF TYP_T059U,
         KUNNR  TYPE KNA1-KUNNR,    "Cutomer Code
         WITHT  TYPE KNBW-WITHT,    "WTH Code
         TEXT40 TYPE T059U-TEXT40,  "WTH Description
       END OF TYP_T059U.

TYPES: BEGIN OF TYP_CDHDR,
         OBJECTCLAS TYPE CDHDR-OBJECTCLAS,        "Change doc. object
         OBJECTID   TYPE CDHDR-OBJECTID,          "Object value
         CHANGENR   TYPE CDHDR-CHANGENR,          "Document number
         USERNAME   TYPE CDHDR-USERNAME,          "Name
         UDATE      TYPE CDHDR-UDATE,             "Date
         UTIME      TYPE CDHDR-UTIME,             "Time
         TCODE      TYPE CDHDR-TCODE,             "Transaction code
       END OF TYP_CDHDR.

TYPES: BEGIN OF TYP_CDPOS,
         OBJECTCLAS TYPE CDPOS-OBJECTCLAS,        "Change doc. object
         OBJECTID   TYPE CDPOS-OBJECTID,          "Object value
         CHANGENR   TYPE CDPOS-CHANGENR,          "Document number
         TABNAME    TYPE CDPOS-TABNAME,           "Table Name
         FNAME      TYPE CDPOS-FNAME,             "Field Name
         VALUE_NEW  TYPE CDPOS-VALUE_NEW,         "New value
         VALUE_OLD  TYPE CDPOS-VALUE_OLD,         "Old value
       END OF TYP_CDPOS.

TYPES : BEGIN OF TY_OUTPUT,
          KUNNR      TYPE KNA1-KUNNR,
          VTWEG      TYPE KNVV-VTWEG,
          VKBUR      TYPE KNVV-VKBUR,
          VKGRP      TYPE KNVV-VKGRP,
          NAME1_TH   TYPE KNA1-NAME1,
          NAME1_EN   TYPE KNA1-NAME1,
          STREET     TYPE ADRC-STREET, "HOUSE NO
          CITY2      TYPE ADRC-CITY2,
          CITY1      TYPE ADRC-CITY1,
          POST_CODE1 TYPE ADRC-POST_CODE1,
          ZTERM      TYPE KNVV-ZTERM,
          KNKLI      TYPE KNKK-KNKLI,
        END OF TY_OUTPUT.

TYPES: BEGIN OF TYP_ADRC,
         ADDRNUMBER TYPE ADRC-ADDRNUMBER,      "Address Number
         NAME1      TYPE ADRC-NAME1,      "Name
         NAME2      TYPE ADRC-NAME2,      "Name2
         STREET     TYPE ADRC-STREET,     "Street
         STR_SUPPL3 TYPE ADRC-STR_SUPPL3, " street4
         LOCATION   TYPE ADRC-LOCATION, "Street 5
         CITY2      TYPE ADRC-CITY2,      "District
         CITY1      TYPE ADRC-CITY1,      "City
         POST_CODE1 TYPE ADRC-POST_CODE1, "post code
         TEL_NUMBER TYPE ADRC-TEL_NUMBER, "telephone
         NATION     TYPE ADRC-NATION,                     "Name2
       END OF TYP_ADRC.

*TYPES: BEGIN OF TYP_IDOC.
*         INCLUDE STRUCTURE ZSFS040.
*TYPES: END OF TYP_IDOC.

TYPES: BEGIN OF TYP_BRANCH,
         KUNNR       TYPE FITHA_PBUPL_D_T-KUNNR,
         J_1TPBUPL   TYPE FITHA_PBUPL_D_T-J_1TPBUPL,
         DESCRIPTION TYPE FITHA_PBUPL_D_T-DESCRIPTION,
       END OF TYP_BRANCH,

       BEGIN OF TYP_UKMBP_CMS_SGM,
         PARTNER      TYPE UKMBP_CMS_SGM-PARTNER,
         CREDIT_SGMNT TYPE UKMBP_CMS_SGM-CREDIT_SGMNT,
         CREDIT_LIMIT TYPE UKMBP_CMS_SGM-CREDIT_LIMIT,
       END OF TYP_UKMBP_CMS_SGM.
*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : IT_OUTPUT  TYPE STANDARD TABLE OF TY_OUTPUT,
       WA_OUTPUT  TYPE TY_OUTPUT,
       LV_ROWS    TYPE I,
       LV_SRWS(6) TYPE C.

DATA: GR_TABLE       TYPE REF TO CL_SALV_TABLE,
      GR_DISPLAY     TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
      GR_COLUMNS     TYPE REF TO CL_SALV_COLUMNS_TABLE,
      GR_COLUMN      TYPE REF TO CL_SALV_COLUMN,
      GR_AGGRS       TYPE REF TO CL_SALV_AGGREGATIONS,
      GR_SORT        TYPE REF TO CL_SALV_SORTS,
      GR_SORT_COLUMN TYPE REF TO CL_SALV_SORT, "column sort
      GR_EVENTS      TYPE REF TO CL_SALV_EVENTS_TABLE,
      GR_FUNCTIONS   TYPE REF TO CL_SALV_FUNCTIONS_LIST,
      GR_LAYOUT      TYPE REF TO CL_SALV_LAYOUT,
      G_VARIANT      TYPE SLIS_VARI,
      GR_KEY         TYPE SALV_S_LAYOUT_KEY,
      GR_SELECTIONS  TYPE REF TO CL_SALV_SELECTIONS,
      WA_ALV         TYPE TY_OUTPUT.

DATA: GT_ADRC TYPE STANDARD TABLE OF TYP_ADRC,
      WA_ADRC TYPE TYP_ADRC,
      GW_ADRC TYPE TYP_ADRC.

*DATA: GT_IDOC          TYPE STANDARD TABLE OF TYP_IDOC,
*      GWA_IDOC         TYPE TYP_IDOC,
*      GWA_IDOC_CONTROL TYPE EDIDC,
*      GT_EDIDC         TYPE STANDARD TABLE OF EDIDC,
*      GWA_EDIDC        LIKE LINE OF GT_EDIDC,
*      GT_EDIDD         TYPE STANDARD TABLE OF EDIDD,
*      GWA_EDIDD        LIKE LINE OF GT_EDIDD,
*      GWA_PATH         TYPE ZSFS999,
*      GWA_EDP13        TYPE EDP13.

DATA : P_PATH   LIKE RLGRAP-FILENAME,
       DF_DAT   TYPE DATS,
       V_PH2(7) TYPE C.

* Data for ALV variant
DATA  GV_REPNAME          LIKE SY-REPID.
DATA  GV_X_VARIANT        LIKE DISVARIANT.
DATA  GV_EXIT(1)          TYPE C.
DATA  GV_SAVE(1)          TYPE C.
DATA  GV_VARIANT          LIKE DISVARIANT.

FIELD-SYMBOLS : <FS_OUT> TYPE TY_OUTPUT.

* The inputs that need to be passed to the REUSE_ALV function module
DATA: GT_LIST_FIELDCAT TYPE LVC_T_FCAT,      "Field Catalog for List Viewer Control
      GT_EXIT(1)       TYPE C,
      GT_VARIANT       TYPE DISVARIANT,
      GX_VARIANT       TYPE DISVARIANT,
      GS_PRINT         TYPE LVC_S_PRNT.

DATA: GT_ITAB        TYPE STANDARD TABLE OF TYP_OUT,
      GT_KNA1        TYPE STANDARD TABLE OF TYP_KNA1,
      GT_T077X       TYPE STANDARD TABLE OF TYP_T077X,
      GT_KNVV        TYPE STANDARD TABLE OF TYP_KNVV,
      GT_KNKK        TYPE STANDARD TABLE OF TYP_KNKK,
      GT_KNVP        TYPE STANDARD TABLE OF TYP_KNVP,
      GT_TVTWT       TYPE STANDARD TABLE OF TYP_TVTWT,
      GT_T016T       TYPE STANDARD TABLE OF TYP_T016T,
      GT_T059U       TYPE STANDARD TABLE OF TYP_T059U,
      GT_CDHDR       TYPE STANDARD TABLE OF TYP_CDHDR, "Add by Wantanee 20130605  T41K915444
      GT_CDPOS       TYPE STANDARD TABLE OF TYP_CDPOS, "Add by Wantanee 20130605  T41K915444
      GT_LAST_CHANGE TYPE STANDARD TABLE OF TYP_CDHDR. "Add by Jakarin 20150926

DATA: GW_ITAB        TYPE TYP_OUT,
      GW_KNA1        TYPE TYP_KNA1,
      GW_T077X       TYPE TYP_T077X,
      GW_KNVV        TYPE TYP_KNVV,
      GW_KNKK        TYPE TYP_KNKK,
      GW_KNVP        TYPE TYP_KNVP,
      GW_TVTWT       TYPE TYP_TVTWT,
      GW_T016T       TYPE TYP_T016T,
      GW_T059U       TYPE TYP_T059U,
      GW_CDHDR       TYPE TYP_CDHDR, "Add by Wantanee 20130605  T41K915444
      GW_CDPOS       TYPE TYP_CDPOS, "Add by Wantanee 20130605  T41K915444
      GW_LAST_CHANGE TYPE TYP_CDHDR. "Add by Jakarin 20150926

DATA: WA_ITAB  TYPE TYP_OUT,
      WA_KNA1  TYPE TYP_KNA1,
      WA_T077X TYPE TYP_T077X,
      WA_KNVV  TYPE TYP_KNVV,
      WA_KNKK  TYPE TYP_KNKK,
      WA_KNVP  TYPE TYP_KNVP,
      WA_TVTWT TYPE TYP_TVTWT,
      WA_T016T TYPE TYP_T016T,
      WA_T059U TYPE TYP_T059U,
      WA_CDHDR TYPE TYP_CDHDR, "Add by Wantanee 20130605  T41K915444
      WA_CDPOS TYPE TYP_CDPOS. "Add by Wantanee 20130605  T41K915444

DATA: GRID_MAIN      TYPE REF TO CL_GUI_ALV_GRID,
      CONTAINER_MAIN TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_VARIANT     LIKE DISVARIANT.

DATA : V_POS TYPE I .
DATA : GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
       GT_LAYOUT   TYPE SLIS_LAYOUT_ALV,
       GT_EVENTS   TYPE SLIS_T_EVENT.
RANGES : GR_TCODE FOR CDHDR-TCODE.

DATA: GT_BRANCH        TYPE STANDARD TABLE OF TYP_BRANCH,
      GT_UKMBP_CMS_SGM TYPE STANDARD TABLE OF TYP_UKMBP_CMS_SGM.
*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS : GC_REPID       TYPE REPID         VALUE 'ZSDSFIR0550',
            GC_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE'.
DATA : BASE_DATE TYPE CHAR10 VALUE '31.12.2013'.
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

*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
*SELECTION-SCREEN BEGIN OF BLOCK s01 WITH FRAME TITLE TEXT-001.
* PARAMETERS:
* SELECT-OPTIONS:
*SELECTION-SCREEN END OF BLOCK s01.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-H01.
  PARAMETERS:     P_BUKRS TYPE BUKRS   OBLIGATORY DEFAULT '1000'.   "Company Code

  SELECT-OPTIONS:
                  S_KUNNR FOR KNA1-KUNNR,     "Customer Code
                  S_ERDAT FOR KNA1-ERDAT,     "Create date
                  S_KTOKD FOR KNA1-KTOKD.     "Account Group

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-H04.
  PARAMETERS : P_PDAT TYPE SY-DATUM DEFAULT SY-DATUM.
SELECTION-SCREEN END OF BLOCK B4.

*SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-H02.
*
*  PARAMETERS: P_IDOC   AS CHECKBOX TYPE JEST-INACT USER-COMMAND UCOMM,   "CREATE IDOC
*              P_IDOC_W AS CHECKBOX TYPE JEST-INACT USER-COMMAND UCOMM.   "CREATE IDOC
*
*SELECTION-SCREEN END OF BLOCK B2.
*
*SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-H03.
*  SELECT-OPTIONS : S_VTWEG FOR ZTFIAR_CREDIT_LI-VTWEG MODIF ID B3.
*SELECTION-SCREEN END OF BLOCK B3.


*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.

*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
*AT SELECTION-SCREEN OUTPUT.
*AT SELECTION-SCREEN ON HELP-REQUEST FOR <field>
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR <field>
*AT SELECTION-SCREEN ON <field>
*AT SELECTION-SCREEN.

*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM F_GET_DATA.
  PERFORM F_MAP_DATA.
  IF GT_ITAB[] IS NOT INITIAL.
*    PERFORM F_GET_ADDTIONAL_DATA.
  ENDIF.

*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
  IF NOT GT_ITAB[] IS INITIAL.
    PERFORM DISPLAY_REPROT.
  ELSE.
    MESSAGE I004.
    EXIT.
  ENDIF.

*-----------------------------------------------------------------------
* T O P - O F – P A G E
*-----------------------------------------------------------------------
*TOP-OF-PAGE.

*-----------------------------------------------------------------------
* AT LINE-SELECTION
*-----------------------------------------------------------------------
*AT LINE-SELECTION.

*-----------------------------------------------------------------------
* F O R M S
*-----------------------------------------------------------------------
FORM F_GET_DATA .

  SELECT A~KUNNR A~NAME1 A~NAME2 B~NAME1 B~NAME2 A~STRAS
         A~ORT02
         A~ORT01 A~PSTLZ A~TELF1 A~ADRNR A~KTOKD A~FAKSD
         A~NODEL A~LOEVM A~LIFSD A~AUFSD A~SPERR A~CASSD
         A~STCD1 A~STCD2 A~BRSCH
         A~STCD3 A~J_1KFTIND    "Add by Wantanee 20131209
         A~ERDAT KN~KVERM A~TELF2
         A~TELFX A~LZONE
  INTO TABLE GT_KNA1
  FROM KNA1 AS A LEFT OUTER JOIN ADRC AS B
                 ON ( A~ADRNR = B~ADDRNUMBER
                 AND B~NATION = 'I' )
                 LEFT OUTER JOIN KNB1 AS KN
                 ON ( A~KUNNR = KN~KUNNR )
  WHERE A~KUNNR IN S_KUNNR
    AND A~ERDAT IN S_ERDAT
    AND A~KTOKD IN S_KTOKD.

  PERFORM F_GET_LAST_CHANGED. "Add by Jakarin 20150926

  SELECT KTOKD TXT30
  INTO TABLE GT_T077X
  FROM T077X
  WHERE SPRAS = 'E'.

  IF NOT GT_KNA1 IS INITIAL.

    SELECT   A~KUNNR A~VTWEG A~VKGRP B~BEZEI A~VKBUR TV~BEZEI A~ZTERM A~VSBED C~VTEXT
    INTO TABLE GT_KNVV
    FROM KNVV AS A LEFT OUTER JOIN TVGRT AS B
                   ON ( A~VKGRP = B~VKGRP
                   AND B~SPRAS = 'E' )
                   LEFT OUTER JOIN TVKBT AS TV
                   ON ( A~VKBUR = TV~VKBUR
                   AND TV~SPRAS = 'E' )
                   INNER JOIN TVSBT AS C ON ( C~VSBED = A~VSBED )
    FOR ALL ENTRIES IN GT_KNA1
    WHERE A~KUNNR EQ GT_KNA1-KUNNR.

    SELECT  KUNNR KLIMK KNKLI CTLPC
    INTO TABLE GT_KNKK
    FROM KNKK
    FOR ALL ENTRIES IN GT_KNA1
    WHERE KUNNR EQ GT_KNA1-KUNNR.

*            SELECT  a~kunnr a~klimk a~knkli a~ctlpc b~rtext
*            INTO TABLE gt_knkk
*            FROM knkk AS a INNER JOIN t691t AS b
*                           ON ( a~ctlpc = b~ctlpc
*                           AND b~spras = 'E' )
*            FOR ALL ENTRIES IN gt_kna1
*            WHERE a~kunnr EQ gt_kna1-kunnr.

    SELECT A~KUNNR A~VTWEG A~PARVW A~PERNR B~ALNAM PA~ENAME
    INTO TABLE GT_KNVP
    FROM KNVP AS A INNER JOIN PA0182 AS B
                   ON A~PERNR = B~PERNR
                   INNER JOIN PA0001 AS PA
                   ON A~PERNR = PA~PERNR
    FOR ALL ENTRIES IN GT_KNA1
    WHERE A~KUNNR EQ GT_KNA1-KUNNR
      AND A~PARVW = 'VE'.

    SELECT A~KUNNR A~WITHT B~TEXT40
    INTO TABLE GT_T059U
    FROM KNBW AS A INNER JOIN T059U AS B
                   ON ( A~WITHT = B~WITHT )
    FOR ALL ENTRIES IN GT_KNA1
    WHERE A~KUNNR = GT_KNA1-KUNNR
      AND B~SPRAS = 'E'.


  ENDIF.



  SELECT A~VTWEG A~VTEXT B~VKBUR
  INTO TABLE GT_TVTWT
  FROM TVTWT AS A INNER JOIN TVKBZ AS B
                  ON A~VTWEG = B~VTWEG
  WHERE A~SPRAS = 'E'
  GROUP BY A~VTWEG A~VTEXT B~VKBUR.
  IF SY-SUBRC = 0.
    SORT GT_TVTWT BY VTWEG VKBUR.
  ENDIF.

  SELECT BRSCH BRTXT
  INTO TABLE GT_T016T
  FROM T016T
  WHERE SPRAS = 'E'.

  "Branch Code (customer)
  IF GT_KNA1 IS NOT INITIAL.
    SELECT
      KUNNR,
      J_1TPBUPL,
      DESCRIPTION
    FROM FITHA_PBUPL_D_T
    INTO TABLE @GT_BRANCH
    FOR ALL ENTRIES IN @GT_KNA1
    WHERE KUNNR = @GT_KNA1-KUNNR
      AND SPRAS = 'E'.
    IF SY-SUBRC = 0.
      SORT GT_BRANCH BY KUNNR.
    ENDIF.

    SELECT
      PARTNER,
      CREDIT_SGMNT,
      CREDIT_LIMIT
      FROM UKMBP_CMS_SGM
      INTO TABLE @GT_UKMBP_CMS_SGM
      FOR ALL ENTRIES IN @GT_KNA1
      WHERE PARTNER      = @GT_KNA1-KUNNR
        AND CREDIT_SGMNT = '1000'.
    IF SY-SUBRC = 0.
      SORT GT_UKMBP_CMS_SGM BY PARTNER.
    ENDIF.
  ENDIF.


ENDFORM.   "Get data.

*&---------------------------------------------------------------------*
*&      Form  MAP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_MAP_DATA.

  DATA: LV_NAME1(40) TYPE C,
        LV_NAME2(40) TYPE C.

  SORT GT_KNA1.

  LOOP AT GT_KNA1 INTO WA_KNA1.
    CLEAR : WA_ITAB,GW_LAST_CHANGE.
    WA_ITAB-KTOKD = WA_KNA1-KTOKD.
    READ TABLE GT_T077X INTO WA_T077X WITH KEY KTOKD = WA_KNA1-KTOKD.
    IF SY-SUBRC IS INITIAL.
      WA_ITAB-GRP_NAME = WA_T077X-TXT30.
    ENDIF.

    READ TABLE GT_LAST_CHANGE INTO GW_LAST_CHANGE
    WITH KEY OBJECTID = WA_KNA1-KUNNR.
    IF SY-SUBRC = 0.
      WA_ITAB-USERNAME = GW_LAST_CHANGE-USERNAME.
      WA_ITAB-UDATE    = GW_LAST_CHANGE-UDATE.
      WA_ITAB-UTIME    = GW_LAST_CHANGE-UTIME.
    ENDIF.

    WA_ITAB-KUNNR = WA_KNA1-KUNNR.
    CONCATENATE WA_KNA1-NAME_THA_1 ' ' WA_KNA1-NAME_THA_2 INTO WA_ITAB-NAME_THA.
    CONCATENATE WA_KNA1-NAME_ENG_1 ' ' WA_KNA1-NAME_ENG_2 INTO WA_ITAB-NAME_ENG.
*    wa_itab-stras = wa_kna1-stras.
*    wa_itab-ort02 = wa_kna1-ort02.
*    wa_itab-ort01 = wa_kna1-ort01.
*    wa_itab-pstlz = wa_kna1-pstlz.
    WA_ITAB-TELF1 = WA_KNA1-TELF1.
    WA_ITAB-TELF2 = WA_KNA1-TELF2.
    WA_ITAB-TELFX = WA_KNA1-TELFX.
    WA_ITAB-STCD1 = WA_KNA1-STCD1.
    WA_ITAB-STCD2 = WA_KNA1-STCD2.
    WA_ITAB-LZONE = WA_KNA1-LZONE.
    WA_ITAB-STCD3 = WA_KNA1-STCD3.   "Add by Wantanee 20131209
    WA_ITAB-J_1KFTIND = WA_KNA1-J_1KFTIND. "Add by Wantanee 20131209

    PERFORM GET_CUSTOMER USING WA_KNA1-ADRNR CHANGING WA_ITAB-STREET WA_ITAB-STR_SUPPL3 WA_ITAB-LOCATION
                                                      WA_ITAB-CITY2 WA_ITAB-CITY1 WA_ITAB-POST_CODE1  "Add by Wantanee 20190111
                                                      WA_ITAB-TEL_NUMBER."CH10 Add by Wantanee 20211213



*    READ TABLE GT_T016T INTO WA_T016T WITH KEY BRSCH = WA_KNA1-BRSCH.
*    IF SY-SUBRC IS INITIAL.
*      WA_ITAB-BRSCH = WA_KNA1-BRSCH. "CH10 Add by Wantanee 20211213
*      WA_ITAB-BRTXT = WA_T016T-BRTXT.
*    ENDIF.
    READ TABLE GT_BRANCH INTO DATA(LS_BRANCH)
                         WITH KEY KUNNR = WA_KNA1-KUNNR
                         BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_ITAB-BRSCH = LS_BRANCH-J_1TPBUPL.
      WA_ITAB-BRTXT = LS_BRANCH-DESCRIPTION.
    ENDIF.

    LOOP AT GT_T059U INTO WA_T059U WHERE KUNNR = WA_KNA1-KUNNR.
      IF WA_T059U-WITHT = 'R1'.
        WA_ITAB-WH_R1 = WA_T059U-WITHT.
        WA_ITAB-WH_R1_TEXT = WA_T059U-TEXT40.

      ENDIF.
      IF WA_T059U-WITHT = 'R2'.
        WA_ITAB-WH_R2 = WA_T059U-WITHT.
        WA_ITAB-WH_R2_TEXT = WA_T059U-TEXT40.
      ENDIF.
    ENDLOOP.
    WA_ITAB-ERDAT = WA_KNA1-ERDAT.
    SPLIT WA_KNA1-KVERM AT '-' INTO WA_ITAB-KVERM_1 WA_ITAB-KVERM_2.
    LOOP AT GT_KNVV INTO WA_KNVV WHERE KUNNR = WA_KNA1-KUNNR.

      "Distribution Channel 00
      IF WA_KNVV-VTWEG EQ '00'.
        WA_ITAB-ZTERM = WA_KNVV-ZTERM.
        READ TABLE GT_TVTWT INTO WA_TVTWT WITH KEY  VTWEG = WA_KNVV-VTWEG
                                                    VKBUR = WA_KNVV-VKBUR
                                                    BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          WA_ITAB-VTWEG_00 = WA_TVTWT-VTWEG.
          WA_ITAB-VTEXT_00 = WA_TVTWT-VTEXT.
          WA_ITAB-VKBUR_00 = WA_KNVV-VKBUR.
          WA_ITAB-BEZEI_00 = WA_KNVV-BEZEI_O.
          WA_ITAB-VKGRP_00 = WA_KNVV-VKGRP.
          WA_ITAB-GBEZEI_00 = WA_KNVV-BEZEI_G.
        ENDIF.
      ENDIF.

      "Distribution Channel D0
      IF WA_KNVV-VTWEG EQ 'D0'.
        WA_ITAB-ZTERM_40 = WA_KNVV-ZTERM.
        READ TABLE GT_TVTWT INTO WA_TVTWT WITH KEY  VTWEG = WA_KNVV-VTWEG
                                                    VKBUR = WA_KNVV-VKBUR
                                                    BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          WA_ITAB-VTWEG_D0 = WA_TVTWT-VTWEG.
          WA_ITAB-VTEXT_D0 = WA_TVTWT-VTEXT.
          WA_ITAB-VKBUR_D0 = WA_KNVV-VKBUR.
          WA_ITAB-BEZEI_D0 = WA_KNVV-BEZEI_O.
          WA_ITAB-VKGRP_D0 = WA_KNVV-VKGRP.
          WA_ITAB-GBEZEI_D0 = WA_KNVV-BEZEI_G.
        ENDIF.
      ENDIF.

      "Distribution Channel S0
      IF WA_KNVV-VTWEG EQ 'S0'.
        WA_ITAB-ZTERM_40 = WA_KNVV-ZTERM.
        READ TABLE GT_TVTWT INTO WA_TVTWT WITH KEY  VTWEG = WA_KNVV-VTWEG
                                                    VKBUR = WA_KNVV-VKBUR
                                                    BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          WA_ITAB-VTWEG_S0 = WA_TVTWT-VTWEG.
          WA_ITAB-VTEXT_S0 = WA_TVTWT-VTEXT.
          WA_ITAB-VKBUR_S0 = WA_KNVV-VKBUR.
          WA_ITAB-BEZEI_S0 = WA_KNVV-BEZEI_O.
          WA_ITAB-VKGRP_S0 = WA_KNVV-VKGRP.
          WA_ITAB-GBEZEI_S0 = WA_KNVV-BEZEI_G.
        ENDIF.
      ENDIF.
    ENDLOOP.
    READ TABLE GT_KNKK INTO WA_KNKK WITH KEY KUNNR = WA_KNA1-KUNNR.
    IF SY-SUBRC IS INITIAL.
*      WA_ITAB-KLIMK = WA_KNKK-KLIMK.
      IF WA_KNKK-KUNNR NE WA_KNKK-KNKLI.
        WA_ITAB-KNKLI = WA_KNKK-KNKLI.
      ENDIF.
      WA_ITAB-CTLPC = WA_KNKK-CTLPC.
    ENDIF.

    READ TABLE GT_UKMBP_CMS_SGM INTO DATA(LS_UKMBP_CMS_SGM)
                                WITH KEY PARTNER = WA_KNA1-KUNNR
                                BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_ITAB-KLIMK = LS_UKMBP_CMS_SGM-CREDIT_LIMIT.
    ENDIF.

    LOOP AT GT_KNVP INTO WA_KNVP WHERE KUNNR = WA_KNA1-KUNNR.

      IF WA_KNVP-VTWEG EQ '00'.
        WA_ITAB-PERNR_00 = WA_KNVP-PERNR.
        WA_ITAB-ALNAM_00 = WA_KNVP-ALNAM.
        SPLIT WA_KNVP-ENAME AT ' ' INTO LV_NAME1 LV_NAME2.
        CONCATENATE LV_NAME2  LV_NAME1 INTO WA_ITAB-ENAME_00 SEPARATED BY  SPACE.
      ENDIF.

      IF WA_KNVP-VTWEG EQ 'D0'.
        WA_ITAB-PERNR_D0 = WA_KNVP-PERNR.
        WA_ITAB-ALNAM_D0 = WA_KNVP-ALNAM.
        SPLIT WA_KNVP-ENAME AT ' ' INTO LV_NAME1 LV_NAME2.
        CONCATENATE LV_NAME2  LV_NAME1 INTO WA_ITAB-ENAME_D0 SEPARATED BY  SPACE.
      ENDIF.

      IF WA_KNVP-VTWEG EQ 'S0'.
        WA_ITAB-PERNR_S0 = WA_KNVP-PERNR.
        WA_ITAB-ALNAM_S0 = WA_KNVP-ALNAM.
        SPLIT WA_KNVP-ENAME AT ' ' INTO LV_NAME1 LV_NAME2.
        CONCATENATE LV_NAME2  LV_NAME1 INTO WA_ITAB-ENAME_S0 SEPARATED BY  SPACE.
      ENDIF.

    ENDLOOP.
    WA_ITAB-FAKSD = WA_KNA1-FAKSD.
    WA_ITAB-NODEL = WA_KNA1-NODEL.
    WA_ITAB-LOEVM = WA_KNA1-LOEVM.
    WA_ITAB-LIFSD = WA_KNA1-LIFSD.
    WA_ITAB-AUFSD = WA_KNA1-AUFSD.
    WA_ITAB-SPERR = WA_KNA1-SPERR.
    WA_ITAB-CASSD = WA_KNA1-CASSD.

    WA_ITAB-VSBED = WA_KNVV-VSBED.
    WA_ITAB-VTEXT = WA_KNVV-VTEXT.

    "Add by Wantanee 20130605  T41K915444
    IF ( WA_ITAB-FAKSD NE '' ) OR ( WA_ITAB-LIFSD NE '' ) OR
       ( WA_ITAB-AUFSD NE '' ) OR ( WA_ITAB-SPERR NE '' ) OR
       ( WA_ITAB-CASSD NE '' ).

      PERFORM F_CHECK_HISTORY_BLOCK USING WA_ITAB-KUNNR.

    ENDIF.

    WA_ITAB-KVERM = WA_KNA1-KVERM.

    "End Add by Wantanee 20130605  T41K915444


    APPEND WA_ITAB TO GT_ITAB.

  ENDLOOP.


ENDFORM.  "map data

*&---------------------------------------------------------------------*
*&      Form  f_check_history_block
*&---------------------------------------------------------------------*
*       text  "Add by Wantanee 20130605  T41K915444
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CHECK_HISTORY_BLOCK USING P_KUNNR TYPE KNA1-KUNNR.

  DATA: LV_OBJECTID TYPE CDHDR-OBJECTID.

  CLEAR: LV_OBJECTID,GT_CDHDR,WA_CDHDR,GT_CDPOS,WA_CDPOS.

  LV_OBJECTID = P_KUNNR.



  SELECT OBJECTCLAS OBJECTID  CHANGENR USERNAME
         UDATE UTIME TCODE
  INTO TABLE GT_CDHDR
  FROM CDHDR
  WHERE OBJECTID EQ P_KUNNR
  AND   OBJECTCLAS EQ 'DEBI'
  AND   TCODE = 'XD05'.

  IF NOT GT_CDHDR IS INITIAL.

    SELECT OBJECTCLAS OBJECTID  CHANGENR TABNAME FNAME
          VALUE_NEW VALUE_OLD
    INTO TABLE GT_CDPOS
    FROM CDPOS
    WHERE OBJECTID EQ P_KUNNR
    AND   OBJECTCLAS EQ 'DEBI'.

    IF NOT GT_CDPOS IS INITIAL.
      SORT GT_CDPOS BY OBJECTID TABNAME FNAME CHANGENR.

      READ TABLE GT_CDPOS INTO WA_CDPOS WITH KEY TABNAME = 'KNA1'
                                                 FNAME = 'SPERR'.
      IF SY-SUBRC EQ 0.
        READ TABLE GT_CDHDR INTO WA_CDHDR WITH KEY CHANGENR = WA_CDPOS-CHANGENR.

        IF SY-SUBRC EQ 0.
          WA_ITAB-SPERR_D = WA_CDHDR-UDATE.
          WA_ITAB-SPERR_T = WA_CDHDR-UTIME.
          WA_ITAB-SPERR_U = WA_CDHDR-USERNAME.
        ENDIF.

      ELSE.
        WA_ITAB-SPERR_D = WA_ITAB-ERDAT.
        CLEAR WA_ITAB-SPERR_T .
        WA_ITAB-SPERR_U = ''.

      ENDIF.


      READ TABLE GT_CDPOS INTO WA_CDPOS WITH KEY TABNAME = 'KNA1'
                                                 FNAME = 'AUFSD'.
      IF SY-SUBRC EQ 0.
        READ TABLE GT_CDHDR INTO WA_CDHDR WITH KEY CHANGENR = WA_CDPOS-CHANGENR.

        IF SY-SUBRC EQ 0.
          WA_ITAB-AUFSD_D = WA_CDHDR-UDATE.
          WA_ITAB-AUFSD_T = WA_CDHDR-UTIME.
          WA_ITAB-AUFSD_U = WA_CDHDR-USERNAME.
        ENDIF.

      ELSE.
        WA_ITAB-AUFSD_D = WA_ITAB-ERDAT.
        CLEAR WA_ITAB-AUFSD_T .
        WA_ITAB-AUFSD_U = ''.

      ENDIF.


      READ TABLE GT_CDPOS INTO WA_CDPOS WITH KEY TABNAME = 'KNA1'
                                                 FNAME = 'LIFSD'.
      IF SY-SUBRC EQ 0.
        READ TABLE GT_CDHDR INTO WA_CDHDR WITH KEY CHANGENR = WA_CDPOS-CHANGENR.

        IF SY-SUBRC EQ 0.
          WA_ITAB-LIFSD_D = WA_CDHDR-UDATE.
          WA_ITAB-LIFSD_T = WA_CDHDR-UTIME.
          WA_ITAB-LIFSD_U = WA_CDHDR-USERNAME.
        ENDIF.

      ELSE.
        WA_ITAB-LIFSD_D = WA_ITAB-ERDAT.
        CLEAR WA_ITAB-LIFSD_T .
        WA_ITAB-LIFSD_U = ''.

      ENDIF.


      READ TABLE GT_CDPOS INTO WA_CDPOS WITH KEY TABNAME = 'KNA1'
                                                 FNAME = 'FAKSD'.
      IF SY-SUBRC EQ 0.
        READ TABLE GT_CDHDR INTO WA_CDHDR WITH KEY CHANGENR = WA_CDPOS-CHANGENR.

        IF SY-SUBRC EQ 0.
          WA_ITAB-FAKSD_D = WA_CDHDR-UDATE.
          WA_ITAB-FAKSD_T = WA_CDHDR-UTIME.
          WA_ITAB-FAKSD_U = WA_CDHDR-USERNAME.
        ENDIF.

      ELSE.
        WA_ITAB-FAKSD_D = WA_ITAB-ERDAT.
        CLEAR WA_ITAB-FAKSD_T .
        WA_ITAB-FAKSD_U = ''.

      ENDIF.

      READ TABLE GT_CDPOS INTO WA_CDPOS WITH KEY TABNAME = 'KNA1'
                                                 FNAME = 'CASSD'.
      IF SY-SUBRC EQ 0.
        READ TABLE GT_CDHDR INTO WA_CDHDR WITH KEY CHANGENR = WA_CDPOS-CHANGENR.

        IF SY-SUBRC EQ 0.
          WA_ITAB-CASSD_D = WA_CDHDR-UDATE.
          WA_ITAB-CASSD_T = WA_CDHDR-UTIME.
          WA_ITAB-CASSD_U = WA_CDHDR-USERNAME.
        ENDIF.

      ELSE.
        WA_ITAB-CASSD_D = WA_ITAB-ERDAT.
        CLEAR WA_ITAB-CASSD_T .
        WA_ITAB-CASSD_U = ''.

      ENDIF.


    ENDIF.

  ENDIF.





ENDFORM.                    "f_check_history_block

**&---------------------------------------------------------------------*
**&      Form  CUSTOMER
**&---------------------------------------------------------------------*
FORM GET_CUSTOMER USING   P_ADRNR TYPE KNA1-ADRNR
                  CHANGING
*                           value(p_name_tha) TYPE c
*                           value(p_name_eng) TYPE c
                           VALUE(P_STREET) TYPE C
                           VALUE(P_STR_SUPPL3) TYPE C
                           VALUE(P_LOCATION) TYPE ADRC-LOCATION
                           VALUE(P_CITY2) TYPE ADRC-CITY2
                           VALUE(P_CITY1) TYPE ADRC-CITY1
                           VALUE(P_POST_CODE1) TYPE ADRC-POST_CODE1
                           VALUE(P_TEL_NUMBER) TYPE ADRC-TEL_NUMBER.

  DATA: LV_ADRNR TYPE KNA1-ADRNR.


  CLEAR:P_LOCATION.

  LV_ADRNR =  P_ADRNR.

  SELECT ADDRNUMBER NAME1 NAME2 STREET STR_SUPPL3 LOCATION CITY2
         CITY1 POST_CODE1 TEL_NUMBER NATION
  INTO TABLE GT_ADRC
  FROM ADRC
  WHERE ADDRNUMBER = LV_ADRNR.
*            AND nation = ''.



  READ TABLE GT_ADRC INTO WA_ADRC WITH KEY ADDRNUMBER = LV_ADRNR
                                           NATION = ''.
  IF SY-SUBRC EQ 0.
    P_STREET = WA_ADRC-STREET.
    P_STR_SUPPL3 = WA_ADRC-STR_SUPPL3.
    P_LOCATION = WA_ADRC-LOCATION.
    P_CITY2 = WA_ADRC-CITY2.
    P_CITY1 = WA_ADRC-CITY1.
    P_POST_CODE1 = WA_ADRC-POST_CODE1.
    P_TEL_NUMBER = WA_ADRC-TEL_NUMBER.
  ENDIF.


ENDFORM. "
*&-----------------------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REPROT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_REPROT .
  PERFORM BUILD_LAYOUT.
  PERFORM BUILD_CATALOG.
  PERFORM BUILD_EVENT USING GT_EVENTS[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = GC_REPID
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
      I_SAVE                  = 'A'
*     is_layout               = gt_layout
      IT_EVENTS               = GT_EVENTS[]
      IT_FIELDCAT             = GT_FIELDCAT
    TABLES
      T_OUTTAB                = GT_ITAB
    EXCEPTIONS
      PROGRAM_ERROR           = 1
      OTHERS                  = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " DISPLAY_REPROT

* Add by K 20110215
*---------------------------------------------------------------------*
*       FORM User_command
*---------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                        RS_SELFIELD TYPE SLIS_SELFIELD.

  IF R_UCOMM = '&IC1'.  "Double click event

*   goto XD03 display Customer Master

    READ TABLE GT_ITAB INTO WA_ITAB INDEX RS_SELFIELD-TABINDEX.

    SET: PARAMETER ID 'KUN'  FIELD WA_ITAB-KUNNR,
            PARAMETER ID 'BUK'  FIELD P_BUKRS  .
    CALL TRANSACTION 'XD03'.

  ENDIF.

  CLEAR R_UCOMM.

ENDFORM.                    "user_comman

* End 20110215

*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT .
  GT_LAYOUT-WINDOW_TITLEBAR = SY-TITLE.
  GT_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GT_LAYOUT-ZEBRA = 'X'.

ENDFORM.                    "build_layout

*&---------------------------------------------------------------------*
*&      Form  BUILD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_CATALOG .
  CLEAR : V_POS.

* Account Group
  PERFORM APPEND_FIELDCAT USING 'KTOKD'
                                'KNA1'
                                'KTOKD'
                                 TEXT-T01
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

* Account Group name
  PERFORM APPEND_FIELDCAT USING 'GRP_NAME'
                                ''
                                ''
                                 TEXT-T02
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

* Customer Code
  PERFORM APPEND_FIELDCAT USING 'KUNNR'
                                'KNA1'
                                'KUNNR'
                                 TEXT-T03
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Customer Name Thai
  PERFORM APPEND_FIELDCAT USING 'NAME_THA'
                                ''
                                ''
                                 TEXT-T04
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Customer Name Eng
  PERFORM APPEND_FIELDCAT USING 'NAME_ENG'
                                ''
                                ''
                                 TEXT-T05
                                 SPACE  SPACE  SPACE
                            GT_FIELDCAT[].

*Address Street
  PERFORM APPEND_FIELDCAT USING 'STREET'
                                'ADRC'
                                'STREET'
                                'Address2'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Address
  PERFORM APPEND_FIELDCAT USING 'STR_SUPPL3'
                                'ADRC'
                                'STR_SUPPL3'
                                'Address2'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*Street5
  PERFORM APPEND_FIELDCAT USING 'LOCATION'
                                'ADRC'
                                'LOCATION'
                                'Address3'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*District
  PERFORM APPEND_FIELDCAT USING 'CITY2'
                                'ADRC'
                                'CITY2'
                                'District'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*City
  PERFORM APPEND_FIELDCAT USING 'CITY1'
                                'ADRC'
                                'CITY1'
                                'City'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*Post code
  PERFORM APPEND_FIELDCAT USING 'POST_CODE1'
                                'ADRC'
                                'POST_CODE1'
                                 TEXT-T09
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Tel
  PERFORM APPEND_FIELDCAT USING 'TELF1'
                                'KNA1'
                                'TELF1'
                                 TEXT-T10
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Mobile phone
  PERFORM APPEND_FIELDCAT USING 'TELF2'
                                'KNA1'
                                'TELF2'
                                 TEXT-T46
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*fax
  PERFORM APPEND_FIELDCAT USING 'TELFX'
                                'KNA1'
                                'TELFX'
                                 TEXT-T47
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Tax Number 1
  PERFORM APPEND_FIELDCAT USING 'STCD1'
                                'KNA1'
                                'STCD1'
                                 TEXT-T36
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Tax Number 2
  PERFORM APPEND_FIELDCAT USING 'STCD2'
                                'KNA1'
                                'STCD2'
                                 TEXT-T37
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

  "Add by Wantanee 20131209

*Tax Number 3
  PERFORM APPEND_FIELDCAT USING 'STCD3'
                                'KNA1'
                                'STCD3'
                                'Tax Number 3'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Branch Number
  PERFORM APPEND_FIELDCAT USING 'BRSCH'
                                'FITHA_PBUPL_D_T'
                                'J_1TPBUPL'
                                'Branch Number'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*Industry Description
  PERFORM APPEND_FIELDCAT USING 'BRTXT'
                                'FITHA_PBUPL_D_T'
                                'DESCRIPTION'
                                 TEXT-T38
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*WHT R1
  PERFORM APPEND_FIELDCAT USING 'WH_R1'
                                'KNBW'
                                'WITHT'
                                 TEXT-T39
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*WHT R1 Desc.
  PERFORM APPEND_FIELDCAT USING 'WH_R1_TEXT'
                                'T059U'
                                'TEXT40'
                                 TEXT-T40
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*WHT R2
  PERFORM APPEND_FIELDCAT USING 'WH_R2'
                                'KNBW'
                                'WITHT'
                                 TEXT-T41
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*WHT R2 Desc
  PERFORM APPEND_FIELDCAT USING 'WH_R2_TEXT'
                                'T059U'
                                'TEXT40'
                                 TEXT-T42
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*Create Date
  PERFORM APPEND_FIELDCAT USING 'ERDAT'
                                'KNA1'
                                'ERDAT'
                                 TEXT-T43
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

  "BOD BOONPIPOP
**Account memo for dealer contract date Start
*  PERFORM APPEND_FIELDCAT USING 'KVERM_1'
*                                'KNB1'
*                                'KVERM'
*                                 TEXT-T44
*                                 SPACE  SPACE  SPACE
*                                 GT_FIELDCAT[].
*
**Account memo for dealer contract date End
*  PERFORM APPEND_FIELDCAT USING 'KVERM_2'
*                                'KNB1'
*                                'KVERM'
*                                 TEXT-T45
*                                 SPACE  SPACE  SPACE
*                                 GT_FIELDCAT[].
  "EOD BOONPIPOP

*Credit Term
  PERFORM APPEND_FIELDCAT USING 'ZTERM'
                                'KNVV'
                                'ZTERM'
                                 TEXT-T11
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].


*Credit limit
  PERFORM APPEND_FIELDCAT USING 'KLIMK'
                                'KNKK'
                                'KLIMK'
                                 TEXT-T12
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*Customer mather
  PERFORM APPEND_FIELDCAT USING 'KNKLI'
                                'KNKK'
                                'KNKLI'
                                 TEXT-T13
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*Risk category
  PERFORM APPEND_FIELDCAT USING 'CTLPC'
                                'KNKK'
                                'CTLPC'
                                 TEXT-T14
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Sale Area 00
"-------------------------------------------------------------
  PERFORM APPEND_FIELDCAT USING 'VTWEG_00'
                                'KNVV'
                                'VTWEG'
                                 TEXT-T15
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Distribution Chanal Detail
  PERFORM APPEND_FIELDCAT USING 'VTEXT_00'
                                'TVTWT'
                                'VTEXT'
                                 TEXT-T16
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].


*Sale office
  PERFORM APPEND_FIELDCAT USING 'VKBUR_00'
                                'KNVV'
                                'VKBUR'
                                 TEXT-T17
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*Sale Office detail
  PERFORM APPEND_FIELDCAT USING 'BEZEI_00'
                                'TVKBT'
                                'BEZEI'
                                 TEXT-T18
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Sale Group
  PERFORM APPEND_FIELDCAT USING 'VKGRP_00'
                                'KNVV'
                                'VKGRP'
                                'Sale Group 10'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*Sale Group description
  PERFORM APPEND_FIELDCAT USING 'GBEZEI_00'
                                'TVGRT'
                                'BEZEI'
                                 'Sale Group Desc. 10'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].


*Sale Code
  PERFORM APPEND_FIELDCAT USING 'PERNR_00'
                                'KNVP'
                                'PERNR'
                                 TEXT-T19
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Sale name thai
  PERFORM APPEND_FIELDCAT USING 'ALNAM_00'
                                'PA0182'
                                'ALNAM'
                                 TEXT-T20
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Sale name eng
  PERFORM APPEND_FIELDCAT USING 'ENAME_00'
                                'PA0001'
                                'ENAME'
                                 TEXT-T21
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*"Sale Area D0
"----------------------------------------------------------------
  PERFORM APPEND_FIELDCAT USING 'VTWEG_D0'
                                'KNVV'
                                'VTWEG'
                                 TEXT-T22
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*Distribution Chanal Detail
  PERFORM APPEND_FIELDCAT USING 'VTEXT_D0'
                                'TVTWT'
                                'VTEXT'
                                 TEXT-T23
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Sale office D0
  PERFORM APPEND_FIELDCAT USING 'VKBUR_D0'
                                'KNVV'
                                'VKBUR'
                                 TEXT-T24
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*Sale Office detail D0
  PERFORM APPEND_FIELDCAT USING 'BEZEI_D0'
                                'TVKBT'
                                'BEZEI'
                                 TEXT-T25
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Sale Group D0
  PERFORM APPEND_FIELDCAT USING 'VKGRP_D0'
                                'KNVV'
                                'VKGRP'
                                 'Sale Group D0'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*Sale Group description 40
  PERFORM APPEND_FIELDCAT USING 'GBEZEI_D0'
                                'TVGRT'
                                'BEZEI'
                                'Sale Group Desc. D0'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Sale Code
  PERFORM APPEND_FIELDCAT USING 'PERNR_D0'
                                'KNVP'
                                'PERNR'
                                 TEXT-T26
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Sale name thai
  PERFORM APPEND_FIELDCAT USING 'ALNAM_D0'
                                'PA0182'
                                'ALNAM'
                                 TEXT-T27
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Sale name eng
  PERFORM APPEND_FIELDCAT USING 'ENAME_D0'
                                'PA0001'
                                'ENAME'
                                 TEXT-T28
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

"--------------------------------------------------------------
*"Sale Area S0
  PERFORM APPEND_FIELDCAT USING 'VTWEG_S0'
                                'KNVV'
                                'VTWEG'
                                 TEXT-T53
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*Distribution Chanal Detail
  PERFORM APPEND_FIELDCAT USING 'VTEXT_S0'
                                'TVTWT'
                                'VTEXT'
                                 TEXT-T54
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Sale office S0
  PERFORM APPEND_FIELDCAT USING 'VKBUR_S0'
                                'KNVV'
                                'VKBUR'
                                 TEXT-T55
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*Sale Office detail S0
  PERFORM APPEND_FIELDCAT USING 'BEZEI_S0'
                                'TVKBT'
                                'BEZEI'
                                 TEXT-T56
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Sale Group S0
  PERFORM APPEND_FIELDCAT USING 'VKGRP_S0'
                                'KNVV'
                                'VKGRP'
                                 'Sale Group S0'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*Sale Group description S0
  PERFORM APPEND_FIELDCAT USING 'GBEZEI_S0'
                                'TVGRT'
                                'BEZEI'
                                'Sale Group Desc. S0'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Sale Code S0
  PERFORM APPEND_FIELDCAT USING 'PERNR_S0'
                                'KNVP'
                                'PERNR'
                                 TEXT-T57
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Sale name thai
  PERFORM APPEND_FIELDCAT USING 'ALNAM_S0'
                                'PA0182'
                                'ALNAM'
                                 TEXT-T58
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Sale name eng
  PERFORM APPEND_FIELDCAT USING 'ENAME_S0'
                                'PA0001'
                                'ENAME'
                                 TEXT-T59
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Central billing block
  PERFORM APPEND_FIELDCAT USING 'FAKSD'
                                'KNA1'
                                'FAKSD'
                                 TEXT-T29
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Central billing block date
  PERFORM APPEND_FIELDCAT USING 'FAKSD_D'
                                'CDHDR'
                                'UDATE'
                                'Bill Block Date'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Central billing block time
  PERFORM APPEND_FIELDCAT USING 'FAKSD_T'
                                'CDHDR'
                                'UTIME'
                                'Bill Block Time'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Central billing block user
  PERFORM APPEND_FIELDCAT USING 'FAKSD_U'
                                'CDHDR'
                                'USERNAME'
                                'Bill Block User'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Central del.block
  PERFORM APPEND_FIELDCAT USING 'NODEL'
                                'KNA1'
                                'NODEL'
                                 TEXT-T30
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*Central deletion flag
  PERFORM APPEND_FIELDCAT USING 'LOEVM'
                                'KNA1'
                                'LOEVM'
                                 TEXT-T31
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Central delivery block
  PERFORM APPEND_FIELDCAT USING 'LIFSD'
                                'KNA1'
                                'LIFSD'
                                 TEXT-T32
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Central delivery block date
  PERFORM APPEND_FIELDCAT USING 'LIFSD_D'
                                'CDHDR'
                                'UDATE'
                                'delivery Block Date'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Central delivery block time
  PERFORM APPEND_FIELDCAT USING 'LIFSD_T'
                                'CDHDR'
                                'UTIME'
                                'delivery Block Time'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Central delivery block user
  PERFORM APPEND_FIELDCAT USING 'LIFSD_U'
                                'CDHDR'
                                'USERNAME'
                                'delivery Block User'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Central order block
  PERFORM APPEND_FIELDCAT USING 'AUFSD'
                                'KNA1'
                                'AUFSD'
                                 TEXT-T33
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Central order block date
  PERFORM APPEND_FIELDCAT USING 'AUFSD_D'
                                'CDHDR'
                                'UDATE'
                                'Order Block Date'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Central order block time
  PERFORM APPEND_FIELDCAT USING 'AUFSD_T'
                                'CDHDR'
                                'UTIME'
                                'Order Block Time'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Central order block user
  PERFORM APPEND_FIELDCAT USING 'AUFSD_U'
                                'CDHDR'
                                'USERNAME'
                                'Order Block User'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*Central posting block
  PERFORM APPEND_FIELDCAT USING 'SPERR'
                                'KNA1'
                                'SPERR'
                                 TEXT-T34
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*Central posting block date
  PERFORM APPEND_FIELDCAT USING 'SPERR_D'
                                'CDHDR'
                                'UDATE'
                                'Posting Block Date'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Central posting block time
  PERFORM APPEND_FIELDCAT USING 'SPERR_T'
                                'CDHDR'
                                'UTIME'
                                'Posting Block Time'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Central posting block user
  PERFORM APPEND_FIELDCAT USING 'SPERR_U'
                                'CDHDR'
                                'USERNAME'
                                'Posting Block User'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Central sale block
  PERFORM APPEND_FIELDCAT USING 'CASSD'
                                'KNA1'
                                'CASSD'
                                 TEXT-T35
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].


*Central order block date
  PERFORM APPEND_FIELDCAT USING 'CASSD_D'
                                'CDHDR'
                                'UDATE'
                                'Sale Block Date'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Central order block time
  PERFORM APPEND_FIELDCAT USING 'CASSD_T'
                                'CDHDR'
                                'UTIME'
                                'Sale Block Time'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Central order block user
  PERFORM APPEND_FIELDCAT USING 'CASSD_U'
                                'CDHDR'
                                'USERNAME'
                                'Sale Block User'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Reason for block
  PERFORM APPEND_FIELDCAT USING 'KVERM'
                                'KNB1'
                                'KVERM'
                                'Reason for block'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Transportation Zone
  PERFORM APPEND_FIELDCAT USING 'LZONE'
                                'KNA1'
                                'LZONE'
                                 TEXT-T48
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Shipping Condition
  PERFORM APPEND_FIELDCAT USING 'VSBED'
                                'KNVV'
                                'VSBED'
                                 TEXT-T49
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Shipping Condition Description
  PERFORM APPEND_FIELDCAT USING 'VTEXT'
                                'TVSBT'
                                'VTEXT'
                                 TEXT-T50
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].


*Add by wantanee 20110907
*Credit Term for divition 40
  PERFORM APPEND_FIELDCAT USING 'ZTERM_40'
                                'KNVV'
                                'ZTERM'
                                 TEXT-T51
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Add by wantanee 20110907

  PERFORM APPEND_FIELDCAT USING 'USERNAME'
                                'CDHDR'
                                SPACE
                                'Last Changed'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

  PERFORM APPEND_FIELDCAT USING 'UDATE'
                                'CDHDR'
                                SPACE
                                'Last Changed Date'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

  PERFORM APPEND_FIELDCAT USING 'UTIME'
                                'CDHDR'
                                SPACE
                                'Last Changed Time'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

  PERFORM APPEND_FIELDCAT USING 'NAME1_C1'
                                SPACE
                                SPACE
                                'Contract name1'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

  PERFORM APPEND_FIELDCAT USING 'VTEXT_C1'
                                SPACE
                                SPACE
                                'Department1'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

  PERFORM APPEND_FIELDCAT USING 'NAME1_C2'
                                SPACE
                                SPACE
                                'Contract name2'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

  PERFORM APPEND_FIELDCAT USING 'VTEXT_C2'
                                SPACE
                                SPACE
                                'Department2'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

  PERFORM APPEND_FIELDCAT USING 'NAME1_C3'
                                SPACE
                                SPACE
                                'Contract name3'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

  PERFORM APPEND_FIELDCAT USING 'VTEXT_C3'
                                SPACE
                                SPACE
                                'Department3'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

  PERFORM APPEND_FIELDCAT USING 'DISPE'
                                SPACE
                                SPACE
                                'DISC SP'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

  PERFORM APPEND_FIELDCAT USING 'KONWA'
                                SPACE
                                SPACE
                                'DISC Rate Unit'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

  PERFORM APPEND_FIELDCAT USING 'KONDA'
                                SPACE
                                SPACE
                                'Pricing Group'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

  PERFORM APPEND_FIELDCAT USING 'KONDA_D'
                                SPACE
                                SPACE
                                'Pricing Group Desc.'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

*Telephone tel_number
  PERFORM APPEND_FIELDCAT USING 'TEL_NUMBER'
                                SPACE
                                SPACE
                                'Telephone1'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*Industry Code
  PERFORM APPEND_FIELDCAT USING 'BRSCH'
                                SPACE
                                SPACE
                                'Industry Code'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

ENDFORM.             "BUILD_ATALOG
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1453   text
*      -->P_1454   text
*      -->P_1455   text
*      -->P_TEXT_T01  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*

FORM APPEND_FIELDCAT USING   P_FIELD   "Field name
                             P_REFTABLE"Reference Table name
                             P_REFFIELD"Reference Field name
                             P_COLTXT  "Col Text(for specify)
                             P_DOSUM   "Sum total
                             P_CFIELDNAME  "  currency
                             P_NO_ZERO     " no zero
                             P_IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

  DATA: WA_INFIELDCAT   TYPE SLIS_FIELDCAT_ALV,
        V_COLTXT_LENGTH TYPE I.

  ADD 1 TO V_POS.

  WA_INFIELDCAT-FIELDNAME     = P_FIELD.
  WA_INFIELDCAT-REF_TABNAME   = P_REFTABLE.
  WA_INFIELDCAT-REF_FIELDNAME = P_REFFIELD.
  WA_INFIELDCAT-COL_POS       = V_POS .
  WA_INFIELDCAT-DO_SUM        = P_DOSUM.

  IF NOT P_NO_ZERO IS INITIAL .
    WA_INFIELDCAT-NO_ZERO = P_NO_ZERO.
  ENDIF.
  IF NOT P_CFIELDNAME IS INITIAL .
    WA_INFIELDCAT-CFIELDNAME = P_CFIELDNAME .
  ENDIF.


*If we need to specify text ,don't need to derive from data dictionary
*program will check length and define width of the colum
  IF NOT P_COLTXT IS INITIAL.
    V_COLTXT_LENGTH = STRLEN( P_COLTXT ).

    IF V_COLTXT_LENGTH > 20.
      WA_INFIELDCAT-DDICTXT = 'L'."Long text
      WA_INFIELDCAT-SELTEXT_L = P_COLTXT.
    ELSEIF V_COLTXT_LENGTH > 10.
      WA_INFIELDCAT-DDICTXT = 'M'."Medium Text
      WA_INFIELDCAT-SELTEXT_M = P_COLTXT.
    ELSE.
      WA_INFIELDCAT-DDICTXT = 'S'."Short Text
      WA_INFIELDCAT-SELTEXT_S = P_COLTXT.
    ENDIF.
    WA_INFIELDCAT-REPTEXT_DDIC = P_COLTXT  .
  ENDIF.
  APPEND WA_INFIELDCAT TO P_IT_FIELDCAT.
ENDFORM.                    " APPEND_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS  text
*----------------------------------------------------------------------*
FORM BUILD_EVENT  USING E03_LT_EVENTS TYPE SLIS_T_EVENT.
  DATA: LW_EVENT TYPE SLIS_ALV_EVENT.
* Read Event
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE = 1
    IMPORTING
      ET_EVENTS   = E03_LT_EVENTS.
  READ TABLE E03_LT_EVENTS WITH KEY NAME = SLIS_EV_TOP_OF_PAGE
                           INTO LW_EVENT.
*   READ TABLE e03_lt_events WITH KEY name = slis_ev_top_of_list
*                             INTO lw_event.
  IF SY-SUBRC = 0.
* register top of page event
    MOVE GC_TOP_OF_PAGE TO LW_EVENT-FORM.
    APPEND LW_EVENT TO E03_LT_EVENTS.
  ENDIF.
ENDFORM.                    " BUILD_EVENT

*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  PERFORM WRITE_HEADING.
ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  WRITE_HEADING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_HEADING .
  DATA: T_HEADER  TYPE   SLIS_T_LISTHEADER,
        WA_HEADER TYPE   SLIS_LISTHEADER.

  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = 'Report Name : '.
  WA_HEADER-INFO = 'Report Customer detail for credit'.
  APPEND WA_HEADER TO T_HEADER.
  CLEAR WA_HEADER.


  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = 'Report Date : '.
  CONCATENATE  SY-DATUM+6(2) '.'
               SY-DATUM+4(2) '.'
               SY-DATUM(4) INTO WA_HEADER-INFO.   "todays date
  APPEND WA_HEADER TO T_HEADER.
  CLEAR: WA_HEADER.

  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = 'Report Time : '.
  WRITE: SY-UZEIT TO WA_HEADER-INFO.    "todays date
  APPEND WA_HEADER TO T_HEADER.
  CLEAR: WA_HEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_HEADER.

ENDFORM.                    " WRITE_HEADING
*&---------------------------------------------------------------------*
*&      Form  F_GET_LAST_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_LAST_CHANGED . "Add by Jakarin 20150926
  CONSTANTS LC_OBJECTCLAS TYPE C LENGTH 4 VALUE '4'.
  SELECT OBJECTCLAS
         OBJECTID
         USERNAME
         MAX( UDATE ) AS UDATE
         UTIME
    FROM CDHDR
    INTO CORRESPONDING FIELDS OF TABLE GT_LAST_CHANGE
    WHERE OBJECTCLAS EQ 'DEBI'
      AND OBJECTID   IN S_KUNNR
    GROUP BY OBJECTCLAS OBJECTID USERNAME UTIME.

  SORT GT_LAST_CHANGE BY OBJECTID
                         UDATE DESCENDING
                         UTIME DESCENDING.
ENDFORM.                    " F_GET_LAST_CHANGED

*&---------------------------------------------------------------------*
*&      Form  F_HIDE_B3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_HIDE_B3 .
  LOOP AT SCREEN.
    CASE SCREEN-GROUP1.
      WHEN 'B3'.
        SCREEN-ACTIVE = 0.
      WHEN OTHERS.
        SCREEN-ACTIVE = 1.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                                                    " F_HIDE_B3
*&---------------------------------------------------------------------*
*&      Form  F_SHOW_B3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SHOW_B3 .
  LOOP AT SCREEN.
    CASE SCREEN-GROUP1.
      WHEN 'B3'.
        SCREEN-ACTIVE = 1.
      WHEN OTHERS.
        SCREEN-ACTIVE = 1.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                                                    " F_SHOW_B3
**&---------------------------------------------------------------------*
**&      Form  F_GET_CONTACT_PERSON
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM F_GET_ADDTIONAL_DATA.
*  DATA : BEGIN OF LS_KNVK,
*           KUNNR TYPE KNVK-KUNNR,
*           NAME1 TYPE KNVK-NAME1,
*           ABTNR TYPE KNVK-ABTNR,
*         END OF LS_KNVK.
*  DATA LT_KNVK LIKE TABLE OF LS_KNVK.
*
*  DATA : BEGIN OF LS_TSABT,
*           ABTNR TYPE TSABT-ABTNR,
*           VTEXT TYPE TSABT-VTEXT,
*         END OF LS_TSABT.
*  DATA LT_TSABT LIKE TABLE OF LS_TSABT.
*
**  DATA : BEGIN OF LS_A552,
**           KNUMH TYPE A552-KNUMH,
**           KAPPL TYPE A552-KAPPL,
**           KSCHL TYPE A552-KSCHL,
**           KUNNR TYPE KNVV-KUNNR,
**           VKGRP TYPE A552-VKGRP,
**           VTWEG TYPE KNVV-VTWEG,
**           KONDA TYPE A552-KONDA,
**         END OF LS_A552.
**  DATA LT_A552 LIKE TABLE OF LS_A552.
*
*  DATA : BEGIN OF LS_KONP,
*           KNUMH TYPE KONP-KNUMH,
*           KBETR TYPE KONP-KBETR,
*           KONWA TYPE KONP-KONWA,
*         END OF LS_KONP.
*  DATA LT_KONP LIKE TABLE OF LS_KONP.
*
*  DATA : BEGIN OF LS_T188T,
*           KONDA TYPE T188T-KONDA,
*           VTEXT TYPE T188T-VTEXT,
*         END OF LS_T188T.
*  DATA LT_T188T LIKE TABLE OF LS_T188T.
*
*  DATA LS_ITAB TYPE TYP_OUT.
*
*  DATA : LV_VTEXT TYPE TSABT-VTEXT,
*         LV_COUNT TYPE I,
*         LV_TABIX TYPE SY-TABIX.
*
*  SELECT KUNNR
*         NAME1
*         ABTNR
*    FROM KNVK
*    INTO TABLE LT_KNVK
*    FOR ALL ENTRIES IN GT_ITAB
*    WHERE KUNNR EQ GT_ITAB-KUNNR.
*
*  IF LT_KNVK[] IS NOT INITIAL.
*    SELECT ABTNR
*           VTEXT
*      FROM TSABT
*      INTO TABLE LT_TSABT
*      FOR ALL ENTRIES IN LT_KNVK
*      WHERE ABTNR EQ LT_KNVK-ABTNR
*        AND SPRAS EQ SY-LANGU.
*  ENDIF.
*
*  SELECT A552~KNUMH
*         A552~KAPPL
*         A552~KSCHL
*         KNVV~KUNNR
*         A552~VKGRP
*         KNVV~VTWEG
*         A552~KONDA
*    FROM A552
*    INNER JOIN KNVV ON A552~KONDA EQ KNVV~KONDA AND
*                       A552~VKORG EQ KNVV~VKORG AND
*                       A552~VTWEG EQ KNVV~VTWEG
*    INTO TABLE LT_A552
*    FOR ALL ENTRIES IN GT_ITAB
*    WHERE A552~KAPPL EQ 'V'
*      AND A552~KSCHL EQ 'ZD04'
*      AND A552~VKORG EQ P_BUKRS
*      AND A552~VTWEG EQ GT_ITAB-VTWEG_40
**      AND a552~vkgrp EQ gt_itab-vkgrp_40
*      AND A552~KONDM EQ '02'
*      AND A552~DATBI GE P_PDAT
*      AND A552~DATAB LE P_PDAT
*      AND KNVV~KUNNR EQ GT_ITAB-KUNNR.
*
*  IF LT_A552 IS NOT INITIAL.
*    SELECT KNUMH
*           KBETR
*           KONWA
*      FROM KONP
*      INTO TABLE LT_KONP
*      FOR ALL ENTRIES IN LT_A552
*      WHERE KNUMH EQ LT_A552-KNUMH.
*
*    SELECT KONDA
*           VTEXT
*      FROM T188T
*      INTO TABLE LT_T188T
*      FOR ALL ENTRIES IN LT_A552
*      WHERE KONDA EQ LT_A552-KONDA.
*
*  ENDIF.
*
*  LOOP AT GT_ITAB INTO LS_ITAB.
*    LV_TABIX = SY-TABIX.
*    CLEAR LV_COUNT.
*    LOOP AT LT_KNVK INTO LS_KNVK WHERE KUNNR = LS_ITAB-KUNNR.
*      ADD 1 TO LV_COUNT.
*      READ TABLE LT_TSABT INTO LS_TSABT
*      WITH KEY ABTNR = LS_KNVK-ABTNR.
*      IF SY-SUBRC = 0.
*        LV_VTEXT = LS_TSABT-VTEXT.
*      ELSE.
*        CLEAR LV_VTEXT.
*      ENDIF.
*
*      IF LV_COUNT = 1.
*        LS_ITAB-NAME1_C1 = LS_KNVK-NAME1.
*        LS_ITAB-VTEXT_C1 = LV_VTEXT.
*      ELSEIF LV_COUNT = 2.
*        LS_ITAB-NAME1_C2 = LS_KNVK-NAME1.
*        LS_ITAB-VTEXT_C2 = LV_VTEXT.
*      ELSEIF LV_COUNT = 3.
*        LS_ITAB-NAME1_C3 = LS_KNVK-NAME1.
*        LS_ITAB-VTEXT_C3 = LV_VTEXT.
*      ENDIF.
*      CLEAR : LS_KNVK,LS_TSABT.
*    ENDLOOP.
*
*    READ TABLE LT_A552 INTO LS_A552
*    WITH KEY KUNNR = LS_ITAB-KUNNR
**             vkgrp = ls_itab-vkgrp_40
*             VTWEG = LS_ITAB-VTWEG_40.
*    IF SY-SUBRC = 0.
*      LS_ITAB-KONDA  = LS_A552-KONDA.
*
*      READ TABLE LT_KONP INTO LS_KONP
*      WITH KEY KNUMH = LS_A552-KNUMH.
*      IF SY-SUBRC = 0.
*        LS_ITAB-DISPE = LS_KONP-KBETR / 10.
*        LS_ITAB-KONWA = LS_KONP-KONWA.
*      ENDIF.
*
*      READ TABLE LT_T188T INTO LS_T188T
*      WITH KEY KONDA = LS_A552-KONDA.
*      IF SY-SUBRC = 0.
*        LS_ITAB-KONDA_D = LS_T188T-VTEXT.
*      ENDIF.
*
*    ENDIF.
*    MODIFY GT_ITAB FROM LS_ITAB INDEX LV_TABIX
*                         TRANSPORTING NAME1_C1 VTEXT_C1 NAME1_C2 VTEXT_C2 NAME1_C3 VTEXT_C3 DISPE KONWA
*                                      KONDA KONDA_D.
*    CLEAR : LS_ITAB.
*  ENDLOOP.
*
*ENDFORM.                    " F_GET_CONTACT_PERSON
