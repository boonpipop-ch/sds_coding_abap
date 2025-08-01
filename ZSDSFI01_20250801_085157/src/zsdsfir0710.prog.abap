*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0280
*  Creation Date      : 03.03.2025
*  Author             : JAKARIN SIRILERTLAK (SDS)
*  Add-on ID          : ZFIGLE001
*  Description        : This is a program to upload FI DOC
*  Purpose            : To upload FI Doc (AP, AR ,GL, AA)
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date         Task #      Programmer  Description
*-----------------------------------------------------------------------
*  29.04.2025   F36K916699  Boonpipop.ch Add JV Voucher for validate
*-----------------------------------------------------------------------
REPORT ZSDSFIR0710.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*
INCLUDE:
  ZSDSFIR0710_TOP.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
##NEEDED
TABLES:
  SSCRFIELDS.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: TS_FNAME  TYPE  CHAR40.
TYPES: TS_MSG_TXT  TYPE  ZSDSDE_MSGTX.

TYPES: BEGIN OF TS_COUNT,
         TOTAL TYPE  I,
         SUCCS TYPE  I,
         ERROR TYPE  I,
       END OF TS_COUNT.

TYPES: BEGIN OF TS_PROC_INFO,
         DATUM TYPE  SY-DATUM,
         UZEIT TYPE  SY-UZEIT,
         UNAME TYPE  SY-UNAME,
         IFILE TYPE  STRING,
         MODE  TYPE  TEXT50,
         COUNT TYPE  TS_COUNT,
       END OF TS_PROC_INFO.

TYPES: BEGIN OF TS_RESULT.
         INCLUDE TYPE ZSDSFIS086.
TYPES: END OF TS_RESULT.
TYPES: TT_RESULT TYPE STANDARD TABLE OF TS_RESULT
                         WITH DEFAULT KEY.

TYPES: BEGIN OF TS_MESSG,
         ROWNO TYPE  TS_RESULT-ROWNO,
         MSGTY TYPE  SY-MSGTY,
         MSGID TYPE  SY-MSGID,
         MSGNO TYPE  SY-MSGNO,
         MSGTX TYPE  TS_MSG_TXT,
       END OF TS_MESSG.

TYPES: TS_XLSX TYPE ZCL_SDSCA_UTILITIES=>TS_XLSX_DATA.
TYPES: TT_XLSX TYPE ZCL_SDSCA_UTILITIES=>TT_XLSX_DATA.

TYPES: BEGIN OF TS_SUM,
         TOTAL TYPE  I, "Total Entries
         SUCCS TYPE  I, "Success Entries
         ERROR TYPE  I, "Error Entries
       END   OF TS_SUM.

TYPES: TT_MESSG  TYPE  STANDARD TABLE OF TS_MESSG
                         WITH DEFAULT KEY.

TYPES: BEGIN OF TS_KEY,
         TRANS_NO TYPE CHAR10,
*         DOCNO    TYPE  TS_RESULT-BELNR,
       END OF TS_KEY.

TYPES: BEGIN OF TS_HEAD,
         BUKRS       TYPE BKPF-BUKRS,
         BLDAT       TYPE BKPF-BLDAT,
         BUDAT       TYPE BKPF-BUDAT,
         MONAT       TYPE BKPF-MONAT,
         WWERT       TYPE BKPF-WWERT,
         LDGRP       TYPE BKPF-LDGRP,
         XBLNR       TYPE BKPF-XBLNR,
         BKTXT       TYPE BKPF-BKTXT,
         BLART       TYPE BKPF-BLART,
         WAERS       TYPE BKPF-WAERS,
         KURSF       TYPE BKPF-KURSF,
         BRNCH       TYPE BKPF-BRNCH,
         WAERS_BUKRS TYPE T001-WAERS,
       END OF TS_HEAD.

TYPES: BEGIN OF TS_ITEM_DATA,
*-----Item-------
         NEWBS                   TYPE RF05A-NEWBS, "Account Key
         NEWKO                   TYPE RF05A-NEWKO, "Account No.
         BSCHL                   TYPE BSEG-BSCHL,
         HKONT                   TYPE BSEG-HKONT,
         UMSKZ                   TYPE RF05A-NEWUM, "special GL
         BCODE                   TYPE BSEG-J_1TPBUPL,
         WRBTR                   TYPE BSEG-WRBTR,
         DMBTR                   TYPE BSEG-DMBTR,
         MWSKZ                   TYPE BSEG-MWSKZ,
         FWBAS                   TYPE BSEG-FWBAS,
         HWBAS                   TYPE BSEG-HWBAS,
         WMWST                   TYPE BSEG-WMWST,
         MWSTS                   TYPE BSEG-MWSTS,
         PARGB                   TYPE BSEG-PARGB,
         VALUT                   TYPE BSEG-VALUT,
         ZUONR                   TYPE BSEG-ZUONR,
         SGTXT                   TYPE BSEG-SGTXT,
         ZFBDT                   TYPE BSEG-ZFBDT,
         ZTERM                   TYPE BSEG-ZTERM,
         ZLSPR                   TYPE BSEG-ZLSPR,
         ZLSCH                   TYPE BSEG-ZLSCH,
         XREF1                   TYPE BSEG-XREF1,
         XREF2                   TYPE BSEG-XREF2,
         XREF3                   TYPE BSEG-XREF3,
         ANLN1                   TYPE BSEG-ANLN1,
         ANLN2                   TYPE BSEG-ANLN2,
         BZDAT                   TYPE BSEG-BZDAT,
         ANBWA                   TYPE BSEG-ANBWA,
         MATNR                   TYPE BSEG-MATNR,
*------CO-object---------
         KOSTL                   TYPE BSEG-KOSTL,
         PRCTR                   TYPE BSEG-PRCTR,
         POSID                   TYPE PS_POSID,
         AUFNR                   TYPE BSEG-AUFNR,

*------Profit-segment----
         CE0_KNDNR               TYPE  CE01000-KNDNR,
         CE0_ARTNR               TYPE  CE01000-ARTNR,
         CE0_FKART               TYPE  CE01000-FKART,
         CE0_KAUFN               TYPE  CE01000-KAUFN,
         CE0_KDPOS               TYPE  CE01000-KDPOS,
         CE0_RKAUFNR             TYPE  CE01000-RKAUFNR,
         CE0_WERKS               TYPE  CE01000-WERKS,
         CE0_FKBER               TYPE  CE01000-FKBER,
         CE0_VKORG               TYPE  CE01000-VKORG,
         CE0_VTWEG               TYPE  CE01000-VTWEG,
         CE0_SPART               TYPE  CE01000-SPART,
         CE0_PSPNR               TYPE  CE01000-PSPNR,
         CE0_COPA_KOSTL          TYPE  CE01000-COPA_KOSTL,
         CE0_KSTRG               TYPE  CE01000-KSTRG,
         CE0_PRCTR               TYPE  CE01000-PRCTR,
         CE0_PPRCTR              TYPE  CE01000-PPRCTR,
         CE0_SERVICE_DOC_TYPE    TYPE  CE01000-SERVICE_DOC_TYPE,
         CE0_SERVICE_DOC_ID      TYPE  CE01000-SERVICE_DOC_ID,
         CE0_SERVICE_DOC_ITEM_ID TYPE  CE01000-SERVICE_DOC_ITEM_ID,
         CE0_MATKL               TYPE  CE01000-MATKL,
         CE0_VKBUR               TYPE  CE01000-VKBUR,
         CE0_VKGRP               TYPE  CE01000-VKGRP,
         CE0_PRODH               TYPE  CE01000-PRODH,
         CE0_KUNWE               TYPE  CE01000-KUNWE,
         CE0_WWLO                TYPE  CE01000-WWLO,
         CE0_LAND1               TYPE  CE01000-LAND1,
         CE0_PAPH1               TYPE  CE01000-PAPH1,
         CE0_PAPH2               TYPE  CE01000-PAPH2,
         CE0_PAPH3               TYPE  CE01000-PAPH3,
         CE0_REGIO               TYPE  CE01000-REGIO,
         CE0_ZZ1_ACTTYPE_MSE     TYPE  CE01000-ZZ1_ACTTYPE_MSE,
         CE0_ZZ1_APPLTN          TYPE  CE01000-ZZ1_APPLTN,
         CE0_ZZ1_FISCYR_MSE      TYPE  CE01000-ZZ1_FISCYR_MSE,
         CE0_ZZ1_ITMTYPE_MSE     TYPE  CE01000-ZZ1_ITMTYPE_MSE,
         CE0_ZZ1_PROJTYPE_MSE    TYPE  CE01000-ZZ1_PROJTYPE_MSE,
         CE0_ZZ1_PROJ_MSE        TYPE  CE01000-ZZ1_PROJ_MSE,
         CE0_ZZ1_SDSDIST_MSE     TYPE  CE01000-ZZ1_SDSDIST_MSE,
         CE0_ZZ1_SERVTYPE_MSE    TYPE  CE01000-ZZ1_SERVTYPE_MSE,
         CE0_ZZ1_SHOPTYPE        TYPE  CE01000-ZZ1_SHOPTYPE,
         CE0_ZZ1_SSERIES         TYPE  CE01000-ZZ1_SSERIES,
         CE0_ZZ1_ZZCAV_MSE       TYPE  CE01000-ZZ1_ZZCAV_MSE,
         CE0_ZZ1_ZZINNI_MSE      TYPE  CE01000-ZZ1_ZZINNI_MSE,
         CE0_ZZ1_ZZIUT_MSE       TYPE  CE01000-ZZ1_ZZIUT_MSE,
         CE0_ZZ1_ZZPHA_MSE       TYPE  CE01000-ZZ1_ZZPHA_MSE,
         CE0_ZZ1_ZZPMT_MSE       TYPE  CE01000-ZZ1_ZZPMT_MSE,
         CE0_KMVTNR              TYPE  CE01000-KMVTNR,
         CE0_ZZ1_ZZREFTN_MSE     TYPE  CE01000-ZZ1_ZZREFTN_MSE,
         CE0_ZZ1_MVGR1           TYPE  CE01000-ZZ1_MVGR1,

         WT_WITHT01              TYPE WITH_ITEM-WITHT,
         WT_WITHCD01             TYPE WITH_ITEM-WT_WITHCD,
         WT_QSSHB01              TYPE WITH_ITEM-WT_QSSHB,
         WT_QSSHH01              TYPE WITH_ITEM-WT_QSSHH,
         WT_QBSHB01              TYPE WITH_ITEM-WT_QBSHB,
         WT_QBSHH01              TYPE WITH_ITEM-WT_QBSHH,

         WT_WITHT02              TYPE WITH_ITEM-WITHT,
         WT_WITHCD02             TYPE WITH_ITEM-WT_WITHCD,
         WT_QSSHB02              TYPE WITH_ITEM-WT_QSSHB,
         WT_QSSHH02              TYPE WITH_ITEM-WT_QSSHH,
         WT_QBSHB02              TYPE WITH_ITEM-WT_QBSHB,
         WT_QBSHH02              TYPE WITH_ITEM-WT_QBSHH,

         WT_WITHT03              TYPE WITH_ITEM-WITHT,
         WT_WITHCD03             TYPE WITH_ITEM-WT_WITHCD,
         WT_QSSHB03              TYPE WITH_ITEM-WT_QSSHB,
         WT_QSSHH03              TYPE WITH_ITEM-WT_QSSHH,
         WT_QBSHB03              TYPE WITH_ITEM-WT_QBSHB,
         WT_QBSHH03              TYPE WITH_ITEM-WT_QBSHH,

         WT_WITHT04              TYPE WITH_ITEM-WITHT,
         WT_WITHCD04             TYPE WITH_ITEM-WT_WITHCD,
         WT_QSSHB04              TYPE WITH_ITEM-WT_QSSHB,
         WT_QSSHH04              TYPE WITH_ITEM-WT_QSSHH,
         WT_QBSHB04              TYPE WITH_ITEM-WT_QBSHB,
         WT_QBSHH04              TYPE WITH_ITEM-WT_QBSHH,
* One-Time Account/Individual Payee
         ONETIME_NAME1           TYPE BSEC-NAME1,
         ONETIME_NAME2           TYPE BSEC-NAME2,
         ONETIME_NAME3           TYPE BSEC-NAME3,
         ONETIME_NAME4           TYPE BSEC-NAME4,
         ONETIME_STREET          TYPE BSEC-ORT01,
         ONETIME_CITY            TYPE BSEC-ORT01,
         ONETIME_POSTAL          TYPE BSEC-PSTL2,
         ONETIME_COUNTRY         TYPE BSEC-LAND1,
         ONETIME_TAX             TYPE BSEC-STCD3,
         ONETIME_BANK_KEY        TYPE BSEC-BANKL,
         ONETIME_BANK_ACCT_NO    TYPE BSEC-BANKN,
         ONETIME_BANK_COUNTRY    TYPE BSEC-BANKS,
*--Bill of Exchange Details
         EXCHG_DUE_ON            TYPE BSEG-ZFBDT,
         EXCHG_CHEQUE_NUMBER     TYPE BSED-BOENO,
         EXCHG_CHEQUE_BANKKEY    TYPE BSED-BANK,
         EXCHG_ACCOUNT_NUMBER    TYPE BSED-ACCOU,
*-- Additional Field
         PSEGMENT                TYPE COBL-PSEGMENT,
         GL_ACCOUNT              TYPE BSEG-HKONT,
*---beg additional items
         KOART                   TYPE  TBSL-KOART,
         SHKZG                   TYPE  TBSL-SHKZG,
         AKONT                   TYPE  LFB1-AKONT,
         LINETYPE(3)             TYPE C,
         ROWNO                   TYPE  TS_RESULT-ROWNO,
*---end additional items

       END OF TS_ITEM_DATA.

TYPES: BEGIN OF TS_WITHT,
         WITHT TYPE  LFBW-WITHT,
       END OF TS_WITHT.
TYPES: TT_WITHT  TYPE  STANDARD TABLE OF TS_WITHT
                         WITH DEFAULT KEY.

TYPES: BEGIN OF TS_ITEM,
         DATA  TYPE  TS_ITEM_DATA,
         WITHT TYPE  TT_WITHT,
       END OF TS_ITEM.
TYPES: TT_ITEM  TYPE  STANDARD TABLE OF TS_ITEM
                        WITH DEFAULT KEY.

TYPES: BEGIN OF TS_FIDOC,
         BUKRS TYPE  BKPF-BUKRS,
         BELNR TYPE  BKPF-BELNR,
         GJAHR TYPE  BKPF-GJAHR,
       END OF TS_FIDOC.

TYPES: BEGIN OF TS_DATA,
         KEY    TYPE  TS_KEY,
         HEAD   TYPE  TS_HEAD,
         ITEM   TYPE  TT_ITEM,
         MESSG  TYPE  TT_MESSG,
         RESULT TYPE  TS_FIDOC,
       END OF TS_DATA.
TYPES: TT_DATA  TYPE  STANDARD TABLE OF TS_DATA.

* *****************************
* Types for Excel Template
* !!!! Changing field name impact to validation logic !!!
* *****************************
TYPES: BEGIN OF TS_TEMPLATE ##NEEDED,
*--Header-----------------
         TRANS_NO                TYPE CHAR10,
         BUKRS                   TYPE BKPF-BUKRS,
         BLART                   TYPE BKPF-BLART,
         BLDAT                   TYPE BKPF-BLDAT,
         BUDAT                   TYPE BKPF-BUDAT,
         MONAT                   TYPE BKPF-MONAT,
         WAERS                   TYPE BKPF-WAERS,
         KURSF                   TYPE BKPF-KURSF,
         WWERT                   TYPE BKPF-WWERT,
         LDGRP                   TYPE BKPF-LDGRP,
         XBLNR                   TYPE BKPF-XBLNR,
         BKTXT                   TYPE BKPF-BKTXT,
         BRNCH                   TYPE BKPF-BRNCH,
*--Item--------------------
         NEWBS                   TYPE RF05A-NEWBS,  "Posting Key
         NEWKO                   TYPE RF05A-NEWKO,  "HKONT Account
         UMSKZ                   TYPE BSEG-UMSKZ,   "Special GL
         BCODE                   TYPE BSEG-J_1TPBUPL, "Branch code
         WRBTR                   TYPE BSEG-WRBTR,
         DMBTR                   TYPE BSEG-DMBTR,
         MWSKZ                   TYPE BSEG-MWSKZ,
         FWBAS                   TYPE BSEG-FWBAS,
         HWBAS                   TYPE BSEG-HWBAS,
         WMWST                   TYPE BSEG-WMWST,
         MWSTS                   TYPE BSEG-MWSTS,
         PARGB                   TYPE BSEG-PARGB,
         VALUT                   TYPE BSEG-VALUT,
         ZUONR                   TYPE BSEG-ZUONR,
         SGTXT                   TYPE BSEG-SGTXT,
         ZFBDT                   TYPE BSEG-ZFBDT,
         ZTERM                   TYPE BSEG-ZTERM,
         ZLSPR                   TYPE BSEG-ZLSPR,
         ZLSCH                   TYPE BSEG-ZLSCH,
         XREF1                   TYPE BSEG-XREF1,
         XREF2                   TYPE BSEG-XREF2,
         XREF3                   TYPE BSEG-XREF3,
         ANLN1                   TYPE BSEG-ANLN1,
         ANLN2                   TYPE BSEG-ANLN2,
         BZDAT                   TYPE BSEG-BZDAT,
         ANBWA                   TYPE BSEG-ANBWA,

*---CO Objects--------------
         KOSTL                   TYPE BSEG-KOSTL,
         PRCTR                   TYPE BSEG-PRCTR,
         POSID                   TYPE POSID,  "WBS
         AUFNR                   TYPE BSEG-AUFNR,

*---Profit-segment----------
         CE0_KNDNR               TYPE  CE01000-KNDNR,
         CE0_ARTNR               TYPE  CE01000-ARTNR,
         CE0_FKART               TYPE  CE01000-FKART,
         CE0_KAUFN               TYPE  CE01000-KAUFN,
         CE0_KDPOS               TYPE  CE01000-KDPOS,
         CE0_RKAUFNR             TYPE  CE01000-RKAUFNR,
         CE0_WERKS               TYPE  CE01000-WERKS,
         CE0_FKBER               TYPE  CE01000-FKBER,
         CE0_VKORG               TYPE  CE01000-VKORG,
         CE0_VTWEG               TYPE  CE01000-VTWEG,
         CE0_SPART               TYPE  CE01000-SPART,
         CE0_PSPNR               TYPE  CE01000-PSPNR,
         CE0_COPA_KOSTL          TYPE  CE01000-COPA_KOSTL,
         CE0_KSTRG               TYPE  CE01000-KSTRG,
         CE0_PRCTR               TYPE  CE01000-PRCTR,
         CE0_PPRCTR              TYPE  CE01000-PPRCTR,
         CE0_SERVICE_DOC_TYPE    TYPE  CE01000-SERVICE_DOC_TYPE,
         CE0_SERVICE_DOC_ID      TYPE  CE01000-SERVICE_DOC_ID,
         CE0_SERVICE_DOC_ITEM_ID TYPE  CE01000-SERVICE_DOC_ITEM_ID,
         CE0_MATKL               TYPE  CE01000-MATKL,
         CE0_VKBUR               TYPE  CE01000-VKBUR,
         CE0_VKGRP               TYPE  CE01000-VKGRP,
         CE0_PRODH               TYPE  CE01000-PRODH,
         CE0_KUNWE               TYPE  CE01000-KUNWE,
         CE0_WWLO                TYPE  CE01000-WWLO,
         CE0_LAND1               TYPE  CE01000-LAND1,
         CE0_PAPH1               TYPE  CE01000-PAPH1,
         CE0_PAPH2               TYPE  CE01000-PAPH2,
         CE0_PAPH3               TYPE  CE01000-PAPH3,
         CE0_REGIO               TYPE  CE01000-REGIO,
         CE0_ZZ1_ACTTYPE_MSE     TYPE  CE01000-ZZ1_ACTTYPE_MSE,
         CE0_ZZ1_APPLTN          TYPE  CE01000-ZZ1_APPLTN,
         CE0_ZZ1_FISCYR_MSE      TYPE  CE01000-ZZ1_FISCYR_MSE,
         CE0_ZZ1_ITMTYPE_MSE     TYPE  CE01000-ZZ1_ITMTYPE_MSE,
         CE0_ZZ1_PROJTYPE_MSE    TYPE  CE01000-ZZ1_PROJTYPE_MSE,
         CE0_ZZ1_PROJ_MSE        TYPE  CE01000-ZZ1_PROJ_MSE,
         CE0_ZZ1_SDSDIST_MSE     TYPE  CE01000-ZZ1_SDSDIST_MSE,
         CE0_ZZ1_SERVTYPE_MSE    TYPE  CE01000-ZZ1_SERVTYPE_MSE,
         CE0_ZZ1_SHOPTYPE        TYPE  CE01000-ZZ1_SHOPTYPE,
         CE0_ZZ1_SSERIES         TYPE  CE01000-ZZ1_SSERIES,
         CE0_ZZ1_ZZCAV_MSE       TYPE  CE01000-ZZ1_ZZCAV_MSE,
         CE0_ZZ1_ZZINNI_MSE      TYPE  CE01000-ZZ1_ZZINNI_MSE,
         CE0_ZZ1_ZZIUT_MSE       TYPE  CE01000-ZZ1_ZZIUT_MSE,
         CE0_ZZ1_ZZPHA_MSE       TYPE  CE01000-ZZ1_ZZPHA_MSE,
         CE0_ZZ1_ZZPMT_MSE       TYPE  CE01000-ZZ1_ZZPMT_MSE,
         CE0_KMVTNR              TYPE  CE01000-KMVTNR,
         CE0_ZZ1_ZZREFTN_MSE     TYPE  CE01000-ZZ1_ZZREFTN_MSE,

*--Withholding Tax Data---------
         WT_WITHT01              TYPE WITH_ITEM-WITHT,
         WT_WITHCD01             TYPE WITH_ITEM-WT_WITHCD,
         WT_QSSHB01              TYPE WITH_ITEM-WT_QSSHB,
         WT_QSSHH01              TYPE WITH_ITEM-WT_QSSHH,
         WT_QBSHB01              TYPE WITH_ITEM-WT_QBSHB,
         WT_QBSHH01              TYPE WITH_ITEM-WT_QBSHH,

         WT_WITHT02              TYPE WITH_ITEM-WITHT,
         WT_WITHCD02             TYPE WITH_ITEM-WT_WITHCD,
         WT_QSSHB02              TYPE WITH_ITEM-WT_QSSHB,
         WT_QSSHH02              TYPE WITH_ITEM-WT_QSSHH,
         WT_QBSHB02              TYPE WITH_ITEM-WT_QBSHB,
         WT_QBSHH02              TYPE WITH_ITEM-WT_QBSHH,

         WT_WITHT03              TYPE WITH_ITEM-WITHT,
         WT_WITHCD03             TYPE WITH_ITEM-WT_WITHCD,
         WT_QSSHB03              TYPE WITH_ITEM-WT_QSSHB,
         WT_QSSHH03              TYPE WITH_ITEM-WT_QSSHH,
         WT_QBSHB03              TYPE WITH_ITEM-WT_QBSHB,
         WT_QBSHH03              TYPE WITH_ITEM-WT_QBSHH,

         WT_WITHT04              TYPE WITH_ITEM-WITHT,
         WT_WITHCD04             TYPE WITH_ITEM-WT_WITHCD,
         WT_QSSHB04              TYPE WITH_ITEM-WT_QSSHB,
         WT_QSSHH04              TYPE WITH_ITEM-WT_QSSHH,
         WT_QBSHB04              TYPE WITH_ITEM-WT_QBSHB,
         WT_QBSHH04              TYPE WITH_ITEM-WT_QBSHH,
*--One-Time Account/Individual Payee
         ONETIME_NAME1           TYPE BSEC-NAME1,
         ONETIME_NAME2           TYPE BSEC-NAME2,
         ONETIME_NAME3           TYPE BSEC-NAME3,
         ONETIME_NAME4           TYPE BSEC-NAME4,
         ONETIME_STREET          TYPE BSEC-ORT01,
         ONETIME_CITY            TYPE BSEC-ORT01,
         ONETIME_POSTAL          TYPE BSEC-PSTL2,
         ONETIME_COUNTRY         TYPE BSEC-LAND1,
         ONETIME_TAX             TYPE BSEC-STCD3,
         ONETIME_BANK_KEY        TYPE BSEC-BANKL,
         ONETIME_BANK_ACCT_NO    TYPE BSEC-BANKN,
         ONETIME_BANK_COUNTRY    TYPE BSEC-BANKS,
*--Bill of Exchange Details
         EXCHG_DUE_ON            TYPE BSEG-ZFBDT,
         EXCHG_CHEQUE_NUMBER     TYPE BSED-BOENO,
         EXCHG_CHEQUE_BANKKEY    TYPE BSED-BANK,
         EXCHG_ACCOUNT_NUMBER    TYPE BSED-ACCOU,
*-- Additional Field
         PSEGMENT                TYPE COBL-PSEGMENT,
         ZZ1_MVGR1               TYPE CE01000-ZZ1_MVGR1,
         MATNR                   TYPE BSEG-MATNR,
         GL_ACCOUNT              TYPE BSEG-HKONT,
       END OF TS_TEMPLATE.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE  TYPE  CHAR1     VALUE 'X',
  GC_TCODE TYPE  SY-TCODE  VALUE 'ZSDSFI024',

  GC_NOCHG TYPE CHAR5      VALUE '-'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
##NEEDED
DATA:
  GT_RESULT TYPE TT_RESULT,
  GT_TBSL   TYPE TABLE OF TBSL,
  GS_TBSL   TYPE TBSL.
*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
##NEEDED
DATA:
  GS_PROC TYPE  TS_PROC_INFO,
  GS_SUM  TYPE  TS_SUM.
*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
##NEEDED
DATA: GS_DOCUMENTHEADER    TYPE BAPIACHE09,
      GS_CUSTOMERCPD       TYPE BAPIACPA09,
      GT_ACCOUNTGL         TYPE TABLE OF BAPIACGL09,
      GT_ACCOUNTRECEIVABLE TYPE TABLE OF BAPIACAR09,
      GS_ACCOUNTRECEIVABLE TYPE BAPIACAR09,
      GT_ACCOUNTPAYABLE    TYPE TABLE OF BAPIACAP09,
      GS_ACCOUNTPAYABLE    TYPE BAPIACAP09,
      GT_ACCOUNTTAX        TYPE TABLE OF BAPIACTX09,
      GS_ACCOUNTTAX        TYPE BAPIACTX09,
      GT_CURRENCYAMOUNT    TYPE TABLE OF BAPIACCR09,
      GS_CURRENCYAMOUNT    TYPE BAPIACCR09,
      GT_CRITERIA          TYPE TABLE OF BAPIACKEC9,

      GT_EXTENSION1        TYPE TABLE OF BAPIACEXTC,
      GT_EXTENSION2        TYPE TABLE OF BAPIPAREX,
      GS_EXTENSION2        TYPE BAPIPAREX,
      GT_RETURN            TYPE TABLE OF BAPIRET2,
      GT_ACCOUNTWT         TYPE TABLE OF BAPIACWT09,
      GF_ITEMNO_ACC        TYPE POSNR_ACC,

      GF_DOC_CUR           TYPE WAERS,
      GF_DOC_EXCH          TYPE KURSF.
##NEEDED
DATA:
  GS_GEN_INFO   TYPE BAPIFAPO_GEN_INFO,
  GS_RETIREMENT TYPE BAPIFAPO_RET,
  GS_FURTHER    TYPE BAPIFAPO_ADD_INFO.

DATA: GV_K2_AMOUNT TYPE ZSDSFIT060-SUM_AMT.
*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_KOART_VEND TYPE  TBSL-KOART  VALUE 'K',
  GC_KOART_CUST TYPE  TBSL-KOART  VALUE 'D',
  GC_KOART_GL   TYPE  TBSL-KOART  VALUE 'S',
  GC_KOART_AA   TYPE  TBSL-KOART  VALUE 'A',
  GC_FB03       TYPE  SY-TCODE    VALUE 'FB03'.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*

CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSFIS086'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 25,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 75.
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

DEFINE CONVERT_ALPHA_INPUT.

  if &1 is not initial.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = &1
      importing
        output = &2.
  endif.

END-OF-DEFINITION.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* Text-s02: File Selection
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME .
  PARAMETERS:
    P_IFILE TYPE  STRING LOWER CASE MODIF ID LOC,
    P_BGROW TYPE I DEFAULT 8,
*BOI F36K916699
    P_JVDOC TYPE ZSDSDE_JV_DOC OBLIGATORY.
*EOI F36K916699
SELECTION-SCREEN END OF BLOCK B2.
* Text-s03: Processing Options
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-S01.
  PARAMETERS:
    CB_TEST TYPE CHAR1 RADIOBUTTON GROUP G2 DEFAULT 'X'
                                    USER-COMMAND DMY,
    CB_POST TYPE CHAR1 RADIOBUTTON GROUP G2.
SELECTION-SCREEN END OF BLOCK B3.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.

  SELECT * FROM TBSL
    INTO TABLE GT_TBSL
   ORDER BY BSCHL.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_IFILE.
* List input File
  PERFORM F_LIST_IFILE CHANGING P_IFILE.

AT SELECTION-SCREEN.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Processing Data
  PERFORM F_PROCESS_DATA  USING  CB_TEST
                        CHANGING GT_RESULT
                                 GS_PROC
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
* Display Processing Result
  PERFORM F_DISPLAY_RESULT USING GT_RESULT.


*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE ZSDSCAI9990 ##INCL_OK.
*&---------------------------------------------------------------------*
*& Form F_AUTHORIZE_CHECK
*&---------------------------------------------------------------------*
*& Check Authorization on t-code
*&---------------------------------------------------------------------*
FORM F_AUTHORIZE_CHECK  USING UF_TCODE  TYPE  SY-TCODE.
  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
    ID 'TCD' FIELD UF_TCODE.

  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE S172(00) WITH UF_TCODE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_LIST_IFILE
*&---------------------------------------------------------------------*
*& Popup for Input file selection
*&---------------------------------------------------------------------*\
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
      FILE_FILTER             = 'Excel File(XLSX)|*.XLSX' ##NO_TEXT
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
*& Form F_PROCESS_DATA
*&---------------------------------------------------------------------*
*& Processing data
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA  USING  UF_TEST   TYPE FLAG
                   CHANGING CT_RESULT TYPE TT_RESULT
                            CS_PROC   TYPE TS_PROC_INFO
                            CS_SUM    TYPE TS_SUM.
  DATA:
    LT_XLSX   TYPE  TT_XLSX,
    LT_RESULT TYPE  TT_RESULT,
    LT_DATA   TYPE  TT_DATA.

* Initialize Output
  CLEAR: CT_RESULT,
         CS_PROC.
  CLEAR: CS_SUM.
  CLEAR: GV_K2_AMOUNT.

* --------------------------------
* Step1: Read Input file data
* --------------------------------
  PERFORM F_READ_FILE CHANGING LT_XLSX.
  IF LT_XLSX IS INITIAL.
    RETURN.
  ENDIF.

*-Assign data
  PERFORM F_ASSIGN_FI_STRUC  USING LT_XLSX
                          CHANGING LT_DATA
*                                   LT_WBS
                                   LT_RESULT.
  IF LT_RESULT IS NOT INITIAL.
*       Collect Final Result
    INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.
    CS_PROC-COUNT-ERROR = CS_PROC-COUNT-ERROR + LINES( LT_RESULT ).
  ENDIF.

  PERFORM F_VALID_JV_K2 USING LT_DATA
                              ABAP_TRUE.

* --------------------------------
* Step3: Upload data from file
* --------------------------------
  PERFORM F_POST_DATA USING  LT_DATA
                             UF_TEST
                    CHANGING CT_RESULT
                             CS_SUM .

  IF CB_TEST IS INITIAL.
    PERFORM F_VALID_JV_K2 USING LT_DATA
                                SPACE.
  ENDIF.

* Assign Processing Info
  CS_PROC-DATUM = SY-DATUM.
  CS_PROC-UZEIT = SY-UZEIT.
  CS_PROC-UNAME = SY-UNAME.
  CS_PROC-IFILE = P_IFILE.

  IF CB_TEST EQ GC_TRUE.
*   Text-x01: Test Run
    CS_PROC-MODE  =  TEXT-X01 .
  ELSE.
*   Text-x02: Production Run
    CS_PROC-MODE  = TEXT-X02 .
  ENDIF.

  CS_PROC-COUNT-TOTAL = CS_PROC-COUNT-SUCCS + CS_PROC-COUNT-ERROR.

* Show Message Complete
* Text-i07: Processing completed.
  MESSAGE S000(ZSDSPS01) WITH TEXT-I07 SPACE SPACE SPACE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_FILE
*&---------------------------------------------------------------------*
*& Read File into Text Table
*&---------------------------------------------------------------------*
FORM F_READ_FILE  CHANGING CT_XLSX TYPE  TT_XLSX.

  DATA:
    LF_FILENAME   TYPE  STRING.

* Initialize Output
  CLEAR: CT_XLSX.

* Show progress
* Text-p01 : Reading Input file . . .
  MC_SHOW_PROGRESS 15 TEXT-P01.

  CLEAR: LF_FILENAME.
  LF_FILENAME = P_IFILE.

  CALL METHOD ZCL_SDSCA_UTILITIES=>READ_XLSX_INTO_ITAB
    EXPORTING
      IF_FILENAME                 = LF_FILENAME
      IF_READ_ACTIVE_WORKSHEET    = 'X'
*     IF_XSTRING                  =
*     IT_WORKSHEET                =
    IMPORTING
      ET_XLSX_DATA                = CT_XLSX
    EXCEPTIONS
      MISSING_FILENAME_OR_XSTRING = 1
      ERROR_READ_FILENAME         = 2
      NO_DATA_FOUND               = 3
      OTHERS                      = 4.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ASSIGN_FI_STRUC
*&---------------------------------------------------------------------*
*& Assign data to structure
*&---------------------------------------------------------------------*
FORM F_ASSIGN_FI_STRUC  USING    UT_XLSX  TYPE  TT_XLSX
                        CHANGING
                                 CT_DATA  TYPE TT_DATA
*                                 CT_WBS
                                 CT_RESULT  TYPE  TT_RESULT.
  DATA:
    LT_RESULT  TYPE  TT_RESULT.


* Initialize Output
  CLEAR:
         CT_DATA,
*         CT_WBS,
         CT_RESULT.

* Show progress
* Text-p02 : Validating Input file . . .
  MC_SHOW_PROGRESS 40 TEXT-P02.

* Read Project Def Sheet
  READ TABLE UT_XLSX ASSIGNING FIELD-SYMBOL(<L_SHEET>) INDEX 1.

  IF SY-SUBRC EQ 0.
    PERFORM F_ASSIGN_DATA USING  <L_SHEET>
                         CHANGING
                                  CT_DATA
                                  LT_RESULT.
    INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_assign_data
*&---------------------------------------------------------------------*
*& Assign data
*&---------------------------------------------------------------------*

FORM F_ASSIGN_DATA  USING  US_XLSX    TYPE TS_XLSX
                 CHANGING  CT_DATA    TYPE TT_DATA
                           CT_RESULT  TYPE TT_RESULT.

  DATA:
    LS_DATA  TYPE TS_DATA,
    LS_MESSG TYPE TS_MESSG.

  DATA:
    LF_ROW_START TYPE I,
    LF_COL_START TYPE I VALUE 2,
    LF_COL_INDEX TYPE I,
    LF_ROWNO     TYPE TS_RESULT-ROWNO,
    LF_FNAME     TYPE TS_FNAME,
*    LF_INVALID   TYPE FLAG,
    LF_MSGTX     TYPE TS_MSG_TXT.

  DATA:
    LREF_STRUC    TYPE REF TO CL_ABAP_STRUCTDESCR,
    LREF_VALIDATE TYPE REF TO ZCL_SDSCA_DATA_VALIDATION.
  ##NEEDED
  DATA:
    LS_KEY       TYPE  TS_KEY,
    LS_ITEM      TYPE TS_ITEM,
    LS_ITEM_DATA TYPE  TS_ITEM_DATA.

  DATA:
    LF_KUNNR  TYPE  KNA1-KUNNR,
    LF_LIFNR  TYPE  LFA1-LIFNR,
    LF_SAKNR  TYPE  SKA1-SAKNR,
    LF_AMOUNT TYPE  F.

* Initialize Output
  CLEAR:
         CT_DATA,
         CT_RESULT.

  LF_ROW_START = P_BGROW .

  ASSIGN US_XLSX-DATA->* TO FIELD-SYMBOL(<L_SHEET_DATA>).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  CREATE OBJECT LREF_VALIDATE.

  LOOP AT <L_SHEET_DATA> ASSIGNING FIELD-SYMBOL(<L_ROW>).

*   Keep Row no
    LF_ROWNO = SY-TABIX.

    CHECK LF_ROWNO GE LF_ROW_START.

    IF LREF_STRUC IS INITIAL.
      LREF_STRUC ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( P_DATA = <L_ROW> ).
    ENDIF.

*   -----------
*   Columns
*   -----------
    CLEAR:
      LS_DATA,
      LF_MSGTX.

    LOOP AT LREF_STRUC->COMPONENTS ASSIGNING FIELD-SYMBOL(<L_COMP>). "#EC CI_NESTED.

      CHECK SY-TABIX GE LF_COL_START.

      LF_COL_INDEX = SY-TABIX - LF_COL_START + 1.

      PERFORM F_GET_TARGET_FIELD  USING  'TS_TEMPLATE'
                                         LF_COL_INDEX
                                CHANGING LF_FNAME.
*     Assign Source Field
      ASSIGN <L_ROW>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

*     Check No change value?
      CHECK <L_FIELD> NE GC_NOCHG.



      CASE LF_FNAME.
*---Fill data from Template Header to Header Data--------
        WHEN 'TRANS_NO' .
          LS_KEY-TRANS_NO  = <L_FIELD>.

        WHEN 'BUKRS'.
          PERFORM F_VALIDATE_COMPCODE  USING  <L_FIELD>
                                     CHANGING LS_DATA-HEAD-BUKRS
                                              LS_DATA-HEAD-WAERS_BUKRS
                                              LF_MSGTX.

        WHEN 'BLART'.
          PERFORM F_VALIDATE_DOCTYPE  USING  <L_FIELD>
                                    CHANGING LS_DATA-HEAD-BLART
                                             LF_MSGTX.

        WHEN 'BLDAT'.
          PERFORM F_VALIDATE_DATE  USING  <L_FIELD>
                                 CHANGING LS_DATA-HEAD-BLDAT
                                          LF_MSGTX.

        WHEN 'BUDAT'.
          PERFORM F_VALIDATE_DATE  USING  <L_FIELD>
                                 CHANGING LS_DATA-HEAD-BUDAT
                                          LF_MSGTX.

        WHEN 'MONAT'.
          LS_DATA-HEAD-MONAT  = <L_FIELD>.

        WHEN 'WAERS'.
*       Validate Currency
          PERFORM F_VALIDATE_CURENCY  USING  <L_FIELD>
                                    CHANGING LS_DATA-HEAD-WAERS
                                             LF_MSGTX.

        WHEN 'KURSF'.
*       Validate Amount
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_DATA-HEAD-KURSF
                                            LF_MSGTX.
          IF LS_DATA-HEAD-WAERS NE 'THB' AND
             LS_DATA-HEAD-KURSF IS INITIAL .

            PERFORM F_SET_EXCHANGE_RATE USING LS_DATA-HEAD-WAERS
                                              LS_DATA-HEAD-BUDAT
                                         CHANGING
                                                 LS_DATA-HEAD-KURSF  .
          ENDIF.

        WHEN 'WWERT'.
          PERFORM F_VALIDATE_DATE  USING  <L_FIELD>
                                 CHANGING LS_DATA-HEAD-WWERT
                                          LF_MSGTX.

        WHEN 'LDGRP'.
          LS_DATA-HEAD-LDGRP  = <L_FIELD>.

        WHEN 'XBLNR'.
*          LS_DATA-HEAD-XBLNR  = <L_FIELD>.
          LS_DATA-HEAD-XBLNR  = P_JVDOC.

        WHEN 'BKTXT'.
          LS_DATA-HEAD-BKTXT  = <L_FIELD>.

        WHEN 'BRNCH'.
          LS_DATA-HEAD-BRNCH  = <L_FIELD>.

*---------ITEM-----------
        WHEN 'NEWBS'.
          PERFORM F_VALIDATE_POSTINGKEY  USING  <L_FIELD>
                                       CHANGING LS_ITEM_DATA-NEWBS
                                                LS_ITEM_DATA-KOART
                                                LS_ITEM_DATA-SHKZG
                                                LF_MSGTX.
        WHEN 'NEWKO'.
          CASE LS_ITEM_DATA-KOART.
            WHEN GC_KOART_VEND.
              PERFORM F_VALIDATE_VENDOR  USING   <L_FIELD>
                                       CHANGING LF_LIFNR
                                                LF_MSGTX.
              LS_ITEM_DATA-NEWKO = LF_LIFNR.

            WHEN GC_KOART_CUST.
              PERFORM F_VALIDATE_CUSTOMER  USING  <L_FIELD>
                                         CHANGING LF_KUNNR
                                                  LF_MSGTX.
              LS_ITEM_DATA-NEWKO = LF_KUNNR.

            WHEN GC_KOART_GL.
              PERFORM F_VALIDATE_GLACCOUNT  USING  <L_FIELD>
                                                   LS_DATA-HEAD-BUKRS
                                          CHANGING LF_SAKNR
                                                   LF_MSGTX.
              LS_ITEM_DATA-NEWKO = LF_SAKNR.

            WHEN GC_KOART_AA.

              LS_ITEM_DATA-NEWKO = <L_FIELD>.

            WHEN OTHERS.
              LS_ITEM_DATA-NEWKO = <L_FIELD>.
          ENDCASE.

        WHEN  'BSCHL' .
          LS_ITEM_DATA-BSCHL  = <L_FIELD>.

        WHEN  'HKONT' .
          PERFORM F_VALIDATE_GLACCOUNT  USING  <L_FIELD>
                                               LS_DATA-HEAD-BUKRS
                                      CHANGING LS_ITEM_DATA-HKONT
                                               LF_MSGTX.

        WHEN  'UMSKZ' .
          LS_ITEM_DATA-UMSKZ  = <L_FIELD>.

        WHEN  'BCODE' .
          LS_ITEM_DATA-BCODE  = <L_FIELD>.


        WHEN  'WRBTR' .
          PERFORM F_VALIDATE_AMOUNT  USING  <L_FIELD>
                                   CHANGING LF_AMOUNT
                                            LF_MSGTX.
          IF LF_MSGTX IS INITIAL.
*         Convert amount into internal format based on currency key
            PERFORM F_CONVERT_INT_AMOUNT  USING  LF_AMOUNT
                                                 LS_DATA-HEAD-WAERS
                                        CHANGING LS_ITEM_DATA-WRBTR. "#EC CI_FLDEXT_OK[2610650]
          ENDIF.

        WHEN  'DMBTR' .
          PERFORM F_VALIDATE_AMOUNT  USING  <L_FIELD>
                                   CHANGING LF_AMOUNT
                                            LF_MSGTX.
          IF LF_MSGTX IS INITIAL.
*         Convert amount into internal format based on currency key
            PERFORM F_CONVERT_INT_AMOUNT  USING  LF_AMOUNT
                                                 LS_DATA-HEAD-WAERS_BUKRS
                                        CHANGING LS_ITEM_DATA-DMBTR. "#EC CI_FLDEXT_OK[2610650]
          ENDIF.

        WHEN  'MWSKZ' .
          LS_ITEM_DATA-MWSKZ  = <L_FIELD>.

        WHEN  'FWBAS' .
          PERFORM F_VALIDATE_AMOUNT  USING  <L_FIELD>
                         CHANGING LF_AMOUNT
                                  LF_MSGTX.
          IF LF_MSGTX IS INITIAL.
*         Convert amount into internal format based on currency key
            PERFORM F_CONVERT_INT_AMOUNT  USING  LF_AMOUNT
                                                 LS_DATA-HEAD-WAERS
                                        CHANGING LS_ITEM_DATA-FWBAS. "#EC CI_FLDEXT_OK[2610650]
          ENDIF.

        WHEN  'HWBAS' .
          PERFORM F_VALIDATE_AMOUNT  USING  <L_FIELD>
                                CHANGING LF_AMOUNT
                                         LF_MSGTX.
          IF LF_MSGTX IS INITIAL.
*         Convert amount into internal format based on currency key
            PERFORM F_CONVERT_INT_AMOUNT  USING  LF_AMOUNT
                                                 LS_DATA-HEAD-WAERS_BUKRS
                                        CHANGING LS_ITEM_DATA-HWBAS. "#EC CI_FLDEXT_OK[2610650]
          ENDIF.

        WHEN  'WMWST' .
          LS_ITEM_DATA-WMWST  = <L_FIELD>.

        WHEN  'MWSTS' .
          LS_ITEM_DATA-MWSTS  = <L_FIELD>.

        WHEN  'PARGB' .
          LS_ITEM_DATA-PARGB  = <L_FIELD>.

        WHEN  'VALUT' .
          PERFORM F_VALIDATE_DATE  USING  <L_FIELD>
                                 CHANGING LS_ITEM_DATA-VALUT
                                          LF_MSGTX.
        WHEN  'ZUONR' .
          LS_ITEM_DATA-ZUONR  = <L_FIELD>.

        WHEN  'SGTXT' .
          LS_ITEM_DATA-SGTXT  = <L_FIELD>.

        WHEN  'ZFBDT' .
          PERFORM F_VALIDATE_DATE  USING  <L_FIELD>
                                 CHANGING LS_ITEM_DATA-ZFBDT
                                          LF_MSGTX.

        WHEN  'ZTERM' .
          LS_ITEM_DATA-ZTERM  = <L_FIELD>.

        WHEN  'ZLSPR' .
          LS_ITEM_DATA-ZLSPR  = <L_FIELD>.

        WHEN  'ZLSCH' .
          LS_ITEM_DATA-ZLSCH  = <L_FIELD>.

        WHEN  'XREF1' .
          LS_ITEM_DATA-XREF1  = <L_FIELD>.

        WHEN  'XREF2' .
          LS_ITEM_DATA-XREF2  = <L_FIELD>.

        WHEN  'XREF3' .
          LS_ITEM_DATA-XREF3  = <L_FIELD>.

        WHEN  'ANLN1' .
          PERFORM F_VALIDATE_ASSETNO USING <L_FIELD>
                                  CHANGING LS_ITEM_DATA-ANLN1
                                           LF_MSGTX .

        WHEN  'ANLN2' .

          CONVERT_ALPHA_INPUT  <L_FIELD>  LS_ITEM_DATA-ANLN2.

        WHEN  'BZDAT' .
          PERFORM F_VALIDATE_DATE  USING  <L_FIELD>
                                 CHANGING LS_ITEM_DATA-BZDAT
                                          LF_MSGTX.
        WHEN  'ANBWA' .
          LS_ITEM_DATA-ANBWA  = <L_FIELD>.

*------CO-Objects-------------
        WHEN 'KOSTL'.
          PERFORM F_VALIDATE_COSTCTR  USING  <L_FIELD>
                                             LS_DATA-HEAD-BUKRS
                                    CHANGING LS_ITEM_DATA-KOSTL
                                              LF_MSGTX.

        WHEN 'PRCTR'.
          PERFORM F_VALIDATE_PROFITCTR  USING  <L_FIELD>
                                               LS_DATA-HEAD-BUKRS
                                      CHANGING LS_ITEM_DATA-PRCTR
                                               LF_MSGTX.

        WHEN 'POSID'.  "WBS
          LS_ITEM_DATA-POSID  = <L_FIELD> .

        WHEN 'AUFNR'.
          PERFORM F_VALIDATE_ORDER  USING  <L_FIELD>
                                  CHANGING LS_ITEM_DATA-AUFNR
                                            LF_MSGTX.

*------Profit-segment----
        WHEN 'CE0_KNDNR' .
          PERFORM F_VALIDATE_CUSTOMER  USING <L_FIELD>
                                    CHANGING LS_ITEM_DATA-CE0_KNDNR
                                             LF_MSGTX.
*          LS_ITEM_DATAX-CE0_KNDNR  = GC_TRUE.
        WHEN 'CE0_ARTNR' .
          PERFORM F_VALIDATE_MATNR  USING <L_FIELD>
                                 CHANGING LS_ITEM_DATA-CE0_ARTNR
                                          LF_MSGTX.
        WHEN 'CE0_FKART' .
          PERFORM F_VALIDATE_BILLING_TYPE  USING <L_FIELD>
                                        CHANGING LS_ITEM_DATA-CE0_FKART
                                                 LF_MSGTX.
        WHEN 'CE0_KAUFN'  .
          PERFORM F_VALIDATE_SALES_ORDER  USING <L_FIELD>
                                       CHANGING LS_ITEM_DATA-CE0_KAUFN
                                                LF_MSGTX.
        WHEN 'CE0_KDPOS'               .
          LS_ITEM_DATA-CE0_KDPOS  = <L_FIELD> .

        WHEN 'CE0_RKAUFNR'             .
          PERFORM F_VALIDATE_ORDER  USING <L_FIELD>
                                 CHANGING LS_ITEM_DATA-CE0_RKAUFNR
                                          LF_MSGTX.
        WHEN 'CE0_WERKS'   .
          PERFORM F_VALIDATE_PLANT  USING <L_FIELD>
                                 CHANGING LS_ITEM_DATA-CE0_WERKS
                                          LF_MSGTX      .
        WHEN 'CE0_FKBER'               .
          LS_ITEM_DATA-CE0_FKBER  = <L_FIELD> .

        WHEN 'CE0_VKORG'               .
          PERFORM F_VALIDATE_SALESORG  USING <L_FIELD>
                                    CHANGING LS_ITEM_DATA-CE0_VKORG
                                             LF_MSGTX.

        WHEN 'CE0_VTWEG'  .
          PERFORM F_VALIDATE_DISTCHAN  USING <L_FIELD>
                                    CHANGING LS_ITEM_DATA-CE0_VTWEG
                                             LF_MSGTX.

*          LS_ITEM_DATAX-CE0_VTWEG  = GC_TRUE.
        WHEN 'CE0_SPART'               .
          PERFORM F_VALIDATE_DIVISION  USING <L_FIELD>
                                    CHANGING LS_ITEM_DATA-CE0_SPART
                                             LF_MSGTX.
        WHEN 'CE0_PSPNR' .
          LS_ITEM_DATA-CE0_PSPNR  = <L_FIELD> .

        WHEN 'CE0_COPA_KOSTL'  .
          PERFORM F_VALIDATE_COSTCTR  USING <L_FIELD>
                                            LS_DATA-HEAD-BUKRS
                                   CHANGING LS_ITEM_DATA-CE0_COPA_KOSTL
                                            LF_MSGTX.

        WHEN 'CE0_KSTRG'  .
          LS_ITEM_DATA-CE0_KSTRG = <L_FIELD> .

        WHEN 'CE0_PRCTR'               .
          PERFORM F_VALIDATE_PROFITCTR  USING  <L_FIELD>
                                               LS_DATA-HEAD-BUKRS
                                      CHANGING LS_ITEM_DATA-CE0_PRCTR
                                               LF_MSGTX.
        WHEN 'CE0_PPRCTR' .
          LS_ITEM_DATA-CE0_PPRCTR  = <L_FIELD> .

        WHEN 'CE0_SERVICE_DOC_TYPE'    .
          LS_ITEM_DATA-CE0_SERVICE_DOC_TYPE  = <L_FIELD> .

        WHEN 'CE0_SERVICE_DOC_ID'      .
          CONVERT_ALPHA_INPUT <L_FIELD> LS_ITEM_DATA-CE0_SERVICE_DOC_ID .

        WHEN 'CE0_SERVICE_DOC_ITEM_ID' .
          LS_ITEM_DATA-CE0_SERVICE_DOC_ITEM_ID  = <L_FIELD> .

        WHEN 'CE0_MATKL'               .
          LS_ITEM_DATA-CE0_MATKL  = <L_FIELD> .

        WHEN 'CE0_VKBUR'               .
          PERFORM F_VALIDATE_SALESOFF  USING <L_FIELD>
                                    CHANGING LS_ITEM_DATA-CE0_VKBUR
                                             LF_MSGTX.
        WHEN 'CE0_VKGRP'               .

          PERFORM F_VALIDATE_SALES_GROUP USING <L_FIELD>
                                      CHANGING LS_ITEM_DATA-CE0_VKGRP
                                               LF_MSGTX.
        WHEN 'CE0_PRODH'               .
          LS_ITEM_DATA-CE0_PRODH   = <L_FIELD> .

        WHEN 'CE0_KUNWE'               .
          LS_ITEM_DATA-CE0_KUNWE   = <L_FIELD> .

        WHEN 'CE0_WWLO'                .
          LS_ITEM_DATA-CE0_WWLO   = <L_FIELD> .

        WHEN 'CE0_LAND1'               .
          LS_ITEM_DATA-CE0_LAND1   = <L_FIELD> .

        WHEN 'CE0_PAPH1'               .
          LS_ITEM_DATA-CE0_PAPH1   = <L_FIELD> .

        WHEN 'CE0_PAPH2'               .
          LS_ITEM_DATA-CE0_PAPH2   = <L_FIELD> .

        WHEN 'CE0_PAPH3'               .
          LS_ITEM_DATA-CE0_PAPH3   = <L_FIELD> .

        WHEN 'CE0_REGIO'               .
          LS_ITEM_DATA-CE0_REGIO   = <L_FIELD> .

        WHEN 'CE0_ZZ1_ACTTYPE_MSE'     .
          LS_ITEM_DATA-CE0_ZZ1_ACTTYPE_MSE   = <L_FIELD> .

        WHEN 'CE0_ZZ1_APPLTN'          .
          LS_ITEM_DATA-CE0_ZZ1_APPLTN   = <L_FIELD> .

        WHEN 'CE0_ZZ1_FISCYR_MSE'      .
          LS_ITEM_DATA-CE0_ZZ1_FISCYR_MSE   = <L_FIELD> .

        WHEN 'CE0_ZZ1_ITMTYPE_MSE'     .
          LS_ITEM_DATA-CE0_ZZ1_ITMTYPE_MSE   = <L_FIELD> .

        WHEN 'CE0_ZZ1_PROJTYPE_MSE'    .
          LS_ITEM_DATA-CE0_ZZ1_PROJTYPE_MSE   = <L_FIELD> .

        WHEN 'CE0_ZZ1_PROJ_MSE'        .
          LS_ITEM_DATA-CE0_ZZ1_PROJ_MSE   = <L_FIELD> .

        WHEN 'CE0_ZZ1_SDSDIST_MSE'     .
          LS_ITEM_DATA-CE0_ZZ1_SDSDIST_MSE   = <L_FIELD> .

        WHEN 'CE0_ZZ1_SERVTYPE_MSE'    .
          LS_ITEM_DATA-CE0_ZZ1_SERVTYPE_MSE   = <L_FIELD> .

        WHEN 'CE0_ZZ1_SHOPTYPE'        .
          LS_ITEM_DATA-CE0_ZZ1_SHOPTYPE   = <L_FIELD> .

        WHEN 'CE0_ZZ1_SSERIES'         .
          LS_ITEM_DATA-CE0_ZZ1_SSERIES   = <L_FIELD> .

        WHEN 'CE0_ZZ1_ZZCAV_MSE'       .
          LS_ITEM_DATA-CE0_ZZ1_ZZCAV_MSE   = <L_FIELD> .

        WHEN 'CE0_ZZ1_ZZINNI_MSE'      .
          LS_ITEM_DATA-CE0_ZZ1_ZZINNI_MSE   = <L_FIELD> .

        WHEN 'CE0_ZZ1_ZZIUT_MSE'       .
          LS_ITEM_DATA-CE0_ZZ1_ZZINNI_MSE   = <L_FIELD> .

        WHEN 'CE0_ZZ1_ZZPHA_MSE'       .
          LS_ITEM_DATA-CE0_ZZ1_ZZPHA_MSE   = <L_FIELD> .

        WHEN 'CE0_ZZ1_ZZPMT_MSE'       .
          LS_ITEM_DATA-CE0_ZZ1_ZZPMT_MSE   = <L_FIELD> .

        WHEN 'CE0_KMVTNR'              .
          LS_ITEM_DATA-CE0_KMVTNR   = <L_FIELD> .

        WHEN 'CE0_ZZ1_ZZREFTN_MSE'     .
          LS_ITEM_DATA-CE0_ZZ1_ZZREFTN_MSE   = <L_FIELD> .

        WHEN 'CE0_ZZ1_MVGR1'     .
          LS_ITEM_DATA-CE0_ZZ1_MVGR1   = <L_FIELD> .

* Withholding Tax Data
        WHEN 'WT_WITHT01'  .
          LS_ITEM_DATA-WT_WITHT01   = <L_FIELD> .

        WHEN 'WT_WITHCD01' .
          LS_ITEM_DATA-WT_WITHCD01   = <L_FIELD> .

        WHEN 'WT_QSSHB01'  .
          LS_ITEM_DATA-WT_QSSHB01   = <L_FIELD> .

        WHEN 'WT_QSSHH01'  .
          LS_ITEM_DATA-WT_QSSHH01   = <L_FIELD> .

        WHEN 'WT_QBSHB01'  .
          LS_ITEM_DATA-WT_QBSHB01   = <L_FIELD> .

        WHEN 'WT_QBSHH01'  .
          LS_ITEM_DATA-WT_QBSHH01   = <L_FIELD> .

        WHEN 'WT_WITHT02'  .
          LS_ITEM_DATA-WT_WITHT02   = <L_FIELD> .

        WHEN 'WT_WITHCD02' .
          LS_ITEM_DATA-WT_WITHCD02   = <L_FIELD> .

        WHEN 'WT_QSSHB02'  .
          LS_ITEM_DATA-WT_QSSHB02   = <L_FIELD> .

        WHEN 'WT_QSSHH02'  .
          LS_ITEM_DATA-WT_QSSHH02   = <L_FIELD> .

        WHEN 'WT_QBSHB02'  .
          LS_ITEM_DATA-WT_QBSHB02   = <L_FIELD> .

        WHEN 'WT_QBSHH02'  .
          LS_ITEM_DATA-WT_QBSHH02   = <L_FIELD> .

        WHEN 'WT_WITHT03'   .
          LS_ITEM_DATA-WT_QBSHH02   = <L_FIELD> .

        WHEN 'WT_WITHCD03'  .
          LS_ITEM_DATA-WT_WITHCD03   = <L_FIELD> .

        WHEN 'WT_QSSHB03'   .
          LS_ITEM_DATA-WT_QSSHB03   = <L_FIELD> .

        WHEN 'WT_QSSHH03'   .
          LS_ITEM_DATA-WT_QSSHH03  = <L_FIELD> .

        WHEN 'WT_QBSHB03'   .
          LS_ITEM_DATA-WT_QBSHB03  = <L_FIELD> .

        WHEN 'WT_QBSHH03'   .
          LS_ITEM_DATA-WT_QBSHH03  = <L_FIELD> .

        WHEN 'WT_WITHT04'   .
          LS_ITEM_DATA-WT_WITHT04  = <L_FIELD> .

        WHEN 'WT_WITHCD04'  .
          LS_ITEM_DATA-WT_WITHCD04  = <L_FIELD> .

        WHEN 'WT_QSSHB04'   .
          LS_ITEM_DATA-WT_QSSHB04  = <L_FIELD> .

        WHEN 'WT_QSSHH04'   .
          LS_ITEM_DATA-WT_QSSHH04  = <L_FIELD> .

        WHEN 'WT_QBSHB04'   .
          LS_ITEM_DATA-WT_QBSHB04  = <L_FIELD> .

        WHEN 'WT_QBSHH04'   .
          LS_ITEM_DATA-WT_QBSHH04  = <L_FIELD> .

* One-Time Account/Individual Payee
        WHEN 'ONETIME_NAME1'           .
          LS_ITEM_DATA-ONETIME_NAME1  = <L_FIELD> .

        WHEN 'ONETIME_NAME2'           .
          LS_ITEM_DATA-ONETIME_NAME2  = <L_FIELD> .

        WHEN 'ONETIME_NAME3'           .
          LS_ITEM_DATA-ONETIME_NAME3  = <L_FIELD> .

        WHEN 'ONETIME_NAME4'           .
          LS_ITEM_DATA-ONETIME_NAME4  = <L_FIELD> .

        WHEN 'ONETIME_STREET'          .
          LS_ITEM_DATA-ONETIME_STREET  = <L_FIELD> .

        WHEN 'ONETIME_CITY'            .
          LS_ITEM_DATA-ONETIME_CITY  = <L_FIELD> .

        WHEN 'ONETIME_POSTAL'          .
          LS_ITEM_DATA-ONETIME_POSTAL  = <L_FIELD> .

        WHEN 'ONETIME_COUNTRY'         .
          LS_ITEM_DATA-ONETIME_COUNTRY  = <L_FIELD> .

        WHEN 'ONETIME_TAX'             .
          LS_ITEM_DATA-ONETIME_TAX  = <L_FIELD> .

        WHEN 'ONETIME_BANK_KEY'        .
          LS_ITEM_DATA-ONETIME_BANK_KEY  = <L_FIELD> .

        WHEN 'ONETIME_BANK_ACCT_NO'    .
          LS_ITEM_DATA-ONETIME_BANK_ACCT_NO  = <L_FIELD> .

        WHEN 'ONETIME_BANK_COUNTRY'    .
          LS_ITEM_DATA-ONETIME_BANK_COUNTRY  = <L_FIELD> .

*--Bill of Exchange Details
        WHEN 'EXCHG_DUE_ON'            .
          LS_ITEM_DATA-EXCHG_DUE_ON  = <L_FIELD> .

        WHEN 'EXCHG_CHEQUE_NUMBER'     .
          LS_ITEM_DATA-EXCHG_CHEQUE_NUMBER  = <L_FIELD> .

        WHEN 'EXCHG_CHEQUE_BANKKEY'    .
          LS_ITEM_DATA-EXCHG_CHEQUE_BANKKEY  = <L_FIELD> .

        WHEN 'EXCHG_ACCOUNT_NUMBER'    .
          LS_ITEM_DATA-EXCHG_ACCOUNT_NUMBER  = <L_FIELD> .

*-- Additional Field
        WHEN 'PSEGMENT'.
          LS_ITEM_DATA-PSEGMENT  = <L_FIELD> .

        WHEN 'GL_ACCOUNT'.
          LS_ITEM_DATA-GL_ACCOUNT = <L_FIELD> .
        WHEN 'MATNR'.
          LS_ITEM_DATA-MATNR = <L_FIELD>.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

    ENDLOOP.

    IF LF_MSGTX IS INITIAL.
      IF LS_KEY-TRANS_NO IS INITIAL .
        CONTINUE.
      ENDIF.

*  Collect Data
      LS_ITEM_DATA-ROWNO = LF_ROWNO .
      LS_ITEM-DATA =  LS_ITEM_DATA .

    ENDIF.

    IF LF_MSGTX IS NOT INITIAL.
      CLEAR LS_MESSG.
      LS_MESSG-ROWNO = LF_ROWNO.
      LS_MESSG-MSGTY = 'E'.
      LS_MESSG-MSGID = 'ZSDSPS01'.
      LS_MESSG-MSGNO = '000'.
      LS_MESSG-MSGTX = LF_MSGTX.

      CONTINUE.
    ENDIF.

*   Validate and Append data into table
    PERFORM F_VALIDATE_AND_APPEND  USING  LF_ROWNO
                                          LS_KEY
                                          LS_DATA-HEAD
*                                          LS_DATA-HEADX
                                          LS_ITEM_DATA
*                                          LS_ITEM_DATAX
                                          LS_MESSG
                                CHANGING  CT_DATA.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_TARGET_FIELD
*&---------------------------------------------------------------------*
*& Get Target Field name based on column sequence
*&---------------------------------------------------------------------*
FORM F_GET_TARGET_FIELD USING  UF_TYPE   TYPE  CHAR20
                               UF_INDEX  TYPE  I
                      CHANGING CF_FIELD  TYPE  TS_FNAME.

  STATICS:
    LF_TYPE   TYPE  CHAR20,
    LT_FIELDS TYPE  ABAP_COMPDESCR_TAB.

  DATA:
    LREF_DESCR TYPE  REF TO CL_ABAP_TYPEDESCR,
    LREF_STRUC TYPE  REF TO CL_ABAP_STRUCTDESCR.

  FIELD-SYMBOLS:
    <L_FIELD>  TYPE  ABAP_COMPDESCR.


* Initialize Output
  CLEAR: CF_FIELD.

  IF LT_FIELDS IS INITIAL OR
     LF_TYPE NE UF_TYPE.
    CLEAR: LF_TYPE,
           LT_FIELDS.
    LF_TYPE = UF_TYPE.
*   Get Template Fields Sequence
    LREF_DESCR = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( LF_TYPE ).
    LREF_STRUC ?= LREF_DESCR.
    LT_FIELDS = LREF_STRUC->COMPONENTS.
  ENDIF.

  READ TABLE LT_FIELDS ASSIGNING <L_FIELD>
                       INDEX UF_INDEX.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Output
  CF_FIELD = <L_FIELD>-NAME.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CONVERT_INT_AMOUNT
*&---------------------------------------------------------------------*
*& Convert amount to internal format based on currency key
*&---------------------------------------------------------------------*
FORM F_CONVERT_INT_AMOUNT USING UF_AMOUNT_I  TYPE  ANY
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


  DATA LF_AMOUNT TYPE WMTO_S-AMOUNT .
  LF_AMOUNT = CF_AMOUNT_O .
  CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_DISPLAY'
    EXPORTING
      CURRENCY        = UF_WAERS
      AMOUNT_INTERNAL = LF_AMOUNT
    IMPORTING
      AMOUNT_DISPLAY  = LF_AMOUNT
    EXCEPTIONS
      INTERNAL_ERROR  = 1
      OTHERS          = 2.
  IF SY-SUBRC <> 0.
    CLEAR  CF_AMOUNT_O .
  ELSE.
    CF_AMOUNT_O = LF_AMOUNT .
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_AMOUNT
*&---------------------------------------------------------------------*
*& Validate Amount
*&---------------------------------------------------------------------*
FORM F_VALIDATE_AMOUNT   USING UF_STRING TYPE  ANY
                      CHANGING CF_AMOUNT TYPE  ANY
                               CF_MSGTX  TYPE  TS_MSG_TXT.
* Initialize Output
  CLEAR: CF_AMOUNT,
         CF_MSGTX.

* Only when value exist
  CHECK UF_STRING IS NOT INITIAL.

* Move to amount field
  TRY.
      CF_AMOUNT = UF_STRING.
    CATCH CX_ROOT.                                       "#EC CATCH_ALL
*     Text-e06 : Invalid amount value:
      CONCATENATE TEXT-E06 UF_STRING
             INTO CF_MSGTX
        SEPARATED BY SPACE.
      RETURN.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_COMPCODE
*&---------------------------------------------------------------------*
*& Validate Company Code
*&---------------------------------------------------------------------*

FORM F_VALIDATE_COMPCODE  USING  UF_STRING  TYPE  STRING
                        CHANGING CF_BUKRS   TYPE  T001-BUKRS
                                 CF_WAERS   TYPE  T001-WAERS
                                 CF_MSGTX   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_T001,
           BUKRS TYPE  T001-BUKRS,
           WAERS TYPE  T001-WAERS,
         END OF LTS_T001.
  TYPES: LTT_T001  TYPE  SORTED TABLE OF LTS_T001
                           WITH UNIQUE KEY BUKRS.

  STATICS:
    LS_T001 TYPE  LTS_T001,
    LT_T001 TYPE  LTT_T001.

  DATA:
    LF_LEN   TYPE  I,
    LF_BUKRS TYPE  LTS_T001-BUKRS.


* Initialize Output
  CLEAR: CF_BUKRS,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 4
  LF_LEN = STRLEN( UF_STRING ).
  IF LF_LEN GT 4.
*   Text-e04 : Invalid Company code:
    CONCATENATE TEXT-E04 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LF_BUKRS = UF_STRING.

* Check Buffer
  IF LS_T001-BUKRS NE LF_BUKRS.
*   Validate with Memory
    READ TABLE LT_T001 INTO LS_T001
                        WITH KEY BUKRS = LF_BUKRS
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_T001.
*     Validate with Database
      SELECT SINGLE BUKRS WAERS
        INTO LS_T001
        FROM T001
       WHERE BUKRS EQ LF_BUKRS.
      IF SY-SUBRC NE 0.
*       Text-e04 : Invalid Company code:
        CONCATENATE TEXT-E04 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_T001 INTO TABLE LT_T001.
    ENDIF.

  ENDIF.

* Assign Output
  CF_BUKRS = LS_T001-BUKRS.
  CF_WAERS = LS_T001-WAERS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_COSTCTR
*&---------------------------------------------------------------------*
*& Validate Cost Center
*&---------------------------------------------------------------------*
FORM F_VALIDATE_COSTCTR  USING  UF_STRING TYPE  STRING
                                UF_BUKRS  TYPE  T001-BUKRS
                       CHANGING CF_KOSTL  TYPE  CSKS-KOSTL
                                CF_MSGTX  TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_CSKS,
           BUKRS TYPE  T001-BUKRS,
           KOSTL TYPE  CSKS-KOSTL,
         END OF LTS_CSKS.
  TYPES: LTT_CSKS  TYPE  SORTED TABLE OF LTS_CSKS
                          WITH UNIQUE KEY BUKRS
                                          KOSTL.

  STATICS:
    LT_CSKS TYPE  LTT_CSKS,
    LS_CSKS TYPE  LTS_CSKS.

  DATA:
    LF_KOSTL  TYPE  CSKS-KOSTL,
    LF_LENGTH TYPE  I.


* Initialize Output
  CLEAR: CF_KOSTL,
         CF_MSGTX.

* Only not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 10.
*   Text-e12 : Invalid Cost Center:
    CONCATENATE TEXT-E12 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Convert to internal format
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = UF_STRING
    IMPORTING
      OUTPUT = LF_KOSTL.

* Check Buffer
  IF LS_CSKS-BUKRS NE UF_BUKRS OR
     LS_CSKS-KOSTL NE LF_KOSTL.
*   Validate with Memory
    READ TABLE LT_CSKS INTO LS_CSKS
                       WITH KEY BUKRS = UF_BUKRS
                                KOSTL = LF_KOSTL
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_CSKS.
*     Validate with Database
      SELECT TKA02~BUKRS CSKS~KOSTL                  "#EC CI_SEL_NESTED
          UP TO 1 ROWS
        INTO LS_CSKS
        FROM TKA02
             INNER JOIN CSKS                           "#EC CI_BUFFJOIN
               ON  CSKS~KOKRS = TKA02~KOKRS
       WHERE TKA02~BUKRS  EQ  UF_BUKRS
         AND TKA02~GSBER  EQ  SPACE
         AND CSKS~KOSTL   EQ  LF_KOSTL
         AND CSKS~DATBI   GE  SY-DATUM
         AND CSKS~DATAB   LE  SY-DATUM
       ORDER BY TKA02~BUKRS ASCENDING
                CSKS~KOSTL  ASCENDING.
      ENDSELECT.
      IF SY-SUBRC NE 0.
*       Text-e12 : Invalid Cost Center:
        CONCATENATE TEXT-E12 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_CSKS INTO TABLE LT_CSKS.
    ENDIF.

  ENDIF.

* Assign Output
  CF_KOSTL = LS_CSKS-KOSTL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_CURENCY
*&---------------------------------------------------------------------*
*& Validate Currency Key
*&---------------------------------------------------------------------*
FORM F_VALIDATE_CURENCY  USING  UF_STRING  TYPE  STRING
                       CHANGING CF_WAERS   TYPE  WAERS
                                CF_MSGTX   TYPE  TS_MSG_TXT.

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
*   Text-e05 : Invalid currency:
    CONCATENATE TEXT-E05 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LF_WAERS = UF_STRING.

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
*       Text-e05 : Invalid currency:
        CONCATENATE TEXT-E05 UF_STRING
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
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_CUSTOMER
*&---------------------------------------------------------------------*
*& Validate Customer Number
*&---------------------------------------------------------------------*
FORM F_VALIDATE_CUSTOMER  USING  UF_STRING  TYPE STRING
                        CHANGING CF_KUNNR   TYPE KNA1-KUNNR
                                 CF_MSGTX   TYPE TS_MSG_TXT.

  TYPES: BEGIN OF LTS_KNA1,
           KUNNR TYPE  KNA1-KUNNR,
         END OF LTS_KNA1.
  TYPES: LTT_KNA1  TYPE  SORTED TABLE OF LTS_KNA1
                           WITH UNIQUE KEY KUNNR.

  STATICS:
    LS_KNA1 TYPE  LTS_KNA1,
    LT_KNA1 TYPE  LTT_KNA1.

  DATA:
    LF_LEN   TYPE  I,
    LF_KUNNR TYPE  LTS_KNA1-KUNNR.


* Initialize Output
  CLEAR: CF_KUNNR,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 10
  LF_LEN = STRLEN( UF_STRING ).
  IF LF_LEN GT 10.
*   Text-e15 : Invalid
    CONCATENATE TEXT-E15 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = UF_STRING
    IMPORTING
      OUTPUT = LF_KUNNR.

* Check Buffer
  IF LS_KNA1-KUNNR NE LF_KUNNR.

*   Validate with Memory
    READ TABLE LT_KNA1 INTO LS_KNA1
                        WITH KEY KUNNR = LF_KUNNR
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_KNA1.
*     Validate with Database
      SELECT SINGLE KUNNR
        INTO LS_KNA1
        FROM KNA1
       WHERE KUNNR  EQ LF_KUNNR.                     "#EC CI_SEL_NESTED
      IF SY-SUBRC NE 0.
*       Text-e08 : Invalid Customer no.:
        CONCATENATE TEXT-E08 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_KNA1 INTO TABLE LT_KNA1.
    ENDIF.

  ENDIF.

* Assign Output
  CF_KUNNR = LS_KNA1-KUNNR.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_DATE
*&---------------------------------------------------------------------*
*& Validate Date
*&---------------------------------------------------------------------*
FORM F_VALIDATE_DATE  USING  UF_STRING  TYPE  ANY
                    CHANGING CF_DATUM   TYPE  SY-DATUM
                             CF_MSGTX   TYPE  TS_MSG_TXT.
  CONSTANTS :
    LC_01(2)   TYPE N VALUE 01,
    LC_31(2)   TYPE N VALUE 31,
    LC_12(2)   TYPE N VALUE 12,
    LC_1900(4) TYPE N VALUE 1900,
    LC_2200(4) TYPE N VALUE 2200.

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
*   Text-e02 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E02 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Length?
  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH NE 10.
*   Text-e02 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E02 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* First 2 Digit is date
  IF NOT UF_STRING(2) BETWEEN LC_01 AND LC_31.
*   Text-e02 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E02 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* 4-5th Digit is month
  IF NOT UF_STRING+3(2) BETWEEN LC_01 AND LC_12.
*   Text-e02 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E02 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* 7-8th digit is year
  IF NOT UF_STRING+6(4) BETWEEN LC_1900 AND LC_2200 AND
     UF_STRING+6(4) NE '9999'.
*   Text-e02 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E02 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Assign Output
  CONCATENATE UF_STRING+6(4) UF_STRING+3(2) UF_STRING(2)
         INTO CF_DATUM.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_doctype
*----------------------------------------------------------------------*
*  Validate Document Type
*----------------------------------------------------------------------*
FORM F_VALIDATE_DOCTYPE  USING  UF_STRING TYPE  STRING
                       CHANGING CF_BLART  TYPE  BKPF-BLART
                                CF_MSGTX  TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_T003,
           BLART TYPE  T003-BLART,
         END OF LTS_T003.
  TYPES: LTT_T003  TYPE  SORTED TABLE OF LTS_T003
                           WITH UNIQUE KEY BLART.

  STATICS:
    LS_T003 TYPE  LTS_T003,
    LT_T003 TYPE  LTT_T003.

  DATA:
    LF_BLART TYPE  LTS_T003-BLART.


* Initialize Output
  CLEAR: CF_BLART,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 2
  IF STRLEN( UF_STRING ) GT 2.
*   Text-e03 : Invalid Document type:
    CONCATENATE TEXT-E03 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LF_BLART = UF_STRING.

* Check Buffer
  IF LS_T003-BLART NE LF_BLART.
*   Validate with Memory
    READ TABLE LT_T003 INTO LS_T003
                        WITH KEY BLART = LF_BLART
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_T003.
*     Validate with Database
      SELECT SINGLE BLART
        INTO LS_T003
        FROM T003
       WHERE BLART EQ LF_BLART.
      IF SY-SUBRC NE 0.
*       Text-e03 : Invalid Document type:
        CONCATENATE TEXT-E03 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_T003 INTO TABLE LT_T003.
    ENDIF.

  ENDIF.

* Assign Output
  CF_BLART = LS_T003-BLART.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_validate_plant
*----------------------------------------------------------------------*
*       Validate Plant
*----------------------------------------------------------------------*
FORM F_VALIDATE_PLANT  USING  UF_STRING TYPE STRING
                     CHANGING CF_WERKS  TYPE T001W-WERKS
                              CF_MSGTX  TYPE TS_MSG_TXT.

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
  LF_LEN = STRLEN(  UF_STRING ).
  IF LF_LEN NE 4.
*   Text-e17 : Invalid plant:
    CONCATENATE TEXT-E17 UF_STRING
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
*       Text-e17 : Invalid plant:
        CONCATENATE TEXT-E17 UF_STRING
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
*  Form f_validate_matnr
*----------------------------------------------------------------------*
*  Validate Material Number
*----------------------------------------------------------------------*
FORM F_VALIDATE_MATNR  USING  UF_STRING  TYPE  STRING
                     CHANGING CF_MATNR   TYPE  MARA-MATNR
                              CF_MSGTX   TYPE  TS_MSG_TXT.

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
*       Text-e18 : Invalid Material number:
        CONCATENATE TEXT-E18 UF_STRING
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
*  Form f_validate_and_append
*----------------------------------------------------------------------*
*  Validate Whole Line data and collect into internal table for posting
*----------------------------------------------------------------------*
FORM F_VALIDATE_AND_APPEND  USING  UF_ROWNO   TYPE TS_RESULT-ROWNO
                                   US_KEY     TYPE TS_KEY
                                   US_HEAD    TYPE TS_HEAD
                                   US_ITEM    TYPE TS_ITEM_DATA
                                   US_MESSG   TYPE TS_MESSG
                          CHANGING CT_DATA    TYPE TT_DATA.

  DATA:
    LT_MESSG TYPE  TT_MESSG.

  DATA:
    LS_DATA  TYPE  TS_DATA,
    LS_ITEM  TYPE  TS_ITEM,
    LS_MESSG TYPE  TS_MESSG.

  FIELD-SYMBOLS:
    <L_DATA>   TYPE  TS_DATA.

* Read Key
  READ TABLE CT_DATA ASSIGNING <L_DATA>
                     WITH KEY KEY = US_KEY.
  IF SY-SUBRC NE 0.
*   Insert New Key
    CLEAR LS_DATA.
    LS_DATA-KEY  = US_KEY.
    INSERT LS_DATA INTO TABLE CT_DATA ASSIGNING <L_DATA>.

*   Validate Header
    PERFORM F_VALIDATE_HEAD  USING  US_HEAD
*                                    US_HEADX
                                    UF_ROWNO
                           CHANGING LS_MESSG.
    IF LS_MESSG IS NOT INITIAL.
      INSERT LS_MESSG INTO TABLE LT_MESSG.
    ENDIF.
    <L_DATA>-HEAD  = US_HEAD.
*    <L_DATA>-HEADX = US_HEADX.
  ENDIF.

  IF US_HEAD IS NOT INITIAL AND
     US_HEAD NE <L_DATA>-HEAD.
*   Header data conflict
    CLEAR: LS_MESSG.
    LS_MESSG-ROWNO = UF_ROWNO.
    LS_MESSG-MSGTY = 'E'.
    LS_MESSG-MSGID = 'ZSDSPS01'.
    LS_MESSG-MSGNO = '000'.
*   Text-e27 : Header data conflict within document.
    LS_MESSG-MSGTX = TEXT-E27.
    INSERT LS_MESSG INTO TABLE LT_MESSG.
  ENDIF.

  PERFORM F_VALIDATE_ITEM  USING  <L_DATA>-HEAD
                                  US_ITEM
*                                  US_ITEMX
                                  UF_ROWNO
                         CHANGING LS_ITEM
                                  LS_MESSG.


  PERFORM F_GET_LINETYPE   CHANGING  LS_ITEM-DATA .

  IF LS_MESSG IS NOT INITIAL.
    INSERT LS_MESSG INTO TABLE LT_MESSG.
  ENDIF.

* Check Posting Fields Setting
  PERFORM F_CHECK_POST_FIELDS  USING  <L_DATA>-HEAD
                                      LS_ITEM
                                      UF_ROWNO
                             CHANGING LS_MESSG.
  IF LS_MESSG IS NOT INITIAL.
    INSERT LS_MESSG INTO TABLE LT_MESSG.
  ENDIF.

  INSERT LS_ITEM INTO TABLE <L_DATA>-ITEM.

  IF LT_MESSG IS NOT INITIAL.
    APPEND LINES OF LT_MESSG TO <L_DATA>-MESSG.
  ENDIF.

  IF US_MESSG IS NOT INITIAL.
    APPEND US_MESSG TO <L_DATA>-MESSG.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_check_post_fields
*----------------------------------------------------------------------*
*  Check Posting Fields
*----------------------------------------------------------------------*
FORM F_CHECK_POST_FIELDS  USING US_HEAD  TYPE  TS_HEAD
                                US_ITEM  TYPE  TS_ITEM
                                UF_ROWNO TYPE  TS_RESULT-ROWNO
                       CHANGING CS_MESSG TYPE  TS_MESSG.

  DATA:
    LF_SAKNR TYPE SKB1-SAKNR,
    LF_FAUS1 TYPE TBSL-FAUS1,
    LF_FAUS2 TYPE TBSL-FAUS2.


* Initialize Output
  CLEAR: CS_MESSG.

  IF US_ITEM-DATA-HKONT IS NOT INITIAL.
    LF_SAKNR = US_ITEM-DATA-HKONT.
  ELSEIF US_ITEM-DATA-KOART EQ GC_KOART_VEND OR
         US_ITEM-DATA-KOART EQ GC_KOART_CUST.
    LF_SAKNR = US_ITEM-DATA-AKONT.
  ELSE.
    LF_SAKNR = US_ITEM-DATA-NEWKO.
  ENDIF.

* Get Field Selection
  CALL FUNCTION 'FI_GET_FIELD_SELECTION_STRING'
    EXPORTING
*     I_BSCHL            = PS_ITEM-DATA-NEWBS
      I_BSCHL            = US_ITEM-DATA-BSCHL
      I_BUKRS            = US_HEAD-BUKRS
      I_KOART            = US_ITEM-DATA-KOART
      I_SAKNR            = LF_SAKNR
    IMPORTING
      E_STRING1          = LF_FAUS1
      E_STRING2          = LF_FAUS2
    EXCEPTIONS
      ERR_BSCHL_UNKNOWN  = 1
      ERR_BUKRS_UNKNOWN  = 2
      ERR_FS_CONSISTENCY = 3
      ERR_FS_GROUP       = 4
      ERR_SAKNR_UNKNOWN  = 5
      OTHERS             = 6.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  PERFORM F_CHECK_FIELD_STATUS  USING  US_ITEM
                                       LF_FAUS1
                                       LF_FAUS2
                              CHANGING CS_MESSG.
  IF CS_MESSG IS NOT INITIAL.
    CS_MESSG-ROWNO = UF_ROWNO.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_check_field_status
*----------------------------------------------------------------------*
*  Check Field Status
*----------------------------------------------------------------------*
FORM F_CHECK_FIELD_STATUS  USING  US_ITEM   TYPE  TS_ITEM
                                  UF_FAUS1  TYPE  TBSL-FAUS1
                                  UF_FAUS2  TYPE  TBSL-FAUS2 ##NEEDED
                         CHANGING CS_MESSG  TYPE  TS_MESSG.

  TYPES: BEGIN OF LTS_TMODU,
           MODIF TYPE  TMODU-MODIF,
           TABNM TYPE  TMODU-TABNM,
           FELDN TYPE  TMODU-FELDN,
           KOART TYPE  TMODU-KOART,
         END OF LTS_TMODU.
  TYPES: LTT_TMODU TYPE SORTED TABLE OF LTS_TMODU
                         WITH NON-UNIQUE KEY TABNM
                                             FELDN
                                             KOART.

  CONSTANTS:
    LC_FAUNA    TYPE  TMODU-FAUNA  VALUE 'SKB1-FAUS1',
    LC_SUPPRESS TYPE  CHAR1     VALUE '-'.

  STATICS:
    LT_TMODU  TYPE  LTT_TMODU,
    LT_FIELDS TYPE  ABAP_COMPDESCR_TAB.

  DATA:
    LREF_DESCR TYPE  REF TO CL_ABAP_TYPEDESCR,
    LREF_STRUC TYPE  REF TO CL_ABAP_STRUCTDESCR.

  DATA:
    LF_FNAME TYPE  CHAR50,
    LF_TABNM TYPE  LTS_TMODU-TABNM,
    LF_FELDN TYPE  LTS_TMODU-FELDN,
    LF_OFFS  TYPE  I,
    LF_TEMP  TYPE  TS_MSG_TXT.

  FIELD-SYMBOLS:
    <L_FIELD> TYPE  ABAP_COMPDESCR,
    <L_FLAG>  TYPE  FLAG,
    <L_TMODU> TYPE  LTS_TMODU.


* Initialize Output
  CLEAR: CS_MESSG.

  IF LT_TMODU IS INITIAL.
    SELECT DISTINCT MODIF TABNM FELDN KOART              "#EC CI_BYPASS
      INTO TABLE LT_TMODU
      FROM TMODU
     WHERE FAUNA EQ LC_FAUNA.                        "#EC CI_SEL_NESTED
    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.
  ENDIF.

  IF LT_FIELDS IS INITIAL.
*   Get Template Fields Sequence
    LREF_DESCR = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( 'GTY_ITEM_DATAX' ).
    LREF_STRUC ?= LREF_DESCR.
    LT_FIELDS = LREF_STRUC->COMPONENTS.
  ENDIF.

  LOOP AT LT_FIELDS ASSIGNING <L_FIELD>.

*   Only when field is needed
    CONCATENATE 'PS_ITEM-DATAX-' <L_FIELD>-NAME
           INTO LF_FNAME.
    ASSIGN (LF_FNAME) TO <L_FLAG>.
    CHECK <L_FLAG> EQ GC_TRUE.

*   Assign Field mapping here. . .
    LF_TABNM = 'BSEG'.
    LF_FELDN = <L_FIELD>-NAME.

    READ TABLE LT_TMODU ASSIGNING <L_TMODU>
                        WITH KEY TABNM = LF_TABNM
                                 FELDN = LF_FELDN
                                 KOART = US_ITEM-DATA-KOART
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    LF_OFFS = <L_TMODU>-MODIF - 1.

*   Error If the field is suppress?
    IF STRLEN( UF_FAUS1 ) GT LF_OFFS AND
       UF_FAUS1+LF_OFFS(1) EQ LC_SUPPRESS.
      CS_MESSG-MSGTY = 'E'.
      CS_MESSG-MSGID = 'ZSDSPS01'.
      CS_MESSG-MSGNO = '000'.
      PERFORM F_GET_FIELD_DESC  USING  <L_TMODU>-TABNM
                                       <L_TMODU>-FELDN
                              CHANGING LF_TEMP.
*     Text-e37: Field &1 cannot be input due to the field is suppress.
      CS_MESSG-MSGTX = TEXT-E37.
      REPLACE ALL OCCURRENCES OF '&1' IN CS_MESSG-MSGTX
                                      WITH LF_TEMP.
      EXIT.
    ENDIF.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_head
*----------------------------------------------------------------------*
*  Validate Header data
*----------------------------------------------------------------------*
FORM F_VALIDATE_HEAD  USING  US_HEAD   TYPE  TS_HEAD
*                             US_HEADX  TYPE  TS_HEADX   ##NEEDED
                             UF_ROWNO  TYPE  TS_RESULT-ROWNO
                    CHANGING CS_MESSG  TYPE  TS_MESSG.

* Initialize Output
  CLEAR: CS_MESSG.

  IF US_HEAD-BLDAT IS INITIAL.
    CS_MESSG-ROWNO = UF_ROWNO.
    CS_MESSG-MSGTY = 'E'.
    CS_MESSG-MSGID = 'ZSDSPS01'.
    CS_MESSG-MSGNO = '000'.
*   Text-e20 : Missing Document Date.
    CS_MESSG-MSGTX = TEXT-E20.
    RETURN.
  ENDIF.

  IF US_HEAD-BUDAT IS INITIAL.
    CS_MESSG-ROWNO = UF_ROWNO.
    CS_MESSG-MSGTY = 'E'.
    CS_MESSG-MSGID = 'ZSDSPS01'.
    CS_MESSG-MSGNO = '000'.
*   Text-e21 : Missing Posting Date.
    CS_MESSG-MSGTX = TEXT-E21.
    RETURN.
  ENDIF.

  IF US_HEAD-BLART IS INITIAL.
    CS_MESSG-ROWNO = UF_ROWNO.
    CS_MESSG-MSGTY = 'E'.
    CS_MESSG-MSGID = 'ZSDSPS01'.
    CS_MESSG-MSGNO = '000'.
*   Text-e23 : Missing Document Type.
    CS_MESSG-MSGTX = TEXT-E23.
    RETURN.
  ENDIF.

  IF US_HEAD-BUKRS IS INITIAL.
    CS_MESSG-ROWNO = UF_ROWNO.
    CS_MESSG-MSGTY = 'E'.
    CS_MESSG-MSGID = 'ZSDSPS01'.
    CS_MESSG-MSGNO = '000'.
*   Text-e24 : Missing Company Code.
    CS_MESSG-MSGTX = TEXT-E24.
    RETURN.


  ENDIF.

  IF US_HEAD-WAERS IS INITIAL.
    CS_MESSG-ROWNO = UF_ROWNO.
    CS_MESSG-MSGTY = 'E'.
    CS_MESSG-MSGID = 'ZSDSPS01'.
    CS_MESSG-MSGNO = '000'.
*   Text-e25 : Missing Currency.
    CS_MESSG-MSGTX = TEXT-E25.
    RETURN.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_get_field_desc
*----------------------------------------------------------------------*
*  Get Field Description
*----------------------------------------------------------------------*
FORM F_GET_FIELD_DESC  USING  UF_TABNM  TYPE  TMODU-TABNM
                              UF_FELDN  TYPE  TMODU-FELDN
                     CHANGING CF_TEXT   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_DD03VT,
           TABNAME   TYPE  DD03VT-TABNAME,
           FIELDNAME TYPE  DD03VT-FIELDNAME,
           TEXT      TYPE  DD03VT-SCRTEXT_M,
         END OF LTS_DD03VT.
  TYPES: LTT_DD03VT TYPE SORTED TABLE OF LTS_DD03VT
                          WITH UNIQUE KEY TABNAME
                                          FIELDNAME.

  STATICS:
    LT_DD03VT TYPE  LTT_DD03VT,
    LS_DD03VT TYPE  LTS_DD03VT.

  DATA:
    LF_TABNAME   TYPE  LTS_DD03VT-TABNAME,
    LF_FIELDNAME TYPE  LTS_DD03VT-FIELDNAME.


* Initialize Output
  CLEAR: CF_TEXT.

  LF_TABNAME   = UF_TABNM.
  LF_FIELDNAME = UF_FELDN.

  IF LS_DD03VT-TABNAME NE LF_TABNAME OR
     LS_DD03VT-FIELDNAME NE LF_FIELDNAME.
    READ TABLE LT_DD03VT INTO LS_DD03VT
                         WITH KEY TABNAME = LF_TABNAME
                                  FIELDNAME = LF_FIELDNAME
                         BINARY SEARCH.
    IF SY-SUBRC NE 0.
      SELECT TABNAME FIELDNAME SCRTEXT_M
          UP TO 1 ROWS
        INTO LS_DD03VT
        FROM DD03VT
       WHERE TABNAME    EQ  LF_TABNAME
         AND FIELDNAME  EQ  LF_FIELDNAME
         AND DDLANGUAGE EQ  SY-LANGU
       ORDER BY TABNAME FIELDNAME SCRTEXT_M.
      ENDSELECT.
      IF SY-SUBRC NE 0.
        RETURN.
      ENDIF.
      INSERT LS_DD03VT INTO TABLE LT_DD03VT.
    ENDIF.
  ENDIF.

* Assign Output
  CF_TEXT = LS_DD03VT-TEXT.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_glaccount
*----------------------------------------------------------------------*
*  Validate GL Account
*----------------------------------------------------------------------*
FORM F_VALIDATE_GLACCOUNT  USING  UF_STRING  TYPE STRING
                                  UF_BUKRS   TYPE T001-BUKRS
                         CHANGING CF_SAKNR   TYPE SKA1-SAKNR
                                  CF_MSGTX   TYPE TS_MSG_TXT.

  TYPES: BEGIN OF LTS_SKA1,
           BUKRS TYPE  T001-BUKRS,
           SAKNR TYPE  SKA1-SAKNR,
         END OF LTS_SKA1.
  TYPES: LTT_SKA1  TYPE  SORTED TABLE OF LTS_SKA1
                           WITH UNIQUE KEY BUKRS
                                           SAKNR.

  STATICS:
    LS_SKA1 TYPE  LTS_SKA1,
    LT_SKA1 TYPE  LTT_SKA1.

  DATA:
    LF_LEN   TYPE  I,
    LF_SAKNR TYPE  LTS_SKA1-SAKNR.


* Initialize Output
  CLEAR: CF_SAKNR,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 10
  LF_LEN = STRLEN( UF_STRING ).
  IF LF_LEN GT 10.
*   Text-e09 : Invalid GL Account:
    CONCATENATE TEXT-E09 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = UF_STRING
    IMPORTING
      OUTPUT = LF_SAKNR.

* Check Buffer
  IF LS_SKA1-BUKRS NE UF_BUKRS OR
     LS_SKA1-SAKNR NE LF_SAKNR.

*   Validate with Memory
    READ TABLE LT_SKA1 INTO LS_SKA1
                        WITH KEY BUKRS = UF_BUKRS
                                 SAKNR = LF_SAKNR
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_SKA1.
*     Validate with Database
      SELECT T001~BUKRS SKA1~SAKNR
          UP TO 1 ROWS
        INTO LS_SKA1                   "#EC CI_DB_OPERATION_OK[2431747]
        FROM T001                      "#EC CI_DB_OPERATION_OK[2389136]
               INNER JOIN SKA1                         "#EC CI_BUFFJOIN
                 ON  SKA1~KTOPL = T001~KTOPL
       WHERE T001~BUKRS  EQ UF_BUKRS
         AND SKA1~SAKNR  EQ LF_SAKNR
       ORDER BY T001~BUKRS ASCENDING
                SKA1~SAKNR ASCENDING.                "#EC CI_SEL_NESTED
      ENDSELECT.
      IF SY-SUBRC NE 0.
*       Text-e09 : Invalid GL Account:
        CONCATENATE TEXT-E09 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_SKA1 INTO TABLE LT_SKA1.
    ENDIF.

  ENDIF.

* Assign Output
  CF_SAKNR = LS_SKA1-SAKNR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_item
*----------------------------------------------------------------------*
*  Validate Item data
*----------------------------------------------------------------------*
FORM F_VALIDATE_ITEM  USING  US_HEAD  TYPE  TS_HEAD
                             US_ITEM  TYPE  TS_ITEM_DATA
*                             US_ITEMX TYPE  TS_ITEM_DATAX
                             UF_ROWNO TYPE  TS_RESULT-ROWNO
                    CHANGING CS_ITEM_O  TYPE  TS_ITEM
                             CS_MESSG   TYPE  TS_MESSG.

  DATA:
    LF_LIFNR TYPE  LFBW-LIFNR,
    LF_KUNNR TYPE  KNA1-KUNNR.


* Initialize Output
  CLEAR: CS_ITEM_O,
         CS_MESSG.

* Assign Item data
  CS_ITEM_O-DATA  = US_ITEM.
*  CS_ITEM_O-DATAX = US_ITEMX.

  IF US_ITEM-NEWBS IS INITIAL.
    CS_MESSG-ROWNO = UF_ROWNO.
    CS_MESSG-MSGTY = 'E'.
    CS_MESSG-MSGID = 'ZSDSPS01'.
    CS_MESSG-MSGNO = '000'.
*   Text-e28 : Missing Posting Key.
    CS_MESSG-MSGTX = TEXT-E28.
    RETURN.
  ENDIF.

  IF US_ITEM-NEWKO IS INITIAL.
    CS_MESSG-ROWNO = UF_ROWNO.
    CS_MESSG-MSGTY = 'E'.
    CS_MESSG-MSGID = 'ZSDSPS01'.
    CS_MESSG-MSGNO = '000'.
*   Text-e29 : Missing GL Account/Vendor/Customer.
    CS_MESSG-MSGTX = TEXT-E29.
    RETURN.
  ENDIF.

  IF US_ITEM-WRBTR IS INITIAL.
    CS_MESSG-ROWNO = UF_ROWNO.
    CS_MESSG-MSGTY = 'E'.
    CS_MESSG-MSGID = 'ZSDSPS01'.
    CS_MESSG-MSGNO = '000'.
*   Text-e31 : Missing Amount in DC.
    CS_MESSG-MSGTX = TEXT-E31.
    RETURN.
  ENDIF.

* -------------------
* For Vendor
* -------------------
  IF US_ITEM-KOART EQ GC_KOART_VEND.
    LF_LIFNR = US_ITEM-NEWKO.
*   Get Withholding tax type for Vendor
    SELECT WITHT
      INTO TABLE CS_ITEM_O-WITHT
      FROM LFBW
     WHERE LIFNR EQ LF_LIFNR
       AND BUKRS EQ US_HEAD-BUKRS
     ORDER BY PRIMARY KEY.                           "#EC CI_SEL_NESTED
    IF SY-SUBRC NE 0.
      CLEAR: CS_ITEM_O-WITHT.
    ENDIF.
*   Get Reconcile account
    SELECT SINGLE AKONT
      INTO CS_ITEM_O-DATA-AKONT
      FROM LFB1
     WHERE LIFNR EQ LF_LIFNR
       AND BUKRS EQ US_HEAD-BUKRS.                   "#EC CI_SEL_NESTED
    IF SY-SUBRC NE 0.
      CS_MESSG-ROWNO = UF_ROWNO.
      CS_MESSG-MSGTY = 'E'.
      CS_MESSG-MSGID = 'ZSDSPS01'.
      CS_MESSG-MSGNO = '000'.
*     Text-e34 : Vendor is not valid for company.
      CS_MESSG-MSGTX = TEXT-E34.
      RETURN.
    ENDIF.
* -------------------
* For Customer
* -------------------
  ELSEIF US_ITEM-KOART EQ GC_KOART_CUST.
    LF_KUNNR = US_ITEM-NEWKO.
*   Get Withholding tax type for Customer
    SELECT WITHT
      INTO TABLE CS_ITEM_O-WITHT
      FROM KNBW
     WHERE KUNNR EQ LF_KUNNR
       AND BUKRS EQ US_HEAD-BUKRS
     ORDER BY PRIMARY KEY.                           "#EC CI_SEL_NESTED
    IF SY-SUBRC NE 0.
      CLEAR: CS_ITEM_O-WITHT.
    ENDIF.
*   Get Reconcile account
    SELECT SINGLE AKONT
      INTO CS_ITEM_O-DATA-AKONT
      FROM KNB1
     WHERE KUNNR EQ LF_KUNNR
       AND BUKRS EQ US_HEAD-BUKRS.                   "#EC CI_SEL_NESTED
    IF SY-SUBRC NE 0.
      CS_MESSG-ROWNO = UF_ROWNO.
      CS_MESSG-MSGTY = 'E'.
      CS_MESSG-MSGID = 'ZSDSPS01'.
      CS_MESSG-MSGNO = '000'.
*     Text-e35 : Customer is not valid for company.
      CS_MESSG-MSGTX = TEXT-E35.
      RETURN.
    ENDIF.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_order
*----------------------------------------------------------------------*
*  Validate Order
*----------------------------------------------------------------------*
FORM F_VALIDATE_ORDER  USING  UF_STRING  TYPE STRING
                     CHANGING CF_AUFNR   TYPE AUFK-AUFNR
                              CF_MSGTX   TYPE TS_MSG_TXT.

  TYPES: BEGIN OF LTS_AUFK,
           AUFNR TYPE  AUFK-AUFNR,
         END OF LTS_AUFK.
  TYPES: LTT_AUFK  TYPE  SORTED TABLE OF LTS_AUFK
                          WITH UNIQUE KEY AUFNR.

  STATICS:
    LT_AUFK TYPE  LTT_AUFK,
    LS_AUFK TYPE  LTS_AUFK.

  DATA:
    LF_AUFNR  TYPE  AUFK-AUFNR,
    LF_LENGTH TYPE  I.


* Initialize Output
  CLEAR: CF_AUFNR,
         CF_MSGTX.

* Only not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 10.
*   Text-e13 : Invalid Order:
    CONCATENATE TEXT-E13 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Convert to internal format
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = UF_STRING
    IMPORTING
      OUTPUT = LF_AUFNR.

* Check Buffer
  IF LS_AUFK-AUFNR NE LF_AUFNR.
*   Validate with Memory
    READ TABLE LT_AUFK INTO LS_AUFK
                       WITH KEY AUFNR = LF_AUFNR
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_AUFK.
*     Validate with Database
      SELECT SINGLE AUFNR                            "#EC CI_SEL_NESTED
        INTO LS_AUFK
        FROM AUFK
       WHERE AUFNR  EQ  LF_AUFNR.
      IF SY-SUBRC NE 0.
*       Text-e13 : Invalid Order:
        CONCATENATE TEXT-E13 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_AUFK INTO TABLE LT_AUFK.
    ENDIF.

  ENDIF.

* Assign Output
  CF_AUFNR = LS_AUFK-AUFNR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_postingkey
*----------------------------------------------------------------------*
*  Validate Posting Key
*----------------------------------------------------------------------*
FORM F_VALIDATE_POSTINGKEY  USING  UF_STRING  TYPE  STRING
                          CHANGING CF_BSCHL   TYPE  TBSL-BSCHL
                                   CF_KOART   TYPE  TBSL-KOART
                                   CF_SHKZG   TYPE  TBSL-SHKZG
                                   CF_MSGTX   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_TBSL,
           BSCHL TYPE  TBSL-BSCHL,
           KOART TYPE  TBSL-KOART,
           SHKZG TYPE  TBSL-SHKZG,
         END OF LTS_TBSL.
  TYPES: LTT_TBSL  TYPE  SORTED TABLE OF LTS_TBSL
                           WITH UNIQUE KEY BSCHL.

  STATICS:
    LT_TBSL TYPE  LTT_TBSL,
    LS_TBSL TYPE  LTS_TBSL.

  DATA:
    LF_LENGTH TYPE  I,
    LF_BSCHL  TYPE  LTS_TBSL-BSCHL.


* Initialize Output
  CLEAR: CF_BSCHL,
         CF_KOART,
         CF_SHKZG,
         CF_MSGTX.

* Only not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Check Length
  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 5.
*   Text-e07 : Invalid Posting Key:
    CONCATENATE TEXT-E07 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LF_BSCHL = UF_STRING.

* Check %, no more validation
  IF LF_BSCHL EQ '%'.
    CF_BSCHL = LF_BSCHL.
    RETURN.
  ENDIF.

* Check Buffer
  IF LF_BSCHL NE LS_TBSL-BSCHL.
*   Read from Memory
    READ TABLE LT_TBSL INTO LS_TBSL
                        WITH KEY BSCHL = LF_BSCHL
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TBSL.
*     Read from Database
      SELECT SINGLE BSCHL KOART SHKZG
        INTO LS_TBSL
        FROM TBSL
       WHERE BSCHL EQ LF_BSCHL.
      IF SY-SUBRC NE 0.
*       Text-e07 : Invalid Posting Key:
        CONCATENATE TEXT-E07 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TBSL INTO TABLE LT_TBSL.
    ENDIF.
  ENDIF.

* Assign Output
  CF_BSCHL = LS_TBSL-BSCHL.
  CF_KOART = LS_TBSL-KOART.
  CF_SHKZG = LS_TBSL-SHKZG.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_profitctr
*----------------------------------------------------------------------*
*  Validate Profit Center
*----------------------------------------------------------------------*
FORM F_VALIDATE_PROFITCTR  USING  UF_STRING TYPE  STRING
                                  UF_BUKRS  TYPE  T001-BUKRS
                         CHANGING CF_PRCTR  TYPE  CEPC-PRCTR
                                  CF_MSGTX  TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_CEPC,
           BUKRS TYPE  T001-BUKRS,
           PRCTR TYPE  CEPC-PRCTR,
         END OF LTS_CEPC.
  TYPES: LTT_CEPC  TYPE  SORTED TABLE OF LTS_CEPC
                         WITH UNIQUE KEY BUKRS
                                         PRCTR.

  STATICS:
    LS_CEPC TYPE  LTS_CEPC,
    LT_CEPC TYPE  LTT_CEPC.

  DATA:
    LF_PRCTR  TYPE  CEPC-PRCTR,
    LF_LENGTH TYPE  I.


* Initialize Output
  CLEAR: CF_PRCTR,
         CF_MSGTX.

* Only not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 10.
*   Text-e11 : Invalid Profit Center:
    CONCATENATE TEXT-E11 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Convert to internal format
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = UF_STRING
    IMPORTING
      OUTPUT = LF_PRCTR.

* Check Buffer
  IF LS_CEPC-BUKRS NE UF_BUKRS OR
     LS_CEPC-PRCTR NE LF_PRCTR.
*   Validate with Memory
    READ TABLE LT_CEPC INTO LS_CEPC
                       WITH KEY BUKRS = UF_BUKRS
                                PRCTR = LF_PRCTR
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_CEPC.
*     Validate with Database
      SELECT TKA02~BUKRS CEPC~PRCTR                  "#EC CI_SEL_NESTED
          UP TO 1 ROWS
        INTO LS_CEPC
        FROM TKA02
             INNER JOIN CEPC                           "#EC CI_BUFFJOIN
               ON  CEPC~KOKRS = TKA02~KOKRS
       WHERE TKA02~BUKRS  EQ  UF_BUKRS
         AND TKA02~GSBER  EQ  SPACE
         AND CEPC~PRCTR   EQ  LF_PRCTR
         AND CEPC~DATBI   GE  SY-DATUM
         AND CEPC~DATAB   LE  SY-DATUM
       ORDER BY TKA02~BUKRS ASCENDING
                CEPC~PRCTR  ASCENDING.
      ENDSELECT.
      IF SY-SUBRC NE 0.
*       Text-e11 : Invalid Profit Center:
        CONCATENATE TEXT-E11 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_CEPC INTO TABLE LT_CEPC.
    ENDIF.

  ENDIF.

* Assign Output
  CF_PRCTR = LS_CEPC-PRCTR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_vendor
*----------------------------------------------------------------------*
*  Validate Vendor
*----------------------------------------------------------------------*
FORM F_VALIDATE_VENDOR  USING  UF_STRING  TYPE  STRING
                      CHANGING CF_LIFNR   TYPE  LFA1-LIFNR
                               CF_MSGTX   TYPE  TS_MSG_TXT.

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
*   Text-e10 : Invalid Vendor:
    CONCATENATE TEXT-E10 UF_STRING
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
*       Text-e10 : Invalid Vendor:
        CONCATENATE TEXT-E10 UF_STRING
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
*&---------------------------------------------------------------------*
*& Form f_collect_result
*&---------------------------------------------------------------------*
*& Collect Result data
*&---------------------------------------------------------------------*
FORM F_COLLECT_RESULT  USING  US_DATA   TYPE TS_DATA
                     CHANGING CT_RESULT TYPE TT_RESULT.

  DATA:
    LS_RESULT  TYPE  TS_RESULT.

  FIELD-SYMBOLS:
    <L_MESSG>  TYPE  TS_MESSG.


  CLEAR: LS_RESULT.
  LS_RESULT-TRANS_NO = US_DATA-KEY-TRANS_NO.
  LS_RESULT-BUKRS = US_DATA-HEAD-BUKRS.
  LS_RESULT-BELNR = US_DATA-RESULT-BELNR.
  LS_RESULT-GJAHR = US_DATA-RESULT-GJAHR.
  LS_RESULT-BLDAT =  US_DATA-HEAD-BLDAT  .
  LS_RESULT-BUDAT =  US_DATA-HEAD-BUDAT  .
  LS_RESULT-WAERS =  US_DATA-HEAD-WAERS  .
  LS_RESULT-BKTXT =  US_DATA-HEAD-BKTXT  .
  LS_RESULT-XBLNR =  US_DATA-HEAD-XBLNR  .

  LOOP AT US_DATA-ITEM ASSIGNING FIELD-SYMBOL(<L_ITEM>) .
    LS_RESULT-BSCHL  = <L_ITEM>-DATA-BSCHL .
    LS_RESULT-HKONT  = <L_ITEM>-DATA-NEWKO .
    LS_RESULT-UMSKZ  = <L_ITEM>-DATA-UMSKZ.
    LS_RESULT-WRBTR  = <L_ITEM>-DATA-WRBTR.

    LOOP AT US_DATA-MESSG ASSIGNING <L_MESSG> .
      LS_RESULT-ROWNO = <L_MESSG>-ROWNO.
      LS_RESULT-MSGTY = <L_MESSG>-MSGTY.
      LS_RESULT-MSGID = <L_MESSG>-MSGID.
      LS_RESULT-MSGNO = <L_MESSG>-MSGNO.
      LS_RESULT-MSGTX = <L_MESSG>-MSGTX.
      CASE LS_RESULT-MSGTY.
        WHEN 'S'.
          LS_RESULT-STATU = ICON_LED_GREEN.
        WHEN 'W'.
          LS_RESULT-STATU = ICON_LED_YELLOW.
        WHEN 'E' OR 'A'.
          LS_RESULT-STATU = ICON_LED_RED.
        WHEN OTHERS.
          LS_RESULT-STATU = ICON_LED_INACTIVE.
      ENDCASE.

      APPEND LS_RESULT TO CT_RESULT.
    ENDLOOP.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_display_result
*&---------------------------------------------------------------------*
*& Display Processing Result
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING  UT_RESULT  TYPE TT_RESULT.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
  GF_ALV_HEADER_1 = GC_TRUE.
* Enable Soft Refresh only in display mode
  GF_SOFT_REFRESH_1 = GC_TRUE.
** No auto refresh in edit mode
*  GF_NO_AUTO_REFRESH_1 = GC_TRUE.

* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
  ASSIGN UT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2610650]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT CHANGING GT_FIELDCAT_1.
* Sort data
  PERFORM F_ALV_SORT_RESULT CHANGING GT_SORT_1.
* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*& ALV Field Catalog for Report
*&---------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT  CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.
  CONSTANTS LC_220(2) TYPE N VALUE 50.

  DATA:
    LF_TEXT         TYPE  TEXT50.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'TRANS_NO'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
*       Text-c02 : TransNo.
        LF_TEXT                  = TEXT-C02.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = 6.
      WHEN 'ROWNO'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
        <L_FIELDCAT>-NO_ZERO   = GC_TRUE.
*       Text-c02 : Row  No.
        LF_TEXT                  = TEXT-C15.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = 6.
      WHEN 'BUKRS'.
*       Text-c04 : Com.Code
        LF_TEXT                  = TEXT-C04.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.

      WHEN 'BELNR'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
        IF CB_TEST IS INITIAL.
          <L_FIELDCAT>-HOTSPOT = GC_TRUE.
        ENDIF.

      WHEN 'GJAHR'.
        <L_FIELDCAT>-KEY       = GC_TRUE.

      WHEN 'BLDAT'.
* Text-C05: Document Date
        LF_TEXT                = TEXT-C05.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'BUDAT'.
* Text-C06: Posting Date
        LF_TEXT                = TEXT-C06.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'WAERS'.
* Text-C07: Doc.Curr.
        LF_TEXT                = TEXT-C07.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'BKTXT'.
* Text-C08: Doc.Header Text
        LF_TEXT                = TEXT-C08.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.

      WHEN 'XBLNR'.
* Text-C09: Reference
        LF_TEXT                = TEXT-C09.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'BSCHL'.
* Text-C10: PstKy
        LF_TEXT                = TEXT-C10.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'HKONT'.
* Text-C11: G/L Account
        LF_TEXT                = TEXT-C11.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'UMSKZ'.
* Text-C12: Special GL indicator
        LF_TEXT                = TEXT-C12.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
* Text-C13: G/L Account Name
      WHEN 'WRBTR'.
* Text-C14: Amount
        LF_TEXT                = TEXT-C14.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-CURRENCY = 'WAERS' .

      WHEN 'STATU'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
*       Text-c01 : Status
        LF_TEXT                  = TEXT-C01.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = 6.
      WHEN 'MSGTY'.
        <L_FIELDCAT>-NO_OUT = GC_TRUE.
      WHEN 'MSGID'.
        <L_FIELDCAT>-NO_OUT = GC_TRUE.
      WHEN 'MSGNO'.
        <L_FIELDCAT>-NO_OUT = GC_TRUE.
      WHEN 'MSGTX'.
*        <LFS_FIELDCAT>-KEY       = GC_TRUE.
*       Text-c03 : Message
        LF_TEXT                  = TEXT-C03.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = LC_220.


      WHEN OTHERS.
        <L_FIELDCAT>-TECH = GC_TRUE.

    ENDCASE.

  ENDLOOP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_LAYOUT
*----------------------------------------------------------------------*
*  ALV Layout for Report
*----------------------------------------------------------------------*
FORM F_ALV_LAYOUT CHANGING CS_LAYOUT  TYPE  LVC_S_LAYO
                           CS_VARIANT TYPE  DISVARIANT
                           CS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR:  CS_LAYOUT, CS_VARIANT, CS_PRINT.

* determine layout
  CS_LAYOUT-SEL_MODE   = 'B'. "Multiple Selection with Push Box
  CS_LAYOUT-CWIDTH_OPT = SPACE.
  CS_LAYOUT-ZEBRA      = GC_TRUE.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.

  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT  CHANGING CT_SORT TYPE LVC_T_SORT.

  CONSTANTS:
    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'TRANS_NO'.
*    lc_sort2 TYPE  lvc_s_sort-fieldname VALUE 'ROWNO'.
  DATA:
    LS_SORT  TYPE  LVC_S_SORT.


* Initialize Output
  CLEAR: CT_SORT.

* Sort by Document Group Number
  CLEAR LS_SORT.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

* Sort by Row Number
*  CLEAR ls_sort.
*  ls_sort-fieldname = lc_sort2.
*  ls_sort-up        = gc_true.
*  ls_sort-subtot    = space.
*  APPEND ls_sort TO pt_sort.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_top_of_page_1
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_1 USING UF_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT. "#EC CALLED

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '30%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '70%'.

  DATA:
    LF_TEXT      TYPE  SDYDO_TEXT_ELEMENT,
    LF_TEMP1     TYPE  TEXT50,
    LF_TEMP2     TYPE  TEXT50,
    LREF_TABLE   TYPE  REF TO CL_DD_TABLE_ELEMENT,
    LREF_COL_KEY TYPE  REF TO CL_DD_AREA,
    LREF_COL_VAL TYPE  REF TO CL_DD_AREA.

* Create table
  CALL METHOD UF_DYNDOC_ID->ADD_TABLE
    EXPORTING
      NO_OF_COLUMNS = 2
      BORDER        = '0'
      WIDTH         = '50%'
    IMPORTING
      TABLE         = LREF_TABLE.
* Get Column Element
  CALL METHOD LREF_TABLE->ADD_COLUMN
    EXPORTING
      WIDTH  = LC_SIZE_KEY
    IMPORTING
      COLUMN = LREF_COL_KEY.
* Get Column Element
  CALL METHOD LREF_TABLE->ADD_COLUMN
    EXPORTING
      WIDTH  = LC_SIZE_VAL
    IMPORTING
      COLUMN = LREF_COL_VAL.
* Set Key column style
  CALL METHOD LREF_TABLE->SET_COLUMN_STYLE
    EXPORTING
      COL_NO       = 1
      SAP_EMPHASIS = CL_DD_AREA=>STRONG.

*-----------------------
* Add value in Line1
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h01 : Program:
  LF_TEXT = TEXT-H01.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  CONCATENATE SY-REPID SY-TITLE
         INTO LF_TEXT
    SEPARATED BY '-'.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line2
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h02 : Processing Date/Time:
  LF_TEXT = TEXT-H02.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE SY-DATUM TO LF_TEMP1.
  WRITE SY-UZEIT TO LF_TEMP2.
  CONCATENATE LF_TEMP1 LF_TEMP2
         INTO LF_TEXT
    SEPARATED BY SPACE.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line3
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h03 : System/Client:
  LF_TEXT = TEXT-H03.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  CONCATENATE SY-SYSID SY-MANDT
         INTO LF_TEXT
    SEPARATED BY '/'.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line4
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h04 : Process By:
  LF_TEXT = TEXT-H04.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  LF_TEXT = SY-UNAME.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line 5
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h05 : Total Documents:
  LF_TEXT = TEXT-H05.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_SUM-TOTAL TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line 6
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h06 : Success Records:
  LF_TEXT = TEXT-H06.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_SUM-SUCCS TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LF_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_POSITIVE.

*-----------------------
* Add value in Line 7
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h07 : Failed Records:
  LF_TEXT = TEXT-H07.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_SUM-ERROR TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LF_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_NEGATIVE.

*-----------------------
* Add value in Line 8
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
  IF CB_TEST IS INITIAL.
*   Text-h09 : Production Run
    LF_TEXT = TEXT-H09.
  ELSE.
*   Text-h08 : Test Run
    LF_TEXT = TEXT-H08.
  ENDIF.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT      = LF_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_TOTAL.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_print_top_of_page_1
*----------------------------------------------------------------------*
*       Print Top of Page
*----------------------------------------------------------------------*
FORM F_PRINT_TOP_OF_PAGE_1.                                 "#EC CALLED

  DATA:
    LF_COL01 TYPE  I VALUE 25,
    LF_COL02 TYPE  I VALUE 35,
    LF_TEXT  TYPE  TEXT50,
    LF_TEMP1 TYPE  TEXT50,
    LF_TEMP2 TYPE  TEXT50.


  CONCATENATE SY-REPID SY-TITLE
         INTO LF_TEXT
    SEPARATED BY '-'.
* Text-h01 : Program:
  WRITE AT: /1(LF_COL01)  TEXT-H01 INTENSIFIED ON NO-GAP,
            (LF_COL02)    LF_TEXT NO-GAP.

  WRITE SY-DATUM TO LF_TEMP1.
  WRITE SY-UZEIT TO LF_TEMP2.
  CONCATENATE LF_TEMP1 LF_TEMP2
         INTO LF_TEXT
    SEPARATED BY SPACE.
* Text-h02 : Processing Date/Time:
  WRITE AT: /1(LF_COL01)  TEXT-H02 INTENSIFIED ON NO-GAP,
            (LF_COL02)    LF_TEXT NO-GAP.

  CONCATENATE SY-SYSID SY-MANDT
         INTO LF_TEXT
    SEPARATED BY '/'.
* Text-h03 : System/Client:
  WRITE AT: /1(LF_COL01)  TEXT-H03 INTENSIFIED ON NO-GAP,
            (LF_COL02)    LF_TEXT NO-GAP.

* Text-h04 : Process By:
  WRITE AT: /1(LF_COL01)  TEXT-H04 INTENSIFIED ON NO-GAP,
            (LF_COL02)    SY-UNAME NO-GAP.

* Text-h05 : Total Records:
  WRITE GS_SUM-TOTAL TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H05 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP.

* Text-h06 : Success Records:
  WRITE GS_SUM-SUCCS TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H06 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP COLOR COL_POSITIVE.

* Text-h07 : Failed Records:
  WRITE GS_SUM-ERROR TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H07 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP COLOR COL_NEGATIVE.
  CASE GC_TRUE .
    WHEN CB_TEST.
*   Text-h09 : Production Run
      LF_TEXT = TEXT-H09.
    WHEN CB_POST .
*   Text-h08 : Test Run
      LF_TEXT = TEXT-H08.
  ENDCASE.
  WRITE AT: /1(20) LF_TEXT CENTERED COLOR COL_TOTAL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_linetype
*&---------------------------------------------------------------------*
*& Define Line type
*&---------------------------------------------------------------------*
FORM F_GET_LINETYPE CHANGING CS_ITEM_DATA TYPE  TS_ITEM_DATA .

  DATA LF_SAKNR TYPE SKB1-SAKNR.

  IF CS_ITEM_DATA-NEWBS+0(1) EQ '0' OR "Customer line item
     CS_ITEM_DATA-NEWBS+0(1) EQ '1'.
************** AR_LINE ***************************
    CS_ITEM_DATA-LINETYPE = 'AR'.
  ELSEIF CS_ITEM_DATA-NEWBS+0(1) EQ '2' OR "Vendor line item
         CS_ITEM_DATA-NEWBS+0(1) EQ '3'.
************** AP_LINE ***************************
    CS_ITEM_DATA-LINETYPE  = 'AP'.
  ELSEIF  CS_ITEM_DATA-NEWBS+0(2) EQ '40' OR "GL line item & Tax line item
          CS_ITEM_DATA-NEWBS+0(2) EQ '50'.
**   Get Tax indicator by table T030K
    CLEAR: LF_SAKNR.
    CONVERT_ALPHA_INPUT  CS_ITEM_DATA-NEWKO LF_SAKNR.
    SELECT * FROM T030K INTO @DATA(LS_T030K) UP TO 1 ROWS  ##NEEDED
      WHERE KONTS EQ @LF_SAKNR
      ORDER BY   KTOPL.
    ENDSELECT.
    IF SY-SUBRC EQ 0.
*************** TAX_LINE ***************************
      CS_ITEM_DATA-LINETYPE  = 'TAX'.
    ELSE.
      IF CS_ITEM_DATA-ANLN1 IS NOT INITIAL .
*  ************* AA_LINE ***************************
        CS_ITEM_DATA-LINETYPE  = 'AA'.
      ELSE.
*  ************* GL_LINE ***************************
        CS_ITEM_DATA-LINETYPE  = 'GL'.
      ENDIF.
    ENDIF.


  ELSEIF CS_ITEM_DATA-NEWBS+0(2) EQ '70' OR "Asset line
         CS_ITEM_DATA-NEWBS+0(2) EQ '75'.
    CS_ITEM_DATA-LINETYPE  = 'AA'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_post_data
*&---------------------------------------------------------------------*
*& Posting data
*&---------------------------------------------------------------------*
FORM F_POST_DATA    USING  UT_DATA   TYPE  TT_DATA
                           UF_TEST   TYPE  FLAG
                  CHANGING CT_RESULT TYPE  TT_RESULT
                           CS_SUM    TYPE  TS_SUM.

  DATA:
    LT_MESSG TYPE  TT_MESSG.

  DATA:
    LS_DATA  TYPE  TS_DATA,
    LS_FIDOC TYPE  TS_FIDOC.

  DATA:
    LF_ERROR  TYPE  FLAG.


* Initialize Output
  CLEAR: CT_RESULT.
  CLEAR:   CS_SUM.

* Show Progress
* Text-p03 : Uploading file data. . .
  MC_SHOW_PROGRESS 45 TEXT-P03.

* Processing each record
  LOOP AT UT_DATA INTO LS_DATA.

*   Only non validation error
    IF LS_DATA-MESSG[] IS INITIAL.
      PERFORM F_POST_FI_DOC       USING  LS_DATA
                                         UF_TEST
                                CHANGING LS_FIDOC
                                         LT_MESSG
                                         LF_ERROR.
      IF LS_FIDOC IS NOT INITIAL.
        LS_DATA-RESULT = LS_FIDOC.
      ENDIF.
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

* Show Message Complete
* Text-i07: Processing completed.
  MESSAGE S000(ZSDSPS01) WITH TEXT-I07 SPACE SPACE SPACE.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_POST_FI_DOC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_POST_FI_DOC USING  US_DATA  TYPE  TS_DATA
                          UF_TEST  TYPE  FLAG
                 CHANGING CS_FIDOC TYPE  TS_FIDOC
                          CT_MESSG TYPE  TT_MESSG
                          CF_ERROR TYPE  FLAG.

* Iniaitlize Output
  CLEAR: CT_MESSG.
  CLEAR: CF_ERROR , CS_FIDOC.

  DATA:
    LS_T003          TYPE T003,
    LS_EXCH_RATE     TYPE BAPI1093_0,
    LS_ERROR_FLAG(1) TYPE C,
    LS_MESSG         TYPE TS_MESSG.

  ##NEEDED
  DATA:
    LS_ORIGINDOC TYPE  BAPIFAPO_DOC_REF,
    LS_RETURN_AA TYPE  BAPIRET2,
    LT_RETURN_AA TYPE  TY_T_BAPIRET2,
    LF_OBJ_TYPE  TYPE  BAPIACHE09-OBJ_TYPE,
    LF_OBJ_KEY   TYPE  BAPIACHE09-OBJ_KEY,
    LF_OBJ_SYS   TYPE  BAPIACHE09-OBJ_SYS.

  CLEAR: GS_DOCUMENTHEADER.
  CLEAR: GF_ITEMNO_ACC, GF_DOC_CUR , GF_DOC_EXCH.

*------- Header ----------
  GS_DOCUMENTHEADER-COMP_CODE   =  US_DATA-HEAD-BUKRS.
  GS_DOCUMENTHEADER-DOC_TYPE    =  US_DATA-HEAD-BLART.
  GS_DOCUMENTHEADER-PSTNG_DATE  =  US_DATA-HEAD-BUDAT.
  GS_DOCUMENTHEADER-DOC_DATE    =  US_DATA-HEAD-BLDAT.
*  GS_DOCUMENTHEADER-REF_DOC_NO  =  US_DATA-HEAD-XBLNR.
  GS_DOCUMENTHEADER-REF_DOC_NO  =  P_JVDOC.
  GS_DOCUMENTHEADER-HEADER_TXT  =  US_DATA-HEAD-BKTXT.
  GS_DOCUMENTHEADER-USERNAME    =  SY-UNAME.

  GS_DOCUMENTHEADER-FIS_PERIOD  =  US_DATA-HEAD-MONAT.
  GS_DOCUMENTHEADER-TRANS_DATE  =  US_DATA-HEAD-WWERT .
  GS_DOCUMENTHEADER-LEDGER_GROUP = US_DATA-HEAD-LDGRP .
  GS_DOCUMENTHEADER-TRANS_DATE   = US_DATA-HEAD-WWERT .

*   Document Currency
  GF_DOC_CUR     = US_DATA-HEAD-WAERS. "Document Currency

  IF GF_DOC_CUR NE 'THB' AND
     US_DATA-HEAD-KURSF  IS INITIAL.
    SELECT SINGLE * FROM T003 INTO LS_T003
      WHERE BLART EQ US_DATA-HEAD-BLART.
    IF SY-SUBRC EQ 0.
      CALL FUNCTION 'BAPI_EXCHANGERATE_GETDETAIL'
        EXPORTING
          RATE_TYPE  = LS_T003-KURST
          FROM_CURR  = GF_DOC_CUR
          TO_CURRNCY = 'THB'
          DATE       = US_DATA-HEAD-BUDAT
        IMPORTING
          EXCH_RATE  = LS_EXCH_RATE.

      GF_DOC_EXCH = LS_EXCH_RATE-EXCH_RATE.
    ENDIF.
  ELSE.
    GF_DOC_EXCH    = US_DATA-HEAD-KURSF . "Exchange rate
  ENDIF.

** Add Business place
*** Use BADI ACC_DOCUMENT with extension2
*  CLEAR: GS_EXTENSION2.
*  GS_EXTENSION2-STRUCTURE = 'ZSDSFIS007-BUPLA'.
*  GS_EXTENSION2-VALUEPART1 = '0000'. "Header
*  GS_EXTENSION2-VALUEPART2 = US_DATA-HEAD-BRNCH.
*  APPEND GS_EXTENSION2 TO GT_EXTENSION2.

*     Reset item number
  GF_ITEMNO_ACC  = '1'.
  DATA LS_DATA_ITEM TYPE TS_ITEM .

  LOOP AT US_DATA-ITEM INTO LS_DATA_ITEM .

    IF LS_DATA_ITEM-DATA-LINETYPE EQ 'AR'.
      PERFORM F_POPULATE_AR_LINE  USING US_DATA-HEAD
                                        LS_DATA_ITEM-DATA.
    ELSEIF LS_DATA_ITEM-DATA-LINETYPE  EQ 'AP'.
      PERFORM F_POPULATE_AP_LINE  USING US_DATA-HEAD
                                        LS_DATA_ITEM-DATA.
    ELSEIF LS_DATA_ITEM-DATA-LINETYPE  EQ 'GL'.
      PERFORM F_POPULATE_GL_LINE USING LS_DATA_ITEM-DATA.

    ELSEIF LS_DATA_ITEM-DATA-LINETYPE EQ 'AA'.
      PERFORM F_POPULATE_AA_LINE  USING US_DATA-HEAD
                                        LS_DATA_ITEM-DATA.
    ENDIF.

    "If Tax amount => Generate Linetaxx
    IF  LS_DATA_ITEM-DATA-WMWST IS NOT INITIAL

     OR ( LS_DATA_ITEM-DATA-WMWST IS INITIAL AND
          LS_DATA_ITEM-DATA-UMSKZ IS NOT INITIAL ) ##BOOL_OK.
      PERFORM F_POPULATE_TAX_LINE USING US_DATA-HEAD-BUKRS
                                        US_DATA-HEAD-BLART
                                        LS_DATA_ITEM-DATA.

    ENDIF.

    GF_ITEMNO_ACC  = GF_ITEMNO_ACC + 1.
  ENDLOOP.

*   Post Account document
  IF UF_TEST EQ 'X'. "Test run
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
      EXPORTING
        DOCUMENTHEADER    = GS_DOCUMENTHEADER
        CUSTOMERCPD       = GS_CUSTOMERCPD
      TABLES
        ACCOUNTGL         = GT_ACCOUNTGL
        ACCOUNTRECEIVABLE = GT_ACCOUNTRECEIVABLE
        ACCOUNTPAYABLE    = GT_ACCOUNTPAYABLE
        ACCOUNTTAX        = GT_ACCOUNTTAX
        CURRENCYAMOUNT    = GT_CURRENCYAMOUNT
        CRITERIA          = GT_CRITERIA
*       VALUEFIELD        =
        EXTENSION1        = GT_EXTENSION1
        RETURN            = GT_RETURN
        EXTENSION2        = GT_EXTENSION2
        ACCOUNTWT         = GT_ACCOUNTWT.
    CLEAR: LS_ERROR_FLAG.
    LOOP AT GT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>)
                          WHERE ( TYPE EQ 'E' OR
*                                  TYPE EQ 'W' OR
                                  TYPE EQ 'A' ).
*      IF <L_RETURN>-TYPE EQ 'W'.
*        IF <L_RETURN>-ID EQ 'KI' AND <L_RETURN>-NUMBER EQ '350'.
*        ELSE.
*          LS_WARNING_FLAG = 'X'.
*        ENDIF.
*      ELSE.
      LS_ERROR_FLAG = 'X'.
*      ENDIF.
    ENDLOOP.

*--AA Retirement--
    IF GS_GEN_INFO IS NOT INITIAL.

      CLEAR: LS_ORIGINDOC, LS_RETURN_AA .

      LS_ORIGINDOC-OBJ_TYPE = 'AMBU'.
      LS_ORIGINDOC-OBJ_KEY  = LF_OBJ_KEY.
      LS_ORIGINDOC-OBJ_SYS  = LF_OBJ_SYS.

      CALL FUNCTION 'BAPI_ASSET_RETIREMENT_POST'
        EXPORTING
          ORIGINDOCREFERENCE = LS_ORIGINDOC
          GENERALPOSTINGDATA = GS_GEN_INFO
          RETIREMENTDATA     = GS_RETIREMENT
          FURTHERPOSTINGDATA = GS_FURTHER
        IMPORTING
          RETURN             = LS_RETURN_AA
        TABLES
          RETURN_ALL         = LT_RETURN_AA.

      IF LS_RETURN_AA-TYPE = 'E' OR  LS_RETURN_AA-TYPE = 'A' .
        LS_ERROR_FLAG = 'X' .

        CLEAR LS_MESSG.
        LS_MESSG-ROWNO = LS_DATA_ITEM-DATA-ROWNO.
        LS_MESSG-MSGTY = LS_RETURN_AA-TYPE.
        LS_MESSG-MSGID = LS_RETURN_AA-ID.
        LS_MESSG-MSGNO = LS_RETURN_AA-NUMBER.
        LS_MESSG-MSGTX = LS_RETURN_AA-MESSAGE .
        INSERT LS_MESSG INTO TABLE CT_MESSG.
      ENDIF.
    ENDIF.
*-AA-- Retriment

    IF LS_ERROR_FLAG   EQ 'X' .

      CF_ERROR = GC_TRUE.

      LOOP AT GT_RETURN ASSIGNING <L_RETURN>
                            WHERE ( TYPE EQ 'E' OR
*                                    TYPE EQ 'W' OR
                                    TYPE EQ 'A' ).
        IF ( <L_RETURN>-ID EQ 'KI' AND <L_RETURN>-NUMBER EQ '350' ) OR
           ( <L_RETURN>-ID EQ 'RW' AND <L_RETURN>-NUMBER EQ '609' ) .

          CONTINUE. "Skip this warning message
        ENDIF.

        CLEAR LS_MESSG.
        LS_MESSG-ROWNO = LS_DATA_ITEM-DATA-ROWNO.
        LS_MESSG-MSGTY = <L_RETURN>-TYPE.
        LS_MESSG-MSGID = <L_RETURN>-ID.
        LS_MESSG-MSGNO = <L_RETURN>-NUMBER.
        LS_MESSG-MSGTX = <L_RETURN>-MESSAGE .
        INSERT LS_MESSG INTO TABLE CT_MESSG.
      ENDLOOP.

    ELSE. "Success
      CLEAR LS_MESSG.
      LS_MESSG-ROWNO = LS_DATA_ITEM-DATA-ROWNO.
      LS_MESSG-MSGTY = 'S'.
      LS_MESSG-MSGID = 'ZSDSCA01'.
      LS_MESSG-MSGNO = '000'.
*     Text-i01: Test upload successfully.
      LS_MESSG-MSGTX = TEXT-I01.

      INSERT LS_MESSG INTO TABLE CT_MESSG.
    ENDIF.


  ELSE. "Actual Run
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        DOCUMENTHEADER    = GS_DOCUMENTHEADER
        CUSTOMERCPD       = GS_CUSTOMERCPD
      IMPORTING
        OBJ_TYPE          = LF_OBJ_TYPE
        OBJ_KEY           = LF_OBJ_KEY
        OBJ_SYS           = LF_OBJ_SYS
      TABLES
        ACCOUNTGL         = GT_ACCOUNTGL
        ACCOUNTRECEIVABLE = GT_ACCOUNTRECEIVABLE
        ACCOUNTPAYABLE    = GT_ACCOUNTPAYABLE
        ACCOUNTTAX        = GT_ACCOUNTTAX
        CURRENCYAMOUNT    = GT_CURRENCYAMOUNT
        CRITERIA          = GT_CRITERIA
*       VALUEFIELD        =
        EXTENSION1        = GT_EXTENSION1
        RETURN            = GT_RETURN
        EXTENSION2        = GT_EXTENSION2
        ACCOUNTWT         = GT_ACCOUNTWT.

    CLEAR: LS_ERROR_FLAG.
    LOOP AT GT_RETURN ASSIGNING <L_RETURN>
                          WHERE ( TYPE EQ 'E' OR
                                  TYPE EQ 'A' ).
      LS_ERROR_FLAG = 'X'.
    ENDLOOP.

*--AA Retirement--
    IF GS_GEN_INFO IS NOT INITIAL.

      CLEAR: LS_ORIGINDOC, LS_RETURN_AA .

      LS_ORIGINDOC-OBJ_TYPE = 'AMBU'.
      LS_ORIGINDOC-OBJ_KEY  = LF_OBJ_KEY.
      LS_ORIGINDOC-OBJ_SYS  = LF_OBJ_SYS.

      CALL FUNCTION 'BAPI_ASSET_RETIREMENT_POST'
        EXPORTING
          ORIGINDOCREFERENCE = LS_ORIGINDOC
          GENERALPOSTINGDATA = GS_GEN_INFO
          RETIREMENTDATA     = GS_RETIREMENT
          FURTHERPOSTINGDATA = GS_FURTHER
        IMPORTING
          RETURN             = LS_RETURN_AA
        TABLES
          RETURN_ALL         = LT_RETURN_AA.

      IF LS_RETURN_AA-TYPE = 'E' OR  LS_RETURN_AA-TYPE = 'A' .
        LS_ERROR_FLAG = 'X' .

        CLEAR LS_MESSG.
        LS_MESSG-ROWNO = LS_DATA_ITEM-DATA-ROWNO.
        LS_MESSG-MSGTY = LS_RETURN_AA-TYPE.
        LS_MESSG-MSGID = LS_RETURN_AA-ID.
        LS_MESSG-MSGNO = LS_RETURN_AA-NUMBER.
        LS_MESSG-MSGTX = LS_RETURN_AA-MESSAGE .
        INSERT LS_MESSG INTO TABLE CT_MESSG.
      ENDIF.
    ENDIF.
*-AA-- Retriment

    IF LS_ERROR_FLAG EQ 'X'. "Error
      LOOP AT GT_RETURN ASSIGNING <L_RETURN> WHERE ( TYPE EQ 'E' OR
                                                     TYPE EQ 'A' ) .
        CLEAR LS_MESSG.
        LS_MESSG-ROWNO = LS_DATA_ITEM-DATA-ROWNO.
        LS_MESSG-MSGTY = <L_RETURN>-TYPE.
        LS_MESSG-MSGID = <L_RETURN>-ID.
        LS_MESSG-MSGNO = <L_RETURN>-NUMBER.
        LS_MESSG-MSGTX = <L_RETURN>-MESSAGE .
        INSERT LS_MESSG INTO TABLE CT_MESSG.
      ENDLOOP.
    ELSE. "Success
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      READ TABLE GT_RETURN ASSIGNING <L_RETURN> WITH KEY ID     = 'RW'
                                                         NUMBER = '605'.

      CLEAR LS_MESSG.
      LS_MESSG-ROWNO = LS_DATA_ITEM-DATA-ROWNO.
      LS_MESSG-MSGTY = 'S'.
      LS_MESSG-MSGID = 'ZSDSCA01'.
      LS_MESSG-MSGNO = '000'.
*     Text-i02: FI Document created successfully.
      LS_MESSG-MSGTX = TEXT-I02.
      INSERT LS_MESSG INTO TABLE CT_MESSG.

      CS_FIDOC-BUKRS = US_DATA-HEAD-BUKRS.
      CS_FIDOC-BELNR = <L_RETURN>-MESSAGE_V2(10).
      CS_FIDOC-GJAHR = <L_RETURN>-MESSAGE_V2+14(4).
    ENDIF.

  ENDIF.

* Clear value
  CLEAR:   GS_CUSTOMERCPD, GS_GEN_INFO, GS_RETIREMENT .

  CLEAR:   GT_ACCOUNTGL, GT_ACCOUNTRECEIVABLE, GT_ACCOUNTPAYABLE,
           GT_ACCOUNTTAX, GT_CURRENCYAMOUNT, GT_CRITERIA, GT_EXTENSION1, GT_EXTENSION2,
           GT_RETURN, GT_ACCOUNTWT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_POPULATE_GL_LINE
*&---------------------------------------------------------------------*
*& Populate data for line type GL
*&---------------------------------------------------------------------*
FORM F_POPULATE_GL_LINE  USING US_ITEM           TYPE TS_ITEM_DATA.

  DATA:
*    LF_TAX_AMT_LC TYPE BAPIACCR09-TAX_AMT,
*    LF_BAS_AMT_LC TYPE BAPIACCR09-TAX_AMT,
    LS_ACCOUNTGL  TYPE BAPIACGL09.

  CLEAR: LS_ACCOUNTGL.

  LS_ACCOUNTGL-ITEMNO_ACC = GF_ITEMNO_ACC.
  LS_ACCOUNTGL-GL_ACCOUNT = US_ITEM-NEWKO .
  LS_ACCOUNTGL-COSTCENTER = US_ITEM-KOSTL.
  LS_ACCOUNTGL-PROFIT_CTR = US_ITEM-PRCTR.

  LS_ACCOUNTGL-BUS_AREA       =  '0001'.
  LS_ACCOUNTGL-TAX_CODE       =  US_ITEM-MWSKZ .
*-Beg of INS
  LS_ACCOUNTGL-WBS_ELEMENT    =  US_ITEM-POSID.
*-End of INS
  LS_ACCOUNTGL-VALUE_DATE     =  US_ITEM-VALUT.
  LS_ACCOUNTGL-ALLOC_NMBR     =  US_ITEM-ZUONR.
  LS_ACCOUNTGL-ITEM_TEXT      =  US_ITEM-SGTXT.
  LS_ACCOUNTGL-REF_KEY_1      =  US_ITEM-XREF1.
  LS_ACCOUNTGL-REF_KEY_2      =  US_ITEM-XREF2.
  LS_ACCOUNTGL-REF_KEY_3      =  US_ITEM-XREF3.


  LS_ACCOUNTGL-COSTCENTER =  US_ITEM-KOSTL.
  LS_ACCOUNTGL-PROFIT_CTR =  US_ITEM-PRCTR.
**ls_accountgl-   US_ITEM--PROJK.
  LS_ACCOUNTGL-ORDERID =  US_ITEM-AUFNR.
  LS_ACCOUNTGL-PARTNER_SEGMENT  = US_ITEM-PSEGMENT.
  LS_ACCOUNTGL-MATERIAL_LONG    = US_ITEM-MATNR.

  APPEND LS_ACCOUNTGL TO GT_ACCOUNTGL.

* Transaction Currency
  IF US_ITEM-WRBTR IS NOT INITIAL.
    CLEAR: GS_CURRENCYAMOUNT.
    GS_CURRENCYAMOUNT-ITEMNO_ACC      = GF_ITEMNO_ACC .
    GS_CURRENCYAMOUNT-CURRENCY        = GF_DOC_CUR.
    GS_CURRENCYAMOUNT-CURR_TYPE       = '00'.
    IF GF_DOC_EXCH IS INITIAL.
      GS_CURRENCYAMOUNT-EXCH_RATE       = 1.
    ELSE.
      GS_CURRENCYAMOUNT-EXCH_RATE       = GF_DOC_EXCH.
    ENDIF.

    IF US_ITEM-SHKZG = 'H'.
      GS_CURRENCYAMOUNT-AMT_DOCCUR  = ABS( US_ITEM-WRBTR ) * ( -1 ).
      GS_CURRENCYAMOUNT-TAX_AMT     = ABS( US_ITEM-WMWST ) * ( -1 ).
    ELSEIF US_ITEM-SHKZG = 'S'.
      GS_CURRENCYAMOUNT-AMT_DOCCUR  = ABS( US_ITEM-WRBTR ).
      GS_CURRENCYAMOUNT-TAX_AMT     = ABS( US_ITEM-WMWST ).
    ENDIF.



    APPEND GS_CURRENCYAMOUNT TO GT_CURRENCYAMOUNT.
  ENDIF.

  PERFORM F_SET_CRITERIA USING US_ITEM .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_POPULATE_AMOUNT
*&---------------------------------------------------------------------*
FORM F_POPULATE_AMOUNT  USING UF_BSCHL         TYPE RF05A-NEWBS
                              UF_INPUT_AMOUNT  TYPE BSEG-FWBAS
                     CHANGING CF_OUTPUT_AMOUNT TYPE BAPIACCR09-TAX_AMT.

* Reformat amount.
*  REPLACE ALL OCCURRENCES OF ',' IN p_input_amount WITH ''.

  READ TABLE GT_TBSL WITH KEY BSCHL = UF_BSCHL TRANSPORTING NO FIELDS .
  IF SY-SUBRC EQ 0.
    IF GS_TBSL-SHKZG EQ 'H'. "Credit
      CF_OUTPUT_AMOUNT = UF_INPUT_AMOUNT * -1. "Convert to negative
    ELSE.
      CF_OUTPUT_AMOUNT = UF_INPUT_AMOUNT.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_POPULATE_AR_LINE
*&---------------------------------------------------------------------*

FORM F_POPULATE_AR_LINE  USING US_HEAD  TYPE TS_HEAD
                               US_ITEM  TYPE TS_ITEM_DATA.

* One time payee
  PERFORM F_SET_ONETIME_DATA  USING US_ITEM .


  CLEAR: GS_ACCOUNTRECEIVABLE.
  GS_ACCOUNTRECEIVABLE-ITEMNO_ACC      = GF_ITEMNO_ACC .

  CONVERT_ALPHA_INPUT  US_ITEM-NEWKO  GS_ACCOUNTRECEIVABLE-CUSTOMER.
  GS_ACCOUNTRECEIVABLE-SP_GL_IND       = US_ITEM-UMSKZ.
  GS_ACCOUNTRECEIVABLE-TAX_CODE        = US_ITEM-MWSKZ.
  GS_ACCOUNTRECEIVABLE-BUSINESSPLACE   = US_HEAD-BRNCH.
*  GS_ACCOUNTRECEIVABLE-BUS_AREA        = GS_FILE-BUSINESS_AREA.
  CONVERT_ALPHA_INPUT US_ITEM-PRCTR GS_ACCOUNTRECEIVABLE-PROFIT_CTR.

  GS_ACCOUNTRECEIVABLE-BLINE_DATE  = US_ITEM-ZFBDT.

  IF US_ITEM-ZTERM IS NOT INITIAL.
    GS_ACCOUNTRECEIVABLE-PMNTTRMS        = US_ITEM-ZTERM.
  ELSE.
    SELECT SINGLE ZTERM
      INTO GS_ACCOUNTRECEIVABLE-PMNTTRMS
      FROM KNB1
      WHERE KUNNR EQ GS_ACCOUNTRECEIVABLE-CUSTOMER
      AND   BUKRS EQ US_HEAD-BUKRS .

  ENDIF.
  GS_ACCOUNTRECEIVABLE-PYMT_METH       = US_ITEM-ZLSCH.
  GS_ACCOUNTRECEIVABLE-PMNT_BLOCK      = US_ITEM-ZLSPR.
  GS_ACCOUNTRECEIVABLE-ALLOC_NMBR      = US_ITEM-ZUONR.
  GS_ACCOUNTRECEIVABLE-ITEM_TEXT       = US_ITEM-SGTXT.
  GS_ACCOUNTRECEIVABLE-REF_KEY_1       = US_ITEM-XREF1.
  GS_ACCOUNTRECEIVABLE-REF_KEY_2       = US_ITEM-XREF2.
  GS_ACCOUNTRECEIVABLE-REF_KEY_3       = US_ITEM-XREF3.
  GS_ACCOUNTRECEIVABLE-PROFIT_CTR      = US_ITEM-PRCTR.
  GS_ACCOUNTRECEIVABLE-GL_ACCOUNT      = US_ITEM-GL_ACCOUNT.
  APPEND GS_ACCOUNTRECEIVABLE TO GT_ACCOUNTRECEIVABLE.

* Transaction Currency
  IF US_ITEM-WRBTR IS NOT INITIAL.
    CLEAR: GS_CURRENCYAMOUNT.
    GS_CURRENCYAMOUNT-ITEMNO_ACC      = GF_ITEMNO_ACC .
    GS_CURRENCYAMOUNT-CURRENCY        = GF_DOC_CUR.
    GS_CURRENCYAMOUNT-CURR_TYPE       = '00'.
    IF GF_DOC_EXCH IS INITIAL.
      GS_CURRENCYAMOUNT-EXCH_RATE       = 1.
    ELSE.
      GS_CURRENCYAMOUNT-EXCH_RATE       = GF_DOC_EXCH.
    ENDIF.

    IF US_ITEM-SHKZG = 'H'.
      GS_CURRENCYAMOUNT-AMT_DOCCUR  = ABS( US_ITEM-WRBTR ) * ( -1 ).
      GS_CURRENCYAMOUNT-TAX_AMT    = ABS( US_ITEM-WMWST ) * ( -1 ).
      GS_CURRENCYAMOUNT-AMT_BASE   = ABS( US_ITEM-FWBAS ) * ( -1 ).
    ELSEIF US_ITEM-SHKZG = 'S'.
      GS_CURRENCYAMOUNT-AMT_DOCCUR = ABS( US_ITEM-WRBTR ).
      GS_CURRENCYAMOUNT-TAX_AMT    = ABS( US_ITEM-WMWST ).
      GS_CURRENCYAMOUNT-AMT_BASE   = ABS( US_ITEM-FWBAS ).
    ENDIF.

    APPEND GS_CURRENCYAMOUNT TO GT_CURRENCYAMOUNT.
  ENDIF.

*  Withholding tax data from Customer
  PERFORM F_SET_WITHHOLDING_TAX USING US_HEAD-BUKRS
                                       US_ITEM .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPULATE_WITHHT_BAS_AMT
*&---------------------------------------------------------------------*

FORM POPULATE_WITHHT_BAS_AMT  USING UF_BUKRS      TYPE BKPF-BUKRS
                                    UF_INPUT_AMT  TYPE WITH_ITEM-WT_QSSHB
                                    UF_TAX_CODE   TYPE WITH_ITEM-WT_WITHCD
                           CHANGING CF_BAS_AMT_TC TYPE BAPIWT_BS1.

  DATA: LS_WRBTR            TYPE BSEG-WRBTR,
        LT_MWDAT            TYPE TABLE OF RTAX1U15,
        LS_MWDAT            TYPE RTAX1U15,
        LS_INPUT_AMOUNT(15) TYPE C.

  LS_INPUT_AMOUNT = UF_INPUT_AMT.

*  REPLACE ALL OCCURRENCES OF ',' IN LS_INPUT_AMOUNT WITH ''.

  IF UF_INPUT_AMT IS NOT INITIAL.
    CF_BAS_AMT_TC = LS_INPUT_AMOUNT.
  ELSE.
*   Calculate amount
    LS_WRBTR = UF_INPUT_AMT.
    CLEAR: LT_MWDAT.
    CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT'
      EXPORTING
        I_BUKRS                 = UF_BUKRS
        I_MWSKZ                 = UF_TAX_CODE
*       I_TXJCD                 = ' '
        I_WAERS                 = 'THB'
        I_WRBTR                 = LS_WRBTR
*       I_ZBD1P                 = 0
*       I_PRSDT                 =
*       I_PROTOKOLL             =
*       I_TAXPS                 =
*       I_ACCNT_EXT             =
*       I_ACCDATA               =
*       IS_ENHANCEMENT          =
*       I_PRICING_REFRESH_TX    = ' '
      TABLES
        T_MWDAT                 = LT_MWDAT
      EXCEPTIONS
        BUKRS_NOT_FOUND         = 1
        COUNTRY_NOT_FOUND       = 2
        MWSKZ_NOT_DEFINED       = 3
        MWSKZ_NOT_VALID         = 4
        ACCOUNT_NOT_FOUND       = 5
        DIFFERENT_DISCOUNT_BASE = 6
        DIFFERENT_TAX_BASE      = 7
        TXJCD_NOT_VALID         = 8
        NOT_FOUND               = 9
        KTOSL_NOT_FOUND         = 10
        KALSM_NOT_FOUND         = 11
        PARAMETER_ERROR         = 12
        KNUMH_NOT_FOUND         = 13
        KSCHL_NOT_FOUND         = 14
        UNKNOWN_ERROR           = 15
        OTHERS                  = 16.
    IF SY-SUBRC EQ 0.
      READ TABLE LT_MWDAT INTO LS_MWDAT INDEX 1.
      IF SY-SUBRC EQ 0.
        CF_BAS_AMT_TC = LS_MWDAT-KAWRT.
      ENDIF.
    ENDIF.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_POPULATE_AP_LINE
*&---------------------------------------------------------------------*
*& Populate AP Line
*&---------------------------------------------------------------------*
FORM F_POPULATE_AP_LINE  USING US_HEAD  TYPE TS_HEAD
                               US_ITEM  TYPE TS_ITEM_DATA.

*   One time payee
  PERFORM F_SET_ONETIME_DATA  USING US_ITEM .

  CLEAR: GS_ACCOUNTPAYABLE.
  GS_ACCOUNTPAYABLE-ITEMNO_ACC      = GF_ITEMNO_ACC .
  CONVERT_ALPHA_INPUT US_ITEM-NEWKO  GS_ACCOUNTPAYABLE-VENDOR_NO.
  GS_ACCOUNTPAYABLE-SP_GL_IND       = US_ITEM-UMSKZ.
  GS_ACCOUNTPAYABLE-TAX_CODE        = US_ITEM-MWSKZ.
  GS_ACCOUNTPAYABLE-BUSINESSPLACE   = US_HEAD-BRNCH.
*  GS_ACCOUNTPAYABLE-BUS_AREA        = GS_FILE-BUSINESS_AREA.
  CONVERT_ALPHA_INPUT US_ITEM-PRCTR GS_ACCOUNTPAYABLE-PROFIT_CTR.

  GS_ACCOUNTPAYABLE-BLINE_DATE = US_ITEM-ZFBDT.

  IF US_ITEM-ZTERM IS NOT INITIAL.
    GS_ACCOUNTPAYABLE-PMNTTRMS        = US_ITEM-ZTERM.
  ELSE.
    SELECT SINGLE ZTERM
      INTO GS_ACCOUNTPAYABLE-PMNTTRMS
      FROM LFB1
     WHERE LIFNR EQ GS_ACCOUNTPAYABLE-VENDOR_NO
       AND BUKRS EQ US_HEAD-BUKRS .

  ENDIF.
  GS_ACCOUNTPAYABLE-PYMT_METH       = US_ITEM-ZLSCH.
  GS_ACCOUNTPAYABLE-PMNT_BLOCK      = US_ITEM-ZLSPR.
  GS_ACCOUNTPAYABLE-ALLOC_NMBR      = US_ITEM-ZUONR.
  GS_ACCOUNTPAYABLE-ITEM_TEXT       = US_ITEM-SGTXT.
  GS_ACCOUNTPAYABLE-REF_KEY_1       = US_ITEM-XREF1.
  GS_ACCOUNTPAYABLE-REF_KEY_2       = US_ITEM-XREF2.
  GS_ACCOUNTPAYABLE-REF_KEY_3       = US_ITEM-XREF3.
  GS_ACCOUNTPAYABLE-GL_ACCOUNT      = US_ITEM-GL_ACCOUNT.
  GS_ACCOUNTPAYABLE-PROFIT_CTR =  US_ITEM-PRCTR.

  APPEND GS_ACCOUNTPAYABLE TO GT_ACCOUNTPAYABLE.

* Transaction Currency
  IF US_ITEM-WRBTR IS NOT INITIAL.
    CLEAR: GS_CURRENCYAMOUNT.
    GS_CURRENCYAMOUNT-ITEMNO_ACC      = GF_ITEMNO_ACC.
    GS_CURRENCYAMOUNT-CURRENCY        = GF_DOC_CUR.
    GS_CURRENCYAMOUNT-CURR_TYPE       = '00'.
    IF GF_DOC_EXCH IS INITIAL.
      GS_CURRENCYAMOUNT-EXCH_RATE       = 1.
    ELSE.
      GS_CURRENCYAMOUNT-EXCH_RATE       =  GF_DOC_EXCH.
    ENDIF.

    IF US_ITEM-SHKZG = 'H'.
      GS_CURRENCYAMOUNT-AMT_DOCCUR  = ABS( US_ITEM-WRBTR ) * ( -1 ).
      GS_CURRENCYAMOUNT-TAX_AMT    = ABS( US_ITEM-WMWST ) * ( -1 ).
      GS_CURRENCYAMOUNT-AMT_BASE   = ABS( US_ITEM-FWBAS ) * ( -1 ).
    ELSEIF US_ITEM-SHKZG = 'S'.
      GS_CURRENCYAMOUNT-AMT_DOCCUR = ABS( US_ITEM-WRBTR ).
      GS_CURRENCYAMOUNT-TAX_AMT    = ABS( US_ITEM-WMWST ).
      GS_CURRENCYAMOUNT-AMT_BASE   = ABS( US_ITEM-FWBAS ).
    ENDIF.


    APPEND GS_CURRENCYAMOUNT TO GT_CURRENCYAMOUNT.
  ENDIF.

* Set Withholding tax data from Vendor
  PERFORM F_SET_WITHHOLDING_TAX USING US_HEAD-BUKRS
                                       US_ITEM .


ENDFORM.                    "populate_ap_line

*&---------------------------------------------------------------------*
*& Form F_SET_ONETIME_DATA
*&---------------------------------------------------------------------*
*& Set One time data to structure
*&---------------------------------------------------------------------*
FORM F_SET_ONETIME_DATA  USING  US_ITEM TYPE TS_ITEM_DATA.

  CLEAR: GS_CUSTOMERCPD.
*   One time payee/customer
  IF US_ITEM-ONETIME_NAME1 IS NOT INITIAL.
    CLEAR: GS_CUSTOMERCPD.
    GS_CUSTOMERCPD-NAME               = US_ITEM-ONETIME_NAME1.
    GS_CUSTOMERCPD-NAME_2             = US_ITEM-ONETIME_NAME2.
    GS_CUSTOMERCPD-NAME_3             = US_ITEM-ONETIME_NAME3.
    GS_CUSTOMERCPD-NAME_4             = US_ITEM-ONETIME_NAME4.
    GS_CUSTOMERCPD-STREET             = US_ITEM-ONETIME_STREET.
    GS_CUSTOMERCPD-CITY               = US_ITEM-ONETIME_CITY.
    GS_CUSTOMERCPD-POSTL_CODE         = US_ITEM-ONETIME_POSTAL.
    GS_CUSTOMERCPD-COUNTRY            = US_ITEM-ONETIME_COUNTRY.
    GS_CUSTOMERCPD-BANK_NO            = US_ITEM-ONETIME_BANK_KEY.
    GS_CUSTOMERCPD-BANK_ACCT          = US_ITEM-ONETIME_BANK_ACCT_NO.
    GS_CUSTOMERCPD-BANK_CTRY          = US_ITEM-ONETIME_BANK_COUNTRY.
    IF GS_CUSTOMERCPD-BANK_CTRY IS INITIAL .
      SELECT BANKS INTO GS_CUSTOMERCPD-BANK_CTRY UP TO 1 ROWS
        FROM BNKA
       WHERE BANKL EQ GS_CUSTOMERCPD-BANK_NO
       ORDER BY BANKS.
      ENDSELECT.
    ENDIF.

    GS_CUSTOMERCPD-LANGU_ISO          = 'EN'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_withholding_tax
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> US_ITEM
*&---------------------------------------------------------------------*
FORM F_SET_WITHHOLDING_TAX  USING UF_BUKRS TYPE BKPF-BUKRS
                                  US_ITEM TYPE TS_ITEM_DATA .

  DATA: LS_ACCOUNTWT     TYPE BAPIACWT09 .

  IF US_ITEM-WT_WITHCD01 IS NOT INITIAL .
    CLEAR: LS_ACCOUNTWT.
    LS_ACCOUNTWT-ITEMNO_ACC  =  GF_ITEMNO_ACC .
    LS_ACCOUNTWT-WT_TYPE     =  US_ITEM-WT_WITHT01 .
    LS_ACCOUNTWT-WT_CODE     =  US_ITEM-WT_WITHCD01.

    LS_ACCOUNTWT-BAS_AMT_TC  = US_ITEM-WT_QSSHB01.
    LS_ACCOUNTWT-BAS_AMT_LC  = US_ITEM-WT_QSSHH01.
*WT_QBSHB01 Withholding Tax Amount in Document Currency
*WT_QBSHH01 Withholding Tax Amount in Local Currency

    IF US_ITEM-WT_WITHCD01    IS NOT INITIAL AND
      LS_ACCOUNTWT-BAS_AMT_TC IS NOT INITIAL .
      PERFORM POPULATE_WITHHT_BAS_AMT USING UF_BUKRS
                                            US_ITEM-WT_QSSHB01
                                            US_ITEM-WT_WITHCD01
                                   CHANGING LS_ACCOUNTWT-BAS_AMT_TC.
    ENDIF.

    APPEND LS_ACCOUNTWT TO GT_ACCOUNTWT.
  ENDIF.

  IF US_ITEM-WT_WITHCD02 IS NOT INITIAL .
    CLEAR: LS_ACCOUNTWT.
    LS_ACCOUNTWT-ITEMNO_ACC  =  GF_ITEMNO_ACC .
    LS_ACCOUNTWT-WT_TYPE     =  US_ITEM-WT_WITHT02 .
    LS_ACCOUNTWT-WT_CODE     =  US_ITEM-WT_WITHCD02.

    LS_ACCOUNTWT-BAS_AMT_TC  = US_ITEM-WT_QSSHB02.
    LS_ACCOUNTWT-BAS_AMT_LC  = US_ITEM-WT_QSSHH02.
*WT_QBSHB01 Withholding Tax Amount in Document Currency
*WT_QBSHH01 Withholding Tax Amount in Local Currency

    IF US_ITEM-WT_WITHCD02    IS NOT INITIAL AND
      LS_ACCOUNTWT-BAS_AMT_TC IS NOT INITIAL .
      PERFORM POPULATE_WITHHT_BAS_AMT USING UF_BUKRS
                                            US_ITEM-WT_QSSHB02
                                            US_ITEM-WT_WITHCD02
                                   CHANGING LS_ACCOUNTWT-BAS_AMT_TC.
    ENDIF.

    APPEND LS_ACCOUNTWT TO GT_ACCOUNTWT.
  ENDIF.

  IF US_ITEM-WT_WITHCD03 IS NOT INITIAL .
    CLEAR: LS_ACCOUNTWT.
    LS_ACCOUNTWT-ITEMNO_ACC  =  GF_ITEMNO_ACC .
    LS_ACCOUNTWT-WT_TYPE     =  US_ITEM-WT_WITHT03 .
    LS_ACCOUNTWT-WT_CODE     =  US_ITEM-WT_WITHCD03.

    LS_ACCOUNTWT-BAS_AMT_TC  = US_ITEM-WT_QSSHB03.
    LS_ACCOUNTWT-BAS_AMT_LC  = US_ITEM-WT_QSSHH03.
*WT_QBSHB01 Withholding Tax Amount in Document Currency
*WT_QBSHH01 Withholding Tax Amount in Local Currency

    IF US_ITEM-WT_WITHCD03    IS NOT INITIAL AND
      LS_ACCOUNTWT-BAS_AMT_TC IS NOT INITIAL .
      PERFORM POPULATE_WITHHT_BAS_AMT USING UF_BUKRS
                                            US_ITEM-WT_QSSHB03
                                            US_ITEM-WT_WITHCD03
                                   CHANGING LS_ACCOUNTWT-BAS_AMT_TC.
    ENDIF.

    APPEND LS_ACCOUNTWT TO GT_ACCOUNTWT.
  ENDIF.

  IF US_ITEM-WT_WITHCD04 IS NOT INITIAL .
    CLEAR: LS_ACCOUNTWT.
    LS_ACCOUNTWT-ITEMNO_ACC  =  GF_ITEMNO_ACC .
    LS_ACCOUNTWT-WT_TYPE     =  US_ITEM-WT_WITHT04 .
    LS_ACCOUNTWT-WT_CODE     =  US_ITEM-WT_WITHCD04.

    LS_ACCOUNTWT-BAS_AMT_TC  = US_ITEM-WT_QSSHB04.
    LS_ACCOUNTWT-BAS_AMT_LC  = US_ITEM-WT_QSSHH04.
*WT_QBSHB01 Withholding Tax Amount in Document Currency
*WT_QBSHH01 Withholding Tax Amount in Local Currency

    IF US_ITEM-WT_WITHCD04    IS NOT INITIAL AND
      LS_ACCOUNTWT-BAS_AMT_TC IS NOT INITIAL .
      PERFORM POPULATE_WITHHT_BAS_AMT USING UF_BUKRS
                                            US_ITEM-WT_QSSHB04
                                            US_ITEM-WT_WITHCD04
                                   CHANGING LS_ACCOUNTWT-BAS_AMT_TC.
    ENDIF.

    APPEND LS_ACCOUNTWT TO GT_ACCOUNTWT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_POPULATE_TAX_LINE
*&---------------------------------------------------------------------*
*& Populate Tax Line
*&---------------------------------------------------------------------*
FORM F_POPULATE_TAX_LINE  USING UF_BUKRS TYPE BKPF-BUKRS
                                UF_BLART TYPE BKPF-BLART
                                US_ITEM  TYPE TS_ITEM_DATA .
  ##NEEDED
  DATA: LT_MWDAT TYPE TABLE OF RTAX1U15,
*        LS_MWDAT       TYPE RTAX1U15,
        LF_FWNAV TYPE  BSET-FWSTE,
        LF_FWNVV TYPE  BSET-FWSTE,
        LF_FWSTE TYPE  BSET-FWSTE,
        LF_FWAST TYPE  BSET-FWSTE,
        LF_KAWRT TYPE  BSEG-FWBAS.

  IF US_ITEM-FWBAS IS NOT INITIAL .
    LF_KAWRT = US_ITEM-FWBAS.
  ELSE.
    LF_KAWRT = US_ITEM-WRBTR .
  ENDIF.

  CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT'
    EXPORTING
      I_BUKRS                 = UF_BUKRS
      I_MWSKZ                 = US_ITEM-MWSKZ
      I_WAERS                 = GF_DOC_CUR
      I_WRBTR                 = US_ITEM-WRBTR
    IMPORTING
      E_FWNAV                 = LF_FWNAV
      E_FWNVV                 = LF_FWNVV
      E_FWSTE                 = LF_FWSTE
      E_FWAST                 = LF_FWAST
    TABLES
      T_MWDAT                 = LT_MWDAT
    EXCEPTIONS
      BUKRS_NOT_FOUND         = 1
      COUNTRY_NOT_FOUND       = 2
      MWSKZ_NOT_DEFINED       = 3
      MWSKZ_NOT_VALID         = 4
      ACCOUNT_NOT_FOUND       = 5
      DIFFERENT_DISCOUNT_BASE = 6
      DIFFERENT_TAX_BASE      = 7
      TXJCD_NOT_VALID         = 8
      NOT_FOUND               = 9
      KTOSL_NOT_FOUND         = 10
      KALSM_NOT_FOUND         = 11
      PARAMETER_ERROR         = 12
      KNUMH_NOT_FOUND         = 13
      KSCHL_NOT_FOUND         = 14
      UNKNOWN_ERROR           = 15
      OTHERS                  = 16.
  ##NEEDED
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT LT_MWDAT ASSIGNING FIELD-SYMBOL(<LF_MWDAT>)." WHERE ktosl <> 'NVV'.
    GF_ITEMNO_ACC  = GF_ITEMNO_ACC + 1.

    CLEAR: GS_ACCOUNTTAX.
    GS_ACCOUNTTAX-ITEMNO_ACC = GF_ITEMNO_ACC.
    GS_ACCOUNTTAX-GL_ACCOUNT = <LF_MWDAT>-HKONT.
    GS_ACCOUNTTAX-COND_KEY   = <LF_MWDAT>-KSCHL.
    GS_ACCOUNTTAX-ACCT_KEY   = <LF_MWDAT>-KTOSL.
    GS_ACCOUNTTAX-TAX_CODE   = US_ITEM-MWSKZ.
    APPEND GS_ACCOUNTTAX TO GT_ACCOUNTTAX.

    CLEAR: GS_CURRENCYAMOUNT.
    GS_CURRENCYAMOUNT-ITEMNO_ACC  = GF_ITEMNO_ACC.
    GS_CURRENCYAMOUNT-CURR_TYPE   = '00'.        "Document Currency
    GS_CURRENCYAMOUNT-CURRENCY    = GF_DOC_CUR.
    IF GF_DOC_EXCH IS INITIAL.
      GS_CURRENCYAMOUNT-EXCH_RATE = 1.
    ELSE.
      GS_CURRENCYAMOUNT-EXCH_RATE = GF_DOC_EXCH.
    ENDIF.

    IF ( US_ITEM-LINETYPE = 'AP' OR
         US_ITEM-LINETYPE = 'TAX'  ) .
      IF US_ITEM-SHKZG = 'S'.
        GS_CURRENCYAMOUNT-AMT_DOCCUR  = US_ITEM-WMWST.
        GS_CURRENCYAMOUNT-TAX_AMT     = US_ITEM-WMWST.
        GS_CURRENCYAMOUNT-AMT_BASE    = LF_KAWRT.

      ELSEIF US_ITEM-SHKZG = 'H'.
        GS_CURRENCYAMOUNT-AMT_DOCCUR  = US_ITEM-WMWST * ( -1 ).
        GS_CURRENCYAMOUNT-TAX_AMT     = US_ITEM-WMWST * ( -1 ).
        GS_CURRENCYAMOUNT-AMT_BASE    = LF_KAWRT * ( -1 ).
      ENDIF.

      IF ( US_ITEM-LINETYPE = 'AP' AND UF_BLART = 'KG' ) .
        IF US_ITEM-SHKZG = 'S'.
          GS_CURRENCYAMOUNT-AMT_DOCCUR  = ABS( US_ITEM-WMWST ) * ( -1 ).
          GS_CURRENCYAMOUNT-TAX_AMT     = ABS( US_ITEM-WMWST ) * ( -1 ).
        ELSE.
          GS_CURRENCYAMOUNT-AMT_DOCCUR  = ABS( US_ITEM-WMWST ).
          GS_CURRENCYAMOUNT-TAX_AMT     = ABS( US_ITEM-WMWST ).
        ENDIF.
      ENDIF.


    ELSEIF ( US_ITEM-LINETYPE = 'AR'  OR
             US_ITEM-LINETYPE = 'GL'  ) .

      IF US_ITEM-SHKZG = 'H'.
        GS_CURRENCYAMOUNT-AMT_DOCCUR  = US_ITEM-WMWST.
        GS_CURRENCYAMOUNT-TAX_AMT     = US_ITEM-WMWST.
        GS_CURRENCYAMOUNT-AMT_BASE    = LF_KAWRT.
*          <fs_data>-amount_in_dc = <fs_data>-amount_in_dc - lw_mwdat-wmwst.
      ELSEIF US_ITEM-SHKZG = 'S'.
        GS_CURRENCYAMOUNT-AMT_DOCCUR  = US_ITEM-WMWST * ( -1 ).
        GS_CURRENCYAMOUNT-TAX_AMT     = US_ITEM-WMWST * ( -1 ).
        GS_CURRENCYAMOUNT-AMT_BASE    = LF_KAWRT * ( -1 ).
*          <fs_data>-amount_in_dc = <fs_data>-amount_in_dc + lw_mwdat-wmwst.
      ENDIF.
    ENDIF.

    APPEND GS_CURRENCYAMOUNT TO GT_CURRENCYAMOUNT.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_POPULATE_AA_LINE
*&---------------------------------------------------------------------*
*& Populate data for Line type Asset
*&---------------------------------------------------------------------*
FORM F_POPULATE_AA_LINE USING  US_HEAD  TYPE TS_HEAD
                               US_ITEM TYPE TS_ITEM_DATA .
  CONSTANTS LC_AA TYPE CHAR2 VALUE 'AA' .

  DATA:
    LF_TAX_AMT_LC TYPE BAPIACCR09-TAX_AMT,
    LF_BAS_AMT_LC TYPE BAPIACCR09-TAX_AMT,

    LS_ACCOUNTGL  TYPE BAPIACGL09.

  CLEAR: LF_TAX_AMT_LC, LF_BAS_AMT_LC.

* One time payee
  PERFORM F_SET_ONETIME_DATA  USING US_ITEM .


  CLEAR: LS_ACCOUNTGL.

  LS_ACCOUNTGL-ITEMNO_ACC = GF_ITEMNO_ACC.
  LS_ACCOUNTGL-GL_ACCOUNT = US_ITEM-NEWKO .
  LS_ACCOUNTGL-COSTCENTER = US_ITEM-KOSTL.
  LS_ACCOUNTGL-PROFIT_CTR = US_ITEM-PRCTR.

  LS_ACCOUNTGL-BUS_AREA       =  '0001'.
  LS_ACCOUNTGL-TAX_CODE       =  US_ITEM-MWSKZ .

*  LS_ACCOUNTGL-WBS_ELEMENT    =  US_ITEM-WBS_ELEMENT.

  LS_ACCOUNTGL-VALUE_DATE     =  US_ITEM-VALUT.
  LS_ACCOUNTGL-ALLOC_NMBR     =  US_ITEM-ZUONR.
  LS_ACCOUNTGL-ITEM_TEXT      =  US_ITEM-SGTXT.
  LS_ACCOUNTGL-REF_KEY_1      =  US_ITEM-XREF1.
  LS_ACCOUNTGL-REF_KEY_2      =  US_ITEM-XREF2.
  LS_ACCOUNTGL-REF_KEY_3      =  US_ITEM-XREF3.


  LS_ACCOUNTGL-ASSET_NO       =  US_ITEM-ANLN1.
  LS_ACCOUNTGL-SUB_NUMBER     =  US_ITEM-ANLN2.
  LS_ACCOUNTGL-REF_KEY_3      =  US_ITEM-BZDAT.
  LS_ACCOUNTGL-REF_KEY_3      =  US_ITEM-ANBWA.

  LS_ACCOUNTGL-COSTCENTER =  US_ITEM-KOSTL.
  LS_ACCOUNTGL-PROFIT_CTR =  US_ITEM-PRCTR.
**ls_accountgl-   US_ITEM--PROJK.
  LS_ACCOUNTGL-ORDERID    =  US_ITEM-AUFNR.
  LS_ACCOUNTGL-PARTNER_SEGMENT  = US_ITEM-PSEGMENT.

  APPEND LS_ACCOUNTGL TO GT_ACCOUNTGL.

* Transaction Currency
  IF US_ITEM-WRBTR IS NOT INITIAL.
    CLEAR: GS_CURRENCYAMOUNT.
    GS_CURRENCYAMOUNT-ITEMNO_ACC      = GF_ITEMNO_ACC .
    GS_CURRENCYAMOUNT-CURRENCY        = GF_DOC_CUR.
    GS_CURRENCYAMOUNT-CURR_TYPE       = '00'.
    IF GF_DOC_EXCH IS INITIAL.
      GS_CURRENCYAMOUNT-EXCH_RATE       = 1.
    ELSE.
      GS_CURRENCYAMOUNT-EXCH_RATE       = GF_DOC_EXCH.
    ENDIF.

    GS_CURRENCYAMOUNT-AMT_DOCCUR = US_ITEM-WRBTR .
    GS_CURRENCYAMOUNT-TAX_AMT    = US_ITEM-WMWST .
*    PERFORM f_POPULATE_AMOUNT USING US_ITEM-NEWBS "posting key
*                            CHANGING US_ITEM-WRBTR
*                                     GS_CURRENCYAMOUNT-AMT_DOCCUR.

*   Logic for tax type start with U, populate TAX_AMT
    IF US_ITEM-MWSKZ+0(1) EQ 'U'. "Tax code
      IF US_ITEM-FWBAS IS INITIAL .
        PERFORM F_POPULATE_AMOUNT USING  US_ITEM-NEWBS
                                         US_ITEM-FWBAS  "Tax base amount
                                CHANGING LF_BAS_AMT_LC.
        LF_TAX_AMT_LC =  GS_CURRENCYAMOUNT-AMT_DOCCUR - LF_BAS_AMT_LC.
        GS_CURRENCYAMOUNT-TAX_AMT = LF_TAX_AMT_LC.
      ENDIF.
    ENDIF.

    APPEND GS_CURRENCYAMOUNT TO GT_CURRENCYAMOUNT.
  ENDIF.


*--Asset Retriment

  GS_GEN_INFO-USERNAME   = SY-UNAME .
  GS_GEN_INFO-DOC_TYPE   = LC_AA .
  GS_GEN_INFO-DOC_DATE   = US_HEAD-BLDAT .
  GS_GEN_INFO-PSTNG_DATE = US_HEAD-BUDAT .

  GS_GEN_INFO-TRANS_DATE = US_HEAD-BLDAT.
  GS_GEN_INFO-COMP_CODE  = US_HEAD-BUKRS .
  GS_GEN_INFO-ASSETMAINO = US_ITEM-ANLN1 .
  GS_GEN_INFO-ASSETSUBNO = US_ITEM-ANLN2 .
  GS_GEN_INFO-ASSETTRTYP = '210' .


  GS_RETIREMENT-COMPL_RET  = GC_TRUE .
  GS_RETIREMENT-REV_ON_RET = ABS( US_ITEM-WRBTR ) .
  GS_RETIREMENT-CURRENCY   = US_HEAD-WAERS .
  GS_RETIREMENT-VALUEDATE  = US_HEAD-BUDAT .


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_criteria
*&---------------------------------------------------------------------*
*& Set criteriai for Profit segment
*&---------------------------------------------------------------------*

FORM F_SET_CRITERIA USING US_ITEM  TYPE TS_ITEM_DATA.

  PERFORM F_SET_CRITERIA_ITEM USING :
    GF_ITEMNO_ACC  'KNDNR'       US_ITEM-CE0_KNDNR ,
    GF_ITEMNO_ACC  'ARTNR'       US_ITEM-CE0_ARTNR ,
    GF_ITEMNO_ACC  'FKART'       US_ITEM-CE0_FKART ,
    GF_ITEMNO_ACC  'KAUFN'       US_ITEM-CE0_KAUFN ,
    GF_ITEMNO_ACC  'KDPOS'       US_ITEM-CE0_KDPOS ,
    GF_ITEMNO_ACC  'RKAUFNR'     US_ITEM-CE0_KDPOS ,
    GF_ITEMNO_ACC  'WERKS'       US_ITEM-CE0_WERKS ,
    GF_ITEMNO_ACC  'FKBER'       US_ITEM-CE0_FKBER ,
    GF_ITEMNO_ACC  'VKORG'       US_ITEM-CE0_VKORG ,
    GF_ITEMNO_ACC  'VTWEG'       US_ITEM-CE0_VTWEG ,
    GF_ITEMNO_ACC  'SPART'       US_ITEM-CE0_SPART ,
    GF_ITEMNO_ACC  'PSPNR'       US_ITEM-CE0_PSPNR ,
    GF_ITEMNO_ACC  'COPA_KOSTL'  US_ITEM-CE0_COPA_KOSTL  ,
    GF_ITEMNO_ACC  'KSTRG'       US_ITEM-CE0_KSTRG ,
    GF_ITEMNO_ACC  'PRCTR'       US_ITEM-CE0_PRCTR ,
    GF_ITEMNO_ACC  'PPRCTR'      US_ITEM-CE0_PPRCTR ,
    GF_ITEMNO_ACC  'SERVICE_DOC_TYPE'    US_ITEM-CE0_SERVICE_DOC_TYPE  ,
    GF_ITEMNO_ACC  'SERVICE_DOC_ID'      US_ITEM-CE0_SERVICE_DOC_ID  ,
    GF_ITEMNO_ACC  'SERVICE_DOC_ITEM_ID' US_ITEM-CE0_SERVICE_DOC_ITEM_ID  ,
    GF_ITEMNO_ACC  'MATKL'       US_ITEM-CE0_MATKL,
    GF_ITEMNO_ACC  'VKBUR'       US_ITEM-CE0_VKBUR,
    GF_ITEMNO_ACC  'VKGRP'       US_ITEM-CE0_VKGRP,
    GF_ITEMNO_ACC  'PRODH'       US_ITEM-CE0_PRODH,
    GF_ITEMNO_ACC  'KUNWE'       US_ITEM-CE0_KUNWE,
    GF_ITEMNO_ACC  'WWLO'        US_ITEM-CE0_WWLO,
    GF_ITEMNO_ACC  'LAND1'       US_ITEM-CE0_LAND1,
    GF_ITEMNO_ACC  'PAPH1'       US_ITEM-CE0_PAPH1,
    GF_ITEMNO_ACC  'PAPH2'       US_ITEM-CE0_PAPH2,
    GF_ITEMNO_ACC  'PAPH3'       US_ITEM-CE0_PAPH3,
    GF_ITEMNO_ACC  'REGIO'            US_ITEM-CE0_REGIO,
    GF_ITEMNO_ACC  'ZZ1_ACTTYPE_MSE'  US_ITEM-CE0_ZZ1_ACTTYPE_MSE  ,
    GF_ITEMNO_ACC  'ZZ1_APPLTN'       US_ITEM-CE0_ZZ1_APPLTN  ,
    GF_ITEMNO_ACC  'ZZ1_FISCYR_MSE'   US_ITEM-CE0_ZZ1_FISCYR_MSE  ,
    GF_ITEMNO_ACC  'ZZ1_ITMTYPE_MSE'  US_ITEM-CE0_ZZ1_ITMTYPE_MSE  ,
    GF_ITEMNO_ACC  'ZZ1_PROJTYPE_MSE' US_ITEM-CE0_ZZ1_PROJTYPE_MSE  ,
    GF_ITEMNO_ACC  'ZZ1_PROJ_MSE'     US_ITEM-CE0_ZZ1_PROJ_MSE  ,
    GF_ITEMNO_ACC  'ZZ1_SDSDIST_MSE'  US_ITEM-CE0_ZZ1_SDSDIST_MSE  ,
    GF_ITEMNO_ACC  'ZZ1_SERVTYPE_MSE' US_ITEM-CE0_ZZ1_SERVTYPE_MSE  ,
    GF_ITEMNO_ACC  'ZZ1_SHOPTYPE'     US_ITEM-CE0_ZZ1_SHOPTYPE,
    GF_ITEMNO_ACC  'ZZ1_SSERIES'      US_ITEM-CE0_ZZ1_SSERIES ,
    GF_ITEMNO_ACC  'ZZ1_ZZCAV_MSE'    US_ITEM-CE0_ZZ1_ZZCAV_MSE ,
    GF_ITEMNO_ACC  'ZZ1_ZZINNI_MSE'   US_ITEM-CE0_ZZ1_ZZINNI_MSE ,
    GF_ITEMNO_ACC  'ZZ1_ZZIUT_MSE'    US_ITEM-CE0_ZZ1_ZZIUT_MSE,
    GF_ITEMNO_ACC  'ZZ1_ZZPHA_MSE'    US_ITEM-CE0_ZZ1_ZZPHA_MSE,
    GF_ITEMNO_ACC  'ZZ1_ZZPMT_MSE'    US_ITEM-CE0_ZZ1_ZZPMT_MSE,
    GF_ITEMNO_ACC  'KMVTNR'           US_ITEM-CE0_KMVTNR ,
    GF_ITEMNO_ACC  'ZZ1_ZZREFTN_MSE'  US_ITEM-CE0_ZZ1_ZZREFTN_MSE,
    GF_ITEMNO_ACC  'ZZ1_MVGR1'        US_ITEM-CE0_ZZ1_MVGR1.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_criteria_item
*&---------------------------------------------------------------------*
*& Set detail of criteria
*&---------------------------------------------------------------------*
FORM F_SET_CRITERIA_ITEM  USING UF_ITEMNO    TYPE BAPIACKEC9-ITEMNO_ACC
                                UF_FIELDNAME TYPE BAPIACKEC9-FIELDNAME
                                UF_CHARACTER TYPE ANY.

  DATA: LS_CRITERIA  TYPE BAPIACKEC9,
        LF_CHARACTER TYPE BAPIACKEC9-CHARACTER.

  IF UF_CHARACTER IS NOT INITIAL.
    LF_CHARACTER = UF_CHARACTER .

    LS_CRITERIA-ITEMNO_ACC = UF_ITEMNO .
    LS_CRITERIA-FIELDNAME	 = UF_FIELDNAME.
    LS_CRITERIA-CHARACTER  = LF_CHARACTER.
    APPEND LS_CRITERIA TO GT_CRITERIA .
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_distchan
*----------------------------------------------------------------------*
*  Validate Distribution Channel
*----------------------------------------------------------------------*
FORM F_VALIDATE_DISTCHAN  USING  UF_STRING  TYPE STRING
                        CHANGING CF_VTWEG   TYPE TVTW-VTWEG
                                 CF_MSGTX   TYPE TS_MSG_TXT.

  TYPES: BEGIN OF LTS_TVTW,
           VTWEG TYPE  TVTW-VTWEG,
         END OF LTS_TVTW.
  TYPES: LTT_TVTW  TYPE  SORTED TABLE OF LTS_TVTW
                           WITH UNIQUE KEY VTWEG.

  STATICS:
    LS_TVTW TYPE  LTS_TVTW,
    LT_TVTW TYPE  LTT_TVTW.

  DATA:
    LF_VTWEG TYPE  LTS_TVTW-VTWEG.


* Initialize Output
  CLEAR: CF_VTWEG,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 2
  IF STRLEN( UF_STRING ) GT 2.
*   Text-e40 : Invalid Channel :
    CONCATENATE TEXT-E40 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LF_VTWEG = UF_STRING.

* Check Buffer
  IF LS_TVTW-VTWEG NE LF_VTWEG.
*   Validate with Memory
    READ TABLE LT_TVTW INTO LS_TVTW
                        WITH KEY VTWEG = LF_VTWEG
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TVTW.
*     Validate with Database
      SELECT SINGLE VTWEG
        INTO LS_TVTW
        FROM TVTW
       WHERE VTWEG EQ LF_VTWEG.
      IF SY-SUBRC NE 0.
*       Text-e40 : Invalid Channel :
        CONCATENATE TEXT-E40 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TVTW INTO TABLE LT_TVTW.
    ENDIF.

  ENDIF.

* Assign Output
  CF_VTWEG = LS_TVTW-VTWEG.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_BILLING_TYPE
*&---------------------------------------------------------------------*
FORM F_VALIDATE_BILLING_TYPE  USING UF_STRING  TYPE  STRING
                           CHANGING CF_FKART   TYPE  ANY
                                    CF_MSGTX   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_TVFK,
           FKART TYPE  TVFK-FKART,
         END OF LTS_TVFK.
  TYPES: LTT_TVFK  TYPE  SORTED TABLE OF LTS_TVFK
                         WITH UNIQUE KEY FKART.

  STATICS:
    LS_TVFK TYPE  LTS_TVFK,
    LT_TVFK TYPE  LTT_TVFK.

  DATA: LF_FKART TYPE  TVFK-FKART.
* Initialize Output
  CLEAR: CF_FKART,
         CF_MSGTX.

* Only when value exist
  CHECK UF_STRING IS NOT INITIAL.

  LF_FKART = UF_STRING.

  IF LS_TVFK-FKART NE LF_FKART.
*   Validate with Memory
    READ TABLE LT_TVFK INTO LS_TVFK
                        WITH KEY FKART = CF_FKART
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TVFK.
*     Validate with Database
      SELECT SINGLE FKART
        INTO LS_TVFK
        FROM TVFK
       WHERE FKART EQ LF_FKART.
      IF SY-SUBRC NE 0.
*       Text-e48 : Invalid Billing Type:
        CONCATENATE TEXT-E48 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TVFK INTO TABLE LT_TVFK.
    ENDIF.
  ENDIF.

* Assign Output
  CF_FKART = LS_TVFK-FKART.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_sales_order
*&---------------------------------------------------------------------*
FORM F_VALIDATE_SALES_ORDER  USING UF_STRING  TYPE  STRING
                          CHANGING CF_KAUFN   TYPE  ANY
                                   CF_MSGTX   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_VBAK,
           KAUFN TYPE  VBAK-VBELN,
         END OF LTS_VBAK.
  TYPES: LTT_VBAK  TYPE  SORTED TABLE OF LTS_VBAK
                           WITH UNIQUE KEY KAUFN.

  STATICS:
    LS_VBAK TYPE  LTS_VBAK,
    LT_VBAK TYPE  LTT_VBAK.

  DATA: LF_KAUFN TYPE  VBAK-VBELN.
* Initialize Output
  CLEAR: CF_KAUFN,
         CF_MSGTX.

* Only when value exist
  CHECK UF_STRING IS NOT INITIAL.

  LF_KAUFN = UF_STRING.

  IF LS_VBAK-KAUFN NE LF_KAUFN.
*   Validate with Memory
    READ TABLE LT_VBAK INTO LS_VBAK
                        WITH KEY KAUFN = LF_KAUFN
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_VBAK.
*     Validate with Database
      SELECT SINGLE VBELN
        INTO LS_VBAK
        FROM VBAK
       WHERE VBELN EQ LF_KAUFN.
      IF SY-SUBRC NE 0.
*       Text-e49 : Invalid Sales Order:
        CONCATENATE TEXT-E49 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_VBAK INTO TABLE LT_VBAK.
    ENDIF.
  ENDIF.

* Assign Output
  CF_KAUFN = LS_VBAK-KAUFN.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_assetno
*&---------------------------------------------------------------------*
*& Validate Asset No.
*&---------------------------------------------------------------------*
FORM F_VALIDATE_ASSETNO  USING UF_STRING  TYPE  STRING
                      CHANGING CF_ANLN1   TYPE  ANLH-ANLN1
                               CF_MSGTX   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_ANLH,
           ANLN1 TYPE  ANLH-ANLN1,
         END OF LTS_ANLH.
  TYPES: LTT_ANLH  TYPE  SORTED TABLE OF LTS_ANLH
                          WITH UNIQUE KEY ANLN1.

  STATICS:
    LT_ANLH TYPE  LTT_ANLH,
    LS_ANLH TYPE  LTS_ANLH.

  DATA:
    LF_ANLN1  TYPE  ANLH-ANLN1.


* Initialize Output
  CLEAR: CF_ANLN1,
         CF_MSGTX.

* Only when not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Convert to internal format
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = UF_STRING
    IMPORTING
      OUTPUT = LF_ANLN1.

* Check Buffer
  IF LS_ANLH-ANLN1 NE LF_ANLN1.
*   Validate with Memory
    READ TABLE LT_ANLH INTO LS_ANLH
                       WITH KEY ANLN1 = LF_ANLN1
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_ANLH.
*     Validate with Database
      SELECT ANLN1 UP TO 1 ROWS                      "#EC CI_SEL_NESTED
        INTO LS_ANLH
        FROM ANLH
       WHERE ANLN1  EQ  LF_ANLN1
        ORDER BY ANLN1 .
      ENDSELECT.
      IF SY-SUBRC NE 0.
*       Text-e01 : Invalid Asset no:
        CONCATENATE TEXT-E01 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_ANLH INTO TABLE LT_ANLH.
    ENDIF.

  ENDIF.

* Assign Output
  CF_ANLN1 = LF_ANLN1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_SALESOFF
*&---------------------------------------------------------------------*
*& Validate Sales Office
*&---------------------------------------------------------------------*
FORM F_VALIDATE_SALESOFF  USING  UF_STRING  TYPE  STRING
                        CHANGING CF_VKBUR   TYPE  TVBUR-VKBUR
                                 CF_MSGTX   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_TVBUR,
           VKBUR TYPE  TVBUR-VKBUR,
         END OF LTS_TVBUR.
  TYPES: LTT_TVBUR  TYPE  SORTED TABLE OF LTS_TVBUR
                           WITH UNIQUE KEY VKBUR.

  STATICS:
    LS_TVBUR TYPE  LTS_TVBUR,
    LT_TVBUR TYPE  LTT_TVBUR.

  DATA:
    LF_VKBUR TYPE  LTS_TVBUR-VKBUR.


* Initialize Output
  CLEAR: CF_VKBUR,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 4
  IF STRLEN( UF_STRING ) GT 4.
*   Text-e39 : Invalid Sales Office :
    CONCATENATE TEXT-E39 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LF_VKBUR = UF_STRING.

* Check Buffer
  IF LS_TVBUR-VKBUR NE LF_VKBUR.
*   Validate with Memory
    READ TABLE LT_TVBUR INTO LS_TVBUR
                        WITH KEY VKBUR = LF_VKBUR
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TVBUR.
*     Validate with Database
      SELECT SINGLE VKBUR
        INTO LS_TVBUR
        FROM TVBUR
       WHERE VKBUR EQ LF_VKBUR.
      IF SY-SUBRC NE 0.
*       Text-e39 : Invalid Sales Office :
        CONCATENATE TEXT-E39 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TVBUR INTO TABLE LT_TVBUR.
    ENDIF.

  ENDIF.

* Assign Output
  CF_VKBUR = LS_TVBUR-VKBUR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_DIVISION
*&---------------------------------------------------------------------*
*& Validate Division
*&---------------------------------------------------------------------*
FORM F_VALIDATE_DIVISION  USING UF_STRING  TYPE  STRING
                       CHANGING CF_SPART   TYPE  ANY
                                CF_MSGTX   TYPE  TS_MSG_TXT.


  TYPES: BEGIN OF LTS_TSPA,
           SPART TYPE  TSPA-SPART,
         END OF LTS_TSPA.
  TYPES: LTT_TSPA  TYPE  SORTED TABLE OF LTS_TSPA
                           WITH UNIQUE KEY SPART.

  STATICS:
    LS_TSPA TYPE  LTS_TSPA,
    LT_TSPA TYPE  LTT_TSPA.

  DATA: LF_SPART TYPE  TSPA-SPART.
* Initialize Output
  CLEAR: CF_SPART,
         CF_MSGTX.

* Only when value exist
  CHECK UF_STRING IS NOT INITIAL.

  LF_SPART = UF_STRING.

  IF LS_TSPA-SPART NE LF_SPART.
*   Validate with Memory
    READ TABLE LT_TSPA INTO LS_TSPA
                        WITH KEY SPART = LF_SPART
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TSPA.
*     Validate with Database
      SELECT SINGLE SPART
        INTO LS_TSPA
        FROM TSPA
       WHERE SPART EQ LF_SPART.
      IF SY-SUBRC NE 0.
*       Text-e45 : Invalid Division:
        CONCATENATE TEXT-E45 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TSPA INTO TABLE LT_TSPA.
    ENDIF.
  ENDIF.

* Assign Output
  CF_SPART = LS_TSPA-SPART.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_SALESORG
*&---------------------------------------------------------------------*
*& Validate SalesOrg
*&---------------------------------------------------------------------*

FORM F_VALIDATE_SALESORG  USING UF_STRING  TYPE  STRING
                       CHANGING CF_VKORG   TYPE  TVKO-VKORG
                                CF_MSGTX   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_TVKO,
           VKORG TYPE  TVKO-VKORG,
         END OF LTS_TVKO.
  TYPES: LTT_TVKO  TYPE  SORTED TABLE OF LTS_TVKO
                           WITH UNIQUE KEY VKORG.

  STATICS:
    LS_TVKO TYPE  LTS_TVKO,
    LT_TVKO TYPE  LTT_TVKO.

  DATA: LF_VKORG TYPE  TVKO-VKORG.
* Initialize Output
  CLEAR: CF_VKORG,
         CF_MSGTX.

* Only when value exist
  CHECK UF_STRING IS NOT INITIAL.

  LF_VKORG = UF_STRING.

  IF LS_TVKO-VKORG NE LF_VKORG.
*   Validate with Memory
    READ TABLE LT_TVKO INTO LS_TVKO
                        WITH KEY VKORG = LF_VKORG
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TVKO.
*     Validate with Database
      SELECT SINGLE VKORG
        INTO LS_TVKO
        FROM TVKO
       WHERE VKORG EQ LF_VKORG.
      IF SY-SUBRC NE 0.
*       Text-e44 : Invalid Sales Organization:
        CONCATENATE TEXT-E44 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TVKO INTO TABLE LT_TVKO.
    ENDIF.
  ENDIF.

* Assign Output
  CF_VKORG = LS_TVKO-VKORG.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_SALES_GROUP
*&---------------------------------------------------------------------*
*& Validate SalesGroup
*&---------------------------------------------------------------------*
FORM F_VALIDATE_SALES_GROUP  USING UF_STRING  TYPE  STRING
                          CHANGING CF_VKGRP   TYPE  ANY
                                   CF_MSGTX   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_TVKGR,
           VKGRP TYPE  TVKGR-VKGRP,
         END OF LTS_TVKGR.
  TYPES: LTT_TVKGR  TYPE  SORTED TABLE OF LTS_TVKGR
                           WITH UNIQUE KEY VKGRP.

  STATICS:
    LS_TVKGR TYPE  LTS_TVKGR,
    LT_TVKGR TYPE  LTT_TVKGR.

  DATA: LF_VKGRP TYPE  TVKGR-VKGRP.
* Initialize Output
  CLEAR: CF_VKGRP,
         CF_MSGTX.

* Only when value exist
  CHECK UF_STRING IS NOT INITIAL.

  LF_VKGRP = UF_STRING.

  IF LS_TVKGR-VKGRP NE LF_VKGRP.
*   Validate with Memory
    READ TABLE LT_TVKGR INTO LS_TVKGR
                        WITH KEY VKGRP = LF_VKGRP
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TVKGR.
*     Validate with Database
      SELECT SINGLE VKGRP
        INTO LS_TVKGR
        FROM TVKGR
       WHERE VKGRP EQ LF_VKGRP.
      IF SY-SUBRC NE 0.
*       Text-e47 : Invalid Sales Group:
        CONCATENATE TEXT-E47 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TVKGR INTO TABLE LT_TVKGR.
    ENDIF.
  ENDIF.

* Assign Output
  CF_VKGRP = LS_TVKGR-VKGRP.


ENDFORM.

*----------------------------------------------------------------------*
*  Form f_display_fidoc
*----------------------------------------------------------------------*
*  Display FI Document
*----------------------------------------------------------------------*
FORM F_DISPLAY_FIDOC  USING  UF_BUKRS  TYPE  BKPF-BUKRS
                             UF_BELNR  TYPE  BKPF-BELNR
                             UF_GJAHR  TYPE  BKPF-GJAHR.

  SET PARAMETER ID 'BLN' FIELD UF_BELNR.
  SET PARAMETER ID 'BUK' FIELD UF_BUKRS.
  SET PARAMETER ID 'GJR' FIELD UF_GJAHR.

* Call FB03
  PERFORM F_AUTHORIZE_CHECK USING GC_FB03.
  CALL TRANSACTION GC_FB03 WITHOUT AUTHORITY-CHECK
                           AND SKIP FIRST SCREEN.        "#EC CI_CALLTA

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_hotspot_click_1
*----------------------------------------------------------------------*
*  ALV Event on Hot spot click
*----------------------------------------------------------------------*
FORM F_HOTSPOT_CLICK_1 USING US_ROW_ID    TYPE LVC_S_ROW
                             US_COLUMN_ID TYPE LVC_S_COL ##CALLED.

  FIELD-SYMBOLS:
    <L_RESULT>  TYPE  TS_RESULT.


* Read Row
  READ TABLE GT_RESULT ASSIGNING <L_RESULT>
                       INDEX US_ROW_ID-INDEX.
  IF SY-SUBRC NE 0.
*   No row found
    RETURN.
  ENDIF.

  CASE US_COLUMN_ID-FIELDNAME.
    WHEN 'BELNR'.
      PERFORM F_DISPLAY_FIDOC  USING  <L_RESULT>-BUKRS
                                      <L_RESULT>-BELNR
                                      <L_RESULT>-GJAHR.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_EXCHANGE_RATE
*&---------------------------------------------------------------------*
FORM F_SET_EXCHANGE_RATE USING    UF_WAERS TYPE BKPF-WAERS
                                  UF_BUDAT TYPE BKPF-BUDAT
                         CHANGING CF_KURSF TYPE BKPF-KURSF.

  DATA LS_EXCH_RATE TYPE BAPI1093_0 .

  CALL FUNCTION 'BAPI_EXCHANGERATE_GETDETAIL'
    EXPORTING
      RATE_TYPE  = 'M'
      FROM_CURR  = UF_WAERS
      TO_CURRNCY = 'THB'
      DATE       = UF_BUDAT
    IMPORTING
      EXCH_RATE  = LS_EXCH_RATE.

  CF_KURSF = LS_EXCH_RATE-EXCH_RATE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALID_JV_K2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_DATA
*&---------------------------------------------------------------------*
FORM F_VALID_JV_K2  USING UT_DATA TYPE TT_DATA
                          UV_TEST TYPE CHAR01.

  DATA: LS_FIT060    TYPE ZSDSFIT060,
        LV_SUM       TYPE BSEG-WRBTR,
        LV_MSG       TYPE BAPIRET2-MESSAGE,
        LV_K2_AMOUNT TYPE ZSDSFIT060-SUM_AMT.

  CLEAR: LV_MSG, LS_FIT060.
  CLEAR: GS_FIT060.

  IF GR_BLART[] IS INITIAL.
    ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZSDSFIR0710'
                                                    IF_PARAM = 'JV_TYPE'
                                          IMPORTING ET_RANGE = GR_BLART ).
  ENDIF.

  SELECT SINGLE * FROM ZSDSFIT060 INTO @LS_FIT060 WHERE JV_DOCNO = @P_JVDOC AND ZDEL_FLG = @SPACE.
  IF SY-SUBRC = 0.
    IF LS_FIT060-POSTED = 'P'.
      "Allow to post
    ELSEIF LS_FIT060-POSTED = 'C'.
      LV_MSG = TEXT-E50.
      REPLACE ALL OCCURRENCES OF '&1' IN LV_MSG WITH P_JVDOC.
      MESSAGE LV_MSG TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

  DATA(LCL_JV_PROCESS) = NEW ZCL_SDSFI_JV_CHECK( ).

  GS_FIT060-JV_DOCNO    = P_JVDOC.
  IF UV_TEST IS NOT INITIAL.                                            "Test Call API
    LOOP AT UT_DATA INTO DATA(LS_DATA).
*      READ TABLE LS_DATA-ITEM INTO DATA(LS_ITEM) INDEX 1.
      LOOP AT LS_DATA-ITEM INTO DATA(LS_ITEM). "WHERE DATA-NEWBS = '40'.
        IF LS_ITEM-DATA-NEWBS = '40'.
          LV_SUM = LV_SUM + LS_ITEM-DATA-WRBTR.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    GS_FIT060-SUM_AMT = LV_SUM.
    GS_FIT060-POSTED  = 'C'.
  ELSE.                                                                 "Normal Call API
    "S = Success , P = Patial
    IF GS_SUM-TOTAL = GS_SUM-SUCCS.
      GS_FIT060-POSTED = 'C'.
    ELSEIF GS_SUM-ERROR < GS_SUM-TOTAL.
      GS_FIT060-POSTED = 'P'.
    ENDIF.
    "Calculate amount when post success
*    LOOP AT GT_RESULT INTO DATA(LS_RESULT).
*      IF LS_RESULT-MSGTY = 'S'.
*        GS_FIT060-SUM_AMT = LS_RESULT-WRBTR.
*      ENDIF.
*    ENDLOOP.
  ENDIF.

  "Get JV document was posted FI
  IF LS_FIT060 IS NOT INITIAL AND LS_FIT060-POSTED <> 'C'.
    SELECT
      BUKRS,
      BELNR,
      GJAHR,
      BLART
    FROM BKPF
    INTO TABLE @DATA(LT_BKPF)
    WHERE XBLNR = @P_JVDOC
      AND BLART IN @GR_BLART[]
      AND STGRD EQ @SPACE.
    IF SY-SUBRC = 0.
      SELECT RBUKRS,
             BELNR,
             GJAHR,
             BUZEI,
             BSCHL,
             TSL
        FROM ACDOCA
        INTO TABLE @DATA(LT_ACDOCA)
        FOR ALL ENTRIES IN @LT_BKPF
        WHERE ( RBUKRS  = @LT_BKPF-BUKRS AND
                BELNR   = @LT_BKPF-BELNR AND
                GJAHR   = @LT_BKPF-GJAHR )
          AND RLDNR = '0L'
          AND BSCHL = '40'
          AND BUZEI <> @SPACE.
    ENDIF.
    LOOP AT LT_ACDOCA INTO DATA(LS_ACDOCA).
      GS_FIT060-SUM_AMT = GS_FIT060-SUM_AMT + LS_ACDOCA-TSL.
    ENDLOOP.
  ENDIF.

  "Assign to Global vairable on SE24
  LCL_JV_PROCESS->GS_FIT060 = GS_FIT060.
  LCL_JV_PROCESS->GV_TEST   = UV_TEST.

  IF UV_TEST IS INITIAL.
    IF GS_FIT060-SUM_AMT < GV_K2_AMOUNT.
      GS_FIT060-POSTED = 'P'.
    ELSE.
      GS_FIT060-POSTED = 'C'.
    ENDIF.
  ENDIF.

  LCL_JV_PROCESS->START_PROCESS( ).

  IF LCL_JV_PROCESS->GS_K2_RES-MESSAGETYPE = 'E'.
    MESSAGE LCL_JV_PROCESS->GS_K2_RES-MESSAGETEXT TYPE 'I' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSE.
    IF UV_TEST IS INITIAL.
      IF LS_FIT060 IS INITIAL.
        LS_FIT060-SUM_AMT     = GS_FIT060-SUM_AMT. "Update Sum amount
        LS_FIT060-ZCRT_DATE   = SY-DATUM.
        LS_FIT060-ZCRT_TIME   = SY-UZEIT.
        LS_FIT060-ZCRT_PGM    = SY-CPROG.
        LS_FIT060-ZCRT_USER   = SY-UNAME.
      ELSE.
        LS_FIT060-SUM_AMT     = LS_FIT060-SUM_AMT + GS_FIT060-SUM_AMT. "Update Sum amount
        CLEAR LS_FIT060-ZDEL_FLG.
      ENDIF.

      LV_K2_AMOUNT = LCL_JV_PROCESS->GS_K2_RES-AMOUNT.
      IF LS_FIT060-SUM_AMT < LV_K2_AMOUNT.
        GS_FIT060-POSTED = 'P'.
      ELSE.
        GS_FIT060-POSTED = 'C'.
      ENDIF.

      "Update table log
      LS_FIT060-JV_DOCNO    = P_JVDOC.
      LS_FIT060-ZUPD_DATE   = SY-DATUM.
      LS_FIT060-ZUPD_TIME   = SY-UZEIT.
      LS_FIT060-ZUPD_USER   = SY-UNAME.
      LS_FIT060-POSTED      = GS_FIT060-POSTED.
      MODIFY ZSDSFIT060 FROM LS_FIT060.
    ELSE.
      "Test Run mode | Keep LV_K2_AMOUNT
      GV_K2_AMOUNT = LV_K2_AMOUNT.
    ENDIF.
  ENDIF.

ENDFORM.
