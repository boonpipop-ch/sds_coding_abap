*-----------------------------------------------------------------------
*  Program ID         : ZDSMMR0440
*  Creation Date      : 24.07.2024
*  Author             : Chanakarn T.(Eviden)
*  Add-on ID          : N/A
*  Description        : Program for download PO
*  Purpose            : Download outstanding PO
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
*& Report ZSDSMMR0440
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSMMR0440.

*-----------------------------------------------------------------------
* DEFINITION OF TABLE
*-----------------------------------------------------------------------
TABLES:
  EKKO,
  EKPO,
  LFA1,
  EKKN,
  EKET,
  KONV,
  ESSR,
  EKES.

*-----------------------------------------------------------------------
* DEFINITION OF TYPE
*-----------------------------------------------------------------------
TYPES: BEGIN OF TS_EKKO,
         BSART        TYPE EKKO-BSART,
         EBELN        TYPE EKKO-EBELN,
         LIFNR        TYPE EKKO-LIFNR,
         BEDAT        TYPE EKKO-BEDAT,
         EKORG        TYPE EKKO-EKORG,
         EKGRP        TYPE EKKO-EKGRP,
         BUKRS        TYPE EKKO-BUKRS,
         ZTERM        TYPE EKKO-ZTERM,
         WAERS        TYPE EKKO-WAERS,
         WKURS        TYPE EKKO-WKURS,
         KUFIX        TYPE EKKO-KUFIX,
         INCO1        TYPE EKKO-INCO1,
         INCO2        TYPE EKKO-INCO2,
         KNUMV        TYPE EKKO-KNUMV,
         IHREZ        TYPE EKKO-IHREZ,
         UNSEZ        TYPE EKKO-UNSEZ,
         VERKF        TYPE EKKO-VERKF,
         RETTP        TYPE EKKO-RETTP,
         RETPC        TYPE EKKO-RETPC,
         ZZ1_PRJ_C_PO TYPE EKKO-ZZ1_PRJ_C_PO,
         ZZ1_DLV_MODE TYPE EKKO-ZZ1_DLV_MODE,
         ZZ1_DIVCD_PO TYPE EKKO-ZZ1_DIVCD_PO,
         ZZ_JTEPA     TYPE EKKO-ZZ_JTEPA,
       END OF TS_EKKO.

TYPES: BEGIN OF TS_EKPO,
         EBELN           TYPE EKPO-EBELN,
         EBELP           TYPE EKPO-EBELP,
         KNTTP           TYPE EKPO-KNTTP,
         PSTYP           TYPE EKPO-PSTYP,
         MATNR           TYPE EKPO-MATNR,
         TXZ01           TYPE EKPO-TXZ01,
         MATKL           TYPE EKPO-MATKL,
         MENGE           TYPE EKPO-MENGE,
         MEINS           TYPE EKPO-MEINS,
         NETPR           TYPE EKPO-NETPR,
         BRTWR           TYPE EKPO-BRTWR,
         PEINH           TYPE EKPO-PEINH,
         BPRME           TYPE EKPO-BPRME,
         BPUMN           TYPE EKPO-BPUMN,
         BPUMZ           TYPE EKPO-BPUMZ,
         WERKS           TYPE EKPO-WERKS,
         LGORT           TYPE EKPO-LGORT,
         AFNAM           TYPE EKPO-AFNAM,
         BEDNR           TYPE EKPO-BEDNR,
         MWSKZ           TYPE EKPO-MWSKZ,
         UEBTO           TYPE EKPO-UEBTO,
         UNTTO           TYPE EKPO-UNTTO,
         UEBTK           TYPE EKPO-UEBTK,
         WEPOS           TYPE EKPO-WEPOS,
         REPOS           TYPE EKPO-REPOS,
         WEBRE           TYPE EKPO-WEBRE,
         XERSY           TYPE EKPO-XERSY,
         INSMK           TYPE EKPO-INSMK,
         MHDRZ           TYPE EKPO-MHDRZ,
         IPRKZ           TYPE EKPO-IPRKZ,
         SSQSS           TYPE EKPO-SSQSS,
         BANFN           TYPE EKPO-BANFN,
         BNFPO           TYPE EKPO-BNFPO,
         KONNR           TYPE EKPO-KONNR,
         KTPNR           TYPE EKPO-KTPNR,
         VRTKZ           TYPE EKPO-VRTKZ,
         TWRKZ           TYPE EKPO-TWRKZ,
         ELIKZ           TYPE EKPO-ELIKZ,
         BSTAE           TYPE EKPO-BSTAE,
         LABNR           TYPE EKPO-LABNR,
         KZABS           TYPE EKPO-KZABS,
         ABSKZ           TYPE EKPO-ABSKZ,
         ZZ1_LOB_PO_PDI  TYPE EKPO-ZZ1_LOB_PO_PDI,
         ZZ1_DES_PT_C_PO TYPE EKPO-ZZ1_DES_PT_C_PO,
       END OF TS_EKPO.

TYPES: BEGIN OF TS_EKPO_DEL,
         EBELN TYPE EKPO-EBELN,
         EBELP TYPE EKPO-EBELP,
       END OF TS_EKPO_DEL.

TYPES: BEGIN OF TS_EKKN,
         EBELN      TYPE EKKN-EBELN,
         EBELP      TYPE EKKN-EBELP,
         ZEKKN      TYPE EKKN-ZEKKN,
         MENGE      TYPE EKKN-MENGE,
         SAKTO      TYPE EKKN-SAKTO,
         KOSTL      TYPE EKKN-KOSTL,
         FISTL      TYPE EKKN-FISTL,
         ANLN1      TYPE EKKN-ANLN1,
         ANLN2      TYPE EKKN-ANLN2,
         FIPOS      TYPE EKKN-FIPOS,
         AUFNR      TYPE EKKN-AUFNR,
         PRCTR      TYPE EKKN-PRCTR,
         PS_PSP_PNR TYPE EKKN-PS_PSP_PNR,
         VBELN      TYPE EKKN-VBELN,
         VBELP      TYPE EKKN-VBELP,
       END OF TS_EKKN.

TYPES: BEGIN OF TS_LFA1,
         LIFNR TYPE LFA1-LIFNR,
         NAME1 TYPE LFA1-NAME1,
       END OF TS_LFA1.

TYPES: BEGIN OF TS_KONV,
         KNUMV TYPE KONV-KNUMV,
         KPOSN TYPE KONV-KPOSN,
         KSCHL TYPE KONV-KSCHL,
         KBETR TYPE KONV-KBETR,
       END OF TS_KONV.

TYPES: BEGIN OF TS_EKET,
         EBELN TYPE EKET-EBELN,
         EBELP TYPE EKET-EBELP,
         ETENR TYPE EKET-ETENR,
         EINDT TYPE EKET-EINDT,
         MENGE TYPE EKET-MENGE,
         WEMNG TYPE EKET-WEMNG,
       END OF TS_EKET.

TYPES: BEGIN OF TS_ESSR,
         LBLNI TYPE ESSR-LBLNI,
         EBELN TYPE ESSR-EBELN,
         EBELP TYPE ESSR-EBELP,
         LWERT TYPE ESSR-LWERT,
         WAERS TYPE ESSR-WAERS,
         KZABN TYPE ESSR-KZABN,
       END OF TS_ESSR.

TYPES: BEGIN OF TS_EKES,
         EBELN TYPE EKES-EBELN,
         EBELP TYPE EKES-EBELP,
         ETENS TYPE EKES-ETENS,
         EBTYP TYPE EKES-EBTYP,
         EINDT TYPE EKES-EINDT,
         MENGE TYPE EKES-MENGE,
         XBLNR TYPE EKES-XBLNR,
         ERDAT TYPE EKES-ERDAT,
       END OF TS_EKES.

TYPES: BEGIN OF TS_DATA,
*         Header
         BSART            TYPE STRING,  "Old PO Type
         NEW_BSART        TYPE STRING,  "New PO Type
         EBELN            TYPE STRING,  "PO No.
         LIFNR            TYPE STRING,  "Old Vendor
         NEW_LIFNR        TYPE STRING,  "New Vendor
         NAME1            TYPE STRING,  "Vendor Name
         BEDAT            TYPE STRING,  "Document Date
         EKORG            TYPE STRING,  "Pur.Org.
         EKGRP            TYPE STRING,  "Old Pur. Group
         NEW_EKGRP        TYPE STRING,  "New Pur. Group
         BUKRS            TYPE STRING,  "Company code
         ZTERM            TYPE STRING,  "Payment Term
         WAERS            TYPE STRING,  "Currency
         WKURS            TYPE STRING,  "Exchange Rate
         KUFIX            TYPE STRING,  "Fixed Exch. Rate
         INCO1            TYPE STRING,  "Old_Incoterm
         NEW_INCO1        TYPE STRING,  "New Incoterm
         INCO2            TYPE STRING,  "Inco. Location1
         NEW_INCO2        TYPE STRING,  "Inco. Location1
         IHREZ            TYPE STRING,  "Your Reference
         UNSEZ            TYPE STRING,  "Our Reference
         VERKF            TYPE STRING,  "Old Sales Person
         NEW_VERKF        TYPE STRING,  "New Sales Person
*         Header Condition
         HEADER_CON       TYPE STRING,  "ZVAT (%)
*         Payment Processing
         RETTP            TYPE STRING,  "Retention Indicator
         HEADER_RETPC     TYPE STRING,  "Retention in Percent
*         Header Text
         FROM_LINE1       TYPE STRING,  "From Line 1
         FROM_LINE2       TYPE STRING,  "From Line 2
         FROM_LINE3       TYPE STRING,  "From Line 3
         ZZREMARKS01      TYPE STRING,  "Remarks L1
         ZZREMARKS02      TYPE STRING,  "Remarks L2
         ZZREMARKS03      TYPE STRING,  "Remarks L3
         ZZREMARKS04      TYPE STRING,  "Remarks L4
         ZZREMARKS05      TYPE STRING,  "Remarks L5
*         Header Customer Data
         ZZ1_PRJ_C_PO     TYPE STRING,  "Project Type
         ZZ1_DLV_MODE     TYPE STRING,  "Old Delivery Mode
         NEW_ZZ1_DLV_MODE TYPE STRING,  "New Delivery Mode
         ZZADDRESS        TYPE STRING,  "IGOM Infomation
         ZZPRJTYP_DIL     TYPE STRING,  "Project Type for DIL
         ZZ1_DIVCD_PO     TYPE STRING,  "Division Code
         ZZ_JTEPA         TYPE STRING,  "JTEPA flag
*         Purchase Order Item
         EBELP            TYPE STRING,  "Old Item Number
         NEW_EBELP        TYPE STRING,  "New Item Number
         KNTTP            TYPE STRING,  "Old Acct. Assignment Cat.
         NEW_KNTTP        TYPE STRING,  "New Acct. Assignment Cat.
         PSTYP            TYPE STRING,  "Old Item Cat.
         NEW_PSTYP        TYPE STRING,  "New Item Cat.
         MATNR            TYPE STRING,  "Old Material No.
         NEW_MATNR        TYPE STRING,  "New Material No.
         TXZ01            TYPE STRING,  "Short Text
         MATKL            TYPE STRING,  "Old Material Group
         NEW_MATKL        TYPE STRING,  "New Material Group
         EKPO_MENGE       TYPE STRING,  "Open PO Quantity
         MEINS            TYPE STRING,  "Old Order Unit
         NEW_MEINS        TYPE STRING,  "New Order Unit
         EKET_EINDT       TYPE STRING,  "Delivery Date
         NETPR            TYPE STRING,  "Net Price
         BRTWR            TYPE STRING,  "Gross Price
         PEINH            TYPE STRING,  "Price Unit
         BPRME            TYPE STRING,  "Old Order Price Unit
         NEW_BPRME        TYPE STRING,  "New Order Price Unit
         BPUMN            TYPE STRING,  "Conv. From Qty Order Unit
         BPUMZ            TYPE STRING,  "Conv. To Order Price Unit
         WERKS            TYPE STRING,  "Old Plant
         NEW_WERKS        TYPE STRING,  "New Plant
         LGORT            TYPE STRING,  "Old Storage Location
         NEW_LGORT        TYPE STRING,  "New Storage Location
         AFNAM            TYPE STRING,  "Requisitioner
         BEDNR            TYPE STRING,  "Old Tracking Number
         NEW_BEDNR        TYPE STRING,  "New Tracking Number
         MWSKZ            TYPE STRING,  "Tax Code
         UEBTO            TYPE STRING,  "% Over Tolenrance
         UNTTO            TYPE STRING,  "% Under Tolenrance
         UEBTK            TYPE STRING,  "Unlimited
         WEPOS            TYPE STRING,  "Goods Receipt
         REPOS            TYPE STRING,  "Invoice Receipt
         WEBRE            TYPE STRING,  "Old GR-Based IV
         NEW_WEBRE        TYPE STRING,  "New GR-Based IV
         XERSY            TYPE STRING,  "ERS
         INSMK            TYPE STRING,  "Stock Type
         MHDRZ            TYPE STRING,  "Rem. Shelf Life
         IPRKZ            TYPE STRING,  "Period Ind.
         SSQSS            TYPE STRING,  "QA Control Key
         BANFN            TYPE STRING,  "PR
         BNFPO            TYPE STRING,  "PR Item
         KONNR            TYPE STRING,  "Outline Agreement
         KTPNR            TYPE STRING,  "Outline Agreement Item
         RETPC            TYPE STRING,  "Retention %
         DPAMT            TYPE STRING,  "Down Payment Amount
         DPPCT            TYPE STRING,  "Down Payment  %
         DPDAT            TYPE STRING,  "Down Payment Due Date
         BSTAE            TYPE STRING,  "Confirmation Control Key
         LABNR            TYPE STRING,  "Order Ack.
         KZABS            TYPE STRING,  "Order Ack. Require
         ABSKZ            TYPE STRING,  "Rejection Indicator
         ETENS            TYPE STRING,  "Seq no.
         EBTYP            TYPE STRING,  "Confirm Category
         EKES_EINDT       TYPE STRING,  "Delivery Date
         MENGE            TYPE STRING,  "Qty
         XBLNR            TYPE STRING,  "Reference
         ERDAT            TYPE STRING,  "Created On
*         Account Assignment Data
         VRTKZ            TYPE STRING,  "Distribution
         TWRKZ            TYPE STRING,  "Partial Inv.
         ZEKKN            TYPE STRING,  "Seq. No.
         EKKN_MENGE       TYPE STRING,  "Open Quantity
         SAKTO            TYPE STRING,  "Old G/L Acct.
         NEW_SAKTO        TYPE STRING,  "New G/L Acct.
         KOSTL            TYPE STRING,  "Old Cost Center
         NEW_KOSTL        TYPE STRING,  "New Cost Center
         FISTL            TYPE STRING,  "Old Fund Center
         NEW_FISTL        TYPE STRING,  "New Fund Center
         ANLN1            TYPE STRING,  "Old Asset No.
         NEW_ANLN1        TYPE STRING,  "New Asset No.
         ANLN2            TYPE STRING,  "Old Sub No.
         NEW_ANLN2        TYPE STRING,  "New Sub No.
         FIPOS            TYPE STRING,  "Old Commitment Item
         NEW_FIPOS        TYPE STRING,  "New Commitment Item
         AUFNR            TYPE STRING,  "Old Order No.
         NEW_AUFNR        TYPE STRING,  "New Order No.
         PRCTR            TYPE STRING,  "Old Profit Center
         NEW_PRCTR        TYPE STRING,  "New Profit Center
         PS_PSP_PNR       TYPE STRING,  "Old WBS
         NEW_PS_PSP_PNR   TYPE STRING,  "New WBS
         VBELN            TYPE STRING,  "Old Sales Order
         NEW_VBELN        TYPE STRING,  "New Sales Order
         VBELP            TYPE STRING,  "Old Sales Order Item
         NEW_VBELP        TYPE STRING,  "New Sales Order Item
*         Item Text
         INTERNAL_LINE1   TYPE STRING,  "Internal Order Line 1
         INTERNAL_LINE2   TYPE STRING,  "Internal Order Line 2
         INTERNAL_LINE3   TYPE STRING,  "Internal Order Line 3
         PROJECT_LINE1    TYPE STRING,  "Project Name Line 1
         PROJECT_LINE2    TYPE STRING,  "Project Name Line 2
         PROJECT_LINE3    TYPE STRING,  "Project Name Line 3
         ATTENTION_LINE1  TYPE STRING,  "Attention Line 1
         ATTENTION_LINE2  TYPE STRING,  "Attention Line 2
         ATTENTION_LINE3  TYPE STRING,  "Attention Line 3
         PERSON_LINE1     TYPE STRING,  "Person In Charge Line 1
         PERSON_LINE2     TYPE STRING,  "Person In Charge Line 2
         PERSON_LINE3     TYPE STRING,  "Person In Charge Line 3
         REMARK_LINE1     TYPE STRING,  "Remarks Line 1
         REMARK_LINE2     TYPE STRING,  "Remarks Line 2
         REMARK_LINE3     TYPE STRING,  "Remarks Line 3
         ITEM_TXT1        TYPE STRING,  "Item Text Line1
         ITEM_TXT2        TYPE STRING,  "Item Text Line2
         ITEM_TXT3        TYPE STRING,  "Item Text Line3
         MEMO_LINE1       TYPE STRING,                            "Memo Line1
         MEMO_LINE2       TYPE STRING,                            "Memo Line2
         MEMO_LINE3       TYPE STRING,                            "Memo Line3
         ADVANCE_LINE1    TYPE STRING,  "Advance Payment Line1
         ADVANCE_LINE2    TYPE STRING,  "Advance Payment Line2
         ADVANCE_LINE3    TYPE STRING,  "Advance Payment Line3
         RETENTION_LINE1  TYPE STRING,  "Retention Line1
         RETENTION_LINE2  TYPE STRING,  "Retention Line2
         RETENTION_LINE3  TYPE STRING,  "Retention Line3
         WARR_LINE1       TYPE STRING,  "warranty Line1
         WARR_LINE2       TYPE STRING,  "warranty Line2
         WARR_LINE3       TYPE STRING,  "warranty Line3
         PENA_LINE1       TYPE STRING,  "Penalty Line1
         PENA_LINE2       TYPE STRING,  "Penalty Line2
         PENA_LINE3       TYPE STRING,  "Penalty Line3
         EN_LINE1         TYPE STRING,  "Engineer Line1
         EN_LINE2         TYPE STRING,  "Engineer Line2
         EN_LINE3         TYPE STRING,  "Engineer Line3
*         Item Customer Data
         ZZ1_LOB_PO_PDI   TYPE STRING, "LOB
         ZZ1_DES_PT_C_PO  TYPE STRING, "Dest. Port Code
         ZZ_ETD           TYPE STRING, "ETD
       END OF TS_DATA.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
* Text ID
  GC_OBJEC_HEADERTXT TYPE  TDOBJECT VALUE 'EKKO',
  GC_OBJEC_ITEMTXT   TYPE  TDOBJECT VALUE 'EKPO'.

*-----------------------------------------------------------------------
* DEFINITION OF WORK AREA
*-----------------------------------------------------------------------
DATA:
  GS_HEADER TYPE TS_DATA,
  GS_DATA   TYPE TS_DATA.

DATA:
  GS_EKKO     TYPE TS_EKKO,
  GS_EKPO     TYPE TS_EKPO,
  GS_EKPO_DEL TYPE TS_EKPO_DEL,
  GS_LFA1     TYPE TS_LFA1,
  GS_EKKN     TYPE TS_EKKN,
  GS_EKET     TYPE TS_EKET,
  GS_KONV     TYPE TS_KONV,
  GS_ESSR     TYPE TS_ESSR,
  GS_EKES     TYPE TS_EKES.

*-----------------------------------------------------------------------
* INTERNAL TABLE DEFINITION
*-----------------------------------------------------------------------
DATA:
  GT_EKKO     TYPE TABLE OF TS_EKKO,
  GT_EKPO     TYPE TABLE OF TS_EKPO,
  GT_EKPO_DEL TYPE TABLE OF TS_EKPO_DEL,
  GT_EKKN     TYPE TABLE OF TS_EKKN,
  GT_EKET     TYPE TABLE OF TS_EKET,
  GT_LFA1     TYPE TABLE OF TS_LFA1,
  GT_KONV     TYPE TABLE OF TS_KONV,
  GT_ESSR     TYPE TABLE OF TS_ESSR,
  GT_EKES     TYPE TABLE OF TS_EKES.

DATA:
  GT_DATA    TYPE TABLE OF TS_DATA.

*-----------------------------------------------------------------------
* Variable
*-----------------------------------------------------------------------
DATA: GF_FILE TYPE STRING.

*-----------------------------------------------------------------------
* DEFINITION OF SELECTION SCREEN
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-B01.
  SELECT-OPTIONS:
        S_EBELN FOR EKKO-EBELN,      " PO No
        S_BEDAT FOR EKKO-BEDAT,      " PO Date
        S_BUKRS FOR EKKO-BUKRS,      " Company code
        S_BSART FOR EKKO-BSART,      " PO Doc. Type
        S_WERKS FOR EKPO-WERKS,      " Plant
        S_LGORT FOR EKPO-LGORT,      " SLoc
        S_EKGRP FOR EKKO-EKGRP,      " Purchase Group
        S_PSTYP FOR EKPO-PSTYP,      " Item Catagory
        S_KNTTP FOR EKPO-KNTTP,      " Account Assignment Cat.
        S_LOEKZ FOR EKPO-LOEKZ ,     " Delete Indicator
        S_ELIKZ FOR EKPO-ELIKZ.      " Delivery Complete
  SELECTION-SCREEN SKIP.
  PARAMETERS: P_MAXREC TYPE I DEFAULT 5000. "Split Line Per File
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME.
  PARAMETERS:
        P_FILE LIKE RLGRAP-FILENAME OBLIGATORY DEFAULT 'C:\Temp\'.  "Local file path for upload
SELECTION-SCREEN END OF BLOCK B2.

************************************************************************
INITIALIZATION.
************************************************************************
* Initial Data
  IF S_LOEKZ IS INITIAL.
    S_LOEKZ-SIGN = 'I'.
    S_LOEKZ-OPTION = 'EQ'.
    APPEND S_LOEKZ TO S_LOEKZ.
  ENDIF.

  IF S_ELIKZ IS INITIAL.
    S_ELIKZ-SIGN = 'I'.
    S_ELIKZ-OPTION = 'EQ'.
    APPEND S_ELIKZ TO S_ELIKZ.
  ENDIF.
  PERFORM F_INIT_DATA.

************************************************************************
AT SELECTION-SCREEN OUTPUT.
*************************************************************************
  GF_FILE = P_FILE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM F_GET_FOLDER_NAME.

************************************************************************
START-OF-SELECTION.
************************************************************************
* Main Process
  PERFORM F_GET_DATA.
  PERFORM F_GET_HEADER.

************************************************************************
END-OF-SELECTION.
************************************************************************
  PERFORM F_PREPARE_DATA.
  PERFORM F_DOWNLOAD_LOCAL.

*&---------------------------------------------------------------------*
*&      Form  F_INIT_DATA
*&---------------------------------------------------------------------*
FORM F_INIT_DATA.
  GF_FILE = P_FILE.
ENDFORM.                    " F_INIT_DATA
*&---------------------------------------------------------------------*
*&      Form  f_get_floder_name
*&---------------------------------------------------------------------*
FORM F_GET_FOLDER_NAME .

  DATA: LF_FOLDER TYPE STRING,
        LF_INIT   TYPE STRING.

  LF_INIT = GF_FILE.

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
    P_FILE = LF_FOLDER.
    GF_FILE = P_FILE.
  ENDIF.

ENDFORM.                    " F_GET_FOLDER_NAME
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
FORM F_GET_DATA .
  PERFORM F_GET_EKKO.
  PERFORM F_GET_EKPO.
  PERFORM F_GET_LFA1.
  PERFORM F_GET_EKKN.
  PERFORM F_GET_EKET.
  PERFORM F_GET_KONV.
  PERFORM F_GET_EKES.

ENDFORM.                    " F_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GET_HEADER
*&---------------------------------------------------------------------*
FORM F_GET_HEADER.

  GS_HEADER-BSART	            = 'Old PO Type'.
  GS_HEADER-NEW_BSART	        =	'New PO Type'.
  GS_HEADER-EBELN	            =	'PO No.'.
  GS_HEADER-LIFNR	            = 'Old Vendor'.
  GS_HEADER-NEW_LIFNR	        =	'New Vendor'.
  GS_HEADER-NAME1	            =	'Vendor Name'.
  GS_HEADER-BEDAT	            =	'Document Date'.
  GS_HEADER-EKORG	            =	'Pur.Org.'.
  GS_HEADER-EKGRP             = 'Old Pur. Group'.
  GS_HEADER-NEW_EKGRP	        =	'New Pur. Group'.
  GS_HEADER-BUKRS	            =	'Company code'.
  GS_HEADER-ZTERM	            =	'Payment Term'.
  GS_HEADER-WAERS	            =	'Currency'.
  GS_HEADER-WKURS	            =	'Exchange Rate'.
  GS_HEADER-KUFIX	            =	'Fixed Exch. Rate'.
  GS_HEADER-INCO1	            =	'Old Incoterm'.
  GS_HEADER-NEW_INCO1	        =	'New Incoterm'.
  GS_HEADER-INCO2	            =	'Old Inco. Location1'.
  GS_HEADER-NEW_INCO2	        =	'New Inco. Location1'.
  GS_HEADER-IHREZ	            =	'Your Reference'.
  GS_HEADER-UNSEZ	            =	'Our Reference'.
  GS_HEADER-VERKF             = 'Old Sales Person'.
  GS_HEADER-NEW_VERKF         = 'New Sales Person'.
  GS_HEADER-HEADER_CON        = 'ZVAT (%)'.
  GS_HEADER-RETTP             = 'Retention Indicator'.
  GS_HEADER-HEADER_RETPC      = 'Retention in Percent'.
  GS_HEADER-FROM_LINE1        = 'From Line 1'.
  GS_HEADER-FROM_LINE2        = 'From Line 2'.
  GS_HEADER-FROM_LINE3        = 'From Line 3'.
  GS_HEADER-ZZREMARKS01	      =	'Remarks L1'.
  GS_HEADER-ZZREMARKS02       = 'Remarks L2'.
  GS_HEADER-ZZREMARKS03	      =	'Remarks L3'.
  GS_HEADER-ZZREMARKS04	      =	'Remarks L4'.
  GS_HEADER-ZZREMARKS05	      =	'Remarks L5'.
  GS_HEADER-ZZ1_PRJ_C_PO      = 'Project Code'.
  GS_HEADER-ZZ1_DLV_MODE      =	'Old Delivery Mode'.
  GS_HEADER-NEW_ZZ1_DLV_MODE  = 'New Delivery Mode'.
  GS_HEADER-ZZADDRESS	        =	'IGOM Infomation'.
  GS_HEADER-ZZPRJTYP_DIL      = 'Project Type for DIL'.
  GS_HEADER-ZZ1_DIVCD_PO      = 'Division Code'.
  GS_HEADER-ZZ_JTEPA          = 'JTEPA flag'.
  GS_HEADER-EBELP	            =	'Old Item Number'.
  GS_HEADER-NEW_EBELP	        =	'New Item Number'.
  GS_HEADER-KNTTP	            =	'Old Acct. Assignment Cat.'.
  GS_HEADER-NEW_KNTTP	        =	'New Acct. Assignment Cat.'.
  GS_HEADER-PSTYP	            =	'Old Item Cat.'.
  GS_HEADER-NEW_PSTYP	        =	'New Item Cat.'.
  GS_HEADER-MATNR	            =	'Old Material No.'.
  GS_HEADER-NEW_MATNR	        =	'New Material No.'.
  GS_HEADER-TXZ01	            =	'Short Text'.
  GS_HEADER-MATKL	            =	'Old Material Group'.
  GS_HEADER-NEW_MATKL	        =	'New Material Group'.
  GS_HEADER-EKPO_MENGE        = 'Open PO Quantity'.
  GS_HEADER-MEINS	            =	'Old Order Unit'.
  GS_HEADER-NEW_MEINS	        =	'New Order Unit'.
  GS_HEADER-EKET_EINDT        = 'Delivery Date'.
  GS_HEADER-NETPR	            =	'Net Price'.
  GS_HEADER-BRTWR	            =	'Gross Price (Condition Type : PBXX, PB00)'.
  GS_HEADER-PEINH	            =	'Price Unit'.
  GS_HEADER-BPRME             =	'Old Order Price Unit'.
  GS_HEADER-NEW_BPRME	        =	'New Order Price Unit'.
  GS_HEADER-BPUMN	            =	'Conv. From Qty Order Unit'.
  GS_HEADER-BPUMZ	            =	'Conv. To Order Price Unit'.
  GS_HEADER-WERKS             =	'Old Plant'.
  GS_HEADER-NEW_WERKS	        =	'New Plant'.
  GS_HEADER-LGORT	            =	'Old Storage Location'.
  GS_HEADER-NEW_LGORT	        =	'New Storage Location'.
  GS_HEADER-AFNAM	            =	'Requisitioner'.
  GS_HEADER-BEDNR	            =	'Old Tracking Number'.
  GS_HEADER-NEW_BEDNR	        =	'New Tracking Number'.
  GS_HEADER-MWSKZ	            =	'Tax Code'.
  GS_HEADER-UEBTO	            =	'% Over Tolenrance'.
  GS_HEADER-UNTTO	            =	'% Under Tolenrance'.
  GS_HEADER-UEBTK	            =	'Unlimited'.
  GS_HEADER-WEPOS	            =	'Goods Receipt'.
  GS_HEADER-REPOS	            =	'Invoice Receipt'.
  GS_HEADER-WEBRE	            =	'Old GR-Based IV'.
  GS_HEADER-NEW_WEBRE         =	'New GR-Based IV'.
  GS_HEADER-XERSY	            =	'ERS'.
  GS_HEADER-INSMK             = 'Stock Type'.
  GS_HEADER-MHDRZ	            =	'Rem. Shelf Life'.
  GS_HEADER-IPRKZ	            =	'Period Ind.'.
  GS_HEADER-SSQSS	            =	'QA Control Key'.
  GS_HEADER-BANFN	            =	'PR'.
  GS_HEADER-BNFPO	            =	'PR Item'.
  GS_HEADER-KONNR	            =	'Outline Agreement'.
  GS_HEADER-KTPNR	            =	'Outline Agreement Item'.
  GS_HEADER-RETPC	            =	'Retention %'.
  GS_HEADER-DPAMT	            =	'Down Payment Amount'.
  GS_HEADER-DPPCT	            =	'Down Payment  %'.
  GS_HEADER-DPDAT	            =	'Down Payment Due Date'.
  GS_HEADER-BSTAE	            =	'Confirmation Control Key'.
  GS_HEADER-LABNR	            =	'Order Ack.'.
  GS_HEADER-KZABS	            =	'Order Ack. Require'.
  GS_HEADER-ABSKZ	            =	'Rejection Indicator'.
  GS_HEADER-ETENS	            =	'Seq no.'.
  GS_HEADER-EBTYP	            =	'Confirm Category'.
  GS_HEADER-EKES_EINDT        = 'Delivery Date'.
  GS_HEADER-MENGE	            =	'Qty'.
  GS_HEADER-XBLNR	            =	'Reference'.
  GS_HEADER-ERDAT	            =	'Created On'.
  GS_HEADER-VRTKZ             = 'Distribution'.
  GS_HEADER-TWRKZ	            =	'Partial Inv.'.
  GS_HEADER-ZEKKN	            =	'Seq. No.'.
  GS_HEADER-EKKN_MENGE        = 'Open Quantity'.
  GS_HEADER-SAKTO	            =	'Old G/L Acct.'.
  GS_HEADER-NEW_SAKTO	        =	'New G/L Acct.'.
  GS_HEADER-KOSTL	            =	'Old Cost Center'.
  GS_HEADER-NEW_KOSTL	        =	'New Cost Center'.
  GS_HEADER-FISTL	            =	'Old Fund Center'.
  GS_HEADER-NEW_FISTL         = 'New Fund Center'.
  GS_HEADER-ANLN1	            =	'Old Asset No.'.
  GS_HEADER-NEW_ANLN1	        =	'New Asset No.'.
  GS_HEADER-ANLN2	            =	'Old Sub No.'.
  GS_HEADER-NEW_ANLN2	        =	'New Sub No.'.
  GS_HEADER-FIPOS	            =	'Old Commitment Item'.
  GS_HEADER-NEW_FIPOS         =	'New Commitment Item'.
  GS_HEADER-AUFNR	            =	'Old Order No. '.
  GS_HEADER-NEW_AUFNR	        =	'New Order No. '.
  GS_HEADER-PRCTR	            =	'Old Profit Center'.
  GS_HEADER-NEW_PRCTR	        =	'New Profit Center'.
  GS_HEADER-PS_PSP_PNR        = 'Old WBS'.
  GS_HEADER-NEW_PS_PSP_PNR    = 'New WBS'.
  GS_HEADER-VBELN             = 'Old Sales Order'.
  GS_HEADER-NEW_VBELN         = 'New Sales Order'.
  GS_HEADER-VBELP             = 'Old Sales Order Item'.
  GS_HEADER-NEW_VBELP         = 'New Sales Order Item'.
  GS_HEADER-INTERNAL_LINE1    = 'Internal Order Line 1'.
  GS_HEADER-INTERNAL_LINE2    = 'Internal Order Line 2'.
  GS_HEADER-INTERNAL_LINE3    = 'Internal Order Line 3'.
  GS_HEADER-PROJECT_LINE1	    =	'Project Name Line 1'.
  GS_HEADER-PROJECT_LINE2	    =	'Project Name Line 2'.
  GS_HEADER-PROJECT_LINE3     = 'Project Name Line 3'.
  GS_HEADER-ATTENTION_LINE1	  =	'Attention Line 1'.
  GS_HEADER-ATTENTION_LINE2	  =	'Attention Line 2'.
  GS_HEADER-ATTENTION_LINE3	  =	'Attention Line 3'.
  GS_HEADER-PERSON_LINE1      = 'Person In Charge Line1'.
  GS_HEADER-PERSON_LINE2      = 'Person In Charge Line2'.
  GS_HEADER-PERSON_LINE3      = 'Person In Charge Line3'.
  GS_HEADER-REMARK_LINE1      = 'Remarks Line1'.
  GS_HEADER-REMARK_LINE2      = 'Remarks Line2'.
  GS_HEADER-REMARK_LINE3      = 'Remarks Line3'.
  GS_HEADER-ITEM_TXT1	        =	'Item text Line1'.
  GS_HEADER-ITEM_TXT2	        =	'Item text Line2'.
  GS_HEADER-ITEM_TXT3	        =	'Item text Line3'.
  GS_HEADER-MEMO_LINE1        = 'Memo Line1'.
  GS_HEADER-MEMO_LINE2        = 'Memo Line2'.
  GS_HEADER-MEMO_LINE3        = 'Memo Line3'.
  GS_HEADER-ADVANCE_LINE1	    =	'Advance Payment Line1'.
  GS_HEADER-ADVANCE_LINE2	    =	'Advance Payment Line2'.
  GS_HEADER-ADVANCE_LINE3	    =	'Advance Payment Line3'.
  GS_HEADER-RETENTION_LINE1	  =	'Retention Line1'.
  GS_HEADER-RETENTION_LINE2	  =	'Retention Line2'.
  GS_HEADER-RETENTION_LINE3	  =	'Retention Line3'.
  GS_HEADER-WARR_LINE1        = 'Warranty Line1'.
  GS_HEADER-WARR_LINE2        = 'Warranty Line2'.
  GS_HEADER-WARR_LINE3        = 'Warranty Line3'.
  GS_HEADER-PENA_LINE1        = 'Penalty Line1'.
  GS_HEADER-PENA_LINE2        = 'Penalty Line2'.
  GS_HEADER-PENA_LINE3        = 'Penalty Line3'.
  GS_HEADER-EN_LINE1          = 'Engineer Line1'.
  GS_HEADER-EN_LINE2          = 'Engineer Line2'.
  GS_HEADER-EN_LINE3          = 'Engineer Line3'.
  GS_HEADER-ZZ1_LOB_PO_PDI    = 'LOB'.
  GS_HEADER-ZZ1_DES_PT_C_PO   = 'Dest. Port Code'.
  GS_HEADER-ZZ_ETD            = 'ETD'.

ENDFORM. " F_GET_HEADER
*&---------------------------------------------------------------------*
*&      Form  F_GET_EKKO
*&---------------------------------------------------------------------*
FORM F_GET_EKKO.
  SELECT  BSART
          EBELN
          LIFNR
          BEDAT
          EKORG
          EKGRP
          BUKRS
          ZTERM
          WAERS
          WKURS
          KUFIX
          INCO1
          INCO2
          KNUMV
          IHREZ
          UNSEZ
          VERKF
          RETTP
          RETPC
          ZZ1_PRJ_C_PO
          ZZ1_DLV_MODE
          ZZ1_DIVCD_PO
          ZZ_JTEPA
  FROM EKKO
  INTO TABLE GT_EKKO
  WHERE EBELN IN S_EBELN
  AND BEDAT IN S_BEDAT
  AND BUKRS IN S_BUKRS
  AND BSART IN S_BSART
  AND EKGRP IN S_EKGRP
  AND LOEKZ IN S_LOEKZ.

  SORT GT_EKKO BY EBELN.

ENDFORM. " F_GET_EKKO
*&---------------------------------------------------------------------*
*&      Form  F_GET_EKPO
*&---------------------------------------------------------------------*
FORM F_GET_EKPO .

  CHECK GT_EKKO IS NOT INITIAL.

  SELECT  EBELN
          EBELP
          KNTTP
          PSTYP
          MATNR
          TXZ01
          MATKL
          MENGE
          MEINS
          NETPR
          BRTWR
          PEINH
          BPRME
          BPUMN
          BPUMZ
          WERKS
          LGORT
          AFNAM
          BEDNR
          MWSKZ
          UEBTO
          UNTTO
          UEBTK
          WEPOS
          REPOS
          WEBRE
          XERSY
          INSMK
          MHDRZ
          IPRKZ
          SSQSS
          BANFN
          BNFPO
          KONNR
          KTPNR
          VRTKZ
          TWRKZ
          ELIKZ
          BSTAE
          LABNR
          KZABS
          ABSKZ
          ZZ1_LOB_PO_PDI
          ZZ1_DES_PT_C_PO
  FROM EKPO
  INTO TABLE GT_EKPO
  FOR ALL ENTRIES IN GT_EKKO
  WHERE EBELN EQ GT_EKKO-EBELN
  AND WERKS IN S_WERKS
  AND LGORT IN S_LGORT
  AND KNTTP IN S_KNTTP
  AND LOEKZ IN S_LOEKZ
  AND PSTYP IN S_PSTYP
  AND ELIKZ IN S_ELIKZ.

  SORT GT_EKPO BY EBELN EBELP.
ENDFORM. " F_GET_EKPO
*&---------------------------------------------------------------------*
*&      Form  F_GET_LFA1
*&---------------------------------------------------------------------*
FORM F_GET_LFA1 .

  CHECK GT_EKKO IS NOT INITIAL .
  SELECT LIFNR
         NAME1
  FROM LFA1
  INTO TABLE GT_LFA1
  FOR ALL ENTRIES IN GT_EKKO
  WHERE LIFNR EQ GT_EKKO-LIFNR.

  SORT GT_LFA1 BY LIFNR.

ENDFORM.  " F_GET_LFA1
*&---------------------------------------------------------------------*
*&      Form  F_GET_EKKN
*&---------------------------------------------------------------------*
FORM F_GET_EKKN .

  CHECK GT_EKPO IS NOT INITIAL .
  SELECT  EBELN
          EBELP
          ZEKKN
          MENGE
          SAKTO
          KOSTL
          FISTL
          ANLN1
          ANLN2
          FIPOS
          AUFNR
          PRCTR
          PS_PSP_PNR
          VBELN
          VBELP
  FROM EKKN
  INTO TABLE GT_EKKN
  FOR ALL ENTRIES IN GT_EKPO
  WHERE EBELN EQ GT_EKPO-EBELN
  AND EBELP EQ GT_EKPO-EBELP
  AND LOEKZ EQ SPACE.

  SORT GT_EKKN BY EBELN EBELP.

ENDFORM. " F_GET_EKKN
*&---------------------------------------------------------------------*
*&      Form  F_GET_EKET
*&---------------------------------------------------------------------*
FORM F_GET_EKET.

  CHECK GT_EKPO IS NOT INITIAL.
  SELECT  EBELN
          EBELP
          ETENR
          EINDT
          MENGE
          WEMNG
  INTO TABLE GT_EKET
    FROM EKET
    FOR ALL ENTRIES IN GT_EKPO
    WHERE EBELN EQ GT_EKPO-EBELN
      AND EBELP EQ GT_EKPO-EBELP.

  SORT GT_EKET BY EBELN EBELP.

ENDFORM. " F_GET_EKET
*&---------------------------------------------------------------------*
*&      Form  F_GET_KONV
*&---------------------------------------------------------------------*
FORM F_GET_KONV .

  CHECK GT_EKPO IS NOT INITIAL.
  SELECT  KNUMV
          KPOSN
          KSCHL
          KBETR
     INTO TABLE GT_KONV
     FROM KONV
    FOR ALL ENTRIES IN GT_EKKO
    WHERE KNUMV EQ GT_EKKO-KNUMV
    AND KSCHL EQ 'ZVAT'.

  SORT GT_KONV BY KNUMV.

ENDFORM. " F_GET_KONV
*&---------------------------------------------------------------------*
*&      Form  F_GET_EKES
*&---------------------------------------------------------------------*
FORM F_GET_EKES .

  CHECK GT_EKPO IS NOT INITIAL.
  SELECT  EBELN
          EBELP
          ETENS
          EBTYP
          EINDT
          MENGE
          XBLNR
          ERDAT
    INTO TABLE GT_EKES
    FROM EKES
    FOR ALL ENTRIES IN GT_EKPO
    WHERE EBELN EQ GT_EKPO-EBELN
    AND EBELP EQ GT_EKPO-EBELP
    AND LOEKZ IN S_LOEKZ.

  SORT GT_ESSR BY EBELN EBELP.

ENDFORM. " F_GET_EKES
*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_DATA
*&---------------------------------------------------------------------*
FORM F_PREPARE_DATA .

  DATA: LF_TEXT TYPE THEAD-TDNAME.

  LOOP AT GT_EKPO INTO GS_EKPO.
    CLEAR: LF_TEXT, GS_DATA.

    MOVE-CORRESPONDING GS_EKPO TO GS_DATA.
    GS_DATA-EKPO_MENGE = GS_EKPO-MENGE.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        INPUT          = GS_EKPO-MEINS
        LANGUAGE       = SY-LANGU
      IMPORTING
        OUTPUT         = GS_DATA-MEINS
      EXCEPTIONS
        UNIT_NOT_FOUND = 1
        OTHERS         = 2.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        INPUT          = GS_EKPO-BPRME
        LANGUAGE       = SY-LANGU
      IMPORTING
        OUTPUT         = GS_DATA-BPRME
      EXCEPTIONS
        UNIT_NOT_FOUND = 1
        OTHERS         = 2.

    READ TABLE GT_EKET INTO GS_EKET WITH KEY EBELN = GS_EKPO-EBELN
                                             EBELP = GS_EKPO-EBELP.
    IF SY-SUBRC EQ 0.
      PERFORM F_TRANSFORM_DATE USING GS_EKET-EINDT
                               CHANGING GS_DATA-EKET_EINDT.
    ENDIF.

    PERFORM F_APPEND_EKKO USING GS_EKPO-EBELN
                          CHANGING GS_DATA.

    PERFORM F_APPEND_EKES USING GS_EKPO-EBELN
                                GS_EKPO-EBELP
                          CHANGING GS_DATA.

*   Get header text
    MOVE GS_EKPO-EBELN TO LF_TEXT.
    PERFORM F_APPEND_TEXT USING LF_TEXT
                                GC_OBJEC_HEADERTXT
                          CHANGING GS_DATA.

    PERFORM F_APPEND_EKKN USING GS_EKPO-EBELN
                                GS_EKPO-EBELP
                          CHANGING GS_DATA.

*   Get item text
    CONCATENATE GS_EKPO-EBELN GS_EKPO-EBELP  INTO LF_TEXT.
    PERFORM F_APPEND_TEXT USING LF_TEXT
                                GC_OBJEC_ITEMTXT
                          CHANGING GS_DATA.

    APPEND GS_DATA TO GT_DATA.
  ENDLOOP.

  SORT GT_DATA BY EBELN EBELP MATNR.

ENDFORM. " F_PREPARE_DATA
*&---------------------------------------------------------------------*
*&      Form  F_TRANSFORM_DATE
*&---------------------------------------------------------------------*
FORM  F_TRANSFORM_DATE USING UF_DATE TYPE DATS
                       CHANGING CF_DATE TYPE STRING.

*  Change date format from YYYYMMDD to DD.MM.YYYY
  CONCATENATE UF_DATE+6(2) UF_DATE+4(2) UF_DATE+0(4) INTO CF_DATE SEPARATED BY '.'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_APPEND_EKKO
*&---------------------------------------------------------------------*
FORM F_APPEND_EKKO USING UF_EBELN TYPE EKPO-EBELN
                   CHANGING CS_DATA TYPE TS_DATA.

  READ TABLE GT_EKKO INTO GS_EKKO WITH KEY EBELN = UF_EBELN.
  IF SY-SUBRC EQ 0.
*     From lfa1
    READ TABLE GT_LFA1 INTO GS_LFA1 WITH KEY LIFNR = GS_EKKO-LIFNR.
    IF SY-SUBRC EQ 0.
      CS_DATA-NAME1 = GS_LFA1-NAME1.
    ENDIF.
*     From konv
    READ TABLE GT_KONV INTO GS_KONV WITH KEY KNUMV = GS_EKKO-KNUMV.
    IF SY-SUBRC EQ 0.
      CS_DATA-HEADER_CON = GS_KONV-KBETR / 10.
    ENDIF.

    CS_DATA-BSART	=	GS_EKKO-BSART.
    CS_DATA-EBELN = GS_EKKO-EBELN.
    CS_DATA-LIFNR =	GS_EKKO-LIFNR.
    PERFORM F_TRANSFORM_DATE USING GS_EKKO-BEDAT
                             CHANGING CS_DATA-BEDAT.
    CS_DATA-EKORG	        =	GS_EKKO-EKORG.
    CS_DATA-EKGRP	        =	GS_EKKO-EKGRP.
    CS_DATA-BUKRS	        =	GS_EKKO-BUKRS.
    CS_DATA-ZTERM	        =	GS_EKKO-ZTERM.
    CS_DATA-WAERS	        =	GS_EKKO-WAERS.
    CS_DATA-WKURS	        =	GS_EKKO-WKURS.
    CS_DATA-KUFIX         =	GS_EKKO-KUFIX.
    CS_DATA-INCO1         =	GS_EKKO-INCO1.
    CS_DATA-INCO2	        =	GS_EKKO-INCO2.
    CS_DATA-IHREZ         =	GS_EKKO-IHREZ.
    CS_DATA-UNSEZ	        =	GS_EKKO-UNSEZ.
    CS_DATA-VERKF         = GS_EKKO-VERKF.
    GS_DATA-RETTP         = GS_EKKO-RETTP.
    GS_DATA-HEADER_RETPC  = GS_EKKO-RETPC.
    CS_DATA-ZZ1_PRJ_C_PO  = GS_EKKO-ZZ1_PRJ_C_PO.
    CS_DATA-ZZ1_DLV_MODE  = GS_EKKO-ZZ1_DLV_MODE.
    CS_DATA-ZZ1_DIVCD_PO  = GS_EKKO-ZZ1_DIVCD_PO.
    CS_DATA-ZZ_JTEPA      = GS_EKKO-ZZ_JTEPA.
  ENDIF.

ENDFORM. " F_APPEND_EKKO
*&---------------------------------------------------------------------*
*&      Form  f_append_ekes
*&---------------------------------------------------------------------*
FORM F_APPEND_EKES USING UF_EBELN TYPE EKPO-EBELN
                         UF_EBELP TYPE EKPO-EBELP
                   CHANGING CS_DATA TYPE TS_DATA.

  READ TABLE GT_EKES INTO GS_EKES WITH KEY EBELN = UF_EBELN
                                           EBELP = UF_EBELN.
  IF SY-SUBRC EQ 0.
    CS_DATA-ETENS = GS_EKES-ETENS.
    CS_DATA-EBTYP = GS_EKES-EBTYP.
    PERFORM F_TRANSFORM_DATE USING GS_EKES-EINDT
                             CHANGING CS_DATA-EKES_EINDT.
    CS_DATA-MENGE = GS_EKES-MENGE.
    CS_DATA-XBLNR = GS_EKES-XBLNR.
    PERFORM F_TRANSFORM_DATE USING GS_EKES-ERDAT
                             CHANGING CS_DATA-ERDAT.

  ENDIF.
ENDFORM. " F_APPEND_EKES
*&---------------------------------------------------------------------*
*&      Form  F_APPEND_TEXT
*&---------------------------------------------------------------------*
FORM F_APPEND_TEXT USING UF_TEXT  TYPE THEAD-TDNAME
                         UF_TDOBJECT TYPE TDOBJECT
                   CHANGING CS_DATA TYPE TS_DATA.

  DATA: LT_LINE TYPE TABLE OF TLINE,
        LS_LINE TYPE TLINE.

  CASE UF_TDOBJECT.
    WHEN GC_OBJEC_HEADERTXT.
      CLEAR: LT_LINE, LS_LINE.
*      Form line
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          ID                      = 'F01'
          LANGUAGE                = 'E'
          NAME                    = UF_TEXT
          OBJECT                  = GC_OBJEC_HEADERTXT
        TABLES
          LINES                   = LT_LINE
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.
      IF SY-SUBRC EQ 0.
        LOOP AT LT_LINE INTO LS_LINE.
          CONDENSE LS_LINE-TDLINE.
          CASE SY-TABIX.
            WHEN 1.
              CS_DATA-FROM_LINE1 = LS_LINE-TDLINE.
            WHEN 2.
              CS_DATA-FROM_LINE2 = LS_LINE-TDLINE.
            WHEN 3.
              CS_DATA-FROM_LINE3 = LS_LINE-TDLINE.
          ENDCASE.
        ENDLOOP.
        CLEAR: LT_LINE.
      ENDIF.

*      Header Remark line
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          ID                      = 'F00'
          LANGUAGE                = 'E'
          NAME                    = UF_TEXT
          OBJECT                  = GC_OBJEC_HEADERTXT
        TABLES
          LINES                   = LT_LINE
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.
      IF SY-SUBRC EQ 0.
        LOOP AT LT_LINE INTO LS_LINE.
          CONDENSE LS_LINE-TDLINE.
          CASE SY-TABIX.
            WHEN 1.
              CS_DATA-ZZREMARKS01 = LS_LINE-TDLINE.
            WHEN 2.
              CS_DATA-ZZREMARKS02 = LS_LINE-TDLINE.
            WHEN 3.
              CS_DATA-ZZREMARKS03 = LS_LINE-TDLINE.
            WHEN 4.
              CS_DATA-ZZREMARKS04 = LS_LINE-TDLINE.
            WHEN 5.
              CS_DATA-ZZREMARKS05 = LS_LINE-TDLINE.
          ENDCASE.
        ENDLOOP.
        CLEAR: LT_LINE.
      ENDIF.
    WHEN GC_OBJEC_ITEMTXT.
      CLEAR: LT_LINE, LS_LINE.

*      Internal Order
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          ID                      = 'F50'
          LANGUAGE                = 'E'
          NAME                    = UF_TEXT
          OBJECT                  = GC_OBJEC_ITEMTXT
        TABLES
          LINES                   = LT_LINE
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.
      IF SY-SUBRC EQ 0.
        LOOP AT LT_LINE INTO LS_LINE.
          CONDENSE LS_LINE-TDLINE.
          CASE SY-TABIX.
            WHEN 1.
              CS_DATA-INTERNAL_LINE1 = LS_LINE-TDLINE.
            WHEN 2.
              CS_DATA-INTERNAL_LINE2 = LS_LINE-TDLINE.
            WHEN 3.
              CS_DATA-INTERNAL_LINE3 = LS_LINE-TDLINE.
          ENDCASE.
        ENDLOOP.
        CLEAR: LT_LINE.
      ENDIF.

*      Project Name
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          ID                      = 'F51'
          LANGUAGE                = 'E'
          NAME                    = UF_TEXT
          OBJECT                  = GC_OBJEC_ITEMTXT
        TABLES
          LINES                   = LT_LINE
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.
      IF SY-SUBRC EQ 0.
        LOOP AT LT_LINE INTO LS_LINE.
          CONDENSE LS_LINE-TDLINE.
          CASE SY-TABIX.
            WHEN 1.
              CS_DATA-PROJECT_LINE1 = LS_LINE-TDLINE.
            WHEN 2.
              CS_DATA-PROJECT_LINE2 = LS_LINE-TDLINE.
            WHEN 3.
              CS_DATA-PROJECT_LINE3 = LS_LINE-TDLINE.
          ENDCASE.
        ENDLOOP.
        CLEAR: LT_LINE.
      ENDIF.

*      Attention
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          ID                      = 'F52'
          LANGUAGE                = 'E'
          NAME                    = UF_TEXT
          OBJECT                  = GC_OBJEC_ITEMTXT
        TABLES
          LINES                   = LT_LINE
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.
      IF SY-SUBRC EQ 0.
        LOOP AT LT_LINE INTO LS_LINE.
          CONDENSE LS_LINE-TDLINE.
          CASE SY-TABIX.
            WHEN 1.
              CS_DATA-ATTENTION_LINE1 = LS_LINE-TDLINE.
            WHEN 2.
              CS_DATA-ATTENTION_LINE2 = LS_LINE-TDLINE.
            WHEN 3.
              CS_DATA-ATTENTION_LINE3 = LS_LINE-TDLINE.
          ENDCASE.
        ENDLOOP.
        CLEAR: LT_LINE.
      ENDIF.

*   Person In Charge
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          ID                      = 'F56'
          LANGUAGE                = 'E'
          NAME                    = UF_TEXT
          OBJECT                  = GC_OBJEC_ITEMTXT
        TABLES
          LINES                   = LT_LINE
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.
      IF SY-SUBRC EQ 0.
        LOOP AT LT_LINE INTO LS_LINE.
          CONDENSE LS_LINE-TDLINE.
          CASE SY-TABIX.
            WHEN 1.
              CS_DATA-PERSON_LINE1 = LS_LINE-TDLINE.
            WHEN 2.
              CS_DATA-PERSON_LINE2 = LS_LINE-TDLINE.
            WHEN 3.
              CS_DATA-PERSON_LINE3 = LS_LINE-TDLINE.
          ENDCASE.
        ENDLOOP.
        CLEAR: LT_LINE.
      ENDIF.

*      Remark
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          ID                      = 'F00'
          LANGUAGE                = 'E'
          NAME                    = UF_TEXT
          OBJECT                  = GC_OBJEC_ITEMTXT
        TABLES
          LINES                   = LT_LINE
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.
      IF SY-SUBRC EQ 0.
        LOOP AT LT_LINE INTO LS_LINE.
          CONDENSE LS_LINE-TDLINE.
          CASE SY-TABIX.
            WHEN 1.
              CS_DATA-REMARK_LINE1 = LS_LINE-TDLINE.
            WHEN 2.
              CS_DATA-REMARK_LINE2 = LS_LINE-TDLINE.
            WHEN 3.
              CS_DATA-REMARK_LINE3 = LS_LINE-TDLINE.
          ENDCASE.
        ENDLOOP.
        CLEAR: LT_LINE.
      ENDIF.

*      Item Text
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          ID                      = 'F01'
          LANGUAGE                = 'E'
          NAME                    = UF_TEXT
          OBJECT                  = GC_OBJEC_ITEMTXT
        TABLES
          LINES                   = LT_LINE
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.
      IF SY-SUBRC EQ 0.
        LOOP AT LT_LINE INTO LS_LINE.
          CONDENSE LS_LINE-TDLINE.
          CASE SY-TABIX.
            WHEN 1.
              CS_DATA-ITEM_TXT1 = LS_LINE-TDLINE.
            WHEN 2.
              CS_DATA-ITEM_TXT2 = LS_LINE-TDLINE.
            WHEN 3.
              CS_DATA-ITEM_TXT3 = LS_LINE-TDLINE.
          ENDCASE.
        ENDLOOP.
        CLEAR: LT_LINE.
      ENDIF.

*      Memo
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          ID                      = 'F75'
          LANGUAGE                = 'E'
          NAME                    = UF_TEXT
          OBJECT                  = GC_OBJEC_ITEMTXT
        TABLES
          LINES                   = LT_LINE
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.
      IF SY-SUBRC EQ 0.
        LOOP AT LT_LINE INTO LS_LINE.
          CONDENSE LS_LINE-TDLINE.
          CASE SY-TABIX.
            WHEN 1.
              CS_DATA-MEMO_LINE1 = LS_LINE-TDLINE.
            WHEN 2.
              CS_DATA-MEMO_LINE2 = LS_LINE-TDLINE.
            WHEN 3.
              CS_DATA-MEMO_LINE3 = LS_LINE-TDLINE.
          ENDCASE.
        ENDLOOP.
        CLEAR: LT_LINE.
      ENDIF.

*      Warranty
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          ID                      = 'F73'
          LANGUAGE                = 'E'
          NAME                    = UF_TEXT
          OBJECT                  = GC_OBJEC_ITEMTXT
        TABLES
          LINES                   = LT_LINE
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.
      IF SY-SUBRC EQ 0.
        LOOP AT LT_LINE INTO LS_LINE.
          CONDENSE LS_LINE-TDLINE.
          CASE SY-TABIX.
            WHEN 1.
              CS_DATA-WARR_LINE1 = LS_LINE-TDLINE.
            WHEN 2.
              CS_DATA-WARR_LINE2 = LS_LINE-TDLINE.
            WHEN 3.
              CS_DATA-WARR_LINE3 = LS_LINE-TDLINE.
          ENDCASE.
        ENDLOOP.
        CLEAR: LT_LINE.
      ENDIF.

*      Engineer
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          ID                      = 'F74'
          LANGUAGE                = 'E'
          NAME                    = UF_TEXT
          OBJECT                  = GC_OBJEC_ITEMTXT
        TABLES
          LINES                   = LT_LINE
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.
      IF SY-SUBRC EQ 0.
        LOOP AT LT_LINE INTO LS_LINE.
          CONDENSE LS_LINE-TDLINE.
          CASE SY-TABIX.
            WHEN 1.
              CS_DATA-EN_LINE1 = LS_LINE-TDLINE.
            WHEN 2.
              CS_DATA-EN_LINE2 = LS_LINE-TDLINE.
            WHEN 3.
              CS_DATA-EN_LINE3 = LS_LINE-TDLINE.
          ENDCASE.
        ENDLOOP.
        CLEAR: LT_LINE.
      ENDIF.
  ENDCASE.
ENDFORM. "F_APPEND_TEXT
*&---------------------------------------------------------------------*
*&      Form  F_APPEND_EKKN
*&---------------------------------------------------------------------*
FORM F_APPEND_EKKN USING UF_EBELN TYPE EKPO-EBELN
                         UF_EBELP TYPE EKPO-EBELP
                   CHANGING CS_DATA TYPE TS_DATA.

  READ TABLE GT_EKKN INTO GS_EKKN WITH KEY EBELN = GS_EKPO-EBELN
                                           EBELP = GS_EKPO-EBELP.
  IF SY-SUBRC EQ 0.
    CS_DATA-ZEKKN	      =	GS_EKKN-ZEKKN.
    CS_DATA-EKKN_MENGE  = GS_EKKN-MENGE.
    CS_DATA-SAKTO	      =	GS_EKKN-SAKTO.
    CS_DATA-KOSTL	      =	GS_EKKN-KOSTL.
    CS_DATA-FISTL	      =	GS_EKKN-FISTL.
    CS_DATA-ANLN1	      =	GS_EKKN-ANLN1.
    CS_DATA-ANLN2	      =	GS_EKKN-ANLN2.
    CS_DATA-FIPOS	      =	GS_EKKN-FIPOS.
    CS_DATA-AUFNR	      =	GS_EKKN-AUFNR.
    CS_DATA-PRCTR	      =	GS_EKKN-PRCTR.
    CS_DATA-PS_PSP_PNR  = GS_EKKN-PS_PSP_PNR.
    CS_DATA-VBELN	      =	GS_EKKN-VBELN.
    CS_DATA-VBELP	      =	GS_EKKN-VBELP.
  ENDIF.
ENDFORM. " F_APPEND_EKKN
*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_LOCAL
*&---------------------------------------------------------------------*
FORM F_DOWNLOAD_LOCAL .

  DATA: LF_FILENAME          TYPE STRING,
        LF_FILENAME_FULLPATH TYPE STRING.

  DATA: LF_CHUNKS TYPE I,
        LF_START  TYPE I,
        LF_END    TYPE I,
        LF_INDEX  TYPE STRING,
        LF_TEXT   TYPE STRING.

  DATA: LT_CHUNKSDATA TYPE TABLE OF TS_DATA,
        LS_CHUNKSDATA TYPE TS_DATA.

  IF P_MAXREC IS NOT INITIAL.
    LF_CHUNKS = CEIL( LINES( GT_DATA ) / P_MAXREC ).

    DO LF_CHUNKS TIMES.

      LF_START = ( SY-INDEX - 1 ) * P_MAXREC + 1.
      LF_END = SY-INDEX * P_MAXREC.

      CLEAR LT_CHUNKSDATA.
      APPEND  GS_HEADER TO LT_CHUNKSDATA.
      LOOP AT GT_DATA INTO LS_CHUNKSDATA FROM LF_START TO LF_END.
        APPEND LS_CHUNKSDATA TO LT_CHUNKSDATA.
      ENDLOOP.
      MOVE SY-INDEX TO LF_INDEX.

      CONCATENATE 'ExtractPO_' SY-DATUM '_' SY-UZEIT '_FileSeq_' LF_INDEX '.xls' INTO LF_FILENAME.
      CONDENSE LF_FILENAME NO-GAPS.
      CONCATENATE P_FILE LF_FILENAME INTO LF_FILENAME_FULLPATH SEPARATED BY '\'.

      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          FILENAME                = LF_FILENAME_FULLPATH
          FILETYPE                = 'ASC'
          WRITE_FIELD_SEPARATOR   = 'X'
          CODEPAGE                = '4103'
          WRITE_BOM               = 'X'
        TABLES
          DATA_TAB                = LT_CHUNKSDATA
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
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDDO.

    IF LF_END < LINES( GT_DATA ).
      LF_START = LF_END + 1.
      LF_END = LINES( GT_DATA ).
      LF_CHUNKS = LF_CHUNKS + 1.
      CLEAR LT_CHUNKSDATA.
      APPEND  GS_HEADER TO LT_CHUNKSDATA.
      LOOP AT GT_DATA INTO LS_CHUNKSDATA FROM LF_START TO LF_END.
        APPEND LS_CHUNKSDATA TO LT_CHUNKSDATA.
      ENDLOOP.
      MOVE LF_CHUNKS  TO LF_INDEX.

      CONCATENATE 'ExtractPO_' SY-DATUM '_' SY-UZEIT '_FileSeq_' LF_INDEX '.xls' INTO LF_FILENAME.
      CONDENSE LF_FILENAME NO-GAPS.
      CONCATENATE P_FILE LF_FILENAME INTO LF_FILENAME_FULLPATH SEPARATED BY '\'.

      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          FILENAME                = LF_FILENAME_FULLPATH
          FILETYPE                = 'ASC'
          WRITE_FIELD_SEPARATOR   = 'X'
          CODEPAGE                = '4103'
          WRITE_BOM               = 'X'
        TABLES
          DATA_TAB                = LT_CHUNKSDATA
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
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        MOVE LINES( GT_DATA ) TO LF_TEXT.
        CONCATENATE 'Download Complete.' LF_TEXT 'Records.' INTO LF_TEXT SEPARATED BY SPACE.
        MESSAGE LF_TEXT TYPE 'S'.
      ENDIF.
    ELSE.
      MOVE LINES( GT_DATA ) TO LF_TEXT.
      CONCATENATE 'Download Complete.' LF_TEXT 'Records.' INTO LF_TEXT SEPARATED BY SPACE.
      MESSAGE LF_TEXT TYPE 'S'.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_DOWNLOAD_LOCAL
