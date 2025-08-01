*&---------------------------------------------------------------------*
*& Include          ZSDSFII0030_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*  TABLE
*&---------------------------------------------------------------------*
TABLES : VBAK,ZSDSMMT003.
*&---------------------------------------------------------------------*
*  TYPE
*&---------------------------------------------------------------------*
TYPES : BEGIN OF GY_HEADER,
          VKBUR        TYPE VBAK-VKBUR,
          SALEO        TYPE TEXT100,
          VTWEG        TYPE VBAK-VTWEG,
          VTEXT        TYPE TVTWT-VTEXT,
          VBELN        TYPE VBAK-VBELN,
          VKGRP        TYPE VBAK-VKGRP,
          SALEG        TYPE TEXT100,
          AUDAT        TYPE VBAK-AUDAT,
          PERNR        TYPE VBPA-PERNR,
          SNAME        TYPE PA0001-SNAME,
          "adatu      TYPE vbak-adatu,
          KUNNR        TYPE VBAK-KUNNR,
          NAME1        TYPE KNA1-NAME1,
          "bdatu      TYPE vbak-bdatu,
          "hdamt      TYPE vbak-hdamt,
          KUNNR1       TYPE VBAK-KUNNR,
          NAME1_SHIP   TYPE KNA1-NAME1,
          "edatu      TYPE vbak-edatu,
          NETWR        TYPE VBAK-NETWR,
          "contact    TYPE vbak-contact,
          AUFNR        TYPE VBAK-AUFNR,
          VTPCT        TYPE P DECIMALS 2,
          VTAMT        TYPE VBAP-MWSBP,
          ZTERM        TYPE KNB1-ZTERM,
          TOTAL        TYPE VBAK-NETWR,
          IHREZ        TYPE VBAK-IHREZ,
          BSTNK        TYPE BSTKD_E,
          BSTDK        TYPE VBAK-BSTDK,
          HDRRMK       TYPE TEXT100,
          PROJTXT      TYPE TEXT100,
          AUART        TYPE VBAK-AUART,
          NETWR_V      TYPE VBAP-NETWR,
          CONTACT_NAME TYPE TEXT100,
          CONTACT_TEL  TYPE TEXT100,
          REFNO        TYPE ZSDSMMT003-REFNO,
          BSTKD_E      TYPE VBKD-BSTKD_E,
          LAND         TYPE TEXT100,
          PO_MASTER    TYPE TEXT35,
        END OF GY_HEADER.

TYPES : BEGIN OF GY_VBPA,
          VBELN TYPE VBPA-VBELN,
          PARVW TYPE VBPA-PARVW,
          KUNNR TYPE VBPA-KUNNR,
          PERNR TYPE VBPA-PERNR,
          NAME1 TYPE KNA1-NAME1,
        END OF GY_VBPA.

TYPES : BEGIN OF GY_KNA1,
          KUNNR TYPE KNA1-KUNNR,
          NAME1 TYPE KNA1-NAME1,
        END OF GY_KNA1.

TYPES : BEGIN OF GY_PA0001,
          PERNR TYPE PA0001-PERNR,
*endda TYPE pa001-endda,
*begda TYPE pa001-begda,
          SNAME TYPE PA0001-SNAME,
        END OF GY_PA0001.

TYPES : BEGIN OF GY_KNB1,
          KUNNR TYPE KNB1-KUNNR,
          ZTERM TYPE KNB1-ZTERM,
        END OF GY_KNB1.

TYPES : BEGIN OF GY_DETAIL,
          SEL     TYPE C,
          VBELN   TYPE VBAP-VBELN,
          POSNR   TYPE VBAP-POSNR,
          MATNR   TYPE VBAP-MATNR,
          KWMENG  TYPE I, "vbap-kwmeng,
          VRKME   TYPE VBAP-VRKME,
          MAKTX   TYPE MAKT-MAKTX,
          NETWR   TYPE VBAP-NETWR,
          WAERK   TYPE VBAP-WAERK,
          MATKL   TYPE VBAP-MATKL,
          PRODH   TYPE VBAP-PRODH,
          ERDAT   TYPE VBAP-ERDAT,
          ERNAM   TYPE VBAP-ERNAM,
          ERZET   TYPE VBAP-ERZET,
          AUFNR   TYPE VBAP-AUFNR,
          AEDAT   TYPE VBAP-AEDAT,
          MWSBP   TYPE VBAP-MWSBP,
          MEINS   TYPE VBAP-MEINS,
          NETPR   TYPE VBAP-NETPR,
          NETWR_V TYPE VBAP-NETWR,
          KTEXT   TYPE AUFK-KTEXT,
          KOWRR   TYPE VBAP-KOWRR,
          UEPOS   TYPE VBAP-UEPOS,
          ITMRMK  TYPE ZSDSDE_REMARK,
          UPMAT   TYPE VBAP-UPMAT,
          PRBOM   TYPE VBAP-NETWR,
          LAND    TYPE TEXT100,
          POSNR_T TYPE VBAP-POSNR,
          KOWRR_T TYPE VBAP-KOWRR,
          UEPOS_T TYPE VBAP-UEPOS,
          UPMAT_T TYPE VBAP-UPMAT,
          CM      TYPE C,
*  kwmeng_a_cm TYPE i,
*  kwmeng_cm   TYPE i,
        END OF GY_DETAIL.

TYPES : BEGIN OF GY_VBAP,
          VBELN  TYPE VBAP-VBELN,
          POSNR  TYPE VBAP-POSNR,
          MATNR  TYPE VBAP-MATNR,
          KWMENG TYPE VBAP-KWMENG,
          VRKME  TYPE VBAP-VRKME,
          NETWR  TYPE VBAP-NETWR,
          WAERK  TYPE VBAP-WAERK,
          MATKL  TYPE VBAP-MATKL,
          PRODH  TYPE VBAP-PRODH,
          ERDAT  TYPE VBAP-ERDAT,
          ERNAM  TYPE VBAP-ERNAM,
          ERZET  TYPE VBAP-ERZET,
          AUFNR  TYPE VBAP-AUFNR,
          MWSBP  TYPE VBAP-MWSBP,
          MEINS  TYPE VBAP-MEINS,
          NETPR  TYPE VBAP-NETPR,
          KOWRR  TYPE VBAP-KOWRR,
          UEPOS  TYPE VBAP-UEPOS,
          UPMAT  TYPE VBAP-UPMAT,
        END OF GY_VBAP.

TYPES : BEGIN OF GY_SUM_BOM,
          MATNR TYPE VBAP-MATNR,
          UEPOS TYPE VBAP-UEPOS,
          PRBOM TYPE VBAP-NETPR,
        END OF GY_SUM_BOM.

TYPES : BEGIN OF GY_VBKD,
          VBELN   TYPE VBKD-VBELN,
          POSNR   TYPE VBKD-POSNR,
          BSTKD_E TYPE VBKD-BSTKD_E,
        END OF GY_VBKD.

TYPES : BEGIN OF GY_REASON,
          YES TYPE C,
          NO  TYPE C,
        END OF GY_REASON.
*&---------------------------------------------------------------------*
*  VARIABLE
*&---------------------------------------------------------------------*
DATA : GT_HEADER TYPE TABLE OF GY_HEADER,
       GS_HEADER TYPE GY_HEADER.

DATA : GT_DETAIL TYPE TABLE OF GY_DETAIL,
       GS_DETAIL TYPE GY_DETAIL.

DATA : GT_VBPA TYPE TABLE OF GY_VBPA,
       GS_VBPA TYPE GY_VBPA.

DATA : GT_KNA1 TYPE TABLE OF GY_KNA1,
       GS_KNA1 TYPE GY_KNA1.

DATA : GT_PA0001 TYPE TABLE OF GY_PA0001,
       GS_PA0001 TYPE GY_PA0001.

DATA : GT_KNB1 TYPE TABLE OF GY_KNB1,
       GS_KNB1 TYPE GY_KNB1.

DATA : GT_VBKD TYPE TABLE OF GY_VBKD,
       GS_VBKD TYPE GY_VBKD.

DATA : GT_ZTSD_PO_D TYPE TABLE OF ZSDSMMT004,
       GS_ZTSD_PO_D TYPE ZSDSMMT004.

DATA : GT_ZTSD_PO_H TYPE TABLE OF ZSDSMMT003,
       GS_ZTSD_PO_H TYPE ZSDSMMT003.

DATA : GT_SUM_BOM TYPE TABLE OF GY_SUM_BOM,
       GS_SUM_BOM TYPE GY_SUM_BOM.

DATA : GS_REASON TYPE GY_REASON.

DATA : GC_ALV1   TYPE REF TO CL_GUI_ALV_GRID,
       GC_CONT1  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       GT_FCAT   TYPE LVC_T_FCAT,
       GT_LAYOUT TYPE LVC_S_LAYO,
       GT_EVENTS TYPE SLIS_T_EVENT WITH HEADER LINE.

DATA : GV_DOCTYPE           TYPE ZSDSDE_DOCTYP,
       GV_KTEXT             TYPE AUFK-KTEXT,
       GV_POSNR             TYPE VBAP-POSNR,
       GV_ERROR             TYPE C,
       GV_EXE               TYPE C LENGTH 3,
       GV_STATUS            TYPE C,
       GV_STA_LOG           TYPE C,
       GV_KUNNR	            TYPE KUNNR,
       GV_BSTKD	            TYPE BSTKD,
       GV_INSET             TYPE I,
       GV_CURSOR            TYPE CHAR30,
       GV_LINE              TYPE I,
       GV_FLAG              TYPE C,
       GV_LINE_DE           TYPE I,
       GV_CHECK_CHANGE_ITEM TYPE I,
       GV_CHECK_CHANGE_LAND TYPE C LENGTH 100,
       GV_CHECK_FLAG_LAND   TYPE C,
       GV_ZCUSTOMER_REASON  TYPE C LENGTH 25,
       GV_ZUSER_REASON      TYPE C LENGTH 25,
       GV_OLD_BSTNK         TYPE VBAK-BSTNK,
       GV_POSNR_INIT        TYPE VBAP-POSNR,
       GV_REASON            TYPE C LENGTH 3.

CONTROLS : COLTROL_QT TYPE TABLEVIEW USING SCREEN '0100'.
*&---------------------------------------------------------------------*
*  RANGES
*&---------------------------------------------------------------------*
RANGES : GR_STAT  FOR JEST-STAT,
         GR_MATNR FOR VBAP-MATNR.
*&---------------------------------------------------------------------*
*  CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS : GC_TRUE      TYPE C VALUE 'X',
            GC_ERROR     TYPE C VALUE 'X',
            GC_CONTAINER TYPE C LENGTH 10 VALUE 'ALV_DETAIL'.

CONSTANTS: GC_I  TYPE C LENGTH 1 VALUE 'I',
           GC_EQ TYPE C LENGTH 2 VALUE 'EQ',
           GC_S  TYPE C LENGTH 1 VALUE 'S',
           GC_E  TYPE C LENGTH 1 VALUE 'E',
           GC_X  TYPE C LENGTH 1 VALUE 'X',
           GC_A  TYPE C LENGTH 1 VALUE 'A',
           GC_L  TYPE C LENGTH 1 VALUE 'L'.
*&---------------------------------------------------------------------*
*  SELECTION-SCREEN
*&---------------------------------------------------------------------*
