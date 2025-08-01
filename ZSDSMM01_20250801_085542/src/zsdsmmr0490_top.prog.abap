*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0490_TOP
*&---------------------------------------------------------------------*
INCLUDE <LIST>.

* Dynpro 100
DATA:  BEGIN OF D0100,
         EBELN        TYPE EBELN,
         TESTRUN      TYPE BAPIFLAG,
         LIFNR        TYPE LIFNR,
         VNAME        TYPE NAME1_GP,
         EKORG        TYPE EKORG,
         EKGRP        TYPE EKGRP,
         BEDAT        TYPE EBDAT,
         TEXT20       TYPE TEXT20,
END OF D0100.

* PO Header & Items
TYPES : BEGIN OF T_EKKOPO, " EKKO + EKPO
          EBELN    TYPE EKKO-EBELN,
          BSART    TYPE EKKO-BSART,
          LIFNR    TYPE EKKO-LIFNR,
          EKORG    TYPE EKKO-EKORG,
          EKGRP    TYPE EKKO-EKGRP,
          BEDAT    TYPE EKKO-BEDAT,
          EBELP    TYPE EKPO-EBELP,  " EKPO
          LOEKZ    TYPE EKPO-LOEKZ,
          ETENR    TYPE EKET-ETENR,
          ETENS    TYPE EKES-ETENS,
          MATNR    TYPE EKPO-MATNR,
          WERKS    TYPE EKPO-WERKS,
          LGORT    TYPE EKPO-LGORT,
          MENGE    TYPE EKPO-MENGE,
          MEINS    TYPE EKPO-MEINS,
          NETWR    TYPE EKPO-NETWR,
END OF T_EKKOPO.

* Table Control
TYPES: BEGIN OF T_TABLE01,
         FLG         TYPE C,
         DELETE(16)  TYPE C,
         MULTI       TYPE C,
         EBELN       TYPE EKKO-EBELN,
         EBELP       TYPE EKPO-EBELP,
         BSART       TYPE EKKO-BSART,
         LOEKZ       TYPE EKPO-LOEKZ,
         EBELP2      TYPE EKPO-EBELP,      "bunkatu moto meisai bango
         ETENR       TYPE EKET-ETENR,
         ETENS       TYPE EKES-ETENS,
         MATNR       TYPE EKPO-MATNR,
         MENGE       TYPE EKPO-MENGE,
         DABMG       TYPE EKES-DABMG,
         EINDT       TYPE EKET-EINDT,
         LGORT       TYPE LGORT_D,
         LGOBE       TYPE T001L-LGOBE,
         MEINS       TYPE EKPO-MEINS,
         I_QTY       TYPE EKPO-MENGE,     "input bunkatu suryo
         I_DAT       TYPE EKET-EINDT,     "input bunkatu date
         MARK        TYPE S_MARK,
         I_LGORT     TYPE T001L-LGORT,
         LGOBE_C     TYPE T001L-LGOBE,
         NETWR       TYPE EKPO-NETWR,
       END OF T_TABLE01.

TYPES : BEGIN OF T_MSG ,        "BAPI Return
         TYPE(1),
         ID(20),
         NUMBER(3),
         MESSAGE(220),
         MESSAGE_V1(50),
         MESSAGE_V2(50),
         MESSAGE_V3(50),
         MESSAGE_V4(50),
END OF T_MSG.

TYPES : BEGIN OF T_UNDOLIN,
         FLG         TYPE C,
         DELETE(16)  TYPE C,
         MULTI       TYPE C,
         EBELN       TYPE EKKO-EBELN,
         EBELP       TYPE EKPO-EBELP,
         EBELP2      TYPE EKPO-EBELP,
         MATNR       TYPE EKPO-MATNR,
         LGORT       TYPE LGORT_D,
         MENGE       TYPE EKPO-MENGE,
         DABMG       TYPE EKES-DABMG,
END OF T_UNDOLIN.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TABLE_CONTROL01' ITSELF
CONTROLS: TABLE_CONTROL01 TYPE TABLEVIEW USING SCREEN 0100.

*&SPWIZARD: LINES OF TABLECONTROL 'TABLE_CONTROL01'
DATA:     G_TABLE_CONTROL01_LINES  LIKE SY-LOOPC.

DATA:     OK_CODE LIKE SY-UCOMM.

DATA: ITAB_TABLE01 TYPE T_TABLE01 OCCURS 0 WITH HEADER LINE,
      ITAB_EKKOPO  TYPE T_EKKOPO  OCCURS 0 WITH HEADER LINE,
      ITAB_MSG     TYPE T_MSG     OCCURS 0 WITH HEADER LINE,
      ITAB_UNDOLIN TYPE T_UNDOLIN OCCURS 0 WITH HEADER LINE.

DATA: G_SRCH_FLD LIKE FELD-NAME,
      G_TEXT20   TYPE TEXT20.

DATA: G_EBELN       TYPE EKKO-EBELN,
      G_CLEAR       TYPE C,
      G_SRCH_LINE   TYPE I,
      G_TODAY       TYPE SY-DATUM,
      G_D0900_UCOMM TYPE C.
