*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0050
*  Creation Date      : 22.02.2024
*  Author             : B. Chiewsarikij
*  Add-on ID          : ZMMF003
*  Description        : Print cover of Purchase order Document
*                         - Out of warranty
*                         - In warranty
*                         - MA Contract
*  Purpose            : N/A
*  Copied from        : ZR_MM_SV_FRONTPO_290113
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Report ZSDSMMR0050
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSMMR0050 MESSAGE-ID ZSDSMM02.
TYPE-POOLS : TRUXS,SLIS,ICON.

*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : EKPO,EKKN,VBAK,KNA1,EKKO,LFA1,TVAKT,ZSDSCAC001,VBPA,ADRC,THEAD,
         VIAUFKS,IHPA,T003P,PMSDO,ZSDSMMT015,AFKO,JEST,VIQMFE,AFIH,
         AFVU.",zmm_frontpo.

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
*TYPES : BEGIN OF gy_ztfi_figlac03,
*          vbeln  TYPE ztfi_figlac03-vbeln,
*          aufnr  TYPE ztfi_figlac03-aufnr,
*          costp  TYPE ztfi_figlac03-costp,
*          costpa TYPE ztfi_figlac03-costpa,
*        END OF gy_ztfi_figlac03.

TYPES : BEGIN OF GY_RESB,
          RSNUM  TYPE RESB-RSNUM,
          MATNR  TYPE RESB-MATNR,
          POSNR  TYPE RESB-POSNR,
          BDMNG  TYPE RESB-BDMNG,
          MEINS  TYPE RESB-MEINS,
          GPREIS TYPE RESB-GPREIS,
          AUFNR  TYPE RESB-AUFNR,
        END OF GY_RESB.

TYPES : BEGIN OF GY_RESB_GROUP,
          GPREIS TYPE RESB-GPREIS,
          AUFNR  TYPE RESB-AUFNR,
        END OF GY_RESB_GROUP.

TYPES : BEGIN OF GY_HEADER,
          EBELN  TYPE EKPO-EBELN,
          VENDOR TYPE LFA1-LIFNR,
          NAME   TYPE C LENGTH 100,
          MATD   TYPE MSEG-MBLNR,
          SALE   TYPE C LENGTH 40,
          PO     TYPE C LENGTH 10,
          DATE1  TYPE EKKO-BEDAT,
          AUFNR  TYPE AUFK-AUFNR,
*BOI CH05
          ERNAM  TYPE EKKO-ERNAM,
          WAERS  TYPE EKKO-WAERS,
*EOI CH05
        END OF GY_HEADER.
TYPES: BEGIN OF GY_CRMS4D_SERV,
         OBJECT_ID     TYPE CRMS4D_SERV_H-OBJECT_ID, "SV number
         AC_ASSIGNMENT TYPE CRMS4D_SERV_H-AC_ASSIGNMENT, "IO Reference

       END OF GY_CRMS4D_SERV.
TYPES: BEGIN OF GY_CRMS4D_PARTNER,
         OBJECT_ID   TYPE CRMS4D_PARTNER-OBJECT_ID, "SV number
         PARTNER_FCT TYPE CRMS4D_PARTNER-PARTNER_FCT, "Partner type
         ADDR_NR     TYPE CRMS4D_PARTNER-ADDR_NR, "adrc reference
         PARTNER_ID  TYPE CRMS4D_PARTNER-PARTNER_ID, "kna1 reference
       END OF GY_CRMS4D_PARTNER.

TYPES: BEGIN OF GY_VBFA,
         VBELV   TYPE VBFA-VBELV,
         VBTYP_V TYPE VBFA-VBTYP_V,
         VBELN   TYPE VBFA-VBELN,
         VBTYP_N TYPE VBFA-VBTYP_N,
         ERDAT   TYPE VBFA-ERDAT,
       END OF GY_VBFA.

TYPES: BEGIN OF GY_IAOM_CRM_AUFK,
         AUFNR     TYPE IAOM_CRM_AUFK-AUFNR,
         OBJECT_ID TYPE IAOM_CRM_AUFK-OBJECT_ID,
       END OF GY_IAOM_CRM_AUFK.


*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : G_SUM        LIKE EKPO-NETWR,
       G_AMOUNT(16) TYPE P DECIMALS 2,
       G_COUNT(16)  TYPE P DECIMALS 0,
       LV_COUNT(16) TYPE P DECIMALS 0,
       G_VAT(16)    TYPE P DECIMALS 2,
       G_TOTAL(16)  TYPE P DECIMALS 2,
       G_SUMPR      LIKE EKPO-NETWR,
       LV_PRNO      LIKE EKPO-BANFN,
       LV_PONO      LIKE EKPO-BANFN,
       G_SUMPR2     LIKE EKPO-NETWR,
       G_SUMQTY     LIKE EKPO-MENGE,
       G_NO         TYPE I.

DATA: BEGIN OF GI_EKPO OCCURS 0,
        EBELN    LIKE EKPO-EBELN, " PO NO.
        LOEKZ    LIKE EKPO-LOEKZ, " DELETION
        EBELP    LIKE EKPO-EBELP, "PO Item
        BANFN    LIKE EKPO-BANFN, "PR NO.
        MENGE    LIKE EKPO-MENGE, "OTY. PO
        MEINS    LIKE EKPO-MEINS, "UNT. PO
        NETWR    LIKE EKPO-NETWR, "NET. PO/PER LINE
        WERKS    LIKE EKPO-WERKS, "PLANT
        KO_PRCTR LIKE EKPO-KO_PRCTR,  "Profit Center
        MATNR    LIKE EKPO-MATNR,
        LIFNR    TYPE EKKO-LIFNR,
      END OF GI_EKPO.

DATA: BEGIN OF GI_TEST OCCURS 0,
        EBELN LIKE EKPO-EBELN, " PO NO.
        BANFN LIKE EKPO-BANFN,
        LOEKZ LIKE EKPO-LOEKZ, " DELETION
        EINS  LIKE EKPO-MEINS, "UNT. PO
        NETWR LIKE EKPO-NETWR, "NET. PO/PER LINE
      END OF GI_TEST.

DATA: BEGIN OF GI_EKKN OCCURS 0,
        EBELN      LIKE EKKN-EBELN,
        EBELP      LIKE EKKN-EBELP,
        VBELN      LIKE EKKN-VBELN,
        AUFNR      LIKE EKKN-AUFNR, " ADD SVO CS MODULE
        PRCTR      LIKE EKKN-PRCTR, "Add by Wantanee C1-20110808
        PS_PSP_PNR LIKE EKKN-PS_PSP_PNR,
      END OF GI_EKKN.

DATA: BEGIN OF GI_EKKO OCCURS 0,
        EBELN LIKE EKKO-EBELN,
        BEDAT LIKE EKKO-BEDAT,
        LIFNR LIKE EKKO-LIFNR,
        ERNAM TYPE EKKO-ERNAM,
        WAERS TYPE EKKO-WAERS,
      END OF GI_EKKO.

TYPES:
  BEGIN OF GY_ZSDSMMT002,
    RUNNG TYPE ZSDSMMT002-RUNNG,
    EBELN TYPE ZSDSMMT002-EBELN,
    STATU TYPE ZSDSMMT002-STATU,
    POSIT TYPE ZSDSMMT002-POSIT,
    FLAGD TYPE ZSDSMMT002-FLAGD,
    REMAK TYPE ZSDSMMT002-REMAK,
    ERNAM TYPE ZSDSMMT002-ERNAM,
    ERDAT TYPE ZSDSMMT002-ERDAT,
    ERZET TYPE ZSDSMMT002-ERZET,

  END OF GY_ZSDSMMT002.

DATA: GT_ZSDSMMT002 TYPE STANDARD TABLE OF GY_ZSDSMMT002,
      GS_TMP        TYPE GY_ZSDSMMT002,
      GS_ZSDSMMT002 TYPE GY_ZSDSMMT002.

DATA: GI_EKKOV  LIKE  GI_EKKO OCCURS 0.

DATA : GT_RESB TYPE TABLE OF GY_RESB,
       GS_RESB TYPE GY_RESB.

DATA : GT_RESB_GROUP TYPE TABLE OF GY_RESB_GROUP,
       GS_RESB_GROUP TYPE GY_RESB_GROUP.

DATA : GT_HEADER TYPE TABLE OF GY_HEADER,
       GS_HEADER TYPE GY_HEADER.

DATA : GT_HEADER_SMART TYPE TABLE OF GY_HEADER.

DATA : GV_LINE TYPE I.

DATA: GT_VBFA TYPE STANDARD TABLE OF GY_VBFA,
      GS_VBFA TYPE GY_VBFA.
DATA: GV_SV_COF TYPE VBFA-VBELN.
DATA: GV_PLAN_COST TYPE MBEW-STPRS.

*-------Variable for Header SAP Script-------------------

DATA: GV_PO(10)      TYPE C,
      GV_INV         LIKE VBRK-VBELN,
      GV_INVD        LIKE VBRK-FKDAT,
      GV_MATD        LIKE MSEG-MBLNR,
      GV_VENDOR      LIKE LFA1-LIFNR,
      GV_NAME(100)   TYPE C,   "Add by Wantanee C1-20110808
      GV_DATE1       LIKE EKKO-BEDAT,
      GV_JOB         LIKE T003P-TXT,
      "gv_job like TVAKT-BEZEI,
      GV_SALE(40)    TYPE C,
      GV_PREPARE(20) TYPE C,
      GV_VAT(3)      TYPE P DECIMALS 2,
      GV_NAME1       LIKE KNA1-NAME1,
      GV_SVOTXT      LIKE T003P-TXT,
      GV_MAKTX       LIKE MAKT-MAKTX,
      GV_DMBTR       LIKE MSEG-DMBTR.
*----------------------------------------------------------------------*

DATA: BEGIN OF GI_DATA OCCURS 0,
        EBELN        LIKE EKPO-EBELN, " PO NO.
        BANFN        LIKE EKPO-BANFN, "PR NO.
        NETWR(8)     TYPE P DECIMALS 2, "NET. PO/PER LIN
        EBELP        LIKE EKPO-EBELP, " PO Item
        LOEKZ        LIKE EKPO-LOEKZ, " DELETION
        MENGE(5)     TYPE P DECIMALS 0 , "OTY. PO
        MEINS(10)    TYPE C, "UNT. PO
        WERKS        LIKE EKPO-WERKS, "PLANT
        VBELN        LIKE EKKN-VBELN,
        KUNNR        LIKE VBAK-KUNNR,
        VGBEL        LIKE VBAK-VGBEL,
        NAME1        LIKE KNA1-NAME1,
        TEXT(20)     TYPE C, " JOB NO. FROM TEXT VBAK
        BEDAT        LIKE EKKO-BEDAT, " PO DATE
        LIFNR        LIKE EKKO-LIFNR, " ACCOUNT VENDOR
*      NAME2 LIKE LFA1-NAME1, " VENDOR NAME Edit by Wantanee C1-20110808
        NAME2(100)   TYPE C,  "Add by Wantanee C1-20110808
        P_SALE(40)   TYPE C,
        AUART        LIKE VBAK-AUART,
        BEZEI        LIKE TVAKT-BEZEI,
        PREPARE(20)  TYPE C,
        VAT(3)       TYPE P DECIMALS 2,
        SUMQTY       LIKE EKPO-MENGE,
        GNO          LIKE EKPO-EBELP,
        ADRNR        LIKE VBPA-ADRNR,
        AUFNR        LIKE EKKN-AUFNR, "SVO CS MODULE modify 211209
        OBJNR        LIKE VIAUFKS-OBJNR, "modify 211209
        KUNUM        LIKE VIAUFKS-KUNUM, "modify 211209
        CUSNAME_SVO  LIKE KNA1-NAME1, "modify 211209
        ADRNR2       LIKE IHPA-ADRNR, "modify 211209
        AUART2       LIKE VIAUFKS-AUART, "modify 211209
        SVOTXT       LIKE T003P-TXT, "modify 211209
        VKBUR        LIKE PMSDO-VKBUR, "modify 211209 add type job for svo
        VKGRP        LIKE PMSDO-VKGRP, "modify 211209 add type job for svo
        HEADER_JOB   LIKE ZSDSMMT015-HEADER_JOB, "modify 211209 add type job for svo
        MAUFNR       LIKE AFKO-MAUFNR, "modify 211209 add Job no.(Super Order(SVO))
        AUFNR2       LIKE VIAUFKS-AUFNR, "modify 211209 add Job no.(Super Order(SVO))
        KDAUF        LIKE VIAUFKS-KDAUF, "modify 70110 add S/O for SVO MA in Job no.
        JOBNO        LIKE VIAUFKS-AUFNR,
        STATUS(10)   TYPE C,
        QMNUM        LIKE AFIH-QMNUM,
        FECOD        LIKE VIQMFE-FECOD,
        AUFPL        LIKE VIAUFKS-AUFPL,
        USR05        LIKE AFVU-USR05,
        PPRCTR       LIKE MSEG-PPRCTR,
        INV          TYPE VBRK-VBELN,
        INV_DATE     TYPE VBRK-ERDAT,
        PLAN_COST    TYPE MBEW-STPRS,
        PS_PSP_PNR   TYPE EKKN-PS_PSP_PNR,
        WBS_TEXT(50) TYPE C,
      END OF GI_DATA.

DATA: BEGIN OF GT_MSEG OCCURS 0.
DATA: MBLNR  LIKE  MSEG-MBLNR,
      MJAHR  LIKE  MSEG-MJAHR,
      ZEILE  LIKE  MSEG-ZEILE,
      BWART  LIKE  MSEG-BWART,
      AUFNR  LIKE  EKKN-AUFNR,
      MATNR  LIKE  MSEG-MATNR,
      MAKTX  LIKE  MAKT-MAKTX,
      DMBTR  LIKE  MSEG-DMBTR,
      SJAHR  LIKE  MSEG-SJAHR,
      SMBLN  LIKE  MSEG-SMBLN,
      SMBLP  LIKE  MSEG-SMBLP,
      PPRCTR LIKE MSEG-PPRCTR,
      EBELN  LIKE  MSEG-EBELN,
      EBELP  LIKE  MSEG-EBELP,
      END OF GT_MSEG.

DATA: BEGIN OF GT_VBRP OCCURS 0.

DATA: VBELN LIKE  VBRK-VBELN,
      FKDAT LIKE  VBRK-FKDAT,
      AUFNR LIKE  EKKN-AUFNR,
      END OF GT_VBRP.

DATA: TEXT LIKE TLINE OCCURS 1 WITH HEADER LINE.

* ----------------Selection gen_c-------------------
DATA: WA_VBAK TYPE VBAK,
      WA_MSEG LIKE GT_MSEG.

DATA: GS_CRMS4D_SERV    TYPE GY_CRMS4D_SERV,
      GT_CRMS4D_SERV    TYPE STANDARD TABLE OF GY_CRMS4D_SERV,
      GS_CRMS4D_PARTNER TYPE GY_CRMS4D_PARTNER,
      GT_CRMS4D_PARTNER TYPE STANDARD TABLE OF GY_CRMS4D_PARTNER,
      GT_IAOM_CRM_AUFK  TYPE STANDARD TABLE OF GY_IAOM_CRM_AUFK,
      GS_IAOM_CRM_AUFK  TYPE GY_IAOM_CRM_AUFK.

DATA: L_NAME  TYPE VRM_ID,
      LI_LIST TYPE VRM_VALUES,
      V_COUNT TYPE I,
      L_VALUE LIKE LINE OF LI_LIST.

DATA: L_PRCTR         TYPE EKPO-KO_PRCTR,
      L_TXTPSALE(20)  TYPE C,
      L_TXTPSALE1(20) TYPE C,
      L_AUFKPRC       TYPE AUFK-PRCTR.

DATA : GV_MNG       TYPE C,
       GV_GM        TYPE C,
       GV_AGM       TYPE C,
       GV_AMD       TYPE C,
       GV_SD        TYPE C,
       GV_MD        TYPE C,
       GV_PRES      TYPE C,
       GV_NAMEMNG   TYPE C LENGTH 70,
       GV_DATEMNG   TYPE C LENGTH 10,
       GV_NAMEGM    TYPE C LENGTH 70,
       GV_NAMEAGM   TYPE C LENGTH 70,
       GV_DATEGM    TYPE C LENGTH 10,
       GV_DATEAGM   TYPE C LENGTH 10,
       GV_NAMEAMD   TYPE C LENGTH 70,
       GV_DATEAMD   TYPE C LENGTH 10,
       GV_NAMESD    TYPE C LENGTH 70,
       GV_DATESD    TYPE C LENGTH 10,
       GV_NAMEMD    TYPE C LENGTH 70,
       GV_DATEMD    TYPE C LENGTH 10,
       GV_NAMEPRES  TYPE C LENGTH 70,
       GV_DATEPRES  TYPE C LENGTH 10,
       GV_NAMEREQ   TYPE C LENGTH 70,
       GV_DATEREQ   TYPE C LENGTH 10,
       GV_NAMEDEPT  TYPE C LENGTH 70,
       GV_DATEDEPT  TYPE C LENGTH 10,
       GV_DATUM     TYPE SY-DATUM,
       GV_APPROVE1  TYPE C LENGTH 20,
       GV_APPROVE2  TYPE C LENGTH 20,
       GV_APPROVE3  TYPE C LENGTH 20,
       GV_APPROVE4  TYPE C LENGTH 20,
       GV_APPROVE5  TYPE C LENGTH 20,
       GV_APPROVE6  TYPE C LENGTH 20,
       GV_APPROVE7  TYPE C LENGTH 20,
       GV_NAME_APP1 TYPE C LENGTH 70,
       GV_NAME_APP2 TYPE C LENGTH 70,
       GV_NAME_APP3 TYPE C LENGTH 70,
       GV_NAME_APP4 TYPE C LENGTH 70,
       GV_NAME_APP5 TYPE C LENGTH 70,
       GV_NAME_APP6 TYPE C LENGTH 70,
       GV_NAME_APP7 TYPE C LENGTH 70,
       GV_DATE_APP1 TYPE C LENGTH 10,
       GV_DATE_APP2 TYPE C LENGTH 10,
       GV_DATE_APP3 TYPE C LENGTH 10,
       GV_DATE_APP4 TYPE C LENGTH 10,
       GV_DATE_APP5 TYPE C LENGTH 10,
       GV_DATE_APP6 TYPE C LENGTH 10,
       GV_DATE_APP7 TYPE C LENGTH 10,
       GS_EKKO      LIKE LINE OF GI_EKKO.

TYPES TTYP_EKPO LIKE GI_EKPO[].

*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B01 WITH FRAME TITLE TEXT-B01.
  SELECT-OPTIONS:     S_EBELN    FOR  EKPO-EBELN.  "PO DOC.NUMBER
  SELECT-OPTIONS:     P_PRE  FOR EKKO-ERNAM. "SALE
  PARAMETERS          P_WERKS    LIKE EKPO-WERKS DEFAULT '1000' OBLIGATORY. "Plant
SELECTION-SCREEN END OF BLOCK B01.

SELECTION-SCREEN BEGIN OF BLOCK B02 WITH FRAME TITLE TEXT-B02.
  PARAMETERS: P_VATX TYPE CHAR01 AS CHECKBOX USER-COMMAND U01,
              P_VAT(3)   TYPE P DECIMALS 2 DEFAULT '0.07' MODIF ID M01.

SELECTION-SCREEN END OF BLOCK B02.
*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.

  SELECT
    PARAM_EXT,
    VALUE_LOW
  FROM ZSDSCAC001
  INTO TABLE @DATA(LT_SALES_GRP)
  WHERE REPID = 'ZSDSMMR0050'.

  LOOP AT LT_SALES_GRP INTO DATA(LS_SALES_GRP).

    L_VALUE-KEY = LS_SALES_GRP-PARAM_EXT.
    L_VALUE-TEXT = LS_SALES_GRP-VALUE_LOW.
    APPEND L_VALUE TO LI_LIST.

  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = 'P_SALE'
      VALUES          = LI_LIST
    EXCEPTIONS
      ID_ILLEGAL_NAME = 1
      OTHERS          = 2.
*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 EQ 'M01'.
      IF P_VATX = ABAP_TRUE.
        SCREEN-ACTIVE     = 1.
      ELSE.
        SCREEN-ACTIVE     = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
*AT SELECTION-SCREEN ON HELP-REQUEST FOR <field>
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR <field>
*AT SELECTION-SCREEN ON P_VATX.
*AT SELECTION-SCREEN.

*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
*-> read data
*----------------------------------------------------
  GI_DATA-PREPARE = P_PRE.
  GI_DATA-VAT = P_VAT.
  SELECT
   EKPO~EBELN
   EKPO~LOEKZ
   EKPO~EBELP
   EKPO~BANFN
   EKPO~MENGE
   EKPO~MEINS
   EKPO~NETWR
   EKPO~WERKS
   EKPO~KO_PRCTR
   EKPO~MATNR
   EKKO~LIFNR
   INTO TABLE GI_EKPO
   FROM EKKO INNER JOIN EKPO ON ( EKKO~EBELN EQ EKPO~EBELN )
   WHERE EKKO~EBELN IN S_EBELN
   AND EKPO~LOEKZ NE 'L'.

  IF P_VATX <> ABAP_TRUE.
    READ TABLE GI_EKPO INTO DATA(LS_EKPO) INDEX 1.
    IF SY-SUBRC = 0.
      SELECT SINGLE
        J_1TPBUPL
        FROM FITHA_PBUPL_K_T
        INTO @DATA(LV_BCODE)
        WHERE LIFNR = @LS_EKPO-LIFNR
          AND SPRAS = 'E'.
      IF sy-subrc = 0.
        IF LV_BCODE = 'VAT'.
          P_VAT = '0.07'.
        ELSE.
          P_VAT = '0.00'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  SELECT
  EBELN
  EBELP
  VBELN
  AUFNR
  PRCTR
  PS_PSP_PNR
  INTO TABLE GI_EKKN
  FROM EKKN
  WHERE EBELN IN S_EBELN.

  SELECT
    EBELN
    BEDAT
    LIFNR
    ERNAM
    WAERS
  INTO TABLE GI_EKKO
  FROM EKKO
  WHERE EBELN IN S_EBELN.

  IF SY-SUBRC = 0.
    SORT GI_EKKO[] BY EBELN.
  ENDIF.

  SELECT
  A~EBELN
  A~BEDAT
  A~LIFNR
  INTO TABLE GI_EKKOV
  FROM EKKO AS A INNER JOIN LFB1 AS B ON ( A~LIFNR = B~LIFNR AND
                                           B~BUKRS = '1000' )
  WHERE A~EBELN IN S_EBELN.

  SORT GI_EKPO[] BY BANFN.
  LOOP AT GI_EKPO.
    CLEAR: L_AUFKPRC.

    READ TABLE GI_EKKN WITH KEY EBELN = GI_EKPO-EBELN
                                EBELP = GI_EKPO-EBELP.
    READ TABLE GI_EKKO WITH KEY EBELN = GI_EKPO-EBELN.

    CLEAR: GV_PLAN_COST.
    SELECT SINGLE STPRS
      INTO GV_PLAN_COST
      FROM MBEW
      WHERE MATNR = GI_EKPO-MATNR.


    G_SUMPR = GI_EKPO-NETWR.
    G_SUMQTY = GI_EKPO-MENGE.
    G_NO = G_NO + 1.
    GI_DATA-PLAN_COST = GV_PLAN_COST.

    MOVE: GI_EKPO-EBELN TO GI_DATA-EBELN,
    GI_EKPO-LOEKZ TO GI_DATA-LOEKZ,
    GI_EKPO-EBELP TO GI_DATA-EBELP,
    GI_EKPO-BANFN TO GI_DATA-BANFN,
    GI_EKPO-MENGE TO GI_DATA-MENGE,
    GI_EKPO-MEINS TO GI_DATA-MEINS,
    G_SUMPR TO GI_DATA-NETWR,
    GI_EKPO-WERKS TO GI_DATA-WERKS,
    GI_EKKN-VBELN TO GI_DATA-VBELN,
    GI_EKKO-BEDAT TO GI_DATA-BEDAT,
    GI_EKKO-LIFNR TO GI_DATA-LIFNR,
    G_SUMQTY TO GI_DATA-MENGE,
    G_NO TO GI_DATA-GNO,
    GI_EKKN-AUFNR TO GI_DATA-AUFNR,
    GI_EKKN-PS_PSP_PNR TO GI_DATA-PS_PSP_PNR.

    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        INPUT  = GI_DATA-PS_PSP_PNR
      IMPORTING
        OUTPUT = GI_DATA-WBS_TEXT.

    COLLECT GI_DATA.
    LV_PRNO = GI_EKPO-BANFN.
    LV_PONO = GI_EKPO-EBELN.
  ENDLOOP.

  SORT GI_DATA BY GNO EBELN BANFN NETWR DESCENDING.

  LOOP AT GI_DATA.

    SELECT SINGLE * FROM VBAK WHERE VBELN = GI_DATA-VBELN.

    IF SY-SUBRC = 0.
      MOVE: VBAK-KUNNR TO GI_DATA-KUNNR,"no. customer
      VBAK-AUART TO GI_DATA-AUART,      "S/O Type
      VBAK-VGBEL TO GI_DATA-VGBEL,      "S/O for Ref.Doc to S/O Repairing Fillup/not uses because get job no. at S/O repair in/out in place of Fill up 310809
      VBAK-VGBEL TO THEAD-TDNAME.       "S/O For Ref.to Job no. text /change because get job no. at S/O repair in/out in place of Fill up 310809


      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          ID                      = 'Z009'
          LANGUAGE                = 'E'
          NAME                    = THEAD-TDNAME
          OBJECT                  = 'VBBK'
        TABLES
          LINES                   = TEXT
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
        LOOP AT TEXT.
          MOVE TEXT-TDLINE TO GI_DATA-TEXT.

        ENDLOOP.

      ENDIF.

      MODIFY GI_DATA.
    ENDIF.

    SELECT SINGLE PPRCTR
    INTO GI_DATA-PPRCTR
    FROM MSEG
    WHERE EBELN EQ GI_DATA-EBELN
      AND EBELP EQ GI_DATA-EBELP.


    CLEAR: GS_CRMS4D_SERV,GS_CRMS4D_PARTNER,GS_IAOM_CRM_AUFK.

    SELECT SINGLE AUFNR OBJECT_ID
      INTO (GS_IAOM_CRM_AUFK-AUFNR,GS_IAOM_CRM_AUFK-OBJECT_ID)
      FROM IAOM_CRM_AUFK
      WHERE AUFNR EQ GI_DATA-AUFNR.

    IF GS_IAOM_CRM_AUFK-OBJECT_ID IS NOT INITIAL.
      SELECT SINGLE OBJECT_ID AC_ASSIGNMENT
      INTO (GS_CRMS4D_SERV-OBJECT_ID,GS_CRMS4D_SERV-AC_ASSIGNMENT)
      FROM CRMS4D_SERV_H
      WHERE OBJECT_ID EQ GS_IAOM_CRM_AUFK-OBJECT_ID.
      IF GS_CRMS4D_SERV-OBJECT_ID IS NOT INITIAL.
        CLEAR: GS_VBFA.
        GI_DATA-JOBNO = GS_CRMS4D_SERV-OBJECT_ID.

        SELECT SINGLE OBJECT_ID PARTNER_FCT ADDR_NR PARTNER_ID
          INTO (GS_CRMS4D_PARTNER-OBJECT_ID,GS_CRMS4D_PARTNER-PARTNER_FCT,GS_CRMS4D_PARTNER-ADDR_NR,GS_CRMS4D_PARTNER-PARTNER_ID)
          FROM CRMS4D_PARTNER
          WHERE OBJECT_ID = GS_CRMS4D_SERV-OBJECT_ID
          AND PARTNER_FCT = '00000003'.


        SELECT SINGLE VBELV VBTYP_V VBELN VBTYP_N ERDAT
          INTO (GS_VBFA-VBELV,GS_VBFA-VBTYP_V,GS_VBFA-VBELN,GS_VBFA-VBTYP_N,GS_VBFA-ERDAT)
          FROM VBFA
          WHERE VBELV = GI_DATA-JOBNO
          AND VBTYP_V = 'CSVO'
          AND VBTYP_N = 'EBDR'.

        IF GS_VBFA-VBELN IS NOT INITIAL.
          GV_SV_COF = GS_VBFA-VBELN.
          CLEAR: GS_VBFA.
          SELECT SINGLE VBELV VBTYP_V VBELN VBTYP_N ERDAT
          INTO (GS_VBFA-VBELV,GS_VBFA-VBTYP_V,GS_VBFA-VBELN,GS_VBFA-VBTYP_N,GS_VBFA-ERDAT)
          FROM VBFA
          WHERE VBELV = GV_SV_COF
          AND VBTYP_V = 'EBDR'
          AND VBTYP_N = 'M'.

          GI_DATA-INV  = GS_VBFA-VBELN.
          GI_DATA-INV_DATE  = GS_VBFA-ERDAT.

        ENDIF.


      ENDIF.
    ENDIF.

    SELECT SINGLE * FROM VBPA WHERE VBELN = GI_DATA-VBELN.
    SELECT SINGLE * FROM TVAKT WHERE AUART = GI_DATA-AUART AND SPRAS ='E'. "S/O Doc type for link with Description Doc type

    IF SY-SUBRC = 0.
      MOVE:VBPA-ADRNR TO GI_DATA-ADRNR,
      TVAKT-BEZEI TO GI_DATA-BEZEI,
      KNA1-NAME1 TO GI_DATA-CUSNAME_SVO.
      MODIFY GI_DATA.
    ENDIF.

    SELECT SINGLE * FROM LFA1 WHERE LIFNR = GI_DATA-LIFNR.
    IF SY-SUBRC = 0.
      CONCATENATE LFA1-NAME1 LFA1-NAME2 INTO GI_DATA-NAME2  SEPARATED BY SPACE.  "Add by Wantanee C1-20110808
      MODIFY GI_DATA.
    ENDIF.

    SELECT SINGLE * FROM KNA1 WHERE KUNNR = GS_CRMS4D_PARTNER-PARTNER_ID."nutchanart 211209
    IF SY-SUBRC = 0.

      MOVE: KNA1-NAME1 TO GI_DATA-CUSNAME_SVO.
      MODIFY GI_DATA.
    ENDIF.

    SELECT SINGLE * FROM ADRC WHERE ADDRNUMBER = GI_DATA-ADRNR OR ADDRNUMBER = GS_CRMS4D_PARTNER-ADDR_NR AND NATION =''.
    IF SY-SUBRC = 0.

      MOVE:ADRC-NAME1 TO GI_DATA-NAME1.
      MODIFY GI_DATA.
    ELSE.
      MOVE:GI_DATA-CUSNAME_SVO TO GI_DATA-NAME1.
      MODIFY GI_DATA.
    ENDIF.

    SELECT SINGLE * FROM ZSDSMMT015 WHERE VKBUR = GI_DATA-VKBUR AND VKGRP = GI_DATA-VKGRP.
    IF SY-SUBRC = 0 AND GI_DATA-MAUFNR NE''.

      MOVE: ZSDSMMT015-HEADER_JOB TO GI_DATA-HEADER_JOB.

      MODIFY GI_DATA.
    ENDIF.

    READ TABLE GI_EKKO INTO GS_EKKO
                       WITH KEY EBELN = GI_DATA-EBELN
                       BINARY SEARCH.
    IF SY-SUBRC = 0.
      GS_HEADER-ERNAM = GS_EKKO-ERNAM.
      GS_HEADER-WAERS = GS_EKKO-WAERS.
    ENDIF.
    GS_HEADER-EBELN  = GI_DATA-EBELN.
    GS_HEADER-VENDOR = GI_DATA-LIFNR.
    GS_HEADER-NAME   = GI_DATA-NAME2.
    GS_HEADER-SALE   = GI_DATA-P_SALE.
    GS_HEADER-PO     = GI_DATA-EBELN.
    GS_HEADER-DATE1  = GI_DATA-BEDAT.
    APPEND GS_HEADER TO GT_HEADER.
    CLEAR GS_HEADER.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM GT_HEADER.

  SELECT A~MBLNR A~MJAHR A~ZEILE A~BWART A~AUFNR A~MATNR B~MAKTX A~DMBTR
         A~SJAHR A~SMBLN A~SMBLP A~PPRCTR A~EBELN A~EBELP
    INTO TABLE GT_MSEG
    FROM MSEG AS A INNER JOIN MAKT AS B ON ( A~MATNR = B~MATNR )
    FOR ALL ENTRIES IN GI_DATA
    WHERE A~AUFNR EQ GI_DATA-AUFNR AND
          A~EBELN EQ GI_DATA-EBELN AND
          A~KZBEW EQ 'B' AND
          B~SPRAS EQ 'E'.

  LOOP AT GT_MSEG INTO WA_MSEG WHERE SMBLN NE SPACE AND
                                     SMBLP NE SPACE.


    DELETE GT_MSEG WHERE MBLNR = WA_MSEG-SMBLN AND
                         MJAHR = WA_MSEG-SJAHR AND
                         ZEILE = WA_MSEG-SMBLP.

  ENDLOOP.

  DELETE GT_MSEG WHERE SMBLN NE SPACE AND SMBLP NE SPACE.
  SORT GT_MSEG BY AUFNR.

  LOOP AT GT_MSEG INTO WA_MSEG.
    GS_HEADER-MATD   = WA_MSEG-MBLNR.
    MODIFY GT_HEADER FROM GS_HEADER TRANSPORTING MATD
                                    WHERE EBELN = WA_MSEG-EBELN.
  ENDLOOP.

  CHECK GT_MSEG[] IS NOT INITIAL.
  SELECT DISTINCT A~VBELN A~FKDAT B~AUFNR INTO TABLE GT_VBRP
    FROM VBRK AS A INNER JOIN VBRP AS B ON ( A~VBELN = B~VBELN )
    FOR ALL ENTRIES IN GT_MSEG
    WHERE B~AUFNR EQ GT_MSEG-AUFNR.

  SORT GT_HEADER BY EBELN.
  DELETE ADJACENT DUPLICATES FROM GT_HEADER COMPARING EBELN.
*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
*-> process data
*  PERFORM f_get_cost.
  PERFORM F_CALL_FORM.
*-----------------------------------------------------------------------
* T O P - O F – P A G E
*-----------------------------------------------------------------------
*TOP-OF-PAGE.

*-----------------------------------------------------------------------
* AT LINE-SELECTION
*-----------------------------------------------------------------------
*AT LINE-SELECTION.

**-----------------------------------------------------------------------
** F O R M S
**-----------------------------------------------------------------------
**&---------------------------------------------------------------------*
**&      Form  F_GET_COST
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM f_get_cost .
*  DATA : BEGIN OF ls_afko,
*           aufnr TYPE afko-aufnr,
*           rsnum TYPE afko-rsnum,
*         END OF ls_afko.
*  DATA lt_afko LIKE TABLE OF ls_afko.
*
*  IF gi_data[] IS NOT INITIAL.
*
*    SELECT aufnr
*           rsnum
*      FROM afko
*      INTO TABLE lt_afko
*      FOR ALL ENTRIES IN gi_data
*      WHERE aufnr EQ gi_data-aufnr.
*
*    IF lt_afko IS NOT INITIAL.
*      SELECT rsnum
*             matnr
*             posnr
*             bdmng
*             meins
*             gpreis
*             aufnr
*        FROM resb
*        INTO TABLE gt_resb
*        FOR ALL ENTRIES IN lt_afko
*        WHERE rsnum EQ lt_afko-rsnum.
*
**      LOOP AT gt_resb INTO gs_resb.
**        gs_resb_group-aufnr  = gs_resb-aufnr.
**        gs_resb_group-gpreis = gs_resb-gpreis * gs_resb-bdmng.
**        COLLECT gs_resb_group INTO gt_resb_group.
**      ENDLOOP.
*
*      LOOP AT gt_mseg.
*        gs_resb_group-aufnr  = gt_mseg-aufnr.
*        gs_resb_group-gpreis = gt_mseg-dmbtr.
*        COLLECT gs_resb_group INTO gt_resb_group.
*      ENDLOOP.
*
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                    " F_GET_COST
*&---------------------------------------------------------------------*
*&      Form  send_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_DATA_HEADER  text
*      -->PV_DATA_DETAIL  text
*----------------------------------------------------------------------*
FORM F_SEND_DATA TABLES FT_DATA
                        FT_RESB
                        FT_MSEG
                        FT_HEADER
                        FT_RESB_GROUP
               CHANGING FV_PREPARE.

  FT_DATA[]          = GI_DATA[].
  FT_RESB[]          = GT_RESB[].
  FT_MSEG[]          = GT_MSEG[].
  FT_HEADER[]        = GT_HEADER_SMART[].
  FT_RESB_GROUP[]    = GT_RESB_GROUP[].
  FV_PREPARE         = P_PRE.

ENDFORM.                    "send_data
*&---------------------------------------------------------------------*
*&      Form  F_CALL_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CALL_FORM .
  DATA LC_SMART_FORMS TYPE STRING VALUE 'ZSDSMM002'.

  DATA: FM_NAME     TYPE RS38L_FNAM,
        LV_FORMNAME TYPE TDSFNAME.

  DATA : LV_TOTAL_LINE TYPE I,
         COUNT         TYPE SSFCTRLOP.

  DATA : LS_HEADER TYPE GY_HEADER.

  DATA : LV_TABIX TYPE SY-TABIX,
         LV_LINE  TYPE I.

  LV_FORMNAME = LC_SMART_FORMS.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = LV_FORMNAME
    IMPORTING
      FM_NAME            = FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT GT_HEADER INTO LS_HEADER.
    ADD 1 TO LV_TOTAL_LINE.
    CLEAR LS_HEADER.
  ENDLOOP.

  LOOP AT GT_HEADER INTO LS_HEADER.
    LV_TABIX = SY-TABIX.
    CLEAR GT_HEADER_SMART[].
    APPEND LS_HEADER TO GT_HEADER_SMART.
    PERFORM F_CHECK_AMOUNT_LOA  USING LS_HEADER-EBELN
                                      LS_HEADER-ERNAM
                                      LS_HEADER-WAERS
                                      GI_EKPO[].
    IF LV_TOTAL_LINE GT 1.
      IF     LV_TABIX = 1.                "FISRT CALL
        COUNT-NO_OPEN   = SPACE .
        COUNT-NO_CLOSE  = 'X' .
      ELSEIF LV_TABIX   = LV_TOTAL_LINE.  "LAST CALL
        COUNT-NO_OPEN   = 'X' .
        COUNT-NO_CLOSE  = SPACE .
      ELSE.                               "OTHER CALLS
        COUNT-NO_OPEN   = 'X' .
        COUNT-NO_CLOSE  = 'X' .
      ENDIF.
    ENDIF.

    CALL FUNCTION FM_NAME
      EXPORTING
        CONTROL_PARAMETERS = COUNT
        USER_SETTINGS      = 'X'
        IV_NAMEREQ         = GV_NAMEREQ  " Request user
        IV_DATEREQ         = GV_DATEREQ  " Request date
        IV_APPROVE1        = GV_APPROVE1
        IV_APPROVE2        = GV_APPROVE2
        IV_APPROVE3        = GV_APPROVE3
        IV_APPROVE4        = GV_APPROVE4
        IV_APPROVE5        = GV_APPROVE5
        IV_APPROVE6        = GV_APPROVE6
        IV_APPROVE7        = GV_APPROVE7
        IV_NAME_APP1       = GV_NAME_APP1
        IV_NAME_APP2       = GV_NAME_APP2
        IV_NAME_APP3       = GV_NAME_APP3
        IV_NAME_APP4       = GV_NAME_APP4
        IV_NAME_APP5       = GV_NAME_APP5
        IV_NAME_APP6       = GV_NAME_APP6
        IV_NAME_APP7       = GV_NAME_APP7
        IV_DATE_APP1       = GV_DATE_APP1
        IV_DATE_APP2       = GV_DATE_APP2
        IV_DATE_APP3       = GV_DATE_APP3
        IV_DATE_APP4       = GV_DATE_APP4
        IV_DATE_APP5       = GV_DATE_APP5
        IV_DATE_APP6       = GV_DATE_APP6
        IV_DATE_APP7       = GV_DATE_APP7
      EXCEPTIONS
        FORMATTING_ERROR   = 1
        INTERNAL_ERROR     = 2
        SEND_ERROR         = 3
        USER_CANCELED      = 4
        OTHERS             = 5.

  ENDLOOP.

ENDFORM.                    " F_CALL_FORM
*&---------------------------------------------------------------------*
*&      Form  F_GET_EXCHANGE_RATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_EXCHANGE_RATE USING UV_DATE  TYPE DATUM
                               UV_WAERS TYPE EKKO-WAERS
                      CHANGING CV_EXCHANGE TYPE UKURSP.

  DATA : LV_RATE_TYPE  TYPE BAPI1093_1-RATE_TYPE,
         LV_FROM_CURR  TYPE BAPI1093_1-FROM_CURR,
         LV_TO_CURRNCY TYPE	BAPI1093_1-TO_CURRNCY,
         LV_DATE       TYPE	BAPI1093_2-TRANS_DATE.

  DATA : LS_EXCH_RATE	TYPE BAPI1093_0,
         LS_RETURN    TYPE  BAPIRET1.

  CONSTANTS : BEGIN OF LC_EXCHANGE,
                RATE_TYPE TYPE C LENGTH 1 VALUE 'M',
                CURRNCY   TYPE C LENGTH 3 VALUE 'THB',
              END OF LC_EXCHANGE.

  LV_RATE_TYPE  = LC_EXCHANGE-RATE_TYPE.
  LV_FROM_CURR  = UV_WAERS.
  LV_TO_CURRNCY = LC_EXCHANGE-CURRNCY.

  PERFORM F_FIRST_DATE USING UV_DATE
                    CHANGING LV_DATE.

  CALL FUNCTION 'BAPI_EXCHANGERATE_GETDETAIL'
    EXPORTING
      RATE_TYPE  = LV_RATE_TYPE
      FROM_CURR  = LV_FROM_CURR
      TO_CURRNCY = LV_TO_CURRNCY
      DATE       = LV_DATE
    IMPORTING
      EXCH_RATE  = LS_EXCH_RATE
      RETURN     = LS_RETURN.

  CV_EXCHANGE = LS_EXCH_RATE-EXCH_RATE.

ENDFORM.                    " F_GET_EXCHANGE_RATE
*&---------------------------------------------------------------------*
*&      Form  F_FIRST_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_DATE  text
*----------------------------------------------------------------------*
FORM F_FIRST_DATE USING UV_DATE_IN TYPE DATUM
               CHANGING CV_DATE    TYPE DATUM.

  DATA : LV_DATE             TYPE DATUM,
         LV_MONTH_BEGIN_DATE TYPE DATUM,
         LV_MONTH_END_DATE   TYPE DATUM.

  LV_DATE = UV_DATE_IN.

  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING
      IV_DATE             = LV_DATE
    IMPORTING
      EV_MONTH_BEGIN_DATE = LV_MONTH_BEGIN_DATE
      EV_MONTH_END_DATE   = LV_MONTH_END_DATE.

  CV_DATE = LV_MONTH_BEGIN_DATE.

ENDFORM.                    " F_FIRST_DATE

*BOI CH05
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_AMOUNT_LOA
*&---------------------------------------------------------------------*
*       Routine for check amount LOA (Refer logic from  ZFM06PE02)
*----------------------------------------------------------------------*
FORM F_CHECK_AMOUNT_LOA USING UV_EBELN TYPE EKKO-EBELN
                              UV_UNAME TYPE EKKO-ERNAM
                              UV_WAERS TYPE EKKO-WAERS
                              UT_EKPO  TYPE TTYP_EKPO.

  "Clear all related LOA variable
  CLEAR:
    GV_MNG,
    GV_GM,
    GV_AMD,
    GV_SD,
    GV_MD,
    GV_PRES,
    GV_NAMEMNG,
    GV_DATEMNG,
    GV_NAMEGM,
    GV_DATEGM,
    GV_NAMEAMD,
    GV_DATEAMD,
    GV_NAMESD,
    GV_DATESD,
    GV_NAMEMD,
    GV_DATEMD,
    GV_NAMEPRES,
    GV_DATEPRES,
    GV_NAMEREQ,
    GV_DATEREQ,
    GV_DATUM,
    GV_APPROVE1,
    GV_APPROVE2,
    GV_APPROVE3,
    GV_APPROVE4,
    GV_NAMEDEPT,
    GV_DATEDEPT,
    GV_APPROVE1,
    GV_APPROVE2,
    GV_APPROVE3,
    GV_APPROVE4,
    GV_APPROVE5,
    GV_APPROVE6,
    GV_APPROVE7,
    GV_NAME_APP1,
    GV_NAME_APP2,
    GV_NAME_APP3,
    GV_NAME_APP4,
    GV_NAME_APP5,
    GV_NAME_APP6,
    GV_NAME_APP7,
    GV_DATE_APP1,
    GV_DATE_APP2,
    GV_DATE_APP3,
    GV_DATE_APP4,
    GV_DATE_APP5,
    GV_DATE_APP6,
    GV_DATE_APP7.

  DATA: LT_EKPO      TYPE TTYP_EKPO,
        LS_EKPO      LIKE LINE OF LT_EKPO,
        LV_EXCH_RATE TYPE UKURSP,
        LV_AMOUNT    TYPE EKPO-NETWR.
  DATA: POSTION_COUNT TYPE SY-TABIX.


  LT_EKPO[] = UT_EKPO[].
  DELETE LT_EKPO[] WHERE EBELN <> UV_EBELN.



*  IF uv_waers NE 'THB'.
*    PERFORM f_get_exchange_rate USING sy-datum
*                                      uv_waers
*                             CHANGING lv_exch_rate.
*
*    lv_exrate = lv_exch_rate / 1000.
*  ENDIF.

  "Summarize Net amount
  CLEAR : LV_AMOUNT.
  LOOP AT LT_EKPO INTO LS_EKPO.
    IF UV_WAERS NE 'THB'.
*      ls_ekpo-netwr = ( ( ls_ekpo-netwr / 100 ) * lv_exrate ).
      LS_EKPO-NETWR = ( ( LS_EKPO-NETWR / 100 ) ).
    ENDIF.

    ADD LS_EKPO-NETWR TO LV_AMOUNT.
  ENDLOOP.


  SELECT RUNNG
         EBELN
         STATU
         POSIT
         FLAGD
         REMAK
         ERNAM
         ERDAT
         ERZET
    FROM ZSDSMMT002
    INTO TABLE GT_ZSDSMMT002
    WHERE EBELN EQ UV_EBELN
      AND FLAGD NE 'X'
      AND STATU NE 'WAI'.
  IF SY-SUBRC = 0.
    SORT GT_ZSDSMMT002 BY EBELN RUNNG.
  ENDIF.


*  gv_namereq = lv_ernam1.

  READ TABLE GT_ZSDSMMT002 INTO GS_TMP
  WITH KEY STATU = 'COM'.
  IF SY-SUBRC = 0.
    POSTION_COUNT = 0.
    LOOP AT GT_ZSDSMMT002 INTO GS_ZSDSMMT002.
      POSTION_COUNT = SY-TABIX.

      IF GS_ZSDSMMT002-POSIT < 7 AND GS_ZSDSMMT002-STATU <> 'COM'.
        GV_NAMEREQ = GS_ZSDSMMT002-ERNAM.
        GV_DATEREQ = GS_ZSDSMMT002-ERDAT.
      ENDIF.

      IF POSTION_COUNT EQ 2.
        PERFORM F_CHECK_APPROVE_LOA USING GS_ZSDSMMT002 CHANGING GV_APPROVE1
                                                                 GV_NAME_APP1
                                                                 GV_DATE_APP1.

      ELSEIF POSTION_COUNT EQ 3.
        PERFORM F_CHECK_APPROVE_LOA USING GS_ZSDSMMT002 CHANGING GV_APPROVE2
                                                                 GV_NAME_APP2
                                                                 GV_DATE_APP2.
      ELSEIF POSTION_COUNT EQ 4.
        PERFORM F_CHECK_APPROVE_LOA USING GS_ZSDSMMT002 CHANGING GV_APPROVE3
                                                                 GV_NAME_APP3
                                                                 GV_DATE_APP3.
      ELSEIF POSTION_COUNT EQ 5.
        PERFORM F_CHECK_APPROVE_LOA USING GS_ZSDSMMT002 CHANGING GV_APPROVE4
                                                                 GV_NAME_APP4
                                                                 GV_DATE_APP4.
      ELSEIF POSTION_COUNT EQ 6.
        PERFORM F_CHECK_APPROVE_LOA USING GS_ZSDSMMT002 CHANGING GV_APPROVE5
                                                                 GV_NAME_APP5
                                                                 GV_DATE_APP5.
      ELSEIF POSTION_COUNT EQ 7.
        PERFORM F_CHECK_APPROVE_LOA USING GS_ZSDSMMT002 CHANGING GV_APPROVE6
                                                                 GV_NAME_APP6
                                                                 GV_DATE_APP6.

      ENDIF.


*        IF gs_ZSDSMMT002-POSIT = 7.
*            gv_approve1 = '(Manager)'.
*            gv_namemng = gs_ZSDSMMT002-ERNAM.
*            gv_datemng = gs_ZSDSMMT002-ERDAT.
*        ENDIF.
*        IF gs_ZSDSMMT002-POSIT = 8.
*            gv_namedept = gs_ZSDSMMT002-ERNAM.
*            gv_datedept = gs_ZSDSMMT002-ERDAT.
*        ENDIF.

*
*        IF ls_ztmm_log_po-posit EQ 'MGR'.
*          gv_namemng  = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'GM'.
*          gv_namegm   = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'AGM'.
*          gv_nameagm  = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'AMD'.
*          gv_nameamd  = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'SD'.
*          gv_namesd   = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'MD'.
*          gv_namemd   = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'PRD'.
*          gv_namepres = ls_ztmm_log_po-adsdr+10(40).
*        ENDIF.
*        gv_datemng  = ls_ztmm_log_po-erdat.
*            gv_dategm   = ls_ztmm_log_po-erdat.
*            gv_dateagm  = ls_ztmm_log_po-erdat.
*            gv_dateamd  = ls_ztmm_log_po-erdat.
*            gv_datesd   = ls_ztmm_log_po-erdat.
*            gv_datemd   = ls_ztmm_log_po-erdat.
*            gv_datepres = ls_ztmm_log_po-erdat.

      CLEAR : GS_ZSDSMMT002.
    ENDLOOP.
  ENDIF.


ENDFORM.                    " F_CHECK_AMOUNT_LOA


*&---------------------------------------------------------------------*
*&      Form  F_CHECK_AMOUNT_LOA
*&---------------------------------------------------------------------*
*       Routine for check amount LOA (Refer logic from  ZFM06PE02)
*----------------------------------------------------------------------*
FORM F_CHECK_APPROVE_LOA USING US_ZSDSMMT002 TYPE GY_ZSDSMMT002
                         CHANGING UV_APP
                                  UV_NAME
                                  UV_DATE.



  IF US_ZSDSMMT002-POSIT = 7.
    UV_APP = '(Sect.Mgr.)'.
    UV_NAME = GS_ZSDSMMT002-ERNAM.
    CONCATENATE  GS_ZSDSMMT002-ERDAT+6(2) GS_ZSDSMMT002-ERDAT+4(2) GS_ZSDSMMT002-ERDAT+0(4)  INTO UV_DATE SEPARATED BY '.'.
*                uv_date = gs_ZSDSMMT002-ERDAT.
  ENDIF.
  IF US_ZSDSMMT002-POSIT = 8.
    UV_APP = '(Dept.Mgr.)'.
    UV_NAME = GS_ZSDSMMT002-ERNAM.
    CONCATENATE  GS_ZSDSMMT002-ERDAT+6(2) GS_ZSDSMMT002-ERDAT+4(2) GS_ZSDSMMT002-ERDAT+0(4)  INTO UV_DATE SEPARATED BY '.'.
*                uv_date = gs_ZSDSMMT002-ERDAT.
  ENDIF.
  IF US_ZSDSMMT002-POSIT = 9.
    UV_APP = '(AGM.)'.
    UV_NAME = GS_ZSDSMMT002-ERNAM.
    CONCATENATE  GS_ZSDSMMT002-ERDAT+6(2) GS_ZSDSMMT002-ERDAT+4(2) GS_ZSDSMMT002-ERDAT+0(4)  INTO UV_DATE SEPARATED BY '.'.
*                uv_date = gs_ZSDSMMT002-ERDAT.
  ENDIF.
  IF US_ZSDSMMT002-POSIT = 10.
    UV_APP = '(GM.)'.
    UV_NAME = GS_ZSDSMMT002-ERNAM.
    CONCATENATE  GS_ZSDSMMT002-ERDAT+6(2) GS_ZSDSMMT002-ERDAT+4(2) GS_ZSDSMMT002-ERDAT+0(4)  INTO UV_DATE SEPARATED BY '.'.
*                uv_date = gs_ZSDSMMT002-ERDAT.
  ENDIF.
  IF US_ZSDSMMT002-POSIT = 12.
    UV_APP = '(AMD.)'.
    UV_NAME = GS_ZSDSMMT002-ERNAM.
    CONCATENATE  GS_ZSDSMMT002-ERDAT+6(2) GS_ZSDSMMT002-ERDAT+4(2) GS_ZSDSMMT002-ERDAT+0(4)  INTO UV_DATE SEPARATED BY '.'.
*                uv_date = gs_ZSDSMMT002-ERDAT.
  ENDIF.
  IF US_ZSDSMMT002-POSIT = 13.
    UV_APP = '(MD)'.
    UV_NAME = GS_ZSDSMMT002-ERNAM.
    CONCATENATE  GS_ZSDSMMT002-ERDAT+6(2) GS_ZSDSMMT002-ERDAT+4(2) GS_ZSDSMMT002-ERDAT+0(4)  INTO UV_DATE SEPARATED BY '.'.
*                uv_date = gs_ZSDSMMT002-ERDAT.
  ENDIF.
  IF US_ZSDSMMT002-POSIT = 14.
    UV_APP = '(PRESIDENT)'.
    UV_NAME = GS_ZSDSMMT002-ERNAM.
    CONCATENATE  GS_ZSDSMMT002-ERDAT+6(2) GS_ZSDSMMT002-ERDAT+4(2) GS_ZSDSMMT002-ERDAT+0(4)  INTO UV_DATE SEPARATED BY '.'.
*                uv_date = gs_ZSDSMMT002-ERDAT.
  ENDIF.


ENDFORM.
