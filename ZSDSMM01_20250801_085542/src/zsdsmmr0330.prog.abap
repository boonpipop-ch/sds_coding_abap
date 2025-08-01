*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0330
*  Creation Date      : 23.05.2024
*  Author             : Jakarin S
*  Add-on ID          : <<Refer WRICEF List)
*  Description        : Program Download Inbound/Outbound Delivery
*  Purpose            :
*  Copied from        : ZMM_DOWNLOAD_WH_EDO
*  Restriction        :
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSMMR0330 MESSAGE-ID ZSDSMM01.
************************************************************************
*      T A B L E S                                                     *
************************************************************************
TABLES: LIKP,
        LIPS,
        T001,
        VBPA,
        ADRC,
        KNA1,
        MAKT,
        SER01,
        OBJK,
        MARA,
        EKET,
        EKKO,
        ZSDSSDC002.

************************************************************************
*      T Y P E S                                                    *
************************************************************************
*Add by Wantanee 20150521 ITR2015-3851
TYPES: BEGIN OF TYP_ZSDSSDC003,
         KUNNR        TYPE ZSDSSDC003-KUNNR,
         VKBUR        TYPE ZSDSSDC003-VKBUR,
         Z_CUST_GROUP TYPE ZSDSSDC003-Z_CUST_GROUP,
       END OF TYP_ZSDSSDC003.

*End Add by Wantanee ITR2015-3851

*Add by Wantanee 20160502
TYPES : BEGIN OF TYP_ZSDSSDC004,
          MATNR TYPE ZSDSSDC004-MATNR,
        END OF TYP_ZSDSSDC004.
*End Add by Wantanee 20160502

TYPES: BEGIN OF TYP_TVLAT,
         LSTEL TYPE TVLAT-LSTEL,
         VTEXT TYPE TVLAT-VTEXT,
       END OF TYP_TVLAT.

TYPES: BEGIN OF TYP_ZSDSSDC005,
         MATNR TYPE ZSDSSDC005-MATNR,
       END OF TYP_ZSDSSDC005.
TYPES: BEGIN OF S_MAT_AT,
         MATNR TYPE VBAP-MATNR, " Material number (BOM Header)
         IDNRK TYPE STPO-IDNRK, " Component
       END OF S_MAT_AT.

TYPES: BEGIN OF TYP_ZSDSSDC006,
         MATNR TYPE ZSDSSDC006-MATNR,
         AUART TYPE ZSDSSDC006-AUART,
       END OF TYP_ZSDSSDC006.
************************************************************************
*      I N T E R N A L     T A B L E S                                 *
************************************************************************
DATA: BEGIN OF GT_ITAB OCCURS 0,
        LFART   LIKE LIKP-LFART,
        VBELN   LIKE LIKP-VBELN,
        LFDAT   LIKE LIKP-LFDAT,
        LGORT   LIKE LIPS-LGORT, "add by Aphai on Sep 4th, 2014

        ERDAT   LIKE LIKP-ERDAT,
        POSNR   LIKE LIPS-POSNR,
        MATNR   LIKE LIPS-MATNR,


        ARKTX   LIKE LIPS-ARKTX,
        LGMNG   LIKE LIPS-LGMNG,
        KUNNR   LIKE LIKP-KUNNR,

        UEPOS   LIKE LIPS-UEPOS,
        LIFNR   LIKE LIKP-LIFNR,
        LSTEL   LIKE LIKP-LSTEL,

        VTWEG   TYPE VBAK-VTWEG,
        VKBUR   TYPE VBAK-VKBUR,
        KNUMV   TYPE VBAK-KNUMV,

        PRODH   TYPE VBAP-PRODH,
        NETWR   TYPE VBAP-NETWR,
        ERSDA   TYPE MARA-ERSDA,

        POSNV   TYPE LIPS-POSNV,  "Add by Wantanee 20141027
        AUFNR   TYPE LIPS-AUFNR,  "Add by Wantanee 20150521

        VGBEL   TYPE LIPS-VGBEL,

        VKGRP   TYPE VBAK-VKGRP, "Sales group
        ZTERM   TYPE VBKD-ZTERM, "Term of payment
        BSTKD   TYPE VBKD-BSTKD, "PO number
        BSTKD_E TYPE VBKD-BSTKD_E, "PO Number item
        MEINS   TYPE LIPS-MEINS, "Base Unit of Measure
        KOWRR   TYPE LIPS-KOWRR, "check bom
      END OF GT_ITAB.
DATA: BEGIN OF GT_IITAB OCCURS 0.
        INCLUDE STRUCTURE GT_ITAB.
DATA: END OF GT_IITAB.

*Internal table
DATA: BEGIN OF GT_WARRANTY OCCURS 0.
        INCLUDE STRUCTURE ZSDSSDC007.
DATA: END OF GT_WARRANTY.

DATA: BEGIN OF GT_KONV OCCURS 0.
        INCLUDE STRUCTURE PRCD_ELEMENTS.
DATA: END OF GT_KONV.

DATA: BEGIN OF GT_INTAB OCCURS 0,
        LFART(1),
        CUST_CODE(3),
        VBELN(10),

        LFDAT(8),
        ERDAT(8),
        POSNR(4),

        MATNR(35),
        ARKTX(40),
        LGMNG(8),

        STATUS(6),
        KUNNR(10),
        CUST_NAME(40),
        DELI_TO(60),

        REMARK(30),
      END OF GT_INTAB.

DATA: BEGIN OF GT_OUTTAB OCCURS 0,
        LFART(1),
        CUST_CODE(3),
        VBELN(10),
        LFDAT(8),
        ERDAT(8),
        POSNR(4),
        MATNR(35),
        ARKTX(40),
        LGMNG(8),
        KUNNR(10),
        CUST_NAME(60),
        DELI_TO(60),
        REMARK(250),
        PICHON(250),
        REMARK_PICHON(1),
        LGORT(10),                                          "T41K912266
        WARRANTY(1),                                        "T41K917643  "Add by Wantanee 20140923
        SO(10),
        REQ_MAP(1),
        REQ_INV(1),
        SHIP_ADDR(220)     TYPE C,
        SHIP_PROVINCE(30),
        POSTCODE(5),
        AM_PM(2),
        LOADING_POINT(20),
        SALESMAN(40),
        DUE_DATE(8),
        PO_NO(35),
        PROJECT(40),
        TERM_OF_PAYMENT(4),
        CONTACT_NAME(35),
        REF_INV(10),
        SALES_DIV(2),
        SALES_OFFICE(20),
        SALES_GROUP(20),
        UOM(5),
        DOC_FLAG(1),
        MHA(1),
        FLAG_BOM(1),
        REFER_BOM(4),
        INSULATION(10),
        SPSOLC(6),                                          "T41K912266

      END OF GT_OUTTAB.

DATA: BEGIN OF GT_OUTTAB3 OCCURS 0,
        LFART(1),
        CUST_CODE(3),
        VBELN(10),

        LFDAT(8),
        ERDAT(8),
        POSNR(4),

        MATNR(35),
        ARKTX(40),
        LGMNG(8),

        KUNNR(10),
        CUST_NAME(40),
        DELI_TO(60),

        REMARK(30),

        PICHON        TYPE CHAR255,
      END OF GT_OUTTAB3.

DATA: BEGIN OF GT_ITAB2 OCCURS 0,
        VBELN LIKE LIKP-VBELN,
        POSNR LIKE LIPS-POSNR,
        MATNR LIKE LIPS-MATNR,
        LIFEX LIKE LIKP-LIFEX,
        VGBEL LIKE LIPS-VGBEL,
        VGPOS LIKE LIPS-VGPOS,
      END OF GT_ITAB2.

DATA: BEGIN OF GT_ITAB3 OCCURS 0,
        LFART LIKE LIKP-LFART,
        VBELN LIKE LIKP-VBELN,
        LFDAT LIKE LIKP-LFDAT,

        ERDAT LIKE LIKP-ERDAT,
        POSNR LIKE LIPS-POSNR,
        MATNR LIKE LIPS-MATNR,

        ARKTX LIKE LIPS-ARKTX,
        LGMNG LIKE LIPS-LGMNG,
        KUNNR LIKE LIKP-KUNNR,

        UEPOS LIKE LIPS-UEPOS,
      END OF GT_ITAB3.

DATA: BEGIN OF GT_SERNR OCCURS 0,
        SERNR LIKE OBJK-SERNR,
      END OF GT_SERNR.

DATA: BEGIN OF GT_INTAB2 OCCURS 0,
        LICHA(15),  "like eket-licha,
        EAN11(18),  "like mara-ean11,
        MATNR(35),  "like lips-matnr,
        SERNR(18),  "like objk-sernr,
        LIFEX(35),  "like likp-lifex,
      END OF GT_INTAB2.

DATA: GW_OBJK TYPE OBJK.

DATA: GT_OBJK LIKE STANDARD TABLE OF GW_OBJK.


DATA: GT_OUTTAB2    LIKE GT_OUTTAB OCCURS 0,
      WA_OUTTAB2    LIKE GT_OUTTAB,
      WA_IITAB      LIKE GT_IITAB,
      WA_IITAB2     LIKE GT_IITAB,
      WA_OUTTAB     LIKE GT_OUTTAB,
      WA_WARRANTY   LIKE GT_WARRANTY,
      WA_KONV       LIKE GT_KONV,
      DT_CONS_LGORT TYPE TABLE OF ZSDSCAC002 WITH HEADER LINE,
      WA_CONS_LGORT TYPE ZSDSCAC002.


DATA: WA_IITAB_TEMP   LIKE GT_IITAB.

DATA: BEGIN OF GT_OUTTAB4 OCCURS 0,
        LFART(1),
        CUST_CODE(3),
        VBELN(10),
        LFDAT(8),
        ERDAT(8),
        POSNR(4),
        MATNR(35),
        ARKTX(40),
        LGMNG(8),
        KUNNR(10),
        CUST_NAME(40),
        DELI_TO(60),
        REMARK(30),
        PICHON           TYPE CHAR255,
        REMARK_PICHON(1),
        LGORT(10),                                          "T41K912266
        WARRANTY(1),                                        "WCH290514
        SPSOLC(6).                                          "T41K912266
DATA: END OF GT_OUTTAB4.


"Add by Wantanee 20151020

DATA: BEGIN OF GT_OUTTAB_HI OCCURS 0,
        LFART(1),
        CUST_CODE(3),
        VBELN(10),

        LFDAT(8),
        ERDAT(8),
        POSNR(4),

        MATNR(35),
        ARKTX(40),
        LGMNG(8),

        KUNNR(10),
        CUST_NAME(40),
        DELI_TO(60),

        REMARK(30),

        PICHON             TYPE CHAR255,

        REMARK_PICHON(1),
        LGORT(10),                                          "T41K912266
        WARRANTY(1),                                        "T41K917643  "Add by Wantanee 20140923
        "Add by wantanee 20151020 ITR2015-4118
        DEST_ADDR(50),
        DEST_SUB_DIS(50),
        DEST_DISTRIC(50),
        DEST_PROVIN(50),
        DEST_POST_CODE(50),
      END OF GT_OUTTAB_HI.
"Add by wantanee 20151020 ITR2015-4118
"End Add by Wantanee 20151020

TYPES: BEGIN OF TYP_ZSDSSDT002,
         VBELN   TYPE ZSDSSDT002-VBELN,
         RUN_ID  TYPE ZSDSSDT002-RUN_ID,
         REQ_INV TYPE ZSDSSDT002-REQ_INV,
         REQ_MAP TYPE ZSDSSDT002-REQ_INV,
         AM_PM   TYPE ZSDSSDT002-AM_PM,
         ERDAT   TYPE ZSDSSDT002-ERDAT,
         ERZET   TYPE ZSDSSDT002-ERZET,
       END OF TYP_ZSDSSDT002.
TYPES: BEGIN OF TYP_DO_NO,
         VBELN TYPE LIKP-VBELN,
       END OF TYP_DO_NO.


TYPES: BEGIN OF TYP_DO_REFER_BOM,
         VBELN TYPE LIPS-VBELN,
         POSNR TYPE LIPS-POSNR,
         LGMNG TYPE LIPS-LGMNG,
         UEPOS TYPE VBAP-UEPOS,

       END OF TYP_DO_REFER_BOM.



*Add by Wantanee 20150521
DATA: GT_CUST_WARRANTY TYPE STANDARD TABLE OF TYP_ZSDSSDC003,
      WA_CUST_WARRANTY TYPE TYP_ZSDSSDC003.

*End Add by Wantanee 20150521
*-T41K908432

DATA: GV_CUST_ADDR(100).
DATA: GV_SHIP_ADDR TYPE CHAR255.
DATA: GV_SHIP_NAME TYPE CHAR255.  "CH33 Add by Wantanee 20210121

DATA: GV_LOADING_POINT TYPE LIKP-LSTEL.  "Add by wantanee 20140807  T41K917437

DATA: GT_ZSDSSDC004 TYPE STANDARD TABLE OF TYP_ZSDSSDC004,
      GW_ZSDSSDC004 TYPE TYP_ZSDSSDC004,
      WA_ZSDSSDC004 TYPE TYP_ZSDSSDC004.

DATA: GT_TVLAT TYPE STANDARD TABLE OF TYP_TVLAT,
      WA_TVLAT TYPE TYP_TVLAT.
DATA: LV_CHK_LOGRT(200) TYPE C.

DATA: GT_ZSDSSDT002 TYPE STANDARD TABLE OF TYP_ZSDSSDT002,
      WA_ZSDSSDT002 TYPE ZSDSSDT002,
      GT_DO_NO      TYPE STANDARD TABLE OF TYP_DO_NO,
      WA_DO_NO      TYPE TYP_DO_NO.
DATA: GT_DO_TEMP TYPE STANDARD TABLE OF TYP_DO_NO,
      WA_DO_TEMP TYPE TYP_DO_NO.

DATA: GT_ZSDSSDC005 TYPE STANDARD TABLE OF TYP_ZSDSSDC005,
      WA_ZSDSSDC005 TYPE TYP_ZSDSSDC005.

DATA: GT_DO_REFER_BOM TYPE STANDARD TABLE OF TYP_DO_REFER_BOM,
      GW_DO_REFER_BOM TYPE TYP_DO_REFER_BOM,
      WA_DO_REFER_BOM TYPE TYP_DO_REFER_BOM.

DATA: GT_ZSDSSDC006 TYPE STANDARD TABLE OF TYP_ZSDSSDC006,
      GW_ZSDSSDC006 TYPE TYP_ZSDSSDC006,
      WA_ZSDSSDC006 TYPE TYP_ZSDSSDC006.

DATA: I_MAT_AT TYPE TABLE OF S_MAT_AT WITH HEADER LINE.

DATA: CHECK_1800_DIT TYPE C.
DATA: CHECK_1600_SMP TYPE C.
DATA: CHECK_1500_JPP TYPE C.  "CH3 Add by Wantanee 20230315
DATA: CHECK_1100_SONY TYPE C.
DATA: FILE_NAME(10) TYPE C.
DATA: TEXT_NAME(100) TYPE C.

*Variable
CONSTANTS: GC_LIFNR(10)   TYPE C VALUE '0000132005'.


************************************************************************
*      S E L E C T I O N     S C R E E N                               *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: S_VBELN  FOR LIKP-VBELN,
                  S_BLDAT  FOR LIKP-BLDAT, "DEFAULT sy-datum
                  S_LGORT  FOR LIPS-LGORT.
SELECTION-SCREEN END OF BLOCK B1.
SELECTION-SCREEN BEGIN OF BLOCK B5 WITH FRAME TITLE TEXT-005.
  PARAMETERS: R_SER RADIOBUTTON GROUP GR3 USER-COMMAND UCOM DEFAULT 'X',
              R_LOC RADIOBUTTON GROUP GR3.
SELECTION-SCREEN END OF BLOCK B5.
SELECTION-SCREEN BEGIN OF BLOCK B6 WITH FRAME TITLE TEXT-005.
  PARAMETERS: R_NEW RADIOBUTTON GROUP GR4 USER-COMMAND UCOM DEFAULT 'X',
              R_UP  RADIOBUTTON GROUP GR4.
SELECTION-SCREEN END OF BLOCK B6.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: P_F1 AS CHECKBOX  USER-COMMAND UCOM. "DEFAULT 'X'
  PARAMETERS: P_FILE1 LIKE RLGRAP-FILENAME
      DEFAULT 'C:\' OBLIGATORY MODIF ID SA1.
  PARAMETERS: P_F2 AS CHECKBOX  USER-COMMAND UCOM. "DEFAULT 'X'
  PARAMETERS: P_FILE2 LIKE RLGRAP-FILENAME DEFAULT 'C:\' OBLIGATORY MODIF ID SA2, "T41K934404 T41K934432
              P_FILE5 LIKE RLGRAP-FILENAME DEFAULT 'C:\' OBLIGATORY MODIF ID SA2, " T41K934404 T41K934432
              P_FILE6 LIKE RLGRAP-FILENAME DEFAULT 'C:\' OBLIGATORY MODIF ID SA2. " CH29 SMP
  PARAMETERS: P_F3 AS CHECKBOX  USER-COMMAND UCOM. "DEFAULT 'X'
  PARAMETERS: P_FILE3 LIKE RLGRAP-FILENAME
      DEFAULT 'C:\' OBLIGATORY MODIF ID SA3.
  PARAMETERS: P_F4 AS CHECKBOX  USER-COMMAND UCOM. "DEFAULT 'X'
  PARAMETERS: P_FILE4 LIKE RLGRAP-FILENAME
      DEFAULT 'C:\' OBLIGATORY MODIF ID SA4.
SELECTION-SCREEN END OF BLOCK B2.

"Add by wantanee 20170817
SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-004.
  PARAMETERS : R_MAP  AS CHECKBOX,
               R_INV  AS CHECKBOX,
               R_AM   AS CHECKBOX,
               R_PM   AS CHECKBOX,
               R_1700 AS  CHECKBOX.  "Add by Wantanee 20200114
SELECTION-SCREEN END OF BLOCK B4.
"End Add by wantanee 20170817

* Edit By Aphai On Sep 8th, 2014
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
  PARAMETERS : R_SO RADIOBUTTON GROUP GR1 DEFAULT 'X',
               R_HI RADIOBUTTON GROUP GR1,
               R_WM RADIOBUTTON GROUP GR1.


SELECTION-SCREEN END OF BLOCK B3.

SELECTION-SCREEN BEGIN OF BLOCK B7 WITH FRAME TITLE TEXT-006.
  PARAMETERS : R_FG RADIOBUTTON GROUP SNY DEFAULT 'X',
               R_SP RADIOBUTTON GROUP SNY.
SELECTION-SCREEN END OF BLOCK B7.
* End Edit By Aphai On Sep 8th, 2014



************************************************************************
*      A T     S E L E C T I O N     S C R E E N                       *
************************************************************************
AT SELECTION-SCREEN OUTPUT.
  PERFORM MODIFY_SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE1.
  PERFORM GET_PATH_NAME USING P_FILE1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE2.
  PERFORM GET_PATH_NAME USING P_FILE2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE3.
  PERFORM GET_PATH_NAME USING P_FILE3.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE4.
  PERFORM GET_PATH_NAME USING P_FILE4.

AT SELECTION-SCREEN.
  PERFORM CHECK_SCREEN_FIELD.

************************************************************************
*      B E G I N     S E L E C T I O N                                 *
************************************************************************
START-OF-SELECTION .

* Get data
  IF P_F1 = 'X'.
    PERFORM GET_DATA_IN.
  ENDIF.

*-T41K908432
  IF P_F2 = 'X' OR P_F4 = 'X'.
    PERFORM GET_DATA_OUT.
  ENDIF.
  PERFORM GET_DATA_CHECK.   "inbound chk file
  PERFORM GET_DATA_RETURN.  "return grade r to grade a

  IF NOT LV_CHK_LOGRT IS INITIAL .
    MESSAGE S001 WITH LV_CHK_LOGRT.
  ELSE.
*     Download Data
    PERFORM DOWNLOAD_DATA.
    IF P_F2 = 'X'.
      PERFORM INSERT_ZTMM_EXPORT_DO.
    ENDIF.
  ENDIF.

************************************************************************
*      FORM GET_DATA _Inbound                                                  *
*----------------------------------------------------------------------*
*      Description: This form is used for get Physical Document Data.  *
************************************************************************
FORM GET_DATA_IN .
  CLEAR: GT_ITAB.

  SELECT A~LFART A~VBELN A~LFDAT A~ERDAT
         B~POSNR B~MATNR B~ARKTX B~LGMNG
         A~KUNNR B~UEPOS A~LIFNR
    INTO CORRESPONDING FIELDS OF TABLE GT_ITAB
    FROM LIKP AS A INNER JOIN LIPS AS B
      ON A~VBELN = B~VBELN
   WHERE A~VBELN IN S_VBELN
     AND A~LFDAT IN S_BLDAT
     AND B~MEINS <> 'SET'
     AND B~LGORT IN S_LGORT. "Chage Storage By Selection-screen By Panida
***     AND b~lgort = '1100'. "SONY Grade A

  LOOP AT GT_ITAB.
*   18* = In bound, 40* = Out bound
    IF GT_ITAB-VBELN(2) = '80'.
      GT_INTAB-LFART = '0'.
      GT_INTAB-CUST_CODE  = 'SDS'.
      GT_INTAB-VBELN      = GT_ITAB-VBELN.

      GT_INTAB-LFDAT      = GT_ITAB-LFDAT.
      GT_INTAB-ERDAT      = GT_ITAB-ERDAT.
      GT_INTAB-POSNR      = GT_ITAB-POSNR+2(4).

      GT_INTAB-MATNR      = GT_ITAB-MATNR.

***Add Status By Panida
      IF GT_ITAB-LIFNR = GC_LIFNR.
        GT_INTAB-STATUS = 'LOCAL'.
      ELSE.
        GT_INTAB-STATUS = 'IMPORT'.
      ENDIF.




      CLEAR: T001.
      SELECT SINGLE *
        FROM MAKT
       WHERE MATNR = GT_ITAB-MATNR
         AND SPRAS = 'E'.

      GT_INTAB-ARKTX      = MAKT-MAKTX.

      WRITE GT_ITAB-LGMNG TO GT_INTAB-LGMNG LEFT-JUSTIFIED DECIMALS 0.

      GT_INTAB-KUNNR      = GT_ITAB-KUNNR.
      SHIFT GT_INTAB-KUNNR LEFT DELETING LEADING '0'.

      CLEAR: KNA1.
      SELECT SINGLE *
        FROM KNA1
       WHERE KUNNR = GT_ITAB-KUNNR.

      CONCATENATE KNA1-NAME1 KNA1-NAME2 KNA1-NAME3
             INTO GT_INTAB-CUST_NAME.

      CONCATENATE KNA1-STRAS KNA1-ORT01
             INTO GV_CUST_ADDR.

      GT_INTAB-DELI_TO = GV_CUST_ADDR(60).

      PERFORM READ_TEXT USING 'ZH10' 'VBBK' GT_ITAB-VBELN
                     CHANGING GT_INTAB-REMARK.

      APPEND GT_INTAB.
***    ELSEIF gt_itab-vbeln(2) = '40'.
***      gt_outtab-lfart = '1'.
***      gt_outtab-cust_code  = 'SDS'.
***      gt_outtab-vbeln      = gt_itab-vbeln.
***
***      gt_outtab-lfdat      = gt_itab-lfdat.
***      gt_outtab-erdat      = gt_itab-erdat.
***      gt_outtab-posnr      = gt_itab-posnr+2(4).
***
***      gt_outtab-matnr      = gt_itab-matnr.
***
***      CLEAR: T001.
***      SELECT SINGLE *
***        FROM makt
***       WHERE matnr = gt_itab-matnr
***         AND spras = 'EN'.
***
***      gt_outtab-arktx      = makt-maktx.
***
***      write gt_itab-lgmng to gt_outtab-lgmng left-justified decimals 0.
***
***      gt_outtab-kunnr      = gt_itab-kunnr.
***      shift gt_outtab-kunnr LEFT DELETING LEADING '0'.
***
***      CLEAR: KNA1.
***      SELECT SINGLE *
***        FROM KNA1
***       WHERE kunnr = gt_itab-kunnr.
***
***      CONCATENATE kna1-name1 kna1-name2 kna1-name3
***             INTO gt_outtab-cust_name.
***
***      CONCATENATE kna1-stras kna1-ort01
***             INTO gv_cust_addr.
***
***      gt_outtab-deli_to = gv_cust_addr(60).
***
***      PERFORM read_text  USING 'Z011' 'VBBK' gt_itab-vbeln
***                     CHANGING gt_outtab-remark.
***
***      PERFORM read_text2 USING 'ZI03' 'VBBP' gt_itab-vbeln gt_itab-uepos
***                     CHANGING gt_outtab-pichon.
***
***      APPEND gt_outtab.
    ENDIF.
  ENDLOOP.

  SORT GT_INTAB  BY LFART CUST_CODE VBELN LFDAT ERDAT POSNR MATNR.
***  SORT gt_outtab by lfart cust_code vbeln lfdat erdat posnr matnr.

ENDFORM.                    " get_data


************************************************************************
*      FORM GET_DATA _Outbound                                                  *
*----------------------------------------------------------------------*
*      Description: This form is used for get Physical Document Data.  *
************************************************************************
FORM  GET_DATA_OUT.

*  Add by Aphai On Sep 8th, 2014
  TYPES : BEGIN OF S_LSTEL,
            VBELN TYPE LIKP-VBELN,
            LSTEL TYPE LIKP-LSTEL,
          END OF S_LSTEL.

  DATA : DT_CONST TYPE TABLE OF ZSDSCAC002 WITH HEADER LINE.

  DATA : TB_LSTEL TYPE TABLE OF S_LSTEL WITH HEADER LINE,
         WA_LSTEL TYPE S_LSTEL,
         F_TO(7)  TYPE C.
  FIELD-SYMBOLS <FS_LSTEL> TYPE S_LSTEL.

  DATA: LV_CHECT_MAT_AT TYPE C.

  DATA: LV_ZBD1T TYPE BSEG-ZBD1T.
  DATA: LV_DUE TYPE SY-DATUM.
  DATA: LV_EZFBDT TYPE BSEG-ZFBDT.
  DATA: LV_INTERNAL_ORDER(40) TYPE C,
        LV_PROJECT(40)        TYPE C.
  DATA: LV_CHECK_TEXT_SITE    TYPE C.  "Add by wantanee 20150722

*  Check Loading point
  SELECT VBELN LSTEL INTO CORRESPONDING FIELDS OF TABLE TB_LSTEL FROM LIKP WHERE VBELN IN S_VBELN.
  ""Add by Wantanee 20150521 ITR2015-3851
  DATA: LV_IO_AUART   TYPE AUFK-AUART,
        LV_CHECK_CUST TYPE C.
  "End Add by Wantanee 20150521 ITR2015-3851
** Text file To so : Sony , hi : Hitachi
*
  DATA : WA_LGORT LIKE S_LGORT,
         LGORT_1  TYPE MARD-LGORT,
         LGORT_2  TYPE MARD-LGORT.
  SELECT * INTO TABLE DT_CONS_LGORT FROM ZSDSCAC002 WHERE REPID EQ SY-REPID AND CONST LIKE 'STOR%'.
  SORT DT_CONS_LGORT[] BY VALUE.

  DATA : LV_KUNNR TYPE KNA1-KUNNR.

  "Add by Wantanee 20160502
  SELECT MATNR
  INTO TABLE GT_ZSDSSDC004
  FROM ZSDSSDC004.
  "End Add by Wantanee 20160502

  "Add by Jakarin 20161221
  DATA : BEGIN OF LS_ZSDSSDC009,
           COND1 TYPE ZSDSSDC009-COND1,
         END OF LS_ZSDSSDC009.
  DATA LT_ZSDSSDC009 LIKE TABLE OF LS_ZSDSSDC009.

  RANGES : LR_KUNNR FOR KNA1-KUNNR.

  SELECT COND1
    FROM ZSDSSDC009
    INTO TABLE LT_ZSDSSDC009
    WHERE COND1 NE SPACE.

  LOOP AT LT_ZSDSSDC009 INTO LS_ZSDSSDC009.
    CLEAR LR_KUNNR.
    LR_KUNNR-SIGN   = 'I'.
    LR_KUNNR-OPTION = 'EQ'.
    LR_KUNNR-LOW    = LS_ZSDSSDC009-COND1.
    APPEND LR_KUNNR.
    CLEAR LS_ZSDSSDC009.
  ENDLOOP.

  IF LR_KUNNR[] IS INITIAL.
    CLEAR LR_KUNNR.
    LR_KUNNR-SIGN   = 'I'.
    LR_KUNNR-OPTION = 'EQ'.
    LR_KUNNR-LOW    = 'NO_CHECK'.
    APPEND LR_KUNNR.
  ENDIF.

  "End add by Jakarin 20161221

  "Add by Wantanee 20180629
  SELECT MATNR
  INTO TABLE GT_ZSDSSDC005
  FROM ZSDSSDC005.

  "End Add by Wantanee 20180629

  SELECT LSTEL VTEXT
  INTO TABLE GT_TVLAT
  FROM TVLAT
  WHERE SPRAS EQ 'E'
    AND VSTEL EQ '1000'.
* Storage that begin with '11' AND 13 that mean to Storage of SONY
* Storage that begin with '15' that mean to Storage of HITACHI
  IF R_SO EQ 'X'.

    LOOP AT DT_CONS_LGORT INTO WA_CONS_LGORT WHERE CONST+5(2) EQ 'SO'. "aphai 6.11.2014
      SPLIT WA_CONS_LGORT-VALUE AT ':' INTO LGORT_1 LGORT_2. "aphai 6.11.2014
      WA_LGORT-LOW = LGORT_1. "aphai 6.11.2014
      WA_LGORT-HIGH = LGORT_2. "aphai 6.11.2014
      WA_LGORT-SIGN = 'I'. "aphai 6.11.2014
      WA_LGORT-OPTION = 'BT'. "aphai 6.11.2014
      APPEND WA_LGORT TO S_LGORT. "aphai 6.11.2014
      CLEAR :WA_CONS_LGORT,WA_LGORT,LGORT_1,LGORT_2. "aphai 6.11.2014
    ENDLOOP.

*    wa_lgort-low = '1100'. "aphai 6.11.2014
*    wa_lgort-high = '1199'. "aphai 6.11.2014
*    wa_lgort-sign = 'I'. "aphai 6.11.2014
*    wa_lgort-option = 'BT'. "aphai 6.11.2014
*    APPEND wa_lgort TO s_lgort. "aphai 6.11.2014
    F_TO = 'SONY'.
  ELSEIF R_WM EQ 'X'.
    LOOP AT DT_CONS_LGORT INTO WA_CONS_LGORT WHERE CONST+5(2) EQ 'SO'. "aphai 6.11.2014
      SPLIT WA_CONS_LGORT-VALUE AT ':' INTO LGORT_1 LGORT_2. "aphai 6.11.2014
      WA_LGORT-LOW = LGORT_1. "aphai 6.11.2014
      WA_LGORT-HIGH = LGORT_2. "aphai 6.11.2014
      WA_LGORT-SIGN = 'I'. "aphai 6.11.2014
      WA_LGORT-OPTION = 'BT'. "aphai 6.11.2014
      APPEND WA_LGORT TO S_LGORT. "aphai 6.11.2014
      CLEAR :WA_CONS_LGORT,WA_LGORT,LGORT_1,LGORT_2. "aphai 6.11.2014
    ENDLOOP.

*    wa_lgort-low = '1100'. "aphai 6.11.2014
*    wa_lgort-high = '1199'. "aphai 6.11.2014
*    wa_lgort-sign = 'I'. "aphai 6.11.2014
*    wa_lgort-option = 'BT'. "aphai 6.11.2014
*    APPEND wa_lgort TO s_lgort. "aphai 6.11.2014
    F_TO = 'WMS'.
  ELSE.
    S_LGORT = SPACE.
    F_TO = 'HITACHI'.
  ENDIF.


*End  Add by Aphai On Sep 8th, 2014

*-T41K908432
  DATA: LV_PICHON(18) TYPE C,
        LV_PICHON1    TYPE VBRP-FKIMG,
        LV_PICHON2    TYPE VBRP-FKIMG,
        PERCENT       TYPE P DECIMALS 2.
*-T41K908432
  CLEAR: GT_IITAB.


  "Add by Wantanaee 20150521 ITR2015-3851
  SELECT KUNNR VKBUR
  INTO TABLE GT_CUST_WARRANTY
  FROM ZSDSSDC003.
  "End by Wantanee ITR2015-3851

  SELECT * FROM ZSDSSDC007 INTO TABLE GT_WARRANTY.          "WCH090514
  SELECT * FROM ZSDSCAC002 INTO TABLE DT_CONST WHERE REPID EQ SY-REPID AND CONST LIKE 'SPM%'. " Add By Aphai On 3.11.2014
  SORT DT_CONST BY VALUE.

  SELECT * FROM ZSDSCAC002 INTO TABLE DT_CONST WHERE REPID EQ SY-REPID AND CONST LIKE 'SPM%'. " Add By Aphai On 3.11.2014
  SORT DT_CONST BY VALUE.

  SELECT MATNR AUART
    INTO TABLE GT_ZSDSSDC006
    FROM ZSDSSDC006.

* IF Generate text file to Sony
* Storage that begin with '11' that mean to Storage of SONY
* Storage that begin with '15' that mean to Storage of HITACHI
  IF R_SO EQ 'X' OR R_WM EQ 'X'. "Generate text File to Sony
    SELECT A~LFART A~VBELN A~LFDAT A~ERDAT B~LGORT
           B~POSNR B~MATNR B~ARKTX B~LGMNG  "b~posnr
           A~KUNNR B~UEPOS A~LSTEL C~VTWEG
           C~VKBUR C~KNUMV D~PRODH D~NETWR
           E~ERSDA
           D~POSNR AS POSNV " b~posnv Aphai 25.12.2014
           B~AUFNR   "Add by Wantanee 20150521 ITR2015-3851
           B~VGBEL   "Add by wantanee 20170817
           C~VKGRP
           G~ZTERM
           G~BSTKD
           G~BSTKD_E
           B~MEINS B~KOWRR
      INTO CORRESPONDING FIELDS OF TABLE GT_IITAB
      FROM LIKP AS A INNER JOIN LIPS AS B ON ( A~VBELN = B~VBELN )
                     INNER JOIN VBAK AS C ON ( C~VBELN = B~VGBEL ) "WCH090514
                     INNER JOIN VBAP AS D ON ( D~VBELN = B~VGBEL AND "WCH090514
                                               D~POSNR = B~VGPOS ) "WCH090514
                     INNER JOIN MARA AS E ON ( B~MATNR = E~MATNR ) "WCH090514
                     INNER JOIN VBKD AS G ON ( G~VBELN = B~VGBEL AND G~POSNR EQ '000000' )
*                     INNER JOIN ZSDSCAC002 AS f ON  f~const = b~lgort "WCH020713
*                     INNER JOIN ZSDSCAC002 AS f ON  f~repid = sy-repid AND f~const = b~lgort "WCH020713 GETCH04112014
     WHERE A~VBELN IN S_VBELN
*     AND a~bldat IN s_bldat
       AND A~LFDAT IN S_BLDAT  "Change field WCH291013
*       AND b~lgort IN s_lgort  "Check storage location
       "AND a~kunag NOT IN lr_kunnr " Check Customer
*       AND ( b~meins <> 'SET' )
*       AND ( b~meins <> 'JOB' ).
      .
  ELSE.
    SELECT A~LFART A~VBELN A~LFDAT A~ERDAT B~LGORT
              B~POSNR B~MATNR B~ARKTX B~LGMNG "b~posnr
              A~KUNNR B~UEPOS A~LSTEL C~VTWEG
              C~VKBUR C~KNUMV D~PRODH D~NETWR
              E~ERSDA
              D~POSNR AS POSNV " b~posnv Aphai 25.12.2014
              B~AUFNR  "Add by Wantanee 20150521 ITR2015-3851
              B~VGBEL   "Add by wantanee 20170817
              C~VKGRP
              G~ZTERM
              G~BSTKD
              G~BSTKD_E
              B~MEINS B~KOWRR
         INTO CORRESPONDING FIELDS OF TABLE GT_IITAB
         FROM LIKP AS A INNER JOIN LIPS AS B ON ( A~VBELN = B~VBELN )
                        INNER JOIN VBAK AS C ON ( C~VBELN = B~VGBEL ) "WCH090514
                        INNER JOIN VBAP AS D ON ( D~VBELN = B~VGBEL AND "WCH090514
                                                  D~POSNR = B~VGPOS ) "WCH090514
                        INNER JOIN MARA AS E ON ( B~MATNR = E~MATNR ) "WCH090514
*                        INNER JOIN ZSDSCAC002 AS f ON ( f~const = b~lgort ) "WCH020713
                         INNER JOIN ZSDSCAC002 AS F ON  ( F~REPID = SY-REPID AND F~CONST = B~LGORT ) "WCH020713 GETCH04112014
                         INNER JOIN VBKD AS G ON ( G~VBELN = B~VGBEL
                                                   AND G~POSNR EQ '000000' )
        WHERE A~VBELN IN S_VBELN
*     AND a~bldat IN s_bldat
          AND A~LFDAT IN S_BLDAT  "Change field WCH291013
          "AND a~kunag NOT IN lr_kunnr " Check Customer
*          AND ( b~meins <> 'SET' )
*          AND ( b~meins <> 'JOB' ).
      .
  ENDIF.
  CHECK GT_IITAB[] IS NOT INITIAL.
  "Add by Wantanee 20190326
  SELECT A~VBELN A~POSNR A~LGMNG
         B~UEPOS
  INTO TABLE GT_DO_REFER_BOM
  FROM LIPS AS A INNER JOIN VBAP AS B
                 ON ( A~VGBEL EQ B~VBELN
                 AND  A~VGPOS EQ B~POSNR )
  FOR ALL ENTRIES IN GT_IITAB
  WHERE A~VBELN EQ GT_IITAB-VBELN.

  "End by Wantanee 20190326

*** lstel = Loading point Z5 : Hitachi,Z1 SONY
** Check Loading point
* Change by Aphai On Sep 9th, 2014
  IF R_HI EQ 'X'.
    SORT TB_LSTEL BY VBELN.
    SORT GT_IITAB[] BY VBELN LGORT.
    DATA TB_LSTEL_TMP TYPE STANDARD TABLE OF S_LSTEL WITH HEADER LINE.
    TB_LSTEL_TMP[] = TB_LSTEL[].
    DELETE TB_LSTEL_TMP WHERE LSTEL EQ 'Z5'. " Remove Delivery Orders that loading point is HITACHI : Z5
*    When Loading point not Hitachi then remove rows that lgort not Hitachi
    LOOP AT TB_LSTEL_TMP INTO WA_LSTEL.
*      If which DO that Loading point no Hitachi (Z5) then delete rows that storage location no Hitachi [1500 - 1599].
*      DELETE gt_iitab WHERE vbeln EQ wa_lstel-vbeln AND lgort LT 1500 OR lgort GT 1599." Aphai 08.01.2016
      DELETE GT_IITAB WHERE VBELN EQ WA_LSTEL-VBELN AND LGORT LT 1400 OR LGORT GT 1599. " Aphai 08.01.2016
    ENDLOOP.
  ENDIF.
* End Change by Aphai On Sep 9th, 2014

  CHECK GT_IITAB[] IS NOT INITIAL.

  SELECT * FROM PRCD_ELEMENTS INTO TABLE GT_KONV FOR ALL ENTRIES IN GT_IITAB
    WHERE KNUMV EQ GT_IITAB-KNUMV AND
          KSCHL EQ 'ZPR0'.

*$-----------------------------------------------------------------*
*    Werathep Ch. to additional storage location to Sony Grade A
*    AND b~lgort IN ('1100','1150'). "SONY Grade A
*$-----------------------------------------------------------------*

  CLEAR: GV_LOADING_POINT.
  DATA: LV_SO_NO   TYPE VBAK-VBELN,
        LV_SO_TYPE TYPE VBAK-AUART,
        PH1        TYPE ZSDSSDC007-PH1,
        PH2        TYPE ZSDSSDC007-PH2,
        PH3        TYPE ZSDSSDC007-PH3.

  DATA: LV_ITEM_TEXT(4)    TYPE C, "Check  char
        LV_VBELN_POSNR(16) TYPE C.

  DATA: LV_SUM_QTY_SPILT TYPE LIPS-LGMNG,
        LV_QTY_1700      TYPE LIPS-LGMNG.

  DATA: LV_SO_AUART TYPE VBAK-AUART.


  DATA: DT_CONS_CLEW TYPE TABLE OF ZSDSCAC002 WITH HEADER LINE,
        WA_CONS_CLEW TYPE ZSDSCAC002.
  DATA: LV_CLEW_MAT TYPE MARA-MATNR.

  DATA: LV_CHK_WARRANTY TYPE C.
  DATA: LV_SO_ITEM(16) TYPE C.
  DATA: LV_TEXT(510) TYPE C.
  DATA: LV_TEXT_RETURN(255) TYPE C.
  DATA: LV_TEXT_INVREMARK(250) TYPE C.
  DATA: LV_MATNR_INSU TYPE ZSDSSDC005-MATNR.

  SELECT * INTO TABLE DT_CONS_CLEW FROM ZSDSCAC002 WHERE REPID EQ SY-REPID AND CONST LIKE 'CLEW%'.

  " 20200116 Revise case check mat SAT new stamp warranty
  "Remove by wantanee 2020015 for case not check mat SAT
  PERFORM GET_BOM_MATERIAL_GROUP_AT TABLES I_MAT_AT .
  "End Remove by wantanee 2020015 for case not check mat SAT
  " 20200116 End Revise case check mat SAT new stamp warranty
  CLEAR: CHECK_1800_DIT.
  CLEAR: CHECK_1600_SMP.
  CLEAR: CHECK_1500_JPP. "CH3 Add by WAntanee 20230315
  CLEAR: CHECK_1100_SONY.
  CLEAR: LV_TEXT_RETURN.
  LOOP AT GT_IITAB.
    CLEAR: LV_CHECK_CUST,GV_SHIP_ADDR,LV_ITEM_TEXT,LV_VBELN_POSNR,LV_CHECT_MAT_AT.
    CLEAR: LV_SO_AUART,LV_CHK_WARRANTY,LV_SO_ITEM.
    CLEAR: GV_SHIP_NAME.
    CLEAR: LV_TEXT,LV_TEXT_INVREMARK,LV_MATNR_INSU.

    PH1   = GT_IITAB-PRODH+0(5).
    PH2   = GT_IITAB-PRODH+5(5).
    PH3   = GT_IITAB-PRODH+10(8).

*   18* = In bound, 40* = Out bound

    IF GT_IITAB-VBELN(2) = '40' OR GT_IITAB-VBELN(2) = '42' OR GT_IITAB-VBELN(2) = '41'.
      IF GT_IITAB-VBELN(2) = '40'.
        GT_OUTTAB-LFART = '1'.
      ELSEIF GT_IITAB-VBELN(2) = '42' OR
             GT_IITAB-VBELN(2) = '41'.
        GT_OUTTAB-LFART = '0'.
      ENDIF.
      GT_OUTTAB-CUST_CODE  = 'SDS'.
      GT_OUTTAB-VBELN      = GT_IITAB-VBELN.

      GT_OUTTAB-LFDAT      = GT_IITAB-LFDAT.
      GT_OUTTAB-ERDAT      = GT_IITAB-ERDAT.
      GT_OUTTAB-POSNR      = GT_IITAB-POSNR+2(4).

      GT_OUTTAB-MATNR      = GT_IITAB-MATNR.

      SELECT SINGLE MATNR
      INTO LV_MATNR_INSU
      FROM ZSDSSDC005
      WHERE MATNR  = GT_IITAB-MATNR.
      IF LV_MATNR_INSU IS NOT INITIAL.
        GT_OUTTAB-INSULATION = 'Insulation'.
      ENDIF.

      GT_OUTTAB-SO        = GT_IITAB-VGBEL. "Sale order
      GT_OUTTAB-FLAG_BOM  = GT_IITAB-KOWRR. "Sale order
      GT_OUTTAB-REFER_BOM = GT_IITAB-UEPOS+2(4). "Sale order

      READ TABLE GT_TVLAT INTO WA_TVLAT WITH KEY LSTEL = GT_IITAB-LSTEL.
      IF SY-SUBRC EQ 0.
        GT_OUTTAB-LOADING_POINT      = WA_TVLAT-VTEXT. "Loading point
      ENDIF.

      IF NOT R_MAP IS INITIAL.
        GT_OUTTAB-REQ_MAP      = 'Y'. "Require Map

      ENDIF.
      IF NOT R_INV IS INITIAL.
        GT_OUTTAB-REQ_INV      = 'Y'. "Require Map
      ENDIF.

      IF NOT R_AM IS INITIAL.
        GT_OUTTAB-AM_PM      = 'AM'. "AM

      ENDIF.
      IF NOT R_PM IS INITIAL.
        GT_OUTTAB-AM_PM      = 'PM'. "AM
      ENDIF.

      CLEAR: T001.
      SELECT SINGLE *
        FROM MAKT
       WHERE MATNR = GT_IITAB-MATNR
         AND SPRAS = 'E'.

      GT_OUTTAB-ARKTX      = MAKT-MAKTX.

      WRITE GT_IITAB-LGMNG TO GT_OUTTAB-LGMNG LEFT-JUSTIFIED DECIMALS 0.

      GT_OUTTAB-KUNNR      = GT_IITAB-KUNNR.
      SHIFT GT_OUTTAB-KUNNR LEFT DELETING LEADING '0'.

      CLEAR: KNA1.
      SELECT SINGLE *
        FROM KNA1
       WHERE KUNNR = GT_IITAB-KUNNR.

*      CONCATENATE kna1-name1 kna1-name2 kna1-name3
*             INTO gt_outtab-cust_name.

      IF GT_IITAB-KUNNR NE 'OT01'.

        SELECT * FROM ADRC WHERE ADDRNUMBER EQ KNA1-ADRNR AND
                                  NATION EQ SPACE.
          IF SY-SUBRC EQ 0.
            CONCATENATE ADRC-NAME1
                        ADRC-NAME2  "Add by wantanee 20150917
                        ADRC-NAME3
                        ADRC-NAME4
                   INTO GT_OUTTAB-CUST_NAME SEPARATED BY SPACE.
          ENDIF.
        ENDSELECT.
      ELSE.
        SELECT * FROM VBPA WHERE VBELN = GT_IITAB-VBELN AND
                                PARVW = 'AG'.
          IF SY-SUBRC EQ 0.
            SELECT * FROM ADRC WHERE ADDRNUMBER EQ VBPA-ADRNR AND
                               NATION EQ SPACE.
              IF SY-SUBRC EQ 0.
                CONCATENATE ADRC-NAME1
                 ADRC-NAME2
                 ADRC-NAME3
                 ADRC-NAME4
                INTO GT_OUTTAB-CUST_NAME SEPARATED BY SPACE.
              ENDIF.
            ENDSELECT.

          ENDIF.
        ENDSELECT.
      ENDIF.

*** lstel = Loading point Z5 : Hitachi,Z1 SONY
*      IF gt_iitab-lstel EQ 'Z5'.
* Change By Aphai On Sep 9th , 2014
      IF R_HI EQ 'X'. " Change By Aphai On Sep 8th, 2014 r_hi EQ 'X' : Generate Text file for Hitachi
        SELECT * FROM VBPA WHERE VBELN = GT_IITAB-VBELN AND
                                 PARVW = 'WE'.
          IF SY-SUBRC EQ 0.
            SELECT * FROM ADRC WHERE ADDRNUMBER EQ VBPA-ADRNR AND
                                     NATION EQ SPACE.
              IF SY-SUBRC EQ 0.
                CONCATENATE ADRC-STREET
                            ADRC-CITY2  "Add by wantanee 20150917
                            ADRC-CITY1
                       INTO GV_CUST_ADDR SEPARATED BY SPACE.
              ENDIF.
            ENDSELECT.
          ENDIF.
        ENDSELECT.
      ELSE.  "Sony
        CONCATENATE KNA1-STRAS KNA1-ORT01
               INTO GV_CUST_ADDR.


        SELECT * FROM VBPA WHERE VBELN = GT_IITAB-VBELN AND
                                 PARVW = 'WE'.
          IF SY-SUBRC EQ 0.
            SELECT * FROM ADRC WHERE ADDRNUMBER EQ VBPA-ADRNR AND
                                     NATION EQ SPACE.
              IF SY-SUBRC EQ 0.
                CONCATENATE ADRC-STREET ADRC-STR_SUPPL1 ADRC-STR_SUPPL2
                            ADRC-STR_SUPPL3 ADRC-LOCATION
                            ADRC-CITY2  "Add by wantanee 20150917
                       INTO GV_SHIP_ADDR SEPARATED BY SPACE.

                CONCATENATE '[' ADRC-NAME1 ADRC-NAME2 ']' INTO GV_SHIP_NAME SEPARATED BY SPACE. "CH33 Add by WAntanee 20210121
                GT_OUTTAB-SHIP_PROVINCE = ADRC-CITY1 . "Ship to province
                GT_OUTTAB-POSTCODE = ADRC-POST_CODE1 . "Post Code
              ENDIF.
            ENDSELECT.
          ENDIF.
        ENDSELECT.
      ENDIF.
* End Change By Aphai On Sep 9th , 2014

      GT_OUTTAB-DELI_TO = GV_CUST_ADDR(60).
      GT_OUTTAB-SHIP_ADDR = GV_SHIP_ADDR(255). "Ship to Address

      CONCATENATE GT_IITAB-VBELN GT_IITAB-POSNR INTO LV_VBELN_POSNR.
      IF GT_IITAB-VBELN(2) = '40'.
        PERFORM READ_TEXT  USING 'ZH10' 'VBBK' GT_IITAB-VBELN
                     CHANGING GT_OUTTAB-REMARK.
        PERFORM READ_TEXT  USING 'ZH13' 'VBBK' GT_IITAB-VBELN
                     CHANGING LV_TEXT_INVREMARK.

        IF LV_TEXT_INVREMARK EQ GT_OUTTAB-REMARK.
          CLEAR: LV_TEXT_INVREMARK.
        ENDIF.

        CONCATENATE GT_OUTTAB-REMARK LV_TEXT_INVREMARK INTO GT_OUTTAB-REMARK SEPARATED BY SPACE.

        "CH33 Add by WAntanee 20210121
        IF GT_IITAB-LSTEL EQ 'ZM'.
          CONCATENATE GV_SHIP_NAME GT_OUTTAB-REMARK INTO GT_OUTTAB-REMARK.
        ENDIF.
        "CH33 End Add by WAntanee 20210121
      ELSEIF GT_IITAB-VBELN(2) = '42' OR
             GT_IITAB-VBELN(2) = '41'.
        BREAK WANTANEE.
*        PERFORM READ_TEXT  USING 'ZI03' 'VBBP' LV_VBELN_POSNR
        PERFORM READ_TEXT  USING 'ZH09' 'VBBK' GT_IITAB-VBELN
                   CHANGING LV_TEXT.
        IF LV_TEXT IS NOT INITIAL.
          GT_OUTTAB-REMARK = LV_TEXT+0(190).
          GT_OUTTAB-PICHON = LV_TEXT+190(190).
        ENDIF.
        .


      ENDIF.



*-T41K908432
***      PERFORM read_text2 USING 'ZI03' 'VBBP' gt_iitab-vbeln gt_iitab-uepos
***                     CHANGING gt_outtab-pichon.

*<<<< T41K912266
      SELECT SINGLE LGORT VGBEL  "Edit by Wantanee 20140721
      INTO (LIPS-LGORT, LV_SO_NO)
      FROM LIPS
      WHERE VBELN EQ GT_IITAB-VBELN AND
            POSNR EQ GT_IITAB-POSNR.
      IF SY-SUBRC EQ 0.
        CONCATENATE GT_IITAB-VBELN GT_IITAB-POSNR INTO LV_VBELN_POSNR.
        IF LIPS-LGORT IS INITIAL.

          "Add CH30
          IF GT_IITAB-LSTEL EQ 'Z1'.  "Add CH32
            CHECK_1100_SONY = 'X'.
          ELSE.
            IF GT_OUTTAB-LGORT+0(2) = '11'.
              CHECK_1100_SONY = 'X'.
            ENDIF.
          ENDIF.
          "End CH30

          IF GT_IITAB-LSTEL EQ 'ZD'.
*             gt_outtab-lgort = '1700' . "Remove for project DDD  CH34 T41K939523 CH35 T41K939532
            GT_OUTTAB-LGORT = '1800' . "Add for project DDD  T41K934404 T41K934432  T41K939523 "CH34 "CH35
            CHECK_1800_DIT = 'X'. "T41K934404 T41K934432
            READ TABLE GT_DO_REFER_BOM INTO WA_DO_REFER_BOM WITH KEY VBELN = GT_IITAB-VBELN
                                                                     POSNR = GT_IITAB-POSNR.
            IF SY-SUBRC EQ 0.
              READ TABLE GT_DO_REFER_BOM INTO GW_DO_REFER_BOM WITH KEY VBELN = GT_IITAB-VBELN
                                                                       POSNR = WA_DO_REFER_BOM-UEPOS.
              IF SY-SUBRC EQ 0.
                CLEAR: LV_SUM_QTY_SPILT,LV_QTY_1700.

                IF GT_IITAB-LGMNG EQ GW_DO_REFER_BOM-LGMNG.
                  WRITE GT_IITAB-LGMNG TO GT_OUTTAB-LGMNG LEFT-JUSTIFIED DECIMALS 0.
                ELSE.
                  LOOP AT GT_IITAB INTO WA_IITAB_TEMP WHERE VGBEL EQ GT_IITAB-VGBEL
                                                    AND POSNV EQ GT_IITAB-POSNV.
                    IF ( NOT WA_IITAB_TEMP-LGORT IS INITIAL ) AND
*                                      wa_iitab_temp-lgort NE '1700' . "Remove for project DDD  CH34 T41K939523 CH35 T41K939532
                         WA_IITAB_TEMP-LGORT NE '1800' . "Add for project DDD  T41K934404 T41K934432  CH34 T41K939523 CH35 T41K939532
                      LV_SUM_QTY_SPILT =  LV_SUM_QTY_SPILT + WA_IITAB_TEMP-LGMNG.
                    ENDIF.

                  ENDLOOP.
                  LV_QTY_1700 = GW_DO_REFER_BOM-LGMNG - LV_SUM_QTY_SPILT.
                  WRITE LV_QTY_1700 TO GT_OUTTAB-LGMNG LEFT-JUSTIFIED DECIMALS 0.
                ENDIF.

              ENDIF.

            ENDIF.


          ELSE.

            IF GT_IITAB-KOWRR NE 'X'.

              PERFORM READ_TEXT  USING 'ZI03' 'VBBP' LV_VBELN_POSNR
                    CHANGING GT_OUTTAB-LGORT.
            ENDIF.

            IF GT_OUTTAB-LGORT IS INITIAL.
              IF GT_IITAB-KOWRR NE 'X'.
                IF GT_IITAB-MATNR CP '*TRANSPORT*'.
                ELSE.
                  CONCATENATE 'Please input location DO item: ' GT_IITAB-VBELN  GT_IITAB-POSNR INTO LV_CHK_LOGRT SEPARATED BY SPACE.
                ENDIF.
              ENDIF.
            ELSE.
              CONDENSE GT_OUTTAB-LGORT.
            ENDIF.
            "Add CH29
            IF GT_OUTTAB-LGORT+0(2) = '16'.
              CHECK_1600_SMP = 'X'.
            ENDIF.
            "End CH29
            "CH3 Add by WAntanee 20230315
            IF GT_OUTTAB-LGORT+0(2) = '15'.
              CHECK_1500_JPP = 'X'.
            ENDIF.

            "CH3 End Add by WAntanee 20230315
            "Add CH30
            IF GT_OUTTAB-LGORT+0(2) = '11'.
              CHECK_1100_SONY = 'X'.
            ENDIF.
            "End CH30

          ENDIF.
        ELSE.
*            IF lips-lgort  IN s_lgort[].
          WRITE: LIPS-LGORT TO GT_OUTTAB-LGORT LEFT-JUSTIFIED.
*            ENDIF.

          IF GT_OUTTAB-LGORT = '1800'.
            CHECK_1800_DIT = 'X'.
          ENDIF.
          "Add CH29
          IF GT_OUTTAB-LGORT+0(2) = '16'.
            CHECK_1600_SMP = 'X'.
          ENDIF.
          "End CH29
          "CH3 Add by WAntanee 20230315
          IF GT_OUTTAB-LGORT+0(2) = '15'.
            CHECK_1500_JPP = 'X'.
          ENDIF.

          "CH3 End Add by WAntanee 20230315
          "Add CH30
*               IF gt_outtab-lgort+0(2) = '11'.
*                 check_1100_sony = 'X'.
*               ENDIF.
          "Add CH32
          IF GT_IITAB-LSTEL EQ 'Z1'.
            CHECK_1100_SONY = 'X'.
          ELSE.
            IF GT_OUTTAB-LGORT+0(2) = '11'.
              CHECK_1100_SONY = 'X'.
            ENDIF.
          ENDIF.
          "End Add CH32
          "End CH30
        ENDIF.



      ENDIF.
*>>>> T41K912266
*<<<< BOI Text file for SONY location
      "Add by Wantanee 20140721
      SELECT SINGLE AUART
      INTO LV_SO_TYPE
      FROM VBAK
      WHERE VBELN EQ LV_SO_NO.
      "Add by Wantanee 20140721



*******************************
***** Check Warranty **********
*******************************
      CLEAR: LV_CHECT_MAT_AT.
      IF I_MAT_AT[] IS NOT INITIAL.
        READ TABLE I_MAT_AT WITH KEY IDNRK = GT_IITAB-MATNR .
        IF SY-SUBRC EQ 0.
          LV_CHECT_MAT_AT = 'X'.
        ENDIF.
      ENDIF.

      SELECT * FROM ZSDSSDC002 WHERE LSTEL = GT_IITAB-LSTEL AND
                                          LGORT = GT_OUTTAB-LGORT.
        IF SY-SUBRC EQ 0.

* Change by aphai On Sep 12th, 2014
*          IF gt_iitab-lstel = 'Z5' AND s_lgort[] IS INITIAL.
*            gv_loading_point = 'Z5'.
*            ZSDSSDC002-desci = 'HITACHI'.
*          ENDIF.
          "Add by Wantanee 20150521 ITR2015-3851



          READ TABLE GT_ZSDSSDC006 INTO GW_ZSDSSDC006 WITH KEY MATNR =  GT_IITAB-MATNR
                                                                         AUART =  LV_SO_TYPE.
          IF SY-SUBRC EQ 0.
            GT_OUTTAB-WARRANTY = 'W'.
          ENDIF.


          IF GT_OUTTAB-WARRANTY NE 'W'.

            SELECT SINGLE AUART
            INTO LV_IO_AUART
            FROM AUFK
            WHERE AUFNR EQ GT_IITAB-AUFNR.

            IF LV_IO_AUART EQ 'H004'.
              READ TABLE GT_CUST_WARRANTY INTO WA_CUST_WARRANTY WITH KEY KUNNR = GT_IITAB-KUNNR
                                                                          VKBUR = GT_IITAB-VKBUR.
              IF SY-SUBRC EQ 0.
                LV_CHECK_CUST = 'X'.
              ENDIF.

            ENDIF.


            IF NOT LV_CHECK_CUST IS INITIAL.
              IF PH2 EQ 'CDU'.
                GT_OUTTAB-WARRANTY = 'W'.
              ENDIF.

            ELSE.
              "End Add by Wantanee 20150521 ITR2015-3851




*                            CASE ZSDSSDC002-desci.
              CASE F_TO.
                WHEN 'SONY'. "Change by aphai On Sep 12th , 2014



                  CASE GT_IITAB-VTWEG.
                    WHEN 10.
                      "Remove by Wantanee 20191126 Change condition warranty stamp
*                            READ TABLE gt_warranty INTO wa_warranty WITH KEY vtweg = gt_iitab-vtweg
*                                                                             vkbur = gt_iitab-vkbur
*                                                                             ph1   = ph1
*                                                                             ph2   = ph2
*                                                                             ph3   = ph3.
                      "End Remove by Wantanee 20191126 Change condition warranty stamp
                      READ TABLE GT_WARRANTY INTO WA_WARRANTY WITH KEY VTWEG = GT_IITAB-VTWEG
                                                                       VKBUR = GT_IITAB-VKBUR
                                                                       PH1   = PH1
                                                                       PH2   = PH2.
*

                      IF SY-SUBRC EQ 0 AND GT_IITAB-ERSDA GE WA_WARRANTY-WDATE.
                        IF LV_SO_TYPE NE 'ZOK1'.
                          " CONCATENATE 'Warranty Card' gt_outtab-deli_to INTO gt_outtab-deli_to. "Remove by Wantanee 20140923
                          GT_OUTTAB-WARRANTY = 'W'. "Add by Wantanee 20140923
                        ENDIF.
                      ENDIF.

*                                Add by aphai on 7.11.2014
                      IF GT_OUTTAB-WARRANTY IS INITIAL.
                        READ TABLE GT_KONV INTO WA_KONV WITH KEY KNUMV = GT_IITAB-KNUMV
                                                                 KPOSN = GT_IITAB-POSNV.
                        IF SY-SUBRC EQ 0.
                          CALL FUNCTION 'Z_SDSMM_CHK_WARRANTY_SP'
                            EXPORTING
                              P_MATNR    = GT_IITAB-MATNR
                              P_VTWEG    = GT_IITAB-VTWEG
                              P_VKBUR    = GT_IITAB-VKBUR
                            IMPORTING
                              ISWARRANTY = GT_OUTTAB-WARRANTY
                            TABLES
                              ITAB       = DT_CONST[].

                        ENDIF.
                      ENDIF.
*                                End add aphai on 7.11.2014

*                            "Add by wantanee 20190521

*                            break wantanee.
                      SELECT SINGLE AUART
                        INTO LV_SO_AUART
                        FROM VBAK
                        WHERE VBELN = GT_OUTTAB-SO.


*
                      IF LV_SO_AUART EQ 'ZO07'.
*                                 IF gt_outtab-lfdat GT '20190520' AND gt_outtab-lfdat LT '20190801'.
*                                    READ TABLE dt_cons_clew INTO wa_cons_clew WITH KEY value = gt_outtab-matnr.
*                                    IF sy-subrc EQ 0.
*                                        gt_outtab-warranty = 'W'.
*                                    ENDIF.
*                                 ENDIF.

                        CONCATENATE GT_OUTTAB-SO GT_IITAB-POSNV INTO LV_SO_ITEM.
                        PERFORM READ_TEXT_ITEM  USING 'ZI13' 'VBBP' LV_SO_ITEM
                        CHANGING LV_CHK_WARRANTY.
                        IF LV_CHK_WARRANTY = 'X' OR LV_CHK_WARRANTY = 'x'.
                          GT_OUTTAB-WARRANTY = 'W'.
                        ENDIF.

                      ENDIF.
*
*
*                            "End Add by Wantanee 20190521

                    WHEN 20.
                      "Remove by Wantanee 20191126 Change condition warranty stamp
*                            READ TABLE gt_warranty INTO wa_warranty WITH KEY vtweg = gt_iitab-vtweg
*                                                                             vkbur = gt_iitab-vkbur
*                                                                             ph1   = ph1
*                                                                             ph2   = ph2
*                                                                             ph3   = space. "Add by Aphai on 13.05.2015
                      "End Remove by Wantanee 20191126 Change condition warranty stamp
                      READ TABLE GT_WARRANTY INTO WA_WARRANTY WITH KEY VTWEG = GT_IITAB-VTWEG
                                                                       VKBUR = GT_IITAB-VKBUR
                                                                       PH1   = PH1
                                                                       PH2   = PH2
                                                                       PH3   = PH3.
                      IF SY-SUBRC EQ 0.
                        CLEAR PERCENT.
                        READ TABLE GT_KONV INTO WA_KONV WITH KEY KNUMV = GT_IITAB-KNUMV
                                                                 KPOSN = GT_IITAB-POSNV.
                        IF SY-SUBRC EQ 0 AND GT_IITAB-ERSDA GE WA_WARRANTY-WDATE.
                          PERCENT = ( GT_IITAB-NETWR * 100 ) / WA_KONV-KWERT.
                          IF PERCENT GE WA_WARRANTY-LP_PERCENT.
                            IF LV_SO_TYPE NE 'ZOK1'.
                              "CONCATENATE 'Warranty Card' gt_outtab-deli_to INTO gt_outtab-deli_to SEPARATED BY space. "Remove by Wantanee 20140923
                              GT_OUTTAB-WARRANTY = 'W'. "Add by Wantanee 20140923
                            ENDIF.
                          ENDIF.
                        ENDIF.
                      ENDIF.

                      IF GT_OUTTAB-DELI_TO+0(1) NE 'W'.
                        READ TABLE GT_WARRANTY INTO WA_WARRANTY WITH KEY VTWEG = GT_IITAB-VTWEG
                                                                         VKBUR = GT_IITAB-VKBUR
                                                                         PH1   = PH1
                                                                         PH2   = PH2
                                                                         PH3   = PH3.
                        IF SY-SUBRC EQ 0.
                          CLEAR PERCENT.
                          READ TABLE GT_KONV INTO WA_KONV WITH KEY KNUMV = GT_IITAB-KNUMV
                                                                   KPOSN = GT_IITAB-POSNV.
                          IF SY-SUBRC EQ 0 AND GT_IITAB-ERSDA GE WA_WARRANTY-WDATE.
                            PERCENT = ( GT_IITAB-NETWR * 100 ) / WA_KONV-KWERT.
                            IF PERCENT GE WA_WARRANTY-LP_PERCENT.
                              IF LV_SO_TYPE NE 'ZOK1'.
                                "CONCATENATE 'Warranty Card' gt_outtab-deli_to INTO gt_outtab-deli_to SEPARATED BY space. "Remove by Wantanee 20140923
                                GT_OUTTAB-WARRANTY = 'W'.  "Add by Wantanee 20140923
                              ENDIF.
                            ENDIF.
                          ENDIF.
                        ENDIF.
                      ENDIF.

                      IF GT_OUTTAB-WARRANTY NE 'W'.
                        READ TABLE GT_WARRANTY INTO WA_WARRANTY WITH KEY VTWEG = GT_IITAB-VTWEG
                                                                         VKBUR = GT_IITAB-VKBUR
                                                                         PH1   = PH1
                                                                         PH2   = PH2
                                                                         PH3   = SPACE.
                        IF SY-SUBRC EQ 0.
                          CLEAR PERCENT.
                          READ TABLE GT_KONV INTO WA_KONV WITH KEY KNUMV = GT_IITAB-KNUMV
                                                                   KPOSN = GT_IITAB-POSNV.
                          IF SY-SUBRC EQ 0 AND GT_IITAB-ERSDA GE WA_WARRANTY-WDATE.
                            PERCENT = ( GT_IITAB-NETWR * 100 ) / WA_KONV-KWERT.
                            IF PERCENT GE WA_WARRANTY-LP_PERCENT.
                              IF LV_SO_TYPE NE 'ZOK1'.
                                "CONCATENATE 'Warranty Card' gt_outtab-deli_to INTO gt_outtab-deli_to SEPARATED BY space. "Remove by Wantanee 20140923
                                GT_OUTTAB-WARRANTY = 'W'.  "Add by Wantanee 20140923
                              ENDIF.
                            ENDIF.
                          ENDIF.
                        ENDIF.


                      ENDIF.





                  ENDCASE.

*                                Add by aphai on 10.11.2014
                  IF GT_OUTTAB-WARRANTY IS INITIAL.
                    READ TABLE GT_KONV INTO WA_KONV WITH KEY KNUMV = GT_IITAB-KNUMV
                                                             KPOSN = GT_IITAB-POSNV.
                    IF SY-SUBRC EQ 0.
                      CALL FUNCTION 'ZSD_CHK_WARRANTY_SP'
                        EXPORTING
                          P_MATNR    = GT_IITAB-MATNR
                          P_NETWR    = GT_IITAB-NETWR
                          P_KWERT    = WA_KONV-KWERT
                          P_VTWEG    = GT_IITAB-VTWEG
                          P_VKBUR    = GT_IITAB-VKBUR
                        IMPORTING
                          ISWARRANTY = GT_OUTTAB-WARRANTY
                        TABLES
                          ITAB       = DT_CONST[].

                    ENDIF.
                  ENDIF.
*                                End add aphai on 10.11.2014

*                  *              Add by aphai on 3.11.2014
*                                IF gt_outtab-warranty EQ space.
*                                  IF dt_const[] IS NOT INITIAL.
*                                    READ TABLE dt_const WITH KEY value = gt_iitab-matnr BINARY SEARCH.
*                                    IF sy-subrc EQ 0.
*                                      gt_outtab-warranty = 'W'.
*                                    ENDIF.
*                                  ENDIF.
*                                ENDIF.
*                  *              End add aphai on 3.11.2014
                  "Add by Wantanee 20180629
                  READ TABLE GT_ZSDSSDC005 INTO WA_ZSDSSDC005 WITH KEY MATNR = GT_IITAB-MATNR.
                  IF SY-SUBRC EQ 0.
*                                    gt_outtab-warranty = 'S'.
                    GT_OUTTAB-REMARK_PICHON = 'X'.
                  ENDIF.
                  "End Add by Wantanee 20180629
*                      WHEN 'HITACHI'.
*                        CASE gt_iitab-vtweg.
*                          WHEN 10.
*                            READ TABLE gt_warranty INTO wa_warranty WITH KEY vtweg = gt_iitab-vtweg
*                                                                             vkbur = gt_iitab-vkbur
*                                                                             ph1   = ph1
*                                                                             ph2   = ph2
*                                                                             ph3   = ph3.
*                            IF sy-subrc EQ 0 AND gt_iitab-ersda GE wa_warranty-wdate.
*                              gt_outtab-warranty = 'W'.
*                            ENDIF.
*
*        *                        Add by aphai on 7.11.2014
*                            IF gt_outtab-warranty IS INITIAL.
*                              READ TABLE gt_konv INTO wa_konv WITH KEY knumv = gt_iitab-knumv
*                                                                       kposn = gt_iitab-posnv.
*                              IF sy-subrc EQ 0.
*                                CALL FUNCTION 'ZSD_CHK_WARRANTY_SP'
*                                  EXPORTING
*                                    p_matnr    = gt_iitab-matnr
*                                    p_vtweg    = gt_iitab-vtweg
*                                    p_vkbur    = gt_iitab-vkbur
*                                  IMPORTING
*                                    iswarranty = gt_outtab-warranty
*                                  TABLES
*                                    itab       = dt_const[].
*
*                              ENDIF.
*                            ENDIF.
*        *                        End add aphai on 7.11.2014
*
*
*                          WHEN 20.
*                            READ TABLE gt_warranty INTO wa_warranty WITH KEY vtweg = gt_iitab-vtweg
*                                                                             vkbur = gt_iitab-vkbur
*                                                                             ph1   = ph1
*                                                                             ph2   = ph2
*                                                                             ph3   = ph3.
*                            IF sy-subrc EQ 0.
*                              CLEAR percent.
*                              READ TABLE gt_konv INTO wa_konv WITH KEY knumv = gt_iitab-knumv
*                                                                       kposn = gt_iitab-posnv.
*                              IF sy-subrc EQ 0 AND gt_iitab-ersda GE wa_warranty-wdate.
*                                percent = ( gt_iitab-netwr * 100 ) / wa_konv-kwert.
*                                IF percent GE wa_warranty-lp_percent.
*                                  gt_outtab-warranty = 'W'.
*                                ENDIF.
*                              ENDIF.
*                            ENDIF.
*
*                            IF gt_outtab-warranty IS INITIAL.
*                              READ TABLE gt_warranty INTO wa_warranty WITH KEY vtweg = gt_iitab-vtweg
*                                                                               vkbur = gt_iitab-vkbur
*                                                                               ph1   = ph1
*                                                                               ph2   = ph2
*                                                                               ph3   = space "Add by Aphai on 13.05.2015
*                                                                               .
*                              IF sy-subrc EQ 0.
*                                CLEAR percent.
*                                READ TABLE gt_konv INTO wa_konv WITH KEY knumv = gt_iitab-knumv
*                                                                         kposn = gt_iitab-posnv.
*                                IF sy-subrc EQ 0 AND gt_iitab-ersda GE wa_warranty-wdate.
*                                  percent = ( gt_iitab-netwr * 100 ) / wa_konv-kwert.
*                                  IF percent GE wa_warranty-lp_percent.
*                                    gt_outtab-warranty = 'W'.
*                                  ENDIF.
*                                ENDIF.
*                              ENDIF.
*                            ENDIF.
*
*
*        *                        Add by aphai on 7.11.2014
*                            IF gt_outtab-warranty IS INITIAL.
*                              READ TABLE gt_konv INTO wa_konv WITH KEY knumv = gt_iitab-knumv
*                                                                       kposn = gt_iitab-posnv.
*                              IF sy-subrc EQ 0.
*                                CALL FUNCTION 'ZSD_CHK_WARRANTY_SP'
*                                  EXPORTING
*                                    p_matnr    = gt_iitab-matnr
*                                    p_netwr    = gt_iitab-netwr
*                                    p_kwert    = wa_konv-kwert
*                                    p_vtweg    = gt_iitab-vtweg
*                                    p_vkbur    = gt_iitab-vkbur
*                                  IMPORTING
*                                    iswarranty = gt_outtab-warranty
*                                  TABLES
*                                    itab       = dt_const[].
*                              ENDIF.
*                            ENDIF.
*        *                        End add aphai on 7.11.2014
*
*                        ENDCASE.
*
*        *          *              Add by aphai on 3.11.2014
*        *                        IF gt_outtab-warranty EQ space.
*        *                          IF dt_const[] IS NOT INITIAL.
*        *                            READ TABLE dt_const WITH KEY value = gt_iitab-matnr BINARY SEARCH.
*        *                            IF sy-subrc EQ 0.
*        *                              gt_outtab-warranty = 'W'.
*        *                            ENDIF.
*        *                          ENDIF.
*        *                        ENDIF.
*        *          *              End add aphai on 3.11.2014


              ENDCASE.
            ENDIF.


            "Add by Wantanee 20160502
            READ TABLE GT_ZSDSSDC004 INTO GW_ZSDSSDC004 WITH KEY MATNR = GT_IITAB-MATNR.
            IF SY-SUBRC = 0.
              GT_OUTTAB-WARRANTY = ''.
            ENDIF.
            "End Add by Wantanee 20160502

            IF LV_CHECT_MAT_AT EQ 'X'.
              GT_OUTTAB-WARRANTY = ''.
            ENDIF.

            " Add by Jakarin 20170727
            IF GT_IITAB-AUFNR = 'SDS'.
              GT_OUTTAB-WARRANTY = ''.
            ENDIF.
            " End Add by Jakarin 20170727
          ENDIF.
        ENDIF.
      ENDSELECT. " End Select warranty
*End Change by aphai On Sep 12th, 2014
*      IF ( gt_iitab-lstel EQ 'Z1' AND gt_outtab-lgort NE '1500' ) OR
*         ( gt_iitab-lstel EQ 'Z2' AND gt_outtab-lgort NE '1500' ) OR
*         ( gt_iitab-lstel EQ 'Z3' AND gt_outtab-lgort NE '1500' ) OR
*         ( gt_iitab-lstel EQ 'Z6' AND gt_outtab-lgort NE '1500' ) OR
*         ( gt_iitab-lstel EQ 'Z9' AND gt_outtab-lgort NE '1500' ) OR
*         ( gt_iitab-lstel EQ 'Z5' AND gt_outtab-lgort EQ '1100' AND  s_lgort[] IS NOT INITIAL ).
*        CASE gt_iitab-vtweg.
*          WHEN 10.
*            READ TABLE gt_warranty INTO wa_warranty WITH KEY vtweg = gt_iitab-vtweg
*                                                             vkbur = gt_iitab-vkbur
*                                                             ph1   = gt_iitab-prodh+0(5)
*                                                             ph2   = gt_iitab-prodh+5(5)
*                                                             ph3   = gt_iitab-prodh+10(8).
*            IF sy-subrc EQ 0 AND gt_iitab-ersda GE wa_warranty-wdate.
*              CONCATENATE 'Warranty Card' gt_outtab-deli_to INTO gt_outtab-deli_to.
*            ENDIF.
*          WHEN 20.
*            READ TABLE gt_warranty INTO wa_warranty WITH KEY vtweg = gt_iitab-vtweg
*                                                             vkbur = gt_iitab-vkbur
*                                                             ph1   = gt_iitab-prodh+0(5)
*                                                             ph2   = gt_iitab-prodh+5(5).
*            IF sy-subrc EQ 0.
*              CLEAR percent.
*              READ TABLE gt_konv INTO wa_konv WITH KEY knumv = gt_iitab-knumv
*                                                       kposn = gt_iitab-posnr.
*              IF sy-subrc EQ 0 AND gt_iitab-ersda GE wa_warranty-wdate.
*                percent = ( gt_iitab-netwr * 100 ) / wa_konv-kbetr.
*                IF percent GE wa_warranty-lp_percent.
*                  CONCATENATE 'Warranty Card' gt_outtab-deli_to INTO gt_outtab-deli_to.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*
*            IF gt_outtab-deli_to+0(1) NE 'W'.
*              READ TABLE gt_warranty INTO wa_warranty WITH KEY vtweg = gt_iitab-vtweg
*                                                               vkbur = gt_iitab-vkbur
*                                                               ph1   = gt_iitab-prodh+0(5)
*                                                               ph2   = gt_iitab-prodh+5(5)
*                                                               ph3   = gt_iitab-prodh+10(8).
*              IF sy-subrc EQ 0.
*                CLEAR percent.
*                READ TABLE gt_konv INTO wa_konv WITH KEY knumv = gt_iitab-knumv
*                                                         kposn = gt_iitab-posnr.
*                IF sy-subrc EQ 0 AND gt_iitab-ersda GE wa_warranty-wdate.
*                  percent = ( gt_iitab-netwr * 100 ) / wa_konv-kbetr.
*                  IF percent GE wa_warranty-lp_percent.
*                    CONCATENATE 'Warranty Card' gt_outtab-deli_to INTO gt_outtab-deli_to.
*                  ENDIF.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*        ENDCASE.
**>>>> BOE Text file for SONY location
**<<<< BOI Text file for Hitachi location
*      ELSEIF gt_iitab-lstel EQ 'Z5' AND s_lgort[] IS INITIAL OR
*           ( gt_iitab-lstel EQ 'Z2' AND gt_outtab-lgort EQ '1500' ) OR
*           ( gt_iitab-lstel EQ 'Z3' AND gt_outtab-lgort EQ '1500' ) OR
*           ( gt_iitab-lstel EQ 'Z6' AND gt_outtab-lgort EQ '1500' ) OR
*           ( gt_iitab-lstel EQ 'Z9' AND gt_outtab-lgort EQ '1500' ).
*        MOVE-CORRESPONDING gt_outtab TO gt_outtab4.
*        CLEAR: gt_outtab.
*
*        CASE gt_iitab-vtweg.
*          WHEN 10.
*            READ TABLE gt_warranty INTO wa_warranty WITH KEY vtweg = gt_iitab-vtweg
*                                                             vkbur = gt_iitab-vkbur
*                                                             ph1   = gt_iitab-prodh+0(5)
*                                                             ph2   = gt_iitab-prodh+5(5)
*                                                             ph3   = gt_iitab-prodh+10(8).
*            IF sy-subrc EQ 0 AND gt_iitab-ersda GE wa_warranty-wdate.
*              gt_outtab4-warranty = 'W'.
*            ENDIF.
*          WHEN 20.
*            READ TABLE gt_warranty INTO wa_warranty WITH KEY vtweg = gt_iitab-vtweg
*                                                             vkbur = gt_iitab-vkbur
*                                                             ph1   = gt_iitab-prodh+0(5)
*                                                             ph2   = gt_iitab-prodh+5(5).
*            IF sy-subrc EQ 0.
*              CLEAR percent.
*              READ TABLE gt_konv INTO wa_konv WITH KEY knumv = gt_iitab-knumv
*                                                       kposn = gt_iitab-posnr.
*              IF sy-subrc EQ 0 AND gt_iitab-ersda GE wa_warranty-wdate.
*                percent = ( gt_iitab-netwr * 100 ) / wa_konv-kbetr.
*                IF percent GE wa_warranty-lp_percent.
*                  gt_outtab4-warranty = 'W'.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*
*            IF gt_outtab4-warranty IS INITIAL.
*              READ TABLE gt_warranty INTO wa_warranty WITH KEY vtweg = gt_iitab-vtweg
*                                                               vkbur = gt_iitab-vkbur
*                                                               ph1   = gt_iitab-prodh+0(5)
*                                                               ph2   = gt_iitab-prodh+5(5)
*                                                               ph3   = gt_iitab-prodh+10(8).
*              IF sy-subrc EQ 0.
*                CLEAR percent.
*                READ TABLE gt_konv INTO wa_konv WITH KEY knumv = gt_iitab-knumv
*                                                         kposn = gt_iitab-posnr.
*                IF sy-subrc EQ 0 AND gt_iitab-ersda GE wa_warranty-wdate.
*                  percent = ( gt_iitab-netwr * 100 ) / wa_konv-kbetr.
*                  IF percent GE wa_warranty-lp_percent.
*                    gt_outtab4-warranty = 'W'.
*                  ENDIF.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*        ENDCASE.
*        APPEND: gt_outtab4.
*        CLEAR:  gt_outtab4.
*      ENDIF.
*>>>> BOE Text file for Hitachi location
***ENDLOOP.
***        MOVE: lv_pic_text TO gv_pic_text,
***              lv_pichon1 TO gv_pichon,
***              lv_pic_text2 TO gv_pic_text2,
***              lv_pichon2 TO gv_pichon2.


** Commment by Aphai On sep 12th, 2014


      "Add by Wantanee 20151020
      GT_OUTTAB_HI = GT_OUTTAB.
      GT_OUTTAB_HI-DEST_ADDR = ADRC-STREET.
      GT_OUTTAB_HI-DEST_SUB_DIS = ADRC-LOCATION.
      GT_OUTTAB_HI-DEST_DISTRIC = ADRC-CITY2.
      GT_OUTTAB_HI-DEST_PROVIN = ADRC-CITY1.
      GT_OUTTAB_HI-DEST_POST_CODE = ADRC-POST_CODE1.

      IF GT_OUTTAB_HI IS NOT INITIAL.

        LV_KUNNR = GT_OUTTAB_HI-KUNNR.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = LV_KUNNR
          IMPORTING
            OUTPUT = LV_KUNNR.

        IF LV_KUNNR IN LR_KUNNR.
          CLEAR GT_OUTTAB_HI-WARRANTY.
        ENDIF.
        APPEND GT_OUTTAB_HI.
        CLEAR: GT_OUTTAB_HI.
      ENDIF.

      "Add by Wantanee 20151020


*                 b~aufnr   "Add by Wantanee 20150521 ITR2015-3851
*           b~vgbel   "Add by wantanee 20170817
*           c~vkgrp
*           g~zterm
*           g~bstkd
*           b~meins

*        salesman(40),

      DATA: LV_PERNR TYPE PA0002-PERNR,
            LV_VORNA TYPE PA0002-VORNA.
      DATA: LV_PARNR TYPE VBPA-PARNR,
            LV_NAMEV TYPE KNVK-NAMEV,
            LV_NAME1 TYPE KNVK-NAME1.

      SELECT SINGLE PERNR
        INTO LV_PERNR
        FROM VBPA
        WHERE VBELN EQ GT_IITAB-VBELN
        AND PARVW EQ 'VE'.

      IF LV_PERNR IS NOT INITIAL.
        SELECT SINGLE VORNA
          INTO LV_VORNA
          FROM PA0002
          WHERE PERNR EQ LV_PERNR.

        GT_OUTTAB-SALESMAN = LV_VORNA.
      ENDIF.


      BREAK WANTANEE.

      CLEAR: LV_ZBD1T,LV_EZFBDT,LV_DUE.                     "T41K939590
      DATA: LV_DATE TYPE SY-DATLO.
      DATA: LV_INV_REF TYPE VBFA-VBELV.

      LV_DATE = GT_IITAB-LFDAT.




      CALL FUNCTION 'FI_TERMS_OF_PAYMENT_PROPOSE'
        EXPORTING
          I_BLDAT = LV_DATE
          I_BUDAT = LV_DATE
          I_CPUDT = LV_DATE
          I_ZFBDT = LV_DATE
          I_ZTERM = GT_IITAB-ZTERM
*         I_REINDAT             = I_REINDAT
*         I_LIFNR = I_LIFNR
*         I_BUKRS = I_BUKRS
        IMPORTING
          E_ZBD1T = LV_ZBD1T
*         E_ZBD1P = E_ZBD1P
*         E_ZBD2T = E_ZBD2T
*         E_ZBD2P = E_ZBD2P
*         E_ZBD3T = E_ZBD3T
          E_ZFBDT = LV_EZFBDT
*         E_SPLIT = E_SPLIT
*         E_ZSCHF = E_ZSCHF
*         E_ZLSCH = E_ZLSCH
*         E_T052  = E_T052
*           EXCEPTIONS
*         TERMS_NOT_FOUND       = 1
        .

      IF LV_ZBD1T IS NOT INITIAL.
        LV_DUE = LV_DATE + LV_ZBD1T.

      ELSEIF LV_EZFBDT IS NOT INITIAL.
        LV_DUE = LV_EZFBDT.
      ENDIF.
      GT_OUTTAB-DUE_DATE = LV_DUE.
      IF GT_IITAB-VBELN(2) = '40'.
        GT_OUTTAB-PO_NO = GT_IITAB-BSTKD.
      ELSEIF GT_IITAB-VBELN(2) = '42' OR
             GT_IITAB-VBELN(2) = '41'.
        GT_OUTTAB-PO_NO = GT_IITAB-BSTKD_E.
      ENDIF.


      SELECT SINGLE KTEXT INTO LV_INTERNAL_ORDER
      FROM COAS
      WHERE AUFNR = GT_IITAB-AUFNR  .


      GT_OUTTAB-PROJECT = LV_INTERNAL_ORDER.
      GT_OUTTAB-TERM_OF_PAYMENT = GT_IITAB-ZTERM.
*        contact_name(35),

      SELECT SINGLE PARNR
        INTO LV_PARNR
        FROM VBPA
        WHERE VBELN EQ GT_IITAB-VBELN
        AND PARVW EQ 'AP'.

      IF LV_PERNR IS NOT INITIAL.
        SELECT SINGLE NAMEV NAME1
          INTO (LV_NAMEV,LV_NAME1)
          FROM KNVK
          WHERE PARNR EQ LV_PARNR.

        CONCATENATE LV_NAMEV LV_NAME1 INTO GT_OUTTAB-CONTACT_NAME SEPARATED BY SPACE.
      ENDIF.


*        ref_inv(10),
      IF GT_IITAB-VBELN(2) = '42' OR
         GT_IITAB-VBELN(2) = '41'.
        SELECT SINGLE VBELV
          INTO LV_INV_REF
          FROM VBFA
          WHERE VBELN = GT_IITAB-VBELN
          AND VBTYP_V = 'M'.

        GT_OUTTAB-REF_INV = LV_INV_REF.
      ENDIF.

      IF GT_IITAB-VTWEG EQ '10'.
        GT_OUTTAB-SALES_DIV = 'DS'.
      ELSEIF GT_IITAB-VTWEG EQ '20'.
        GT_OUTTAB-SALES_DIV = 'GS'.
      ELSEIF GT_IITAB-VTWEG EQ '30'.
        GT_OUTTAB-SALES_DIV = 'EN'.
      ELSEIF GT_IITAB-VTWEG EQ '40'.
        GT_OUTTAB-SALES_DIV = 'SV'.
      ENDIF.

      SELECT SINGLE BEZEI
      INTO GT_OUTTAB-SALES_OFFICE
      FROM TVKBT
      WHERE VKBUR = GT_IITAB-VKBUR
        AND SPRAS EQ 'E'.

      IF GT_IITAB-VBELN(2) = '42' OR
         GT_IITAB-VBELN(2) = '41'.
        CONCATENATE GT_OUTTAB-SALESMAN '/' GT_OUTTAB-SALES_OFFICE INTO GT_OUTTAB-SALESMAN.
      ENDIF.

      SELECT SINGLE BEZEI
      INTO GT_OUTTAB-SALES_GROUP
      FROM TVGRT
      WHERE VKGRP = GT_IITAB-VKGRP
        AND SPRAS EQ 'E'.

*        sales_group(20),
      GT_OUTTAB-UOM = GT_IITAB-MEINS.
      IF R_NEW IS NOT INITIAL.

        GT_OUTTAB-DOC_FLAG = 'A'.
      ELSEIF R_UP IS NOT INITIAL.
        GT_OUTTAB-DOC_FLAG = 'E'.
      ENDIF.


      PERFORM READ_TEXT_HEAD USING 'ZH18' 'VBBK' GT_IITAB-VGBEL
                             CHANGING LV_CHECK_TEXT_SITE.
      IF LV_CHECK_TEXT_SITE EQ 'X' OR LV_CHECK_TEXT_SITE EQ 'x'.
        GT_OUTTAB-MHA = 'X'.
      ELSE.
        GT_OUTTAB-MHA = ''.
      ENDIF.

**-T41K908432
      IF GT_OUTTAB IS NOT INITIAL.
        LV_KUNNR = GT_OUTTAB-KUNNR.
        "Add by Wantanee 20200114
        IF NOT R_1700 IS  INITIAL.
          IF GT_OUTTAB-LGORT EQ '1800'.
*               IF gt_iitab-kowrr NE 'X'. "CH1
            GT_OUTTAB-LGORT = '1700'.
            CHECK_1800_DIT = ''. "T41K934404 T41K934432
*               ENDIF.
          ENDIF.
        ENDIF.
        "Add by Wantanee 20200114
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = LV_KUNNR
          IMPORTING
            OUTPUT = LV_KUNNR.
        IF LV_KUNNR IN LR_KUNNR.
          CLEAR GT_OUTTAB-WARRANTY.
        ENDIF.
        APPEND GT_OUTTAB.
        CLEAR: GT_OUTTAB.   "WCH301112 To clear tmp_data for Gift Set.
      ENDIF.


    ENDIF.
*
*    CLEAR : ph1,ph2,ph3.
  ENDLOOP. " end loop gt_outtab
*
*  gt_outtab2[] = gt_outtab[].
*  DELETE gt_outtab2 WHERE NOT deli_to+0(8) = 'Warranty'.


*BOI : This loop for sony location
* Commend By Aphai On Sep 12th, 2014
*  LOOP AT gt_outtab2 INTO wa_outtab2. " where deli_to+0(8) = 'Warranty'.
*    READ TABLE gt_iitab INTO wa_iitab WITH KEY vbeln = wa_outtab2-vbeln
*                                               posnr = wa_outtab2-posnr.
*    IF sy-subrc EQ 0.
*      LOOP AT gt_iitab INTO wa_iitab2 WHERE vbeln = wa_iitab-vbeln AND
*                                            uepos = wa_iitab-uepos.
*        LOOP AT gt_outtab INTO wa_outtab WHERE vbeln EQ wa_iitab2-vbeln AND
*                                               posnr EQ wa_iitab2-posnr AND
*                                               deli_to+0(8) NE 'Warranty'.
*          CONCATENATE 'Warranty Card' wa_outtab-deli_to INTO wa_outtab-deli_to SEPARATED BY space.
*          MODIFY: gt_outtab FROM wa_outtab.
*        ENDLOOP.
*      ENDLOOP.
*    ENDIF.
*  ENDLOOP.
*EOI : This loop for sony location
*** End Commment by Aphai On sep 12th, 2014

  SORT GT_OUTTAB  BY LFART CUST_CODE VBELN LFDAT ERDAT POSNR MATNR.
  SORT GT_OUTTAB4 BY LFART CUST_CODE VBELN LFDAT ERDAT POSNR MATNR.

*Commment by Aphai On sep 12th, 2014
*  IF s_lgort IS INITIAL.
*    LOOP AT gt_outtab4.
*      MOVE-CORRESPONDING gt_outtab4 TO gt_outtab.
*      APPEND gt_outtab.
*    ENDLOOP.
*  ENDIF.
** End Commment by Aphai On sep 12th, 2014
ENDFORM.                    " get_data_outbound

*&---------------------------------------------------------------------*
*&      Form  download_data
*&---------------------------------------------------------------------*
FORM DOWNLOAD_DATA .
  DATA: FILE1        TYPE STRING,
        FILE2        TYPE STRING,
        FILE3        TYPE STRING,
        FILE4        TYPE STRING,
        FILE5        TYPE STRING,
        FILE6        TYPE STRING, "CH29
        L_TEXT(4096) TYPE C OCCURS 0,
        LV_TEXT(100) TYPE C.

* inbound file
  IF P_F1 IS NOT INITIAL.
    IF GT_INTAB[] IS NOT INITIAL.
      FILE1 = P_FILE1.

      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          FILENAME        = FILE1
          FILETYPE        = 'ASC'
          CODEPAGE        = '8600'
        TABLES
          DATA_TAB        = GT_INTAB
        EXCEPTIONS
          FILE_OPEN_ERROR = 1
          FILE_READ_ERROR = 2.

      MESSAGE S001 WITH 'Download Inbound data Completed !'.
    ELSE.
      MESSAGE S001 WITH 'Data for Inbound not found !'.
    ENDIF.
  ENDIF.


* outbound file
  IF P_F2 IS NOT INITIAL OR
     P_F4 IS NOT INITIAL.
    "Add by Wantanee 20140807
    IF R_HI IS NOT INITIAL.  "Add by Wantanee 20151020

      IF GT_OUTTAB_HI[] IS NOT INITIAL.
        FILE2 = P_FILE2.

        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            FILENAME              = FILE2
            FILETYPE              = 'ASC'
            CODEPAGE              = '8600'
            TRUNC_TRAILING_BLANKS = SPACE
          TABLES
            DATA_TAB              = GT_OUTTAB_HI[]
          EXCEPTIONS
            FILE_OPEN_ERROR       = 1
            FILE_READ_ERROR       = 2.

        MESSAGE S001 WITH 'Download Outbound data Completed !'.
      ELSE.
*                 Change by Aphai on Sep 8th, 2014
        IF R_SO EQ 'X'.
          MESSAGE S001 WITH 'No Data Outbound For SONY!'.
        ELSEIF R_WM EQ 'X'.
          MESSAGE S001 WITH 'No Data Outbound For WMS!'.
        ELSE.
          MESSAGE S001 WITH 'No Data Outbound For HITACHI!'.
        ENDIF.
*                end change by aphai on Sep 8th, 2014
      ENDIF.  "End "Add by Wantanee 20151020
    ELSE.
      IF GT_OUTTAB[] IS NOT INITIAL.

*        IF file_name = ''.
*          READ TABLE s_vbeln INTO s_vbeln INDEX 1.
*          IF sy-subrc EQ 0.
*            IF sy-sysid EQ 'T41'.
*              CONCATENATE '\\172.31.136.37\wh_sony\OUT\DEV\' s_vbeln-low '.txt' INTO p_file2.
*            ELSEIF sy-sysid EQ 'T44'.
*              CONCATENATE '\\172.31.136.37\wh_sony\OUT\QAS\SDS_DO\' s_vbeln-low '.txt' INTO p_file2.
*            ELSE.
*              CONCATENATE '\\172.31.136.37\wh_sony\OUT\PRD\SDS_DO\' s_vbeln-low '.txt' INTO p_file2.
*            ENDIF.
*            " CONCATENATE '\\172.31.136.37\wh_sony\OUT\QAS\SDS_DO\' s_vbeln-low '.txt' INTO p_file2.
*          ENDIF.
*        ENDIF.
        IF P_F2 = 'X' OR P_F4 = 'X'.

          SELECT VBELN
          INTO TABLE GT_DO_TEMP
          FROM LIKP
          WHERE VBELN IN S_VBELN
          ORDER BY VBELN.

          LOOP AT GT_DO_TEMP INTO WA_DO_TEMP.
            IF SY-TABIX EQ 1.
              LV_TEXT = WA_DO_TEMP-VBELN.
              TEXT_NAME = WA_DO_TEMP-VBELN.
            ELSE.
              CONCATENATE TEXT_NAME WA_DO_TEMP-VBELN+6(4) INTO TEXT_NAME SEPARATED BY ','.
            ENDIF.
          ENDLOOP.
          FILE_NAME = TEXT_NAME.

          IF NOT R_SER IS INITIAL.

*            IF P_F2 EQ 'X'.
*
*                  IF SY-SYSID EQ 'T41'.
*
*                    CONCATENATE '\\172.31.136.37\wh_sony\OUT\DEV\' TEXT_NAME '.txt' INTO P_FILE2.
*                    CONCATENATE '\\172.31.136.37\ddd_project\QAS\OUT\DO\' TEXT_NAME '.txt' INTO P_FILE5. "T41K934404 T41K934432
*                    CONCATENATE '\\172.31.136.57\uat\OUT\' TEXT_NAME '.txt' INTO P_FILE6. "T41K934404 T41K934432
*
*
*                  ELSEIF SY-SYSID EQ 'T44'.
*                    CONCATENATE '\\172.31.136.37\wh_sony\OUT\QAS\SDS_DO\' TEXT_NAME '.txt' INTO P_FILE2.
*                    CONCATENATE '\\172.31.136.37\ddd_project\QAS\OUT\DO\' TEXT_NAME '.txt' INTO P_FILE5. "T41K934404 T41K934432
*                    CONCATENATE '\\172.31.136.57\uat\OUT\' TEXT_NAME '.txt' INTO P_FILE6. "T41K934404 T41K934432
*
*                  ELSE.
*                    CONCATENATE '\\172.31.136.37\wh_sony\OUT\PRD\SDS_DO\' TEXT_NAME '.txt' INTO P_FILE2.
*                    CONCATENATE '\\172.31.136.37\ddd_project\PRD\OUT\DO\' TEXT_NAME '.txt' INTO P_FILE5. "T41K934404 T41K934432
*                    CONCATENATE '\\172.31.136.51\uat\OUT\' TEXT_NAME '.txt' INTO P_FILE6. "T41K934404 T41K934432
*
*                  ENDIF.
*            ELSEIF P_F4 EQ 'X'.
*                  IF SY-SYSID EQ 'T41'.
*                    CONCATENATE '\\172.31.136.37\wh_sony\OUT\QAS\SDS_DO\' TEXT_NAME '.txt' INTO P_FILE4.
*                    CONCATENATE '\\172.31.136.57\uat\OUT\' TEXT_NAME '.txt' INTO P_FILE6. "T41K934404 T41K934432
*                  ELSEIF SY-SYSID EQ 'T44'.
*
*                    CONCATENATE '\\172.31.136.37\wh_sony\OUT\QAS\SDS_DO\' TEXT_NAME '.txt' INTO P_FILE4.
*                    CONCATENATE '\\172.31.136.57\uat\OUT\' TEXT_NAME '.txt' INTO P_FILE6. "T41K934404 T41K934432
*                  ELSE.
*                    CONCATENATE '\\172.31.136.37\wh_sony\OUT\PRD\SDS_DO\' TEXT_NAME '.txt' INTO P_FILE4.
*                    CONCATENATE '\\172.31.136.51\uat\OUT\' TEXT_NAME '.txt' INTO P_FILE6. "T41K934404 T41K934432
*
*                  ENDIF.
*            ENDIF.




          ENDIF.

        ENDIF.
        FILE2 = P_FILE2.
        FILE5 = P_FILE5.                                    "T41K934404
        FILE6 = P_FILE6. "CH29
        IF P_F4 = 'X'.
          FILE2 = P_FILE4.
        ENDIF.
        PERFORM F_CHECK_CHANGE_PROVINCE.

        BREAK WANTANEE.

        IF R_LOC IS INITIAL.
*          IF R_WM EQ 'X'.
          PERFORM F_GET_DATA_FROM_API.
*          ELSEIF R_SO EQ 'X'.
          PERFORM F_GET_DATA_FROM_API_SONY.
*          ENDIF.
*                 IF check_1100_sony = 'X'. "Add CH30

*                     CALL FUNCTION 'GUI_DOWNLOAD'
*                       EXPORTING
*                         FILENAME              = FILE2
*                         FILETYPE              = 'ASC'
*                         CODEPAGE              = '8600'
*                         TRUNC_TRAILING_BLANKS = SPACE
*                       TABLES
*                         DATA_TAB              = GT_OUTTAB[]
*                       EXCEPTIONS
*                         FILE_OPEN_ERROR       = 1
*                         FILE_READ_ERROR       = 2.

*                 ELSEIF p_f4 = 'X'.
*                     CALL FUNCTION 'GUI_DOWNLOAD'
*                       EXPORTING
*                         filename              = file2
*                         filetype              = 'ASC'
*                         codepage              = '8600'
*                         trunc_trailing_blanks = space
*                       TABLES
*                         data_tab              = gt_outtab[]
*                       EXCEPTIONS
*                         file_open_error       = 1
*                         file_read_error       = 2.
*
*                 ENDIF. "End Add CH30
        ELSE.
          CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
              FILENAME              = FILE2
              FILETYPE              = 'ASC'
              CODEPAGE              = '8600'
              TRUNC_TRAILING_BLANKS = SPACE
            TABLES
              DATA_TAB              = GT_OUTTAB[]
            EXCEPTIONS
              FILE_OPEN_ERROR       = 1
              FILE_READ_ERROR       = 2.
        ENDIF.
                                                            "T41K934432
                                                            "T41K934404
        "CH34 Remove by Wantanee
        "CH35 T41K939532
        IF R_LOC IS  INITIAL.
*          IF CHECK_1800_DIT = 'X'.
*            CALL FUNCTION 'GUI_DOWNLOAD'
*              EXPORTING
*                FILENAME              = FILE5
*                FILETYPE              = 'ASC'
*                CODEPAGE              = '8600'
*                TRUNC_TRAILING_BLANKS = SPACE
*              TABLES
*                DATA_TAB              = GT_OUTTAB[]
*              EXCEPTIONS
*                FILE_OPEN_ERROR       = 1
*                FILE_READ_ERROR       = 2.
*          ENDIF.
          "CH35 T41K939532
          "CH34 End Remove by Wantanee

                                                            "T41K934404

          "Add CH29


*          IF CHECK_1600_SMP = 'X'
*             OR CHECK_1500_JPP = 'X'. "CH3 Add by WAntanee 20230315
*            CALL FUNCTION 'GUI_DOWNLOAD'
*              EXPORTING
*                FILENAME              = FILE6
*                FILETYPE              = 'ASC'
*                CODEPAGE              = '4103' "'8600'
*                TRUNC_TRAILING_BLANKS = SPACE
*              TABLES
*                DATA_TAB              = GT_OUTTAB[]
*              EXCEPTIONS
*                FILE_OPEN_ERROR       = 1
*                FILE_READ_ERROR       = 2.
*          ENDIF.
          "End CH29
        ENDIF.

                                                            "T41K934432

        MESSAGE S001 WITH 'Download Outbound data Completed !'.
      ELSE.
*                 Change by Aphai on Sep 8th, 2014
        IF R_SO EQ 'X'.
          MESSAGE S001 WITH 'No Data Outbound For SONY!'.
        ELSEIF R_WM EQ 'X'.
          MESSAGE S001 WITH 'No Data Outbound For WMS!'.
        ELSE.
          MESSAGE S001 WITH 'No Data Outbound For HITACHI!'.
        ENDIF.
*                end change by aphai on Sep 8th, 2014
      ENDIF.
    ENDIF.
* Comment by aphai on 4.11.2014
*    ELSE.
*      IF gt_outtab[] IS NOT INITIAL.
*        CALL FUNCTION 'GUI_DOWNLOAD'
*          EXPORTING
*            filename              = p_file2
*            filetype              = 'ASC'
*            codepage              = '8600'
*            trunc_trailing_blanks = space
*          TABLES
*            data_tab              = gt_outtab
*          EXCEPTIONS
*            file_open_error       = 1
*            file_read_error       = 2.
*
*        MESSAGE S001 WITH 'Download Outbound data Completed !'.
*      ELSEIF gt_outtab[] IS INITIAL AND gt_outtab4[] IS NOT INITIAL.
*        file2 = p_file2.
*
*        CALL FUNCTION 'GUI_DOWNLOAD'
*          EXPORTING
*            filename              = file2
*            filetype              = 'ASC'
*            codepage              = '8600'
*            trunc_trailing_blanks = space
*          TABLES
*            data_tab              = gt_outtab4
*          EXCEPTIONS
*            file_open_error       = 1
*            file_read_error       = 2.
*
*        MESSAGE S001 WITH 'Download Outbound data Completed !'.
*      ELSE.
**        Change by Aphai on Sep 8th, 2014
*        IF r_so EQ 'X'.
*          MESSAGE S001 WITH 'No Data Outbound For SONY!'.
*        ELSE.
*          MESSAGE S001 WITH 'No Data Outbound For HITACHI!'.
*        ENDIF.
**       end change by aphai on Sep 8th, 2014
*      ENDIF.
*    ENDIF.

    "End Add by Wantanee 20140807

  ENDIF.


* inbound chk file
  IF P_F3 IS NOT INITIAL.
    IF GT_INTAB2[] IS NOT INITIAL.
      FILE3 = P_FILE3.

      CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
        EXPORTING
          I_FIELD_SEPERATOR    = ','
        TABLES
          I_TAB_SAP_DATA       = GT_INTAB2
        CHANGING
          I_TAB_CONVERTED_DATA = L_TEXT
        EXCEPTIONS
          CONVERSION_FAILED    = 1
          OTHERS               = 2.

      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          FILENAME        = FILE3
          FILETYPE        = 'DAT'
        TABLES
          DATA_TAB        = L_TEXT
        EXCEPTIONS
          FILE_OPEN_ERROR = 1
          FILE_READ_ERROR = 2.

*      BOC - Krittin Angchunt 20090121
*      file3 = p_file3.
*
*      CALL FUNCTION 'GUI_DOWNLOAD'
*        EXPORTING
*          filename        = file3
*          filetype        = 'ASC'
*          codepage        = '8600'
*        TABLES
*          data_tab        = gt_intab2
*        EXCEPTIONS
*          file_open_error = 1
*          file_read_error = 2.
*      EOC - Krittin Angchunt 20090121

      MESSAGE S001 WITH 'Download Inbound data for check Completed !'.
    ELSE.
      MESSAGE S001 WITH 'Data for Inbound check not found !'.
    ENDIF.
  ENDIF.

* return outbound file
*  IF p_f4 IS NOT INITIAL.
*    IF gt_outtab3[] IS NOT INITIAL.
*      file4 = p_file4.
*
*      CALL FUNCTION 'GUI_DOWNLOAD'
*        EXPORTING
*          filename        = file4
*          filetype        = 'ASC'
*          codepage        = '8600'
*        TABLES
*          data_tab        = gt_outtab3
*        EXCEPTIONS
*          file_open_error = 1
*          file_read_error = 2.
*
*      MESSAGE S001 WITH 'Download Return Outbound data Completed !'.
*    ELSE.
*      MESSAGE S001 WITH 'Data for Outbound not found !'.
*    ENDIF.
*  ENDIF.

ENDFORM.                    " download_data
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
FORM READ_TEXT  USING    P_ID
                         P_OBJECT
                         P_VBELN
                CHANGING P_VALUE.
  DATA: IT_LINES TYPE STANDARD TABLE OF TLINE.
  DATA: WA_LINES LIKE LINE OF IT_LINES.
  DATA: V_NAME TYPE THEAD-TDNAME.

  CLEAR: P_VALUE, IT_LINES[].

  V_NAME = P_VBELN.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = P_ID
      LANGUAGE                = SY-LANGU
      NAME                    = V_NAME
      OBJECT                  = P_OBJECT
    TABLES
      LINES                   = IT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

  IF SY-SUBRC = 0.
    LOOP AT IT_LINES INTO WA_LINES.
      IF WA_LINES-TDLINE IS NOT INITIAL.
        CONCATENATE P_VALUE WA_LINES-TDLINE INTO P_VALUE.

      ENDIF.

    ENDLOOP.
*    REPLACE ALL OCCURRENCES OF REGEX '(#(?=[^"]*"[^"]*(?:"[^"]*"[^"]*)*$))' IN p_value WITH ''.
*    replace all occurrences of regex
*                    '(\t(?=[^"]*"[^"]*(?:"[^"]*"[^"]*)*$))'
*                    in p_value with ' '.

*    CALL METHOD CL_BSPWD_UTIL=>REPLACE_CRLF_WITH_LF(
*    EXPORTING
*    IV_ORIGINAL_STRING = p_value
*    RECEIVING
*    RV_STRING_WITH_REPLACED_CRLF = p_value ).
*   data cr_lf type c value CL_ABAP_CHAR_UTILITIES=>CR_LF.
*   data cr_lf(2) type c value CL_ABAP_CHAR_UTILITIES=>CR_LF(1).
*   REPLACE ALL OCCURRENCES OF REGEX cr_lf IN p_value WITH ''.
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>NEWLINE IN P_VALUE WITH ''.


  ENDIF.

ENDFORM.                    " READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
FORM READ_TEXT_ITEM  USING    P_ID
                         P_OBJECT
                         P_VBELN
                CHANGING P_VALUE.
  DATA: IT_LINES TYPE STANDARD TABLE OF TLINE.
  DATA: WA_LINES LIKE LINE OF IT_LINES.
  DATA: V_NAME TYPE THEAD-TDNAME.

  CLEAR: P_VALUE, IT_LINES[].

  V_NAME = P_VBELN.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = P_ID
      LANGUAGE                = SY-LANGU
      NAME                    = V_NAME
      OBJECT                  = P_OBJECT
    TABLES
      LINES                   = IT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

  IF SY-SUBRC = 0.
    LOOP AT IT_LINES INTO WA_LINES.
      P_VALUE = WA_LINES-TDLINE.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
FORM MODIFY_SCREEN .

  DATA: LV_TEXT(100) TYPE C.

  LOOP AT SCREEN.
    IF P_F1 = ''.
      IF SCREEN-GROUP1 = 'SA1'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF P_F2 = ''.
      IF SCREEN-GROUP1 = 'SA2'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF P_F3 = ''.
      IF SCREEN-GROUP1 = 'SA3'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF P_F4 = ''.
      IF SCREEN-GROUP1 = 'SA4'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF R_LOC IS INITIAL.
      IF SCREEN-GROUP1 = 'SA2'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
      IF P_F2 = 'X'.
        IF SCREEN-GROUP1 = 'SA2'.
          SCREEN-INPUT = '1'.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
*
    ENDIF.
  ENDLOOP.

  "break wantanee.

  CONCATENATE 'C:\I-'    SY-DATUM '.txt' INTO P_FILE1.
  IF NOT R_LOC IS INITIAL.

    CONCATENATE 'C:\O-'    SY-DATUM '.txt' INTO P_FILE2.
  ELSE.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'SA4'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

    IF P_F2 = 'X' OR P_F4 = 'X'.

*      SELECT VBELN
*      INTO TABLE GT_DO_TEMP
*      FROM LIKP
*      WHERE VBELN IN S_VBELN
*      ORDER BY VBELN.
*
*      LOOP AT GT_DO_TEMP INTO WA_DO_TEMP.
*        IF SY-TABIX EQ 1.
*          LV_TEXT = WA_DO_TEMP-VBELN.
*          TEXT_NAME = WA_DO_TEMP-VBELN.
*        ELSE.
*          CONCATENATE TEXT_NAME WA_DO_TEMP-VBELN+6(4) INTO TEXT_NAME SEPARATED BY ','.
*        ENDIF.
*      ENDLOOP.
*      FILE_NAME = TEXT_NAME.



*      BREAK wantanee.

*      IF P_F2 = 'X'.
*        IF SY-SYSID EQ 'T41'.
*          CONCATENATE '\\172.31.136.37\wh_sony\OUT\DEV\' TEXT_NAME '.txt' INTO P_FILE2.
*          CONCATENATE '\\172.31.136.37\ddd_project\QAS\OUT\DO\' TEXT_NAME '.txt' INTO P_FILE5. "T41K934404 T41K934432
*          CONCATENATE '\\172.31.136.57\uat\OUT\' TEXT_NAME '.txt' INTO P_FILE6. "T41K934404 T41K934432
*
*
*
*        ELSEIF SY-SYSID EQ 'T44'.
*          CONCATENATE '\\172.31.136.37\wh_sony\OUT\QAS\SDS_DO\' TEXT_NAME '.txt' INTO P_FILE2.
*          CONCATENATE '\\172.31.136.37\ddd_project\QAS\OUT\DO\' TEXT_NAME '.txt' INTO P_FILE5. "T41K934404 T41K934432
*          CONCATENATE '\\172.31.136.57\uat\OUT\' TEXT_NAME '.txt' INTO P_FILE6. "T41K934404 T41K934432
*
*        ELSE.
*          CONCATENATE '\\172.31.136.37\wh_sony\OUT\PRD\SDS_DO\' TEXT_NAME '.txt' INTO P_FILE2.
*          CONCATENATE '\\172.31.136.37\ddd_project\PRD\OUT\DO\' TEXT_NAME '.txt' INTO P_FILE5. "T41K934404 T41K934432
*          CONCATENATE '\\172.31.136.51\uat\OUT\' TEXT_NAME '.txt' INTO P_FILE6. "T41K934404 T41K934432
*
*        ENDIF.
*      ELSEIF P_F4 = 'X'.
*        IF SY-SYSID EQ 'T41'.
*          CONCATENATE '\\172.31.136.37\wh_sony\OUT\QAS\SDS_DO\' TEXT_NAME '.txt' INTO P_FILE4.
*        ELSEIF SY-SYSID EQ 'T44'.
*          CONCATENATE '\\172.31.136.37\wh_sony\OUT\QAS\SDS_DO\' TEXT_NAME '.txt' INTO P_FILE4.
*        ELSE.
*          CONCATENATE '\\172.31.136.37\wh_sony\OUT\PRD\SDS_DO\' TEXT_NAME '.txt' INTO P_FILE4.
*        ENDIF.
*
*      ENDIF .
    ENDIF.


  ENDIF.
  CONCATENATE 'C:\I-chk' SY-DATUM '.txt' INTO P_FILE3.
*  CONCATENATE 'C:\O-re'  sy-datum '.txt' INTO p_file4.

ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  GET_PATH_NAME
*&---------------------------------------------------------------------*
FORM GET_PATH_NAME  USING    PATH.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      MASK             = ',*.txt,*.txt.'
      MODE             = 'S'
    IMPORTING
      FILENAME         = PATH
    EXCEPTIONS
      INV_WINSYS       = 01
      NO_BATCH         = 02
      SELECTION_CANCEL = 03
      SELECTION_ERROR  = 04.

ENDFORM.                    " GET_PATH_NAME
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_CHECK
*&---------------------------------------------------------------------*
FORM GET_DATA_CHECK .
  CLEAR: GT_ITAB2.

  SELECT A~VBELN B~POSNR B~MATNR A~LIFEX B~VGBEL B~VGPOS
    INTO TABLE GT_ITAB2
    FROM LIKP AS A INNER JOIN LIPS AS B
      ON A~VBELN = B~VBELN
   WHERE A~VBELN IN S_VBELN
     AND A~BLDAT IN S_BLDAT
     AND B~MEINS <> 'SET'.

* 18* = In bound, 40* = Out bound
  LOOP AT GT_ITAB2 WHERE VBELN(2) = '18'.
*    BOC-20090121 Krittin ไปอ่านที่ header แทน
*    clear: eket.
*    SELECT SINGLE *
*      FROM eket
*     WHERE ebeln = gt_itab2-vgbel
*       AND ebelp = gt_itab2-vgpos.
*    EOC-20090121 Krittin

    CLEAR: EKKO.
    SELECT SINGLE *
      FROM EKKO
     WHERE EBELN = GT_ITAB2-VGBEL.

    CLEAR: MARA.
    SELECT SINGLE *
      FROM MARA
     WHERE MATNR = GT_ITAB2-MATNR.

    CLEAR: SER01, OBJK, GT_SERNR[].
    SELECT B~SERNR
      INTO TABLE GT_SERNR
      FROM SER01 AS A INNER JOIN OBJK AS B
        ON A~OBKNR = B~OBKNR
     WHERE A~LIEF_NR = GT_ITAB2-VBELN
       AND A~POSNR = GT_ITAB2-POSNR.

    IF GT_SERNR[] IS NOT INITIAL.
      LOOP AT GT_SERNR.
        GT_INTAB2-LICHA  = EKKO-UNSEZ.  "eket-licha
        GT_INTAB2-EAN11  = MARA-EAN11.
        GT_INTAB2-MATNR  = GT_ITAB2-MATNR.
        GT_INTAB2-SERNR  = GT_SERNR-SERNR.
        GT_INTAB2-LIFEX  = GT_ITAB2-LIFEX.

        APPEND GT_INTAB2.
      ENDLOOP.
    ELSE.
      GT_INTAB2-LICHA  = EKET-LICHA.
      GT_INTAB2-EAN11  = MARA-EAN11.
      GT_INTAB2-MATNR  = GT_ITAB2-MATNR.
      GT_INTAB2-SERNR  = ''.
      GT_INTAB2-LIFEX  = GT_ITAB2-LIFEX.

      APPEND GT_INTAB2.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_DATA_CHECK
*&---------------------------------------------------------------------*
*&      Form  CHECK_SCREEN_FIELD
*&---------------------------------------------------------------------*
FORM CHECK_SCREEN_FIELD .
  IF P_F1 IS INITIAL AND
     P_F2 IS INITIAL AND
     P_F3 IS INITIAL AND
     P_F4 IS INITIAL.
    MESSAGE E001 WITH 'Please, Select checkbox at least 1 !'.
  ENDIF.

ENDFORM.                    " CHECK_SCREEN_FIELD
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT2
*&---------------------------------------------------------------------*
FORM READ_TEXT2  USING    P_ID
                          P_OBJECT
                          P_VBELN
                          P_UEPOS
                 CHANGING P_VALUE.
  DATA: IT_LINES TYPE STANDARD TABLE OF TLINE.
  DATA: WA_LINES LIKE LINE OF IT_LINES.
  DATA: V_NAME TYPE THEAD-TDNAME.

  CLEAR: P_VALUE, IT_LINES[].

  CONCATENATE P_VBELN P_UEPOS INTO V_NAME.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = P_ID
      LANGUAGE                = SY-LANGU
      NAME                    = V_NAME
      OBJECT                  = P_OBJECT
    TABLES
      LINES                   = IT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

  IF SY-SUBRC = 0.
    LOOP AT IT_LINES INTO WA_LINES.
      IF WA_LINES-TDLINE = 'x' OR WA_LINES-TDLINE = 'X'.
        CLEAR: LIPS.
        SELECT SINGLE *
          FROM LIPS
         WHERE VBELN = P_VBELN
           AND POSNR = P_UEPOS.

        GT_OUTTAB-REMARK_PICHON = 'X'.

        CALL FUNCTION 'ZSD_CONDITION_PICHONKUN'
          EXPORTING
            VKBUR          = LIPS-VKBUR
            FKIMG          = LIPS-LFIMG
          IMPORTING
            PICHONKUN_TEXT = P_VALUE
          EXCEPTIONS
            NOT_FOUND      = 1
            OTHERS         = 2.
      ELSE.
        P_VALUE  = ''.
        GT_OUTTAB-REMARK_PICHON = ''.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " READ_TEXT2
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_RETURN
*&---------------------------------------------------------------------*
FORM GET_DATA_RETURN .
*-T41K908432
  DATA:  LV_PICHON(18) TYPE C.
*-T41K908432

  CLEAR: GT_ITAB3.

  SELECT A~LFART A~VBELN A~LFDAT A~ERDAT
         B~POSNR B~MATNR B~ARKTX B~LGMNG
         A~KUNNR B~UEPOS
    INTO CORRESPONDING FIELDS OF TABLE GT_ITAB3
    FROM LIKP AS A INNER JOIN LIPS AS B
      ON A~VBELN = B~VBELN
   WHERE A~VBELN IN S_VBELN
     AND A~BLDAT IN S_BLDAT
     AND B~MEINS <> 'SET'.

  LOOP AT GT_ITAB3.
*   18* = In bound, 40* = Out bound
    IF GT_ITAB3-VBELN(2) = '42' OR
       GT_IITAB-VBELN(2) = '41'.
      GT_OUTTAB3-LFART = '1'.
      GT_OUTTAB3-CUST_CODE  = 'SDS'.
      GT_OUTTAB3-VBELN      = GT_ITAB3-VBELN.

      GT_OUTTAB3-LFDAT      = GT_ITAB3-LFDAT.
      GT_OUTTAB3-ERDAT      = GT_ITAB3-ERDAT.
      GT_OUTTAB3-POSNR      = GT_ITAB3-POSNR+2(4).

      GT_OUTTAB3-MATNR      = GT_ITAB3-MATNR.

      CLEAR: T001.
      SELECT SINGLE *
        FROM MAKT
       WHERE MATNR = GT_ITAB3-MATNR
         AND SPRAS = 'E'.

      GT_OUTTAB3-ARKTX      = MAKT-MAKTX.

      WRITE GT_ITAB3-LGMNG TO GT_OUTTAB3-LGMNG LEFT-JUSTIFIED DECIMALS 0.

      GT_OUTTAB3-KUNNR      = GT_ITAB3-KUNNR.
      SHIFT GT_OUTTAB3-KUNNR LEFT DELETING LEADING '0'.

      CLEAR: KNA1.
      SELECT SINGLE *
        FROM KNA1
       WHERE KUNNR = GT_ITAB3-KUNNR.

      CONCATENATE KNA1-NAME1 KNA1-NAME2 KNA1-NAME3
             INTO GT_OUTTAB3-CUST_NAME.

      CONCATENATE KNA1-STRAS KNA1-ORT01
             INTO GV_CUST_ADDR.

      GT_OUTTAB3-DELI_TO = GV_CUST_ADDR(60).

      PERFORM READ_TEXT  USING 'ZH10' 'VBBK' GT_ITAB3-VBELN
                     CHANGING GT_OUTTAB3-REMARK.
**
*-T41K908432
***      PERFORM read_text2 USING 'ZI03' 'VBBP' gt_itab3-vbeln gt_itab3-uepos
***                     CHANGING gt_outtab3-pichon.


***ENDLOOP.
***        MOVE: lv_pic_text TO gv_pic_text,
***              lv_pichon1 TO gv_pichon,
***              lv_pic_text2 TO gv_pic_text2,
***              lv_pichon2 TO gv_pichon2.

*-T41K908432

      APPEND GT_OUTTAB3.
    ENDIF.
  ENDLOOP.

  SORT GT_OUTTAB3 BY LFART CUST_CODE VBELN LFDAT ERDAT POSNR MATNR.

ENDFORM.                    " GET_DATA_RETURN
*&---------------------------------------------------------------------*
*&      Form  INSERT_ZTMM_EXPORT_DO
*&      For
*&---------------------------------------------------------------------*
FORM INSERT_ZTMM_EXPORT_DO.
  DATA: LV_RUNID TYPE ZSDSSDT002-RUN_ID.

  IF S_VBELN[] IS NOT INITIAL.
    SELECT VBELN
    INTO TABLE GT_DO_NO
    FROM LIKP
    WHERE VBELN IN S_VBELN.


    LOOP AT GT_DO_NO INTO WA_DO_NO.

      SELECT MAX( RUN_ID )
        INTO LV_RUNID
        FROM ZSDSSDT002
        WHERE VBELN EQ WA_DO_NO-VBELN.


      IF LV_RUNID IS INITIAL.
        WA_ZSDSSDT002-RUN_ID = 1.
      ELSE.
        WA_ZSDSSDT002-RUN_ID = LV_RUNID + 1.
      ENDIF.
      WA_ZSDSSDT002-VBELN = WA_DO_NO-VBELN.
      IF NOT R_MAP IS INITIAL.
        WA_ZSDSSDT002-REQ_MAP      = 'Y'. "Require Map

      ENDIF.
      IF NOT R_INV IS INITIAL.
        WA_ZSDSSDT002-REQ_INV      = 'Y'. "Require Map
      ENDIF.

      IF NOT R_AM IS INITIAL.
        WA_ZSDSSDT002-AM_PM      = 'AM'. "AM
      ENDIF.
      IF NOT R_PM IS INITIAL.
        WA_ZSDSSDT002-AM_PM      = 'PM'. "AM
      ENDIF.
      WA_ZSDSSDT002-ERDAT = SY-DATUM.
      WA_ZSDSSDT002-ERZET = SY-TIMLO.
*                  wa_ZSDSSDT002-erzet = sy-mandt.
      INSERT ZSDSSDT002 FROM WA_ZSDSSDT002.
      COMMIT WORK.
    ENDLOOP.
  ENDIF.
ENDFORM.

***---
*--For C1-250510
*"Add For Case Get Premium from ZSDS_PREMIUM


***TYPES: BEGIN OF ty_vbrp,
***      vbeln LIKE vbrp-vbeln,
***      posnr LIKE vbrp-posnr,
***      uepos LIKE vbrp-uepos,
***      fkimg LIKE vbrp-fkimg,
***      vkbur LIKE vbrp-vkbur,
***      prodh LIKE vbrp-prodh,
***      END OF ty_vbrp.
****---
***
***
***DATA:  lv_tdname  TYPE stxh-tdname,
***       lv_text_pic  TYPE tline-tdline,
***       lv_pic_text TYPE char255,
***       lv_pic_text2 TYPE char255,
***
***
***       lt_vbrp TYPE TABLE OF ty_vbrp,
***       lt_check TYPE TABLE OF ty_check,
***       wa_vbrp TYPE ty_vbrp,
***       wa_check TYPE ty_check,
***       wa_item LIKE LINE OF IS_BIL_INVOICE-IT_GEN,
***       lv_value TYPE ty_premium-value,

***       lv_pichon TYPE vbrp-fkimg,
***       lv_pichon1 TYPE vbrp-fkimg,
***       lv_pichon2 TYPE vbrp-fkimg.
***DATA : lw_item LIKE LINE OF IS_BIL_INVOICE-IT_GEN.
***
***
***CLEAR: lv_pichon, lv_pichon2.
***CLEAR gv_pic_text.
***
***
***
***SELECT vbeln posnr uepos fkimg vkbur prodh
***INTO TABLE lt_vbrp
***FROM vbrp
***FOR ALL ENTRIES IN IS_BIL_INVOICE-IT_GEN
***WHERE vbeln EQ IS_BIL_INVOICE-IT_GEN-bil_number
***AND posnr EQ IS_BIL_INVOICE-IT_GEN-itm_number.
***
***LOOP AT IS_BIL_INVOICE-IT_GEN INTO lw_item WHERE uepos IS INITIAL.
***CLEAR: lv_pichon.
***
***READ TABLE IS_BIL_INVOICE-IT_GEN INTO wa_item WITH KEY bil_number = lw_item-bil_number
***                                          uepos = lw_item-itm_number.
***   IF sy-subrc EQ 0.
***   READ TABLE lt_vbrp INTO wa_vbrp WITH KEY vbeln = lw_item-bil_number
***                                            posnr = lw_item-itm_number.
***
***   IF sy-subrc EQ 0 AND NOT lt_value1[] IS INITIAL.
***   CLEAR: wa_value1, wa_value2, wa_value3.
***   LOOP AT lt_value1 INTO wa_value1 WHERE value = wa_vbrp-vkbur.
***   IF NOT lt_value2[] IS INITIAL.
***   LOOP AT lt_value2 INTO wa_value2 WHERE value = wa_vbrp-prodh(5).
***
***   ENDLOOP.
***   IF NOT lt_value3[] IS INITIAL.
***
***
***   LOOP AT lt_value3 INTO wa_value3 WHERE value = wa_vbrp-prodh+10(8).
***    READ TABLE lt_premium INTO wa_premium WITH KEY value = wa_vbrp-prodh+10(8).
***    lv_pichon = wa_premium-value_c * wa_vbrp-fkimg.
***   ENDLOOP.
***
***   IF sy-subrc EQ 0.
***      IF lv_pic_text IS INITIAL.
***        lv_pic_text = wa_premium-value_p.
***        lv_pichon1 = lv_pichon.
***      ELSEIF lv_pic_text EQ wa_premium-value_p.
***        lv_pichon1 = lv_pichon1 + lv_pichon.
***      ELSEIF lv_pic_text NE wa_premium-value_p AND lv_pic_text2 IS INITIAl.
***        lv_pic_text2  = wa_premium-value_p.
***        lv_pichon2 = lv_pichon.
***      ELSEIF lv_pic_text2 EQ wa_premium-value_p.
***        lv_pichon2 = lv_pichon2 + lv_pichon.
***
***      ENDIF.
***   ENDIF.
***   ENDIF.
***   ENDIF.
***
***   ENDLOOP.
***   ENDIF.
***
***   ENDIF.
***
***
***ENDLOOP.
***        MOVE: lv_pic_text TO gv_pic_text,
***              lv_pichon1 TO gv_pichon,
***              lv_pic_text2 TO gv_pic_text2,
***              lv_pichon2 TO gv_pichon2.
***---
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_CHANGE_PROVINCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CHECK_CHANGE_PROVINCE.

  DATA LS_OUTTAB LIKE LINE OF GT_OUTTAB.

  DATA LV_TABIX TYPE SY-TABIX.

  LOOP AT GT_OUTTAB INTO LS_OUTTAB.
    LV_TABIX = SY-TABIX.

    SEARCH LS_OUTTAB-SHIP_PROVINCE FOR 'จังหวัด'.
    IF SY-SUBRC = 0.
      LS_OUTTAB-SHIP_PROVINCE+SY-FDPOS(7) = SPACE.
    ENDIF.

    SEARCH LS_OUTTAB-SHIP_PROVINCE FOR 'จ.'.
    IF SY-SUBRC = 0.
      LS_OUTTAB-SHIP_PROVINCE+SY-FDPOS(2) = SPACE.
    ENDIF.

    SEARCH LS_OUTTAB-SHIP_PROVINCE FOR 'กรุงเทพ'.
    IF SY-SUBRC = 0.
      CLEAR LS_OUTTAB-SHIP_PROVINCE.
      LS_OUTTAB-SHIP_PROVINCE = 'กรุงเทพมหานคร'.
    ENDIF.

    SEARCH LS_OUTTAB-SHIP_PROVINCE FOR 'อยุธยา'.
    IF SY-SUBRC = 0.
      CLEAR LS_OUTTAB-SHIP_PROVINCE.
      LS_OUTTAB-SHIP_PROVINCE = 'พระนครศรีอยุธยา'.
    ENDIF.

    SEARCH LS_OUTTAB-SHIP_PROVINCE FOR 'กทม'.
    IF SY-SUBRC = 0.
      CLEAR LS_OUTTAB-SHIP_PROVINCE.
      LS_OUTTAB-SHIP_PROVINCE = 'กรุงเทพมหานคร'.
    ENDIF.

    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_OUTTAB-SHIP_PROVINCE WITH ''.
    MODIFY GT_OUTTAB FROM LS_OUTTAB INDEX LV_TABIX
                             TRANSPORTING SHIP_PROVINCE.

    CLEAR LS_OUTTAB.
  ENDLOOP.

ENDFORM.                    " F_CHECK_CHANGE_PROVINCE


*&---------------------------------------------------------------------*
*&      Form  get_bom_material_group_at
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_MAT_AT   text
*      -->GT_VBAP    text
*----------------------------------------------------------------------*
FORM GET_BOM_MATERIAL_GROUP_AT TABLES
                                  I_MAT_AT STRUCTURE I_MAT_AT.
  .
  CHECK : I_MAT_AT[] IS INITIAL.

  SELECT MAST~MATNR
         STPO~IDNRK
    INTO TABLE I_MAT_AT
    FROM STKO
    INNER JOIN STPO ON STPO~STLNR EQ STKO~STLNR  AND STPO~STLTY EQ STKO~STLTY
    INNER JOIN MAST ON MAST~STLNR EQ STKO~STLNR AND MAST~STLAL EQ STKO~STLAL
    FOR ALL ENTRIES IN GT_IITAB
    WHERE STPO~IDNRK EQ GT_IITAB-MATNR
    AND MAST~STLAN EQ '5'
    AND MAST~WERKS EQ '1000'.

  SORT I_MAT_AT BY MATNR IDNRK.
  DELETE ADJACENT DUPLICATES FROM I_MAT_AT COMPARING ALL FIELDS.
  DELETE I_MAT_AT WHERE MATNR+0(3) NE 'SAT'.
  SORT I_MAT_AT BY IDNRK.

* End Aphai 26.11.2014

ENDFORM.                    "get_bom_material_group_at
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
"Add by Wantanee 20150722
*&---------------------------------------------------------------------*
*&      Form  read_text_head
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ID       text
*      -->P_OBJECT   text
*      -->P_VBELN    text
*      -->P_VALUE    text
*----------------------------------------------------------------------*
FORM READ_TEXT_HEAD  USING    P_ID
                         P_OBJECT
                         P_VBELN
                CHANGING P_VALUE.

  DATA: IT_LINES TYPE STANDARD TABLE OF TLINE.
  DATA: WA_LINES LIKE LINE OF IT_LINES.
  DATA: V_NAME TYPE THEAD-TDNAME.

  CLEAR: P_VALUE, IT_LINES[].

  V_NAME = P_VBELN.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = P_ID
      LANGUAGE                = SY-LANGU
      NAME                    = V_NAME
      OBJECT                  = P_OBJECT
    TABLES
      LINES                   = IT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

  IF SY-SUBRC = 0.
    LOOP AT IT_LINES INTO WA_LINES.
      CONCATENATE P_VALUE  WA_LINES-TDLINE INTO P_VALUE.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " READ_TEXT
*&---------------------------------------------------------------------*
*& Form f_get_data_from_api
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_DATA_FROM_API.
  DATA : LCL_API TYPE REF TO ZCL_SDSSD_SEND_API.

  DATA : LS_DATA TYPE ZSDSMMS040,
         LT_DATA TYPE TABLE OF ZSDSMMS040.

  DATA: BEGIN OF LS_RETURN,
          SUCCESS      TYPE STRING,
          MESSAGE      TYPE STRING,
          ERROR_CODE   TYPE STRING,
          MESSAGE_CODE TYPE STRING,
          DATA         TYPE STRING,
          NEXT_CONTROL TYPE STRING,
          ORDER_NUMBER TYPE STRING,
          SHOW_MESSAGE TYPE STRING,
          PAGER        TYPE STRING,
        END OF LS_RETURN.

  DATA : LV_MESTYPE TYPE CHAR1.

  DATA : LS_OUTTAB LIKE LINE OF GT_OUTTAB.

  IF LCL_API IS NOT BOUND.
    CREATE OBJECT LCL_API.
  ENDIF.

  LOOP AT GT_OUTTAB INTO LS_OUTTAB.
    LS_DATA-LFART           = LS_OUTTAB-LFART.
    LS_DATA-CUST_CODE       = LS_OUTTAB-CUST_CODE.
    LS_DATA-VBELN           = LS_OUTTAB-VBELN.
    LS_DATA-LFDAT           = LS_OUTTAB-LFDAT.
    LS_DATA-ERDAT           = LS_OUTTAB-ERDAT.
    LS_DATA-POSNR           = LS_OUTTAB-POSNR.
    LS_DATA-MATNR           = LS_OUTTAB-MATNR.
    LS_DATA-ARKTX           = LS_OUTTAB-ARKTX.
    LS_DATA-LGMNG           = LS_OUTTAB-LGMNG.
    LS_DATA-KUNNR           = LS_OUTTAB-KUNNR.
    LS_DATA-CUST_NAME       = LS_OUTTAB-CUST_NAME.
    LS_DATA-DELI_TO         = LS_OUTTAB-DELI_TO.
    LS_DATA-REMARK          = LS_OUTTAB-REMARK.
    LS_DATA-PICHON          = LS_OUTTAB-PICHON.
    LS_DATA-REMARK_PICHON   = LS_OUTTAB-REMARK_PICHON.
    LS_DATA-LGORT           = LS_OUTTAB-LGORT.
    LS_DATA-WARRANTY        = LS_OUTTAB-WARRANTY.
    LS_DATA-SO              = LS_OUTTAB-SO.
    LS_DATA-REQ_MAP         = LS_OUTTAB-REQ_MAP.
    LS_DATA-REQ_INV         = LS_OUTTAB-REQ_INV.
    LS_DATA-SHIP_ADDR       = LS_OUTTAB-SHIP_ADDR.
    LS_DATA-SHIP_PROVINCE   = LS_OUTTAB-SHIP_PROVINCE.
    LS_DATA-POSTCODE        = LS_OUTTAB-POSTCODE.
    LS_DATA-AM_PM           = LS_OUTTAB-AM_PM.
    LS_DATA-LOADING_POINT   = LS_OUTTAB-LOADING_POINT.
    LS_DATA-SALESMAN        = LS_OUTTAB-SALESMAN.
    LS_DATA-DUE_DATE        = LS_OUTTAB-DUE_DATE.
    LS_DATA-PO_NO           = LS_OUTTAB-PO_NO.
    LS_DATA-PROJECT         = LS_OUTTAB-PROJECT.
    LS_DATA-TERM_OF_PAYMENT = LS_OUTTAB-TERM_OF_PAYMENT.
    LS_DATA-CONTACT_NAME    = LS_OUTTAB-CONTACT_NAME.
    LS_DATA-REF_INV         = LS_OUTTAB-REF_INV.
    LS_DATA-SALES_DIV       = LS_OUTTAB-SALES_DIV.
    LS_DATA-SALES_OFFICE    = LS_OUTTAB-SALES_OFFICE.
    LS_DATA-SALES_GROUP     = LS_OUTTAB-SALES_GROUP.
    LS_DATA-UOM             = LS_OUTTAB-UOM.
    LS_DATA-DOC_FLAG        = LS_OUTTAB-DOC_FLAG.
    LS_DATA-MHA             = LS_OUTTAB-MHA.
    LS_DATA-FLAG_BOM        = LS_OUTTAB-FLAG_BOM.
    LS_DATA-REFER_BOM       = LS_OUTTAB-REFER_BOM.
    LS_DATA-INSULATION      = LS_OUTTAB-INSULATION.
    LS_DATA-SPSOLC          = LS_OUTTAB-SPSOLC.
    APPEND LS_DATA TO LT_DATA.
    CLEAR LS_DATA.
  ENDLOOP.

  LCL_API->SEND_DO_TO_WMS( EXPORTING IT_DATA   = LT_DATA
                           IMPORTING E_MESTYPE = LV_MESTYPE
                           CHANGING  C_RETURN  = LS_RETURN ).

  IF LS_RETURN-SUCCESS EQ ABAP_TRUE.
    MESSAGE S001 WITH TEXT-S01.
  ELSE.
    MESSAGE S001 WITH TEXT-E01 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


*  DATA: LV_URL              TYPE STRING,
*        LV_METHOD           TYPE STRING,
*        LV_BODY_TEXT        TYPE STRING,
*        LV_BODY_BIN         TYPE XSTRING,
*        LV_LEN              TYPE I,
*        LV_LEN_BIN          TYPE I,
*        LV_RETURN_BODY_TEXT TYPE STRING,
*        LV_RETURN_BODY_BIN  TYPE XSTRING,
*        LV_MESSAGE          TYPE STRING,
*        LV_STATUS           TYPE C.
*
*  DATA: LT_HEADER TYPE ZSDSCAS001_TT.
*
*  DATA: BEGIN OF LS_DATA,
*          SUCCESS      TYPE STRING,
*          MESSAGE      TYPE STRING,
*          ERROR_CODE   TYPE STRING,
*          MESSAGE_CODE TYPE STRING,
*          DATA         TYPE STRING,
*          NEXT_CONTROL TYPE STRING,
*          ORDER_NUMBER TYPE STRING,
*          SHOW_MESSAGE TYPE STRING,
*          PAGER        TYPE STRING,
*        END OF LS_DATA.
*
*  LV_METHOD = 'POST'.
*
*  PERFORM F_GET_END_POINT CHANGING LV_URL.
*
*  PERFORM F_GET_HEADER CHANGING LT_HEADER.
*
*  PERFORM F_GET_BODY CHANGING LV_BODY_TEXT.
*
*  PERFORM F_GET_LEN  USING LV_BODY_TEXT
*                  CHANGING LV_LEN.
*
*  CALL METHOD ZCL_SDSCA_CAL_API=>CALL_API
*    EXPORTING
*      I_URL              = LV_URL
*      I_METHOD           = LV_METHOD
*      I_HEADER           = LT_HEADER
*      I_BODY_TEXT        = LV_BODY_TEXT
*      I_BODY_BIN         = LV_BODY_BIN
*      I_LEN              = LV_LEN
*      I_LEN_BIN          = LV_LEN_BIN
*    IMPORTING
*      E_RETURN           = LS_DATA
*      E_RETURN_BODY_TEXT = LV_RETURN_BODY_TEXT
*      E_RETURN_BODY_BIN  = LV_RETURN_BODY_BIN
*      E_MESSAGE          = LV_MESSAGE
*      E_STATUS           = LV_STATUS.
*  IF LV_STATUS = 'S'.
*
*  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GET_BODY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> UV_START_DATE
*&      --> UV_END_DATE
*&      <-- LV_BODY_TEXT
*&---------------------------------------------------------------------*
FORM F_GET_BODY CHANGING LV_BODY_TEXT  TYPE STRING.

  DATA : BEGIN OF LS_DATA,
           LFART           TYPE C LENGTH 1,
           CUST_CODE       TYPE C LENGTH 3,
           VBELN           TYPE C LENGTH 10,
           LFDAT           TYPE C LENGTH 8,
           ERDAT           TYPE C LENGTH 8,
           POSNR           TYPE C LENGTH 4,
           MATNR           TYPE C LENGTH 18,
           ARKTX           TYPE C LENGTH 40,
           LGMNG           TYPE C LENGTH 8,
           KUNNR           TYPE C LENGTH 10,
           CUST_NAME       TYPE C LENGTH 60,
           DELI_TO         TYPE C LENGTH 60,
           REMARK          TYPE C LENGTH 250,
           PICHON          TYPE C LENGTH 250,
           REMARK_PICHON   TYPE C LENGTH 1,
           LGORT           TYPE C LENGTH 10,
           WARRANTY        TYPE C LENGTH 1,
           SO              TYPE C LENGTH 10,
           REQ_MAP         TYPE C LENGTH 1,
           REQ_INV         TYPE C LENGTH 1,
           SHIP_ADDR       TYPE C LENGTH 220,
           SHIP_PROVINCE   TYPE C LENGTH 30,
           POSTCODE        TYPE C LENGTH 5,
           AM_PM           TYPE C LENGTH 2,
           LOADING_POINT   TYPE C LENGTH 20,
           SALESMAN        TYPE C LENGTH 40,
           DUE_DATE        TYPE C LENGTH 8,
           PO_NO           TYPE C LENGTH 35,
           PROJECT         TYPE C LENGTH 40,
           TERM_OF_PAYMENT TYPE C LENGTH 4,
           CONTACT_NAME    TYPE C LENGTH 35,
           REF_INV         TYPE C LENGTH 10,
           SALES_DIV       TYPE C LENGTH 2,
           SALES_OFFICE    TYPE C LENGTH 20,
           SALES_GROUP     TYPE C LENGTH 20,
           UOM             TYPE C LENGTH 5,
           DOC_FLAG        TYPE C LENGTH 1,
           MHA             TYPE C LENGTH 1,
           FLAG_BOM        TYPE C LENGTH 1,
           REFER_BOM       TYPE C LENGTH 4,
           INSULATION      TYPE C LENGTH 10,
           SPSOLC          TYPE C LENGTH 6,
         END OF LS_DATA.

  DATA : BEGIN OF LT_DATA,
           RESULT LIKE TABLE OF LS_DATA,
         END OF LT_DATA.

  DATA : LS_OUTTAB LIKE LINE OF GT_OUTTAB.

  LOOP AT GT_OUTTAB INTO LS_OUTTAB.

    LS_DATA-LFART           = LS_OUTTAB-LFART.
    LS_DATA-CUST_CODE       = LS_OUTTAB-CUST_CODE.
    LS_DATA-VBELN           = LS_OUTTAB-VBELN.
    LS_DATA-LFDAT           = LS_OUTTAB-LFDAT.
    LS_DATA-ERDAT           = LS_OUTTAB-ERDAT.
    LS_DATA-POSNR           = LS_OUTTAB-POSNR.
    LS_DATA-MATNR           = LS_OUTTAB-MATNR.
    LS_DATA-ARKTX           = LS_OUTTAB-ARKTX.
    LS_DATA-LGMNG           = LS_OUTTAB-LGMNG.
    LS_DATA-KUNNR           = LS_OUTTAB-KUNNR.
    LS_DATA-CUST_NAME       = LS_OUTTAB-CUST_NAME.
    LS_DATA-DELI_TO         = LS_OUTTAB-DELI_TO.
    LS_DATA-REMARK          = LS_OUTTAB-REMARK.
    LS_DATA-PICHON          = LS_OUTTAB-PICHON.
    LS_DATA-REMARK_PICHON   = LS_OUTTAB-REMARK_PICHON.
    LS_DATA-LGORT           = LS_OUTTAB-LGORT.
    LS_DATA-WARRANTY        = LS_OUTTAB-WARRANTY.
    LS_DATA-SO              = LS_OUTTAB-SO.
    LS_DATA-REQ_MAP         = LS_OUTTAB-REQ_MAP.
    LS_DATA-REQ_INV         = LS_OUTTAB-REQ_INV.
    LS_DATA-SHIP_ADDR       = LS_OUTTAB-SHIP_ADDR.
    LS_DATA-SHIP_PROVINCE   = LS_OUTTAB-SHIP_PROVINCE.
    LS_DATA-POSTCODE        = LS_OUTTAB-POSTCODE.
    LS_DATA-AM_PM           = LS_OUTTAB-AM_PM.
    LS_DATA-LOADING_POINT   = LS_OUTTAB-LOADING_POINT.
    LS_DATA-SALESMAN        = LS_OUTTAB-SALESMAN.
    LS_DATA-DUE_DATE        = LS_OUTTAB-DUE_DATE.
    LS_DATA-PO_NO           = LS_OUTTAB-PO_NO.
    LS_DATA-PROJECT         = LS_OUTTAB-PROJECT.
    LS_DATA-TERM_OF_PAYMENT = LS_OUTTAB-TERM_OF_PAYMENT.
    LS_DATA-CONTACT_NAME    = LS_OUTTAB-CONTACT_NAME.
    LS_DATA-REF_INV         = LS_OUTTAB-REF_INV.
    LS_DATA-SALES_DIV       = LS_OUTTAB-SALES_DIV.
    LS_DATA-SALES_OFFICE    = LS_OUTTAB-SALES_OFFICE.
    LS_DATA-SALES_GROUP     = LS_OUTTAB-SALES_GROUP.
    LS_DATA-UOM             = LS_OUTTAB-UOM.
    LS_DATA-DOC_FLAG        = LS_OUTTAB-DOC_FLAG.
    LS_DATA-MHA             = LS_OUTTAB-MHA.
    LS_DATA-FLAG_BOM        = LS_OUTTAB-FLAG_BOM.
    LS_DATA-REFER_BOM       = LS_OUTTAB-REFER_BOM.
    LS_DATA-INSULATION      = LS_OUTTAB-INSULATION.
    LS_DATA-SPSOLC          = LS_OUTTAB-SPSOLC.

    APPEND LS_DATA TO LT_DATA-RESULT.

  ENDLOOP.

  CALL METHOD /UI2/CL_JSON=>SERIALIZE
    EXPORTING
      DATA        = LT_DATA-RESULT
      PRETTY_NAME = /UI2/CL_JSON=>PRETTY_MODE-LOW_CASE
    RECEIVING
      R_JSON      = LV_BODY_TEXT
    EXCEPTIONS
      OTHERS      = 1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_LEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_BODY_TEXT
*&      <-- LV_LEN
*&---------------------------------------------------------------------*
FORM F_GET_LEN  USING    LV_BODY_TEXT
                CHANGING LV_LEN.
  LV_LEN = STRLEN( LV_BODY_TEXT ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_HEADER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_HEADER
*&---------------------------------------------------------------------*
FORM F_GET_HEADER  CHANGING LT_HEADER TYPE ZSDSCAS001_TT.

  DATA : LS_HEADER LIKE LINE OF LT_HEADER.

  LS_HEADER-NAME  = 'Content-Type'.
  LS_HEADER-VALUE = 'application/json'.
  APPEND LS_HEADER TO LT_HEADER.
  CLEAR LS_HEADER.

  LS_HEADER-NAME  = 'Accept'.
  LS_HEADER-VALUE = 'application/json'.
  APPEND LS_HEADER TO LT_HEADER.
  CLEAR LS_HEADER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_END_POINT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_URL
*&---------------------------------------------------------------------*
FORM F_GET_END_POINT CHANGING LV_URL.
  LV_URL = 'http://172.31.136.250:8081/api/v1.0/S4HANA/AddOrderFromDO'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_DATA_FROM_API_SONY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_DATA_FROM_API_SONY .

  IF     R_FG EQ ABAP_TRUE.
    PERFORM F_SEND_DATA_FG.
  ELSEIF R_SP EQ ABAP_TRUE.
    PERFORM F_SEND_DATA_SP.
  ENDIF.

*  DATA: LV_URL              TYPE STRING,
*        LV_METHOD           TYPE STRING,
*        LV_BODY_TEXT        TYPE STRING,
*        LV_BODY_BIN         TYPE XSTRING,
*        LV_LEN              TYPE I,
*        LV_LEN_BIN          TYPE I,
*        LV_RETURN_BODY_TEXT TYPE STRING,
*        LV_RETURN_BODY_BIN  TYPE XSTRING,
*        LV_MESSAGE          TYPE STRING,
*        LV_STATUS           TYPE C.
*
*  DATA: LT_HEADER TYPE ZSDSCAS001_TT.
*
*  DATA: BEGIN OF LS_DATA,
*          SUCCESS      TYPE STRING,
*          MESSAGE      TYPE STRING,
*          ERROR_CODE   TYPE STRING,
*          MESSAGE_CODE TYPE STRING,
*          DATA         TYPE STRING,
*          NEXT_CONTROL TYPE STRING,
*          ORDER_NUMBER TYPE STRING,
*          SHOW_MESSAGE TYPE STRING,
*          PAGER        TYPE STRING,
*        END OF LS_DATA.
*
*  DATA : LV_TOKEN TYPE STRING.
*
*  PERFORM F_GET_TOKEN CHANGING LV_TOKEN.
*
*  LV_METHOD = 'POST'.
*
*  PERFORM F_GET_HEADER_SONY USING LV_TOKEN
*                         CHANGING LT_HEADER.
*  IF R_FG EQ ABAP_TRUE.
*    PERFORM F_GET_END_POINT_SONY CHANGING LV_URL.
*    PERFORM F_GET_BODY CHANGING LV_BODY_TEXT.
*  ELSEIF R_SP EQ ABAP_TRUE.
*    PERFORM F_GET_END_POINT_SONY_SP CHANGING LV_URL.
*    PERFORM F_GET_BODY_SP CHANGING LV_BODY_TEXT.
*  ENDIF.
*  PERFORM F_GET_LEN  USING LV_BODY_TEXT
*                  CHANGING LV_LEN.
*
*  CALL METHOD ZCL_SDSCA_CAL_API=>CALL_API
*    EXPORTING
*      I_URL              = LV_URL
*      I_METHOD           = LV_METHOD
*      I_HEADER           = LT_HEADER
*      I_BODY_TEXT        = LV_BODY_TEXT
*      I_BODY_BIN         = LV_BODY_BIN
*      I_LEN              = LV_LEN
*      I_LEN_BIN          = LV_LEN_BIN
*    IMPORTING
*      E_RETURN           = LS_DATA
*      E_RETURN_BODY_TEXT = LV_RETURN_BODY_TEXT
*      E_RETURN_BODY_BIN  = LV_RETURN_BODY_BIN
*      E_MESSAGE          = LV_MESSAGE
*      E_STATUS           = LV_STATUS.
*  IF LV_STATUS = 'S'.
*
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_END_POINT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_URL
*&---------------------------------------------------------------------*
FORM F_GET_END_POINT_SONY CHANGING LV_URL.
  LV_URL = 'https://dev-exchain.ilogisoft.com:4443/sds/interface/do/fg'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_HEADER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_HEADER
*&---------------------------------------------------------------------*
FORM F_GET_HEADER_SONY  USING LF_TOKEN
                     CHANGING LT_HEADER TYPE ZSDSCAS001_TT.

  DATA : LS_HEADER LIKE LINE OF LT_HEADER.

  DATA : LV_TOKEN TYPE STRING.

  LS_HEADER-NAME  = 'Content-Type'.
  LS_HEADER-VALUE = 'application/json'.
  APPEND LS_HEADER TO LT_HEADER.
  CLEAR LS_HEADER.

  LS_HEADER-NAME  = 'Accept'.
  LS_HEADER-VALUE = 'application/json'.
  APPEND LS_HEADER TO LT_HEADER.
  CLEAR LS_HEADER.

  CONCATENATE 'Bearer'
              LF_TOKEN
         INTO LV_TOKEN SEPARATED BY SPACE.

  LS_HEADER-NAME  = 'Authorization'.
  LS_HEADER-VALUE = LV_TOKEN.
  APPEND LS_HEADER TO LT_HEADER.
  CLEAR LS_HEADER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_BODY_SP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_BODY_TEXT
*&---------------------------------------------------------------------*
FORM F_GET_BODY_SP  CHANGING LV_BODY_TEXT  TYPE STRING.

  DATA : BEGIN OF LS_DATA,
           LFART         TYPE C LENGTH 1,
           CUST_CODE     TYPE C LENGTH 3,
           VBELN         TYPE C LENGTH 10,
           LFDAT         TYPE C LENGTH 8,
           ERDAT         TYPE C LENGTH 8,
           POSNR         TYPE C LENGTH 4,
           MATNR         TYPE C LENGTH 18,
           ARKTX         TYPE C LENGTH 40,
           LGMNG         TYPE C LENGTH 8,
           KUNNR         TYPE C LENGTH 10,
           CUST_NAME     TYPE C LENGTH 60,
           DELI_TO       TYPE C LENGTH 60,
           REMARK        TYPE C LENGTH 250,
           PICHON        TYPE C LENGTH 250,
           REMARK_PICHON TYPE C LENGTH 1,
           LGORT         TYPE C LENGTH 10,

         END OF LS_DATA.

  DATA : BEGIN OF LT_DATA,
           RESULT LIKE TABLE OF LS_DATA,
         END OF LT_DATA.

  DATA : LS_OUTTAB LIKE LINE OF GT_OUTTAB.

  LOOP AT GT_OUTTAB INTO LS_OUTTAB.

    LS_DATA-LFART           = LS_OUTTAB-LFART.
    LS_DATA-CUST_CODE       = LS_OUTTAB-CUST_CODE.
    LS_DATA-VBELN           = LS_OUTTAB-VBELN.
    LS_DATA-LFDAT           = LS_OUTTAB-LFDAT.
    LS_DATA-ERDAT           = LS_OUTTAB-ERDAT.
    LS_DATA-POSNR           = LS_OUTTAB-POSNR.
    LS_DATA-MATNR           = LS_OUTTAB-MATNR.
    LS_DATA-ARKTX           = LS_OUTTAB-ARKTX.
    LS_DATA-LGMNG           = LS_OUTTAB-LGMNG.
    LS_DATA-KUNNR           = LS_OUTTAB-KUNNR.
    LS_DATA-CUST_NAME       = LS_OUTTAB-CUST_NAME.
    LS_DATA-DELI_TO         = LS_OUTTAB-DELI_TO.
    LS_DATA-REMARK          = LS_OUTTAB-REMARK.
    LS_DATA-PICHON          = LS_OUTTAB-PICHON.
    LS_DATA-REMARK_PICHON   = LS_OUTTAB-REMARK_PICHON.
    LS_DATA-LGORT           = LS_OUTTAB-LGORT.

    APPEND LS_DATA TO LT_DATA-RESULT.

  ENDLOOP.

  CALL METHOD /UI2/CL_JSON=>SERIALIZE
    EXPORTING
      DATA        = LT_DATA-RESULT
      PRETTY_NAME = /UI2/CL_JSON=>PRETTY_MODE-LOW_CASE
    RECEIVING
      R_JSON      = LV_BODY_TEXT
    EXCEPTIONS
      OTHERS      = 1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_END_POINT_SONY_SP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_URL
*&---------------------------------------------------------------------*
FORM F_GET_END_POINT_SONY_SP  CHANGING LV_URL.
  LV_URL = 'https://dev-exchain.ilogisoft.com:4443/sds/interface/do/sp'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_TOKEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_TOKEN
*&---------------------------------------------------------------------*
FORM F_GET_TOKEN CHANGING LV_TOKEN.
  DATA: LV_URL              TYPE STRING,
        LV_METHOD           TYPE STRING,
        LV_BODY_TEXT        TYPE STRING,
        LV_BODY_BIN         TYPE XSTRING,
        LV_LEN              TYPE I,
        LV_LEN_BIN          TYPE I,
        LV_RETURN_BODY_TEXT TYPE STRING,
        LV_RETURN_BODY_BIN  TYPE XSTRING,
        LV_MESSAGE          TYPE STRING,
        LV_STATUS           TYPE C.

  DATA: LT_HEADER TYPE ZSDSCAS001_TT.

  DATA: BEGIN OF LS_DATA,
          SUCCESS      TYPE STRING,
          MESSAGE      TYPE STRING,
          ERROR_CODE   TYPE STRING,
          MESSAGE_CODE TYPE STRING,
          DATA         TYPE STRING,
          NEXT_CONTROL TYPE STRING,
          ORDER_NUMBER TYPE STRING,
          SHOW_MESSAGE TYPE STRING,
          PAGER        TYPE STRING,
        END OF LS_DATA.

  LV_METHOD = 'POST'.

  PERFORM F_GET_HEADER_SONY_TOKEN CHANGING LT_HEADER.

  PERFORM F_GET_END_POINT_TOKEN CHANGING LV_URL.

  PERFORM F_GET_BODY_TOKEN CHANGING LV_BODY_TEXT.

  PERFORM F_GET_LEN  USING LV_BODY_TEXT
                  CHANGING LV_LEN.

  CALL METHOD ZCL_SDSCA_CAL_API=>CALL_API
    EXPORTING
      I_URL              = LV_URL
      I_METHOD           = LV_METHOD
      I_HEADER           = LT_HEADER
      I_BODY_TEXT        = LV_BODY_TEXT
      I_BODY_BIN         = LV_BODY_BIN
      I_LEN              = LV_LEN
      I_LEN_BIN          = LV_LEN_BIN
    IMPORTING
      E_RETURN           = LS_DATA
      E_RETURN_BODY_TEXT = LV_RETURN_BODY_TEXT
      E_RETURN_BODY_BIN  = LV_RETURN_BODY_BIN
      E_MESSAGE          = LV_MESSAGE
      E_STATUS           = LV_STATUS.
  IF LV_STATUS = 'S'.
    LV_TOKEN = LS_DATA.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_HEADER_SONY_TOKEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_HEADER
*&---------------------------------------------------------------------*
FORM F_GET_HEADER_SONY_TOKEN  CHANGING LT_HEADER TYPE ZSDSCAS001_TT.

  DATA : LS_HEADER LIKE LINE OF LT_HEADER.

  LS_HEADER-NAME  = 'Content-Type'.
  LS_HEADER-VALUE = 'application/json'.
  APPEND LS_HEADER TO LT_HEADER.
  CLEAR LS_HEADER.

  LS_HEADER-NAME  = 'Accept'.
  LS_HEADER-VALUE = 'application/json'.
  APPEND LS_HEADER TO LT_HEADER.
  CLEAR LS_HEADER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_END_POINT_TOKEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_URL
*&---------------------------------------------------------------------*
FORM F_GET_END_POINT_TOKEN  CHANGING LV_URL.
  LV_URL = 'https://dev-exchain.ilogisoft.com:4443/sds/interface/do/fg'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_BODY_TOKEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_BODY_TEXT
*&---------------------------------------------------------------------*
FORM F_GET_BODY_TOKEN  CHANGING LV_BODY_TEXT  TYPE STRING.

  DATA : BEGIN OF LS_DATA,
           REFRESH_TOKEN TYPE STRING,
         END OF LS_DATA.

  LS_DATA-REFRESH_TOKEN = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6Ijo6MSIsImlhdCI6MTcyMTE4NDQyNCwiZXhwIjoxNzIxNzg5MjI0fQ.MFShY4ZL8zcqadZNZcrK9no0-v6QST2lRK3tHBNBJFo'.
  CALL METHOD /UI2/CL_JSON=>SERIALIZE
    EXPORTING
      DATA        = LS_DATA
      PRETTY_NAME = /UI2/CL_JSON=>PRETTY_MODE-LOW_CASE
    RECEIVING
      R_JSON      = LV_BODY_TEXT
    EXCEPTIONS
      OTHERS      = 1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SEND_DATA_FG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_SEND_DATA_FG .
  DATA : LCL_API TYPE REF TO ZCL_SDSSD_SEND_API.

  DATA : LS_DATA TYPE ZSDSMMS042,
         LT_DATA TYPE TABLE OF ZSDSMMS042.

  DATA: BEGIN OF LS_RETURN,
          IS_SUCCESS TYPE STRING,
          STATUS     TYPE P DECIMALS 0,
          MESSAGE    TYPE STRING,
        END OF LS_RETURN.

  DATA : LV_MESTYPE TYPE CHAR1.

  DATA : LS_OUTTAB LIKE LINE OF GT_OUTTAB.

  DATA : LR_WARANTY_GROUP TYPE RANGE OF ZSDSCAC001-VALUE_LOW.

  DATA : LCL_FORM TYPE REF TO ZCL_SDSSD_GEN_DATA_SD_FORM.

  DATA : LT_WARRANTY TYPE ZSDSSDS113_TT,
         LS_WARRANTY TYPE ZSDSSDS113.

  DATA : LT_TMP LIKE LT_WARRANTY.

  DATA : LT_TMP_DO LIKE GT_OUTTAB[].

  DATA : LV_POSNR LIKE LS_WARRANTY-POSNR.

  IF LCL_API IS NOT BOUND.
    CREATE OBJECT LCL_API.
  ENDIF.

  IF LCL_FORM IS NOT BOUND.
    CREATE OBJECT LCL_FORM.
  ENDIF.

  LT_TMP_DO = GT_OUTTAB[].
  SORT LT_TMP_DO BY VBELN.
  DELETE ADJACENT DUPLICATES FROM LT_TMP_DO COMPARING VBELN.

  LOOP AT LT_TMP_DO INTO DATA(LS_TMP_DO).
    LT_TMP = LCL_FORM->GET_LINE_ITEM_WARRANTY( LS_TMP_DO-VBELN ).
    APPEND LINES OF LT_TMP TO LT_WARRANTY.
  ENDLOOP.

*  PERFORM F_GET_CONFIG TABLES LR_WARANTY_GROUP.

  LOOP AT GT_OUTTAB INTO LS_OUTTAB.
    LS_DATA-LFART           = LS_OUTTAB-LFART.
    LS_DATA-CUST_CODE       = LS_OUTTAB-CUST_CODE.
    LS_DATA-VBELN           = LS_OUTTAB-VBELN.
    LS_DATA-LFDAT           = LS_OUTTAB-LFDAT.
    LS_DATA-ERDAT           = LS_OUTTAB-ERDAT.
    LS_DATA-POSNR           = LS_OUTTAB-POSNR.
    LS_DATA-MATNR           = LS_OUTTAB-MATNR.
    LS_DATA-ARKTX           = LS_OUTTAB-ARKTX.
    LS_DATA-LGMNG           = LS_OUTTAB-LGMNG.
    LS_DATA-KUNNR           = LS_OUTTAB-KUNNR.
    LS_DATA-CUST_NAME       = LS_OUTTAB-CUST_NAME.
    LS_DATA-DELI_TO         = LS_OUTTAB-DELI_TO.
    LS_DATA-REMARK          = LS_OUTTAB-REMARK.
    LS_DATA-PICHON          = LS_OUTTAB-PICHON.
    LS_DATA-REMARK_PICHON   = LS_OUTTAB-REMARK_PICHON.
    LS_DATA-LGORT           = LS_OUTTAB-LGORT.

    LV_POSNR = LS_DATA-POSNR.
    LV_POSNR = |{ LV_POSNR ALPHA = IN }|.
    READ TABLE LT_WARRANTY INTO LS_WARRANTY
    WITH KEY VBELN = LS_DATA-VBELN
             POSNR = LV_POSNR.
    IF SY-SUBRC EQ 0.
      LS_DATA-WARRANTY      = 'W'.
    ELSE.
      LS_DATA-WARRANTY      = LS_OUTTAB-WARRANTY.
    ENDIF.
    LS_DATA-SO              = LS_OUTTAB-SO.
    LS_DATA-REQ_MAP         = LS_OUTTAB-REQ_MAP.
    LS_DATA-REQ_INV         = LS_OUTTAB-REQ_INV.
    LS_DATA-SHIP_ADDR       = LS_OUTTAB-SHIP_ADDR.
    LS_DATA-SHIP_PROVINCE   = LS_OUTTAB-SHIP_PROVINCE.
    LS_DATA-POSTCODE        = LS_OUTTAB-POSTCODE.
    LS_DATA-AM_PM           = LS_OUTTAB-AM_PM.
    LS_DATA-LOADING_POINT   = LS_OUTTAB-LOADING_POINT.
    LS_DATA-SALESMAN        = LS_OUTTAB-SALESMAN.
    LS_DATA-DUE_DATE        = LS_OUTTAB-DUE_DATE.
    LS_DATA-PO_NO           = LS_OUTTAB-PO_NO.
    LS_DATA-PROJECT         = LS_OUTTAB-PROJECT.
    LS_DATA-TERM_OF_PAYMENT = LS_OUTTAB-TERM_OF_PAYMENT.
    LS_DATA-CONTACT_NAME    = LS_OUTTAB-CONTACT_NAME.
    LS_DATA-REF_INV         = LS_OUTTAB-REF_INV.
    LS_DATA-SALES_DIV       = LS_OUTTAB-SALES_DIV.
    LS_DATA-SALES_OFFICE    = LS_OUTTAB-SALES_OFFICE.
    LS_DATA-SALES_GROUP     = LS_OUTTAB-SALES_GROUP.
    LS_DATA-UOM             = LS_OUTTAB-UOM.
    LS_DATA-DOC_FLAG        = LS_OUTTAB-DOC_FLAG.
    LS_DATA-MHA             = LS_OUTTAB-MHA.
    LS_DATA-FLAG_BOM        = LS_OUTTAB-FLAG_BOM.
    LS_DATA-REFER_BOM       = LS_OUTTAB-REFER_BOM.
*    LS_DATA-INSULATION      = LS_OUTTAB-INSULATION.
    LS_DATA-SPSOLC          = LS_OUTTAB-SPSOLC.

*    LS_DATA-SPSOLC          = '1100'.
*    LS_DATA-MHA             = 'X'.
*    LS_DATA-SALES_DIV       = '00'.
*    LS_DATA-REF_INV         = '512000000'.
*    LS_DATA-CONTACT_NAME    = 'Test1234'.
*    LS_DATA-PROJECT         = 'Test Project'.
*    LS_DATA-SALESMAN        = 'Test'.
*    LS_DATA-LOADING_POINT   = 'Z0'.
*    LS_DATA-AM_PM           = 'AM'.
*    LS_DATA-REQ_MAP         = 'X'.
*    LS_DATA-REQ_INV         = 'X'.
*    LS_DATA-WARRANTY        = 'X'.
*    LS_DATA-LGORT           = '1100'.
*    LS_DATA-REMARK_PICHON   = 'TEST'.
*    LS_DATA-PICHON          = 'TESt'.
*    LS_DATA-REMARK          = 'TEST'.
*    LS_DATA-REFER_BOM       = 'X'.
*    LS_DATA-FLAG_BOM        = 'X'.
    APPEND LS_DATA TO LT_DATA.
    CLEAR LS_DATA.
  ENDLOOP.

  LCL_API->SEND_DO_FG_TO_SONY( EXPORTING IT_DATA   = LT_DATA
                               IMPORTING E_MESTYPE = LV_MESTYPE
                               CHANGING  C_RETURN  = LS_RETURN ).
  IF LS_RETURN-IS_SUCCESS EQ ABAP_TRUE.
    PERFORM F_UPDATE_STATUS.
    MESSAGE S001 WITH TEXT-S01.
  ELSE.
    MESSAGE S001 WITH LS_RETURN-MESSAGE DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 1000.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SEND_DATA_SP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_SEND_DATA_SP .
  DATA : LCL_API TYPE REF TO ZCL_SDSSD_SEND_API.

  DATA : LS_DATA TYPE ZSDSMMS041,
         LT_DATA TYPE TABLE OF ZSDSMMS041.

  DATA: BEGIN OF LS_RETURN,
          IS_SUCCESS TYPE STRING,
          STATUS     TYPE P DECIMALS 0,
          MESSAGE    TYPE STRING,
        END OF LS_RETURN.

  DATA : LV_MESTYPE TYPE CHAR1.

  DATA : LS_OUTTAB LIKE LINE OF GT_OUTTAB.

  CONSTANTS : BEGIN OF LC_CON,
                ZDR    TYPE C LENGTH 3 VALUE 'ZDR',
                RETURN TYPE C LENGTH 6 VALUE 'RETURN',
                SL_DO  TYPE C LENGTH 5 VALUE 'SL_DO',
              END OF LC_CON.

  IF LCL_API IS NOT BOUND.
    CREATE OBJECT LCL_API.
  ENDIF.

  LOOP AT GT_OUTTAB INTO LS_OUTTAB WHERE LGORT IS NOT INITIAL.
    READ TABLE GT_IITAB
    WITH KEY VBELN      = LS_OUTTAB-VBELN
             LFART+0(3) = LC_CON-ZDR  TRANSPORTING NO FIELDS.
    IF SY-SUBRC EQ 0.
      LS_DATA-DELI_TO       = LC_CON-RETURN.
    ELSE.
      LS_DATA-DELI_TO       = LC_CON-SL_DO.
    ENDIF.
    LS_DATA-LFART           = LS_OUTTAB-LFART.
    LS_DATA-CUST_CODE       = LS_OUTTAB-CUST_CODE.
    LS_DATA-VBELN           = LS_OUTTAB-VBELN.
    LS_DATA-LFDAT           = LS_OUTTAB-LFDAT.
    LS_DATA-ERDAT           = LS_OUTTAB-ERDAT.
    LS_DATA-POSNR           = LS_OUTTAB-POSNR.
    LS_DATA-MATNR           = LS_OUTTAB-MATNR.
    LS_DATA-ARKTX           = LS_OUTTAB-ARKTX.
    LS_DATA-LGMNG           = LS_OUTTAB-LGMNG.
    LS_DATA-KUNNR           = LS_OUTTAB-KUNNR.
    LS_DATA-CUST_NAME       = LS_OUTTAB-CUST_NAME.
    LS_DATA-REMARK          = LS_OUTTAB-REMARK.
    LS_DATA-PICHON          = LS_OUTTAB-PICHON.
    LS_DATA-REMARK_PICHON   = LS_OUTTAB-REMARK_PICHON.
    LS_DATA-LGORT           = LS_OUTTAB-LGORT.

*    LS_DATA-LGORT         = '2100'.
*    LS_DATA-REMARK_PICHON = 'X'.
*    LS_DATA-PICHON        = 'X'.
*    LS_DATA-REMARK        = 'TEst'.

    APPEND LS_DATA TO LT_DATA.
    CLEAR LS_DATA.
  ENDLOOP.

  LCL_API->SEND_DO_SP_TO_SONY( EXPORTING IT_DATA   = LT_DATA
                               IMPORTING E_MESTYPE = LV_MESTYPE
                               CHANGING  C_RETURN  = LS_RETURN ).
  IF LS_RETURN-IS_SUCCESS EQ ABAP_TRUE..
    PERFORM F_UPDATE_STATUS.
    MESSAGE S001 WITH TEXT-S01.
  ELSE.
    MESSAGE S001 WITH LS_RETURN-MESSAGE DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 1000.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_status
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_UPDATE_STATUS.

  DATA : ls_ZSDSSDT001 TYPE ZSDSSDT001,
         ls_ZSDSSDT002 TYPE ZSDSSDT002.

  DATA : LT_TMP LIKE GT_OUTTAB[].

  LT_TMP = GT_OUTTAB[].

  SORT LT_TMP BY VBELN.
  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING VBELN.

  LOOP AT LT_TMP INTO DATA(LS_TMP).
    LS_ZSDSSDT001-DONUM = LS_TMP-VBELN.
    LS_ZSDSSDT001-WMSDT = ''.
    LS_ZSDSSDT001-WMSTM = ''.
    LS_ZSDSSDT001-STAPC = ''.
    LS_ZSDSSDT001-STAPT = ''.
    LS_ZSDSSDT001-LOADD = ''.
    LS_ZSDSSDT001-LOADT = ''.
    LS_ZSDSSDT001-CONTD = ''.
    LS_ZSDSSDT001-CONTS = ''.
    LS_ZSDSSDT001-CONTP = ''.
    LS_ZSDSSDT001-CONTT = ''.
    LS_ZSDSSDT001-DOSIG = ''.
    LS_ZSDSSDT001-PODDA = ''.
    LS_ZSDSSDT001-PODRE = ''.
    LS_ZSDSSDT001-DOSDS = ''.
    LS_ZSDSSDT001-DOSDT = ''.
    LS_ZSDSSDT001-ORDDA = ''.
    LS_ZSDSSDT001-DEALC = ''.
    LS_ZSDSSDT001-DEALN = ''.
    LS_ZSDSSDT001-REMAK = LS_TMP-REMARK.
    LS_ZSDSSDT001-SHPAD = LS_TMP-SHIP_ADDR.
    LS_ZSDSSDT001-SHPPV = LS_TMP-SHIP_PROVINCE.
    LS_ZSDSSDT001-AMPMF = LS_TMP-AM_PM.
    LS_ZSDSSDT001-LOADP = LS_TMP-LOADING_POINT.
    LS_ZSDSSDT001-REMAP = LS_TMP-REQ_MAP.
    LS_ZSDSSDT001-REINV = LS_TMP-REQ_INV.
    LS_ZSDSSDT001-SERNF = ''.
    LS_ZSDSSDT001-MAPST = ''.
    LS_ZSDSSDT001-EDOTM = ''.
    LS_ZSDSSDT001-DIVNM = ''.
    LS_ZSDSSDT001-PLTNO = ''.
    LS_ZSDSSDT001-CARNA = ''.
    LS_ZSDSSDT001-TUKTY = ''.
    LS_ZSDSSDT001-SHPNO = ''.
    LS_ZSDSSDT001-ERNAM = SY-UNAME.
    LS_ZSDSSDT001-ERDAT = SY-DATUM.
    LS_ZSDSSDT001-ERZET = SY-UZEIT.
    LS_ZSDSSDT001-AENAM = SY-UNAME.
    LS_ZSDSSDT001-AEDAT = SY-DATUM.
    LS_ZSDSSDT001-AEZET = SY-UZEIT.
    MODIFY ZSDSSDT001 FROM LS_ZSDSSDT001.

    LS_ZSDSSDT002-VBELN   = LS_TMP-VBELN.
    LS_ZSDSSDT002-RUN_ID  = 1.
    LS_ZSDSSDT002-REQ_INV = LS_TMP-REQ_INV.
    LS_ZSDSSDT002-REQ_MAP = LS_TMP-REQ_MAP.
    LS_ZSDSSDT002-AM_PM   = LS_TMP-AM_PM  .
    LS_ZSDSSDT002-ERDAT   = SY-DATUM.
    LS_ZSDSSDT002-ERZET   = SY-UZEIT.
    MODIFY ZSDSSDT002 FROM LS_ZSDSSDT002.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_CONFIG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LR_WARANTY_GROUP
*&---------------------------------------------------------------------*
FORM F_GET_CONFIG  TABLES LR_WARANTY_GROUP.

  CONSTANTS : BEGIN OF LC_CON,
                REPID TYPE ZSDSCAC001-REPID VALUE 'WARRANTY_CARD',
                PARAM TYPE ZSDSCAC001-PARAM VALUE 'SALES_GROUP',
              END OF LC_CON.

  ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                I_PARAM             = LC_CON-PARAM
                                      CHANGING  CR_RETURN           = LR_WARANTY_GROUP ).
ENDFORM.
