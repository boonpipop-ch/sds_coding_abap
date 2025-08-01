*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0280
*  Creation Date      : 27.08.2024
*  Author             : Jakarin S.
*  Add-on ID          : -
*  Description        : Print Warranty Card
*  Purpose            :
*  Copied from        : ZT_SD_WARRANTY_CARD
*  Restriction        :
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

REPORT ZSDSSDR0280 MESSAGE-ID ZSDSMM01.

************************************************************************
*      T A B L E S                                                     *
************************************************************************
TABLES: LIKP, LIPS, OBJK,MARD,ZSDSSDC007,VBAK,VBAP.

************************************************************************
*      D A T A                                                         *
************************************************************************
*TYPES: BEGIN OF ty_output,
*         sel      TYPE char1,
**         pages    TYPE ssfcompop-tdcopies,  "bsbook-bookpages,
*         prtno    TYPE ZSDSSDT019-print_numb,
*         lfdat    TYPE likp-lfdat,
*         vbeln    TYPE likp-vbeln,
*         posnr    TYPE lips-posnr,
*         matnr    TYPE lips-matnr,
*         sernr    TYPE objk-sernr,
*       END OF ty_output.
TYPES: BEGIN OF TY_OUTPUT.
         INCLUDE STRUCTURE ZSDSSDS059.
TYPES:   SALES_SP   TYPE KNA1-NAME1,
         CONDITION  TYPE VBAK-KNUMV,
         MAT_DIVI   TYPE VBAP-SPART,
         NET_VALUE  TYPE VBAP-NETWR,
         SALES_EMP  TYPE PA0001-ENAME,
*         sales_org  TYPE tvkot-vtext,
         VTWEG      TYPE TVTWT-VTWEG,
         DISTR_CHN  TYPE TVTWT-VTEXT,
         VKBUR      TYPE TVKBT-VKBUR,
         SALES_OFF  TYPE TVKBT-BEZEI,
         MAT_CREADT TYPE MARA-ERSDA,
         SEL        TYPE CHAR1,
         ISWAR(1)   TYPE C, " Add By Aphai on 30.10.2014
         LGORT      TYPE LIPS-LGORT, " Add By Aphai on 30.10.2014
         AUFNR      TYPE LIPS-AUFNR,  "Add by Wantanee 20150521 ITR2015-3851
         SO_NO      TYPE LIPS-VGBEL, "Add by Wantanee 20190726
         SO_ITEM    TYPE LIPS-VGPOS, "Add by Wantanee 20190726
*        print_numb TYPE ZSDSSDT019-print_numb,
         PRINT_NUMB TYPE CVINUMALL,
       END OF TY_OUTPUT.

TYPES: BEGIN OF TY_SERIAL,
         OBKNR   TYPE SER01-OBKNR,
         LIEF_NR TYPE SER01-LIEF_NR,
         POSNR   TYPE SER01-POSNR,
         SERNR   TYPE OBJK-SERNR,
         MATNR   TYPE OBJK-MATNR,
       END OF TY_SERIAL.

TYPES: BEGIN OF TY_DELIV,
         VBELN    TYPE LIKP-VBELN,
         LFART    TYPE LIKP-LFART,
         LFDAT    TYPE LIKP-LFDAT,
         POSNR    TYPE LIPS-POSNR,
         MATNR    TYPE LIPS-MATNR,
         LGORT    TYPE LIPS-LGORT, " add by Aphai on 30.10.2014
         LFIMG    TYPE LIPS-LFIMG,
         VRKME    TYPE LIPS-VRKME,
         VGBEL    TYPE LIPS-VGBEL,
         VGPOS    TYPE LIPS-VGPOS,
         POSNV    TYPE LIPS-POSNV, " add by aphai on 30.10.2014
         MEINS    TYPE LIPS-MEINS, " aphai 6.11.2014
         AUFNR    TYPE LIPS-AUFNR, " Add by wantanee 20150521 ITR2015-3851
         ERSDA    TYPE MARA-ERSDA,
         MTART    TYPE MARA-MTART,
*         prdha  TYPE mara-prdha,
         PRODH    TYPE LIPS-PRODH,
         SERNR    TYPE OBJK-SERNR,
         KUNNR    TYPE VBPA-KUNNR,   " Add by wantanee 20150521 ITR2015-3851

         VTWEG    TYPE VBAK-VTWEG, " add by Aphai on 30.10.2014
         VKBUR    TYPE VBAK-VKBUR, " add by Aphai on 30.10.2014
         KNUMV    TYPE VBAK-KNUMV, " add by aphai on 30.10.2014
         NETWR    TYPE VBAP-NETWR, " add by aphai on 30.10.2014
         AUART    TYPE VBAK-AUART,
         ISWAR(1) TYPE C, " is Warranty
       END OF TY_DELIV.

TYPES : BEGIN OF TY_KONV,
          KNUMV TYPE VBAK-KNUMV,
          KPOSN TYPE KONV-KPOSN,
          KWERT TYPE KONV-KWERT,
        END OF TY_KONV.

TYPES: BEGIN OF TY_DELIV_SP,
         VBELN TYPE VBPA-VBELN,
         POSNR TYPE VBPA-POSNR,
         PARVW TYPE VBPA-PARVW,
         KUNNR TYPE VBPA-KUNNR,
         ADRNR TYPE VBPA-ADRNR,
         NAME1 TYPE ADRC-NAME1,
       END OF TY_DELIV_SP.

TYPES: BEGIN OF TY_SALES,
         VBELN TYPE VBAK-VBELN,
         VKORG TYPE VBAK-VKORG,
         VTWEG TYPE VBAK-VTWEG,
         VKBUR TYPE VBAK-VKBUR,
         KNUMV TYPE VBAK-KNUMV,
         KUNNR TYPE VBAK-KUNNR,
         POSNR TYPE VBAP-POSNR,
         SPART TYPE VBAP-SPART,
         NETWR TYPE VBAP-NETWR,
         AUART TYPE VBAK-AUART,
         UPMAT TYPE VBAP-UPMAT,  "Add by WAntanee 20200116
       END OF TY_SALES.

TYPES: BEGIN OF TY_SALES_SP,
         VBELN TYPE VBPA-VBELN,
         POSNR TYPE VBPA-POSNR,
         PARVW TYPE VBPA-PARVW,
         KUNNR TYPE VBPA-KUNNR,
         ADRNR TYPE VBPA-ADRNR,
         NAME1 TYPE ADRC-NAME1,
       END OF TY_SALES_SP.

TYPES: BEGIN OF TY_SALES_EMP,
         VBELN TYPE VBPA-VBELN,
         POSNR TYPE VBPA-POSNR,
         PARVW TYPE VBPA-PARVW,
         PERNR TYPE VBPA-PERNR,
         ENAME TYPE PA0001-ENAME,
       END OF TY_SALES_EMP.

*Add by Wantanee 20150306 ITR2015-3747

TYPES: BEGIN OF TYP_CHK_MODEL_SERIAL,
         MATNR TYPE MARA-MATNR,
         SERNR TYPE OBJK-SERNR,

       END OF TYP_CHK_MODEL_SERIAL.
TYPES: BEGIN OF TYP_ZSDSCAC002_CHK,
         REPID TYPE ZSDSCAC002-REPID,
         CONST TYPE ZSDSCAC002-CONST,
         VALUE TYPE ZSDSCAC002-VALUE,
       END OF TYP_ZSDSCAC002_CHK.
*End Add by Wantanee 20150306   ITR2015-3747

*Add by Wantanee 20150521 ITR2015-3851
TYPES: BEGIN OF TYP_ZSDSSDC003,
         KUNNR        TYPE ZSDSSDC003-KUNNR,
         VKBUR        TYPE ZSDSSDC003-VKBUR,
         Z_CUST_GROUP TYPE ZSDSSDC003-Z_CUST_GROUP,
       END OF TYP_ZSDSSDC003.


*End Add by Wantanee ITR2015-3851

*Add by Wantanee 20160502

*End Add by Wantanee 20160502


TYPES: BEGIN OF TYP_ZSDSSDC006,
         MATNR TYPE ZSDSSDC006-MATNR,
         AUART TYPE ZSDSSDC006-AUART,
       END OF TYP_ZSDSSDC006.


*---------- Internal tables --------------------------------------------
* Data for ALV display
TYPE-POOLS: SLIS.
DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      GT_EVENTS   TYPE SLIS_T_EVENT,
      GS_LAYOUT   TYPE SLIS_LAYOUT_ALV.
*     gt_heading  TYPE slis_t_listheader,
*     gt_sort     TYPE slis_t_sortinfo_alv.
DATA: GT_LIKP        TYPE STANDARD TABLE OF LIKP,
      GT_LIPS        TYPE STANDARD TABLE OF LIPS,
      GT_MARA        TYPE STANDARD TABLE OF MARA,
      GT_VBPA        TYPE STANDARD TABLE OF VBPA,
      GT_ADRC        TYPE STANDARD TABLE OF ADRC,
      GT_SERIAL      TYPE STANDARD TABLE OF TY_SERIAL,
      GT_OUTPUT      TYPE STANDARD TABLE OF TY_OUTPUT,
      GT_WARRANTYLOG TYPE STANDARD TABLE OF ZSDSSDT019.
*Add by Wantanee 20150306 ITR2015-3747

DATA: GT_CHK_MODEL_SERIAL TYPE STANDARD TABLE OF TYP_CHK_MODEL_SERIAL,
      WA_CHK_MODEL_SERIAL TYPE TYP_CHK_MODEL_SERIAL,
      GW_CHK_MODEL_SERIAL TYPE TYP_CHK_MODEL_SERIAL.
DATA: GT_SDS_GEN_C_CHK TYPE STANDARD TABLE OF TYP_ZSDSCAC002_CHK,
      WA_SDS_GEN_C_CHK TYPE TYP_ZSDSCAC002_CHK,
      GW_SDS_GEN_C_CHK TYPE TYP_ZSDSCAC002_CHK.

*End Add by Wantanee 20150306 ITR2015-3747
*DATA: gs_nast     TYPE nast.
*DATA: gv_preview  TYPE char1.
DATA: GT_DELIV TYPE STANDARD TABLE OF TY_DELIV WITH HEADER LINE,
      GT_KONV  TYPE STANDARD TABLE OF TY_KONV WITH HEADER LINE,
      DT_CONST TYPE TABLE OF ZSDSCAC002 WITH HEADER LINE,
      WA_KONV  TYPE TY_KONV.
DATA: GT_DELIV_SP TYPE STANDARD TABLE OF TY_DELIV_SP.
DATA: GT_SALES    TYPE STANDARD TABLE OF TY_SALES.
DATA: GT_SALES_SP  TYPE STANDARD TABLE OF TY_SALES_SP,
      GT_SALES_EMP TYPE STANDARD TABLE OF TY_SALES_EMP.
DATA: GT_TVTWT TYPE STANDARD TABLE OF TVTWT,
*      gt_tvkot    TYPE STANDARD TABLE OF tvkot,
      GT_TVKBT TYPE STANDARD TABLE OF TVKBT.

*Add by Wantanee 20150521
DATA: GT_CUST_WARRANTY TYPE STANDARD TABLE OF TYP_ZSDSSDC003,
      WA_CUST_WARRANTY TYPE TYP_ZSDSSDC003.

*End Add by Wantanee 20150521

*Add by Wantanee 20160502

*End Add by Wantanee 20160502


DATA: GT_ZSDSSDC006 TYPE STANDARD TABLE OF TYP_ZSDSSDC006,
      GW_ZSDSSDC006 TYPE TYP_ZSDSSDC006,
      WA_ZSDSSDC006 TYPE TYP_ZSDSSDC006.

*---------- Constants --------------------------------------------------
CONSTANTS:
  GC_REPID             TYPE SY-REPID VALUE 'ZSDSSDR0280',
  "gc_warranty_formname TYPE tdsfname VALUE 'ZSF_SD_WARRANTY_CARD',
  GC_WARRANTY_FORMNAME TYPE TDSFNAME VALUE 'ZSDSSD011',
  GC_DELIVERY_FORMNAME TYPE TDSFNAME VALUE 'ZSDSSD009'.


************************************************************************
*      S E L E C T I O N  S C R E E N                                  *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B01 WITH FRAME TITLE TEXT-S01.
  SELECT-OPTIONS: S_LFDAT   FOR  LIKP-LFDAT,
                  S_PH1     FOR  ZSDSSDC007-PH1 NO INTERVALS, " add by aphai on 30.10.2014
                  S_VBELN   FOR  LIKP-VBELN,
                  S_MATNR   FOR  LIPS-MATNR,
                  S_LGORT   FOR  MARD-LGORT, " add by aphai on 30.10.2014
                  S_SERNR   FOR  OBJK-SERNR.
SELECTION-SCREEN END OF BLOCK B01.

* add by aphai on 30.10.2014
SELECTION-SCREEN BEGIN OF BLOCK B02 WITH FRAME TITLE TEXT-S02.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS  CBO_WAR AS CHECKBOX USER-COMMAND RAD.
    SELECTION-SCREEN COMMENT 2(30) TEXT-001 FOR FIELD CBO_WAR.
    SELECTION-SCREEN POSITION 10.
*PARAMETERS  cbox2 AS CHECKBOX USER-COMMAND rad.
*selection-SCREEN COMMENT 12(8) text-002 for field cbox1.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B02.
*end add by aphai on 30.10.2014
************************************************************************
*      B E G I N      S E L E C T I O N                                *
************************************************************************
START-OF-SELECTION.
  IF S_LFDAT IS INITIAL AND S_VBELN IS INITIAL AND
     S_MATNR IS INITIAL AND S_SERNR IS INITIAL.
*    MESSAGE i019.
    MESSAGE I000 WITH 'Please input at least one selection criteria.'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  PERFORM CLEAR_DATA.
  PERFORM GET_DATA.
  PERFORM SPLIT_WARRANTY TABLES GT_DELIV[]. " add by aphai on 31.10.2014


************************************************************************
*      E N D   O F    S E L E C T I O N                                *
************************************************************************
END-OF-SELECTION.
  PERFORM PREPARE_OUTPUT.
  PERFORM F_DELETE_DATA.
  IF GT_OUTPUT IS NOT INITIAL.
    PERFORM DISPLAY_ALV_OUTPUT.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CLEAR_DATA.

  REFRESH: GT_FIELDCAT[], GT_EVENTS[].
  CLEAR: GS_LAYOUT.

  REFRESH: GT_LIKP[], GT_LIPS[], GT_MARA[],
           GT_VBPA[], GT_ADRC[],
           GT_SERIAL[], GT_OUTPUT[].
  REFRESH: GT_DELIV[], GT_DELIV_SP[],
           GT_SALES[], GT_SALES_SP[], GT_SALES_EMP[],
           GT_TVTWT[], GT_TVKBT[].
*           gt_tvkot[]
*  CLEAR: gs_nast.
*  CLEAR: gv_preview.

ENDFORM.                    " CLEAR_DATA

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_DATA.

* Get delivery order data
  SELECT A~VBELN
         A~LFART
         A~LFDAT
         B~POSNR
         B~MATNR
         B~LGORT "Add by Aphai on 30.10.2014
         B~LFIMG
         B~VRKME
         B~VGBEL
         B~VGPOS
         B~POSNV "Add by Aphai on 30.10.2014
         B~MEINS " aphai 6.11.2014
         B~AUFNR  " Add by Wantnaee 20150521 ITR2015-3851
         C~ERSDA
         C~MTART
*         c~prdha  "PH remove by wantanee
         B~PRODH  "PH add by wantanee
         E~SERNR
         F~KUNNR  " Add by Wantnaee 20150521 ITR2015-3851
*         k~vtweg "Add by Aphai on 30.10.2014
*         k~vkbur "Add by Aphai on 30.10.2014
*         k~knumv "Add by Aphai on 30.10.2014
*         f~netwr "Add by Aphai on 30.10.2014
*    INTO CORRESPONDING FIELDS OF TABLE gt_deliv
    INTO TABLE GT_DELIV
    FROM LIKP AS A
    INNER JOIN LIPS AS B ON A~VBELN = B~VBELN
*    INNER JOIN vbak AS k ON b~vgbel = k~vbeln
*    INNER JOIN vbap AS f ON b~vgbel = f~vbeln AND b~vgpos = f~posnr
    INNER JOIN MARA AS C ON C~MATNR = B~MATNR
    INNER JOIN SER01 AS D ON D~LIEF_NR = B~VBELN AND D~POSNR = B~POSNR
    INNER JOIN OBJK AS E ON E~OBKNR = D~OBKNR
    INNER JOIN VBPA AS F ON ( A~VBELN EQ F~VBELN
                         AND  F~PARVW EQ 'AG' )  " Add by Wantnaee 20150521 ITR2015-3851
    WHERE A~VBELN IN S_VBELN
           AND A~LFDAT IN S_LFDAT
           AND B~MATNR IN S_MATNR
           AND B~LGORT IN S_LGORT
           AND E~SERNR IN S_SERNR.

*  Add by Aphai On 31.10.2014

  IF S_PH1 IS NOT INITIAL.
    SORT S_PH1[] BY LOW.
    DELETE ADJACENT DUPLICATES FROM S_PH1 COMPARING LOW.

    DATA: PH1   TYPE ZSDSSDC007-PH1,
          ITEMP TYPE TABLE OF TY_DELIV WITH HEADER LINE,
          WATMP TYPE TY_DELIV.

    ITEMP[] = GT_DELIV[].
    SORT ITEMP[] BY MATNR.
    DELETE ADJACENT DUPLICATES FROM ITEMP COMPARING MATNR.

    LOOP AT ITEMP.
      PH1 = ITEMP-PRODH+0(5).
      READ TABLE S_PH1 WITH KEY LOW = PH1 BINARY SEARCH.
      IF SY-SUBRC NE 0.
        DELETE GT_DELIV WHERE MATNR = ITEMP-MATNR.
      ENDIF.
      CLEAR PH1.
    ENDLOOP.
  ENDIF.
*  End Add by Aphai On 31.10.2014


*BOI WCH290514
  IF GT_DELIV[] IS INITIAL.
    SELECT A~VBELN A~LFART A~LFDAT
           B~POSNR B~MATNR B~LFIMG B~VRKME B~VGBEL B~VGPOS
           C~ERSDA C~MTART
*           c~prdha  "remove add by wantanee
           B~PRODH  "PH add by wantanee
           B~MEINS
           B~AUFNR  "Add by Wantanee 20150521 ITR2015-3851
            INTO CORRESPONDING FIELDS OF TABLE GT_DELIV
      FROM LIKP AS A
      INNER JOIN LIPS AS B
              ON A~VBELN = B~VBELN
      INNER JOIN MARA AS C
              ON C~MATNR = B~MATNR
         WHERE A~VBELN IN S_VBELN
           AND A~LFDAT IN S_LFDAT
           AND B~MATNR IN S_MATNR.
  ENDIF.
*EOI WCH290514

  SORT GT_DELIV BY VBELN.

  DELETE GT_DELIV WHERE LFART EQ 'EL'.  "no inbound delivery
  DELETE GT_DELIV WHERE MTART EQ 'ZBOM'.
* Get only condensing material - product hierarchy level 2 = CDU)
*  DELETE GT_DELIV WHERE PRODH NS ' CDU '.

  IF GT_DELIV[] IS INITIAL.
*    MESSAGE i003.
    LEAVE LIST-PROCESSING.

  ELSE.
*    Aphai 5.11.2014
    SELECT * FROM ZSDSCAC002 INTO TABLE DT_CONST WHERE REPID EQ SY-REPID AND CONST LIKE 'SPM%'. " Add By Aphai On 3.11.2014
    SORT DT_CONST BY VALUE.
*    end aphai 5.11.2014

*   Get print log
    SELECT *
      FROM ZSDSSDT019
      INTO TABLE GT_WARRANTYLOG
      FOR ALL ENTRIES IN GT_DELIV
      WHERE DELIV_NUMB = GT_DELIV-VBELN
      AND   ITM_NUMBER = GT_DELIV-POSNR
      AND   SERIAL_NUM = GT_DELIV-SERNR.
    SORT GT_WARRANTYLOG BY PRINT_SEQU DESCENDING
                           DELIV_NUMB ASCENDING
                           ITM_NUMBER ASCENDING
                           SERIAL_NUM ASCENDING.

*   Get delivery sold-to-party name
*    SELECT a~vbeln a~posnr a~parvw a~adrnr
*           b~name1
*      FROM vbpa AS a
*      INNER JOIN adrc AS b
*              ON a~adrnr = b~addrnumber
*      INTO TABLE gt_deliv_sp
*      FOR ALL ENTRIES IN gt_deliv
*      WHERE a~vbeln = gt_deliv-vbeln
*      AND   a~posnr = ''
*      AND   a~parvw = 'AG'.
    SELECT A~VBELN A~POSNR A~PARVW A~KUNNR A~ADRNR
           B~NAME1
      INTO CORRESPONDING FIELDS OF TABLE GT_DELIV_SP " aphai 04.11.2014
      FROM VBPA AS A
      INNER JOIN KNA1 AS B
              ON A~KUNNR = B~KUNNR
      FOR ALL ENTRIES IN GT_DELIV
      WHERE A~VBELN = GT_DELIV-VBELN
*      AND   a~posnr = ''
      AND   A~PARVW = 'AG'.
    SORT GT_DELIV_SP BY VBELN.

*   Get sales data
    SELECT A~VBELN A~VKORG A~VTWEG A~VKBUR A~KNUMV A~KUNNR
           B~POSNR B~SPART B~NETWR A~AUART B~UPMAT
      INTO TABLE GT_SALES
      FROM VBAK AS A
      INNER JOIN VBAP AS B
              ON A~VBELN = B~VBELN
      FOR ALL ENTRIES IN GT_DELIV
      WHERE B~VBELN = GT_DELIV-VGBEL
      AND   B~POSNR = GT_DELIV-VGPOS.
    SORT GT_SALES BY VBELN.

    IF GT_SALES[] IS NOT INITIAL.

**     Get sales organization description
*      SELECT *
*        FROM tvkot
*        INTO TABLE gt_tvkot
*        FOR ALL ENTRIES IN gt_sales
*        WHERE spras = sy-langu
*        AND   vkorg = gt_sales-vkorg.
*      SORT gt_tvkot BY vkorg.

*     Get distribution channel description
      SELECT *
        INTO TABLE GT_TVTWT
        FROM TVTWT
        FOR ALL ENTRIES IN GT_SALES
        WHERE SPRAS = SY-LANGU
        AND   VTWEG = GT_SALES-VTWEG.
      SORT GT_TVTWT BY VTWEG.

*     Get sales office description
      SELECT *
        INTO TABLE GT_TVKBT
        FROM TVKBT
        FOR ALL ENTRIES IN GT_SALES
        WHERE SPRAS = SY-LANGU
        AND   VKBUR = GT_SALES-VKBUR.
      SORT GT_TVKBT BY VKBUR.

*     Get sales sold-to-party name
      SELECT A~VBELN A~POSNR A~PARVW A~KUNNR A~ADRNR
             B~NAME1
        INTO CORRESPONDING FIELDS OF TABLE GT_SALES_SP
        FROM VBPA AS A
        INNER JOIN KNA1 AS B
                ON A~KUNNR = B~KUNNR
        FOR ALL ENTRIES IN GT_SALES
        WHERE A~VBELN = GT_SALES-VBELN
*        AND   a~posnr = ''
        AND   A~PARVW = 'AG'.
      SORT GT_SALES_SP BY VBELN.

*     Get sales employee name
      SELECT A~VBELN A~POSNR A~PARVW A~PERNR
             B~ENAME
         INTO CORRESPONDING FIELDS OF TABLE GT_SALES_EMP
        FROM VBPA AS A
        INNER JOIN PA0001 AS B
                ON A~PERNR = B~PERNR
        FOR ALL ENTRIES IN GT_SALES
        WHERE A~VBELN = GT_SALES-VBELN
*        AND   a~posnr = ''
        AND   A~PARVW = 'VE'.
      SORT GT_SALES_EMP BY VBELN.

    ENDIF.
    "Add by wantanee 20150306
    SELECT REPID CONST VALUE
    INTO TABLE GT_SDS_GEN_C_CHK
    FROM ZSDSCAC002
    WHERE REPID EQ  GC_REPID
    AND CONST LIKE 'CHK%'.

    IF NOT GT_SDS_GEN_C_CHK IS INITIAL.
      LOOP AT GT_SDS_GEN_C_CHK INTO WA_SDS_GEN_C_CHK.
        CLEAR: WA_CHK_MODEL_SERIAL.
        SPLIT WA_SDS_GEN_C_CHK-VALUE  AT '_' INTO: WA_CHK_MODEL_SERIAL-MATNR WA_CHK_MODEL_SERIAL-SERNR .
        APPEND WA_CHK_MODEL_SERIAL TO GT_CHK_MODEL_SERIAL.

      ENDLOOP.

    ENDIF.
    "End Add by wantanee 20150306
  ENDIF.

  "Add by Wantanaee 20150521 ITR2015-3851
  SELECT KUNNR VKBUR
  INTO TABLE GT_CUST_WARRANTY
  FROM ZSDSSDC003.


  "End by Wantanee ITR2015-3851

  "Add by Wantanee 20160502

  "End Add by Wantanee 20160502

  SELECT MATNR AUART
   INTO TABLE GT_ZSDSSDC006
   FROM ZSDSSDC006.



ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  PREPARE_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PREPARE_OUTPUT.

  DATA: LT_WARRANTYLOG  TYPE STANDARD TABLE OF ZSDSSDT019.
  DATA: LS_DELIV       TYPE TY_DELIV,
        LS_DELIV_SP    TYPE TY_DELIV_SP,
        LS_SALES       TYPE TY_SALES,
        LS_SALES_SP    TYPE TY_SALES_SP,
        LS_SALES_EMP   TYPE TY_SALES_EMP,
*       ls_tvkot        TYPE tvkot,
        LS_TVTWT       TYPE TVTWT,
        LS_TVKBT       TYPE TVKBT,
        LS_OUTPUT      TYPE TY_OUTPUT,
        LS_WARRANTYLOG TYPE ZSDSSDT019.
  DATA: LV_LINES        TYPE I.
  DATA: CHECK_PRINT_SERIAL_SPECIAL TYPE C.

  DATA: LV_AUART_SO     TYPE VBAK-AUART.

  LOOP AT GT_DELIV INTO LS_DELIV.

    CLEAR: WA_SDS_GEN_C_CHK,CHECK_PRINT_SERIAL_SPECIAL,LV_AUART_SO.
    "Add by Wantanee 20150306

    READ TABLE GT_CHK_MODEL_SERIAL INTO WA_CHK_MODEL_SERIAL WITH KEY MATNR = LS_DELIV-MATNR.
    IF SY-SUBRC EQ 0.
      IF LS_DELIV-SERNR GT WA_CHK_MODEL_SERIAL-SERNR.
        CHECK_PRINT_SERIAL_SPECIAL = 'X'.
      ENDIF.

    ENDIF.
    IF CHECK_PRINT_SERIAL_SPECIAL NE 'X'.
      "End Add by Wantanee 20150306
      LS_OUTPUT-DELIV_NUMB = LS_DELIV-VBELN.
      LS_OUTPUT-DELIV_DATE = LS_DELIV-LFDAT.
      LS_OUTPUT-ITM_NUMBER = LS_DELIV-POSNR.
      LS_OUTPUT-DELIV_QTY  = LS_DELIV-LFIMG.
      LS_OUTPUT-SALES_UNIT = LS_DELIV-VRKME.
      LS_OUTPUT-MATERIAL   = LS_DELIV-MATNR.
      LS_OUTPUT-LGORT   = LS_DELIV-LGORT. " Add by Aphai On 30.10.2014
      LS_OUTPUT-MAT_CREADT = LS_DELIV-ERSDA.
      LS_OUTPUT-PROD_HIER  = LS_DELIV-PRODH.
      LS_OUTPUT-SERIAL_NUM = LS_DELIV-SERNR.
      LS_OUTPUT-ISWAR = LS_DELIV-ISWAR. " Add by Aphai on 31.10.2014
      LS_OUTPUT-AUFNR = LS_DELIV-AUFNR.  "Add by Wantanee 20150521 ITR2015-3851
      LS_OUTPUT-SO_NO = LS_DELIV-VGBEL.  "Add by Wantanee 20190726 ITR20190521-149
      LS_OUTPUT-SO_ITEM = LS_DELIV-VGPOS.  "Add by Wantanee 20190726 ITR20190521-149


      READ TABLE GT_DELIV_SP INTO LS_DELIV_SP WITH KEY VBELN = LS_DELIV-VBELN BINARY SEARCH.
      IF SY-SUBRC = 0.
        LS_OUTPUT-SOLD_TO_PARTY = LS_DELIV_SP-KUNNR.
        LS_OUTPUT-ADDR_NO       = LS_DELIV_SP-ADRNR.
        LS_OUTPUT-SOLD_TO_NAME  = LS_DELIV_SP-NAME1.
      ENDIF.

      READ TABLE GT_SALES INTO LS_SALES WITH KEY VBELN = LS_DELIV-VGBEL
                                                 POSNR = LS_DELIV-VGPOS.
      IF SY-SUBRC = 0.
        LS_OUTPUT-CONDITION = LS_SALES-KNUMV.
        LS_OUTPUT-MAT_DIVI  = LS_SALES-SPART.
        LS_OUTPUT-NET_VALUE = LS_SALES-NETWR.
        LV_AUART_SO = LS_SALES-AUART.

        READ TABLE GT_SALES_SP INTO LS_SALES_SP WITH KEY VBELN = LS_SALES-VBELN BINARY SEARCH.
        IF SY-SUBRC = 0.
          LS_OUTPUT-SALES_SP = LS_SALES_SP-NAME1.
        ENDIF.
        READ TABLE GT_SALES_EMP INTO LS_SALES_EMP WITH KEY VBELN = LS_SALES-VBELN BINARY SEARCH.
        IF SY-SUBRC = 0.
          LS_OUTPUT-SALES_EMP = LS_SALES_EMP-ENAME.
        ENDIF.
*               ls_output-vkorg = ls_sales-vkorg.
*               READ TABLE gt_tvkot INTO ls_tvkot WITH KEY vkorg = ls_sales-vkorg BINARY SEARCH.
*               IF sy-subrc = 0.
*                 ls_output-sales_org = ls_tvkot-vtext.
*               ENDIF.
        LS_OUTPUT-VTWEG = LS_SALES-VTWEG.
        READ TABLE GT_TVTWT INTO LS_TVTWT WITH KEY VTWEG = LS_SALES-VTWEG BINARY SEARCH.
        IF SY-SUBRC = 0.
          LS_OUTPUT-DISTR_CHN = LS_TVTWT-VTEXT.
        ENDIF.
        LS_OUTPUT-VKBUR = LS_SALES-VKBUR.
        READ TABLE GT_TVKBT INTO LS_TVKBT WITH KEY VKBUR = LS_SALES-VKBUR BINARY SEARCH.
        IF SY-SUBRC = 0.
          LS_OUTPUT-SALES_OFF = LS_TVKBT-BEZEI.
        ENDIF.
      ENDIF.

      READ TABLE GT_WARRANTYLOG INTO LS_WARRANTYLOG WITH KEY DELIV_NUMB = LS_DELIV-VBELN
                                                             ITM_NUMBER = LS_DELIV-POSNR
                                                             SERIAL_NUM = LS_DELIV-SERNR.
      IF SY-SUBRC = 0.
        LS_OUTPUT-PRINT_NUMB = LS_WARRANTYLOG-PRINT_SEQU.
      ENDIF.

*             REFRESH lt_warrantylog[].
*             lt_warrantylog[] = gt_warrantylog[].
*             DELETE lt_warrantylog WHERE deliv_numb NE ls_deliv-vbeln.
*             DELETE lt_warrantylog WHERE itm_number NE ls_deliv-posnr.
*             DELETE lt_warrantylog WHERE serial_num NE ls_deliv-sernr.
*             DESCRIBE TABLE lt_warrantylog[] LINES lv_lines.
*             IF lv_lines IS NOT INITIAL.
*               ls_output-print_numb = lv_lines.
*             ENDIF.

*             LOOP AT gt_warrantylog INTO ls_warrantylog WHERE deliv_numb = ls_lips-vbeln.
*               ls_output-print_numb = ls_output-print_numb + ls_warrantylog-print_numb.
*             ENDLOOP.

*             READ TABLE gt_serial INTO ls_serial WITH KEY lief_nr = ls_lips-vbeln
*                                                          posnr   = ls_lips-posnr.
*             IF sy-subrc = 0.
*               CLEAR ls_serial.
*               LOOP AT gt_serial INTO ls_serial WHERE lief_nr = ls_lips-vbeln
*                                                AND   posnr   = ls_lips-posnr.
*                 ls_output-serial_num = ls_serial-sernr.
*                 APPEND ls_output TO gt_output.
*               ENDLOOP.
*             ELSE.
*               APPEND ls_output TO gt_output.
*             ENDIF.
*             CLEAR: ls_likp, ls_lips, ls_mara,
*                    ls_vbpa, ls_adrc,
*                    ls_serial, ls_output.

      IF LS_SALES-UPMAT+0(3) NE 'SAT'. "Add by Wantanee 20200116
        READ TABLE GT_ZSDSSDC006 INTO GW_ZSDSSDC006 WITH KEY MATNR =  LS_DELIV-MATNR
                                                                       AUART =  LV_AUART_SO.
        IF SY-SUBRC EQ 0.
          APPEND LS_OUTPUT TO GT_OUTPUT.
        ELSE.
          APPEND LS_OUTPUT TO GT_OUTPUT.
        ENDIF.
      ENDIF."End Add by Wantanee 20200116

      "Add by Wantanee 20160502

      "End Add by Wantanee 20160502
      CLEAR: LS_DELIV, LS_DELIV_SP,
             LS_SALES, LS_SALES_SP, LS_SALES_EMP,
*                    ls_tvkot,
             LS_TVTWT, LS_TVKBT,
             LS_OUTPUT.
    ENDIF.

  ENDLOOP.

  SORT GT_OUTPUT BY DELIV_DATE
                    DELIV_NUMB
                    ITM_NUMBER
                    SERIAL_NUM.

*  DATA: ls_likp   TYPE likp,
*        ls_lips   TYPE lips,
*        ls_mara   TYPE mara,
*        ls_vbpa   TYPE vbpa,
*        ls_adrc   TYPE adrc,
*        ls_serial TYPE ty_serial,
*        ls_output TYPE ty_output,
*        ls_warrantylog TYPE ZSDSSDT019.
*
*  LOOP AT gt_lips INTO ls_lips.
*
*    READ TABLE gt_mara INTO ls_mara WITH KEY matnr = ls_lips-matnr.
*    CHECK sy-subrc = 0.
*
*    ls_output-deliv_numb = ls_lips-vbeln.
*    ls_output-itm_number = ls_lips-posnr.
*    ls_output-deliv_qty  = ls_lips-lfimg.
*    ls_output-sales_unit = ls_lips-vrkme.
*    ls_output-material   = ls_mara-matnr.
*    ls_output-prod_hier  = ls_mara-prdha.
*
*    READ TABLE gt_likp INTO ls_likp WITH KEY vbeln = ls_lips-vbeln.
*    IF sy-subrc = 0.
*      ls_output-deliv_date = ls_likp-lfdat.
*    ENDIF.
*
*    READ TABLE gt_vbpa INTO ls_vbpa WITH KEY vbeln = ls_lips-vbeln.
*    IF sy-subrc = 0.
*      ls_output-sold_to_party = ls_vbpa-kunnr.
*      ls_output-addr_no       = ls_vbpa-adrnr.
*
*      READ TABLE gt_adrc INTO ls_adrc WITH KEY addrnumber = ls_vbpa-adrnr.
*      IF sy-subrc = 0.
*        ls_output-sold_to_name = ls_adrc-name1.
*      ENDIF.
*    ENDIF.
*
**    LOOP AT gt_warrantylog INTO ls_warrantylog WHERE deliv_numb = ls_lips-vbeln.
**      ls_output-print_numb = ls_output-print_numb + ls_warrantylog-print_numb.
**    ENDLOOP.
*
*    READ TABLE gt_serial INTO ls_serial WITH KEY lief_nr = ls_lips-vbeln
*                                                 posnr   = ls_lips-posnr.
*    IF sy-subrc = 0.
*      CLEAR ls_serial.
*      LOOP AT gt_serial INTO ls_serial WHERE lief_nr = ls_lips-vbeln
*                                       AND   posnr   = ls_lips-posnr.
*        ls_output-serial_num = ls_serial-sernr.
*        APPEND ls_output TO gt_output.
*      ENDLOOP.
*    ELSE.
*      APPEND ls_output TO gt_output.
*    ENDIF.
*
*    CLEAR: ls_likp, ls_lips, ls_mara,
*           ls_vbpa, ls_adrc,
*           ls_serial, ls_output.
*  ENDLOOP.
*
*  SORT gt_output BY deliv_date
*                    deliv_numb
*                    itm_number
*                    serial_num.
*
**  DATA: ls_likp   TYPE likp,
**        ls_lips   TYPE lips,
**        ls_mara   TYPE mara,
**        ls_serial TYPE ty_serial,
**        ls_output TYPE ty_output.
**
**  LOOP AT gt_lips INTO ls_lips.
**
**    READ TABLE gt_mara INTO ls_mara WITH KEY matnr = ls_lips-matnr.
**    CHECK sy-subrc = 0.
**
**    ls_output-vbeln = ls_lips-vbeln.
**    ls_output-posnr = ls_lips-posnr.
**    ls_output-matnr = ls_mara-matnr.
**
**    READ TABLE gt_likp INTO ls_likp WITH KEY vbeln = ls_lips-vbeln.
**    IF sy-subrc = 0.
**      ls_output-lfdat = ls_likp-lfdat.
**    ENDIF.
**
**    READ TABLE gt_serial INTO ls_serial WITH KEY lief_nr = ls_lips-vbeln
**                                                 posnr   = ls_lips-posnr.
**    IF sy-subrc = 0.
**      CLEAR ls_serial.
**      LOOP AT gt_serial INTO ls_serial WHERE lief_nr = ls_lips-vbeln
**                                       AND   posnr   = ls_lips-posnr.
**        ls_output-sernr = ls_serial-sernr.
**        APPEND ls_output TO gt_output.
**      ENDLOOP.
**    ELSE.
**      APPEND ls_output TO gt_output.
**    ENDIF.
**
**    CLEAR: ls_likp, ls_lips, ls_mara, ls_serial, ls_output.
**  ENDLOOP.
**
**  SORT gt_output BY lfdat vbeln posnr sernr.

ENDFORM.                    " PREPARE_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV_OUTPUT.

  PERFORM BUILD_LAYOUT  USING GS_LAYOUT.
  PERFORM BUILD_CATALOG USING GT_FIELDCAT[].
  PERFORM BUILD_EVENTS  USING GT_EVENTS[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = GC_REPID
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      IS_LAYOUT                = GS_LAYOUT
      IT_FIELDCAT              = GT_FIELDCAT
      I_SAVE                   = 'A'
*     is_variant               = gt_variant
      IT_EVENTS                = GT_EVENTS
    TABLES
      T_OUTTAB                 = GT_OUTPUT
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.

  IF SY-SUBRC <> 0.
    MESSAGE E018.
  ENDIF.

ENDFORM.                    " DISPLAY_ALV_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT USING IS_LAYOUT TYPE SLIS_LAYOUT_ALV.

  IS_LAYOUT-BOX_FIELDNAME     = 'SEL'.
  IS_LAYOUT-WINDOW_TITLEBAR   = SY-TITLE.
  IS_LAYOUT-ZEBRA             = 'X'.
  IS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

ENDFORM.                    " BUILD_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  BUILD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BUILD_CATALOG USING RT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

  PERFORM APPEND_FIELDCAT
    USING 'PRINT_NUMB' '' '' '' '' 'Number of Copies'.   "number of copies
  PERFORM APPEND_FIELDCAT
    USING 'DELIV_DATE' 'ZSDSSDS059' 'DELIV_DATE' '' '' 'Delivery Date'.   "delivery date
  PERFORM APPEND_FIELDCAT
    USING 'DELIV_NUMB' 'ZSDSSDS059' 'DELIV_NUMB' '' '' 'Delivery Number'.   "delivery order number
  PERFORM APPEND_FIELDCAT
    USING 'ITM_NUMBER' 'ZSDSSDS059' 'ITM_NUMBER' '' '' 'Item'.   "item number
  PERFORM APPEND_FIELDCAT
    USING 'DELIV_QTY'  'ZSDSSDS059' 'DELIV_QTY'
                       'ZSDSSDS059' 'SALES_UNIT' 'Quantity'.         "delivery quantity
  PERFORM APPEND_FIELDCAT
    USING 'MATERIAL'   'ZSDSSDS059' 'MATERIAL'   '' '' 'Material'.   "material
  PERFORM APPEND_FIELDCAT
    USING 'SERIAL_NUM' 'ZSDSSDS059' 'SERIAL_NUM' '' '' 'Serial Number'.   "serial number
  PERFORM APPEND_FIELDCAT
    USING 'LGORT' 'LIPS' 'LGORT' '' '' 'Stor. Location'.   "Storage Location Add by Aphai on 30.10.2014
  PERFORM APPEND_FIELDCAT
    USING 'SALES_SP'   'KNA1'   'NAME1' '' '' 'Customer'.            "customer
  PERFORM APPEND_FIELDCAT
    USING 'MAT_DIVI'   'VBAP'   'SPART' '' '' 'Material Division'.   "material devision
  PERFORM APPEND_FIELDCAT
    USING 'SALES_EMP'  'PA0001' 'ENAME' '' '' 'Sales Employee'.      "sales employee
*  PERFORM append_fieldcat
*    USING 'SALES_ORG'  'TVKOT'  'VTEXT' '' '' 'Sales Orgnization'.   "sales organization
  PERFORM APPEND_FIELDCAT
    USING 'DISTR_CHN'  'TVTWT'  'VTEXT' '' '' 'Distribution Channel'.   "distribution channel
  PERFORM APPEND_FIELDCAT
    USING 'SALES_OFF'  'TVKBT'  'BEZEI' '' '' 'Sales Office'.        "sales office
  PERFORM APPEND_FIELDCAT
    USING 'ISWAR'  ''  '' '' '' 'W/R'.        " Is Warranty

**  PERFORM append_fieldcat
**    USING 'PRINT_NUMB' 'ZSDSSDT019'   'PRINT_NUMB' '' ''.   "number of copies
*  PERFORM append_fieldcat
*    USING 'DELIV_DATE' 'ZSDSSDS059' 'DELIV_DATE' '' ''.   "delivery date
*  PERFORM append_fieldcat
*    USING 'DELIV_NUMB' 'ZSDSSDS059' 'DELIV_NUMB' '' ''.   "delivery order number
*  PERFORM append_fieldcat
*    USING 'ITM_NUMBER' 'ZSDSSDS059' 'ITM_NUMBER' '' ''.   "item number
*  PERFORM append_fieldcat
*    USING 'DELIV_QTY'  'ZSDSSDS059' 'DELIV_QTY'
*                       'ZSDSSDS059' 'SALES_UNIT'.         "delivery quantity
** PERFORM append_fieldcat
**   USING 'SALES_UNIT' 'ZSDSSDS059' 'SALES_UNIT' '' ''.   "sales unit
*  PERFORM append_fieldcat
*    USING 'MATERIAL'   'ZSDSSDS059' 'MATERIAL'   '' ''.   "material
*  PERFORM append_fieldcat
*    USING 'SERIAL_NUM' 'ZSDSSDS059' 'SERIAL_NUM' '' ''.   "serial number
*  PERFORM append_fieldcat
*    USING 'SALES_SP'   'KNA1'   'NAME1' '' ''.   "customer
*  PERFORM append_fieldcat
*    USING 'MAT_DIVI'   'VBAP'   'SPART' '' ''.   "material devision
*  PERFORM append_fieldcat
*    USING 'SALES_EMP'  'PA0001' 'ENAME' '' ''.   "sales employee
*  PERFORM append_fieldcat
*    USING 'SALES_ORG'  'TVKOT'  'VTEXT' '' ''.   "sales organization
*  PERFORM append_fieldcat
*    USING 'SALES_OFF'  'TVKBT'  'BEZEI' '' ''.   "sales office

ENDFORM.                    " BUILD_CATALOG

*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM APPEND_FIELDCAT USING IV_FIELD        "Field name
                           IV_REFTABLE     "Reference Table name
                           IV_REFFIELD     "Reference Field name
                           IV_QTABNAME     "Table with quantity unit
                           IV_QFIELDNAME   "Field with quantity unit
*                          iv_edit         "Editable
                           IV_COLTXT.      "Col Text(for specify)

  DATA: LS_FIELDCAT      TYPE SLIS_FIELDCAT_ALV,
        LV_POS           TYPE I,
        LV_COLTXT_LENGTH TYPE I.

  ADD 1 TO LV_POS.

  LS_FIELDCAT-COL_POS       = LV_POS.
  LS_FIELDCAT-FIELDNAME     = IV_FIELD.
  LS_FIELDCAT-REF_TABNAME   = IV_REFTABLE.
  LS_FIELDCAT-REF_FIELDNAME = IV_REFFIELD.
*  IF iv_field = 'DELIV_QTY'.
  LS_FIELDCAT-QTABNAME      = IV_QTABNAME.
  LS_FIELDCAT-QFIELDNAME    = IV_QFIELDNAME.
*  ENDIF.
* ls_fieldcat-edit          = iv_edit.

*If we need to specify text, no need to derive from data dictionary
*program will check length and define width of the column
  IF NOT IV_COLTXT IS INITIAL.
    LV_COLTXT_LENGTH = STRLEN( IV_COLTXT ).
    IF LV_COLTXT_LENGTH > 20.
      LS_FIELDCAT-DDICTXT = 'L'."Long text
      LS_FIELDCAT-SELTEXT_L = IV_COLTXT.
    ELSEIF LV_COLTXT_LENGTH > 10.
      LS_FIELDCAT-DDICTXT = 'M'."Medium Text
      LS_FIELDCAT-SELTEXT_M = IV_COLTXT.
    ELSE.
      LS_FIELDCAT-DDICTXT = 'S'."Short Text
      LS_FIELDCAT-SELTEXT_S = IV_COLTXT.
    ENDIF.
    LS_FIELDCAT-REPTEXT_DDIC = IV_COLTXT.
  ENDIF.

  APPEND LS_FIELDCAT TO GT_FIELDCAT.

ENDFORM.                    " APPEND_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BUILD_EVENTS USING IT_EVENTS TYPE SLIS_T_EVENT.

*  DATA: ls_event LIKE LINE OF t_events.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    IMPORTING
      ET_EVENTS = IT_EVENTS.

*  READ TABLE it_events INTO ls_event WITH KEY name = slis_ev_user_command.

ENDFORM.                    " BUILD_EVENTS

*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_PF_STATUS USING IT_EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'ZSTANDARD'.

ENDFORM.                    " SET_PF_STATUS

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM     LIKE SY-UCOMM
                        RS_SELFIELD TYPE SLIS_SELFIELD.

  DATA: LR_GRID     TYPE REF TO CL_GUI_ALV_GRID.
  DATA: LT_OUTPUT TYPE STANDARD TABLE OF TY_OUTPUT,
        LS_OUTPUT TYPE TY_OUTPUT.
  DATA: LT_WARRANTY TYPE ZSDSSDS059_TT,
        LS_WARRANTY TYPE ZSDSSDS059.
  DATA: LV_RETCODE  TYPE SY-SUBRC.

  LT_OUTPUT[] = GT_OUTPUT[].
  DELETE LT_OUTPUT WHERE SEL EQ ' '.
  LT_WARRANTY[] = LT_OUTPUT[].

* Check function code
  CASE R_UCOMM.

    WHEN '&PRT_PRV'.  "print preview
      CHECK LT_WARRANTY[] IS NOT INITIAL.
      PERFORM PRINT_WARRANTY_CARD USING 'X'         "preview flag
                                        LT_WARRANTY
                               CHANGING LV_RETCODE.

    WHEN '&PRT'.      "print
      CHECK LT_WARRANTY[] IS NOT INITIAL.
      PERFORM PRINT_WARRANTY_CARD USING ' '         "preview flag
                                        LT_WARRANTY
                               CHANGING LV_RETCODE.
      CHECK LV_RETCODE IS INITIAL.
      PERFORM UPDATE_WARRANTYLOG USING LT_WARRANTY
                              CHANGING LV_RETCODE.

    WHEN '&IC1'.      "double click
      CHECK RS_SELFIELD-TABINDEX > 0.
      IF RS_SELFIELD-FIELDNAME = 'DELIV_NUMB' AND
         RS_SELFIELD-VALUE IS NOT INITIAL.
        SET PARAMETER ID 'VL' FIELD RS_SELFIELD-VALUE.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
      ENDIF.
      IF RS_SELFIELD-FIELDNAME = 'SERIAL_NUM' AND
         RS_SELFIELD-VALUE IS NOT INITIAL.
        READ TABLE GT_OUTPUT INTO LS_OUTPUT INDEX RS_SELFIELD-TABINDEX.
        SET PARAMETER ID: 'MAT' FIELD LS_OUTPUT-MATERIAL,
                          'SER' FIELD RS_SELFIELD-VALUE.
        CALL TRANSACTION 'IQ03' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN '&REFR'.    "refresh

  ENDCASE.

  PERFORM CLEAR_DATA.
  PERFORM GET_DATA.
  PERFORM SPLIT_WARRANTY TABLES GT_DELIV[]. " add by aphai on 31.10.2014
  PERFORM PREPARE_OUTPUT.
  PERFORM F_DELETE_DATA.
  RS_SELFIELD-REFRESH = ABAP_TRUE.  " refresh ALV list

**  BREAK-POINT.
**  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
**    IMPORTING
**      e_grid = lr_grid.
**  CALL METHOD lr_grid->check_changed_data .
*
** Check function code
*  CASE r_ucomm.
*
*    WHEN '&PRT_PRV'.
*      LOOP AT gt_output INTO ls_output WHERE sel EQ 'X'.
**        IF ls_output-pages > 0.
*        gv_preview = 'X'.
*        PERFORM print_warranty_card USING ls_output.
**        ENDIF.
*      ENDLOOP.
*
*    WHEN '&PRT'.
*      LOOP AT gt_output INTO ls_output WHERE sel EQ 'X'.
**        IF ls_output-pages > 0.
*        gv_preview = ' '.
*        PERFORM print_warranty_card USING ls_output.
**        ENDIF.
*      ENDLOOP.
*
*  ENDCASE.

ENDFORM.                    " USER_COMMAND

**&---------------------------------------------------------------------*
**&      Form  GET_DATA_DELIVERY
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM get_data_delivery
*              USING    is_print_data_to_read TYPE ledlv_print_data_to_read
*              CHANGING cs_addr_key           LIKE addr_key
*                       cs_dlv_delnote        TYPE ledlv_delnote
*                       cf_retcode.
*
*  DATA: ls_delivery_key TYPE  leshp_delivery_key.
*
*  ls_delivery_key-vbeln = gs_nast-objky.
*
*  CALL FUNCTION 'LE_SHP_DLV_OUTP_READ_PRTDATA'
*    EXPORTING
*      is_delivery_key       = ls_delivery_key
*      is_print_data_to_read = is_print_data_to_read
*      if_parvw              = gs_nast-parvw
*      if_parnr              = gs_nast-parnr
*      if_language           = gs_nast-spras
*    IMPORTING
*      es_dlv_delnote        = cs_dlv_delnote
*    EXCEPTIONS
*      records_not_found     = 1
*      records_not_requested = 2
*      OTHERS                = 3.
*
*  PERFORM get_addr_key_delivery
*                       USING    cs_dlv_delnote-hd_adr
*                       CHANGING cs_addr_key.
*
*ENDFORM.                    " GET_DATA_DELIVERY
*
**&---------------------------------------------------------------------*
**&      Form  GET_ADDR_KEY_DELIVERY
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM get_addr_key_delivery
*                  USING    it_hd_adr   TYPE ledlv_delnote-hd_adr
*                  CHANGING cs_addr_key LIKE addr_key.
*
*  FIELD-SYMBOLS <fs_hd_adr> TYPE LINE OF ledlv_delnote-hd_adr.
*
*  READ TABLE it_hd_adr ASSIGNING <fs_hd_adr>
*                       WITH KEY deliv_numb = gs_nast-objky
*                                partn_role = gs_nast-parvw.
*  IF sy-subrc = 0.
*    cs_addr_key-addrnumber = <fs_hd_adr>-addr_no.
*    cs_addr_key-persnumber = <fs_hd_adr>-person_numb.
*    cs_addr_key-addr_type  = <fs_hd_adr>-address_type.
*  ENDIF.
*
*ENDFORM.                    " GET_ADDR_KEY_DELIVERY
*
**&---------------------------------------------------------------------*
**&      Form  SET_PRINT_PARAM_DELIVERY
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM set_print_param_delivery
*                     USING    is_addr_key       LIKE addr_key
*                     CHANGING cs_control_param  TYPE ssfctrlop
*                              cs_composer_param TYPE ssfcompop
*                              cs_recipient      TYPE swotobjid
*                              cs_sender         TYPE swotobjid
*                              cf_retcode        TYPE sy-subrc.
*
*  DATA: ls_itcpo     TYPE itcpo.
*  DATA: lf_repid     TYPE sy-repid.
*  DATA: lf_device    TYPE tddevice.
*  DATA: ls_recipient TYPE swotobjid.
*  DATA: ls_sender    TYPE swotobjid.
*
*  lf_repid = sy-repid.
*
*  CALL FUNCTION 'WFMC_PREPARE_SMART_FORM'
*    EXPORTING
*      pi_nast       = gs_nast
*      pi_addr_key   = is_addr_key
*      pi_repid      = lf_repid
*    IMPORTING
*      pe_returncode = cf_retcode
*      pe_itcpo      = ls_itcpo
*      pe_device     = lf_device
*      pe_recipient  = cs_recipient
*      pe_sender     = cs_sender.
*
*  IF cf_retcode = 0.
*    MOVE-CORRESPONDING ls_itcpo TO cs_composer_param.
*    cs_control_param-device      = lf_device.
*    cs_control_param-no_dialog   = 'X'.
*    cs_control_param-preview     = gv_preview.
*    cs_control_param-getotf      = ls_itcpo-tdgetotf.
*    cs_control_param-langu       = gs_nast-spras.
*  ENDIF.
*
*ENDFORM.                    " SET_PRINT_PARAM_DELIVERY

*&---------------------------------------------------------------------*
*&      Form  PREPARE_WARRANTY_CARD
*&---------------------------------------------------------------------*
*       Called in Z_SD_RLE_DELNOTE when print DO
*----------------------------------------------------------------------*
FORM PREPARE_WARRANTY_CARD USING IV_DELIV_NUMB TYPE VBELN
                                 IV_DELIV_DATE TYPE LFDAT
                        CHANGING CT_WARRANTY   TYPE ZSDSSDS059_TT
                                 CV_RETCODE    TYPE SY-SUBRC.

  DATA: LT_CONDITION TYPE STANDARD TABLE OF ZSDSSDC007,
        LS_CONDITION TYPE ZSDSSDC007,
        LT_KONV      TYPE STANDARD TABLE OF PRCD_ELEMENTS,
        LS_KONV      TYPE PRCD_ELEMENTS.
  DATA: LS_RANGE  TYPE DDRANGE,
        LS_OUTPUT TYPE TY_OUTPUT.
  DATA: LV_INDEX   TYPE SY-TABIX,
        LV_FOUND   TYPE XFELD,
        LV_PH1     TYPE CHAR5,
        LV_PH2     TYPE CHAR5,
        LV_PH3     TYPE CHAR8,
        LV_PERCENT TYPE KWERT.
  ""Add by Wantanee 20150521 ITR2015-3851
  DATA: LV_IO_AUART   TYPE AUFK-AUART,
        LV_CHECK_CUST TYPE C.

  "End Add by Wantanee 20150521 ITR2015-3851
  "Add by Wantanee 20190726 ITR20190521-149
  DATA: LV_SO_ITEM(16) TYPE C.
  DATA: LV_SO_AUART TYPE VBAK-AUART.
  DATA: LV_CHK_WARRANTY TYPE C.
  "End Add by Wantanee 20190726 ITR20190521-149

  DATA: LV_AUART TYPE VBAK-AUART.
* Build range table
  LS_RANGE-SIGN   = 'I'.
  LS_RANGE-OPTION = 'EQ'.
  LS_RANGE-LOW    = IV_DELIV_NUMB.
  APPEND LS_RANGE TO S_VBELN.

  CLEAR LS_RANGE.
  LS_RANGE-SIGN   = 'I'.
  LS_RANGE-OPTION = 'EQ'.
  LS_RANGE-LOW    = IV_DELIV_DATE.
  APPEND LS_RANGE TO S_LFDAT.

* Clear data
  PERFORM CLEAR_DATA.
* Get data
  PERFORM GET_DATA.
* Prepare output
  PERFORM PREPARE_OUTPUT.

* Get print warranty condition
  IF GT_OUTPUT[] IS NOT INITIAL.
    SELECT *
      FROM ZSDSSDC007
      INTO TABLE LT_CONDITION
      FOR ALL ENTRIES IN GT_OUTPUT
      WHERE VTWEG = GT_OUTPUT-VTWEG
      AND   VKBUR = GT_OUTPUT-VKBUR.
*      AND   wdate <= gt_output-mat_creadt.
    SORT LT_CONDITION BY VTWEG.

    SELECT *
      FROM PRCD_ELEMENTS
      INTO TABLE LT_KONV
      FOR ALL ENTRIES IN GT_OUTPUT
      WHERE KNUMV = GT_OUTPUT-CONDITION
      AND   KPOSN = GT_OUTPUT-ITM_NUMBER
      AND   KSCHL = 'ZPR0'.   "Price


    LOOP AT GT_OUTPUT INTO LS_OUTPUT.
      CLEAR: LV_AUART.
      LV_INDEX = SY-TABIX.

      SELECT SINGLE AUART
      INTO LV_AUART
      FROM VBAK
      WHERE VBELN EQ LS_OUTPUT-SO_NO.

      "Add by Wantanee 20150521 ITR2015-3851
      SELECT SINGLE AUART
      INTO LV_IO_AUART
      FROM AUFK
      WHERE AUFNR EQ LS_OUTPUT-AUFNR.

      IF LV_IO_AUART EQ 'H004'.
        READ TABLE GT_CUST_WARRANTY INTO WA_CUST_WARRANTY WITH KEY KUNNR = LS_OUTPUT-SOLD_TO_PARTY
                                                                    VKBUR = LS_OUTPUT-VKBUR.
        IF SY-SUBRC EQ 0.
          LV_CHECK_CUST = 'X'.
        ENDIF.

      ENDIF.

      IF LV_CHECK_CUST IS INITIAL.

        LV_PH1 = LS_OUTPUT-PROD_HIER+0(5).
        LV_PH2 = LS_OUTPUT-PROD_HIER+5(5).
        LV_PH3 = LS_OUTPUT-PROD_HIER+10(8).



*        CASE LS_OUTPUT-VTWEG.
*          WHEN '10'.
*            READ TABLE GT_ZSDSSDC006 INTO GW_ZSDSSDC006 WITH KEY MATNR =  LS_OUTPUT-MATERIAL
*                                                                    AUART =  LV_AUART.
*            IF SY-SUBRC EQ 0.
*              LV_FOUND = 'X'.
*
*            ELSE.
*
*              LOOP AT LT_CONDITION INTO LS_CONDITION
*                WHERE VTWEG = LS_OUTPUT-VTWEG
*                  AND VKBUR = LS_OUTPUT-VKBUR
*                  AND PH1   = LV_PH1
*                  AND PH2   = LV_PH2
*                  AND WDATE <= LS_OUTPUT-MAT_CREADT.
*                LV_FOUND = 'X'.
*                EXIT.
*              ENDLOOP.
*
*              IF LV_FOUND EQ SPACE.
*                CALL FUNCTION 'ZSD_CHK_WARRANTY_SP'
*                  EXPORTING
*                    P_MATNR    = LS_OUTPUT-MATERIAL
*                    P_VTWEG    = LS_OUTPUT-VTWEG
*                    P_VKBUR    = LS_OUTPUT-VKBUR
*                  IMPORTING
*                    ISWARRANTY = LV_FOUND
*                  TABLES
*                    ITAB       = DT_CONST[].
*              ENDIF.
*              IF LV_FOUND EQ SPACE.
*
*                BREAK WANTANEE.
*                SELECT SINGLE AUART
*                  INTO LV_SO_AUART
*                  FROM VBAK
*                  WHERE VBELN = LS_OUTPUT-SO_NO.
*
*
**
*                IF LV_SO_AUART EQ 'ZO07'.
*
*                  CONCATENATE LS_OUTPUT-SO_NO LS_OUTPUT-SO_ITEM INTO LV_SO_ITEM.
*                  PERFORM READ_TEXT_ITEM  USING 'ZI13' 'VBBP' LV_SO_ITEM
*                  CHANGING LV_CHK_WARRANTY.
*                  IF LV_CHK_WARRANTY = 'X' OR LV_CHK_WARRANTY = 'x'.
*                    LV_FOUND = 'X'.
*                  ENDIF.
*
*                ENDIF.
*              ENDIF.
*            ENDIF.
*
*          WHEN '20'.
*
*            READ TABLE LT_KONV INTO LS_KONV WITH KEY KNUMV = LS_OUTPUT-CONDITION
*                                                        KPOSN = LS_OUTPUT-ITM_NUMBER.
*            IF SY-SUBRC EQ 0.
*              LV_PERCENT = ( LS_OUTPUT-NET_VALUE * 100 ) / LS_KONV-KWERT.
*              LOOP AT LT_CONDITION INTO LS_CONDITION
*                WHERE VTWEG = LS_OUTPUT-VTWEG
*                  AND VKBUR = LS_OUTPUT-VKBUR
*                  AND PH1   = LV_PH1
*                  AND PH2   = LV_PH2
*                  AND PH3   =  LV_PH3
*
*                  AND WDATE <= LS_OUTPUT-MAT_CREADT
*                  AND LP_PERCENT <= LV_PERCENT.
*                LV_FOUND = 'X'.
*                EXIT.
*              ENDLOOP.
*
*              IF LV_FOUND EQ SPACE.
*                LOOP AT LT_CONDITION INTO LS_CONDITION
*                  WHERE VTWEG = LS_OUTPUT-VTWEG
*                  AND VKBUR = LS_OUTPUT-VKBUR
*                  AND PH1   = LV_PH1
*                  AND PH2   = LV_PH2
*                  AND PH3   =  SPACE
*                  AND WDATE <= LS_OUTPUT-MAT_CREADT
*                  AND LP_PERCENT <= LV_PERCENT.
*                  LV_FOUND = 'X'.
*                  EXIT.
*                ENDLOOP.
*              ENDIF.
*            ENDIF.
*
*            IF LV_FOUND EQ SPACE.
*              CALL FUNCTION 'ZSD_CHK_WARRANTY_SP'
*                EXPORTING
*                  P_MATNR    = LS_OUTPUT-MATERIAL
*                  P_VTWEG    = LS_OUTPUT-VTWEG
*                  P_VKBUR    = LS_OUTPUT-VKBUR
*                  P_NETWR    = LS_OUTPUT-NET_VALUE
*                  P_KWERT    = LS_KONV-KWERT
*                IMPORTING
*                  ISWARRANTY = LV_FOUND
*                TABLES
*                  ITAB       = DT_CONST[].
*            ENDIF.
*        ENDCASE.


        IF LV_FOUND IS INITIAL.
          DELETE GT_OUTPUT INDEX LV_INDEX.
        ENDIF.
        CLEAR: LS_OUTPUT, LS_CONDITION, LS_KONV.
        CLEAR: LV_INDEX, LV_FOUND, LV_PH1, LV_PH2, LV_PH3, LV_PERCENT.

      ENDIF.
    ENDLOOP.

  ENDIF.

  CT_WARRANTY[] = GT_OUTPUT[].
  IF CT_WARRANTY[] IS INITIAL.
    CV_RETCODE = 1.
  ENDIF.

ENDFORM.                    " PREPARE_WARRANTY_CARD

*&---------------------------------------------------------------------*
*&      Form  PRINT_WARRANTY_CARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PRINT_WARRANTY_CARD USING IV_PREVIEW  TYPE CHAR1
                               IT_WARRANTY TYPE ZSDSSDS059_TT
                      CHANGING CV_RETCODE  TYPE SY-SUBRC.

  DATA: LS_CONTROL_PARAM  TYPE SSFCTRLOP.
  DATA: LS_COMPOSER_PARAM TYPE SSFCOMPOP.
  DATA: LV_FM_NAME        TYPE RS38L_FNAM.

* determine smartform function module for warranty card
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = GC_WARRANTY_FORMNAME
*     variant            = ' '
*     direct_call        = ' '
    IMPORTING
      FM_NAME            = LV_FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.

  IF SY-SUBRC = 0.

    LS_COMPOSER_PARAM-TDARMOD    = '1'.
    LS_COMPOSER_PARAM-TDDEST     = 'SDPF'.
    LS_COMPOSER_PARAM-TDIMMED    = 'X'.
    LS_COMPOSER_PARAM-TDDELETE   = 'X'.
    LS_COMPOSER_PARAM-TDRECEIVER = SY-UNAME.
*   ls_composer_param-tdcopies   = is_output-pages.
    LS_COMPOSER_PARAM-TDCOPIES   = '001'.
    LS_CONTROL_PARAM-DEVICE      = 'PRINTER'.
    LS_CONTROL_PARAM-NO_DIALOG   = 'X'.
    LS_CONTROL_PARAM-PREVIEW     = IV_PREVIEW.
    LS_CONTROL_PARAM-LANGU       = SY-LANGU.

*   call smartform warranty card
    CALL FUNCTION LV_FM_NAME
      EXPORTING
*       archive_index      = toa_dara
*       archive_parameters = arc_params
        CONTROL_PARAMETERS = LS_CONTROL_PARAM
*       mail_appl_obj      =
*       mail_recipient     = is_recipient
*       mail_sender        = is_sender
        OUTPUT_OPTIONS     = LS_COMPOSER_PARAM
        USER_SETTINGS      = ' '
*       is_dlv_delnote     = ls_dlv_delnote
*       is_nast            = nast
      TABLES
        IT_WARRANTY        = IT_WARRANTY
*      importing  document_output_info =
*       job_output_info    =
*       job_output_options =
      EXCEPTIONS
        FORMATTING_ERROR   = 1
        INTERNAL_ERROR     = 2
        SEND_ERROR         = 3
        USER_CANCELED      = 4
        OTHERS             = 5.

    IF SY-SUBRC = 0.
    ELSE.
      CV_RETCODE = SY-SUBRC.
    ENDIF.

  ELSE.
    CV_RETCODE = SY-SUBRC.
  ENDIF.

ENDFORM.                    " PRINT_WARRANTY_CARD
*FORM print_warranty_card USING is_output TYPE ty_output.
*
*  DATA: lt_nast               TYPE STANDARD TABLE OF nast.
*  DATA: ls_print_data_to_read TYPE ledlv_print_data_to_read.
*  DATA: ls_dlv_delnote        TYPE ledlv_delnote.
*  DATA: ls_addr_key           LIKE addr_key.
*  DATA: ls_control_param      TYPE ssfctrlop.
*  DATA: ls_composer_param     TYPE ssfcompop.
*  DATA: ls_recipient          TYPE swotobjid.
*  DATA: ls_sender             TYPE swotobjid.
*  DATA: lv_fm_name            TYPE rs38l_fnam.
*  DATA: lv_retcode            TYPE sy-subrc.
*
**  READ TABLE gt_output INTO ls_output INDEX rs_selfield-tabindex.
*
*  SELECT * FROM nast INTO TABLE lt_nast
*    WHERE objky = is_output-vbeln
*    AND ( kschl = 'ZD01' OR
*          kschl = 'ZTD1' )
*    AND   spras = sy-langu.
*  SORT lt_nast BY erdat DESCENDING
*                  eruhr DESCENDING.
*  READ TABLE lt_nast INTO gs_nast INDEX 1.
*
** SELECT single * FROM vlkpa INTO ls_vlkpa
**   WHERE vbeln = ls_output-vbeln.
*
*  PERFORM set_print_data_to_read IN PROGRAM z_sd_rle_delnote IF FOUND
*                                 USING    gc_delivery_formname
*                                 CHANGING ls_print_data_to_read
*                                          lv_retcode.
*  IF lv_retcode = 0.
**   PERFORM get_data IN PROGRAM z_sd_rle_delnote IF FOUND
*    PERFORM get_data_delivery
*                     USING    ls_print_data_to_read
*                     CHANGING ls_addr_key
*                              ls_dlv_delnote
*                              lv_retcode.
*  ENDIF.
*  IF lv_retcode = 0.
**   PERFORM set_print_param IN PROGRAM z_sd_rle_delnote IF FOUND
*    PERFORM set_print_param_delivery
*                            USING    ls_addr_key
*                            CHANGING ls_control_param
*                                     ls_composer_param
*                                     ls_recipient
*                                     ls_sender
*                                     lv_retcode.
*  ENDIF.
*
*  IF lv_retcode = 0.
*
**   print only selected line item
*    DELETE ls_dlv_delnote-it_gen WHERE itm_number NE is_output-posnr.
*
*    IF ls_dlv_delnote-it_gen[] IS NOT INITIAL.
*
**   determine smartform function module for warranty card
*      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*           EXPORTING  formname           = gc_warranty_formname
**                     variant            = ' '
**                     direct_call        = ' '
*           IMPORTING  fm_name            = lv_fm_name
*           EXCEPTIONS no_form            = 1
*                      no_function_module = 2
*                      OTHERS             = 3.
*
*      IF sy-subrc = 0.
*
*        ls_composer_param-tddest     = 'EPSO'.
**        ls_composer_param-tdcopies   = is_output-pages.
**        ls_control_param-device      = 'PRINTER'.
*        ls_control_param-no_dialog   = 'X'.
*
**   call smartform warranty card
*        CALL FUNCTION lv_fm_name
*             EXPORTING
**                     ARCHIVE_INDEX        = TOA_DARA
**                     ARCHIVE_PARAMETERS   = ARC_PARAMS
*                      control_parameters   = ls_control_param
**                     mail_appl_obj        =
**                     MAIL_RECIPIENT       = IS_RECIPIENT
**                     MAIL_SENDER          = IS_SENDER
*                      output_options       = ls_composer_param
*                      user_settings        = ' '
*                      is_dlv_delnote       = ls_dlv_delnote
**                     IS_NAST              = NAST
**          importing  document_output_info =
**                     job_output_info      =
**                     job_output_options   =
*           EXCEPTIONS formatting_error     = 1
*                      internal_error       = 2
*                      send_error           = 3
*                      user_canceled        = 4
*                      OTHERS               = 5.
*
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                    " PRINT_WARRANTY_CARD

*&---------------------------------------------------------------------*
*&      Form  UPDATE_WARRANTYLOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM UPDATE_WARRANTYLOG USING IT_WARRANTY TYPE ZSDSSDS059_TT
                     CHANGING CV_RETCODE  TYPE SY-SUBRC.

  DATA: LS_WARRANTY    TYPE ZSDSSDS059,
        LS_WARRANTYLOG TYPE ZSDSSDT019,
        LV_PRINT_SEQU  TYPE ZSDSSDT019-PRINT_SEQU.

  CLEAR LS_WARRANTY.
  LOOP AT IT_WARRANTY INTO LS_WARRANTY.

    CALL FUNCTION 'ENQUEUE_EZSDSSDT019'
      EXPORTING
        MODE_ZSDSSDT019 = 'E'
        CLIENT          = SY-MANDT
        DELIV_NUMB      = LS_WARRANTY-DELIV_NUMB
        ITM_NUMBER      = LS_WARRANTY-ITM_NUMBER
        SERIAL_NUM      = LS_WARRANTY-SERIAL_NUM
        PRINT_SEQU      = LV_PRINT_SEQU.
*       X_DELIV_NUMB          = ' '
*       _SCOPE                = '2'
*       _WAIT                 = ' '
*       _COLLECT              = ' '
*     EXCEPTIONS
*       FOREIGN_LOCK          = 1
*       SYSTEM_FAILURE        = 2
*       OTHERS                = 3.
    IF SY-SUBRC = 0.

      READ TABLE GT_WARRANTYLOG INTO LS_WARRANTYLOG WITH KEY DELIV_NUMB = LS_WARRANTY-DELIV_NUMB
                                                             ITM_NUMBER = LS_WARRANTY-ITM_NUMBER
                                                             SERIAL_NUM = LS_WARRANTY-SERIAL_NUM.
      IF SY-SUBRC = 0.
        LV_PRINT_SEQU = LS_WARRANTYLOG-PRINT_SEQU + 1.
      ELSE.
        LV_PRINT_SEQU = 1.
      ENDIF.

      LS_WARRANTYLOG-DELIV_NUMB = LS_WARRANTY-DELIV_NUMB.
      LS_WARRANTYLOG-ITM_NUMBER = LS_WARRANTY-ITM_NUMBER.
      LS_WARRANTYLOG-SERIAL_NUM = LS_WARRANTY-SERIAL_NUM.
      LS_WARRANTYLOG-PRINT_SEQU = LV_PRINT_SEQU.
      LS_WARRANTYLOG-DELIV_DATE = LS_WARRANTY-DELIV_DATE.
      LS_WARRANTYLOG-MATERIAL   = LS_WARRANTY-MATERIAL.
      LS_WARRANTYLOG-PRINT_DATE = SY-DATUM.
      LS_WARRANTYLOG-PRINT_TIME = SY-UZEIT.

      INSERT ZSDSSDT019 FROM LS_WARRANTYLOG.
    ELSE.
      CV_RETCODE = 4.
    ENDIF.

    CALL FUNCTION 'DEQUEUE_EZSDSSDT019'
      EXPORTING
        MODE_ZSDSSDT019 = 'E'
        CLIENT          = SY-MANDT
        DELIV_NUMB      = LS_WARRANTY-DELIV_NUMB
        ITM_NUMBER      = LS_WARRANTY-ITM_NUMBER
        SERIAL_NUM      = LS_WARRANTY-SERIAL_NUM
        PRINT_SEQU      = LV_PRINT_SEQU.
*       X_DELIV_NUMB          = ' '
*       _SCOPE                = '3'
*       _SYNCHRON             = ' '
*       _COLLECT              = ' '

    CLEAR LS_WARRANTY.
  ENDLOOP.

ENDFORM.                    " UPDATE_WARRANTYLOG

* Add by Aphai On 30.10.2014
FORM SPLIT_WARRANTY TABLES
                      ITAB STRUCTURE GT_DELIV.
  CHECK ITAB[] IS NOT INITIAL.
  DATA : GT_WARRANTY TYPE TABLE OF ZSDSSDC007 WITH HEADER LINE,
         WA_WARRANTY TYPE ZSDSSDC007,
         PERCENT     TYPE ZSDSSDC007-LP_PERCENT.

  FIELD-SYMBOLS : <FS1> TYPE TY_DELIV.

  DATA: PH1 TYPE ZSDSSDC007-PH1,
        PH2 TYPE ZSDSSDC007-PH2,
        PH3 TYPE ZSDSSDC007-PH3.
  ""Add by Wantanee 20150521 ITR2015-3851
  DATA: LV_IO_AUART   TYPE AUFK-AUART,
        LV_CHECK_CUST TYPE C.
  "End Add by Wantanee 20150521 ITR2015-3851
  SELECT * INTO TABLE GT_WARRANTY FROM ZSDSSDC007.


  LOOP AT ITAB ASSIGNING <FS1>.
    PERFORM GD_VBAK USING <FS1>-VGBEL CHANGING <FS1>-VTWEG <FS1>-VKBUR <FS1>-KNUMV <FS1>-AUART.
    PERFORM GD_VBAP USING <FS1>-VGBEL <FS1>-VGPOS CHANGING <FS1>-NETWR.

  ENDLOOP.
  UNASSIGN <FS1>.

  SELECT KNUMV KPOSN KWERT
  FROM KONV INTO CORRESPONDING FIELDS OF TABLE GT_KONV
  FOR ALL ENTRIES IN ITAB
  WHERE KNUMV EQ ITAB-KNUMV AND
        KSCHL EQ 'PR00'.


  LOOP AT ITAB ASSIGNING <FS1> WHERE MEINS NE 'SET'.
    CLEAR: LV_CHECK_CUST.

    READ TABLE GT_ZSDSSDC006 INTO GW_ZSDSSDC006 WITH KEY MATNR =  <FS1>-MATNR
                                                               AUART =  <FS1>-AUART.
    IF SY-SUBRC EQ 0.
      <FS1>-ISWAR = 'W'.
    ELSE.




      "Add by Wantanee 20150521 ITR2015-3851
      SELECT SINGLE AUART
      INTO LV_IO_AUART
      FROM AUFK
      WHERE AUFNR EQ <FS1>-AUFNR.



      IF LV_IO_AUART EQ 'H004'.
        READ TABLE GT_CUST_WARRANTY INTO WA_CUST_WARRANTY WITH KEY KUNNR = <FS1>-KUNNR
                                                                    VKBUR = <FS1>-VKBUR.
        IF SY-SUBRC EQ 0.
          LV_CHECK_CUST = 'X'.
        ENDIF.

      ENDIF.
      "End Add by Wantanee 20150521 ITR2015-3851


*
      IF LV_CHECK_CUST = 'X'.
        <FS1>-ISWAR = 'W'."Warranty.

      ELSE.
        PH1   = <FS1>-PRODH+0(5).
        PH2   = <FS1>-PRODH+5(5).
        PH3   = <FS1>-PRODH+10(8).

*        CASE <FS1>-VTWEG.
*          WHEN 10.
*            READ TABLE GT_WARRANTY INTO WA_WARRANTY WITH KEY VTWEG = <FS1>-VTWEG
*                                                             VKBUR = <FS1>-VKBUR
*                                                             PH1   = PH1
*                                                             PH2   = PH2
*                                                             PH3   = SPACE.
*            IF SY-SUBRC EQ 0 AND <FS1>-ERSDA GE WA_WARRANTY-WDATE.
*              <FS1>-ISWAR = 'W'."Warranty.
*            ENDIF.
*
**                                    Add by aphai on 7.11.2014
*            IF <FS1>-ISWAR IS INITIAL.
*              READ TABLE GT_KONV INTO WA_KONV WITH KEY KNUMV = <FS1>-KNUMV
**                                                                         kposn = <fs1>-posnv. "comment by aphai 09.11.2015
*                                                        KPOSN = <FS1>-VGPOS. "add by aphai 09.11.2015
*              IF SY-SUBRC EQ 0.
*                CALL FUNCTION 'ZSD_CHK_WARRANTY_SP'
*                  EXPORTING
*                    P_MATNR    = <FS1>-MATNR
*                    P_VTWEG    = <FS1>-VTWEG
*                    P_VKBUR    = <FS1>-VKBUR
*                  IMPORTING
*                    ISWARRANTY = <FS1>-ISWAR
*                  TABLES
*                    ITAB       = DT_CONST[].
*              ENDIF.
*            ENDIF.
**                                    End add aphai on 7.11.2014
*
*          WHEN 20.
        READ TABLE GT_WARRANTY INTO WA_WARRANTY WITH KEY VTWEG = <FS1>-VTWEG
                                                         VKBUR = <FS1>-VKBUR
                                                         PH1   = PH1
                                                         PH2   = PH2
                                                         PH3   = PH3.
        IF SY-SUBRC EQ 0.
          CLEAR PERCENT.
          READ TABLE GT_KONV INTO WA_KONV WITH KEY KNUMV = <FS1>-KNUMV
*                                                                         kposn = <fs1>-posnv.
                                                   KPOSN = <FS1>-VGPOS. "Add by Aphai 09.11.2015
          IF SY-SUBRC EQ 0 AND <FS1>-ERSDA GE WA_WARRANTY-WDATE.
            PERCENT = ( <FS1>-NETWR * 100 ) / WA_KONV-KWERT.
            IF PERCENT GE WA_WARRANTY-LP_PERCENT.
              <FS1>-ISWAR = 'W'.
            ENDIF.
          ENDIF.
        ENDIF.

        IF <FS1>-ISWAR IS INITIAL.
          READ TABLE GT_WARRANTY INTO WA_WARRANTY WITH KEY VTWEG = <FS1>-VTWEG
                                                           VKBUR = <FS1>-VKBUR
                                                           PH1   = PH1
                                                           PH2   = PH2
                                                           PH3   = SPACE.
          IF SY-SUBRC EQ 0.
            CLEAR PERCENT.
            READ TABLE GT_KONV INTO WA_KONV WITH KEY KNUMV = <FS1>-KNUMV
*                                                                         kposn = <fs1>-posnv.
                                                   KPOSN = <FS1>-VGPOS. "Add by Aphai 09.11.2015
            IF SY-SUBRC EQ 0 AND <FS1>-ERSDA GE WA_WARRANTY-WDATE.
              PERCENT = ( <FS1>-NETWR * 100 ) / WA_KONV-KWERT.
              IF PERCENT GE WA_WARRANTY-LP_PERCENT.
                <FS1>-ISWAR = 'W'.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

*                              IF <fs1>-iswar IS INITIAL.
*                      *              Add by aphai on 3.11.2014
*                                IF dt_const[] IS NOT INITIAL.
*                                  READ TABLE dt_const WITH KEY value = <fs1>-matnr BINARY SEARCH.
*                                  IF sy-subrc EQ 0.
*                                    <fs1>-iswar = 'W'.
*                                  ENDIF.
*                                ENDIF.
*                      *              End add aphai on 3.11.2014
*                              ENDIF.

*                                    Add by aphai on 7.11.2014
        IF <FS1>-ISWAR IS INITIAL.
          READ TABLE GT_KONV INTO WA_KONV WITH KEY KNUMV = <FS1>-KNUMV
*                                                                         kposn = <fs1>-posnv.
                                                   KPOSN = <FS1>-VGPOS. "Add by Aphai 09.11.2015
          IF SY-SUBRC EQ 0.
            CALL FUNCTION 'ZSD_CHK_WARRANTY_SP'
              EXPORTING
                P_MATNR    = <FS1>-MATNR
                P_NETWR    = <FS1>-NETWR
                P_KWERT    = WA_KONV-KWERT
                P_VTWEG    = <FS1>-VTWEG
                P_VKBUR    = <FS1>-VKBUR
              IMPORTING
                ISWARRANTY = <FS1>-ISWAR
              TABLES
                ITAB       = DT_CONST[].
          ENDIF.
        ENDIF.
*                                    End add aphai on 7.11.2014

*        ENDCASE.
        CLEAR : PH1,PH2,PH3.
      ENDIF.

    ENDIF.

  ENDLOOP.

  IF CBO_WAR EQ 'X'.
    DELETE ITAB WHERE ISWAR EQ SPACE.
  ENDIF.

ENDFORM.                    "split_warranty_only

*&---------------------------------------------------------------------*
*&      Form  gd_vbak
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VBELN    text
*      -->R_VTWEG    text
*      -->R_VKBUR    text
*----------------------------------------------------------------------*
FORM GD_VBAK USING
               P_VBELN TYPE VBAK-VBELN
              CHANGING
               R_VTWEG TYPE VBAK-VTWEG
               R_VKBUR TYPE VBAK-VKBUR
               R_KNUMV TYPE VBAK-KNUMV
               R_AUART TYPE VBAK-AUART.

  CHECK P_VBELN IS NOT INITIAL.

  SELECT SINGLE VTWEG VKBUR KNUMV AUART
    INTO (R_VTWEG,R_VKBUR,R_KNUMV,R_AUART)
    FROM VBAK
    WHERE VBELN EQ P_VBELN.

ENDFORM.                    "gd_vbak

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
*&      Form  gd_vbap
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VBELN    text
*      -->P_POSNR    text
*      -->R_NETWR    text
*----------------------------------------------------------------------*
FORM GD_VBAP USING
               P_VBELN TYPE VBAP-VBELN
               P_POSNR TYPE VBAP-POSNR
             CHANGING
               R_NETWR TYPE VBAP-NETWR.

  CHECK : P_VBELN IS NOT INITIAL,
          P_POSNR IS NOT INITIAL.

  SELECT SINGLE NETWR INTO (R_NETWR) FROM VBAP
    WHERE VBELN EQ P_VBELN AND POSNR EQ P_POSNR.

ENDFORM.                    "gd_vbap

* End Add by Aphai On 30.10.2014

*FORM update_warrantylog USING it_warranty TYPE ZSDSSDS059_TT
*                     CHANGING cv_retcode  TYPE sy-subrc.
*
*  DATA: ls_warranty    TYPE ZSDSSDS059,
*        ls_warrantylog TYPE ZSDSSDT019,
*        lv_print_sequ  TYPE ZSDSSDT019-print_sequ.
*
*  READ TABLE it_warranty INTO ls_warranty INDEX 1.
*  CHECK sy-subrc = 0 AND ls_warranty-deliv_numb IS NOT INITIAL.
*
*  SELECT SINGLE MAX( DISTINCT print_sequ )
*    FROM ZSDSSDT019 INTO lv_print_sequ
*    WHERE deliv_numb = ls_warranty-deliv_numb.
*
*  lv_print_sequ = lv_print_sequ + 1.
*
*  CLEAR ls_warranty.
*  LOOP AT it_warranty INTO ls_warranty.
*
*    CALL FUNCTION 'ENQUEUE_EZ_WARRANTYLOG'
*      EXPORTING
*        mode_ZSDSSDT019 = 'E'
*        client                = sy-mandt
*        deliv_numb            = ls_warranty-deliv_numb
*        itm_number            = ls_warranty-itm_number
*        serial_num            = ls_warranty-serial_num
*        print_sequ            = lv_print_sequ.
**       X_DELIV_NUMB          = ' '
**       _SCOPE                = '2'
**       _WAIT                 = ' '
**       _COLLECT              = ' '
**     EXCEPTIONS
**       FOREIGN_LOCK          = 1
**       SYSTEM_FAILURE        = 2
**       OTHERS                = 3.
*    IF sy-subrc = 0.
*      ls_warrantylog-deliv_numb = ls_warranty-deliv_numb.
*      ls_warrantylog-itm_number = ls_warranty-itm_number.
*      ls_warrantylog-serial_num = ls_warranty-serial_num.
*      ls_warrantylog-print_sequ = lv_print_sequ.
*      ls_warrantylog-deliv_date = ls_warranty-deliv_date.
*      ls_warrantylog-material   = ls_warranty-material.
*      ls_warrantylog-print_date = sy-datum.
*      ls_warrantylog-print_time = sy-uzeit.
*
*      INSERT ZSDSSDT019 FROM ls_warrantylog.
*    ELSE.
*      cv_retcode = 4.
*    ENDIF.
*
*    CALL FUNCTION 'DEQUEUE_EZ_WARRANTYLOG'
*      EXPORTING
*        mode_ZSDSSDT019 = 'E'
*        client                = sy-mandt
*        deliv_numb            = ls_warranty-deliv_numb
*        itm_number            = ls_warranty-itm_number
*        serial_num            = ls_warranty-serial_num
*        print_sequ            = lv_print_sequ.
**       X_DELIV_NUMB          = ' '
**       _SCOPE                = '3'
**       _SYNCHRON             = ' '
**       _COLLECT              = ' '
*
*    CLEAR ls_warranty.
*  ENDLOOP.
*
*ENDFORM.                    " UPDATE_WARRANTYLOG
*FORM update_warrantylog USING iv_deliv_numb TYPE vbeln_vl
*                              iv_deliv_date TYPE lfdat
*                     CHANGING cv_retcode    TYPE sy-subrc.
*
*  DATA: ls_warrantylog TYPE ZSDSSDT019,
*        lv_print_sequ  TYPE ZSDSSDT019-print_sequ.
*
*
*  SELECT SINGLE MAX( DISTINCT print_sequ )
*    FROM ZSDSSDT019 INTO lv_print_sequ
*    WHERE deliv_numb = iv_deliv_numb.
*
*  lv_print_sequ = lv_print_sequ + 1.
*
*  CALL FUNCTION 'ENQUEUE_EZ_WARRANTYLOG'
*    EXPORTING
*      mode_ZSDSSDT019 = 'E'
*      client                = sy-mandt
*      deliv_numb            = iv_deliv_numb
*      print_sequ            = lv_print_sequ.
**     X_DELIV_NUMB          = ' '
**     _SCOPE                = '2'
**     _WAIT                 = ' '
**     _COLLECT              = ' '
**   EXCEPTIONS
**     FOREIGN_LOCK          = 1
**     SYSTEM_FAILURE        = 2
**     OTHERS                = 3.
*  IF sy-subrc = 0.
*    ls_warrantylog-deliv_numb = iv_deliv_numb.
*    ls_warrantylog-print_sequ = lv_print_sequ.
*    ls_warrantylog-deliv_date = iv_deliv_date.
**    ls_warrantylog-print_numb = 1.
*    ls_warrantylog-print_date = sy-datum.
*    ls_warrantylog-print_time = sy-uzeit.
*
*    INSERT ZSDSSDT019 FROM ls_warrantylog.
*  ENDIF.
*
*  CALL FUNCTION 'DEQUEUE_EZ_WARRANTYLOG'
*    EXPORTING
*      mode_ZSDSSDT019 = 'E'
*      client                = sy-mandt
*      deliv_numb            = iv_deliv_numb
*      print_sequ            = lv_print_sequ.
**     X_DELIV_NUMB          = ' '
**     _SCOPE                = '3'
**     _SYNCHRON             = ' '
**     _COLLECT              = ' '
*
*ENDFORM.                    " UPDATE_WARRANTYLOG

*    WHEN '&IC1'.
*
*      CHECK rs_selfield-tabindex > 0.
*
*      CALL FUNCTION 'POPUP_TO_CONFIRM'
*        EXPORTING
*          titlebar              = 'Print Warranty Card'
*          text_question         = 'Do you want to print the selected line?'
*          text_button_1         = 'Yes'
*          text_button_2         = 'No'
*          default_button        = '1'
*          display_cancel_button = ''
*        IMPORTING
*          answer                = lv_answer.
*
*      IF lv_answer = '1'.
*
**        IF rs_selfield-fieldname = 'VBELN'.
*        READ TABLE gt_output INTO ls_output INDEX rs_selfield-tabindex.
*
*        SELECT * FROM nast INTO TABLE lt_nast
*          WHERE objky = ls_output-vbeln
*          AND ( kschl = 'ZD01' OR
*                kschl = 'ZTD1' )
*          AND   spras = sy-langu.
*        SORT lt_nast BY erdat DESCENDING
*                        eruhr DESCENDING.
*        READ TABLE lt_nast INTO gs_nast INDEX 1.
*
**        SELECT single * FROM vlkpa INTO ls_vlkpa
**          WHERE vbeln = ls_output-vbeln.
*
*        PERFORM set_print_data_to_read IN PROGRAM z_sd_rle_delnote IF FOUND
*                                       USING    gc_delivery_formname
*                                       CHANGING ls_print_data_to_read
*                                                lv_retcode.
*        IF lv_retcode = 0.
**          PERFORM get_data IN PROGRAM z_sd_rle_delnote IF FOUND
*          PERFORM get_data_delivery
*                           USING    ls_print_data_to_read
*                           CHANGING ls_addr_key
*                                    ls_dlv_delnote
*                                    lv_retcode.
*        ENDIF.
*        IF lv_retcode = 0.
**          PERFORM set_print_param IN PROGRAM z_sd_rle_delnote IF FOUND
*          PERFORM set_print_param_delivery
*                                  USING    ls_addr_key
*                                  CHANGING ls_control_param
*                                           ls_composer_param
*                                           ls_recipient
*                                           ls_sender
*                                           lv_retcode.
*        ENDIF.
*
*        IF lv_retcode = 0.
*          DELETE ls_dlv_delnote-it_gen WHERE itm_number NE ls_output-posnr.
*
*          IF ls_dlv_delnote-it_gen[] IS NOT INITIAL.
*
**   determine smartform function module for warranty card
*            CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*                 EXPORTING  formname           = gc_warranty_formname
**                           variant            = ' '
**                           direct_call        = ' '
*                 IMPORTING  fm_name            = lf_fm_name
*                 EXCEPTIONS no_form            = 1
*                            no_function_module = 2
*                            OTHERS             = 3.
*
*            IF sy-subrc = 0.
*
*              ls_composer_param-tddest     = 'EPSO'.
**             LS_CONTROL_PARAM-DEVICE      = 'PRINTER'.
*              ls_control_param-no_dialog   = 'X'.
*
**   call smartform warranty card
*              CALL FUNCTION lf_fm_name
*                   EXPORTING
**                           ARCHIVE_INDEX        = TOA_DARA
**                           ARCHIVE_PARAMETERS   = ARC_PARAMS
*                            control_parameters   = ls_control_param
**                           mail_appl_obj        =
**                           MAIL_RECIPIENT       = IS_RECIPIENT
**                           MAIL_SENDER          = IS_SENDER
*                            output_options       = ls_composer_param
*                            user_settings        = ' '
*                            is_dlv_delnote       = ls_dlv_delnote
**                           IS_NAST              = NAST
**                importing  document_output_info =
**                           job_output_info      =
**                           job_output_options   =
*                 EXCEPTIONS formatting_error     = 1
*                            internal_error       = 2
*                            send_error           = 3
*                            user_canceled        = 4
*                            OTHERS               = 5.
*
*            ENDIF.
*          ENDIF.
*        ENDIF.
**        ENDIF.
*      ENDIF.
*&---------------------------------------------------------------------*
*& Form f_delete_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_DELETE_DATA .
  DATA : LT_TMP LIKE GT_OUTPUT[],
         LS_TMP LIKE LINE OF LT_TMP.

  DATA : LT_WARRANTY TYPE ZSDSSDS113_TT,
         LS_WARRANTY TYPE ZSDSSDS113.

  DATA : LCL_FORM TYPE REF TO ZCL_SDSSD_GEN_DATA_SD_FORM.

  DATA : LT_VBELN TYPE TT_VBELN,
         LS_VBELN TYPE VBELN.

  LT_TMP = GT_OUTPUT[].

  IF LCL_FORM IS NOT BOUND.
    CREATE OBJECT LCL_FORM.
  ENDIF.

  SORT LT_TMP BY DELIV_NUMB.
  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING DELIV_NUMB.

  LOOP AT LT_TMP INTO LS_TMP.
    LS_VBELN = LS_TMP-DELIV_NUMB.
    APPEND LS_VBELN TO LT_VBELN.
  ENDLOOP.

  LT_WARRANTY = LCL_FORM->GET_LINE_ITEM_WARRANTY( CHANGING CT_DATA = LT_VBELN ).

  CLEAR : LT_TMP.

  LOOP AT GT_OUTPUT INTO DATA(LS_OUTPUT).
    READ TABLE LT_WARRANTY INTO LS_WARRANTY
    WITH KEY VBELN = LS_OUTPUT-DELIV_NUMB
             POSNR = LS_OUTPUT-ITM_NUMBER.
    IF SY-SUBRC EQ 0.
      APPEND LS_OUTPUT TO LT_TMP.
    ENDIF.
  ENDLOOP.

  GT_OUTPUT = LT_TMP.

  SORT GT_OUTPUT BY DELIV_DATE
                    DELIV_NUMB
                    ITM_NUMBER
                    SERIAL_NUM.
ENDFORM.
