*&---------------------------------------------------------------------*
*& Report ZSDSMMR0190
*  Creation Date      : 016.04.2024
*  Author             : Wantanee Prateep na thalang
*  Add-on ID          : ZSDI003
*  Description        : Report and Export interface delivery and return delivery of Spare part to WMS(SP)
*  Purpose            :
*  Copied from        : ZR_MM_DO_REDO_TO_WMS
*  Restriction        :
*&---------------------------------------------------------------------*
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSMMR0190.
*---------DATA DEFINATION-------------------------*
*&-----------------------------------------------------------------------------------*
*& T A B L E S
*&-----------------------------------------------------------------------------------*
TABLES: LIKP,LIPS,VBAK,MSEG.


*&-----------------------------------------------------------------------------------*
*& D A T A
*&-----------------------------------------------------------------------------------*

TYPES: BEGIN OF GY_ITAB,
         VBELN TYPE LIKP-VBELN,  "DO Document
         POSNR TYPE LIPS-POSNR,   " ITEM
         WERKS TYPE LIPS-WERKS, "Plant
         LGORT TYPE LIPS-LGORT,   "Storage Location
         ERDAT TYPE LIKP-ERDAT,   "Posting Date
         MATNR TYPE LIPS-MATNR,  "Material
         ARKTX TYPE LIPS-ARKTX, "Material Description
         LFIMG TYPE I,  "Qty
         MEINS TYPE LIPS-MEINS,  "Unit of Entry
         ERNAM TYPE LIKP-ERNAM,  "User name
         ERZET TYPE LIKP-ERZET,   "Time of Entry
         VGBEL TYPE LIPS-VGBEL,  "SO reference
       END OF GY_ITAB.

TYPES: BEGIN OF GY_LIKP,
         VBELN TYPE LIKP-VBELN,  "DO Document
         ERDAT TYPE LIKP-ERDAT,   "Posting Date
         ERZET TYPE LIKP-ERZET,   "Time of Entry
         ERNAM TYPE LIKP-ERNAM,  "User name
         LFART TYPE LIKP-LFART,  "Document Type
         POSNR TYPE LIPS-POSNR,  "Item
         WERKS TYPE LIPS-WERKS, "Plant
         LGORT TYPE LIPS-LGORT,   "Storage Location
         MATNR TYPE LIPS-MATNR,  "Material
         ARKTX TYPE LIPS-ARKTX, "Material Description
         LFIMG TYPE LIPS-LFIMG,  "Qty
         MEINS TYPE LIPS-MEINS,  "Unit of Entry
         VGBEL TYPE LIPS-VGBEL, " SO Doc.
         VGPOS TYPE LIPS-VGPOS, " SO Doc.

       END OF GY_LIKP.

TYPES: BEGIN OF GY_ADRC,
         ADDRNUMBER TYPE ADRC-ADDRNUMBER,      "Address Number
         NAME1      TYPE ADRC-NAME1,      "Name
         NAME2      TYPE ADRC-NAME2,      "Name2
         STREET     TYPE ADRC-STREET,     "Street
         STR_SUPPL3 TYPE ADRC-STR_SUPPL3, "
         LOCATION   TYPE ADRC-LOCATION,
         CITY2      TYPE ADRC-CITY2,      "District
         CITY1      TYPE ADRC-CITY1,      "City
         POST_CODE1 TYPE ADRC-POST_CODE1, "post code
         TEL_NUMBER TYPE ADRC-TEL_NUMBER, "telephone
         FAX_NUMBER TYPE ADRC-FAX_NUMBER, "fax_number
         NATION     TYPE ADRC-NATION,      "Name2
       END OF GY_ADRC.
TYPES: BEGIN OF GY_OUT_DELI_TEXT,
         VBELN(10) TYPE C,  "DO Document
         POSNR(4)  TYPE C,  "Item
         WERKS(4)  TYPE C, "Plant
         VGBEL(10) TYPE C,  "SO reference
         LGORT(4)  TYPE C,   "Storage Location
         ERZET(6)  TYPE C,   "Time of Entry
         ERDAT(8)  TYPE C,   "Posting Date
         MATNR(35) TYPE C,  "Material
         ARKTX(40) TYPE C, "Material Description
         LFIMG(13) TYPE C,  "Qty
         MEINS(3)  TYPE C,  "Unit of Entry
         ERNAM(12) TYPE C,  "User name
       END OF GY_OUT_DELI_TEXT.
TYPES: BEGIN OF GY_MKPF,
         MBLNR    TYPE MKPF-MBLNR,
         LE_VBELN TYPE MKPF-LE_VBELN,
       END OF GY_MKPF.
TYPES: BEGIN OF GY_VBELN,
         VBELN TYPE LIKP-VBELN,
       END OF GY_VBELN.
TYPES: BEGIN OF GY_CHECK_INV,
         VBELV TYPE VBFA-VBELV,   "sale order no
         POSNV TYPE VBFA-POSNV,     "item
         VBELN TYPE VBFA-VBELN,   "invoice or cn
       END OF GY_CHECK_INV.

TYPES: BEGIN OF GY_MKPF_MSEG,
       MBLNR    TYPE MKPF-MBLNR,
       CPUDT    TYPE MKPF-CPUDT,
       CPUTM    TYPE MKPF-CPUTM,
       USNAM    TYPE MKPF-USNAM,
       MJAHR    TYPE MSEG-MJAHR,
       ZEILE    TYPE MSEG-ZEILE,
       WERKS    TYPE MSEG-WERKS,
       LGORT    TYPE MSEG-LGORT,
       SHKZG    TYPE MSEG-SHKZG,
       MATNR    TYPE MSEG-MATNR,
       MENGE    TYPE MSEG-MENGE,
       MEINS    TYPE MSEG-MEINS,
       MAKTX    TYPE MAKT-MAKTX,  "material description
END OF GY_MKPF_MSEG.




*&-----------------------------------------------------------------------------------*
*& I N T E R N A L   T A B L E S
*&-----------------------------------------------------------------------------------*
* Data for ALV display

TYPE-POOLS: SLIS.
TYPE-POOLS: TRUXS.
* The inputs that need to be passed to the REUSE_ALV function module
DATA: GT_LIST_FIELDCAT TYPE LVC_T_FCAT,      "Field Catalog for List Viewer Control
      GT_EXIT(1)       TYPE C,
      GT_VARIANT       TYPE DISVARIANT,
      GX_VARIANT       TYPE DISVARIANT,
      GS_PRINT         TYPE LVC_S_PRNT.


DATA: GT_ITAB         TYPE STANDARD TABLE OF GY_ITAB,
      GT_LIKP         TYPE STANDARD TABLE OF GY_LIKP,
      GT_ADRC         TYPE STANDARD TABLE OF GY_ADRC,
      GT_DELI_WMS_TXT TYPE STANDARD TABLE OF GY_OUT_DELI_TEXT,
      GT_MKPF         TYPE STANDARD TABLE OF GY_MKPF,
      GT_VBELN        TYPE STANDARD TABLE OF GY_VBELN,
      GT_CHECK_INV_NS TYPE STANDARD TABLE OF GY_CHECK_INV,
      GT_MKPF_MSEG    TYPE STANDARD TABLE OF GY_MKPF_MSEG.

DATA: GS_DELI_WMS_TXT TYPE GY_OUT_DELI_TEXT,
      GS_CHECK_INV_NS TYPE GY_CHECK_INV.

DATA: GS_ITAB          TYPE GY_ITAB,
      GS_LIKP          TYPE GY_LIKP,
      GS_ADRC          TYPE GY_ADRC,
      GS_DELI_WMS_TXT1 TYPE GY_OUT_DELI_TEXT,
      GS_MKPF          TYPE GY_MKPF,
      GS_VBELN         TYPE GY_VBELN,
      GS_MKPF_MSEG     TYPE GY_MKPF_MSEG.


DATA: GS_GRID_MAIN      TYPE REF TO CL_GUI_ALV_GRID,
      GS_CONTAINER_MAIN TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_VARIANT        LIKE DISVARIANT.

DATA: GT_EXPORT_TXT TYPE TRUXS_T_TEXT_DATA.

DATA: GS_EXPORT_TXT   LIKE LINE OF GT_EXPORT_TXT.

DATA: GS_PATH          TYPE STRING.



DATA: GS_PATH_NAME TYPE STRING.
DATA: GS_TEXT_EXPORT(506)    TYPE C.
DATA: GT_LINES TYPE STANDARD TABLE OF TLINE.
DATA: GS_LINES LIKE LINE OF GT_LINES.
DATA: GS_NAME TYPE THEAD-TDNAME.
DATA: CT_RESULT TYPE TABLE OF STRING.
DATA: CS_STRING TYPE STRING.
DATA: GS_SPACE1 TYPE STRING.

RANGES: R_BLART         FOR BSID-BLART.  "order type

*&-----------------------------------------------------------------------------------*
*& V A R I A B L E
*&-----------------------------------------------------------------------------------*
DATA : V_POS TYPE I .

DATA : GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
       GT_LAYOUT   TYPE SLIS_LAYOUT_ALV,
       GT_EVENTS   TYPE SLIS_T_EVENT.
*&--------------------------------------------*
*& C O N S T A N T
*&-----------------------------------------------------------------------------------*
CONSTANTS : GC_REPID       TYPE REPID         VALUE 'ZSDSMMR0190',
            GC_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE'.
CONSTANTS : LC_SPACE1   TYPE SYHEX02 VALUE '0x20'.
*&-----------------------------------------------------------------------------------*
*& S E L E C T I O N   S C R E E N
*&-----------------------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-H01.

  PARAMETERS:       P_BUKRS       TYPE BUKRS   OBLIGATORY DEFAULT '1000'.

  SELECT-OPTIONS: S_LFART         FOR VBAK-AUART,
                  S_ERDAT         FOR LIKP-ERDAT,
                  S_VBELN         FOR LIKP-VBELN,
                  S_WERKS         FOR LIPS-WERKS,
                  S_LGORT         FOR LIPS-LGORT,
                  S_BWART         FOR MSEG-BWART.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-H02.
  PARAMETERS: P_DISP  RADIOBUTTON GROUP GR1,
              P_LOCAL RADIOBUTTON GROUP GR1,
              P_IDOC  RADIOBUTTON GROUP GR1 DEFAULT 'X',
              P_PATH  TYPE STRING  LOWER CASE.
*                p_path like rlgrap-filename.
SELECTION-SCREEN END OF BLOCK B2.

*END SELECTION SCREEN
*&-----------------------------------------------------------------------------------*
* Event:Initialization
INITIALIZATION.
  P_PATH = '/usr/sap/tmp/'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_PATH.
*  PERFORM get_path_name CHANGING p_path.
  PERFORM PF_DIRECTORY_BROWSE USING P_PATH.
*&-----------------------------------------------------------------------------------*
*& * START-OF-SELECTION
*&-----------------------------------------------------------------------------------*
START-OF-SELECTION .



  PERFORM F_GET_DATA.
  PERFORM F_MAP_DATA.


*&-----------------------------------------------------------------------------------*
*& * END-OF-SELECTION
*&-----------------------------------------------------------------------------------*
END-OF-SELECTION.

  IF ( NOT GT_ITAB[] IS INITIAL ) .
    IF NOT P_DISP IS INITIAL.
      PERFORM DISPLAY_REPROT.
    ELSEIF NOT P_LOCAL IS INITIAL.



      CLEAR: GS_PATH_NAME.

*             CONCATENATE 'DI_'  sy-datum sy-timlo '.txt' INTO gs_path_name.
      GS_PATH_NAME = 'SDS_DAILY_DO_STK'.
      PERFORM F_TEXT_EXPORT_LOCAL.
      CONCATENATE P_PATH GS_PATH_NAME INTO GS_PATH_NAME SEPARATED BY '\'.
      PERFORM EXPORT USING GS_PATH_NAME
                           GT_EXPORT_TXT.



    ELSE.
      PERFORM F_TEXT_EXPORT_SERVER.

    ENDIF.
  ELSE.
    IF P_LOCAL IS INITIAL.
      PERFORM F_TEXT_EXPORT_SERVER.
    ENDIF.
*    MESSAGE i004.
    EXIT.
  ENDIF.
*--------------------------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*

FORM F_GET_DATA .

  IF S_ERDAT IS INITIAL.
    S_ERDAT-SIGN   = 'I'.
    S_ERDAT-OPTION = 'BT'.
    S_ERDAT-LOW    = '20160101'.
    S_ERDAT-HIGH    = SY-DATUM.
    APPEND S_ERDAT.
  ENDIF.




  SELECT A~VBELN  A~ERDAT A~ERZET A~ERNAM A~LFART
         B~POSNR  B~WERKS B~LGORT B~MATNR
         B~ARKTX  B~LFIMG B~MEINS B~VGBEL B~VGPOS

  INTO TABLE GT_LIKP
  FROM LIKP AS A INNER JOIN LIPS AS B
             ON ( A~VBELN EQ B~VBELN )
*                          INNER JOIN vbuk AS d
*                      ON  ( a~vbeln EQ d~vbeln )
                 INNER JOIN VBAK AS F
             ON  ( B~VGBEL EQ F~VBELN )
  WHERE A~VBELN IN S_VBELN
    AND A~ERDAT IN S_ERDAT
    AND B~LGORT IN S_LGORT
    AND B~WERKS IN S_WERKS
    AND A~VSTEL NE ''
*             AND d~fkstk <> 'C'
  AND F~AUART IN S_LFART.

  IF NOT GT_LIKP IS INITIAL.
    SELECT VBELV POSNV VBELN
     INTO TABLE GT_CHECK_INV_NS
     FROM VBFA
     FOR ALL ENTRIES IN GT_LIKP
     WHERE VBELV EQ GT_LIKP-VGBEL
       AND POSNV EQ GT_LIKP-VGPOS
       AND ( VBTYP_N EQ 'M'
       OR  VBTYP_N EQ 'N'
       OR  VBTYP_N EQ 'O'
    OR  VBTYP_N EQ 'P' ).
  ENDIF.


  LOOP AT GT_LIKP INTO GS_LIKP.
    READ TABLE GT_CHECK_INV_NS INTO GS_CHECK_INV_NS WITH KEY VBELV = GS_LIKP-VGBEL
                                                            POSNV = GS_LIKP-VGPOS.
    IF SY-SUBRC NE 0.
      GS_VBELN-VBELN = GS_LIKP-VBELN.
      APPEND GS_VBELN TO GT_VBELN.
    ENDIF.
  ENDLOOP.


  SORT GT_VBELN.
  DELETE ADJACENT DUPLICATES FROM GT_VBELN.

  IF NOT GT_VBELN IS INITIAL.
    SELECT MBLNR LE_VBELN
    INTO TABLE GT_MKPF
    FROM MKPF
    FOR ALL ENTRIES IN GT_VBELN
    WHERE LE_VBELN = GT_VBELN-VBELN
    AND CPUDT IN S_ERDAT.

  ENDIF.

  SELECT B~MBLNR, B~CPUDT, B~CPUTM, B~USNAM,
         A~MJAHR, A~ZEILE, A~WERKS, A~LGORT,
         A~SHKZG, A~MATNR, A~MENGE, A~MEINS,
         D~MAKTX
  INTO TABLE @GT_MKPF_MSEG
  FROM MSEG AS A INNER JOIN MKPF AS B
                 ON ( A~MBLNR = B~MBLNR
                 AND  A~MJAHR = B~MJAHR )
                 INNER JOIN MAKT AS D
                 ON ( A~MATNR = D~MATNR )
  WHERE B~CPUDT IN @S_ERDAT
  AND A~LGORT   IN @S_LGORT
  AND A~BWART   IN @S_BWART.








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

*  LOOP AT GT_LIKP INTO GS_LIKP.
*    CLEAR: GS_ITAB.
*    READ TABLE GT_MKPF INTO GS_MKPF WITH KEY LE_VBELN = GS_LIKP-VBELN.
*    IF SY-SUBRC NE 0.
*
*      READ TABLE GT_CHECK_INV_NS INTO GS_CHECK_INV_NS WITH KEY VBELV = GS_LIKP-VGBEL
*                                                                 POSNV = GS_LIKP-VGPOS.
*      IF SY-SUBRC NE 0.
*
*
*
*        GS_ITAB-VBELN = GS_LIKP-VBELN.
*        GS_ITAB-ERDAT = GS_LIKP-ERDAT.
*        GS_ITAB-ERZET = GS_LIKP-ERZET.
*        GS_ITAB-ERNAM = GS_LIKP-ERNAM.
*        GS_ITAB-POSNR = GS_LIKP-POSNR.
*        GS_ITAB-WERKS = GS_LIKP-WERKS.
*        GS_ITAB-LGORT = GS_LIKP-LGORT.
*        GS_ITAB-MATNR = GS_LIKP-MATNR.
*        GS_ITAB-ARKTX = GS_LIKP-ARKTX.
*
*        IF GS_LIKP-LFART NE 'ZDR2'.
*          GS_ITAB-LFIMG = GS_LIKP-LFIMG .
*        ELSE.
*          GS_ITAB-LFIMG = GS_LIKP-LFIMG * -1.
*        ENDIF.
*
*        GS_ITAB-MEINS = GS_LIKP-MEINS.
*        GS_ITAB-VGBEL = GS_LIKP-VGBEL.
*
*
*
*
*
*
*        APPEND GS_ITAB TO GT_ITAB.
*
*        GS_DELI_WMS_TXT-VBELN = GS_ITAB-VBELN.
*        GS_DELI_WMS_TXT-POSNR = GS_ITAB-POSNR.
*        GS_DELI_WMS_TXT-WERKS = GS_ITAB-WERKS.
*        GS_DELI_WMS_TXT-LGORT = GS_ITAB-LGORT.
*        GS_DELI_WMS_TXT-ERDAT = GS_ITAB-ERDAT.
*        GS_DELI_WMS_TXT-MATNR = GS_ITAB-MATNR.
*        GS_DELI_WMS_TXT-ARKTX = GS_ITAB-ARKTX.
*
*        GS_DELI_WMS_TXT-LFIMG = GS_ITAB-LFIMG.
*        CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
*          CHANGING
*            VALUE = GS_DELI_WMS_TXT-LFIMG.
*
*        GS_DELI_WMS_TXT-MEINS = GS_ITAB-MEINS.
*        GS_DELI_WMS_TXT-ERNAM = GS_ITAB-ERNAM.
*        GS_DELI_WMS_TXT-ERZET = GS_ITAB-ERZET.
*        GS_DELI_WMS_TXT-VGBEL = GS_ITAB-VGBEL.
*
*
*        APPEND GS_DELI_WMS_TXT TO GT_DELI_WMS_TXT.
*      ENDIF.
*    ENDIF.
*
*
*
*
*
*
*  ENDLOOP.


  LOOP AT GT_LIKP INTO GS_LIKP.
    CLEAR: GS_ITAB.
*    READ TABLE GT_MKPF INTO GS_MKPF WITH KEY LE_VBELN = GS_LIKP-VBELN.
*    IF SY-SUBRC NE 0.
*
*      READ TABLE GT_CHECK_INV_NS INTO GS_CHECK_INV_NS WITH KEY VBELV = GS_LIKP-VGBEL
*                                                                 POSNV = GS_LIKP-VGPOS.
*      IF SY-SUBRC NE 0.



        GS_ITAB-VBELN = GS_LIKP-VBELN.
        GS_ITAB-ERDAT = GS_LIKP-ERDAT.
        GS_ITAB-ERZET = GS_LIKP-ERZET.
        GS_ITAB-ERNAM = GS_LIKP-ERNAM.
        GS_ITAB-POSNR = GS_LIKP-POSNR.
        GS_ITAB-WERKS = GS_LIKP-WERKS.
        GS_ITAB-LGORT = GS_LIKP-LGORT.
        GS_ITAB-MATNR = GS_LIKP-MATNR.
        GS_ITAB-ARKTX = GS_LIKP-ARKTX.

        IF GS_LIKP-LFART NE 'ZDR2'.
          GS_ITAB-LFIMG = GS_LIKP-LFIMG .
        ELSE.
          GS_ITAB-LFIMG = GS_LIKP-LFIMG * -1.
        ENDIF.

        GS_ITAB-MEINS = GS_LIKP-MEINS.
        GS_ITAB-VGBEL = GS_LIKP-VGBEL.






        APPEND GS_ITAB TO GT_ITAB.

        GS_DELI_WMS_TXT-VBELN = GS_ITAB-VBELN.
        GS_DELI_WMS_TXT-POSNR = GS_ITAB-POSNR.
        GS_DELI_WMS_TXT-WERKS = GS_ITAB-WERKS.
        GS_DELI_WMS_TXT-LGORT = GS_ITAB-LGORT.
        GS_DELI_WMS_TXT-ERDAT = GS_ITAB-ERDAT.
        GS_DELI_WMS_TXT-MATNR = GS_ITAB-MATNR.
        GS_DELI_WMS_TXT-ARKTX = GS_ITAB-ARKTX.

        GS_DELI_WMS_TXT-LFIMG = GS_ITAB-LFIMG.
        CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
          CHANGING
            VALUE = GS_DELI_WMS_TXT-LFIMG.

        GS_DELI_WMS_TXT-MEINS = GS_ITAB-MEINS.
        GS_DELI_WMS_TXT-ERNAM = GS_ITAB-ERNAM.
        GS_DELI_WMS_TXT-ERZET = GS_ITAB-ERZET.
        GS_DELI_WMS_TXT-VGBEL = GS_ITAB-VGBEL.


        APPEND GS_DELI_WMS_TXT TO GT_DELI_WMS_TXT.
*      ENDIF.
*    ENDIF.






  ENDLOOP.
*
*  MBLNR    TYPE MKPF-MBLNR,
*       CPUDT    TYPE MKPF-CPUDT,
*       CPUTM    TYPE MKPF-CPUTM,
*       USNAM    TYPE MKPF-USNAM,
*       MJAHR    TYPE MSEG-MJAHR,
*       ZEILE    TYPE MSEG-ZEILE,
*       WERKS    TYPE MSEG-WERKS,
*       LGORT    TYPE MSEG-LGORT,
*       SHKZG    TYPE MSEG-SHKZG,
*       MATNR    TYPE MSEG-MATNR,
*       MENGE    TYPE MSEG-MENGE,
*       MEINS    TYPE MSEG-MEINS,
*       MAKTX    TYPE MAKT-MAKTX,  "material description


  LOOP AT GT_MKPF_MSEG INTO GS_MKPF_MSEG.
        CLEAR: GS_ITAB.
        GS_ITAB-VBELN = GS_MKPF_MSEG-MBLNR.
        GS_ITAB-ERDAT = GS_MKPF_MSEG-CPUDT.
        GS_ITAB-ERZET = GS_MKPF_MSEG-CPUTM.
        GS_ITAB-ERNAM = GS_MKPF_MSEG-USNAM.
        GS_ITAB-POSNR = GS_MKPF_MSEG-ZEILE.
        GS_ITAB-WERKS = GS_MKPF_MSEG-WERKS.
        GS_ITAB-LGORT = GS_MKPF_MSEG-LGORT.
        GS_ITAB-MATNR = GS_MKPF_MSEG-MATNR.
        GS_ITAB-ARKTX = GS_MKPF_MSEG-MAKTX.

        IF GS_MKPF_MSEG-SHKZG EQ 'H'.
          GS_ITAB-LFIMG = GS_MKPF_MSEG-MENGE .
        ELSE.
          GS_ITAB-LFIMG = GS_MKPF_MSEG-MENGE * -1.
        ENDIF.

        GS_ITAB-MEINS = GS_MKPF_MSEG-MEINS.
*        GS_ITAB-VGBEL = GS_LIKP-VGBEL.

        APPEND GS_ITAB TO GT_ITAB.

        GS_DELI_WMS_TXT-VBELN = GS_ITAB-VBELN.
        GS_DELI_WMS_TXT-POSNR = GS_ITAB-POSNR.
        GS_DELI_WMS_TXT-WERKS = GS_ITAB-WERKS.
        GS_DELI_WMS_TXT-LGORT = GS_ITAB-LGORT.
        GS_DELI_WMS_TXT-ERDAT = GS_ITAB-ERDAT.
        GS_DELI_WMS_TXT-MATNR = GS_ITAB-MATNR.
        GS_DELI_WMS_TXT-ARKTX = GS_ITAB-ARKTX.

        GS_DELI_WMS_TXT-LFIMG = GS_ITAB-LFIMG.
        CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
          CHANGING
            VALUE = GS_DELI_WMS_TXT-LFIMG.

        GS_DELI_WMS_TXT-MEINS = GS_ITAB-MEINS.
        GS_DELI_WMS_TXT-ERNAM = GS_ITAB-ERNAM.
        GS_DELI_WMS_TXT-ERZET = GS_ITAB-ERZET.
        GS_DELI_WMS_TXT-VGBEL = GS_ITAB-VGBEL.


        APPEND GS_DELI_WMS_TXT TO GT_DELI_WMS_TXT.
  ENDLOOP.











ENDFORM.  "map data

**&---------------------------------------------------------------------*
**&      F_TEXT_Export
**&---------------------------------------------------------------------*
FORM F_TEXT_EXPORT_LOCAL.
  DATA: G_SPACE TYPE STRING,
        L_POS   TYPE I.
  G_SPACE = CL_ABAP_CONV_IN_CE=>UCCP( '00a0' ).
  DATA: LV_SCB_CORPORATE_NAME(30) TYPE C,
        LV_SCB_CORPORATE_CODE(5)  TYPE C,
        LV_TO_BANK(10)            TYPE C,
        LV_DATE_SEND(12)          TYPE C.

  CLEAR: GS_ITAB.

  GS_SPACE1    = CL_ABAP_CONV_IN_CE=>UCCP( LC_SPACE1 ).

  LOOP AT GT_DELI_WMS_TXT INTO GS_DELI_WMS_TXT1.

    CONCATENATE GS_DELI_WMS_TXT1-VBELN
                GS_DELI_WMS_TXT1-POSNR
                 GS_DELI_WMS_TXT1-WERKS
                 GS_DELI_WMS_TXT1-VGBEL
                 GS_DELI_WMS_TXT1-LGORT
                 GS_DELI_WMS_TXT1-ERZET
                 GS_DELI_WMS_TXT1-ERDAT
                 GS_DELI_WMS_TXT1-MATNR
                 GS_DELI_WMS_TXT1-ARKTX
                 GS_DELI_WMS_TXT1-LFIMG
                 GS_DELI_WMS_TXT1-MEINS
                 GS_DELI_WMS_TXT1-ERNAM

                INTO GS_TEXT_EXPORT RESPECTING BLANKS.
    L_POS = STRLEN( GS_TEXT_EXPORT ).
    WHILE L_POS < 130.

*                            gs_text_export+l_pos(1) = g_space.
      CONCATENATE GS_TEXT_EXPORT  GS_SPACE1 INTO GS_TEXT_EXPORT.
      L_POS = L_POS + 1.
    ENDWHILE.
    APPEND GS_TEXT_EXPORT TO GT_EXPORT_TXT.

    CONCATENATE 'DIDO' SY-DATUM '_' SY-TIMLO '.txt' INTO GS_PATH_NAME.


  ENDLOOP.



ENDFORM.
**&---------------------------------------------------------------------*
**&      F_TEXT_Export
**&---------------------------------------------------------------------*
FORM F_TEXT_EXPORT_SERVER.
  DATA: G_SPACE TYPE STRING,
        L_POS   TYPE I.
  G_SPACE = CL_ABAP_CONV_IN_CE=>UCCP( '00a0' ).
  DATA: LV_SCB_CORPORATE_NAME(30) TYPE C,
        LV_SCB_CORPORATE_CODE(5)  TYPE C,
        LV_TO_BANK(10)            TYPE C,
        LV_DATE_SEND(12)          TYPE C.

*     CLEAR: gs_itab.
*
*     gs_space1    = cl_abap_conv_in_ce=>uccp( lc_space1 ).
*
*      LOOP AT gt_deli_wms_txt INTO gs_deli_wms_txt1.
*
*           CONCATENATE gs_deli_wms_txt1-vbeln
*                       gs_deli_wms_txt1-posnr
*                        gs_deli_wms_txt1-werks
*                        gs_deli_wms_txt1-vgbel
*                        gs_deli_wms_txt1-lgort
*                        gs_deli_wms_txt1-erzet
*                        gs_deli_wms_txt1-erdat
*                        gs_deli_wms_txt1-matnr
*                        gs_deli_wms_txt1-arktx
*                        gs_deli_wms_txt1-lfimg
*                        gs_deli_wms_txt1-meins
*                        gs_deli_wms_txt1-ernam
*
*                       INTO gs_text_export RESPECTING BLANKS.
*                        l_pos = strlen( gs_text_export ).
*                        WHILE l_pos < 130.
*
**                            gs_text_export+l_pos(1) = g_space.
*                            CONCATENATE gs_text_export  gs_space1 INTO gs_text_export.
*                            l_pos = l_pos + 1.
*                        ENDWHILE.
*                       APPEND gs_text_export TO gt_export_txt.
*
*                CONCATENATE 'DIDO' sy-datum '_' sy-timlo '.txt' INTO gs_path_name.
*
*
*      ENDLOOP.
*  CT_RESULT = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( EXPORTING I_ITEM      = GT_DELI_WMS_TXT "gt_export_txt
*                                                                 I_FIX_LEN   = 'X'
*                                                                 I_LEN       = 149 ).
**                                                             I_SEPARATOR = '","'
**                                                             I_START_END_VALUE = '"').

    CT_RESULT = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( EXPORTING "I_HEADER    = LT_HEADER
                                                                I_ITEM      = GT_DELI_WMS_TXT
                                                                I_SEPARATOR = '|').
*                                                                I_START_END_VALUE = '|').

  PERFORM F_EXPORT_TO_SERVER.


ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  CUSTOMER
**&---------------------------------------------------------------------*
FORM GET_CUSTOMER USING    P_KUNNR TYPE IHPA-PARNR
                           P_ADRNR TYPE KNA1-ADRNR
                  CHANGING VALUE(P_NAME_THA) TYPE C.


  DATA: LV_ADRNR TYPE KNA1-ADRNR.


  CLEAR:P_NAME_THA.

  LV_ADRNR =  P_ADRNR.

  IF P_ADRNR EQ ''.
    SELECT SINGLE ADRNR
      INTO (LV_ADRNR)
      FROM KNA1
    WHERE KUNNR EQ P_KUNNR.

  ENDIF.


  SELECT ADDRNUMBER NAME1 NAME2 STREET STR_SUPPL3
         LOCATION CITY2 CITY1 POST_CODE1 TEL_NUMBER
         FAX_NUMBER NATION
  INTO TABLE GT_ADRC
  FROM ADRC
  WHERE ADDRNUMBER = LV_ADRNR
  AND NATION = ''.



  READ TABLE GT_ADRC INTO GS_ADRC WITH KEY ADDRNUMBER = LV_ADRNR
                                           NATION = ''.
  IF SY-SUBRC EQ 0.
    CONCATENATE GS_ADRC-NAME1 GS_ADRC-NAME2 INTO P_NAME_THA.
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
      I_CALLBACK_PROGRAM = GC_REPID
*     i_callback_user_command = 'USER_COMMAND'
      I_SAVE             = 'A'
*     is_layout          = gt_layout
      IT_EVENTS          = GT_EVENTS[]
      IT_FIELDCAT        = GT_FIELDCAT
    TABLES
      T_OUTTAB           = GT_ITAB
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.

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

*     READ TABLE gt_itab INTO gs_itab INDEX rs_selfield-tabindex.
*
*              SET: PARAMETER ID 'KUN'  FIELD gs_itab-kunnr,
*                      PARAMETER ID 'BUK'  FIELD p_bukrs  .
*              CALL TRANSACTION 'XD03'.

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

ENDFORM.

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

  IF NOT P_DISP IS INITIAL.



*       Document
    PERFORM APPEND_FIELDCAT USING 'VBELN'
                                  ''
                                  ''
                                  'Document'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].
*       Document
    PERFORM APPEND_FIELDCAT USING 'POSNR'
                                  ''
                                  ''
                                  'ITEM'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].
*       Plant
    PERFORM APPEND_FIELDCAT USING 'WERKS'
                                  ''
                                  ''
                                  'Plant'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].

    PERFORM APPEND_FIELDCAT USING 'LGORT'
                                  ''
                                  ''
                                  'Storage Location'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].
*       Document Date
    PERFORM APPEND_FIELDCAT USING 'ERDAT'
                                  ''
                                  ''
                                  'Document Date'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].
*       Material
    PERFORM APPEND_FIELDCAT USING 'MATNR'
                                  ''
                                  ''
                                  'Material'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].
*       Material Description
    PERFORM APPEND_FIELDCAT USING 'ARKTX'
                                  ''
                                  ''
                                  'Material Description'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].


*       Qty
    PERFORM APPEND_FIELDCAT USING 'LFIMG'
                                  ''
                                  ''
                                  'Qty'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].
*       Unit of Entry
    PERFORM APPEND_FIELDCAT USING 'MEINS'
                                  ''
                                  ''
                                  'Unit of Entry'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].

*       User name
    PERFORM APPEND_FIELDCAT USING 'ERNAM'
                                  ''
                                  ''
                                  'User name '
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].
*       Time
    PERFORM APPEND_FIELDCAT USING 'ERZET'
                                  ''
                                  ''
                                  'Create Time'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].
*       SO Reference
    PERFORM APPEND_FIELDCAT USING 'VGBEL'
                                  ''
                                  ''
                                  'SO Reference'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].

  ENDIF.


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
  WA_HEADER-INFO = 'Daily DO to WMS'.
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

FORM EXPORT USING PA_PATH
                  LT_DATA_TAB TYPE TRUXS_T_TEXT_DATA.


  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME                = PA_PATH
      FILETYPE                = 'ASC'
      WRITE_FIELD_SEPARATOR   = 'X'
      TRUNC_TRAILING_BLANKS   = 'X'
    TABLES
*     data_tab                = <fs_table>
      DATA_TAB                = LT_DATA_TAB
*     fieldnames              = lt_data_tab
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
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_PATH_NAME
*&---------------------------------------------------------------------*
FORM GET_PATH_NAME  CHANGING PATH.
  DATA: L_LENGTH TYPE I.
  DATA: L_MASK(20) TYPE C.

* S = Save, O = Open
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      MASK             = ',*.txt,*.txt.'
      MODE             = 'O'
    IMPORTING
      FILENAME         = PATH
    EXCEPTIONS
      INV_WINSYS       = 01
      NO_BATCH         = 02
      SELECTION_CANCEL = 03
      SELECTION_ERROR  = 04.

  .


ENDFORM.                    " GET_PATH_NAME

*&---------------------------------------------------------------------*
*&      Form  PF_DIRECTORY_BROWSE
*&---------------------------------------------------------------------*
FORM PF_DIRECTORY_BROWSE  USING  LV_PATH.
*                                 lv_filename.

  DATA: LV_TEMP TYPE STRING.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE
    EXPORTING
*     window_title         =
      INITIAL_FOLDER       = 'C:'
    CHANGING
      SELECTED_FOLDER      = LV_TEMP
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
*    CONCATENATE  lv_temp
**                 lv_filename
*                INTO lv_path SEPARATED BY '\'.
    LV_PATH = LV_TEMP.

  ENDIF.

ENDFORM.                    " PF_DIRECTORY_BROWSE
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
FORM READ_TEXT  USING    P_ID
                         P_OBJECT
                         P_VBELN
                CHANGING P_VALUE.

  DATA: GT_LINES TYPE STANDARD TABLE OF TLINE.
  DATA: GS_LINES LIKE LINE OF GT_LINES.
  DATA: GS_NAME TYPE THEAD-TDNAME.

  CLEAR: P_VALUE, GT_LINES[].

  GS_NAME = P_VBELN.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = P_ID
      LANGUAGE                = SY-LANGU
      NAME                    = GS_NAME
      OBJECT                  = P_OBJECT
    TABLES
      LINES                   = GT_LINES
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
    LOOP AT GT_LINES INTO GS_LINES.
      CONCATENATE P_VALUE  GS_LINES-TDLINE INTO P_VALUE.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  F_export_to_server
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_EXPORT_TO_SERVER.
  DATA : LCL_FTP TYPE REF TO ZCL_SDSCA_GEN_DATA_FILE.

  DATA : LV_STATUS TYPE C.

  DATA : LV_FILE TYPE EVE_TT_STRING.

  DATA : LT_FILE TYPE EREC_T_STRING,
         LS_FILE LIKE LINE OF LT_FILE.
  DATA: LV_PATH(100) TYPE C.
  DATA: LV_PATH_FILE TYPE STRING.


*  ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = 'ZSDSMMR0190'
*                                                  I_SINGLE_VALUE_FLAG = ABAP_TRUE "--> ต้องการค่าเดียว ใส่ ABAP_TRUE ต้องการหลายค่าให้ Comment
*                                                  I_PARAM             = SY-SYSID "LC_CON-SEPARATOR
**                                                  I_PARAM_EXT      =
*                                        CHANGING  C_RETURN            = LV_PATH_FILE ).

*  IF sy-sysid = 'F36'.
*     LV_PATH_FILE = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/Sony_Logi/ZSDI003'.
*  ELSEIF sy-sysid = 'F46'.
*     LV_PATH_FILE = '/interface/Z_DS/SDS/20_OUTBOUND/Sony_Logi/ZSDI003'.
*  ELSE.
**     LV_PATH_FILE = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/DSS/ZMMI008'.
*  ENDIF.

  IF LCL_FTP IS NOT BOUND.
    CREATE OBJECT LCL_FTP.
  ENDIF.

*        LS_FILE = 'Hello Wold'.
*        APPEND LS_FILE TO LT_FILE.
*        LS_FILE = 'Hello Wold1'.
*        APPEND LS_FILE TO LT_FILE.

*  CONCATENATE 'SDSTF' sy-datum '_' sy-timlo '.txt' INTO LV_PATH.
  LV_PATH_FILE = P_PATH.
  CONCATENATE 'DIDO' SY-DATUM '_' SY-TIMLO '.txt' INTO LV_PATH.
  LV_STATUS = LCL_FTP->FTP_FILE_PUT( "I_WINDOW_PATH = 'DEALER_CLAIM/DEV/OUT'
*                                     I_AL11_PATH   = '/tmp'
                                  I_AL11_PATH   = LV_PATH_FILE
                                 I_FILE_NAME   = LV_PATH "'TEST_FTP_FILE.txt'
                                " I_USER        = 'ds'
                                 "I_PASS        = 'ds=20240521'
                                " I_IP          = '172.31.136.250'
                                " I_PORT        = '21'
                                  I_DATA_SPIDER = 'X'
                                 IT_DATA       = CT_RESULT ).

*  LV_STATUS = LCL_FTP->FTP_FILE_PUT( I_WINDOW_PATH = 'DEALER_CLAIM/DEV/OUT'
*                                     I_AL11_PATH   = '/tmp'
*                                     I_FILE_NAME   = LV_PATH "'TEST_FTP_FILE.txt'
*                                     I_USER        = 'ds'
*                                     I_PASS        = 'ds=20240521'
*                                     I_IP          = '172.31.136.250'
*                                     I_PORT        = '21'
*                                     IT_DATA       = CT_RESULT ).
  IF LV_STATUS EQ 'S'.
    " SUCCESS FTP FILE
  ELSE.
    " CANNOT FTP FILE
  ENDIF.
ENDFORM.                 " F_GET_RESULT
