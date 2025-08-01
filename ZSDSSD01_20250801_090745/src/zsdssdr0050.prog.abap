*&---------------------------------------------------------------------*
*& Report ZSDSSDR0050
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSSDR0050.
SET EXTENDED CHECK OFF.
TABLES: VBPLA, THEAD, TTXERN, TTXIT, T005, VBDDL, STXH, SADR, ADR2. "SADR40A
INCLUDE VTTKDATA.                      "Shipment Header
INCLUDE VTTSDATA.                      "Shipment Segment
INCLUDE VTTPDATA.                      "Shipment Items
INCLUDE VBPADATA.                      "Partner
INCLUDE VTFADATA.                      "Flow
INCLUDE SADRDATA.                      "Address
INCLUDE VTLFDATA.                      "Delivery Selection
INCLUDE RVADTABL.                      "Messages
INCLUDE VSEDATA.                       "shipping units
INCLUDE RV56ACOM.                      "I/O-Structure
SET EXTENDED CHECK ON.

DATA:
  XSCREEN(1)              TYPE C,
  RETCODE                 LIKE SY-SUBRC VALUE 0,
  THERE_WAS_OUTPUT(1)     TYPE C        VALUE SPACE,
  NEW_PAGE_WAS_ORDERED(1) TYPE C        VALUE SPACE,
  SUM_QTY                 TYPE P,
  I_PICH                  TYPE P,
  SUM_VOLUM               LIKE LIPS-VOLUM,
  SUM_WEIGHT              TYPE P DECIMALS 2,
  I_QTY                   TYPE P,
  I_VOLUM                 LIKE LIPS-VOLUM,
  I_WEIGHT                TYPE P DECIMALS 2,
  GZ_TEXT(100)            TYPE C,
*  pichonk(1) TYPE c,
  CUS_NAME(120)           TYPE C,
  GCUS_NAME2(30)          TYPE C,
  LCON_NAME(120)          TYPE C.

DATA : GS_DATA TYPE ZSDSSDS001.

CONSTANTS:
  NO(1)  VALUE SPACE,
  YES(1) VALUE 'X'.

TABLES : TPAR .
***********************************************************************
*       FORM ENTRY                                                    *
***********************************************************************
*       Called from the Output Controll program                       *
***********************************************************************
*  -->  RETURN_CODE Status                                            *
*  -->  US_SCREEN                                                     *
***********************************************************************
FORM ENTRY USING RETURN_CODE LIKE SY-SUBRC                  "#EC CALLED
                 US_SCREEN   TYPE C.                        "#EC CALLED

  DATA: LF_FM_NAME        TYPE RS38L_FNAM,
        LS_CONTROL_PARAM  TYPE SSFCTRLOP,
        LS_COMPOSER_PARAM TYPE SSFCOMPOP,
        LS_RECIPIENT      TYPE SWOTOBJID,
        LS_SENDER         TYPE SWOTOBJID,
        LF_FORMNAME       TYPE TDSFNAME,
        LS_ADDR_KEY       LIKE ADDR_KEY,
        LS_DLV_LAND       LIKE VBRK-LAND1,
        LS_JOB_INFO       TYPE SSFCRESCL.

  RETURN_CODE = 1.

  CLEAR : GS_DATA, GS_DATA-T_DETAIL.
  PERFORM DATA_INIT USING US_SCREEN.
  PERFORM GET_DATA.
  PERFORM F_MAP_DATA.
  CHECK RETCODE EQ 0.

  LF_FORMNAME = TNAPR-SFORM.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = LF_FORMNAME
*     variant            = ' '
*     direct_call        = ' '
    IMPORTING
      FM_NAME            = LF_FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.
*     error handling
    PERFORM PROTOCOL_UPDATE.
  ENDIF.
*     call smartform invoice
  CALL FUNCTION LF_FM_NAME
    EXPORTING
      ARCHIVE_INDEX      = TOA_DARA
      ARCHIVE_PARAMETERS = ARC_PARAMS
      CONTROL_PARAMETERS = LS_CONTROL_PARAM
*     mail_appl_obj      =
      MAIL_RECIPIENT     = LS_RECIPIENT
      MAIL_SENDER        = LS_SENDER
      OUTPUT_OPTIONS     = LS_COMPOSER_PARAM
      USER_SETTINGS      = SPACE
      GS_DATA_INIT       = GS_DATA
    IMPORTING
      JOB_OUTPUT_INFO    = LS_JOB_INFO
*     document_output_info =
*     job_output_options =
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.
  IF SY-SUBRC <> 0.
*       error handling
    PERFORM PROTOCOL_UPDATE.
  ELSE.

  ENDIF.
  RETURN_CODE = 0.
ENDFORM.                    "ENTRY
***********************************************************************
*       FORM data_init                                               *
***********************************************************************
FORM DATA_INIT USING VALUE(US_SCREEN) TYPE C.
  XSCREEN = US_SCREEN.
  CLEAR:
    RETCODE,
    THERE_WAS_OUTPUT,
    NEW_PAGE_WAS_ORDERED.
ENDFORM.                    "DATA_INIT
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA .
  DATA LANGUAGE LIKE NAST-SPRAS.
  DATA SHIPMENT_NUMBER LIKE VTTK-TKNUM.
  DATA VBELN LIKE VBPA-VBELN.
  DATA LFIMG LIKE LIPS-LFIMG.

  CLEAR: SUM_QTY, SUM_WEIGHT, SUM_VOLUM.

  LANGUAGE = NAST-SPRAS.
  SHIPMENT_NUMBER = NAST-OBJKY.
  CALL FUNCTION 'RV_SHIPMENT_PRINT_VIEW'
    EXPORTING
      SHIPMENT_NUMBER     = SHIPMENT_NUMBER
      OPTION_TVTK         = 'X'  "Shipmenttype J/N
      OPTION_TTDS         = 'X'  "Disposition J/N
      LANGUAGE            = LANGUAGE
      OPTION_ITEMS        = 'X'  "Transport Items J/N
      OPTION_SEGMENTS     = 'X'  "Transport Segments J/N
      OPTION_PARTNERS     = 'X'  "Partners J/N
      OPTION_SALES_ORDERS = 'X'  "Sales orders J/N
      OPTION_EXPORT_DATA  = 'X'  "Export data J/N
      OPTION_PACKAGES     = 'X'  "Packages J/N
      OPTION_FLOW         = ' '  "Flow J/N
      OPTION_NO_REFRESH   = ' '  "Refresh Tables J/N
    IMPORTING
      F_VTTKVB            = VTTKVB  "Shipment Header
      F_TVTK              = TVTK "Shipmenttype
      F_TVTKT             = TVTKT "Description Shipmenttype
      F_TTDS              = TTDS "Disposition
      F_TTDST             = TTDST "Description Disposition
      F_VBPLA             = VBPLA "Packages
    TABLES
      F_VTTP              = XVTTP "Shipment Items
      F_TRLK              = SLK  "Delivery
      F_TRLP              = SLP  "Delivery Item
      F_VTTS              = XVTTS "Shipment Segments
      F_VTSP              = XVTSP "Segments/Items
      F_VBPA              = XVBPA "Partner
      F_VBADR             = XVBADR  "Address
      F_VTFA              = XVTFA "Flow
      F_VBPLK             = XVBPLK  "Shipment Unit Header
      F_VBPLP             = XVBPLP  "Shipment Unit
      F_VBPLS             = XVBPLS  "Shipment Unit Sum
    EXCEPTIONS
      NOT_FOUND           = 1.

  IF SY-SUBRC NE 0.
    SYST-MSGID = 'VW'.
    SYST-MSGNO = '010'.
    SYST-MSGTY = 'E'.
    SYST-MSGV1 = DBVTTK-TKNUM.
    SYST-MSGV2 = SY-SUBRC.
    RETCODE    = 1.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

  CHECK RETCODE EQ 0.

* Sort shipment items by itenary (i.e. TPRFO)                 "n_902657
  SORT XVTTP BY TPRFO.                                      "n_902657
* SORT SEGMENTS BY CORRECT ORDER (I.E. TSRFO)
  SORT XVTTS BY TSRFO.

* CONVERT UNITS IN DELIVERIES AND DELIVERY-ITEMS
* TO BE CONFORM TO VTTK-UNITS:

  LOOP AT SLK.

    MOVE SLK-VBELN_VAUF TO VBELN.

    SELECT SINGLE * FROM VBPA WHERE VBELN = VBELN
    AND PARNR NE ' '.

    SELECT SINGLE * FROM ADRC WHERE ADDRNUMBER = VBPA-ADRNR.

*    SELECT SINGLE * FROM adr2 WHERE ADDRNUMBER = adrc-ADDRNUMBER.
*    AND date_from = adrc-date_from.

* start of insertion HP_364727
    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        INPUT    = SLK-BRGEW
        UNIT_IN  = SLK-GEWEI
        UNIT_OUT = VTTKVB-DTMEG
      IMPORTING
        OUTPUT   = SLK-BRGEW.

* end of insertion HP_364727
    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        INPUT    = SLK-BTGEW
        UNIT_IN  = SLK-GEWEI
        UNIT_OUT = VTTKVB-DTMEG
      IMPORTING
        OUTPUT   = SLK-BTGEW.

    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        INPUT    = SLK-NTGEW
        UNIT_IN  = SLK-GEWEI
        UNIT_OUT = VTTKVB-DTMEG
      IMPORTING
        OUTPUT   = SLK-NTGEW.

    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        INPUT    = SLK-VOLUM
        UNIT_IN  = SLK-VOLEH
        UNIT_OUT = VTTKVB-DTMEV
      IMPORTING
        OUTPUT   = SLK-VOLUM.

    SLK-GEWEI = VTTKVB-DTMEG.
    SLK-VOLEH = VTTKVB-DTMEV.
    MODIFY SLK.
  ENDLOOP.

  LOOP AT SLP.

    CLEAR VBELN.
    MOVE SLP-LFIMG TO LFIMG.

    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        INPUT    = SLP-BRGEW
        UNIT_IN  = SLP-GEWEI
        UNIT_OUT = VTTKVB-DTMEG
      IMPORTING
        OUTPUT   = SLP-BRGEW.

    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        INPUT    = SLP-NTGEW
        UNIT_IN  = SLP-GEWEI
        UNIT_OUT = VTTKVB-DTMEG
      IMPORTING
        OUTPUT   = SLP-NTGEW.

    SLP-GEWEI = VTTKVB-DTMEG.
    MODIFY SLP.
  ENDLOOP.
* Transfer address number for mail
  IF NAST-NACHA = '5'.                "e-mail                "v_n_742056.
* Determine the type of the partner number
    SELECT SINGLE * FROM TPAR
                  WHERE PARVW = NAST-PARVW.
    IF SY-SUBRC NE 0.
      EXIT.
    ENDIF.
* Search the address number
    LOOP AT XVBPA
     WHERE PARVW = NAST-PARVW.
      CASE TPAR-NRART.             "type of the partner number
        WHEN 'KU'.                 "- customer
          CHECK XVBPA-KUNNR = NAST-PARNR.
        WHEN 'LI'.                 "- vendor
          CHECK XVBPA-LIFNR = NAST-PARNR.
        WHEN 'AP'.                 "- contact person
          CHECK XVBPA-PARNR = NAST-PARNR.
        WHEN 'PE'.                 "- personell number
          CHECK XVBPA-PERNR = NAST-PARNR.
      ENDCASE.
      "^_n_742056.
* deleted line of n_656692
      ADDR_KEY-ADDRNUMBER = XVBPA-ADRNR.
      ADDR_KEY-PERSNUMBER = XVBPA-ADRNP.
      EXIT.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROTOCOL_UPDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PROTOCOL_UPDATE .
  IF XSCREEN = SPACE.
    CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
      EXPORTING
        MSG_ARBGB = SYST-MSGID
        MSG_NR    = SYST-MSGNO
        MSG_TY    = SYST-MSGTY
        MSG_V1    = SYST-MSGV1
        MSG_V2    = SYST-MSGV2
        MSG_V3    = SYST-MSGV3
        MSG_V4    = SYST-MSGV4.
  ELSE.
    MESSAGE ID SYST-MSGID TYPE 'I' NUMBER SYST-MSGNO
            WITH SYST-MSGV1 SYST-MSGV2 SYST-MSGV3 SYST-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_MAP_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_MAP_DATA .

  DATA : LS_DETAIL LIKE LINE OF GS_DATA-T_DETAIL.

  DATA : LCL_UTIL TYPE REF TO ZCL_SDSCA_UTIL_SDS.

  DATA : LS_SLP LIKE LINE OF SLP.

  DATA : LV_SHQTY TYPE P DECIMALS 0,
         LV_VOLME TYPE P DECIMALS 3,
         LV_WEIGH TYPE P DECIMALS 2.

  DATA : LV_SHQTY_N TYPE P DECIMALS 0,
         LV_VOLME_N TYPE P DECIMALS 3,
         LV_WEIGH_N TYPE P DECIMALS 2.

  DATA : LV_SHQTY_SUM TYPE CHAR20,
         LV_VOLME_SUM TYPE CHAR20,
         LV_WEIGH_SUM TYPE CHAR20.

  DATA : BEGIN OF LS_DATA,
           TKNUM TYPE ZSDSSDT013-TKNUM,
           VBELN TYPE ZSDSSDT013-VBELN,
           POSNR TYPE ZSDSSDT013-POSNR,
           LFIMG TYPE ZSDSSDT013-LFIMG,
         END OF LS_DATA.
  DATA : LT_DATA LIKE TABLE OF LS_DATA.

  DATA : LS_SUM TYPE ZSDSSDS114.

  CONSTANTS : BEGIN OF LC_CON,
                ID_REMARK     TYPE THEAD-TDID     VALUE 'ZH10',
                OBJECT_REMARK TYPE THEAD-TDOBJECT VALUE 'VBBK',
                VTTK          TYPE THEAD-TDOBJECT VALUE 'VTTK',
                OTHER         TYPE THEAD-TDID     VALUE 'Z021',
              END OF LC_CON.

  IF LCL_UTIL IS NOT BOUND.
    CREATE OBJECT LCL_UTIL.
  ENDIF.

  GS_DATA-SHINO = |{ VTTKVB-TKNUM ALPHA = OUT }|.

  PERFORM f_GET_ZSHIP_MENT USING VTTKVB-TKNUM
                        CHANGING LT_DATA.

  LCL_UTIL->GET_VEND_NAME( EXPORTING I_VEND_NO  = VTTKVB-TDLNR
                                     I_NATION   = SPACE
                           IMPORTING E_NAME_ALL = GS_DATA-DLIBY ).

  GS_DATA-CONTP = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = LC_CON-ID_REMARK
                                                I_NAME     = SLK-TDNAME
                                                I_OBJECT   = LC_CON-OBJECT_REMARK
                                                I_LANGUAGE = SY-LANGU ).

  WRITE VTTKVB-DTDIS TO GS_DATA-EXEDT.
  IF VTTKVB-DATEN IS NOT INITIAL.
    WRITE VTTKVB-DATEN TO GS_DATA-SHIED.
  ENDIF.
*GS_DATA-SHIED = ''.
  GS_DATA-DRINA = VTTKVB-EXTI2.
  GS_DATA-TRKNO = VTTKVB-EXTI1.
*GS_DATA-MANPW = ''.
  PERFORM F_GET_SHIPMENT_TYPE USING VTTKVB-SDABW
                           CHANGING GS_DATA-TRKTY.

  GS_DATA-ZONCS = VTTKVB-VSBED.
  PERFORM F_GET_SHIPPING_COND_TEXT USING VTTKVB-VSBED
                                CHANGING GS_DATA-ZONES.
  GS_DATA-ZONCE = VTTKVB-ROUTE.
  PERFORM F_GET_ROUTE_TEXT USING VTTKVB-ROUTE
                        CHANGING GS_DATA-ZONEE.

  SPLIT GS_DATA-ZONEE AT ':' INTO DATA(LV_TEXT1)
                                  DATA(LV_TEXT2).

  IF LV_TEXT2 IS NOT INITIAL.
    GS_DATA-ZONEE = LV_TEXT2.
  ENDIF.

  SHIFT GS_DATA-ZONEE LEFT DELETING LEADING SPACE.

  GS_DATA-OTHER = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = LC_CON-OTHER
                                                I_NAME     = VTTKVB-TKNUM
                                                I_OBJECT   = LC_CON-VTTK
                                                I_LANGUAGE = SY-LANGU ).

  LOOP AT SLP INTO LS_SLP WHERE LGORT IS NOT INITIAL.
    READ TABLE LT_DATA INTO LS_DATA
    WITH KEY VBELN = LS_SLP-VBELN
             POSNR = LS_SLP-POSNR.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    LS_DETAIL-DOCNO = LS_SLP-VBELN.
    PERFORM F_GET_CUST_NAME USING LS_SLP-VBELN
                         CHANGING LS_DETAIL-CUSNA.
    LS_DETAIL-DESCR = LS_SLP-MATNR.
    LS_DETAIL-STORG = LS_SLP-LGORT.
    LV_SHQTY_N      = LS_DATA-LFIMG."SLP-LFIMG.
    LV_VOLME_N      = LS_SLP-VOLUM.
    LV_WEIGH_N      = LS_SLP-BRGEW.

    WRITE LV_SHQTY_N TO LS_DETAIL-SHQTY.
    WRITE LV_VOLME_N TO LS_DETAIL-VOLME.
    WRITE LV_WEIGH_N TO LS_DETAIL-WEIGH.

    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DETAIL-SHQTY WITH SPACE.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DETAIL-VOLME WITH SPACE.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DETAIL-WEIGH WITH SPACE.

*  LS_DETAIL-DEPAT = ''.
*  LS_DETAIL-ARRIE = ''.
    APPEND LS_DETAIL TO GS_DATA-T_DETAIL.

    ADD LS_SLP-LFIMG TO LV_SHQTY.
    ADD LS_SLP-VOLUM TO LV_VOLME.
    ADD LS_SLP-BRGEW TO LV_WEIGH.
  ENDLOOP.

  WRITE LV_SHQTY TO GS_DATA-TTQTY.
  WRITE LV_VOLME TO GS_DATA-TTVOL.
  WRITE LV_WEIGH TO GS_DATA-TTWAG.

  REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN GS_DATA-TTQTY WITH SPACE.
  REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN GS_DATA-TTVOL WITH SPACE.
  REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN GS_DATA-TTWAG WITH SPACE.

  SORT GS_DATA-T_DETAIL BY DOCNO.

  LOOP AT GS_DATA-T_DETAIL ASSIGNING FIELD-SYMBOL(<LFS_TMP>).
    MOVE-CORRESPONDING <LFS_TMP> TO LS_DETAIL.
    AT END OF DOCNO.
      LS_DETAIL-ENDNO = ABAP_TRUE.
    ENDAT.

*    MOVE-CORRESPONDING <LFS_TMP> TO LS_SUM.
    LV_SHQTY_SUM = LS_DETAIL-SHQTY.
    LV_VOLME_SUM = LS_DETAIL-VOLME.
    LV_WEIGH_SUM = LS_DETAIL-WEIGH.

    REPLACE ALL OCCURRENCES OF PCRE ',' IN LV_SHQTY_SUM WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE ',' IN LV_VOLME_SUM WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE ',' IN LV_WEIGH_SUM WITH ''.

    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_SHQTY_SUM WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_VOLME_SUM WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_WEIGH_SUM WITH ''.

    LS_SUM-DOCNO = LS_DETAIL-DOCNO.
    LS_SUM-SHQTY = LV_SHQTY_SUM.
    LS_SUM-VOLME = LV_VOLME_SUM.
    LS_SUM-WEIGH = LV_WEIGH_SUM.

    COLLECT LS_SUM INTO GS_DATA-T_DETAIL_SUM.
    <LFS_TMP> = LS_DETAIL.
    CLEAR : LS_DETAIL,LS_SUM.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_SHIPMENT_TYPE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> VTTKVB_VSART
*&      <-- GS_DATA_TRKTY
*&---------------------------------------------------------------------*
FORM F_GET_SHIPMENT_TYPE  USING    LV_SDABW
                          CHANGING LV_TRKTY.
*  SELECT SINGLE ( VSART && ' ' && BEZEI ) AS NAME_ALL
*      FROM T173T
*      WHERE VSART EQ @LV_VSART
*        AND SPRAS EQ @SY-LANGU
*       INTO @LV_TRKTY.

  SELECT SINGLE ( SDABW && ' ' && BEZEI ) AS NAME_ALL

      FROM TVSAKT

      WHERE SDABW EQ @LV_SDABW
        AND SPRAS EQ @SY-LANGU
       INTO @LV_TRKTY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_SHIPPING_COND_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> VTTKVB_VSBED
*&      <-- GS_DATA_ZONES
*&---------------------------------------------------------------------*
FORM F_GET_SHIPPING_COND_TEXT  USING    LV_VSBED
                               CHANGING LV_ZONES.

  SELECT SINGLE VTEXT
    FROM TVSBT
    INTO LV_ZONES
    WHERE VSBED EQ LV_VSBED
      AND SPRAS EQ SY-LANGU.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_ROUTE_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> VTTKVB_ROUTE
*&      <-- GS_DATA_ZONEE
*&---------------------------------------------------------------------*
FORM F_GET_ROUTE_TEXT  USING    LV_ROUTE
                       CHANGING LV_ZONEE.

  SELECT SINGLE BEZEI
      FROM TVROT
      WHERE ROUTE EQ @LV_ROUTE
        AND SPRAS EQ @SY-LANGU
       INTO @LV_ZONEE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_CUST_NAME
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_SLP_VBELN
*&      <-- LS_DETAIL_CUSNA
*&---------------------------------------------------------------------*
FORM F_GET_CUST_NAME  USING    LV_VBELN
                      CHANGING LV_CUSNA.

  DATA : LS_SLK LIKE LINE OF SLK.

  READ TABLE SLK INTO LS_SLK
  WITH KEY TDNAME = LV_VBELN.
  IF SY-SUBRC EQ 0.
    SELECT SINGLE
      NAME1,
      NAME2,
      NAME3,
      NAME4
      FROM ADRC
      INTO @DATA(LS_ADRC)
      WHERE ADDRNUMBER = @LS_SLK-ADRNR_AG
        AND NATION = @SPACE.
    IF SY-SUBRC = 0.
      CONCATENATE LS_ADRC-NAME1
                  LS_ADRC-NAME2
                  LS_ADRC-NAME3
                  LS_ADRC-NAME4
             INTO LV_CUSNA SEPARATED BY SPACE.
    ELSE.
      CONCATENATE LS_SLK-NAME1_AG
                  LS_SLK-NAME2_AG
                  LS_SLK-NAME3_AG
                  LS_SLK-NAME4_AG
             INTO LV_CUSNA SEPARATED BY SPACE.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_GET_ZSHIP_MENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_DATA_SHINO
*&      <-- LT_DATA
*&---------------------------------------------------------------------*
FORM f_GET_ZSHIP_MENT  USING    LU_SHINO
                       CHANGING LCG_DATA.

  SELECT TKNUM,
         VBELN,
         POSNR,
         LFIMG
    FROM ZSDSSDT013
    WHERE TKNUM EQ @LU_SHINO
    INTO TABLE @LCG_DATA.

ENDFORM.
