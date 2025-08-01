*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0210
*  Creation Date      : 05.07.2024
*  Author             : Jakarin S.
*  Add-on ID          :
*  Description        : Create Shipment Document
*  Purpose            :
*  Copied from        :
*  Restriction        :
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSSDR0210 LINE-COUNT 65
                   LINE-SIZE 200
                   MESSAGE-ID ZSDSSD01
                   NO STANDARD PAGE HEADING.
*----------------------------------------------------------------------*
*   T A B L E S                                                        *
*----------------------------------------------------------------------*
TABLES: LIKP, T173T, TVSBT, TVROT, TTDS, TVTK.

* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : Start
TABLES: VTTK.
* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : End


*----------------------------------------------------------------------*
*   T Y P E S                                                          *
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_MARA,
         MATNR TYPE MARA-MATNR,
         GROES TYPE MARA-GROES,
       END   OF TY_MARA.

*----------------------------------------------------------------------*
*   C O N S T A N T S                                                  *
*----------------------------------------------------------------------*
CONSTANTS C_TCODE TYPE SY-TCODE VALUE 'VT02N'.

*----------------------------------------------------------------------*
*   R A N G E S                                                        *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   S E L E C T - O P T I O N S   A N D   P A R A M E T E R S          *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B01 WITH FRAME TITLE TEXT-B01.
  SELECT-OPTIONS S_VSTEL FOR LIKP-VSTEL.             "Shipping Point
SELECTION-SCREEN END   OF BLOCK B01.

SELECTION-SCREEN BEGIN OF BLOCK B02 WITH FRAME TITLE TEXT-B02.
  SELECT-OPTIONS S_LFDAT FOR LIKP-LFDAT OBLIGATORY.  "Delivery Date
SELECTION-SCREEN END   OF BLOCK B02.

SELECTION-SCREEN BEGIN OF BLOCK B03 WITH FRAME TITLE TEXT-B03.
  SELECT-OPTIONS: S_LSTEL FOR LIKP-LSTEL,    "Loading Point
                  S_VBELN FOR LIKP-VBELN.    "Delivery Order No.
SELECTION-SCREEN END   OF BLOCK B03.

SELECTION-SCREEN BEGIN OF BLOCK B04 WITH FRAME TITLE TEXT-B04.
  PARAMETERS: P_NOCOM  RADIOBUTTON GROUP MODE USER-COMMAND MODE DEFAULT 'X',
              P_COMPT  RADIOBUTTON GROUP MODE,
* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : Start
              P_SHPLST RADIOBUTTON GROUP MODE.
  SELECT-OPTIONS:
              S_DLIDAT FOR LIKP-LFDAT MODIF ID SH1,
              S_SHPBEG FOR VTTK-DATBG MODIF ID SH1,
              S_SHPENT FOR VTTK-ERDAT MODIF ID SH1.
  PARAMETERS:
* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : End
              P_VARI TYPE DISVARIANT-VARIANT.
SELECTION-SCREEN END   OF BLOCK B04.

SELECTION-SCREEN BEGIN OF BLOCK B05 WITH FRAME TITLE TEXT-B05.
  PARAMETERS: P_TPLST TYPE VTTK-TPLST MEMORY ID TDP DEFAULT 1000,
              P_SHTYP TYPE VTTK-SHTYP MEMORY ID TSH DEFAULT 1000.
SELECTION-SCREEN END   OF BLOCK B05.

*----------------------------------------------------------------------*
*   I N T E R N A L   T A B L E S                                      *
*----------------------------------------------------------------------*
DATA: BEGIN OF T_LIPS OCCURS 0,
        VBELN TYPE LIKP-VBELN,
        POSNR TYPE LIPS-POSNR,
        VSTEL TYPE LIKP-VSTEL,
        LFDAT TYPE LIKP-LFDAT,
        LSTEL TYPE LIKP-LSTEL,
        LFIMG TYPE LIPS-LFIMG,
        VRKME TYPE LIPS-VRKME,
        ROUTE TYPE LIKP-ROUTE,
        MATNR TYPE LIPS-MATNR,
        LGORT TYPE LIPS-LGORT,
        BRGEW TYPE LIPS-BRGEW,
        VOLUM TYPE LIPS-VOLUM,
        KUNNR TYPE LIKP-KUNNR,
        GEWEI TYPE LIPS-GEWEI,
        VOLEH TYPE LIPS-VOLEH,
        VKBUR TYPE LIPS-VKBUR,
* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : Start
        VTWEG TYPE LIPS-VTWEG,
        VGBEL TYPE LIPS-VGBEL,
        PRODH TYPE LIPS-PRODH,
* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : End
      END   OF T_LIPS.

DATA: BEGIN OF T_DELIV OCCURS 0,
        LFDAT       TYPE LIKP-LFDAT,      "(01) Delivery Date
        ROUTE       TYPE LIKP-ROUTE,      "(02) Route  ...
        LSTEL       TYPE LIKP-LSTEL,      "(03) Loading Point
        KUNNR       TYPE LIKP-KUNNR,      "(04) Ship-to
        NAME1(80),                   "(05) Full Name
        VKBURT      TYPE TVKBT-BEZEI,     "(06) Sales Office
        REMARK(512),                 "(07) Request remark
        CITY1       TYPE ADRC-CITY1,      "(08) City  ...
        ADDRS(200),                  "(09) Address
        VBELN       TYPE LIKP-VBELN,      "(10) Delivery
        POSNR       TYPE LIPS-POSNR,      "(11) Item  ...
        MATNR       TYPE LIPS-MATNR,      "(12) Material
        LFIMG       TYPE LIPS-LFIMG,      "(13) Delivery Qty
        LGORT       TYPE LIPS-LGORT,      "(14) SLoc  ...
        BRGEW       TYPE LIPS-BRGEW,      "(15) Total weight
        REWEI       TYPE LIPS-BRGEW,      "(16) Remaining weight
        VOLUM       TYPE LIPS-VOLUM,      "(17) Total M3
        REM3        TYPE LIPS-VOLUM,      "(18) Remaining M3
        SHQTY       TYPE LIPS-LFIMG,      "(19) Shipped Qty
        RELOAD      TYPE LIPS-LFIMG,      "(20) Remaining Load
        LOAD        TYPE LIPS-LFIMG,      "(21) Load Qty
        GROES       TYPE MARA-GROES,      "(**) Dimension
        GEWEI       TYPE LIPS-GEWEI,      "(**) Weight Unit
        VOLEH       TYPE LIPS-VOLEH,      "(**) Volume Unit
        VRKME       TYPE LIPS-VRKME,      "(**) Unit
        VKBUR       TYPE LIPS-VKBUR,      "(**) Sales Office
        VSART       TYPE VTTK-VSART,
        SEL,                         "(**) Select
        MSG(100),                    "(**) Message
        STAT(4),                     "(**) Status
        SCOL        TYPE LVC_T_SCOL,      "(**) Color
      END   OF T_DELIV.

DATA: BEGIN OF T_VTTK OCCURS 0,
        TKNUM TYPE VTTK-TKNUM,
        DTDIS TYPE VTTK-DTDIS,
        EXTI2 TYPE VTTK-EXTI2,
        EXTI1 TYPE VTTK-EXTI1,
        VSART TYPE VTTK-VSART,
        VSBED TYPE VTTK-VSBED,
        ROUTE TYPE VTTK-ROUTE,
        TDLNR TYPE VTTK-TDLNR,
        DATEN TYPE VTTK-DATEN,
        LFIMG TYPE LIPS-LFIMG,
* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : Start
        ERDAT TYPE VTTK-ERDAT,
        DATBG TYPE VTTK-DATBG,
* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : End
      END   OF T_VTTK.

DATA: BEGIN OF T_SHIPM OCCURS 0,
        TKNUM       TYPE VTTK-TKNUM,      "(26) Shipment No.
        DELBY(80),                  "(27) Delivery by
        DTDIS       TYPE VTTK-DTDIS,      "(28) Date Execute
        EXTI2       TYPE VTTK-EXTI2,      "(29) Driver Name
        EXTI1       TYPE VTTK-EXTI1,      "(30) Truck No.
        MANPO(200),                 "(31) Manpower
        OTHER(200),                 "(32) Others
        VSARTT(25),                 "(33) Truck Type
        VSBEDT(25),                 "(34) Zone ต้นทาง
        ROUTET(25),                 "(35) Zone ปลายทาง
        CONTA(200),                 "(36) Contact person/Tel. No.
        VBELN       TYPE LIKP-VBELN,      "(37) Delivery
        LFDAT       TYPE LIKP-LFDAT,      "(38) Delivery Date
        NAME1(80),                  "(39) Customer name
        ADDRS(200),                 "(40) Address
        MATNR       TYPE LIPS-MATNR,      "(41) Description
        LGORT       TYPE LIPS-LGORT,      "(42) SLoc  ...
        SHQTY       TYPE LIPS-LFIMG,      "(43) Quantity
        REM3        TYPE LIPS-VOLUM,      "(44) Volume
        REWEI       TYPE LIPS-BRGEW,      "(45) Weight (KG)
        LFIMG       TYPE LIPS-LFIMG,      "(**) Delivery Qty
        POSNR       TYPE LIPS-POSNR,      "(**) Item
        KUNNR       TYPE LIKP-KUNNR,      "(**) Ship-to
        CITY1       TYPE ADRC-CITY1,      "(**) City  ...
        VRKME       TYPE LIPS-VRKME,      "(**) Unit
        GEWEI       TYPE LIPS-GEWEI,      "(**) Weight Unit
        VOLEH       TYPE LIPS-VOLEH,      "(**) Volume Unit
        VOLUM       TYPE LIPS-VOLUM,      "(**) Total M3
        BRGEW       TYPE LIPS-BRGEW,      "(**) Total weigh
        GROES       TYPE MARA-GROES,      "(**) Dimension
        REMARK(512),                "(**) Request remark
        DATEN       TYPE VTTK-DATEN,      "(**) Shipment End

        VSART       TYPE T173T-VSART,    "(**)  Truck type code " iBS 05.05.2017 10:02:49 CS-SDS-0001
      END   OF T_SHIPM.

DATA: BEGIN OF T_VBPA OCCURS 0,
        VBELN  TYPE VBPA-VBELN,
        KUNNR  TYPE VBPA-KUNNR,
        ADRNR  TYPE VBPA-ADRNR,
        NAME1  TYPE ADRC-NAME1,
        NAME2  TYPE ADRC-NAME2,
        CITY1  TYPE ADRC-CITY1,
        CITY2  TYPE ADRC-CITY2,
        STREET TYPE ADRC-STREET,
* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : Start
        PARVW  TYPE VBPA-PARVW,
        NATION TYPE ADRC-NATION,
* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : End
      END   OF T_VBPA.

DATA: BEGIN OF T_LOCKED OCCURS 0,
        VBELN TYPE LIKP-VBELN,
      END   OF T_LOCKED.

DATA: BEGIN OF T_NOTE OCCURS 0,
        VBELN TYPE LIKP-VBELN,
        LINE  TYPE TXW_NOTE-LINE,
      END   OF T_NOTE.

DATA: T_UPD_SHIP_DOC TYPE TABLE OF ZSDSSDT013 WITH HEADER LINE,
      T_TVKBT        TYPE TABLE OF TVKBT        WITH HEADER LINE,
      T_TLINE        TYPE TABLE OF TLINE        WITH HEADER LINE.

DATA: t_ZSDSSDT013 TYPE SORTED TABLE OF ZSDSSDT013 WITH NON-UNIQUE KEY VBELN POSNR
                                                       WITH HEADER LINE,
      T_MARA       TYPE HASHED TABLE OF TY_MARA      WITH UNIQUE KEY MATNR
                                                       WITH HEADER LINE.

* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : Start
DATA: BEGIN OF T_SHIPLIST OCCURS 0,
        VTWEG       TYPE LIPS-VTWEG,      "(2) Distribution Channel
        VTEXT       TYPE TVTWT-VTEXT,     "(20) Distribution Channel Name
        VKBUR       TYPE LIPS-VKBUR,      "(4) Sale Office
        VKBURT      TYPE TVKBT-BEZEI,    "(06) Sales Office Name
        VBELN       TYPE LIKP-VBELN,      "(37) Delivery
        TKNUM       TYPE VTTK-TKNUM,      "(26) Shipment No.
        VGBEL       TYPE LIPS-VGBEL,      "(10) Document number of the reference document
        CUSNAME     TYPE CHAR80,       "(80) Customer Name(adrc name1 + name2)
        CAT         TYPE LIPS-PRODH,        "(5) Product hierarchy(only digits 1 - 5)
        CLASS       TYPE LIPS-PRODH,      "(5) Product hierarchy(only digits 6 - 10)
        MATNR       TYPE LIPS-MATNR,      "(18) Material No.
        VRKME       TYPE LIPS-VRKME,                              "(3) Unit
        SHQTY       TYPE LIPS-LFIMG,      "(43) Quantity
        SHIPPED     TYPE ZSDSSDT013-LFIMG,     "(43) Quantity
        REMARK(512),                "(512) Request remark
        LSTEL       TYPE LIKP-LSTEL,      "(03) Loading Point
        LOADTEXT    TYPE TVLAT-VTEXT,  "(20) Loading Point Text
        TDLNR       TYPE VTTK-TDLNR,       "(10) Transport
        LGORT       TYPE LIPS-LGORT,      "(4) SLoc  ...
        EXTI1       TYPE VTTK-EXTI1,      "(20)External ID 1
        EXTI2       TYPE VTTK-EXTI2,      "(20)Truck driver
        VSART       TYPE VTTK-VSART,      "(2)Shipping type
        M3          TYPE LIPS-VOLUM ,        "(15/3) (LIPS-VOLUM / LIPS-LFIMG) * Shipped.
        SHPTOCITY   TYPE ADRC-CITY1,  "(40)Ship-to City
        SOLDTOLOC   TYPE ADRC-CITY1,   "(40)Ship-to Location
        SHPTOADD    TYPE CHAR140,   "(140)Ship-to Address
        SOLDTOADD   TYPE CHAR140,   "(140)Sold-to Address
        LFDAT       TYPE LIKP-LFDAT,      "(38) Delivery Date
        DTDIS       TYPE VTTK-DTDIS,      "(8)Date Execute
        ERDAT       TYPE VTTK-ERDAT,      "(8)Create on
        DATBG       TYPE VTTK-DATBG,      "(8)Shipment start
        MANPOW(512),                "(512) Man power
        OTHER(512),                "(512) Other
      END   OF T_SHIPLIST,

      T_TVTWT LIKE TVTWT OCCURS 0 WITH HEADER LINE,
      T_TVLAT LIKE TVLAT OCCURS 0 WITH HEADER LINE.
* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : End

DATA: T_SCOL TYPE LVC_T_SCOL,
      W_SCOL TYPE LVC_S_SCOL.

*----------------------------------------------------------------------*
*   S T R U C T U R E S                                                *
*----------------------------------------------------------------------*
DATA: W_LIPS  LIKE LINE OF T_LIPS,
      W_DELIV LIKE LINE OF T_DELIV,
      W_VTTK  LIKE LINE OF T_VTTK,
      W_SHIPM LIKE LINE OF T_SHIPM,
      W_VBPA  LIKE LINE OF T_VBPA.

*----------------------------------------------------------------------*
*   W O R K I N G   F I E L D   S Y M B O L S                          *
*----------------------------------------------------------------------*
FIELD-SYMBOLS <W_DELIV> LIKE LINE OF T_DELIV.

*----------------------------------------------------------------------*
*   W O R K   A R E A   A N D   F I E L D - S Y M B O L
*   ( FOR WORKING WITH SPECIFIED INTERNAL TABLES )
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   W O R K I N G   S T O R A G E   D A T A                            *
*----------------------------------------------------------------------*
DATA: W_LFIMG   TYPE LIPS-LFIMG,
      W_SUBRC   TYPE I,
      W_CHANGED.

*----------------------------------------------------------------------*
*   A L V Parameters                                                   *
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.  "for event handling

DATA: W_OKCODE           LIKE SY-UCOMM,
      W_SAVE_OK          LIKE SY-UCOMM,
      W_CONTAINER        TYPE SCRFNAME VALUE 'CON_9000',
      W_GRID             TYPE REF TO CL_GUI_ALV_GRID,
      W_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      W_EVENT_RECEIVER   TYPE REF TO LCL_EVENT_RECEIVER,
      W_DOCKING          TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      W_LAYOUT           TYPE LVC_S_LAYO,
      W_VARIANT          TYPE DISVARIANT,
      W_LAYO             TYPE SLIS_LAYOUT_ALV,
      W_STABLE           TYPE LVC_S_STBL.

DATA: DOCUMENT       TYPE REF TO CL_DD_DOCUMENT,
      DG_SPLITTER    TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_PARENT_GRID TYPE REF TO CL_GUI_CONTAINER,
      DG_HTML_CNTRL  TYPE REF TO CL_GUI_HTML_VIEWER,
      DG_PARENT_HTML TYPE REF TO CL_GUI_CONTAINER.

DATA: T_FIELDCAT TYPE LVC_T_FCAT,
      W_FIELDCAT TYPE LVC_S_FCAT,
      T_SORT     TYPE LVC_T_SORT,
      W_SORT     TYPE LVC_S_SORT,
      T_EXC      TYPE UI_FUNCTIONS.

*// for BAPI_SHIPMENT_CREATE
DATA: T_HEADERDEADLINE TYPE TABLE OF BAPISHIPMENTHEADERDEADLINE,
      T_ITEMDATA       TYPE TABLE OF BAPISHIPMENTITEM,
      T_STAGEDATA      TYPE TABLE OF BAPISHIPMENTSTAGE,
      T_STAGEDEADLINE  TYPE TABLE OF BAPISHIPMENTSTAGEDEADLINE,
      T_ITEMONSTAGE    TYPE TABLE OF BAPISHIPMENTITEMONSTAGE,
      T_ADDRESS        TYPE TABLE OF BAPISHIPMENTADDRESS,
      T_HDUNHEADER     TYPE TABLE OF BAPISHIPMENTHDUNHEADER,
      T_HDUNITEM       TYPE TABLE OF BAPISHIPMENTHDUNITEM,
      T_RETURN         TYPE TABLE OF BAPIRET2,
      W_HEADERDEADLINE TYPE BAPISHIPMENTHEADERDEADLINE,
      W_ITEMDATA       TYPE BAPISHIPMENTITEM,
      W_STAGEDATA      TYPE BAPISHIPMENTSTAGE,
      W_STAGEDEADLINE  TYPE BAPISHIPMENTSTAGEDEADLINE,
      W_ITEMONSTAGE    TYPE BAPISHIPMENTITEMONSTAGE,
      W_ADDRESS        TYPE BAPISHIPMENTADDRESS,
      W_HDUNHEADER     TYPE BAPISHIPMENTHDUNHEADER,
      W_HDUNITEM       TYPE BAPISHIPMENTHDUNITEM,
      W_RETURN         TYPE BAPIRET2,
      W_HEADERDATA     TYPE BAPISHIPMENTHEADER,
      W_TRANSPORT      TYPE BAPISHIPMENTIDS-SHIPMENTNUM,
      W_SHIPMENTGUID   TYPE BAPISHIPMENTIDS-GUID.

*----------------------------------------------------------------------*
*   M A C R O   C O M M A N D   D E F I N I T I O N                    *
*----------------------------------------------------------------------*
DEFINE M%APPEND_T_SCOL.
  CLEAR W_SCOL.
  W_SCOL-FNAME     = &1.
  W_SCOL-COLOR-COL = 7.
  W_SCOL-COLOR-INT = 1.
  W_SCOL-COLOR-INV = 0.
  APPEND W_SCOL TO T_SCOL.
END-OF-DEFINITION.

DEFINE M%APPEND_RANGE.
  &1-SIGN   = &2.
  &1-OPTION = &3.
  &1-LOW    = &4.
  &1-HIGH   = &5.
  APPEND &1.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS: CATCH_DATA_CHANGED
      FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
      IMPORTING
        ER_DATA_CHANGED
        E_UCOMM.

    METHODS: CATCH_DATA_CHANGED_FINISHED
      FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
      IMPORTING
        E_MODIFIED
        ET_GOOD_CELLS.

    METHODS: CATCH_DOUBLECLICK
      FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
      IMPORTING
        E_COLUMN
        ES_ROW_NO
        SENDER.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
  METHOD CATCH_DATA_CHANGED.
    MOVE 'X' TO W_CHANGED.
  ENDMETHOD.                    "catch_data_changed

  METHOD CATCH_DATA_CHANGED_FINISHED.
    IF ( P_NOCOM IS NOT INITIAL ) AND ( W_CHANGED IS NOT INITIAL ).
      LOOP AT T_DELIV ASSIGNING <W_DELIV>.
        IF ( <W_DELIV>-SEL IS NOT INITIAL ).
          <W_DELIV>-SCOL[] = T_SCOL[].
        ELSE.
          REFRESH <W_DELIV>-SCOL.
        ENDIF.
      ENDLOOP.

      CLEAR W_CHANGED.

      CALL METHOD W_GRID->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = W_STABLE.
    ENDIF.
  ENDMETHOD.                    "catch_data_changed_finished

  METHOD CATCH_DOUBLECLICK.
    CASE 'X'.
      WHEN P_NOCOM.
        READ TABLE T_DELIV INTO W_DELIV INDEX ES_ROW_NO-ROW_ID.
        IF ( SY-SUBRC EQ 0 ).
          CASE E_COLUMN-FIELDNAME.
            WHEN 'KUNNR'.
              CHECK W_DELIV-KUNNR IS NOT INITIAL.
              SET PARAMETER ID 'KUN' FIELD W_DELIV-KUNNR.
              CALL TRANSACTION 'XD03' AND SKIP FIRST SCREEN.

            WHEN 'VBELN'.
              CHECK W_DELIV-VBELN IS NOT INITIAL.
              SET PARAMETER ID 'VL' FIELD W_DELIV-VBELN.
              CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.

            WHEN 'MATNR'.
              CHECK W_DELIV-MATNR IS NOT INITIAL.
              SET PARAMETER ID 'MAT' FIELD W_DELIV-MATNR.
              CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

            WHEN 'REMARK'.
              CHECK W_DELIV-REMARK IS NOT INITIAL.
              PERFORM POPUP_REMARK USING W_DELIV-VBELN.
          ENDCASE.
        ENDIF.

      WHEN P_COMPT.
        READ TABLE T_SHIPM INTO W_SHIPM INDEX ES_ROW_NO-ROW_ID.
        IF ( SY-SUBRC EQ 0 ).
          CASE E_COLUMN-FIELDNAME.
            WHEN 'TKNUM'.
              CHECK W_SHIPM-TKNUM IS NOT INITIAL.
              SET PARAMETER ID 'TNR' FIELD W_SHIPM-TKNUM.
              CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.

            WHEN 'VBELN'.
              CHECK W_SHIPM-VBELN IS NOT INITIAL.
              SET PARAMETER ID 'VL' FIELD W_SHIPM-VBELN.
              CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.

            WHEN 'REMARK'.
              CHECK W_SHIPM-REMARK IS NOT INITIAL.
              PERFORM POPUP_REMARK USING W_SHIPM-VBELN.
          ENDCASE.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "catch_doubleclick

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*$*$-------------------------------------------------------------------*
*$*$                   I N I T I A L I Z A T I O N                     *
*$*$-------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.


* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : Start
*$*$-------------------------------------------------------------------*
*$*$      A T   S E L E C T I O N - S C R E E N   O U T P U T          *
*$*$-------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF P_SHPLST = 'X'.

      IF SCREEN-GROUP1 = 'SH1'.

        SCREEN-ACTIVE = '1'.

        MODIFY SCREEN.
      ENDIF.

    ELSE.

      IF SCREEN-GROUP1 = 'SH1'.

        SCREEN-ACTIVE = '0'.

        MODIFY SCREEN.
      ENDIF.

    ENDIF.

  ENDLOOP.
* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : End


*$*$-------------------------------------------------------------------*
*$*$                A T   S E L E C T I O N - S C R E E N              *
*$*$-------------------------------------------------------------------*
AT SELECTION-SCREEN.

AT SELECTION-SCREEN ON P_TPLST.
  IF ( P_TPLST IS INITIAL ).
    IF ( SY-UCOMM NE 'MODE' ) AND ( P_NOCOM IS NOT INITIAL ).
      MESSAGE E000 WITH TEXT-REQ.
    ENDIF.
  ELSE.
    SELECT SINGLE *
    FROM TTDS
    WHERE TPLST EQ P_TPLST.

    IF ( SY-SUBRC EQ 0 ).
      AUTHORITY-CHECK OBJECT 'V_VTTK_TDS'
               ID 'TPLST' FIELD P_TPLST
               ID 'ACTVT' DUMMY.

      IF ( SY-SUBRC NE 0 ).
        MESSAGE E000 WITH 'No Auth. for' 'Transport. Planning Points' P_TPLST.
      ENDIF.
    ELSE.
      MESSAGE E000 WITH 'Transport. Planning Points' P_TPLST 'does not exist'.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON P_SHTYP.
  IF ( P_SHTYP IS INITIAL ).
    IF ( SY-UCOMM NE 'MODE' ) AND ( P_NOCOM IS NOT INITIAL ).
      MESSAGE E000 WITH TEXT-REQ.
    ENDIF.
  ELSE.
    SELECT SINGLE *
    FROM TVTK
    WHERE SHTYP EQ P_SHTYP.

    IF ( SY-SUBRC EQ 0 ).
      AUTHORITY-CHECK OBJECT 'V_VTTK_SHT'
               ID 'SHTYP' FIELD P_SHTYP
               ID 'ACTVT' DUMMY.

      IF ( SY-SUBRC NE 0 ).
        MESSAGE E000 WITH 'No Authorization for' 'Shipment Type' P_SHTYP.
      ENDIF.
    ELSE.
      MESSAGE E000 WITH 'Shipment type' P_SHTYP 'does not exist'.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  W_VARIANT-REPORT   = SY-REPID.
  W_VARIANT-USERNAME = SY-UNAME.

* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : Start
  IF P_SHPLST EQ 'X'.
    W_VARIANT-HANDLE = '0003'.
  ELSE.
    CLEAR W_VARIANT-HANDLE.
  ENDIF.
* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : End




  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT = W_VARIANT
      I_SAVE     = 'A'
    IMPORTING
      ES_VARIANT = W_VARIANT
    EXCEPTIONS
      NOT_FOUND  = 2.

  IF ( SY-SUBRC EQ 0 ).
    P_VARI = W_VARIANT-VARIANT.
  ENDIF.

*$*$-------------------------------------------------------------------*
*$*$                 S T A R T - O F - S E L E C T I O N               *
*$*$-------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM INIT_DATA.
* eBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : Start
*  PERFORM read_main_data." Comment
  IF P_SHPLST NE 'X'.
    PERFORM READ_MAIN_DATA.
  ELSE.
    PERFORM READ_MAIN_DATA_SHIPMENT_LIST.
  ENDIF.
* eBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : End
  PERFORM GENERATE_T_PRINT.

*$*$-------------------------------------------------------------------*
*$*$                  E N D - O F - S E L E C T I O N                  *
*$*$-------------------------------------------------------------------*
END-OF-SELECTION.
* eBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : Start
*  IF ( t_deliv[] IS NOT INITIAL ) OR ( t_shipm[] IS NOT INITIAL )."Comment

  IF P_SHPLST = 'X'.
    W_VARIANT-REPORT   = SY-REPID.
    W_VARIANT-USERNAME = SY-UNAME.
    W_VARIANT-HANDLE = '0003'.
  ELSE.
    W_VARIANT-REPORT   = SY-REPID.
    W_VARIANT-USERNAME = SY-UNAME.
    CLEAR W_VARIANT-HANDLE.
  ENDIF.

  IF ( T_DELIV[] IS NOT INITIAL ) OR ( T_SHIPM[] IS NOT INITIAL ) OR ( T_SHIPLIST[] IS NOT INITIAL ).
* eBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : End
    PERFORM PRINT_REPORT.
  ELSE.
    MESSAGE S003.
  ENDIF.

*$*$********************************************************************
*$*$               E N D   O F   M A I N   P R O G R A M               *
*$*$********************************************************************
*$*$-------------------------------------------------------------------*
*$*$                  I N T E R A C T I V E   E V E N T                *
*$*$-------------------------------------------------------------------*

*$*$-------------------------------------------------------------------*
*$*$                       T O P - O F - P A G E                       *
*$*$-------------------------------------------------------------------*

*$*$********************************************************************
*$*$                     S U B   R O U T I N E S                       *
*$*$********************************************************************
*&---------------------------------------------------------------------*
*&      Form  INIT_DATA
*&---------------------------------------------------------------------*
FORM INIT_DATA .

  REFRESH: T_EXC, T_SCOL.
  APPEND: 'REFRESH' TO T_EXC,
          'SEL'     TO T_EXC,
          'DSEL'    TO T_EXC,
          'CREATE'  TO T_EXC.

  M%APPEND_T_SCOL: 'STAT',   'LFDAT',  'ROUTE', 'LSTEL',
                   'KUNNR',  'NAME1',  'VKBUR', 'VKBURT',
                   'REMARK', 'CITY1',  'ADDRS', 'VBELN',
                   'POSNR',  'MATNR',  'LFIMG', 'VRKME',
                   'LGORT',  'GROES',  'BRGEW', 'REWEI',
                   'GEWEI',  'VOLUM',  'REM3',  'VOLEH',
                   'SHQTY',  'RELOAD', 'MSG'.

ENDFORM.                    " INIT_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_MAIN_DATA
*&---------------------------------------------------------------------*
FORM READ_MAIN_DATA.
  DATA: BEGIN OF LT_KNA1 OCCURS 0,
          KUNNR TYPE KNA1-KUNNR,
        END   OF LT_KNA1.

  DATA: BEGIN OF LT_TVKB OCCURS 0,
          VKBUR TYPE TVBUR-VKBUR,
        END   OF LT_TVKB.

  DATA: BEGIN OF LT_MARA OCCURS 0,
          MATNR TYPE MARA-MATNR,
        END   OF LT_MARA.

  REFRESH: T_LIPS, T_MARA, T_VBPA, T_TVKBT, t_ZSDSSDT013, T_NOTE.

*// Delivery Order
  SELECT B~VBELN B~POSNR A~VSTEL A~LFDAT A~LSTEL
         B~LFIMG B~VRKME A~ROUTE B~MATNR B~LGORT
         B~BRGEW B~VOLUM A~KUNNR B~GEWEI B~VOLEH
         B~VKBUR
  INTO TABLE T_LIPS
  FROM LIKP AS A
    INNER JOIN LIPS AS B
       ON B~VBELN EQ A~VBELN

*    INNER JOIN vbup AS c
*       ON c~vbeln EQ b~vbeln
*      AND c~posnr EQ b~posnr

  WHERE A~VBELN IN S_VBELN
    AND A~VSTEL IN S_VSTEL
    AND A~LFDAT IN S_LFDAT
    AND A~LSTEL IN S_LSTEL
    AND B~KOSTA NE 'C'.

  CHECK T_LIPS[] IS NOT INITIAL.

  LOOP AT T_LIPS INTO W_LIPS.
    LT_KNA1-KUNNR = W_LIPS-KUNNR.
    COLLECT LT_KNA1.

    LT_TVKB-VKBUR = W_LIPS-VKBUR.
    COLLECT LT_TVKB.

    LT_MARA-MATNR = W_LIPS-MATNR.
    COLLECT LT_MARA.
  ENDLOOP.

*// Ship-to
  SELECT A~VBELN A~KUNNR A~ADRNR B~NAME1
         B~NAME2 B~CITY1 B~CITY2 B~STREET
  INTO TABLE T_VBPA
  FROM VBPA AS A
    INNER JOIN ADRC AS B
       ON B~ADDRNUMBER EQ A~ADRNR

    FOR ALL ENTRIES IN T_LIPS
  WHERE A~VBELN     EQ T_LIPS-VBELN
    AND A~POSNR     EQ '000000'
    AND A~PARVW     EQ 'WE'
    AND B~NATION    EQ SPACE
    AND B~DATE_FROM LE SY-DATUM.

  SORT T_VBPA BY VBELN.
  DELETE ADJACENT DUPLICATES FROM T_VBPA COMPARING ALL FIELDS.

*// Material Data
  IF ( LT_MARA[] IS NOT INITIAL ).
    SELECT MATNR GROES
    INTO TABLE T_MARA
    FROM MARA FOR ALL ENTRIES IN LT_MARA
    WHERE MATNR EQ LT_MARA-MATNR.
  ENDIF.

*// Sales Offices
  IF ( LT_TVKB[] IS NOT INITIAL ).
    SELECT *
    INTO TABLE T_TVKBT
    FROM TVKBT FOR ALL ENTRIES IN LT_TVKB
    WHERE SPRAS EQ 'E'
      AND VKBUR EQ LT_TVKB-VKBUR.

    SORT T_TVKBT BY SPRAS VKBUR.
  ENDIF.

*// Shipment document Item
  SELECT *
  INTO TABLE t_ZSDSSDT013
  FROM ZSDSSDT013 FOR ALL ENTRIES IN T_LIPS
  WHERE VBELN EQ T_LIPS-VBELN
    AND POSNR EQ T_LIPS-POSNR.

ENDFORM.                               " READ_MAIN_DATA

*&---------------------------------------------------------------------*
*&      Form  GENERATE_T_PRINT
*&---------------------------------------------------------------------*
FORM GENERATE_T_PRINT.
  DATA: LV_NAME1     TYPE LFA1-NAME1,
        LV_NAME2     TYPE LFA1-NAME2,
        LV_TEXT1(40),
        LV_TEXT2(40).

* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : Start
  DATA: LV_NAME  TYPE THEAD-TDNAME.
* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : End

  REFRESH: T_DELIV, T_SHIPM, T_LOCKED.

  CASE 'X'.
    WHEN P_NOCOM.
      LOOP AT T_LIPS INTO W_LIPS.
        PERFORM CHECK_SHIPMENT_ITEM USING W_SUBRC.
        CHECK W_SUBRC EQ 0.

        CLEAR W_DELIV.
        PERFORM ENQUEUE_DELIVERY USING W_LIPS-VBELN.

        W_DELIV-LFDAT  = W_LIPS-LFDAT.
        W_DELIV-ROUTE  = W_LIPS-ROUTE.
        W_DELIV-LSTEL  = W_LIPS-LSTEL.
        W_DELIV-KUNNR  = W_LIPS-KUNNR.
        W_DELIV-VBELN  = W_LIPS-VBELN.
        W_DELIV-POSNR  = W_LIPS-POSNR.
        W_DELIV-MATNR  = W_LIPS-MATNR.
        W_DELIV-VRKME  = W_LIPS-VRKME.
        W_DELIV-LGORT  = W_LIPS-LGORT.
        W_DELIV-GEWEI  = W_LIPS-GEWEI.
        W_DELIV-VOLEH  = W_LIPS-VOLEH.
        W_DELIV-VKBUR  = W_LIPS-VKBUR.
        W_DELIV-LFIMG  = W_LIPS-LFIMG.                     "(13) Delivery Qty
        W_DELIV-BRGEW  = W_LIPS-BRGEW.                     "(15) Total weight
        W_DELIV-VOLUM  = W_LIPS-VOLUM.                     "(17) Total M3
        W_DELIV-SHQTY  = W_LFIMG.                          "(19) Shipped Qty
        W_DELIV-RELOAD = W_DELIV-LFIMG - W_DELIV-SHQTY.    "(20) Remaining Load
        W_DELIV-LOAD   = W_DELIV-RELOAD.                   "(21) Load Qty

        IF ( W_DELIV-LFIMG NE 0 ).
*// --->(16) Remaining weight
          W_DELIV-REWEI = ( W_DELIV-BRGEW * W_DELIV-RELOAD ) / W_DELIV-LFIMG.

*// --->(18) Remaining M3
          W_DELIV-REM3 = ( W_DELIV-VOLUM * W_DELIV-RELOAD ) / W_DELIV-LFIMG.
        ENDIF.

        READ TABLE T_MARA WITH KEY MATNR = W_LIPS-MATNR.
        IF ( SY-SUBRC EQ 0 ).
          W_DELIV-GROES = T_MARA-GROES.
        ENDIF.

        READ TABLE T_VBPA INTO W_VBPA WITH KEY VBELN = W_LIPS-VBELN BINARY SEARCH.
        IF ( SY-SUBRC EQ 0 ).
          W_DELIV-CITY1 = W_VBPA-CITY1.

          CONCATENATE W_VBPA-NAME1 W_VBPA-NAME2
            INTO W_DELIV-NAME1 SEPARATED BY SPACE.

          CONCATENATE W_VBPA-STREET W_VBPA-CITY2
            INTO W_DELIV-ADDRS SEPARATED BY SPACE.
        ENDIF.

        READ TABLE T_TVKBT WITH KEY VKBUR = W_LIPS-VKBUR BINARY SEARCH.
        IF ( SY-SUBRC EQ 0 ).
          W_DELIV-VKBURT = T_TVKBT-BEZEI.
        ENDIF.

        PERFORM GET_REQUEST_REMARK USING W_DELIV-REMARK.

        APPEND W_DELIV TO T_DELIV.
      ENDLOOP.

    WHEN P_COMPT.
      LOOP AT T_LIPS INTO W_LIPS.
        PERFORM CHECK_SHIPMENT_ITEM USING W_SUBRC.
        CHECK W_SUBRC EQ 0.

        LOOP AT T_VTTK INTO W_VTTK.
          CLEAR W_SHIPM.
          W_SHIPM-TKNUM = W_VTTK-TKNUM.
          W_SHIPM-DTDIS = W_VTTK-DTDIS.
          W_SHIPM-EXTI2 = W_VTTK-EXTI2.
          W_SHIPM-EXTI1 = W_VTTK-EXTI1.
          W_SHIPM-VBELN = W_LIPS-VBELN.
          W_SHIPM-POSNR = W_LIPS-POSNR.
          W_SHIPM-LFDAT = W_LIPS-LFDAT.
          W_SHIPM-KUNNR = W_LIPS-KUNNR.
          W_SHIPM-MATNR = W_LIPS-MATNR.
          W_SHIPM-LGORT = W_LIPS-LGORT.
          W_SHIPM-BRGEW = W_LIPS-BRGEW.
          W_SHIPM-VOLUM = W_LIPS-VOLUM.
          W_SHIPM-LFIMG = W_VTTK-LFIMG.
          W_SHIPM-SHQTY = W_LFIMG.
          W_SHIPM-DATEN = W_VTTK-DATEN.

          IF ( W_SHIPM-SHQTY NE 0 ).
*// ------->(16) Remaining weight
            W_SHIPM-REWEI = ( W_SHIPM-BRGEW * W_SHIPM-LFIMG ) / W_SHIPM-SHQTY.

*// ------->(18) Remaining M3
            W_SHIPM-REM3 = ( W_SHIPM-VOLUM * W_SHIPM-LFIMG ) / W_SHIPM-SHQTY.
          ENDIF.

*// ----->(27) Delivery by
          SELECT SINGLE NAME1 NAME2
          INTO (LV_NAME1, LV_NAME2)
          FROM LFA1
          WHERE LIFNR EQ W_VTTK-TDLNR.

          IF ( SY-SUBRC EQ 0 ).
            CONCATENATE LV_NAME1 LV_NAME2
              INTO W_SHIPM-DELBY SEPARATED BY SPACE.
          ENDIF.

*// ----->(39) Customer name & (40) Address
          READ TABLE T_VBPA INTO W_VBPA WITH KEY VBELN = W_LIPS-VBELN BINARY SEARCH.
          IF ( SY-SUBRC EQ 0 ).
            W_SHIPM-CITY1 = W_VBPA-CITY1.

            CONCATENATE W_VBPA-NAME1 W_VBPA-NAME2
              INTO W_SHIPM-NAME1 SEPARATED BY SPACE.

            CONCATENATE W_VBPA-STREET W_VBPA-CITY2
              INTO W_SHIPM-ADDRS SEPARATED BY SPACE.
          ENDIF.

          PERFORM GET_MANPOWER.
          PERFORM GET_OTHERS.
          PERFORM GET_CONTRACT.

*// ----->(33) Truck Type
          SELECT SINGLE *
          FROM T173T
          WHERE SPRAS EQ 'E'
            AND VSART EQ W_VTTK-VSART.

          IF ( SY-SUBRC EQ 0 ).
            CONCATENATE T173T-VSART T173T-BEZEI
              INTO W_SHIPM-VSARTT SEPARATED BY SPACE.

            MOVE : T173T-VSART TO W_SHIPM-VSART.      " iBS 05.05.2017 10:05:22 MA " For sort Truck type code CS-SDS-0001
          ENDIF.

*// ----->(34) Zone ต้นทาง
          SELECT SINGLE *
          FROM TVSBT
          WHERE SPRAS EQ 'E'
            AND VSBED EQ W_VTTK-VSBED.

          IF ( SY-SUBRC EQ 0 ).
            CONCATENATE TVSBT-VSBED TVSBT-VTEXT
              INTO W_SHIPM-VSBEDT SEPARATED BY SPACE.
          ENDIF.

*// ----->(35) Zone ปลายทาง
          SELECT SINGLE *
          FROM TVROT
          WHERE SPRAS EQ 'E'
            AND ROUTE EQ W_VTTK-ROUTE.

          IF ( SY-SUBRC EQ 0 ).
            FIND ':' IN TVROT-BEZEI.
            IF ( SY-SUBRC EQ 0 ).
              SPLIT TVROT-BEZEI AT ':' INTO LV_TEXT1 LV_TEXT2.
              CONCATENATE TVROT-ROUTE LV_TEXT2
                INTO W_SHIPM-ROUTET SEPARATED BY SPACE.
            ELSE.
              CONCATENATE TVROT-ROUTE TVROT-BEZEI
                INTO W_SHIPM-ROUTET SEPARATED BY SPACE.
            ENDIF.
          ENDIF.

          READ TABLE T_MARA WITH KEY MATNR = W_LIPS-MATNR.
          IF ( SY-SUBRC EQ 0 ).
            W_SHIPM-GROES = T_MARA-GROES.
          ENDIF.

          PERFORM GET_REQUEST_REMARK USING W_SHIPM-REMARK.

          APPEND W_SHIPM TO T_SHIPM.
        ENDLOOP.
      ENDLOOP.

* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : Start
    WHEN   P_SHPLST.

      LOOP AT T_LIPS.

        LOOP AT t_ZSDSSDT013
          WHERE VBELN = T_LIPS-VBELN
            AND POSNR = T_LIPS-POSNR.
          CLEAR T_SHIPLIST.
          T_SHIPLIST-TKNUM = t_ZSDSSDT013-TKNUM.

          READ TABLE T_VTTK INTO W_VTTK
           WITH KEY TKNUM = t_ZSDSSDT013-TKNUM.
          IF SY-SUBRC = 0.
            T_SHIPLIST-TDLNR = W_VTTK-TDLNR.
            T_SHIPLIST-EXTI1 = W_VTTK-EXTI1.
            T_SHIPLIST-EXTI2 = W_VTTK-EXTI2.
            T_SHIPLIST-VSART = W_VTTK-VSART.
            T_SHIPLIST-DTDIS = W_VTTK-DTDIS.
            T_SHIPLIST-ERDAT = W_VTTK-ERDAT.
            T_SHIPLIST-DATBG = W_VTTK-DATBG.
            T_SHIPLIST-SHIPPED = T_SHIPLIST-SHIPPED + t_ZSDSSDT013-LFIMG.

            CLEAR T_TLINE.
            REFRESH T_TLINE[].
            LV_NAME  = W_VTTK-TKNUM.
            PERFORM READ_TEXT TABLES T_TLINE
                      USING  '0007' 'E' LV_NAME 'VTTK'.
            LOOP AT T_TLINE.
              IF ( T_SHIPLIST-MANPOW IS NOT INITIAL ).
                CONCATENATE T_SHIPLIST-MANPOW T_TLINE-TDLINE
                  INTO T_SHIPLIST-MANPOW SEPARATED BY SPACE.
              ELSE.
                T_SHIPLIST-MANPOW = T_TLINE-TDLINE.
              ENDIF.
            ENDLOOP.

            CLEAR T_TLINE.
            REFRESH T_TLINE[].

            PERFORM READ_TEXT TABLES T_TLINE
                      USING  '0008' 'E' LV_NAME 'VTTK'.
            LOOP AT T_TLINE.
              IF ( T_SHIPLIST-OTHER IS NOT INITIAL ).
                CONCATENATE T_SHIPLIST-OTHER T_TLINE-TDLINE
                  INTO T_SHIPLIST-OTHER SEPARATED BY SPACE.
              ELSE.
                T_SHIPLIST-OTHER = T_TLINE-TDLINE.
              ENDIF.
            ENDLOOP.
          ENDIF. " t_vttk

          AT END OF POSNR.

            T_SHIPLIST-M3 = ( T_LIPS-VOLUM / T_LIPS-LFIMG ) * T_SHIPLIST-SHIPPED .

            T_SHIPLIST-VTWEG = T_LIPS-VTWEG.
            T_SHIPLIST-VKBUR = T_LIPS-VKBUR.
            T_SHIPLIST-VBELN = T_LIPS-VBELN.
            T_SHIPLIST-VGBEL = T_LIPS-VGBEL.
            T_SHIPLIST-CAT = T_LIPS-PRODH+0(5).
            T_SHIPLIST-CLASS = T_LIPS-PRODH+5(5).
            T_SHIPLIST-MATNR = T_LIPS-MATNR.
            T_SHIPLIST-VRKME = T_LIPS-VRKME.
            T_SHIPLIST-LSTEL = T_LIPS-LSTEL.
            T_SHIPLIST-LGORT = T_LIPS-LGORT.
            T_SHIPLIST-LFDAT = T_LIPS-LFDAT.
            T_SHIPLIST-SHQTY = T_LIPS-LFIMG.

            READ TABLE T_TVKBT WITH KEY VKBUR = T_LIPS-VKBUR.
            IF ( SY-SUBRC EQ 0 ).
              T_SHIPLIST-VKBURT = T_TVKBT-BEZEI.
            ENDIF.

            READ TABLE T_TVTWT
              WITH KEY VTWEG = T_LIPS-VTWEG.
            IF SY-SUBRC = 0.
              T_SHIPLIST-VTEXT = T_TVTWT-VTEXT.
            ENDIF.

            READ TABLE T_TVLAT
              WITH KEY LSTEL = T_LIPS-LSTEL
                       VSTEL = T_LIPS-VSTEL.
            IF SY-SUBRC = 0.
              T_SHIPLIST-LOADTEXT = T_TVLAT-VTEXT.
            ENDIF.
            CLEAR T_TLINE.
            REFRESH T_TLINE[].
            LV_NAME = T_LIPS-VBELN.

            PERFORM READ_TEXT TABLES T_TLINE
                      USING  'ZH10' 'E' LV_NAME 'VBBK'.
            LOOP AT T_TLINE.
              IF ( T_SHIPLIST-REMARK IS NOT INITIAL ).
                CONCATENATE T_SHIPLIST-REMARK T_TLINE-TDLINE
                  INTO T_SHIPLIST-REMARK SEPARATED BY SPACE.
              ELSE.
                T_SHIPLIST-REMARK = T_TLINE-TDLINE.
              ENDIF.
            ENDLOOP.

            READ TABLE T_VBPA INTO W_VBPA
              WITH KEY VBELN = T_LIPS-VBELN
                       PARVW = 'WE'
                       NATION = SPACE.
            IF ( SY-SUBRC EQ 0 ).
              T_SHIPLIST-SHPTOCITY = W_VBPA-CITY1.
              CONCATENATE W_VBPA-STREET W_VBPA-CITY2 W_VBPA-CITY1
                INTO T_SHIPLIST-SHPTOADD SEPARATED BY SPACE.
            ENDIF.

            READ TABLE T_VBPA INTO W_VBPA
              WITH KEY VBELN = T_LIPS-VBELN
                       PARVW = 'AG'
                       NATION = SPACE.
            IF ( SY-SUBRC EQ 0 ).
              T_SHIPLIST-SOLDTOLOC = W_VBPA-CITY1.
              CONCATENATE W_VBPA-STREET W_VBPA-CITY2 W_VBPA-CITY1
                INTO T_SHIPLIST-SOLDTOADD SEPARATED BY SPACE.
            ENDIF.

            READ TABLE T_VBPA INTO W_VBPA
              WITH KEY VBELN = T_LIPS-VBELN
                       PARVW = 'AG'
                       NATION = 'I'.
            IF ( SY-SUBRC EQ 0 ).
              CONCATENATE W_VBPA-NAME1 W_VBPA-NAME2
                INTO T_SHIPLIST-CUSNAME SEPARATED BY SPACE.
            ENDIF.

            APPEND T_SHIPLIST.
          ENDAT.
        ENDLOOP. " t_ZSDSSDT013
      ENDLOOP." t_lips
* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : End
  ENDCASE.

ENDFORM.                               " GENERATE_T_PRINT

*&---------------------------------------------------------------------*
*&      Form  PRINT_REPORT
*&---------------------------------------------------------------------*
FORM PRINT_REPORT.

  CALL SCREEN 9000.

ENDFORM.                               " PRINT_REPORT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.

  CASE 'X'.
    WHEN P_NOCOM.
      SET PF-STATUS 'ALV'.
      SET TITLEBAR  'ALV'.

    WHEN P_COMPT.
      SET PF-STATUS 'ALV' EXCLUDING T_EXC.
      SET TITLEBAR  'REP'.

* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : Start
    WHEN P_SHPLST.
      SET PF-STATUS 'ALV' EXCLUDING T_EXC.
      SET TITLEBAR  'SHP'.
* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : End

  ENDCASE.

ENDMODULE.                    "status_9000 OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PBO_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO_9000 OUTPUT.

  IF ( W_CUSTOM_CONTAINER IS INITIAL ).
    PERFORM CREATE_AND_INIT_ALV.
  ELSE.
    CALL METHOD W_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = W_STABLE.
  ENDIF.

ENDMODULE.                 " PBO_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

  CALL METHOD W_GRID->CHECK_CHANGED_DATA.

  W_SAVE_OK = W_OKCODE.
  CLEAR W_OKCODE.

  CASE W_SAVE_OK.
    WHEN '&F03'.
      LEAVE TO SCREEN 0.

    WHEN '&F12' OR '&F15'.
      LEAVE PROGRAM.

    WHEN 'REFRESH'.
      PERFORM READ_MAIN_DATA.
      PERFORM GENERATE_T_PRINT.
      PERFORM PRINT_REPORT.

    WHEN 'SEL'.
      PERFORM SELECT_OR_NOT_SELECT TABLES T_DELIV USING 'X'.

    WHEN 'DSEL'.
      PERFORM SELECT_OR_NOT_SELECT TABLES T_DELIV USING ' '.

    WHEN 'CREATE'.
      READ TABLE T_DELIV INTO W_DELIV WITH KEY SEL = 'X'.
      IF ( SY-SUBRC EQ 0 ).
        CLEAR W_SUBRC.
        LOOP AT T_DELIV INTO W_DELIV WHERE SEL EQ 'X'.
          IF ( W_DELIV-LOAD GT W_DELIV-RELOAD ).
            MOVE 4 TO W_SUBRC.
            MESSAGE S000 WITH 'Load qty not allow greater than Remaining Qty'
                         DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
        ENDLOOP.

        CHECK W_SUBRC EQ 0.
        PERFORM CREATE_SHIPMENT.

        CALL METHOD W_GRID->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = W_STABLE.
      ELSE.
        MESSAGE S000 WITH 'No select to create Shipment' DISPLAY LIKE 'E'.
      ENDIF.
  ENDCASE.

ENDMODULE.                    "user_command_9000 INPUT

*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
FORM CREATE_AND_INIT_ALV.
  DATA LT_EXCLUDE TYPE UI_FUNCTIONS.

  IF ( CL_GUI_ALV_GRID=>OFFLINE( ) IS INITIAL ).   " Online Process
    CREATE OBJECT W_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME = W_CONTAINER.

    CREATE OBJECT W_GRID
      EXPORTING
        I_PARENT = W_CUSTOM_CONTAINER.
  ELSE.
    CREATE OBJECT W_GRID
      EXPORTING
        I_PARENT = W_DOCKING.
  ENDIF.

*// Create Event receiver
  IF ( W_EVENT_RECEIVER IS INITIAL ).
    CREATE OBJECT W_EVENT_RECEIVER.
  ENDIF.

  PERFORM FIELDCAT_INIT        USING T_FIELDCAT[].
  PERFORM SORT_INIT            USING T_SORT[].
  PERFORM EXCLUDE_TB_FUNCTIONS CHANGING LT_EXCLUDE.

  W_LAYOUT-ZEBRA      = 'X'.
  W_LAYOUT-CWIDTH_OPT = 'X'.
  W_LAYOUT-SEL_MODE   = 'D'.
  W_LAYOUT-NO_ROWMARK = 'X'.
  W_VARIANT-REPORT    = SY-REPID.
  W_STABLE-ROW        = 'X'.
  W_STABLE-COL        = 'X'.

  CASE 'X'.
    WHEN P_NOCOM.
      W_LAYOUT-CTAB_FNAME = 'SCOL'.

      CALL METHOD W_GRID->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          IS_VARIANT                    = W_VARIANT
          I_SAVE                        = 'A'
          IS_LAYOUT                     = W_LAYOUT
          IT_TOOLBAR_EXCLUDING          = LT_EXCLUDE
        CHANGING
          IT_FIELDCATALOG               = T_FIELDCAT
          IT_OUTTAB                     = T_DELIV[]
          IT_SORT                       = T_SORT[]
        EXCEPTIONS
          INVALID_PARAMETER_COMBINATION = 1
          PROGRAM_ERROR                 = 2
          TOO_MANY_LINES                = 3
          OTHERS                        = 4.

    WHEN P_COMPT.
      CALL METHOD W_GRID->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          IS_VARIANT                    = W_VARIANT
          I_SAVE                        = 'A'
          IS_LAYOUT                     = W_LAYOUT
          IT_TOOLBAR_EXCLUDING          = LT_EXCLUDE
        CHANGING
          IT_FIELDCATALOG               = T_FIELDCAT
          IT_OUTTAB                     = T_SHIPM[]
          IT_SORT                       = T_SORT[]
        EXCEPTIONS
          INVALID_PARAMETER_COMBINATION = 1
          PROGRAM_ERROR                 = 2
          TOO_MANY_LINES                = 3
          OTHERS                        = 4.
* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : Start
    WHEN P_SHPLST.
      SORT T_SHIPLIST BY TKNUM VBELN.
      CALL METHOD W_GRID->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          IS_VARIANT                    = W_VARIANT
          I_SAVE                        = 'A'
          IS_LAYOUT                     = W_LAYOUT
          IT_TOOLBAR_EXCLUDING          = LT_EXCLUDE
        CHANGING
          IT_FIELDCATALOG               = T_FIELDCAT
          IT_OUTTAB                     = T_SHIPLIST[]
          IT_SORT                       = T_SORT[]
        EXCEPTIONS
          INVALID_PARAMETER_COMBINATION = 1
          PROGRAM_ERROR                 = 2
          TOO_MANY_LINES                = 3
          OTHERS                        = 4.
* iBA 14/Feb/2017 CS-SDS-0003 : Add Process Mode Shipment List : End
  ENDCASE.

  CALL METHOD W_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  CALL METHOD W_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  SET HANDLER: W_EVENT_RECEIVER->CATCH_DOUBLECLICK           FOR W_GRID,
               W_EVENT_RECEIVER->CATCH_DATA_CHANGED          FOR W_GRID,
               W_EVENT_RECEIVER->CATCH_DATA_CHANGED_FINISHED FOR W_GRID.

ENDFORM.                    " CREATE_AND_INIT_ALV

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
FORM FIELDCAT_INIT USING PT_FIELDCAT TYPE LVC_T_FCAT.
  DATA L_POS TYPE I.

  REFRESH PT_FIELDCAT.

  CASE 'X'.
    WHEN P_NOCOM.
*// ->(**) Select
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'SEL'.
      W_FIELDCAT-INTTYPE    = 'C'.
      W_FIELDCAT-INTLEN     = 1.
      W_FIELDCAT-CHECKBOX   = 'X'.
      W_FIELDCAT-EDIT       = 'X'.
      W_FIELDCAT-FIX_COLUMN = 'X'.
      W_FIELDCAT-COLTEXT    = 'Select'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(**) Status
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'STAT'.
      W_FIELDCAT-INTTYPE    = 'C'.
      W_FIELDCAT-INTLEN     = 4.
      W_FIELDCAT-ICON       = 'X'.
      W_FIELDCAT-FIX_COLUMN = 'X'.
      W_FIELDCAT-COLTEXT    = 'Status'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(01) Delivery Date
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'LFDAT'.
      W_FIELDCAT-REF_FIELD  = 'LFDAT'.
      W_FIELDCAT-REF_TABLE  = 'LIKP'.
      W_FIELDCAT-FIX_COLUMN = 'X'.
      W_FIELDCAT-COLTEXT    = 'Delivery Date'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(02) Route
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'ROUTE'.
      W_FIELDCAT-REF_FIELD = 'ROUTE'.
      W_FIELDCAT-REF_TABLE = 'LIKP'.
      W_FIELDCAT-COLTEXT   = 'Route'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(03) Loading Point
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'LSTEL'.
      W_FIELDCAT-REF_FIELD = 'LSTEL'.
      W_FIELDCAT-REF_TABLE = 'LIKP'.
      W_FIELDCAT-COLTEXT   = 'Loading Point'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(04) Ship-to
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'KUNNR'.
      W_FIELDCAT-REF_FIELD = 'KUNNR'.
      W_FIELDCAT-REF_TABLE = 'LIKP'.
      W_FIELDCAT-COLTEXT   = 'Ship-to'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(05) Full Name
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'NAME1'.
      W_FIELDCAT-INTTYPE   = 'C'.
      W_FIELDCAT-INTLEN    = 80.
      W_FIELDCAT-COLTEXT   = 'Full Name'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(**) Sales Office
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'VKBUR'.
      W_FIELDCAT-REF_FIELD = 'VKBUR'.
      W_FIELDCAT-REF_TABLE = 'TVKB'.
      W_FIELDCAT-NO_OUT    = 'X'.
      W_FIELDCAT-COLTEXT   = 'Sales Office'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(06) Sales Office
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'VKBURT'.
      W_FIELDCAT-REF_FIELD = 'BEZEI'.
      W_FIELDCAT-REF_TABLE = 'TVKBT'.
      W_FIELDCAT-COLTEXT   = 'Sales Office'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(07) Request remark
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'REMARK'.
      W_FIELDCAT-INTTYPE   = 'C'.
      W_FIELDCAT-INTLEN    = 512.
      W_FIELDCAT-COLTEXT   = 'Request remark'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(08) City
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'CITY1'.
      W_FIELDCAT-REF_FIELD = 'CITY1'.
      W_FIELDCAT-REF_TABLE = 'ADRC'.
      W_FIELDCAT-COLTEXT   = 'City'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(09) Address
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'ADDRS'.
      W_FIELDCAT-INTTYPE   = 'C'.
      W_FIELDCAT-INTLEN    = 200.
      W_FIELDCAT-COLTEXT   = 'Address'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(10) Delivery
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'VBELN'.
      W_FIELDCAT-REF_FIELD = 'VBELN'.
      W_FIELDCAT-REF_TABLE = 'LIKP'.
      W_FIELDCAT-COLTEXT   = 'Delivery'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(11) Item
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'POSNR'.
      W_FIELDCAT-REF_FIELD = 'POSNR'.
      W_FIELDCAT-REF_TABLE = 'LIPS'.
      W_FIELDCAT-COLTEXT   = 'Item'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(12) Material
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'MATNR'.
      W_FIELDCAT-REF_FIELD = 'MATNR'.
      W_FIELDCAT-REF_TABLE = 'LIPS'.
      W_FIELDCAT-COLTEXT   = 'Material'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(13) Delivery Qty
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'LFIMG'.
      W_FIELDCAT-REF_FIELD  = 'LFIMG'.
      W_FIELDCAT-REF_TABLE  = 'LIPS'.
      W_FIELDCAT-QFIELDNAME = 'VRKME'.
      W_FIELDCAT-DO_SUM     = 'X'.
      W_FIELDCAT-COLTEXT    = 'Delivery Qty'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(**) Unit
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'VRKME'.
      W_FIELDCAT-REF_FIELD = 'VRKME'.
      W_FIELDCAT-REF_TABLE = 'LIPS'.
      W_FIELDCAT-COLTEXT   = 'Unit'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(14) SLoc
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'LGORT'.
      W_FIELDCAT-REF_FIELD = 'LGORT'.
      W_FIELDCAT-REF_TABLE = 'LIPS'.
      W_FIELDCAT-COLTEXT   = 'SLoc'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(**) Dimension
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'GROES'.
      W_FIELDCAT-REF_FIELD = 'GROES'.
      W_FIELDCAT-REF_TABLE = 'MARA'.
      W_FIELDCAT-COLTEXT   = 'Dimension'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(15) Total weight
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'BRGEW'.
      W_FIELDCAT-REF_FIELD  = 'BRGEW'.
      W_FIELDCAT-REF_TABLE  = 'LIPS'.
      W_FIELDCAT-QFIELDNAME = 'GEWEI'.
      W_FIELDCAT-DO_SUM     = 'X'.
      W_FIELDCAT-COLTEXT    = 'Total weight'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(16) Remaining weight
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'REWEI'.
      W_FIELDCAT-REF_FIELD  = 'BRGEW'.
      W_FIELDCAT-REF_TABLE  = 'LIPS'.
      W_FIELDCAT-QFIELDNAME = 'GEWEI'.
      W_FIELDCAT-DO_SUM     = 'X'.
      W_FIELDCAT-COLTEXT    = 'Remaining weight'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(**) Weight Unit
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'GEWEI'.
      W_FIELDCAT-REF_FIELD = 'GEWEI'.
      W_FIELDCAT-REF_TABLE = 'LIPS'.
      W_FIELDCAT-COLTEXT   = 'Weight Unit'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(17) Total M3
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'VOLUM'.
      W_FIELDCAT-REF_FIELD  = 'VOLUM'.
      W_FIELDCAT-REF_TABLE  = 'LIPS'.
      W_FIELDCAT-QFIELDNAME = 'VOLEH'.
      W_FIELDCAT-DO_SUM     = 'X'.
      W_FIELDCAT-COLTEXT    = 'Total M3'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(18) Remaining M3
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'REM3'.
      W_FIELDCAT-REF_FIELD  = 'VOLUM'.
      W_FIELDCAT-REF_TABLE  = 'LIPS'.
      W_FIELDCAT-QFIELDNAME = 'VOLEH'.
      W_FIELDCAT-DO_SUM     = 'X'.
      W_FIELDCAT-COLTEXT    = 'Remaining M3'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(**) Wun Unit
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'VOLEH'.
      W_FIELDCAT-REF_FIELD = 'VOLEH'.
      W_FIELDCAT-REF_TABLE = 'LIPS'.
      W_FIELDCAT-COLTEXT   = 'Wun Unit'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(19) Shipped Qty
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'SHQTY'.
      W_FIELDCAT-REF_FIELD  = 'LFIMG'.
      W_FIELDCAT-REF_TABLE  = 'LIPS'.
      W_FIELDCAT-QFIELDNAME = 'VRKME'.
      W_FIELDCAT-DO_SUM     = 'X'.
      W_FIELDCAT-COLTEXT    = 'Shipped Qty'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(20) Remaining Load
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'RELOAD'.
      W_FIELDCAT-REF_FIELD  = 'LFIMG'.
      W_FIELDCAT-REF_TABLE  = 'LIPS'.
      W_FIELDCAT-QFIELDNAME = 'VRKME'.
      W_FIELDCAT-DO_SUM     = 'X'.
      W_FIELDCAT-COLTEXT    = 'Remaining Load'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(21) Load Qty
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'LOAD'.
      W_FIELDCAT-REF_FIELD  = 'LFIMG'.
      W_FIELDCAT-REF_TABLE  = 'LIPS'.
      W_FIELDCAT-QFIELDNAME = 'VRKME'.
      W_FIELDCAT-DO_SUM     = 'X'.
      W_FIELDCAT-EDIT       = 'X'.
      W_FIELDCAT-COLTEXT    = 'Load Qty'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(**) Message
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'MSG'.
      W_FIELDCAT-INTTYPE   = 'C'.
      W_FIELDCAT-INTLEN    = 100.
      W_FIELDCAT-COLTEXT   = 'Message'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

    WHEN P_COMPT.
*// ->(26) Shipment No.
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'TKNUM'.
      W_FIELDCAT-REF_FIELD  = 'TKNUM'.
      W_FIELDCAT-REF_TABLE  = 'VTTK'.
      W_FIELDCAT-FIX_COLUMN = 'X'.
      W_FIELDCAT-COLTEXT    = 'Shipment No.'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(27) Delivery by
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'DELBY'.
      W_FIELDCAT-INTTYPE   = 'C'.
      W_FIELDCAT-INTLEN    = 80.
      W_FIELDCAT-COLTEXT   = 'Delivery by'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(28) Date Execute
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'DTDIS'.
      W_FIELDCAT-REF_FIELD = 'DTDIS'.
      W_FIELDCAT-REF_TABLE = 'VTTK'.
      W_FIELDCAT-COLTEXT   = 'Date Execute'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(**) Shipment End
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'DATEN'.
      W_FIELDCAT-REF_FIELD = 'DATEN'.
      W_FIELDCAT-REF_TABLE = 'VTTK'.
      W_FIELDCAT-COLTEXT   = 'Shipment End'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(29) Driver Name
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'EXTI2'.
      W_FIELDCAT-REF_FIELD = 'EXTI2'.
      W_FIELDCAT-REF_TABLE = 'VTTK'.
      W_FIELDCAT-COLTEXT   = 'Driver Name'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(30) Truck No.
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'EXTI1'.
      W_FIELDCAT-REF_FIELD = 'EXTI1'.
      W_FIELDCAT-REF_TABLE = 'VTTK'.
      W_FIELDCAT-COLTEXT   = 'Truck No.'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(31) Manpower
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'MANPO'.
      W_FIELDCAT-INTTYPE   = 'C'.
      W_FIELDCAT-INTLEN    = 200.
      W_FIELDCAT-COLTEXT   = 'Manpower'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(32) Others
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'OTHER'.
      W_FIELDCAT-INTTYPE   = 'C'.
      W_FIELDCAT-INTLEN    = 200.
      W_FIELDCAT-COLTEXT   = 'Others'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(33) Truck Type
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'VSARTT'.
      W_FIELDCAT-INTTYPE   = 'C'.
      W_FIELDCAT-INTLEN    = 25.
      W_FIELDCAT-COLTEXT   = 'Truck Type'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(34) Zone ต้นทาง
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'VSBEDT'.
      W_FIELDCAT-INTTYPE   = 'C'.
      W_FIELDCAT-INTLEN    = 25.
      W_FIELDCAT-COLTEXT   = 'Zone ต้นทาง'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(35) Zone ปลายทาง
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'ROUTET'.
      W_FIELDCAT-INTTYPE   = 'C'.
      W_FIELDCAT-INTLEN    = 50.
      W_FIELDCAT-COLTEXT   = 'Zone ปลายทาง'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(36) Contact person/Tel. No.
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'CONTA'.
      W_FIELDCAT-INTTYPE   = 'C'.
      W_FIELDCAT-INTLEN    = 200.
      W_FIELDCAT-COLTEXT   = 'Contact person/Tel. No.'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(37) Delivery
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'VBELN'.
      W_FIELDCAT-REF_FIELD = 'VBELN'.
      W_FIELDCAT-REF_TABLE = 'LIKP'.
      W_FIELDCAT-COLTEXT   = 'Delivery'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(**) Item
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'POSNR'.
      W_FIELDCAT-REF_FIELD = 'POSNR'.
      W_FIELDCAT-REF_TABLE = 'LIPS'.
      W_FIELDCAT-NO_OUT    = 'X'.
      W_FIELDCAT-COLTEXT   = 'Item'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(38) Delivery Date
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'LFDAT'.
      W_FIELDCAT-REF_FIELD = 'LFDAT'.
      W_FIELDCAT-REF_TABLE = 'LIKP'.
      W_FIELDCAT-COLTEXT   = 'Delivery Date'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(39) Customer name
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'NAME1'.
      W_FIELDCAT-INTTYPE   = 'C'.
      W_FIELDCAT-INTLEN    = 80.
      W_FIELDCAT-COLTEXT   = 'Customer name'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(40) Address
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'ADDRS'.
      W_FIELDCAT-INTTYPE   = 'C'.
      W_FIELDCAT-INTLEN    = 200.
      W_FIELDCAT-COLTEXT   = 'Address'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(**) Request remark
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'REMARK'.
      W_FIELDCAT-INTTYPE   = 'C'.
      W_FIELDCAT-INTLEN    = 512.
      W_FIELDCAT-COLTEXT   = 'Request remark'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(41) Description
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'MATNR'.
      W_FIELDCAT-REF_FIELD = 'MATNR'.
      W_FIELDCAT-REF_TABLE = 'LIPS'.
      W_FIELDCAT-COLTEXT   = 'Description'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(42) SLoc
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'LGORT'.
      W_FIELDCAT-REF_FIELD = 'LGORT'.
      W_FIELDCAT-REF_TABLE = 'LIPS'.
      W_FIELDCAT-COLTEXT   = 'SLoc'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(**) Dimension
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'GROES'.
      W_FIELDCAT-REF_FIELD = 'GROES'.
      W_FIELDCAT-REF_TABLE = 'MARA'.
      W_FIELDCAT-COLTEXT   = 'Dimension'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(43) Quantity
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'LFIMG'.
      W_FIELDCAT-REF_FIELD  = 'LFIMG'.
      W_FIELDCAT-REF_TABLE  = 'LIPS'.
      W_FIELDCAT-QFIELDNAME = 'VRKME'.
      W_FIELDCAT-DO_SUM     = 'X'.
      W_FIELDCAT-COLTEXT    = 'Quantity'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(**) Unit
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'VRKME'.
      W_FIELDCAT-REF_FIELD = 'VRKME'.
      W_FIELDCAT-REF_TABLE = 'LIPS'.
      W_FIELDCAT-NO_OUT    = 'X'.
      W_FIELDCAT-COLTEXT   = 'Unit'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(44) Volume
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'REM3'.
      W_FIELDCAT-REF_FIELD  = 'VOLUM'.
      W_FIELDCAT-REF_TABLE  = 'LIPS'.
      W_FIELDCAT-QFIELDNAME = 'VOLEH'.
      W_FIELDCAT-DO_SUM     = 'X'.
      W_FIELDCAT-COLTEXT    = 'Volume'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(**) Volumn Unit
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'VOLEH'.
      W_FIELDCAT-REF_FIELD = 'VOLEH'.
      W_FIELDCAT-REF_TABLE = 'LIPS'.
      W_FIELDCAT-NO_OUT    = 'X'.
      W_FIELDCAT-COLTEXT   = 'Volumn Unit'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(45) Weight (KG)
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'REWEI'.
      W_FIELDCAT-REF_FIELD  = 'BRGEW'.
      W_FIELDCAT-REF_TABLE  = 'LIPS'.
      W_FIELDCAT-QFIELDNAME = 'GEWEI'.
      W_FIELDCAT-DO_SUM     = 'X'.
      W_FIELDCAT-COLTEXT    = 'Weight (KG)'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

*// ->(**) Weight Unit
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS   = L_POS.
      W_FIELDCAT-FIELDNAME = 'GEWEI'.
      W_FIELDCAT-REF_FIELD = 'GEWEI'.
      W_FIELDCAT-REF_TABLE = 'LIPS'.
      W_FIELDCAT-NO_OUT    = 'X'.
      W_FIELDCAT-COLTEXT   = 'Weight Unit'.
      W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

    WHEN P_SHPLST.
*//(20) Distribution Channel Name
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'VTEXT'.
      W_FIELDCAT-REF_FIELD  = 'VTEXT'.
      W_FIELDCAT-REF_TABLE  = 'TVTWT'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Channel Division Text'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(06) Sales Office Name
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'VKBURT'.
      W_FIELDCAT-REF_FIELD  = 'BEZEI'.
      W_FIELDCAT-REF_TABLE  = 'TVKBT'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Sales Office'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(37) Delivery
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'VBELN'.
      W_FIELDCAT-REF_FIELD  = 'VBELN'.
      W_FIELDCAT-REF_TABLE  = 'LIKP'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'D/O Number'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(26) Shipment No.
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'TKNUM'.
      W_FIELDCAT-REF_FIELD  = 'TKNUM'.
      W_FIELDCAT-REF_TABLE  = 'VTTK'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Shipment'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(10) S/O Document number of the reference document
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'VGBEL'.
      W_FIELDCAT-REF_FIELD  = 'VGBEL'.
      W_FIELDCAT-REF_TABLE  = 'LIPS'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'S/O'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(80) Customer Name(adrc name1 + name2)
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'CUSNAME'.
*      w_fieldcat-ref_field  = ''.
*      w_fieldcat-ref_table  = ''.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Customer Name (English)'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(5) Product hierarchy(only digits 1 - 5)
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'CAT'.
      W_FIELDCAT-REF_FIELD  = 'PRODH'.
      W_FIELDCAT-REF_TABLE  = 'LIPS'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Cat.'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(5) Product hierarchy(only digits 6 - 10)
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'CLASS'.
      W_FIELDCAT-REF_FIELD  = 'PRODH'.
      W_FIELDCAT-REF_TABLE  = 'LIPS'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Class'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(18) Material No.
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'MATNR'.
      W_FIELDCAT-REF_FIELD  = 'MATNR'.
      W_FIELDCAT-REF_TABLE  = 'LIPS'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Material'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(3) Unit
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'VRKME'.
      W_FIELDCAT-REF_FIELD  = 'VRKME'.
      W_FIELDCAT-REF_TABLE  = 'LIPS'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'UoM'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.


      "(43) Delivery QTY
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'SHQTY'.
      W_FIELDCAT-REF_FIELD  = 'LFIMG'.
      W_FIELDCAT-REF_TABLE  = 'LIPS'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Delivery Qty'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(43) Shipped Quantity
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'SHIPPED'.
      W_FIELDCAT-REF_FIELD  = 'LFIMG'.
      W_FIELDCAT-REF_TABLE  = 'ZSDSSDT013'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Shipped'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.


      "(512) Request remark
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'REMARK'.
*      w_fieldcat-ref_field  = ''.
*      w_fieldcat-ref_table  = ''.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Request remark'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(20) Loading Point Text
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'LOADTEXT'.
      W_FIELDCAT-REF_FIELD  = 'VTEXT'.
      W_FIELDCAT-REF_TABLE  = 'TVLAT'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Loading Point Desc.'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(10) Transport
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'TDLNR'.
      W_FIELDCAT-REF_FIELD  = 'TDLNR'.
      W_FIELDCAT-REF_TABLE  = 'VTTK'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Transport'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.


      "(4) SLoc  ...
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'LGORT'.
      W_FIELDCAT-REF_FIELD  = 'LGORT'.
      W_FIELDCAT-REF_TABLE  = 'LIPS'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Storage Location'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(20)External ID 1
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'EXTI1'.
      W_FIELDCAT-REF_FIELD  = 'EXTI1'.
      W_FIELDCAT-REF_TABLE  = 'VTTK'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'External ID 1'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(20)Truck driver
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'EXTI2'.
      W_FIELDCAT-REF_FIELD  = 'EXTI2'.
      W_FIELDCAT-REF_TABLE  = 'VTTK'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Truck driver'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(2)Shipping type
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'VSART'.
      W_FIELDCAT-REF_FIELD  = 'VSART'.
      W_FIELDCAT-REF_TABLE  = 'VTTK'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Shipping type'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(15/3) (LIPS-VOLUM / LIPS-LFIMG) * Shipped.
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'M3'.
      W_FIELDCAT-REF_FIELD  = 'VOLUM'.
      W_FIELDCAT-REF_TABLE  = 'LIPS'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'M3'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(40)Ship-to City
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'SHPTOCITY'.
      W_FIELDCAT-REF_FIELD  = 'CITY1'.
      W_FIELDCAT-REF_TABLE  = 'ADRC'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Ship-to City'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(40)Ship-to Location
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'SOLDTOLOC'.
      W_FIELDCAT-REF_FIELD  = 'CITY1'.
      W_FIELDCAT-REF_TABLE  = 'ADRC'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Sold-to location'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(140)Ship-to Address
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'SHPTOADD'.
*      w_fieldcat-ref_field  = ''.
*      w_fieldcat-ref_table  = ''.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Ship-to Address'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(140)Sold-to Address
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'SOLDTOADD'.
*      w_fieldcat-ref_field  = ''.
*      w_fieldcat-ref_table  = ''.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Sold-to Address'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(38) Delivery Date
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'LFDAT'.
      W_FIELDCAT-REF_FIELD  = 'LFDAT'.
      W_FIELDCAT-REF_TABLE  = 'LIKP'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Delivery Date'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(8)Date Execute
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'DTDIS'.
      W_FIELDCAT-REF_FIELD  = 'DTDIS'.
      W_FIELDCAT-REF_TABLE  = 'VTTK'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Date Execute'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(8)Create on
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'ERDAT'.
      W_FIELDCAT-REF_FIELD  = 'ERDAT'.
      W_FIELDCAT-REF_TABLE  = 'VTTK'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Create on'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(8)Shipment start
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'DATBG'.
      W_FIELDCAT-REF_FIELD  = 'DATBG'.
      W_FIELDCAT-REF_TABLE  = 'VTTK'.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Shipment start'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(512) Man power
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'MANPOW'.
*      w_fieldcat-ref_field  = ''.
*      w_fieldcat-ref_table  = ''.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Manpower'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.

      "(512) Other
      CLEAR W_FIELDCAT.
      ADD 1 TO L_POS.
      W_FIELDCAT-COL_POS    = L_POS.
      W_FIELDCAT-FIELDNAME  = 'OTHER'.
*      w_fieldcat-ref_field  = ''.
*      w_fieldcat-ref_table  = ''.
      W_FIELDCAT-FIX_COLUMN = ''.
      W_FIELDCAT-COLTEXT    = 'Other'.
      W_FIELDCAT-SCRTEXT_L  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_M  = W_FIELDCAT-COLTEXT.
      W_FIELDCAT-SCRTEXT_S  = W_FIELDCAT-COLTEXT.
      APPEND W_FIELDCAT TO T_FIELDCAT.
  ENDCASE.

  "beg+++ iBS 05.05.2017 10:20:56 CS-SDS-0001

*// -> Shipping type
  CLEAR W_FIELDCAT.
  ADD 1 TO L_POS.
  W_FIELDCAT-COL_POS   = L_POS.
  W_FIELDCAT-FIELDNAME = 'VSART'.
  W_FIELDCAT-INTTYPE   = 'C'.
  W_FIELDCAT-INTLEN    = 2.
  W_FIELDCAT-JUST      = 'C'  .
  W_FIELDCAT-COLTEXT   = 'Truck Type code'.
  W_FIELDCAT-SCRTEXT_L = W_FIELDCAT-COLTEXT.
  W_FIELDCAT-SCRTEXT_M = W_FIELDCAT-COLTEXT.
  W_FIELDCAT-SCRTEXT_S = W_FIELDCAT-COLTEXT.
  APPEND W_FIELDCAT TO T_FIELDCAT.
  "end+++ iBS 05.05.2017 10:20:56 CS-SDS-0001

ENDFORM.                    "fieldcat_init

*&---------------------------------------------------------------------*
*&      Form  SORT_INIT
*&---------------------------------------------------------------------*
FORM SORT_INIT USING PT_SORT TYPE LVC_T_SORT.

  REFRESH PT_SORT.

  CASE 'X'.
    WHEN P_NOCOM.

    WHEN P_COMPT.
      CLEAR W_SORT.
      W_SORT-SPOS      = 1.
      W_SORT-FIELDNAME = 'TKNUM'.
      W_SORT-UP        = 'X'.
      APPEND W_SORT TO PT_SORT.

      CLEAR W_SORT.
      W_SORT-SPOS      = 2.
      W_SORT-FIELDNAME = 'VBELN'.
      W_SORT-UP        = 'X'.
      APPEND W_SORT TO PT_SORT.

      CLEAR W_SORT.
      W_SORT-SPOS      = 23.
      W_SORT-FIELDNAME = 'POSNR'.
      W_SORT-UP        = 'X'.
      APPEND W_SORT TO PT_SORT.


  ENDCASE.

ENDFORM.                    "sort_init

*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
FORM EXCLUDE_TB_FUNCTIONS CHANGING PT_EXCLUDE TYPE UI_FUNCTIONS.
  DATA LS_EXCLUDE TYPE UI_FUNC.

  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_GRAPH.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_HELP.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_INFO.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_CHECK.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_PRINT.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_DETAIL.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.

ENDFORM.                    "exclude_tb_functions

*&---------------------------------------------------------------------*
*&      Form  SELECT_OR_NOT_SELECT
*&---------------------------------------------------------------------*
FORM SELECT_OR_NOT_SELECT TABLES PT_DELIV STRUCTURE W_DELIV
                          USING  VALUE(PL_FLAG).

  LOOP AT PT_DELIV INTO W_DELIV.
    IF ( W_DELIV-SEL NE '-' ).
      MOVE PL_FLAG TO W_DELIV-SEL.

      IF ( W_DELIV-SEL IS NOT INITIAL ).
        W_DELIV-SCOL[] = T_SCOL[].
      ELSE.
        REFRESH W_DELIV-SCOL.
      ENDIF.
    ELSE.
      REFRESH W_DELIV-SCOL.
    ENDIF.

    MODIFY PT_DELIV FROM W_DELIV TRANSPORTING SEL SCOL.
  ENDLOOP.

  CALL METHOD W_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = W_STABLE.

ENDFORM.                    "select_or_not_select

*&---------------------------------------------------------------------*
*&      Form  GET_REQUEST_REMARK
*&---------------------------------------------------------------------*
FORM GET_REQUEST_REMARK USING PL_REMARK.
  DATA: LV_NAME  TYPE THEAD-TDNAME,
        LV_SUBRC TYPE SY-SUBRC.

  MOVE W_LIPS-VBELN TO LV_NAME.
  PERFORM READ_TEXT TABLES T_TLINE
                    USING  'ZH10' 'E' LV_NAME 'VBBK'.

  READ TABLE T_NOTE WITH KEY VBELN = W_LIPS-VBELN.
  MOVE SY-SUBRC TO LV_SUBRC.

  LOOP AT T_TLINE.
    IF ( PL_REMARK IS NOT INITIAL ).
      CONCATENATE PL_REMARK T_TLINE-TDLINE
        INTO PL_REMARK SEPARATED BY SPACE.
    ELSE.
      PL_REMARK = T_TLINE-TDLINE.
    ENDIF.

    CHECK LV_SUBRC NE 0.

    CLEAR T_NOTE.
    T_NOTE-VBELN = W_LIPS-VBELN.
    T_NOTE-LINE  = T_TLINE-TDLINE.
    APPEND T_NOTE.
  ENDLOOP.

ENDFORM.                    " GET_REQUEST_REMARK

*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
FORM READ_TEXT TABLES PT_LINES    STRUCTURE TLINE
               USING  PL_ID       TYPE THEAD-TDID
                      PL_LANGUAGE TYPE THEAD-TDSPRAS
                      PL_NAME     TYPE THEAD-TDNAME
                      PL_OBJECT   TYPE THEAD-TDOBJECT.

  REFRESH PT_LINES.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = PL_ID
      LANGUAGE                = PL_LANGUAGE
      NAME                    = PL_NAME
      OBJECT                  = PL_OBJECT
    TABLES
      LINES                   = PT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

  DELETE PT_LINES WHERE TDLINE IS INITIAL.

ENDFORM.                    " READ_TEXT

*&---------------------------------------------------------------------*
*&      Form  CREATE_SHIPMENT
*&---------------------------------------------------------------------*
FORM CREATE_SHIPMENT.
  DATA LT_SPAGPA TYPE TABLE OF RFC_SPAGPA WITH HEADER LINE.

  DATA: BEGIN OF LT_DELIV OCCURS 0,
          VBELN TYPE LIKP-VBELN,
        END   OF LT_DELIV.

  REFRESH: T_HEADERDEADLINE, T_ITEMDATA, T_STAGEDATA, T_STAGEDEADLINE,
           T_ITEMONSTAGE, T_ADDRESS, T_HDUNHEADER, T_HDUNITEM, T_RETURN,
           T_UPD_SHIP_DOC, LT_DELIV.
  CLEAR:   W_HEADERDEADLINE, W_ITEMDATA, W_STAGEDATA, W_STAGEDEADLINE,
           W_ITEMONSTAGE, W_ADDRESS, W_HDUNHEADER, W_HDUNITEM, W_RETURN,
           W_HEADERDATA, W_TRANSPORT, W_SHIPMENTGUID.

  LOOP AT T_DELIV INTO W_DELIV WHERE SEL EQ 'X'.
    CLEAR W_HEADERDATA.
    W_HEADERDATA-TRANS_PLAN_PT = P_TPLST.
    W_HEADERDATA-SHIPMENT_TYPE = P_SHTYP.

    CLEAR W_ITEMDATA.
    W_ITEMDATA-DELIVERY  = W_DELIV-VBELN.
    W_ITEMDATA-ITENERARY = W_DELIV-POSNR.
    APPEND W_ITEMDATA TO T_ITEMDATA.

    CLEAR T_UPD_SHIP_DOC.
    T_UPD_SHIP_DOC-MANDT = SY-MANDT.
    T_UPD_SHIP_DOC-VBELN = W_DELIV-VBELN.
    T_UPD_SHIP_DOC-POSNR = W_DELIV-POSNR.
    T_UPD_SHIP_DOC-LFIMG = W_DELIV-LOAD.
    APPEND T_UPD_SHIP_DOC.

    LT_DELIV-VBELN = W_DELIV-VBELN.
    COLLECT LT_DELIV.
  ENDLOOP.

  IF ( T_ITEMDATA[] IS NOT INITIAL ).
    CALL FUNCTION 'BAPI_SHIPMENT_CREATE'
      EXPORTING
        HEADERDATA     = W_HEADERDATA
      IMPORTING
        TRANSPORT      = W_TRANSPORT
        SHIPMENTGUID   = W_SHIPMENTGUID
      TABLES
        HEADERDEADLINE = T_HEADERDEADLINE
        ITEMDATA       = T_ITEMDATA
        STAGEDATA      = T_STAGEDATA
        STAGEDEADLINE  = T_STAGEDEADLINE
        ITEMONSTAGE    = T_ITEMONSTAGE
        ADDRESS        = T_ADDRESS
        HDUNHEADER     = T_HDUNHEADER
        HDUNITEM       = T_HDUNITEM
        RETURN         = T_RETURN.

    IF ( W_TRANSPORT IS NOT INITIAL ).
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      PERFORM UPDATE_TABLE_SHIPMENT.

      LOOP AT T_DELIV INTO W_DELIV WHERE SEL EQ 'X'.
        W_DELIV-SEL  = '-'.
        W_DELIV-STAT = ICON_GREEN_LIGHT.
        MODIFY T_DELIV FROM W_DELIV.
      ENDLOOP.

      LOOP AT LT_DELIV.
        PERFORM DEQUEUE_DELIVERY USING LT_DELIV-VBELN.
      ENDLOOP.

      CLEAR LT_SPAGPA.
      LT_SPAGPA-PARID  = 'TNR'.
      LT_SPAGPA-PARVAL = W_TRANSPORT.
      APPEND LT_SPAGPA.

      CALL FUNCTION 'ABAP4_CALL_TRANSACTION' STARTING NEW TASK 'VT02N'
        EXPORTING
          TCODE                   = C_TCODE
          SKIP_SCREEN             = 'X'
        TABLES
          SPAGPA_TAB              = LT_SPAGPA
        EXCEPTIONS
          CALL_TRANSACTION_DENIED = 1
          TCODE_INVALID           = 2
          OTHERS                  = 3.

      MESSAGE S000 WITH 'Shipment' W_TRANSPORT 'has been saved'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.
  ENDIF.

ENDFORM.                    " CREATE_SHIPMENT

*&---------------------------------------------------------------------*
*&      Form  CHECK_SHIPMENT_ITEM
*&---------------------------------------------------------------------*
FORM CHECK_SHIPMENT_ITEM USING PL_SUBRC TYPE SY-SUBRC.
  DATA LV_TKNUM TYPE VTTK-TKNUM.

  REFRESH T_VTTK.
  CLEAR W_LFIMG.
  MOVE 4 TO PL_SUBRC.

  LOOP AT t_ZSDSSDT013 WHERE VBELN EQ W_LIPS-VBELN
                           AND POSNR EQ W_LIPS-POSNR.
    CLEAR W_VTTK.

    SELECT SINGLE TKNUM DTDIS EXTI2 EXTI1 VSART
                  VSBED ROUTE TDLNR DATEN
    INTO (W_VTTK-TKNUM, W_VTTK-DTDIS, W_VTTK-EXTI2,
          W_VTTK-EXTI1, W_VTTK-VSART, W_VTTK-VSBED,
          W_VTTK-ROUTE, W_VTTK-TDLNR, W_VTTK-DATEN)
    FROM VTTK
    WHERE TKNUM EQ t_ZSDSSDT013-TKNUM.

    CHECK SY-SUBRC EQ 0.
    ADD t_ZSDSSDT013-LFIMG TO W_LFIMG.
    MOVE t_ZSDSSDT013-LFIMG TO W_VTTK-LFIMG.
    COLLECT W_VTTK INTO T_VTTK.
  ENDLOOP.

  CASE 'X'.
    WHEN P_NOCOM.
      IF ( W_LIPS-LFIMG GT W_LFIMG ).
        CLEAR PL_SUBRC.
      ENDIF.

    WHEN P_COMPT.
      IF ( W_LIPS-LFIMG EQ W_LFIMG ).
        CLEAR PL_SUBRC.
      ENDIF.
  ENDCASE.

ENDFORM.                    " CHECK_SHIPMENT_ITEM

*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE_SHIPMENT
*&---------------------------------------------------------------------*
FORM UPDATE_TABLE_SHIPMENT.

  CHECK T_UPD_SHIP_DOC[] IS NOT INITIAL.

  LOOP AT T_UPD_SHIP_DOC.
    T_UPD_SHIP_DOC-TKNUM = W_TRANSPORT.
    MODIFY T_UPD_SHIP_DOC.
  ENDLOOP.

  CALL FUNCTION 'Z_SDSSD_UPDATE_ZSD_SHIP_DOC'
    TABLES
      pt_ZSDSSDT013 = T_UPD_SHIP_DOC.

  COMMIT WORK.

ENDFORM.                    " UPDATE_TABLE_SHIPMENT

*&---------------------------------------------------------------------*
*&      Form  ENQUEUE_DELIVERY
*&---------------------------------------------------------------------*
FORM ENQUEUE_DELIVERY USING PL_VBELN TYPE LIKP-VBELN.
  DATA: LT_ENQ  TYPE TABLE OF SEQG7 WITH HEADER LINE,
        LV_GARG TYPE SEQG3-GARG.

  SORT T_LOCKED BY VBELN.
  READ TABLE T_LOCKED WITH KEY VBELN = PL_VBELN BINARY SEARCH.
  CHECK SY-SUBRC NE 0.

  CALL FUNCTION 'ENQUEUE_EVVBLKE'
    EXPORTING
      VBELN          = PL_VBELN
    EXCEPTIONS
      FOREIGN_LOCK   = 1
      SYSTEM_FAILURE = 2
      OTHERS         = 3.

  IF ( SY-SUBRC EQ 0 ).
    T_LOCKED-VBELN = PL_VBELN.
    APPEND T_LOCKED.
  ELSE.
    W_DELIV-SEL  = '-'.
    W_DELIV-STAT = ICON_LOCKED.

    CONCATENATE SY-MANDT W_LIPS-VBELN INTO LV_GARG.

    CALL FUNCTION 'ENQUE_READ2'
      EXPORTING
        GNAME = 'LIKP'
        GARG  = LV_GARG
      TABLES
        ENQ   = LT_ENQ.

    READ TABLE LT_ENQ INDEX 1.
    IF ( SY-SUBRC EQ 0 ).
      CONCATENATE 'Locked by' LT_ENQ-GUNAME
        INTO W_DELIV-MSG SEPARATED BY SPACE.
    ENDIF.
  ENDIF.

ENDFORM.                    " ENQUEUE_DELIVERY

*&---------------------------------------------------------------------*
*&      Form  DEQUEUE_DELIVERY
*&---------------------------------------------------------------------*
FORM DEQUEUE_DELIVERY USING PL_VBELN TYPE LIKP-VBELN.

  CALL FUNCTION 'DEQUEUE_EVVBLKE'
    EXPORTING
      VBELN = PL_VBELN.

ENDFORM.                    " DEQUEUE_DELIVERY

*&---------------------------------------------------------------------*
*&      Form  GET_MANPOWER
*&---------------------------------------------------------------------*
FORM GET_MANPOWER.
  DATA LV_NAME TYPE THEAD-TDNAME.

  MOVE W_VTTK-TKNUM TO LV_NAME.
  PERFORM READ_TEXT TABLES T_TLINE
                    USING  '0007' 'E' LV_NAME 'VTTK'.

  LOOP AT T_TLINE.
    IF ( W_SHIPM-MANPO IS NOT INITIAL ).
      CONCATENATE W_SHIPM-MANPO T_TLINE-TDLINE
        INTO W_SHIPM-MANPO SEPARATED BY SPACE.
    ELSE.
      W_SHIPM-MANPO = T_TLINE-TDLINE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_MANPOWER

*&---------------------------------------------------------------------*
*&      Form  GET_OTHERS
*&---------------------------------------------------------------------*
FORM GET_OTHERS.
  DATA LV_NAME TYPE THEAD-TDNAME.

  MOVE W_VTTK-TKNUM TO LV_NAME.
  PERFORM READ_TEXT TABLES T_TLINE
                    USING  '0008' 'E' LV_NAME 'VTTK'.

  LOOP AT T_TLINE.
    IF ( W_SHIPM-OTHER IS NOT INITIAL ).
      CONCATENATE W_SHIPM-OTHER T_TLINE-TDLINE
        INTO W_SHIPM-OTHER SEPARATED BY SPACE.
    ELSE.
      W_SHIPM-OTHER = T_TLINE-TDLINE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_OTHERS

*&---------------------------------------------------------------------*
*&      Form  GET_CONTRACT
*&---------------------------------------------------------------------*
FORM GET_CONTRACT.
  DATA LV_NAME TYPE THEAD-TDNAME.

  MOVE W_VTTK-TKNUM TO LV_NAME.
  PERFORM READ_TEXT TABLES T_TLINE
                    USING  '0009' 'E' LV_NAME 'VTTK'.

  LOOP AT T_TLINE.
    IF ( W_SHIPM-CONTA IS NOT INITIAL ).
      CONCATENATE W_SHIPM-CONTA T_TLINE-TDLINE
        INTO W_SHIPM-CONTA SEPARATED BY SPACE.
    ELSE.
      W_SHIPM-CONTA = T_TLINE-TDLINE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_CONTRACT

*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
  DATA LV_DATUM TYPE SY-DATUM.

*// Default 1000 to Shipping Point
  M%APPEND_RANGE S_VSTEL 'I' 'EQ' '1000' ''.

*// Default (Current Date - 4) to Current Date
  LV_DATUM = SY-DATUM + 4.
  M%APPEND_RANGE S_LFDAT 'I' 'BT' SY-DATUM LV_DATUM.

*// Default Z2 to Loading Point
  M%APPEND_RANGE S_LSTEL 'I' 'EQ' 'Z2' ''.

ENDFORM.                    " INITIALIZATION

*&---------------------------------------------------------------------*
*&      Form  POPUP_REMARK
*&---------------------------------------------------------------------*
FORM POPUP_REMARK USING PL_VBELN TYPE LIKP-VBELN.
  DATA LT_NOTE TYPE TABLE OF TXW_NOTE WITH HEADER LINE.

  REFRESH LT_NOTE.

  LOOP AT T_NOTE WHERE VBELN EQ PL_VBELN.
    CLEAR LT_NOTE.
    LT_NOTE-LINE = T_NOTE-LINE.
    APPEND LT_NOTE.
  ENDLOOP.

  CHECK LT_NOTE[] IS NOT INITIAL.

  CALL FUNCTION 'TXW_TEXTNOTE_EDIT'
    EXPORTING
      EDIT_MODE = ' '
    TABLES
      T_TXWNOTE = LT_NOTE.

ENDFORM.                    " POPUP_REMARK
*&---------------------------------------------------------------------*
*&      Form  READ_MAIN_DATA_SHIPMENT_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_MAIN_DATA_SHIPMENT_LIST .
  DATA: BEGIN OF LT_MARA OCCURS 0,
          MATNR TYPE MARA-MATNR,
        END   OF LT_MARA.

  DATA: BEGIN OF LT_TVKB OCCURS 0,
          VKBUR TYPE TVBUR-VKBUR,
        END   OF LT_TVKB,
        BEGIN OF lt_ZSDSSDT013_key OCCURS 0 ,
          VBELN TYPE LIPS-VBELN,
          POSNR TYPE LIPS-POSNR,
        END OF lt_ZSDSSDT013_key.

  REFRESH: T_LIPS, T_MARA, T_VBPA, T_TVKBT, t_ZSDSSDT013, T_NOTE,lt_ZSDSSDT013_key.

*// Shipment document Item
  SELECT TKNUM TDLNR EXTI1 EXTI2 VSART DTDIS ERDAT DATBG
    INTO CORRESPONDING FIELDS OF TABLE T_VTTK
      FROM VTTK
      WHERE DATBG IN S_SHPBEG
        AND ERDAT IN S_SHPENT.
  IF SY-SUBRC = 0.
    SELECT *
    INTO TABLE t_ZSDSSDT013
    FROM ZSDSSDT013 FOR ALL ENTRIES IN T_VTTK
    WHERE TKNUM EQ T_VTTK-TKNUM.
  ENDIF.

  IF ( t_ZSDSSDT013[] IS NOT INITIAL ).
*// Delivery Order
    SELECT B~VBELN B~POSNR A~VSTEL A~LFDAT A~LSTEL
           B~LFIMG B~VRKME A~ROUTE B~MATNR B~LGORT
           B~BRGEW B~VOLUM A~KUNNR B~GEWEI B~VOLEH
           B~VKBUR B~VTWEG B~VGBEL B~PRODH
    INTO TABLE T_LIPS
    FROM LIKP AS A
      INNER JOIN LIPS AS B
         ON B~VBELN EQ A~VBELN
      FOR ALL ENTRIES IN t_ZSDSSDT013
    WHERE A~VBELN EQ t_ZSDSSDT013-VBELN
      AND B~POSNR EQ t_ZSDSSDT013-POSNR
      AND A~VBELN IN S_VBELN
      AND A~VSTEL IN S_VSTEL
      AND A~LFDAT IN S_DLIDAT
      AND A~LSTEL IN S_LSTEL.
  ENDIF.

  CHECK T_LIPS[] IS NOT INITIAL.

  LOOP AT T_LIPS INTO W_LIPS.
    LT_MARA-MATNR = W_LIPS-MATNR.
    COLLECT LT_MARA.

    LT_TVKB-VKBUR = W_LIPS-VKBUR.
    COLLECT LT_TVKB.

    lt_ZSDSSDT013_key-VBELN = W_LIPS-VBELN.
    lt_ZSDSSDT013_key-POSNR = W_LIPS-POSNR.
    APPEND lt_ZSDSSDT013_key.
  ENDLOOP.

  SORT lt_ZSDSSDT013_key.
  DELETE ADJACENT DUPLICATES FROM lt_ZSDSSDT013_key.

*// Division Channel Description
  SELECT * INTO TABLE T_TVTWT
    FROM TVTWT
      FOR ALL ENTRIES IN T_LIPS
      WHERE SPRAS = 'E'
        AND VTWEG = T_LIPS-VTWEG.

*// Loading Point Description
  SELECT * INTO TABLE T_TVLAT
    FROM TVLAT
      FOR ALL ENTRIES IN T_LIPS
      WHERE SPRAS = 'E'
        AND LSTEL = T_LIPS-LSTEL
        AND VSTEL = T_LIPS-VSTEL.

*// Ship-to , Sold-to
  SELECT A~VBELN A~KUNNR A~ADRNR B~NAME1
         B~NAME2 B~CITY1 B~CITY2 B~STREET A~PARVW B~NATION
  INTO TABLE T_VBPA
  FROM VBPA AS A
    INNER JOIN ADRC AS B
       ON B~ADDRNUMBER EQ A~ADRNR
    FOR ALL ENTRIES IN T_LIPS
  WHERE A~VBELN     EQ T_LIPS-VBELN
    AND A~POSNR     EQ '000000'
    AND A~PARVW     IN ('WE' , 'AG') " WE = Ship-to,AG = Sold-to
    AND B~DATE_FROM LE SY-DATUM.

  SORT T_VBPA BY VBELN.
  DELETE ADJACENT DUPLICATES FROM T_VBPA COMPARING ALL FIELDS.

*// Material Data
  IF ( LT_MARA[] IS NOT INITIAL ).
    SELECT MATNR GROES
    INTO TABLE T_MARA
    FROM MARA FOR ALL ENTRIES IN LT_MARA
    WHERE MATNR EQ LT_MARA-MATNR.
  ENDIF.

*// Sales Offices
  IF ( LT_TVKB[] IS NOT INITIAL ).
    SELECT *
    INTO TABLE T_TVKBT
    FROM TVKBT FOR ALL ENTRIES IN LT_TVKB
    WHERE SPRAS EQ 'E'
      AND VKBUR EQ LT_TVKB-VKBUR.

    SORT T_TVKBT BY SPRAS VKBUR.
  ENDIF.

ENDFORM.                    " READ_MAIN_DATA_SHIPMENT_LIST
