*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0150_TOP
*  Creation Date      : 03.04.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : N/A
*  Description        : Share Declaration with program and form for
*                       Serial number report
*  Purpose            : Serial number report and form printing
*  Copied from        : ZR_MM_EXPORT_SERIAL
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES : BEGIN OF TS_HEADER,
          XBLNR    TYPE EKES-XBLNR,
          LIEF_NR  TYPE SER01-LIEF_NR,
          ZTRUCKNO TYPE LIKP-ZTRUCKNO,
          LFDAT    TYPE LIKP-LFDAT,
          TOTAL    TYPE I,
        END OF TS_HEADER.
TYPES: TT_HEADER TYPE STANDARD TABLE OF TS_HEADER
                      WITH DEFAULT KEY.

TYPES : BEGIN OF TS_HEADER_DETAIL,
          XBLNR   TYPE EKES-XBLNR,
          LIEF_NR TYPE SER01-LIEF_NR,
          EBELN   TYPE EKPO-EBELN,
          MATNR   TYPE MARA-MATNR,
          TOTAL   TYPE I,
        END OF TS_HEADER_DETAIL.
TYPES: TT_HEADER_DETAIL TYPE STANDARD TABLE OF TS_HEADER_DETAIL.

TYPES : BEGIN OF TS_DETAIL,
          XBLNR   TYPE EKES-XBLNR,
          LIEF_NR TYPE SER01-LIEF_NR,
          EBELN   TYPE EKPO-EBELN,
          MATNR   TYPE MARA-MATNR,
          SERNR   TYPE OBJK-SERNR,
          SER02   TYPE OBJK-SERNR,
          SER03   TYPE OBJK-SERNR,
          SER04   TYPE OBJK-SERNR,
          SER05   TYPE OBJK-SERNR,
          SER06   TYPE OBJK-SERNR,
          SER07   TYPE OBJK-SERNR,
          SER08   TYPE OBJK-SERNR,
          SER09   TYPE OBJK-SERNR,
          SER10   TYPE OBJK-SERNR,
        END OF TS_DETAIL.
TYPES: TT_DETAIL TYPE STANDARD TABLE OF TS_DETAIL.

TYPES : BEGIN OF TS_COUNT_PAGE,
          XBLNR   TYPE EKES-XBLNR,
          LIEF_NR TYPE SER01-LIEF_NR,
          EBELN   TYPE EKPO-EBELN,
          TOTAL   TYPE I,
        END OF TS_COUNT_PAGE.
TYPES: TT_COUNT_PAGE TYPE STANDARD TABLE OF TS_COUNT_PAGE.
