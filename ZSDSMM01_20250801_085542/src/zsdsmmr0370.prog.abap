*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0370
*  Creation Date      : 08.06.2024
*  Author             : Jakarin S.
*  Add-on ID          :
*  Description        : Material List Compare Product Hierarchy
*  Purpose            :
*  Copied from        : ZR_MM_MAT_LIST
*  Restriction        :
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*  11.07.2025  F36K921059 Boonpipop Ch(SDS) Add fields
*-----------------------------------------------------------------------

REPORT ZSDSMMR0370 NO STANDARD PAGE HEADING
                   MESSAGE-ID ZSDSMM01.

RANGES : GR_OBJEK FOR AUSP-OBJEK.

INCLUDE ZSDSMMR0370_TOP.
INCLUDE ZSDSMMR0370_F01.

INITIALIZATION.
  PERFORM F_INITIALIZE_VARIANT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_VARI.
  PERFORM F_F4_FOR_VARIANT.

AT SELECTION-SCREEN.
  PERFORM F_AT_SELECTION_SCREEN.

************************************************************************
*    S T A R T   O F  S E L E C T I O N                                *
************************************************************************
START-OF-SELECTION.
  PERFORM:F_GET_DATA,
          F_CREATE_DYN_TABLE,
          F_MAP_DATA,
          F_ADD_ADDITIONAL_DATA.


* **********************************************************************
*    E N D   O F  S E L E C T I O N                                    *
************************************************************************
END-OF-SELECTION.

  IF <FS_TABLE> IS NOT INITIAL.
    PERFORM F_SHOW_ALV.
  ELSE.
    MESSAGE S003 DISPLAY LIKE 'E'.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  F_ADD_ADDITIONAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ADD_ADDITIONAL_DATA .
  DATA : BEGIN OF LS_AUSP,
    OBJEK TYPE AUSP-OBJEK,
    ATZHL TYPE AUSP-ATZHL,
    ATWRT TYPE AUSP-ATWRT,
    ATINN TYPE AUSP-ATINN,
  END OF LS_AUSP.
  DATA LT_AUSP LIKE TABLE OF LS_AUSP.

  DATA : BEGIN OF LS_MARA,
    MATNR TYPE MARA-MATNR,
    WRKST TYPE MARA-WRKST,
    ZEINR TYPE MARA-ZEINR,
  END OF LS_MARA.
  DATA LT_MARA LIKE TABLE OF LS_MARA.

  DATA : BEGIN OF LS_MARC,
    MATNR TYPE MARA-MATNR,
*    werks TYPE marc-werks,
    MTVFP TYPE MARC-MTVFP,
  END OF LS_MARC.
  DATA LT_MARC LIKE TABLE OF LS_MARC.

  DATA : T_CLASS      TYPE TABLE OF SCLASS,
         T_OBJECTDATA	TYPE TABLE OF CLOBJDAT.

  DATA : LS_OBJECTDATA TYPE CLOBJDAT.

  DATA : LV_MATNR TYPE MARA-MATNR.

  RANGES : LR_ATWTB FOR CAWNT-ATWTB.

  IF GR_OBJEK[] IS NOT INITIAL.
    SELECT AUSP~OBJEK
           AUSP~ATZHL
           AUSP~ATWRT
           AUSP~ATINN
      FROM AUSP
      INTO TABLE LT_AUSP
      WHERE AUSP~OBJEK IN GR_OBJEK
        AND AUSP~ATINN EQ '0000000832'.

    SELECT MATNR
           WRKST
           ZEINR
      FROM MARA
      INTO TABLE LT_MARA
      WHERE MATNR IN GR_OBJEK.

    SELECT MATNR
           MTVFP
      FROM MARC
      INTO TABLE LT_MARC
      WHERE MATNR IN GR_OBJEK.
  ENDIF.

  LOOP AT <FS_TABLE> ASSIGNING <FS_LINE>.

    ASSIGN COMPONENT 'MATNR'  OF STRUCTURE <FS_LINE> TO <FS_FIELD>.

    LV_MATNR = <FS_FIELD>.

    READ TABLE LT_AUSP INTO LS_AUSP
    WITH KEY OBJEK = LV_MATNR.
    IF SY-SUBRC = 0.
      ASSIGN COMPONENT 'ROTAT'  OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      MOVE LS_AUSP-ATWRT TO <FS_FIELD>.
    ENDIF.

    READ TABLE LT_MARA INTO LS_MARA
    WITH KEY MATNR = LV_MATNR.
    IF SY-SUBRC = 0.
      ASSIGN COMPONENT 'WRKST'  OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      MOVE LS_MARA-WRKST TO <FS_FIELD>.

      ASSIGN COMPONENT 'ZEINR'  OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      MOVE LS_MARA-ZEINR TO <FS_FIELD>.

    ENDIF.

    READ TABLE LT_MARC INTO LS_MARC
    WITH KEY MATNR = LV_MATNR.
    IF SY-SUBRC EQ 0.
      ASSIGN COMPONENT 'MTVFP'  OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      MOVE LS_MARC-MTVFP TO <FS_FIELD>.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " F_ADD_ADDITIONAL_DATA
