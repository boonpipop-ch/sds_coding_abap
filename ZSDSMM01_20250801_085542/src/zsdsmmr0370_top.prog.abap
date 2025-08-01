*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0370_TOP
*&---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* §5.1 define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
CLASS LCL_HANDLE_EVENTS DEFINITION.
  PUBLIC SECTION.
    METHODS:
      ON_USER_COMMAND FOR EVENT ADDED_FUNCTION OF CL_SALV_EVENTS
        IMPORTING E_SALV_FUNCTION,

      ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_SALV_EVENTS_TABLE
        IMPORTING ROW COLUMN,


      ON_LINK_CLICK FOR EVENT LINK_CLICK OF CL_SALV_EVENTS_TABLE
        IMPORTING ROW COLUMN.
ENDCLASS.                    "lcl_handle_events DEFINITION

*---------------------------------------------------------------------*
* §5.2 implement the events for handling the events of cl_salv_table
*---------------------------------------------------------------------*
CLASS LCL_HANDLE_EVENTS IMPLEMENTATION.
  METHOD ON_USER_COMMAND.

  BREAK-POINT.

  ENDMETHOD.                    "on_user_command

  METHOD ON_DOUBLE_CLICK.

      PERFORM F_CALL_MM03 USING ROW COLUMN.

  ENDMETHOD.                    "on_double_click

  METHOD ON_LINK_CLICK.
*    break-point.
*    perform show_cell_info using 0 row column text-i06.
  ENDMETHOD.                    "on_single_click
ENDCLASS.                    "lcl_handle_events IMPLEMENTATION

*&--------------------------------------------------------------------*
*& Table
*&--------------------------------------------------------------------*
*
TABLES: MARA,MAKT.


*&---------------------------------------------------------------------*
*& TYPES - POOLS
*&---------------------------------------------------------------------*
TYPE-POOLS : VRM,SLIS.
*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*


*--Supoj Add on 02.03.2015

TYPES: BEGIN OF TY_MARM,
       MATNR TYPE MARM-MATNR,
       MEINH TYPE MARM-MEINH,
       LAENG  TYPE MARM-LAENG, "Length
        BREIT  TYPE MARM-BREIT,  " Width
        HOEHE TYPE MARM-HOEHE, "Height
        VOLUM TYPE MARM-VOLUM, "Volumn
        BRGEW TYPE MARM-BRGEW, "Gross Weight
       END OF TY_MARM.
*--End Add

TYPES: BEGIN OF TY_T003P,
       AUART  TYPE T003P-AUART, "Type IO
       TXT    TYPE T003P-TXT,   "Type IO Description
END OF TY_T003P.



TYPES: BEGIN OF TY_AUFK_COSS_H,
      AUFNR          TYPE AUFK-AUFNR,  "IO Number
      KTEXT          TYPE AUFK-KTEXT,  "Description
      USER2          TYPE AUFK-USER2,   "Customer
      KDAUF          TYPE AUFK-KDAUF,  "Quotation no referance
      USER0          TYPE AUFK-USER0,   " Applicant
      USER3          TYPE AUFK-USER3,   " Post Code
      USER6          TYPE AUFK-USER6,   "Sub contegory
      ERNAM          TYPE AUFK-ERNAM,   "Create By
      ERDAT          TYPE AUFK-ERDAT,   "Create Date
      AUART          TYPE AUFK-AUART,   "Type IO
      OBJNR          TYPE AUFK-OBJNR,   "Object Number
      KSTAR          TYPE V_COSS_VIEW-KSTAR,   "Cost Element
      MATNR          TYPE MARA-MATNR,     "csku-ktext,  "material
      MEG001         TYPE V_COSS_VIEW-MEG001,                      "QTY 1
      MEG002         TYPE V_COSS_VIEW-MEG002,                      "QTY 2
      MEG003         TYPE V_COSS_VIEW-MEG003,                      "QTY 3
      MEG004         TYPE V_COSS_VIEW-MEG004,                      "QTY 4
      MEG005         TYPE V_COSS_VIEW-MEG005,                      "QTY 5
      MEG006         TYPE V_COSS_VIEW-MEG006,                      "QTY 6
      MEG007         TYPE V_COSS_VIEW-MEG007,                      "QTY 7
      MEG008         TYPE V_COSS_VIEW-MEG008,                      "QTY 8
      MEG009         TYPE V_COSS_VIEW-MEG009,                      "QTY 9
      MEG010         TYPE V_COSS_VIEW-MEG010,                      "QTY 10
      MEG011         TYPE V_COSS_VIEW-MEG011,                      "QTY 11
      MEG012         TYPE V_COSS_VIEW-MEG012,                      "QTY 12
      MTART          TYPE MARA-MTART,

END OF TY_AUFK_COSS_H.
*

TYPES: BEGIN OF TY_VBAK,
       VBELN TYPE VBAK-VBELN,
       VTWEG TYPE VBAK-VTWEG,
       VKBUR TYPE VBAK-VKBUR,
       KUNNR TYPE VBAK-KUNNR,
      END OF TY_VBAK.


TYPES: BEGIN OF TY_BOM,
       MATNR TYPE MARA-MATNR,
       STLNR TYPE MAST-STLNR,
       BMENG TYPE STKO-BMENG,
       IDNRK TYPE STPO-IDNRK,
       MENGE TYPE STPO-MENGE,
       VERPR TYPE MBEW-VERPR,
      END OF TY_BOM.

*
*types:begin of  ty_konv,
*      knumv  type konv-knumv,
*      kposn  type  konv-kposn,
*      kschl  type  konv-kschl,
*      kbetr  type  konv-kbetr,
*end of ty_konv.
*

TYPES:BEGIN OF  TY_ZSD_FOB_MATERIAL,
       MODEL TYPE ZSDSMMT018-MODEL,
       LNC   TYPE ZSDSMMT018-LNC,
END OF TY_ZSD_FOB_MATERIAL.

*
*
*types:begin of  ty_vbpa,
*       vbeln type vbpa-vbeln,
*       posnr type vbpa-posnr,
*       parvw type vbpa-parvw,
*       kunnr type vbpa-kunnr,
**       pernr type vbpa-pernr,
*       adrnr type vbpa-adrnr,
*       name1 type adrc-name1,
*       name2 type adrc-name2,
*       nation type adrc-nation,
*end of ty_vbpa.
*

*types:begin of  ty_tvaut,
*       augru type vbak-augru,
*       spras type tvaut-spras,
*       bezie type tvaut-bezei,
*end of ty_tvaut.
*
*



TYPES: BEGIN OF TY_LFA1,
       LIFNR TYPE LFA1-LIFNR,
       NAME1 TYPE LFA1-NAME1,
       NAME2 TYPE LFA1-NAME2,
      END OF TY_LFA1.

TYPES: BEGIN OF TY_MVKE,
       MATNR TYPE MVKE-MATNR,
       VKORG TYPE MVKE-VKORG,
       VTWEG TYPE MVKE-VTWEG,
       PRODH TYPE MVKE-PRODH,
       MVGR1 TYPE MVKE-MVGR1,
       MVGR2 TYPE MVKE-MVGR2,
       MVGR3 TYPE MVKE-MVGR3,
       MVGR4 TYPE MVKE-MVGR4,
       MVGR5 TYPE MVKE-MVGR5,
       END OF TY_MVKE.



TYPES: BEGIN OF TY_MARC,
       WERKS TYPE MARC-WERKS,
       MATNR TYPE MARC-MATNR,
       WEBAZ TYPE MARC-WEBAZ,
       SERNP  TYPE MARC-SERNP,
       LADGR  TYPE MARC-LADGR,
       END OF TY_MARC.


TYPES: BEGIN OF TY_MBEW,
       MATNR TYPE MBEW-MATNR,
       BWKEY TYPE MBEW-BWKEY,
       BWTAR TYPE MBEW-BWTAR,
       VERPR TYPE MBEW-VERPR,
       STPRS TYPE MBEW-STPRS,
      END OF TY_MBEW.


TYPES: BEGIN OF TY_MARA,
     MATNR TYPE MARA-MATNR,
     ERSDA TYPE MARA-ERSDA,
     ERNAM TYPE MARA-ERNAM,
     LAEDA  TYPE MARA-LAEDA,
     AENAM  TYPE MARA-AENAM,
     MTART  TYPE MARA-MTART,
     MATKL  TYPE MARA-MATKL,
     SPART  TYPE MARA-SPART,
     PRDHA  TYPE MARA-PRDHA,
     MFRNR  TYPE MARA-MFRNR,
     EAN11  TYPE MARA-EAN11,
     MEINS  TYPE MARA-MEINS,
     OBJEK  TYPE AUSP-OBJEK,
     MAKTX TYPE  MAKT-MAKTX,
     LABOR TYPE MARA-LABOR,
     BRGEW TYPE MARA-BRGEW,  " Supoj Add on 02..03.2015
     NTGEW TYPE MARA-NTGEW,  " Supoj Add on 02..03.2015
     GROES TYPE MARA-GROES, " Supoj Add on 02..03.2015
     MSTAE TYPE MARA-MSTAE,
     MSTAV TYPE MARA-MSTAV,
     LBTXT  TYPE T024X-LBTXT,
     GEWEI  TYPE  MARA-GEWEI,
     VOLEH  TYPE MARA-VOLEH,
     ZZFUCP TYPE MARA-ZZFUCP,
     ZZSBCT TYPE MARA-ZZSBCT,
     ZZCAV  TYPE MARA-ZZCAV,
     ZZCAU  TYPE MARA-ZZCAU,
     ZZIOD  TYPE MARA-ZZIOD,
     ZZIUT  TYPE MARA-ZZIUT,

     END OF TY_MARA.


TYPES: BEGIN OF TY_AUSP,
     OBJEK TYPE AUSP-OBJEK,
     KLART TYPE AUSP-KLART,
     ATWRT TYPE AUSP-ATWRT,
     ATINN TYPE AUSP-ATINN,
     ATBEZ TYPE CABNT-ATBEZ,
     ZCOUNT TYPE AUSP-ATWRT,
     END OF TY_AUSP.

*
TYPES: BEGIN OF TY_ALV,
      MATNR TYPE MARA-MATNR,
      ERSDA TYPE MARA-ERSDA,
      ERNAM TYPE MARA-ERNAM,
      LAEDA  TYPE MARA-LAEDA,
      AENAM  TYPE MARA-AENAM,
      MTART  TYPE MARA-MTART,
      MATKL  TYPE MARA-MATKL,
      SPART  TYPE MARA-SPART,
      PRDHA  TYPE MARA-PRDHA,
      MFRNR  TYPE MARA-MFRNR,
      VENDOR_NAME(50),
      EAN11  TYPE MARA-EAN11,
      MEINS  TYPE MARA-MEINS,
      VERPR  TYPE MBEW-VERPR,
      WAERS  TYPE T001-WAERS,
      SERNP TYPE MARC-SERNP,
      PRODH  TYPE MVKE-PRODH,
      MAKTX TYPE  MAKT-MAKTX,
      "Add field
      GEWEI TYPE  MARA-GEWEI,
      VOLEH TYPE MARA-VOLEH,
      ZZFUCP TYPE MARA-ZZFUCP,
      ZZSBCT TYPE MARA-ZZSBCT,
      LADGR  TYPE MARC-LADGR,
      ZZCAV  TYPE MARA-ZZCAV,
      ZZCAU  TYPE MARA-ZZCAU,
      ZZIOD  TYPE MARA-ZZIOD,
      ZZIUT  TYPE MARA-ZZIUT,
      MSTAE  TYPE MARA-MSTAE,
      MVGR1 TYPE MVKE-MVGR1,
      MVGR2 TYPE MVKE-MVGR2,
      MVGR3 TYPE MVKE-MVGR3,
      MVGR4 TYPE MVKE-MVGR4,
      MVGR5 TYPE MVKE-MVGR5,
      ZZIOD_DES TYPE ZDSMMC001-DESCRIPTION,
      ZZIUT_DES TYPE ZDSMMC026-DESCRIPTION,
      ZZCAU_DES TYPE ZDSMMC027-DESCRIPTION,
      MVGR1_DES TYPE TVM1T-BEZEI,
      MVGR2_DES TYPE TVM2T-BEZEI,
      MVGR3_DES TYPE TVM3T-BEZEI,
      MVGR4_DES TYPE TVM4T-BEZEI,
      MVGR5_DES TYPE TVM5T-BEZEI,
      END OF TY_ALV.

  TYPES: BEGIN OF GY_ZDSMMC026,
      ZZIUT       TYPE ZDSMMC026-ZZIUT,
      DESCRIPTION TYPE ZDSMMC026-DESCRIPTION,
  END OF GY_ZDSMMC026.

  TYPES: BEGIN OF GY_ZDSMMC001,
      ZZIOD       TYPE ZDSMMC001-ZZIOD,
      DESCRIPTION TYPE ZDSMMC001-DESCRIPTION,
  END OF GY_ZDSMMC001.

  TYPES: BEGIN OF GY_ZDSMMC027,
      ZZCAU       TYPE ZDSMMC027-ZZCAU,
      DESCRIPTION TYPE ZDSMMC027-DESCRIPTION,
  END OF GY_ZDSMMC027.

  TYPES: BEGIN OF GY_TVM1T,
      MVGR1       TYPE TVM1T-MVGR1,
      BEZEI       TYPE TVM1T-BEZEI,
  END OF GY_TVM1T.

  TYPES: BEGIN OF GY_TVM2T,
      MVGR2       TYPE TVM2T-MVGR2,
      BEZEI       TYPE TVM2T-BEZEI,
  END OF GY_TVM2T.

  TYPES: BEGIN OF GY_TVM3T,
      MVGR3       TYPE TVM3T-MVGR3,
      BEZEI       TYPE TVM3T-BEZEI,
  END OF GY_TVM3T.

  TYPES: BEGIN OF GY_TVM4T,
      MVGR4       TYPE TVM4T-MVGR4,
      BEZEI       TYPE TVM4T-BEZEI,
  END OF GY_TVM4T.

  TYPES: BEGIN OF GY_TVM5T,
      MVGR5       TYPE TVM5T-MVGR5,
      BEZEI       TYPE TVM5T-BEZEI,
  END OF GY_TVM5T.

*
**&-----------------------------------------------------------------------------------*
**& D A T A
**&-----------------------------------------------------------------------------------*

**&-----------------------------------------------------------------------------------*
**& I N T E R N A L   T A B L E S
**&-----------------------------------------------------------------------------------*
DATA: GT_MARA TYPE TABLE OF TY_MARA,
      GT_LFA1 TYPE TABLE OF TY_LFA1,
      GT_MVKE TYPE TABLE OF TY_MVKE,
      GT_AUSP TYPE TABLE OF TY_AUSP,
      GT_COL_AUSP TYPE TABLE OF TY_AUSP,
      GT_MBEW TYPE TABLE OF TY_MBEW,
      GT_MBEW_MA_PRODUCT TYPE TABLE OF TY_MBEW,
      GT_MARC TYPE TABLE OF TY_MARC,
      GT_MARM TYPE TABLE OF TY_MARM,  "Supoj Add on 02.03.2015
      GT_ALV  TYPE TABLE OF TY_ALV.
*      GT_ZTCS_MA_PRODUCT TYPE TABLE OF ZTCS_MA_PRODUCT.


DATA: GR_TABLE   TYPE REF TO CL_SALV_TABLE,
      GR_DISPLAY TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
      GR_COLUMNS  TYPE REF TO CL_SALV_COLUMNS_TABLE,
      GR_COLUMN  TYPE REF TO CL_SALV_COLUMN,
      GR_AGGRS TYPE REF TO CL_SALV_AGGREGATIONS,
      GR_SORT TYPE REF TO CL_SALV_SORTS,
      GR_SORT_COLUMN TYPE REF TO CL_SALV_SORT, "column sort
      GR_EVENTS TYPE REF TO CL_SALV_EVENTS_TABLE,
      GR_EVENTS_HANDLE TYPE REF TO LCL_HANDLE_EVENTS,
      GR_FUNCTIONS TYPE REF TO CL_SALV_FUNCTIONS_LIST,
      GR_LAYOUT  TYPE REF TO CL_SALV_LAYOUT,
      G_VARIANT TYPE SLIS_VARI,
      GR_KEY    TYPE SALV_S_LAYOUT_KEY,
      GR_SELECTIONS TYPE REF TO CL_SALV_SELECTIONS.
**        lt_rows       type salv_t_row,
**        lt_column     type salv_t_column,
**        ls_cell       type salv_s_cell.
**

DATA: GT_ZDSMMC026 TYPE STANDARD TABLE OF GY_ZDSMMC026,
      GT_ZDSMMC001 TYPE STANDARD TABLE OF GY_ZDSMMC001,
      GT_ZDSMMC027 TYPE STANDARD TABLE OF GY_ZDSMMC027,
      GT_TVM1T TYPE STANDARD TABLE OF GY_TVM1T,
      GT_TVM2T TYPE STANDARD TABLE OF GY_TVM2T,
      GT_TVM3T TYPE STANDARD TABLE OF GY_TVM3T,
      GT_TVM4T TYPE STANDARD TABLE OF GY_TVM4T,
      GT_TVM5T TYPE STANDARD TABLE OF GY_TVM5T.

DATA: GS_ZDSMMC026 TYPE GY_ZDSMMC026,
      GS_ZDSMMC001 TYPE GY_ZDSMMC001,
      GS_ZDSMMC027 TYPE GY_ZDSMMC027,
      GS_TVM1T TYPE GY_TVM1T,
      GS_TVM2T TYPE GY_TVM2T,
      GS_TVM3T TYPE GY_TVM3T,
      GS_TVM4T TYPE GY_TVM4T,
      GS_TVM5T TYPE GY_TVM5T.


* Data for ALV variant
DATA  GV_REPNAME          LIKE SY-REPID.
DATA  GV_X_VARIANT        LIKE DISVARIANT.
DATA  GV_EXIT(1)          TYPE C.
DATA  GV_SAVE(1)          TYPE C.
DATA  GV_VARIANT          LIKE DISVARIANT.



FIELD-SYMBOLS: <FS_TABLE> TYPE STANDARD TABLE,
               <FS_LINE>  TYPE ANY,
               <FS_FIELD> TYPE ANY.

*&-----------------------------------------------------------------------------------*
*& C O N S T A N T
*&-----------------------------------------------------------------------------------*
*Variable
*CONSTANTS: gc_charx    TYPE c VALUE 'X',

*&---------------------------------------------------------------------*
*& Program Variables
*&---------------------------------------------------------------------*


*&-----------------------------------------------------------------------------------*
*& S E L E C T I O N   S C R E E N
*&-----------------------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-T01.

SELECT-OPTIONS: S_MATNR  FOR MARA-MATNR,
                S_MTART  FOR MARA-MTART,
                S_AENAM  FOR MARA-AENAM,
                S_ERNAM  FOR MARA-ERNAM,
                S_MATKL  FOR MARA-MATKL,
                S_PRDHA  FOR MARA-PRDHA,
                S_SPART  FOR MARA-SPART,
                S_SPRAS  FOR  MAKT-SPRAS DEFAULT 'EN'.

SELECTION-SCREEN END OF BLOCK B1.


SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-T02.

PARAMETERS: S_VARI LIKE DISVARIANT-VARIANT.

SELECTION-SCREEN END OF BLOCK B2.
