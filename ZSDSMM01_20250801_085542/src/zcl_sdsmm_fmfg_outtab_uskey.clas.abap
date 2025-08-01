CLASS ZCL_SDSMM_FMFG_OUTTAB_USKEY DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

*"* public components of class CL_EXM_IM_ME_CHANGE_OUTTAB_CUS
*"* do not include other source files here!!!
  PUBLIC SECTION.

    INTERFACES IF_BADI_INTERFACE .
    INTERFACES IF_EX_ME_CHANGE_OUTTAB_CUS .

    TYPES:
      BEGIN OF GY_DATA,
        EBELN TYPE EKPO-EBELN,
        EBELP TYPE EKPO-EBELP,
      END OF GY_DATA .
    TYPES:
      GTY_DATA TYPE TABLE OF GY_DATA WITH EMPTY KEY.
    TYPES:
      BEGIN OF GY_ADDTIONAL,
        EBELN TYPE EKPO-EBELN,
        EBELP TYPE EKPO-EBELP,
        INBNB TYPE CHAR20,
        INVNB TYPE CHAR20,
      END OF GY_ADDTIONAL.
    TYPES:
      GTY_ADDTIONAL TYPE TABLE OF GY_ADDTIONAL WITH EMPTY KEY.
protected section.
*"* protected components of class CL_EXM_IM_ME_CHANGE_OUTTAB_CUS
*"* do not include other source files here!!!
private section.
*"* private components of class CL_EXM_IM_ME_CHANGE_OUTTAB_CUS
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_SDSMM_FMFG_OUTTAB_USKEY IMPLEMENTATION.


METHOD IF_EX_ME_CHANGE_OUTTAB_CUS~FILL_OUTTAB.

* When processing this source code, you activate the following functionality:
* The reporting transactions for purchasing documents provide three main views
* for display: basic list, delivery schedule, and account assignment. All
* three views contain a column "Material". If the material of a purchasing
* document item is a manufacturer part number (MPN) then this MPN is shown
* as "Material". The internal inventory managed material is not visible.
* The following source code replaces the MPN by the inventory managed material.

  DATA: LS_EKPO TYPE EKPO.

  DATA: LT_DATA TYPE GTY_DATA.

  DATA: LT_ADDITIONAL TYPE GTY_ADDTIONAL.

  DATA: LT_PURC TYPE TABLE OF MEREP_OUTTAB_PURCHDOC WITH EMPTY KEY,
        LT_SCHD TYPE TABLE OF MEREP_OUTTAB_SCHEDLINES WITH EMPTY KEY,
        LT_ACCO TYPE TABLE OF MEREP_OUTTAB_ACCOUNTING WITH EMPTY KEY.

  FIELD-SYMBOLS: <FS_OUTTAB>         TYPE ANY,
                 <FS_EBELN>          TYPE EBELN,
                 <FS_EBELP>          TYPE EBELP,
                 <FS_INBOUND_NUMBER> TYPE CHAR20,
                 <FS_INV_NUMBER>     TYPE CHAR20.

* check that a purchasing document view is displayed
  CHECK IM_STRUCT_NAME EQ 'MEREP_OUTTAB_PURCHDOC'   OR    "view: basic list
        IM_STRUCT_NAME EQ 'MEREP_OUTTAB_SCHEDLINES' OR    "view: delivery schedule
        IM_STRUCT_NAME EQ 'MEREP_OUTTAB_ACCOUNTING'.      "view: account assignment

  IF IM_STRUCT_NAME EQ 'MEREP_OUTTAB_PURCHDOC' .
    APPEND LINES OF CH_OUTTAB TO LT_PURC.
    LT_DATA = CORRESPONDING #( LT_PURC MAPPING EBELN = EBELN EBELP = EBELP ).
  ENDIF.

  IF IM_STRUCT_NAME EQ 'MEREP_OUTTAB_SCHEDLINES'.
    APPEND LINES OF CH_OUTTAB TO LT_SCHD.
    LT_DATA = CORRESPONDING #( LT_SCHD MAPPING EBELN = EBELN EBELP = EBELP ).
  ENDIF.

  IF IM_STRUCT_NAME EQ 'MEREP_OUTTAB_ACCOUNTING'.
    APPEND LINES OF CH_OUTTAB TO LT_ACCO.
    LT_DATA = CORRESPONDING #( LT_ACCO MAPPING EBELN = EBELN EBELP = EBELP ).
  ENDIF.

  IF LT_DATA IS NOT INITIAL.
    SORT LT_ADDITIONAL by ebeln ebelp.
    DELETE ADJACENT DUPLICATES FROM LT_ADDITIONAL COMPARING ALL FIELDS.
    LT_ADDITIONAL = LCL_DATA=>GET_ADDITIONAL_DATA( LT_DATA ).
  ENDIF.
* loop at the output table and assign a field symbol
  LOOP AT CH_OUTTAB ASSIGNING <FS_OUTTAB>.

*-- assign the purchasing document number to a field symbol
    ASSIGN COMPONENT 'EBELN' OF STRUCTURE <FS_OUTTAB> TO <FS_EBELN>.
    CHECK SY-SUBRC = 0.
*-- assign the purchasing document item number to a field symbol
    ASSIGN COMPONENT 'EBELP' OF STRUCTURE <FS_OUTTAB> TO <FS_EBELP>.
    CHECK SY-SUBRC = 0.

    ASSIGN COMPONENT 'ZZ1_INBOUND_NUM' OF STRUCTURE <FS_OUTTAB> TO <FS_INBOUND_NUMBER>.
    CHECK SY-SUBRC = 0.

    ASSIGN COMPONENT 'ZZ1_INV_NUM' OF STRUCTURE <FS_OUTTAB> TO <FS_INV_NUMBER>.
    CHECK SY-SUBRC = 0.

    READ TABLE LT_ADDITIONAL INTO DATA(LS_ADDITIONAL)
    WITH KEY EBELN = <FS_EBELN>
             EBELP = <FS_EBELP>.
    IF SY-SUBRC EQ 0.
      <FS_INBOUND_NUMBER> = LS_ADDITIONAL-INBNB.
      <FS_INV_NUMBER>     = LS_ADDITIONAL-INVNB.
    ENDIF.

*-- read the corresponding purchasing document item
*    CALL FUNCTION 'ME_EKPO_SINGLE_READ'
*      EXPORTING
*        PI_EBELN         = <FS_EBELN>
*        PI_EBELP         = <FS_EBELP>
*      IMPORTING
*        PO_EKPO          = LS_EKPO
*      EXCEPTIONS
*        NO_RECORDS_FOUND = 1
*        OTHERS           = 2.
*    CHECK SY-SUBRC = 0.

  ENDLOOP.

ENDMETHOD.
ENDCLASS.
