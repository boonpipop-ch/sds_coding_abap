*&--------------------------------------------------------------------*
*& Include          ZRFPPD0020_TOP
*&--------------------------------------------------------------------*
*& Version/TP No: 2.00( F36K904021 )                                  *
*& Changed On/By: 21.08.2024 Warat (EY TH)                            *
*& Description : CR#018 - Dynamically Calling Program                 *
*&--------------------------------------------------------------------*

DATA: LT_SELTAB TYPE TABLE OF RSPARAMS,
      LS_SELTAB LIKE LINE OF LT_SELTAB.

TYPES:
  BEGIN OF TS_ALV_MNU,
    FIELD01    TYPE CHAR20,           "STS_GR
    FIELD02    TYPE CHAR20,           "STS_COMP
    FIELD03    TYPE CHAR20,           "RELEASED
    FIELD04    TYPE CHAR20,           "FIELDS
    FIELD05    TYPE CHAR20,           "MATNR
    FIELD06    TYPE CHAR40,           "AETXT
    FIELD07    TYPE CHAR20,           "MM_PU_MAN
    FIELD08(6) TYPE C,                "MM_PU_IF
    FIELD09(6) TYPE C,                "MM_PE_MAN
    FIELD10(6) TYPE C,                "MM_PE_IF
    FIELD11(6) TYPE C,                "MM_CMD_MAN
    FIELD12(6) TYPE C,                "MM_CMD_IF
    FIELD13(6) TYPE C,                "MM_SPS_MAN
    FIELD14(6) TYPE C,                "MM_SPS_IF
    FIELD15(6) TYPE C,                "MM_GA_MAN
    FIELD16(6) TYPE C,                "MM_GA_IF
    FIELD17(6) TYPE C,                "MM_LGT_MAN
    FIELD18(6) TYPE C,                "MM_LGT_IF
    FIELD19(6) TYPE C,                "MM_SAG_MAN
    FIELD20(6) TYPE C,                "MM_SAG_IF
    FIELD21(6) TYPE C,                "MM_ACG_MAN
    FIELD22(6) TYPE C,                "MM_ACG_IF
    FIELD23(6) TYPE C,                "MM_QC_MAN
    FIELD24(6) TYPE C,                "MM_QC_IF
    FIELD25(6) TYPE C,                "MM_PC_MAN
    FIELD26(6) TYPE C,                "MM_PC_IF
    FIELD27(6) TYPE C,                "MM_BCG_MAN
    FIELD28(6) TYPE C,                "MM_BCG_IF
    FIELD29(6) TYPE C,                "MM_R&D_MAN
    FIELD30(6) TYPE C,                "MM_R&D_IF
    FIELD31(6) TYPE C,                "MM_IT_MAN
    FIELD32(6) TYPE C,                "MM_IT_IF
    FIELD33    TYPE CHAR20,           "PURCHINF_REC
    FIELD34    TYPE CHAR20,           "SOURCE_CD_LST
    FIELD35    TYPE CHAR20,           "QUOTA_ARR
    FIELD36    TYPE CHAR20,           "BWF
    FIELD37    TYPE CHAR20,           "ROUTING
    FIELD38    TYPE CHAR20,           "BOM
    FIELD39    TYPE CHAR20,           "PRD_VER
    FIELD40    TYPE CHAR20,           "APPLY_DATE
    FIELD41    TYPE CHAR50,           "TEXT
  END OF TS_ALV_MNU,



  BEGIN OF TY_MATNR,
    MATNR TYPE MATNR,
  END OF TY_MATNR.

CONSTANTS:
  LC_Z1 TYPE CHAR2  VALUE 'Z1',
  LC_Z2 TYPE CHAR2  VALUE 'Z2',
  LC_Z3 TYPE CHAR2  VALUE 'Z3'.

DATA:
  LT_DATA_MNU   TYPE REF TO  DATA,
  LT_TEST_MNU   TYPE STANDARD TABLE OF TS_ALV_MNU WITH EMPTY KEY,
  LT_MATNR      TYPE STANDARD TABLE OF TY_MATNR,
  LS_MATNR      TYPE TY_MATNR,
  LT_HEADDATA   LIKE BAPIMATHEAD,
  LT_PLANTDATA  LIKE BAPI_MARC,
  LT_PLANTDATAX LIKE BAPI_MARCX,
  LT_RETURN     TYPE BAPIRET2.

FIELD-SYMBOLS: <LT_TEST_DATA> TYPE ANY TABLE,
               <LT_TEST_MAP>  TYPE TS_ALV_MNU.

NEW CL_SALV_BS_RUNTIME_INFO( )->SET( EXPORTING DISPLAY = ABAP_FALSE
                                            METADATA = ABAP_FALSE
                                            DATA = ABAP_TRUE ).

*<<<BEG LOCAL ADDONS CR018: Call Dynamic Program by Plant on 21.08.2024

DATA: GF_SUBMIT_PROGRAM TYPE TRDIR-NAME.

TYPES: BEGIN OF TS_ALV_MNU_KEYFIELD,
         STAT_GR   TYPE CHAR20,           "STS_GR
         STAT_COMP TYPE CHAR20,           "STS_COMP
         RELEASED  TYPE CHAR20,           "RELEASED
         FIELDS    TYPE CHAR20,           "FIELDS
         MATNR     TYPE CHAR20,           "MATNR
         DCS_NO    TYPE CHAR40,           "AETXT
       END OF TS_ALV_MNU_KEYFIELD,

      BEGIN OF TS_ALV_MNU_STATUS,
        FIELD01    TYPE CHAR20,           "STS_GR
        FIELD02    TYPE CHAR20,           "STS_COMP
        FIELD03    TYPE CHAR20,           "RELEASED
        FIELD04    TYPE CHAR20,           "FIELDS
        FIELD05    TYPE CHAR20,           "MATNR
        FIELD06    TYPE CHAR40,           "AETXT.
      END OF TS_ALV_MNU_STATUS.

DATA: LS_TEST_MNU       TYPE TS_ALV_MNU_KEYFIELD,
      LT_TEST_MNU_KF    TYPE STANDARD TABLE OF TS_ALV_MNU_KEYFIELD WITH EMPTY KEY,
      LT_TEST_MNU_STAT  TYPE STANDARD TABLE OF TS_ALV_MNU_STATUS WITH EMPTY KEY,
      LT_TEST_MNU_3     TYPE STANDARD TABLE OF TS_ALV_MNU WITH EMPTY KEY.

*>>>END LOCAL ADDONS CR018: Call Dynamic Program by Plant on 21.08.2024
