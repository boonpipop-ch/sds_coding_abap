*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0280
*  Creation Date      : 03.05.2024
*  Author             : Suthichai P.(Eviden)
*  Add-on ID          : ZMME014
*  Description        : Program to add condition types for planned cost
*                       to PO.
*  Purpose            : N/A
*  Copied from        : N/A
*  Restriction        : N/A
*  Relate Program     : RV61A909
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSMMR0280.
*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
*TABLES:

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: TT_XKOMV  TYPE  STANDARD TABLE OF KOMV_INDEX.

TYPES: BEGIN OF TS_EKKO_CHECK,
         INCO1        TYPE  EKKO-INCO1,
         ZZ1_DLV_MODE TYPE  EKKO-ZZ1_DLV_MODE,
         IHREZ        TYPE  EKKO-IHREZ,
       END OF TS_EKKO_CHECK.

TYPES: BEGIN OF TS_EKPO_CHECK,
         BRGEW           TYPE  EKPO-BRGEW,
         VOLUM           TYPE  EKPO-VOLUM,
         NETPR           TYPE  EKPO-NETPR,
         MENGE           TYPE  EKPO-MENGE,
         ZZ1_DES_PT_C_PO TYPE  EKPO-ZZ1_DES_PT_C_PO,
       END OF TS_EKPO_CHECK.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:

  BEGIN OF GC_UPDATE_PROCESS,
    FIXED       TYPE RV61A-KSTEU VALUE 'E',
    CALCULATION TYPE RV61A-KSTEU VALUE 'X',
    MANAUL      TYPE RV61A-KSTEU VALUE 'Y',
  END OF GC_UPDATE_PROCESS,

  GC_TRUE  TYPE  CHAR1     VALUE 'X',
  GC_REPID TYPE  PROGRAMM  VALUE 'ZSDSMMR0280'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*
DATA:
  GT_TRTYP TYPE  RANGE OF T160-TRTYP                          ##NEEDED,
  GT_BUKRS TYPE  RANGE OF EKKO-BUKRS                          ##NEEDED,
  GT_EKORG TYPE  RANGE OF EKKO-EKORG                          ##NEEDED,
  GT_BSART TYPE  RANGE OF EKKO-BSART                          ##NEEDED,
  GT_INCO1 TYPE  RANGE OF EKKO-INCO1                          ##NEEDED.

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* Text-s01: Selection Criteria
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  PARAMETERS RB_08 RADIOBUTTON GROUP G1 DEFAULT 'X'.
  PARAMETERS RB_09 RADIOBUTTON GROUP G1.
  PARAMETERS RB_10 RADIOBUTTON GROUP G1.
  PARAMETERS RB_11 RADIOBUTTON GROUP G1.
  PARAMETERS RB_12 RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  CASE GC_TRUE.
    WHEN RB_08.
      PERFORM F_CALL_MAINTAIN USING 'ZSDSV_INSURANCE'.
    WHEN RB_09.
      PERFORM F_CALL_MAINTAIN USING 'ZSDSV_AVG_CLEAR'.
    WHEN RB_10.
      PERFORM F_CALL_MAINTAIN USING 'ZSDSV_AVG_PRICE'.
    WHEN RB_11.
      PERFORM F_CALL_MAINTAIN USING 'ZSDSV_UNL_PRICE'.
    WHEN RB_12.
      PERFORM F_CALL_MAINTAIN USING 'ZSDSV_IMP_DUTY'.
  ENDCASE.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Form F_CALL_MAINTAIN
*----------------------------------------------------------------------*
*  Call Maintain View
*----------------------------------------------------------------------*
FORM F_CALL_MAINTAIN USING UF_VNAME  TYPE  DD02V-TABNAME.

  CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
    EXPORTING
      ACTION                       = 'S'
      VIEW_NAME                    = UF_VNAME
    EXCEPTIONS
      CLIENT_REFERENCE             = 1
      FOREIGN_LOCK                 = 2
      INVALID_ACTION               = 3
      NO_CLIENTINDEPENDENT_AUTH    = 4
      NO_DATABASE_FUNCTION         = 5
      NO_EDITOR_FUNCTION           = 6
      NO_SHOW_AUTH                 = 7
      NO_TVDIR_ENTRY               = 8
      NO_UPD_AUTH                  = 9
      ONLY_SHOW_ALLOWED            = 10
      SYSTEM_FAILURE               = 11
      UNKNOWN_FIELD_IN_DBA_SELLIST = 12
      VIEW_NOT_FOUND               = 13
      MAINTENANCE_PROHIBITED       = 14
      OTHERS                       = 15 ##FM_SUBRC_OK.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_IS_ACTIVE
*----------------------------------------------------------------------*
*  Is Active
*----------------------------------------------------------------------*
FORM F_IS_ACTIVE  USING    US_KOMK   TYPE KOMK
                  CHANGING CF_SUBRC  TYPE SY-SUBRC.

  DATA :
    LRT_BUKRS TYPE RANGE OF EKKO-BUKRS,
    LRT_EKORG TYPE RANGE OF EKKO-EKORG,
    LRT_BSART TYPE RANGE OF EKKO-BSART.

  CF_SUBRC = 4.

  ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = GC_REPID
                                                  IF_PARAM = 'BUKRS'
                                        IMPORTING ET_RANGE = LRT_BUKRS ).

  ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = GC_REPID
                                                  IF_PARAM = 'EKORG'
                                        IMPORTING ET_RANGE = LRT_EKORG ).

  ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = GC_REPID
                                                  IF_PARAM = 'BSART'
                                        IMPORTING ET_RANGE = LRT_BSART ).

  IF LRT_BUKRS IS INITIAL.
    RETURN.
  ENDIF.

  IF LRT_EKORG IS INITIAL.
    RETURN.
  ENDIF.

  IF LRT_BSART IS INITIAL.
    RETURN.
  ENDIF.

  IF US_KOMK-BUKRS NOT IN LRT_BUKRS.
    RETURN.
  ENDIF.


  IF US_KOMK-EKORG NOT IN LRT_EKORG.
    RETURN.
  ENDIF.

  IF US_KOMK-BSART NOT IN LRT_BSART.
    RETURN.
  ENDIF.

  CF_SUBRC = 0.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_GET_MASTER_ZIB1
*----------------------------------------------------------------------*
*  Get Master ZIB1
*----------------------------------------------------------------------*
FORM F_GET_MASTER_ZIB1 USING    UF_CODE   TYPE ZRTDE_DLV_MODE
                       CHANGING CT_MASTER TYPE ZSDSMMS014_TT.

  SELECT FROM ZSDSMMT008
    FIELDS *
    WHERE CODE EQ @UF_CODE
    AND   ZDEL_FLG EQ @SPACE
    INTO TABLE @CT_MASTER.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_GET_MASTER_FRB1
*----------------------------------------------------------------------*
*  Get Master FRB1
*----------------------------------------------------------------------*
FORM F_GET_MASTER_FRB1 USING    US_EKKO   TYPE EKKO
                       CHANGING CT_MASTER TYPE ZSDSMMS015_TT.

  DATA:
    LF_IHREZ  TYPE  EKKO-IHREZ.


* Use Upper case value for searching
  LF_IHREZ = US_EKKO-IHREZ.
  TRANSLATE LF_IHREZ TO UPPER CASE.

  SELECT FROM ZSDSMMT009
    FIELDS *
    WHERE CODE EQ @US_EKKO-ZZ1_DLV_MODE
    AND   IHREZ EQ @LF_IHREZ
    AND   ZDEL_FLG EQ @SPACE
    INTO TABLE @CT_MASTER.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_SET_AMOUNT
*----------------------------------------------------------------------*
*  Set Amout
*----------------------------------------------------------------------*
FORM F_SET_AMOUNT USING     US_KOMK   TYPE KOMK
                            UT_XKMOV  TYPE TT_XKOMV
                  CHANGING  CS_XKOMV  TYPE KOMV_INDEX.

  CHECK SY-UCOMM <> 'MESAVE'.

* Get Constants
  PERFORM F_GET_CONSTANTS.

  CASE CS_XKOMV-KSCHL.
    WHEN 'ZIB1'. "Insurance in Thai Baht
      PERFORM F_SET_ZIB1_AMOUNT
        USING    US_KOMK CS_XKOMV-UPDKZ
        CHANGING CS_XKOMV-KBETR.

    WHEN 'FRB1'. "Freight in Thai Baht
      PERFORM F_SET_FRB1_AMOUNT
        USING    CS_XKOMV-UPDKZ
        CHANGING CS_XKOMV-KBETR.

    WHEN 'ZOB1'. "Customs clearance in Thai Baht
      PERFORM F_SET_ZOB1_AMOUNT
        USING    CS_XKOMV-UPDKZ
        CHANGING CS_XKOMV-KBETR.

    WHEN 'ZTB1'. "Transportation and unloading in Thai Baht
      PERFORM F_SET_ZTB1_AMOUNT
        USING    CS_XKOMV-UPDKZ
        CHANGING CS_XKOMV-KBETR.

    WHEN 'ZDB1'. "Import duty in Thai Baht
      PERFORM F_SET_ZDB1_AMOUNT
        USING    UT_XKMOV CS_XKOMV-UPDKZ
        CHANGING CS_XKOMV-KBETR.

    WHEN 'ZEB1'. "Equipment in Thai Baht
      PERFORM F_SET_ZEB1_AMOUNT
        CHANGING CS_XKOMV-KBETR.

  ENDCASE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_SET_ZIB1_AMOUNT
*----------------------------------------------------------------------*
*  Set ZTB1 Amout
*----------------------------------------------------------------------*
FORM F_SET_ZIB1_AMOUNT USING     US_KOMK   TYPE KOMK
                                 UF_UPDKZ  TYPE KOMV_INDEX-UPDKZ ##NEEDED
                       CHANGING  CF_AMOUNT  TYPE KBETR.

  DATA :
    LT_ZIB1   TYPE ZSDSMMS014_TT,
    LRT_INCO1 TYPE RANGE OF EKKO-INCO1,
    LS_EKKO   TYPE EKKO,
    LS_EKPO   TYPE EKPO,
*    LF_OLD    TYPE KBETR,
    LF_KSTEU  TYPE RV61A-KSTEU.

  ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = GC_REPID
                                                  IF_PARAM = 'INCO1'
                                        IMPORTING ET_RANGE = LRT_INCO1 ).

  PERFORM F_GET_VALUE
    USING '(SAPLMEPO)EKKO'
    CHANGING LS_EKKO.

  PERFORM F_GET_VALUE
    USING '(SAPLMEPO)EKPO'
    CHANGING LS_EKPO.

  PERFORM F_GET_VALUE
*    USING '(SAPLV69A)RV61A-KSTEU'
    USING '(SAPLV61A)PREISFINDUNGSART'
    CHANGING LF_KSTEU.

  DATA:
    LF_TRTYP  TYPE  T160-TRTYP.

* Get Transaction Type
  PERFORM F_GET_VALUE
    USING '(SAPLMECOM1)GF_TRTYP'
    CHANGING LF_TRTYP.

* Not Fix or Blank
  IF LF_KSTEU EQ SPACE OR
     LF_KSTEU EQ GC_UPDATE_PROCESS-FIXED.
    RETURN.
  ENDIF.

* Only when in Specified mode
* or KSTEU = X
  IF NOT ( ( GT_TRTYP IS NOT INITIAL AND LF_TRTYP IN GT_TRTYP ) OR
            LF_KSTEU EQ GC_UPDATE_PROCESS-CALCULATION ).
    RETURN.
  ENDIF.


*  LF_OLD = CF_AMOUNT.
*  CASE LF_KSTEU.
*    WHEN GC_UPDATE_PROCESS-CALCULATION.
*      "Calculate Condtion Price
*    WHEN GC_UPDATE_PROCESS-MANAUL.
*      IF UF_UPDKZ IS NOT INITIAL AND LF_OLD IS NOT INITIAL.
*        CF_AMOUNT = LF_OLD.
*        RETURN.
*      ENDIF.
*    WHEN SPACE OR GC_UPDATE_PROCESS-FIXED.
*      RETURN.
*    WHEN OTHERS.
*      RETURN.
*  ENDCASE.

  IF US_KOMK-INCO1 IN LRT_INCO1.

    PERFORM F_GET_MASTER_ZIB1
      USING LS_EKKO-ZZ1_DLV_MODE
      CHANGING LT_ZIB1.

    IF LT_ZIB1[] IS NOT INITIAL AND
       LS_EKPO-NETPR IS NOT INITIAL AND
       LS_EKPO-MENGE IS NOT INITIAL.

      CF_AMOUNT = LS_EKPO-NETPR * LS_EKPO-MENGE.
      IF LS_EKKO-WAERS <> 'THB'.
        CF_AMOUNT = CF_AMOUNT * LS_EKKO-WKURS.
      ENDIF.

      LOOP AT LT_ZIB1 ASSIGNING FIELD-SYMBOL(<L_ZIB1>).
        CF_AMOUNT = ( CF_AMOUNT * <L_ZIB1>-INS_PERCENT ) / 100.
      ENDLOOP.

    ENDIF.

  ELSE.

    CLEAR CF_AMOUNT.

  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_SET_FRB1_AMOUNT
*----------------------------------------------------------------------*
*  Set FRB1 Amout
*----------------------------------------------------------------------*
FORM F_SET_FRB1_AMOUNT USING    UF_UPDKZ  TYPE KOMV_INDEX-UPDKZ ##NEEDED
                       CHANGING CF_AMOUNT TYPE KBETR.


  DATA :
    LS_EKKO  TYPE EKKO,
    LS_EKPO  TYPE EKPO,
*    LF_OLD   TYPE KBETR,
    LF_KSTEU TYPE RV61A-KSTEU,
    LF_IHREZ TYPE EKKO-IHREZ.


  PERFORM F_GET_VALUE
    USING '(SAPLMEPO)EKKO'
    CHANGING LS_EKKO.

  PERFORM F_GET_VALUE
    USING '(SAPLMEPO)EKPO'
    CHANGING LS_EKPO.

  PERFORM F_GET_VALUE
*    USING '(SAPLV69A)RV61A-KSTEU'
    USING '(SAPLV61A)PREISFINDUNGSART'
    CHANGING LF_KSTEU.

  DATA:
    LF_TRTYP  TYPE  T160-TRTYP.

* Get Transaction Type
  PERFORM F_GET_VALUE
    USING '(SAPLMECOM1)GF_TRTYP'
    CHANGING LF_TRTYP.

* Not Fix or Blank
  IF LF_KSTEU EQ SPACE OR
     LF_KSTEU EQ GC_UPDATE_PROCESS-FIXED.
    RETURN.
  ENDIF.

* Only when in Specified mode
* or KSTEU = X
  IF NOT ( ( GT_TRTYP IS NOT INITIAL AND LF_TRTYP IN GT_TRTYP ) OR
            LF_KSTEU EQ GC_UPDATE_PROCESS-CALCULATION ).
    RETURN.
  ENDIF.

*  LF_OLD = CF_AMOUNT.
*  CASE LF_KSTEU.
*    WHEN GC_UPDATE_PROCESS-CALCULATION.
*      "Calculate Condtion Price
*    WHEN GC_UPDATE_PROCESS-MANAUL.
*      IF UF_UPDKZ IS NOT INITIAL AND LF_OLD IS NOT INITIAL.
*        CF_AMOUNT = LF_OLD.
*        RETURN.
*      ENDIF.
*    WHEN SPACE OR GC_UPDATE_PROCESS-FIXED.
*      RETURN.
*    WHEN OTHERS.
*      RETURN.
*  ENDCASE.

* Use Upper case value for searching
  LF_IHREZ = LS_EKKO-IHREZ.
  TRANSLATE LF_IHREZ TO UPPER CASE.

  SELECT SINGLE FROM ZSDSMMT010
    FIELDS AVG_GROSS , AVG_VOLUME
    WHERE CODE EQ @LS_EKKO-ZZ1_DLV_MODE
    AND   IHREZ EQ @LF_IHREZ
    AND   ZDEL_FLG EQ @SPACE
    INTO @DATA(LS_MASTER).
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  IF LS_MASTER-AVG_GROSS IS NOT INITIAL AND
     LS_MASTER-AVG_VOLUME IS NOT INITIAL.
    IF LS_EKPO-BRGEW > LS_EKPO-VOLUM.
      CF_AMOUNT = LS_EKPO-BRGEW * LS_EKPO-MENGE * LS_MASTER-AVG_GROSS.
    ELSE.
      CF_AMOUNT = LS_EKPO-VOLUM * LS_EKPO-MENGE * LS_MASTER-AVG_VOLUME.
    ENDIF.
  ELSE.
    IF LS_MASTER-AVG_GROSS IS NOT INITIAL.
      CF_AMOUNT = LS_EKPO-BRGEW * LS_EKPO-MENGE * LS_MASTER-AVG_GROSS.
    ELSE.
      CF_AMOUNT = LS_EKPO-VOLUM * LS_EKPO-MENGE * LS_MASTER-AVG_VOLUME.
    ENDIF.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_SET_ZOB1_AMOUNT
*----------------------------------------------------------------------*
*  Set ZOB1 Amout
*----------------------------------------------------------------------*
FORM F_SET_ZOB1_AMOUNT USING    UF_UPDKZ  TYPE KOMV_INDEX-UPDKZ ##NEEDED
                       CHANGING CF_AMOUNT  TYPE KBETR.

  DATA :
    LT_FRB1    TYPE ZSDSMMS015_TT,
    LRT_VOLUME TYPE RANGE OF VOLUM,
    LS_EKKO    TYPE EKKO,
    LS_EKPO    TYPE EKPO,
*    LF_OLD     TYPE KBETR,
    LF_KSTEU   TYPE RV61A-KSTEU.

  PERFORM F_GET_VALUE
    USING '(SAPLMEPO)EKKO'
    CHANGING LS_EKKO.

  PERFORM F_GET_VALUE
    USING '(SAPLMEPO)EKPO'
    CHANGING LS_EKPO.

  PERFORM F_GET_VALUE
*    USING '(SAPLV69A)RV61A-KSTEU'
    USING '(SAPLV61A)PREISFINDUNGSART'
    CHANGING LF_KSTEU.

  DATA:
    LF_TRTYP  TYPE  T160-TRTYP.

* Get Transaction Type
  PERFORM F_GET_VALUE
    USING '(SAPLMECOM1)GF_TRTYP'
    CHANGING LF_TRTYP.

* Not Fix or Blank
  IF LF_KSTEU EQ SPACE OR
     LF_KSTEU EQ GC_UPDATE_PROCESS-FIXED.
    RETURN.
  ENDIF.

* Only when in Specified mode
* or KSTEU = X
  IF NOT ( ( GT_TRTYP IS NOT INITIAL AND LF_TRTYP IN GT_TRTYP ) OR
            LF_KSTEU EQ GC_UPDATE_PROCESS-CALCULATION ).
    RETURN.
  ENDIF.

*  LF_OLD = CF_AMOUNT.
*  CASE LF_KSTEU.
*    WHEN GC_UPDATE_PROCESS-CALCULATION.
*      "Calculate Condtion Price
*    WHEN GC_UPDATE_PROCESS-MANAUL.
*      IF UF_UPDKZ IS NOT INITIAL AND LF_OLD IS NOT INITIAL.
*        CF_AMOUNT = LF_OLD.
*        RETURN.
*      ENDIF.
*    WHEN SPACE OR GC_UPDATE_PROCESS-FIXED.
*      RETURN.
*    WHEN OTHERS.
*      RETURN.
*  ENDCASE.

  PERFORM F_GET_MASTER_FRB1
    USING    LS_EKKO
    CHANGING LT_FRB1.

  LOOP AT LT_FRB1 ASSIGNING FIELD-SYMBOL(<L_FRB1>).

    IF <L_FRB1>-VOL_OPTION IS NOT INITIAL AND
       <L_FRB1>-VOLUME IS NOT INITIAL.
      LRT_VOLUME = VALUE #(
                    SIGN = 'I'
                    OPTION = <L_FRB1>-VOL_OPTION
                    ( LOW = <L_FRB1>-VOLUME ) ).
      CHECK LS_EKPO-VOLUM IN LRT_VOLUME.
    ENDIF.

    CF_AMOUNT = LS_EKPO-MENGE * <L_FRB1>-AVG_CLEAR.

    IF <L_FRB1>-VOLUME IS NOT INITIAL.
      CF_AMOUNT = CF_AMOUNT * LS_EKPO-VOLUM.
    ENDIF.

    EXIT.                                               "#EC CI_NOORDER

  ENDLOOP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_SET_ZTB1_AMOUNT
*----------------------------------------------------------------------*
*  Set ZTB1 Amout
*----------------------------------------------------------------------*
FORM F_SET_ZTB1_AMOUNT USING    UF_UPDKZ  TYPE KOMV_INDEX-UPDKZ ##NEEDED
                       CHANGING CF_AMOUNT TYPE KBETR.

  DATA :
    LF_PRODH TYPE MARA-PRDHA,
    LF_PRVOL TYPE ZSDSMMT011-PRICE_VOLUME,
    LS_EKKO  TYPE EKKO,
    LS_EKPO  TYPE EKPO,
*    LF_OLD   TYPE KBETR,
    LF_KSTEU TYPE RV61A-KSTEU.

  PERFORM F_GET_VALUE
    USING '(SAPLMEPO)EKKO'
    CHANGING LS_EKKO.

  PERFORM F_GET_VALUE
    USING '(SAPLMEPO)EKPO'
    CHANGING LS_EKPO.

  PERFORM F_GET_VALUE
*    USING '(SAPLV69A)RV61A-KSTEU'
    USING '(SAPLV61A)PREISFINDUNGSART'
    CHANGING LF_KSTEU.

  DATA:
    LF_TRTYP  TYPE  T160-TRTYP.

* Get Transaction Type
  PERFORM F_GET_VALUE
    USING '(SAPLMECOM1)GF_TRTYP'
    CHANGING LF_TRTYP.

* Not Fix or Blank
  IF LF_KSTEU EQ SPACE OR
     LF_KSTEU EQ GC_UPDATE_PROCESS-FIXED.
    RETURN.
  ENDIF.

* Only when in Specified mode
* or KSTEU = X
  IF NOT ( ( GT_TRTYP IS NOT INITIAL AND LF_TRTYP IN GT_TRTYP ) OR
            LF_KSTEU EQ GC_UPDATE_PROCESS-CALCULATION ).
    RETURN.
  ENDIF.

*  LF_OLD = CF_AMOUNT.
*  CASE LF_KSTEU.
*    WHEN GC_UPDATE_PROCESS-CALCULATION.
*      "Calculate Condtion Price
*    WHEN GC_UPDATE_PROCESS-MANAUL.
*      IF UF_UPDKZ IS NOT INITIAL AND LF_OLD IS NOT INITIAL.
*        CF_AMOUNT = LF_OLD.
*        RETURN.
*      ENDIF.
*    WHEN SPACE OR GC_UPDATE_PROCESS-FIXED.
*      RETURN.
*    WHEN OTHERS.
*      RETURN.
*  ENDCASE.

  IF LS_EKPO-MATNR IS NOT INITIAL.
    SELECT SINGLE FROM MARA
      FIELDS SUBSTRING( PRDHA , 1 , 5 )
      WHERE MATNR EQ @LS_EKPO-MATNR
      INTO @LF_PRODH.
    IF SY-SUBRC <> 0.
      RETURN.
    ENDIF.

    SELECT SINGLE FROM ZSDSMMT011
      FIELDS PRICE_VOLUME
      WHERE MTART EQ @LS_EKPO-MTART
      AND   PRODH EQ @LF_PRODH
      AND   ZZ1_DES_PT_C EQ @LS_EKPO-ZZ1_DES_PT_C_PO
      AND   ZDEL_FLG EQ @SPACE
      INTO @LF_PRVOL.
    IF SY-SUBRC <> 0.
      RETURN.
    ENDIF.

    CF_AMOUNT = LS_EKPO-MENGE * LS_EKPO-VOLUM * LF_PRVOL.

  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_SET_ZDB1_AMOUNT
*----------------------------------------------------------------------*
*  Set ZDB1 Amout
*----------------------------------------------------------------------*
FORM F_SET_ZDB1_AMOUNT USING     UT_XKMOV  TYPE TT_XKOMV
                                 UF_UPDKZ  TYPE KOMV_INDEX-UPDKZ ##NEEDED
                       CHANGING  CF_AMOUNT  TYPE KBETR.

  DATA :
    LF_CIF   TYPE KBETR,
    LS_EKKO  TYPE EKKO,
    LS_EKPO  TYPE EKPO,
*    LF_OLD   TYPE KBETR,
    LF_KSTEU TYPE RV61A-KSTEU.

  PERFORM F_GET_VALUE
    USING '(SAPLMEPO)EKKO'
    CHANGING LS_EKKO.

  PERFORM F_GET_VALUE
    USING '(SAPLMEPO)EKPO'
    CHANGING LS_EKPO.

  PERFORM F_GET_VALUE
*    USING '(SAPLV69A)RV61A-KSTEU'
    USING '(SAPLV61A)PREISFINDUNGSART'
    CHANGING LF_KSTEU.

  DATA:
    LF_TRTYP  TYPE  T160-TRTYP.

* Get Transaction Type
  PERFORM F_GET_VALUE
    USING '(SAPLMECOM1)GF_TRTYP'
    CHANGING LF_TRTYP.

* Not Fix or Blank
  IF LF_KSTEU EQ SPACE OR
     LF_KSTEU EQ GC_UPDATE_PROCESS-FIXED.
    RETURN.
  ENDIF.

* Only when in Specified mode
* or KSTEU = X
  IF NOT ( ( GT_TRTYP IS NOT INITIAL AND LF_TRTYP IN GT_TRTYP ) OR
            LF_KSTEU EQ GC_UPDATE_PROCESS-CALCULATION ).
    RETURN.
  ENDIF.

*  LF_OLD = CF_AMOUNT.
*  CASE LF_KSTEU.
*    WHEN GC_UPDATE_PROCESS-CALCULATION.
*      "Calculate Condtion Price
*    WHEN GC_UPDATE_PROCESS-MANAUL.
*      IF UF_UPDKZ IS NOT INITIAL AND LF_OLD IS NOT INITIAL.
*        CF_AMOUNT = LF_OLD.
*        RETURN.
*      ENDIF.
*    WHEN SPACE OR GC_UPDATE_PROCESS-FIXED.
*      RETURN.
*    WHEN OTHERS.
*      RETURN.
*  ENDCASE.

  IF LS_EKPO-MATNR IS NOT INITIAL.
    SELECT SINGLE FROM ZSDSMMT012
      FIELDS DUTY_PERCENT
      WHERE MATNR EQ @LS_EKPO-MATNR
      AND   ZDEL_FLG EQ @SPACE
      INTO @DATA(LF_PERCENT).
    IF SY-SUBRC <> 0.
      RETURN.
    ENDIF.

    LF_CIF = LS_EKPO-NETPR * LS_EKPO-MENGE.
    IF LS_EKKO-WAERS <> 'THB'.
      LF_CIF = LF_CIF * LS_EKKO-WKURS.
    ENDIF.

    READ TABLE UT_XKMOV INTO DATA(LS_XKOMV)
      WITH KEY KSCHL = 'ZIB1'.
    IF SY-SUBRC EQ 0.
      LF_CIF = LF_CIF + LS_XKOMV-KBETR.
    ENDIF.

    READ TABLE UT_XKMOV INTO LS_XKOMV
      WITH KEY KSCHL = 'FRB1'.
    IF SY-SUBRC EQ 0.
      LF_CIF = LF_CIF + LS_XKOMV-KBETR.
    ENDIF.

    CF_AMOUNT = ( LF_CIF * LF_PERCENT ) / 100.

  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_SET_ZEB1_AMOUNT
*----------------------------------------------------------------------*
*  Set ZEB1 Amout
*----------------------------------------------------------------------*
FORM F_SET_ZEB1_AMOUNT CHANGING CF_AMOUNT  TYPE KBETR.

  DATA :
    LF_KSTEU  TYPE RV61A-KSTEU.

  PERFORM F_GET_VALUE
*    USING '(SAPLV69A)RV61A-KSTEU'
    USING '(SAPLV61A)PREISFINDUNGSART'
    CHANGING LF_KSTEU.

  DATA:
    LF_TRTYP  TYPE  T160-TRTYP.

* Get Transaction Type
  PERFORM F_GET_VALUE
    USING '(SAPLMECOM1)GF_TRTYP'
    CHANGING LF_TRTYP.

* Not Fix or Blank
  IF LF_KSTEU EQ SPACE OR
     LF_KSTEU EQ GC_UPDATE_PROCESS-FIXED.
    RETURN.
  ENDIF.

* Only when in Specified mode
* or KSTEU = X
  IF NOT ( ( GT_TRTYP IS NOT INITIAL AND LF_TRTYP IN GT_TRTYP ) OR
            LF_KSTEU EQ GC_UPDATE_PROCESS-CALCULATION ).
    RETURN.
  ENDIF.

  CLEAR CF_AMOUNT.

*  CASE LF_KSTEU.
*    WHEN GC_UPDATE_PROCESS-CALCULATION.
*      CLEAR CF_AMOUNT.
*    WHEN OTHERS.
*      RETURN.
*  ENDCASE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_GET_VALUE
*----------------------------------------------------------------------*
*  Get Value
*----------------------------------------------------------------------*
FORM F_GET_VALUE  USING     UF_FNAME  TYPE FIELDNAME
                  CHANGING  CF_FVAL   TYPE ANY.

  ASSIGN (UF_FNAME) TO FIELD-SYMBOL(<L_FVAL>).
  IF SY-SUBRC EQ 0 AND <L_FVAL> IS ASSIGNED.
    CF_FVAL = <L_FVAL>.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CHECK_EKKO_CHANGED
*----------------------------------------------------------------------*
*  Check EKKO Changed
*----------------------------------------------------------------------*
FORM F_CHECK_EKKO_CHANGED  USING  US_OEKKO  TYPE  EKKO
                                  US_EKKO   TYPE  EKKO
                         CHANGING CF_RECALCULATE TYPE C.

  DATA:
    LS_CHECK  TYPE  TS_EKKO_CHECK,
    LS_OCHECK TYPE  TS_EKKO_CHECK.

  DATA:
    LF_ACTIVE TYPE  CHAR1.


* Only not yet active
  IF CF_RECALCULATE IS NOT INITIAL.
    RETURN.
  ENDIF.

  PERFORM F_IS_ACTIVE2  USING  US_EKKO
                      CHANGING LF_ACTIVE.
  IF LF_ACTIVE IS INITIAL.
    RETURN.
  ENDIF.

  MOVE-CORRESPONDING US_EKKO TO LS_CHECK.
  MOVE-CORRESPONDING US_OEKKO TO LS_OCHECK.

* Mark Flag when Changed
  IF LS_CHECK NE LS_OCHECK.
    CF_RECALCULATE = 'X'.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_CONSTANTS
*----------------------------------------------------------------------*
*  Get GenC Constants
*----------------------------------------------------------------------*
FORM F_GET_CONSTANTS .

  CONSTANTS:
    LC_TRTYP TYPE  ZSDSDE_PARAM_NAME VALUE 'TRTYP',
    LC_BUKRS TYPE  ZSDSDE_PARAM_NAME VALUE 'BUKRS',
    LC_BSART TYPE  ZSDSDE_PARAM_NAME VALUE 'BSART',
    LC_EKORG TYPE  ZSDSDE_PARAM_NAME VALUE 'EKORG',
    LC_INCO1 TYPE  ZSDSDE_PARAM_NAME VALUE 'INCO1'.

  STATICS:
    LF_READ       TYPE  FLAG.

  DATA:
    LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA:
    LF_REPID   TYPE  PROGRAMM.


* Check Already Read?
  IF LF_READ EQ GC_TRUE.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GT_TRTYP,
         GT_BUKRS,
         GT_EKORG,
         GT_BSART,
         GT_INCO1.

* Assign REPID
  LF_REPID = SY-REPID.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = LF_REPID
    IMPORTING
      ET_GEN_C = LT_GENC.

* Mark Read Flag
  LF_READ = GC_TRUE.

* Assign GenC Constants
  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).

    CASE <L_GENC>-PARAM.
*     ------------------------------------
*     Active Transaction Type
*     ------------------------------------
      WHEN LC_TRTYP.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GT_TRTYP.

*     ------------------------------------
*     Active Company Code
*     ------------------------------------
      WHEN LC_BUKRS.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GT_BUKRS.

*     ------------------------------------
*     Active Purchase Org
*     ------------------------------------
      WHEN LC_EKORG.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GT_EKORG.

*     ------------------------------------
*     Active Doc Type
*     ------------------------------------
      WHEN LC_BSART.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GT_BSART.

*     ------------------------------------
*     Active Inco Term 1
*     ------------------------------------
      WHEN LC_INCO1.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GT_INCO1.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CHECK_REDETERMINE_PRICE
*----------------------------------------------------------------------*
*  Check Price needs to be redetermined?
*----------------------------------------------------------------------*
FORM F_CHECK_REDETERMINE_PRICE  USING  US_OEKKO TYPE  EKKO
                                       US_EKKO  TYPE  EKKO
                                       US_OEKPO TYPE  EKPO
                                       US_EKPO  TYPE  EKPO
                              CHANGING CF_CALCTYPE TYPE KOMV-KSTEU.

  DATA:
    LS_EKKO_CHECK  TYPE  TS_EKKO_CHECK,
    LS_EKKO_OCHECK TYPE  TS_EKKO_CHECK,
    LS_EKPO_CHECK  TYPE  TS_EKPO_CHECK,
    LS_EKPO_OCHECK TYPE  TS_EKPO_CHECK.

  DATA:
    LF_ACTIVE   TYPE  CHAR1,
    LF_CALCTYPE TYPE  KOMV-KSTEU.


* Only when Blank
  IF CF_CALCTYPE IS NOT INITIAL.
    RETURN.
  ENDIF.

  PERFORM F_IS_ACTIVE2  USING  US_EKKO
                      CHANGING LF_ACTIVE.
  IF LF_ACTIVE IS INITIAL.
    RETURN.
  ENDIF.

* Set CALCTYPE
  LF_CALCTYPE = GC_UPDATE_PROCESS-CALCULATION.

* Check Header is changed
  MOVE-CORRESPONDING US_EKKO TO LS_EKKO_CHECK.
  MOVE-CORRESPONDING US_OEKKO TO LS_EKKO_OCHECK.

* Check Item is changed
  MOVE-CORRESPONDING US_EKPO TO LS_EKPO_CHECK.
  MOVE-CORRESPONDING US_OEKPO TO LS_EKPO_OCHECK.

* Redetermine when changing found
  IF LS_EKKO_CHECK NE LS_EKKO_OCHECK OR
     LS_EKPO_CHECK NE LS_EKPO_OCHECK.
    CF_CALCTYPE = LF_CALCTYPE.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_IS_ACTIVE2
*----------------------------------------------------------------------*
*  Is Activated for Redetermination?
*----------------------------------------------------------------------*
FORM F_IS_ACTIVE2  USING  US_EKKO  TYPE  EKKO
                 CHANGING CF_ACTIVE TYPE CHAR1.

  DATA:
    LF_TRTYP  TYPE  T160-TRTYP.


* Initialize Output
  CLEAR: CF_ACTIVE.

* Get Constants
  PERFORM F_GET_CONSTANTS.

* Get Transaction Type
  PERFORM F_GET_VALUE
    USING '(SAPLMECOM1)GF_TRTYP'
    CHANGING LF_TRTYP.

* Only Activated transaction type
  IF NOT ( GT_TRTYP IS NOT INITIAL AND
           LF_TRTYP IN GT_TRTYP ).
    RETURN.
  ENDIF.

* Only Activated Company
  IF NOT ( GT_BUKRS IS NOT INITIAL AND
           US_EKKO-BUKRS IN GT_BUKRS ).
    RETURN.
  ENDIF.

* Only Activated Purchase Org
  IF NOT ( GT_EKORG IS NOT INITIAL AND
           US_EKKO-EKORG IN GT_EKORG ).
    RETURN.
  ENDIF.

* Only Activated Doc Type
  IF NOT ( GT_BSART IS NOT INITIAL AND
           US_EKKO-BSART IN GT_BSART ).
    RETURN.
  ENDIF.

* Activated
  CF_ACTIVE = GC_TRUE.

ENDFORM.
