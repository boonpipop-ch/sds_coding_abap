class ZCL_SDSFI_HCM_EMP_VENDOR_SYNC definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_FITV_VENDOR_SYNC .

  types:
    TT_BUKRS_RANGE TYPE RANGE OF T001-BUKRS .
  types:
    TT_POSTAL_RANGE  TYPE  RANGE OF P0006-PSTLZ .
  types:
    BEGIN OF TS_TAX,
        NATPERS TYPE  BUT000-NATPERS, "Natural Pers.
        TAXTYPE TYPE  DFKKBPTAXNUM-TAXTYPE,  "Tax Type
        TAXNUM  TYPE  DFKKBPTAXNUM-TAXNUM,   "Tax No.
      END OF TS_TAX .
  types:
    BEGIN OF TS_TAXX,
        NATPERS TYPE  FLAG, "Natural Pers.
        TAXTYPE TYPE  FLAG,  "Tax Type
        TAXNUM  TYPE  FLAG,   "Tax No.
      END OF TS_TAXX .

  constants GC_TRUE type CHAR1 value 'X' ##NO_TEXT.
  constants GC_TAXTTYPE type BPTAXTYPE value 'TH3' ##NO_TEXT.
  constants GC_BANK_ID type BU_BKVID value '001' ##NO_TEXT.
  constants GC_ADDR_TH type P0006-ANSSA value '1' ##NO_TEXT.
  constants GC_ADDR_EN type P0006-ANSSA value '2' ##NO_TEXT.
protected section.

  data GT_BUKRS type TT_BUKRS_RANGE .
  data GF_DEFT_BU_GROUP type TB001-BU_GROUP .
  data GF_READ type FLAG .
  data GF_DEFT_BCODE type BCODE .
  data GF_DEFT_BDESC type FITH_DESC .
  data GR_BKK_POSTAL type TT_POSTAL_RANGE .
private section.

  methods MAINTAIN_VENDOR_BRANCH
    importing
      !IF_PARTNER type BU_PARTNER .
  methods MAP_ADDR_HR_TO_FI
    importing
      !IS_HOME_ADDRESS type P0006
    changing
      !CS_POSTAL type BUS_EI_BUPA_POSTAL_ADDRESS .
  methods GET_GENC .
  methods IS_SDS
    importing
      !IF_BUKRS type T001-BUKRS
    returning
      value(RF_RESULT) type ABAP_BOOLEAN .
  methods ASSIGN_BAPI_TAX
    importing
      !IS_TAX type TS_TAX
      !IS_TAXX type TS_TAXX
    changing
      !CS_BAPI_TAX type BUS_EI_TAXNUMBER .
ENDCLASS.



CLASS ZCL_SDSFI_HCM_EMP_VENDOR_SYNC IMPLEMENTATION.


METHOD ASSIGN_BAPI_TAX.

  DATA:
    LS_TAX  TYPE  BUS_EI_BUPA_TAXNUMBER.


  CS_BAPI_TAX-COMMON-DATA-NAT_PERSON  = IS_TAX-NATPERS.
  CS_BAPI_TAX-COMMON-DATAX-NAT_PERSON = IS_TAXX-NATPERS.

  IF IS_TAXX-TAXTYPE EQ GC_TRUE OR
     IS_TAXX-TAXNUM  EQ GC_TRUE .
    CLEAR LS_TAX.
    LS_TAX-DATA_KEY-TAXTYPE   = IS_TAX-TAXTYPE.
    LS_TAX-DATA_KEY-TAXNUMBER = IS_TAX-TAXNUM.
    INSERT LS_TAX INTO TABLE CS_BAPI_TAX-TAXNUMBERS.
    CS_BAPI_TAX-CURRENT_STATE = GC_TRUE.
  ENDIF.

ENDMETHOD.


METHOD GET_GENC.

  CONSTANTS:
    LC_BUKRS         TYPE  ZSDSDE_PARAM_NAME VALUE 'COMPANY_IN_SCOPE',
    LC_DEFT_BU_GROUP TYPE  ZSDSDE_PARAM_NAME VALUE 'DEFAULT_BU_GROUP',
    LC_DEFT_BRANCH   TYPE  ZSDSDE_PARAM_NAME VALUE 'DEFAULT_BRANCH',
    LC_BKK_POSTAL    TYPE  ZSDSDE_PARAM_NAME VALUE 'BANGKOK_POSTAL'.

  DATA:
    LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA:
    LF_REPID   TYPE  PROGRAMM VALUE 'ZCL_SDSFI_HCM_EMP_VENDOR_SYNC'.


* Check if already read
  IF GF_READ EQ 'X'.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GT_BUKRS,
         GF_DEFT_BU_GROUP,
         GF_DEFT_BCODE,
         GF_DEFT_BDESC,
         GR_BKK_POSTAL.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = LF_REPID
    IMPORTING
      ET_GEN_C = LT_GENC.

* Mark Already read
  GF_READ = 'X'.

* Assign GenC Constants
  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).

    CASE <L_GENC>-PARAM.
*     ------------------------------------
*     Company code in enhancement scope
*     ------------------------------------
      WHEN LC_BUKRS.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GT_BUKRS.

*     ------------------------------------
*     Default Partner Group for Employee
*     ------------------------------------
      WHEN LC_DEFT_BU_GROUP.
        GF_DEFT_BU_GROUP = <L_GENC>-VALUE_LOW.

*     ------------------------------------
*     Default Vendor Branch data
*     ------------------------------------
      WHEN LC_DEFT_BRANCH.
        GF_DEFT_BCODE = <L_GENC>-VALUE_LOW.
        GF_DEFT_BDESC = <L_GENC>-VALUE_HIGH.

*     ------------------------------------
*     BKK Postal code
*     ------------------------------------
      WHEN LC_BKK_POSTAL.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GR_BKK_POSTAL.

    ENDCASE.

  ENDLOOP.

ENDMETHOD.


  method IF_FITV_VENDOR_SYNC~IS_VENDOR_TO_BE_CREATED.
  endmethod.


  method IF_FITV_VENDOR_SYNC~MODIFY_ADDRESSES.
  endmethod.


METHOD IF_FITV_VENDOR_SYNC~MODIFY_BANKING_DATA.

ENDMETHOD.


METHOD IF_FITV_VENDOR_SYNC~MODIFY_BP_GROUP_NUMBER.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSFI_HCM_EMP_VENDOR_SYNC
*  Creation Date      : 17.07.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : N/A
*  Description        : This is an implemented class for BADI
*                       BADI_FITV_VENDOR_SYNC to assign BP number and
*                       BP group for employee sync
*  Purpose            : To assign BP number and BP group for Employee
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

* ---------------
* Only Activated Company code
* ---------------
  IF NOT IS_SDS( I_EMPLOYEE_DATA-IT0001-BUKRS ).
    RETURN.
  ENDIF.

* ---------------
* Get Part Group from number range assigned
* ---------------
  DATA(LF_NRRNG) = C_BP_GROUP.
  SELECT BU_GROUP
    FROM TB001
   WHERE NRRNG EQ @LF_NRRNG
   ORDER BY PRIMARY KEY
    INTO @DATA(LF_BU_GROUP)
      UP TO 1 ROWS.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    LF_BU_GROUP = GF_DEFT_BU_GROUP.
  ENDIF.

* ---------------
* Assign Result
* ---------------
  C_BP_GROUP = LF_BU_GROUP.
* Assign Partner number as E + 6 Digit from Employee code
  C_BP_NUMBER = |E{ I_EMPLOYEE_DATA-PERNR+2(6) }|.

ENDMETHOD.


  method IF_FITV_VENDOR_SYNC~MODIFY_COMPANY_CODE_DATA.
  endmethod.


METHOD IF_FITV_VENDOR_SYNC~MODIFY_COMPLETE_DATA.

  DATA:
    LS_TAX       TYPE  TS_TAX,
    LS_TAXX      TYPE  TS_TAXX,
    LS_PHONE     TYPE  BUS_EI_BUPA_TELEPHONE,
    LS_FAX       TYPE  BUS_EI_BUPA_FAX,
    LS_SMTP      TYPE  BUS_EI_BUPA_SMTP.

  DATA:
    LF_DATUM  TYPE  SY-DATUM.


* ---------------
* Only Activated Company code
* ---------------
  IF NOT IS_SDS( I_EMPLOYEE_DATA-IT0001-BUKRS ).
    RETURN.
  ENDIF.

* ---------------
* Read Partner Line
* ---------------
  READ TABLE CT_BP_DATA_TAB ASSIGNING FIELD-SYMBOL(<L_BP_DATA>)
                            INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Date to determine data
  LF_DATUM = SY-DATUM.

* ---------------
* Get Personnel ID
* ---------------
  SELECT ICNUM
    FROM PA0185
   WHERE PERNR EQ @I_EMPLOYEE_DATA-PERNR
     AND BEGDA LE @LF_DATUM
     AND ENDDA GE @LF_DATUM
   ORDER BY PRIMARY KEY
    INTO @DATA(LF_ICNUM)
      UP TO 1 ROWS.
  ENDSELECT.
  IF SY-SUBRC EQ 0.
*   Remove -
    REPLACE ALL OCCURRENCES OF '-' IN LF_ICNUM WITH ''.
    CONDENSE LF_ICNUM NO-GAPS.

*   ---------------
*   Maintain Tax Data
*   ---------------
    CLEAR: LS_TAX,
           LS_TAXX.
    LS_TAX-NATPERS  = 'X'.
    LS_TAXX-NATPERS = 'X'.
    LS_TAX-TAXTYPE  = GC_TAXTTYPE.
    LS_TAXX-TAXTYPE = 'X'.
    LS_TAX-TAXNUM   = LF_ICNUM.
    LS_TAXX-TAXNUM  = 'X'.

    ASSIGN_BAPI_TAX(
      EXPORTING
        IS_TAX      = LS_TAX
        IS_TAXX     = LS_TAXX
      CHANGING
        CS_BAPI_TAX = <L_BP_DATA>-PARTNER-CENTRAL_DATA-TAXNUMBER ).
  ENDIF.

* ---------------
* Modify Banking Data
* ---------------
  LOOP AT <L_BP_DATA>-PARTNER-CENTRAL_DATA-BANKDETAIL-BANKDETAILS ASSIGNING FIELD-SYMBOL(<L_BANKDETAIL>).

*   Change Key HC to 00
    IF <L_BANKDETAIL>-DATA_KEY(2) EQ 'HC'.
      <L_BANKDETAIL>-DATA_KEY(2) = '00'.
    ELSE.
*     Remove Deletion of Already Changed Key
      IF <L_BANKDETAIL>-TASK EQ 'D'.
        DELETE <L_BP_DATA>-PARTNER-CENTRAL_DATA-BANKDETAIL-BANKDETAILS.
        CONTINUE.
      ENDIF.
    ENDIF.
*   Update Bank Reference
    SELECT SINGLE BRNCH
      FROM BNKA
     WHERE BANKS EQ @<L_BANKDETAIL>-DATA-BANK_CTRY
       AND BANKL EQ @<L_BANKDETAIL>-DATA-BANK_KEY
      INTO @DATA(LF_BRNCH).
    IF SY-SUBRC NE 0.
      CLEAR LF_BRNCH.
    ENDIF.
    <L_BANKDETAIL>-DATA-BANK_REF  = LF_BRNCH.
    <L_BANKDETAIL>-DATAX-BANK_REF = 'X'.
  ENDLOOP.

* ---------------
* Read IT0105
* ---------------
  SELECT BEGDA,
         ENDDA,
         USRTY,
         USRID,
         USRID_LONG
    FROM PA0105
   WHERE PERNR EQ @I_EMPLOYEE_DATA-PERNR
     AND BEGDA LE @LF_DATUM
     AND ENDDA GE @LF_DATUM
   ORDER BY USRTY ASCENDING,
            BEGDA ASCENDING
    INTO TABLE @DATA(LT_P0105).
  IF SY-SUBRC NE 0.
    CLEAR LT_P0105.
  ENDIF.

* ---------------
* Modify Address Data
* ---------------
  READ TABLE <L_BP_DATA>-PARTNER-CENTRAL_DATA-ADDRESS-ADDRESSES ASSIGNING FIELD-SYMBOL(<L_ADDRESS>)
                                                                INDEX 1.
  IF SY-SUBRC EQ 0.

*   ---------------
*   Thai Address Data
*   ---------------
    READ TABLE I_EMPLOYEE_DATA-HOME_ADDRESS_TAB ASSIGNING FIELD-SYMBOL(<L_HOME_ADDRESS>)
                                                WITH KEY SUBTY = GC_ADDR_TH.
    IF SY-SUBRC EQ 0.

      MAP_ADDR_HR_TO_FI(
        EXPORTING
          IS_HOME_ADDRESS = <L_HOME_ADDRESS>
        CHANGING
          CS_POSTAL       = <L_ADDRESS>-DATA-POSTAL ).

    ENDIF.

*   ---------------
*   Communication Data
*   ---------------
    LOOP AT LT_P0105 ASSIGNING FIELD-SYMBOL(<L_P0105>).

      CASE <L_P0105>-USRTY.

*       Mobile
        WHEN 'CELL'.
          CLEAR LS_PHONE.

          LS_PHONE-CONTACT-TASK = 'I'.

          LS_PHONE-CONTACT-DATA-TELEPHONE  = <L_P0105>-USRID.
          LS_PHONE-CONTACT-DATAX-TELEPHONE = 'X'.

          LS_PHONE-CONTACT-DATA-R_3_USER   = '1'.
          LS_PHONE-CONTACT-DATAX-R_3_USER  = 'X'.

          LS_PHONE-CONTACT-DATA-VALID_FROM  = <L_P0105>-BEGDA.
          LS_PHONE-CONTACT-DATAX-VALID_FROM = 'X'.

          <L_ADDRESS>-DATA-COMMUNICATION-PHONE-CURRENT_STATE = 'X'.
          INSERT LS_PHONE INTO TABLE <L_ADDRESS>-DATA-COMMUNICATION-PHONE-PHONE.

*       Fax
        WHEN '0005'.
          CLEAR LS_FAX.

          LS_FAX-CONTACT-TASK = 'I'.

          LS_FAX-CONTACT-DATA-FAX     = <L_P0105>-USRID.
          LS_FAX-CONTACT-DATAX-FAX    = 'X'.

          LS_FAX-CONTACT-DATA-R_3_USER   = '1'.
          LS_FAX-CONTACT-DATAX-R_3_USER  = 'X'.

          LS_FAX-CONTACT-DATA-VALID_FROM  = <L_P0105>-BEGDA.
          LS_FAX-CONTACT-DATAX-VALID_FROM = 'X'.

          <L_ADDRESS>-DATA-COMMUNICATION-FAX-CURRENT_STATE = 'X'.
          INSERT LS_FAX INTO TABLE <L_ADDRESS>-DATA-COMMUNICATION-FAX-FAX.

*       Email
        WHEN '0010'.
          CLEAR LS_SMTP.

          LS_SMTP-CONTACT-TASK = 'I'.

          LS_SMTP-CONTACT-DATA-E_MAIL     = <L_P0105>-USRID_LONG.
          LS_SMTP-CONTACT-DATAX-E_MAIL    = 'X'.

          LS_SMTP-CONTACT-DATA-VALID_FROM  = <L_P0105>-BEGDA.
          LS_SMTP-CONTACT-DATAX-VALID_FROM = 'X'.

          <L_ADDRESS>-DATA-COMMUNICATION-SMTP-CURRENT_STATE = 'X'.
          INSERT LS_SMTP INTO TABLE <L_ADDRESS>-DATA-COMMUNICATION-SMTP-SMTP.

      ENDCASE.

    ENDLOOP.

  ENDIF.

* ---------------
* Vendor Branch
* ---------------
  MAINTAIN_VENDOR_BRANCH(
    EXPORTING
      IF_PARTNER = I_EMPLOYEE_DATA-BUPA_ID ).

ENDMETHOD.


  method IF_FITV_VENDOR_SYNC~MODIFY_IDENTIFICATION_NUMBERS.
  endmethod.


  method IF_FITV_VENDOR_SYNC~MODIFY_PERSON_CENTRAL_DATA.
  endmethod.


  method IF_FITV_VENDOR_SYNC~MODIFY_ROLES.
  endmethod.


METHOD IF_FITV_VENDOR_SYNC~MODIFY_TAX_NUMBERS.
ENDMETHOD.


  method IF_FITV_VENDOR_SYNC~MODIFY_VENDOR_GENERAL_DATA.
  endmethod.


  method IF_FITV_VENDOR_SYNC~MODIFY_VENDOR_GROUP_NUMBER.
  endmethod.


METHOD IS_SDS.

* Initialize Output
  CLEAR: RF_RESULT.

* Get Constants
  GET_GENC( ).

  IF GT_BUKRS IS NOT INITIAL AND
     IF_BUKRS IN GT_BUKRS.
    RF_RESULT = ABAP_TRUE.
  ELSE.
    RF_RESULT = ABAP_FALSE.
  ENDIF.

ENDMETHOD.


METHOD MAINTAIN_VENDOR_BRANCH.

  DATA:
    LT_BCODE     TYPE  CL_FINLOC_CVI_PERSIST_DATA=>TT_FITHA_PBUPL_K,
    LT_BCODE_DEL TYPE  CL_FINLOC_CVI_PERSIST_DATA=>TT_FITHA_PBUPL_K,
    LT_BDESC     TYPE  CL_FINLOC_CVI_PERSIST_DATA=>TT_FITHA_PBUPL_K_T,
    LT_BDESC_DEL TYPE  CL_FINLOC_CVI_PERSIST_DATA=>TT_FITHA_PBUPL_K_T.

  DATA:
    LF_LIFNR  TYPE  LFA1-LIFNR.


* Only when Default branch set
  IF GF_DEFT_BCODE IS INITIAL.
    RETURN.
  ENDIF.

* Determine Vendor Code
  LF_LIFNR = IF_PARTNER.

* --------------------
* Modify Branch code
* --------------------
  INSERT VALUE #( LIFNR = LF_LIFNR
                  J_1TPBUPL = GF_DEFT_BCODE
                  DEFAULT_BRANCH = 'X' )
         INTO TABLE LT_BCODE.

  CALL FUNCTION 'LVTH_BUPA_UPDATE_BRANCH_CODE'
    IN UPDATE TASK
    EXPORTING
      IT_FITHA_PBUPL_K        = LT_BCODE
      IT_FITHA_PBUPL_K_DELETE = LT_BCODE_DEL.

* --------------------
* Modify Branch Description
* --------------------
  INSERT VALUE #( SPRAS = SY-LANGU
                  LIFNR = LF_LIFNR
                  J_1TPBUPL = GF_DEFT_BCODE
                  DESCRIPTION = GF_DEFT_BDESC )
         INTO TABLE LT_BDESC.

  CALL FUNCTION 'LVTH_BUPA_UPDATE_BRANCH_DESC'
    IN UPDATE TASK
    EXPORTING
      IT_FITHA_PBUPL_K_T        = LT_BDESC
      IT_FITHA_PBUPL_K_T_DELETE = LT_BDESC_DEL.


ENDMETHOD.


METHOD MAP_ADDR_HR_TO_FI.

  TYPES: BEGIN OF TS_ADDR,
           LINE TYPE  STRING,
         END OF TS_ADDR.
  TYPES: TT_ADDR  TYPE  STANDARD TABLE OF TS_ADDR.

  DATA:
    LT_ADDR  TYPE TT_ADDR.

  DATA:
    LF_LEN  TYPE  I.


* Initialize Data
  CLEAR CS_POSTAL-DATA.

* ---------------------
* Assign Address from Fields
* ---------------------
  IF IS_HOME_ADDRESS-BLDNG IS NOT INITIAL.
    INSERT VALUE #( LINE = IS_HOME_ADDRESS-BLDNG ) INTO TABLE LT_ADDR.
  ENDIF.

  IF IS_HOME_ADDRESS-HSNMR IS NOT INITIAL.
    INSERT VALUE #( LINE = |ม. { IS_HOME_ADDRESS-HSNMR }| ) INTO TABLE LT_ADDR.
  ENDIF.

  IF IS_HOME_ADDRESS-ADR03 IS NOT INITIAL.
    INSERT VALUE #( LINE = IS_HOME_ADDRESS-ADR03 ) INTO TABLE LT_ADDR.
  ENDIF.

  IF IS_HOME_ADDRESS-ADR04 IS NOT INITIAL.
    INSERT VALUE #( LINE = IS_HOME_ADDRESS-ADR04 ) INTO TABLE LT_ADDR.
  ENDIF.

  IF IS_HOME_ADDRESS-POSTA IS NOT INITIAL.
    INSERT VALUE #( LINE = |ห้อง { IS_HOME_ADDRESS-POSTA }| ) INTO TABLE LT_ADDR.
  ENDIF.

  IF IS_HOME_ADDRESS-FLOOR IS NOT INITIAL.
    INSERT VALUE #( LINE = |ชั้น { IS_HOME_ADDRESS-FLOOR }| ) INTO TABLE LT_ADDR.
  ENDIF.

  IF IS_HOME_ADDRESS-STRAS IS NOT INITIAL.
    INSERT VALUE #( LINE = |ซ. { IS_HOME_ADDRESS-STRAS }| ) INTO TABLE LT_ADDR.
  ENDIF.

  IF IS_HOME_ADDRESS-LOCAT IS NOT INITIAL.
    INSERT VALUE #( LINE = |ถ. { IS_HOME_ADDRESS-LOCAT }| ) INTO TABLE LT_ADDR.
  ENDIF.

* ---------------------
* Assign Address into Street and Street4
* ---------------------
  CLEAR LF_LEN.
  LOOP AT LT_ADDR ASSIGNING FIELD-SYMBOL(<L_ADDR>).

    LF_LEN = LF_LEN + STRLEN( <L_ADDR>-LINE ).

*   Assign into Street
    IF LF_LEN LT 35  ##NUMBER_OK.
      IF CS_POSTAL-DATA-STREET IS INITIAL.
        CS_POSTAL-DATA-STREET = <L_ADDR>-LINE.
      ELSE.
        CS_POSTAL-DATA-STREET = |{ CS_POSTAL-DATA-STREET } { <L_ADDR>-LINE } |.
      ENDIF.
      CS_POSTAL-DATAX-STREET = 'X'.

*   Assign into Stree4
    ELSE.
      IF CS_POSTAL-DATA-STR_SUPPL3 IS INITIAL.
        CS_POSTAL-DATA-STR_SUPPL3 = <L_ADDR>-LINE.
      ELSE.
        CS_POSTAL-DATA-STR_SUPPL3 = |{ CS_POSTAL-DATA-STR_SUPPL3 } { <L_ADDR>-LINE } |.
      ENDIF.
      CS_POSTAL-DATAX-STR_SUPPL3 = 'X'.

    ENDIF.

  ENDLOOP.

  IF IS_HOME_ADDRESS-CONKK IS NOT INITIAL.
*   Check if Bangkok?
    IF IS_HOME_ADDRESS-PSTLZ IN GR_BKK_POSTAL.
      CS_POSTAL-DATA-LOCATION  = |แขวง { IS_HOME_ADDRESS-CONKK }|.
    ELSE.
      CS_POSTAL-DATA-LOCATION  = |ต. { IS_HOME_ADDRESS-CONKK }|.
    ENDIF.
    CS_POSTAL-DATAX-LOCATION = 'X'.
  ENDIF.

  IF IS_HOME_ADDRESS-ORT02 IS NOT INITIAL.
*   Check if Bangkok?
    IF IS_HOME_ADDRESS-PSTLZ IN GR_BKK_POSTAL.
      CS_POSTAL-DATA-DISTRICT  = |เขต { IS_HOME_ADDRESS-ORT02 }|.
    ELSE.
      CS_POSTAL-DATA-DISTRICT  = |อ. { IS_HOME_ADDRESS-ORT02 }|.
    ENDIF.
    CS_POSTAL-DATAX-DISTRICT = 'X'.
  ENDIF.

  IF IS_HOME_ADDRESS-ORT01 IS NOT INITIAL.
    CS_POSTAL-DATA-CITY        = IS_HOME_ADDRESS-ORT01.
    CS_POSTAL-DATAX-CITY       = 'X'.
  ENDIF.

  IF IS_HOME_ADDRESS-STATE IS NOT INITIAL.
    CS_POSTAL-DATA-REGION        = IS_HOME_ADDRESS-STATE.
    CS_POSTAL-DATAX-REGION       = 'X'.
  ENDIF.

  CS_POSTAL-DATA-POSTL_COD1  = IS_HOME_ADDRESS-PSTLZ.
  CS_POSTAL-DATAX-POSTL_COD1 = 'X'.

  CS_POSTAL-DATA-COUNTRY     = IS_HOME_ADDRESS-LAND1.
  CS_POSTAL-DATAX-COUNTRY    = 'X'.

  CS_POSTAL-DATA-VALIDFROMDATE  = IS_HOME_ADDRESS-BEGDA.
  CS_POSTAL-DATAX-VALIDFROMDATE = 'X'.

  CS_POSTAL-DATA-VALIDTODATE    = IS_HOME_ADDRESS-ENDDA.
  CS_POSTAL-DATAX-VALIDTODATE   = 'X'.

ENDMETHOD.
ENDCLASS.
