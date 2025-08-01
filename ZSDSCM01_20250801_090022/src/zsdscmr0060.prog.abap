*-----------------------------------------------------------------------
*  Program ID         : ZSDSCMR0060
*  Creation Date      : 10.02.2025
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : 420000380
*  Description        : This is a subroutine pool to keep all logic
*                       related to VOFM routine 903
*  Purpose            : - To adjust tax amount from rounding issue.
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
PROGRAM ZSDSCMR0060.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: TT_KALSM_RANGE TYPE RANGE OF KOMK-KALSM.

TYPES: BEGIN OF TS_BILLPLAN,
         OBJTYPE_H  TYPE  CRMS4D_BILLREQ_I-OBJTYPE_H,
         OBJECT_ID  TYPE  CRMS4D_BILLREQ_I-OBJECT_ID,
         NUMBER_INT TYPE  CRMS4D_BILLREQ_I-NUMBER_INT,
         RECORD_NO  TYPE  CRMS4D_BILLREQ_I-RECORD_NO,
       END OF TS_BILLPLAN.
TYPES: TT_BILLPLAN  TYPE  STANDARD TABLE OF TS_BILLPLAN
                          WITH DEFAULT KEY.

TYPES: BEGIN OF TS_ORDER_ITEM,
         OBJTYPE_H  TYPE  CRMS4D_BILLREQ_I-OBJTYPE_H,
         OBJECT_ID  TYPE  CRMS4D_BILLREQ_I-OBJECT_ID,
         NUMBER_INT TYPE  CRMS4D_BILLREQ_I-NUMBER_INT,
       END OF TS_ORDER_ITEM.
TYPES: TT_ORDER_ITEM TYPE STANDARD TABLE OF TS_ORDER_ITEM
                          WITH DEFAULT KEY.

TYPES: BEGIN OF TS_POSTED_BDR,
         VGBEL    TYPE  VBRP-VGBEL,
         VGPOS    TYPE  VBRP-VGPOS,
         VBELN    TYPE  VBRP-VBELN,
         POSNR    TYPE  VBRP-POSNR,
         BASE_AMT TYPE  VBRP-KZWI3,
         TAX_AMT  TYPE  VBRP-MWSBP,
       END OF TS_POSTED_BDR.
TYPES: TT_POSTED_BDR  TYPE  SORTED TABLE OF TS_POSTED_BDR
                            WITH UNIQUE KEY VGBEL
                                            VGPOS
                                            VBELN
                                            POSNR.

TYPES: BEGIN OF TS_POSTED_ITEM,
         HEADER_GUID TYPE  CRMS4D_SERV_I-HEADER_GUID,
         NUMBER_INT  TYPE  CRMS4D_SERV_I-NUMBER_INT,
         BASE_AMT    TYPE  CRMS4D_SERV_I-NET_VALUE_I,
         TAX_AMT     TYPE  CRMS4D_SERV_I-TAX_AMOUNT_I,
       END OF TS_POSTED_ITEM.
TYPES: TT_POSTED_ITEM  TYPE  SORTED TABLE OF TS_POSTED_ITEM
                             WITH UNIQUE KEY HEADER_GUID
                                             NUMBER_INT.

TYPES: BEGIN OF TS_POSTED_BILL,
         VBELN    TYPE  VBRP-VBELN,
         POSNR    TYPE  VBRP-POSNR,
         BASE_AMT TYPE  VBRP-KZWI3,
         TAX_AMT  TYPE  VBRP-MWSBP,
       END OF TS_POSTED_BILL.
TYPES: TT_POSTED_BILL  TYPE  SORTED TABLE OF TS_POSTED_BILL
                             WITH UNIQUE KEY VBELN
                                             POSNR.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE     TYPE  CHAR1     VALUE 'X',

  GC_CONTRACT TYPE  CRMT_SUBOBJECT_CATEGORY_DB VALUE 'BUS2000112',
  GC_ORDER    TYPE  CRMT_SUBOBJECT_CATEGORY_DB VALUE 'BUS2000116'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_POSTING_BDR  TYPE  TT_POSTED_BDR                          ##NEEDED,
  GT_POSTING_ITEM TYPE  TT_POSTED_ITEM                         ##NEEDED,
  GT_POSTING_BILL TYPE  TT_POSTED_BILL                         ##NEEDED.

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
  GR_KALSM_SVO TYPE  TT_KALSM_RANGE                           ##NEEDED,
  GR_KALSM_SVC TYPE  TT_KALSM_RANGE                           ##NEEDED.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Form F_ADJUST_TAX_AMT
*----------------------------------------------------------------------*
*  Adjust Tax Amount from rounding issue
*----------------------------------------------------------------------*
FORM F_ADJUST_TAX_AMT  USING  US_KOMK  TYPE  KOMK
                              US_KOMP  TYPE  KOMP
                              US_GKOMV TYPE  GKOMV
                              US_XKOMV TYPE  KOMV_INDEX
                     CHANGING CF_XKWERT TYPE KOMV-KWERT.

* Get Constants
  PERFORM F_GET_CONSTANTS.

* --------------------------------
* For Group condition, assign the group value
* --------------------------------
  IF US_GKOMV-KWERT IS NOT INITIAL.
    IF US_KOMP-VGBEL IS INITIAL AND
       US_KOMP-VGPOS IS INITIAL AND
       US_KOMP-KPOSN IS INITIAL AND
       US_XKOMV-KAWRT EQ US_GKOMV-KAWRT. "Only when Group Amount found and base is same
      CF_XKWERT = US_GKOMV-KWERT.
      RETURN.
    ENDIF.
  ENDIF.

* --------------------------------
* Service Contract Calculation
* --------------------------------
  IF GR_KALSM_SVC IS NOT INITIAL AND
     US_KOMK-KALSM IN GR_KALSM_SVC.
*   Adjust tax on BDR reference to billing plan
*   in service contract and
*   billing tax will copy from reference BDR.
    PERFORM F_ADJUST_TAX_AMT_SVC  USING  US_KOMK
                                         US_KOMP
                                         US_XKOMV
                                CHANGING CF_XKWERT.
* --------------------------------
* Service Order Calculation
* --------------------------------
  ELSEIF GR_KALSM_SVO IS NOT INITIAL AND
         US_KOMK-KALSM IN GR_KALSM_SVO.
*   Adjust tax on BDR/Billing documents to match
*   header calculation in each document.
    PERFORM F_ADJUST_TAX_AMT_SVO  USING  US_KOMK
                                         US_KOMP
                                         US_XKOMV
                                CHANGING CF_XKWERT.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_CONSTANTS
*----------------------------------------------------------------------*
*  Get GenC Constants
*----------------------------------------------------------------------*
FORM F_GET_CONSTANTS .

  CONSTANTS:
    LC_KALSM_SVO TYPE  ZSDSDE_PARAM_NAME VALUE 'SVO_PRICE_PROC',
    LC_KALSM_SVC TYPE  ZSDSDE_PARAM_NAME VALUE 'SVC_PRICE_PROC'.

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
  CLEAR: GR_KALSM_SVO,
         GR_KALSM_SVC.

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
*     Pricing Procedure for Service Order
*     ------------------------------------
      WHEN LC_KALSM_SVO.
        INSERT VALUE #( SIGN = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GR_KALSM_SVO.

*     ------------------------------------
*     Pricing Procedure for Service Contract
*     ------------------------------------
      WHEN LC_KALSM_SVC.
        INSERT VALUE #( SIGN = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GR_KALSM_SVC.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ADJUST_TAX_AMT_SVC
*----------------------------------------------------------------------*
*  Adjust Tax Amount from rounding issue (Service Contract Scenario)
*----------------------------------------------------------------------*
FORM F_ADJUST_TAX_AMT_SVC  USING  US_KOMK  TYPE  KOMK
                                  US_KOMP  TYPE  KOMP
                                  US_XKOMV TYPE  KOMV_INDEX
                         CHANGING CF_XKWERT TYPE KOMV-KWERT.

  DATA:
    LF_GUID   TYPE CRMT_OBJECT_GUID.


  IF US_KOMP-VGBEL IS NOT INITIAL.

*   --------------------------------
*   In case Billing from BDR,
*   get Amount from reference BDR
*   --------------------------------
    SELECT SINGLE A~VBELN,
                  A~POSNR
      FROM VBRP AS A
     WHERE A~VBELN EQ @US_KOMP-VGBEL
       AND A~POSNR EQ @US_KOMP-VGPOS
      INTO @DATA(LS_VBRP) ##NEEDED.
    IF SY-SUBRC EQ 0.
*     Assign Tax Amount from BDR
      PERFORM F_ASSIGN_TAX_AMT_BDR  USING  US_KOMP
                                           US_XKOMV
                                  CHANGING CF_XKWERT .
    ELSE.
      CLEAR LS_VBRP.

*     Get Reference Document Category
      SELECT OBJTYPE_H
        FROM CRMS4D_SERV_H
       WHERE OBJECT_ID EQ @US_KOMP-VGBEL
       ORDER BY OBJTYPE_H ASCENDING,
                OBJECT_ID ASCENDING
        INTO @DATA(LF_OBJTYPE_H)
          UP TO 1 ROWS.
      ENDSELECT.
      IF SY-SUBRC NE 0.
        CLEAR LF_OBJTYPE_H.
      ENDIF.

      CASE LF_OBJTYPE_H.

*       -----------------------
*       Service Contract - Case BDR Reference Service Contract
*       -----------------------
        WHEN GC_CONTRACT.

*         Assign Tax Amount from Service Contract
          PERFORM F_ASSIGN_TAX_AMT_SVC  USING  US_KOMP
                                               US_XKOMV
                                      CHANGING CF_XKWERT .

*       -----------------------
*       Service Order - Case BDR Reference Service Order
*       -----------------------
        WHEN GC_ORDER.
*         Assign Tax Amount from Service Order
          PERFORM F_ASSIGN_TAX_AMT_SVO  USING  US_KOMP
                                               US_XKOMV
                                      CHANGING CF_XKWERT .

      ENDCASE.

      IF LF_OBJTYPE_H IS NOT INITIAL.
*       Collect posting new tax data
        READ TABLE GT_POSTING_BDR ASSIGNING FIELD-SYMBOL(<L_POSTING_BDR>)
                                  WITH KEY VGBEL = US_KOMP-VGBEL
                                           VGPOS = US_KOMP-VGPOS
                                  BINARY SEARCH.
        IF SY-SUBRC EQ 0.
*         Updating values
          <L_POSTING_BDR>-BASE_AMT = US_XKOMV-KAWRT.
          <L_POSTING_BDR>-TAX_AMT  = CF_XKWERT.
        ELSE.
          INSERT VALUE #( VGBEL = US_KOMP-VGBEL
                          VGPOS = US_KOMP-VGPOS
                          VBELN = US_KOMK-BELNR
                          POSNR = US_KOMP-KPOSN
                          BASE_AMT = US_XKOMV-KAWRT
                          TAX_AMT  = CF_XKWERT )
                 INTO TABLE GT_POSTING_BDR.

*         Initiate Clear value when commit
          PERFORM F_CLEAR_POSTING_DATA ON COMMIT.
        ENDIF.
      ENDIF.

    ENDIF.

  ELSE.
*   --------------------------------
*   In case no reference
*     - Pricing within Service Order
*         - To update Sum item tax must equal to tax calculated from header amount
*   --------------------------------
    PERFORM F_ASSIGN_TAX_AMT_NOREF USING  US_KOMK
                                          US_KOMP
                                          US_XKOMV
                                 CHANGING CF_XKWERT
                                          LF_GUID.
    IF LF_GUID IS NOT INITIAL.
*     Collect posting new tax data
      READ TABLE GT_POSTING_ITEM ASSIGNING FIELD-SYMBOL(<L_POSTING_ITEM>)
                                 WITH KEY HEADER_GUID = LF_GUID
                                          NUMBER_INT  = US_KOMP-KPOSN
                                 BINARY SEARCH.
      IF SY-SUBRC EQ 0.
*       Updating values
        <L_POSTING_ITEM>-BASE_AMT = US_XKOMV-KAWRT.
        <L_POSTING_ITEM>-TAX_AMT  = CF_XKWERT.
      ELSE.
        INSERT VALUE #( HEADER_GUID = LF_GUID
                        NUMBER_INT  = US_KOMP-KPOSN
                        BASE_AMT    = US_XKOMV-KAWRT
                        TAX_AMT     = CF_XKWERT )
               INTO TABLE GT_POSTING_ITEM.

*       Initiate Clear value when commit
        PERFORM F_CLEAR_POSTING_DATA ON COMMIT.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_BILLPLAN
*----------------------------------------------------------------------*
*  Get list of Related Service contract billing plan
*----------------------------------------------------------------------*
FORM F_GET_BILLPLAN  USING  UF_OBJECT_ID TYPE TS_BILLPLAN-OBJECT_ID
                            UF_RECORD_NO TYPE TS_BILLPLAN-RECORD_NO
                   CHANGING CT_BILLPLAN TYPE  TT_BILLPLAN.

* Initialize Output
  CLEAR: CT_BILLPLAN.

* Get Related billing plan
  SELECT OBJTYPE_H,
         OBJECT_ID,
         NUMBER_INT,
         RECORD_NO
    FROM CRMS4D_BILLREQ_I
   WHERE OBJTYPE_H  EQ @GC_CONTRACT
     AND OBJECT_ID  EQ @UF_OBJECT_ID
     AND NUMBER_INT IN ( SELECT NUMBER_INT
                           FROM CRMS4D_BILLREQ_I
                          WHERE OBJTYPE_H  EQ @GC_CONTRACT
                            AND OBJECT_ID  EQ @UF_OBJECT_ID
                            AND RECORD_NO  EQ @UF_RECORD_NO  )
   ORDER BY OBJECT_ID ASCENDING,
            RECORD_NO ASCENDING,
            OBJTYPE_H ASCENDING,
            NUMBER_INT
    INTO TABLE @CT_BILLPLAN.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_POSTED_BDR
*----------------------------------------------------------------------*
*  Get Posted BDR Data on related Billing plan
*----------------------------------------------------------------------*
FORM F_GET_POSTED_BDR  USING  UT_BILLPLAN  TYPE  TT_BILLPLAN
                     CHANGING CT_POSTED_BDR TYPE  TT_POSTED_BDR.

  DATA:
    LT_POSTED_BDR  TYPE  TT_POSTED_BDR.


* Initialize Output
  CLEAR: CT_POSTED_BDR.

* Get Posted BDR Data on Related Billing plan
  SELECT A~VGBEL,
         A~VGPOS,
         A~VBELN,
         A~POSNR,
         A~KZWI3,
         A~MWSBP
    FROM @UT_BILLPLAN AS X  ##ITAB_KEY_IN_SELECT
           INNER JOIN VBRP AS A
             ON  A~VGBEL EQ X~OBJECT_ID
             AND A~VGPOS EQ X~RECORD_NO
           INNER JOIN VBRK AS B
             ON  B~VBELN = A~VBELN
   WHERE B~BDR_STATUS NE 'D'
   ORDER BY A~VGBEL ASCENDING,
            A~VGPOS ASCENDING,
            A~VBELN ASCENDING,
            A~POSNR ASCENDING
    INTO TABLE @LT_POSTED_BDR.
  IF SY-SUBRC NE 0.
    CLEAR LT_POSTED_BDR.
  ENDIF.

* Add Posting document into the list
  LOOP AT GT_POSTING_BDR ASSIGNING FIELD-SYMBOL(<L_POSTING_BDR>).

*   Only related
    READ TABLE UT_BILLPLAN TRANSPORTING NO FIELDS
                           WITH KEY OBJECT_ID = <L_POSTING_BDR>-VGBEL
                                    RECORD_NO = <L_POSTING_BDR>-VGPOS
                           BINARY SEARCH.
    IF SY-SUBRC NE 0.
*     Not Related posting item
      CONTINUE.
    ENDIF.

    READ TABLE LT_POSTED_BDR ASSIGNING FIELD-SYMBOL(<L_POSTED_BDR>)
                             WITH KEY VGBEL = <L_POSTING_BDR>-VGBEL
                                      VGPOS = <L_POSTING_BDR>-VGPOS
                             BINARY SEARCH.
    IF SY-SUBRC NE 0.
      INSERT <L_POSTING_BDR> INTO TABLE LT_POSTED_BDR.
    ELSE.
*     Update Data
      <L_POSTED_BDR>-BASE_AMT = <L_POSTING_BDR>-BASE_AMT.
      <L_POSTED_BDR>-TAX_AMT  = <L_POSTING_BDR>-TAX_AMT.
    ENDIF.

  ENDLOOP.

* Assign Output
  CT_POSTED_BDR = LT_POSTED_BDR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CLEAR_POSTING_DATA
*----------------------------------------------------------------------*
*  Clear Posting data when Commit
*  - This data is used since tcode CRMS4_BIL_BDR_GEN can post multiple
*    BDR documents from 1 execution and the posting BDR data cannot be
*    seen on following up posting BDR, therefore, the internal table
*    GT_POSTING_BDR is used.
*  - Function RV_BDR_POST is called in UPDATE TASK by SAP logic.
*    therefore, VBRP data is not updated and cannot be read from
*    IN UPDATE function paramters.
*----------------------------------------------------------------------*
FORM F_CLEAR_POSTING_DATA .
  CLEAR: GT_POSTING_BDR,
         GT_POSTING_ITEM,
         GT_POSTING_BILL.
ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ASSIGN_TAX_AMT_SVC
*----------------------------------------------------------------------*
*  Assign Tax Amount from Service Contract
*----------------------------------------------------------------------*
FORM F_ASSIGN_TAX_AMT_SVC  USING  US_KOMP  TYPE  KOMP
                                  US_XKOMV TYPE  KOMV_INDEX
                         CHANGING CF_XKWERT TYPE KOMV-KWERT.

  DATA:
    LT_BILLPLAN   TYPE  TT_BILLPLAN,
    LT_POSTED_BDR TYPE  TT_POSTED_BDR.

  DATA:
    LF_OBJECT_ID     TYPE  CRMS4D_BILLREQ_I-OBJECT_ID,
    LF_RECORD_NO     TYPE  CRMS4D_BILLREQ_I-RECORD_NO,
    LF_CURR_BASE_AMT TYPE  TS_POSTED_BDR-BASE_AMT,
    LF_CURR_TAX_AMT  TYPE  TS_POSTED_BDR-TAX_AMT,
    LF_BASE_AMT      TYPE  TS_POSTED_BDR-BASE_AMT,
    LF_TAX_AMT       TYPE  TS_POSTED_BDR-TAX_AMT,
    LF_NEW_TOTAL_TAX TYPE  TS_POSTED_BDR-TAX_AMT.


* --------------------------------
* Assign Key data
* --------------------------------
* Assign Reference Billing Plan Key
  LF_OBJECT_ID = US_KOMP-VGBEL.
  LF_RECORD_NO = US_KOMP-VGPOS.
* Assign Current Tax amount for this posting
  LF_CURR_BASE_AMT = US_XKOMV-KAWRT.
  LF_CURR_TAX_AMT  = CF_XKWERT.

* --------------------------------
* Get List of Billing Plan Related
* --------------------------------
  PERFORM F_GET_BILLPLAN  USING  LF_OBJECT_ID
                                 LF_RECORD_NO
                        CHANGING LT_BILLPLAN.
  IF LT_BILLPLAN IS INITIAL.
*   No Billing plan related
    RETURN.
  ENDIF.

* --------------------------------
* Get Already generated BDR amount on Related Billing Plan
* --------------------------------
  PERFORM F_GET_POSTED_BDR  USING  LT_BILLPLAN
                          CHANGING LT_POSTED_BDR.
  IF LT_POSTED_BDR IS INITIAL.
*   No Posted BDR
    RETURN.
  ENDIF.

* --------------------------------
* Use BDR Tax Amount if found
* --------------------------------
  READ TABLE LT_POSTED_BDR ASSIGNING FIELD-SYMBOL(<L_POSTED_BDR>)
                           WITH KEY VGBEL = US_KOMP-VGBEL
                                    VGPOS = US_KOMP-VGPOS
                           BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    CF_XKWERT = <L_POSTED_BDR>-TAX_AMT.
    RETURN.
  ENDIF.

* --------------------------------
* Sum all posted tax amount
* --------------------------------
  LOOP AT LT_POSTED_BDR ASSIGNING <L_POSTED_BDR>.
    LF_BASE_AMT = LF_BASE_AMT + <L_POSTED_BDR>-BASE_AMT.
    LF_TAX_AMT  = LF_TAX_AMT  + <L_POSTED_BDR>-TAX_AMT.
  ENDLOOP.

* --------------------------------
* Calculate New Total tax
* --------------------------------
  LF_BASE_AMT = LF_BASE_AMT + LF_CURR_BASE_AMT.
  LF_TAX_AMT  = LF_TAX_AMT  + LF_CURR_TAX_AMT.
  LF_NEW_TOTAL_TAX = LF_BASE_AMT * US_XKOMV-KBETR / 1000.

* --------------------------------
* Assgin Diff Tax amount
* --------------------------------
  IF LF_NEW_TOTAL_TAX NE LF_TAX_AMT.
    CF_XKWERT = CF_XKWERT + ( LF_NEW_TOTAL_TAX - LF_TAX_AMT ).
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ASSIGN_TAX_AMT_BDR
*----------------------------------------------------------------------*
*  Assign Tax Amount from reference BDR
*----------------------------------------------------------------------*
FORM F_ASSIGN_TAX_AMT_BDR  USING  US_KOMP  TYPE  KOMP
                                  US_XKOMV TYPE  KOMV_INDEX
                         CHANGING CF_XKWERT TYPE KOMV-KWERT.

* Get Condition value from reference BDR
  SELECT C~KWERT
    FROM VBRP AS A
           INNER JOIN VBRK AS B
             ON  B~VBELN = A~VBELN
           INNER JOIN PRCD_ELEMENTS AS C
             ON  C~KNUMV = B~KNUMV
             AND C~KPOSN = A~POSNR
             AND C~KSCHL = @US_XKOMV-KSCHL
   WHERE A~VBELN EQ @US_KOMP-VGBEL
     AND A~POSNR EQ @US_KOMP-VGPOS
   ORDER BY C~STUNR ASCENDING,
            C~ZAEHK ASCENDING
    INTO @DATA(LF_KWERT)
      UP TO 1 ROWS.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Output
  CF_XKWERT = LF_KWERT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ASSIGN_TAX_AMT_SVO
*----------------------------------------------------------------------*
*  Assign Tax Amount from Service Order
*----------------------------------------------------------------------*
FORM F_ASSIGN_TAX_AMT_SVO  USING  US_KOMP  TYPE  KOMP
                                  US_XKOMV TYPE  KOMV_INDEX
                         CHANGING CF_XKWERT TYPE KOMV-KWERT.


  DATA:
    LT_ORDER_ITEM TYPE  TT_ORDER_ITEM,
    LT_POSTED_BDR TYPE  TT_POSTED_BDR.

  DATA:
    LF_OBJECT_ID     TYPE  CRMS4D_BILLREQ_I-OBJECT_ID,
    LF_CURR_BASE_AMT TYPE  TS_POSTED_BDR-BASE_AMT,
    LF_CURR_TAX_AMT  TYPE  TS_POSTED_BDR-TAX_AMT,
    LF_BASE_AMT      TYPE  TS_POSTED_BDR-BASE_AMT,
    LF_TAX_AMT       TYPE  TS_POSTED_BDR-TAX_AMT,
    LF_NEW_TOTAL_TAX TYPE  TS_POSTED_BDR-TAX_AMT.


* --------------------------------
* Assign Key data
* --------------------------------
* Assign Reference Service Order
  LF_OBJECT_ID = US_KOMP-VGBEL.
* Assign Current Tax amount for this posting
  LF_CURR_BASE_AMT = US_XKOMV-KAWRT.
  LF_CURR_TAX_AMT  = CF_XKWERT.

* --------------------------------
* Get List of Service Order Items Related
* --------------------------------
  PERFORM F_GET_ORDER_ITEMS  USING  LF_OBJECT_ID
                           CHANGING LT_ORDER_ITEM.
  IF LT_ORDER_ITEM IS INITIAL.
*   No Order Item related
    RETURN.
  ENDIF.

* --------------------------------
* Get Already generated BDR amount on Related Service Order Item
* --------------------------------
  PERFORM F_GET_POSTED_BDR_SVO  USING  LT_ORDER_ITEM
                              CHANGING LT_POSTED_BDR.
  IF LT_POSTED_BDR IS INITIAL.
*   No Posted BDR
    RETURN.
  ENDIF.

* --------------------------------
* Use BDR Tax Amount if found
* --------------------------------
  READ TABLE LT_POSTED_BDR ASSIGNING FIELD-SYMBOL(<L_POSTED_BDR>)
                           WITH KEY VGBEL = US_KOMP-VGBEL
                                    VGPOS = US_KOMP-VGPOS
                           BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    CF_XKWERT = <L_POSTED_BDR>-TAX_AMT.
    RETURN.
  ENDIF.

* --------------------------------
* Sum all posted tax amount
* --------------------------------
  LOOP AT LT_POSTED_BDR ASSIGNING <L_POSTED_BDR>.
    LF_BASE_AMT = LF_BASE_AMT + <L_POSTED_BDR>-BASE_AMT.
    LF_TAX_AMT  = LF_TAX_AMT  + <L_POSTED_BDR>-TAX_AMT.
  ENDLOOP.

* --------------------------------
* Calculate New Total tax
* --------------------------------
  LF_BASE_AMT = LF_BASE_AMT + LF_CURR_BASE_AMT.
  LF_TAX_AMT  = LF_TAX_AMT  + LF_CURR_TAX_AMT.
  LF_NEW_TOTAL_TAX = LF_BASE_AMT * US_XKOMV-KBETR / 1000.

* --------------------------------
* Assgin Diff Tax amount
* --------------------------------
  IF LF_NEW_TOTAL_TAX NE LF_TAX_AMT.
    CF_XKWERT = CF_XKWERT + ( LF_NEW_TOTAL_TAX - LF_TAX_AMT ).
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_ORDER_ITEMS
*----------------------------------------------------------------------*
*  Get list of Related Service Order Items
*----------------------------------------------------------------------*
FORM F_GET_ORDER_ITEMS  USING  UF_OBJECT_ID TYPE CRMS4D_SERV_I-OBJECT_ID
                      CHANGING CT_ORDER_ITEM TYPE  TT_ORDER_ITEM.

* Initialize Output
  CLEAR: CT_ORDER_ITEM.

* Get Related Order Items
  SELECT OBJTYPE_H,
         OBJECT_ID,
         NUMBER_INT
    FROM CRMS4D_SERV_I
   WHERE OBJTYPE_H  EQ @GC_ORDER
     AND OBJECT_ID  EQ @UF_OBJECT_ID
   ORDER BY OBJECT_ID ASCENDING,
            OBJTYPE_H ASCENDING,
            NUMBER_INT ASCENDING
    INTO TABLE @CT_ORDER_ITEM.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_POSTED_BDR_SVO
*----------------------------------------------------------------------*
*  Get Posted BDR Data on related Service Order Items
*----------------------------------------------------------------------*
FORM F_GET_POSTED_BDR_SVO  USING  UT_ORDER_ITEM TYPE TT_ORDER_ITEM
                         CHANGING CT_POSTED_BDR TYPE  TT_POSTED_BDR.

  DATA:
    LT_POSTED_BDR  TYPE  TT_POSTED_BDR.


* Initialize Output
  CLEAR: CT_POSTED_BDR.

* Get Posted BDR Data on Related Billing plan
  SELECT A~VGBEL,
         A~VGPOS,
         A~VBELN,
         A~POSNR,
         A~KZWI3,
         A~MWSBP
    FROM @UT_ORDER_ITEM AS X  ##ITAB_KEY_IN_SELECT
           INNER JOIN VBRP AS A
             ON  A~VGBEL EQ X~OBJECT_ID
             AND A~VGPOS EQ X~NUMBER_INT
           INNER JOIN VBRK AS B
             ON  B~VBELN = A~VBELN
   WHERE B~BDR_STATUS NE 'D'
   ORDER BY A~VGBEL ASCENDING,
            A~VGPOS ASCENDING,
            A~VBELN ASCENDING,
            A~POSNR ASCENDING
    INTO TABLE @LT_POSTED_BDR.
  IF SY-SUBRC NE 0.
    CLEAR LT_POSTED_BDR.
  ENDIF.

* Add Posting document into the list
  LOOP AT GT_POSTING_BDR ASSIGNING FIELD-SYMBOL(<L_POSTING_BDR>).

*   Only related
    READ TABLE UT_ORDER_ITEM TRANSPORTING NO FIELDS
                             WITH KEY OBJECT_ID  = <L_POSTING_BDR>-VGBEL
                                      NUMBER_INT = <L_POSTING_BDR>-VGPOS
                             BINARY SEARCH.
    IF SY-SUBRC NE 0.
*     Not Related posting item
      CONTINUE.
    ENDIF.

    READ TABLE LT_POSTED_BDR ASSIGNING FIELD-SYMBOL(<L_POSTED_BDR>)
                             WITH KEY VGBEL = <L_POSTING_BDR>-VGBEL
                                      VGPOS = <L_POSTING_BDR>-VGPOS
                             BINARY SEARCH.
    IF SY-SUBRC NE 0.
      INSERT <L_POSTING_BDR> INTO TABLE LT_POSTED_BDR.
    ELSE.
*     Update Data
      <L_POSTED_BDR>-BASE_AMT = <L_POSTING_BDR>-BASE_AMT.
      <L_POSTED_BDR>-TAX_AMT  = <L_POSTING_BDR>-TAX_AMT.
    ENDIF.

  ENDLOOP.

* Assign Output
  CT_POSTED_BDR = LT_POSTED_BDR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ASSIGN_TAX_AMT_NOREF
*----------------------------------------------------------------------*
*  Assign Tax Amount for no reference scenario
*  - Service order items within a document
*----------------------------------------------------------------------*
FORM F_ASSIGN_TAX_AMT_NOREF  USING US_KOMK  TYPE  KOMK
                                   US_KOMP  TYPE  KOMP
                                   US_XKOMV TYPE  KOMV_INDEX
                          CHANGING CF_XKWERT TYPE KOMV-KWERT
                                   CF_GUID   TYPE CRMT_OBJECT_GUID.

  DATA:
    LT_POSTED_ITEM  TYPE  TT_POSTED_ITEM.

  DATA:
    LF_CURR_BASE_AMT TYPE  TS_POSTED_ITEM-BASE_AMT,
    LF_CURR_TAX_AMT  TYPE  TS_POSTED_ITEM-TAX_AMT,
    LF_BASE_AMT      TYPE  TS_POSTED_ITEM-BASE_AMT,
    LF_TAX_AMT       TYPE  TS_POSTED_ITEM-TAX_AMT,
    LF_NEW_TOTAL_TAX TYPE  TS_POSTED_ITEM-TAX_AMT.


* Initialize Output
  CLEAR CF_GUID.

* Only Amount found
  IF CF_XKWERT IS INITIAL.
    RETURN.
  ENDIF.

* -----------------------
* Get Reference GUID
* -----------------------
  PERFORM F_GET_REFGUID  USING  US_KOMK-BELNR
                       CHANGING CF_GUID.
  IF CF_GUID IS INITIAL.
    RETURN.
  ENDIF.
* Assign Current Tax amount for this posting
  LF_CURR_BASE_AMT = US_XKOMV-KAWRT.
  LF_CURR_TAX_AMT  = CF_XKWERT.

* --------------------------------
* Get Already generated Item
* --------------------------------
  PERFORM F_GET_POSTED_ITEM_SVO  USING  CF_GUID
                                        US_KOMP-KPOSN
                                        LF_CURR_BASE_AMT
                               CHANGING LT_POSTED_ITEM.
  IF LT_POSTED_ITEM IS INITIAL.
*   No Posted BDR
    RETURN.
  ENDIF.

* --------------------------------
* Use Item Tax Amount if found
* --------------------------------
  READ TABLE LT_POSTED_ITEM ASSIGNING FIELD-SYMBOL(<L_POSTED_ITEM>)
                            WITH KEY HEADER_GUID = CF_GUID
                                     NUMBER_INT  = US_KOMP-KPOSN
                            BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    CF_XKWERT = <L_POSTED_ITEM>-TAX_AMT.
    RETURN.
  ENDIF.

* --------------------------------
* Sum all posted tax amount
* --------------------------------
  LOOP AT LT_POSTED_ITEM ASSIGNING <L_POSTED_ITEM>.
    LF_BASE_AMT = LF_BASE_AMT + <L_POSTED_ITEM>-BASE_AMT.
    LF_TAX_AMT  = LF_TAX_AMT  + <L_POSTED_ITEM>-TAX_AMT.
  ENDLOOP.

* --------------------------------
* Calculate New Total tax
* --------------------------------
  LF_BASE_AMT = LF_BASE_AMT + LF_CURR_BASE_AMT.
  LF_TAX_AMT  = LF_TAX_AMT  + LF_CURR_TAX_AMT.
  LF_NEW_TOTAL_TAX = LF_BASE_AMT * US_XKOMV-KBETR / 1000.

* --------------------------------
* Assgin Diff Tax amount
* --------------------------------
  IF LF_NEW_TOTAL_TAX NE LF_TAX_AMT.
    CF_XKWERT = CF_XKWERT + ( LF_NEW_TOTAL_TAX - LF_TAX_AMT ).
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_REFGUID
*----------------------------------------------------------------------*
*  Get Reference GUID
*----------------------------------------------------------------------*
FORM F_GET_REFGUID  USING  UF_BELNR  TYPE  KOMK-BELNR
                  CHANGING CF_GUID TYPE CRMT_OBJECT_GUID.

  DATA:
    LT_GUID       TYPE  CRMT_OBJECT_GUID_TAB.

  DATA:
    LF_FNAME TYPE  CHAR50 VALUE '(SAPLCRM_PRIDOC_COM_OW)GS_COMADM_H_COM'.

  FIELD-SYMBOLS:
    <L_COMADM_H>  TYPE  CRMT_COMADM_H_COM.


* Initialize Output
  CLEAR CF_GUID.

* ---------------------------
* In case, changing document
* ---------------------------
  IF UF_BELNR IS NOT INITIAL.
*   Get Object GUID
    CALL FUNCTION 'CRM_ORDERADM_H_GUID_GET_OB'
      EXPORTING
        IV_OBJECT_ID       = UF_BELNR
        IV_OBJECT_TYPE     = GC_ORDER
      IMPORTING
        ET_ORDERADM_H_GUID = LT_GUID
      EXCEPTIONS
        RECORD_NOT_FOUND   = 1
        OTHERS             = 2.
    IF SY-SUBRC <> 0.
      RETURN.
    ENDIF.
    READ TABLE LT_GUID INTO CF_GUID
                       INDEX 1.
    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.
* ---------------------------
* In case, creating document
* ---------------------------
  ELSE.
    ASSIGN (LF_FNAME) TO <L_COMADM_H>.
    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.
    CF_GUID = <L_COMADM_H>-HEADER.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_POSTED_ITEM_SVO
*----------------------------------------------------------------------*
*  Get Posted BDR Data on related Service Order Items
*----------------------------------------------------------------------*
FORM F_GET_POSTED_ITEM_SVO  USING  UF_GUID  TYPE  TS_POSTED_ITEM-HEADER_GUID
                                   UF_NUMBER_INT TYPE TS_POSTED_ITEM-NUMBER_INT
                                   UF_BASE_AMT TYPE TS_POSTED_ITEM-BASE_AMT
                          CHANGING CT_POSTED_ITEM TYPE  TT_POSTED_ITEM.

  DATA:
    LT_POSTED_ITEM  TYPE  TT_POSTED_ITEM.


* Initialize Output
  CLEAR: CT_POSTED_ITEM.

* Get Posted Item Data
  SELECT A~HEADER_GUID,
         A~NUMBER_INT,
         A~NET_VALUE_I,
         A~TAX_AMOUNT_I
    FROM CRMS4D_SERV_I AS A
   WHERE A~HEADER_GUID EQ @UF_GUID
   ORDER BY A~HEADER_GUID ASCENDING,
            A~NUMBER_INT ASCENDING
    INTO TABLE @LT_POSTED_ITEM.
  IF SY-SUBRC NE 0.
    CLEAR LT_POSTED_ITEM.
  ENDIF.
* Reset if Base amount is changing
  DELETE LT_POSTED_ITEM WHERE NUMBER_INT EQ UF_NUMBER_INT
                          AND BASE_AMT NE UF_BASE_AMT.  "#EC CI_SORTSEQ

* Add Posting document into the list
  LOOP AT GT_POSTING_ITEM ASSIGNING FIELD-SYMBOL(<L_POSTING_ITEM>)
                          WHERE HEADER_GUID EQ UF_GUID.

    READ TABLE LT_POSTED_ITEM ASSIGNING FIELD-SYMBOL(<L_POSTED_ITEM>)
                              WITH KEY HEADER_GUID = <L_POSTING_ITEM>-HEADER_GUID
                                       NUMBER_INT  = <L_POSTING_ITEM>-NUMBER_INT
                              BINARY SEARCH.
    IF SY-SUBRC NE 0.
*     Ignore if Base amount is changing
      IF <L_POSTING_ITEM>-NUMBER_INT EQ UF_NUMBER_INT AND
         <L_POSTING_ITEM>-BASE_AMT NE UF_BASE_AMT.
        CONTINUE.
      ENDIF.
      INSERT <L_POSTING_ITEM> INTO TABLE LT_POSTED_ITEM.
    ELSE.
*     Update Data
      <L_POSTED_ITEM>-BASE_AMT = <L_POSTING_ITEM>-BASE_AMT.
      <L_POSTED_ITEM>-TAX_AMT  = <L_POSTING_ITEM>-TAX_AMT.
    ENDIF.

  ENDLOOP.

* Assign Output
  CT_POSTED_ITEM = LT_POSTED_ITEM.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ADJUST_TAX_AMT_SVO
*----------------------------------------------------------------------*
*  Adjust Tax Amount from rounding issue (Service Order Scenario )
*----------------------------------------------------------------------*
FORM F_ADJUST_TAX_AMT_SVO  USING  US_KOMK  TYPE  KOMK
                                  US_KOMP  TYPE  KOMP
                                  US_XKOMV TYPE  KOMV_INDEX
                         CHANGING CF_XKWERT TYPE KOMV-KWERT.

  CONSTANTS:
    LC_BILL TYPE  KOMK-VBTYP VALUE 'M',
    LC_BDR  TYPE  KOMK-VBTYP VALUE 'EBDR'.


* Only Amount found
  IF CF_XKWERT IS INITIAL.
    RETURN.
  ENDIF.

  IF ( US_KOMK-VBTYP EQ LC_BILL  OR
       US_KOMK-VBTYP EQ LC_BDR ) AND
     US_KOMK-BELNR IS NOT INITIAL.

*   --------------------------------
*   In case Billing Documents,
*    - Adjust Item to have sum tax equal to tax calculated
*      from sum amount at header
*   --------------------------------
    PERFORM F_ASSIGN_TAX_AMT_BILL USING  US_KOMK
                                         US_KOMP
                                         US_XKOMV
                                CHANGING CF_XKWERT.

*   Collect posting new tax data
    READ TABLE GT_POSTING_BILL ASSIGNING FIELD-SYMBOL(<L_POSTING_BILL>)
                               WITH KEY VBELN = US_KOMK-BELNR
                                        POSNR = US_KOMP-KPOSN
                               BINARY SEARCH.
    IF SY-SUBRC EQ 0.
*     Updating values
      <L_POSTING_BILL>-BASE_AMT = US_XKOMV-KAWRT.
      <L_POSTING_BILL>-TAX_AMT  = CF_XKWERT.
    ELSE.
      INSERT VALUE #( VBELN    = US_KOMK-BELNR
                      POSNR    = US_KOMP-KPOSN
                      BASE_AMT = US_XKOMV-KAWRT
                      TAX_AMT  = CF_XKWERT )
             INTO TABLE GT_POSTING_BILL.

*     Initiate Clear value when commit
      PERFORM F_CLEAR_POSTING_DATA ON COMMIT.
    ENDIF.

  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ASSIGN_TAX_AMT_BILL
*----------------------------------------------------------------------*
*  Assign Tax Amount within Billing document
*----------------------------------------------------------------------*
FORM F_ASSIGN_TAX_AMT_BILL  USING  US_KOMK  TYPE  KOMK
                                   US_KOMP  TYPE  KOMP
                                   US_XKOMV TYPE  KOMV_INDEX
                          CHANGING CF_XKWERT TYPE KOMV-KWERT.

  DATA:
    LT_POSTED_BILL  TYPE  TT_POSTED_BILL.

  DATA:
    LF_CURR_BASE_AMT TYPE  TS_POSTED_BILL-BASE_AMT,
    LF_CURR_TAX_AMT  TYPE  TS_POSTED_BILL-TAX_AMT,
    LF_BASE_AMT      TYPE  TS_POSTED_BILL-BASE_AMT,
    LF_TAX_AMT       TYPE  TS_POSTED_BILL-TAX_AMT,
    LF_NEW_TOTAL_TAX TYPE  TS_POSTED_BILL-TAX_AMT.


* Only Amount found
  IF CF_XKWERT IS INITIAL.
    RETURN.
  ENDIF.

* Assign Current Tax amount for this posting
  LF_CURR_BASE_AMT = US_XKOMV-KAWRT.
  LF_CURR_TAX_AMT  = CF_XKWERT.

* --------------------------------
* Get Already generated Item
* --------------------------------
  PERFORM F_GET_POSTED_ITEM_BILL  USING US_KOMK-BELNR
                                        US_KOMP-KPOSN
                                        LF_CURR_BASE_AMT
                               CHANGING LT_POSTED_BILL.
  IF LT_POSTED_BILL IS INITIAL.
*   No Posted Billing Item
    RETURN.
  ENDIF.

* --------------------------------
* Use Item Tax Amount if found
* --------------------------------
  READ TABLE LT_POSTED_BILL ASSIGNING FIELD-SYMBOL(<L_POSTED_BILL>)
                            WITH KEY VBELN = US_KOMK-BELNR
                                     POSNR = US_KOMP-KPOSN
                            BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    CF_XKWERT = <L_POSTED_BILL>-TAX_AMT.
    RETURN.
  ENDIF.

* --------------------------------
* Sum all posted tax amount
* --------------------------------
  LOOP AT LT_POSTED_BILL ASSIGNING <L_POSTED_BILL>.
    LF_BASE_AMT = LF_BASE_AMT + <L_POSTED_BILL>-BASE_AMT.
    LF_TAX_AMT  = LF_TAX_AMT  + <L_POSTED_BILL>-TAX_AMT.
  ENDLOOP.

* --------------------------------
* Calculate New Total tax
* --------------------------------
  LF_BASE_AMT = LF_BASE_AMT + LF_CURR_BASE_AMT.
  LF_TAX_AMT  = LF_TAX_AMT  + LF_CURR_TAX_AMT.
  LF_NEW_TOTAL_TAX = LF_BASE_AMT * US_XKOMV-KBETR / 1000.

* --------------------------------
* Assgin Diff Tax amount
* --------------------------------
  IF LF_NEW_TOTAL_TAX NE LF_TAX_AMT.
    CF_XKWERT = CF_XKWERT + ( LF_NEW_TOTAL_TAX - LF_TAX_AMT ).
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_POSTED_ITEM_BILL
*----------------------------------------------------------------------*
*  Get Posted BDR Data on related Service Order Items
*----------------------------------------------------------------------*
FORM F_GET_POSTED_ITEM_BILL  USING  UF_VBELN  TYPE  TS_POSTED_BILL-VBELN
                                    UF_POSNR  TYPE TS_POSTED_BILL-POSNR
                                    UF_BASE_AMT TYPE TS_POSTED_BILL-BASE_AMT
                           CHANGING CT_POSTED_BILL TYPE  TT_POSTED_BILL.

  DATA:
    LT_POSTED_BILL  TYPE  TT_POSTED_BILL.


* Initialize Output
  CLEAR: CT_POSTED_BILL.

* Get Posted Item Data
  SELECT A~VBELN,
         A~POSNR,
         A~KZWI3,
         A~MWSBP
    FROM VBRP AS A
   WHERE A~VBELN EQ @UF_VBELN
   ORDER BY A~VBELN ASCENDING,
            A~POSNR ASCENDING
    INTO TABLE @LT_POSTED_BILL.
  IF SY-SUBRC NE 0.
    CLEAR LT_POSTED_BILL.
  ENDIF.
* Reset if Base amount is changing
  DELETE LT_POSTED_BILL WHERE POSNR EQ UF_POSNR
                          AND BASE_AMT NE UF_BASE_AMT.  "#EC CI_SORTSEQ

* Add Posting document into the list
  LOOP AT GT_POSTING_BILL ASSIGNING FIELD-SYMBOL(<L_POSTING_BILL>)
                          WHERE VBELN EQ UF_VBELN.

    READ TABLE LT_POSTED_BILL ASSIGNING FIELD-SYMBOL(<L_POSTED_BILL>)
                              WITH KEY VBELN = <L_POSTING_BILL>-VBELN
                                       POSNR = <L_POSTING_BILL>-POSNR
                              BINARY SEARCH.
    IF SY-SUBRC NE 0.
*     Ignore if Base amount is changing
      IF <L_POSTING_BILL>-POSNR EQ UF_POSNR AND
         <L_POSTING_BILL>-BASE_AMT NE UF_BASE_AMT.
        CONTINUE.
      ENDIF.
      INSERT <L_POSTING_BILL> INTO TABLE LT_POSTED_BILL.
    ELSE.
*     Update Data
      <L_POSTED_BILL>-BASE_AMT = <L_POSTING_BILL>-BASE_AMT.
      <L_POSTED_BILL>-TAX_AMT  = <L_POSTING_BILL>-TAX_AMT.
    ENDIF.

  ENDLOOP.

* Assign Output
  CT_POSTED_BILL = LT_POSTED_BILL.

ENDFORM.
