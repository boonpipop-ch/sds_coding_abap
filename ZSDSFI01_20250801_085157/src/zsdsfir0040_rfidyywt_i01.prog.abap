*&---------------------------------------------------------------------*
*&  INCLUDE I_RFIDYYWT_I01                                             *
*&---------------------------------------------------------------------*
*&                                                                     *
*& form routines used in RFIDYYWT                                      *
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form I01_initialization                                        *
*&---------------------------------------------------------------------*
*       initialization : before the first screen-displaying .
*       and when the key changes ( country + output group )
*----------------------------------------------------------------------*
FORM I01_INITIALIZATION.

  G_REPID = SY-REPID.

  G_VARID = SY-SLSET.

* Open / close the selection-screen blocks :
  IF WA_POINT = 'initialization'.
    P_CB1 = SPACE.
  ELSE.
    P_CB1 = 'X'.
  ENDIF.
  P_CB2 = 'X'.
  P_CB3 = 'X'.
  P_CB4 = 'X'.
  P_CB5 = 'X'.
  P_CB6 = 'X'.
  P_CB7 = 'X'.
*NOTE 545125 begin
  P_CB8 = 'X'.
*NOTE 545125 end
* Set Text & Icon for Pushbutton c1 - c8, o1 - o8

  CONCATENATE ICON_COLLAPSE: TEXT-001 INTO PUSHB_C1,
                             TEXT-002 INTO PUSHB_C2,
                             TEXT-003 INTO PUSHB_C3,
                             TEXT-004 INTO PUSHB_C4,
                             TEXT-005 INTO PUSHB_C5,
                             TEXT-011 INTO PUSHB_C6,
                             TEXT-012 INTO PUSHB_C7,
*NOTE 545125 begin
                             TEXT-013 INTO PUSHB_C8.
*NOTE 545125 end


  CONCATENATE ICON_EXPAND:   TEXT-001 INTO PUSHB_O1,
                             TEXT-002 INTO PUSHB_O2,
                             TEXT-003 INTO PUSHB_O3,
                             TEXT-004 INTO PUSHB_O4,
                             TEXT-005 INTO PUSHB_O5,
                             TEXT-011 INTO PUSHB_O6,
                             TEXT-012 INTO PUSHB_O7,
*NOTE 545125 begin
                             TEXT-013 INTO PUSHB_O8.
*NOTE 545125 end

* 14092001 - BoI Additional selection for specific countries
  CONCATENATE ICON_SELECTION TEXT-601 INTO P_LAND_E.
  CLEAR F_ACTIVE_ADD_SEL_FIELD.
* 14092001 - EoI Additional selection for specific countries

  CLEAR WA_HWAER.                      " wa to test local currencies

* reinitialize all the selection fields if the output group key has
* changed
  IF WA_POINT EQ 'check_output_gp'.
    REFRESH P_BUKRS.
    REFRESH P_GJAHR.
    REFRESH P_WITHT.
    REFRESH P_WTHCD.
    REFRESH P_QSSKZ.
    REFRESH P_LIFNR.
    REFRESH P_KUNNR.
    REFRESH P_BLART.
    REFRESH P_BELNR.
    REFRESH P_CPUDT.
    REFRESH P_XBLNR.
    REFRESH P_BUPLA.
    REFRESH P_GSBER.
    REFRESH P_REGIO.
    REFRESH P_HKONT.
    REFRESH P_KOART.
*Note 545125 begin
*    REFRESH p_umskz.
*Note 545125 end
    REFRESH P_CERT.
    REFRESH P_ISSU.
    REFRESH P_STCD1.
    REFRESH P_STCD2.
*
    CLEAR P_BUKRS.
    CLEAR P_GJAHR.
    CLEAR P_WITHT.
    CLEAR P_WTHCD.
    CLEAR P_QSSKZ.
    CLEAR P_LIFNR.
    CLEAR P_KUNNR.
    CLEAR P_BLART.
    CLEAR P_BELNR.
    CLEAR P_CPUDT.
    CLEAR P_XBLNR.
    CLEAR P_BUPLA.
    CLEAR P_GSBER.
    CLEAR P_REGIO.
    CLEAR P_HKONT.
    CLEAR P_KOART.
*Note 545125 begin
*    CLEAR p_umskz.
*Note 545125 end
    CLEAR P_CERT.
    CLEAR P_ISSU.
    CLEAR P_STCD1.
    CLEAR P_STCD2.

  ENDIF.


*set 1SAP as default variant for the screen output :

  PERFORM STANDARD_ALV_VARIANTE_EXIST USING C_COMPANY .

  PERFORM STANDARD_ALV_VARIANTE_EXIST USING C_VEND_TAX.

  PERFORM STANDARD_ALV_VARIANTE_EXIST USING C_FIITEMS.

  PERFORM STANDARD_ALV_VARIANTE_EXIST USING C_ERRORS.



ENDFORM.                               " I01_initialization.

*&---------------------------------------------------------------------*
*&      Form I01_control_sel_screen_pbo.                               *
*&---------------------------------------------------------------------*
*       prepare the screen before output :
*----------------------------------------------------------------------*
FORM I01_CONTROL_SEL_SCREEN_PBO.

  LOOP AT SCREEN.
* 14092001 - BoI Additional selection for specific countries
    IF SCREEN-NAME = 'P_LAND_E'.
      IF F_ACTIVE_ADD_SEL_FIELD IS INITIAL.
        SCREEN-INVISIBLE = 1.
      ELSE.
        SCREEN-INVISIBLE = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
* 14092001 - EoI Additional selection for specific countries
*    IF SCREEN-NAME = 'PUSHB_C1'.
    PERFORM CLOSE_BLOCK USING P_CB1 'MC1' SPACE.
    PERFORM CLOSE_BLOCK USING P_CB1 'MO1' 'X'.
*    ELSEIF SCREEN-NAME = 'P_cb2'.
    PERFORM CLOSE_BLOCK USING P_CB2 'MC2' SPACE.
    PERFORM CLOSE_BLOCK USING P_CB2 'MO2' 'X'.
*    ELSEIF SCREEN-NAME = 'P_cb3'.
    PERFORM CLOSE_BLOCK USING P_CB3 'MC3' SPACE.
    PERFORM CLOSE_BLOCK USING P_CB3 'MO3' 'X'.
*    ELSEIF SCREEN-NAME = 'P_cb4'.
    PERFORM CLOSE_BLOCK USING P_CB4 'MC4' SPACE.
    PERFORM CLOSE_BLOCK USING P_CB4 'MO4' 'X'  .
*    ELSEIF SCREEN-NAME = 'P_cb5'.
    PERFORM CLOSE_BLOCK USING P_CB5 'MC5' SPACE.
    PERFORM CLOSE_BLOCK USING P_CB5 'MO5' 'X'  .
*    ELSEIF SCREEN-NAME = 'P_cb6'.
    PERFORM CLOSE_BLOCK USING P_CB6 'MC6' SPACE.
    PERFORM CLOSE_BLOCK USING P_CB6 'MO6' 'X'  .
*    ELSEIF SCREEN-NAME = 'P_cb7'.
    PERFORM CLOSE_BLOCK USING P_CB7 'MC7' SPACE.
    PERFORM CLOSE_BLOCK USING P_CB7 'MO7' 'X'  .
*Note 545125 begin
    PERFORM CLOSE_BLOCK USING P_CB8 'MC8' SPACE.
    PERFORM CLOSE_BLOCK USING P_CB8 'MO8' 'X'  .
*Note 545125 end

*    ENDIF.

  ENDLOOP.


  CASE G_SSCR_UCOMM.
*NOTE 545125 begin
    WHEN 'UCOMM_O8'.                   "Open Block 1.1
      SET CURSOR FIELD 'P_GL'.
*NOTE 545125 end

    WHEN 'UCOMM_O1'.                   "Open Block 1
      SET CURSOR FIELD 'P_BUKRS-LOW'.

    WHEN 'UCOMM_O2'.                   "Open Block 2
      SET CURSOR FIELD 'P_BLART'.

    WHEN 'UCOMM_O3'.                   "Open Block 3
      SET CURSOR FIELD 'P_TEST'.

    WHEN 'UCOMM_O4'.                   "Open Block 4
      SET CURSOR FIELD 'P_ALCUR'.

    WHEN 'UCOMM_O5'.                   "Open Block 5
      SET CURSOR FIELD 'P_SENDER'.

    WHEN 'UCOMM_O6'.                   "Open Block 6
      SET CURSOR FIELD 'P_FILE'.

    WHEN 'UCOMM_O7'.                   "Open Block 7
      SET CURSOR FIELD 'P_UPGM'.

    WHEN OTHERS.
      SET CURSOR FIELD G_CURSOR_FIELD.
  ENDCASE.


ENDFORM.                               " I01_control_sel_screen_pbo.

*---------------------------------------------------------------------*
*       FORM close_block                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  value(u_close_block)                                          *
*  -->  u_modify_id                                                   *
*  -->  u_convert                                                     *
*---------------------------------------------------------------------*
FORM CLOSE_BLOCK USING    VALUE(U_CLOSE_BLOCK) LIKE P_CB1
                          U_MODIFY_ID LIKE SCREEN-GROUP1
                          U_CONVERT.

  IF NOT U_CONVERT IS INITIAL.
    IF  U_CLOSE_BLOCK IS INITIAL.
      U_CLOSE_BLOCK = 'X'.
    ELSE.
      CLEAR U_CLOSE_BLOCK.
    ENDIF.
  ENDIF.

  IF ( SCREEN-GROUP1 = U_MODIFY_ID ) AND
     ( NOT U_CLOSE_BLOCK IS INITIAL ).
    SCREEN-ACTIVE = '0'.
    MODIFY SCREEN.
  ENDIF.

ENDFORM.                               " CLOSE_BLOCK



*&---------------------------------------------------------------------*
*&      Form  I01_CHECK_COMPANY_CODE                                   *
*&---------------------------------------------------------------------*
*       Checks company codes
*----------------------------------------------------------------------*
FORM I01_CHECK_COMPANY_CODES.

*------- Check if there are selected values ----------------------------
  DESCRIBE TABLE P_BUKRS LINES SY-TFILL.
  CHECK SY-TFILL NE 0.                 " only if selected comp.codes

*------- Check if comp. code low and high values exist -----------------
  LOOP AT P_BUKRS.

    IF NOT P_BUKRS-LOW IS INITIAL.
      SELECT SINGLE * FROM T001 WHERE BUKRS = P_BUKRS-LOW.
      IF SY-SUBRC <> 0.
        SET CURSOR FIELD 'P_BUKRS-LOW'.
        MESSAGE E165(F5) WITH P_BUKRS-LOW.
      ENDIF.
* 17092001 - BoD Allow same company in different country selection
*     IF t001-land1 NE p_land1.
*       SET CURSOR FIELD 'P_BUKRS-LOW'.
*       MESSAGE e025(id_wt) WITH p_bukrs-low p_land1.
*     ENDIF.
* 17092001 - EoD Allow same company in different country selection
    ENDIF.
    IF NOT P_BUKRS-HIGH IS INITIAL.
      SELECT SINGLE * FROM T001 WHERE BUKRS = P_BUKRS-HIGH.
      IF SY-SUBRC <> 0.
        SET CURSOR FIELD 'P_BUKRS-HIGH'.
        MESSAGE E165(F5) WITH P_BUKRS-HIGH.
      ENDIF.
* 17092001 - BoD Allow same company in different country selection
*     IF t001-land1 NE p_land1.
*       SET CURSOR FIELD 'P_BUKRS-HIGH'.
*       MESSAGE e025(id_wt) WITH p_bukrs-high p_land1.
*     ENDIF.
* 17092001 - EoD Allow same company in different country selection
    ENDIF.

* It's necessary to work in a common currency : either all companies
* have the same or an alternative currency must be chosen.

    IF WA_HWAER IS INITIAL.
      MOVE SPACE TO WA_ALCUR_NEEDED.
      MOVE T001-WAERS  TO WA_HWAER.
    ELSE.
      IF WA_HWAER <> T001-WAERS AND
         P_ALCUR IS INITIAL.
        WA_ALCUR_NEEDED = 'X'.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.                               " I01_CHECK_COMPANY_CODES

*&---------------------------------------------------------------------*
*&      Form  I01_CHECK_WT_TYPES
*&---------------------------------------------------------------------*
*       Checks WT tax types
*----------------------------------------------------------------------*
FORM I01_CHECK_WT_TYPES.

  IF P_WITHT NE SPACE.

    SELECT * FROM T059P WHERE WITHT IN P_WITHT.
    ENDSELECT.

    IF SY-SUBRC NE '0'.
      SET CURSOR FIELD 'P_WITHT'.
      MESSAGE E456(FR).
    ENDIF.

  ENDIF.

ENDFORM.                               " I01_CHECK_WT_TYPES

*&---------------------------------------------------------------------*
*&      Form  CHECK_WT_CODES
*&---------------------------------------------------------------------*
*       Checks the selected WT codes and define the selected range
*----------------------------------------------------------------------*
FORM I01_CHECK_WT_CODES.

  IF P_QSSKZ NE SPACE.                 " Old WT functionality

    SELECT * FROM T059Q WHERE QSSKZ IN P_QSSKZ.
    ENDSELECT.

    IF SY-SUBRC NE '0'.
      SET CURSOR FIELD 'P_QSSKZ'.
      MESSAGE E151(FR).
    ENDIF.

  ENDIF.


  IF P_WTHCD NE SPACE.                 " New WT functionality

    IF P_WITHT NE SPACE.

      SELECT * FROM T059Z  WHERE WITHT     IN P_WITHT AND
                                 WT_WITHCD IN P_WTHCD.
      ENDSELECT.
    ELSE.
      SELECT * FROM T059Z  WHERE WT_WITHCD IN P_WTHCD.
      ENDSELECT.
    ENDIF.

    IF SY-SUBRC NE '0'.
      SET CURSOR FIELD 'P_WTHCD'.
      MESSAGE E151(FR).
    ENDIF.

  ENDIF.

ENDFORM.                               " I01_CHECK_WT_CODES

*&---------------------------------------------------------------------*
*&      Form  I01_CHECK_BUS_PLACE                                      *
*&---------------------------------------------------------------------*
*&      Checks if the business place component is active for the given
*&      country and if the business places exist for the company code
*&---------------------------------------------------------------------*
FORM I01_CHECK_BUS_PLACE.

  IF NOT P_BUPLA-LOW IS INITIAL OR
     NOT P_BUPLA-HIGH IS INITIAL.

    SELECT * FROM T001 WHERE BUKRS IN P_BUKRS.

      CALL FUNCTION 'J_1BSA_COMPONENT_ACTIVE'
        EXPORTING
          BUKRS                = T001-BUKRS
          COMPONENT            = '**'
        EXCEPTIONS
          COMPONENT_NOT_ACTIVE = 1.
      IF SY-SUBRC = 1.
        SET CURSOR FIELD 'P_BUPLA-LOW'.
        MESSAGE E001(ID_WT).
      ELSE.

        IF NOT P_BUPLA-LOW IS INITIAL.
          SELECT SINGLE * FROM J_1BBRANCH WHERE BUKRS  = T001-BUKRS
                                          AND BRANCH = P_BUPLA-LOW.
          IF SY-SUBRC <> 0.
            SET CURSOR FIELD 'P_BUPLA-LOW'.
            MESSAGE E562(ICC-KR) WITH P_BUPLA-LOW T001-BUKRS.
          ENDIF.
        ENDIF.
        IF NOT P_BUPLA-HIGH IS INITIAL.
          SELECT SINGLE * FROM J_1BBRANCH WHERE BUKRS  = T001-BUKRS
                                        AND BRANCH = P_BUPLA-HIGH.
          IF SY-SUBRC <> 0.
            SET CURSOR FIELD 'P_BUPLA-HIGH'.
            MESSAGE E562(ICC-KR) WITH P_BUPLA-HIGH T001-BUKRS.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDSELECT.
  ENDIF.

ENDFORM.                               " I01_CHECK_BUS_PLACE


*&---------------------------------------------------------------------*
*&      Form I01_control_sel_screen_pai.                               *
*&---------------------------------------------------------------------*
*       check the screen after input :
*----------------------------------------------------------------------*
FORM I01_CONTROL_SEL_SCREEN_PAI.

* Fields compatibility checks :

* Activation checkboxes and related parameters :
  IF NOT P_FILE IS INITIAL.
    IF P_FNM1 IS INITIAL.
      SET CURSOR FIELD 'P_FNM1'.
      MESSAGE E126(FR).
    ENDIF.
  ENDIF.

  IF P_FILE IS INITIAL.
    IF NOT P_FNM1 IS INITIAL OR
       NOT P_NULL IS INITIAL.
      MESSAGE I029.
      SET CURSOR FIELD 'P_FILE'.
    ENDIF.
  ENDIF.

  IF P_FOR1 IS INITIAL.
    IF NOT P_F1SP IS INITIAL.
      MESSAGE I030.
      SET CURSOR FIELD 'P_FOR1'.
    ENDIF.
  ENDIF.

  IF P_FOR2 IS INITIAL.
    IF NOT P_F2SP IS INITIAL.
      MESSAGE I030.
      SET CURSOR FIELD 'P_FOR2'.
    ENDIF.
  ENDIF.

* test run or updates ?
  IF NOT P_TEST IS INITIAL.
    IF NOT P_STOR IS INITIAL.
      P_STOR = ' '.
      MESSAGE I036.
      SET CURSOR FIELD 'P_TEST'.
    ENDIF.
  ENDIF.

* certificate numbers storage : only if numbering is activated.
  IF P_TEST IS INITIAL AND
     NOT P_STOR IS INITIAL AND
     P_NUMB IS INITIAL.
    SET CURSOR FIELD 'P_NUMB'.
    MESSAGE E037.
  ENDIF.

* numbering : only 1 option is possible .
*  if P_TEST is initial.
  IF NOT P_NUMB IS INITIAL.
    IF P_RGNB IS INITIAL AND
       P_NEXT IS INITIAL.
      SET CURSOR FIELD 'P_NUMB'.
      MESSAGE E033.
    ENDIF.
  ENDIF.

  IF NOT P_NUMB IS INITIAL.
    IF NOT P_RGNB IS INITIAL AND
       NOT P_NEXT IS INITIAL.
      SET CURSOR FIELD 'P_NUMB'.
      MESSAGE E033.
    ENDIF.
  ENDIF.

  IF P_NUMB IS INITIAL.
    IF NOT P_RGNB IS INITIAL OR
       NOT P_NEXT IS INITIAL.
      MESSAGE I034.
      SET CURSOR FIELD 'P_NUMB'.
    ENDIF.
  ENDIF.
*  endif.

* company codes must have the same local currency, otherwise an
* alternative currency must be chosen .
  IF WA_ALCUR_NEEDED = 'X'.
    SET CURSOR FIELD 'P_ALCUR'.


*    MESSAGE e017.
    MESSAGE I017.

  ENDIF.


* storage of screen fields into the internal table IDWTGLOB :

  WA_IDWTGLOB-WT_SC_FILE_ACT  =  P_FILE.
  WA_IDWTGLOB-WT_SC_FILE_NAME =  P_FNM1.
  WA_IDWTGLOB-WT_SC_REP_FROM  =  P_BUDAT-LOW.
  WA_IDWTGLOB-WT_SC_REP_TO    =  P_BUDAT-HIGH.
  WA_IDWTGLOB-WT_SC_REP_PER   =  P_REPT.
  WA_IDWTGLOB-WT_SC_ALTCURR   =  P_ALCUR.
  WA_IDWTGLOB-WT_SC_CONVDATE  =  P_EXCDT.
  WA_IDWTGLOB-WT_SC_REF_INTER =  P_REFI.


  IF NOT P_FOR1 IS INITIAL.
    WA_IDWTGLOB-WT_SC_FORM_SP1  =  P_F1SP.
    WA_IDWTGLOB-WT_SC_FORM_CP1  =  P_F1CP.
    WA_IDWTGLOB-WT_SC_FORM_PR1  =  P_F1PR.
    WA_IDWTGLOB-WT_SC_FORM_IM1  =  P_F1PI.
    WA_IDWTGLOB-WT_SC_FORM_ACT1 =  P_FOR1.
  ENDIF.

  IF NOT P_FOR2 IS INITIAL.
    WA_IDWTGLOB-WT_SC_FORM_SP2  =  P_F2SP.
    WA_IDWTGLOB-WT_SC_FORM_CP2  =  P_F2CP.
    WA_IDWTGLOB-WT_SC_FORM_PR2  =  P_F2PR.
    WA_IDWTGLOB-WT_SC_FORM_IM2  =  P_F2PI.
    WA_IDWTGLOB-WT_SC_FORM_ACT2 =  P_FOR2.
  ENDIF.

  WA_IDWTGLOB-WT_SC_LIST_ACT1    =  P_LIS1.
  WA_IDWTGLOB-WT_SC_LIST_ACT2    =  P_LIS2.
  WA_IDWTGLOB-WT_SC_LIST_ACT3    =  P_LIS3.
  WA_IDWTGLOB-WT_SC_LIST_ACT4    =  P_LIS4.
  WA_IDWTGLOB-WT_OG_LIST_VR1     =  P_VAR1.
  WA_IDWTGLOB-WT_OG_LIST_VR2     =  P_VAR2.
  WA_IDWTGLOB-WT_OG_LIST_VR3     =  P_VAR3.
  WA_IDWTGLOB-WT_OG_LIST_VR4     =  P_VAR4.

  WA_IDWTGLOB-WT_SC_CHECKS_ACT    =  P_CHECKS.
  WA_IDWTGLOB-WT_SC_EXCL_WT0      =  P_EWT0.
  WA_IDWTGLOB-WT_SC_EXCL_BUPLA    =  P_EBUPLA.
  WA_IDWTGLOB-WT_SC_SEPA_REVERSED =  P_REVERS.
  WA_IDWTGLOB-WT_OG_PARTNER_NOTC  =  P_NOTC.
  WA_IDWTGLOB-WT_SC_USER_ACT      =  P_UPGM.

  WA_IDWTGLOB-WT_SC_NUMB_ACT     =  P_NUMB.
  WA_IDWTGLOB-WT_SC_CERT_STOR    =  P_STOR.
  WA_IDWTGLOB-WT_SC_TEST_RUN     =  P_TEST.
  WA_IDWTGLOB-WT_SC_RANGE_NB     =  P_RGNB.
  WA_IDWTGLOB-WT_SC_NEXT_NB      =  P_NEXT.
  WA_IDWTGLOB-WT_SC_FILE_NULL    =  P_NULL.
  WA_IDWTGLOB-WT_SC_UNIT         =  P_UNIT.


  WA_IDWTGLOB-WT_SC_NEW_ISSUDT   =  P_ISDT.
  WA_IDWTGLOB-WT_SC_BATCH_HEAD   =  P_BATCHH.
  WA_IDWTGLOB-PGMNAME            =  G_REPID.
  WA_IDWTGLOB-LAND1              =  P_LAND1.
  WA_IDWTGLOB-WT_REPORT_EXEC_DATE = SY-DATUM.

* Find the ISO code of the country :
  SELECT SINGLE * FROM T005  WHERE LAND1 EQ P_LAND1.
  IF SY-SUBRC = 0.
    WA_IDWTGLOB-INTCA         =  T005-INTCA.
  ELSE.
    SET CURSOR FIELD 'P_LAND1'.
    MESSAGE I026 WITH 'T005' P_LAND1.
  ENDIF.

* 17092001 - BoD Remove feature (seems make more confuse than useful)
**if there is no selected company code, fill the table with the
**companies belonging to the selected country :
*
* DESCRIBE TABLE p_bukrs LINES sy-tfill.
* IF sy-tfill EQ 0.                    " only if no selected comp.code
*
*   SELECT * FROM t001.
*     IF sy-subrc EQ 0.
*       IF t001-land1 EQ p_land1.
*         p_bukrs-sign     = 'I'.
*         p_bukrs-option   = 'EQ'.
*         p_bukrs-low      = t001-bukrs.
*         APPEND p_bukrs.
*       ENDIF.
*     ENDIF.
*   ENDSELECT.
* ENDIF.
* 17092001 - EoD Remove feature (seems make more confuse than useful)

  GET CURSOR FIELD G_CURSOR_FIELD.

  G_SSCR_UCOMM = SSCRFIELDS-UCOMM.


* react to User-commands :
  CASE SSCRFIELDS-UCOMM.
*NOTE 545125 begin
    WHEN 'UCOMM_O8'.                   "Open Block 1.1
      CLEAR P_CB8.
    WHEN 'UCOMM_C8'.                   "Close Block 1.1
      P_CB8 = 'X'.
*NOTE 545125 end

* 14092001 - BoI Additional selection for specific countries
    WHEN 'UCOMM_E1'.                   "Request for additional fields
      PERFORM GET_ADD_SEL_FIELDS.
* 14092001 - EoI Additional selection for specific countries
    WHEN 'UCOMM_O1'.                   "Open Block 1
      CLEAR P_CB1.
    WHEN 'UCOMM_C1'.                   "Close Block 1
      P_CB1 = 'X'.
    WHEN 'UCOMM_O2'.                   "Open Block 2
      CLEAR P_CB2.
    WHEN 'UCOMM_C2'.                   "Close Block 2
      P_CB2 = 'X'.
    WHEN 'UCOMM_O3'.                   "Open Block 3
      CLEAR P_CB3.
    WHEN 'UCOMM_C3'.                   "Close Block 3
      P_CB3 = 'X'.
    WHEN 'UCOMM_O4'.                   "Open Block 4
      CLEAR P_CB4.
    WHEN 'UCOMM_C4'.                   "Close Block 4
      P_CB4 = 'X'.
    WHEN 'UCOMM_O5'.                   "Open Block 5
      IF NOT T059ID01-WT_OG_FORM_NM1 IS INITIAL  OR
         NOT T059ID01-WT_OG_FORM_NM2 IS INITIAL.  " only if necessary
        CLEAR P_CB5.
      ELSE.
        MESSAGE W005.
      ENDIF.
    WHEN 'UCOMM_C5'.                   "Close Block 5
      P_CB5 = 'X'.
    WHEN 'UCOMM_O6'.                   "Open Block 6
      IF NOT T059ID01-WT_OG_FILE IS INITIAL.       " only if necessary
        CLEAR P_CB6.
      ELSE.
        MESSAGE W006.
      ENDIF.
    WHEN 'UCOMM_C6'.                   "Close Block 6
      P_CB6 = 'X'.
    WHEN 'UCOMM_O7'.                   "Open Block 7
      IF NOT T059ID01-WT_OG_USER_PGM IS INITIAL.   " only if necessary
        CLEAR P_CB7.
      ELSE.
        MESSAGE W007.
      ENDIF.
    WHEN 'UCOMM_C7'.                   "Close Block 7
      P_CB7 = 'X'.


    WHEN 'CON1'.                       "List configuration
      PERFORM CONFIG_LIST USING '1'.
    WHEN 'CON2'.                       "List configuration
      PERFORM CONFIG_LIST USING '2'.
    WHEN 'CON3'.                       "List configuration
      PERFORM CONFIG_LIST USING '3'.
    WHEN 'CON4'.                       "List configuration
      PERFORM CONFIG_LIST USING '4'.

  ENDCASE.


ENDFORM.                               " I01_control_sel_screen_pai.


*---------------------------------------------------------------------*
*       FORM config_list                                              *
*---------------------------------------------------------------------*
*       Call Popup 100 and handle user-command                        *
*---------------------------------------------------------------------*
FORM CONFIG_LIST USING LIST_NUMBER.


  WA_IDWTGLOB_CON  = WA_IDWTGLOB.
  WA_IDWTGLOB_CON-WT_SC_LIST_ACT1 = SPACE.
  WA_IDWTGLOB_CON-WT_SC_LIST_ACT2 = SPACE.
  WA_IDWTGLOB_CON-WT_SC_LIST_ACT3 = SPACE.
  WA_IDWTGLOB_CON-WT_SC_LIST_ACT4 = SPACE.
  WA_IDWTGLOB_CON-WT_SC_FORM_ACT1 = SPACE.
  WA_IDWTGLOB_CON-WT_SC_FORM_ACT2 = SPACE.
  WA_IDWTGLOB_CON-WT_SC_FILE_ACT  = SPACE.


  IF LIST_NUMBER = 1.
    WA_IDWTGLOB_CON-WT_SC_LIST_ACT1 = 'X'.
  ENDIF.
  IF LIST_NUMBER = 2.
    WA_IDWTGLOB_CON-WT_SC_LIST_ACT2 = 'X'.
  ENDIF.
  IF LIST_NUMBER = 3.
    WA_IDWTGLOB_CON-WT_SC_LIST_ACT3 = 'X'.
  ENDIF.
  IF LIST_NUMBER = 4.
    WA_IDWTGLOB_CON-WT_SC_LIST_ACT4 = 'X'.
  ENDIF.

*  call the dispatcher only for list configuration :
  MOVE LIST_NUMBER  TO WA_IDWTGLOB_CON-DISP_METHOD.
  APPEND WA_IDWTGLOB_CON.

  CALL FUNCTION 'IDWT_DISPATCHER'
    EXPORTING
      I_GLOB              = WA_IDWTGLOB_CON
    CHANGING
      C_COMPCD            = I_IDWTCOMPCD[]
      C_PARTNER           = I_IDWTPARTNER[]
      C_FIDOC             = I_IDWTFIDOC[]
      C_ERROR             = I_IDWTERROR[]
    EXCEPTIONS
      ERROR_FORM          = 1
      ERROR_FILE          = 2
      ERROR_SCREEN_OUTPUT = 3
      ERROR_MULTI         = 4
      OTHERS              = 5.
  IF SY-SUBRC <> 0.
*         already handled by the output dispatcher
  ENDIF.

*  ENDIF.

ENDFORM.                    "config_list

*&---------------------------------------------------------------------*
*&      Form  I01_check_output_group
*&---------------------------------------------------------------------*
*&      reads the customizing table for the selected output group
*&---------------------------------------------------------------------*
FORM I01_CHECK_OUTPUT_GROUP.

  IF P_OUTG NE WA_IDWTGLOB-WT_OUTPUT_GRP OR
     P_CUST NE WA_IDWTGLOB-WT_OUTPUT_CUST.

    IF NOT WA_IDWTGLOB-WT_OUTPUT_GRP IS INITIAL. " real change of keys
      WA_POINT = 'check_output_gp'.
      PERFORM I01_INITIALIZATION.
    ENDIF.

    SELECT SINGLE * FROM  IDWTNAVIG
        WHERE  PGMNAME        = 'RFIDYYWT' "sy-repid Comment by Teerasithi.
        AND    WT_OUTPUT_GRP  = P_OUTG.

    IF SY-SUBRC NE 0.
      MESSAGE E367(FR) WITH 'IDWTNAVIG'.
    ENDIF.                             .

    SELECT SINGLE * FROM  T059ID01
        WHERE  WT_OUTPUT_GRP   = P_OUTG
        AND    WT_OUTPUT_CUST  = P_CUST.

    IF SY-SUBRC NE 0.
      MESSAGE E020(ID_WT).
    ENDIF.

    SELECT SINGLE * FROM T059ID01T
        WHERE  SPRAS = SY-LANGU
        AND    WT_OUTPUT_GRP   = P_OUTG
        AND    WT_OUTPUT_CUST  = P_CUST.


*-------- transfer data from the customizing table to the internal table


    MOVE-CORRESPONDING T059ID01T   TO WA_IDWTGLOB.
    MOVE-CORRESPONDING T059ID01   TO WA_IDWTGLOB.
*--------- Define the Data Source --------------------------------------

    CLEAR WA_DATA_SRC.

    IF T059ID01-WT_OG_REP_DOC = '1'.   " rep.items= vendor invoices
      WA_DATA_SRC = 'DS1'.

    ELSEIF T059ID01-WT_OG_REP_DOC = '2'.  " rep.items= vendor payments
      WA_DATA_SRC = 'DS2'.

    ELSEIF T059ID01-WT_OG_REP_DOC = '3'." rep.items = cust.invoices
      WA_DATA_SRC = 'DS3'.

    ELSEIF T059ID01-WT_OG_REP_DOC = '4'." rep.items= cust.paymts
      WA_DATA_SRC = 'DS4'.

    ELSEIF T059ID01-WT_OG_REP_DOC = '5'." rep.items =
*         vendor invoices and payments depending on the WT posting time.
      WA_DATA_SRC = 'DS5'.

    ELSEIF T059ID01-WT_OG_REP_DOC = '6'." rep.items =
*         cust. invoices and payments depending on the WT posting time.
      WA_DATA_SRC = 'DS6'.

    ELSEIF T059ID01-WT_OG_REP_DOC = '7'." rep.items = ven inv on clrg dt
*    vendor invoices based on clearing date. note 787062
      WA_DATA_SRC = 'DS7'.
    ENDIF.

    IF WA_DATA_SRC EQ SPACE.
      MESSAGE E003(ID_WT).
    ELSE.
      WA_IDWTGLOB-WT_DATA_SOURCE = WA_DATA_SRC.
    ENDIF.                             .

*------------ initialize parameters on the screen :---------------------


    P_F1NM  =   WA_IDWTGLOB-WT_OG_FORM_TI1.       " form title 1
    P_F2NM  =   WA_IDWTGLOB-WT_OG_FORM_TI2.       " form title 2

* modify screen attributes according to customizing :

    LOOP AT SCREEN.

      IF WA_IDWTGLOB-WT_OG_FORM_NM1 IS INITIAL.
        IF SCREEN-NAME = 'P_F1NM'  OR
           SCREEN-NAME = 'P_F1CP' OR
           SCREEN-NAME = 'P_F1PR' OR
           SCREEN-NAME = 'P_FOR1' OR
           SCREEN-NAME = 'P_F1SP' OR
           SCREEN-NAME = 'P_F1PI'.

          SCREEN-INPUT = 0.
          SCREEN-OUTPUT = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

      IF WA_IDWTGLOB-WT_OG_FORM_NM2 IS INITIAL.
        IF SCREEN-NAME = 'P_F2NM'  OR
           SCREEN-NAME = 'P_F2CP' OR
           SCREEN-NAME = 'P_F2PR' OR
           SCREEN-NAME = 'P_FOR2' OR
           SCREEN-NAME = 'P_F2SP' OR
           SCREEN-NAME = 'P_F2PI'.

          SCREEN-INPUT = 0.
          SCREEN-OUTPUT = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

    ENDLOOP.



  ENDIF.



ENDFORM.                               " I01_check_output_group

*&---------------------------------------------------------------------*
*&      Form  I01_CHECK_REPORTING_PERIOD
*&---------------------------------------------------------------------*
*       a defined range is mandatory ( 2 dates )
*----------------------------------------------------------------------*
FORM I01_CHECK_REPORTING_PERIOD.

  IF P_BUDAT-LOW  IS INITIAL.
    MESSAGE E703(7Q).
  ELSE.
    DESCRIBE TABLE P_BUDAT LINES SY-TFILL.
    IF SY-TFILL EQ 1.                  " if only From-date selected
      IF P_BUDAT-HIGH IS INITIAL.
        WA_TO_DATE = P_BUDAT-LOW.
        CLEAR P_BUDAT. REFRESH P_BUDAT.
        P_BUDAT-SIGN     = 'I'.
        P_BUDAT-OPTION   = 'BT'.
        P_BUDAT-LOW      = WA_TO_DATE.
        P_BUDAT-HIGH     = SY-DATUM.
        APPEND P_BUDAT.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " I01_CHECK_REPORTING_PERIOD

*&---------------------------------------------------------------------*
*&      Form  I01_CREATE_REPORTING_DATA
*&---------------------------------------------------------------------*
*       create and  complete the internal tables
*----------------------------------------------------------------------*
FORM I01_CREATE_REPORTING_DATA.

* 0 - initializations :
  MOVE 0 TO WA_PARTNER_SEQ_NB.

* 1 - Check the amount limits and reject the non-taxable items :

*     perform I01_limits_checks.

* 2 - Sort the main table :

  SORT MAN_QSTRMAIN BY BUKRS BUPLA PARTNO_TYPE PARTNERNO WT_REVERSED
                       WT_PERIOD WITHT WT_WITHCD GJAHR BELNR BUZEI
                       ITEM_REF.

* 3 - Create the internal tables IDWTxxxx per company, per
*     vendor/type/code, per FI line item

  LOOP AT MAN_QSTRMAIN.

    MOVE MAN_QSTRMAIN  TO WA_QSTRMAIN.

*     convert from local into alternative currency :
    PERFORM I01_CONVERT_LOCAL_TO_ALTERNAT.

*     express the amounts with the selected factor :
    PERFORM I01_EXPRESS_WITH_FACTOR.


    AT NEW BUKRS.
      PERFORM I01_INITIALIZE_COMPANY.
    ENDAT.

    AT NEW BUPLA.
      PERFORM I01_INITIALIZE_BUSPLACE.
    ENDAT.

    AT NEW PARTNERNO.
      IF WA_QSTRMAIN-PARTNO_TYPE = SPACE.
        IF WA_QSTRMAIN-KOART = 'D'.                        "NOTE 430704
          PERFORM I01_INITIALIZE_CUSTOMER.
        ELSE.
          PERFORM I01_INITIALIZE_VENDOR.
        ENDIF.
      ELSEIF WA_QSTRMAIN-PARTNO_TYPE = '1'.
        PERFORM I01_INITIALIZE_BUPART.
      ENDIF.
    ENDAT.

    AT NEW WT_REVERSED.
      PERFORM I01_INITIALIZE_REVERSED.
    ENDAT.

    AT NEW WT_PERIOD.
      PERFORM I01_INITIALIZE_PERIOD.
    ENDAT.

    AT NEW WITHT.
      PERFORM I01_INITIALIZE_WTTYPE.
    ENDAT.

    AT NEW WT_WITHCD.
      PERFORM I01_INITIALIZE_WTCODE.
    ENDAT.

*                   For each FI line item :
    MOVE-CORRESPONDING WA_QSTRMAIN  TO WA_IDWTFIDOC.
    PERFORM I01_COMPLETE_FIDOC.
    PERFORM I01_STORE_IDWTFIDOC.
    PERFORM I01_SUM_AMOUNTS_PARTNER.

    AT END OF WT_WITHCD.
      PERFORM I01_STORE_IDWTPARTNER.
      PERFORM I01_SUM_AMOUNTS_COMPANY.
    ENDAT.

    AT END OF BUPLA.
      PERFORM I01_STORE_IDWTCOMPCD.
    ENDAT.

  ENDLOOP.



ENDFORM.                               " I01_CREATE_REPORTING_DATA

*-----------------------------------------------------------------------
*------- Complete Reporting tool 'Fi line items' table -----------------
*-----------------------------------------------------------------------
FORM I01_COMPLETE_FIDOC.

  DATA: L_QBSHB TYPE QBSHB,
        L_QSSHB TYPE QSSHB,
        L_QBSHH TYPE QBSHH.
* for Withholding tax posted at invoice time: the invoice amount picked
* up on the vendor item is net of Withholding tax ( the WT is already
* posted, so there is an item for it ) . In order to display always the
* invoice amount as the gross amount including WT, we reintegrate the
* WT inside the invoice amount.
* The base amounts of ALL withholding tax types have to be taken

  IF WA_IDWTFIDOC-ITEM_SOURCE = SPACE.                  " FI item
    WA_IDWTFIDOC-ITEM_REF(10)   = WA_IDWTFIDOC-BELNR.
    WA_IDWTFIDOC-ITEM_REF+11(3) = WA_IDWTFIDOC-BUZEI.
    WA_IDWTFIDOC-ITEM_REF+15(4) = WA_IDWTFIDOC-GJAHR.
    IF WA_IDWTFIDOC-WT_POSTM = '1'.
      CALL FUNCTION 'FI_WT_CALCULATE_DEDUCTION'           " NOTE 498660
        EXPORTING
          I_BUKRS    = WA_IDWTFIDOC-BUKRS
          I_BELNR    = WA_IDWTFIDOC-BELNR
          I_GJAHR    = WA_IDWTFIDOC-GJAHR
          I_BUZEI    = WA_IDWTFIDOC-BUZEI
          I_WT_POSTM = '1'
        CHANGING
          C_QBSHB    = L_QBSHB
          C_QSSHB    = L_QSSHB
          C_QBSHH    = L_QBSHH.

*      check: invoice or credit memo

      IF WA_IDWTFIDOC-WRBTR > 0.
        WA_IDWTFIDOC-DMBTR  = WA_IDWTFIDOC-DMBTR + ABS( L_QBSHH ).
        WA_IDWTFIDOC-WRBTR  = WA_IDWTFIDOC-WRBTR + ABS( L_QBSHB ).
**Note 1104452 Begins
        WA_IDWTFIDOC-WT_QSFHB = WA_IDWTFIDOC-DMBTR - WA_IDWTFIDOC-WT_QSSHB.
        WA_IDWTFIDOC-WT_QSFHH = WA_IDWTFIDOC-DMBTR - WA_IDWTFIDOC-WT_QSSHH.
**Note 1104452 Ends
      ELSE.
        WA_IDWTFIDOC-DMBTR  = WA_IDWTFIDOC-DMBTR - ABS( L_QBSHH ).
        WA_IDWTFIDOC-WRBTR  = WA_IDWTFIDOC-WRBTR - ABS( L_QBSHB ).
**Note 1104452 Begins
        WA_IDWTFIDOC-WT_QSFHB = WA_IDWTFIDOC-DMBTR - WA_IDWTFIDOC-WT_QSSHB.
        WA_IDWTFIDOC-WT_QSFHH = WA_IDWTFIDOC-DMBTR - WA_IDWTFIDOC-WT_QSSHH.
**Note 1104452 Ends
      ENDIF.
    ENDIF.
  ENDIF.

*    amounts sums :

  WA_IDWTFIDOC-WT_AMEXCLVAT1  = WA_IDWTFIDOC-WRBTR
                              - WA_IDWTFIDOC-WT_TAXHB.

  WA_IDWTFIDOC-WT_AMEXCLVAT   = WA_IDWTFIDOC-DMBTR
                              - WA_IDWTFIDOC-WT_TAXHH.

* Note 0434242 begin
*  IF wa_idwtfidoc-wt_qsfhh GE wa_idwtfidoc-wt_taxhh. "10/09/2001

  "10/09/2001
  WA_IDWTFIDOC-WT_AMREXMP     = WA_IDWTFIDOC-WT_QSFHH
                              - WA_IDWTFIDOC-WT_TAXHH.
*  ELSE.                                              "10/09/2001

*    wa_idwtfidoc-wt_amrexmp     = wa_idwtfidoc-wt_qsfhh.

*  ENDIF.                                             "10/09/2001
* Note 0434242 end

  IF WA_IDWTFIDOC-WT_QSFHB GE WA_IDWTFIDOC-WT_TAXHB.
    WA_IDWTFIDOC-WT_AMREXMP1    = WA_IDWTFIDOC-WT_QSFHB
                                - WA_IDWTFIDOC-WT_TAXHB.
  ELSE.
    WA_IDWTFIDOC-WT_AMREXMP1    = WA_IDWTFIDOC-WT_QSFHB.
  ENDIF.

  WA_IDWTFIDOC-WT_AMRBASE     = WA_IDWTFIDOC-WT_QSSHH
                              - WA_IDWTFIDOC-WT_TAXHH.

  WA_IDWTFIDOC-WT_AMRBASE1    = WA_IDWTFIDOC-WT_QSSHB
                              - WA_IDWTFIDOC-WT_TAXHB.

* Partner data will be exceptionnaly repeated at the FI line item level
* in order to allow customers to recognize the vendor when they are
* looking at the ALV list for FI documents :

  MOVE WA_IDWTPARTNER-WT_SHORT_ADDR TO WA_IDWTFIDOC-WT_SHORT_ADDR.

* issue date updated by screen entry :
  IF NOT WA_IDWTGLOB-WT_SC_NEW_ISSUDT IS INITIAL.
    MOVE WA_IDWTGLOB-WT_SC_NEW_ISSUDT TO WA_IDWTFIDOC-WT_ISSDT.
  ENDIF.

* BoI - Additional info could available on line item     "Enterprise
  MOVE WA_IDWTPARTNER-QSCOD TO WA_IDWTFIDOC-QSCOD.
* EoI - Additional info could available on line item     "Enterprise


ENDFORM.                               " I01_complete_fidoc.

*-----------------------------------------------------------------------
*------- Initialize Reporting tool 'Company codes' table ---------------
*        with company code data
*-----------------------------------------------------------------------
FORM I01_INITIALIZE_COMPANY.

  DATA: WA_ADDRESS_VALUE LIKE ADDR1_VAL,
        WA_ADDR1_TEXT    LIKE ADDR1_TEXT,
        WA_READ_TEXT     LIKE SZAD_FIELD-FLAG VALUE 'X'.

  MOVE-CORRESPONDING WA_QSTRMAIN  TO WA_IDWTCOMPCD.

  SELECT SINGLE * FROM T001 WHERE BUKRS = WA_QSTRMAIN-BUKRS.

  IF SY-SUBRC = 0.

    WA_IDWTCOMPCD-BUTXT     = T001-BUTXT.
    WA_IDWTCOMPCD-KTOPL     = T001-KTOPL.
    WA_IDWTCOMPCD-WAERS     = T001-WAERS.
    WA_IDWTCOMPCD-STCEG     = T001-STCEG.
    WA_IDWTCOMPCD-WT_NEWWT  = T001-WT_NEWWT.

    CLEAR: ADDR1_SEL, SADR.                                 "SADR40A

*   the address is the company's one if there's no business place :
    IF WA_QSTRMAIN-BUPLA IS INITIAL.

      ADDR1_SEL-ADDRNUMBER = T001-ADRNR.                    "SADR40A


      CALL FUNCTION 'ADDR_GET'
        EXPORTING
          ADDRESS_SELECTION = ADDR1_SEL
*         ADDRESS_GROUP     =
*         READ_SADR_ONLY    = ' '
          READ_TEXTS        = WA_READ_TEXT
        IMPORTING
          ADDRESS_VALUE     = WA_ADDRESS_VALUE
*         ADDRESS_ADDITIONAL_INFO       =
          RETURNCODE        = WA_SADR_RC
          ADDRESS_TEXT      = WA_ADDR1_TEXT
          SADR              = SADR
*        TABLES
*         ADDRESS_GROUPS    =
*         ERROR_TABLE       =
*         VERSIONS          =
        EXCEPTIONS
          PARAMETER_ERROR   = 1
          ADDRESS_NOT_EXIST = 2
          VERSION_NOT_EXIST = 3
          INTERNAL_ERROR    = 4
          OTHERS            = 5
        .                                                 "SADR40A

      IF SY-SUBRC <> 0.
        IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
          MESSAGE E013  WITH WA_QSTRMAIN-BUKRS
                          WA_IDWTCOMPCD-ADDRNUMBER SY-SUBRC.
        ELSE.
          CLEAR WA_ERROR.
          WA_ERROR-WT_MSGID       = 'ID_WT'.
          WA_ERROR-WT_MSGTY       = 'E'.
          WA_ERROR-WT_MSGNR       = 13.
          MESSAGE E013 WITH WA_QSTRMAIN-BUKRS
                            WA_IDWTCOMPCD-ADDRNUMBER SY-SUBRC
                       INTO WA_ERROR-WT_NATXT.
          PERFORM F01_FILL_IDWTERROR TABLES I_IDWTERROR[]
                                    USING WA_ERROR.
        ENDIF.
      ELSE.
        MOVE-CORRESPONDING WA_ADDRESS_VALUE TO WA_IDWTCOMPCD.
*        MOVE-CORRESPONDING wa_addr1_text TO wa_idwtcompcd.
        WA_IDWTCOMPCD-LANDX = WA_ADDR1_TEXT-LANDX.
        WA_IDWTCOMPCD-NATIO = WA_ADDR1_TEXT-NATIO.
        WA_IDWTCOMPCD-BEZEI = WA_ADDR1_TEXT-BEZEI.

      ENDIF.
    ENDIF.


    SELECT SINGLE * FROM T005  WHERE LAND1 EQ T001-LAND1.

    IF SY-SUBRC = 0.

      SELECT SINGLE * FROM T005T  WHERE LAND1 EQ T005-LAND1
                                    AND SPRAS EQ SY-LANGU.

      IF SY-SUBRC = 0.
        WA_IDWTCOMPCD-LANDX  = T005T-LANDX.
      ENDIF.

    ENDIF.

  ENDIF.

*  data relating to sender variant :

  SELECT SINGLE * FROM T001G WHERE BUKRS       = WA_IDWTCOMPCD-BUKRS
                              AND PROGRAMM     = SY-REPID
                              AND TXTID        = P_SENDER.

  IF SY-SUBRC = 0.

    WA_IDWTCOMPCD-TXTID     = T001G-TXTID.
    WA_IDWTCOMPCD-TXTKO     = T001G-TXTKO.
    WA_IDWTCOMPCD-TXTFU     = T001G-TXTFU.
    WA_IDWTCOMPCD-TXTUN     = T001G-TXTUN.
    WA_IDWTCOMPCD-TXTAB     = T001G-TXTAB.

  ENDIF.

* Company code Tax references :

  IF NOT WA_IDWTGLOB-WT_TAXREF1 IS INITIAL.
    SELECT SINGLE * FROM  T001Z    WHERE
                BUKRS      =  WA_IDWTCOMPCD-BUKRS
           AND  PARTY      =  WA_IDWTGLOB-WT_TAXREF1.

    IF SY-SUBRC = 0.
      MOVE  T001Z-PAVAL  TO WA_IDWTCOMPCD-WT_TAXREF1_VAL.
    ENDIF.
  ENDIF.

  IF NOT WA_IDWTGLOB-WT_TAXREF2 IS INITIAL.
    SELECT SINGLE * FROM  T001Z    WHERE
                BUKRS      =  WA_IDWTCOMPCD-BUKRS
           AND  PARTY      =  WA_IDWTGLOB-WT_TAXREF2.

    IF SY-SUBRC = 0.
      MOVE  T001Z-PAVAL  TO WA_IDWTCOMPCD-WT_TAXREF2_VAL.
    ENDIF.
  ENDIF.

  IF WA_IDWTCOMPCD-WT_TAXREF1_VAL IS INITIAL AND
     WA_IDWTCOMPCD-WT_TAXREF2_VAL IS INITIAL.

    SELECT SINGLE * FROM  T001Z    WHERE
                BUKRS      =  WA_IDWTCOMPCD-BUKRS
           AND  PARTY      =  'WT_TAX'.

    IF SY-SUBRC <> 0.
*        if wa_idwtglob-wt_sc_list_act4 is initial.
*           MESSAGE I027 WITH  wa_IDWTCOMPCD-bukrs .
*        else.
      IF NOT ( WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL ).
        CLEAR WA_ERROR.
        WA_ERROR-WT_MSGID       = 'ID_WT'.
        WA_ERROR-WT_MSGTY       = 'I'.
        WA_ERROR-WT_MSGNR       = 27.
        MESSAGE I027 WITH  WA_IDWTCOMPCD-BUKRS
                     INTO WA_ERROR-WT_NATXT.
        PERFORM F01_FILL_IDWTERROR TABLES I_IDWTERROR[]
                                   USING WA_ERROR.
      ENDIF.

    ELSE.
      MOVE  T001Z-PAVAL  TO WA_IDWTCOMPCD-WT_TAXREF1_VAL.
    ENDIF.

  ENDIF.

* EDI Identifier ( only if a file is required ):

  IF NOT P_FILE IS INITIAL.

    IF NOT WA_IDWTGLOB-WT_EDINO IS INITIAL.
      SELECT SINGLE * FROM  T001Z    WHERE
                  BUKRS      =  WA_IDWTCOMPCD-BUKRS
             AND  PARTY      =  WA_IDWTGLOB-WT_EDINO.

      IF SY-SUBRC = 0.
        MOVE  T001Z-PAVAL  TO WA_IDWTCOMPCD-WT_EDIREF_VAL.
      ENDIF.
    ENDIF.

    IF WA_IDWTCOMPCD-WT_EDIREF_VAL IS INITIAL.

      SELECT SINGLE * FROM  T001Z    WHERE
             BUKRS      = WA_IDWTCOMPCD-BUKRS
        AND  PARTY      = 'WT_EDI'.

      IF SY-SUBRC <> 0.
        IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
          MESSAGE I028 WITH  WA_IDWTCOMPCD-BUKRS .
        ELSE.
          CLEAR WA_ERROR.
          WA_ERROR-WT_MSGID       = 'ID_WT'.
          WA_ERROR-WT_MSGTY       = 'I'.
          WA_ERROR-WT_MSGNR       = 28.
          MESSAGE I028 WITH  WA_IDWTCOMPCD-BUKRS
                      INTO WA_ERROR-WT_NATXT.
          PERFORM F01_FILL_IDWTERROR TABLES I_IDWTERROR[]
                                    USING WA_ERROR.
        ENDIF.
      ELSE.
        MOVE  T001Z-PAVAL      TO WA_IDWTCOMPCD-WT_EDIREF_VAL.
      ENDIF.
    ENDIF.

  ENDIF.


*  initialization of all amounts :

  WA_IDWTCOMPCD-DMBTR          = 0.
  WA_IDWTCOMPCD-WT_TAXHH       = 0.
  WA_IDWTCOMPCD-WT_QSFHH       = 0.
  WA_IDWTCOMPCD-WT_QBSHH       = 0.
  WA_IDWTCOMPCD-WT_QSSHH       = 0.
  WA_IDWTCOMPCD-WT_AMREXMP     = 0.
  WA_IDWTCOMPCD-WT_AMRBASE     = 0.
  WA_IDWTCOMPCD-WT_AMEXCLVAT   = 0.
  WA_IDWTCOMPCD-SKNTO          = 0.


ENDFORM.                               " I01_initialize_company

*-----------------------------------------------------------------------
*------- Initialize Reporting tool 'Company codes' table ---------------
*        with business place data .
*-----------------------------------------------------------------------
FORM I01_INITIALIZE_BUSPLACE.

  DATA: WA_ADDRESS_VALUE LIKE ADDR1_VAL,
        WA_ADDR1_TEXT    LIKE ADDR1_TEXT.

  WA_IDWTCOMPCD-BUPLA = WA_QSTRMAIN-BUPLA.

  IF NOT WA_QSTRMAIN-BUPLA IS INITIAL.

    SELECT SINGLE * FROM J_1BBRANCH WHERE BUKRS  = T001-BUKRS
                                      AND BRANCH = WA_QSTRMAIN-BUPLA.
    IF SY-SUBRC <> 0.
      IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
        MESSAGE E562(ICC-KR) WITH WA_QSTRMAIN-BUPLA T001-BUKRS.
      ELSE.
        CLEAR WA_ERROR.
        WA_ERROR-WT_MSGID       = 'ICC-KR'.
        WA_ERROR-WT_MSGTY       = 'E'.
        WA_ERROR-WT_MSGNR       = 562.
        MESSAGE E562(ICC-KR) WITH WA_QSTRMAIN-BUPLA T001-BUKRS
                     INTO WA_ERROR-WT_NATXT.
        PERFORM F01_FILL_IDWTERROR TABLES I_IDWTERROR[]
                                  USING WA_ERROR.
      ENDIF.

    ELSE.                              " a branch exists :

*   branch-specific data.
      WA_IDWTCOMPCD-KR_REPRES = J_1BBRANCH-KR_REPRES.
      WA_IDWTCOMPCD-KR_TAXOFF = J_1BBRANCH-KR_TAXOFF.

      IF NOT J_1BBRANCH-KR_TAXOFF IS INITIAL.

        SELECT SINGLE  * FROM  TAXOFFICET
               WHERE  SPRAS      = SY-LANGU
               AND    TAXOFFICE  = J_1BBRANCH-KR_TAXOFF.

        IF SY-SUBRC = 0.
          WA_IDWTCOMPCD-KR_TAXOFF_NAME = TAXOFFICET-NAME.
        ENDIF.
      ENDIF.

*    tax references are those from the business place ( if present )

      IF NOT J_1BBRANCH-STCD1 IS INITIAL OR
         NOT J_1BBRANCH-STCD2 IS INITIAL.
        WA_IDWTCOMPCD-WT_TAXREF1_VAL = J_1BBRANCH-STCD1.
        WA_IDWTCOMPCD-WT_TAXREF2_VAL = J_1BBRANCH-STCD2.
      ENDIF.


*   the address is the business place's one :

      CLEAR: ADDR1_SEL, SADR.                               "SADR40A
      ADDR1_SEL-ADDRNUMBER = J_1BBRANCH-ADRNR.              "SADR40A

      CALL FUNCTION 'ADDR_GET'
        EXPORTING
          ADDRESS_SELECTION = ADDR1_SEL
*         ADDRESS_GROUP     =
*         READ_SADR_ONLY    = ' '
          READ_TEXTS        = 'X'
        IMPORTING
          ADDRESS_VALUE     = WA_ADDRESS_VALUE
*         ADDRESS_ADDITIONAL_INFO       =
          RETURNCODE        = WA_SADR_RC
          ADDRESS_TEXT      = WA_ADDR1_TEXT
          SADR              = SADR
*        TABLES
*         ADDRESS_GROUPS    =
*         ERROR_TABLE       =
*         VERSIONS          =
        EXCEPTIONS
          PARAMETER_ERROR   = 1
          ADDRESS_NOT_EXIST = 2
          VERSION_NOT_EXIST = 3
          INTERNAL_ERROR    = 4
          OTHERS            = 5
        .                                           "SADR40A
      IF SY-SUBRC <> 0.
        IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
          MESSAGE E016 WITH WA_QSTRMAIN-BUPLA
                     WA_IDWTCOMPCD-ADDRNUMBER  WA_SADR_RC.
        ELSE.
          CLEAR WA_ERROR.
          WA_ERROR-WT_MSGID       = 'ID-WT'.
          WA_ERROR-WT_MSGTY       = 'E'.
          WA_ERROR-WT_MSGNR       = 16.
          MESSAGE E016 WITH WA_QSTRMAIN-BUPLA
                     WA_IDWTCOMPCD-ADDRNUMBER  WA_SADR_RC
                               INTO WA_ERROR-WT_NATXT.
          PERFORM F01_FILL_IDWTERROR TABLES I_IDWTERROR[]
                               USING WA_ERROR.
        ENDIF.
      ELSE.
        MOVE-CORRESPONDING WA_ADDRESS_VALUE TO WA_IDWTCOMPCD.
        MOVE-CORRESPONDING WA_ADDR1_TEXT TO WA_IDWTCOMPCD.
      ENDIF.

    ENDIF.
  ENDIF.

*    initialization of all amounts :

  WA_IDWTCOMPCD-DMBTR          = 0.
  WA_IDWTCOMPCD-WT_TAXHH       = 0.
  WA_IDWTCOMPCD-WT_QSFHH       = 0.
  WA_IDWTCOMPCD-WT_QBSHH       = 0.
  WA_IDWTCOMPCD-WT_QSSHH       = 0.
  WA_IDWTCOMPCD-WT_AMREXMP     = 0.
  WA_IDWTCOMPCD-WT_AMRBASE     = 0.
  WA_IDWTCOMPCD-WT_AMEXCLVAT   = 0.
  WA_IDWTCOMPCD-SKNTO          = 0.


ENDFORM.                               " I01_initialize_busplace


*-----------------------------------------------------------------------
*------- Initialize Reporting tool 'Partner / WT types-codes' table with
*        Vendor data
*-----------------------------------------------------------------------
FORM I01_INITIALIZE_VENDOR.
  DATA: WA_ADDRESS_VALUE LIKE ADDR1_VAL,
        WA_ADDR1_TEXT    LIKE ADDR1_TEXT.


  WA_IDWTPARTNER-PARTNERNO = WA_QSTRMAIN-PARTNERNO.
  WA_IDWTPARTNER-LIFNR     = WA_QSTRMAIN-LIFNR.
  WA_IDWTPARTNER-HWAER     = WA_QSTRMAIN-HWAER.
  WA_PARTNER_SEQ_NB        = WA_PARTNER_SEQ_NB + 1.
  WA_IDWTPARTNER-PARTNER_SEQ_NB = WA_PARTNER_SEQ_NB.

*  fill in IDWTPARTNER with data coming from additional tables :
  SELECT SINGLE * FROM LFA1  WHERE LIFNR  = WA_IDWTPARTNER-LIFNR.

*  for normal vendors :
**Note 1281813 Begin
*  IF  lfa1-xcpdk  = space.             "no CPD account
  IF ( LFA1-XCPDK = SPACE AND LFA1-XZEMP = SPACE ) OR  "kln1538061       "YMK1411708
     ( LFA1-XZEMP = 'X' AND P_ALTPAY = SPACE ).        "kln1538061                 "YMK1411708
**Note 1281813 End
*  IF ( lfa1-xcpdk = space AND lfa1-xzemp = space ).    "kln1538061  "YMK1411708
    MOVE-CORRESPONDING LFA1 TO WA_IDWTPARTNER.

*        If an alternative payee exists : fetch name and address for
*        this alternative vendor :
    IF P_ALTPAY = 'X'.
***Note YMK1411708 Begin
      IF WA_QSTRMAIN-EMPFB NE ''.
*        BOI 1588561
        CALL FUNCTION 'FI_WT_READ_ALTPAYEE_DATA'
          EXPORTING
*           QSTRMAIN    =
            LIFNR       = WA_QSTRMAIN-EMPFB
          CHANGING
            IDWTPARTNER = WA_IDWTPARTNER.
*        EOI 1588561
*        SELECT SINGLE adrnr FROM lfa1 INTO wa_altern_partner
*                                      WHERE lifnr = wa_qstrmain-empfb.
        IF SY-SUBRC = 0.
          LFA1-ADRNR = WA_IDWTPARTNER-ADDRNUMBER.
        ENDIF.
      ELSE.
***Note YMK1411708 End
*        Alternative payee in master record
        SELECT SINGLE * FROM LFB1
          WHERE LIFNR = WA_IDWTPARTNER-LIFNR
          AND   BUKRS = WA_QSTRMAIN-BUKRS.                  "ANM1493369
        IF NOT LFB1-LNRZB IS INITIAL.
          SELECT SINGLE ADRNR FROM LFA1 INTO WA_ALTERN_PARTNER
            WHERE LIFNR = LFB1-LNRZB.
**Note 1137454 Begin
          IF SY-SUBRC = 0.
            LFA1-ADRNR = WA_ALTERN_PARTNER.
          ENDIF.
**Note 1137454 End
        ELSEIF NOT LFA1-LNRZA IS INITIAL.
*          BOI 1588561
          CALL FUNCTION 'FI_WT_READ_ALTPAYEE_DATA'
            EXPORTING
*             QSTRMAIN    =
              LIFNR       = LFA1-LNRZA
            CHANGING
              IDWTPARTNER = WA_IDWTPARTNER.
*EOI 1588561
*          SELECT SINGLE adrnr FROM lfa1  INTO wa_altern_partner
*                 WHERE lifnr  = lfa1-lnrza.
          IF SY-SUBRC = 0.
            LFA1-ADRNR = WA_IDWTPARTNER-ADDRNUMBER.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR: ADDR1_SEL, SADR.                                 "SADR40A
    ADDR1_SEL-ADDRNUMBER = LFA1-ADRNR.                      "SADR40A

    CALL FUNCTION 'ADDR_GET'
      EXPORTING
        ADDRESS_SELECTION = ADDR1_SEL
*       ADDRESS_GROUP     =
*       READ_SADR_ONLY    = ' '
        READ_TEXTS        = 'X'
      IMPORTING
        ADDRESS_VALUE     = WA_ADDRESS_VALUE
*       ADDRESS_ADDITIONAL_INFO       =
        RETURNCODE        = WA_SADR_RC
        ADDRESS_TEXT      = WA_ADDR1_TEXT
        SADR              = SADR
*        TABLES
*       ADDRESS_GROUPS    =
*       ERROR_TABLE       =
*       VERSIONS          =
      EXCEPTIONS
        PARAMETER_ERROR   = 1
        ADDRESS_NOT_EXIST = 2
        VERSION_NOT_EXIST = 3
        INTERNAL_ERROR    = 4
        OTHERS            = 5.
    IF SY-SUBRC <> 0.
      IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
        MESSAGE E014  WITH LFA1-LIFNR LFA1-ADRNR WA_SADR_RC.
      ELSE.
        CLEAR WA_ERROR.
        WA_ERROR-WT_MSGID       = 'ID-WT'.
        WA_ERROR-WT_MSGTY       = 'E'.
        WA_ERROR-WT_MSGNR       = 14.
        MESSAGE E014  WITH LFA1-LIFNR LFA1-ADRNR WA_SADR_RC
                             INTO WA_ERROR-WT_NATXT.
        PERFORM F01_FILL_IDWTERROR TABLES I_IDWTERROR[]
                             USING WA_ERROR.
      ENDIF.
    ELSE.
      MOVE-CORRESPONDING WA_ADDRESS_VALUE TO WA_IDWTPARTNER.
      MOVE-CORRESPONDING WA_ADDRESS_VALUE TO WA_ADRS.
      MOVE-CORRESPONDING WA_ADDR1_TEXT TO WA_IDWTPARTNER.
      MOVE-CORRESPONDING WA_ADDR1_TEXT TO WA_ADRS.
    ENDIF.


    SELECT SINGLE * FROM LFB1  WHERE BUKRS = WA_QSTRMAIN-BUKRS
                                 AND LIFNR = WA_QSTRMAIN-LIFNR.

    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING LFB1 TO WA_IDWTPARTNER.

*     Read additional info for QLAND
      IF NOT LFB1-QLAND IS INITIAL.
        SELECT SINGLE * FROM T005R
          WHERE SPRAS EQ WA_IDWTPARTNER-LANGU
            AND LAND1 EQ T001-LAND1
            AND QLAND EQ WA_IDWTPARTNER-QLAND.
        IF SY-SUBRC EQ 0.
          MOVE T005R-QLTXT TO WA_IDWTPARTNER-QLTXT.
        ELSE.
          CLEAR WA_IDWTPARTNER-QLTXT.
        ENDIF.
      ELSE.
        CLEAR WA_IDWTPARTNER-QLTXT.
      ENDIF.
    ENDIF.


*  creation of a short one-line address for normal vendors :

    CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
      EXPORTING
        ADRSWA_IN            = WA_ADRS
*       ADDRESS_1            =
*       ADDRESS_2            =
*       ADDRESS_3            =
        ADDRESS_TYPE         = '1'
        ADDRESS_NUMBER       = WA_ADDRESS_VALUE-ADDRNUMBER
*       ADDRESS_HANDLE       = ' '
*       PERSON_NUMBER        = ' '
*       PERSON_HANDLE        = ' '
*       SENDER_COUNTRY       = ' '
*       RECEIVER_LANGUAGE    = ' '
*       NUMBER_OF_LINES      = 10
*       STREET_HAS_PRIORITY  = ' '
*       LINE_PRIORITY        = '6CO'
*       COUNTRY_NAME_IN_RECEIVER_LANGU   = ' '
*       LANGUAGE_FOR_COUNTRY_NAME        = ' '
*       NO_UPPER_CASE_FOR_CITY           = ' '
      IMPORTING
*       ADRSWA_OUT           =
*       ADDRESS_PRINTFORM    =
*       ADDRESS_SHORT_FORM   =
        ADDRESS_SHORT_FORM_S = WA_IDWTPARTNER-WT_SHORT_ADDR
*       ADDRESS_DATA_CARRIER =
*       ADDRESS_DATA_CARRIER_0           =
*       NUMBER_OF_USED_LINES =
*       NAME_IS_EMPTY        =
*       ADDRESS_NOT_FOUND    =
*       ADDRESS_PRINTFORM_TABLE          =
      EXCEPTIONS
        OTHERS               = 1.
    IF SY-SUBRC <> 0.
      IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
        MESSAGE W023 WITH LFA1-LIFNR.
      ELSE.
        CLEAR WA_ERROR.
        WA_ERROR-WT_MSGID       = 'ID-WT'.
        WA_ERROR-WT_MSGTY       = 'W'.
        WA_ERROR-WT_MSGNR       = 23.
        MESSAGE W023 WITH LFA1-LIFNR
                             INTO WA_ERROR-WT_NATXT.
        PERFORM F01_FILL_IDWTERROR TABLES I_IDWTERROR[]
                             USING WA_ERROR.
      ENDIF.
      WA_IDWTPARTNER-WT_SHORT_ADDR = SPACE.
    ENDIF.


  ELSE.
*  for one-time vendor accounts and individual alternative payees

    SELECT SINGLE * FROM BSEC
           WHERE BUKRS EQ WA_QSTRMAIN-BUKRS
             AND   BELNR EQ WA_QSTRMAIN-BELNR
             AND   GJAHR EQ WA_QSTRMAIN-GJAHR
             AND   BUZEI EQ WA_QSTRMAIN-BUZEI.
* Note 492786 begin
    CLEAR WA_IDWTPARTNER.
* Note 492786 end
    IF SY-SUBRC EQ 0.          "Note 822765
      MOVE BSEC-NAME1 TO WA_IDWTPARTNER-NAME1.
      MOVE BSEC-NAME2 TO WA_IDWTPARTNER-NAME2.
      MOVE BSEC-NAME3 TO WA_IDWTPARTNER-NAME3.
      MOVE BSEC-NAME4 TO WA_IDWTPARTNER-NAME4.
      MOVE BSEC-LAND1 TO WA_IDWTPARTNER-COUNTRY.
*Note 492786 begin
*   MOVE bsec-stras+0(10)  TO wa_idwtpartner-house_num1.
*   MOVE bsec-stras+10(10) TO wa_idwtpartner-house_num2.
*   MOVE bsec-stras+20(10) TO wa_idwtpartner-house_num3.
*Note 492786 end
      MOVE BSEC-STRAS        TO WA_IDWTPARTNER-STREET.
      MOVE BSEC-PSTLZ TO WA_IDWTPARTNER-POST_CODE1.
      MOVE BSEC-PSTL2 TO WA_IDWTPARTNER-POST_CODE2.
      MOVE BSEC-PFACH TO WA_IDWTPARTNER-PO_BOX.
      MOVE BSEC-ORT01 TO WA_IDWTPARTNER-CITY1.
      MOVE BSEC-STCD1 TO WA_IDWTPARTNER-STCD1.
      MOVE BSEC-STCD2 TO WA_IDWTPARTNER-STCD2.
      MOVE BSEC-STCD3 TO WA_IDWTPARTNER-STCD3.              "1686208
      MOVE BSEC-REGIO TO WA_IDWTPARTNER-REGION.
**Note 1104452 Begins
      MOVE BSEC-J_1KFREPRE TO WA_IDWTPARTNER-J_1KFREPRE.
* Note 930326 BEGIN
      MOVE BSEC-STKZN TO WA_IDWTPARTNER-STKZN.
      MOVE BSEC-J_1KFTBUS TO WA_IDWTPARTNER-J_1KFTBUS.
      MOVE BSEC-J_1KFTIND TO WA_IDWTPARTNER-J_1KFTIND.
* Note 930326 END
**Note 1104452 Ends

*NOTE 1538061 BEGIN
*BOI                                    "kln1538061
*    ENDIF.

      "Note 822765
    ELSEIF ( SY-SUBRC <> 0 AND  LFA1-XCPDK = SPACE ).
      MOVE-CORRESPONDING LFA1 TO WA_IDWTPARTNER.

      IF WA_QSTRMAIN-EMPFB NE ''.

*        SELECT SINGLE adrnr FROM lfa1 INTO wa_altern_partner
*                                      WHERE lifnr = wa_qstrmain-empfb.
*BOI 1588561
        CALL FUNCTION 'FI_WT_READ_ALTPAYEE_DATA'
          EXPORTING
*           QSTRMAIN    =
            LIFNR       = WA_QSTRMAIN-EMPFB
          CHANGING
            IDWTPARTNER = WA_IDWTPARTNER.
*EOI 1588561
        IF SY-SUBRC = 0.
          LFA1-ADRNR = WA_IDWTPARTNER-ADDRNUMBER.           "1588561
        ENDIF.
      ELSE.
        SELECT SINGLE * FROM LFB1
        WHERE LIFNR = WA_IDWTPARTNER-LIFNR
        AND   BUKRS = WA_IDWTPARTNER-BUKRS.
        IF NOT LFB1-LNRZB IS INITIAL.
*          BOI 1588561
          CALL FUNCTION 'FI_WT_READ_ALTPAYEE_DATA'
            EXPORTING
*             QSTRMAIN    =
              LIFNR       = LFB1-LNRZB
            CHANGING
              IDWTPARTNER = WA_IDWTPARTNER.
*EOI 1588561
*          SELECT SINGLE adrnr FROM lfa1 INTO wa_altern_partner
*            WHERE lifnr = lfb1-lnrzb.
**Note 1137454 Begin
          IF SY-SUBRC = 0.
            LFA1-ADRNR = WA_IDWTPARTNER-ADDRNUMBER.         "1588561
          ENDIF.
**Note 1137454 End
        ELSEIF NOT LFA1-LNRZA IS INITIAL.
*          BOI 1588561
          CALL FUNCTION 'FI_WT_READ_ALTPAYEE_DATA'
            EXPORTING
*             QSTRMAIN    =
              LIFNR       = LFA1-LNRZA
            CHANGING
              IDWTPARTNER = WA_IDWTPARTNER.
**EOI 1588561
*           SELECT SINGLE adrnr FROM lfa1  INTO wa_altern_partner
*                 WHERE lifnr  = lfa1-lnrza.

          IF SY-SUBRC = 0.
            LFA1-ADRNR = WA_IDWTPARTNER-ADDRNUMBER.         "1588561
          ENDIF.
        ENDIF.
      ENDIF.
      CLEAR: ADDR1_SEL, SADR.                               "SADR40A
      ADDR1_SEL-ADDRNUMBER = LFA1-ADRNR.                    "SADR40A

      CALL FUNCTION 'ADDR_GET'
        EXPORTING
          ADDRESS_SELECTION = ADDR1_SEL
*         ADDRESS_GROUP     =
*         READ_SADR_ONLY    = ' '
          READ_TEXTS        = 'X'
        IMPORTING
          ADDRESS_VALUE     = WA_ADDRESS_VALUE
*         ADDRESS_ADDITIONAL_INFO       =
          RETURNCODE        = WA_SADR_RC
          ADDRESS_TEXT      = WA_ADDR1_TEXT
          SADR              = SADR
*        TABLES
*         ADDRESS_GROUPS    =
*         ERROR_TABLE       =
*         VERSIONS          =
        EXCEPTIONS
          PARAMETER_ERROR   = 1
          ADDRESS_NOT_EXIST = 2
          VERSION_NOT_EXIST = 3
          INTERNAL_ERROR    = 4
          OTHERS            = 5.
      IF SY-SUBRC <> 0.
        IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
          MESSAGE E014  WITH LFA1-LIFNR LFA1-ADRNR WA_SADR_RC.
        ELSE.
          CLEAR WA_ERROR.
          WA_ERROR-WT_MSGID       = 'ID-WT'.
          WA_ERROR-WT_MSGTY       = 'E'.
          WA_ERROR-WT_MSGNR       = 14.
          MESSAGE E014  WITH LFA1-LIFNR LFA1-ADRNR WA_SADR_RC
                               INTO WA_ERROR-WT_NATXT.
          PERFORM F01_FILL_IDWTERROR TABLES I_IDWTERROR[]
                               USING WA_ERROR.
        ENDIF.
      ELSE.
        MOVE-CORRESPONDING WA_ADDRESS_VALUE TO WA_IDWTPARTNER.
        MOVE-CORRESPONDING WA_ADDRESS_VALUE TO WA_ADRS.
        MOVE-CORRESPONDING WA_ADDR1_TEXT TO WA_IDWTPARTNER.
        MOVE-CORRESPONDING WA_ADDR1_TEXT TO WA_ADRS.
      ENDIF.


      SELECT SINGLE * FROM LFB1  WHERE BUKRS = WA_QSTRMAIN-BUKRS
                                   AND LIFNR = WA_QSTRMAIN-LIFNR.

      IF SY-SUBRC = 0.
        MOVE-CORRESPONDING LFB1 TO WA_IDWTPARTNER.

*     Read additional info for QLAND
        IF NOT LFB1-QLAND IS INITIAL.
          SELECT SINGLE * FROM T005R
            WHERE SPRAS EQ WA_IDWTPARTNER-LANGU
              AND LAND1 EQ T001-LAND1
              AND QLAND EQ WA_IDWTPARTNER-QLAND.
          IF SY-SUBRC EQ 0.
            MOVE T005R-QLTXT TO WA_IDWTPARTNER-QLTXT.
          ELSE.
            CLEAR WA_IDWTPARTNER-QLTXT.
          ENDIF.
        ELSE.
          CLEAR WA_IDWTPARTNER-QLTXT.
        ENDIF.
      ENDIF.


*  creation of a short one-line address for normal vendors :

      CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
        EXPORTING
          ADRSWA_IN            = WA_ADRS
*         ADDRESS_1            =
*         ADDRESS_2            =
*         ADDRESS_3            =
          ADDRESS_TYPE         = '1'
          ADDRESS_NUMBER       = WA_ADDRESS_VALUE-ADDRNUMBER
*         ADDRESS_HANDLE       = ' '
*         PERSON_NUMBER        = ' '
*         PERSON_HANDLE        = ' '
*         SENDER_COUNTRY       = ' '
*         RECEIVER_LANGUAGE    = ' '
*         NUMBER_OF_LINES      = 10
*         STREET_HAS_PRIORITY  = ' '
*         LINE_PRIORITY        = '6CO'
*         COUNTRY_NAME_IN_RECEIVER_LANGU   = ' '
*         LANGUAGE_FOR_COUNTRY_NAME        = ' '
*         NO_UPPER_CASE_FOR_CITY           = ' '
        IMPORTING
*         ADRSWA_OUT           =
*         ADDRESS_PRINTFORM    =
*         ADDRESS_SHORT_FORM   =
          ADDRESS_SHORT_FORM_S = WA_IDWTPARTNER-WT_SHORT_ADDR
*         ADDRESS_DATA_CARRIER =
*         ADDRESS_DATA_CARRIER_0           =
*         NUMBER_OF_USED_LINES =
*         NAME_IS_EMPTY        =
*         ADDRESS_NOT_FOUND    =
*         ADDRESS_PRINTFORM_TABLE          =
        EXCEPTIONS
          OTHERS               = 1.
      IF SY-SUBRC <> 0.
        IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
          MESSAGE W023 WITH LFA1-LIFNR.
        ELSE.
          CLEAR WA_ERROR.
          WA_ERROR-WT_MSGID       = 'ID-WT'.
          WA_ERROR-WT_MSGTY       = 'W'.
          WA_ERROR-WT_MSGNR       = 23.
          MESSAGE W023 WITH LFA1-LIFNR
                               INTO WA_ERROR-WT_NATXT.
          PERFORM F01_FILL_IDWTERROR TABLES I_IDWTERROR[]
                               USING WA_ERROR.
        ENDIF.
        WA_IDWTPARTNER-WT_SHORT_ADDR = SPACE.
      ENDIF.

    ENDIF.
*    EOI                                          "kln1538061

*NOTE 1538061 END


*BoD 1248393
*    CONCATENATE wa_idwtpartner-street wa_idwtpartner-post_code1
*    wa_idwtpartner-city1 INTO wa_idwtpartner-wt_short_addr SEPARATED
*    BY space.
*EoD 1248393
*BoI 1248393
    CONCATENATE WA_IDWTPARTNER-NAME1 WA_IDWTPARTNER-STREET WA_IDWTPARTNER-POST_CODE1
    WA_IDWTPARTNER-CITY1 INTO WA_IDWTPARTNER-WT_SHORT_ADDR SEPARATED
    BY SPACE.
*EoI 1248393

  ENDIF.


*  initialization of all amounts ( in case where no type and code and
*  no period and no reversed-indicator are stored inside the table
*  IDWTPARTNER ) :

  WA_IDWTPARTNER-DMBTR          = 0.
  WA_IDWTPARTNER-WT_TAXHH       = 0.
  WA_IDWTPARTNER-WT_QSFHH       = 0.
  WA_IDWTPARTNER-WT_QBSHH       = 0.
  WA_IDWTPARTNER-WT_QSSHH       = 0.
  WA_IDWTPARTNER-WT_AMREXMP     = 0.
  WA_IDWTPARTNER-WT_AMRBASE     = 0.
  WA_IDWTPARTNER-WT_AMEXCLVAT   = 0.
  WA_IDWTPARTNER-SKNTO          = 0.


ENDFORM.                               " I01_initialize_vendor

*-----------------------------------------------------------------------
*------- Initialize Reporting tool 'Partner / WT types-codes' table with
*        Customer data
*-----------------------------------------------------------------------
FORM I01_INITIALIZE_CUSTOMER.
  DATA: WA_ADDRESS_VALUE LIKE ADDR1_VAL,
        WA_ADDR1_TEXT    LIKE ADDR1_TEXT.

  WA_IDWTPARTNER-PARTNERNO = WA_QSTRMAIN-PARTNERNO.
  WA_IDWTPARTNER-KUNNR     = WA_QSTRMAIN-KUNNR.
  WA_IDWTPARTNER-HWAER     = WA_QSTRMAIN-HWAER.
  WA_PARTNER_SEQ_NB        = WA_PARTNER_SEQ_NB + 1.
  WA_IDWTPARTNER-PARTNER_SEQ_NB = WA_PARTNER_SEQ_NB.

*  fill in IDWTPARTNER with data coming from additional tables :
  SELECT SINGLE * FROM KNA1  WHERE KUNNR  = WA_IDWTPARTNER-KUNNR.

*  for normal customers :
  IF  KNA1-XCPDK  = SPACE.             "no CPD account

    MOVE-CORRESPONDING KNA1 TO WA_IDWTPARTNER.

*        If an alternative customer exists : fetch name and address for
*        this alternative customer :
    IF P_ALTPAY = 'X'.
*        Alternative payee in master record
      SELECT SINGLE * FROM KNB1
        WHERE KUNNR = WA_IDWTPARTNER-KUNNR
        AND   BUKRS = WA_IDWTPARTNER-BUKRS.
      IF NOT KNB1-KNRZB IS INITIAL.
        SELECT SINGLE ADRNR FROM KNA1 INTO WA_ALTERN_PARTNER
          WHERE KUNNR = KNB1-KNRZB.
      ELSEIF NOT KNA1-KNRZA IS INITIAL.
        SELECT SINGLE ADRNR FROM KNA1  INTO WA_ALTERN_PARTNER
               WHERE KUNNR  = KNA1-KNRZA.
        IF SY-SUBRC = 0.
          KNA1-ADRNR = WA_ALTERN_PARTNER.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR: ADDR1_SEL, SADR.
    ADDR1_SEL-ADDRNUMBER = KNA1-ADRNR.

    CALL FUNCTION 'ADDR_GET'
      EXPORTING
        ADDRESS_SELECTION = ADDR1_SEL
*       ADDRESS_GROUP     =
*       READ_SADR_ONLY    = ' '
        READ_TEXTS        = 'X'
      IMPORTING
        ADDRESS_VALUE     = WA_ADDRESS_VALUE
*       ADDRESS_ADDITIONAL_INFO       =
        RETURNCODE        = WA_SADR_RC
        ADDRESS_TEXT      = WA_ADDR1_TEXT
        SADR              = SADR
*        TABLES
*       ADDRESS_GROUPS    =
*       ERROR_TABLE       =
*       VERSIONS          =
      EXCEPTIONS
        PARAMETER_ERROR   = 1
        ADDRESS_NOT_EXIST = 2
        VERSION_NOT_EXIST = 3
        INTERNAL_ERROR    = 4
        OTHERS            = 5.
    IF SY-SUBRC <> 0.
      IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
        MESSAGE E014  WITH LFA1-LIFNR LFA1-ADRNR WA_SADR_RC.
      ELSE.
        CLEAR WA_ERROR.
        WA_ERROR-WT_MSGID       = 'ID-WT'.
        WA_ERROR-WT_MSGTY       = 'E'.
        WA_ERROR-WT_MSGNR       = 14.
        MESSAGE E014  WITH LFA1-LIFNR LFA1-ADRNR WA_SADR_RC
                             INTO WA_ERROR-WT_NATXT.
        PERFORM F01_FILL_IDWTERROR TABLES I_IDWTERROR[]
                             USING WA_ERROR.
      ENDIF.
    ELSE.
      MOVE-CORRESPONDING WA_ADDRESS_VALUE TO WA_IDWTPARTNER.
      MOVE-CORRESPONDING WA_ADDRESS_VALUE TO WA_ADRS.
      MOVE-CORRESPONDING WA_ADDR1_TEXT TO WA_IDWTPARTNER.
      MOVE-CORRESPONDING WA_ADDR1_TEXT TO WA_ADRS.
    ENDIF.


    SELECT SINGLE * FROM KNB1  WHERE BUKRS = WA_QSTRMAIN-BUKRS
                                 AND KUNNR = WA_QSTRMAIN-KUNNR.

    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING KNB1 TO WA_IDWTPARTNER.
    ENDIF.



*  creation of a short one-line address for normal vendors :

    CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
      EXPORTING
        ADRSWA_IN            = WA_ADRS
*       ADDRESS_1            =
*       ADDRESS_2            =
*       ADDRESS_3            =
        ADDRESS_TYPE         = ' '
*       ADDRESS_NUMBER       = ' '
*       ADDRESS_HANDLE       = ' '
*       PERSON_NUMBER        = ' '
*       PERSON_HANDLE        = ' '
*       SENDER_COUNTRY       = ' '
*       RECEIVER_LANGUAGE    = ' '
*       NUMBER_OF_LINES      = 10
*       STREET_HAS_PRIORITY  = ' '
*       LINE_PRIORITY        = '6CO'
*       COUNTRY_NAME_IN_RECEIVER_LANGU       = ' '
*       LANGUAGE_FOR_COUNTRY_NAME            = ' '
*       NO_UPPER_CASE_FOR_CITY               = ' '
      IMPORTING
*       ADRSWA_OUT           =
*       ADDRESS_PRINTFORM    =
*       ADDRESS_SHORT_FORM   =
        ADDRESS_SHORT_FORM_S = WA_IDWTPARTNER-WT_SHORT_ADDR
*       ADDRESS_DATA_CARRIER =
*       ADDRESS_DATA_CARRIER_0               =
*       NUMBER_OF_USED_LINES =
*       NAME_IS_EMPTY        =
*       ADDRESS_NOT_FOUND    =
*       ADDRESS_PRINTFORM_TABLE              =
      EXCEPTIONS
        OTHERS               = 1.
    IF SY-SUBRC <> 0.
      IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
        MESSAGE W023 WITH LFA1-LIFNR.
      ELSE.
        CLEAR WA_ERROR.
        WA_ERROR-WT_MSGID       = 'ID-WT'.
        WA_ERROR-WT_MSGTY       = 'W'.
        WA_ERROR-WT_MSGNR       = 23.
        MESSAGE W023 WITH LFA1-LIFNR
                             INTO WA_ERROR-WT_NATXT.
        PERFORM F01_FILL_IDWTERROR TABLES I_IDWTERROR[]
                             USING WA_ERROR.
      ENDIF.
      WA_IDWTPARTNER-WT_SHORT_ADDR = SPACE.
    ENDIF.


  ELSE.
*  for one-time customer accounts :

    SELECT SINGLE * FROM BSEC
           WHERE BUKRS EQ WA_QSTRMAIN-BUKRS
             AND   BELNR EQ WA_QSTRMAIN-BELNR
             AND   GJAHR EQ WA_QSTRMAIN-GJAHR
             AND   BUZEI EQ WA_QSTRMAIN-BUZEI.
    CLEAR WA_IDWTPARTNER.     "Note 822765
    IF SY-SUBRC EQ 0.         "Note 822765
      MOVE BSEC-NAME1 TO WA_IDWTPARTNER-NAME1.
      MOVE BSEC-NAME2 TO WA_IDWTPARTNER-NAME2.
      MOVE BSEC-NAME3 TO WA_IDWTPARTNER-NAME3.
      MOVE BSEC-NAME4 TO WA_IDWTPARTNER-NAME4.
      MOVE BSEC-LAND1 TO WA_IDWTPARTNER-COUNTRY.
      MOVE BSEC-STRAS+0(10)  TO WA_IDWTPARTNER-HOUSE_NUM1.
      MOVE BSEC-STRAS+10(10) TO WA_IDWTPARTNER-HOUSE_NUM2.
      MOVE BSEC-STRAS+20(10) TO WA_IDWTPARTNER-HOUSE_NUM3.
      MOVE BSEC-STRAS        TO WA_IDWTPARTNER-STREET.
      MOVE BSEC-PSTLZ TO WA_IDWTPARTNER-POST_CODE1.
      MOVE BSEC-PSTL2 TO WA_IDWTPARTNER-POST_CODE2.
      MOVE BSEC-PFACH TO WA_IDWTPARTNER-PO_BOX.
      MOVE BSEC-ORT01 TO WA_IDWTPARTNER-CITY1.
      MOVE BSEC-STCD1 TO WA_IDWTPARTNER-STCD1.
      MOVE BSEC-STCD2 TO WA_IDWTPARTNER-STCD2.
      MOVE BSEC-REGIO TO WA_IDWTPARTNER-REGION.
    ENDIF.                       "Note 822765

    CONCATENATE WA_IDWTPARTNER-STREET WA_IDWTPARTNER-POST_CODE1
    WA_IDWTPARTNER-CITY1 INTO WA_IDWTPARTNER-WT_SHORT_ADDR SEPARATED
    BY SPACE.


  ENDIF.


*  initialization of all amounts ( in case where no type and code and
*  no period and no reversed-indicator are stored inside the table
*  IDWTPARTNER ) :

  WA_IDWTPARTNER-DMBTR          = 0.
  WA_IDWTPARTNER-WT_TAXHH       = 0.
  WA_IDWTPARTNER-WT_QSFHH       = 0.
  WA_IDWTPARTNER-WT_QBSHH       = 0.
  WA_IDWTPARTNER-WT_QSSHH       = 0.
  WA_IDWTPARTNER-WT_AMREXMP     = 0.
  WA_IDWTPARTNER-WT_AMRBASE     = 0.
  WA_IDWTPARTNER-WT_AMEXCLVAT   = 0.
  WA_IDWTPARTNER-SKNTO          = 0.


ENDFORM.                               " I01_initialize_customer

*-----------------------------------------------------------------------
*------- Initialize Reporting tool 'Vendor / WT types-codes' table
*        to separate reversed documents from the others
*--------------------------------------------------------------------
FORM I01_INITIALIZE_REVERSED.

*  initialization of all amounts ( in case where no type and code
*  and period are stored inside the table IDWTPARTNER ) :

  WA_IDWTPARTNER-DMBTR          = 0.
  WA_IDWTPARTNER-WT_TAXHH       = 0.
  WA_IDWTPARTNER-WT_QSFHH       = 0.
  WA_IDWTPARTNER-WT_QBSHH       = 0.
  WA_IDWTPARTNER-WT_QSSHH       = 0.
  WA_IDWTPARTNER-WT_AMREXMP     = 0.
  WA_IDWTPARTNER-WT_AMRBASE     = 0.
  WA_IDWTPARTNER-WT_AMEXCLVAT   = 0.
  WA_IDWTPARTNER-SKNTO          = 0.

ENDFORM.                               " I01_initialize_reversed

*-----------------------------------------------------------------------
*------- Initialize Reporting tool 'Vendor / WT types-codes' table with
*        period data
*-----------------------------------------------------------------------
FORM I01_INITIALIZE_PERIOD.


  WA_IDWTPARTNER-WT_PERIOD      = WA_QSTRMAIN-WT_PERIOD.

*  initialization of all amounts ( in case where no type and code
*  are stored inside the table IDWTPARTNER ) :

  WA_IDWTPARTNER-DMBTR          = 0.
  WA_IDWTPARTNER-WT_TAXHH       = 0.
  WA_IDWTPARTNER-WT_QSFHH       = 0.
  WA_IDWTPARTNER-WT_QBSHH       = 0.
  WA_IDWTPARTNER-WT_QSSHH       = 0.
  WA_IDWTPARTNER-WT_AMREXMP     = 0.
  WA_IDWTPARTNER-WT_AMRBASE     = 0.
  WA_IDWTPARTNER-WT_AMEXCLVAT   = 0.
  WA_IDWTPARTNER-SKNTO          = 0.

ENDFORM.                               " I01_initialize_period


*-----------------------------------------------------------------------
*------- initialize Reporting tool 'Vendor /WT types-codes' table with
*        WT types
*-----------------------------------------------------------------------
FORM I01_INITIALIZE_WTTYPE.


  CHECK WA_QSTRMAIN-WITHT <> SPACE.

* search for additional data :

  SELECT SINGLE * FROM T059P  WHERE  LAND1 EQ T001-LAND1
                                 AND WITHT EQ WA_QSTRMAIN-WITHT.

  IF SY-SUBRC = 0.
    WA_IDWTPARTNER-WT_POSTM   = T059P-WT_POSTM.
  ENDIF.

  SELECT SINGLE * FROM T059U  WHERE SPRAS EQ SY-LANGU
                                 AND LAND1 EQ T001-LAND1
                                 AND WITHT EQ WA_QSTRMAIN-WITHT.

  IF SY-SUBRC = 0.
    WA_IDWTPARTNER-TEXT40   = T059U-TEXT40.
  ENDIF.




*  fetch data  'Vendor + WT type' dependent :

  IF WA_IDWTGLOB-WT_OG_NO_EXEMPT IS INITIAL.   " read only if necessary

    IF WA_QSTRMAIN-KOART = 'K'.

      IF NOT WA_QSTRMAIN-LIFNR IS INITIAL.

        SELECT   SINGLE * FROM  LFBW
               WHERE  LIFNR  = WA_QSTRMAIN-LIFNR
               AND    BUKRS  = WA_QSTRMAIN-BUKRS
               AND    WITHT  = WA_QSTRMAIN-WITHT.

        IF SY-SUBRC = 0.
          MOVE-CORRESPONDING LFBW TO WA_IDWTPARTNER.
        ENDIF.
      ENDIF.

    ELSE.

*  fetch data  'Customer + WT type' dependent :

      IF NOT WA_QSTRMAIN-KUNNR IS INITIAL.

        SELECT   SINGLE * FROM  KNBW
               WHERE  KUNNR  = WA_QSTRMAIN-KUNNR
               AND    BUKRS  = WA_QSTRMAIN-BUKRS
               AND    WITHT  = WA_QSTRMAIN-WITHT.

        IF SY-SUBRC = 0.
          MOVE-CORRESPONDING KNBW TO WA_IDWTPARTNER.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.                               " I01_initialize_wttype


*-----------------------------------------------------------------------
*------- initialize Reporting tool 'Vendor /WT types-codes' table with
*        WT codes
*-----------------------------------------------------------------------
FORM I01_INITIALIZE_WTCODE.

* BoI - Protect same field names for overwritten.       "25042002
  DATA: WA_PARTNER_TEMP TYPE IDWTPARTNER.

  MOVE-CORRESPONDING WA_IDWTPARTNER TO WA_PARTNER_TEMP.
* EoI                                                   "25042002

  MOVE-CORRESPONDING WA_QSTRMAIN  TO WA_IDWTPARTNER.

* BoI - Protect same field names for overwritten.       "25042002
  MOVE: WA_PARTNER_TEMP-QSREC TO WA_IDWTPARTNER-QSREC.
***Note 1081791 Begin
  MOVE: WA_PARTNER_TEMP-WT_POSTM TO WA_IDWTPARTNER-WT_POSTM.
***Note 1081791 End
* EoI                                                   "25042002
  MOVE: WA_PARTNER_TEMP-STCD1 TO WA_IDWTPARTNER-STCD1,      "YMK1406944
        WA_PARTNER_TEMP-STCD2 TO WA_IDWTPARTNER-STCD2,      "YMK1406944
        WA_PARTNER_TEMP-STCD3 TO WA_IDWTPARTNER-STCD3.      "1686208

  IF WA_QSTRMAIN-WT_WITHCD <> SPACE.

* search for additional data for Extended WT :

    IF NOT WA_IDWTCOMPCD-WT_NEWWT IS INITIAL.

      SELECT SINGLE * FROM T059Z  WHERE LAND1 EQ T001-LAND1
                                  AND WITHT EQ WA_QSTRMAIN-WITHT
                                 AND WT_WITHCD EQ WA_QSTRMAIN-WT_WITHCD.

      IF SY-SUBRC = 0.
        WA_IDWTPARTNER-TAX_REGION  = T059Z-REGIO.
        WA_IDWTPARTNER-QSCOD    = T059Z-QSCOD.
        WA_IDWTPARTNER-FPRCD    = T059Z-FPRCD.
        WA_IDWTPARTNER-QEKAR    = T059Z-QEKAR.
        WA_IDWTPARTNER-QSATZ    = T059Z-QSATZ.
        WA_IDWTPARTNER-QSATR    = T059Z-QSATR.
        WA_IDWTPARTNER-WT_POSIN = T059Z-WT_POSIN.
        WA_IDWTPARTNER-QPROZ    = T059Z-QPROZ.
        WA_IDWTPARTNER-XQFOR    = T059Z-XQFOR.
        WA_IDWTPARTNER-CIIU     = T059Z-WITHCD2.     "Refer Note 773037


        SELECT SINGLE * FROM T059ZT  WHERE SPRAS EQ SY-LANGU
                                 AND LAND1     EQ T001-LAND1
                                 AND WITHT     EQ WA_QSTRMAIN-WITHT
                                 AND WT_WITHCD EQ WA_QSTRMAIN-WT_WITHCD.

        IF SY-SUBRC = 0.
          WA_IDWTPARTNER-WT_TYPECODE_TEXT    = T059ZT-TEXT40.
        ENDIF.

*       INSERT NOTE 0385487 START

        SELECT SINGLE * FROM T005P WHERE LAND1 EQ T001-LAND1
                                   AND BLAND EQ T059Z-REGIO
                                   AND FPRCD EQ T059Z-FPRCD.

        IF SY-SUBRC = 0.
          WA_IDWTPARTNER-BEZEI = T005P-BEZEI.
        ENDIF.
*       INSERT NOTE 0385487 END

        SELECT SINGLE * FROM T005U  WHERE SPRAS EQ SY-LANGU
                                      AND LAND1 EQ T001-LAND1
                                      AND BLAND EQ T059Z-REGIO.

        IF SY-SUBRC = 0.
          WA_IDWTPARTNER-TAX_REGION_TXT    = T005U-BEZEI.
        ENDIF.

        SELECT SINGLE * FROM T059G  WHERE SPRAS EQ SY-LANGU
                                      AND LAND1 EQ T001-LAND1
                                      AND QEKAR EQ T059Z-QEKAR.

        IF SY-SUBRC = 0.
          WA_IDWTPARTNER-EATXT   = T059G-EATXT.
        ENDIF.

        SELECT SINGLE * FROM T059OT  WHERE LAND1 EQ T001-LAND1
                                     AND   SPRAS EQ SY-LANGU
                                 AND WT_QSCOD    EQ T059Z-QSCOD.

        IF SY-SUBRC = 0.
          WA_IDWTPARTNER-WT_OFFWT_TEXT   = T059OT-TEXT40.
        ENDIF.

*       in some countries the W.T. rate is depending on the country of
*       the partner. Then, if formulas have been defined in table T059FB
*       and if there is only one formula : pick up the rate ( if there
*       are several rates,it is meaningless to keep only one of them ).

        IF T059Z-QSATZ IS INITIAL.
          SELECT * FROM T059FB INTO TABLE WA_T059FB
                             WHERE LAND1     EQ T001-LAND1
                               AND WAERS     EQ T001-WAERS
                               AND WITHT     EQ WA_QSTRMAIN-WITHT
                               AND WT_WITHCD EQ WA_QSTRMAIN-WT_WITHCD
                               AND QLAND     EQ WA_IDWTPARTNER-QLAND
                               ORDER BY WT_VALID DESCENDING.

          IF SY-SUBRC = 0.

            WA_RATES_NB = 0.
            WA_VALID_DATE = 0.
            LOOP AT WA_T059FB.
              IF WA_T059FB-WT_VALID  GT WA_IDWTGLOB-WT_SC_REP_TO.
                CONTINUE.
              ENDIF.
              IF WA_T059FB-WT_VALID  EQ WA_VALID_DATE.
                ADD 1 TO WA_RATES_NB.
                CONTINUE.
              ENDIF.
              IF WA_T059FB-WT_VALID  LE WA_IDWTGLOB-WT_SC_REP_TO.
                WA_VALID_DATE = WA_T059FB-WT_VALID.
                ADD 1 TO WA_RATES_NB.
              ENDIF.
            ENDLOOP.

            IF WA_RATES_NB EQ 1.
*Note 822765
              READ TABLE WA_T059FB
                             WITH KEY LAND1  = T001-LAND1
                                WAERS     = T001-WAERS
                               WITHT     = WA_QSTRMAIN-WITHT
                               WT_WITHCD = WA_QSTRMAIN-WT_WITHCD
                               QLAND     = WA_IDWTPARTNER-QLAND
                               WT_VALID = WA_VALID_DATE.

*Note 822765
              WA_IDWTPARTNER-QSATZ   = WA_T059FB-QSATZ.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.

    ELSE.
* search for additional data for Classical WT :

      SELECT SINGLE * FROM T059Q  WHERE LAND1 EQ T001-LAND1
                                  AND QSSKZ  EQ WA_QSTRMAIN-WT_WITHCD.

      IF SY-SUBRC = 0.
        WA_IDWTPARTNER-TAX_REGION   = T059Q-BLAND.
        WA_IDWTPARTNER-QSCOD    = T059Q-QSCOD.
        WA_IDWTPARTNER-TEXT40   = T059Q-QSBEZ.
        WA_IDWTPARTNER-FPRCD    = T059Q-FPRCD.
        WA_IDWTPARTNER-QEKAR    = T059Q-QEKAR.
        WA_IDWTPARTNER-QSATZ    = T059Q-QSATZ.
        WA_IDWTPARTNER-QSATR    = T059Q-QSATR.
        WA_IDWTPARTNER-QPROZ    = T059Q-QPROZ.
        WA_IDWTPARTNER-XQFOR    = T059Q-XQFOR.

        SELECT SINGLE * FROM T005U  WHERE SPRAS EQ SY-LANGU
                                      AND LAND1 EQ T001-LAND1
                                      AND BLAND EQ T059Q-BLAND.

        IF SY-SUBRC = 0.
          WA_IDWTPARTNER-TAX_REGION   = T005U-BEZEI.
        ENDIF.

        SELECT SINGLE * FROM T059G  WHERE SPRAS EQ SY-LANGU
                                      AND LAND1 EQ T001-LAND1
                                      AND QEKAR EQ T059Q-QEKAR.

        IF SY-SUBRC = 0.
          WA_IDWTPARTNER-EATXT   = T059G-EATXT.
        ENDIF.

        SELECT SINGLE * FROM T059OT  WHERE LAND1 EQ T001-LAND1
                                     AND   SPRAS EQ SY-LANGU
                                 AND WT_QSCOD    EQ T059Q-QSCOD.

        IF SY-SUBRC = 0.
          WA_IDWTPARTNER-WT_OFFWT_TEXT   = T059OT-TEXT40.
        ENDIF.

*       in some countries the W.T. rate is depending on the country of
*       the partner. Then, if formulas have been defined in table T059F
*       and if there is only one formula : pick up the rate ( if there
*       are several rates,it is meaningless to keep only one of them ).

        IF T059Q-QSATZ IS INITIAL.
          SELECT * FROM T059F INTO TABLE WA_T059F
                             WHERE LAND1     EQ T001-LAND1
                               AND WAERS     EQ T001-WAERS
                               AND QSSKZ     EQ WA_QSTRMAIN-WT_WITHCD
                               AND QLAND     EQ WA_IDWTPARTNER-QLAND.

          IF SY-SUBRC = 0.
            IF SY-DBCNT EQ 1.
              READ TABLE WA_T059F INDEX 1. "Note 822765
              WA_IDWTPARTNER-QSATZ   = WA_T059F-QSATZ.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDIF.
  ENDIF.

*  initialization of all amounts :

  WA_IDWTPARTNER-DMBTR          = 0.
  WA_IDWTPARTNER-WT_TAXHH       = 0.
  WA_IDWTPARTNER-WT_QSFHH       = 0.
  WA_IDWTPARTNER-WT_QBSHH       = 0.
  WA_IDWTPARTNER-WT_QSSHH       = 0.
  WA_IDWTPARTNER-WT_AMREXMP     = 0.
  WA_IDWTPARTNER-WT_AMRBASE     = 0.
  WA_IDWTPARTNER-WT_AMEXCLVAT   = 0.
  WA_IDWTPARTNER-SKNTO          = 0.


ENDFORM.                               " I01_initialize_wtcode.


*-----------------------------------------------------------------------
*------- Cumulate the amounts per WT code                ---------------
*-----------------------------------------------------------------------
FORM I01_SUM_AMOUNTS_PARTNER.

* Do not sum up items if they are excluded from single item tab
  IF NOT WA_IDWTGLOB-WT_SC_EXCL_WT0 IS INITIAL
     AND WA_IDWTFIDOC-WT_QBSHH = 0.
*   exit
  ELSE.
    ADD WA_IDWTFIDOC-DMBTR    TO WA_IDWTPARTNER-DMBTR.
    ADD WA_IDWTFIDOC-WT_TAXHH TO WA_IDWTPARTNER-WT_TAXHH.
    ADD WA_IDWTFIDOC-WT_QSFHH TO WA_IDWTPARTNER-WT_QSFHH.
    ADD WA_IDWTFIDOC-WT_QBSHH TO WA_IDWTPARTNER-WT_QBSHH.
    ADD WA_IDWTFIDOC-WT_QSSHH TO WA_IDWTPARTNER-WT_QSSHH.
    ADD WA_IDWTFIDOC-WT_AMREXMP TO WA_IDWTPARTNER-WT_AMREXMP.
    ADD WA_IDWTFIDOC-WT_AMRBASE TO WA_IDWTPARTNER-WT_AMRBASE.
    ADD WA_IDWTFIDOC-WT_AMEXCLVAT TO WA_IDWTPARTNER-WT_AMEXCLVAT.
    ADD WA_IDWTFIDOC-SKNTO    TO WA_IDWTPARTNER-SKNTO.
  ENDIF.

ENDFORM.                               " I01_sum_amounts_wtcode

*-----------------------------------------------------------------------
*------- Cumulate the amounts per company code -------------------------
*-----------------------------------------------------------------------
FORM I01_SUM_AMOUNTS_COMPANY.

* Do not sum up items if they are excluded from partner tab
  IF NOT WA_IDWTGLOB-WT_SC_EXCL_WT0 IS INITIAL
     AND WA_IDWTPARTNER-WT_QBSHH = 0.
* exit
  ELSE.
    ADD WA_IDWTPARTNER-DMBTR    TO WA_IDWTCOMPCD-DMBTR.
    ADD WA_IDWTPARTNER-WT_TAXHH TO WA_IDWTCOMPCD-WT_TAXHH.
    ADD WA_IDWTPARTNER-WT_QSFHH TO WA_IDWTCOMPCD-WT_QSFHH.
    ADD WA_IDWTPARTNER-WT_QBSHH TO WA_IDWTCOMPCD-WT_QBSHH.
    ADD WA_IDWTPARTNER-WT_QSSHH TO WA_IDWTCOMPCD-WT_QSSHH.
    ADD WA_IDWTPARTNER-WT_AMREXMP TO WA_IDWTCOMPCD-WT_AMREXMP.
    ADD WA_IDWTPARTNER-WT_AMRBASE TO WA_IDWTCOMPCD-WT_AMRBASE.
    ADD WA_IDWTPARTNER-WT_AMEXCLVAT TO WA_IDWTCOMPCD-WT_AMEXCLVAT.
    ADD WA_IDWTPARTNER-SKNTO    TO WA_IDWTCOMPCD-SKNTO.
  ENDIF.
ENDFORM.                               " I01_sum_amounts_company

*&---------------------------------------------------------------------*
*&      Form  I01_SELECT_WT_DATA
*&---------------------------------------------------------------------*
*       select the data :
*          - call the LDB process
*          - apply callback events
*          -
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM I01_SELECT_WT_DATA.

  DATA:
    LWA_SP     TYPE SYLDB_SP,
    LT_SEL_TAB LIKE RSPARAMS OCCURS 10  WITH HEADER LINE.

  DATA: L_SUBRC   LIKE SY-SUBRC.

* 17092001 - BoI Interface global area to BADI call back from LDB
  EXPORT WA_IDWTGLOB TO MEMORY ID C_MEM_IDWTGLOB.
* 17092001 - EoI Interface global area to BADI call back from LDB

  CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
    EXPORTING
      CURR_REPORT     = G_REPID                          "#EC DOM_EQUAL
    IMPORTING
      SP              = LWA_SP                              "#EC NEEDED
    TABLES
      SELECTION_TABLE = LT_SEL_TAB
    EXCEPTIONS
      NOT_FOUND       = 1
      NO_REPORT       = 2
      OTHERS          = 3.
  CHECK SY-SUBRC = 0.

**** New CIS Legal Change -- Import SELTAB to memory*****
  EXPORT LT_SEL_TAB TO MEMORY ID C_MEM_SELTAB.
**** New CIS Legal Change********************************

  CASE WA_DATA_SRC.

    WHEN 'DS1' OR 'DS7'.
      CALL FUNCTION 'FI_WT_DATA_SOURCE_DS1'
        EXPORTING
          IMP_IDWTGLOB      = WA_IDWTGLOB
        TABLES
          T_SEL_TAB         = LT_SEL_TAB
          T_FREE_SELPAR_TAB = WA_FREE_SELP_TAB
          T_MAIN_TAB        = MAN_QSTRMAIN
          T_IDWTERROR       = I_IDWTERROR
        EXCEPTIONS
          NO_BUDAT_LOW      = 1
          NO_SINGLE_PERIOD  = 2
          LDB_ERROR         = 3
          NOTHING_FOUND     = 4
          OTHERS            = 5.

    WHEN 'DS2'.
      CALL FUNCTION 'FI_WT_DATA_SOURCE_DS2'
        EXPORTING
          IMP_IDWTGLOB      = WA_IDWTGLOB
        TABLES
          T_SEL_TAB         = LT_SEL_TAB
          T_FREE_SELPAR_TAB = WA_FREE_SELP_TAB
          T_MAIN_TAB        = MAN_QSTRMAIN
          T_IDWTERROR       = I_IDWTERROR
        EXCEPTIONS
          NO_BUDAT_LOW      = 1
          NO_SINGLE_PERIOD  = 2
          LDB_ERROR         = 3
          NOTHING_FOUND     = 4
          OTHERS            = 5.

    WHEN 'DS3'.
      CALL FUNCTION 'FI_WT_DATA_SOURCE_DS3'
        EXPORTING
          IMP_IDWTGLOB      = WA_IDWTGLOB
        TABLES
          T_SEL_TAB         = LT_SEL_TAB
          T_FREE_SELPAR_TAB = WA_FREE_SELP_TAB
          T_MAIN_TAB        = MAN_QSTRMAIN
          T_IDWTERROR       = I_IDWTERROR
        EXCEPTIONS
          NO_BUDAT_LOW      = 1
          NO_SINGLE_PERIOD  = 2
          LDB_ERROR         = 3
          NOTHING_FOUND     = 4
          OTHERS            = 5.

      PERFORM F_GET_DATA_FOC.
    WHEN 'DS4'.
      CALL FUNCTION 'FI_WT_DATA_SOURCE_DS4'
        EXPORTING
          IMP_IDWTGLOB      = WA_IDWTGLOB
        TABLES
          T_SEL_TAB         = LT_SEL_TAB
          T_FREE_SELPAR_TAB = WA_FREE_SELP_TAB
          T_MAIN_TAB        = MAN_QSTRMAIN
          T_IDWTERROR       = I_IDWTERROR
        EXCEPTIONS
          NO_BUDAT_LOW      = 1
          NO_SINGLE_PERIOD  = 2
          LDB_ERROR         = 3
          NOTHING_FOUND     = 4
          OTHERS            = 5.

    WHEN 'DS5'.                        " combination of DS1 and DS2
      CALL FUNCTION 'FI_WT_DATA_SOURCE_DS1'
        EXPORTING
          IMP_IDWTGLOB      = WA_IDWTGLOB
        TABLES
          T_SEL_TAB         = LT_SEL_TAB
          T_FREE_SELPAR_TAB = WA_FREE_SELP_TAB
          T_MAIN_TAB        = MAN_QSTRMAIN
          T_IDWTERROR       = I_IDWTERROR
        EXCEPTIONS
          NO_BUDAT_LOW      = 1
          NO_SINGLE_PERIOD  = 2
          LDB_ERROR         = 3
          NOTHING_FOUND     = 4
          OTHERS            = 5.

      CALL FUNCTION 'FI_WT_DATA_SOURCE_DS2'
        EXPORTING
          IMP_IDWTGLOB      = WA_IDWTGLOB
        TABLES
          T_SEL_TAB         = LT_SEL_TAB
          T_FREE_SELPAR_TAB = WA_FREE_SELP_TAB
          T_MAIN_TAB        = MAN_QSTRMAIN
          T_IDWTERROR       = I_IDWTERROR
        EXCEPTIONS
          NO_BUDAT_LOW      = 1
          NO_SINGLE_PERIOD  = 2
          LDB_ERROR         = 3
          NOTHING_FOUND     = 4
          OTHERS            = 5.

    WHEN 'DS6'.                        " combination of DS3 and DS4
      CALL FUNCTION 'FI_WT_DATA_SOURCE_DS3'
        EXPORTING
          IMP_IDWTGLOB      = WA_IDWTGLOB
        TABLES
          T_SEL_TAB         = LT_SEL_TAB
          T_FREE_SELPAR_TAB = WA_FREE_SELP_TAB
          T_MAIN_TAB        = MAN_QSTRMAIN
          T_IDWTERROR       = I_IDWTERROR
        EXCEPTIONS
          NO_BUDAT_LOW      = 1
          NO_SINGLE_PERIOD  = 2
          LDB_ERROR         = 3
          NOTHING_FOUND     = 4
          OTHERS            = 5.

      CALL FUNCTION 'FI_WT_DATA_SOURCE_DS4'
        EXPORTING
          IMP_IDWTGLOB      = WA_IDWTGLOB
        TABLES
          T_SEL_TAB         = LT_SEL_TAB
          T_FREE_SELPAR_TAB = WA_FREE_SELP_TAB
          T_MAIN_TAB        = MAN_QSTRMAIN
          T_IDWTERROR       = I_IDWTERROR
        EXCEPTIONS
          NO_BUDAT_LOW      = 1
          NO_SINGLE_PERIOD  = 2
          LDB_ERROR         = 3
          NOTHING_FOUND     = 4
          OTHERS            = 5.
    WHEN OTHERS.
      MESSAGE E003.


  ENDCASE.

* Select addtional FI-CA data ---------------------------------
  IF X_FICA_ACTIVE = 'X'.
    SET EXTENDED CHECK OFF.

    CALL FUNCTION 'FKK_WT_DATA_SOURCE'
      EXPORTING
        IMP_IDWTGLOB      = WA_IDWTGLOB
      TABLES
        T_SEL_TAB         = LT_SEL_TAB
        T_FREE_SELPAR_TAB = WA_FREE_SELP_TAB
        T_MAIN_TAB        = MAN_QSTRMAIN
        T_IDWTERROR       = I_IDWTERROR
      EXCEPTIONS
        NO_BUDAT_LOW      = 1
        NO_SINGLE_PERIOD  = 2
        LDB_ERROR         = 3
        NOTHING_FOUND     = 4
        OTHERS            = 5.

    SET EXTENDED CHECK ON.
  ENDIF.

* Insert for BOE processing for France. BOE processing based on
* BOE due date and not posting date. Bills of exchange documents whose
* due dates do not fall within posting date criteria need to be
* filtered.

  L_SUBRC = SY-SUBRC.
  IF WA_DATA_SRC EQ 'DS2' OR WA_DATA_SRC EQ 'DS5' .
    PERFORM FILTER_BOE  TABLES MAN_QSTRMAIN
*                        changing sy-subrc .
                        CHANGING L_SUBRC.
  ENDIF.

*------- Check sy-subrc ------------------------------------------------
**  case sy-subrc.
*  CASE l_subrc.
*
*
**   ... if no items have been selected : message + deactivate forms
**       printing, file creation, and lists display.
*    WHEN '4'.
*      MOVE space TO wa_idwtglob-wt_sc_list_act1.
*      MOVE space TO wa_idwtglob-wt_sc_list_act2.
*      MOVE space TO wa_idwtglob-wt_sc_list_act3.
*      MOVE space TO wa_idwtglob-wt_sc_list_act4.
*      MOVE space TO wa_idwtglob-wt_sc_form_act1.
*      MOVE space TO wa_idwtglob-wt_sc_form_act2.
*      MOVE space TO wa_idwtglob-wt_sc_file_act.
**      MESSAGE i702(7q).                " No items have been selected
**
*    WHEN OTHERS.
*
*  ENDCASE.


ENDFORM.                               " wa_select_WT_DATA

*&---------------------------------------------------------------------*
*&      Form  I01_CHECK_P_upgm
*&---------------------------------------------------------------------*
*       check the existence of a user program
*----------------------------------------------------------------------*
FORM I01_CHECK_P_UPGM.

  IF  WA_IDWTGLOB-WT_OG_USER_PGM IS INITIAL.
    SET CURSOR FIELD 'P_UPGM'.
    MESSAGE E004.
  ENDIF.


ENDFORM.                               " I01_CHECK_P_upgm

*&---------------------------------------------------------------------*
*&      Form  I01_STORE_IDWTFIDOC
*&---------------------------------------------------------------------*
*       store the data in the FI line items table
*----------------------------------------------------------------------*
FORM I01_STORE_IDWTFIDOC.

*       exclude items with no withheld amount if this is required on
*       screen :
  IF NOT WA_IDWTGLOB-WT_SC_EXCL_WT0 IS INITIAL
     AND WA_IDWTFIDOC-WT_QBSHH = 0.
*          exclude
  ELSE.
*          keep
    WA_IDWTFIDOC-PARTNER_SEQ_NB = WA_IDWTFIDOC-PARTNER_SEQ_NB + 1.
    APPEND WA_IDWTFIDOC TO I_IDWTFIDOC.
  ENDIF.

ENDFORM.                               " I01_STORE_IDWTFIDOC

*&---------------------------------------------------------------------*
*&      Form  I01_STORE_IDWTPARTNER
*&---------------------------------------------------------------------*
*       store the data in the Vendor / WT types and codes table
*----------------------------------------------------------------------*
FORM I01_STORE_IDWTPARTNER.

*        exclude partners with no withheld amount if this is
*        required on screen :
  IF NOT WA_IDWTGLOB-WT_SC_EXCL_WT0 IS INITIAL
     AND WA_IDWTPARTNER-WT_QBSHH = 0.
*           exclude
  ELSE.
*           keep
    APPEND WA_IDWTPARTNER TO I_IDWTPARTNER .
  ENDIF.

ENDFORM.                               " I01_STORE_IDWTPARTNER

*&---------------------------------------------------------------------*
*&      Form  I01_STORE_IDWTCOMPCD
*&---------------------------------------------------------------------*
*       store the data in the compnay codes table
*----------------------------------------------------------------------*
FORM I01_STORE_IDWTCOMPCD.

*     exclude companies with no withheld amount if this is required on
*     screen :
  IF NOT WA_IDWTGLOB-WT_SC_EXCL_WT0 IS INITIAL
     AND WA_IDWTCOMPCD-WT_QBSHH = 0.
*        exclude
  ELSE.
*        keep
    APPEND WA_IDWTCOMPCD TO I_IDWTCOMPCD.
  ENDIF.

ENDFORM.                               " I01_STORE_IDWTCOMPCD

*&---------------------------------------------------------------------*
*&      Form  I01_EXPRESS_WITH_FACTOR
*&---------------------------------------------------------------------*
*       express the amounts with respect of the selected factor :
*----------------------------------------------------------------------*
FORM I01_EXPRESS_WITH_FACTOR.


  IF WA_IDWTGLOB-WT_SC_UNIT IS INITIAL OR
     WA_IDWTGLOB-WT_SC_UNIT EQ 0       OR
     WA_IDWTGLOB-WT_SC_UNIT EQ 1.
    EXIT.
  ELSE.

    DIVIDE WA_QSTRMAIN-DMBTR    BY WA_IDWTGLOB-WT_SC_UNIT.
    DIVIDE WA_QSTRMAIN-WRBTR    BY WA_IDWTGLOB-WT_SC_UNIT.
    DIVIDE WA_QSTRMAIN-WT_QSFHH BY WA_IDWTGLOB-WT_SC_UNIT.
    DIVIDE WA_QSTRMAIN-WT_QSFHB BY WA_IDWTGLOB-WT_SC_UNIT.
    DIVIDE WA_QSTRMAIN-WT_QBSHH BY WA_IDWTGLOB-WT_SC_UNIT.
    DIVIDE WA_QSTRMAIN-WT_QBSHB BY WA_IDWTGLOB-WT_SC_UNIT.
    DIVIDE WA_QSTRMAIN-WT_QSSHH BY WA_IDWTGLOB-WT_SC_UNIT.
    DIVIDE WA_QSTRMAIN-WT_QSSHB BY WA_IDWTGLOB-WT_SC_UNIT.
    DIVIDE WA_QSTRMAIN-WT_NETHH BY WA_IDWTGLOB-WT_SC_UNIT.
    DIVIDE WA_QSTRMAIN-WT_NETHB BY WA_IDWTGLOB-WT_SC_UNIT.
    DIVIDE WA_QSTRMAIN-WT_TAXHH BY WA_IDWTGLOB-WT_SC_UNIT.
    DIVIDE WA_QSTRMAIN-WT_TAXHB BY WA_IDWTGLOB-WT_SC_UNIT.
    DIVIDE WA_QSTRMAIN-SKNTO    BY WA_IDWTGLOB-WT_SC_UNIT.
    DIVIDE WA_QSTRMAIN-WSKTO    BY WA_IDWTGLOB-WT_SC_UNIT.

  ENDIF.

ENDFORM.                               " I01_EXPRESS_WITH_FACTOR

*&---------------------------------------------------------------------*
*&      Form  I01_CONVERT_LOCAL_TO_ALTERNATIVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM I01_CONVERT_LOCAL_TO_ALTERNAT.

*------- Convert local company code currencies into alternative  -------
*------ -currency ALTCURR     with exchange rate at CONVDATE -------
*BoD Note 1114450
*  CHECK NOT wa_idwtglob-wt_sc_altcurr  IS INITIAL AND
*         wa_idwtglob-wt_sc_convdate <> '00000000'.
*EoD Note 1114450

*BoI Note 1114450
  DATA L_CONV_DT LIKE WA_IDWTGLOB-WT_SC_CONVDATE.

  CLEAR L_CONV_DT.

  CHECK NOT WA_IDWTGLOB-WT_SC_ALTCURR  IS INITIAL.

  IF P_POSDT = 'X'. "Posting Date is selected
    L_CONV_DT = WA_QSTRMAIN-BUDAT.

  ELSEIF P_CLGDT = 'X'. "Clearing Date
* If clearing date is empty, consider system date
    IF WA_QSTRMAIN-AUGDT <> '00000000'.
      L_CONV_DT = WA_QSTRMAIN-AUGDT.
    ELSE.
      L_CONV_DT = SY-DATUM.
    ENDIF.

  ELSEIF P_SYSDT = 'X'. "Current System Date

    L_CONV_DT = SY-DATUM.

  ELSEIF P_UDFDT = 'X'. "User Defined Date
* If user hasn't given a date, consider system date.
    IF  WA_IDWTGLOB-WT_SC_CONVDATE <> '00000000'.
      L_CONV_DT = WA_IDWTGLOB-WT_SC_CONVDATE.
    ELSE.
      L_CONV_DT = SY-DATUM.
    ENDIF.

  ENDIF.
*EoI Note 1114450

*    loop at wa_qstrmain.

  CHECK WA_IDWTGLOB-WT_SC_ALTCURR <> WA_QSTRMAIN-HWAER.

  IF WA_QSTRMAIN-DMBTR <> 0.
    LV_DMBTR = WA_QSTRMAIN-DMBTR.
  ELSE.
    IF WA_QSTRMAIN-WT_QSSHH <> 0.
      LV_DMBTR = WA_QSTRMAIN-WT_QSSHH.
    ELSE.
      WA_QSTRMAIN-HWAER = WA_IDWTGLOB-WT_SC_ALTCURR.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
    EXPORTING
* BoD Note 1114450
*     date             = wa_idwtglob-wt_sc_convdate      "#EC DOM_EQUAL
* EoD Note 1114450
* BoI Note 1114450
      DATE             = L_CONV_DT
* EoI Note 1114450
      FOREIGN_CURRENCY = WA_IDWTGLOB-WT_SC_ALTCURR
      LOCAL_AMOUNT     = LV_DMBTR
      LOCAL_CURRENCY   = WA_QSTRMAIN-HWAER
      TYPE_OF_RATE     = 'M'
    IMPORTING
      FOREIGN_AMOUNT   = LV_DMBTR_ALT
    EXCEPTIONS
      NO_RATE_FOUND    = 1
      OVERFLOW         = 2
      NO_FACTORS_FOUND = 3
      NO_SPREAD_FOUND  = 4
      DERIVED_2_TIMES  = 5
      OTHERS           = 6.

  IF SY-SUBRC = 0.
    LV_ALTRATE = LV_DMBTR_ALT / LV_DMBTR.
    WA_QSTRMAIN-HWAER    = WA_IDWTGLOB-WT_SC_ALTCURR.
    WA_QSTRMAIN-DMBTR    = WA_QSTRMAIN-DMBTR    * LV_ALTRATE.
    WA_QSTRMAIN-WT_QSSHH = WA_QSTRMAIN-WT_QSSHH * LV_ALTRATE.
    WA_QSTRMAIN-WT_QBSHH = WA_QSTRMAIN-WT_QBSHH * LV_ALTRATE.
    WA_QSTRMAIN-WT_QSFHH = WA_QSTRMAIN-WT_QSFHH * LV_ALTRATE.
    WA_QSTRMAIN-WT_NETHH = WA_QSTRMAIN-WT_NETHH * LV_ALTRATE.
    WA_QSTRMAIN-WT_TAXHH = WA_QSTRMAIN-WT_TAXHH * LV_ALTRATE.
    WA_QSTRMAIN-SKNTO    = WA_QSTRMAIN-SKNTO    * LV_ALTRATE.
  ELSE.
    IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      CLEAR WA_ERROR.
      WA_ERROR-WT_MSGID       = SY-MSGID.
      WA_ERROR-WT_MSGTY       = SY-MSGTY.
      WA_ERROR-WT_MSGNR       = SY-MSGNO.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
           INTO WA_ERROR-WT_NATXT.
      PERFORM F01_FILL_IDWTERROR TABLES I_IDWTERROR[]
                               USING WA_ERROR.
    ENDIF.
  ENDIF.

ENDFORM.                               " I01_CONVERT_LOCAL_TO_ALTERNAT.

* 14092001 - BoI Additional field for countries
*&---------------------------------------------------------------------*
*&      Form  i01_check_additional_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM I01_CHECK_ADDITIONAL_FIELD .

  DATA: ADD_EXIT TYPE REF TO IF_EX_IDWTREP_ADDFUNCINT.
  DATA: LAND1 TYPE INTCA.
* Check active additional selection from BADI per country by P_LAND1

  MOVE P_LAND1 TO LAND1.
  CLEAR F_ACTIVE_ADD_SEL_FIELD.

  CALL METHOD CL_EXITHANDLER=>GET_INSTANCE
    CHANGING
      INSTANCE = ADD_EXIT.


  CALL METHOD ADD_EXIT->SET_FLAG_USE_ADD_SELECTION
    EXPORTING
      FLT_VAL        = LAND1
      I_GLOB         = WA_IDWTGLOB                "Note 764501
    CHANGING
      CH_USE_ADD_SEL = F_ACTIVE_ADD_SEL_FIELD.
  IF SY-SUBRC NE 0.

    IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
      MESSAGE E024 WITH 'SET_FLAG_USE_ADD_SELECTION'.
    ELSE.
      CLEAR WA_ERROR.
      WA_ERROR-WT_MSGID       = 'ID_WT'.
      WA_ERROR-WT_MSGTY       = 'E'.
      WA_ERROR-WT_MSGNR       = 24.
      MESSAGE E024 WITH 'SET_FLAG_USE_ADD_SELECTION'
              INTO WA_ERROR-WT_NATXT.
      PERFORM F01_FILL_IDWTERROR TABLES I_IDWTERROR[]
                                      USING WA_ERROR.
    ENDIF.
  ENDIF.

* Note # 723872
*  IF sy-batch IS INITIAL AND
*     sy-ucomm EQ 'SJOB' AND
*     NOT sy-slset IS INITIAL AND
*     NOT f_active_add_sel_field IS INITIAL.
*
*    SUBMIT (sy-cprog)
*           TO SAP-SPOOL
*           USING SELECTION-SET sy-slset .
*
*  ENDIF.

ENDFORM.                    " i01_check_additional_field

*&---------------------------------------------------------------------*
*&      Form  get_add_sel_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ADD_SEL_FIELDS .

* call the method from Country-BADI IDWTREP_ADDFUNCINT to get additional
* info from user

  CALL METHOD EXIT->ADDINFO_ENTER_BEFORE_SELECTION
    EXPORTING
      FLT_VAL  = WA_IDWTGLOB-INTCA
      I_GLOB   = WA_IDWTGLOB
    IMPORTING
      E_FIELDS = I_FIELDS[]
    CHANGING
      C_ERROR  = I_IDWTERROR[].

  IF SY-SUBRC EQ 0.

*   Export to specific memory ID
    EXPORT I_FIELDS TO MEMORY ID C_MEM_ADD_FIELD.

*   Set flag for execute in background
    F_EXISTING_EXPORT_FIELDS = 'X'.

  ELSE.
    IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
      MESSAGE E024 WITH 'ADDINFO_ENTER_BEFORE_SELECTION'.
    ELSE.
      CLEAR WA_ERROR.
      WA_ERROR-WT_MSGID       = 'ID_WT'.
      WA_ERROR-WT_MSGTY       = 'E'.
      WA_ERROR-WT_MSGNR       = 24.
      MESSAGE E024 WITH 'ADDINFO_ENTER_BEFORE_SELECTION'
              INTO WA_ERROR-WT_NATXT.
      PERFORM F01_FILL_IDWTERROR TABLES I_IDWTERROR[]
                                      USING WA_ERROR.
    ENDIF.
  ENDIF.

ENDFORM.                    " get_add_sel_fields
* 14092001 - EoI Additional field for countries
*&---------------------------------------------------------------------*
*&      Form  I01_CHECK_SPECIAL_GL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*Note 545125 begin
FORM I01_CHECK_SPECIAL_GL.
  DATA: T074U,
        WA_T074U       TYPE T074U,
        ACCT-TYPE      TYPE T074U-KOART,
        INT            TYPE I,
        TEXT_UMSKZ(20) TYPE C.

  DATA: BEGIN OF TAB OCCURS 0,
          TEXT(1) TYPE C,
        END OF TAB.
*Note 605880 begin
  CONDENSE P_AUMSKZ NO-GAPS.
*Eliminate the duplicated special G/L indicators
  WHILE  P_AUMSKZ(1) NE SPACE.
    TAB-TEXT = P_AUMSKZ(1).
    COLLECT TAB.
    SHIFT P_AUMSKZ LEFT.
*Note 605880 end
  ENDWHILE.
* Determine specific account type according to the output group.
  IF WA_IDWTGLOB-WT_DATA_SOURCE = 'DS1' OR
     WA_IDWTGLOB-WT_DATA_SOURCE = 'DS2' OR
     WA_IDWTGLOB-WT_DATA_SOURCE = 'DS5' OR
     WA_IDWTGLOB-WT_DATA_SOURCE = 'DS7'.  "Note 787062
    ACCT-TYPE = 'K'.
  ELSE.
    IF WA_IDWTGLOB-WT_DATA_SOURCE = 'DS3' OR
       WA_IDWTGLOB-WT_DATA_SOURCE = 'DS4' OR
       WA_IDWTGLOB-WT_DATA_SOURCE = 'DS6'.
      ACCT-TYPE = 'D'.
    ENDIF.
  ENDIF.

* Check if the special G/L indicator is exsiting in specific account
* type.
  LOOP AT TAB.
    SELECT * FROM T074U INTO WA_T074U
      WHERE
        KOART  = ACCT-TYPE        AND
        UMSKZ  = TAB-TEXT.
    ENDSELECT.
    IF SY-SUBRC = 0.
* Check if the special G/L indicator has downpayment or BOE G/L trans.
* type
      IF ( WA_T074U-UMSKS = 'A' ) OR
         ( WA_T074U-UMSKS = 'W' ).
        TEXT_UMSKZ(1) = TAB-TEXT.
        SHIFT TEXT_UMSKZ RIGHT.
        INT = INT + 1.
      ELSE.
*Note 605880 begin
        P_AUMSKZ(1) = TAB-TEXT.
        SHIFT P_AUMSKZ RIGHT.
*Note 605880 end
      ENDIF.
    ELSE.
      TEXT_UMSKZ(1) = TAB-TEXT.
      SHIFT TEXT_UMSKZ RIGHT.
      INT = INT + 1.
    ENDIF.
    AT LAST.
*Note 605880 begin
      SHIFT P_AUMSKZ LEFT.
*Note 605880 end
    ENDAT.

  ENDLOOP.
  IF INT > 0.
*Note 605880 begin
    SET CURSOR FIELD 'P_AUMSKZ'.
*Note 605880 end
    MESSAGE W043(ID_WT) WITH TEXT_UMSKZ.
  ENDIF.
ENDFORM.                    " I01_CHECK_SPECIAL_GL
*Note 545125 end
*&---------------------------------------------------------------------*
*& Form F_GET_DATA_FOC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_DATA_FOC .

  DATA : LS_DATA LIKE LINE OF MAN_QSTRMAIN[],
         LS_FI   LIKE LINE OF I_IDWTFIDOC[].

  SELECT WITH_ITEM~*
    FROM ZSDSFIT054
    INNER JOIN WITH_ITEM ON ZSDSFIT054~DOCNO    EQ WITH_ITEM~BELNR AND
                            ZSDSFIT054~CTNUMBER EQ WITH_ITEM~CTNUMBER
    WHERE ZSDSFIT054~DOCNO IN @P_BELNR[]
    INTO TABLE @DATA(LT_TMP).

  LOOP AT LT_TMP INTO DATA(LS_TMP).
    LS_DATA-BUKRS           = LS_TMP-BUKRS.
    LS_DATA-BUPLA           = '0000'.
    LS_DATA-PARTNO_TYPE     = ''.
    LS_DATA-PARTNERNO       = LS_TMP-WT_ACCO.
    LS_DATA-WT_REVERSED     = ''.
    LS_DATA-WT_PERIOD       = ''.
    LS_DATA-MONA            = ''.
    LS_DATA-WITHT           = LS_TMP-WITHT.
    LS_DATA-WT_WITHCD       = LS_TMP-WT_WITHCD.
    LS_DATA-GJAHR           = LS_TMP-GJAHR.
    LS_DATA-BELNR           = LS_TMP-BELNR.
    LS_DATA-BUZEI           = LS_TMP-BUZEI.
    LS_DATA-ITEM_SOURCE     = ''.
    LS_DATA-ITEM_REF        = ''.
    LS_DATA-BUDAT           = LS_TMP-CTISSUEDATE.
    LS_DATA-LIFNR           = ''.
    LS_DATA-KUNNR           = LS_TMP-WT_ACCO.
    LS_DATA-KOART           = LS_TMP-KOART.
    LS_DATA-GSBER           = ''.
    LS_DATA-BLDAT           = ''.
    LS_DATA-SGTXT           = ''.
    LS_DATA-AUGDT           = ''.
    LS_DATA-AUGBL           = ''.
    LS_DATA-WT_PAYDT        = ''.
    LS_DATA-UMSKS           = ''.
    LS_DATA-UMSKZ           = ''.
    LS_DATA-ZUMSK           = ''.
    LS_DATA-XBLNR           = ''.
    LS_DATA-BLART           = ''.
    LS_DATA-BSCHL           = ''.
    LS_DATA-XZAHL           = ''.
    LS_DATA-XCPDD           = ''.
    LS_DATA-SHKZG           = ''.
    LS_DATA-MWSKZ           = ''.
    LS_DATA-REBZG           = ''.
    LS_DATA-REBZJ           = ''.
    LS_DATA-REBZZ           = ''.
    LS_DATA-REBZT           = ''.
    LS_DATA-XREF2           = ''.
    LS_DATA-XREF3           = ''.
    LS_DATA-HWAER           = ''.
    LS_DATA-DMBTR           = ''.
    LS_DATA-WAERS           = ''.
    LS_DATA-WRBTR           = ''.
    LS_DATA-WT_QSSHH        = ABS( LS_TMP-WT_QSSHH ).
    LS_DATA-WT_QSSHB        = ABS( LS_TMP-WT_QSSHB ).
    LS_DATA-WT_QBSHH        = ABS( LS_TMP-WT_QBSHH ).
    LS_DATA-WT_QBSHB        = ABS( LS_TMP-WT_QBSHB ).
    LS_DATA-WT_QSFHH        = ABS( LS_TMP-WT_QSFHH ).
    LS_DATA-WT_QSFHB        = ABS( LS_TMP-WT_QSFHB ).
*    LS_DATA-WT_NETHH        = LS_TMP-WT_NETHH.
*    LS_DATA-WT_NETHB        = LS_TMP-WT_NETHB.
*    LS_DATA-WT_TAXHH        = LS_TMP-WT_TAXHH.
*    LS_DATA-WT_TAXHB        = LS_TMP-WT_TAXHB.
*    LS_DATA-WT_PAYBH        = LS_TMP-WT_PAYBH.
*    LS_DATA-WT_PAYBB        = LS_TMP-WT_PAYBB.
*    LS_DATA-WT_XDMBTR       = LS_TMP-WT_XDMBTR.
*    LS_DATA-WT_XWRBTR       = LS_TMP-WT_XWRBTR.
*    LS_DATA-WT_XDMBTR1      = LS_TMP-WT_XDMBTR1.
*    LS_DATA-WT_XWRBTR1      = LS_TMP-WT_XWRBTR1.
*    LS_DATA-WT_XDMBTR2      = LS_TMP-WT_XDMBTR2.
*    LS_DATA-WT_XWRBTR2      = LS_TMP-WT_XWRBTR2.
    LS_DATA-SKNTO           = ''.
    LS_DATA-WSKTO           = ''.
    LS_DATA-QSREC           = LS_TMP-QSREC.
    LS_DATA-RCTXT           = ''.
    LS_DATA-QSREP           = ''.
    LS_DATA-QSCOD           = ''.
    LS_DATA-RPTXT           = ''.
    LS_DATA-QSATZ           = ''.
    LS_DATA-WT_SLFWTPD      = ''.
    LS_DATA-WT_GRUWTPD      = ''.
    LS_DATA-WT_OPOWTPD      = ''.
    LS_DATA-CTNUMBER        = LS_TMP-CTNUMBER.
    LS_DATA-WT_WTEXMN       = ''.
    LS_DATA-WT_QSZRT        = ''.
    LS_DATA-LAND1           = ''.
    LS_DATA-SECCO           = ''.
    LS_DATA-WT_POSIN        = ''.
    LS_DATA-WT_BASMAN       = ''.
    LS_DATA-WT_AMNMAN       = ''.
    LS_DATA-WT_OPEN_ITEM    = ''.
    LS_DATA-WT_NEWWT        = ''.
    LS_DATA-WT_POSTM        = ''.
    LS_DATA-WT_TCODE        = ''.
    LS_DATA-WT_MWSK1        = ''.
    LS_DATA-WT_ISSDT        = LS_TMP-ctissuedate.
    LS_DATA-ZFBDT           = ''.
    LS_DATA-EMPFB           = ''.
    LS_DATA-STCD1           = ''.
    LS_DATA-STCD2           = ''.
    LS_DATA-BUKRS_ORG       = ''.
    LS_DATA-LIFNR_ORG       = ''.
    LS_DATA-KUNNR_ORG       = ''.
    LS_DATA-STENR           = ''.
    LS_DATA-AUGGJ           = ''.
    LS_DATA-SKFBT           = ''.
    LS_DATA-J_1TPBUPL       = ''.
    LS_DATA-STBLG           = ''.
    APPEND LS_DATA TO MAN_QSTRMAIN.

    APPEND LS_DATA-BELNR TO GT_DOC_FOC.
*    MOVE-CORRESPONDING LS_DATA TO LS_FI.
*    APPEND LS_FI TO I_IDWTFIDOC[].

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_DOC_FOC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_SET_DOC_FOC .

  LOOP AT GT_DOC_FOC INTO DATA(LS_FOC).
    READ TABLE GT_IDWTFIDOC INTO DATA(LS_DATA)
    WITH KEY BELNR = LS_FOC-DOCNO.
    IF SY-SUBRC EQ 0.
      APPEND LS_DATA TO I_IDWTFIDOC.
    ENDIF.
  ENDLOOP.

ENDFORM.
