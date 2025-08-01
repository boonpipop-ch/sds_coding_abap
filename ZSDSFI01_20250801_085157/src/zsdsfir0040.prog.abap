*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0040
*  Creation Date      : 30.01.2024
*  Author             : Jakarin S.
*  Add-on ID          : ZFIARF009
*  Description        : Generic Withholding Tax Reporting
*  Purpose            :
*  Copied from        : <<Reference Program>>
*  Restriction        :
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSFIR0040 MESSAGE-ID ID_WT.
************************************************************************
* Global data :                                                        *
*----------------------------------------------------------------------*
INCLUDE ZSDSFIR0040_TOP.
INCLUDE I_RFIDYYWT_TOP.
************************************************************************
* Screen interface :                                                   *
*----------------------------------------------------------------------*
INCLUDE I_RFIDYYWT_001.


INITIALIZATION.
************************************************************************
* INITIALIZATION :                                                     *
*----------------------------------------------------------------------*
  WA_POINT = 'initialization'.
  PERFORM I01_INITIALIZATION.
  WA_POINT = 'no-more-init'.

* Initialization include for FICA usage only
  INCLUDE I_RFIDYYWT_FICA_INIT.

************************************************************************
* SCREEN CHECKS ( ON SELECTION FIELDS )                                *
*----------------------------------------------------------------------*

*-------------------------------------- check country + output group ---
AT SELECTION-SCREEN ON BLOCK M1.

  PERFORM I01_CHECK_OUTPUT_GROUP.

* 14092001 - BoI Additional field for countries
AT SELECTION-SCREEN ON P_LAND1.
*------- check if additional field needed in some country
  PERFORM I01_CHECK_ADDITIONAL_FIELD.
* 14092001 - EoI Additional field for countries


*----------------------------------------- check reporting period ------
AT SELECTION-SCREEN ON P_BUDAT.

  PERFORM I01_CHECK_REPORTING_PERIOD.

*Note 545125 begin
*----------------------------------------- check special G/L ind ------
*Note 605880 begin
AT SELECTION-SCREEN ON P_AUMSKZ.
*Note 605880 end
  PERFORM I01_CHECK_SPECIAL_GL.
*Note 545125 end
*----------------------------------------- check company codes ---------
AT SELECTION-SCREEN ON P_BUKRS.

  PERFORM I01_CHECK_COMPANY_CODES.

*----------------------------------------- check WT types --------------
AT SELECTION-SCREEN ON P_WITHT.

  PERFORM I01_CHECK_WT_TYPES.

*----------------------------------------- check WT codes --------------
AT SELECTION-SCREEN ON P_WTHCD.

  IF P_WTHCD NE SPACE  OR
     P_QSSKZ NE SPACE.
    PERFORM I01_CHECK_WT_CODES.
  ENDIF.

*----------------------------------------- check business place --------
AT SELECTION-SCREEN ON P_BUPLA.

  PERFORM I01_CHECK_BUS_PLACE.

*----------------------------------------- Alternative local currency --
AT SELECTION-SCREEN ON P_EXCDT.

  IF NOT P_ALCUR IS INITIAL AND P_EXCDT IS INITIAL.
    P_EXCDT = SY-DATUM.
  ENDIF.

*----------------------------------------- User program activation -----
AT SELECTION-SCREEN ON P_UPGM.

  IF NOT P_UPGM IS INITIAL.
    PERFORM I01_CHECK_P_UPGM.
  ENDIF.

*----------------------------------------- check ALV lists variants ----
AT SELECTION-SCREEN ON P_VAR1.
  PERFORM F01_ALV_VARIANTE_EXIST USING C_COMPANY P_VAR1.

AT SELECTION-SCREEN ON P_VAR2.
  PERFORM F01_ALV_VARIANTE_EXIST USING C_VEND_TAX P_VAR2.

AT SELECTION-SCREEN ON P_VAR3.
  PERFORM F01_ALV_VARIANTE_EXIST USING C_FIITEMS P_VAR3.

AT SELECTION-SCREEN ON P_VAR4.
  PERFORM F01_ALV_VARIANTE_EXIST USING C_ERRORS P_VAR4.

* Selection screen checks for FICA
  INCLUDE I_RFIDYYWT_FICA_CHK.

************************************************************************
* SELECTION SCREEN - Checks after input  ( P.A.I.)                     *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  PERFORM I01_CONTROL_SEL_SCREEN_PAI.

* call the method from Country-BADI IDWTREP_ADDFUNCINT to check screen
** input

  CALL METHOD CL_EXITHANDLER=>GET_INSTANCE
    CHANGING
      INSTANCE = EXIT.

  CALL METHOD EXIT->SCREEN_CHECKS_AFTER_INPUT
    EXPORTING
      FLT_VAL = WA_IDWTGLOB-INTCA
      I_GLOB  = WA_IDWTGLOB.



************************************************************************
* F4 HELPS ( ON VALUE-REQUEST )                                        *
*----------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FNM1.
  PERFORM VALUE_REQ_FILE_OUT USING P_FNM1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VAR1.
  PERFORM F01_ALV_VARIANTE_F4 USING C_COMPANY.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VAR2.
  PERFORM F01_ALV_VARIANTE_F4 USING C_VEND_TAX.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VAR3.
  PERFORM F01_ALV_VARIANTE_F4 USING C_FIITEMS.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VAR4.
  PERFORM F01_ALV_VARIANTE_F4 USING C_ERRORS.

************************************************************************
* SELECTION SCREEN PREPARATION  ( P.B.O.)                              *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  PERFORM I01_CONTROL_SEL_SCREEN_PBO.


************************************************************************
* DATA SELECTION :                                                     *
*----------------------------------------------------------------------*
START-OF-SELECTION.

*  first call for business add-ins :

*   call method cl_exithandler=>get_instance
*        changing instance = exit.
  WA_IDWTGLOB-WT_REPORT_VARIANT = SY-SLSET.  "Note 787598

  CALL METHOD CL_EXITHANDLER=>GET_INSTANCE
    CHANGING
      INSTANCE = USEREXIT.

* 14092001 - BoI Check execute in background  (Batch)
  IF SY-BATCH IS INITIAL AND
     F_EXISTING_EXPORT_FIELDS IS INITIAL.
* 14092001 - EoI Check execute in background  (Batch)

* call the method from Country-BADI IDWTREP_ADDFUNCINT to get additional
* info from user
* e.g. send popup to collect the info for updating issues.

    CALL METHOD EXIT->ADDINFO_ENTER_BEFORE_SELECTION
      EXPORTING
        FLT_VAL  = WA_IDWTGLOB-INTCA
        I_GLOB   = WA_IDWTGLOB
      IMPORTING
        E_FIELDS = I_FIELDS[]
      CHANGING
        C_ERROR  = I_IDWTERROR[].

*Note 774204 -BOI
    IF SY-SUBRC EQ 0.

*   Export to specific memory ID
      EXPORT I_FIELDS TO MEMORY ID C_MEM_ADD_FIELD.

    ELSE.
*Note 774204 -EOI

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

* 14092001 - BoI Check execute in background  (Batch)
  ELSE.
*   Import selection enter on screen from memory ID
    IMPORT I_FIELDS FROM MEMORY ID C_MEM_ADD_FIELD.

* BoI - Additional fields from additional variant
*   In case execute in Background and no memory saved
    IF I_FIELDS IS INITIAL.
*     call method with exit when finished build up i_fields
      CALL METHOD EXIT->ADDINFO_ENTER_BEFORE_SELECTION
        EXPORTING
          FLT_VAL  = WA_IDWTGLOB-INTCA
          I_GLOB   = WA_IDWTGLOB
        IMPORTING
          E_FIELDS = I_FIELDS[]
        CHANGING
          C_ERROR  = I_IDWTERROR[].

*Note 774204 -BOI
      IF SY-SUBRC EQ 0.

*   Export to specific memory ID
        EXPORT I_FIELDS TO MEMORY ID C_MEM_ADD_FIELD.

      ELSE.
*Note 774204 -EOI

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
    ENDIF.
* EoI - Additional fields from additional variant

  ENDIF.
* 14092001 - EoI Check execute in background  (Batch)

* call the method from User-BADI WTAXREPORT_MODIFY for customers

  CALL METHOD USEREXIT->ENTER_ADDINFO_BEFORE_SELECTION
    EXPORTING
      FLT_VAL  = WA_IDWTGLOB-INTCA
      I_GLOB   = WA_IDWTGLOB
    IMPORTING
      E_FIELDS = I_FIELDS[]
    CHANGING
      C_ERROR  = I_IDWTERROR[].


  IF SY-SUBRC NE 0.
    IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
      MESSAGE E018 WITH 'ENTER_ADDINFO_BEFORE_SELECTION'.
    ELSE.
      CLEAR WA_ERROR.
      WA_ERROR-WT_MSGID       = 'ID_WT'.
      WA_ERROR-WT_MSGTY       = 'E'.
      WA_ERROR-WT_MSGNR       = 18.
      MESSAGE E018 WITH 'ENTER_ADDINFO_BEFORE_SELECTION'
            INTO WA_ERROR-WT_NATXT.
      PERFORM F01_FILL_IDWTERROR TABLES I_IDWTERROR[]
                                      USING WA_ERROR.
    ENDIF.
  ENDIF.



*  data selection: call LDB + callback events + build the table QSTRMAIN

  PERFORM I01_SELECT_WT_DATA.

END-OF-SELECTION.

************************************************************************
* BEFORE OUTPUT PROCESSING :                                           *
*----------------------------------------------------------------------*

*  1 - processing at the FI line item level with QSTRMAIN table :
*-----------------------------------------------------------------------

* modify the qstrmain to fulfill the country specific requirements
* e.g. simulate the w/tax for Japan; country specific limit check

  CALL METHOD EXIT->QSTRMAIN_MODIFY_BEFORE_OUTPUT
    EXPORTING
      FLT_VAL    = WA_IDWTGLOB-INTCA
      I_GLOB     = WA_IDWTGLOB
      I_FIELDS   = I_FIELDS[]
    CHANGING
      C_QSTRMAIN = MAN_QSTRMAIN[]
      C_ERROR    = I_IDWTERROR[].

  IF SY-SUBRC NE 0.

    IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
      MESSAGE E024 WITH 'QSTRMAIN_MODIFY_BEFORE_OUTPUT'.
    ELSE.
      CLEAR WA_ERROR.
      WA_ERROR-WT_MSGID       = 'ID_WT'.
      WA_ERROR-WT_MSGTY       = 'E'.
      WA_ERROR-WT_MSGNR       = 24.
      MESSAGE E024 WITH 'QSTRMAIN_MODIFY_BEFORE_OUTPUT'
            INTO WA_ERROR-WT_NATXT.
      PERFORM F01_FILL_IDWTERROR TABLES I_IDWTERROR[]
                                      USING WA_ERROR.
    ENDIF.
  ENDIF.


* call the BADI for user and partner:

  CALL METHOD USEREXIT->MODIFY_FIDOC_BEFORE_OUTPUT
    EXPORTING
      FLT_VAL     = WA_IDWTGLOB-INTCA
      I_FIELDS    = I_FIELDS[]
      I_GLOB      = WA_IDWTGLOB
    CHANGING
      C_FILINEDOC = MAN_QSTRMAIN[]
      C_ERROR     = I_IDWTERROR[].


  IF SY-SUBRC NE 0.

    IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
      MESSAGE E018 WITH 'MODIFY_FIDOC_BEFORE_OUTPUT'.
    ELSE.
      CLEAR WA_ERROR.
      WA_ERROR-WT_MSGID       = 'ID_WT'.
      WA_ERROR-WT_MSGTY       = 'E'.
      WA_ERROR-WT_MSGNR       = 18.
      MESSAGE E018 WITH 'MODIFY_FIDOC_BEFORE_OUTPUT'
            INTO WA_ERROR-WT_NATXT.
      PERFORM F01_FILL_IDWTERROR TABLES I_IDWTERROR[]
                                      USING WA_ERROR.
    ENDIF.
  ENDIF.




*  2 - processing at other levels with internal tables IDWTxxxx :
*-----------------------------------------------------------------------

*  create the internal tables IDWTxxxx :

  PERFORM I01_CREATE_REPORTING_DATA.

*  free memory associated with man_qstrmain

  GT_IDWTFIDOC = I_IDWTFIDOC[].

  FREE MAN_QSTRMAIN.


*  business add-in to modify data inside the internal tables
*  before output :
*  ---------------------------------------------------------

  CALL METHOD EXIT->ALLDATA_MODIFY_BEFORE_OUTPUT
    EXPORTING
      FLT_VAL   = WA_IDWTGLOB-INTCA
      I_GLOB    = WA_IDWTGLOB
      I_FIELDS  = I_FIELDS[]
    CHANGING
      C_COMPCD  = I_IDWTCOMPCD[]
      C_PARTNER = I_IDWTPARTNER[]
      C_FIDOC   = I_IDWTFIDOC[]
      C_ERROR   = I_IDWTERROR[]
    EXCEPTIONS
      OTHERS    = 1.
  IF SY-SUBRC NE 0.

    IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
      MESSAGE E024 WITH 'ALLDATA_MODIFY_BEFORE_OUTPUT'.
    ELSE.
      CLEAR WA_ERROR.
      WA_ERROR-WT_MSGID       = 'ID_WT'.
      WA_ERROR-WT_MSGTY       = 'E'.
      WA_ERROR-WT_MSGNR       = 24.
      MESSAGE E024 WITH 'ALLDATA_MODIFY_BEFORE_OUTPUT'
            INTO WA_ERROR-WT_NATXT.
      PERFORM F01_FILL_IDWTERROR TABLES I_IDWTERROR[]
                                      USING WA_ERROR.
    ENDIF.
  ENDIF.


* call the BADI for customer and partner:

  CALL METHOD USEREXIT->MODIFY_DATA_BEFORE_OUTPUT
    EXPORTING
      FLT_VAL   = WA_IDWTGLOB-INTCA
      I_FIELDS  = I_FIELDS[]
      I_GLOB    = WA_IDWTGLOB
    CHANGING
      C_COMPCD  = I_IDWTCOMPCD[]
      C_PARTNER = I_IDWTPARTNER[]
      C_FIDOC   = I_IDWTFIDOC[]
      C_ERROR   = I_IDWTERROR[]
    EXCEPTIONS
      OTHERS    = 1.

  IF SY-SUBRC NE 0.

    IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
      MESSAGE E018 WITH 'MODIFY_DATA_BEFORE_OUTPUT'.
    ELSE.
      CLEAR WA_ERROR.
      WA_ERROR-WT_MSGID       = 'ID_WT'.
      WA_ERROR-WT_MSGTY       = 'E'.
      WA_ERROR-WT_MSGNR       = 18.
      MESSAGE E018 WITH 'MODIFY_DATA_BEFORE_OUTPUT'
            INTO WA_ERROR-WT_NATXT.
      PERFORM F01_FILL_IDWTERROR TABLES I_IDWTERROR[]
                                      USING WA_ERROR.
    ENDIF.
  ENDIF.




************************************************************************
* OUTPUT DISPATCHING :                                                 *
*----------------------------------------------------------------------*

*   call the user program if necessary :

  IF NOT WA_IDWTGLOB-WT_SC_USER_ACT IS INITIAL.
    PERFORM OUTPUT_CREATION IN PROGRAM (WA_IDWTGLOB-WT_OG_USER_PGM)
            USING WA_IDWTGLOB
                  I_IDWTCOMPCD[]
                  I_IDWTPARTNER[]
                  I_IDWTFIDOC[]
         CHANGING I_IDWTERROR[]
            .
  ENDIF.

  IF SY-SUBRC NE 0.

    IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
      MESSAGE E032 WITH WA_IDWTGLOB-WT_OG_USER_PGM SY-SUBRC.
    ELSE.
      CLEAR WA_ERROR.
      WA_ERROR-WT_MSGID       = 'ID_WT'.
      WA_ERROR-WT_MSGTY       = 'E'.
      WA_ERROR-WT_MSGNR       = 32.
      MESSAGE E032  WITH WA_IDWTGLOB-WT_OG_USER_PGM SY-SUBRC
              INTO WA_ERROR-WT_NATXT.
      PERFORM F01_FILL_IDWTERROR TABLES I_IDWTERROR[]
                                      USING WA_ERROR.
    ENDIF.
  ENDIF.


*    Create required lists, file, forms :
  MOVE '0'  TO WA_IDWTGLOB-DISP_METHOD.

*  IF GT_DOC_FOC IS NOT INITIAL.
*    PERFORM F_SET_DOC_FOC.
*  ENDIF.

  IF I_IDWTFIDOC[] IS INITIAL.

    MOVE SPACE TO WA_IDWTGLOB-WT_SC_LIST_ACT1.
    MOVE SPACE TO WA_IDWTGLOB-WT_SC_LIST_ACT2.
    MOVE SPACE TO WA_IDWTGLOB-WT_SC_LIST_ACT3.
    MOVE SPACE TO WA_IDWTGLOB-WT_SC_LIST_ACT4.
    MOVE SPACE TO WA_IDWTGLOB-WT_SC_FORM_ACT1.
    MOVE SPACE TO WA_IDWTGLOB-WT_SC_FORM_ACT2.
    MOVE SPACE TO WA_IDWTGLOB-WT_SC_FILE_ACT.

    MESSAGE I702(7Q).                " No items have been selected
  ELSE.
*----> Begin Comment by Teerasithi.
*  CALL FUNCTION 'IDWT_DISPATCHER'
*    EXPORTING
*      i_glob              = wa_idwtglob
*      i_fields            = i_fields[]
*    CHANGING
*      c_compcd            = i_idwtcompcd[]
*      c_partner           = i_idwtpartner[]
*      c_fidoc             = i_idwtfidoc[]
*      c_error             = i_idwterror[]
*    EXCEPTIONS
*      error_form          = 1
*      error_file          = 2
*      error_screen_output = 3
*      error_multi         = 4
*      OTHERS              = 5.
*  IF sy-subrc NE 0.
**         MESSAGE already processed by the dispatcher itself
*  ENDIF.
*<---- End Comment by Teerasithi.
*----> Begin Insert by Teerasithi.
    CALL FUNCTION 'Z_SDSCA_IDWT_DISPATCHER'
      EXPORTING
        I_GLOB              = WA_IDWTGLOB
        I_FIELDS            = I_FIELDS[]
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
    IF SY-SUBRC NE 0.
*         MESSAGE already processed by the dispatcher itself
    ENDIF.
*<---- End Insert by Teerasithi.
  ENDIF.

************************************************************************
*  AFTER OUTPUT PROCESSING :                                           *
*----------------------------------------------------------------------*



************************************************************************
************************************************************************
************************************************************************
* END  OF  PROCESS                                                     *
*----------------------------------------------------------------------*

************************************************************************
* List of 'processing' INCLUDEs                                        *
*----------------------------------------------------------------------*
  INCLUDE ZSDSFIR0040_RFIDYYWT_I01.

************************************************************************
* List of function modules called                                      *
*----------------------------------------------------------------------*
  INCLUDE I_RFIDYYWT_F01.
  INCLUDE I_RFIDYYWT_FICA_F01.


************************************************************************
  .


  INCLUDE I_RFIDYYWT_I02.
