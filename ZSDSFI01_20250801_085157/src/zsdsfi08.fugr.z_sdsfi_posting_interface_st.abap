FUNCTION Z_SDSFI_POSTING_INTERFACE_ST.
*"--------------------------------------------------------------------
*"*"Global Interface:
*"  IMPORTING
*"     VALUE(I_CLIENT) LIKE  SY-MANDT DEFAULT SY-MANDT
*"     VALUE(I_FUNCTION) LIKE  RFIPI-FUNCT
*"     VALUE(I_GROUP) LIKE  APQI-GROUPID DEFAULT SPACE
*"     VALUE(I_HOLDDATE) LIKE  APQI-STARTDATE DEFAULT SPACE
*"     VALUE(I_KEEP) LIKE  APQI-QERASE DEFAULT SPACE
*"     VALUE(I_MODE) LIKE  RFPDO-ALLGAZMD DEFAULT 'N'
*"     VALUE(I_UPDATE) LIKE  RFPDO-ALLGVBMD DEFAULT 'S'
*"     VALUE(I_USER) LIKE  APQI-USERID DEFAULT SPACE
*"     VALUE(I_XBDCC) LIKE  RFIPI-XBDCC DEFAULT SPACE
*"     VALUE(I_BDC_APP_AREA) TYPE  BDC_APP_AREA DEFAULT SPACE
*"  EXCEPTIONS
*"      CLIENT_INCORRECT
*"      FUNCTION_INVALID
*"      GROUP_NAME_MISSING
*"      MODE_INVALID
*"      UPDATE_INVALID
*"      USER_INVALID
*"--------------------------------------------------------------------

  GET RUN TIME FIELD RUNTIME.
*------- Vorhergehende Mappe schließen? --------------------------------
  IF GROUP_OPEN = 'X'.
    PERFORM MAPPE_SCHLIESSEN.

*   falls kein Startdatum und keine Zeit übergeben werden soll
*   erwartet der 'JOB_CLOSE' nicht initial sondern SPACE
    IF BDCSTRTDT IS INITIAL.
       MOVE SPACE TO BDCSTRTDT.
    ENDIF.
    IF BDCSTRTTM IS INITIAL.
       MOVE SPACE TO BDCSTRTTM.
    ENDIF.
    IF ( FUNCT = 'B'  AND BDCIMMED = 'X' )
    OR ( FUNCT = 'B'  AND BDCSTRTDT NE SPACE ).
      PERFORM MAPPE_ABSPIELEN_IM_BATCH.
    ENDIF.
  ENDIF.


  MANDT     = I_CLIENT.
  FUNCT     = I_FUNCTION.
  GROUP     = I_GROUP.
  HOLDD     = I_HOLDDATE.
  XKEEP     = I_KEEP.
  MODE      = I_MODE.
  UPDATE    = I_UPDATE.
  USNAM     = I_USER.
  XBDCC     = I_XBDCC.
  BDC_APP_AREA = I_BDC_APP_AREA.

*------- Prüfung der Schnittstellenfelder ------------------------------
  IF MANDT NE SY-MANDT.
    MESSAGE E001 WITH MANDT SY-MANDT RAISING CLIENT_INCORRECT.
  ENDIF.

  IF FUNCT CN 'BCI'.
    MESSAGE E002 WITH FUNCT RAISING FUNCTION_INVALID.
  ENDIF.

  CASE FUNCT.

*------- Funktion: Batch-Input -----------------------------------------
    WHEN 'B'.
      IF GROUP = SPACE.
        MESSAGE E003 RAISING GROUP_NAME_MISSING.
      ENDIF.
      PERFORM MAPPE_OEFFNEN.

*------- Funktion: Call Transaction ... Using ... ----------------------
    WHEN 'C'.
      IF MODE CN 'ANE'.
        MESSAGE E004 WITH MODE RAISING MODE_INVALID.
      ENDIF.
      IF UPDATE CN 'SAL'.
        MESSAGE E005 WITH UPDATE RAISING UPDATE_INVALID.
      ENDIF.
      IF XBDCC = 'X' AND GROUP IS INITIAL.
        MESSAGE E003 RAISING GROUP_NAME_MISSING.
      ENDIF.

*------- Funktion: Interaktive Buchungsschnittstelle
    WHEN 'I'.

  ENDCASE.

*------- NewGL active ? ----------------------------------- Note1605537*
  IF GLFLEX_ACTIVE IS INITIAL.
    CALL FUNCTION 'FAGL_CHECK_GLFLEX_ACTIVE'
      IMPORTING
        E_GLFLEX_ACTIVE = GLFLEX_ACTIVE
      EXCEPTIONS
        ERROR_MESSAGE   = 1 ##FM_SUBRC_OK.
  ENDIF.

ENDFUNCTION.
