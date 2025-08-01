FUNCTION Z_SDSFI_POSTING_INTERFACE_END.
*"----------------------------------------------------------------------
*"*"Global Interface:
*"  IMPORTING
*"     VALUE(I_BDCIMMED) LIKE  RFIPI-BDCIMMED DEFAULT SPACE
*"     VALUE(I_BDCSTRTDT) LIKE  TBTCJOB-SDLSTRTDT DEFAULT NO_DATE
*"     VALUE(I_BDCSTRTTM) LIKE  TBTCJOB-SDLSTRTTM DEFAULT NO_TIME
*"  EXCEPTIONS
*"      SESSION_NOT_PROCESSABLE
*"----------------------------------------------------------------------

  BDCIMMED  = I_BDCIMMED.
  BDCSTRTDT = I_BDCSTRTDT.
  BDCSTRTTM = I_BDCSTRTTM.

* falls kein Startdatum und keine Zeit übergeben werden soll
* erwartet der 'JOB_CLOSE' nicht initial sondern SPACE
  IF BDCSTRTDT IS INITIAL.
     MOVE SPACE TO BDCSTRTDT.
  ENDIF.
  IF BDCSTRTTM IS INITIAL.
     MOVE SPACE TO BDCSTRTTM.
  ENDIF.


  IF GROUP_OPEN = 'X'.
    CALL FUNCTION 'BDC_CLOSE_GROUP'.
    CLEAR GROUP_OPEN.
    IF ( FUNCT = 'B'  AND BDCIMMED = 'X' )
    OR ( FUNCT = 'B'  AND BDCSTRTDT NE SPACE ).
* fuer die IDOC-Verarbeitung soll sichergestellt werden, dass
* die Mappe existiert.
      COMMIT WORK.
      PERFORM MAPPE_ABSPIELEN_IM_BATCH.
    ENDIF.
  ENDIF.

  CLEAR:  BDCIMMED.
  BDCSTRTDT = SPACE.
  BDCSTRTTM = SPACE.
ENDFUNCTION.
