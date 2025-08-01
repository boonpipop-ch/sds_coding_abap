"Name: \TY:CL_IM_UKM_SD_FSCM_INTEGR1\IN:IF_EX_BADI_SD_CM\ME:FSCM_DISPLAY_CREDIT_MESSAGES\SE:BEGIN\EI
ENHANCEMENT 0 ZSDS_FICM_UKM_SD_FSCM_INTEGR1.
*-----------------------------------------------------------------------
*  Program ID         : ZSDS_FICM_UKM_SD_FSCM_INTEGR1
*  Creation Date      : 20.08.2024
*  Author             : Boontip R. (Eviden)
*  Add-on ID          : FSCME035
*  Description        : if found order type in genc and no dialog then
*                       merging message into 1 line to show message in
*                       status bar
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  18/03/2025   F36K914377  Boontip R.  CR 420000498 change position of
*                                       export memory
*-----------------------------------------------------------------------
  ASSIGN ('(SAPLVKMP)XVBAK-AUART') TO FIELD-SYMBOL(<L_AUART>).
  IF <L_AUART> IS ASSIGNED.
    IF ZCL_SDSFICM_UKM_SD_FSCM=>CHECK_ACTIVE_WBS_ORDER_TYPE( <L_AUART> ) = ABAP_TRUE
    OR ZCL_SDSFICM_UKM_SD_FSCM=>CHECK_ACTIVE_NORMAL_ORDER_TYPE( <L_AUART> ) = ABAP_TRUE.
*BOD 420000498
*      EXPORT CREDIT_MESSAGE = GT_FSCM_CREDIT_MESSAGES  TO MEMORY ID 'ZSDS_FSCM'.
*EOD 420000498
      IF  I_NO_DIALOG = ABAP_FALSE AND I_MESSAGE_TYPE = 'E'.
        CALL METHOD ZCL_SDSFICM_UKM_SD_FSCM=>CREDIT_SET_MESSAGE_SHOW_ERROR
          CHANGING
            CT_FSCM_CREDIT_MESSAGES = GT_FSCM_CREDIT_MESSAGES.
      ENDIF.
    ENDIF.
  ENDIF.
ENDENHANCEMENT.
