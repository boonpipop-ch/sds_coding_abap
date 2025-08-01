"Name: \TY:CL_IM_UKM_SD_FSCM_INTEGR1\IN:IF_EX_BADI_SD_CM\ME:FSCM_CREDIT_CHECK_ORDER\SE:BEGIN\EI
ENHANCEMENT 0 ZSDS_FICM_UKM_SD_FSCM_INTEGR1.
*-----------------------------------------------------------------------
*  Program ID         : ZSDS_FICM_UKM_SD_FSCM_INTEGR1
*  Creation Date      : 20.08.2024
*  Author             : Boontip R. (Eviden)
*  Add-on ID          : FSCME035
*  Description        : if found payment term in genc => no check credit
*                       if Project order type found WBS => check credit by WBS budget
*                       if normal order that found in genc => check credit as customized
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  15/01/2025   F36K911050  Boontip R.  CR 420000239 add check ZZRUNNING
*  18/03/2025   F36K914377  Boontip R.  CR 420000498 change position of
*                                       export memory
*-----------------------------------------------------------------------
  READ TABLE XVBKD INTO DATA(LS_VBKD) INDEX 1.
  IF ZCL_SDSFICM_UKM_SD_FSCM=>IGNORE_CREDIT_CHECK(
      IF_VKORG = XVBAK-VKORG
      IF_ZTERM = LS_VBKD-ZTERM
      IF_VBTYP = XVBAK-VBTYP ) = ABAP_TRUE.
    CLEAR: GT_FSCM_CREDIT_MESSAGES,
           FSCM_TE_CHECK,
           FSCM_SET.
    RETURN.
  ENDIF.
  IF ZCL_SDSFICM_UKM_SD_FSCM=>CHECK_ACTIVE_WBS_ORDER_TYPE( XVBAK-AUART ) = ABAP_TRUE.
*boi CR 420000239
    IF XVBAK-ZZRUNNING = 1.
      CALL METHOD ZCL_SDSFICM_UKM_SD_FSCM=>CREDIT_CHECK_NORMAL_ORDER
        EXPORTING
          IF_TRTYP         = TRTYP
          IS_XVBAK         = XVBAK
          IT_XVBAP         = XVBAP
        CHANGING
          CT_MESSAGE       = GT_FSCM_CREDIT_MESSAGES
          CF_FSCM_TE_CHECK = FSCM_TE_CHECK
          CF_FSCM_SET      = FSCM_SET.
*BOI CR 420000498
      EXPORT CREDIT_MESSAGE = GT_FSCM_CREDIT_MESSAGES  TO MEMORY ID 'ZSDS_FSCM'.
*EOI CR 420000498
    ELSE.
*eoi CR 420000239
      CALL METHOD ZCL_SDSFICM_UKM_SD_FSCM=>CREDIT_CHECK_WBS_ORDER
        EXPORTING
          IF_TRTYP         = TRTYP
          IS_XVBAK         = XVBAK
          IT_XVBAP         = XVBAP
        CHANGING
          CT_MESSAGE       = GT_FSCM_CREDIT_MESSAGES
          CF_FSCM_TE_CHECK = FSCM_TE_CHECK
          CF_FSCM_SET      = FSCM_SET.
*BOI CR 420000498
      EXPORT CREDIT_MESSAGE = GT_FSCM_CREDIT_MESSAGES  TO MEMORY ID 'ZSDS_FSCM'.
*EOI CR 420000498
    ENDIF. "CR 420000239
    RETURN.
  ELSEIF ZCL_SDSFICM_UKM_SD_FSCM=>CHECK_ACTIVE_NORMAL_ORDER_TYPE( XVBAK-AUART ) = ABAP_TRUE.
    CALL METHOD ZCL_SDSFICM_UKM_SD_FSCM=>CREDIT_CHECK_NORMAL_ORDER
      EXPORTING
        IF_TRTYP         = TRTYP
        IS_XVBAK         = XVBAK
        IT_XVBAP         = XVBAP
      CHANGING
        CT_MESSAGE       = GT_FSCM_CREDIT_MESSAGES
        CF_FSCM_TE_CHECK = FSCM_TE_CHECK
        CF_FSCM_SET      = FSCM_SET.
*BOI CR 420000498
      EXPORT CREDIT_MESSAGE = GT_FSCM_CREDIT_MESSAGES  TO MEMORY ID 'ZSDS_FSCM'.
*EOI CR 420000498
    RETURN.
  ENDIF.

ENDENHANCEMENT.
