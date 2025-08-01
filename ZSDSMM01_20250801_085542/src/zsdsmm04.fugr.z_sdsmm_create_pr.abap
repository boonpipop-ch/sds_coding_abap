FUNCTION Z_SDSMM_CREATE_PR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_HEADER) TYPE  ZSDSMMS002 OPTIONAL
*"     VALUE(IT_ITEM) TYPE  ZSDSMMS003_TT OPTIONAL
*"     VALUE(IT_ASSET_NUM) TYPE  ZSDSFIS012_TT OPTIONAL
*"     VALUE(I_GET_DATA_FROM_K2) TYPE  FLAG OPTIONAL
*"     VALUE(I_WEBNO) TYPE  CHAR20 OPTIONAL
*"     VALUE(I_SEND_MAIL) TYPE  FLAG OPTIONAL
*"     VALUE(I_TEST_RUN) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     VALUE(E_OUTPUT) TYPE  BANFN
*"     VALUE(E_MESSAGE) TYPE  CHAR255
*"----------------------------------------------------------------------
  CLEAR : GS_DATA-GV_LOCK,GS_DATA, GS_DATA-GV_COATING,GS_DATA-GV_EKGRP.
  LCL_DATA=>GET_CONFIG( ).
  IF I_GET_DATA_FROM_K2 EQ ABAP_TRUE.
    LCL_DATA=>GET_DATA_FROM_K2( EXPORTING I_WEB_NO     = I_WEBNO
                                IMPORTING E_HEADER_PR  = I_HEADER
                                          ET_ITEM      = IT_ITEM ).

    IT_ASSET_NUM = LCL_DATA=>CREATE_FIX_ASSET( IT_ITEM = IT_ITEM ).
  ENDIF.

  GS_DATA-GT_ITEM      = IT_ITEM.
  GS_DATA-GS_HEADER    = I_HEADER.
*  IT_ASSET_NUM = LCL_DATA=>CREATE_FIX_ASSET( IT_ITEM = IT_ITEM ).
  GS_DATA-GT_ASSET     = IT_ASSET_NUM.
  GS_DATA-GT_ASSET_TMP = IT_ASSET_NUM.
  GS_DATA-GV_EMAIL     = I_SEND_MAIL.

  IF GS_DATA-GV_MESSAGE IS INITIAL.
*--------------------------------------------------------------------*
* Create PR
*--------------------------------------------------------------------*
    LCL_DATA=>CREATE_PR( I_TEST_RUN ).
*--------------------------------------------------------------------*
* End Create PR
*--------------------------------------------------------------------*
    E_OUTPUT  = GS_DATA-GV_PR_SAP.
    E_MESSAGE = GS_DATA-GV_MESSAGE.

    IF I_GET_DATA_FROM_K2 EQ ABAP_TRUE AND
       I_TEST_RUN         EQ SPACE.
      LCL_DATA=>SDSMMT006( I_WEB_NO = I_WEBNO
                           I_PR_NO  = E_OUTPUT ).

      IF GS_DATA-GV_EMAIL EQ ABAP_TRUE AND
         E_OUTPUT IS NOT INITIAL.
        LCL_DATA=>SEND_EMAIL_PR( E_OUTPUT ).
      ENDIF.
    ENDIF.

  ELSE.
    E_MESSAGE = GS_DATA-GV_MESSAGE.
  ENDIF.

  IF E_OUTPUT IS INITIAL AND
     GS_DATA-GT_ASSET IS NOT INITIAL.
    LCL_DATA=>UPDATE_ZSDSMMT022( E_OUTPUT ).
  ENDIF.
  IF GS_DATA-GV_LOCK EQ ABAP_TRUE.
    LCL_DATA=>UNLOCK_TABLE_ZSDSMMT022( ).
  ENDIF.
ENDFUNCTION.
