*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0480
*  Creation Date      : 26.09.2024
*  Author             : Lakkhana U.(Eviden)
*  Add-on ID          : N/A
*  Description        : Post Contract Services to RAR
*                       WRICEP ZFIARE040
*  Purpose            :
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer   CR/IMS               Search No.
*  Description
*-----------------------------------------------------------------------
*  05.06.2025  F36K918884  Lakkhana U.  420000649/420000680  CH01
*  SVO balance is zero? and Skip post amount in svo item = 0
*-----------------------------------------------------------------------
*  14.07.2025  F36K921150  Lakkhana U.  420000637,420000714  CH02
*  1. Add Posting doc & Display Report for () MA Contract
*  2. Add STAT_CANCELLED to check Status
*-----------------------------------------------------------------------
REPORT ZSDSFIR0480.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*
INCLUDE ZSDSFIR0480_TOP.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_CON-TCODE.
  PERFORM F_GET_CONSTANTS.
  PERFORM F_SET_SELSCR_DEFAULT.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM F_SET_SEL_SCREEN.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM F_SET_INIT_DATA.
  PERFORM F_CHK_SEL_SCREEN.
*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  CASE GC_CON-TRUE.
    WHEN RB_SOMA.
      PERFORM F_GET_LOGS_SOMA CHANGING GT_LOGS
                                       GT_BKPF.

      PERFORM F_GET_DATA_SOMA CHANGING GT_LOGS
                                       GT_DATA_H
                                       GT_DATA.
      CASE GC_CON-TRUE.
        WHEN RB_POST.
          PERFORM F_POST_DOCUMENT_SOMA USING     GC_MODE-TEST_RUN
                                       CHANGING  GT_DATA_H
                                                 GT_DATA
                                                 GT_OUTPUT.
        WHEN RB_REPT.
          PERFORM F_PREPARE_REPORT_SOMA CHANGING GT_LOGS
                                               GT_OUTPUT.
      ENDCASE.
    WHEN RB_WBS.

      PERFORM F_GET_LOGS_WBS_EXT CHANGING GT_LOGS
                                          GT_BKPF
                                          GT_DATA
                                          GF_ERROR.
      CASE GC_CON-TRUE.
        WHEN RB_POST.
          IF GF_ERROR IS INITIAL.

            PERFORM F_GET_WBS_EXTEND_WARRANTY USING GRT_WBS_EXT
                                              CHANGING GT_DATA_H
                                                       GT_DATA ##PERF_GLOBAL_PAR_OK.



            PERFORM F_POST_DOCUMENT_WBS_EXT USING GC_MODE-TEST_RUN
                                                  GT_DATA_H
                                         CHANGING GT_DATA
                                                  GT_OUTPUT.
          ELSE.
            GT_OUTPUT[] = GT_DATA[].
          ENDIF.
        WHEN RB_REPT.
          PERFORM F_PREPARE_REPORT_WBS_EXT USING GT_LOGS
                                   CHANGING GT_OUTPUT.
      ENDCASE.
    WHEN RB_SOSLA.
      PERFORM F_GET_LOGS_SOSLA CHANGING GT_LOGS
                                        GT_BKPF
                                        GT_DATA
                                        GF_ERROR.
      IF GF_ERROR IS INITIAL.
        PERFORM F_GET_DATA_SOSLA CHANGING GT_LOGS
                                          GT_DATA_H
                                          GT_DATA.
      ENDIF.
      CASE GC_CON-TRUE.
        WHEN RB_POST.
          IF GF_ERROR IS INITIAL.
            PERFORM F_POST_DOCUMENT_SOSLA USING    GC_MODE-TEST_RUN
                                                   GT_DATA_H
                                          CHANGING GT_DATA
                                                   GT_OUTPUT.
          ELSE.
            GT_OUTPUT[] = GT_DATA[].
          ENDIF.
        WHEN RB_REPT.
          PERFORM F_PREPARE_REPORT_SOSLA CHANGING GT_LOGS
                                            GT_OUTPUT.
      ENDCASE.
*-BOI ++CH02 Lakkhana(14.07.2025)
    WHEN RB_MACON.
      PERFORM F_GET_LOGS_MACON CHANGING GT_LOGS
                                       GT_BKPF.

      PERFORM F_GET_DATA_MACON CHANGING GT_LOGS
                                        GT_DATA.
      CASE GC_CON-TRUE.
        WHEN RB_POST.
          PERFORM F_POST_DOCUMENT_MACON USING    GC_MODE-TEST_RUN
                                       CHANGING  GT_DATA
                                                 GT_OUTPUT.
        WHEN RB_REPT.
          PERFORM F_PREPARE_REPORT_MACON CHANGING GT_LOGS
                                               GT_OUTPUT.
      ENDCASE.
*-EOI ++CH02 Lakkhana(14.07.2025)
  ENDCASE.

*  CASE GC_CON-TRUE.
*    WHEN RB_POST.
*      PERFORM F_GET_DATA CHANGING GT_LOGS
*                                  GT_DATA.

*      PERFORM F_POST_DOCUMENT USING     GC_MODE-TEST_RUN
*                              CHANGING  GT_DATA
*                                        GT_OUTPUT.
*    WHEN RB_REPT.
*      PERFORM F_PREPARE_REPORT USING GT_LOGS
*                               CHANGING GT_OUTPUT.
*  ENDCASE.

  IF GT_OUTPUT IS INITIAL.
*   Message: No data found.
    MESSAGE S001(ZSDSCA01).
    RETURN.
  ENDIF.
*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
*   Display Processing Result
  PERFORM F_DISPLAY_RESULT USING GT_OUTPUT.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE ZSDSCAI9990 ##INCL_OK.
  INCLUDE ZSDSFIR0480_F01.
