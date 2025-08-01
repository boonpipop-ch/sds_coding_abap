*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0290_I01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       Module  EXIT_COMMANDS INPUT
*----------------------------------------------------------------------*
*       Cancel transaction and leave the program
*----------------------------------------------------------------------*
MODULE EXIT_COMMANDS INPUT.
  GF_SAVE_OK = GF_OK_CODE_1.
  CLEAR GF_OK_CODE_1.

  CASE GF_SAVE_OK.
    WHEN 'BACK' OR 'END' OR 'CANC'.
      PERFORM F_USER_COMMAND_EXIT.

  ENDCASE.

ENDMODULE.                 " EXIT_COMMANDS  INPUT
*----------------------------------------------------------------------*
*       Module  USER_COMMAND_ALV  INPUT
*----------------------------------------------------------------------*
*       User-Commands from ALV
*----------------------------------------------------------------------*
MODULE USER_COMMAND_ALV INPUT.

* to react on oi_custom_events:
  CALL METHOD CL_GUI_CFW=>DISPATCH.

  GF_SAVE_OK = GF_OK_CODE_1.
  CLEAR GF_OK_CODE_1.
  CASE GF_SAVE_OK.
    WHEN GC_SAVE_1.
      PERFORM F_USER_COMMAND_SAVE.
    WHEN GC_PRINT_1.
      PERFORM F_USER_COMMAND_PRINT.
    WHEN GC_CANCEL_1.
      PERFORM F_USER_COMMAND_CANCEL.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_ALV  INPUT
