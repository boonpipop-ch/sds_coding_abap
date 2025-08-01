*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0420_MODULE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101_EXIT INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'OUT'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'BT_CANCEL'.
      CLEAR : GV_VENDOR,GV_PO_TYPE,GV_PUR_GROUP.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0101_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101 INPUT.
  CASE SY-UCOMM.
    WHEN 'BT_CRE'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0101 OUTPUT.
  SET PF-STATUS 'ZGUI_101'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0101  OUTPUT
