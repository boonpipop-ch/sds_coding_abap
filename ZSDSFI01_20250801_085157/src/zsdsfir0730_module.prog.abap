*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0730_MODULE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101_EXIT INPUT.

CASE  SY-UCOMM.
  WHEN 'BACK'.
    LEAVE TO SCREEN 0.
  WHEN 'EXIT'.
    LEAVE TO SCREEN 0.
  WHEN 'CANCEL'.
    LEAVE PROGRAM.
ENDCASE.

ENDMODULE.                 " USER_COMMAND_0101_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101 INPUT.

ENDMODULE.                 " USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0101 OUTPUT.
  SET PF-STATUS 'Z101'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*& Module CALL_ALV OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE CALL_ALV OUTPUT.
  IF GCL_ALV IS INITIAL.
    LCL_DATA=>CREATE_OBJECT_ALV( ).
    LCL_DATA=>EXCLUDING_TOOLBAR( ).
    LCL_DATA=>ADD_EVENT( ).
    LCL_DATA=>GUI_STATUS( ).
    LCL_DATA=>SET_LAYOUT_OO( ).
    LCL_DATA=>SET_FCAT_OO( ).
    LCL_DATA=>CALL_ALV( ).
  ENDIF.
ENDMODULE.

MODULE CALL_ALV2 OUTPUT.
  IF GCL_ALV2 IS INITIAL.
    LCL_DATA=>CREATE_OBJECT_ALV2( ).
    LCL_DATA=>EXCLUDING_TOOLBAR2( ).
    LCL_DATA=>ADD_EVENT2( ).
    LCL_DATA=>GUI_STATUS2( ).
    LCL_DATA=>SET_LAYOUT2_OO( ).
    LCL_DATA=>SET_FCAT2_OO( ).
    LCL_DATA=>CALL_ALV2( ).
  ENDIF.
ENDMODULE.
