*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0330_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_2000 OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_2000 OUTPUT.
  SET PF-STATUS '2000'.
  SET TITLEBAR '2000'.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module TC_INPUT_CHANGE_TC_ATTR OUTPUT
*&---------------------------------------------------------------------*
*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_INPUT'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TC_INPUT_CHANGE_TC_ATTR OUTPUT ##NEEDED.
  TC_INPUT-LINES =  LINES( GT_INPUT ).
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module TC_INPUT_GET_LINES OUTPUT
*&---------------------------------------------------------------------*
*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_INPUT'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE TC_INPUT_GET_LINES OUTPUT.
  G_TC_INPUT_LINES = SY-LOOPC.
ENDMODULE.
