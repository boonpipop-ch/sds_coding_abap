*&---------------------------------------------------------------------*
*& Include          ZRFPPD0020_SEL
*&---------------------------------------------------------------------*
TABLES:
  t001w.

*&---------------------------------------------------------------------*
*& Selection screen definition                                         *
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1
  WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: so_plant FOR t001w-werks NO INTERVALS OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
