*&---------------------------------------------------------------------*
*& Include          ZIINT_MONITOR_SEL
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Selection Screen
*&---------------------------------------------------------------------*
PARAMETERS: p_intsys TYPE char20 USER-COMMAND cm1 AS LISTBOX VISIBLE LENGTH 20 OBLIGATORY.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
  SELECT-OPTIONS: s_date FOR sy-datum NO-EXTENSION,
                  s_objid FOR ztint_monitor-object_id NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b01.
