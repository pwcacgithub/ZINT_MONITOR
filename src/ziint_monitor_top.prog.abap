*&---------------------------------------------------------------------*
*& Include          ZIINT_MONITOR_TOP
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Constants Declaration
*&---------------------------------------------------------------------*
CONSTANTS: gc_rfc_cpi_msglog TYPE rvari_vnam VALUE 'P_RFC_CPI_MSGLOG'.

*&---------------------------------------------------------------------*
*& Type Declaration
*&---------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_message_log,
    icon                 TYPE c,
    messageguid          TYPE string,
    integrationflowname  TYPE string,
    applicationmessageid TYPE string,
    start_date           TYPE dats,
    start_time           TYPE tims,
    sender               TYPE string,
    receiver             TYPE string,
    status               TYPE string,
    error_log            TYPE string,
  END OF ty_message_log .

*&---------------------------------------------------------------------*
*& Data Declaration
*&---------------------------------------------------------------------*
DATA: gt_message_log TYPE STANDARD TABLE OF ty_message_log,
      gt_fieldcat    TYPE slis_t_fieldcat_alv,
      gv_rfc_dest    TYPE rfcdest.

*&---------------------------------------------------------------------*
*& Tables
*&---------------------------------------------------------------------*
TABLES: ztint_monitor.
