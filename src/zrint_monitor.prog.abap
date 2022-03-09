*&---------------------------------------------------------------------*
*& Report Z252_INT_MONITOR
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program ID: Z252_INT_MONITOR
*& Program Title: Integrated Monitor Tool
*& Description: Tool helps the users to monitor the Interfaces in
*&              S/4 system and other systems
*&---------------------------------------------------------------------*
*& Modification History
*&---------------------------------------------------------------------*
*& Date        User ID        REQ#        Transport# / Description
*&---------------------------------------------------------------------*
*& 30/04/2020  SSUDHENDRA001  V1          E2TK903247 - Initial version
*&---------------------------------------------------------------------*
REPORT zrint_monitor.

*&---------------------------------------------------------------------*
*& Includes
*&---------------------------------------------------------------------*
*** Top Include - Data Declaration
INCLUDE ziint_monitor_top.

*** Selection Screen Include - Selection Screen Declaration
INCLUDE ziint_monitor_sel.

*** Subroutine Include - Subroutine Declaration
INCLUDE ziint_monitor_sub.

*&---------------------------------------------------------------------*
*& Initialization Event
*&---------------------------------------------------------------------*
INITIALIZATION.
  DATA: lt_list  TYPE vrm_values,
        ls_value LIKE LINE OF lt_list.

  ls_value-key = '1'.
  ls_value-text = 'IDoc'.
  APPEND ls_value TO lt_list.

  ls_value-key = '2'.
  ls_value-text = 'Proxy'.
  APPEND ls_value TO lt_list.

  ls_value-key = '3'.
  ls_value-text = 'CPI'.
  APPEND ls_value TO lt_list.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_INTSYS'
      values = lt_list.

  REFRESH lt_list[].

*&---------------------------------------------------------------------*
*& Selection Screen Output Event
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  CASE p_intsys.
*** Display selection fields for CPI Integration
    WHEN '3'.
      LOOP AT SCREEN.
        IF screen-name CS 'S_DATE' OR screen-name CS 'S_OBJID' OR screen-name CS 'B01'.
          screen-active = 1.
          IF screen-name CS 'S_DATE-LOW'.
            screen-required = 2.
          ENDIF.
          IF screen-name CS 'S_OBJID'.
            screen-required = 2.
          ENDIF.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.

*** Hide selection screen when not CPI Integration
    WHEN OTHERS.
      LOOP AT SCREEN.
        IF screen-name CS 'S_DATE' OR screen-name CS 'S_OBJID' OR screen-name CS 'B01'.
          screen-active = 0.
          screen-required = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
  ENDCASE.

*&---------------------------------------------------------------------*
*& Start of Selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.
*** Check the Integration selected
  CASE p_intsys.
    WHEN 1.   "IDoc
*** Call IDoc Monitor
      CALL TRANSACTION 'WE02'.

    WHEN 2.   "Proxy
*** Call Proxy Monitor
      CALL TRANSACTION 'SXMB_MONI'.

    WHEN 3.   "CPI
*** Validate the selection fields
      PERFORM f_validate_selfields.

*** Get the RFC Destination for the Integration System
      PERFORM f_get_rfc_dest.

*** Call the API to get the Message Log
      PERFORM f_call_api.

*** Display the Message Log ALV
      PERFORM f_disply_msg_log.
    WHEN OTHERS.
  ENDCASE.

*&---------------------------------------------------------------------*
*& End of Selection
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*** Clear the global data
  PERFORM f_clear_global.
