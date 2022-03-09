*&---------------------------------------------------------------------*
*& Include          ZIINT_MONITOR_SUB
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form F_VALIDATE_SELFIELDS
*&---------------------------------------------------------------------*
*& Validate the selection fields
*&---------------------------------------------------------------------*
FORM f_validate_selfields.

*** Validate Date
  IF s_date[] IS INITIAL.
    MESSAGE 'Please enter valid date' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

*** Validate Object ID
  IF s_objid[] IS INITIAL.
    MESSAGE 'Please enter valid object' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GET_RFC_DEST
*&---------------------------------------------------------------------*
*& Get the RFC Destination for the Integration System
*&---------------------------------------------------------------------*
FORM f_get_rfc_dest.

  CASE p_intsys.
    WHEN '3'.
      SELECT SINGLE low
        FROM tvarvc
        INTO gv_rfc_dest    "CPI_MSGLOG
        WHERE name = gc_rfc_cpi_msglog. "'P_RFC_CPI_MSGLOG'.
      IF sy-subrc NE 0.
        MESSAGE 'Maintain the RFC Destination in TVARVC with Variable Name P_RFC_CPI_MSGLOG' TYPE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_CALL_API
*&---------------------------------------------------------------------*
*& Call the API to get the Message Log
*&---------------------------------------------------------------------*
FORM f_call_api.

*** Type Declaration
  TYPES:
    BEGIN OF integrationartifact,
      id   TYPE string,
      name TYPE string,
      type TYPE string,
    END OF integrationartifact .

  TYPES:
    BEGIN OF ts_messageprocessinglog,
      messageguid            TYPE string,
      correlationid          TYPE string,
      applicationmessageid   TYPE string,
      applicationmessagetype TYPE string,
      logstart               TYPE string, "timestamp,
      logend                 TYPE string, "timestamp,
      sender                 TYPE string,
      receiver               TYPE string,
      integrationflowname    TYPE string,
      status                 TYPE string,
      alternateweblink       TYPE string,
      integrationartifact    TYPE integrationartifact,
      loglevel               TYPE string,
      customstatus           TYPE string,
    END OF ts_messageprocessinglog .

  TYPES:
    tt_messageprocessinglog TYPE STANDARD TABLE OF ts_messageprocessinglog WITH NON-UNIQUE DEFAULT KEY.

  TYPES:
    BEGIN OF ty_results,
      results TYPE tt_messageprocessinglog,
    END OF ty_results,

    BEGIN OF ty_data,
      d TYPE ty_results,
    END OF ty_data.

*** Data Declaration
  DATA: lo_http_client_msglog   TYPE REF TO if_http_client,
        lo_http_client_errorlog TYPE REF TO if_http_client,
        lo_abap_conv            TYPE REF TO cl_abap_conv_in_ce,
        lv_json_str             TYPE string,
        lv_json_xtr             TYPE xstring,
        ls_response             TYPE ty_data,
        ls_message_log          TYPE ty_message_log,
        lv_filter_uri           TYPE string,
        lv_error_msg_uri        TYPE string.

*** Get the Interface ID for the entered Object ID from table ZTINT_MONITOR
  SELECT object_id, conn_system, interface_name, interface_type, interface_id
    INTO TABLE @DATA(lt_ztint_monitor)
    FROM ztint_monitor
    WHERE object_id IN @s_objid.
  IF sy-subrc EQ 0.
    SORT lt_ztint_monitor BY object_id.
  ENDIF.

*** Loop the API Call for all the Interface ID
  LOOP AT lt_ztint_monitor INTO DATA(lw_ztint_monitor).

*** Create the Request from RFC Destination - "URL - https://p250002-tmn.hci.us3.hana.ondemand.com/api/v1/MessageProcessingLogs
    cl_http_client=>create_by_destination( EXPORTING destination = gv_rfc_dest IMPORTING client = lo_http_client_msglog ).

*** Set the Request Method to GET
    lo_http_client_msglog->request->set_method( if_http_request=>co_request_method_get ).

*** Create filter for Log Start Date
    DATA(lv_start_date_time) = s_date-low+0(4) && |-| && s_date-low+4(2) && |-| && s_date-low+6(2).
    IF s_date-high IS INITIAL.
      DATA(lv_end_date_time) = s_date-low+0(4) && |-| && s_date-low+4(2) && |-| && s_date-low+6(2).
    ELSE.
      lv_end_date_time = s_date-high+0(4) && |-| && s_date-high+4(2) && |-| && s_date-high+6(2).
    ENDIF.

*** Create filter for Integraon Flow Name
    lv_filter_uri = |?$filter=| &&
                    |IntegrationFlowName eq '| && lw_ztint_monitor-interface_id && |'| &&
                    | and | &&
                    |LogStart gt datetime'| && lv_start_date_time && |T00:00:00.000| && |'| &&
                    | and | &&
                    |LogStart lt datetime'| && lv_end_date_time && |T23:59:59.999| && |'|.

*** Add path prefix to filter based on Run Date and Interface ID
    lo_http_client_msglog->request->set_header_field( EXPORTING  name  = '~REQUEST_URI' value = lv_filter_uri ).

*** Set the Header fields
    lo_http_client_msglog->request->set_header_field( EXPORTING  name  = 'ACCEPT' value = 'application/json' ).

*** Send the request
    CALL METHOD lo_http_client_msglog->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.
    IF sy-subrc = 0.
*** Recieve the response
      CALL METHOD lo_http_client_msglog->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4.
      IF sy-subrc = 0.
        lv_json_xtr = lo_http_client_msglog->response->get_data( ).
      ENDIF.
    ENDIF.

*** Convert the Response from XString to String
    CALL METHOD cl_abap_conv_in_ce=>create
      EXPORTING
        input       = lv_json_xtr
        encoding    = 'UTF-8'
        replacement = '?'
        ignore_cerr = abap_true
      RECEIVING
        conv        = lo_abap_conv.

    TRY.
        CALL METHOD lo_abap_conv->read
          IMPORTING
            data = lv_json_str.
        " Should ignore errors in code conversions
      CATCH cx_sy_conversion_codepage.
      CATCH cx_sy_codepage_converter_init.
      CATCH cx_parameter_invalid_type.
      CATCH cx_parameter_invalid_range.
    ENDTRY.

*** Convert the JSON to ABAP Structure
    CALL METHOD cl_fdt_json=>json_to_data
      EXPORTING
        iv_json = lv_json_str
      CHANGING
        ca_data = ls_response.

    LOOP AT ls_response-d-results ASSIGNING FIELD-SYMBOL(<lfs_results>).
      ls_message_log = CORRESPONDING #( <lfs_results> ).

      IF <lfs_results>-logstart IS NOT INITIAL.
*** Convert Date format from JSON to ABAP
        PERFORM f_conv_json_abap_date
          USING <lfs_results>-logstart
          CHANGING ls_message_log-start_date ls_message_log-start_time.
      ENDIF.

*** Get the Error Message for the failed connection
      IF ls_message_log-status = 'FAILED'.
        lv_error_msg_uri = |('| && ls_message_log-messageguid && |')| && |/ErrorInformation/$value|.

*** Create the Request from RFC Destination - "URL - https://p250002-tmn.hci.us3.hana.ondemand.com/api/v1/MessageProcessingLogs
        cl_http_client=>create_by_destination( EXPORTING destination = gv_rfc_dest IMPORTING client = lo_http_client_errorlog ).

*** Set the Request Method to GET
        lo_http_client_errorlog->request->set_method( if_http_request=>co_request_method_get ).

*** Set the Header fields
        lo_http_client_errorlog->request->set_header_field( EXPORTING  name  = 'ACCEPT' value = 'application/json' ).

*** Set the Header field to get the Error message
        lo_http_client_errorlog->request->set_header_field( EXPORTING  name  = '~REQUEST_URI' value = lv_error_msg_uri ).

*** Send the request
        CALL METHOD lo_http_client_errorlog->send
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            http_invalid_timeout       = 4
            OTHERS                     = 5.
        IF sy-subrc = 0.
*** Recieve the response
          CALL METHOD lo_http_client_errorlog->receive
            EXCEPTIONS
              http_communication_failure = 1
              http_invalid_state         = 2
              http_processing_failed     = 3
              OTHERS                     = 4.
          IF sy-subrc = 0.
            ls_message_log-error_log = lo_http_client_errorlog->response->get_cdata( ).

* Declarations
            DATA lo_conv           TYPE REF TO cl_abap_conv_in_ce.
            DATA lo_rest_client    TYPE REF TO cl_rest_http_client.
            DATA lo_response       TYPE REF TO if_rest_entity.
            DATA response          TYPE string.

* Instantiate REST client
            CREATE OBJECT lo_rest_client
              EXPORTING
                io_http_client = lo_http_client_errorlog.

* Get Response data
            lo_response = lo_rest_client->if_rest_client~get_response_entity( ).

* Get string data
            response = lo_response->get_string_data( ).
          ENDIF.
        ENDIF.
        CLEAR lo_http_client_errorlog.
      ENDIF.

*** Set the Traffic Lights Icon based on the status
      CASE ls_message_log-status.
        WHEN 'FAILED'.
          ls_message_log-icon = 1.
        WHEN 'RETRY'.
          ls_message_log-icon = 2.
        WHEN 'COMPLETED'.
          ls_message_log-icon = 3.
      ENDCASE.

*** Update the final log table
      APPEND ls_message_log TO gt_message_log.
      CLEAR ls_message_log.

    ENDLOOP.
    CLEAR ls_response.
    CLEAR lo_http_client_msglog.
  ENDLOOP.

  REFRESH lt_ztint_monitor.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_CONV_JSON_ABAP_DATE
*&---------------------------------------------------------------------*
*& Convert Date format from JSON to ABAP
*&---------------------------------------------------------------------*
FORM f_conv_json_abap_date USING      ip_json_timestamp TYPE string
                           CHANGING   cp_abap_date      TYPE dats
                                      cp_abap_time      TYPE tims .

  DATA: lv_ticks        TYPE string,
        lv_offset       TYPE string,
        lv_start_date   TYPE dats VALUE '19700101',
        lv_start_time   TYPE dats VALUE '000000',
        lv_milli_sec    TYPE i VALUE 86400000,
        lv_days_num(16) TYPE p DECIMALS 5,
        lv_seconds(16)  TYPE p DECIMALS 5.

  FIND REGEX '^\\?/Date\((-?[[:digit:]]+)([+-][[:digit:]]{1,4})?\)\\?/$'
  IN ip_json_timestamp SUBMATCHES lv_ticks lv_offset.

  lv_days_num = lv_ticks / lv_milli_sec.
  cp_abap_date = lv_start_date + trunc( lv_days_num ).
  lv_seconds = frac( lv_days_num ) * 24 * 60 * 60.
  cp_abap_time = lv_start_time + lv_seconds.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_DISPLY_MSG_LOG
*&---------------------------------------------------------------------*
*& Display the Message Log ALV
*&---------------------------------------------------------------------*
FORM f_disply_msg_log.

*** Build FieldCatlog
  PERFORM f_build_fieldcat.

*** Display ALV
  PERFORM f_display_alv.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*& Build FieldCatlog
*&---------------------------------------------------------------------*
FORM f_build_fieldcat.

*** Data Declaration
  DATA: ls_fieldcat TYPE slis_fieldcat_alv.

*** Build the field catalog
  ls_fieldcat-fieldname = 'INTEGRATIONFLOWNAME'.
  ls_fieldcat-tabname = 'GT_MESSAGE_LOG'.
  ls_fieldcat-outputlen = '30'.
  ls_fieldcat-seltext_l = 'Integration Flow Name'.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'APPLICATIONMESSAGEID'.
  ls_fieldcat-tabname = 'GT_MESSAGE_LOG'.
  ls_fieldcat-outputlen = '20'.
  ls_fieldcat-seltext_l = 'Application Message ID'.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'START_DATE'.
  ls_fieldcat-tabname = 'GT_MESSAGE_LOG'.
  ls_fieldcat-outputlen = '10'.
  ls_fieldcat-seltext_l = 'Start Date'.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'START_TIME'.
  ls_fieldcat-tabname = 'GT_MESSAGE_LOG'.
  ls_fieldcat-outputlen = '10'.
  ls_fieldcat-seltext_l = 'Start Time'.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'SENDER'.
  ls_fieldcat-tabname = 'GT_MESSAGE_LOG'.
  ls_fieldcat-outputlen = '20'.
  ls_fieldcat-seltext_l = 'Sender'.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'RECEIVER'.
  ls_fieldcat-tabname = 'GT_MESSAGE_LOG'.
  ls_fieldcat-outputlen = '20'.
  ls_fieldcat-seltext_l = 'Receiver'.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'MESSAGEGUID'.
  ls_fieldcat-tabname = 'GT_MESSAGE_LOG'.
  ls_fieldcat-outputlen = '40'.
  ls_fieldcat-seltext_l = 'Message GUID'.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'STATUS'.
  ls_fieldcat-tabname = 'GT_MESSAGE_LOG'.
  ls_fieldcat-outputlen = '20'.
  ls_fieldcat-seltext_l = 'Status'.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'ERROR_LOG'.
  ls_fieldcat-tabname = 'GT_MESSAGE_LOG'.
  ls_fieldcat-outputlen = '100'.
  ls_fieldcat-seltext_l = 'Error Log'.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR ls_fieldcat.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_DISPLAY_ALV
*&---------------------------------------------------------------------*
*& Display ALV
*&---------------------------------------------------------------------*
FORM f_display_alv.

  DATA: ls_layout TYPE slis_layout_alv.

*** Specify field for Traffic Lights Icon
  ls_layout-lights_fieldname = 'ICON'.

*** Create ALV Output
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = ls_layout
      it_fieldcat        = gt_fieldcat
    TABLES
      t_outtab           = gt_message_log
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_CLEAR_GLOBAL
*&---------------------------------------------------------------------*
*& Clear the global data
*&---------------------------------------------------------------------*
FORM f_clear_global.

  REFRESH: gt_message_log, gt_fieldcat.

  CLEAR: gv_rfc_dest.

ENDFORM.
