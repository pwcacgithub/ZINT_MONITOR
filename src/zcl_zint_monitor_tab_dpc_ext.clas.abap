class ZCL_ZINT_MONITOR_TAB_DPC_EXT definition
  public
  inheriting from ZCL_ZINT_MONITOR_TAB_DPC
  create public .

public section.
protected section.

  methods INTERFACELISTSET_CREATE_ENTITY
    redefinition .
  methods INTERFACELISTSET_UPDATE_ENTITY
    redefinition .
  methods INTERFACELISTSET_DELETE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZINT_MONITOR_TAB_DPC_EXT IMPLEMENTATION.


  METHOD interfacelistset_create_entity.
*&---------------------------------------------------------------------*
*& Program ID: ZCL_ZINT_MONITOR_TAB_DPC_EXT-INTERFACELISTSET_CREATE_ENTITY
*& Program Title: Redefinition Method for InterfaceListSet Create Entity
*& Description: Redefine the Create Entity method to create the
*&              table ZTINT_MONITOR through OData
*&---------------------------------------------------------------------*
*& Modification History
*&---------------------------------------------------------------------*
*& Date        User ID        REQ#        Transport# / Description
*&---------------------------------------------------------------------*
*& 30/04/2020  SSUDHENDRA001  V1          E2TK903247 - Initial version
*&---------------------------------------------------------------------*

*** Check the entity set name
    IF iv_entity_set_name = 'InterfaceListSet'.
      DATA: ls_data        TYPE zcl_zint_monitor_tab_mpc=>ts_interfacelist,
            ls_int_monitor TYPE ztint_monitor.

*** Get the data from UI
      io_data_provider->read_entry_data( IMPORTING es_data = ls_data ).

      MOVE-CORRESPONDING ls_data TO ls_int_monitor.

*** Fill Created By and Created On
      ls_int_monitor-ersda = sy-datum.
      ls_int_monitor-ernam = sy-uname.

*** Fill Changed By and Changed On
      ls_int_monitor-aedat = sy-datum.
      ls_int_monitor-aenam = sy-uname.

*** Use Enqueue function to lock the table
      CALL FUNCTION 'ENQUEUE_E_TABLE'
        EXPORTING
          mode_rstable   = 'E'
          tabname        = 'ZTINT_MONITOR'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc EQ 0.
*** Insert the data to the ZTINT_MONITOR Table
        INSERT ztint_monitor FROM ls_int_monitor.
        IF sy-subrc EQ 0.
          CLEAR ls_int_monitor.

*** Use Dequeue function to lock the table
          CALL FUNCTION 'DEQUEUE_E_TABLE'
            EXPORTING
              mode_rstable = 'E'
              tabname      = 'ZTINT_MONITOR'.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD interfacelistset_delete_entity.
*&---------------------------------------------------------------------*
*& Program ID: ZCL_ZINT_MONITOR_TAB_DPC_EXT-INTERFACELISTSET_DELETE_ENTITY
*& Program Title: Redefinition Method for InterfaceListSet Delete Entity
*& Description: Redefine the Delete Entity method to delete the entry
*&              from table ZTINT_MONITOR through OData
*&---------------------------------------------------------------------*
*& Modification History
*&---------------------------------------------------------------------*
*& Date        User ID        REQ#        Transport# / Description
*&---------------------------------------------------------------------*
*& 30/04/2020  SSUDHENDRA001  V1          E2TK903247 - Initial version
*&---------------------------------------------------------------------*

*** Check the entity set name
    IF iv_entity_set_name = 'InterfaceListSet'.
      DATA: ls_int_monitor TYPE ztint_monitor.

*** Get the Key fields
      LOOP AT it_key_tab INTO DATA(lw_key_tab).
        CASE lw_key_tab-name.
          WHEN 'ObjectId'.
            ls_int_monitor-object_id = lw_key_tab-value.
          WHEN 'ConnectionSystem'.
            ls_int_monitor-conn_system = lw_key_tab-value.
          WHEN 'InterfaceName'.
            ls_int_monitor-interface_name = lw_key_tab-value.
          WHEN 'InterfaceType'.
            ls_int_monitor-interface_type = lw_key_tab-value.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.

*** Delete the data from ZTINT_MONITOR Table
      DELETE ztint_monitor FROM ls_int_monitor.
      CLEAR ls_int_monitor.
    ENDIF.

  ENDMETHOD.


  METHOD interfacelistset_update_entity.
*&---------------------------------------------------------------------*
*& Program ID: ZCL_ZINT_MONITOR_TAB_DPC_EXT-INTERFACELISTSET_UPDATE_ENTITY
*& Program Title: Redefinition Method for InterfaceListSet Update Entity
*& Description: Redefine the Update Entity method to update the
*&              table ZTINT_MONITOR through OData
*&---------------------------------------------------------------------*
*& Modification History
*&---------------------------------------------------------------------*
*& Date        User ID        REQ#        Transport# / Description
*&---------------------------------------------------------------------*
*& 30/04/2020  SSUDHENDRA001  V1          E2TK903247 - Initial version
*&---------------------------------------------------------------------*

*** Check the entity set name
    IF iv_entity_set_name = 'InterfaceListSet'.
      DATA: ls_data        TYPE zcl_zint_monitor_tab_mpc=>ts_interfacelist,
            ls_int_monitor TYPE ztint_monitor.

*** Get the data from UI
      io_data_provider->read_entry_data( IMPORTING es_data = ls_data ).

      MOVE-CORRESPONDING ls_data TO ls_int_monitor.

*** Fill Changed By and Changed On
      ls_int_monitor-aedat = sy-datum.
      ls_int_monitor-aenam = sy-uname.

*** Use Enqueue function to lock the table
      CALL FUNCTION 'ENQUEUE_E_TABLE'
        EXPORTING
          mode_rstable   = 'E'
          tabname        = 'ZTINT_MONITOR'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc EQ 0.
*** Update the data to the ZTINT_MONITOR Table
        UPDATE ztint_monitor SET  interface_id = ls_int_monitor-interface_id
                                  rfc_dest = ls_int_monitor-rfc_dest
                                  application_id = ls_int_monitor-application_id
                                  sender_id = ls_int_monitor-sender_id
                                  aedat = ls_int_monitor-aedat
                                  aenam = ls_int_monitor-aenam
                                  WHERE object_id = ls_int_monitor-object_id
                                    AND conn_system = ls_int_monitor-conn_system
                                    AND interface_name = ls_int_monitor-interface_name
                                    AND interface_type = ls_int_monitor-interface_type.
        IF sy-subrc EQ 0.
          CLEAR ls_int_monitor.

*** Use Dequeue function to lock the table
          CALL FUNCTION 'DEQUEUE_E_TABLE'
            EXPORTING
              mode_rstable = 'E'
              tabname      = 'ZTINT_MONITOR'.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
