*&---------------------------------------------------------------------*
*& Include          ZCOR0100ALV
*&---------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.
DATA G_EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.

CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    METHODS: ON_USER_COMMAND FOR EVENT ADDED_FUNCTION OF CL_SALV_EVENTS
      IMPORTING E_SALV_FUNCTION.

    METHODS: TOP_OF_PAGE FOR EVENT TOP_OF_PAGE OF CL_SALV_EVENTS_TABLE
      IMPORTING R_TOP_OF_PAGE PAGE TABLE_INDEX.

    METHODS: ON_LINK_CLICK FOR EVENT LINK_CLICK OF CL_SALV_EVENTS_TABLE
      IMPORTING ROW COLUMN.

  PRIVATE SECTION.

    DATA: ERROR_IN_DATA TYPE C.


ENDCLASS. "lcl_event_receiver DEFINITION
*&---------------------------------------------------------------------*
* LOCAL CLASS Implementation
*&---------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD ON_USER_COMMAND.
    PERFORM HANDLE_USER_COMMAND USING E_SALV_FUNCTION.
  ENDMETHOD.                    "on_user_command

  METHOD TOP_OF_PAGE.
  ENDMETHOD.                    "on_top_of_page

  METHOD ON_LINK_CLICK.
    PERFORM HANDLE_LINK_CLICK USING ROW COLUMN.
  ENDMETHOD.                    "on_link_click
ENDCLASS. "LCL_EVENT_RECEIVER IMPLEMENTATION
