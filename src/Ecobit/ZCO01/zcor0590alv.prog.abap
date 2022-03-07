*&---------------------------------------------------------------------*
*& Include          ZCOR0590ALV
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Class (Definition) LCL_EVENT_RECEIVER
*&---------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.

    METHODS:
      ON_HOTSPOT_CLICK  FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
                        IMPORTING E_ROW_ID
                                  E_COLUMN_ID
                                  ES_ROW_NO
                                  SENDER.

ENDCLASS.
*&---------------------------------------------------------------------*
*& Class (Implementation) LCL_EVENT_RECEIVER
*&---------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
  METHOD ON_HOTSPOT_CLICK.
    PERFORM HANDLE_HOTSPOT_CLICK USING E_ROW_ID
                                       E_COLUMN_ID
                                       ES_ROW_NO
                                       SENDER.

  ENDMETHOD.
ENDCLASS.
