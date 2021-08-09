*&---------------------------------------------------------------------*
*& Include          ZCOR101_CLS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&       Class LCL_EVENT_RECEIVER
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS : HANDLE_DATA_CHANGED FOR EVENT DATA_CHANGED
                OF CL_GUI_ALV_GRID
      IMPORTING ER_DATA_CHANGED.
ENDCLASS.               "LCL_EVENT_RECEIVER
*&---------------------------------------------------------------------*
*&       Class (Implementation)  LCL_EVENT_RECEIVER
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD : HANDLE_DATA_CHANGED.
    PERFORM DATA_CHANGED USING ER_DATA_CHANGED.
* 강제 pbo, pai
    CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
      EXPORTING
        NEW_CODE = 'ENTER'.
  ENDMETHOD.                    ":

ENDCLASS.               "LCL_EVENT_RECEIVER
