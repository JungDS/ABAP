*&---------------------------------------------------------------------*
*& Include          ZCOR0420NALV
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           Z30MMR5540ALV
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&       Class LCL_ALV_GRID
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS LCL_ALV_GRID DEFINITION INHERITING FROM CL_GUI_ALV_GRID.
  PUBLIC SECTION.
    CLASS-DATA F_ALV  TYPE C.

    METHODS: SET_OPTIMIZE_ALL_COLS,
      SET_CURSOR IMPORTING ROW TYPE I
                           COL TYPE I,
      SET_FIXED_COLUMN,
      SET_ROW_RESIZE.
ENDCLASS.                    "LCL_ALV_GRID DEFINITION\
*&---------------------------------------------------------------------*
*&       Class (Implementation)  LCL_ALV_GRID
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS LCL_ALV_GRID IMPLEMENTATION.
  METHOD SET_OPTIMIZE_ALL_COLS.
    CALL METHOD ME->OPTIMIZE_ALL_COLS
      EXPORTING
        INCLUDE_HEADER = 1.
  ENDMETHOD.                    " SET_OPTIMIZE_ALL_COLS
*
  METHOD SET_CURSOR.
    CALL METHOD ME->SET_CURRENT_CELL_BASE
      EXPORTING
        ROW = ROW
        COL = COL.
  ENDMETHOD.                    " SET_CURSOR
*
  METHOD SET_FIXED_COLUMN.
    CALL METHOD ME->SET_FIXED_COLS
      EXPORTING
        COLS = 3.
  ENDMETHOD.                    " SET_FIXED_COLUMN
*
  METHOD SET_ROW_RESIZE.
    CALL METHOD ME->SET_RESIZE_ROWS
      EXPORTING
        ENABLE = 1.
  ENDMETHOD.                    " SET_ROW_RESIZE
ENDCLASS.               "LCL_ALV_GRID
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    TYPES : DDSHRETVAL_TABLE TYPE TABLE OF DDSHRETVAL.
    METHODS:
      HANDLE_TOOLBAR  FOR EVENT TOOLBAR  OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT E_INTERACTIVE,

      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM,

      HANDLE_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED E_ONF4,

      HANDLE_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
         IMPORTING E_ROW E_COLUMN.

ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
  METHOD HANDLE_TOOLBAR.
    PERFORM GRID_TOOLBAR_INCLUDEING USING E_OBJECT
                                          E_INTERACTIVE.
  ENDMETHOD.                    "handle_toolbar

  METHOD HANDLE_USER_COMMAND.
    CASE E_UCOMM.
      WHEN 'ADDL'.
        PERFORM ALL_CHECK.
        PERFORM REFRESH_DATA.
      WHEN 'DELL'.
        PERFORM ALL_CHECK_DELETE.
        PERFORM REFRESH_DATA.
    ENDCASE.
    PERFORM GO_PBO.
  ENDMETHOD.                    "LCL_EVENT_RECEIVER

  METHOD HANDLE_DATA_CHANGED.
    PERFORM DATA_CHANGED  USING ER_DATA_CHANGED.
  ENDMETHOD.                    "handle_data_changed

  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM DOUBLE_CLICK USING E_ROW E_COLUMN.
  ENDMETHOD.                    "handle_double_click
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION
