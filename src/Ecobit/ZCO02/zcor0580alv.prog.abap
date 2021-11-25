*&---------------------------------------------------------------------*
*& Include          ZCOR0580ALV
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Class (Definition) LCL_EVENT_RECEIVER
*&---------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.

    METHODS:

      ON_DATA_CHANGED   FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
                        IMPORTING ER_DATA_CHANGED
                                  E_ONF4
                                  E_ONF4_BEFORE
                                  E_ONF4_AFTER
                                  E_UCOMM
                                  SENDER,
      ON_FINISHED       FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
                        IMPORTING E_MODIFIED
                                  ET_GOOD_CELLS
                                  SENDER,
      ON_TOOLBAR        FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
                        IMPORTING E_OBJECT
                                  SENDER,
      ON_USER_COMMAND   FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
                        IMPORTING E_UCOMM
                                  SENDER,

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
  METHOD ON_DATA_CHANGED.
    PERFORM HANDLE_DATA_CHANGED USING ER_DATA_CHANGED
                                      E_ONF4
                                      E_ONF4_BEFORE
                                      E_ONF4_AFTER
                                      E_UCOMM
                                      SENDER.
  ENDMETHOD.
  METHOD ON_FINISHED.
    PERFORM HANDLE_FINISHED USING E_MODIFIED
                                  ET_GOOD_CELLS
                                  SENDER.
  ENDMETHOD.
  METHOD ON_TOOLBAR.
    PERFORM HANDLE_TOOLBAR USING E_OBJECT
                                 SENDER.
  ENDMETHOD.
  METHOD ON_USER_COMMAND.
    PERFORM HANDLE_USER_COMMAND USING E_UCOMM
                                      SENDER.
  ENDMETHOD.

  METHOD ON_HOTSPOT_CLICK.
    PERFORM HANDLE_HOTSPOT_CLICK USING E_ROW_ID
                                       E_COLUMN_ID
                                       ES_ROW_NO
                                       SENDER.

  ENDMETHOD.
ENDCLASS.
