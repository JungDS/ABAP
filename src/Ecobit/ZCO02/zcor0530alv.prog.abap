*&---------------------------------------------------------------------*
*& Include          ZCOR0530ALV
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Class (Definition) LCL_EVENT_RECEIVER
*&---------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS:
      ON_TOOLBAR        FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
                        IMPORTING E_OBJECT
                                  SENDER,
      ON_USER_COMMAND   FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
                        IMPORTING E_UCOMM
                                  SENDER,
      ON_ADDED_FUNCTION FOR EVENT ADDED_FUNCTION OF IF_SALV_EVENTS_FUNCTIONS
                        IMPORTING E_SALV_FUNCTION
                                  SENDER.
ENDCLASS.
*&---------------------------------------------------------------------*
*& Class (Implementation) LCL_EVENT_RECEIVER
*&---------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
  METHOD ON_TOOLBAR.
    PERFORM HANDLE_TOOLBAR USING E_OBJECT
                                 SENDER.
  ENDMETHOD.
  METHOD ON_USER_COMMAND.
    PERFORM HANDLE_USER_COMMAND USING E_UCOMM
                                      SENDER.
  ENDMETHOD.
  METHOD ON_ADDED_FUNCTION.
    PERFORM HANDLE_ADDED_FUNCTION USING E_SALV_FUNCTION
                                        SENDER.
  ENDMETHOD.
ENDCLASS.
