*&---------------------------------------------------------------------*
*& Include          ZCOR0330ALV
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Local Class Definition (Program 내부)                               *
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. LCL_  : Local Class Definition
* - EX). CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.
*----------------------------------------------------------------------*
*  Reference Varialbles for Class/Interfaces                           *
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. GR_  : Global Reference Varialbles
*   2. LR_  : Local Reference Varialbles
* - EX). GR_GRID TYPE REF TO CL_GUI_ALV_GRID.

* EX).
*DATA: GR_CON1            TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
*      GR_SPLITTER1       TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
*      GR_HEAD_CONTAINER  TYPE REF TO CL_GUI_CONTAINER,
*      GR_DATA_CONTAINER  TYPE REF TO CL_GUI_CONTAINER,
*      GR_DOCK1           TYPE REF TO CL_GUI_DOCKING_CONTAINER,
*      GR_GRID_HEAD       TYPE REF TO CL_GUI_ALV_GRID,
*      GR_GRID_DATA       TYPE REF TO CL_GUI_ALV_GRID,
*      GR_GRID1           TYPE REF TO CL_GUI_ALV_GRID,
*      GR_EVENT_RECEIVER  TYPE REF TO LCL_EVENT_RECEIVER.

DATA: GR_CON1            TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GR_SPLITTER1       TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GR_HEAD_CONTAINER  TYPE REF TO CL_GUI_CONTAINER,
      GR_DATA_CONTAINER  TYPE REF TO CL_GUI_CONTAINER,
      GR_DOCK1           TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GR_GRID_HEAD       TYPE REF TO CL_GUI_ALV_GRID,
      GR_GRID_DATA       TYPE REF TO CL_GUI_ALV_GRID,
      GR_GRID1           TYPE REF TO CL_GUI_ALV_GRID,
      GR_EVENT_RECEIVER  TYPE REF TO LCL_EVENT_RECEIVER,

      "Dynamic Documents: Document
      GR_TOP_DOCUMENT    TYPE REF TO CL_DD_DOCUMENT,
      "Reference to html container
      GR_HTML_CNTRL      TYPE REF TO CL_GUI_HTML_VIEWER,
      "Reference to html container
      GR_PARENT_HTML     TYPE REF TO CL_GUI_CONTAINER.

*----------------------------------------------------------------------*
*  Global Internal Table                                               *
*----------------------------------------------------------------------*
*DATA: GT_FIELDCAT TYPE LVC_T_FCAT,
*      GT_SORTCAT  TYPE LVC_T_SORT,
*      GT_EXCLUDE  TYPE UI_FUNCTIONS,
*      GT_F4       TYPE LVC_T_F4,
*      GT_ROWS     TYPE LVC_T_ROW,
*      GT_STYLE    TYPE LVC_T_STYL,
*      GT_COLOR    TYPE LVC_T_SCOL,
*      GT_PARA     TYPE TABLE OF RFC_SPAGPA.

DATA: GT_FIELDCAT  TYPE LVC_T_FCAT,
      GT_FIELDCAT2 TYPE LVC_T_FCAT,
      GT_SORT     TYPE LVC_T_SORT,
      GT_EXCLUDE  TYPE UI_FUNCTIONS,
      GT_F4       TYPE LVC_T_F4,
      GT_ROWS     TYPE LVC_T_ROW,
      GS_ROW      TYPE LVC_S_ROW,
      GT_STYLE    TYPE LVC_T_STYL,
      GT_COLOR    TYPE LVC_T_SCOL,
      GT_PARA     TYPE TABLE OF RFC_SPAGPA.

*----------------------------------------------------------------------*
*  Global Structure                                                    *
*----------------------------------------------------------------------*
*DATA: GS_LAYOUT   TYPE LVC_S_LAYO,
*      GS_FIELDCAT TYPE LVC_S_FCAT,
*      GS_SORT     TYPE LVC_S_SORT,
*      GS_PRINT    TYPE LVC_S_PRNT,
*      GS_ROWS     TYPE LVC_S_ROW,
*      GS_F4       TYPE LVC_S_F4,
*      GS_STYLE    TYPE LVC_S_STYL,
*      GS_COLOR    TYPE LVC_S_SCOL,
*      GS_STABLE   TYPE LVC_S_STBL,
*      GS_VARIANT  TYPE DISVARIANT,
*      GS_COL      TYPE LVC_S_COL.

DATA: GS_LAYOUT   TYPE LVC_S_LAYO,
      GS_FIELDCAT TYPE LVC_S_FCAT,
      GS_SORT     TYPE LVC_S_SORT,
      GS_PRINT    TYPE LVC_S_PRNT,
      GS_ROWS     TYPE LVC_S_ROW,
      GS_F4       TYPE LVC_S_F4,
      GS_STYLE    TYPE LVC_S_STYL,
      GS_COLOR    TYPE LVC_S_SCOL,
      GS_STABLE   TYPE LVC_S_STBL,
      GS_VARIANT  TYPE DISVARIANT,
      GS_COL      TYPE LVC_S_COL.

*----------------------------------------------------------------------*
*  Global Variant                                                      *
*----------------------------------------------------------------------*
*DATA: GV_CONTAINER TYPE SCRFNAME VALUE 'CT_0100',
*      GV_SAVE      TYPE C        VALUE 'U',
*      GV_POS       TYPE I,
*      GV_REPID     TYPE SY-REPID.

DATA: GV_CONTAINER TYPE SCRFNAME VALUE 'CT_0100',
      GV_SAVE      TYPE C        VALUE 'U',
      GV_POS       TYPE I,
      GV_REPID     TYPE SY-REPID.

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*---------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    METHODS:
      HANDLE_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW
                  E_COLUMN
                  ES_ROW_NO
                  SENDER.

  PRIVATE SECTION.

ENDCLASS. "LCL_EVENT_RECEIVER DEFINITION
*---------------------------------------------------------------------*
* LOCAL CLASSES: Implementation                                       *
*---------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM EVENT_DOUBLE_CLICK USING E_ROW
                                     E_COLUMN
                                     ES_ROW_NO
                                     SENDER.

  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

ENDCLASS. "LCL_EVENT_RECEIVER IMPLEMENTATION
