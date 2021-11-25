*&---------------------------------------------------------------------*
*&  Include           ZFIICALV_CLASS                                   *
*&---------------------------------------------------------------------*
DATA : G_COUNT_C TYPE I ,
       G_DEL ,
       G_CHG ,
       G_DYNNR LIKE SY-DYNNR.

DATA : G_FALSE TYPE I VALUE '0' ,
       G_TRUE  TYPE I VALUE '1' .

CONSTANTS: G_LINE_LENGTH TYPE I VALUE 72.
*---------------------------------------------------------------------*
*       CLASS lcl_events DEFINITION
*---------------------------------------------------------------------*
CLASS LCL_EVENTS DEFINITION.
  PUBLIC SECTION.
    DATA: MR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
    METHODS:
    USER_COMMAND         FOR EVENT USER_COMMAND
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_UCOMM SENDER,
    BEFORE_USER_COMMAND  FOR EVENT BEFORE_USER_COMMAND
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_UCOMM,
    AFTER_USER_COMMAND   FOR EVENT AFTER_USER_COMMAND
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_UCOMM,
    DOUBLE_CLICK         FOR EVENT DOUBLE_CLICK
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_ROW
                                   E_COLUMN
                                   ES_ROW_NO,
    ITEM_DOUBLE_CLICK FOR EVENT ITEM_DOUBLE_CLICK
                         OF CL_GUI_ALV_TREE
                         IMPORTING NODE_KEY
                                  FIELDNAME,
    NODE_DOUBLE_CLICK FOR EVENT NODE_DOUBLE_CLICK
                         OF CL_GUI_ALV_TREE
                         IMPORTING NODE_KEY,
    ITEM_KEYPRESS FOR EVENT ITEM_KEYPRESS
                         OF CL_GUI_ALV_TREE
                         IMPORTING NODE_KEY
                                   FIELDNAME KEY,
    NODE_KEYPRESS FOR EVENT NODE_KEYPRESS
                         OF CL_GUI_ALV_TREE
                         IMPORTING NODE_KEY
                                   KEY,
    CHECKBOX_CHANGE FOR EVENT CHECKBOX_CHANGE
                         OF CL_GUI_ALV_TREE
                         IMPORTING NODE_KEY
                                   FIELDNAME
                                   CHECKED,
    LINK_CLICK      FOR EVENT LINK_CLICK
                         OF CL_GUI_ALV_TREE
                         IMPORTING FIELDNAME
                                   NODE_KEY,
    BUTTON_CLICK_TREE FOR EVENT BUTTON_CLICK
                         OF CL_GUI_ALV_TREE
                         IMPORTING FIELDNAME
                                   NODE_KEY,
    HEADER_CLICK      FOR EVENT HEADER_CLICK
                         OF CL_GUI_ALV_TREE
                         IMPORTING FIELDNAME ,
    HOTSPOT_CLICK        FOR EVENT HOTSPOT_CLICK
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_ROW_ID
                                   E_COLUMN_ID
                                   ES_ROW_NO,
    MENU_BUTTON          FOR EVENT MENU_BUTTON
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_OBJECT
                                   E_UCOMM,
    TOOLBAR              FOR EVENT TOOLBAR
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_OBJECT
                                   E_INTERACTIVE,
    CONTEXT_MENU_REQUEST FOR EVENT CONTEXT_MENU_REQUEST
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_OBJECT,
    TOP_OF_PAGE          FOR EVENT TOP_OF_PAGE
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_DYNDOC_ID,
    END_OF_LIST          FOR EVENT END_OF_LIST
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_DYNDOC_ID,
    PRINT_TOP_OF_PAGE    FOR EVENT PRINT_TOP_OF_PAGE
                         OF CL_GUI_ALV_GRID,
    PRINT_END_OF_PAGE    FOR EVENT PRINT_END_OF_PAGE
                         OF CL_GUI_ALV_GRID,
    PRINT_TOP_OF_LIST    FOR EVENT PRINT_TOP_OF_LIST
                         OF CL_GUI_ALV_GRID,
    PRINT_END_OF_LIST    FOR EVENT PRINT_END_OF_LIST
                         OF CL_GUI_ALV_GRID,
    AFTER_REFRESH        FOR EVENT AFTER_REFRESH
                         OF CL_GUI_ALV_GRID,
    DELAYED_CALLBACK     FOR EVENT DELAYED_CALLBACK
                         OF CL_GUI_ALV_GRID,
    DELAYED_CHANGED_SEL_CALLBACK
                         FOR EVENT DELAYED_CHANGED_SEL_CALLBACK
                         OF CL_GUI_ALV_GRID,
    SUBTOTAL_TEXT        FOR EVENT SUBTOTAL_TEXT
                         OF CL_GUI_ALV_GRID
                         IMPORTING ES_SUBTOTTXT_INFO
                                   EP_SUBTOT_LINE
                                   E_EVENT_DATA,
    ONDRAG               FOR EVENT ONDRAG
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_ROW
                                   E_COLUMN
                                   ES_ROW_NO
                                   E_DRAGDROPOBJ,
    ONDROP               FOR EVENT ONDROP
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_ROW
                                   E_COLUMN
                                   ES_ROW_NO
                                   E_DRAGDROPOBJ,
    ONDROPCOMPLETE       FOR EVENT ONDROPCOMPLETE
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_ROW
                                   E_COLUMN
                                   ES_ROW_NO
                                   E_DRAGDROPOBJ,
    ONDROPGETFLAVOR      FOR EVENT ONDROPGETFLAVOR
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_ROW
                                   E_COLUMN
                                   ES_ROW_NO
                                   E_DRAGDROPOBJ
                                   E_FLAVORS,
    DATA_CHANGED         FOR EVENT DATA_CHANGED
                         OF CL_GUI_ALV_GRID
                         IMPORTING ER_DATA_CHANGED
                                   E_ONF4
                                   E_ONF4_BEFORE
                                   E_ONF4_AFTER
                                   E_UCOMM,
    DATA_CHANGED_FINISHED
                         FOR EVENT DATA_CHANGED_FINISHED
                         OF CL_GUI_ALV_GRID,
    BUTTON_CLICK         FOR EVENT BUTTON_CLICK
                         OF CL_GUI_ALV_GRID
                         IMPORTING ES_COL_ID
                                   ES_ROW_NO,
    ONF1                 FOR EVENT ONF1
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_FIELDNAME
                                   ES_ROW_NO
                                   ER_EVENT_DATA,
    ON_F4                FOR EVENT ONF4 OF CL_GUI_ALV_GRID
                         IMPORTING SENDER
                                   E_FIELDNAME
                                   E_FIELDVALUE
                                   ES_ROW_NO
                                   ER_EVENT_DATA
                                   ET_BAD_CELLS
                                   E_DISPLAY,
    FUNCTION_SELECTED    FOR EVENT FUNCTION_SELECTED OF CL_GUI_TOOLBAR
                         IMPORTING FCODE.

  PRIVATE SECTION.
    TYPES : DDSHRETVAL_TABLE TYPE TABLE OF DDSHRETVAL.

    METHODS : MY_F4
          IMPORTING SENDER         TYPE REF TO CL_GUI_ALV_GRID
                    ET_BAD_CELLS   TYPE LVC_T_MODI
                    ES_ROW_NO      TYPE LVC_S_ROID
                    ER_EVENT_DATA  TYPE REF TO CL_ALV_EVENT_DATA
                    E_DISPLAY      TYPE C
                    E_FIELDNAME    TYPE LVC_FNAME
          EXPORTING LT_F4          TYPE DDSHRETVAL_TABLE.

ENDCLASS.                    "lcl_events DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_events_d0100 IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_EVENTS IMPLEMENTATION.

  METHOD USER_COMMAND.

    PERFORM EVENT_UCOMM IN PROGRAM (SY-CPROG)
            USING E_UCOMM 'X' .

  ENDMETHOD.                    "user_command

  METHOD BEFORE_USER_COMMAND.
*    PERFORM D0100_EVENT_BEFORE_UCOMM
*            USING E_UCOMM.
  ENDMETHOD.                    "before_user_command

  METHOD AFTER_USER_COMMAND.
*    PERFORM D0100_EVENT_AFTER_UCOMM
*            USING E_UCOMM.
  ENDMETHOD.                    "after_user_command

  METHOD DOUBLE_CLICK.
    PERFORM EVENT_DOUBLE_CLICK
            IN PROGRAM (SY-CPROG)
            USING E_ROW
                  E_COLUMN
                  ES_ROW_NO
                  'X' .
  ENDMETHOD.                    "double_click

  METHOD ITEM_DOUBLE_CLICK.
    PERFORM EVENT_ITEM_DOUBLE_CLICK
            IN PROGRAM (SY-CPROG)
            USING NODE_KEY
                  FIELDNAME
                  'X'.
  ENDMETHOD.                    "on_item_double_click

  METHOD NODE_DOUBLE_CLICK.
    PERFORM EVENT_NODE_DOUBLE_CLICK
            IN PROGRAM (SY-CPROG)
            USING NODE_KEY
                  'X'.
  ENDMETHOD.                    "on_node_double_click

  METHOD ITEM_KEYPRESS.
    MESSAGE I000(0K) WITH TEXT-E07 FIELDNAME NODE_KEY KEY.
  ENDMETHOD.                    "on_item_keypress

  METHOD NODE_KEYPRESS.
    MESSAGE I000(0K) WITH TEXT-E10 NODE_KEY KEY.
  ENDMETHOD.                    "on_node_keypress
*
  METHOD CHECKBOX_CHANGE.
*
    PERFORM EVENT_CHECKBOX_CHANGE
            IN PROGRAM (SY-CPROG)
            USING NODE_KEY
                  FIELDNAME
                  CHECKED
                  'X'.
*
  ENDMETHOD.                    "CHECKBOX_CHANGE
*
  METHOD LINK_CLICK.
    PERFORM EVENT_LINK_CLICK
            IN PROGRAM (SY-CPROG)
            USING FIELDNAME
                  NODE_KEY
                  'X'.
  ENDMETHOD.                    "LINK_CLICK

  METHOD BUTTON_CLICK_TREE.
    PERFORM EVENT_BUTTON_CLICK_TREE
            IN PROGRAM (SY-CPROG)
            USING FIELDNAME
                  NODE_KEY
                  'X'.
  ENDMETHOD.                    "LINK_CLICK

  METHOD HEADER_CLICK.
    PERFORM EVENT_HEADER_CLICK
            IN PROGRAM (SY-CPROG)
            USING FIELDNAME
                  'X'.
  ENDMETHOD.                    "HEADER_CLICK
*
  METHOD HOTSPOT_CLICK.
    PERFORM EVENT_HOTSPOT_CLICK
            IN PROGRAM (SY-CPROG)
            USING E_ROW_ID
                  E_COLUMN_ID
                  'X' .
  ENDMETHOD.                    "hotspot_click

  METHOD MENU_BUTTON.
*    PERFORM D0100_EVENT_MENU_BUTTON
*            USING E_OBJECT
*                  E_UCOMM.
  ENDMETHOD.                    "menu_button

*---------------------------------------------------------------
*_METHOD TOOLBAR
  METHOD TOOLBAR.
    PERFORM EVENT_TOOLBAR
            IN PROGRAM (SY-CPROG)
            USING E_OBJECT
                  E_INTERACTIVE
                  'X'.
  ENDMETHOD.                    "TOOLBAR

*---------------------------------------------------------------
*_METHOD context_menu_request
  METHOD CONTEXT_MENU_REQUEST.
*    PERFORM D0100_EVENT_CONTEXT_MENU_REQST
*            USING E_OBJECT.
  ENDMETHOD.                    "context_menu_request

*---------------------------------------------------------------
*_METHOD top_of_page
  METHOD TOP_OF_PAGE.
*    PERFORM D0100_EVENT_TOP_OF_PAGE
*            USING E_DYNDOC_ID.
  ENDMETHOD.                    "top_of_page

*---------------------------------------------------------------
*_METHOD end_of_list
  METHOD END_OF_LIST.
*    PERFORM D0100_EVENT_END_OF_LIST
*            USING E_DYNDOC_ID.
  ENDMETHOD.                    "end_of_list

*---------------------------------------------------------------
*_METHOD print_top_of_page
  METHOD PRINT_TOP_OF_PAGE.

    DATA: LD_PAGE(11) TYPE C,
          SY_PAGE(5),
          PAGE(5),
          LINSZ LIKE SY-LINSZ,
          WS_PAGES TYPE I,
          WS_REM TYPE I,
          TITLE_CNT TYPE I,
          LINCT TYPE I,
          HLINSZ TYPE I,
          TCODE(12).

    LINCT = SY-LINCT - 7.
    WS_PAGES = G_COUNT_C DIV LINCT.
    WS_REM   = G_COUNT_C MOD LINCT.
    IF WS_REM > 0.
      ADD 1 TO WS_PAGES.
    ENDIF.

    WRITE SY-PAGNO TO SY_PAGE RIGHT-JUSTIFIED. CONDENSE SY_PAGE.
    TCODE = SY-TCODE. CONDENSE TCODE.

    LINSZ  = SY-LINSZ.
    LINSZ  = LINSZ - 20.
    HLINSZ = LINSZ + 20.

***   프린트 용 헤더
    WRITE :/(100) SY-TITLE CENTERED ,
          AT LINSZ 'Tcode : ', (12) TCODE,
          / '일자/시간 : ', SY-DATUM, SY-UZEIT,
          AT LINSZ 'Page  : ', (12) SY_PAGE.    "LD_PAGE.

  ENDMETHOD.                    "print_top_of_page

*---------------------------------------------------------------
*_METHOD print_end_of_page
  METHOD PRINT_END_OF_PAGE.
*    PERFORM D0100_EVENT_PRINT_END_OF_PAGE.
  ENDMETHOD.                    "print_end_of_page

*---------------------------------------------------------------
*_METHOD print_top_of_list
  METHOD PRINT_TOP_OF_LIST.
*    PERFORM D0100_EVENT_PRINT_TOP_OF_LIST.
  ENDMETHOD.                    "print_top_of_list

*---------------------------------------------------------------
*_METHOD print_end_of_list
  METHOD PRINT_END_OF_LIST.
*    PERFORM D0100_EVENT_PRINT_END_OF_LIST.
  ENDMETHOD.                    "print_end_of_list

*---------------------------------------------------------------
*_METHOD after_refresh
  METHOD AFTER_REFRESH.
    PERFORM EVENT_AFTER_REFRESH
            IN PROGRAM (SY-CPROG)
            USING 'X'.
  ENDMETHOD.                    "after_refresh

*---------------------------------------------------------------
*_METHOD delayed_callback
  METHOD DELAYED_CALLBACK.
*    PERFORM D0100_EVENT_DELAYED_CALLBACK.
  ENDMETHOD.                    "delayed_callback

*---------------------------------------------------------------
*_METHOD delayed_changed_sel_callba
  METHOD DELAYED_CHANGED_SEL_CALLBACK.
*    PERFORM D0100_EVENT_CHANGED_SEL_CALLBA.
  ENDMETHOD.                    "delayed_changed_sel_callback

*---------------------------------------------------------------
*_METHOD SUBtotal_text
  METHOD SUBTOTAL_TEXT.
*    PERFORM D0100_EVENT_SUBTOTAL_TEXT
*            USING ES_SUBTOTTXT_INFO
*                  EP_SUBTOT_LINE
*                  E_EVENT_DATA.
  ENDMETHOD.                    "SUBtotal_text

*---------------------------------------------------------------
*_METHOD ondrag
  METHOD ONDRAG.
*    PERFORM D0100_EVENT_ONDRAG
*            USING E_ROW
*                  E_COLUMN
*                  E_DRAGDROPOBJ.
  ENDMETHOD.                    "ondrag

*---------------------------------------------------------------
*_METHOD ondrop
  METHOD ONDROP.
*    PERFORM D0100_EVENT_ONDROP
*            USING E_ROW
*            E_COLUMN
*            E_DRAGDROPOBJ.
  ENDMETHOD.                    "ondrop

*---------------------------------------------------------------
*_METHOD ondropcomplete
  METHOD ONDROPCOMPLETE.
*    PERFORM D0100_EVENT_ONDROPCOMPLETE
*            USING E_ROW
*                  E_COLUMN
*                  E_DRAGDROPOBJ.
  ENDMETHOD.                    "ondropcomplete

*---------------------------------------------------------------
*_METHOD ondropgetflavor
  METHOD ONDROPGETFLAVOR.

  ENDMETHOD.                    "ondropgetflavor

*---------------------------------------------------------------
*_METHOD data_changed
  METHOD DATA_CHANGED.
    PERFORM EVENT_DATA_CHANGED
            IN PROGRAM (SY-CPROG)
             USING ER_DATA_CHANGED
                   E_ONF4
                   E_ONF4_BEFORE
                   E_ONF4_AFTER
                   E_UCOMM
                   'X' .
  ENDMETHOD.                    "data_changed

*---------------------------------------------------------------
*       METHOD data_changed_finished
  METHOD DATA_CHANGED_FINISHED.
    PERFORM EVENT_DATA_CHANGED_FINIS
            IN PROGRAM (SY-CPROG)
            USING 'X'.
  ENDMETHOD.                    "data_changed_finished

*---------------------------------------------------------------
*_METHOD button_click
  METHOD BUTTON_CLICK.
    PERFORM EVENT_BUTTON_CLICK
            IN PROGRAM (SY-CPROG)
            USING ES_COL_ID
                  ES_ROW_NO
                  'X'.
  ENDMETHOD.                    "button_click

*---------------------------------------------------------------
*_METHOD onf1
  METHOD ONF1.
*    PERFORM D0100_EVENT_ONF1
*            USING E_FIELDNAME
*                  ES_ROW_NO
*                  ER_EVENT_DATA.
  ENDMETHOD.                                                "ONF1

*---------------------------------------------------------------
**_METHOD onf4
*  METHOD ONF4.
*    BREAK-POINT.
*    PERFORM EVENT_ONF4
*            IN PROGRAM (SY-CPROG)
*            USING E_FIELDNAME
*                  E_FIELDVALUE
*                  ES_ROW_NO
*                  ER_EVENT_DATA
*                  ET_BAD_CELLS
*                  E_DISPLAY
*                  'X'      .
*  ENDMETHOD.                                                "ONF4

*---------------------------------------------------------------
  METHOD ON_F4.
    PERFORM EVENT_ON_F4
            IN PROGRAM (SY-CPROG)
            USING SENDER
                 E_FIELDNAME
                 E_FIELDVALUE
                 ES_ROW_NO
                 ER_EVENT_DATA
                 ET_BAD_CELLS
                 E_DISPLAY
                 'X'.
  ENDMETHOD.                                                "ON_F4
*---------------------------------------------------------------
  METHOD MY_F4.
    PERFORM EVENT_MY_F4
            IN PROGRAM (SY-CPROG)
            TABLES LT_F4
            USING SENDER
                  ET_BAD_CELLS
                  ES_ROW_NO
                  ER_EVENT_DATA
                  E_DISPLAY
                  E_FIELDNAME.
  ENDMETHOD.                                                "MY_F4
*---------------------------------------------------------------
  METHOD FUNCTION_SELECTED.
*
    PERFORM EVENT_FUNCTION_SELECTED
            IN PROGRAM (SY-CPROG)
            USING FCODE 'X'.
  ENDMETHOD.                    "on_function_selected
*
ENDCLASS.                    "lcl_events_d0100 IMPLEMENTATION
*******



*&**********************************************************************
*  CLASS DATA                                                          *
*&**********************************************************************
DATA :  G_CUSTOM_CONTAINER  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
        G_CUSTOM_CONTAINER2 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
        G_SPLIT_CONTAINER   TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
        G_DOCKING_CONTAINER TYPE REF TO CL_GUI_DOCKING_CONTAINER,
        G_GUI_CONTAINER1   TYPE REF TO CL_GUI_CONTAINER ,
        G_GUI_CONTAINER2   TYPE REF TO CL_GUI_CONTAINER ,
        G_ALV_TREE         TYPE REF TO CL_GUI_ALV_TREE,
        G_ALV_TREE1        TYPE REF TO CL_GUI_ALV_TREE,
        G_ALV_TREE2        TYPE REF TO CL_GUI_ALV_TREE,
        G_TREE_SIMPLE      TYPE REF TO CL_GUI_ALV_TREE_SIMPLE ,
        G_TREE             TYPE REF TO CL_ALV_TREE_BASE,
        G_TREE1            TYPE REF TO CL_ALV_TREE_BASE,
        G_TREE2            TYPE REF TO CL_ALV_TREE_BASE,
        G_CONTAINER        TYPE SCRFNAME VALUE 'CONTAINER',
        G_CONTAINER2       TYPE SCRFNAME VALUE 'CONTAINER2',
        G_GRID             TYPE REF TO CL_GUI_ALV_GRID,
        G_GRID1            TYPE REF TO CL_GUI_ALV_GRID,
        G_GRID2            TYPE REF TO CL_GUI_ALV_GRID,
        G_GRID3            TYPE REF TO CL_GUI_ALV_GRID,
        G_EVENTS           TYPE REF TO LCL_EVENTS,
        G_EDITOR           TYPE REF TO CL_GUI_TEXTEDIT,
        G_EDITOR_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
        G_TOOLBAR          TYPE REF TO CL_GUI_TOOLBAR.

* TABLE TYPE.
DATA :  GT_FIELDCAT    TYPE LVC_T_FCAT,
        GT_SORT        TYPE LVC_T_SORT,
        GT_STYL        TYPE LVC_T_STYL,
        GT_TABCOLOR    TYPE LVC_T_SCOL,
        GT_EXCL_FUNC   TYPE UI_FUNCTIONS,
        GT_LAYOUT_ITEM TYPE LVC_T_LAYI.
*
DATA : G_FIELD_CT         TYPE SLIS_T_FIELDCAT_ALV .
DATA:  GS_HIERARCHY_HEADER TYPE TREEV_HHDR ,
       GT_LISTHEADER  TYPE SLIS_T_LISTHEADER ,
       GS_LISTHEADER  TYPE SLIS_LISTHEADER,
       GS_EXCEPTION_FIELD  TYPE LVC_S_L004.
*
* STRUCTURE TYPE
DATA :  GS_LAYOUT         TYPE LVC_S_LAYO,
        GS_SORT           TYPE LVC_S_SORT,
        GS_STYL_C         TYPE LVC_S_STYL,
        GS_TABCOLOR       TYPE LVC_S_SCOL,
        GS_TOOLBAR        TYPE STB_BUTTON,
        GS_CURR_COL       TYPE LVC_S_COL,
        GS_CURR_ROW       TYPE LVC_S_ROID,
        GS_LAYOUT_LEAF    TYPE LVC_S_LAYN,
        GS_LAYOUT_NODE    TYPE LVC_S_LAYN,
        GS_FIELDCAT       TYPE LVC_S_FCAT.

DATA : GS_STABLE          TYPE LVC_S_STBL. "
*ADD BSGSM_FCM 20200422


DATA :  GS_T001C   TYPE T001.

DATA :  GS_O_LAYOUT TYPE DISVARIANT.      "for parameter IS_VAPIANT


DATA : OK_CODE_C LIKE SY-UCOMM ,
       G_CHANGE_DATA,
       G_ERROR_C ,
       G_SAVE_C  ,
       G_INPUT TYPE I VALUE 0,
       G_ANSWER,
       G_CHECK_C,
       G_TOT_CNT TYPE I,
       G_DEL_CNT TYPE I,
       G_PERCENT TYPE P,
       G_MESSAGE(20).

DATA : G_REPID_C TYPE SY-REPID.
DATA : G_COMMENT(70).

*_F4 필드관련
DATA : GS_F4        TYPE LVC_S_F4,
       GT_F4        TYPE LVC_T_F4.


* message popup.
DATA : GT_MESSAGE_C TYPE BAPIRETTAB.
DATA : GS_MESSAGE_C LIKE BAPIRET2.

* 생성정보
DATA : G_AEDAT LIKE SY-DATUM,
       G_AEZEI LIKE SY-UZEIT,
       G_ERNAM LIKE SY-UNAME.




*---------------------------------------------------------------
FIELD-SYMBOLS : <F4TAB> TYPE LVC_T_MODI.

*---------------------------------------------------------------
*------Possible Entry
DATA : LT_VALUES TYPE TABLE OF SEAHLPRES,
       LT_FIELDS TYPE TABLE OF DFIES,
       LS_VALUE  TYPE SEAHLPRES,
       LS_FIELD  TYPE DFIES,

       LS_F4     TYPE DDSHRETVAL,
       LS_MODI   TYPE LVC_S_MODI.

************************************************************************
* DEFINE...
************************************************************************
DEFINE D_TOOLBAR.
  CLEAR GS_TOOLBAR.
  GS_TOOLBAR-FUNCTION  = &1.
  GS_TOOLBAR-ICON      = &2.
  GS_TOOLBAR-BUTN_TYPE = &3.
  GS_TOOLBAR-DISABLED  = &4.
  GS_TOOLBAR-TEXT      = &5.
  GS_TOOLBAR-QUICKINFO = &6.
  GS_TOOLBAR-CHECKED   = &7.
  APPEND GS_TOOLBAR TO &8->MT_TOOLBAR.
END-OF-DEFINITION.


*&---------------------------------------------------------------------*
*&      Form  SET_SORT
*&---------------------------------------------------------------------*
FORM SET_SORT_C  USING  P_SPOS
                      P_FIELD
                      P_UP
                      P_DOWN
                      P_GROUP
                      P_SUBTOT
                      P_COMP
                      P_EXPA.

  DATA: LS_SORT TYPE LVC_S_SORT.
*
  LS_SORT-SPOS      = P_SPOS .
  LS_SORT-FIELDNAME = P_FIELD.
  LS_SORT-UP        = P_UP.
  LS_SORT-DOWN      = P_DOWN.
  LS_SORT-GROUP     = P_GROUP.
  LS_SORT-SUBTOT    = P_SUBTOT.
  LS_SORT-COMP      = P_COMP.
  LS_SORT-EXPA      = P_EXPA.
*
  INSERT LS_SORT INTO TABLE GT_SORT.

ENDFORM.                    " SET_SORT
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
FORM SET_LAYOUT_C .
*  INPUT.
*  GS_LAYOUT-EDIT = 'X' .
  GS_LAYOUT-STYLEFNAME = 'H_STYLE'.
*  GS_LAYOUT-ZEBRA      = 'X'.
* B:single C:multi D:cell A:rowcol
  GS_LAYOUT-SEL_MODE   = 'B'.
* ROW COLOR
  GS_LAYOUT-INFO_FNAME = 'COLOR'.
* CELL COLOR
  GS_LAYOUT-CTAB_FNAME = 'TABCOLOR'.
** BOX
*  GS_LAYOUT-BOX_FNAME  = 'MAPK'.
* OPTIMAZE
  GS_LAYOUT-CWIDTH_OPT = 'X'.
* Title
  GS_LAYOUT-GRID_TITLE = TEXT-T01 .
ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  CALL_FIRST_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CALL_FIRST_DISPLAY  TABLES   P_TABLE .
*---------------------------------------------------------------
  CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_SAVE               = 'A'
      I_DEFAULT            = 'X'
      IS_LAYOUT            = GS_LAYOUT
      IS_VARIANT           = GS_O_LAYOUT "&see below
      IT_TOOLBAR_EXCLUDING = GT_EXCL_FUNC
    CHANGING
      IT_FIELDCATALOG      = GT_FIELDCAT
      IT_SORT              = GT_SORT
      IT_OUTTAB            = P_TABLE[].

ENDFORM.                    " CALL_FIRST_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  SET_STYLE
*&---------------------------------------------------------------------*
*       CELL EDIT.
*----------------------------------------------------------------------*
FORM SET_STYLE USING PT_STYL TYPE LVC_T_STYL
                     P_FIELD
                     P_MAXLEN
                     P_DIS.
  CLEAR : GS_STYL_C.
  GS_STYL_C-FIELDNAME  = P_FIELD.
  CASE P_DIS.
    WHEN 'E'.
      GS_STYL_C-STYLE      = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    WHEN 'D'.
      GS_STYL_C-STYLE      = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
  ENDCASE.

  GS_STYL_C-MAXLEN     = P_MAXLEN.
  INSERT GS_STYL_C INTO TABLE PT_STYL .
*
ENDFORM.                    " SET_STYLE
*&---------------------------------------------------------------------*
*&      Form  SET_CEL_COLOR
*&---------------------------------------------------------------------*
*       CELL COLOR
*----------------------------------------------------------------------*
FORM SET_CEL_COLOR  USING    PT_TABLCOLOR TYPE LVC_T_SCOL
                             P_FIELD
                             P_COLOR.
*
  GS_TABCOLOR-FNAME     = P_FIELD.
  GS_TABCOLOR-COLOR-COL = P_COLOR+0(1).
  GS_TABCOLOR-COLOR-INT = P_COLOR+1(1).
  GS_TABCOLOR-COLOR-INV = P_COLOR+2(1).
  GS_TABCOLOR-NOKEYCOL  = 'X'.
  INSERT GS_TABCOLOR INTO TABLE PT_TABLCOLOR.
ENDFORM.                    " SET_CEL_COLOR

*&---------------------------------------------------------------------*
*&      Form  SET_GRID_TOOLBAR
*&---------------------------------------------------------------------*
FORM SET_GRID_TOOLBAR CHANGING CT_EXCL_FUNC
                                              TYPE UI_FUNCTIONS.
  DATA: LS_FUNC TYPE LVC_S_EXCL.

  APPEND CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL TO CT_EXCL_FUNC.
*
*  LOOP AT GS_GRID_OPT-TOOLBAR-TOOLBAR_EXCL_FUNC INTO LS_FUNC.
*    APPEND LS_FUNC TO CT_EXCL_FUNC.
*  ENDLOOP.
ENDFORM.                    " SET_GRID_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  SET_GRID_TOOLBAR
*&---------------------------------------------------------------------*
FORM SET_GRID_TOOLBAR_CON1 CHANGING CT_EXCL_FUNC
                                              TYPE UI_FUNCTIONS.
  DATA: LS_FUNC TYPE LVC_S_EXCL.

  LOOP AT GT_EXCL_FUNC INTO LS_FUNC.
    APPEND LS_FUNC TO CT_EXCL_FUNC.
  ENDLOOP.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL TO CT_EXCL_FUNC.

ENDFORM.                    " SET_GRID_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  REFRESH_DISPLAY
*&---------------------------------------------------------------------*
FORM REFRESH_DISPLAY USING P_GRID TYPE REF TO CL_GUI_ALV_GRID.
  DATA :  LS_ROW_ID  TYPE LVC_S_ROW ,
          LS_COL_ID  TYPE LVC_S_COL ,
          LS_ROID_ID TYPE LVC_S_ROID.

*  SCROLL 위치고정을 위함.
  CALL METHOD P_GRID->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_CURR_COL
      ES_ROW_NO   = GS_CURR_ROW.
*
  CALL METHOD P_GRID->GET_CURRENT_CELL
    IMPORTING
      ES_ROW_ID = LS_ROW_ID
      ES_COL_ID = LS_COL_ID
      ES_ROW_NO = LS_ROID_ID.

*  REFRESH DISPLAY
  CALL METHOD P_GRID->REFRESH_TABLE_DISPLAY.
*REFRESH 후 SCROLL 위치로 돌림.
  CALL METHOD P_GRID->SET_SCROLL_INFO_VIA_ID
    EXPORTING
      IS_COL_INFO = GS_CURR_COL
      IS_ROW_NO   = GS_CURR_ROW.

  CALL METHOD P_GRID->SET_CURRENT_CELL_VIA_ID
    EXPORTING
      IS_ROW_ID    = LS_ROW_ID
      IS_COLUMN_ID = LS_COL_ID
      IS_ROW_NO    = LS_ROID_ID.

ENDFORM.                    " REFRESH_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM_1
*&---------------------------------------------------------------------*
FORM POPUP_TO_CONFIRM_1 USING P_DEFAULT
                            P_TITLE
                            P_TEXT1
                            P_TEXT2
                            P_DISPLAY
                      CHANGING P_ANSWER.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
       DEFAULTOPTION           = P_DEFAULT
       TEXTLINE1               = P_TEXT1
       TEXTLINE2               = P_TEXT2
       TITEL                   = P_TITLE
       CANCEL_DISPLAY          = P_DISPLAY
    IMPORTING
       ANSWER                  = P_ANSWER
    EXCEPTIONS
        TEXT_NOT_FOUND                      .

ENDFORM.                    " POPUP_TO_CONFIRM_1

*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT_CON
*&---------------------------------------------------------------------*
FORM SET_LAYOUT_CON USING P_FNAME
                          P_EDIT
                          P_SEL
                          P_STYLEFNAME
                          P_COLOR
                          P_TABC
                          P_BOX
                          P_OPT
                          P_FLAG.
*_Lights
  GS_LAYOUT-EXCP_FNAME = P_FNAME.
*_INPUT.
  GS_LAYOUT-EDIT = P_EDIT .

  GS_LAYOUT-ZEBRA      = 'X'.

  GS_LAYOUT-SEL_MODE   = P_SEL.

*_
  GS_LAYOUT-STYLEFNAME = P_STYLEFNAME.

*_ROW COLOR
  GS_LAYOUT-INFO_FNAME = P_COLOR.
*_CELL COLOR
  GS_LAYOUT-CTAB_FNAME = P_TABC .
*_BOX
  GS_LAYOUT-BOX_FNAME  = P_BOX.

*_OPTIMAZE
  GS_LAYOUT-CWIDTH_OPT = P_OPT.

*_Title
CASE P_FLAG.
  WHEN 1.
  GS_LAYOUT-GRID_TITLE = TEXT-T01 .
  WHEN 2.
      GS_LAYOUT-GRID_TITLE = TEXT-T02 .
ENDCASE.
ENDFORM.                    " SET_LAYOUT_CON
*&---------------------------------------------------------------------*
*&      Form  SET_INPUT_MET_CON
*&---------------------------------------------------------------------*
FORM SET_INPUT_MET_CON
     USING  P_GB
            P_IN.

  IF P_GB EQ 'X'.
*___ENTER EVENT
    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
  ELSE.
*___DATA MODIFY EVENT
    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
  ENDIF.

*_INPUT EVENT
  CALL METHOD G_GRID->SET_READY_FOR_INPUT
    EXPORTING
      I_READY_FOR_INPUT = P_IN.

ENDFORM.                    " SET_INPUT_MET_CON
**&---------------------------------------------------------------------
**
**&      Form  SET_GRID_TOOLBAR_CON
**&---------------------------------------------------------------------
**
FORM SET_GRID_TOOLBAR_CON
     CHANGING CT_EXCL_FUNC TYPE UI_FUNCTIONS.


  PERFORM LOCAL_TOOL_CON IN PROGRAM (SY-CPROG)
          CHANGING CT_EXCL_FUNC.


ENDFORM.                    " SET_GRID_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  FREE_RTN
*&---------------------------------------------------------------------*
FORM FREE_RTN .
  CALL METHOD G_GRID->FREE.
  CALL METHOD G_CUSTOM_CONTAINER->FREE.
  CALL METHOD CL_GUI_CFW=>FLUSH.
  CLEAR : G_CUSTOM_CONTAINER.
ENDFORM.                    " FREE_RTN
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_BAP
*&---------------------------------------------------------------------*
FORM PROGRESS_BAP USING P_TOT_CNT .

  G_PERCENT = ( SY-TABIX / G_TOT_CNT ) * 100.
  MOVE : G_PERCENT TO G_MESSAGE+0(3).
  G_MESSAGE+3(17) = '% 자료 갱신중...' .
*
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      PERCENTAGE = G_PERCENT
      TEXT       = G_MESSAGE
    EXCEPTIONS
      OTHERS     = 1.
ENDFORM.                    " PROGRESS_BAP
*&---------------------------------------------------------------------*
*&      Form  SET_INPUT_CON
*&---------------------------------------------------------------------*
FORM SET_INPUT_CON USING    P_GRID TYPE REF TO CL_GUI_ALV_GRID
                            P_GB
                            P_IN.

  IF P_GB EQ 'X'.
*___ENTER EVENT
    CALL METHOD P_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
  ELSE.
*___DATA MODIFY EVENT
    CALL METHOD P_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
  ENDIF.

*_INPUT EVENT
  CALL METHOD P_GRID->SET_READY_FOR_INPUT
    EXPORTING
      I_READY_FOR_INPUT = P_IN.


ENDFORM.                    " SET_INPUT_CON
*&---------------------------------------------------------------------*
*&      Form  CALL_GRID_DISPLAY
*&---------------------------------------------------------------------*
FORM CALL_GRID_DISPLAY    TABLES   P_TABLE
                          USING    P_GRID TYPE REF TO CL_GUI_ALV_GRID  .

  DESCRIBE TABLE P_TABLE[] LINES G_COUNT_C.
  MESSAGE S000(ZCO01) WITH G_COUNT_C '건을 조회하였습니다!!!'.

  CALL METHOD P_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_SAVE               = 'A'
      I_DEFAULT            = 'X'
      IS_LAYOUT            = GS_LAYOUT
      IS_VARIANT           = GS_O_LAYOUT
      IT_TOOLBAR_EXCLUDING = GT_EXCL_FUNC
    CHANGING
      IT_FIELDCATALOG      = GT_FIELDCAT
      IT_SORT              = GT_SORT
      IT_OUTTAB            = P_TABLE[].

ENDFORM.                    " CALL_GRID_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  ALV_MODIFY_CELL
*&---------------------------------------------------------------------*
FORM ALV_MODIFY_CELL_C  USING RR_DATA_CHANGED  TYPE REF TO
                                      CL_ALV_CHANGED_DATA_PROTOCOL
                            P_INDEX TYPE SY-TABIX P_FIELD P_VALUE TYPE
                            ANY.

  CALL METHOD RR_DATA_CHANGED->MODIFY_CELL
    EXPORTING
      I_ROW_ID    = P_INDEX
      I_FIELDNAME = P_FIELD
      I_VALUE     = P_VALUE.

ENDFORM.                    " ALV_MODIFY_CELL
*&---------------------------------------------------------------------*
*&      Form  MULTI_MSG_POPUP_CLASS
*&---------------------------------------------------------------------*
FORM MULTI_MSG_POPUP_CLASS  USING PT_MESSAGE TYPE BAPIRETTAB.
  CALL FUNCTION 'OXT_MESSAGE_TO_POPUP'
    EXPORTING
      IT_MESSAGE = PT_MESSAGE
    EXCEPTIONS
      BAL_ERROR  = 1
      OTHERS     = 2.
ENDFORM.                    " MULTI_MSG_POPUP_CLASS
*&---------------------------------------------------------------------*
*&      Form  MOVE_MSG_CLASS
*&---------------------------------------------------------------------*
FORM MOVE_MSG_CLASS  USING    P_TYPE
                              P_ID
                              P_NUM
                              P_V1
                              P_V2
                              P_V3
                              P_V4.

  MOVE: P_TYPE TO GS_MESSAGE_C-TYPE ,
        P_ID   TO GS_MESSAGE_C-ID ,
        P_NUM  TO GS_MESSAGE_C-NUMBER ,
        P_V1   TO GS_MESSAGE_C-MESSAGE_V1 ,
        P_V2   TO GS_MESSAGE_C-MESSAGE_V2 ,
        P_V3   TO GS_MESSAGE_C-MESSAGE_V3 ,
        P_V4   TO GS_MESSAGE_C-MESSAGE_V4 .
  INSERT GS_MESSAGE_C INTO TABLE GT_MESSAGE_C.

ENDFORM.                    " MOVE_MSG_CLASS
*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_MESSAGE_C
*&---------------------------------------------------------------------*
FORM POPUP_TO_MESSAGE_C USING P_DEFAULT P_TITLE P_TEXT1 P_TEXT2
P_DISPLAY
                        CHANGING P_ANSWER.
*
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
       DEFAULTOPTION           = P_DEFAULT
       TEXTLINE1               = P_TEXT1
       TEXTLINE2               = P_TEXT2
       TITEL                   = P_TITLE
       CANCEL_DISPLAY          = P_DISPLAY
    IMPORTING
       ANSWER                  = P_ANSWER
    EXCEPTIONS
        TEXT_NOT_FOUND                      .

ENDFORM.                    " POPUP_TO_MESSAGE_C
*&---------------------------------------------------------------------*
*&      Form  F4_SPMON_CLASS
*&---------------------------------------------------------------------*
FORM F4_SPMON_CLASS .
  DATA: BEGIN OF MF_DYNPFIELDS OCCURS 1.
          INCLUDE STRUCTURE DYNPREAD.
  DATA: END   OF MF_DYNPFIELDS.
  DATA: MF_RETURNCODE   LIKE SY-SUBRC,
        MF_PERIOD       LIKE ISELLIST-MONTH,
        MF_HLP_REPID    LIKE SY-REPID.
  FIELD-SYMBOLS: <FS>.

* Wert von Dynpro lesen
  CLEAR: MF_DYNPFIELDS, MF_DYNPFIELDS[].
  GET CURSOR FIELD MF_DYNPFIELDS-FIELDNAME.
  APPEND MF_DYNPFIELDS.
  MF_HLP_REPID = SY-REPID.
  DO 2 TIMES.
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        DYNAME               = MF_HLP_REPID
        DYNUMB               = SY-DYNNR
      TABLES
        DYNPFIELDS           = MF_DYNPFIELDS
      EXCEPTIONS
        INVALID_ABAPWORKAPEA = 01
        INVALID_DYNPROFIELD  = 02
        INVALID_DYNPRONAME   = 03
        INVALID_DYNPRONUMMER = 04
        INVALID_REQUEST      = 05
        NO_FIELDDESCRIPTION  = 06
        UNDEFIND_ERROR       = 07.
    IF SY-SUBRC = 3.
*     Aktuelles Dynpro ist Wertemengenbild
      MF_HLP_REPID = 'SAPLALDB'.
    ELSE.
      READ TABLE MF_DYNPFIELDS INDEX 1.
*     Unterstriche durch Blanks ersetzen
      TRANSLATE MF_DYNPFIELDS-FIELDVALUE USING '_ '.
      EXIT.
    ENDIF.
  ENDDO.
  IF SY-SUBRC = 0.
*   Konvertierung ins interne Format
    CALL FUNCTION 'CONVERSION_EXIT_PERI_INPUT'
      EXPORTING
        INPUT  = MF_DYNPFIELDS-FIELDVALUE
      IMPORTING
        OUTPUT = MF_PERIOD.
    IF MF_PERIOD IS INITIAL.
*     period ist initial => Vorschlagswert aus akt. Datum ableiten
      MF_PERIOD = SY-DATLO(6).
    ENDIF.
    CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
      EXPORTING
        ACTUAL_MONTH               = MF_PERIOD
      IMPORTING
        SELECTED_MONTH             = MF_PERIOD
        RETURN_CODE                = MF_RETURNCODE
      EXCEPTIONS
        FACTORY_CALENDAP_NOT_FOUND = 01
        HOLIDAY_CALENDAP_NOT_FOUND = 02
        MONTH_NOT_FOUND            = 03.
    IF SY-SUBRC = 0 AND MF_RETURNCODE = 0.
      ASSIGN (MF_DYNPFIELDS-FIELDNAME) TO <FS>.
      <FS> = MF_PERIOD.
    ENDIF.
  ENDIF.

ENDFORM.                    " F4_SPMON_CLASS
*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_MESSAGE_CLASS
*&---------------------------------------------------------------------*
FORM POPUP_TO_MESSAGE_CLASS USING P_DEFAULT P_TITLE P_TEXT1 P_TEXT2
P_DISPLAY
                            CHANGING P_ANSWER.
*
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
       DEFAULTOPTION           = P_DEFAULT
       TEXTLINE1               = P_TEXT1
       TEXTLINE2               = P_TEXT2
       TITEL                   = P_TITLE
       CANCEL_DISPLAY          = P_DISPLAY
    IMPORTING
       ANSWER                  = P_ANSWER
    EXCEPTIONS
        TEXT_NOT_FOUND                      .
ENDFORM.                    " POPUP_TO_MESSAGE_CLASS
*&---------------------------------------------------------------------*
*&      Form  F4_ALV_VAPIANT_CLASS
*&---------------------------------------------------------------------*
FORM F4_ALV_VAPIANT_CLASS  CHANGING PS_VAPIANT TYPE DISVARIANT .
  DATA : LS_MODE TYPE AQLIMODE.
  LS_MODE-PERSREPORT = SY-CPROG.
  LS_MODE-LID        = PS_VAPIANT-LOG_GROUP.
*  CALL FUNCTION 'ZREAD_LAYOUT_VALUE'
*    CHANGING
*      RTMODE  = LS_MODE
*      VAPIANT = PS_VAPIANT-VAPIANT.

ENDFORM.                    " F4_ALV_VAPIANT_CLASS

*&---------------------------------------------------------------------*
*&      Form  MODIFY_CELL_CLASS
*&---------------------------------------------------------------------*
FORM MODIFY_CELL_CLASS USING P_IDX
                             P_FIELDNAME
                             P_VALUE.

  DATA : LT_MODI TYPE LVC_T_MODI ,
         LS_MODI TYPE LVC_S_MODI .

  DATA : L_ALV TYPE REF TO CL_GUI_ALV_GRID.
*
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = L_ALV.
*
  REFRESH LT_MODI.
  LS_MODI-ROW_ID     = P_IDX.
  LS_MODI-FIELDNAME  = P_FIELDNAME.
  LS_MODI-VALUE      = P_VALUE.
  INSERT LS_MODI INTO TABLE LT_MODI.

  IF NOT L_ALV IS INITIAL.
    CALL METHOD L_ALV->SET_DELTA_CELLS
      EXPORTING
        IT_DELTA_CELLS = LT_MODI.
  ENDIF.

  IF NOT G_GRID IS INITIAL.
    CALL METHOD G_GRID->SET_DELTA_CELLS
      EXPORTING
        IT_DELTA_CELLS = LT_MODI.
  ENDIF.

  IF NOT G_GRID1 IS INITIAL.
    CALL METHOD G_GRID1->SET_DELTA_CELLS
      EXPORTING
        IT_DELTA_CELLS = LT_MODI.
  ENDIF.

ENDFORM.                    " MODIFY_CELL_CLASS
*&---------------------------------------------------------------------*
*&      Form  F4_SPMON_C
*&---------------------------------------------------------------------*
FORM F4_SPMON_C .
  DATA: BEGIN OF MF_DYNPFIELDS OCCURS 1.
          INCLUDE STRUCTURE DYNPREAD.
  DATA: END   OF MF_DYNPFIELDS.
  DATA: MF_RETURNCODE   LIKE SY-SUBRC,
        MF_PERIOD       LIKE ISELLIST-MONTH,
        MF_HLP_REPID    LIKE SY-REPID.
  FIELD-SYMBOLS: <FS>.

* Wert von Dynpro lesen
  CLEAR: MF_DYNPFIELDS, MF_DYNPFIELDS[].
  GET CURSOR FIELD MF_DYNPFIELDS-FIELDNAME.
  APPEND MF_DYNPFIELDS.
  MF_HLP_REPID = SY-REPID.
  DO 2 TIMES.
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        DYNAME               = MF_HLP_REPID
        DYNUMB               = SY-DYNNR
      TABLES
        DYNPFIELDS           = MF_DYNPFIELDS
      EXCEPTIONS
        INVALID_ABAPWORKAPEA = 01
        INVALID_DYNPROFIELD  = 02
        INVALID_DYNPRONAME   = 03
        INVALID_DYNPRONUMMER = 04
        INVALID_REQUEST      = 05
        NO_FIELDDESCRIPTION  = 06
        UNDEFIND_ERROR       = 07.
    IF SY-SUBRC = 3.
*     Aktuelles Dynpro ist Wertemengenbild
      MF_HLP_REPID = 'SAPLALDB'.
    ELSE.
      READ TABLE MF_DYNPFIELDS INDEX 1.
*     Unterstriche durch Blanks ersetzen
      TRANSLATE MF_DYNPFIELDS-FIELDVALUE USING '_ '.
      EXIT.
    ENDIF.
  ENDDO.
  IF SY-SUBRC = 0.
*   Konvertierung ins interne Format
    CALL FUNCTION 'CONVERSION_EXIT_PERI_INPUT'
      EXPORTING
        INPUT  = MF_DYNPFIELDS-FIELDVALUE
      IMPORTING
        OUTPUT = MF_PERIOD.
    IF MF_PERIOD IS INITIAL.
*     period ist initial => Vorschlagswert aus akt. Datum ableiten
      MF_PERIOD = SY-DATLO(6).
    ENDIF.
    CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
      EXPORTING
        ACTUAL_MONTH               = MF_PERIOD
      IMPORTING
        SELECTED_MONTH             = MF_PERIOD
        RETURN_CODE                = MF_RETURNCODE
      EXCEPTIONS
        FACTORY_CALENDAP_NOT_FOUND = 01
        HOLIDAY_CALENDAP_NOT_FOUND = 02
        MONTH_NOT_FOUND            = 03.
    IF SY-SUBRC = 0 AND MF_RETURNCODE = 0.
      ASSIGN (MF_DYNPFIELDS-FIELDNAME) TO <FS>.
      <FS> = MF_PERIOD.
    ENDIF.
  ENDIF.

ENDFORM.                    " F4_SPMON_C
*&---------------------------------------------------------------------*
*&      Form  LAST_DAY_C
*&---------------------------------------------------------------------*
FORM LAST_DAY_C  USING    PF_DATE
                          PT_DATE.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = PF_DATE
    IMPORTING
      LAST_DAY_OF_MONTH = PT_DATE
    EXCEPTIONS
      DAY_IN_NO_DATE    = 1
      OTHERS            = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " LAST_DAY_C
***

*&---------------------------------------------------------------------*
*&      Form  ADD_NODE
*&---------------------------------------------------------------------*
FORM ADD_NODE  USING    P_ALV_TREE TYPE REF TO CL_GUI_ALV_TREE
                        PS_ORG
                        P_RELAT_KEY TYPE LVC_NKEY
                        P_NODE_TEXT
               CHANGING P_NODE_KEY  TYPE LVC_NKEY.

  CALL METHOD P_ALV_TREE->ADD_NODE
    EXPORTING
      I_RELAT_NODE_KEY = P_RELAT_KEY
      I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
      I_NODE_TEXT      = P_NODE_TEXT
      IS_OUTTAB_LINE   = PS_ORG
      IS_NODE_LAYOUT   = GS_LAYOUT_LEAF
      IT_ITEM_LAYOUT   = GT_LAYOUT_ITEM
    IMPORTING
      E_NEW_NODE_KEY   = P_NODE_KEY.


ENDFORM.                    " ADD_NODE

*&---------------------------------------------------------------------*
*&      Form  SET_COLUMN_OPTIMIZE
*&---------------------------------------------------------------------*
FORM SET_COLUMN_OPTIMIZE  USING P_ALV_TREE TYPE REF TO CL_GUI_ALV_TREE
                                P_INCHDR.
*
  CALL METHOD P_ALV_TREE->COLUMN_OPTIMIZE
    EXPORTING
      I_INCLUDE_HEADING = P_INCHDR.
*
ENDFORM.                    " SET_COLUMN_OPTIMIZE
*&---------------------------------------------------------------------*
*&      Form  GET_OUTTAB_LINE
*&---------------------------------------------------------------------*
FORM GET_OUTTAB_LINE  USING    P_ALV_TREE TYPE REF TO CL_GUI_ALV_TREE
                               P_NODE
                               P_NODE_LAYOUT
                      CHANGING P_OUTTAB_LINE.
*
  CALL METHOD P_ALV_TREE->GET_OUTTAB_LINE
    EXPORTING
      I_NODE_KEY     = P_NODE
    IMPORTING
      E_OUTTAB_LINE  = P_OUTTAB_LINE
      ES_NODE_LAYOUT = P_NODE_LAYOUT.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
*
ENDFORM.                    " GET_OUTTAB_LINE
*&---------------------------------------------------------------------*
*&      Form  GET_T001C
*&---------------------------------------------------------------------*
FORM GET_T001C  USING    P_BUKRS.

  SELECT SINGLE *
    INTO GS_T001C
    FROM T001
   WHERE BUKRS EQ P_BUKRS.

ENDFORM.                                                    " GET_T001C
*&---------------------------------------------------------------------*
*&      Form  EXPAND_NODE
*&---------------------------------------------------------------------*
FORM EXPAND_NODE  USING PD_NODE_KEY TYPE LVC_NKEY.

  CALL METHOD G_ALV_TREE->EXPAND_NODE
    EXPORTING
      I_NODE_KEY = PD_NODE_KEY.

ENDFORM.                    " EXPAND_NODE

************************************************************************
*      CALL METHOD G_GRID->RAISE_EVENT
*        EXPORTING
*          I_UCOMM = UCOMM.
************************************************************************
