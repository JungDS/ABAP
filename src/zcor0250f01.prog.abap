*&---------------------------------------------------------------------*
*& Include          ZCOR0250F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

  PERFORM SELECTED_MAIN_DATA.

ENDFORM.                    " SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
*&      Form  SELECTED_MAIN_DATA
*&---------------------------------------------------------------------*
FORM SELECTED_MAIN_DATA.

  DATA: LT_CELLTYPE TYPE SALV_T_INT4_COLUMN.
  DATA: LS_CELLTYPE LIKE LINE OF LT_CELLTYPE.
  DATA LV_LINES TYPE I.

  CLEAR: GT_DISPLAY.

  SELECT A~SAKNR AS KSTAR, B~TXT20 AS KTEXT,
         C~KOKRS, C~CCTR, C~CCTR_SAL, C~CCTR_PRD,
         C~W01, C~W02, C~W03,
         C~W04,   C~W05,  C~W06, C~W07
    FROM SKA1 AS A
    LEFT JOIN SKAT AS B
      ON A~KTOPL = B~KTOPL
     AND A~SAKNR = B~SAKNR
     AND B~SPRAS = @SY-LANGU
    LEFT JOIN ZCOT1170 AS C
      ON A~SAKNR = C~KSTAR
     AND C~KOKRS = @PA_KOKRS
   WHERE A~KTOPL          = @GC_KTOPL
     AND A~SAKNR BETWEEN '0400000000' AND '0899999999'
     AND A~SAKNR          IN @SO_KSTAR
     ORDER BY A~SAKNR
    INTO CORRESPONDING FIELDS OF TABLE @GT_DISPLAY.

  MOVE PA_KOKRS TO GS_DISPLAY-KOKRS.
  MODIFY GT_DISPLAY FROM GS_DISPLAY
                TRANSPORTING KOKRS
                    WHERE KOKRS <> PA_KOKRS.

  LV_LINES = LINES( GT_DISPLAY ).

  MESSAGE S039 WITH LV_LINES.

ENDFORM.                    " SELECTED_MAIN_DATA
*&---------------------------------------------------------------------*
*&      Form  EVENT_TOOLBAR
*&---------------------------------------------------------------------*
FORM EVENT_TOOLBAR
       USING PR_OBJECT     TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
             PV_INTERACTIVE TYPE CHAR01
             PR_SENDER     TYPE REF TO CL_GUI_ALV_GRID.

  CASE PR_SENDER.
    WHEN GR_GRID1.

      CHECK GV_MODE <> 'S'.

      PERFORM ADD_BUTTON
        USING: PR_OBJECT '3' SPACE SPACE SPACE SPACE SPACE, "분리자

               PR_OBJECT '0' '&BT1' '' TEXT-C12 TEXT-C12 SPACE,

               PR_OBJECT '0' '&BT2' '' TEXT-C13 TEXT-C13 SPACE.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " EVENT_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form ADD_BUTTON
*&---------------------------------------------------------------------*
FORM ADD_BUTTON USING PR_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
                    PV_BTYPE
                    PV_FUNC
                    PV_ICON
                    PV_INFO
                    PV_TEXT
                    PV_DISA.

  DATA: LS_BUTTON TYPE STB_BUTTON,
        LS_BTNMNU TYPE STB_BTNMNU,

        LT_BUTTON TYPE TTB_BUTTON,
        LT_BTNMNU TYPE TTB_BTNMNU.

  CLEAR LS_BUTTON.
  LS_BUTTON-BUTN_TYPE = PV_BTYPE.
  LS_BUTTON-FUNCTION  = PV_FUNC.
  LS_BUTTON-ICON      = PV_ICON.
  LS_BUTTON-QUICKINFO = PV_INFO.

  LS_BUTTON-TEXT      = PV_TEXT.
  LS_BUTTON-DISABLED  = PV_DISA.

  APPEND LS_BUTTON TO PR_OBJECT->MT_TOOLBAR.

ENDFORM.                   " ADD_BUTTON
*&---------------------------------------------------------------------*
*&      Form  EVENT_USER_COMMAND
*&---------------------------------------------------------------------*
FORM EVENT_USER_COMMAND  USING PV_UCOMM   TYPE SY-UCOMM
                               PR_SENDER TYPE REF TO CL_GUI_ALV_GRID.

  DATA: LV_ROW TYPE I,
        LV_COL TYPE I.

  CLEAR: GT_ROWS,    GT_ROWS[].
  CLEAR: GT_COLUMNS, GT_COLUMNS[].

*  CALL METHOD PR_SENDER->GET_SELECTED_ROWS
*    IMPORTING
*      ET_INDEX_ROWS = GT_ROWS[].

  CALL METHOD PR_SENDER->GET_SELECTED_COLUMNS
    IMPORTING
      ET_INDEX_COLUMNS = GT_COLUMNS[].

  CASE PR_SENDER.

    WHEN GR_GRID1.

      CASE PV_UCOMM.

        WHEN '&BT1'.    "전체 O

          IF GT_COLUMNS[] IS INITIAL.
            MESSAGE S058 DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          LOOP AT GT_COLUMNS INTO GS_COLUMNS
                   WHERE FIELDNAME = 'KOKRS'
                      OR FIELDNAME = 'KSTAR'
                      OR FIELDNAME = 'KTEXT'.

            EXIT.
          ENDLOOP.

          IF SY-SUBRC = 0.
            MESSAGE S000 WITH TEXT-E03 DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          PERFORM SET_CHECK_VALUE USING 'O'.
          MESSAGE S000 WITH TEXT-S01.

          PERFORM REFRESH_GRID_0100.

        WHEN '&BT2'.   "전체 X

          IF GT_COLUMNS[] IS INITIAL.
            MESSAGE S058 DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          LOOP AT GT_COLUMNS INTO GS_COLUMNS
                   WHERE FIELDNAME = 'KOKRS'
                      OR FIELDNAME = 'KSTAR'
                      OR FIELDNAME = 'KTEXT'.

            EXIT.
          ENDLOOP.

          IF SY-SUBRC = 0.
            MESSAGE S000 WITH TEXT-E03 DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.


          PERFORM SET_CHECK_VALUE USING 'X'.
          MESSAGE S000 WITH TEXT-S01.
          PERFORM REFRESH_GRID_0100.

      ENDCASE.

    WHEN OTHERS.
  ENDCASE.


ENDFORM.                    " EVENT_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  EVENT_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM EVENT_DATA_CHANGED
       USING PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
             PV_ONF4          TYPE CHAR01
             PV_ONF4_BEFORE   TYPE CHAR01
             PV_ONF4_AFTER    TYPE CHAR01
             PV_UCOMM         TYPE SY-UCOMM
             PR_SENDER       TYPE REF TO CL_GUI_ALV_GRID.


* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_  : Reference Variables

*--- Begin or Example
  DATA: LS_MOD_CELLS TYPE LVC_S_MODI,
        LS_INS_CELLS TYPE LVC_S_MOCE,
        LS_DEL_CELLS TYPE LVC_S_MOCE.

*  DATA LS_DD07T  TYPE DD07T.
  DATA LV_CTEXT  TYPE ZECTEXT.
  DATA LV_KTEXT  TYPE KTEXT.
  DATA LV_KSTAR  TYPE KSTAR.
  DATA LV_KSTAR_CHECK TYPE KSTAR.


  DATA LV_TABIX TYPE SY-TABIX.

  ERROR_IN_DATA = SPACE.

  DEFINE _MODIFY_CELL.
    CALL METHOD pr_data_changed->modify_cell
      EXPORTING
        i_fieldname = &1
        i_row_id    = &2
        i_value     = &3.

  END-OF-DEFINITION.

  DEFINE _GET_CELL_VALUE.
    CALL METHOD pr_data_changed->get_cell_value
      EXPORTING
        i_fieldname = &1
        i_row_id    = &2
      IMPORTING
        e_value     = &3.
  END-OF-DEFINITION.

  DEFINE _ADD_PROTOCOL.
    CALL METHOD pr_data_changed->add_protocol_entry
      EXPORTING
        i_fieldname = &1
        i_row_id    = &2
        i_msgid     = 'ZCO01'
        i_msgty     = &3
        i_msgno     = &4
        i_msgv1     = &5
        i_msgv2     = &6
        i_msgv3     = &7
        i_msgv4     = &8.
  END-OF-DEFINITION.

  DEFINE _MODIFY_STYLE.
    CALL METHOD pr_data_changed->modify_style
      EXPORTING
        i_fieldname = &1
        i_row_id    = &2
        i_style     = &3.
  END-OF-DEFINITION.
*--- End of Example


ENDFORM.
.                    " EVENT_DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  EVENT_DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
FORM EVENT_DATA_CHANGED_FINISHED
       USING P_MODIFIED    TYPE CHAR01
             PT_GOOD_CELLS TYPE LVC_T_MODI
             PR_SENDER    TYPE REF TO CL_GUI_ALV_GRID.


* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_ : Reference Variables


ENDFORM.                    " EVENT_DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*&      Form  EVENT_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM EVENT_HOTSPOT_CLICK USING PS_ROW_ID    TYPE LVC_S_ROW
                               PS_COLUMN_ID TYPE LVC_S_COL
                               PS_ROW_NO    TYPE LVC_S_ROID
                               PR_SENDER   TYPE REF TO CL_GUI_ALV_GRID.


* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_ : Reference Variables

*
*  CASE PR_SENDER.
*    WHEN GR_GRID1.
*
*      CASE PS_COLUMN_ID-FIELDNAME.
*        WHEN 'CONNID'.
*
*      ENDCASE.
*
*    WHEN OTHERS.
*  ENDCASE.


ENDFORM.                    " EVENT_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Form  EVENT_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM EVENT_DOUBLE_CLICK  USING PS_ROW     TYPE LVC_S_ROW
                               PS_COLUMN  TYPE LVC_S_COL
                               PS_ROW_NO  TYPE LVC_S_ROID
                               PR_SENDER TYPE REF TO CL_GUI_ALV_GRID.


* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_ : Reference Variables


*  CASE PR_SENDER.
*    WHEN GR_GRID1.
*
*    WHEN OTHERS.
*  ENDCASE.

ENDFORM.                    " EVENT_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  EVENT_HELP_ON_F4
*&---------------------------------------------------------------------*
FORM EVENT_HELP_ON_F4
       USING PV_FIELDNAME   TYPE LVC_FNAME
             PV_FIELDVALUE  TYPE LVC_VALUE
             PS_ROW_NO      TYPE LVC_S_ROID
             PR_EVENT_DATA TYPE REF TO CL_ALV_EVENT_DATA
             PT_BAD_CELLS   TYPE LVC_T_MODI
             PV_DISPLAY     TYPE CHAR01
             PR_SENDER     TYPE REF TO CL_GUI_ALV_GRID.


  DATA :  IS_MODI TYPE LVC_S_MODI.
  FIELD-SYMBOLS <F4TAB> TYPE LVC_T_MODI.
  ASSIGN PR_EVENT_DATA->M_DATA->* TO <F4TAB>.

  DATA LV_KAGRU TYPE KAGRU.

  CASE PR_SENDER.

    WHEN GR_GRID1.

      CASE PV_FIELDNAME.

        WHEN 'KAGRU'.

          CALL FUNCTION 'K_GROUP_SELECT'
            EXPORTING
              FIELD_NAME    = 'KSTAR'
              KOKRS         = PA_KOKRS
              KTOPL         = GC_KTOPL
            IMPORTING
              SET_NAME      = LV_KAGRU
            EXCEPTIONS
              NO_SET_PICKED = 1
              OTHERS        = 2.

          IF SY-SUBRC <> 0.
            EXIT.
          ENDIF.

          IF PV_DISPLAY IS INITIAL AND LV_KAGRU IS NOT INITIAL.
            IS_MODI-ROW_ID    = PS_ROW_NO-ROW_ID.
            IS_MODI-FIELDNAME = PV_FIELDNAME.
            IS_MODI-VALUE     = LV_KAGRU.
            APPEND IS_MODI TO <F4TAB>.
          ENDIF.

          PR_EVENT_DATA->M_EVENT_HANDLED = 'X'.

      ENDCASE.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " EVENT_HELP_ON_F4
*&---------------------------------------------------------------------*
*&      Form  EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM EVENT_TOP_OF_PAGE USING PR_DD         TYPE REF TO CL_DD_DOCUMENT
                             PV_TABLE_INDEX TYPE SYINDEX
                             PR_SENDER     TYPE REF TO CL_GUI_ALV_GRID.

* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_ : Reference Variables


*  CASE PR_SENDER.
*    WHEN GR_GRID1.
*    WHEN OTHERS.
*  ENDCASE.
ENDFORM.                    " EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  EVENT_END_OF_LIST
*&---------------------------------------------------------------------*
FORM EVENT_END_OF_LIST USING PR_DD     TYPE REF TO CL_DD_DOCUMENT
                             PR_SENDER TYPE REF TO CL_GUI_ALV_GRID.

* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_ : Reference Variables


*  CASE PR_SENDER.
*    WHEN GR_GRID1.
*    WHEN OTHERS.
*  ENDCASE.


ENDFORM.                    " EVENT_END_OF_LIST
*&---------------------------------------------------------------------*
*&      Form  CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
FORM CREATE_INSTANCE_0100 .


*-- 2. full screen
  CREATE OBJECT GR_SPLITTER1
    EXPORTING
      ROWS    = 2
      COLUMNS = 1
      PARENT  = CL_GUI_SPLITTER_CONTAINER=>SCREEN0.

*== get container instance
*-- 1. top of page
  GR_PARENT_HTML = GR_SPLITTER1->GET_CONTAINER(
      ROW       = 1
      COLUMN    = 1 ).

  GR_DATA_CONTAINER = GR_SPLITTER1->GET_CONTAINER(
      ROW       = 2
      COLUMN    = 1 ).

  CALL METHOD GR_SPLITTER1->SET_ROW_HEIGHT
    EXPORTING
      ID     = 1
      HEIGHT = 5.

  CALL METHOD GR_SPLITTER1->SET_ROW_HEIGHT
    EXPORTING
      ID     = 2
      HEIGHT = 50.

  CREATE OBJECT GR_GRID1
    EXPORTING
      I_PARENT = GR_DATA_CONTAINER.

ENDFORM.                    " CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT_0100
*&---------------------------------------------------------------------*
FORM INIT_LAYOUT_0100.

  CLEAR GS_LAYOUT.

*  GS_LAYOUT-EDIT_MODE  = ABAP_TRUE.
  GS_LAYOUT-ZEBRA      = ABAP_TRUE.
  GS_LAYOUT-SEL_MODE   = SPACE.     "B:단일,C:복수,D:셀,A:행/열
  GS_LAYOUT-BOX_FNAME  = SPACE.
  GS_LAYOUT-NO_ROWMARK = SPACE.

ENDFORM.                    " INIT_LAYOUT_0100
*&---------------------------------------------------------------------*
*&      Form  SET_GRID_EXCLUDE_0100
*&---------------------------------------------------------------------*
FORM SET_GRID_EXCLUDE_0100 .

  DATA: LS_EXCLUDE LIKE LINE OF GT_EXCLUDE.
  REFRESH: GT_EXCLUDE.

  "-- DEFINE _SET_EX
  DEFINE _SET_EX.
    CLEAR: ls_exclude.
    ls_exclude = &1.
    APPEND ls_exclude TO gt_exclude.
  END-OF-DEFINITION.


  _SET_EX:
*   CL_GUI_ALV_GRID=>MC_FC_FIND,

    "-- begin 기능버튼활성화시 제외
    CL_GUI_ALV_GRID=>MC_FC_SORT_ASC,
    CL_GUI_ALV_GRID=>MC_FC_SORT_DSC,
    CL_GUI_ALV_GRID=>MC_MB_SUBTOT,
    CL_GUI_ALV_GRID=>MC_MB_SUM,
    "-- end

    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,


    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,
    CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW,

    CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
    CL_GUI_ALV_GRID=>MC_FC_CHECK,

*   CL_GUI_ALV_GRID=>MC_FC_DETAIL,
*   CL_GUI_ALV_GRID=>MC_FC_FILTER,
    CL_GUI_ALV_GRID=>MC_FC_GRAPH,
    CL_GUI_ALV_GRID=>MC_FC_HTML,
    CL_GUI_ALV_GRID=>MC_FC_INFO,
    CL_GUI_ALV_GRID=>MC_FC_REFRESH,

*   CL_GUI_ALV_GRID=>MC_FC_VIEWS,
*   CL_GUI_ALV_GRID=>MC_FC_LOAD_VARIANT,
*   CL_GUI_ALV_GRID=>MC_FC_PRINT,
*   CL_GUI_ALV_GRID=>MC_MB_VARIANT,
*   CL_GUI_ALV_GRID=>MC_MB_EXPORT,

    CL_GUI_ALV_GRID=>MC_FC_VIEW_CRYSTAL,
    CL_GUI_ALV_GRID=>MC_FC_VIEW_EXCEL,
    CL_GUI_ALV_GRID=>MC_FC_VIEW_GRID,
    CL_GUI_ALV_GRID=>MC_FC_VIEW_LOTUS,
    CL_GUI_ALV_GRID=>MC_FC_EXPCRDATA,
    CL_GUI_ALV_GRID=>MC_FC_EXPCRDESIG,
    CL_GUI_ALV_GRID=>MC_FC_EXPCRTEMPL,
    CL_GUI_ALV_GRID=>MC_FC_CALL_ABC,
    CL_GUI_ALV_GRID=>MC_FC_CALL_CRBATCH.

ENDFORM. " SET_GRID_EXCLUDE_0100
*&---------------------------------------------------------------------*
*&      Form  ALV_SORT_0100
*&---------------------------------------------------------------------*
FORM ALV_SORT_0100 .

  CLEAR: GS_SORT, GT_SORT.
  REFRESH: GT_SORT.

ENDFORM.                    " ALV_SORT_0100
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDCAT_0100
*&---------------------------------------------------------------------*
FORM APPEND_FIELDCAT_0100 .

  PERFORM GET_FIELDCATLOG_DATA.
  PERFORM MODIFY_FIELDCATLOG_DATA.

ENDFORM.                    " APPEND_FIELDCAT_0100
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM GET_FIELDCATLOG_DATA .

  DATA: LT_FIELDCAT TYPE KKBLO_T_FIELDCAT.

  CALL FUNCTION 'K_KKB_FIELDCAT_MERGE'
    EXPORTING
      I_CALLBACK_PROGRAM     = SY-REPID
      I_STRUCNAME            = 'ZCOS0240' "ABAP DIC. 정의된 STRUCTURE
      I_BYPASSING_BUFFER     = ABAP_TRUE
      I_INCLNAME             = SY-REPID
    CHANGING
      CT_FIELDCAT            = LT_FIELDCAT[]
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      OTHERS                 = 2.

  IF SY-SUBRC EQ 0.

    "-- Trasnfer LVC.
    CALL FUNCTION 'LVC_TRANSFER_FROM_KKBLO'
      EXPORTING
        IT_FIELDCAT_KKBLO = LT_FIELDCAT[]
      IMPORTING
        ET_FIELDCAT_LVC   = GT_FIELDCAT[]
      EXCEPTIONS
        IT_DATA_MISSING   = 1.
  ELSE.

    MESSAGE E020.

  ENDIF.

ENDFORM.                    " GET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM MODIFY_FIELDCATLOG_DATA .

  DATA:  LV_TEXT(50).

  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.

    CLEAR: LV_TEXT.

    CASE GS_FIELDCAT-FIELDNAME.

      WHEN 'KOKRS'.

      WHEN 'KSTAR'.
        LV_TEXT = TEXT-C01.
        GS_FIELDCAT-OUTPUTLEN = '10'.

      WHEN 'KTEXT'.
        LV_TEXT = TEXT-C02.
        GS_FIELDCAT-OUTPUTLEN = '30'.

      WHEN 'CCTR'.
        LV_TEXT = TEXT-C03.
        GS_FIELDCAT-OUTPUTLEN = '10'.
        GS_FIELDCAT-EDIT      = ABAP_TRUE.

      WHEN 'CCTR_SAL'.
        LV_TEXT = TEXT-C14.
        GS_FIELDCAT-OUTPUTLEN = '10'.
        GS_FIELDCAT-EDIT      = ABAP_TRUE.

      WHEN 'CCTR_PRD'.
        LV_TEXT = TEXT-C15.
        GS_FIELDCAT-OUTPUTLEN = '10'.
        GS_FIELDCAT-EDIT      = ABAP_TRUE.

      WHEN 'W01'.
        LV_TEXT = TEXT-C04 && '_' && GS_FIELDCAT-FIELDNAME.
        GS_FIELDCAT-OUTPUTLEN = '15'.
        GS_FIELDCAT-EDIT      = ABAP_TRUE.

      WHEN 'W02'.
        LV_TEXT = TEXT-C05 && '_' && GS_FIELDCAT-FIELDNAME.
        GS_FIELDCAT-OUTPUTLEN = '15'.
        GS_FIELDCAT-EDIT      = ABAP_TRUE.

      WHEN 'W03'.
        LV_TEXT = TEXT-C06 && '_' && GS_FIELDCAT-FIELDNAME.
        GS_FIELDCAT-OUTPUTLEN = '15'.
        GS_FIELDCAT-EDIT      = ABAP_TRUE.

      WHEN 'W04'.
        LV_TEXT = TEXT-C07 && '_' && GS_FIELDCAT-FIELDNAME.
        GS_FIELDCAT-OUTPUTLEN = '15'.
        GS_FIELDCAT-EDIT      = ABAP_TRUE.

      WHEN 'W05'.
        LV_TEXT = TEXT-C08 && '_' && GS_FIELDCAT-FIELDNAME.
        GS_FIELDCAT-OUTPUTLEN = '15'.
        GS_FIELDCAT-EDIT      = ABAP_TRUE.

      WHEN 'W06'.
        LV_TEXT = TEXT-C11 && '_' && GS_FIELDCAT-FIELDNAME.
        GS_FIELDCAT-OUTPUTLEN = '15'.
        GS_FIELDCAT-EDIT      = ABAP_TRUE.

      WHEN 'W07'.
        LV_TEXT = TEXT-C09 && '_' && GS_FIELDCAT-FIELDNAME.
        GS_FIELDCAT-OUTPUTLEN = '15'.
        GS_FIELDCAT-EDIT      = ABAP_TRUE.

      WHEN 'MESSAGE'.
        LV_TEXT = TEXT-C10.
        GS_FIELDCAT-OUTPUTLEN = '10'.

      WHEN OTHERS.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.

    ENDCASE.

    "-- Common attribute
    IF LV_TEXT IS NOT INITIAL.
      GS_FIELDCAT-COLTEXT   = LV_TEXT.
      GS_FIELDCAT-SCRTEXT_L = LV_TEXT.
      GS_FIELDCAT-SCRTEXT_M = LV_TEXT.
      GS_FIELDCAT-SCRTEXT_S = LV_TEXT.
    ENDIF.

    MODIFY GT_FIELDCAT FROM GS_FIELDCAT.

  ENDLOOP.

ENDFORM.                    " MODIFY_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
*&      Form  fill_field_category
*&---------------------------------------------------------------------*
FORM FILL_FIELD_CATEGORY USING PV_GUB PV_FNAME PV_CON.

  IF PV_GUB = 'S'.
    CLEAR GS_FIELDCAT.
  ENDIF.

* 속성 MOVE
  DATA LV_COL(40).
  FIELD-SYMBOLS <FS>.
  CONCATENATE 'GS_FIELDCAT-' PV_FNAME  INTO LV_COL.
  ASSIGN      (LV_COL)       TO        <FS>.
  MOVE         PV_CON        TO        <FS>.

  IF PV_GUB = 'E'.
    APPEND GS_FIELDCAT TO GT_FIELDCAT.
  ENDIF.
ENDFORM. " fill_field_category
*&---------------------------------------------------------------------*
*&      Form  REGIST_ALV_EVENT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM REGIST_ALV_EVENT_0100 USING PR_GRID TYPE REF TO CL_GUI_ALV_GRID.

  CASE GV_MODE.

    WHEN 'S'.

      CALL METHOD PR_GRID->SET_READY_FOR_INPUT
        EXPORTING
          I_READY_FOR_INPUT = 0.

    WHEN OTHERS.

      CALL METHOD PR_GRID->REGISTER_EDIT_EVENT
        EXPORTING
          I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

      CALL METHOD PR_GRID->SET_READY_FOR_INPUT
        EXPORTING
          I_READY_FOR_INPUT = 1.

  ENDCASE.

*-- GR_EVENT_RECEIVER
  IF GR_EVENT_RECEIVER IS INITIAL.
    CREATE OBJECT GR_EVENT_RECEIVER.
  ENDIF.

* Handler Event
  SET HANDLER:
    GR_EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALL INSTANCES,
    GR_EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALL INSTANCES,
    GR_EVENT_RECEIVER->HANDLE_DATA_CHANGED_FINISHED
      FOR ALL INSTANCES,
    GR_EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALL INSTANCES,
    GR_EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR ALL INSTANCES,
    GR_EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR ALL INSTANCES,
    GR_EVENT_RECEIVER->HANDLE_ONF4          FOR ALL INSTANCES.

ENDFORM.                    " REGIST_ALV_EVENT_0100
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_TITLE_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV_TITLE_0100 .

  DATA: LV_TITLE TYPE LVC_TITLE.

  LV_TITLE = TEXT-GT1.

  CALL METHOD GR_GRID1->SET_GRIDTITLE
    EXPORTING
      I_GRIDTITLE = LV_TITLE.

ENDFORM.                    " DISPLAY_ALV_TITLE_0100
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_GRID_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV_GRID_0100 .

  GS_VARIANT-REPORT = SY-REPID.

  GV_SAVE = 'A'.

  "*-- Build field catalog for the alv control
  CALL METHOD GR_GRID1->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_DEFAULT                     = ABAP_TRUE
      IS_LAYOUT                     = GS_LAYOUT
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = GV_SAVE
      IT_TOOLBAR_EXCLUDING          = GT_EXCLUDE
    CHANGING
      IT_FIELDCATALOG               = GT_FIELDCAT
      IT_SORT                       = GT_SORT
      IT_OUTTAB                     = GT_DISPLAY[]
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3.

  IF SY-SUBRC NE 0.
    MESSAGE E000(0K) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " DISPLAY_ALV_GRID_0100
*&---------------------------------------------------------------------*
*&      Form  REFRESH_GRID_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM REFRESH_GRID_0100 .

  GS_STABLE-ROW = ABAP_TRUE. "Row
  GS_STABLE-COL = ABAP_TRUE. "column

  CALL METHOD GR_GRID1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = GS_STABLE
      I_SOFT_REFRESH = ABAP_TRUE.

  CALL METHOD CL_GUI_CFW=>FLUSH.

ENDFORM.                    " REFRESH_GRID_0100
*&---------------------------------------------------------------------*
*&      Form  GET_GRID_CURSOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_GRID_CURSOR USING PR_SENDER TYPE REF TO CL_GUI_ALV_GRID
                   CHANGING PV_ROW
                            PV_COL.

  DATA: LV_ROW    TYPE I,
        LV_VALUE  TYPE C,
        LV_COL    TYPE I,
        LS_ROW_ID TYPE LVC_S_ROW,
        LS_COL_ID TYPE LVC_S_COL,
        LS_ROW_NO TYPE LVC_S_ROID.

  CLEAR: PV_ROW, PV_COL.

  CALL METHOD PR_SENDER->GET_CURRENT_CELL
    IMPORTING
      E_ROW     = LV_ROW
      E_VALUE   = LV_VALUE
      E_COL     = LV_COL
      ES_ROW_ID = LS_ROW_ID
      ES_COL_ID = LS_COL_ID
      ES_ROW_NO = LS_ROW_NO.

  " ROW RETURN
  PV_ROW = LV_ROW.

  " COL RETURN
  PV_COL = LV_COL.


ENDFORM.                    "GET_GRID_CURSOR
*&---------------------------------------------------------------------*
*&      Form  SET_GRID_CURSOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_GRID_CURSOR USING PR_SENDER TYPE REF TO CL_GUI_ALV_GRID
                           PV_ROW
                           PV_COL.

  DATA: LS_ROW_ID    TYPE LVC_S_ROW,
        LS_COLUMN_ID TYPE LVC_S_COL,
        LS_ROW_NO    TYPE LVC_S_ROID.

  IF PV_ROW IS NOT INITIAL AND PV_ROW > 0.
    LS_ROW_ID-INDEX = PV_ROW.
  ENDIF.

  IF PV_COL IS NOT INITIAL.
    LS_COLUMN_ID-FIELDNAME = PV_COL.
  ENDIF.

  CALL METHOD PR_SENDER->SET_CURRENT_CELL_VIA_ID
    EXPORTING
      IS_ROW_ID    = LS_ROW_ID
      IS_COLUMN_ID = LS_COLUMN_ID
      IS_ROW_NO    = LS_ROW_NO.

ENDFORM.                    " SET_GRID_CURSOR
*&---------------------------------------------------------------------*
*&      Form  CHECKED_SAVED_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CHECKED_SAVED_DATA .

  CALL METHOD GR_GRID1->CHECK_CHANGED_DATA( ).

  CLEAR GV_EXIT.

  LOOP AT GT_DISPLAY INTO GS_DISPLAY
                     WHERE CCTR IS INITIAL
                        OR W01  IS INITIAL
                        OR W02  IS INITIAL
                        OR W03  IS INITIAL
                        OR W04  IS INITIAL
                        OR W05  IS INITIAL
                        OR W07  IS INITIAL.
    EXIT.
  ENDLOOP.

  IF SY-SUBRC = 0.
    MESSAGE W000 WITH TEXT-E00
                      GS_DISPLAY-KSTAR
                      TEXT-E01.
  ENDIF.

ENDFORM.                    " CHECKED_SAVED_DATA
*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM POPUP_TO_CONFIRM USING PV_TITLE
                            PV_QUEST.

  "-- call popup
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR       = PV_TITLE                "TEXT-PT1
      TEXT_QUESTION  = PV_QUEST                "TEXT-QT1
    IMPORTING
      ANSWER         = GV_ANSWER
    EXCEPTIONS
      TEXT_NOT_FOUND = 1
      OTHERS         = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SAVE_DATA_RTN .

  DATA: LV_MESSAGE TYPE STRING.

  DATA: LT_ZCOT1170 TYPE TABLE OF ZCOT1170 WITH HEADER LINE.

  LOOP AT GT_DISPLAY INTO GS_DISPLAY.

    MOVE-CORRESPONDING GS_DISPLAY TO LT_ZCOT1170.

    LT_ZCOT1170-ERDAT  = SY-DATUM.
    LT_ZCOT1170-ERZET  = SY-UZEIT.
    LT_ZCOT1170-ERNAM  = SY-UNAME.
    LT_ZCOT1170-AEDAT  = SY-DATUM.
    LT_ZCOT1170-AEZET  = SY-UZEIT.
    LT_ZCOT1170-AENAM  = SY-UNAME.

    APPEND LT_ZCOT1170.
    CLEAR  LT_ZCOT1170.

  ENDLOOP.

  TRY .

      DELETE FROM ZCOT1170 WHERE KOKRS  = @PA_KOKRS
                                AND KSTAR  IN @SO_KSTAR.

      INSERT ZCOT1170 FROM TABLE LT_ZCOT1170.

      COMMIT WORK.

      MESSAGE S007.

    CATCH CX_SY_SQL_ERROR INTO DATA(LR_ERROR).

      ROLLBACK WORK.

      LV_MESSAGE = LR_ERROR->GET_TEXT( ).
      MESSAGE S001 WITH LV_MESSAGE DISPLAY LIKE 'E'.

  ENDTRY.

ENDFORM.                    " SAVE_DATA_RTN
*&---------------------------------------------------------------------*
*& Form CHECK_CONTROLLING_AREA
*&---------------------------------------------------------------------*
FORM CHECK_CONTROLLING_AREA .

  SELECT SINGLE BEZEI INTO @PA_KTXT
    FROM TKA01
   WHERE KOKRS = @PA_KOKRS.

  IF SY-SUBRC <> 0.
    SET CURSOR FIELD 'PA_KOKRS'.
    MESSAGE E027  WITH PA_KOKRS.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INITIAL_SET
*&---------------------------------------------------------------------*
FORM INITIAL_SET .

  PA_KOKRS = '1000'.

  SELECT SINGLE BEZEI INTO @PA_KTXT
    FROM TKA01
   WHERE KOKRS = @PA_KOKRS.

  CASE SY-TCODE.
    WHEN 'ZCOR0251'.
      GV_MODE = 'S'.
    WHEN OTHERS.
      GV_MODE = 'E'.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form TOP_OF_PAGE_CREATE_OBJECT_0100
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE_CREATE_OBJECT_0100 .

* Create TOP-Document
  CREATE OBJECT GR_TOP_DOCUMENT
    EXPORTING
      STYLE = 'ALV_GRID'.

* Initialize
  CALL METHOD GR_TOP_DOCUMENT->INITIALIZE_DOCUMENT( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_TOP_OF_PAGE_DATA_0100
*&---------------------------------------------------------------------*
FORM MAKE_TOP_OF_PAGE_DATA_0100 .

  DATA: LT_TEXTS TYPE SDYDO_TEXT_TABLE,
        LV_TEXT  TYPE SDYDO_TEXT_ELEMENT.

  CONCATENATE TEXT-001 ':' PA_KOKRS
        INTO LV_TEXT SEPARATED BY SPACE.

  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE.


  " Get Ready
  CALL METHOD GR_TOP_DOCUMENT->MERGE_DOCUMENT.

*" Display TOP document
  CALL METHOD GR_TOP_DOCUMENT->DISPLAY_DOCUMENT
    EXPORTING
      REUSE_CONTROL      = 'X'
      PARENT             = GR_PARENT_HTML
    EXCEPTIONS
      HTML_DISPLAY_ERROR = 1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_CHECK_VALUE
*&---------------------------------------------------------------------*
FORM SET_CHECK_VALUE  USING  VALUE(P_CHECK).

  LOOP AT GT_DISPLAY ASSIGNING FIELD-SYMBOL(<LS_DISPLAY>).

    LOOP AT GT_COLUMNS INTO GS_COLUMNS.

      ASSIGN COMPONENT GS_COLUMNS-FIELDNAME
          OF STRUCTURE <LS_DISPLAY> TO FIELD-SYMBOL(<FS_01>).
      MOVE P_CHECK TO <FS_01>.

    ENDLOOP.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCRFIELDS_FUNCTXT
*&---------------------------------------------------------------------*
FORM SCRFIELDS_FUNCTXT .
  GS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  GS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = GS_FUNTXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCR_USER_COMMAND
*&---------------------------------------------------------------------*
FORM SCR_USER_COMMAND .

  CASE SSCRFIELDS-UCOMM.
    WHEN 'FC01'.
      PERFORM CALL_POPUP_HELP(ZCAR9000) USING SY-REPID
                                              SY-DYNNR
                                              SY-LANGU ''.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.
