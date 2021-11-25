*&---------------------------------------------------------------------*
*& Include          ZCOR0050F01
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

  CLEAR GT_DISPLAY.
  CLEAR GT_DISPLAY_LOG.

  SELECT A~* , B~KTEXT, C~POST1
    INTO CORRESPONDING FIELDS OF TABLE @GT_DISPLAY
    FROM ZCOT0030 AS A
    LEFT JOIN CSKT AS B
      ON A~KOSTL = B~KOSTL
     AND B~SPRAS = @SY-LANGU
     AND B~DATBI = '99991231'
    LEFT JOIN PRPS AS C
      ON A~POSID = C~POSID
   WHERE A~KOKRS = @PA_KOKRS
     AND A~GJAHR = @PA_GJAHR.

  DATA(LV_LINES) = LINES( GT_DISPLAY ).

  MESSAGE S039 WITH LV_LINES.

  GT_DISPLAY_LOG[] = GT_DISPLAY[].

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

*      "ADD_BUTTON : OBJECT, BTYPE, FUNC, ICON, INFO, TEXT, DISABLE
*      PERFORM ADD_BUTTON
*        USING: PR_OBJECT '3' SPACE SPACE SPACE SPACE SPACE, "분리자
*
*               PR_OBJECT '0' '&APD' ICON_CREATE TEXT-BT1
*                 TEXT-BT1 SPACE,
*
*               PR_OBJECT '0' '&INS' ICON_INSERT_ROW TEXT-BT2
*                 TEXT-BT2 SPACE,
*
*               PR_OBJECT '0' '&DEL' ICON_DELETE_ROW TEXT-BT3
*                 TEXT-BT3 SPACE.

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

  CLEAR: GT_ROWS, GT_ROWS[].

  "선택 ROW가져오기
  CALL METHOD PR_SENDER->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROWS[].


  "GRID에 따라 처리.
  CASE PR_SENDER.
    WHEN GR_GRID1.
      CASE PV_UCOMM.
        WHEN '&APD'.   "-- 행 추가

*          " 마지막행에 ROW 추가하기.
*          "   -선언한 STRUCTURE로 초기값을 넣어 추가할 수도 있다.
*          CLEAR: GS_DISPLAY, GT_STYLE[], GS_STYLE.
*          GS_DISPLAY-UPDATE_FLAG = 'I'. "삽입.
*
*          " TEXT 필드 편집금지
*          _STYLE_DISABLED: 'CITYFROM', 'CITYTO'.
*          GS_DISPLAY-STYLE[] = GT_STYLE[].
*          APPEND GS_DISPLAY TO GT_DISPLAY.
*
*
*          " REFRESH
*          PERFORM REFRESH_GRID_0100.
*
*          "-- APPEND를 하면 제일 아래로 행이 추가된다.
*          "   CURSOR을 제일 밑으로 자동으로 이동.
*          "   * SORT가 되어 있는 곳에서는 주석으로 막아야 함.
*          SY-TFILL = LINES( GT_DISPLAY ).
*          PERFORM SET_GRID_CURSOR
*            USING PR_SENDER SY-TFILL SPACE.
*
*        WHEN '&INS'.   "-- 행 삽입
*          "밑에서 부터 ROW를 추가해야 함.
*          IF GT_ROWS[] IS NOT INITIAL.
*            SORT GT_ROWS BY INDEX DESCENDING.
*
*            CLEAR: GS_DISPLAY, GT_STYLE[], GS_STYLE.
*            GS_DISPLAY-UPDATE_FLAG = 'I'. "삽입.
*            LOOP AT GT_ROWS INTO GS_ROWS WHERE ROWTYPE IS INITIAL.
*              " TEXT 필드 편집금지
*              _STYLE_DISABLED: 'CITYFROM', 'CITYTO'.
*              GS_DISPLAY-STYLE[] = GT_STYLE[].
*
*              INSERT GS_DISPLAY INTO GT_DISPLAY INDEX GS_ROWS-INDEX.
*            ENDLOOP.
*
*
*            " REFRESH
*            PERFORM REFRESH_GRID_0100.
*
*            " 제일 마지막 ROW 바로 위 추가된 행에 커서 위치.
*            "   * SORT가 되어 있는 곳에서는 주석으로 막아야 함.
*            READ TABLE GT_ROWS INTO GS_ROWS INDEX 1.
*            SY-TFILL = GS_ROWS-INDEX + LINES( GT_ROWS ) - 1.
*            PERFORM SET_GRID_CURSOR
*              USING PR_SENDER SY-TFILL SPACE.
*
*          ELSE.
*            CLEAR: LV_ROW, LV_COL.
*
*            "선택된 ROW가 없으면 CURSOR의 위치를 기준으로 행추가
*            PERFORM GET_GRID_CURSOR USING PR_SENDER
*                                 CHANGING LV_ROW
*                                          LV_COL.
*
*            CLEAR: GS_DISPLAY, GT_STYLE[], GS_STYLE.
*            GS_DISPLAY-UPDATE_FLAG = 'I'. "삽입.
*
*            " TEXT 필드 편집금지
*            _STYLE_DISABLED: 'CITYFROM', 'CITYTO'.
*            GS_DISPLAY-STYLE[] = GT_STYLE[].
*
*            IF LINES( GT_DISPLAY[] ) IS NOT INITIAL.
*              INSERT GS_DISPLAY INTO GT_DISPLAY INDEX LV_ROW.
*            ENDIF.
*
*
*            " REFRESH
*            PERFORM REFRESH_GRID_0100.
*
*            "추가된 행에 커서 위치.
*            "   * SORT가 되어 있는 곳에서는 주석으로 막아야 함.
*            PERFORM SET_GRID_CURSOR
*              USING PR_SENDER LV_ROW SPACE.
*          ENDIF.
*
*        WHEN '&DEL'.   "-- 행 삭제
*
*          IF GT_ROWS[] IS INITIAL.
*            MESSAGE S021 DISPLAY LIKE 'E'.
*            RETURN.
*          ENDIF.
*
*          "밑에서 부터 지워야 한다.
*          SORT GT_ROWS BY INDEX DESCENDING.
*
*          LOOP AT GT_ROWS INTO GS_ROWS WHERE ROWTYPE IS INITIAL.
*
*            "삭제할 데이터를 ITAB에 따로 모은다.
*            "신규로 추가된 행은 제외.
*            READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX GS_ROWS-INDEX.
*            IF SY-SUBRC EQ 0 AND GS_DISPLAY-UPDATE_FLAG NE 'I'.
*              INSERT GS_DISPLAY INTO TABLE GT_DISPLAY_DEL.
*            ENDIF.
*
*            "실제 화면 DISPLAY에서 삭제.
*            DELETE GT_DISPLAY INDEX GS_ROWS-INDEX.
*          ENDLOOP.
*
*          PERFORM REFRESH_GRID_0100.

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

*--- Begin or Example
  DATA: LS_MOD_CELLS TYPE LVC_S_MODI,
        LS_INS_CELLS TYPE LVC_S_MOCE,
        LS_DEL_CELLS TYPE LVC_S_MOCE.

  DATA LV_POST1 TYPE PS_POST1.
  DATA LV_KTEXT TYPE KTEXT.
  DATA LV_POSID TYPE PS_POSID.
  DATA LV_KOSTL TYPE KOSTL.


  DATA LV_TABIX TYPE SY-TABIX.

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

  CLEAR GV_EXIT.

  CASE PR_SENDER.

    WHEN GR_GRID1.

      LOOP AT PR_DATA_CHANGED->MT_INSERTED_ROWS INTO LS_INS_CELLS.

        _MODIFY_CELL:   'GJAHR' LS_INS_CELLS-ROW_ID
                                PA_GJAHR,
                        'KOKRS' LS_INS_CELLS-ROW_ID
                                PA_KOKRS,
                        'BEXCL' LS_INS_CELLS-ROW_ID
                                ABAP_TRUE.

      ENDLOOP.

      LOOP AT PR_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELLS.

        CASE LS_MOD_CELLS-FIELDNAME.

          WHEN 'KOSTL'.

            IF LS_MOD_CELLS-VALUE IS NOT INITIAL.

              LV_KOSTL = LS_MOD_CELLS-VALUE.

              _CONVERSION_IN LV_KOSTL.

              SELECT SINGLE B~KTEXT INTO @LV_KTEXT
                FROM CSKS AS A
                LEFT JOIN CSKT AS B
                 ON A~KOKRS = B~KOKRS
                AND A~KOSTL = B~KOSTL
                AND A~DATBI = B~DATBI
                AND B~SPRAS = @SY-LANGU
              WHERE A~KOKRS = @PA_KOKRS
                AND A~KOSTL = @LV_KOSTL
                AND A~DATBI = '99991231'.

              IF SY-SUBRC <> 0.
                _ADD_PROTOCOL LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID
                              'E' 023 TEXT-C04 SPACE SPACE SPACE.
                GV_EXIT = ABAP_TRUE . EXIT.
              ENDIF.
            ENDIF.

            _MODIFY_CELL 'KTEXT' LS_MOD_CELLS-ROW_ID
                                 LV_KTEXT.

          WHEN 'POSID'.

            IF LS_MOD_CELLS-VALUE IS NOT INITIAL.

              CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
                EXPORTING
                  INPUT  = LS_MOD_CELLS-VALUE
                IMPORTING
                  OUTPUT = LV_POSID.

              SELECT SINGLE POST1 INTO @LV_POST1
                FROM PRPS
               WHERE POSID = @LV_POSID.

              IF SY-SUBRC <> 0.
                _ADD_PROTOCOL LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID
                              'E' 023 TEXT-C06 SPACE SPACE SPACE.
                GV_EXIT = ABAP_TRUE . EXIT.
              ENDIF.
            ENDIF.

            _MODIFY_CELL 'POST1' LS_MOD_CELLS-ROW_ID
                                 LV_POST1.
          WHEN OTHERS.

        ENDCASE.

      ENDLOOP.

  ENDCASE.

  CHECK GV_EXIT IS INITIAL.

  CALL METHOD PR_DATA_CHANGED->DISPLAY_PROTOCOL.

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


*  DATA :  IS_MODI TYPE LVC_S_MODI.
*  FIELD-SYMBOLS <F4TAB> TYPE LVC_T_MODI.
*  ASSIGN PR_EVENT_DATA->M_DATA->* TO <F4TAB>.
*
*  DATA LV_KAGRU TYPE KAGRU.

*  CASE PR_SENDER.
*
*    WHEN GR_GRID1.
*
*      CASE PV_FIELDNAME.
*
*        WHEN 'KAGRU'.
*
**          CALL FUNCTION 'K_GROUP_SELECT'
**            EXPORTING
**              FIELD_NAME    = 'KSTAR'
**              KOKRS         = PA_KOKRS
**              KTOPL         = GC_KTOPL
**            IMPORTING
**              SET_NAME      = LV_KAGRU
**            EXCEPTIONS
**              NO_SET_PICKED = 1
**              OTHERS        = 2.
**
**          IF SY-SUBRC <> 0.
**            EXIT.
**          ENDIF.
**
**          IF PV_DISPLAY IS INITIAL AND LV_KAGRU IS NOT INITIAL.
**            IS_MODI-ROW_ID    = PS_ROW_NO-ROW_ID.
**            IS_MODI-FIELDNAME = PV_FIELDNAME.
**            IS_MODI-VALUE     = LV_KAGRU.
**            APPEND IS_MODI TO <F4TAB>.
**          ENDIF.
**
**          PR_EVENT_DATA->M_EVENT_HANDLED = 'X'.
*
*      ENDCASE.
*
*    WHEN OTHERS.
*
*  ENDCASE.

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
*-- 1. customer container

*  CREATE OBJECT GR_CON1
*    EXPORTING
*      CONTAINER_NAME = GV_CONTAINER. "USER가 정의한 CONTAINER
*
*  CREATE OBJECT GR_GRID1
*    EXPORTING
*      I_PARENT = GR_CON1.

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
*  GS_LAYOUT-CWIDTH_OPT = ABAP_TRUE.
  GS_LAYOUT-SEL_MODE   = SPACE.     "B:단일,C:복수,D:셀,A:행/열
  GS_LAYOUT-BOX_FNAME  = SPACE.
  GS_LAYOUT-NO_ROWMARK = SPACE.

*  GS_LAYOUT-STYLEFNAME = 'STYLE'.
*  GS_LAYOUT-CTAB_FNAME = 'COLOR'.
*  GS_LAYOUT-INFO_FNAME = 'INFO'.

**  "alv title
**  GS_LAYOUT-GRID_TITLE = TEXT-GT1.

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

*
*  _SET_EX:
**   CL_GUI_ALV_GRID=>MC_FC_FIND,
*
*    "-- begin 기능버튼활성화시 제외
*    CL_GUI_ALV_GRID=>MC_FC_SORT_ASC,
*    CL_GUI_ALV_GRID=>MC_FC_SORT_DSC,
*    CL_GUI_ALV_GRID=>MC_MB_SUBTOT,
*    CL_GUI_ALV_GRID=>MC_MB_SUM,
*    "-- end
*
**    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
**    CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
**    CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
**    CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
**    CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,
*
*    "-- begin 기능버튼활성화
**    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,
**    CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
**    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,
**    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW,
*    "-- end
*
**    CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
**    CL_GUI_ALV_GRID=>MC_FC_CHECK,
**
***   CL_GUI_ALV_GRID=>MC_FC_DETAIL,
***   CL_GUI_ALV_GRID=>MC_FC_FILTER,
**    CL_GUI_ALV_GRID=>MC_FC_GRAPH,
**    CL_GUI_ALV_GRID=>MC_FC_HTML,
**    CL_GUI_ALV_GRID=>MC_FC_INFO,
**    CL_GUI_ALV_GRID=>MC_FC_REFRESH,
**
***   CL_GUI_ALV_GRID=>MC_FC_VIEWS,
***   CL_GUI_ALV_GRID=>MC_FC_LOAD_VARIANT,
***   CL_GUI_ALV_GRID=>MC_FC_PRINT,
***   CL_GUI_ALV_GRID=>MC_MB_VARIANT,
***   CL_GUI_ALV_GRID=>MC_MB_EXPORT,
**
**    CL_GUI_ALV_GRID=>MC_FC_VIEW_CRYSTAL,
**    CL_GUI_ALV_GRID=>MC_FC_VIEW_EXCEL,
**    CL_GUI_ALV_GRID=>MC_FC_VIEW_GRID,
**    CL_GUI_ALV_GRID=>MC_FC_VIEW_LOTUS,
**    CL_GUI_ALV_GRID=>MC_FC_EXPCRDATA,
**    CL_GUI_ALV_GRID=>MC_FC_EXPCRDESIG,
**    CL_GUI_ALV_GRID=>MC_FC_EXPCRTEMPL,
**    CL_GUI_ALV_GRID=>MC_FC_CALL_ABC,
**    CL_GUI_ALV_GRID=>MC_FC_CALL_CRBATCH.

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

  "-- field catalog data
  "   field catalog merge or set fieldcatalog를 사용할 수 있음.

  "{ FIELDCATLOG MERGE 사용
  PERFORM GET_FIELDCATLOG_DATA.

  PERFORM MODIFY_FIELDCATLOG_DATA.
  "}

  "{ SET FIELDCATLOG 사용
*  PERFORM SET_FIELDCATLOG_DATA.
  "}

ENDFORM.                    " APPEND_FIELDCAT_0100
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM GET_FIELDCATLOG_DATA .

  DATA: LT_FIELDCAT TYPE KKBLO_T_FIELDCAT.

  CALL FUNCTION 'K_KKB_FIELDCAT_MERGE'
    EXPORTING
      I_CALLBACK_PROGRAM     = SY-REPID
      I_STRUCNAME            = 'ZCOS0030' "ABAP DIC. 정의된 STRUCTURE
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

      WHEN 'GJAHR'.
        LV_TEXT = TEXT-C01.
        GS_FIELDCAT-OUTPUTLEN = '8'.

      WHEN 'KOKRS'.
        LV_TEXT = TEXT-C02.
        GS_FIELDCAT-OUTPUTLEN = '8'.

      WHEN 'KOSTL'.
        LV_TEXT = TEXT-C04.
        GS_FIELDCAT-OUTPUTLEN = '10'.
        GS_FIELDCAT-EDIT = ABAP_TRUE.
        GS_FIELDCAT-F4AVAILABL = ABAP_TRUE.

      WHEN 'KTEXT'.
        LV_TEXT = TEXT-C05.
        GS_FIELDCAT-OUTPUTLEN = '20'.

      WHEN 'POSID'.
        LV_TEXT = TEXT-C06.
        GS_FIELDCAT-EDIT = ABAP_TRUE.
        GS_FIELDCAT-OUTPUTLEN = '15'.
        GS_FIELDCAT-F4AVAILABL = ABAP_TRUE.

      WHEN 'POST1'.
        LV_TEXT = TEXT-C07.
        GS_FIELDCAT-OUTPUTLEN = '30'.

      WHEN 'BEXCL'.
        LV_TEXT = TEXT-C08.
        GS_FIELDCAT-EDIT   = ABAP_TRUE.
        GS_FIELDCAT-CHECKBOX  = ABAP_TRUE.
        GS_FIELDCAT-OUTPUTLEN = '12'.

      WHEN OTHERS.

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
*&      Form  SET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM SET_FIELDCATLOG_DATA.

  CLEAR GT_FIELDCAT[].

  PERFORM FILL_FIELD_CATEGORY USING :
        'S' 'FIELDNAME'   'ICON',
        ' ' 'OUTPUTLEN'   '4',
        ' ' 'EDIT'        'X',
        'E' 'COLTEXT'     'ID',

        'S' 'FIELDNAME'   'CARRID',
        ' ' 'OUTPUTLEN'   '3',
        ' ' 'EDIT'        'X',
        'E' 'COLTEXT'     '항공사 ID',

        'S' 'FIELDNAME'   'CONNID',
        ' ' 'OUTPUTLEN'   '4',
        ' ' 'EDIT'        'X',
        'E' 'COLTEXT'     '운항연결 ID',

        'S' 'FIELDNAME'   'FLDATE',
        ' ' 'OUTPUTLEN'   '10',
        ' ' 'EDIT'        'X',
        'E' 'COLTEXT'     '운항일',

        'S' 'FIELDNAME'   'PRICE',
        ' ' 'OUTPUTLEN'   '20',
        ' ' 'EDIT'        'X',
        ' ' 'CFIELDNAME'  'CURRENCY',
        'E' 'COLTEXT'     '가격',

        'S' 'FIELDNAME'   'CURRENCY',
        ' ' 'OUTPUTLEN'   '5',
        ' ' 'EDIT'        'X',
        'E' 'COLTEXT'     '현지통화',

        'S' 'FIELDNAME'   'PLANETYPE',
        ' ' 'OUTPUTLEN'   '10',
        ' ' 'EDIT'        'X',
        'E' 'COLTEXT'     '항공기',

        'S' 'FIELDNAME'   'SEATSMAX',
        ' ' 'OUTPUTLEN'   '10',
        ' ' 'EDIT'        'X',
        'E' 'COLTEXT'     '최대용량',

        'S' 'FIELDNAME'   'SEATSOCC',
        ' ' 'OUTPUTLEN'   '10',
        ' ' 'EDIT'        'X',
        'E' 'COLTEXT'     '점유좌석',

        'S' 'FIELDNAME'   'PAYMENTSUM',
        ' ' 'OUTPUTLEN'   '22',
        ' ' 'EDIT'        'X',
        ' ' 'CFIELDNAME'  'CURRENCY',
        'E' 'COLTEXT'     '예약 총계',

        'S' 'FIELDNAME'   'SEATSMAX_B',
        ' ' 'OUTPUTLEN'   '10',
        ' ' 'EDIT'        'X',
        'E' 'COLTEXT'     'MAX. Business Class',

        'S' 'FIELDNAME'   'SEATSOCC_B',
        ' ' 'OUTPUTLEN'   '10',
        ' ' 'EDIT'        'X',
        'E' 'COLTEXT'     'OCC. Business Class',

        'S' 'FIELDNAME'   'SEATSMAX_F',
        ' ' 'OUTPUTLEN'   '10',
        ' ' 'EDIT'        'X',
        'E' 'COLTEXT'     'MAX. First Class',

        'S' 'FIELDNAME'   'SEATSOCC_F',
        ' ' 'OUTPUTLEN'   '10',
        ' ' 'EDIT'        'X',
        'E' 'COLTEXT'     'OCC. First Class'.

ENDFORM.                    " SET_FIELDCATLOG_DATA
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

    WHEN 'E'.

* REGISTER EVENT
      CALL METHOD PR_GRID->REGISTER_EDIT_EVENT
        EXPORTING
          I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
*
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

  PERFORM SET_F4 USING PR_GRID.

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
      I_SOFT_REFRESH = SPACE.

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

  DATA: BEGIN OF LS_KEY,
          GJAHR TYPE GJAHR,
          KOKRS TYPE KOKRS,
          KOSTL TYPE KOSTL,
          POSID TYPE PS_POSID,
        END OF LS_KEY,
        LT_KEY LIKE SORTED TABLE OF LS_KEY
                    WITH UNIQUE KEY GJAHR KOKRS KOSTL POSID.

  CALL METHOD GR_GRID1->CHECK_CHANGED_DATA( ).

  CLEAR GV_EXIT.

  "-- 중복키 CHECK LOGIC.
  LOOP AT GT_DISPLAY INTO GS_DISPLAY.
    MOVE-CORRESPONDING GS_DISPLAY TO LS_KEY.
    INSERT LS_KEY INTO TABLE LT_KEY.
    IF SY-SUBRC NE 0.
      GV_EXIT = ABAP_TRUE.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF GV_EXIT EQ ABAP_TRUE.
    MESSAGE S017 WITH LS_KEY-GJAHR LS_KEY-KOKRS LS_KEY-KOSTL
                      LS_KEY-POSID
      DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT GT_DISPLAY INTO GS_DISPLAY.

    IF GS_DISPLAY-KOSTL IS NOT INITIAL AND
       GS_DISPLAY-POSID IS NOT INITIAL.

      MESSAGE S000 WITH TEXT-E02 DISPLAY LIKE 'E'.
      GV_EXIT = ABAP_TRUE.
      EXIT.

    ENDIF.
    IF GS_DISPLAY-KOSTL   IS  INITIAL AND
       GS_DISPLAY-POSID   IS  INITIAL.

      MESSAGE S000 WITH TEXT-E03 DISPLAY LIKE 'E'.
      GV_EXIT = ABAP_TRUE.
      EXIT.

    ENDIF.

  ENDLOOP.

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
*     DIAGNOSE_OBJECT             = ' '
      TEXT_QUESTION  = PV_QUEST                "TEXT-QT1
*     TEXT_BUTTON_1  = 'Ja'(001)
*     ICON_BUTTON_1  = ' '
*     TEXT_BUTTON_2  = 'Nein'(002)
*     ICON_BUTTON_2  = ' '
*     DEFAULT_BUTTON = '1'
*     DISPLAY_CANCEL_BUTTON       = 'X'
*     USERDEFINED_F1_HELP         = ' '
*     START_COLUMN   = 25
*     START_ROW      = 6
*     POPUP_TYPE     =
*     IV_QUICKINFO_BUTTON_1       = ' '
*     IV_QUICKINFO_BUTTON_2       = ' '
    IMPORTING
      ANSWER         = GV_ANSWER
*   TABLES
*     PARAMETER      =
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

  DATA: LT_ZCOT0030 TYPE TABLE OF ZCOT0030 WITH HEADER LINE.

  LOOP AT GT_DISPLAY INTO GS_DISPLAY.

    MOVE-CORRESPONDING GS_DISPLAY TO LT_ZCOT0030.

    LT_ZCOT0030-ERDAT = SY-DATUM.
    LT_ZCOT0030-ERZET = SY-UZEIT.
    LT_ZCOT0030-ERNAM = SY-UNAME.
    LT_ZCOT0030-AEDAT = SY-DATUM.
    LT_ZCOT0030-AEZET = SY-UZEIT.
    LT_ZCOT0030-AENAM = SY-UNAME.
    LT_ZCOT0030-KOKRS = PA_KOKRS.

    APPEND LT_ZCOT0030.
    CLEAR  LT_ZCOT0030.

  ENDLOOP.

  TRY .

      DELETE FROM ZCOT0030 WHERE KOKRS  = @PA_KOKRS
                             AND GJAHR  = @PA_GJAHR.

      INSERT ZCOT0030 FROM TABLE LT_ZCOT0030.

      COMMIT WORK.

      MESSAGE S007.

      CLEAR GT_DISPLAY_LOG.

      GT_DISPLAY_LOG[] = GT_DISPLAY[].

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
*& Form SET_F4
*&---------------------------------------------------------------------*
FORM SET_F4 USING PR_GRID TYPE REF TO CL_GUI_ALV_GRID.

  CLEAR : GS_F4, GT_F4, GT_F4[].
  GS_F4-FIELDNAME = 'KAGRU'.
  GS_F4-REGISTER  = 'X'.
  INSERT GS_F4 INTO TABLE GT_F4.

  CALL METHOD PR_GRID->REGISTER_F4_FOR_FIELDS
    EXPORTING
      IT_F4 = GT_F4.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INITIAL_SET
*&---------------------------------------------------------------------*
FORM INITIAL_SET .

  CASE SY-TCODE.
    WHEN 'ZCOR0051'.
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

  CONCATENATE TEXT-C01 ':' PA_GJAHR
        INTO LV_TEXT SEPARATED BY SPACE.

  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE
    EXPORTING
      REPEAT = 1.

*  CALL METHOD GR_TOP_DOCUMENT->ADD_GAP
*    EXPORTING
*      WIDTH = 20.

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
*& Form CHECK_CHANGE
*&---------------------------------------------------------------------*
FORM CHECK_CHANGE  CHANGING P_GV_VALID.

  IF GT_DISPLAY_LOG[] = GT_DISPLAY[].
    CLEAR P_GV_VALID.
  ELSE.
    P_GV_VALID = ABAP_TRUE.
  ENDIF.

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
