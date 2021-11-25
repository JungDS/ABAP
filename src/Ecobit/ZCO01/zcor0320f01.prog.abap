*&---------------------------------------------------------------------*
*& Include          ZCOR0320F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITAIL
*&---------------------------------------------------------------------*
FORM INITAIL .
  GV_REPID = SY-REPID.

  SELECT SINGLE BEZEI INTO @PA_KTXT
    FROM TKA01
   WHERE KOKRS = @PA_KOKRS.

  SET CURSOR FIELD 'PA_POSID'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
FORM SET_SCREEN .

  LOOP AT SCREEN.
    IF SCREEN-NAME = 'PA_KOKRS'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

  SELECT A~BNAME, C~NAME_TEXT, A~SEQ, A~KOKRS, A~PRCTR1, A~BUKRS,
         A~PSPID, A~PRCTR2, A~BUKRS2
    INTO TABLE @DATA(LT_TEMP)
    FROM ZCOT0320 AS A JOIN USR21 AS B
                         ON A~BNAME = B~BNAME
                       JOIN ADRP AS C
                         ON B~PERSNUMBER = C~PERSNUMBER
   WHERE A~BNAME IN @SO_BNAME
     AND A~KOKRS  = @PA_KOKRS.

  LOOP AT LT_TEMP ASSIGNING FIELD-SYMBOL(<LS_TEMP>).
    MOVE-CORRESPONDING <LS_TEMP> TO GS_DISPLAY.

    _ADD_STYLE 'BUKRS' CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    GS_DISPLAY-STYLE = GT_STYLE.

    _ADD_STYLE 'BUKRS2' CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    GS_DISPLAY-STYLE = GT_STYLE.

    _ADD_STYLE 'PRCTR1' CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    GS_DISPLAY-STYLE = GT_STYLE.

    _ADD_STYLE 'PRCTR2' CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    GS_DISPLAY-STYLE = GT_STYLE.

    _ADD_STYLE 'PSPID' CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    GS_DISPLAY-STYLE = GT_STYLE.

    APPEND GS_DISPLAY TO GT_DISPLAY.
    CLEAR: GS_DISPLAY, GT_STYLE, GT_STYLE[].
  ENDLOOP.

  SORT GT_DISPLAY BY BNAME SEQ.

  GT_DISPLAY_LOG[] = GT_DISPLAY[].

ENDFORM.
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

**-- 2. full screen
*  CREATE OBJECT GR_SPLITTER1
*    EXPORTING
*      ROWS    = 2
*      COLUMNS = 1
*      PARENT  = CL_GUI_SPLITTER_CONTAINER=>SCREEN0.
*
**== get container instance
**-- 1. top of page
*  GR_PARENT_HTML = GR_SPLITTER1->GET_CONTAINER(
*      ROW       = 1
*      COLUMN    = 1 ).
*
*  GR_DATA_CONTAINER = GR_SPLITTER1->GET_CONTAINER(
*      ROW       = 2
*      COLUMN    = 1 ).
*
*  CALL METHOD GR_SPLITTER1->SET_ROW_HEIGHT
*    EXPORTING
*      ID     = 1
*      HEIGHT = 7.
*
*  CALL METHOD GR_SPLITTER1->SET_ROW_HEIGHT
*    EXPORTING
*      ID     = 2
*      HEIGHT = 50.

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

  GS_LAYOUT-STYLEFNAME = 'STYLE'.
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


  _SET_EX:
*   CL_GUI_ALV_GRID=>MC_FC_FIND,
*   CL_GUI_ALV_GRID=>MC_FC_SORT_ASC,
*   CL_GUI_ALV_GRID=>MC_FC_SORT_DSC,
*   CL_GUI_ALV_GRID=>MC_MB_SUBTOT,
*   CL_GUI_ALV_GRID=>MC_MB_SUM,

    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
*    CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
*    CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,
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
      I_STRUCNAME            = 'ZCOS0320' "ABAP DIC. 정의된 STRUCTURE
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

  DATA: LV_TEXT(50).

  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.

    CLEAR: LV_TEXT.

    CASE GS_FIELDCAT-FIELDNAME.

      WHEN 'BNAME'.
        LV_TEXT                = TEXT-C01.
        GS_FIELDCAT-COL_POS    = 1.
*        GS_FIELDCAT-EMPHASIZE = 'C112'.
        GS_FIELDCAT-OUTPUTLEN  = 10.
        GS_FIELDCAT-F4AVAILABL = ABAP_TRUE.

      WHEN 'NAME_TEXT'.
        LV_TEXT               = TEXT-C02.
        GS_FIELDCAT-COL_POS   = 2.
*        GS_FIELDCAT-EMPHASIZE = 'C112'.
        GS_FIELDCAT-OUTPUTLEN = 20.

      WHEN 'SEQ'.
        LV_TEXT               = TEXT-C03.
        GS_FIELDCAT-COL_POS   = 3.
*        GS_FIELDCAT-EMPHASIZE = 'C112'.
        GS_FIELDCAT-OUTPUTLEN = 5.

      WHEN 'KOKRS'.
        LV_TEXT               = TEXT-C04.
        GS_FIELDCAT-COL_POS   = 4.
        GS_FIELDCAT-OUTPUTLEN = 10.

      WHEN 'PRCTR1'.
        LV_TEXT               = TEXT-C05.
        GS_FIELDCAT-COL_POS   = 5.
        GS_FIELDCAT-OUTPUTLEN = 20.

      WHEN 'BUKRS'.
        LV_TEXT               = TEXT-C06.
        GS_FIELDCAT-COL_POS   = 6.
        GS_FIELDCAT-OUTPUTLEN = 20.

      WHEN 'PSPID'.
        LV_TEXT               = TEXT-C07.
        GS_FIELDCAT-COL_POS   = 7.
        GS_FIELDCAT-OUTPUTLEN = 20.

      WHEN 'PRCTR2'.
        LV_TEXT               = TEXT-C08.
        GS_FIELDCAT-COL_POS   = 8.
        GS_FIELDCAT-OUTPUTLEN = 20.

      WHEN 'BUKRS2'.
        LV_TEXT               = TEXT-C10.
        GS_FIELDCAT-COL_POS   = 9.
        GS_FIELDCAT-OUTPUTLEN = 20.

      WHEN 'MESSAGE'.
        LV_TEXT               = TEXT-C09.
        GS_FIELDCAT-COL_POS   = 10.
        GS_FIELDCAT-OUTPUTLEN = 50.

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
        'S' 'FIELDNAME'   'GJAHR',
        ' ' 'OUTPUTLEN'   '4',
        'E' 'COLTEXT'     '적용연도',

        'S' 'FIELDNAME'   'CARRID',
        ' ' 'OUTPUTLEN'   '3',
        ' ' 'EDIT'        'X',
        'E' 'COLTEXT'     '항공사 ID'.

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
*
*  CONCATENATE TEXT-001 ':' PA_KOKRS
*        INTO LV_TEXT SEPARATED BY SPACE.
*
*  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
*    EXPORTING
*      TEXT         = LV_TEXT
*      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
*      SAP_EMPHASIS = CL_DD_AREA=>KEY
*      STYLE_CLASS  = SPACE.
*
*  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE.
*
*  CONCATENATE TEXT-004 ':' PA_GJAHR
*        INTO LV_TEXT SEPARATED BY SPACE.
*
*  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
*    EXPORTING
*      TEXT         = LV_TEXT
*      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
*      SAP_EMPHASIS = CL_DD_AREA=>KEY
*      STYLE_CLASS  = SPACE.
*
*  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE.
*
*  CONCATENATE TEXT-005 ':' PA_VERSN
*        INTO LV_TEXT SEPARATED BY SPACE.
*
*  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
*    EXPORTING
*      TEXT         = LV_TEXT
*      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
*      SAP_EMPHASIS = CL_DD_AREA=>KEY
*      STYLE_CLASS  = SPACE.
*
*  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE
*    EXPORTING
*      REPEAT = 1.

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
*&      Form  REGIST_ALV_EVENT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM REGIST_ALV_EVENT_0100 USING PR_GRID TYPE REF TO CL_GUI_ALV_GRID.

* REGISTER EVENT
  CALL METHOD PR_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
*
  CALL METHOD PR_GRID->SET_READY_FOR_INPUT
    EXPORTING
      I_READY_FOR_INPUT = 1.

*-- GR_EVENT_RECEIVER
  IF GR_EVENT_RECEIVER IS INITIAL.
    CREATE OBJECT GR_EVENT_RECEIVER.
  ENDIF.

* Handler Event
  SET HANDLER:
*    GR_EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALL INSTANCES,
    GR_EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR ALL INSTANCES,
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
*&      Form  EVENT_DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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

*--- BEGIN OR EXAMPLE
  DATA: LS_MOD_CELLS TYPE LVC_S_MODI,
        LS_INS_CELLS TYPE LVC_S_MOCE,
        LS_DEL_CELLS TYPE LVC_S_MOCE.

  DATA: LT_ITAB LIKE TABLE OF GS_DISPLAY,
        LS_ITAB LIKE GS_DISPLAY.

  DATA: LV_PSPID LIKE PROJ-PSPID,
        LV_BUKRS LIKE T001-BUKRS.

  DEFINE _MODIFY_CELL.
    CALL METHOD PR_DATA_CHANGED->MODIFY_CELL
      EXPORTING
        I_FIELDNAME = &1
        I_ROW_ID    = &2
        I_VALUE     = &3.
  END-OF-DEFINITION.

  DEFINE _GET_CELL_VALUE.
    CALL METHOD PR_DATA_CHANGED->GET_CELL_VALUE
      EXPORTING
        I_FIELDNAME = &1
        I_ROW_ID    = &2
      IMPORTING
        E_VALUE     = &3.
  END-OF-DEFINITION.

  DEFINE _ADD_PROTOCOL.
    CALL METHOD PR_DATA_CHANGED->ADD_PROTOCOL_ENTRY
      EXPORTING
        I_FIELDNAME = &1
        I_ROW_ID    = &2
        I_MSGID     = 'ZCO01'
        I_MSGTY     = &3
        I_MSGNO     = &4
        I_MSGV1     = &5
        I_MSGV2     = &6
        I_MSGV3     = &7
        I_MSGV4     = &8.
  END-OF-DEFINITION.

  DEFINE _MODIFY_STYLE.
    CALL METHOD PR_DATA_CHANGED->MODIFY_STYLE
      EXPORTING
        I_FIELDNAME = &1
        I_ROW_ID    = &2
        I_STYLE     = &3.
  END-OF-DEFINITION.

*--- End of Example


  CASE PR_SENDER.
    WHEN GR_GRID1.

*     Insert Row
      LOOP AT PR_DATA_CHANGED->MT_INSERTED_ROWS INTO LS_INS_CELLS.

        _MODIFY_CELL: 'KOKRS' LS_INS_CELLS-ROW_ID PA_KOKRS.

        _MODIFY_STYLE: 'BNAME'  LS_INS_CELLS-ROW_ID CL_GUI_ALV_GRID=>MC_STYLE_ENABLED,
                       'PRCTR1' LS_INS_CELLS-ROW_ID CL_GUI_ALV_GRID=>MC_STYLE_ENABLED,
                       'BUKRS'  LS_INS_CELLS-ROW_ID CL_GUI_ALV_GRID=>MC_STYLE_ENABLED,
                       'PSPID'  LS_INS_CELLS-ROW_ID CL_GUI_ALV_GRID=>MC_STYLE_ENABLED,
                       'PRCTR2' LS_INS_CELLS-ROW_ID CL_GUI_ALV_GRID=>MC_STYLE_ENABLED,
                       'BUKRS2' LS_INS_CELLS-ROW_ID CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.

      ENDLOOP.


*     Delete Row
      LOOP AT PR_DATA_CHANGED->MT_DELETED_ROWS INTO LS_DEL_CELLS.

        READ TABLE GT_DISPLAY INDEX LS_DEL_CELLS-ROW_ID ASSIGNING FIELD-SYMBOL(<FS_DEL>).

        IF <FS_DEL>-SEQ IS NOT INITIAL.
          APPEND <FS_DEL> TO GT_DISPLAY_DEL.
        ENDIF.

      ENDLOOP.


*     Change Cell
      LOOP AT PR_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELLS.
        CASE LS_MOD_CELLS-FIELDNAME.
          WHEN 'BNAME'.

            IF LS_MOD_CELLS-VALUE IS NOT INITIAL.

              SELECT SINGLE A~BNAME, B~NAME_TEXT INTO @DATA(LS_BNAME)
                FROM USR21 AS A JOIN ADRP AS B
                                  ON A~PERSNUMBER = B~PERSNUMBER
               WHERE A~BNAME = @LS_MOD_CELLS-VALUE.
              IF SY-SUBRC EQ 0.
                _MODIFY_CELL: 'NAME_TEXT' LS_MOD_CELLS-ROW_ID LS_BNAME-NAME_TEXT.
              ELSE.
                _ADD_PROTOCOL LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID
                              'E' 047 LS_MOD_CELLS-VALUE SPACE SPACE SPACE.
                ERROR_IN_DATA = ABAP_TRUE . EXIT.
              ENDIF.

            ELSE.

              _MODIFY_CELL: 'NAME_TEXT' LS_MOD_CELLS-ROW_ID SPACE.

            ENDIF.

          WHEN 'PRCTR1' OR 'PRCTR2'.

            IF LS_MOD_CELLS-VALUE IS NOT INITIAL.

              SELECT SINGLE PRCTR INTO @DATA(LV_PRCTR)
                FROM CEPC
               WHERE PRCTR EQ @LS_MOD_CELLS-VALUE.
              IF SY-SUBRC NE 0.
                _ADD_PROTOCOL LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID
                               'E' 047 LS_MOD_CELLS-VALUE SPACE SPACE SPACE.
                ERROR_IN_DATA = ABAP_TRUE . EXIT.
              ENDIF.

            ENDIF.

          WHEN 'BUKRS'.

            IF LS_MOD_CELLS-VALUE IS NOT INITIAL.

              SELECT SINGLE BUKRS INTO @LV_BUKRS
                FROM T001
               WHERE BUKRS EQ @LS_MOD_CELLS-VALUE.
              IF SY-SUBRC NE 0.
                _ADD_PROTOCOL LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID
                               'E' 047 LS_MOD_CELLS-VALUE SPACE SPACE SPACE.
                ERROR_IN_DATA = ABAP_TRUE . EXIT.
              ENDIF.

            ENDIF.

          WHEN 'PSPID'.

            IF LS_MOD_CELLS-VALUE IS NOT INITIAL.

              LV_PSPID = LS_MOD_CELLS-VALUE.

              _CONVERSION_ABPSN_INPUT LV_PSPID.

              SELECT SINGLE PSPNR INTO @DATA(LV_PSPNR)
                FROM PROJ
               WHERE PSPID EQ @LV_PSPID.
              IF SY-SUBRC NE 0.
                _ADD_PROTOCOL LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID
                               'E' 047 LS_MOD_CELLS-VALUE SPACE SPACE SPACE.
                ERROR_IN_DATA = ABAP_TRUE . EXIT.
              ENDIF.

            ENDIF.

          WHEN 'BUKRS2'.

            IF LS_MOD_CELLS-VALUE IS NOT INITIAL.

              SELECT SINGLE BUKRS INTO @LV_BUKRS
                FROM T001
               WHERE BUKRS EQ @LS_MOD_CELLS-VALUE.
              IF SY-SUBRC NE 0.
                _ADD_PROTOCOL LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID
                               'E' 047 LS_MOD_CELLS-VALUE SPACE SPACE SPACE.
                ERROR_IN_DATA = ABAP_TRUE . EXIT.
              ENDIF.

            ENDIF.

        ENDCASE.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

  IF ERROR_IN_DATA IS NOT INITIAL.

    CALL METHOD PR_DATA_CHANGED->DISPLAY_PROTOCOL.
    CLEAR ERROR_IN_DATA.

  ENDIF.

ENDFORM.                    " EVENT_DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  EVENT_HELP_ON_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM EVENT_HELP_ON_F4
       USING PV_FIELDNAME   TYPE LVC_FNAME
             PV_FIELDVALUE  TYPE LVC_VALUE
             PS_ROW_NO      TYPE LVC_S_ROID
             PR_EVENT_DATA TYPE REF TO CL_ALV_EVENT_DATA
             PT_BAD_CELLS   TYPE LVC_T_MODI
             PV_DISPLAY     TYPE CHAR01
             PR_SENDER     TYPE REF TO CL_GUI_ALV_GRID.

* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_ : Reference Variables

  DATA: LT_RETURN TYPE DDSHRETVAL OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF LT_BNAME OCCURS 0,
          BNAME     TYPE XUBNAME,
          NAME_TEXT TYPE AD_NAMTEXT,
        END OF LT_BNAME.

  DATA: LS_MODI TYPE LVC_S_MODI.
  FIELD-SYMBOLS <F4TAB> TYPE LVC_T_MODI.
  ASSIGN PR_EVENT_DATA->M_DATA->* TO <F4TAB>.

  CASE PR_SENDER.

    WHEN GR_GRID1.

      CASE PV_FIELDNAME.

        WHEN 'BNAME'.

          SELECT A~BNAME, B~NAME_TEXT INTO TABLE @LT_BNAME
            FROM USR21 AS A JOIN ADRP AS B
                              ON A~PERSNUMBER = B~PERSNUMBER.

          SORT LT_BNAME BY BNAME.

          CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
            EXPORTING
              RETFIELD        = 'BNAME'
              DYNPPROG        = SY-REPID
              DYNPNR          = SY-DYNNR
              WINDOW_TITLE    = TEXT-C02
              VALUE_ORG       = 'S'
            TABLES
              VALUE_TAB       = LT_BNAME
              RETURN_TAB      = LT_RETURN
            EXCEPTIONS
              PARAMETER_ERROR = 1
              NO_VALUES_FOUND = 2
              OTHERS          = 3.

          IF LINES( LT_RETURN ) > 0.

            READ TABLE LT_RETURN INDEX 1.

            LS_MODI-ROW_ID    = PS_ROW_NO-ROW_ID.
            LS_MODI-FIELDNAME = PV_FIELDNAME.
            LS_MODI-VALUE     = LT_RETURN-FIELDVAL.
            APPEND LS_MODI TO <F4TAB>.

            READ TABLE LT_BNAME ASSIGNING FIELD-SYMBOL(<FS_BNAME>)
                                WITH KEY BNAME = LT_RETURN-FIELDVAL.

            LS_MODI-ROW_ID    = PS_ROW_NO-ROW_ID.
            LS_MODI-FIELDNAME = 'NAME_TEXT'.
            LS_MODI-VALUE     = <FS_BNAME>-NAME_TEXT.
            APPEND LS_MODI TO <F4TAB>.

          ENDIF.

          PR_EVENT_DATA->M_EVENT_HANDLED = 'X'.

      ENDCASE.

    WHEN OTHERS.

  ENDCASE.




ENDFORM.                    " EVENT_HELP_ON_F4
*&---------------------------------------------------------------------*
*& Form SET_F4
*&---------------------------------------------------------------------*
FORM SET_F4 USING PR_GRID TYPE REF TO CL_GUI_ALV_GRID.

  CLEAR : GS_F4, GT_F4, GT_F4[].
  GS_F4-FIELDNAME = 'BNAME'.
  GS_F4-REGISTER  = 'X'.
  INSERT GS_F4 INTO TABLE GT_F4.

  CALL METHOD PR_GRID->REGISTER_F4_FOR_FIELDS
    EXPORTING
      IT_F4 = GT_F4.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_VALIDATION_DATA
*&---------------------------------------------------------------------*
FORM CHECK_VALIDATION_DATA .
  CLEAR: GV_EXIT.

  DATA: LS_OLD LIKE GS_DISPLAY,
        LV_CNT TYPE I.

  SORT GT_DISPLAY BY BNAME  ASCENDING
                     PRCTR1 DESCENDING
                     BUKRS  DESCENDING
                     PSPID  DESCENDING
                     PRCTR2 DESCENDING
                     BUKRS2 DESCENDING
                     SEQ    ASCENDING.

* 중복 제거
  DELETE ADJACENT DUPLICATES FROM GT_DISPLAY
                        COMPARING BNAME PRCTR1 BUKRS PSPID PRCTR2 BUKRS2.

* 불필요한 데이터 제거
  DELETE GT_DISPLAY WHERE BNAME EQ SPACE
                       OR ( BNAME NE SPACE AND ( PRCTR1 EQ SPACE AND
                                                 BUKRS  EQ SPACE AND
                                                 PSPID  EQ SPACE AND
                                                 PRCTR2 EQ SPACE AND
                                                 BUKRS2 EQ SPACE ) ).

** 손익센터, 회사코드, 프로젝트는 중복 불가
*  LOOP AT GT_DISPLAY ASSIGNING FIELD-SYMBOL(<FS_TEMP>).
*    CLEAR <FS_TEMP>-MESSAGE.
*
*    IF LS_OLD-BNAME EQ <FS_TEMP>-BNAME .
*
*      IF <FS_TEMP>-PRCTR2 IS NOT INITIAL AND ( <FS_TEMP>-PRCTR1 IS INITIAL AND
*                                               <FS_TEMP>-BUKRS  IS INITIAL AND
*                                               <FS_TEMP>-PSPID  IS INITIAL ).
*
*        <FS_TEMP>-MESSAGE = TEXT-E02.
*        GV_EXIT = ABAP_TRUE.
*
*      ELSEIF LS_OLD-PRCTR1 IS NOT INITIAL AND <FS_TEMP>-PRCTR1 IS INITIAL OR
*             LS_OLD-BUKRS  IS NOT INITIAL AND <FS_TEMP>-BUKRS  IS INITIAL OR
*             LS_OLD-PSPID  IS NOT INITIAL AND <FS_TEMP>-PSPID  IS INITIAL.
*
*        <FS_TEMP>-MESSAGE = TEXT-E01.
*        GV_EXIT = ABAP_TRUE.
*      ENDIF.
*
*    ELSE.
*
*      IF <FS_TEMP>-PRCTR1 IS NOT INITIAL. ADD 1 TO LV_CNT. ENDIF.
*      IF <FS_TEMP>-BUKRS  IS NOT INITIAL. ADD 1 TO LV_CNT. ENDIF.
*      IF <FS_TEMP>-PSPID  IS NOT INITIAL. ADD 1 TO LV_CNT. ENDIF.
**      IF <FS_TEMP>-PRCTR2 IS NOT INITIAL. ADD 1 TO LV_CNT. ENDIF.
*
*      IF LV_CNT > 1.
*        <FS_TEMP>-MESSAGE = TEXT-E05.
*        GV_EXIT = ABAP_TRUE.
*      ENDIF.
*
*    ENDIF.
*
*    IF LS_OLD-BNAME NE <FS_TEMP>-BNAME.
*      MOVE-CORRESPONDING <FS_TEMP> TO LS_OLD.
*    ENDIF.
*
*    CLEAR LV_CNT.
*
*  ENDLOOP.

  DATA: LV_PRCTR1 TYPE C,
        LV_BUKRS  TYPE C,
        LV_PSPID  TYPE C,
        LV_PRCTR2 TYPE C,
        LV_BUKRS2 TYPE C.

* 손익센터, 회사코드, 프로젝트는 중복 불가
  LOOP AT GT_DISPLAY ASSIGNING FIELD-SYMBOL(<FS_TEMP>).

    AT NEW BNAME.
      CLEAR: LV_PRCTR1, LV_BUKRS, LV_PSPID, LV_PRCTR2, LV_BUKRS2.
    ENDAT.

    CLEAR <FS_TEMP>-MESSAGE.

    IF <FS_TEMP>-PRCTR2 IS NOT INITIAL AND ( <FS_TEMP>-PRCTR1 IS INITIAL AND
                                             <FS_TEMP>-BUKRS  IS INITIAL AND
                                             <FS_TEMP>-PSPID  IS INITIAL ).

      <FS_TEMP>-MESSAGE = TEXT-E02. "코스트센터(손익센터)는 손익센터, 회사코드, 프로젝트에 값이 존재해야 합니다.
*      GV_EXIT = ABAP_TRUE.

    ELSEIF <FS_TEMP>-BUKRS2 IS NOT INITIAL AND ( <FS_TEMP>-PRCTR1 IS INITIAL AND
                                                 <FS_TEMP>-BUKRS  IS INITIAL AND
                                                 <FS_TEMP>-PSPID  IS INITIAL ).

      <FS_TEMP>-MESSAGE = TEXT-E08. "코스트센터(회사코드)는 손익센터, 회사코드, 프로젝트에 값이 존재해야 합니다.
*      GV_EXIT = ABAP_TRUE.

    ENDIF.

    CLEAR LV_CNT.
    IF <FS_TEMP>-PRCTR1 IS NOT INITIAL. ADD 1 TO LV_CNT. MOVE ABAP_TRUE TO LV_PRCTR1. ENDIF.
    IF <FS_TEMP>-BUKRS  IS NOT INITIAL. ADD 1 TO LV_CNT. MOVE ABAP_TRUE TO LV_BUKRS.  ENDIF.
    IF <FS_TEMP>-PSPID  IS NOT INITIAL. ADD 1 TO LV_CNT. MOVE ABAP_TRUE TO LV_PSPID.  ENDIF.

    IF LV_CNT > 1.
      <FS_TEMP>-MESSAGE = TEXT-E05. "하나의 Row에 손익센터, 회사코드, 프로젝트는 중복 불가
      GV_EXIT = ABAP_TRUE.
    ENDIF.

    CLEAR LV_CNT.
    IF <FS_TEMP>-PRCTR2 IS NOT INITIAL. ADD 1 TO LV_CNT. MOVE ABAP_TRUE TO LV_PRCTR2. ENDIF.
    IF <FS_TEMP>-BUKRS2 IS NOT INITIAL. ADD 1 TO LV_CNT. MOVE ABAP_TRUE TO LV_BUKRS2. ENDIF.

    IF LV_CNT > 1.
      <FS_TEMP>-MESSAGE = TEXT-E06. "하나의 Row에 코스트센터(손익센터), 코스트센터(회사코드) 중복 불가
      GV_EXIT = ABAP_TRUE.
    ENDIF.

    IF LS_OLD-BNAME EQ <FS_TEMP>-BNAME.

      IF LV_PRCTR1 IS NOT INITIAL AND ( LV_BUKRS  IS NOT INITIAL OR LV_PSPID IS NOT INITIAL ) OR
         LV_BUKRS  IS NOT INITIAL AND ( LV_PRCTR1 IS NOT INITIAL OR LV_PSPID IS NOT INITIAL ) OR
         LV_PSPID  IS NOT INITIAL AND ( LV_PRCTR1 IS NOT INITIAL OR LV_BUKRS IS NOT INITIAL ).
        <FS_TEMP>-MESSAGE = TEXT-E01. "여러 Row에 손익센터, 회사코드, 프로젝트는 중복 불가
        GV_EXIT = ABAP_TRUE.
      ENDIF.

      IF LV_PRCTR2 IS NOT INITIAL AND LV_BUKRS2 IS NOT INITIAL.
        <FS_TEMP>-MESSAGE = TEXT-E07. "여러 Row에 코스트센터(손익센터), 코스트센터(회사코드) 중복 불가
        GV_EXIT = ABAP_TRUE.
      ENDIF.

    ENDIF.

    AT NEW BNAME.
      MOVE-CORRESPONDING <FS_TEMP> TO LS_OLD.
    ENDAT.

*    IF LS_OLD-BNAME NE <FS_TEMP>-BNAME.
*      MOVE-CORRESPONDING <FS_TEMP> TO LS_OLD.
*    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_DATA_RTN
*&---------------------------------------------------------------------*
FORM SAVE_DATA_RTN .

  DATA: LV_MESSAGE  TYPE STRING,
        LV_SEQ      TYPE SEQN3,
        LS_ZCOT0320 TYPE ZCOT0320,
        LV_FLAG     TYPE C.

  FIELD-SYMBOLS: <FS_STYLE> TYPE LVC_T_STYL.

  LOOP AT GT_DISPLAY_DEL ASSIGNING FIELD-SYMBOL(<FS_DEL>).

    TRY .

        DELETE FROM ZCOT0320 WHERE BNAME = <FS_DEL>-BNAME
                               AND SEQ   = <FS_DEL>-SEQ.

        COMMIT WORK.

      CATCH CX_SY_SQL_ERROR INTO DATA(LR_ERROR).

        ROLLBACK WORK.

        LV_MESSAGE = LR_ERROR->GET_TEXT( ).
        MESSAGE S001 WITH LV_MESSAGE DISPLAY LIKE 'E'.

    ENDTRY.

  ENDLOOP.


  LOOP AT GT_DISPLAY ASSIGNING FIELD-SYMBOL(<FS_TEMP>).

    MOVE-CORRESPONDING <FS_TEMP> TO LS_ZCOT0320.

    LS_ZCOT0320-ERDAT = SY-DATUM.
    LS_ZCOT0320-ERZET = SY-UZEIT.
    LS_ZCOT0320-ERNAM = SY-UNAME.
    LS_ZCOT0320-AEDAT = SY-DATUM.
    LS_ZCOT0320-AEZET = SY-UZEIT.
    LS_ZCOT0320-AENAM = SY-UNAME.

    IF LS_ZCOT0320-SEQ IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(LS_TABLE)
        FROM ZCOT0320
       WHERE BNAME EQ @LS_ZCOT0320-BNAME
         AND SEQ   EQ @LS_ZCOT0320-SEQ.

      LS_ZCOT0320-ERDAT = LS_TABLE-ERDAT.
      LS_ZCOT0320-ERZET = LS_TABLE-ERZET.
      LS_ZCOT0320-ERNAM = LS_TABLE-ERNAM.

    ELSE.

      SELECT MAX( SEQ ) INTO @LV_SEQ
        FROM ZCOT0320
       WHERE BNAME EQ @LS_ZCOT0320-BNAME.

      LS_ZCOT0320-SEQ = LV_SEQ + 1.
      <FS_TEMP>-SEQ   = LV_SEQ + 1.

    ENDIF.

    MODIFY ZCOT0320 FROM LS_ZCOT0320.
    IF SY-SUBRC EQ 0.

      IF <FS_TEMP>-MESSAGE IS INITIAL.
        <FS_TEMP>-MESSAGE = TEXT-S01.
      ENDIF.

      COMMIT WORK.

      ASSIGN <FS_TEMP>-STYLE TO <FS_STYLE>.
      DELETE <FS_STYLE> WHERE FIELDNAME = 'BNAME'.

    ELSE.
      LV_FLAG = ABAP_TRUE.
      <FS_TEMP>-MESSAGE = TEXT-E03.
      ROLLBACK WORK.
    ENDIF.

  ENDLOOP.

  IF LV_FLAG IS INITIAL.
    SORT GT_DISPLAY BY BNAME SEQ.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_INIT_HELP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_INIT_HELP .

  "__ Function Key
  DATA: LS_FUNTXT TYPE SMP_DYNTXT.

  LS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  LS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = LS_FUNTXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCR_USER_COMMAND_HELP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SCR_USER_COMMAND_HELP .

  CASE SSCRFIELDS-UCOMM.
    WHEN 'FC01'.
      PERFORM CALL_POPUP_HELP(ZCAR9000) USING SY-REPID SY-DYNNR SY-LANGU ''.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
