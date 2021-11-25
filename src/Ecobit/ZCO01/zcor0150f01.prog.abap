*&---------------------------------------------------------------------*
*& Include          ZCOR0150F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITAIL
*&---------------------------------------------------------------------*
FORM INITAIL .
  GV_REPID = SY-REPID.

*  GET PARAMETER ID 'CAC' FIELD PA_KOKRS.

  SELECT SINGLE BEZEI INTO @PA_KTXT
    FROM TKA01
   WHERE KOKRS = @PA_KOKRS.

  "__ 20191223 BSGSM_FCM ADD default cac
  SET PARAMETER ID 'CAC' FIELD PA_KOKRS.

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
*& Form F4_VERSN
*&---------------------------------------------------------------------*
FORM F4_VERSN  CHANGING PV_VERSN.
  DATA: LT_RETURN TYPE DDSHRETVAL OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF LT_VERSN OCCURS 0,
          KOKRS TYPE KOKRS,
          VERSN TYPE VERSN,
        END OF LT_VERSN.

  SELECT KOKRS VERSN INTO TABLE LT_VERSN
    FROM ZCOT0150
   WHERE KOKRS = PA_KOKRS
   GROUP BY KOKRS VERSN.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'VERSN'
      DYNPPROG        = SY-CPROG
      DYNPNR          = SY-DYNNR
      WINDOW_TITLE    = TEXT-C01
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = LT_VERSN
      RETURN_TAB      = LT_RETURN
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.

  IF SY-SUBRC EQ 0.
    READ TABLE LT_RETURN INDEX 1.
    PV_VERSN = LT_RETURN-FIELDVAL.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

  "-- SELECTED MAIN DATA
  PERFORM SELECTED_MAIN_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_MAIN_DATA
*&---------------------------------------------------------------------*
FORM SELECTED_MAIN_DATA .

  CLEAR: GS_DISPLAY, GT_DISPLAY.
  REFRESH: GT_DISPLAY.

  DATA: LS_CELL TYPE LVC_S_STYL.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE @GT_DISPLAY
    FROM ZCOT0150
   WHERE KOKRS = @PA_KOKRS
     AND VERSN = @PA_VERSN.

  IF SY-SUBRC EQ 0.

    LOOP AT GT_DISPLAY INTO GS_DISPLAY.

      IF GS_DISPLAY-GUBUN = '1'.
        CLEAR LS_CELL.
        LS_CELL-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        LS_CELL-FIELDNAME = 'CALCU'.
        APPEND LS_CELL TO GS_DISPLAY-STYLE.

      ENDIF.

      CLEAR LS_CELL.
      LS_CELL-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      LS_CELL-FIELDNAME = 'DESCR'.
      APPEND LS_CELL TO GS_DISPLAY-STYLE.


      IF GS_DISPLAY-GUBUN = '2'.
        CLEAR LS_CELL.
        LS_CELL-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        LS_CELL-FIELDNAME = 'FKAGRU'.
        APPEND LS_CELL TO GS_DISPLAY-STYLE.
      ENDIF.

      CLEAR LS_CELL.
      LS_CELL-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      LS_CELL-FIELDNAME = 'GUBUN'.
      APPEND LS_CELL TO GS_DISPLAY-STYLE.

      IF GS_DISPLAY-GUBUN = '2'.
        CLEAR LS_CELL.
        LS_CELL-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        LS_CELL-FIELDNAME = 'TKAGRU'.
        APPEND LS_CELL TO GS_DISPLAY-STYLE.
      ENDIF.

      MODIFY GT_DISPLAY FROM GS_DISPLAY.

    ENDLOOP.

  ENDIF.

  SORT GT_DISPLAY BY ZCODE.

*  GT_DISPLAY_LOG[] = GT_DISPLAY[].

*  GS_DISPLAY-ZCODE = '1010'.
*  GS_DISPLAY-DESCR = 'DDFFD'.
*  APPEND GS_DISPLAY TO GT_DISPLAY.
*
*  GS_DISPLAY-ZCODE = '1020'.
*  GS_DISPLAY-CALCU = 'TEST'.
*  APPEND GS_DISPLAY TO GT_DISPLAY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
*       Postfix : _xxxx ( Screen No ).
*         부가적으로 더 필요한 경우 : _xx ( Seq No )를 추가로 붙임.
*----------------------------------------------------------------------*
FORM CREATE_INSTANCE_0100 .
**-- 1. customer container
*
*  CREATE OBJECT GR_CON1
*    EXPORTING
*      CONTAINER_NAME = GV_CONTAINER. "USER가 정의한 CONTAINER
*
*  CREATE OBJECT GR_GRID1
*    EXPORTING
*      I_PARENT = GR_CON1.



*-- 2. full screen

*  CREATE OBJECT GR_GRID1
*    EXPORTING
*      I_PARENT = CL_GUI_CONTAINER=>SCREEN0.

  "CL_GUI_CONTAINER=>SCREEN0 : Dummy for Top Level 0 Screen Container


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
      HEIGHT = 8.

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
*       text
*----------------------------------------------------------------------*
FORM INIT_LAYOUT_0100.

  GS_LAYOUT-EDIT_MODE  = ABAP_TRUE.
  GS_LAYOUT-ZEBRA      = ABAP_TRUE.
  GS_LAYOUT-CWIDTH_OPT = SPACE.
  GS_LAYOUT-STYLEFNAME = 'STYLE'.
  GS_LAYOUT-SEL_MODE   = SPACE.     "B:단일,C:복수,D:셀,A:행/열
  GS_LAYOUT-BOX_FNAME  = SPACE.
  GS_LAYOUT-NO_ROWMARK = SPACE.

*  GS_LAYOUT-CTAB_FNAME = 'COLOR'.
*  GS_LAYOUT-INFO_FNAME = 'INFO'.

**  "alv title
**  GS_LAYOUT-GRID_TITLE = TEXT-GT1.

ENDFORM.                    " INIT_LAYOUT_0100
*&---------------------------------------------------------------------*
*&      Form  SET_GRID_EXCLUDE_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_GRID_EXCLUDE_0100 .

  DATA: LS_EXCLUDE LIKE LINE OF GT_EXCLUDE.
  REFRESH: GT_EXCLUDE.

  "-- DEFINE _SET_EX
  DEFINE _SET_EX.
    CLEAR: LS_EXCLUDE.
    LS_EXCLUDE = &1.
    APPEND LS_EXCLUDE TO GT_EXCLUDE.
  END-OF-DEFINITION.


  _SET_EX:
*   CL_GUI_ALV_GRID=>MC_FC_FIND,
   CL_GUI_ALV_GRID=>MC_FC_SORT_ASC,
   CL_GUI_ALV_GRID=>MC_FC_SORT_DSC,
   CL_GUI_ALV_GRID=>MC_MB_SUBTOT,
   CL_GUI_ALV_GRID=>MC_MB_SUM,

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
*       text
*----------------------------------------------------------------------*
FORM ALV_SORT_0100 .

*  CLEAR: GS_SORT, GT_SORT.
*  REFRESH: GT_SORT.
*
*  GS_SORT-FIELDNAME = 'CARRID'.
*  GS_SORT-GROUP  = ABAP_TRUE.
*  GS_SORT-SUBTOT = ABAP_TRUE.
*  APPEND GS_SORT TO GT_SORT.
*
*
*  GS_SORT-FIELDNAME = 'CONNID'.
*  GS_SORT-GROUP  = ABAP_TRUE.
*  GS_SORT-SUBTOT = ABAP_TRUE.
*  APPEND GS_SORT TO GT_SORT.

ENDFORM.                    " ALV_SORT_0100
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDCAT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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
*       text
*----------------------------------------------------------------------*
FORM GET_FIELDCATLOG_DATA .

  DATA: LT_FIELDCAT TYPE KKBLO_T_FIELDCAT.

  CALL FUNCTION 'K_KKB_FIELDCAT_MERGE'
    EXPORTING
      I_CALLBACK_PROGRAM     = SY-REPID
*     I_TABNAME              = 'ZSTDS0060' "프로그램내 ITAB
      I_STRUCNAME            = 'ZCOS0150' "ABAP DIC. 정의된 STRUCTURE
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

    " Error Fieldcatalog merge!!
    MESSAGE E020.

  ENDIF.

ENDFORM.                    " GET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MODIFY_FIELDCATLOG_DATA .

  DATA:  LV_TEXT(50).

  "--- Change Fieldcat.
  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.
    CLEAR: LV_TEXT.

    "-- Change fieldcat Attribute
    CASE GS_FIELDCAT-FIELDNAME.
      WHEN 'ZCODE'.
        LV_TEXT = TEXT-F01.
        GS_FIELDCAT-OUTPUTLEN = '4'.
        GS_FIELDCAT-COL_POS   = '1'.

      WHEN 'GUBUN'.
        LV_TEXT = TEXT-F02.
        GS_FIELDCAT-VALEXI     = '!'.
        GS_FIELDCAT-OUTPUTLEN  = '15'.
        GS_FIELDCAT-COL_POS    = '2'.
        GS_FIELDCAT-DRDN_HNDL  = '1'.
        GS_FIELDCAT-DRDN_ALIAS = ABAP_TRUE.

      WHEN 'DESCR'.
        LV_TEXT = TEXT-F03.
        GS_FIELDCAT-OUTPUTLEN = '30'.
        GS_FIELDCAT-COL_POS   = '3'.

      WHEN 'FKAGRU'.
        LV_TEXT = TEXT-F04.
        GS_FIELDCAT-OUTPUTLEN  = '15'.
        GS_FIELDCAT-COL_POS    = '4'.
        GS_FIELDCAT-F4AVAILABL = ABAP_TRUE.

      WHEN 'TKAGRU'.
        LV_TEXT = TEXT-F05.
        GS_FIELDCAT-OUTPUTLEN  = '15'.
        GS_FIELDCAT-COL_POS    = '5'.
        GS_FIELDCAT-F4AVAILABL = ABAP_TRUE.

      WHEN 'CALCU'.
        LV_TEXT = TEXT-F06.
        GS_FIELDCAT-OUTPUTLEN = '50'.
        GS_FIELDCAT-COL_POS   = '6'.

      WHEN 'VERSN_T'.
        LV_TEXT = TEXT-F07.
        GS_FIELDCAT-OUTPUTLEN = '50'.
        GS_FIELDCAT-COL_POS   = '7'.
        GS_FIELDCAT-EDIT = ABAP_TRUE .


      WHEN OTHERS.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
    ENDCASE.

*    GS_FIELDCAT-COL_OPT = ABAP_TRUE.

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
*       text
*----------------------------------------------------------------------*
FORM SET_FIELDCATLOG_DATA.

  CLEAR GT_FIELDCAT[].

  PERFORM FILL_FIELD_CATEGORY USING :
        'S' 'FIELDNAME'   'ICON',
        ' ' 'OUTPUTLEN'   '4',
        ' ' 'FIX_COLUMN'  'X',
        'E' 'COLTEXT'     'ID',

        'S' 'FIELDNAME'   'CARRID',
        ' ' 'OUTPUTLEN'   '3',
        'E' 'COLTEXT'     '항공사 ID',

        'S' 'FIELDNAME'   'CONNID',
        ' ' 'OUTPUTLEN'   '4',
        ' ' 'HOTSPOT'     'X',
        'E' 'COLTEXT'     '운항연결 ID',

        'S' 'FIELDNAME'   'FLDATE',
        ' ' 'OUTPUTLEN'   '10',
        'E' 'COLTEXT'     '운항일',

        'S' 'FIELDNAME'   'PRICE',
        ' ' 'OUTPUTLEN'   '20',
        ' ' 'CFIELDNAME'  'CURRENCY',
        'E' 'COLTEXT'     '가격',

        'S' 'FIELDNAME'   'CURRENCY',
        ' ' 'OUTPUTLEN'   '5',
        'E' 'COLTEXT'     '현지통화',

        'S' 'FIELDNAME'   'PLANETYPE',
        ' ' 'OUTPUTLEN'   '10',
        ' ' 'HOTSPOT'     'X',
        'E' 'COLTEXT'     '항공기',

        'S' 'FIELDNAME'   'SEATSMAX',
        ' ' 'OUTPUTLEN'   '10',
        'E' 'COLTEXT'     '최대용량',

        'S' 'FIELDNAME'   'SEATSOCC',
        ' ' 'OUTPUTLEN'   '10',
        'E' 'COLTEXT'     '점유좌석',

        'S' 'FIELDNAME'   'PAYMENTSUM',
        ' ' 'OUTPUTLEN'   '22',
        ' ' 'CFIELDNAME'  'CURRENCY',
        'E' 'COLTEXT'     '예약 총계',

        'S' 'FIELDNAME'   'SEATSMAX_B',
        ' ' 'OUTPUTLEN'   '10',
        'E' 'COLTEXT'     'MAX. Business Class',

        'S' 'FIELDNAME'   'SEATSOCC_B',
        ' ' 'OUTPUTLEN'   '10',
        'E' 'COLTEXT'     'OCC. Business Class',

        'S' 'FIELDNAME'   'SEATSMAX_F',
        ' ' 'OUTPUTLEN'   '10',
        'E' 'COLTEXT'     'MAX. First Class',

        'S' 'FIELDNAME'   'SEATSOCC_F',
        ' ' 'OUTPUTLEN'   '10',
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
    GR_EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALL INSTANCES,
    GR_EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALL INSTANCES,
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

ENDFORM.                    " REFRESH_GRID_0100
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

  CONCATENATE TEXT-C01 ':' PA_VERSN
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

  LV_TEXT = TEXT-C03.
  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.
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
        LS_ITAB LIKE GS_DISPLAY,
        LV_CODE TYPE N LENGTH 4.

  DATA: LV_KAGRU       TYPE KAGRU,
        LV_KAGRU_CHECK TYPE KAGRU.

  DATA: BEGIN OF LS_CALCU,
          CAL LIKE ZCOS0150-CALCU,
        END OF LS_CALCU.
  DATA: LT_CALCU LIKE TABLE OF LS_CALCU.
  DATA: LV_SEPERATOR TYPE C VALUE ',',
        LV_CALCU     LIKE ZCOS0150-CALCU.

  DATA : LV_PATTERN(200) TYPE C,
         LV_SUCCESS      TYPE C,
         LO_MATCHER      TYPE REF TO CL_ABAP_MATCHER.

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

      LOOP AT PR_DATA_CHANGED->MT_INSERTED_ROWS INTO LS_INS_CELLS.

        CLEAR: LT_ITAB, LS_ITAB.
        REFRESH LT_ITAB.

        MOVE GT_DISPLAY TO LT_ITAB.

        SORT LT_ITAB BY ZCODE DESCENDING.
        READ TABLE LT_ITAB INDEX 1 INTO LS_ITAB.
        LV_CODE = LS_ITAB-ZCODE.
        IF LV_CODE IS INITIAL.
          LV_CODE = '1000'.
        ELSE.
          ADD 10 TO LV_CODE.
        ENDIF.

        _MODIFY_CELL: 'ZCODE' LS_INS_CELLS-ROW_ID LV_CODE,
                      'GUBUN' LS_INS_CELLS-ROW_ID '2'.

        _MODIFY_STYLE: 'GUBUN'  LS_INS_CELLS-ROW_ID CL_GUI_ALV_GRID=>MC_STYLE_ENABLED,
                       'FKAGRU' LS_INS_CELLS-ROW_ID CL_GUI_ALV_GRID=>MC_STYLE_ENABLED,
                       'TKAGRU' LS_INS_CELLS-ROW_ID CL_GUI_ALV_GRID=>MC_STYLE_ENABLED,
                       'DESCR'  LS_INS_CELLS-ROW_ID CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.

      ENDLOOP.

      LOOP AT PR_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELLS.
        CASE LS_MOD_CELLS-FIELDNAME.
          WHEN 'GUBUN'.
            IF LS_MOD_CELLS-VALUE = '1'.
              _MODIFY_CELL: 'DESCR'  LS_MOD_CELLS-ROW_ID '',
                            'FKAGRU' LS_MOD_CELLS-ROW_ID '',
                            'TKAGRU' LS_MOD_CELLS-ROW_ID ''.

              _MODIFY_STYLE: 'FKAGRU' LS_MOD_CELLS-ROW_ID CL_GUI_ALV_GRID=>MC_STYLE_DISABLED,
                             'TKAGRU' LS_MOD_CELLS-ROW_ID CL_GUI_ALV_GRID=>MC_STYLE_DISABLED,
                             'CALCU'  LS_MOD_CELLS-ROW_ID CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.


            ELSEIF LS_MOD_CELLS-VALUE = '2'.
              _MODIFY_CELL: 'CALCU' LS_MOD_CELLS-ROW_ID '',
                            'DESCR' LS_MOD_CELLS-ROW_ID ''.

              _MODIFY_STYLE: 'FKAGRU' LS_MOD_CELLS-ROW_ID CL_GUI_ALV_GRID=>MC_STYLE_ENABLED,
                             'TKAGRU' LS_MOD_CELLS-ROW_ID CL_GUI_ALV_GRID=>MC_STYLE_ENABLED,
                             'CALCU'  LS_MOD_CELLS-ROW_ID CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.

            ENDIF.

          WHEN 'FKAGRU'.

            CLEAR: LV_KAGRU.

            IF LS_MOD_CELLS-VALUE IS NOT INITIAL.

              LV_KAGRU = LS_MOD_CELLS-VALUE.

              _GET_CELL_VALUE 'TKAGRU' LS_MOD_CELLS-ROW_ID LV_KAGRU_CHECK.

              SELECT SINGLE SETNAME INTO @LV_KAGRU
                FROM SETHEADER
               WHERE SETCLASS = @GC_SETCLASS
                 AND SUBCLASS = @GC_KTOPL
                 AND SETNAME  = @LV_KAGRU.

              IF SY-SUBRC <> 0.
                _ADD_PROTOCOL LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID
                              'E' 023 TEXT-F04 SPACE SPACE SPACE.
                _MODIFY_CELL: LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID ''.
                ERROR_IN_DATA = ABAP_TRUE . EXIT.
              ENDIF.

              IF ( LV_KAGRU_CHECK < LV_KAGRU ) AND LV_KAGRU_CHECK IS NOT INITIAL.
                _ADD_PROTOCOL LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID
                              'E' 018 TEXT-F04 SPACE SPACE SPACE.
                _MODIFY_CELL: LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID ''.
                ERROR_IN_DATA = ABAP_TRUE . EXIT.
              ENDIF.
            ENDIF.

          WHEN 'TKAGRU'.

            CLEAR: LV_KAGRU.

            IF LS_MOD_CELLS-VALUE IS NOT INITIAL.

              LV_KAGRU = LS_MOD_CELLS-VALUE.

              _GET_CELL_VALUE 'FKAGRU' LS_MOD_CELLS-ROW_ID LV_KAGRU_CHECK.
              IF LV_KAGRU_CHECK IS INITIAL.
                _ADD_PROTOCOL LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID
                              'E' 019 TEXT-F04 SPACE SPACE SPACE.
                _MODIFY_CELL: LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID ''.
                ERROR_IN_DATA = ABAP_TRUE . EXIT.
              ENDIF.


              SELECT SINGLE SETNAME INTO @LV_KAGRU
                FROM SETHEADER
               WHERE SETCLASS = @GC_SETCLASS
                 AND SUBCLASS = @GC_KTOPL
                 AND SETNAME  = @LV_KAGRU.

              IF SY-SUBRC <> 0.
                _ADD_PROTOCOL LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID
                              'E' 023 TEXT-F04 SPACE SPACE SPACE.
                _MODIFY_CELL: LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID ''.
                ERROR_IN_DATA = ABAP_TRUE . EXIT.
              ENDIF.

              IF ( LV_KAGRU_CHECK > LV_KAGRU ) AND LV_KAGRU_CHECK IS NOT INITIAL.
                _ADD_PROTOCOL LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID
                              'E' 018 TEXT-F04 SPACE SPACE SPACE.
                _MODIFY_CELL: LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID ''.
                ERROR_IN_DATA = ABAP_TRUE . EXIT.
              ENDIF.
            ENDIF.

          WHEN 'CALCU'.

            IF LS_MOD_CELLS-VALUE IS NOT INITIAL.

              CLEAR: LT_ITAB, LS_ITAB.
              REFRESH LT_ITAB.

              MOVE GT_DISPLAY TO LT_ITAB.

              LV_CALCU = LS_MOD_CELLS-VALUE.

              CONDENSE LV_CALCU NO-GAPS.

              " 정규식으로 숫자, 사칙연산만 포함되어 있는지 점검
              CLEAR : LV_PATTERN, LO_MATCHER, LV_SUCCESS.
              CONCATENATE `[0-9\+\-\*\/]+$` '' INTO LV_PATTERN.
              LO_MATCHER = CL_ABAP_MATCHER=>CREATE( PATTERN = LV_PATTERN TEXT = LV_CALCU ).
              CALL METHOD LO_MATCHER->MATCH RECEIVING SUCCESS = LV_SUCCESS.
              IF LV_SUCCESS IS INITIAL.
                _ADD_PROTOCOL LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID
                              'E' 046 TEXT-F06 SPACE SPACE SPACE.
                ERROR_IN_DATA = ABAP_TRUE . EXIT.
              ENDIF.

              REPLACE ALL OCCURRENCES OF '+' IN LV_CALCU WITH LV_SEPERATOR.
              REPLACE ALL OCCURRENCES OF '-' IN LV_CALCU WITH LV_SEPERATOR.
              REPLACE ALL OCCURRENCES OF '*' IN LV_CALCU WITH LV_SEPERATOR.
              REPLACE ALL OCCURRENCES OF '/' IN LV_CALCU WITH LV_SEPERATOR.

              SPLIT LV_CALCU AT LV_SEPERATOR INTO TABLE LT_CALCU.

              LOOP AT LT_CALCU INTO LS_CALCU.

                READ TABLE LT_ITAB INTO LS_ITAB WITH KEY ZCODE = LS_CALCU-CAL.
                IF SY-SUBRC <> 0.
                  _ADD_PROTOCOL LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID
                                'E' 047 LS_CALCU-CAL SPACE SPACE SPACE.
                  ERROR_IN_DATA = ABAP_TRUE . EXIT.
                ENDIF.

              ENDLOOP.

              IF ERROR_IN_DATA IS INITIAL.
                CONDENSE LS_MOD_CELLS-VALUE NO-GAPS.
                _MODIFY_CELL: 'CALCU' LS_MOD_CELLS-ROW_ID LS_MOD_CELLS-VALUE.
              ENDIF.

            ENDIF.

          WHEN OTHERS.
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
*& Form APPEND_DROPDOWN_LIST_0100
*&---------------------------------------------------------------------*
FORM APPEND_DROPDOWN_LIST_0100 .
  CLEAR: GS_DRAL, GT_DRAL, GT_DRAL[].
  DATA: LS_DD07T TYPE DD07T,
        LT_DD07T TYPE TABLE OF DD07T.

  SELECT * INTO TABLE LT_DD07T
    FROM DD07T
   WHERE DOMNAME    = 'ZGUBN'
     AND DDLANGUAGE = SY-LANGU.

  LOOP AT LT_DD07T INTO LS_DD07T.
    GS_DRAL-HANDLE = '1'.
    GS_DRAL-INT_VALUE = LS_DD07T-DOMVALUE_L.
    GS_DRAL-VALUE = LS_DD07T-DOMVALUE_L && ` : ` && LS_DD07T-DDTEXT.
    APPEND GS_DRAL TO GT_DRAL.
    CLEAR GS_DRAL.
  ENDLOOP.

  CALL METHOD GR_GRID1->SET_DROP_DOWN_TABLE
    EXPORTING
      IT_DROP_DOWN_ALIAS = GT_DRAL.

ENDFORM.
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


  DATA: LS_MODI TYPE LVC_S_MODI.
  FIELD-SYMBOLS <F4TAB> TYPE LVC_T_MODI.
  ASSIGN PR_EVENT_DATA->M_DATA->* TO <F4TAB>.

  DATA LV_KAGRU TYPE KAGRU.

  CASE PR_SENDER.

    WHEN GR_GRID1.

      CASE PV_FIELDNAME.

        WHEN 'FKAGRU' OR 'TKAGRU'.

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
            LS_MODI-ROW_ID    = PS_ROW_NO-ROW_ID.
            LS_MODI-FIELDNAME = PV_FIELDNAME.
            LS_MODI-VALUE     = LV_KAGRU.
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
  GS_F4-FIELDNAME = 'FKAGRU'.
  GS_F4-REGISTER  = 'X'.
  INSERT GS_F4 INTO TABLE GT_F4.

  GS_F4-FIELDNAME = 'TKAGRU'.
  GS_F4-REGISTER  = 'X'.
  INSERT GS_F4 INTO TABLE GT_F4.

  CALL METHOD PR_GRID->REGISTER_F4_FOR_FIELDS
    EXPORTING
      IT_F4 = GT_F4.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECKED_SAVED_DATA
*&---------------------------------------------------------------------*
FORM CHECKED_SAVED_DATA .
  DATA: BEGIN OF LS_CALCU,
          CAL LIKE ZCOS0150-CALCU,
        END OF LS_CALCU.
  DATA: LT_CALCU LIKE TABLE OF LS_CALCU.
  DATA: LV_SEPERATOR TYPE C VALUE ',',
        LV_CALCU     LIKE ZCOS0150-CALCU.

  DATA: LT_ITAB LIKE TABLE OF GS_DISPLAY,
        LS_ITAB LIKE GS_DISPLAY.

  DATA: LCL_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
  DATA: LV_VERSN_T LIKE ZCOS0150-VERSN_T.

  FREE: LCL_DATA_CHANGED.

  IF LCL_DATA_CHANGED IS INITIAL.
    CREATE OBJECT LCL_DATA_CHANGED
      EXPORTING
        I_CALLING_ALV = GR_GRID1.
  ENDIF.

  LCL_DATA_CHANGED->MT_FIELDCATALOG = GT_FIELDCAT.
  LCL_DATA_CHANGED->REFRESH_PROTOCOL( ).

  DEFINE _ADD_PROTOCOL_SAVE.
    CALL METHOD LCL_DATA_CHANGED->ADD_PROTOCOL_ENTRY
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

  CLEAR: LT_ITAB, LS_ITAB.
  REFRESH LT_ITAB.

  MOVE GT_DISPLAY TO LT_ITAB.


  LOOP AT GT_DISPLAY INTO GS_DISPLAY.

    IF LV_VERSN_T IS INITIAL .
      LV_VERSN_T = GS_DISPLAY-VERSN_T.
    ELSE.
      IF LV_VERSN_T <> GS_DISPLAY-VERSN_T.
        MESSAGE I000 DISPLAY LIKE 'I' WITH  TEXT-E01.
        GV_EXIT = ABAP_TRUE .
        EXIT.
      ENDIF.
    ENDIF.

  ENDLOOP .

  CHECK GV_EXIT IS INITIAL .

  LOOP AT GT_DISPLAY INTO GS_DISPLAY WHERE GUBUN = '1'.

    LV_CALCU = GS_DISPLAY-CALCU.


    REPLACE ALL OCCURRENCES OF '+' IN LV_CALCU WITH LV_SEPERATOR.
    REPLACE ALL OCCURRENCES OF '-' IN LV_CALCU WITH LV_SEPERATOR.
    REPLACE ALL OCCURRENCES OF '*' IN LV_CALCU WITH LV_SEPERATOR.
    REPLACE ALL OCCURRENCES OF '/' IN LV_CALCU WITH LV_SEPERATOR.

    SPLIT LV_CALCU AT LV_SEPERATOR INTO TABLE LT_CALCU.

    LOOP AT LT_CALCU INTO LS_CALCU.

      READ TABLE LT_ITAB INTO LS_ITAB WITH KEY ZCODE = LS_CALCU-CAL.
      IF SY-SUBRC <> 0.
        _ADD_PROTOCOL_SAVE 'CALCU' SY-TABIX
                           'E' 048 GS_DISPLAY-CALCU LS_CALCU-CAL SPACE SPACE.

        GV_EXIT = ABAP_TRUE . EXIT.
      ENDIF.

      CLEAR: LS_CALCU, LS_ITAB.
    ENDLOOP.

    CLEAR: GS_DISPLAY, LV_CALCU, LT_CALCU.
    REFRESH LT_CALCU.
  ENDLOOP.

  IF GV_EXIT IS NOT INITIAL.
    CALL METHOD LCL_DATA_CHANGED->DISPLAY_PROTOCOL.
    CLEAR GV_EXIT.
  ENDIF.

ENDFORM.
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

  DATA: LT_ZCOT0150 TYPE TABLE OF ZCOT0150 WITH HEADER LINE.

  LOOP AT GT_DISPLAY INTO GS_DISPLAY.

    MOVE-CORRESPONDING GS_DISPLAY TO LT_ZCOT0150.
    LT_ZCOT0150-KOKRS = PA_KOKRS.
    LT_ZCOT0150-VERSN = PA_VERSN.
    LT_ZCOT0150-ERDAT = SY-DATUM.
    LT_ZCOT0150-ERZET = SY-UZEIT.
    LT_ZCOT0150-ERNAM = SY-UNAME.

    APPEND LT_ZCOT0150.
    CLEAR LT_ZCOT0150.

  ENDLOOP.

  TRY .
      DELETE FROM ZCOT0150 WHERE KOKRS = @PA_KOKRS
                             AND VERSN = @PA_VERSN.

      INSERT ZCOT0150 FROM TABLE LT_ZCOT0150.
      COMMIT WORK.

      MESSAGE S007.

    CATCH CX_SY_SQL_ERROR INTO DATA(LR_ERROR).

      ROLLBACK WORK.

      LV_MESSAGE = LR_ERROR->GET_TEXT( ).
      MESSAGE S001 WITH LV_MESSAGE DISPLAY LIKE 'E'.

  ENDTRY.

ENDFORM.                    " SAVE_DATA_RTN
*&---------------------------------------------------------------------*
*& Form SET_SAVE_GUBUN
*&---------------------------------------------------------------------*
FORM SET_SAVE_GUBUN .

  GV_RAD1 = 'X'.
  GV_VER1 = PA_VERSN.

  CALL SCREEN 0110 STARTING AT 35 5 ENDING AT 70 8.

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
