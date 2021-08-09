*&---------------------------------------------------------------------*
*& Include          ZCOR0140F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITAIL
*&---------------------------------------------------------------------*
FORM INITAIL .
  GV_REPID = SY-REPID.

  SET CURSOR FIELD 'PA_KSTAR'.

  SELECT SINGLE BEZEI INTO @PA_KTXT
    FROM TKA01
   WHERE KOKRS = @PA_KOKRS.

  SET PARAMETER ID 'KPL' FIELD GC_KTOPL.

  DATA : LV_KTOPL TYPE KTOPL.
  GET PARAMETER ID 'KPL' FIELD LV_KTOPL.

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

  PERFORM SELECTED_MAIN_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_MAIN_DATA
*&---------------------------------------------------------------------*
FORM SELECTED_MAIN_DATA .

  DATA: LV_INDEX     TYPE I,
        LV_MONTH     TYPE N LENGTH 2,
        LV_FIELDNAME TYPE FIELDNAME.

  DATA: LS_CELL TYPE LVC_S_STYL,
        LT_CELL TYPE LVC_T_STYL.

  FIELD-SYMBOLS: <FS_WKF>   TYPE ANY,
                 <FS_OUT>   TYPE ANY,
                 <FS_MOD>   TYPE ANY,
                 <FS_TOTAL> TYPE ANY.

  SELECT A~KOKRS,
         A~GJAHR,
         A~VERSN,
         A~KSTAR,
         B~KTEXT AS KSTXT,
         A~KOSTL,
         C~KTEXT AS KOTXT,
         A~ZBSUP,
         A~WAERS,
         A~WKF01,
         A~WKF02,
         A~WKF03,
         A~WKF04,
         A~WKF05,
         A~WKF06,
         A~WKF07,
         A~WKF08,
         A~WKF09,
         A~WKF10,
         A~WKF11,
         A~WKF12
       INTO TABLE @DATA(LT_ZCOT0140)
       FROM ZCOT0140 AS A LEFT JOIN CSKU AS B
                              ON A~KSTAR = B~KSTAR
                             AND B~SPRAS = @SY-LANGU
                             AND B~KTOPL = @GC_KTOPL
                       LEFT JOIN CSKT AS C
                              ON A~KOSTL = C~KOSTL
                             AND C~SPRAS = @SY-LANGU
                             AND C~DATBI = '99991231'
       WHERE A~GJAHR = @PA_GJAHR
       AND A~KOKRS = @PA_KOKRS
       AND A~VERSN = @PA_VERSN
       AND A~KSTAR = @PA_KSTAR.
  IF SY-SUBRC EQ 0.
    READ TABLE LT_ZCOT0140 INDEX 1 ASSIGNING FIELD-SYMBOL(<LS_ZCOT0140>).
    GV_ZBSUP = <LS_ZCOT0140>-ZBSUP.
  ENDIF.


  SELECT SINGLE KTEXT
    INTO @DATA(LV_KSTXT)
    FROM CSKU
   WHERE KSTAR = @PA_KSTAR
     AND SPRAS = @SY-LANGU
     AND KTOPL = @GC_KTOPL.

  SELECT A~KOKRS   AS KOKRS,
         @PA_GJAHR AS GJAHR,
         @PA_VERSN AS VERSN,
         @PA_KSTAR AS KSTAR,
         @LV_KSTXT AS KSTXT,
         A~KOSTL   AS KOSTL,
         B~KTEXT   AS KOTXT,
         @GV_WAERS AS WAERS
    INTO TABLE @DATA(LT_CSKS)
    FROM CSKS AS A LEFT JOIN CSKT AS B
                          ON A~KOKRS = B~KOKRS
                         AND A~DATBI = B~DATBI
                         AND A~KOSTL = B~KOSTL
                         AND B~SPRAS = @SY-LANGU
   WHERE A~KOKRS = @PA_KOKRS.

  SORT LT_CSKS BY KOSTL.

  LOOP AT LT_CSKS ASSIGNING FIELD-SYMBOL(<LS_CSKS>).

    MOVE-CORRESPONDING <LS_CSKS> TO GS_OUTTAB.
    MOVE-CORRESPONDING <LS_CSKS> TO GS_MODTAB.

    ADD 1 TO LV_INDEX.
    GS_OUTTAB-ZBSUP = GV_ZBSUP.
    GS_OUTTAB-INDEX = LV_INDEX.

    ADD 1 TO LV_INDEX.
    GS_MODTAB-ZBSUP = GV_ZBSUP.
    GS_MODTAB-INDEX = LV_INDEX.

    READ TABLE LT_ZCOT0140 INTO <LS_ZCOT0140>
                           WITH KEY KOSTL = <LS_CSKS>-KOSTL.
    IF SY-SUBRC EQ 0.

      LV_MONTH = '01'.

      DO 12 TIMES.

        LV_FIELDNAME = '<LS_ZCOT0140>-WKF' && LV_MONTH.

        ASSIGN (LV_FIELDNAME) TO <FS_WKF>.

        LV_FIELDNAME = 'GS_OUTTAB-WKF' && LV_MONTH.
        ASSIGN (LV_FIELDNAME) TO <FS_OUT>.

        <FS_OUT> = <FS_WKF>.

        ASSIGN COMPONENT 'TOTAL' OF STRUCTURE GS_OUTTAB TO <FS_TOTAL>.
        <FS_TOTAL> = <FS_TOTAL> + <FS_WKF>.

        LV_FIELDNAME = 'GS_MODTAB-WKF' && LV_MONTH.
        ASSIGN (LV_FIELDNAME) TO <FS_MOD>.

        <FS_MOD> = <FS_WKF> * ( GS_MODTAB-ZBSUP * 100 ).

        ASSIGN COMPONENT 'TOTAL' OF STRUCTURE GS_MODTAB TO <FS_TOTAL>.
        <FS_TOTAL> = <FS_TOTAL> + <FS_MOD>.

        ADD 1 TO LV_MONTH.

      ENDDO.

    ELSE.

      GS_OUTTAB-ZBSUP = GV_ZBSUP.

    ENDIF.

    LV_MONTH = '01'.

    DO 12 TIMES.

      CLEAR LS_CELL.
      LS_CELL-STYLE  = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      LS_CELL-FIELDNAME = 'WKF' && LV_MONTH.
      APPEND LS_CELL TO GS_OUTTAB-STYLE.

      LS_CELL-STYLE  = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      APPEND LS_CELL TO GS_MODTAB-STYLE.

      ADD 1 TO LV_MONTH.

    ENDDO.

    APPEND GS_OUTTAB TO GT_OUTTAB.
    APPEND GS_MODTAB TO GT_OUTTAB.
    CLEAR: GS_OUTTAB, GS_MODTAB.

  ENDLOOP.

  GT_DISPLAY[] = GT_OUTTAB.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
*       Postfix : _xxxx ( Screen No ).
*         부가적으로 더 필요한 경우 : _xx ( Seq No )를 추가로 붙임.
*----------------------------------------------------------------------*
FORM CREATE_INSTANCE_0100 .
*-- 1. customer container

  CREATE OBJECT GR_CON1
    EXPORTING
      CONTAINER_NAME = GV_CONTAINER. "USER가 정의한 CONTAINER

  CREATE OBJECT GR_GRID1
    EXPORTING
      I_PARENT = GR_CON1.



*-- 2. full screen

*  CREATE OBJECT GR_GRID1
*    EXPORTING
*      I_PARENT = CL_GUI_CONTAINER=>SCREEN0.

  "CL_GUI_CONTAINER=>SCREEN0 : Dummy for Top Level 0 Screen Container


ENDFORM.                    " CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM INIT_LAYOUT_0100.

*  GS_LAYOUT-EDIT_MODE  = ABAP_TRUE.
  GS_LAYOUT-ZEBRA      = ABAP_TRUE.
*  GS_LAYOUT-CWIDTH_OPT = ABAP_TRUE.
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
*   CL_GUI_ALV_GRID=>MC_FC_SORT_ASC,
*   CL_GUI_ALV_GRID=>MC_FC_SORT_DSC,
*   CL_GUI_ALV_GRID=>MC_MB_SUBTOT,
*   CL_GUI_ALV_GRID=>MC_MB_SUM,

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
*       text
*----------------------------------------------------------------------*
FORM ALV_SORT_0100 .

  CLEAR: GS_SORT, GT_SORT.
  REFRESH: GT_SORT.

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
FORM GET_FIELDCATLOG_DATA .

  DATA: LT_FIELDCAT TYPE KKBLO_T_FIELDCAT.

  CALL FUNCTION 'K_KKB_FIELDCAT_MERGE'
    EXPORTING
      I_CALLBACK_PROGRAM     = SY-REPID
      I_STRUCNAME            = 'ZCOS0140' "ABAP DIC. 정의된 STRUCTURE
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

  DATA: LV_TEXT(50),
        LV_POS    TYPE I VALUE 6.

  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.

    CLEAR: LV_TEXT.

    CASE GS_FIELDCAT-FIELDNAME.

      WHEN 'KSTAR'.
        LV_TEXT               = TEXT-C01.
        GS_FIELDCAT-COL_POS   = 1.
        GS_FIELDCAT-EMPHASIZE = 'C112'.

      WHEN 'KSTXT'.
        LV_TEXT               = TEXT-C02.
        GS_FIELDCAT-COL_POS   = 2.
        GS_FIELDCAT-EMPHASIZE = 'C112'.

      WHEN 'KOSTL'.
        LV_TEXT               = TEXT-C03.
        GS_FIELDCAT-COL_POS   = 3.
        GS_FIELDCAT-EMPHASIZE = 'C112'.

      WHEN 'KOTXT'.
        LV_TEXT               = TEXT-C04.
        GS_FIELDCAT-COL_POS   = 4.
        GS_FIELDCAT-EMPHASIZE = 'C112'.

      WHEN 'TOTAL'.
        LV_TEXT               = TEXT-C05.
        GS_FIELDCAT-COL_POS   = 5.
        GS_FIELDCAT-OUTPUTLEN = 10.

      WHEN 'KOKRS' OR 'GJAHR' OR 'VERSN' OR 'INDEX' OR 'ZBSUP' OR 'WAERS'.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.

      WHEN 'MESSAGE'.
        LV_TEXT               = TEXT-C06.
        GS_FIELDCAT-OUTPUTLEN = 50.

    ENDCASE.

    IF GS_FIELDCAT-FIELDNAME CP 'WKF*'.

      CONCATENATE GS_FIELDCAT-FIELDNAME+3(2) '월' INTO LV_TEXT.
      GS_FIELDCAT-COL_POS   = LV_POS.
      GS_FIELDCAT-OUTPUTLEN = 10.
      GS_FIELDCAT-EDIT      = ABAP_TRUE.
      ADD 1 TO LV_POS.

    ENDIF.

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
    GR_EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALL INSTANCES.
*    GR_EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_ONF4          FOR ALL INSTANCES.

ENDFORM.                    " REGIST_ALV_EVENT_0100
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

*--- Begin or Example
  DATA: LS_MOD_CELLS TYPE LVC_S_MODI,
        LS_INS_CELLS TYPE LVC_S_MOCE,
        LS_DEL_CELLS TYPE LVC_S_MOCE.

  DEFINE _MODIFY_CELL.

    CALL METHOD PR_DATA_CHANGED->MODIFY_CELL
      EXPORTING
        I_FIELDNAME = &1
        I_ROW_ID    = &2
        I_VALUE     = &3.

  END-OF-DEFINITION.
*
*  DEFINE _GET_CELL_VALUE.
*    CALL METHOD PR_DATA_CHANGED->GET_CELL_VALUE
*      EXPORTING
*        I_FIELDNAME = &1
*        I_ROW_ID    = &2
*      IMPORTING
*        E_VALUE     = &3.
*  END-OF-DEFINITION.
*
*  DEFINE _ADD_PROTOCOL.
*    CALL METHOD PR_DATA_CHANGED->ADD_PROTOCOL_ENTRY
*      EXPORTING
*        I_FIELDNAME = &1
*        I_ROW_ID    = &2
*        I_MSGID     = 'ZMSTD'
*        I_MSGTY     = &3
*        I_MSGNO     = &4
*        I_MSGV1     = &5
*        I_MSGV2     = &6
*        I_MSGV3     = &7
*        I_MSGV4     = &8.
*  END-OF-DEFINITION.
*
*--- End of Example

  DATA: LV_INDEX     TYPE I,
        LV_MONTH     TYPE N LENGTH 2,
        LV_FIELDNAME TYPE FIELDNAME,
        LV_FIELD     TYPE FIELDNAME.

  FIELD-SYMBOLS: <FS_CNT>   TYPE ANY,
                 <FS_CNT_T> TYPE ANY,
                 <FS_MOD>   TYPE ANY,
                 <FS_MOD_T> TYPE ANY.

  CASE PR_SENDER.
    WHEN GR_GRID1.
*      LOOP AT PR_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELLS.
      LOOP AT PR_DATA_CHANGED->MT_GOOD_CELLS INTO LS_MOD_CELLS.

        IF LS_MOD_CELLS-FIELDNAME(3) CP 'WKF'.

          READ TABLE GT_DISPLAY INDEX LS_MOD_CELLS-ROW_ID ASSIGNING FIELD-SYMBOL(<LS_DISPLAY>).

          LV_MONTH = '01'.

          DO 12 TIMES.

            LV_FIELDNAME = '<LS_DISPLAY>-WKF' && LV_MONTH.
            ASSIGN (LV_FIELDNAME) TO <FS_CNT>.

            LV_FIELD = 'WKF' && LV_MONTH.

            IF LV_FIELD = LS_MOD_CELLS-FIELDNAME.

              <FS_CNT> = LS_MOD_CELLS-VALUE.

            ENDIF.

            IF LV_MONTH = '01'.
              CLEAR <LS_DISPLAY>-TOTAL.
            ENDIF.

            ASSIGN COMPONENT 'TOTAL' OF STRUCTURE <LS_DISPLAY> TO <FS_CNT_T>.
            <FS_CNT_T> = <FS_CNT_T> + <FS_CNT>.


            READ TABLE GT_DISPLAY WITH KEY INDEX = ( <LS_DISPLAY>-INDEX + 1 ) ASSIGNING FIELD-SYMBOL(<LS_TOTAL>).

            LV_FIELDNAME = '<LS_TOTAL>-WKF' && LV_MONTH.
            ASSIGN (LV_FIELDNAME) TO <FS_MOD>.

            <FS_MOD> = ( ( <FS_CNT> * 100 ) * ( GV_ZBSUP * 100 ) ) / 100.

            IF LV_MONTH = '01'.
              CLEAR <LS_TOTAL>-TOTAL.
            ENDIF.

            ASSIGN COMPONENT 'TOTAL' OF STRUCTURE <LS_TOTAL> TO <FS_MOD_T>.
            <FS_MOD_T> = <FS_MOD_T> + <FS_MOD>.

            ADD 1 TO LV_MONTH.

          ENDDO.

        ENDIF.
      ENDLOOP.

      "-- ALV Refresh
      PERFORM REFRESH_GRID_0100.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " EVENT_DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM POPUP_TO_CONFIRM.

  "-- call popup
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR       = TEXT-PT1
*     DIAGNOSE_OBJECT             = ' '
      TEXT_QUESTION  = TEXT-QT1
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

  DATA: LR_ERROR   TYPE REF TO CX_SY_SQL_ERROR,
        LV_MESSAGE TYPE STRING.

  DATA LV_FNAME TYPE STFNA.
  DATA LV_MONTH TYPE N LENGTH 2.

  FIELD-SYMBOLS: <FS1> TYPE ANY,
                 <FS2> TYPE ANY.

*  CLEAR: GV_ERROR, GV_SUCESS, GV_TOTAL.

  DATA : LS_HEADERINFO  LIKE  BAPIPLNHDR.

  DATA : LT_INDEXSTRUCTURE LIKE BAPIACPSTRU OCCURS 0 WITH HEADER LINE,
         LT_COOBJECT       LIKE BAPIPCPOBJ OCCURS 0 WITH HEADER LINE,
         LT_PERVALUE       LIKE BAPIPCPVAL OCCURS 0 WITH HEADER LINE.

  LOOP AT GT_TRANSFER.

    LV_MONTH = '01'.

    DO 12 TIMES.

      CLEAR: GT_RETURN, GT_RETURN[].

      CLEAR: LS_HEADERINFO, LT_INDEXSTRUCTURE,
                            LT_INDEXSTRUCTURE[],
                            LT_COOBJECT,
                            LT_COOBJECT[],
                            LT_PERVALUE,
                            LT_PERVALUE[].

      "Planning ( KP06 )
*-- Header Data
      LS_HEADERINFO-CO_AREA     = PA_KOKRS.     "관리 회계영역
      LS_HEADERINFO-FISC_YEAR   = PA_GJAHR.     "회계연도
      LS_HEADERINFO-PERIOD_FROM = LV_MONTH.     "기간 시작
      LS_HEADERINFO-PERIOD_TO   = LV_MONTH.     "기간 종료
      LS_HEADERINFO-VERSION     = PA_VERSN.     "버전

*  CONCATENATE PA_GJAHR '년 사업계획' INTO LS_HEADERINFO-DOC_HDR_TX.

*-- 전표 헤더 텍스트
      LS_HEADERINFO-PLAN_CURRTYPE = 'C'. "통화

*-- CO-계획: 액티비티투입 & 주요지표 계획 BAPIs
      LT_INDEXSTRUCTURE-OBJECT_INDEX = 1.
      LT_INDEXSTRUCTURE-VALUE_INDEX  = 1.
      APPEND LT_INDEXSTRUCTURE.

*-- CO 계획: 1차 원가 BAPI에 대한 오브젝트
      LT_COOBJECT-OBJECT_INDEX = 1.
      LT_COOBJECT-COSTCENTER   = GT_TRANSFER-KOSTL.
      APPEND LT_COOBJECT.

*-- CO 계획: 1차 원가 BAPI에 대한 값
      LT_PERVALUE-VALUE_INDEX  = 1.
      LT_PERVALUE-COST_ELEM    = GT_TRANSFER-KSTAR.   "원가요소
      LT_PERVALUE-TRANS_CURR   = GT_TRANSFER-WAERS.

      LV_FNAME = 'GT_TRANSFER-WKF' && LV_MONTH.
      ASSIGN (LV_FNAME) TO <FS1>.

      LV_FNAME = 'LT_PERVALUE-FIX_VAL_PER' && LV_MONTH.
      ASSIGN (LV_FNAME) TO <FS2>.

      IF GT_TRANSFER-WAERS = 'KRW'.
        <FS2> = <FS1> * 100.
      ELSE.
        <FS2> = <FS1>.
      ENDIF.

      IF GT_TRANSFER-KSTAR CP '04*'   OR GT_TRANSFER-KSTAR CP '0701*' OR
         GT_TRANSFER-KSTAR CP '0703*' OR GT_TRANSFER-KSTAR CP '0705*'.
        <FS2> = <FS2> * -1.
      ENDIF.

      IF <FS2> IS INITIAL.
        ADD 1 TO LV_MONTH.
        CONTINUE.
      ENDIF.

      APPEND LT_PERVALUE.

      CALL FUNCTION 'BAPI_COSTACTPLN_POSTPRIMCOST'
        EXPORTING
          HEADERINFO     = LS_HEADERINFO
        TABLES
          INDEXSTRUCTURE = LT_INDEXSTRUCTURE
          COOBJECT       = LT_COOBJECT
          PERVALUE       = LT_PERVALUE
          RETURN         = GT_RETURN.

      READ TABLE GT_RETURN WITH KEY TYPE = 'E'.
      IF SY-SUBRC EQ 0 .

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.


        PERFORM BUILD_MESSAGE USING    GT_RETURN
                              CHANGING LV_MESSAGE.

        LOOP AT GT_DISPLAY WHERE KOKRS = GT_TRANSFER-KOKRS
                             AND GJAHR = GT_TRANSFER-GJAHR
                             AND VERSN = GT_TRANSFER-VERSN
                             AND KSTAR = GT_TRANSFER-KSTAR
                             AND KOSTL = GT_TRANSFER-KOSTL.

          GT_DISPLAY-MESSAGE = LV_MESSAGE.
          MODIFY GT_DISPLAY.

        ENDLOOP.

      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

      ENDIF.

      ADD 1 TO LV_MONTH.

    ENDDO.

  ENDLOOP.

  TRY .
      "... SAVE DATA

      DELETE FROM ZCOT0140 WHERE KOKRS = PA_KOKRS
                             AND GJAHR = PA_GJAHR
                             AND VERSN = PA_VERSN
                             AND KSTAR = PA_KSTAR.

      MODIFY ZCOT0140 FROM TABLE GT_ZCOT0140.

      "성공적으로 저장하였습니다.
      MESSAGE S007.
    CATCH CX_SY_SQL_ERROR INTO LR_ERROR.

      ROLLBACK WORK.

      LV_MESSAGE = LR_ERROR->GET_TEXT( ).
      MESSAGE S001 WITH LV_MESSAGE DISPLAY LIKE 'E'.

  ENDTRY.

ENDFORM.                    " SAVE_DATA_RTN
*&---------------------------------------------------------------------*
*& Form CHECKED_SAVED_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM CHECKED_SAVED_DATA .
  CLEAR: GT_ZCOT0140, GT_ZCOT0140[],
         GT_TRANSFER, GT_TRANSFER[].

  LOOP AT GT_DISPLAY.

    IF ( GT_DISPLAY-INDEX MOD 2 ) = 1 AND GT_DISPLAY-TOTAL IS NOT INITIAL.

      MOVE-CORRESPONDING GT_DISPLAY TO GS_ZCOT0140.
      GS_ZCOT0140-ERDAT = SY-DATUM.
      GS_ZCOT0140-ERZET = SY-UZEIT.
      GS_ZCOT0140-ERNAM = SY-UNAME.

      APPEND GS_ZCOT0140 TO GT_ZCOT0140.

    ELSEIF ( GT_DISPLAY-INDEX MOD 2 ) = 0 AND GT_DISPLAY-TOTAL IS NOT INITIAL.

      GT_TRANSFER = GT_DISPLAY.
      APPEND GT_TRANSFER.

    ENDIF.

    CLEAR: GT_TRANSFER, GT_DISPLAY, GS_ZCOT0140.

  ENDLOOP.

*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_MESSAGE
*&---------------------------------------------------------------------*
FORM BUILD_MESSAGE  USING    PS_MESSAGE STRUCTURE BAPIRET2
                     CHANGING PV_TEXT.

  CLEAR PV_TEXT.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      MSGID               = PS_MESSAGE-ID
      MSGNR               = PS_MESSAGE-NUMBER
      MSGV1               = PS_MESSAGE-MESSAGE_V1
      MSGV2               = PS_MESSAGE-MESSAGE_V2
      MSGV3               = PS_MESSAGE-MESSAGE_V3
      MSGV4               = PS_MESSAGE-MESSAGE_V4
    IMPORTING
      MESSAGE_TEXT_OUTPUT = PV_TEXT.

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
