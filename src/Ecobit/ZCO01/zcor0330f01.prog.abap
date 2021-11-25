*&---------------------------------------------------------------------*
*& Include          ZCOR0330F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITAIL
*&---------------------------------------------------------------------*
FORM INITAIL .

  GV_REPID = SY-REPID.

  SELECT SINGLE BEZEI, WAERS INTO (@PA_KTXT, @GV_WAERS)
    FROM TKA01
   WHERE KOKRS = @PA_KOKRS.

  SO_ERDAT-LOW = SY-DATUM - 7.
  SO_ERDAT-HIGH = SY-DATUM.
  SO_ERDAT-SIGN = 'I'.
  SO_ERDAT-OPTION = 'BT'.
  APPEND SO_ERDAT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
FORM SET_SCREEN .

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'MG1'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.

    CASE SY-TCODE.
      WHEN 'ZCOR0331'.
        IF SCREEN-GROUP1 = 'MG2'.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATA_GET
*&---------------------------------------------------------------------*
FORM DATA_GET .

  CLEAR GV_LINES.

  SELECT A~GWKEY,  A~GWTYP, D~DDTEXT AS GWTXT,
         A~GWSTS,  F~DDTEXT AS GWSXT, A~ERDAT, A~ERNAM, A~ERZET,
         A~TITLE,  B~RSEQ, B~PROCESS_9,
         B~ROBJNR, B~RKSTAR, C~KTEXT, B~FAMOUNT,
         HSL01, HSL02, HSL03, HSL04, HSL05, HSL06,
         HSL07, HSL08, HSL09, HSL10, HSL11, HSL12,
         'KRW' AS WAERS
    INTO TABLE @GT_ZCOT1200
    FROM ZCOT1190 AS A
   INNER JOIN ZCOT1200 AS B
      ON A~GWKEY = B~GWKEY
    LEFT JOIN CSKU AS C
      ON B~RKSTAR = C~KSTAR
     AND C~SPRAS = @SY-LANGU
     AND C~KTOPL = @GC_KTOPL
    LEFT JOIN DD07T AS D
      ON D~DDLANGUAGE  = @SY-LANGU
     AND D~DOMNAME     = 'ZDGWTYP'
     AND D~DOMVALUE_L  = A~GWTYP
    LEFT JOIN DD07T AS F
      ON F~DDLANGUAGE  = @SY-LANGU
     AND F~DOMNAME     = 'ZDGWSTS'
     AND F~DOMVALUE_L  = A~GWSTS
   WHERE B~RLDNR = '00'
     AND B~RRCTY = '1'
     AND B~RVERS = 'B1'
     AND B~RKOKRS = @PA_KOKRS
     AND A~GWKEY  IN @SO_GWKEY
     AND A~GWTYP  IN @SO_GWTYP
     AND A~GWSTS  IN @SO_GWSTS
     AND A~ERDAT  IN @SO_ERDAT
     AND A~ERNAM  IN @SO_ERNAM.

  IF SY-SUBRC <> 0.
    MESSAGE S004 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  LOOP AT GT_ZCOT1200.
    MOVE-CORRESPONDING GT_ZCOT1200 TO GT_DISPLAY.
    COLLECT GT_DISPLAY.
    CLEAR   GT_DISPLAY.
  ENDLOOP.

  GV_LINES = LINES( GT_DISPLAY ).
  MESSAGE S039 WITH GV_LINES.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
FORM CREATE_INSTANCE_0100 .

  CREATE OBJECT GR_SPLITTER1
    EXPORTING
      ROWS    = 3
      COLUMNS = 1
      PARENT  = CL_GUI_SPLITTER_CONTAINER=>SCREEN0.

*== get container instance
*-- 1. top of page
  GR_PARENT_HTML = GR_SPLITTER1->GET_CONTAINER(
      ROW       = 1
      COLUMN    = 1 ).

*-- 2. head data
  GR_HEAD_CONTAINER = GR_SPLITTER1->GET_CONTAINER(
      ROW       = 2
      COLUMN    = 1 ).

*-- 3. data
  GR_DATA_CONTAINER = GR_SPLITTER1->GET_CONTAINER(
      ROW       = 3
      COLUMN    = 1 ).

  CALL METHOD GR_SPLITTER1->SET_ROW_HEIGHT
    EXPORTING
      ID     = 1
      HEIGHT = 10.

*-- 2. head data
  CALL METHOD GR_SPLITTER1->SET_ROW_HEIGHT
    EXPORTING
      ID     = 2
      HEIGHT = 50.

*-- 3. data
  CALL METHOD GR_SPLITTER1->SET_ROW_HEIGHT
    EXPORTING
      ID     = 3
      HEIGHT = 40.

*-- 2. head data
  CREATE OBJECT GR_GRID_HEAD
    EXPORTING
      I_PARENT = GR_HEAD_CONTAINER.

*-- 3. data
  CREATE OBJECT GR_GRID_DATA
    EXPORTING
      I_PARENT = GR_DATA_CONTAINER.

ENDFORM.                    " CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM INIT_LAYOUT_0100.

  GS_LAYOUT-EDIT_MODE  = ABAP_FALSE.
  GS_LAYOUT-ZEBRA      = ABAP_TRUE.
  GS_LAYOUT-CWIDTH_OPT = ABAP_TRUE.
  GS_LAYOUT-SEL_MODE   = 'D'.     "B:단일,C:복수,D:셀,A:행/열
*  GS_LAYOUT-BOX_FNAME  = SPACE.
*  GS_LAYOUT-NO_ROWMARK = SPACE.


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
    CLEAR: ls_exclude.
    ls_exclude = &1.
    APPEND ls_exclude TO gt_exclude.
  END-OF-DEFINITION.


  _SET_EX:
*   CL_GUI_ALV_GRID=>MC_FC_FIND,

    "-- begin 기능버튼활성화시 제외
   "-- end

    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,

    "-- begin 기능버튼활성화
*    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,
*    CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
*    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,
*    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW,
    "-- end

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

  PERFORM GET_FIELDCATLOG_DATA.
  PERFORM MODIFY_FIELDCATLOG_DATA.

ENDFORM.                    " APPEND_FIELDCAT_0100
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_FIELDCATLOG_DATA .

  DATA: LT_FIELDCAT TYPE KKBLO_T_FIELDCAT.
  CLEAR GT_FIELDCAT[].

  CALL FUNCTION 'K_KKB_FIELDCAT_MERGE'
    EXPORTING
      I_CALLBACK_PROGRAM     = SY-REPID
      I_STRUCNAME            = 'ZCOS0330'
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
*       text
*----------------------------------------------------------------------*
FORM MODIFY_FIELDCATLOG_DATA .

  DATA:  LV_TEXT(50).

  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.
    CLEAR: LV_TEXT.

    CASE GS_FIELDCAT-FIELDNAME.

      WHEN 'GWKEY'.
        LV_TEXT = TEXT-C01.

      WHEN 'GWTXT'.
        LV_TEXT = TEXT-C02.

      WHEN 'GWSXT'.
        LV_TEXT = TEXT-C03.

      WHEN 'TITLE'.
        LV_TEXT = TEXT-C04.

      WHEN 'ERDAT'.
        LV_TEXT = TEXT-C06.

      WHEN 'ERZET'.
        LV_TEXT = TEXT-C07.

      WHEN 'ERNAM'.
        LV_TEXT = TEXT-C05.

      WHEN OTHERS.

    ENDCASE.

    "Column Optimizer
    GS_FIELDCAT-COL_OPT = ABAP_TRUE.

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

** REGISTER EVENT
*  CALL METHOD PR_GRID->REGISTER_EDIT_EVENT
*    EXPORTING
*      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
**
*  CALL METHOD PR_GRID->SET_READY_FOR_INPUT
*    EXPORTING
*      I_READY_FOR_INPUT = 1.

*-- GR_EVENT_RECEIVER
  IF GR_EVENT_RECEIVER IS INITIAL.
    CREATE OBJECT GR_EVENT_RECEIVER.
  ENDIF.

* Handler Event
  SET HANDLER:
    GR_EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR PR_GRID.

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
  CALL METHOD GR_GRID_HEAD->SET_TABLE_FOR_FIRST_DISPLAY
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

  CALL METHOD GR_GRID_HEAD->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = GS_STABLE
      I_SOFT_REFRESH = SPACE.

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
FORM CHECKED_SAVED_DATA USING P_OK.

  CLEAR: GT_ROWS, GV_EXIT.

  CALL METHOD GR_GRID_HEAD->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROWS[].

  IF GT_ROWS[] IS INITIAL.
    MESSAGE S015 DISPLAY LIKE 'E'.
    GV_EXIT = ABAP_TRUE.
    EXIT.
  ENDIF.

  IF P_OK = '&URL'.
    DATA(LV_LINES) = LINES( GT_ROWS ).
    IF LV_LINES > 1.
      MESSAGE S016 DISPLAY LIKE 'E'.
      GV_EXIT = ABAP_TRUE.
      EXIT.
    ENDIF.
  ENDIF.

  LOOP AT GT_ROWS INTO GS_ROW.
    READ TABLE GT_DISPLAY INDEX GS_ROW-INDEX.
    IF SY-SUBRC = 0 AND GT_DISPLAY-GWSTS <> 'I0'.

      CASE P_OK.

        WHEN '&DELE'.
          MESSAGE S000 WITH TEXT-E01 DISPLAY LIKE 'E'.

        WHEN '&URL'.
          MESSAGE S000 WITH TEXT-E02 DISPLAY LIKE 'E'.

      ENDCASE.

      GV_EXIT = ABAP_TRUE.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CHECKED_SAVED_DATA
*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM POPUP_TO_CONFIRM USING PV_TITLE
                            PV_QUEST.

  "-- call popup
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR       = PV_TITLE
      TEXT_QUESTION  = PV_QUEST
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
FORM SAVE_DATA_RTN .

  DATA LT_DD07V TYPE TABLE OF DD07V WITH HEADER LINE.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      DOMNAME         = 'ZDGWSTS'
    TABLES
      VALUES_TAB      = LT_DD07V
    EXCEPTIONS
      NO_VALUES_FOUND = 1
      OTHERS          = 2.

  READ TABLE LT_DD07V WITH KEY DOMVALUE_L = 'Z9'.

  LOOP AT GT_ROWS INTO GS_ROW.

    READ TABLE GT_DISPLAY INDEX GS_ROW-INDEX.

    IF SY-SUBRC = 0.

      UPDATE ZCOT1190 SET GWSTS = 'Z9'
                          AEDAT = SY-DATUM
                          AEZET = SY-UZEIT
                          AENAM = SY-UNAME
       WHERE GWKEY = GT_DISPLAY-GWKEY.

      COMMIT WORK.

      MOVE: 'Z9'             TO GT_DISPLAY-GWSTS,
             LT_DD07V-DDTEXT TO GT_DISPLAY-GWSXT.

      MODIFY GT_DISPLAY TRANSPORTING GWSTS GWSXT
                    WHERE GWKEY = GT_DISPLAY-GWKEY.
    ENDIF.

  ENDLOOP.

  PERFORM REFRESH_GRID_0100.
  MESSAGE S007.

ENDFORM.                    " SAVE_DATA_RTN
*&---------------------------------------------------------------------*
*& Form EVENT_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM EVENT_DOUBLE_CLICK  USING PS_ROW     TYPE LVC_S_ROW
                               PS_COLUMN  TYPE LVC_S_COL
                               PS_ROW_NO  TYPE LVC_S_ROID
                               PR_SENDER TYPE REF TO CL_GUI_ALV_GRID.


  DATA LV_FIELDNAME TYPE FIELDNAME.
  DATA LV_MONTH TYPE N LENGTH 2.

  DATA LT_ZCOT1200 TYPE TABLE OF TY_ZCOT1200 WITH HEADER LINE.

  LT_ZCOT1200[] = GT_ZCOT1200[].

  CASE PR_SENDER.

    WHEN GR_GRID_HEAD.

      CLEAR: GT_ITEM, GT_ITEM[].

      READ TABLE GT_DISPLAY INTO DATA(LS_DISPLAY) INDEX PS_ROW.

      IF SY-SUBRC = 0.

        DELETE LT_ZCOT1200 WHERE GWKEY <> LS_DISPLAY-GWKEY.

        CASE LS_DISPLAY-GWTYP.

          WHEN 'A'.

            LOOP AT LT_ZCOT1200 .

              CASE LT_ZCOT1200-ROBJNR(2).

                WHEN 'PR'.
                  PERFORM GET_POST1_CODE_TXT USING     LT_ZCOT1200-ROBJNR
                                             CHANGING GT_ITEM-POSID
                                                      GT_ITEM-POST1.
                WHEN 'KS'.

                  MOVE LT_ZCOT1200-ROBJNR+6 TO GT_ITEM-KOSTL.

                  PERFORM GET_KOSTL_TEXT USING      GT_ITEM-KOSTL
                                                    PA_KOKRS
                                         CHANGING   GT_ITEM-KTEXT.
              ENDCASE.

              CASE LT_ZCOT1200-PROCESS_9.

                WHEN 'SEND'.

                  MOVE: LT_ZCOT1200-RKSTAR TO GT_ITEM-S_KSTAR,
                        LT_ZCOT1200-KTEXT  TO GT_ITEM-S_KSTXT,
                        LT_ZCOT1200-WAERS  TO GT_ITEM-WAERS.

                WHEN 'RECV'.

                  MOVE: LT_ZCOT1200-RKSTAR TO GT_ITEM-R_KSTAR,
                        LT_ZCOT1200-KTEXT  TO GT_ITEM-R_KSTXT,
                        LT_ZCOT1200-WAERS  TO GT_ITEM-WAERS.

                  CLEAR LV_MONTH.

                  DO 12 TIMES.
                    ADD 1 TO LV_MONTH.
                    LV_FIELDNAME = 'LT_ZCOT1200-HSL' && LV_MONTH.
                    ASSIGN (LV_FIELDNAME) TO FIELD-SYMBOL(<FS_HSL>).
                    GT_ITEM-AMOUNT = GT_ITEM-AMOUNT + <FS_HSL>.
                  ENDDO.

              ENDCASE.

              AT LAST.
                APPEND GT_ITEM.
                CLEAR  GT_ITEM.
              ENDAT.

            ENDLOOP.

          WHEN 'P'.

            LOOP AT LT_ZCOT1200
                  WHERE PROCESS_9 = 'RECV'.

              MOVE: LT_ZCOT1200-RKSTAR  TO GT_ITEM-KSTAR,
                    LT_ZCOT1200-KTEXT   TO GT_ITEM-KSTXT,
                    LT_ZCOT1200-WAERS  TO GT_ITEM-WAERS.

              CASE LT_ZCOT1200-ROBJNR(2).

                WHEN 'PR'.
                  PERFORM GET_POST1_CODE_TXT USING LT_ZCOT1200-ROBJNR
                                             CHANGING GT_ITEM-POSID
                                                      GT_ITEM-POST1.
                WHEN 'KS'.

                  MOVE LT_ZCOT1200-ROBJNR+6 TO GT_ITEM-KOSTL.

                  PERFORM GET_KOSTL_TEXT USING      GT_ITEM-KOSTL
                                                    PA_KOKRS
                                         CHANGING   GT_ITEM-KTEXT.
              ENDCASE.

              CLEAR LV_MONTH.
              DO 12 TIMES.
                ADD 1 TO LV_MONTH.
                LV_FIELDNAME = 'LT_ZCOT1200-HSL' && LV_MONTH.
                ASSIGN (LV_FIELDNAME) TO <FS_HSL>.
                GT_ITEM-AMOUNT = GT_ITEM-AMOUNT + <FS_HSL>.
              ENDDO.

              APPEND GT_ITEM.
              CLEAR  GT_ITEM.

            ENDLOOP.

          WHEN 'C'.

            LOOP AT LT_ZCOT1200.

              CASE LT_ZCOT1200-ROBJNR(2).

                WHEN 'PR'.
                  PERFORM GET_POST1_CODE_TXT USING    LT_ZCOT1200-ROBJNR
                                             CHANGING GT_ITEM-POSID
                                                      GT_ITEM-POST1.
                WHEN 'KS'.

                  MOVE LT_ZCOT1200-ROBJNR+6 TO GT_ITEM-KOSTL.

                  PERFORM GET_KOSTL_TEXT USING      GT_ITEM-KOSTL
                                                    PA_KOKRS
                                         CHANGING   GT_ITEM-KTEXT.
              ENDCASE.

              MOVE: LT_ZCOT1200-RKSTAR  TO GT_ITEM-KSTAR,
                    LT_ZCOT1200-KTEXT   TO GT_ITEM-KSTXT,
                    LT_ZCOT1200-FAMOUNT TO GT_ITEM-FAMOUNT,
                    LT_ZCOT1200-WAERS   TO GT_ITEM-WAERS.

              CLEAR LV_MONTH.

              DO 12 TIMES.
                ADD 1 TO LV_MONTH.
                LV_FIELDNAME = 'LT_ZCOT1200-HSL' && LV_MONTH.
                ASSIGN (LV_FIELDNAME) TO <FS_HSL>.
                GT_ITEM-IAMOUNT = GT_ITEM-IAMOUNT + <FS_HSL>.
              ENDDO.

              AT LAST.
                APPEND GT_ITEM.
                CLEAR  GT_ITEM.
              ENDAT.

            ENDLOOP.

*-- 반영 후 합계
            LOOP AT GT_ITEM.

              GT_ITEM-TAMOUNT =  GT_ITEM-FAMOUNT +
                                 GT_ITEM-IAMOUNT.
              MODIFY GT_ITEM.
            ENDLOOP.

        ENDCASE.

        PERFORM APPEND_FIELDCAT_0100_01 USING LS_DISPLAY-GWTYP.
        PERFORM DISPLAY_ALV_GRID_0100_01.
        MESSAGE S056 .

      ENDIF.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_FIELDCAT_0100_01
*&---------------------------------------------------------------------*
FORM APPEND_FIELDCAT_0100_01 USING P_TYPE.

  PERFORM GET_FIELDCATLOG_DATA_0100.
  PERFORM MODIFY_FIELDCATLOG_DATA_0100 USING P_TYPE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV_GRID_0100_01
*&---------------------------------------------------------------------*
FORM DISPLAY_ALV_GRID_0100_01 .

  GS_VARIANT-REPORT = SY-REPID.

  GV_SAVE = 'A'.

  "*-- Build field catalog for the alv control
  CALL METHOD GR_GRID_DATA->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_DEFAULT                     = ABAP_TRUE
      IS_LAYOUT                     = GS_LAYOUT
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = GV_SAVE
      IT_TOOLBAR_EXCLUDING          = GT_EXCLUDE
    CHANGING
      IT_FIELDCATALOG               = GT_FIELDCAT2
      IT_SORT                       = GT_SORT
      IT_OUTTAB                     = GT_ITEM[]
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3.

  IF SY-SUBRC NE 0.
    MESSAGE S000(0K) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
      DISPLAY LIKE 'E'.
    GV_EXIT = ABAP_TRUE. EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_TOP_OF_PAGE_DATA_0100
*&---------------------------------------------------------------------*
FORM MAKE_TOP_OF_PAGE_DATA_0100 .

  DATA: LT_TEXTS TYPE SDYDO_TEXT_TABLE.

  DATA: LV_TEXT TYPE CHAR255.

  DEFINE __ADD_TEXT.
    CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
      EXPORTING
        TEXT          =  &1.
      CALL METHOD GR_TOP_DOCUMENT->NEW_LINE .
  END-OF-DEFINITION .


  IF SO_ERDAT-HIGH IS NOT INITIAL .
    LV_TEXT = TEXT-H01 && | : | && SO_ERDAT-LOW  && | | && SO_ERDAT-HIGH .
    __ADD_TEXT LV_TEXT .
  ELSE.
    LV_TEXT = TEXT-H01 && | : | && SO_ERDAT-LOW."  && | | && SO_ERDAT-HIGH .
    __ADD_TEXT LV_TEXT .
  ENDIF.

  IF SO_ERNAM-HIGH IS NOT INITIAL .
    LV_TEXT = TEXT-H02 && | : | && SO_ERNAM-LOW  && | | && SO_ERNAM-HIGH .
    __ADD_TEXT LV_TEXT .
  ELSE.
    LV_TEXT = TEXT-H02 && | : | && SO_ERNAM-LOW."  && | | && SO_ERNAM-HIGH .
    __ADD_TEXT LV_TEXT .
  ENDIF.


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
*& Form GET_FIELDCATLOG_DATA_0100
*&---------------------------------------------------------------------*
FORM GET_FIELDCATLOG_DATA_0100 .

  DATA: LT_FIELDCAT TYPE KKBLO_T_FIELDCAT.
  CLEAR GT_FIELDCAT2[].

  CALL FUNCTION 'K_KKB_FIELDCAT_MERGE'
    EXPORTING
      I_CALLBACK_PROGRAM     = SY-REPID
      I_STRUCNAME            = 'ZCOS0340'
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
        ET_FIELDCAT_LVC   = GT_FIELDCAT2[]
      EXCEPTIONS
        IT_DATA_MISSING   = 1.
  ELSE.
    MESSAGE E020.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_FIELDCATLOG_DATA_0100
*&---------------------------------------------------------------------*
FORM MODIFY_FIELDCATLOG_DATA_0100 USING P_TYPE.

  DATA:  LV_TEXT(50).

  LOOP AT GT_FIELDCAT2 INTO GS_FIELDCAT.
    CLEAR: LV_TEXT.

    CASE GS_FIELDCAT-FIELDNAME.

      WHEN 'KTEXT'.
        LV_TEXT = TEXT-C08.

      WHEN 'POSID'.
        LV_TEXT = TEXT-C10.

      WHEN 'POST1'.
        LV_TEXT = TEXT-C09.

      WHEN 'KSTAR'.
        LV_TEXT = TEXT-C11.
        IF P_TYPE = 'A'.
          GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        ENDIF.

      WHEN 'KSTXT'.
        LV_TEXT = TEXT-C12.

        IF P_TYPE = 'A'.
          GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        ENDIF.

      WHEN 'S_KSTAR'.
        IF P_TYPE = 'P' OR P_TYPE = 'C'.
          GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        ENDIF.

        LV_TEXT = TEXT-C13.

      WHEN 'S_KSTXT'.
        LV_TEXT = TEXT-C14.
        IF P_TYPE = 'P' OR P_TYPE = 'C'.
          GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        ENDIF.

      WHEN 'R_KSTAR'.
        LV_TEXT = TEXT-C15.
        IF P_TYPE = 'P' OR P_TYPE = 'C'.
          GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        ENDIF.

      WHEN 'R_KSTXT'.
        LV_TEXT = TEXT-C16.
        IF P_TYPE = 'P' OR P_TYPE = 'C'.
          GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        ENDIF.

      WHEN 'AMOUNT'.
        LV_TEXT = TEXT-C17.

        IF P_TYPE = 'C'.
          GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        ENDIF.

      WHEN 'FAMOUNT'.
        LV_TEXT = TEXT-C18.
        IF P_TYPE = 'P' OR P_TYPE = 'A'.
          GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        ENDIF.

      WHEN 'IAMOUNT'.
        LV_TEXT = TEXT-C19.
        IF P_TYPE = 'P' OR P_TYPE = 'A'.
          GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        ENDIF.

      WHEN 'TAMOUNT'.
        LV_TEXT = TEXT-C20.
        IF P_TYPE = 'P' OR P_TYPE = 'A'.
          GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        ENDIF.

      WHEN OTHERS.

    ENDCASE.

    "Column Optimizer
    GS_FIELDCAT-COL_OPT = ABAP_TRUE.

    "-- Common attribute
    IF LV_TEXT IS NOT INITIAL.
      GS_FIELDCAT-COLTEXT   = LV_TEXT.
      GS_FIELDCAT-SCRTEXT_L = LV_TEXT.
      GS_FIELDCAT-SCRTEXT_M = LV_TEXT.
      GS_FIELDCAT-SCRTEXT_S = LV_TEXT.
    ENDIF.

    MODIFY GT_FIELDCAT2 FROM GS_FIELDCAT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_GRID_0100_01
*&---------------------------------------------------------------------*
FORM REFRESH_GRID_0100_01 .

  GS_STABLE-ROW = ABAP_TRUE. "Row
  GS_STABLE-COL = ABAP_TRUE. "column

  CALL METHOD GR_GRID_DATA->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = GS_STABLE
      I_SOFT_REFRESH = SPACE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_POST1_CODE_TXT
*&---------------------------------------------------------------------*
FORM GET_POST1_CODE_TXT  USING    P_ROBJNR TYPE J_OBJNR
                         CHANGING P_POSID  TYPE PS_POSID
                                  P_POST1  TYPE PS_POST1.

  SELECT SINGLE POSID, POST1 INTO (@P_POSID, @P_POST1)
    FROM PRPS
   WHERE OBJNR = @P_ROBJNR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_KOSTL_TEXT
*&---------------------------------------------------------------------*
FORM GET_KOSTL_TEXT  USING    P_KOSTL TYPE KOSTL
                              P_KOKRS TYPE KOKRS
                     CHANGING P_KTEXT TYPE KTEXT.

  SELECT SINGLE B~KTEXT INTO @P_KTEXT
    FROM CSKS AS A
    LEFT  JOIN CSKT AS B
      ON A~KOKRS = B~KOKRS
     AND A~KOSTL = B~KOSTL
     AND A~DATBI = B~DATBI
     AND B~SPRAS = @SY-LANGU
   WHERE A~KOKRS = @P_KOKRS
     AND A~KOSTL = @P_KOSTL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_URL
*&---------------------------------------------------------------------*
FORM CALL_URL .

  DATA LV_URL TYPE CHAR255.

  LOOP AT GT_ROWS INTO GS_ROW.

    READ TABLE GT_DISPLAY INDEX GS_ROW-INDEX.

    IF SY-SUBRC = 0.

      SELECT SINGLE URL INTO @LV_URL
        FROM ZCOT1210
       WHERE SYSID = @SY-SYSID
         AND GWTYP = @GT_DISPLAY-GWTYP.

      LV_URL = LV_URL && GT_DISPLAY-GWKEY.

      CALL FUNCTION 'CALL_BROWSER'
        EXPORTING
          URL                    = LV_URL
        EXCEPTIONS
          FRONTEND_NOT_SUPPORTED = 1
          FRONTEND_ERROR         = 2
          PROG_NOT_FOUND         = 3
          NO_BATCH               = 4
          UNSPECIFIED_ERROR      = 5
          OTHERS                 = 6.

    ENDIF.

  ENDLOOP.

  MESSAGE S057 WITH GT_DISPLAY-GWKEY.

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
