*&---------------------------------------------------------------------*
*& Include          ZCOR0370O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  DATA FCODE TYPE TABLE OF SY-UCOMM.

  REFRESH FCODE.

  CASE GV_MODE.

    WHEN 'S'.
      APPEND '&SAV'  TO FCODE.
      APPEND '&DELE' TO FCODE.

  ENDCASE.

  SET PF-STATUS 'PF_0100' EXCLUDING FCODE.
  SET TITLEBAR  'TT_0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_INIT_DISPLAY_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE ALV_INIT_DISPLAY_0100 OUTPUT.

  IF GR_GRID1 IS NOT BOUND.
    PERFORM CREATE_INSTANCE_0100.
    PERFORM INIT_LAYOUT_0100.
    PERFORM SET_GRID_EXCLUDE_0100.
    PERFORM ALV_SORT_0100.
    PERFORM APPEND_FIELDCAT_0100.
    PERFORM TOP_OF_PAGE_CREATE_OBJECT_0100.
    PERFORM MAKE_TOP_OF_PAGE_DATA_0100.
    PERFORM REGIST_ALV_EVENT_0100 USING GR_GRID1.
    PERFORM DISPLAY_ALV_GRID_0100.

  ELSE.
    PERFORM REFRESH_GRID_0100.

  ENDIF.

ENDMODULE.                 " ALV_INIT_DISPLAY_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  REFRESH_GRID_0100
*&---------------------------------------------------------------------*
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
FORM SAVE_DATA_RTN .

  DATA: LV_MESSAGE TYPE STRING.

  DATA: LT_ZFIT0621 TYPE TABLE OF ZFIT0621 WITH HEADER LINE.

  CALL METHOD GR_GRID1->CHECK_CHANGED_DATA.

  LOOP AT GT_DISPLAY INTO GS_DISPLAY.

    MOVE-CORRESPONDING GS_DISPLAY TO LT_ZFIT0621.

    LT_ZFIT0621-KOKRS = PA_KOKRS.
    LT_ZFIT0621-RYEAR = PA_GJAHR.
    LT_ZFIT0621-RVERS = PA_VERSN.
    LT_ZFIT0621-WRTTP = '01'.      "계획
    LT_ZFIT0621-CPUDT = SY-DATUM.
    LT_ZFIT0621-CPUTM = SY-UZEIT.
    LT_ZFIT0621-USNAM = SY-UNAME.

    APPEND LT_ZFIT0621.
    CLEAR  LT_ZFIT0621.

  ENDLOOP.

  TRY .

      DELETE FROM ZFIT0621 WHERE KOKRS  = @PA_KOKRS
                             AND RVERS  = @PA_VERSN
                             AND RYEAR  = @PA_GJAHR.

      INSERT ZFIT0621 FROM TABLE LT_ZFIT0621.

      COMMIT WORK.

      MESSAGE S007.

    CATCH CX_SY_SQL_ERROR INTO DATA(LR_ERROR).

      ROLLBACK WORK.

      LV_MESSAGE = LR_ERROR->GET_TEXT( ).
      MESSAGE S001 WITH LV_MESSAGE DISPLAY LIKE 'E'.

  ENDTRY.

ENDFORM.                    " SAVE_DATA_RTN
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

  CONCATENATE TEXT-003 ':' PA_GJAHR
        INTO LV_TEXT SEPARATED BY SPACE.

  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE.

  CONCATENATE TEXT-002 ':' PA_VERSN PA_VTXT
        INTO LV_TEXT SEPARATED BY SPACE.

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
