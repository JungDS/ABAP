*&---------------------------------------------------------------------*
*& Include          ZCOR0420NF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SELECT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SELECT_DATA .
  DATA : LV_SPMON   TYPE SPMON,
         LV_YEAR(4) TYPE N.
  CLEAR   : GT_COBK, GT_BKPF, GT_LIST, GR_DATE.
  REFRESH : GT_COBK, GT_BKPF, GT_LIST, GR_DATE.

* 전기년월
  GR_DATE(3) = 'IBT'.
  CONCATENATE S_SPMON-LOW  '01' INTO GR_DATE-LOW.

  IF S_SPMON-HIGH IS INITIAL.
    IF S_SPMON-LOW+4(2) = '12'.
      LV_YEAR = S_SPMON-LOW(4).
      LV_YEAR = LV_YEAR + 1.
      CONCATENATE LV_YEAR '01' INTO LV_SPMON.
    ELSE.
     LV_SPMON = S_SPMON-LOW + 1.
    ENDIF.
  ELSE.
    IF S_SPMON-HIGH+4(2) = '12'.
      LV_YEAR = S_SPMON-HIGH(4).
      LV_YEAR = LV_YEAR + 1.
      CONCATENATE LV_YEAR '01' INTO LV_SPMON.
    ELSE.
     LV_SPMON = S_SPMON-HIGH + 1.
    ENDIF.
  ENDIF.
  CONCATENATE LV_SPMON     '01' INTO GR_DATE-HIGH.
  GR_DATE-HIGH = GR_DATE-HIGH - 1.
  APPEND GR_DATE.

** BKPF  FI전표 기준....

* FI 전표(V:임시전표, Z: 임시전표취소)
  SELECT DISTINCT BUKRS BELNR BUDAT BLART BSTAT AWKEY GJAHR
    INTO CORRESPONDING FIELDS OF TABLE GT_BKPF
    FROM BKPF
   WHERE BUKRS IN S_BUKRS   "EQ P_BUKRS
     AND BELNR IN S_BELNR
     AND BUDAT IN GR_DATE
    " AND BLART IN ('Z6', 'Z3')
     AND BSTAT IN ('Z', 'V').
 IF GT_BKPF[] IS INITIAL.
   EXIT.
 ENDIF.

* CO전표
  SELECT A~KOKRS A~BELNR A~AWKEY A~REFBN A~AWORG B~PSPNR
    INTO CORRESPONDING FIELDS OF TABLE GT_COBK
    FROM COBK AS A INNER JOIN COEP  AS B
      ON A~BELNR = B~BELNR
        FOR ALL ENTRIES IN GT_BKPF
   WHERE A~KOKRS EQ P_KOKRS
     AND A~REFBN EQ GT_BKPF-BELNR
     AND A~AWTYP EQ 'BKPF'
     AND A~VRGNG EQ 'COIN'
     AND A~BELNR IN S_BELNR2.
  "   AND BLART IN ('Z6', 'Z3').

  PERFORM MODIFY_DATA.
  PERFORM SELLTAB_CONTROL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM MODIFY_DATA .
* ??? 아이콘 처리 확인필요
  LOOP AT GT_BKPF.
    MOVE-CORRESPONDING GT_BKPF TO GT_LIST.

    READ TABLE GT_COBK WITH KEY REFBN = GT_BKPF-BELNR
                                AWKEY = GT_BKPF-AWKEY.
* COBK전표 있을경우 BKPF-BSTAT = Z일 경우 적색
    IF SY-SUBRC EQ 0.
      GT_LIST-BELNR2 = GT_COBK-BELNR.
      GT_LIST-AWORG  = GT_COBK-AWORG.
      GT_LIST-PSPNR  = GT_COBK-PSPNR.


      IF GT_LIST-BSTAT = 'Z'.
        GT_LIST-ICON = '@5C@'. "적색
      ELSE.
        GT_LIST-ICON = '@5B@'.
      ENDIF.
* COBK전표 없을경우 BKPF-BSTAT = Z일 경우 녹색
*                   BKPF-BSTAT = V일 경우 녹색(COBK 있을수도 없을수도)
    ELSE.
      IF GT_LIST-BSTAT = 'Z'.
        GT_LIST-ICON = '@5B@'.   "녹색
      ENDIF.
    ENDIF.
* 적색 아닐경우 무조건 녹색으로.....흠....
    IF GT_LIST-ICON IS INITIAL.
      GT_LIST-ICON = '@5C@'.  "녹색
    ENDIF.
    APPEND GT_LIST. CLEAR GT_LIST.
  ENDLOOP.
  SORT GT_LIST BY BELNR AWORG BELNR2.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELLTAB_CONTROL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SELLTAB_CONTROL .
  DATA : LS_CELLTAB TYPE LVC_S_STYL,
         LT_CELLTAB TYPE LVC_T_STYL.
  LOOP AT GT_LIST WHERE ICON = '@5B@'.
    CLEAR LT_CELLTAB.
      LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      LS_CELLTAB-FIELDNAME = 'CHECK'.
      INSERT LS_CELLTAB INTO TABLE LT_CELLTAB.
    CLEAR GT_LIST-CELLTAB.
    INSERT LINES OF LT_CELLTAB INTO TABLE GT_LIST-CELLTAB.
    MODIFY GT_LIST. CLEAR GT_LIST.
  ENDLOOP.


 DATA V_BELNR LIKE BKPF-BELNR.


    LOOP AT GT_LIST WHERE ICON = '@5C@'.

         SELECT SINGLE BELNR
           INTO V_BELNR
           FROM BSEG  AS A INNER JOIN SKA1  AS B
             ON  A~HKONT  = B~SAKNR
          WHERE  BUKRS =  GT_LIST-BUKRS
               AND BELNR = GT_LIST-BELNR
               AND GJAHR = GT_LIST-GJAHR
               AND KTOPL = '1000'
               AND  KTOKS = 'PL'.


         IF  SY-SUBRC <>  0.

             GT_LIST-ICON = '@5B@'.
             GT_LIST-XBILK =  'X'.

         ENDIF.



    MODIFY GT_LIST TRANSPORTING ICON XBILK .
    CLEAR   GT_LIST.
    CLEAR V_BELNR.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CALL_SCREEN .
  IF GT_LIST[] IS INITIAL.
    MESSAGE S004 DISPLAY LIKE 'E'.
  ELSE.
    CALL SCREEN 2000.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM MAKE_FIELD_CATEGORY .
  CLEAR : GS_FCAT, G_POS, GT_FIELDCAT.
  REFRESH GT_FIELDCAT.

  DEFINE __DEF_FC_BASIC.
    G_POS  =  G_POS  +  1.
    GS_FCAT-COL_POS        = G_POS.
    GS_FCAT-FIELDNAME      = &1.
    GS_FCAT-COLTEXT        = &2.
    GS_FCAT-OUTPUTLEN      = &3.
    GS_FCAT-FIX_COLUMN     = &4.
    GS_FCAT-KEY            = &5.
    GS_FCAT-JUST           = &6.
    GS_FCAT-EDIT           = &7.
    GS_FCAT-CHECKBOX       = &8.
    GS_FCAT-CONVEXIT       = &9.
    APPEND    GS_FCAT  TO  GT_FIELDCAT.
  END-OF-DEFINITION.
*
  __DEF_FC_BASIC:
     'CHECK'  '선택'       '4'  'X' 'X' 'C' 'X' 'X' '',
     'ICON'   '점검'       '5'  'X' 'X' 'C' ''  ''  '',
     'BUKRS'  '회사코드'   '8'  'X' 'X' 'C' ''  ''  '',
     'BELNR'  'FI 전표'    '10' 'X' 'X' 'C' ''  ''  'ALPHA',
     'BUDAT'  '전기일자'   '10' ''  ''  'C' ''  ''  '',
     'BLART'  '전표유형'   '7'  ''  ''  'C' ''  ''  '',
     'BSTAT'  '전표상태'   '7'  ''  ''  'C' ''  ''  '',
     'BELNR2'  'CO 전표'    '10' ''  ''  'C' ''  ''  'ALPHA',
     'PSPNR' 'WBS'          '10' 'X' 'X' 'C' ''  ''  'ALPHA',
     'XBILK'  '손익계정 유무 '    '10' ''  ''  'C' ''  ''  ''.

ENDFORM.                    " MAKE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0030   text
*----------------------------------------------------------------------*
FORM EXCLUDE_FUNCTIONS USING    P_TABNAME.
  FIELD-SYMBOLS : <TABLE> TYPE UI_FUNCTIONS.

  DATA : LS_EXCLUDE   TYPE UI_FUNC.
  DATA : L_TABLE_NAME LIKE FELD-NAME.

  CONCATENATE P_TABNAME '[]' INTO  L_TABLE_NAME.
  ASSIGN     (L_TABLE_NAME)    TO <TABLE>.


  PERFORM P1000_APPEND_EXCLUDE_FUNCTIONS
  TABLES <TABLE>
  USING : CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO, " 실행취소 &LOCAL&UNDO
        CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,          " 행 카피.
        CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,      " 행 카피.
        CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,           " 가위.
        CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,    " 행삭제.
        CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,    " 행삽입.
        CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,    " 라인생성.
        CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW ,
        CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE ,         " 겹쳐쓰기.
        CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW. " 겹쳐쓰기.
ENDFORM.                    "EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  P1000_APPEND_EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<TABLE>  text
*      -->P_CL_GUI_ALV_GRID=>MC_FC_LOC_UND  text
*----------------------------------------------------------------------*
FORM P1000_APPEND_EXCLUDE_FUNCTIONS    TABLES P_TABLE USING P_VALUE.
  DATA : LS_EXCLUDE TYPE UI_FUNC.

  LS_EXCLUDE = P_VALUE.
  APPEND LS_EXCLUDE TO P_TABLE.
ENDFORM.                    "P1000_APPEND_EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  GRID_DISPLAY_PART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GRID_DISPLAY_PART .
  CLEAR : GS_VARIANT, GS_LAYOUT.

  GS_VARIANT-REPORT    = SY-REPID.
  GS_LAYOUT-SEL_MODE   = 'A'.
  GS_LAYOUT-NO_ROWMARK = 'X'.
  GS_LAYOUT-STYLEFNAME = 'CELLTAB'.

  CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT            = GS_LAYOUT
      IT_TOOLBAR_EXCLUDING = GT_EXCLUDE
      IS_VARIANT           = GS_VARIANT
    CHANGING
      IT_FIELDCATALOG      = GT_FIELDCAT[]
      IT_OUTTAB            = GT_LIST[].
ENDFORM.                    "GRID_DISPLAY_PART
*&---------------------------------------------------------------------*
*& Form EVENT_HANDLER_REGISTER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM EVENT_HANDLER_REGISTER .
  IF SY-BATCH IS INITIAL.
    CALL METHOD G_GRID->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.

    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
  ENDIF.
  CREATE OBJECT G_EVENT_RECEIVER.
  SET HANDLER G_EVENT_RECEIVER->HANDLE_TOOLBAR      FOR G_GRID.
  SET HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR G_GRID.
  SET HANDLER G_EVENT_RECEIVER->HANDLE_USER_COMMAND FOR G_GRID.
  SET HANDLER G_EVENT_RECEIVER->HANDLE_DOUBLE_CLICK FOR G_GRID.
  CALL METHOD G_GRID->SET_TOOLBAR_INTERACTIVE.
  LCL_ALV_GRID=>F_ALV = '1'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONTAINER_FREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONTAINER_FREE .
  IF G_GRID IS NOT INITIAL.
    CALL METHOD G_GRID->FREE.
    CLEAR  G_GRID.
  ENDIF.
ENDFORM.                    "CONTAINER_FREE
*&---------------------------------------------------------------------*
*&      Form  REFRESH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFRESH_DATA .
  CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      I_SOFT_REFRESH = 'X'
      IS_STABLE      = GV_SCROLL.
ENDFORM.                    " REFRESH_DATA
*&---------------------------------------------------------------------*
*& Form GRID_TOOLBAR_INCLUDEING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_OBJECT
*&      --> E_INTERACTIVE
*&---------------------------------------------------------------------*
FORM GRID_TOOLBAR_INCLUDEING  USING R_OBJECT
                                    TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
                                    R_INTERACTIVE.
DATA : LS_TOOLBAR  TYPE STB_BUTTON.

  CLEAR LS_TOOLBAR.
  LS_TOOLBAR-BUTN_TYPE = '3'.
  APPEND LS_TOOLBAR  TO R_OBJECT->MT_TOOLBAR.

  CLEAR: LS_TOOLBAR.
  LS_TOOLBAR-FUNCTION  = 'ADDL'.
  LS_TOOLBAR-ICON      = '@4B@'.
  LS_TOOLBAR-TEXT      = '전체선택'.
  LS_TOOLBAR-QUICKINFO = 'Add line'.
  APPEND LS_TOOLBAR TO R_OBJECT->MT_TOOLBAR.

  CLEAR LS_TOOLBAR.
  LS_TOOLBAR-BUTN_TYPE = '3'.
  APPEND LS_TOOLBAR  TO R_OBJECT->MT_TOOLBAR.

  CLEAR: LS_TOOLBAR.
  LS_TOOLBAR-FUNCTION  = 'DELL'.
  LS_TOOLBAR-ICON      = '@4D@'.
  LS_TOOLBAR-TEXT      = '전체선택 해제'.
  LS_TOOLBAR-QUICKINFO = '전체선택 해제'.
  APPEND LS_TOOLBAR TO R_OBJECT->MT_TOOLBAR.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALL_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ALL_CHECK .
  DATA: LT_FIDX      TYPE LVC_T_FIDX,
        LV_INDEX(10) TYPE N.
  DATA : BEGIN OF LT_LINE OCCURS 0,
           INDEX TYPE I,
         END OF LT_LINE.

  FIELD-SYMBOLS: <PT> TYPE ANY.

  CALL METHOD G_GRID->GET_FILTERED_ENTRIES
    IMPORTING
      ET_FILTERED_ENTRIES = LT_FIDX.

  LOOP AT LT_FIDX ASSIGNING  <PT>.
    WRITE <PT> TO LV_INDEX.
    LT_LINE-INDEX = LV_INDEX.
    APPEND LT_LINE. CLEAR LT_LINE.
  ENDLOOP.

  CLEAR GT_LIST-CHECK.
  MODIFY GT_LIST TRANSPORTING CHECK
   WHERE CHECK = 'X'.

  IF LT_LINE[] IS INITIAL.
    GT_LIST-CHECK = 'X'.
    MODIFY GT_LIST TRANSPORTING CHECK
     WHERE ICON = '@5C@'
       AND CHECK = ''.
  ELSE.
    LOOP AT GT_LIST WHERE ICON = '@5C@'.
      READ TABLE LT_LINE WITH KEY INDEX = SY-TABIX.
      IF SY-SUBRC NE 0.
        GT_LIST-CHECK = 'X'.
        MODIFY GT_LIST. CLEAR GT_LIST.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALL_CHECK_DELETE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ALL_CHECK_DELETE .
  CLEAR GT_LIST-CHECK.
  MODIFY GT_LIST TRANSPORTING CHECK
   WHERE CHECK = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GO_PBO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GO_PBO .
  CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
    EXPORTING
      NEW_CODE = 'ENTER'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_DATA .
  CLEAR GV_ERROR.
  READ TABLE GT_LIST WITH KEY CHECK = 'X'.
  IF SY-SUBRC NE 0.
    GV_ERROR = 'X'.
    MESSAGE S015 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE GT_LIST WITH KEY CHECK  = 'X'
                              BELNR2 = ''.
  IF SY-SUBRC EQ 0.
    GV_ERROR = 'X'.
    MESSAGE S000 WITH TEXT-M01 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CALL SCREEN 3000 STARTING AT 15  6
                     ENDING AT 40  7.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATA_CHANGED
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ER_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM DATA_CHANGED  USING RR_DATA_CHANGED
                         TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
  DATA: LS_MOD_CELLS   TYPE LVC_S_MODI,
        LV_COL(40).

  FIELD-SYMBOLS: <FM>.

  CLEAR: GT_LIST.
  LOOP AT RR_DATA_CHANGED->MT_GOOD_CELLS INTO LS_MOD_CELLS.
    READ TABLE GT_LIST INDEX LS_MOD_CELLS-ROW_ID.
      CONCATENATE 'GT_LIST-' LS_MOD_CELLS-FIELDNAME  INTO LV_COL.
      ASSIGN      (LV_COL)         TO       <FM>.
      <FM> = LS_MOD_CELLS-VALUE.

      CALL METHOD RR_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_MOD_CELLS-ROW_ID
          I_FIELDNAME = LS_MOD_CELLS-FIELDNAME
          I_VALUE     = <FM>.
      MODIFY GT_LIST INDEX LS_MOD_CELLS-ROW_ID.
  ENDLOOP.
  PERFORM REFRESH_DATA.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOUBLE_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW
*&      --> E_COLUMN
*&---------------------------------------------------------------------*
FORM DOUBLE_CLICK  USING   P_ROW
                           P_COLUMN.
  DATA LV_AWORG LIKE ACCHD-AWORG.

  READ TABLE GT_LIST INDEX P_ROW.
  CASE P_COLUMN.
    WHEN 'BELNR'.
      IF GT_LIST-BELNR IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE GT_LIST-BUKRS GT_LIST-GJAHR
             INTO LV_AWORG.

    WHEN 'BELNR2'.
      IF GT_LIST-BELNR2 IS INITIAL.
        EXIT.
      ENDIF.
      LV_AWORG = GT_LIST-AWORG.
  ENDCASE.

  CALL FUNCTION 'AC_DOCUMENT_RECORD'
    EXPORTING
      I_AWTYP = 'BKPF'
      I_AWREF = GT_LIST-BELNR
      I_AWORG = LV_AWORG.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZCOR0430_GO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ZCOR0430_GO .
  DATA : LV_BELNR LIKE COBK-BELNR,
         LV_AWORG LIKE COBK-AWORG.
  DATA : LV_PRO   LIKE SY-REPID VALUE 'ZCOR0430'.
  RANGES: LR_BELNR FOR COBK-BELNR.

  LR_BELNR-SIGN   = 'I'.
  LR_BELNR-OPTION = 'EQ'.
  LOOP AT GT_LIST WHERE CHECK = 'X'.
    IF LV_BELNR NE GT_LIST-BELNR
    OR LV_AWORG NE GT_LIST-AWORG.
      LR_BELNR-LOW = GT_LIST-BELNR2 .
      APPEND LR_BELNR.
    ENDIF.
    LV_BELNR = GT_LIST-BELNR.
    LV_AWORG = GT_LIST-AWORG.
  ENDLOOP.

SUBMIT (LV_PRO)
       USING SELECTION-SCREEN 1000
        WITH P_KOKRS = P_KOKRS
        WITH P_PROT  = 'X'
        WITH P_TEST  = GV_TEST
        WITH S_BELNR IN LR_BELNR
        AND RETURN.
ENDFORM.
