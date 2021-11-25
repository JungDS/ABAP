*&---------------------------------------------------------------------*
*& Include          ZCOR0480F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form F4_P_MONTH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F4_P_MONTH .
  DATA: BEGIN OF MF_DYNPFIELDS OCCURS 1.
          INCLUDE STRUCTURE DYNPREAD.
        DATA: END   OF MF_DYNPFIELDS.
  DATA: MF_RETURNCODE LIKE SY-SUBRC,
        MF_MONAT      LIKE ISELLIST-MONTH,
        MF_HLP_REPID  LIKE SY-REPID.
  FIELD-SYMBOLS: <MF_FELD>.

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
        INVALID_ABAPWORKAREA = 01
        INVALID_DYNPROFIELD  = 02
        INVALID_DYNPRONAME   = 03
        INVALID_DYNPRONUMMER = 04
        INVALID_REQUEST      = 05
        NO_FIELDDESCRIPTION  = 06
        UNDEFIND_ERROR       = 07.
    IF SY-SUBRC = 3.
      MF_HLP_REPID = 'SAPLALDB'.
    ELSE.
      READ TABLE MF_DYNPFIELDS INDEX 1.
      TRANSLATE MF_DYNPFIELDS-FIELDVALUE USING '_ '.
      EXIT.
    ENDIF.
  ENDDO.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'CONVERSION_EXIT_PERI_INPUT'
      EXPORTING
        INPUT  = MF_DYNPFIELDS-FIELDVALUE
      IMPORTING
        OUTPUT = MF_MONAT.
    IF MF_MONAT IS INITIAL.
      MF_MONAT = SY-DATLO(6).
    ENDIF.
    CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
      EXPORTING
        ACTUAL_MONTH               = MF_MONAT
      IMPORTING
        SELECTED_MONTH             = MF_MONAT
        RETURN_CODE                = MF_RETURNCODE
      EXCEPTIONS
        FACTORY_CALENDAR_NOT_FOUND = 01
        HOLIDAY_CALENDAR_NOT_FOUND = 02
        MONTH_NOT_FOUND            = 03.
    IF SY-SUBRC = 0 AND MF_RETURNCODE = 0.
      CALL FUNCTION 'CONVERSION_EXIT_PERI_OUTPUT'
        EXPORTING
          INPUT  = MF_MONAT
        IMPORTING
          OUTPUT = MF_DYNPFIELDS-FIELDVALUE.
**** 날짜 . 없애기 추가
*      REPLACE '.' IN MF_DYNPFIELDS-FIELDVALUE  WITH ''.

      CONDENSE MF_DYNPFIELDS-FIELDVALUE.
******

      COLLECT MF_DYNPFIELDS.
      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          DYNAME               = MF_HLP_REPID
          DYNUMB               = SY-DYNNR
        TABLES
          DYNPFIELDS           = MF_DYNPFIELDS
        EXCEPTIONS
          INVALID_ABAPWORKAREA = 01
          INVALID_DYNPROFIELD  = 02
          INVALID_DYNPRONAME   = 03
          INVALID_DYNPRONUMMER = 04
          INVALID_REQUEST      = 05
          NO_FIELDDESCRIPTION  = 06
          UNDEFIND_ERROR       = 07.
    ENDIF.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form MODIFY_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM MODIFY_SCREEN .
  LOOP AT SCREEN.


    IF SCREEN-NAME = 'PA_KOKRS' OR
     SCREEN-NAME = 'PA_MTART'  OR
     SCREEN-NAME = 'PA_BUKRS'.

      SCREEN-INPUT = 0.


    ENDIF.


    MODIFY SCREEN.
  ENDLOOP.


ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT .
  I_SPLIT = 'X'.

  CLEAR GV_DATUM.
  "전월
  _GET_LAST_DATE SY-DATUM '00' '01' '-' '00' GV_DATUM.
  PA_MONTH  = GV_DATUM(6).


  GV_REPID = SY-REPID.
  SELECT SINGLE BEZEI, WAERS INTO (@PA_KTXT, @GV_WAERS)
    FROM TKA01
   WHERE KOKRS = '1000'.

  SET PARAMETER ID 'CAC' FIELD PA_KOKRS.


  SELECT SINGLE BUTXT INTO @PA_BUTXT
    FROM T001
   WHERE BUKRS = @PA_BUKRS.


  GS_FUNTXT-ICON_ID   = ICON_INFORMATION.
*  GS_FUNTXT-QUICKINFO = 'HELP'.
*  GS_FUNTXT-ICON_TEXT = 'HELP'.

  SSCRFIELDS-FUNCTXT_01 = GS_FUNTXT.



ENDFORM.                    " INIT
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .
  REFRESH : GT_1260,  GT_SDMM .

  CLEAR   :   GT_SDMM.
*

  PERFORM GET_MMT0600.

  PERFORM SET_RANGE_DATA.

***좌측 alv 용   co 결산 가져오기

  SELECT *
     FROM ZCOT1260
     WHERE BUKRS = @PA_BUKRS
       AND SPMON = @PA_MONTH
       AND WERKS = @PA_WERKS
     INTO CORRESPONDING FIELDS OF TABLE @GT_1260.

*
*MATNR  자재 번호
*WERKS  플랜트
*VRKME  판매 단위
*FKIMG  실제 대금청구 수량
*MEINS  기본 단위
*NETWR  문서통화의 청구품목 정가
*WAERK  SD 문서 통화
*NETWR_L  문서통화의 청구품목 정가
*WAERS  SD 문서 통화


** 우측 ALV

* SD 플래트 별 판매수량 금액 가져오기
  PERFORM GET_SALES_QTY_NETWR.

  LOOP AT GT_SD_RESULT2 ASSIGNING FIELD-SYMBOL(<SD>).

    GS_SDMM-BUKRS   = PA_BUKRS.
    GS_SDMM-WERKS   = PA_WERKS.

*    GS_SDMM-TWAER   = <SD>-WAERS. " 항상 KRW
    GS_SDMM-SPMON   =  PA_MONTH.

    GS_SDMM-FMATNR  = <SD>-MATNR.  "제품
*    GS_SDMM-SMEINS  = <SD>-VRKME. " KG
    GS_SDMM-SMENGE  = <SD>-FKIMG. " 수량
*    GS_SDMM-SWRBTR  = <SD>-NETWR_L. " 매출액


    COLLECT GS_SDMM  INTO GT_SDMM.
    CLEAR GS_SDMM.

  ENDLOOP.

* mm 생산 수량 가져오기
  PERFORM GET_MM_QTY.

  CLEAR GS_SDMM.

  LOOP AT GT_RAW ASSIGNING FIELD-SYMBOL(<FERT>).

*    GS_SDMM-TWAER = 'KRW'.

    GS_SDMM-BUKRS  = PA_BUKRS.
    GS_SDMM-WERKS  = PA_WERKS.
    GS_SDMM-SPMON  = PA_MONTH.
    GS_SDMM-FMATNR = <FERT>-FMATNR.
    GS_SDMM-FMENGE = <FERT>-FMENGE.
*    GS_SDMM-EMEINS = <FERT>-MEINS.

    COLLECT GS_SDMM  INTO GT_SDMM.
    CLEAR GS_SDMM.

  ENDLOOP.

*MM  기말 수량 가져오기
  PERFORM GET_LASTDAY_STOCK_MM.  "

**MATNR	자재 번호
**LABST	평가된 가용 재고
**MEINS	기본 단위
**LFGJA	현재 기간의 회계연도
**LFMON	현재 기간 (전기기간)
**SPMON	분석기간 - 월

  SORT GT_STOCK BY MATNR.
  DELETE GT_STOCK WHERE LABST IS INITIAL.
  CLEAR GS_SDMM.

  LOOP AT GT_STOCK ASSIGNING FIELD-SYMBOL(<STOCK>).

    GS_SDMM-BUKRS = PA_BUKRS.
    GS_SDMM-WERKS = PA_WERKS.
    GS_SDMM-SPMON = PA_MONTH.
*    GS_SDMM-TWAER = 'KRW'.

    GS_SDMM-FMATNR = <STOCK>-MATNR.
    GS_SDMM-EMENGE = <STOCK>-LABST.
*    GS_SDMM-EMEINS = <STOCK>-MEINS.

    COLLECT GS_SDMM  INTO GT_SDMM.
    CLEAR GS_SDMM.

  ENDLOOP.


*마스터의 단위 넣기

  LOOP AT GT_SDMM ASSIGNING FIELD-SYMBOL(<SM>).

    SELECT SINGLE MEINS MTART
      INTO ( <SM>-FMEINS, <SM>-MTART )
      FROM MARA
       WHERE MATNR = <SM>-FMATNR.

    IF SY-SUBRC EQ 0.

      <SM>-SMEINS = <SM>-FMEINS. "판매단위
      <SM>-EMEINS = <SM>-FMEINS. "기말단위

    ENDIF.

*    제품명
    SELECT SINGLE MAKTX
      INTO <SM>-FMATNR_MAKTX
      FROM MAKT
    WHERE MATNR = <SM>-FMATNR
      AND SPRAS = SY-LANGU.

  ENDLOOP.


  DELETE GT_SDMM WHERE MTART NE 'UNB3'.  " 제품 아니면 삭제

  SORT GT_1260 BY FMATNR.
  SORT GT_SDMM  BY FMATNR.


*CO 결산 테이블 기준 대사

  LOOP AT GT_1260 ASSIGNING FIELD-SYMBOL(<ZZ>).

*    제품명
    SELECT SINGLE MAKTX
      INTO <ZZ>-FMATNR_MAKTX
      FROM MAKT
    WHERE MATNR = <ZZ>-FMATNR
      AND SPRAS = SY-LANGU.

    READ TABLE GT_SDMM ASSIGNING FIELD-SYMBOL(<KK>) WITH KEY FMATNR = <ZZ>-FMATNR BINARY SEARCH.

    IF SY-SUBRC EQ 0.

      IF <ZZ>-FMENGE = <KK>-FMENGE  AND  "생산수량
         <ZZ>-SMENGE = <KK>-SMENGE  AND  "판매수량
         <ZZ>-EMENGE = <KK>-EMENGE .  "기말수량    일치.......!!

        <ZZ>-LIGHTS =  '3'.
        <ZZ>-MSG = 'CO / SD,MM   생산,판매,기말수량  모두 일치함'.

        <KK>-LIGHTS =  '3'.
        <KK>-MSG = 'CO / SD,MM   생산,판매,기말수량  모두 일치함'.

      ELSE. "  불일치............!!!
        IF   <ZZ>-FMENGE NE <KK>-FMENGE.
          <ZZ>-MSG = ' 생산수량, '.
          <KK>-MSG = ' 생산수량, '.
        ENDIF.

        IF <ZZ>-SMENGE NE <KK>-SMENGE.
          <ZZ>-MSG =  <ZZ>-MSG  &&  ' 판매수량, '.
          <KK>-MSG =  <ZZ>-MSG  &&  ' 판매수량, '.
        ENDIF.

        IF <ZZ>-EMENGE NE <KK>-EMENGE.
          <ZZ>-MSG =  <ZZ>-MSG  &&  ' 기말 수량 '.
          <KK>-MSG =  <ZZ>-MSG  &&  ' 기말 수량 '.
        ENDIF.


        <ZZ>-MSG =  <ZZ>-MSG  &&  ' 불일치함 '.
        <KK>-MSG =  <ZZ>-MSG  &&  ' 불일치함 '.


        <ZZ>-LIGHTS =  '1'.    " co는 생산 이 있거나 전월재고가 있어야 판매단가가 계산됨
        " 생산 없이  기말 수량 만 있는 오류 자료
        " 생산 없이  판매 수량 만 있는 오류 자료

        <KK>-LIGHTS =  '1'.
      ENDIF.


    ELSE.

      <ZZ>-LIGHTS =  '1'.
      <ZZ>-MSG = 'SD MM 제공 펑션 재실행 결과 미존재 불일치 .'. " 아마도 mig 재고 거나  해외에서 환입된  양품...

    ENDIF.


  ENDLOOP.



  LOOP AT GT_SDMM ASSIGNING FIELD-SYMBOL(<GG>) WHERE LIGHTS IS  INITIAL.

    <GG>-MSG = 'CO 결산 자료에 없음, '.

  ENDLOOP.



ENDFORM.                    " GET_DATA



*&---------------------------------------------------------------------
*
*& Form CONV_AMOUNT_TO_SAP
*&---------------------------------------------------------------------
*
*& text
*&---------------------------------------------------------------------
*
*&      --> GS_EXCEL_TOTAL
*&---------------------------------------------------------------------
*
FORM CONV_AMOUNT_TO_SAP  USING    P_AMT.
  DATA :  L_DMBTR TYPE DMBTR .

  CLEAR L_DMBTR.
  CALL FUNCTION 'CURRENCY_AMOUNT_IDOC_TO_SAP'
    EXPORTING
      CURRENCY    = 'KRW'
      IDOC_AMOUNT = P_AMT
    IMPORTING
      SAP_AMOUNT  = L_DMBTR.

  P_AMT = L_DMBTR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_CONFIRM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> LV_ANSWER
*&---------------------------------------------------------------------*
FORM POPUP_CONFIRM  USING    P_TITLE
                             P_TEXTLINE1
                             P_TEXTLINE2
                             P_ANSWER.
*  CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
*    EXPORTING
*      TEXTLINE1     = P_TEXTLINE1
*      TEXTLINE2     = P_TEXTLINE2
*      TITEL         = P_TITLE
*      START_COLUMN  = 25
*      START_ROW     = 6
*      DEFAULTOPTION = 'N'
*    IMPORTING
*      ANSWER        = P_ANSWER.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR      = P_TITLE
      TEXT_QUESTION = P_TEXTLINE1
    IMPORTING
      ANSWER        = P_ANSWER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_BL1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CHECK_BL1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_BL1 .


  SELECT SINGLE NAME1
     FROM T001W AS A INNER JOIN  T024E  AS B
       ON A~EKORG = B~EKORG
   WHERE B~BUKRS  = @PA_BUKRS
     AND A~WERKS =  @PA_WERKS
     INTO @PA_WENM.

  IF SY-SUBRC EQ 0.
    CLEAR GV_EXIT .

    GV_WE_NM = PA_WENM.

  ELSE.
    SET CURSOR FIELD 'PA_WERKS'.
    MESSAGE E027  WITH TEXT-E02.

  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCR_USER_COMMAND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SCR_USER_COMMAND .

  CASE SSCRFIELDS-UCOMM.
    WHEN 'FC01'.
      PERFORM CALL_POPUP_HELP(ZCAR9000) USING SY-REPID SY-DYNNR SY-LANGU ''.

    WHEN 'FC02'.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_CONTAINER_ITEM1
*&---------------------------------------------------------------------*
FORM SET_CONTAINER_ITEM1 .

  IF G_GRID1 IS INITIAL.
    GS_O_LAYOUT-REPORT = G_REPID_C = SY-REPID .
    CLEAR : GS_LAYOUT.
*
    IF I_SPLIT EQ SPACE.
      CALL METHOD G_SPLIT_CONTAINER->GET_CONTAINER
        EXPORTING
          ROW       = 1
          COLUMN    = 1
        RECEIVING
          CONTAINER = G_GUI_CONTAINER1.
    ELSE.
      CALL METHOD G_SPLIT_CONTAINER->GET_CONTAINER
        EXPORTING
          ROW       = 1
          COLUMN    = 1
        RECEIVING
          CONTAINER = G_GUI_CONTAINER1.
    ENDIF.
*
    CREATE OBJECT G_GRID1
      EXPORTING
        I_PARENT = G_GUI_CONTAINER1.
*
    CLEAR : GS_LAYOUT.
    PERFORM SET_LAYOUT_CON USING 'LIGHTS' ' ' 'D' ' '
                                 'COLOR' ' ' 'BOX' 'X' '1'.
*
    PERFORM SET_INPUT_CON USING G_GRID1 'X' G_INPUT.
*
*    PERFORM SET_SORT_C USING: '1' 'APPR_DATE'  'X' ' ' ' ' ' ' ' ' ' '.
*
    PERFORM SET_TOOLBAR.
*
    PERFORM SET_GRID_EVENTS USING G_GRID1 '1'.
*
    PERFORM GET_FILEDCAT_ALV1 USING GT_FIELDCAT[].
*
    PERFORM SORT_SETTING USING :
             'S' 'FIELDNAME' 'FMATNR',
             ' ' 'SPOS'      '1',
             ' ' 'UP'        'X',
             'E' 'SUBTOT'    'X'.

    GS_O_LAYOUT-LOG_GROUP = 'A'.
    PERFORM CALL_GRID_DISPLAY   TABLES GT_1260[]
                                USING  G_GRID1.
  ELSE.
    PERFORM GET_FILEDCAT_ALV1 USING GT_FIELDCAT[].
    PERFORM SORT_SETTING USING :
     'S' 'FIELDNAME' 'FMATNR',
     ' ' 'SPOS'      '1',
     ' ' 'UP'        'X',
     'E' 'SUBTOT'    'X'.
*
    GS_O_LAYOUT-LOG_GROUP = 'A'.
    PERFORM CALL_GRID_DISPLAY   TABLES GT_1260[]
                                USING  G_GRID1.
  ENDIF.
*
ENDFORM.                    " SET_CONTAINER_ITEM1

*&---------------------------------------------------------------------*
*&      Form  SET_TOOLBAR
*&---------------------------------------------------------------------*
FORM SET_TOOLBAR .

  CLEAR: GT_EXCL_FUNC, GT_EXCL_FUNC[].
*  APPEND CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL          TO GT_EXCL_FUNC.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW    TO GT_EXCL_FUNC.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY          TO GT_EXCL_FUNC.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW      TO GT_EXCL_FUNC.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_CUT           TO GT_EXCL_FUNC.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW    TO GT_EXCL_FUNC.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW    TO GT_EXCL_FUNC.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW      TO GT_EXCL_FUNC.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE         TO GT_EXCL_FUNC.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW TO GT_EXCL_FUNC.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO          TO GT_EXCL_FUNC.

ENDFORM.                    " SET_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  SET_GRID_EVENTS
*&---------------------------------------------------------------------*
FORM SET_GRID_EVENTS USING P_GRID TYPE REF TO CL_GUI_ALV_GRID P_NUM.

  DATA : P_OBJECT          TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET,
         P_ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL,
         PS_ROW_NO         TYPE LVC_S_ROID,
         PR_EVENT_DATA     TYPE REF TO CL_ALV_EVENT_DATA,
         PT_BAD_CELLS      TYPE LVC_T_MODI,
         PE_ROW	           TYPE LVC_S_ROW,
         PE_COLUMN         TYPE LVC_S_COL.


  CREATE OBJECT G_EVENTS.
*
* HOTSPOT CLICK.
  SET HANDLER G_EVENTS->HOTSPOT_CLICK FOR P_GRID.
  PERFORM EVENT_HOTSPOT_CLICK USING PE_ROW PE_COLUMN ''.
** DATA CHANGED
*  SET HANDLER G_EVENTS->DATA_CHANGED FOR P_GRID.
*  PERFORM EVENT_DATA_CHANGED USING P_ER_DATA_CHANGED '' '' '' '' ''.
* 버튼제어..
  IF P_NUM EQ '2'.
    SET HANDLER G_EVENTS->TOOLBAR FOR P_GRID.
    PERFORM EVENT_TOOLBAR USING P_OBJECT '' '' .
  ENDIF.
*
*  SET HANDLER G_EVENTS->DOUBLE_CLICK FOR P_GRID.
*  PERFORM EVENT_DOUBLE_CLICK USING PE_ROW PE_COLUMN PS_ROW_NO '' .
*
  SET HANDLER G_EVENTS->USER_COMMAND FOR P_GRID.
  PERFORM EVENT_UCOMM USING SY-UCOMM ''.
*
*  SET HANDLER G_EVENTS->DATA_CHANGED_FINISHED FOR P_GRID.
*  PERFORM EVENT_DATA_CHANGED_FINIS USING '' .

ENDFORM.                    " SET_GRID_EVENTS
*&---------------------------------------------------------------------*
*&      Form  EVENT_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM EVENT_HOTSPOT_CLICK  USING    PS_LVC_S_ROW TYPE LVC_S_ROW
                                   PS_LVC_S_COL TYPE LVC_S_COL
                                   P_CHECK.
  CHECK P_CHECK EQ 'X'.
* HOTSPOT 정의

  CASE PS_LVC_S_COL-FIELDNAME.
    WHEN 'BELNR'.
      READ TABLE GT_1260 INDEX PS_LVC_S_ROW-INDEX.
*      CHECK SY-SUBRC EQ 0 AND IT_1200-BELNR IS NOT INITIAL.
*      SET PARAMETER ID 'BUK' FIELD IT_1200-BUKRS.
*      SET PARAMETER ID 'BLN' FIELD IT_1200-BELNR.
*      SET PARAMETER ID 'GJR' FIELD IT_1200-GJAHR.
*      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ENDCASE.

*
ENDFORM.                    " EVENT_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Form  EVENT_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM EVENT_HOTSPOT_CLICK2  USING    PS_LVC_S_ROW TYPE LVC_S_ROW
                                   PS_LVC_S_COL TYPE LVC_S_COL
                                   P_CHECK.
  CHECK P_CHECK EQ 'X'.
* HOTSPOT 정의

  CASE PS_LVC_S_COL-FIELDNAME.

    WHEN 'BELNR'.
      READ TABLE GT_SDMM INDEX PS_LVC_S_ROW-INDEX.
      CHECK SY-SUBRC EQ 0.
*      SET PARAMETER ID 'BUK' FIELD IT_1200-BUKRS.
*      SET PARAMETER ID 'BLN' FIELD IT_1200-BELNR.
*      SET PARAMETER ID 'GJR' FIELD IT_1200-GJAHR.
*      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

  ENDCASE.

*
ENDFORM.                    " EVENT_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Form  EVENT_TOOLBAR
*&---------------------------------------------------------------------*
FORM EVENT_TOOLBAR  USING    P_OBJECT TYPE
                                         REF TO CL_ALV_EVENT_TOOLBAR_SET
                             P_INTERACTIVE
                             P_CHECK.
  CHECK P_CHECK EQ 'X'.
  CLEAR GS_TOOLBAR.
*  D_TOOLBAR: 'ALL' '@I2@' '0' '' '전체조회' '전체조회' '' P_OBJECT.
*
*
ENDFORM.                    " EVENT_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  EVENT_UCOMM
*&---------------------------------------------------------------------*
FORM EVENT_UCOMM  USING    P_UCOMM
                           P_CHECK.

  CHECK P_CHECK EQ 'X'.

  CASE P_UCOMM.
    WHEN 'ALL'.
*      IT_1200[] = IT_1200_ALL[].
      CALL METHOD G_GRID2->REFRESH_TABLE_DISPLAY.
  ENDCASE.


ENDFORM.                    " EVENT_UCOMM
*&---------------------------------------------------------------------*
*&      Form  GET_FILEDCAT_ALV1
*&---------------------------------------------------------------------*
FORM GET_FILEDCAT_ALV1  USING GT_FIELDCAT TYPE LVC_T_FCAT.


  PERFORM GET_FIELDCATLOG_DATA(ZCAR9000) TABLES GT_FIELDCAT
                                        USING 'ZCOS0480'
                                           IF FOUND.

  PERFORM MODIFY_FIELDCATLOG_DATA.

ENDFORM.                    " GET_FILEDCAT_ALV1
*&---------------------------------------------------------------------*
*&      Form  SET_CONTAINER_ITEM2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_CONTAINER_ITEM2 .


  IF G_GRID2 IS INITIAL.
    GS_O_LAYOUT-REPORT = G_REPID_C = SY-REPID .
    CLEAR : GS_LAYOUT.
*
    IF I_SPLIT EQ SPACE.
      CALL METHOD G_SPLIT_CONTAINER->GET_CONTAINER
        EXPORTING
          ROW       = 1
          COLUMN    = 2
        RECEIVING
          CONTAINER = G_GUI_CONTAINER2.
    ELSE.
      CALL METHOD G_SPLIT_CONTAINER->GET_CONTAINER
        EXPORTING
          ROW       = 1
          COLUMN    = 2
        RECEIVING
          CONTAINER = G_GUI_CONTAINER2.
    ENDIF.
*
    CREATE OBJECT G_GRID2
      EXPORTING
        I_PARENT = G_GUI_CONTAINER2.
*
    CLEAR : GS_LAYOUT.
    PERFORM SET_LAYOUT_CON USING 'LIGHTS' ' ' 'D' ' '
                                 'COLOR' ' ' 'BOX' 'X' '2'.
*
    PERFORM SET_INPUT_CON USING G_GRID2 'X' G_INPUT.
*
    CLEAR : GT_SORT[].
*    PERFORM SET_SORT_C USING: '1' 'APPR_DATE'  'X' ' ' ' ' ' ' ' ' ' '.
*
    PERFORM SET_TOOLBAR.
*
    PERFORM SET_GRID_EVENTS USING G_GRID2 '2'.
*
    PERFORM GET_FILEDCAT_ALV2 USING GT_FIELDCAT[].
*
    PERFORM SORT_SETTING USING :
     'S' 'FIELDNAME' 'FMATNR',
     ' ' 'SPOS'      '1',
     ' ' 'UP'        'X',
     'E' 'SUBTOT'    'X'.
    GS_O_LAYOUT-LOG_GROUP = 'B'.
    PERFORM CALL_GRID_DISPLAY   TABLES GT_SDMM[]
                                USING  G_GRID2.
  ELSE.
    PERFORM GET_FILEDCAT_ALV2 USING GT_FIELDCAT[].
*
    PERFORM SORT_SETTING USING :
     'S' 'FIELDNAME' 'FMATNR',
     ' ' 'SPOS'      '1',
     ' ' 'UP'        'X',
     'E' 'SUBTOT'    'X'.
    GS_O_LAYOUT-LOG_GROUP = 'B'.
    PERFORM CALL_GRID_DISPLAY   TABLES GT_SDMM[]
                                USING  G_GRID2.
  ENDIF.

ENDFORM.                    " SET_CONTAINER_ITEM2
*&---------------------------------------------------------------------*
*&      Form  GET_FILEDCAT_ALV2
*&---------------------------------------------------------------------*
FORM GET_FILEDCAT_ALV2  USING GT_FIELDCAT TYPE LVC_T_FCAT.

*  DATA : LS_FIELDCAT TYPE LVC_S_FCAT.
*  DATA : LT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
*         L_FIELDCAT  TYPE SLIS_FIELDCAT_ALV.
*
*  DEFINE GET_TEXT.
*    LS_FIELDCAT-REPTEXT    = &1.
*    LS_FIELDCAT-SCRTEXT_L  = &1.
*    LS_FIELDCAT-SCRTEXT_M  = &1.
*    LS_FIELDCAT-SCRTEXT_S  = &1.
*  END-OF-DEFINITION.
*
*  CLEAR : LT_FIELDCAT ,LT_FIELDCAT[].
*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
*      I_PROGRAM_NAME     = G_REPID_C
*      I_INTERNAL_TABNAME = 'GT_SDMM'
*      I_BYPASSING_BUFFER = 'X'
*      I_INCLNAME         = G_REPID_C
*    CHANGING
*      CT_FIELDCAT        = LT_FIELDCAT[].
*
*  CLEAR : GT_FIELDCAT , GT_FIELDCAT[].
**
**
*  LOOP AT LT_FIELDCAT INTO L_FIELDCAT.
*    CLEAR : LS_FIELDCAT.
*    MOVE-CORRESPONDING L_FIELDCAT TO LS_FIELDCAT.
*    MOVE: L_FIELDCAT-SELTEXT_S     TO LS_FIELDCAT-SCRTEXT_S ,
*          L_FIELDCAT-SELTEXT_M     TO LS_FIELDCAT-SCRTEXT_M ,
*          L_FIELDCAT-SELTEXT_L     TO LS_FIELDCAT-SCRTEXT_L ,
*          'M'                      TO LS_FIELDCAT-COLDDICTXT  ,
*          L_FIELDCAT-REF_TABNAME   TO LS_FIELDCAT-REF_TABLE ,
*          L_FIELDCAT-REF_FIELDNAME TO LS_FIELDCAT-REF_FIELD.
**
*    CASE LS_FIELDCAT-FIELDNAME.
*
*      WHEN 'LIGHTS' OR 'BOX'.
*        LS_FIELDCAT-NO_OUT     = 'X'.
*      WHEN 'BPERNR' OR 'BERDAT' OR 'BERZET' OR 'DELFLG' OR
*            'DELCODE' OR 'DERDAT' OR 'DERZET' OR 'ROUTE' OR
*            'BELNR_OLD' OR 'GJAHR_OLD' OR 'VATYN' OR 'DPERNR' OR
*            'HKONT' OR 'KOSTL' OR 'MWSKZ' OR 'SGTXT' OR
*            'COMMT' OR 'BUZEI' OR 'FLAG1' OR 'SEND_FLAG' OR
*            'ERDAT' OR 'ERZET' OR 'ERNAM' OR 'ERID' OR
*            'AEDAT' OR 'AEZET' OR 'AENAM' OR 'AEID'.
*        LS_FIELDCAT-NO_OUT  = 'X'.
*      WHEN 'APPR_TYPE_TXT'.
*        LS_FIELDCAT-SCRTEXT_M = '승인/취소'.
*      WHEN 'STATS_TXT'.
*        LS_FIELDCAT-SCRTEXT_M = '승인/매입'.
*    ENDCASE.
**
**   회사코드통화...
*    IF LS_FIELDCAT-DATATYPE EQ 'CURR'.
*      LS_FIELDCAT-CURRENCY   = 'KRW'.
*      IF LS_FIELDCAT-FIELDNAME EQ 'FTOTAL'.
*        LS_FIELDCAT-CURRENCY   = ''.
*        LS_FIELDCAT-CFIELDNAME = 'FWAERS'.
*        LS_FIELDCAT-SCRTEXT_M = '승인통화'.
*      ENDIF.
*      LS_FIELDCAT-DO_SUM     = 'X'.
*    ENDIF.
*
*    INSERT LS_FIELDCAT INTO TABLE GT_FIELDCAT.
*
*  ENDLOOP.




  PERFORM GET_FIELDCATLOG_DATA(ZCAR9000) TABLES GT_FIELDCAT
                                        USING 'ZCOS0480'
                                           IF FOUND.

  PERFORM MODIFY_FIELDCATLOG_DATA.


ENDFORM.                    " GET_FILEDCAT_ALV2
*&---------------------------------------------------------------------*



*&---------------------------------------------------------------------*
*&      Form  GET_SELECTED_ROW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_GRID1  text
*      -->P_L_INDEX1  text
*----------------------------------------------------------------------*
FORM GET_SELECTED_ROW  USING P_GRID TYPE REF TO CL_GUI_ALV_GRID
                             P_INDEX1.
  DATA: LT_ROWS TYPE LVC_T_ROW,
        LS_ROWS TYPE LVC_S_ROW.
*
  DATA : CNT TYPE I.
* 선택된 자료 가져오기.
  CALL METHOD P_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_ROWS.
*
  DESCRIBE TABLE LT_ROWS LINES CNT.
  IF CNT EQ '1'.
    LOOP AT LT_ROWS INTO LS_ROWS.
      P_INDEX1 = LS_ROWS-INDEX.
    ENDLOOP.
  ELSE.
    P_INDEX1 = 0 .
  ENDIF.

ENDFORM.                    " GET_SELECTED_ROW
*

*&---------------------------------------------------------------------*
*& Form SORT_SETTING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM SORT_SETTING  USING  P_GUB
                                                 P_FNAME
                                                 P_CON.
  DATA : L_COL(40).

  FIELD-SYMBOLS <FS>.

  IF P_GUB = 'S'.
    CLEAR GS_SORT.

    CONCATENATE 'GS_SORT-' P_FNAME  INTO L_COL.
    ASSIGN      (L_COL)         TO       <FS>.
    MOVE         P_CON          TO       <FS>.

    EXIT.
  ENDIF.

  "속성 MOVE
  CONCATENATE 'GS_SORT-' P_FNAME  INTO L_COL.
  ASSIGN      (L_COL)         TO       <FS>.
  MOVE         P_CON          TO       <FS>.

  "DATA  APPEND
  CHECK  P_GUB = 'E'.
  APPEND GS_SORT TO GT_SORT.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_SALES_QTY_NETWR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_SALES_QTY_NETWR .


  E_SPMON = PA_MONTH.
  E_WERKS = PA_WERKS.


  CLEAR : GT_SD_RESULT, GT_SD_RESULT[].
  CLEAR : GT_SD_RESULT2, GT_SD_RESULT2[].

  TRY.

      CALL FUNCTION 'ZSD_SALES_QTY_NETWR_CALCULATE'
        EXPORTING
          I_SPMON   = E_SPMON
          I_WERKS   = E_WERKS
*         I_MATNR   =
        TABLES
          T_RESULT  = GT_SD_RESULT
          T_RESULT2 = GT_SD_RESULT2.

  ENDTRY.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_MM_QTY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_MM_QTY .


*   ZMMT0610 [MM] 생산 투입/입고 처리 가져오기

*
  SELECT  WERKS ,
          RMBLNR ,  " 원자재의 mvt 903으로 생성된 자재 문서번호
          FMATNR ,   "제품
          FMENGE ,  "
          FMEINS ,
          'KRW'  AS TWAER ,
          RMATNR  ,  " 원자재
          RMENGE  ,
          RMEINS  ,
          RDMBTR AS RWRBTR      " mseg-dmbtr
    FROM ZMMT0610
   WHERE WERKS =  @PA_WERKS
     AND BUDAT IN @GR_BUDAT
     AND FMENGE > 0    " 제품 수량 있는  항목
     AND LVORM <> 'X'  "삭제 지시자
    INTO CORRESPONDING FIELDS OF TABLE @GT_RAW.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_RANGE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*

FORM SET_RANGE_DATA .
  CLEAR : GR_GJAHR[],
          GR_BUDAT[],
          GR_RACCT[].

  DATA : LV_DATE     TYPE SY-DATUM,
         LV_LAST_DAY TYPE SY-DATUM.

  DATA : LV_YY(4),
         LV_YM(6).


  CLEAR GR_BUDAT. REFRESH GR_BUDAT.

  LV_DATE = PA_MONTH && '01'.
  PERFORM RP_LAST_DAY_OF_MONTHS(ZCAR9000) USING LV_DATE
                                       CHANGING LV_LAST_DAY
                                             IF FOUND.

  _SET_RANGES : GR_GJAHR 'I' 'EQ' PA_MONTH(4) '',  "연도
                GR_BUDAT 'I' 'BT' LV_DATE LV_LAST_DAY .   "전기월



ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_LASTDAY_STOCK_MM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_LASTDAY_STOCK_MM .

  DATA :

    I_MATNR TYPE  MATNR    ,              "     자재 번호
    I_LFGJA TYPE  LFGJA    ,              "    현재 기간의 회계연도
    I_LFMON TYPE  LFMON    .              "    현재 기간 (전기기간)


  DATA : E_LFGJA TYPE LFGJA,
         E_LFMON TYPE LFMON,
         E_WERKS TYPE WERKS.    "    플랜트

  E_LFGJA = PA_MONTH+0(4).
  E_LFMON = PA_MONTH+4(2).
  E_WERKS = PA_WERKS.

  CLEAR : GT_STOCK, GT_STOCK[].

  TRY.

      CALL FUNCTION 'ZMM_PERIOD_END_STOCK_QUANTITY'
        EXPORTING
*         I_MATNR =
          I_LFGJA = E_LFGJA
          I_LFMON = E_LFMON
          I_WERKS = E_WERKS
        TABLES
          ET_LIST = GT_STOCK.

*CATCH

  ENDTRY.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
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
      WHEN  'LIGHTS'.
        GS_FIELDCAT-JUST = GC_C.
        GS_FIELDCAT-OUTPUTLEN = 4.
        LV_TEXT = TEXT-F77.
        GS_FIELDCAT-FIX_COLUMN = 'X'.
*      WHEN 'ICON'.
*        GS_FIELDCAT-JUST = GC_C.
*        GS_FIELDCAT-OUTPUTLEN = 4.
*        GS_FIELDCAT-FIX_COLUMN = 'X'.
*        LV_TEXT = TEXT-F01.
      WHEN 'BUKRS'.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        GS_FIELDCAT-FIX_COLUMN = 'X'.
      WHEN 'EMEINS'.
        LV_TEXT = TEXT-G02.  "단위
        GS_FIELDCAT-DECIMALS = 3.
      WHEN 'EMENGE'. "기말수량
        LV_TEXT = TEXT-G01.
        GS_FIELDCAT-QFIELDNAME = 'EMEINS' .
      WHEN 'FMEINS'.
        GS_FIELDCAT-DECIMALS = 3.
        LV_TEXT = TEXT-G02.  "단위
      WHEN 'FMENGE'. "생산수량
        GS_FIELDCAT-QFIELDNAME = 'FMEINS' .
        LV_TEXT = TEXT-F11.
      WHEN 'FMATNR'."제품코드
        GS_FIELDCAT-LZERO = 'X'.
        LV_TEXT = TEXT-F06.
        GS_FIELDCAT-EMPHASIZE = 'C711'.
        GS_FIELDCAT-FIX_COLUMN = 'X'..
      WHEN 'FMATNR_MAKTX' ."제품코드명 .
        LV_TEXT = TEXT-F10.
        GS_FIELDCAT-OUTPUTLEN = 10.
        GS_FIELDCAT-FIX_COLUMN = 'X'.
        GS_FIELDCAT-EMPHASIZE = 'C711'.
        GS_FIELDCAT-FIX_COLUMN = 'X'.
      WHEN 'MATKL'.
        GS_FIELDCAT-OUTPUTLEN = 10.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        GS_FIELDCAT-FIX_COLUMN = 'X'.
      WHEN 'SMEINS'.
        GS_FIELDCAT-DECIMALS = 3.
        LV_TEXT = TEXT-G02.  "단위
      WHEN 'SMENGE'.
        GS_FIELDCAT-QFIELDNAME = 'SMEINS' .
      WHEN 'BMEINS'.
        GS_FIELDCAT-DECIMALS = 3.
        LV_TEXT = TEXT-G11.  " 기초이월 단위
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
      WHEN 'BMENGE'.
        GS_FIELDCAT-QFIELDNAME = 'BMEINS' .
*        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        LV_TEXT = TEXT-G12.  " 기초이월 수량
      WHEN 'SPMON'.
        GS_FIELDCAT-OUTPUTLEN = 10.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
      WHEN 'WERKS'.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
      WHEN 'WERKS_NAME'.
        LV_TEXT = TEXT-F05.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        GS_FIELDCAT-OUTPUTLEN = 10.
      WHEN  'MSG'.
        LV_TEXT = 'ICON 설명'.


      WHEN OTHERS.


        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
    ENDCASE.

    "-- fix column
    IF GS_FIELDCAT-COL_POS LE 4.
      GS_FIELDCAT-FIX_COLUMN = ABAP_TRUE.
    ENDIF.

    "-- 최적화

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
*& Form GET_MMT0600
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_MMT0600 .
  SELECT *
   FROM ZMMT0600
   WHERE LVORM <> 'X'
    INTO TABLE @GT_ZMMT600.


*제품 순으로 SORT
  SORT GT_ZMMT600 BY MATNR MATNR2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_AUTH_COND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_AUTH_COND .
  DATA : LV_TYPE(1) .
  CLEAR : GV_EXIT , GV_MESSAGE  .

  CALL FUNCTION 'ZCA_PRG_AUTH_CHECK'
    EXPORTING
      I_BNAME    = SY-UNAME
      I_MODULE   = 'CO'
      I_BUKRS_FI = PA_BUKRS
    IMPORTING
      E_TYPE     = LV_TYPE
      E_MESSAGE  = GV_MESSAGE
      E_EXIT     = GV_EXIT.

  IF LV_TYPE EQ GC_E.
    MESSAGE S000 WITH GV_MESSAGE DISPLAY LIKE GC_E.
  ENDIF.
ENDFORM.
