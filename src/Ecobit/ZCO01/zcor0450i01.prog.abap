*&---------------------------------------------------------------------*
*& Include          ZCOR0450I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.

*-- OK_CODE ACTIONS.
  CASE OK_CODE.
    WHEN '&BCK'.
      LEAVE TO SCREEN 0.

    WHEN '&CNC'.
      LEAVE TO SCREEN 0.
    WHEN '&EXT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CLEAR: SAVE_OK, GV_EXIT, GV_ANSWER, GV_ERROR.

  "-- move ok code.
  SAVE_OK = OK_CODE.   CLEAR: OK_CODE.

*-- ALV CHECKED CHANGE DATA
  CALL METHOD GR_GRID1->CHECK_CHANGED_DATA( ).
  CALL METHOD GR_GRID1->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROWS[].
  .

*-- SAVE_OK ACTIONS.
  CASE SAVE_OK.

    WHEN '&BCK'.
      LEAVE TO SCREEN 0.

      "__ 도움말
    WHEN '&HELP'.

      PERFORM CALL_POPUP_HELP(ZCAR9000) USING SY-REPID SY-DYNNR SY-LANGU ''.

    WHEN '&POST'.":회계기표

      IF  GV_EXE_FLAG_COGS = 'D'  AND
           GV_EXE_FLAG_RAW  = 'D' AND
           GV_EXE_FLAG_LABOR = 'D' .

      ELSE.
        GV_EXIT =   'X'.

        MESSAGE S000 WITH  TEXT-E66  DISPLAY LIKE 'E'.


      ENDIF.

      CHECK GV_EXIT EQ SPACE.
      LOOP AT GT_DISPLAY INTO GS_DISPLAY WHERE BELNR1 IS NOT INITIAL OR BELNR3 IS NOT INITIAL.
        GV_EXIT =   'X'.
        EXIT.
      ENDLOOP.

      IF GV_EXIT = 'X'.

        MESSAGE S000 WITH  TEXT-E33  DISPLAY LIKE 'E'.
      ENDIF.


      CHECK GV_EXIT EQ SPACE.

      PERFORM CREATE_DATA_RTN.
      PERFORM REFRESH_GRID_0100.

    WHEN '&CANC'.":취소
**      IF GT_ROWS[] IS INITIAL.
**        MESSAGE S021.
**        GV_EXIT = ABAP_TRUE.
**      ENDIF.
**      CHECK GV_EXIT EQ SPACE.
**      PERFORM CHECK_SELECT_DATA  USING GC_C.
**      CHECK GV_EXIT = SPACE.
**      PERFORM CANCLE_DATA_RTN.
**      PERFORM REFRESH_GRID_0100.
**

*  선택 안해도 일괄  REVERSE 되도록 수정..
**      FU POPUP_TO_CONFIRM              ANSWER
**      RETURN VALUES: '1', '2', 'A'
**      '1' LEFT PUSHBUTTON
**      '2' NEXT PUSHBUTTON
**      'A' 'Cancel' PUSHBUTTON
**

      CLEAR GV_ANSWER.

      "-- call popup
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR              = '확인'
*         DIAGNOSE_OBJECT       = ' '
          TEXT_QUESTION         = '전표를 일괄 취소합니다.'
*         TEXT_BUTTON_1         = 'Ja'(001)
*         ICON_BUTTON_1         = ' '
*         TEXT_BUTTON_2         = 'Nein'(002)
*         ICON_BUTTON_2         = ' '
*         DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ' '
*         USERDEFINED_F1_HELP   = ' '
*         START_COLUMN          = 25
*         START_ROW             = 6
*         POPUP_TYPE            =
*         IV_QUICKINFO_BUTTON_1 = ' '
*         IV_QUICKINFO_BUTTON_2 = ' '
        IMPORTING
          ANSWER                = GV_ANSWER
*   TABLES
*         PARAMETER             =
        EXCEPTIONS
          TEXT_NOT_FOUND        = 1
          OTHERS                = 2.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.


      IF GV_ANSWER = '1'.

        CLEAR : GS_INFO .
        LOOP AT GT_DISPLAY ASSIGNING <FS_DISP> .


          IF <FS_DISP>-BELNR3 IS NOT INITIAL.
            GS_INFO-BELNR = <FS_DISP>-BELNR3.
            GS_INFO-GJAHR = <FS_DISP>-SPMON+0(4).
            PERFORM CANCEL_DOC USING 'RR'.
          ENDIF.

          IF <FS_DISP>-BELNR1 IS NOT INITIAL.
            CLEAR : GS_INFO.
            GS_INFO-BELNR = <FS_DISP>-BELNR1.
            GS_INFO-GJAHR = <FS_DISP>-SPMON+0(4).
            PERFORM CANCEL_DOC USING 'YY'.

          ENDIF.
          CLEAR : GS_INFO , GV_EXIT.

        ENDLOOP.

      ELSE.
        MESSAGE S000 WITH TEXT-M55.

        PERFORM REFRESH_GRID_0100.
      ENDIF.

    WHEN '&RCOM' ."   원재료비차이
      CLEAR GV_DISTRL_ERROR1.
      CLEAR GV_DISTRL_ERROR2.
      CLEAR GV_DISTRL_ERROR3.

      PERFORM RAW_DISTRIBUTION.  " ZMMR4050에서 미처리된 전표  배부방법1  우선순위 1  직접귀속 ..**
      PERFORM RAW_DISTRIBUTION2. " 가격차이
      PERFORM RAW_DISTRIBUTION3. " 재고조정
      PERFORM COMPUTE_COMS.

      IF GV_DISTRL_ERROR1 IS NOT INITIAL  OR
            GV_DISTRL_ERROR2 IS NOT INITIAL OR
           GV_DISTRL_ERROR3 IS NOT INITIAL.

        MESSAGE I000 WITH TEXT-M44.
        .
      ENDIF.

      GV_EXE_FLAG_RAW =  'D'.


      PERFORM REFRESH_GRID_0100.

    WHEN '&LCOM'.  "    인건비,경비배부
      PERFORM LCOST_DISTRIBUTION.
      PERFORM MCOST_DISTRIBUTION.
      PERFORM COMPUTE_COMS.


      GV_EXE_FLAG_LABOR =  'D'.
      PERFORM REFRESH_GRID_0100.


    WHEN '&ZCOGS'.   "   매출원가계산

      PERFORM COMPUTE_COGS.

      GV_EXE_FLAG_COGS =  'D'.

      PERFORM REFRESH_GRID_0100.

    WHEN '&DELE'.
      PERFORM DELETE_COT1260.

      PERFORM REFRESH_GRID_0100.

    WHEN '&SAV'.
      PERFORM SAVE_COT1260.

   WHEN '&QUAN'.     " 기표 후 SD MM 호출 펑션으로  수량 조회

     SUBMIT ZCOR0480 WITH PA_MONTH = PA_MONTH
                     WITH PA_WERKS =  PA_WERKS
                     VIA SELECTION-SCREEN AND RETURN.


    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT`
*&---------------------------------------------------------------------*
*& Form RAW_DISTRIBUTION3
*&---------------------------------------------------------------------*
*& 재고조정
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM RAW_DISTRIBUTION3 .


* 재고조정  1  직접귀속


  LOOP AT GT_DISPLAY ASSIGNING FIELD-SYMBOL(<CLR>) .

    CLEAR <CLR>-D4WRBTR .

  ENDLOOP.



  CHECK GT_DIFF_003[] IS  NOT INITIAL.
  DATA : LV_MATKL  TYPE MARA-MATKL,
         LV_LIFNR  TYPE LFA1-LIFNR,
         LV_FMATNR TYPE ZCOS0450-FMATNR.


  SORT GT_RATE3 BY LIFNR MATKL FMATNR.
  SORT GT_ZMMT600  BY MATNR2.

*           투입된 원재료의 매핑된 제품코드에 직접귀속..

  LOOP AT GT_DIFF_003 ASSIGNING FIELD-SYMBOL(<D003>).
    CLEAR <D003>-MSG_TEXT .

* 배부방법 1 : 직접귀속

    IF <D003>-MATNR IS NOT INITIAL.

      READ TABLE GT_DISPLAY ASSIGNING FIELD-SYMBOL(<CCC>) WITH   KEY  RMATNR =  <D003>-MATNR .


      IF SY-SUBRC EQ 0  AND <CCC>-FMENGE IS NOT INITIAL.. ""직접귀속 원자재 MMT0610에 있다

        <CCC>-D4WRBTR  = <CCC>-D4WRBTR   +  <D003>-HSL.

        <D003>-MSG_TEXT = 'S: 배부방법1 직접귀속   우선순위1'.

        CONTINUE.



      ENDIF.
    ENDIF.


*전표의 가격차이와 매칭되는 MMT0610의 원재료가 없을때

    CHECK SY-SUBRC NE 0 AND  <D003>-MSG_TEXT IS INITIAL.. ""  직접귀속 못할때...



    CLEAR :  LV_MATKL,
             LV_LIFNR ,
             LV_FMATNR.

* 전표의 자재로  매핑된 제품 코드 찾기
    READ TABLE GT_ZMMT600 INTO GS_ZMMT600 WITH KEY  MATNR2 = <D003>-MATNR BINARY SEARCH.
    IF SY-SUBRC EQ 0.

      LV_FMATNR = GS_ZMMT600-MATNR.


**   찾은 제품의 자재그룹
      READ TABLE GT_T023T ASSIGNING FIELD-SYMBOL(<023T>)  WITH KEY FMATNR =    LV_FMATNR   BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LV_FMATNR = <023T>-FMATNR.

      ENDIF.

*** 원재료의 구매처
      READ TABLE GT_LFA1 INTO GS_LFA1 WITH KEY   RMATNR = <D003>-MATNR BINARY SEARCH.


      IF SY-SUBRC EQ 0.
        LV_LIFNR = GS_LFA1-LIFNR.

      ENDIF.


    ELSE.
      <D003>-MSG_TEXT = 'E0: 원재료에 매핑되는 제품이 cbo 마스터에 없음. 전체 생산수량비율로 배부  '.

    ENDIF.


    READ TABLE GT_RATE3 ASSIGNING FIELD-SYMBOL(<R3>) WITH KEY  LIFNR  = LV_LIFNR
                                                               MATKL = LV_MATKL
                                                               FMATNR = LV_FMATNR BINARY SEARCH.


    IF SY-SUBRC EQ 0.


      READ TABLE GT_DISPLAY ASSIGNING FIELD-SYMBOL(<$DISP>) WITH   KEY     LIFNR  = LV_LIFNR
                                                                      MATKL = LV_MATKL
                                                                      FMATNR = LV_FMATNR BINARY SEARCH.


      IF SY-SUBRC EQ 0 AND <$DISP>-FMENGE IS NOT INITIAL.
        " 가격 차이  전표번호별 같은 원자재 있을수 있기때문에 누적
        <$DISP>-D4WRBTR  = <$DISP>-D4WRBTR   +  <D003>-HSL.

        <D003>-MSG_TEXT = 'S: 배부방법1  우선순위2'.


        CONTINUE.


      ELSE.

        <D003>-MSG_TEXT = 'E0: 우선순위2까지 매핑 비율 미존재 '.
      ENDIF.


    ELSE.

      READ TABLE GT_RATE4 ASSIGNING FIELD-SYMBOL(<R4>) WITH KEY  LIFNR  = LV_LIFNR
                                                                FMATNR = LV_FMATNR BINARY SEARCH.

      IF SY-SUBRC EQ 0.

        READ TABLE GT_DISPLAY ASSIGNING FIELD-SYMBOL(<$DISP2>) WITH   KEY     LIFNR  = LV_LIFNR
                                                                    MATKL = LV_MATKL
                                                                    FMATNR = LV_FMATNR BINARY SEARCH.

        IF SY-SUBRC EQ 0 AND <$DISP2>-FMENGE IS NOT INITIAL..
          <D003>-MSG_TEXT = 'S: 배부방법1  우선순위3 '.

          " 가격 차이  전표번호별 같은 원자재 있을수 있기때문에 누적
          <$DISP2>-D4WRBTR  = <$DISP2>-D4WRBTR   +  <D003>-HSL.
          CONTINUE.
        ELSE.
          <D003>-MSG_TEXT = 'E0: 우선순위3까지 매핑 비율 미존재 '.
        ENDIF.
      ELSE.
        <D003>-MSG_TEXT = 'E0: 우선순위3까지 매핑 비율 미존재 '.

      ENDIF.

    ENDIF.


  ENDLOOP.


  DATA : LV_ERROR(2).
  DATA : LV_ERROR_RESULT(2).
  DATA : NO_DISTRI_AMT TYPE ACDOCA-HSL.
  DATA : LV_DIFF_ACDOCA_SUM  TYPE ACDOCA-HSL.
  DATA : LV_D4WRBTR   TYPE ACDOCA-HSL.

  LV_ERROR = 'E0'.
  CLEAR LV_ERROR_RESULT.


  CLEAR NO_DISTRI_AMT.

  LOOP AT GT_DIFF_003 ASSIGNING FIELD-SYMBOL(<001B$>) WHERE MSG_TEXT+0(2) EQ LV_ERROR OR MSG_TEXT IS INITIAL.

    LV_ERROR_RESULT = 'YY'.
    NO_DISTRI_AMT = NO_DISTRI_AMT +   <001B$>-HSL.
    <001B$>-MSG_TEXT = '배부율 우선순위 4 적용'.

  ENDLOOP.



  CHECK  LV_ERROR_RESULT  EQ 'YY'.
  CHECK NO_DISTRI_AMT IS NOT INITIAL.

  LV_DIFF_ACDOCA_SUM  =   NO_DISTRI_AMT.

  SORT GT_RATE5 BY FMATNR.

  LOOP AT GT_DISPLAY ASSIGNING FIELD-SYMBOL(<DATA>) WHERE FMENGE  IS NOT INITIAL.


    READ TABLE GT_RATE5 ASSIGNING FIELD-SYMBOL(<$R5>) WITH KEY FMATNR = <DATA>-FMATNR BINARY SEARCH.
    CHECK SY-SUBRC EQ 0 AND <$R5>-RATE5 IS NOT INITIAL.


    CLEAR LV_D4WRBTR  .
    LV_D4WRBTR  =   NO_DISTRI_AMT  *  <$R5>-RATE5 .


    <DATA>-D4WRBTR  =  <DATA>-D4WRBTR  +   LV_D4WRBTR .

    LV_DIFF_ACDOCA_SUM   = LV_DIFF_ACDOCA_SUM   -   LV_D4WRBTR .


  ENDLOOP.


**차이보정

  IF LV_DIFF_ACDOCA_SUM  IS NOT INITIAL AND GT_DISPLAY[] IS NOT  INITIAL.

    SORT GT_DISPLAY BY FMENGE DESCENDING.

    LOOP AT GT_DISPLAY ASSIGNING FIELD-SYMBOL(<$FS>) WHERE FMATNR IS NOT INITIAL AND FMENGE IS NOT INITIAL.

      <$FS>-D4WRBTR   =   <$FS>-D4WRBTR   +  LV_DIFF_ACDOCA_SUM  .

      EXIT.
    ENDLOOP.

  ENDIF.


  LOOP AT GT_DISPLAY ASSIGNING FIELD-SYMBOL(<ST>) WHERE D4WRBTR  IS NOT INITIAL.
    <ST>-ICON =  ICON_LED_YELLOW.
    <ST>-STATS =  <ST>-STATS && '전체 수량 비율 재고조정 배부'.

  ENDLOOP.


  SORT GT_DISPLAY BY LIFNR MATKL FMATNR RMATNR.

  SORT GT_ZMMT600  BY MATNR.



  GV_DISTRL_ERROR3 = '3E'.    " 재조조정 전체수량 배부 존재





ENDFORM.
