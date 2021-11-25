*&---------------------------------------------------------------------*
*& Include          ZCOR0460F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INIT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM INIT_DATA .

  "__ Function Key
  DATA: LS_FUNTXT TYPE SMP_DYNTXT.

  LS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  LS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = LS_FUNTXT.

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

    WHEN OTHERS.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

  PERFORM GET_FI_REV_GL.

  PERFORM GET_WERKS_NAME.
  PERFORM GET_MMT0600.
  PERFORM GET_GSBER.

  SELECT *
    FROM ZCOT1261
    WHERE BUKRS = @PA_BUKRS
      AND SPMON = @PA_MONTH
      AND WERKS = @PA_WERKS
    INTO CORRESPONDING FIELDS OF TABLE @GT_1261.


  IF SY-SUBRC EQ 0.



    LOOP AT GT_1261 ASSIGNING FIELD-SYMBOL(<$FS>) .

      MOVE-CORRESPONDING <$FS> TO GS_DISPLAY.

* *자재그룹명
      SELECT SINGLE WGBEZ INTO GS_DISPLAY-WGBEZ
        FROM T023T
       WHERE MATKL = GS_DISPLAY-MATKL.

* 제품자재명
      SELECT SINGLE MAKTX INTO GS_DISPLAY-FMATNR_MAKTX
        FROM MAKT
       WHERE SPRAS = SY-LANGU
        AND MATNR = GS_DISPLAY-FMATNR.

* 원자재  자재명
      SELECT SINGLE MAKTX INTO GS_DISPLAY-RMATNR_MAKTX
        FROM MAKT
       WHERE SPRAS = SY-LANGU
        AND MATNR = GS_DISPLAY-RMATNR.

*  구매처명
      SELECT SINGLE NAME1 INTO GS_DISPLAY-NAME1
        FROM LFA1
      WHERE LIFNR = GS_DISPLAY-LIFNR.



      IF  GS_DISPLAY-BELNR3 IS NOT INITIAL.

        GS_DISPLAY-ICON = ICON_LED_GREEN.
      ELSE.
        GS_DISPLAY-ICON = ICON_LED_YELLOW.

      ENDIF.

*역분개 여부 추가
      IF  GS_DISPLAY-BELNR3 IS NOT INITIAL.
        PERFORM CHECK_REVERSE USING GS_DISPLAY.

      ENDIF.


      SELECT SINGLE MTART
            INTO GS_DISPLAY-MTART
            FROM MARA
          WHERE MATNR = GS_DISPLAY-FMATNR.

      CLEAR GT_STYLE[].
      _STYLE_DISABLED : 'FMATNR' , 'SMEINS' , 'SMENGE',
                        'SWRBTR' , 'ZCOGS1'.

      GS_DISPLAY-STYLE[] = GT_STYLE[].

*  GT_STYLE         TYPE LVC_T_STYL,

      READ TABLE GT_FI_REV INTO GS_FI_REV WITH KEY MATNR = GS_DISPLAY-FMATNR BINARY SEARCH.

      IF SY-SUBRC EQ 0.
        GS_DISPLAY-DMBTR = GS_FI_REV-HSL.
      ENDIF.

      APPEND GS_DISPLAY TO GT_DISPLAY.
      CLEAR GS_DISPLAY.
    ENDLOOP.

    DELETE GT_DISPLAY WHERE BELNR3 IS INITIAL.



    SORT GT_DISPLAY BY FMATNR.


*정상 제조 수불을 표시 필요할까?

**    SELECT *
**      FROM ZCOT1260
**      WHERE BUKRS = @PA_BUKRS
**        AND SPMON = @PA_MONTH
**        AND WERKS = @PA_WERKS
**        AND FMATNR IN @SO_FMAT
**      INTO TABLE @DATA(LT_1260).
**
**
**    SORT LT_1260 BY FMATNR.







***
***    CLEAR GS_FI_REV.
***    LOOP AT GT_FI_REV INTO GS_FI_REV.
***
***      READ TABLE GT_DISPLAY ASSIGNING FIELD-SYMBOL(<$FS2>)
***                         WITH KEY FMATNR = GS_FI_REV-MATNR BINARY SEARCH.
***      IF SY-SUBRC EQ 0.
***        <$FS2>-DMBTR =  GS_FI_REV-HSL * ( -1 ).
***
***
***      ELSE.
***
***
***        READ TABLE LT_1260 ASSIGNING FIELD-SYMBOL(<LT>) WITH KEY FMATNR =  GS_FI_REV-MATNR BINARY SEARCH.
***        IF SY-SUBRC EQ 0.
***          PERFORM APPEND_DISP USING <LT>
***                                    <$FS2>-DMBTR.
***        ELSE.
***
***          CLEAR  GS_DISPLAY.
***          SELECT SINGLE MTART
***                 INTO GS_DISPLAY-MTART
***                 FROM MARA
***               WHERE MATNR = GS_FI_REV-MATNR.
***
***
***          GS_DISPLAY-FMATNR = GS_FI_REV-MATNR.
***          GS_DISPLAY-DMBTR =  GS_FI_REV-HSL * ( -1 ).
***          GS_DISPLAY-TWAER = 'KRW'.
***
***
****    제품명
***          SELECT SINGLE MAKTX
***            INTO GS_DISPLAY-FMATNR_MAKTX
***            FROM MAKT
***          WHERE MATNR = GS_FI_REV-MATNR
***            AND SPRAS = SY-LANGU.
***
***          GS_DISPLAY-MSG  =    GS_DISPLAY-MSG && '수불 테이블에 존재하지 않는 FI 자재별  매출추가 '.
***
***
***          CLEAR GT_STYLE[].
***          _STYLE_DISABLED : 'FMATNR' , 'SMEINS' , 'SMENGE',
***                            'SWRBTR'.
***
***          GS_DISPLAY-STYLE[] = GT_STYLE[].
***
***
***
***          APPEND GS_DISPLAY TO  GT_DISPLAY.
***
***          SORT GT_DISPLAY BY FMATNR.
***
***
***        ENDIF.
***      ENDIF.
***    ENDLOOP.




  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_DATA_CHANGED
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ER_DATA_CHANGED
*&      --> E_ONF4
*&      --> E_ONF4_BEFORE
*&      --> E_ONF4_AFTER
*&      --> E_UCOMM
*&      --> SENDER
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

  DATA LV_MATNR TYPE MARA-MATNR.

  LOOP AT PR_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELLS.
    CLEAR: GS_DISPLAY.

    READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX LS_MOD_CELLS-ROW_ID.

    CASE LS_MOD_CELLS-FIELDNAME.

      WHEN 'ZCOGS1'.   " 매출원가 차감
        _GET_CELL_VALUE
         'ZCOGS1' LS_MOD_CELLS-ROW_ID GS_DISPLAY-ZCOGS1.

        IF GS_DISPLAY-ZCOGS1 > 0.
          GS_DISPLAY-ZCOGS1 = GS_DISPLAY-ZCOGS1 * -1.
        ENDIF.

        IF GS_DISPLAY-ZCOGS1 IS NOT INITIAL.
          GS_DISPLAY-EWRBTR =  GS_DISPLAY-BWRBTR -   GS_DISPLAY-ZCOGS1.

          _MODIFY_CELL 'ZCOGS1' LS_MOD_CELLS-ROW_ID GS_DISPLAY-ZCOGS1.
          _MODIFY_CELL 'EWRBTR' LS_MOD_CELLS-ROW_ID GS_DISPLAY-EWRBTR.

        ENDIF.


      WHEN 'SMENGE'.   " 판매수량
        _GET_CELL_VALUE
         'SMENGE' LS_MOD_CELLS-ROW_ID GS_DISPLAY-SMENGE.
        _GET_CELL_VALUE
         'BMENGE' LS_MOD_CELLS-ROW_ID GS_DISPLAY-BMENGE.

        IF GS_DISPLAY-SMENGE > 0.
          GS_DISPLAY-SMENGE = GS_DISPLAY-SMENGE * -1.
        ENDIF.

* 생산은 없다..
        IF GS_DISPLAY-SMENGE IS NOT INITIAL.
          GS_DISPLAY-EMENGE =  GS_DISPLAY-BMENGE -   GS_DISPLAY-SMENGE.

          _MODIFY_CELL 'SMENGE' LS_MOD_CELLS-ROW_ID GS_DISPLAY-SMENGE.
          _MODIFY_CELL 'EMENGE' LS_MOD_CELLS-ROW_ID GS_DISPLAY-EMENGE.
          _MODIFY_CELL 'EMEINS' LS_MOD_CELLS-ROW_ID GS_DISPLAY-EMEINS.

        ENDIF.

      WHEN 'SMEINS'.   " 판매단위
        _GET_CELL_VALUE
         'SMEINS' LS_MOD_CELLS-ROW_ID GS_DISPLAY-SMEINS.

        _MODIFY_CELL 'SMEINS' LS_MOD_CELLS-ROW_ID GS_DISPLAY-SMEINS.
        _MODIFY_CELL 'EMEINS' LS_MOD_CELLS-ROW_ID GS_DISPLAY-EMEINS.

      WHEN 'FMATNR'.   " 제품명
        _GET_CELL_VALUE
         'FMATNR' LS_MOD_CELLS-ROW_ID GS_DISPLAY-FMATNR.

        IF GS_DISPLAY-FMATNR IS NOT INITIAL.

          SELECT SINGLE  B~MAKTX AS FMATNR_MAKTX,
                         A~MTART AS MTART,
                         A~MATKL AS MATKL
              FROM MARA AS A INNER JOIN MAKT AS B
            ON  A~MATNR = B~MATNR
              INNER JOIN MARC  AS C
            ON  B~MATNR = C~MATNR
          WHERE C~WERKS = @PA_WERKS
            AND A~MTART = 'UNB3' " 비평가 제품
            AND B~SPRAS = @SY-LANGU
            AND A~MATNR = @GS_DISPLAY-FMATNR
                INTO CORRESPONDING FIELDS OF @GS_DISPLAY.


          IF SY-SUBRC EQ 0.

* 월마감  제조, 판매가 있는지 체크 .. 있으면  추가 불가..

            SELECT SINGLE *
             FROM ZCOT1260
            WHERE BUKRS = @PA_BUKRS
              AND WERKS = @PA_WERKS
              AND SPMON = @PA_MONTH
              AND FMATNR = @GS_DISPLAY-FMATNR
              AND ( BELNR2 > '' OR
                  BELNR1 > ''  )
              INTO @DATA(LS_1260).

            IF SY-SUBRC EQ 0. " 기존 분개 전표가 있다.

              MOVE-CORRESPONDING LS_1260 TO GS_DISPLAY.

              _MODIFY_CELL 'FMATNR' LS_MOD_CELLS-ROW_ID GS_DISPLAY-FMATNR.
              _MODIFY_CELL 'FMATNR_MAKTX' LS_MOD_CELLS-ROW_ID GS_DISPLAY-FMATNR_MAKTX.
              _MODIFY_CELL 'MTART' LS_MOD_CELLS-ROW_ID GS_DISPLAY-MTART.
              _MODIFY_CELL 'MATKL' LS_MOD_CELLS-ROW_ID GS_DISPLAY-MATKL.

              READ TABLE GT_ZMMT600 ASSIGNING FIELD-SYMBOL(<$ZZ2>)
                    WITH KEY MATNR =   GS_DISPLAY-FMATNR BINARY SEARCH.
              IF SY-SUBRC EQ 0.
                GS_DISPLAY-RMATNR =  <$ZZ2>-MATNR2. " 매핑 원자재 넣기  COLLECT  목적...

                _MODIFY_CELL 'RMATNR' LS_MOD_CELLS-ROW_ID GS_DISPLAY-FMATNR_MAKTX.
                PERFORM GET_LIFNR USING GS_DISPLAY.

                _MODIFY_CELL 'NAME1' LS_MOD_CELLS-ROW_ID GS_DISPLAY-NAME1.

              ENDIF.
              _ADD_PROTOCOL LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID
                          'E' 000 TEXT-E66 LS_1260-BELNR1 LS_1260-BELNR3 SPACE.
**            ZCOR0450, 기 처리 전표가 있습니다.

              GV_EXIT = ABAP_TRUE . EXIT.

            ELSE.

*인터널 테이블  중복 체크

              READ TABLE GT_DISPLAY ASSIGNING FIELD-SYMBOL(<FS>)   WITH KEY FMATNR = GS_DISPLAY-FMATNR BINARY SEARCH.
              IF SY-SUBRC EQ 0.

                _ADD_PROTOCOL LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID
                            'E' 000 TEXT-E33 <FS>-FMATNR <FS>-BELNR3 SPACE.
                GV_EXIT = ABAP_TRUE . EXIT.

**이미 등록된 자재코드입니다.
              ELSE.


                _MODIFY_CELL 'FMATNR_MAKTX' LS_MOD_CELLS-ROW_ID GS_DISPLAY-FMATNR_MAKTX.
                _MODIFY_CELL 'MTART' LS_MOD_CELLS-ROW_ID GS_DISPLAY-MTART.

* 제품에 매핑된 원자재등등..
*자재그룹, 원자재 찾아넣기

                READ TABLE GT_ZMMT600 ASSIGNING FIELD-SYMBOL(<$ZZ>)
                       WITH KEY MATNR =   GS_DISPLAY-FMATNR BINARY SEARCH.
                IF SY-SUBRC EQ 0.
                  GS_DISPLAY-RMATNR =  <$ZZ>-MATNR2. " 매핑 원자재 넣기  COLLECT  목적...

                  _MODIFY_CELL 'RMATNR' LS_MOD_CELLS-ROW_ID   GS_DISPLAY-RMATNR.
                  PERFORM GET_LIFNR USING GS_DISPLAY.

                  _MODIFY_CELL 'NAME1' LS_MOD_CELLS-ROW_ID GS_DISPLAY-NAME1.


                ELSE.  " 매핑 자료는 없고.

                ENDIF. " / GT_ZMT600
              ENDIF.  " GT_DISPLAY
            ENDIF. "/ 1260  END IF
          ELSE.  "  오 등록 건이다..
            _ADD_PROTOCOL LS_MOD_CELLS-FIELDNAME LS_MOD_CELLS-ROW_ID
                          'E' 000 TEXT-E44 SPACE SPACE SPACE.
**          제품 자재 코드를 확인하세요

            GV_EXIT = ABAP_TRUE . EXIT.
          ENDIF.

        ELSE.
          CLEAR GS_DISPLAY-FMATNR_MAKTX.
          CLEAR GS_DISPLAY-MTART.
          _MODIFY_CELL 'FMATNR_MAKTX' LS_MOD_CELLS-ROW_ID GS_DISPLAY-FMATNR_MAKTX.
          _MODIFY_CELL 'MTART' LS_MOD_CELLS-ROW_ID GS_DISPLAY-MTART.


        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  CHECK GV_EXIT IS INITIAL.

ENDFORM.                    " EVENT_DATA_CHANGED

*&---------------------------------------------------------------------*
*& Form EVENT_DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_MODIFIED
*&      --> ET_GOOD_CELLS
*&      --> SENDER
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
*& Form EVENT_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_ID
*&      --> E_COLUMN_ID
*&      --> ES_ROW_NO
*&      --> SENDER
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


  CASE PR_SENDER.
    WHEN GR_GRID1.

      CASE PS_COLUMN_ID-FIELDNAME.
        WHEN 'MATNR'.
*          READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX PS_ROW_NO-ROW_ID.
*          CHECK SY-SUBRC EQ 0.
*
*          "Call Transaction
*          SET PARAMETER ID 'MAT' FIELD GS_DISPLAY-MATNR .
*          SET PARAMETER ID 'MXX' FIELD 'K' .              "Directly Display Basic Data
*          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN .

      ENDCASE.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " EVENT_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*& Form EVENT_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW
*&      --> E_COLUMN
*&      --> ES_ROW_NO
*&      --> SENDER
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


  CASE PR_SENDER.
    WHEN GR_GRID1.
*-- 전표번호를 클릭하면 전표조회 화면으로 이동
      CASE PS_COLUMN .
        WHEN 'BELNR1'.
          CLEAR GS_DISPLAY.
          READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX PS_ROW_NO-ROW_ID.

          CHECK GS_DISPLAY-BELNR1 IS NOT INITIAL.

          SET PARAMETER ID 'BLN' FIELD GS_DISPLAY-BELNR1.
          SET PARAMETER ID 'BUK' FIELD PA_BUKRS.
          SET PARAMETER ID 'GJR' FIELD PA_MONTH+0(4).

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        WHEN 'BELNR3'.
          CLEAR GS_DISPLAY.
          READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX PS_ROW_NO-ROW_ID.

          CHECK GS_DISPLAY-BELNR3 IS NOT INITIAL.
*
          SET PARAMETER ID 'BLN' FIELD GS_DISPLAY-BELNR3.
          SET PARAMETER ID 'BUK' FIELD PA_BUKRS.
          SET PARAMETER ID 'GJR' FIELD PA_MONTH+0(4).


          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.


        WHEN 'FMATNR' OR 'FMATNR_MAKTX'.

          CLEAR GS_DISPLAY.
          READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX PS_ROW_NO-ROW_ID.

          CHECK GS_DISPLAY-FMATNR IS NOT INITIAL.
*
          SET PARAMETER ID 'MAT' FIELD GS_DISPLAY-FMATNR.
          SET PARAMETER ID 'MXX' FIELD 'K'. .
          SET PARAMETER ID 'WRK' FIELD PA_WERKS.
          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

        WHEN 'RMATNR' OR 'RMATNR_MAKTX'.

          CLEAR GS_DISPLAY.
          READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX PS_ROW_NO-ROW_ID.

          CHECK GS_DISPLAY-RMATNR IS NOT INITIAL.
*
          SET PARAMETER ID 'MAT' FIELD GS_DISPLAY-RMATNR.
          SET PARAMETER ID 'MXX' FIELD 'K'. .
          SET PARAMETER ID 'WRK' FIELD PA_WERKS.
          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

        WHEN OTHERS.
      ENDCASE.
  ENDCASE.




ENDFORM.                    " EVENT_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*& Form EVENT_HELP_ON_F4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_FIELDNAME
*&      --> E_FIELDVALUE
*&      --> ES_ROW_NO
*&      --> ER_EVENT_DATA
*&      --> ET_BAD_CELLS
*&      --> E_DISPLAY
*&      --> SENDER
*&---------------------------------------------------------------------*
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


  DATA: LT_RETTAB LIKE TABLE OF DDSHRETVAL,
        LS_RETTAB LIKE LINE OF LT_RETTAB,
        LS_MODI   LIKE LVC_S_MODI.
  FIELD-SYMBOLS <FS_RET> TYPE TABLE.


*  CASE PR_SENDER.
*    WHEN GR_GRID1.
*    WHEN OTHERS.
*  ENDCASE.

  CASE PV_FIELDNAME.
    WHEN 'FMATNR'.

      SELECT A~MATNR AS FMATNR,
             B~MAKTX AS FMATNR_MAKTX
        FROM MARA AS A INNER JOIN MAKT AS B
          ON  A~MATNR = B~MATNR
            INNER JOIN MARC  AS C
          ON  B~MATNR = C~MATNR
        WHERE C~WERKS = @PA_WERKS
          AND A~MTART = 'UNB3' " 비평가 제품
          AND B~SPRAS = @SY-LANGU
        INTO TABLE @GT_FERT.

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          RETFIELD        = 'FMATNR'
          DYNPPROG        = SY-CPROG
          DYNPNR          = SY-DYNNR
          WINDOW_TITLE    = TEXT-Z01
          VALUE_ORG       = 'S'
          DISPLAY         = PV_DISPLAY
        TABLES
          VALUE_TAB       = GT_FERT
          RETURN_TAB      = LT_RETTAB
        EXCEPTIONS
          PARAMETER_ERROR = 1
          NO_VALUES_FOUND = 2
          OTHERS          = 3.



      "__ 선택된 값이 있으면 값을 return.
      READ TABLE LT_RETTAB INTO LS_RETTAB INDEX LINES( LT_RETTAB ).
      IF SY-SUBRC EQ 0.

        ASSIGN PR_EVENT_DATA->M_DATA->* TO <FS_RET>.

        LS_MODI-ROW_ID    = PS_ROW_NO-ROW_ID.
        LS_MODI-FIELDNAME = PV_FIELDNAME.
        LS_MODI-VALUE     = LS_RETTAB-FIELDVAL.
        APPEND LS_MODI TO <FS_RET>.
      ENDIF.

      PR_EVENT_DATA->M_EVENT_HANDLED = 'X'.


    WHEN OTHERS.

  ENDCASE.




ENDFORM.                    " EVENT_HELP_ON_F4
*&---------------------------------------------------------------------*
*& Form EVENT_TOOLBAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_OBJECT
*&      --> E_INTERACTIVE
*&      --> SENDER
*&---------------------------------------------------------------------*
FORM EVENT_TOOLBAR
       USING PR_OBJECT     TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
             PV_INTERACTIVE TYPE CHAR01
             PR_SENDER     TYPE REF TO CL_GUI_ALV_GRID.

* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_ : Reference Variables

* - BUTTON TYPE - BTYPE.
*  0 버튼(일반)
*  1 메뉴 및 기본 버튼
*  2 메뉴
*  3 분리자
*  4 라디오 버튼
*  5 체크박스
*  6 메뉴 엔트리

  CASE PR_SENDER.
    WHEN GR_GRID1.

      "ADD_BUTTON : OBJECT, BTYPE, FUNC, ICON, INFO, TEXT, DISABLE
      PERFORM ADD_BUTTON
        USING: PR_OBJECT '3' SPACE SPACE SPACE SPACE SPACE, "분리자
               PR_OBJECT '0' '&APD' SPACE TEXT-BT0 TEXT-BT0 SPACE,
               PR_OBJECT '0' '&DEL' SPACE TEXT-BT1 TEXT-BT1 SPACE,
               PR_OBJECT '3' SPACE SPACE SPACE SPACE SPACE. "분리자

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " EVENT_TOOLBAR
*&---------------------------------------------------------------------*
*& Form ADD_BUTTON
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PR_OBJECT
*&      --> P_
*&      --> SPACE
*&      --> SPACE
*&      --> SPACE
*&      --> SPACE
*&      --> SPACE
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
*& Form EVENT_USER_COMMAND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_UCOMM
*&      --> SENDER
*&---------------------------------------------------------------------*
FORM EVENT_USER_COMMAND  USING PV_UCOMM   TYPE SY-UCOMM
                               PR_SENDER TYPE REF TO CL_GUI_ALV_GRID.

* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_  : Reference Variables

*  CLEAR: GT_ROWS, GT_ROWS[].
*  "선택 ROW가져오기
*  CALL METHOD PR_SENDER->GET_SELECTED_ROWS
*    IMPORTING
*      ET_INDEX_ROWS = GT_ROWS[].

  DATA: LV_ROW   TYPE I,
        LV_COL   TYPE I,
        GV_DATUM TYPE SY-DATUM,
        LV_TABIX TYPE SY-TABIX.

  CLEAR: GS_DISPLAY.

  CASE PR_SENDER.
    WHEN GR_GRID1.
      CASE PV_UCOMM.
        WHEN '&APD'.   "--  빈줄 삽입
*          PERFORM SALV_CALL USING 0.

          GS_DISPLAY-BUKRS = PA_BUKRS.
          GS_DISPLAY-WERKS = PA_WERKS.
          GS_DISPLAY-SPMON = PA_MONTH.
          GS_DISPLAY-TWAER = 'KRW'.
          GS_DISPLAY-SMEINS = 'KG'.

*CELL STYLE  넣기
          CLEAR GT_STYLE[].
          _STYLE_ENABLED : 'FMATNR' , 'SMEINS' , 'SMENGE',
                           'SWRBTR' , 'ZCOGS1'.

          GS_DISPLAY-STYLE[] = GT_STYLE[].

          APPEND GS_DISPLAY TO GT_DISPLAY.
          CLEAR GS_DISPLAY.

          PERFORM REFRESH_GRID.

        WHEN '&DEL'.
          CALL METHOD PR_SENDER->GET_SELECTED_ROWS
            IMPORTING
              ET_INDEX_ROWS = GT_ROWS[].
          IF GT_ROWS[] IS INITIAL.
            MESSAGE S015.  "
            GV_EXIT = ABAP_TRUE. EXIT.
          ELSE.
            SORT GT_ROWS[] DESCENDING.
            LOOP AT GT_ROWS INTO GS_ROWS.
              CLEAR : GS_DISPLAY.
*              READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX GS_ROWS-INDEX.
              DELETE GT_DISPLAY INDEX GS_ROWS-INDEX.


            ENDLOOP.
            PERFORM REFRESH_GRID.
          ENDIF.



      ENDCASE.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " EVENT_USER_COMMAND
*&---------------------------------------------------------------------*
*& Form SALV_CALL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_0
*&---------------------------------------------------------------------*
FORM SALV_CALL  USING PV.
  TRY.

      CASE PV.
        WHEN '0'.

        WHEN ''.   "

          CL_SALV_TABLE=>FACTORY(
                 IMPORTING
                   R_SALV_TABLE = GO_ALV
                 CHANGING
                   T_TABLE      = GT_DIFF_001 ).

          CALL METHOD GO_ALV->SET_SCREEN_POPUP
            EXPORTING
              START_COLUMN = 15
              END_COLUMN   = 150
              START_LINE   = 5
              END_LINE     = 50.
        WHEN OTHERS.
      ENDCASE.

*         * Select lines 라인 선택
      GO_SELECT = GO_ALV->GET_SELECTIONS( ).
      GO_SELECT->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>ROW_COLUMN ).

    CATCH CX_SALV_MSG.
  ENDTRY.

  PERFORM SET_PF_STATUS USING PV.

  PERFORM SET_LAYOUT.

  PERFORM SET_TOP_OF_PAGE USING PV.
*
  PERFORM SET_EVENT.


  PERFORM SET_TABLE_SETTINGS USING PV.


  GO_ALV->DISPLAY( ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_PF_STATUS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PV
*&---------------------------------------------------------------------*
FORM SET_PF_STATUS  USING PV.
  DATA: LO_FUNCTIONS TYPE REF TO CL_SALV_FUNCTIONS,
        LT_FUNC_LIST TYPE SALV_T_UI_FUNC,
        LA_FUNC_LIST LIKE LINE OF LT_FUNC_LIST,
        LS_FUNC_LIST TYPE SALV_S_UI_FUNC.

  DATA: L_TITLE TYPE LVC_TITLE,
        L_LINES TYPE C LENGTH 100,
        L_COUNT TYPE I.

  DATA LV_TEXT TYPE CHAR100.


  GO_ALV->SET_SCREEN_STATUS(
    PFSTATUS      =  'STANDARD_T'
    REPORT        =  GV_REPID
    SET_FUNCTIONS = GO_ALV->C_FUNCTIONS_ALL ).

* GET ALL FUNCTIONS
  LO_FUNCTIONS =   GO_ALV->GET_FUNCTIONS( ).


  LT_FUNC_LIST = LO_FUNCTIONS->GET_FUNCTIONS( ).

* Now hide the MYFUNCTION
*  LOOP AT LT_FUNC_LIST INTO LA_FUNC_LIST.

**    IF LA_FUNC_LIST-R_FUNCTION->GET_NAME( ) = '&CREATE'.
**      LA_FUNC_LIST-R_FUNCTION->SET_VISIBLE( ' ' ).
**    ENDIF.

*  ENDLOOP.


  CASE PV.
    WHEN 0.
      L_TITLE = TEXT-BT0.
  ENDCASE.


* set output control : ZEBRA

  GO_DSPSET = GO_ALV->GET_DISPLAY_SETTINGS( ).

  GO_DSPSET->SET_STRIPED_PATTERN( ABAP_TRUE ).
  GO_DSPSET->SET_LIST_HEADER( L_TITLE ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LAYOUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_LAYOUT .

  GS_SALV_LAYOUT-REPID = SY-REPID.
  GS_SALV_LAYOUT-DEFAULT = 'X'.
  GS_SALV_LAYOUT-LAYOUT = '/STANDARD'.
  GS_KEY-REPORT = GV_REPID.


  GO_LAYOUT = GO_ALV->GET_LAYOUT( ).
  GO_LAYOUT->SET_KEY( GS_KEY ).

  GS_SALV_LAYOUT-RESTRICT = IF_SALV_C_LAYOUT=>RESTRICT_NONE.
  GO_LAYOUT->SET_DEFAULT( GS_SALV_LAYOUT-DEFAULT ).
  GO_LAYOUT->SET_SAVE_RESTRICTION( GS_SALV_LAYOUT-RESTRICT ).
  GO_LAYOUT->SET_INITIAL_LAYOUT( GS_SALV_LAYOUT-LAYOUT ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_SALV_FUNCTION
*&---------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND USING P_UCOMM TYPE SALV_DE_FUNCTION.

  DATA L_DUMMY TYPE C LENGTH 100.

  CASE P_UCOMM.

    WHEN '&SAVE'.
    WHEN '&HELP'.
      PERFORM CALL_POPUP_HELP(ZCAR9000)
               USING SY-REPID SY-DYNNR SY-LANGU ''.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_EVENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_EVENT .

*-- EVENT
  DATA: LR_EVENTS TYPE REF TO CL_SALV_EVENTS_TABLE.


  GO_FUNCTIONS = GO_ALV->GET_FUNCTIONS( ).

  GO_FUNCTIONS->SET_ALL( ).

  LR_EVENTS = GO_ALV->GET_EVENT( ).


  CREATE OBJECT G_EVENT_RECEIVER_SALV.
  SET HANDLER G_EVENT_RECEIVER_SALV->ON_USER_COMMAND FOR LR_EVENTS.
  SET HANDLER G_EVENT_RECEIVER_SALV->TOP_OF_PAGE     FOR LR_EVENTS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TABLE_SETTINGS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PV
*&---------------------------------------------------------------------*
FORM SET_TABLE_SETTINGS USING PV.

  DATA: LR_COLUMNS  TYPE REF TO CL_SALV_COLUMNS_TABLE.
  DATA: LO_AGGRS    TYPE REF TO CL_SALV_AGGREGATIONS.

  DATA : LR_COLUMN TYPE REF TO CL_SALV_COLUMN_TABLE.

  TRY.
      LR_COLUMNS = GO_ALV->GET_COLUMNS( ).
      LO_AGGRS   = GO_ALV->GET_AGGREGATIONS( ).

      CASE PV.
        WHEN '0'.

          TRY.
              CALL METHOD LO_AGGRS->ADD_AGGREGATION
                EXPORTING
                  COLUMNNAME  = 'RMENGE'
                  AGGREGATION = IF_SALV_C_AGGREGATION=>TOTAL.

              CALL METHOD LO_AGGRS->ADD_AGGREGATION
                EXPORTING
                  COLUMNNAME  = 'FMENGE'
                  AGGREGATION = IF_SALV_C_AGGREGATION=>TOTAL.

              CALL METHOD LO_AGGRS->ADD_AGGREGATION
                EXPORTING
                  COLUMNNAME  = 'RWRBTR'
                  AGGREGATION = IF_SALV_C_AGGREGATION=>TOTAL.


              CALL METHOD LO_AGGRS->ADD_AGGREGATION
                EXPORTING
                  COLUMNNAME  = 'RATE3'
                  AGGREGATION = IF_SALV_C_AGGREGATION=>TOTAL.

              CALL METHOD LO_AGGRS->ADD_AGGREGATION
                EXPORTING
                  COLUMNNAME  = 'RATE5'
                  AGGREGATION = IF_SALV_C_AGGREGATION=>TOTAL.



              CALL METHOD LO_AGGRS->ADD_AGGREGATION
                EXPORTING
                  COLUMNNAME  = 'D1WRBTR'
                  AGGREGATION = IF_SALV_C_AGGREGATION=>TOTAL.

            CATCH CX_SALV_DATA_ERROR .
            CATCH CX_SALV_NOT_FOUND .
            CATCH CX_SALV_EXISTING .
          ENDTRY.

        WHEN ''.
          CALL METHOD LO_AGGRS->ADD_AGGREGATION
            EXPORTING
              COLUMNNAME  = 'HSL'
              AGGREGATION = IF_SALV_C_AGGREGATION=>TOTAL.


        WHEN OTHERS.



      ENDCASE.


      TRY.
          LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( COLUMNNAME = 'BUKRS' ).
          LR_COLUMN->SET_VISIBLE( VALUE  = IF_SALV_C_BOOL_SAP=>FALSE ).

        CATCH CX_SALV_DATA_ERROR .
        CATCH CX_SALV_NOT_FOUND .
        CATCH CX_SALV_EXISTING .
      ENDTRY.



      LR_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ).
      LR_COLUMNS->SET_CELL_TYPE_COLUMN( 'CELLTYPE' ).




      .
    CATCH CX_SALV_DATA_ERROR.
  ENDTRY.

  PERFORM SET_COLUMNS_TECHNICAL USING LR_COLUMNS
                              LO_AGGRS.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_COLUMNS_TECHNICAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LR_COLUMNS
*&      --> LO_AGGRS
*&---------------------------------------------------------------------*
FORM SET_COLUMNS_TECHNICAL USING IR_COLUMNS TYPE REF TO
                                            CL_SALV_COLUMNS_TABLE
                                 IR_AGG TYPE REF TO
                                            CL_SALV_AGGREGATIONS.

  DATA LR_COLUMN  TYPE REF TO CL_SALV_COLUMN_TABLE.

  DATA: LR_SELECTIONS TYPE REF TO CL_SALV_SELECTIONS,
        LT_COLUMN     TYPE SALV_T_COLUMN.

  DATA LV_LTEXT TYPE SCRTEXT_L.
  DATA LV_MTEXT TYPE SCRTEXT_M.
  DATA LV_STEXT TYPE SCRTEXT_S.

  FIELD-SYMBOLS:
    <COLUMN_REF> LIKE LINE OF GT_COLUMN_REF.

  GT_COLUMN_REF = IR_COLUMNS->GET( ).

  TRY.
      LOOP AT GT_COLUMN_REF
        ASSIGNING <COLUMN_REF>.
        GR_COLUMN ?= IR_COLUMNS->GET_COLUMN( <COLUMN_REF>-COLUMNNAME ).



        PERFORM SET_COLUMN_TEXT USING    <COLUMN_REF>-COLUMNNAME
                                CHANGING GV_COLUMN_TEXT.



        IF GV_COLUMN_TEXT IS NOT INITIAL.

          GV_SCRTEXT_S = GV_COLUMN_TEXT.
          GV_SCRTEXT_M = GV_COLUMN_TEXT.
          GV_SCRTEXT_L = GV_COLUMN_TEXT.

          GR_COLUMN->SET_SHORT_TEXT( GV_SCRTEXT_S ).
          GR_COLUMN->SET_MEDIUM_TEXT( GV_SCRTEXT_M ).
          GR_COLUMN->SET_LONG_TEXT( GV_SCRTEXT_L ).
        ENDIF.

      ENDLOOP.

    CATCH CX_SALV_NOT_FOUND.
    CATCH CX_SALV_DATA_ERROR.
    CATCH CX_SALV_EXISTING.

  ENDTRY.
*
*  TRY.
*      LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( 'STATUS' ).
*      LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
*    CATCH CX_SALV_NOT_FOUND.
*  ENDTRY.

*-- SELECT FIELD 추가

  LR_SELECTIONS = GO_ALV->GET_SELECTIONS( ).
  LR_SELECTIONS->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>CELL ).


*  IR_AGG->SET_AGGREGATION_BEFORE_ITEMS( ).

ENDFORM. " SET_COLUMNS_TECHNICAL
*&---------------------------------------------------------------------*
*& Form SET_COLUMN_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <COLUMN_REF>_COLUMNNAME
*&      <-- GV_COLUMN_TEXT
*&---------------------------------------------------------------------*
FORM SET_COLUMN_TEXT USING P_COLUMNNAME
                      CHANGING P_COLUMN_TEXT.
  DATA : LS_COLOR    TYPE  LVC_S_COLO.
  DATA : LV_MONTH   TYPE N LENGTH 2.

  DATA L_FIELD TYPE LVC_CFNAME VALUE 'TWAER'.

  TRY.

      CLEAR P_COLUMN_TEXT.

      CASE P_COLUMNNAME.
        WHEN 'RBUKRS'.
          P_COLUMN_TEXT = '회사코드'.
*          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'GJAHR'.
          P_COLUMN_TEXT = '회계년도'.
*          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'RACCT'.
          P_COLUMN_TEXT = '계정'.
*          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'TXT20'.
          P_COLUMN_TEXT = '계정명'.
*          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'WAERS' OR 'TWAER'.
          P_COLUMN_TEXT = '통화'.

          IF P_COLUMNNAME CP 'HSL*' OR P_COLUMNNAME  = 'RWRBTR'.
*        CONCATENATE P_COLUMNNAME+4(2) TEXT-F12 INTO P_COLUMN_TEXT.
            P_COLUMN_TEXT =  '금액' .
            GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).


            LS_COLOR-COL = COL_POSITIVE .
            LS_COLOR-INT = 1.
            LS_COLOR-INV = 0.
            GR_COLUMN->SET_COLOR( LS_COLOR ).
          ENDIF.

      ENDCASE.
    CATCH CX_SALV_NOT_FOUND.
    CATCH CX_SALV_DATA_ERROR.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PV
*&---------------------------------------------------------------------*
FORM SET_TOP_OF_PAGE  USING PV.
  DATA: LR_CONTENT TYPE REF TO CL_SALV_FORM_ELEMENT.
  PERFORM BUILT_HEADER  USING PV
                        CHANGING LR_CONTENT.

  GO_ALV->SET_TOP_OF_LIST( LR_CONTENT ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILT_HEADER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PV
*&      <-- LR_CONTENT
*&---------------------------------------------------------------------*
FORM BUILT_HEADER USING PV
                  CHANGING CR_CONTENT TYPE REF TO CL_SALV_FORM_ELEMENT.
  DATA: LR_GRID   TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
        LR_GRID_1 TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
        LR_LABEL  TYPE REF TO CL_SALV_FORM_LABEL,
        LR_TEXT   TYPE REF TO CL_SALV_FORM_TEXT,
        LV_TT     TYPE STRING,
        L_TEXT    TYPE STRING.

  CREATE OBJECT LR_GRID.

  LR_GRID_1 = LR_GRID->CREATE_GRID(
                ROW    = 1
                COLUMN = 1 ).

  CLEAR LV_TT.

  CASE PV.
    WHEN 0.
      LV_TT = 'MM 재료비'.
    WHEN 1.
      LV_TT = ' FI/MM  비교'.

  ENDCASE.

  CONCATENATE  GV_WE_NM  ':' PA_MONTH+0(4) '년'   PA_MONTH+4(2) '월'
   LV_TT INTO L_TEXT ." SEPARATED BY SPACE.
  CONDENSE L_TEXT.

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
   ROW    = 1
   COLUMN = 1
   TEXT    = L_TEXT
   TOOLTIP = L_TEXT ).

  CR_CONTENT = LR_GRID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM MODIFY_SCREEN .

  LOOP AT SCREEN.
**    IF P_RA1 = 'X'.
**      IF SCREEN-GROUP1 = 'D' .
**        SCREEN-ACTIVE = '0'.
**      ENDIF.
**
**    ELSEIF P_RA2 = 'X'.
***      IF SCREEN-GROUP1 = 'C' .
***        SCREEN-ACTIVE = '0'.
***      ENDIF.
**
**      IF SCREEN-NAME CP '*P_MODE*'.
**        SCREEN-ACTIVE = '0'.
**      ENDIF.
**
**    ENDIF .

    IF SCREEN-NAME = 'PA_KOKRS' OR
     SCREEN-NAME = 'PA_BUKRS'.

      SCREEN-INPUT = 0.

    ENDIF.

* 제품 체크 제외는 옵션은 숨기고 DEFAULT 제품만 조회됨

    IF SCREEN-NAME CP '*P_CHK*'.
      SCREEN-ACTIVE = '0'.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  CLEAR: GV_TITLE, GT_EXCLUDE[].


  GV_TITLE =  GV_WE_NM  && TEXT-TX1 && ' ' && PA_MONTH+0(4) && '년'  &&  PA_MONTH+4(2) &&'월 )'.


  SET PF-STATUS 'PF_0100' EXCLUDING GT_EXCLUDE  .
  SET TITLEBAR  'TT_0100' WITH GV_TITLE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
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

  CREATE OBJECT GR_GRID1
    EXPORTING
      I_PARENT = CL_GUI_CONTAINER=>SCREEN0.

  "CL_GUI_CONTAINER=>SCREEN0 : Dummy for Top Level 0 Screen Container


ENDFORM.
*&---------------------------------------------------------------------*
*& Form INIT_LAYOUT_0100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM INIT_LAYOUT_0100 .

*  GS_LAYOUT-EDIT_MODE  = ABAP_TRUE.
*  GS_LAYOUT-ZEBRA      = ABAP_FALSE.
*  GS_LAYOUT-CWIDTH_OPT = ABAP_TRUE.
  GS_LAYOUT-STYLEFNAME = 'STYLE'.
  GS_LAYOUT-SEL_MODE   = 'D'.     "B:단일,C:복수,D:셀,A:행/열
  GS_LAYOUT-BOX_FNAME  = SPACE.
  GS_LAYOUT-NO_ROWMARK = SPACE.

*  GS_LAYOUT-CTAB_FNAME = 'COLOR'.
*  GS_LAYOUT-INFO_FNAME = 'INFO'.

**  "alv title
**  GS_LAYOUT-GRID_TITLE = TEXT-GT1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_GRID_EXCLUDE_0100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
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
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_SORT_0100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ALV_SORT_0100 .

  CLEAR: GS_SORT, GT_SORT.
  REFRESH: GT_SORT.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_FIELDCAT_0100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM APPEND_FIELDCAT_0100 .
  "-- field catalog data
  "   field catalog merge or set fieldcatalog를 사용할 수 있음.

  "{ FIELDCATLOG MERGE 사용
  PERFORM GET_FIELDCATLOG_DATA(ZCAR9000) TABLES GT_FIELDCAT
                                          USING 'ZCOS0451'
                                             IF FOUND.

  PERFORM MODIFY_FIELDCATLOG_DATA.
  "}

  "{ SET FIELDCATLOG 사용
*  PERFORM SET_FIELDCATLOG_DATA.
  "}

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM MODIFY_FIELDCATLOG_DATA .

  DATA:  LV_TEXT(50).

  "--- Change Fieldcat.
  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.
    CLEAR: LV_TEXT.

    "-- Change fieldcat Attribute
    CASE GS_FIELDCAT-FIELDNAME.
      WHEN 'STATS'.
        GS_FIELDCAT-JUST = GC_C.
        GS_FIELDCAT-OUTPUTLEN = 4.
        LV_TEXT = TEXT-F77.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.

      WHEN 'ICON'.
        GS_FIELDCAT-JUST = GC_C.
        GS_FIELDCAT-OUTPUTLEN = 4.
        GS_FIELDCAT-FIX_COLUMN = 'X'.
        LV_TEXT = TEXT-F01.
      WHEN 'BELNR1'.
        LV_TEXT = TEXT-F02.

        GS_FIELDCAT-NO_OUT = ABAP_TRUE.

      WHEN 'BELNR2'.
        LV_TEXT = TEXT-F13.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.

      WHEN 'BELNR3'.
        LV_TEXT = TEXT-F03.

      WHEN 'BUKRS'.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.

      WHEN 'D1WRBTR' OR  'D2WRBTR' OR 'D3WRBTR' OR 'D4WRBTR'
            OR 'LWRBTR'  .

        GS_FIELDCAT-JUST = GC_R.
        GS_FIELDCAT-NO_ZERO = GC_X.
        GS_FIELDCAT-OUTPUTLEN = '22'.
        GS_FIELDCAT-CFIELDNAME = 'TWAER' .
*        GS_FIELDCAT-EMPHASIZE = 'C411'.
        GS_FIELDCAT-DO_SUM = 'X'.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.

      WHEN  'EWRBTR' OR   'ZCOGS1' OR 'DMBTR'.

        GS_FIELDCAT-JUST = GC_R.
        GS_FIELDCAT-NO_ZERO = GC_X.
        GS_FIELDCAT-OUTPUTLEN = '22'.
        GS_FIELDCAT-CFIELDNAME = 'TWAER' .
*        GS_FIELDCAT-EMPHASIZE = 'C411'.
        GS_FIELDCAT-DO_SUM = 'X'.


      WHEN   'ZCOGS1'.

        GS_FIELDCAT-JUST = GC_R.
        GS_FIELDCAT-NO_ZERO = GC_X.
        GS_FIELDCAT-OUTPUTLEN = '22'.
        GS_FIELDCAT-CFIELDNAME = 'TWAER' .
*        GS_FIELDCAT-EMPHASIZE = 'C411'.
        GS_FIELDCAT-DO_SUM = 'X'.

*        GS_FIELDCAT-EDIT = ABAP_TRUE.


      WHEN 'EMEINS'.
        LV_TEXT = TEXT-G02.  "단위
        GS_FIELDCAT-DECIMALS = 3.
      WHEN 'EMENGE'. "기말수량
        LV_TEXT = TEXT-G01.
        GS_FIELDCAT-QFIELDNAME = 'EMEINS' .
      WHEN 'FMEINS'.
        GS_FIELDCAT-DECIMALS = 3.
        LV_TEXT = TEXT-G02.  "단위
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
      WHEN 'FMENGE'. "생산수량
        GS_FIELDCAT-QFIELDNAME = 'FMEINS' .
        LV_TEXT = TEXT-F11.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
      WHEN 'FMATNR'."제품코드
        GS_FIELDCAT-LZERO = 'X'.
        GS_FIELDCAT-OUTPUTLEN = 10.
        LV_TEXT = TEXT-F06.
        GS_FIELDCAT-F4AVAILABL = GC_X.

*        GS_FIELDCAT-EDIT = GC_X.
*        GS_FIELDCAT-JUST = GC_C.

*        GS_FIELDCAT-FIX_COLUMN = 'X'.
*        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
*        GS_FIELDCAT-EMPHASIZE = 'C711'.
      WHEN 'FMATNR_MAKTX' ."제품코드명 .
        LV_TEXT = TEXT-F10.
        GS_FIELDCAT-OUTPUTLEN = 20.
        GS_FIELDCAT-FIX_COLUMN = 'X'.
*        GS_FIELDCAT-EMPHASIZE = 'C711'.
      WHEN 'LIFNR'.
        GS_FIELDCAT-OUTPUTLEN = 10.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        GS_FIELDCAT-FIX_COLUMN = 'X'.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
      WHEN 'NAME1'.
        LV_TEXT = TEXT-F04.
        GS_FIELDCAT-OUTPUTLEN = 8.
        GS_FIELDCAT-FIX_COLUMN = 'X'.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
      WHEN 'MATKL'.
        GS_FIELDCAT-OUTPUTLEN = 10.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        GS_FIELDCAT-FIX_COLUMN = 'X'.
      WHEN 'WGBEZ'.
        LV_TEXT = TEXT-F07. "자재그룹명
        GS_FIELDCAT-OUTPUTLEN = 10.
        GS_FIELDCAT-FIX_COLUMN = 'X'.
      WHEN 'RMATNR'.  "원자재
        GS_FIELDCAT-LZERO = 'X'.
        LV_TEXT = TEXT-F08.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
*        GS_FIELDCAT-OUTPUTLEN = 10.
*        GS_FIELDCAT-FIX_COLUMN = 'X'.
        GS_FIELDCAT-EMPHASIZE = 'C310'.

        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
      WHEN 'RMATNR_MAKTX'.
        LV_TEXT = TEXT-F18.
*        GS_FIELDCAT-EMPHASIZE = 'C310'.
        GS_FIELDCAT-FIX_COLUMN = 'X'.
        GS_FIELDCAT-OUTPUTLEN = 8.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
      WHEN 'RMEINS'.
        GS_FIELDCAT-DECIMALS = 3.
        LV_TEXT = TEXT-G02.  "단위
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
      WHEN 'RMENGE'.
        GS_FIELDCAT-QFIELDNAME = 'RMEINS' .
        LV_TEXT = TEXT-F12.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
      WHEN 'SMEINS'.
        GS_FIELDCAT-DECIMALS = 3.
        LV_TEXT = TEXT-G02.  "단위
*        GS_FIELDCAT-EDIT = ABAP_TRUE.
      WHEN 'SMENGE'.
        GS_FIELDCAT-QFIELDNAME = 'SMEINS' .
*        GS_FIELDCAT-EDIT = ABAP_TRUE.
      WHEN 'BWRBTR'.
*        GS_FIELDCAT-DECIMALS = 3.
        LV_TEXT = TEXT-G10.  " 기초이월 금액
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.

        GS_FIELDCAT-JUST = GC_R.
        GS_FIELDCAT-NO_ZERO = GC_X.
        GS_FIELDCAT-OUTPUTLEN = '22'.
        GS_FIELDCAT-CFIELDNAME = 'TWAER' .
*        GS_FIELDCAT-EMPHASIZE = 'C411'.
      WHEN 'BMEINS'.
        GS_FIELDCAT-DECIMALS = 3.
        LV_TEXT = TEXT-G11.  " 기초이월 단위
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.

      WHEN 'BMENGE'.
        GS_FIELDCAT-QFIELDNAME = 'BMEINS' .
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        LV_TEXT = TEXT-G12.  " 기초이월 수량
      WHEN 'SPMON'.
        GS_FIELDCAT-OUTPUTLEN = 10.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
      WHEN 'TWAER'.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
      WHEN 'WERKS'.
*        GS_FIELDCAT-NO_OUT = ABAP_TRUE.

      WHEN 'WERKS_NAME'.
        LV_TEXT = TEXT-F05.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        GS_FIELDCAT-OUTPUTLEN = 10.

      WHEN 'DMBTR'.
        LV_TEXT =  TEXT-F66.
*        LV_TEXT = '제조매출(FI)'.
      WHEN  'MSG'.
        LV_TEXT = 'ICON 설명'.

*                GS_FIELDCAT-FIX_COLUMN = 'X'.
      WHEN OTHERS.
*      WHEN 'AEDAT'.
*      WHEN 'AENAM'.
*      WHEN 'AEZET'.
*      WHEN 'ERDAT'.
*      WHEN 'ERNAM'.
*      WHEN 'ERZET'.

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
ENDFORM.
*&---------------------------------------------------------------------*
*& Form REGIST_ALV_EVENT_0100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GR_GRID1
*&---------------------------------------------------------------------*
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

ENDFORM.                    " REGIST_ALV_EVENT_0100
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV_GRID_0100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DISPLAY_ALV_GRID_0100 .
  GS_VARIANT-REPORT = SY-REPID.
  GS_VARIANT-HANDLE = SY-DYNNR.

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
ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_GRID_0100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM REFRESH_GRID_0100 .
  GS_STABLE-ROW = ABAP_TRUE. "Row
  GS_STABLE-COL = ABAP_TRUE. "column

  CALL METHOD GR_GRID1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = GS_STABLE
      I_SOFT_REFRESH = SPACE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_WERKS_NAME
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_WERKS_NAME .

  SELECT SINGLE  A~NAME1
    FROM T001W AS A
   WHERE A~WERKS = @PA_WERKS
    INTO  @DATA(GV_WE_NM).

ENDFORM.
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
*& Form GET_GSBER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_GSBER .
  SELECT * INTO TABLE GT_T134G
    FROM T134G
  WHERE WERKS = PA_WERKS.

**19AA 플랜트는 발생안한다   김호준위원  20200306

  REFRESH GR_GSBER.
  CLEAR GR_GSBER.

  GR_GSBER-SIGN = 'I'.
  GR_GSBER-OPTION = 'EQ'.

  LOOP AT GT_T134G ASSIGNING FIELD-SYMBOL(<$GG>).

    GR_GSBER-LOW = <$GG>-GSBER.
    COLLECT  GR_GSBER.

  ENDLOOP.




  CLEAR  GR_POSID.
  REFRESH GR_POSID.

  GR_POSID-SIGN = 'I'.
  GR_POSID-OPTION = 'CP'.
  GR_POSID-LOW = 'M*'.

  APPEND GR_POSID.

* 플랜트별  wbs 설정  m  로 시작하는거만...

  PERFORM GET_WBS_ZSDT0120.

  CLEAR  GR_POSID.
  REFRESH GR_POSID.

  GR_POSID-SIGN = 'I'.
  GR_POSID-OPTION = 'EQ'.

  DATA : LV_WBS LIKE PRPS-POSID.

  LOOP AT GT_WBS ASSIGNING FIELD-SYMBOL(<WBS>).

    GR_POSID-LOW =  <WBS>-POSID.
    COLLECT GR_POSID.

  ENDLOOP.


* 매출원가 계정 WBS

  SELECT A~POSID, A~SAKNR, B~TXT20
    FROM ZCOT1230 AS A
   INNER JOIN SKAT AS B
      ON A~SAKNR = B~SAKNR
     AND B~SPRAS = @SY-LANGU
     AND B~KTOPL = '1000'
    INTO TABLE @GT_ZCOT1230
   WHERE A~BUKRS = @PA_BUKRS.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_WBS_ZSDT0120
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_WBS_ZSDT0120 .



  CLEAR : GT_WBS, GT_WBS[].

  SELECT POSID
      FROM PRPS AS A INNER JOIN ZSDT0120 AS B
                       ON B~WBSNR EQ A~PSPNR
     WHERE B~MODUL = 'M'
       AND B~BUKRS = @PA_BUKRS
       AND B~WERKS EQ @PA_WERKS
    INTO  TABLE @GT_WBS.



  CLEAR : GT_WBS_COGS, GT_WBS_COGS[].

  SELECT POSID
      FROM PRPS AS A INNER JOIN ZSDT0120 AS B
                       ON B~WBSNR EQ A~PSPNR
     WHERE B~MODUL = 'S'
       AND B~BUKRS = @PA_BUKRS
       AND B~WERKS EQ @PA_WERKS
    INTO  TABLE @GT_WBS_COGS.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_FI_REV_GL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_FI_REV_GL .
  RANGES : LR_RACCT FOR ACDOCA-RACCT.

  REFRESH LR_RACCT. CLEAR LR_RACCT.

  LR_RACCT = 'IEQ'.
  LR_RACCT-LOW = '0402101001' .
  APPEND LR_RACCT.
  LR_RACCT-LOW = '0402101003' .
  APPEND LR_RACCT.

  CLEAR : GT_FI_REV, GT_FI_REV[].

  PERFORM SET_RANGE_DATA.

  SELECT  A~MATNR AS MATNR,
          SUM( A~HSL  ) AS HSL
     FROM ACDOCA AS A
    WHERE A~RLDNR = '0L'
      AND A~RBUKRS = @PA_BUKRS
      AND A~WERKS = @PA_WERKS
      AND A~BUDAT IN @GR_BUDAT
      AND A~RACCT IN @LR_RACCT
*      AND A~RACCT IN ( '0402101001' ,'0402101003' )
     GROUP BY MATNR
     ORDER BY MATNR
      INTO  CORRESPONDING FIELDS OF TABLE @GT_FI_REV.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_REVERSE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_DISPLAY
*&---------------------------------------------------------------------*
FORM CHECK_REVERSE  USING    PS STRUCTURE GS_DISPLAY.

  DATA : LV_STBLG LIKE BKPF-STBLG.
  CLEAR LV_STBLG.
  SELECT SINGLE STBLG
   INTO LV_STBLG
    FROM BKPF
   WHERE BUKRS = PS-BUKRS
     AND BELNR = PS-BELNR1
     AND GJAHR = PA_MONTH+0(4).

  IF SY-SUBRC EQ 0 AND LV_STBLG IS NOT INITIAL.

    CLEAR : PS-BELNR1.

  ENDIF.

  CLEAR LV_STBLG.
  SELECT SINGLE STBLG
  INTO LV_STBLG
   FROM BKPF
  WHERE BUKRS = PS-BUKRS
    AND BELNR = PS-BELNR3
     AND GJAHR = PA_MONTH+0(4).

  IF SY-SUBRC EQ 0 AND LV_STBLG IS NOT INITIAL.

    CLEAR : PS-BELNR3.

    DELETE FROM ZCOT1260       WHERE BUKRS = @PA_BUKRS
      AND SPMON = @PA_MONTH
      AND WERKS = @PA_WERKS
      AND FMATNR = @PS-FMATNR
      AND BELNR3 = @PS-BELNR3.

    IF SY-SUBRC EQ 0.
      COMMIT WORK AND WAIT.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ENDIF.

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
*& Form REFRESH_GRID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM REFRESH_GRID .

  GS_STABLE-ROW = ABAP_TRUE. "Row
  GS_STABLE-COL = ABAP_TRUE. "column
*-- checked change data
  CALL METHOD GR_GRID1->CHECK_CHANGED_DATA( ).
  CALL METHOD GR_GRID1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = GS_STABLE
      I_SOFT_REFRESH = SPACE.

  CALL METHOD CL_GUI_CFW=>FLUSH.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REGIST_ALV_F4_0100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GR_GRID1
*&---------------------------------------------------------------------*
FORM REGIST_ALV_F4_0100  USING    PR_GRID TYPE REF TO CL_GUI_ALV_GRID.

  DEFINE _REGIST_F4.
    GS_F4-FIELDNAME = &1.
    GS_F4-REGISTER  = ABAP_TRUE.
    INSERT GS_F4 INTO TABLE GT_F4.
  END-OF-DEFINITION.

  CLEAR: GT_F4, GS_F4.

  _REGIST_F4 : 'FMATNR'.


  CALL METHOD PR_GRID->REGISTER_F4_FOR_FIELDS
    EXPORTING
      IT_F4 = GT_F4[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_LIFNR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_DISPLAY
*&---------------------------------------------------------------------*
FORM GET_LIFNR  USING   PS_DISPLAY STRUCTURE GS_DISPLAY.

  CLEAR :  GS_LFA1-MATKL,
           GS_LFA1-LIFNR.

  SELECT SINGLE   A~MATKL   A~WRKST AS LIFNR    "
 FROM MARA AS A
    INTO (PS_DISPLAY-MATKL,  PS_DISPLAY-LIFNR )
  WHERE A~MATNR =  PS_DISPLAY-RMATNR
    AND A~LVORM = ' '.



*  구매처명
  SELECT SINGLE NAME1 INTO PS_DISPLAY-NAME1
    FROM LFA1
  WHERE LIFNR = PS_DISPLAY-LIFNR.





ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_DISP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LT>
*&---------------------------------------------------------------------*
FORM APPEND_DISP  USING   PS STRUCTURE ZCOT1260
                           PV  TYPE DMBTR.

  MOVE-CORRESPONDING PS TO GS_DISPLAY.

* *자재그룹명
  SELECT SINGLE WGBEZ INTO GS_DISPLAY-WGBEZ
    FROM T023T
   WHERE MATKL = GS_DISPLAY-MATKL.

* 제품자재명
  SELECT SINGLE MAKTX INTO GS_DISPLAY-FMATNR_MAKTX
    FROM MAKT
   WHERE SPRAS = SY-LANGU
    AND MATNR = GS_DISPLAY-FMATNR.

* 원자재  자재명
  SELECT SINGLE MAKTX INTO GS_DISPLAY-RMATNR_MAKTX
    FROM MAKT
   WHERE SPRAS = SY-LANGU
    AND MATNR = GS_DISPLAY-RMATNR.

*  구매처명
  SELECT SINGLE NAME1 INTO GS_DISPLAY-NAME1
    FROM LFA1
  WHERE LIFNR = GS_DISPLAY-LIFNR.



  IF  GS_DISPLAY-BELNR3 IS NOT INITIAL.

    GS_DISPLAY-ICON = ICON_LED_GREEN.
  ELSE.
    GS_DISPLAY-ICON = ICON_LED_YELLOW.

  ENDIF.

*역분개 여부 추가
  IF  GS_DISPLAY-BELNR3 IS NOT INITIAL.
    PERFORM CHECK_REVERSE USING GS_DISPLAY.
  ENDIF.


  SELECT SINGLE MTART
        INTO GS_DISPLAY-MTART
        FROM MARA
      WHERE MATNR = GS_DISPLAY-FMATNR.

  CLEAR GT_STYLE[].
  _STYLE_DISABLED : 'FMATNR' , 'SMEINS' , 'SMENGE',
                    'SWRBTR'.

  GS_DISPLAY-STYLE[] = GT_STYLE[].
  GS_DISPLAY-DMBTR = PV. " FI 매출액



  APPEND GS_DISPLAY TO GT_DISPLAY.
  CLEAR GS_DISPLAY.


  SORT GT_DISPLAY BY FMATNR.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_DATA_RTN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_DATA_RTN .

  PERFORM CLEAR_ITAB.

  IF GT_ROWS[] IS INITIAL.
    MESSAGE S015.
    GV_EXIT = ABAP_TRUE.
  ENDIF.
  CHECK GV_EXIT EQ SPACE.
  PERFORM CHECK_SELECT_DATA  USING GC_C.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEAR_ITAB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CLEAR_ITAB .
  CLEAR: GT_BK,GT_BK[],
         GT_BS, GT_BS[],
         GS_IBKPF,GT_IBKPF[],
         GS_IBSEG,GT_IBSEG[],
         GS_ISELK,GT_ISELK[],
         GS_IBSELP, GT_IBSELP[],
         GS_SELP, GT_SELP[],
         GS_DISPLAY , GV_EXIT , GS_ROWS .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_SELECT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GC_C
*&---------------------------------------------------------------------*
FORM CHECK_SELECT_DATA  USING PV_VAL.
  CLEAR : GS_ROWS , GS_DISPLAY .

  LOOP AT GT_ROWS INTO GS_ROWS.
    READ TABLE GT_DISPLAY ASSIGNING <FS_DISP> INDEX GS_ROWS-INDEX.
    CASE PV_VAL .
      WHEN 'C' ." 매출원가 기표
        "전표 처리 상태 체크
        IF <FS_DISP>-BELNR3 IS NOT INITIAL .

        ELSEIF <FS_DISP>-ZCOGS1 IS  INITIAL.

          <FS_DISP>-MSG = <FS_DISP>-ZCOGS1 && TEXT-E11 .

        ELSE.


* 동일 제품 자재가  매출원가 기표 있으면 오류  제외처리
          SELECT SINGLE *
            FROM ZCOT1260
           WHERE BUKRS =  @PA_BUKRS
             AND SPMON = @PA_MONTH
             AND FMATNR = @<FS_DISP>-FMATNR
             AND WERKS = @<FS_DISP>-WERKS
             AND BELNR3 NE ''
            INTO  @DATA(LS_1260).

          IF SY-SUBRC NE 0.
            PERFORM MAKE_DATA USING 'ZZ'.
            PERFORM BDC_FB01 USING 'ZZ'.
          ELSE.
            <FS_DISP>-MSG = LS_1260-BELNR3 && LS_1260-BELNR1   && TEXT-E77 .
          ENDIF.

        ENDIF.
      WHEN OTHERS.
    ENDCASE.

  ENDLOOP .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_DATA
*&---------------------------------------------------------------------*
*& 현재 사용되는 ZCOR0380 프로그램의 매출원가 전기 로직 참조  SPEC  매출원가전표

*&---------------------------------------------------------------------*
*&      --> GC_A
*&---------------------------------------------------------------------*
FORM MAKE_DATA  USING    PV_VAL.

  DATA: LV_SEQ TYPE VVNLFDNUM.
  CLEAR : LV_SEQ , GT_BK, GT_BS, GV_SPMON,
  GT_BK[], GT_BS[].

  DATA: LV_WRBTR   LIKE GS_DISPLAY-ZCOGM,
        LV_WRBTR_C TYPE C LENGTH 20.

  GV_SPMON    =  PA_MONTH && '01'.


  "입력기간 말일
  PERFORM RP_LAST_DAY_OF_MONTHS(ZCAR9000) USING GV_SPMON
                                       CHANGING GV_SPMON
                                             IF FOUND.

  CASE PV_VAL.

    WHEN 'ZZ'.  "<<<< 매출원가 기표

      "Header
      CLEAR GT_BK.
      GT_BK-SERIAL =  '0001'.   " 순번
      GT_BK-TCODE = 'FB01'.   " 트랜잭션 코드
      GT_BK-BLART = GC_WZ. " 전표유형
      GT_BK-BUKRS = PA_BUKRS. "회사코드
      GT_BK-BLDAT = GV_SPMON. " 증빙일자
      GT_BK-BUDAT = GV_SPMON. "전기일자
      GT_BK-WAERS = GC_KRW.  "<FS_DISP>-WAERS.  "거래통화
      GT_BK-BKTXT = GV_SPMON+(6) && TEXT-H07.  "전표 헤더텍스트_ 매출원가
      GT_BK-XBLNR = ''.

      APPEND GT_BK. CLEAR GT_BK.


      READ TABLE GT_ZCOT1230 INTO GS_ZCOT1230 INDEX 1.
      READ TABLE GT_WBS_COGS INTO GS_WBS_COGS INDEX 1.

      DO 2 TIMES.
        LV_SEQ = LV_SEQ + 1.
        GT_BS-SERIAL = '0001'. "일련번호
        GT_BS-ITMNO = LV_SEQ . "라인 일련번호

        CASE LV_SEQ.
          WHEN 1. "첫번째라인
            IF  <FS_DISP>-ZCOGS1 >  0 .
              GT_BS-NEWBS = '40'. "전기키  "차변
              GT_BS-NEWKO =  GS_ZCOT1230-SAKNR.  " 제품매출원가

            ELSE .
              GT_BS-NEWBS = '50'.
              GT_BS-NEWKO = GS_ZCOT1230-SAKNR..

            ENDIF.


            GT_BS-PROJK  =  GS_WBS_COGS-POSID.



          WHEN 2. "두번째라인
            IF  <FS_DISP>-ZCOGS1 >  0 .
              GT_BS-NEWBS = '50'. "전기키  "차변
              GT_BS-NEWKO = '0101302001 '.  " 제품
            ELSE .
              GT_BS-NEWBS = '40'.
              GT_BS-NEWKO = '0101302001'.
            ENDIF.


        ENDCASE.

        LV_WRBTR =   ABS( <FS_DISP>-ZCOGS1 ).
        LV_WRBTR_C =  LV_WRBTR. CONDENSE LV_WRBTR_C.
        PERFORM CURR_SAP_TO_IDOC(ZCAR9000) USING GC_KRW LV_WRBTR_C
                                           IF FOUND.



        GT_BS-WRBTR = LV_WRBTR_C.

*19AA = 1900 인데   제조원가 발생안한다고 함
        IF PA_WERKS = '19AA'.
          GT_BS-GSBER  = '1900'.
        ELSE.
          GT_BS-GSBER = <FS_DISP>-WERKS.       "사업영역 = 플랜트
        ENDIF.

        GT_BS-WRBTR = LV_WRBTR_C.
        GT_BS-SGTXT =  '[CO] 제품별 매출원가 차감 전기'.


        GT_BS-MATNR = <FS_DISP>-FMATNR.

        APPEND GT_BS.  CLEAR: GT_BS.

      ENDDO.


  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_FB01
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form BDC_FB01
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GC_A
*&---------------------------------------------------------------------*
FORM BDC_FB01  USING    PV_VAL.
  DATA : LV_LINES TYPE SY-TABIX.

  CLEAR: GV_MESSAGE.

  CALL FUNCTION 'Z_FI_CREATE_DOC'
    EXPORTING
      RFBIFUNCT       = 'C'
      CMODE           = P_MODE
    TABLES
      IBKPF           = GT_BK
      IBSEG           = GT_BS
      T_FIMSG         = GT_FIMSG
      IWITH           = GT_WI
      ITTAX           = GT_TA
    EXCEPTIONS
      NO_POSTING_DATA = 1
      ERROR_MODE      = 2
      POSTING_ERROR   = 3
      OTHERS          = 4.


  " 결과 화면에 출력 및 db update
  " 성공건이 있는지 확인
  READ TABLE GT_FIMSG WITH KEY MSGID = 'F5' MSGTY = 'S' MSGNO = '312'.
  IF SY-SUBRC = 0.
    " 성공 건
    PERFORM MESSAGE_TEXT_BUILD USING GT_FIMSG-MSGID
                                 GT_FIMSG-MSGNO
                                 GT_FIMSG-MSGV1
                                 GT_FIMSG-MSGV2
                                 GT_FIMSG-MSGV3
                                 GT_FIMSG-MSGV4
                               CHANGING GV_MESSAGE.       "결과 메시지
    CASE PV_VAL.
*      WHEN 'YY'.
*        <FS_DISP>-BELNR1 = GT_FIMSG-MSGV1. "전표번호
      WHEN 'ZZ'.
        <FS_DISP>-BELNR3 = GT_FIMSG-MSGV1. "전표번호 매출원가

    ENDCASE.

    <FS_DISP>-ICON = '@08@'." 성공
    <FS_DISP>-MSG =  GV_MESSAGE ."

* 필드 비활성화
    CLEAR : GT_STYLE[].
    _STYLE_DISABLED : 'FMATNR' , 'SMEINS' , 'SMENGE',
                         'SWRBTR' , 'ZCOGS1'.

    <FS_DISP>-STYLE[] = GT_STYLE[].

    PERFORM SAVE_1260.


  ELSE.
    READ TABLE GT_FIMSG WITH KEY MSGTY = GC_E.
    IF SY-SUBRC NE 0 .
      CLEAR LV_LINES.
      DESCRIBE TABLE GT_FIMSG LINES LV_LINES.
      READ TABLE GT_FIMSG INDEX LV_LINES.
    ENDIF.
*    READ TABLE GT_FIMSG INDEX 1.
    " 실패건
    PERFORM MESSAGE_TEXT_BUILD USING GT_FIMSG-MSGID
                                 GT_FIMSG-MSGNO
                                 GT_FIMSG-MSGV1
                                 GT_FIMSG-MSGV2
                                 GT_FIMSG-MSGV3
                                 GT_FIMSG-MSGV4
                               CHANGING GV_MESSAGE.       "결과 메시지
    <FS_DISP>-ICON = '@0A@'."에러
    <FS_DISP>-MSG =  GV_MESSAGE ."에러

    GV_ERROR = ABAP_TRUE. "각 단계 ERROR체크
  ENDIF.

  CLEAR: GT_BK,GT_BK[],
      GT_BS, GT_BS[],
      GT_FIMSG, GT_FIMSG[].
ENDFORM.
*&---------------------------------------------------------------------*
*& Form MESSAGE_TEXT_BUILD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_FIMSG_MSGID
*&      --> GT_FIMSG_MSGNO
*&      --> GT_FIMSG_MSGV1
*&      --> GT_FIMSG_MSGV2
*&      --> GT_FIMSG_MSGV3
*&      --> GT_FIMSG_MSGV4
*&      <-- GV_MESSAGE
*&---------------------------------------------------------------------*
FORM MESSAGE_TEXT_BUILD  USING   PV_MSGID
                        PV_MSGNR
                        PV_MSGV1
                        PV_MSGV2
                        PV_MSGV3
                        PV_MSGV4
               CHANGING PV_MESG.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      MSGID               = PV_MSGID
      MSGNR               = PV_MSGNR
      MSGV1               = PV_MSGV1
      MSGV2               = PV_MSGV2
      MSGV3               = PV_MSGV3
      MSGV4               = PV_MSGV4
    IMPORTING
      MESSAGE_TEXT_OUTPUT = PV_MESG.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_1260
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*

FORM SAVE_1260 .
  DATA : LS_1260 LIKE ZCOT1260.
  DATA : LS_1261 LIKE ZCOT1261.

  <FS_DISP>-ERDAT = SY-DATUM.
  <FS_DISP>-ERZET = SY-UZEIT.
  <FS_DISP>-ERNAM = SY-UNAME.

  CLEAR  LS_1260.


  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF LS_1260
    FROM ZCOT1260
   WHERE BUKRS =  PA_BUKRS
     AND SPMON = PA_MONTH
     AND FMATNR = <FS_DISP>-FMATNR
     AND WERKS = <FS_DISP>-WERKS
     AND BELNR3 NE ''.

  IF SY-SUBRC EQ 0.

  ELSE.
    CLEAR  LS_1260.
    LS_1260-BUKRS       =     PA_BUKRS..
    LS_1260-SPMON       =     PA_MONTH.
    LS_1260-FMATNR      =     <FS_DISP>-FMATNR.
    LS_1260-WERKS       =     PA_WERKS.

    LS_1260-LIFNR       =     <FS_DISP>-LIFNR .
    LS_1260-MATKL       =     <FS_DISP>-MATKL .

*    LS_1260-BWRBTR      =     <FS_DISP>-BWRBTR.
*    LS_1260-BMENGE      =     <FS_DISP>-BMENGE.
*    LS_1260-BMEINS      =     <FS_DISP>-BMEINS.

    LS_1260-FMENGE      =     <FS_DISP>-FMENGE.
    LS_1260-FMEINS      =     <FS_DISP>-FMEINS.
    LS_1260-RMATNR      =     <FS_DISP>-RMATNR.
    LS_1260-RMENGE      =     <FS_DISP>-RMENGE.
    LS_1260-RMEINS      =     <FS_DISP>-RMEINS.
    LS_1260-RWRBTR      =     <FS_DISP>-RWRBTR.
*    LS_1260-D1WRBTR     =     <FS_DISP>-D1WRBTR.
*    LS_1260-D2WRBTR     =     <FS_DISP>-D2WRBTR.
*    LS_1260-D3WRBTR     =     <FS_DISP>-D3WRBTR.
*    LS_1260-D4WRBTR     =     <FS_DISP>-D4WRBTR.
*    LS_1260-LWRBTR      =     <FS_DISP>-LWRBTR.
*    LS_1260-MWRBTR      =     <FS_DISP>-MWRBTR.
*    LS_1260-ZCOGM       =     <FS_DISP>-ZCOGM.
    LS_1260-SMENGE      =     <FS_DISP>-SMENGE.
    LS_1260-SMEINS      =     <FS_DISP>-SMEINS.
*    LS_1260-SWRBTR      =     <FS_DISP>-SWRBTR.
    LS_1260-ZCOGS       =     <FS_DISP>-ZCOGS1. " 매출원가 차감

    LS_1260-EMENGE      =     <FS_DISP>-EMENGE.
    LS_1260-EMEINS      =     <FS_DISP>-EMEINS.
    LS_1260-EWRBTR      =     <FS_DISP>-EWRBTR.
    LS_1260-EUWRBTR     =     <FS_DISP>-EUWRBTR.
*    LS_1260-BELNR1      =     <FS_DISP>-BELNR1.
    LS_1260-BELNR3      =     <FS_DISP>-BELNR3.
    LS_1260-TWAER       =     <FS_DISP>-TWAER.
    LS_1260-ERDAT       =     <FS_DISP>-ERDAT.
    LS_1260-ERZET       =     <FS_DISP>-ERZET.
    LS_1260-ERNAM       =     <FS_DISP>-ERNAM.

    MODIFY ZCOT1260 FROM LS_1260 .

    CLEAR LS_1261.
    MOVE-CORRESPONDING LS_1260 TO LS_1261.
    LS_1261-ZCOGS1 =  <FS_DISP>-ZCOGS1. " 매출원가 차감

    MODIFY ZCOT1261 FROM LS_1261 .

    COMMIT WORK.

  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form CANCEL_DOC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM CANCEL_DOC USING PV_VAL.

* 역분개 여부 체크
  SELECT SINGLE *
  INTO @DATA(LV_BKPF)
   FROM BKPF
   WHERE BUKRS = @PA_BUKRS
     AND BELNR = @GS_INFO-BELNR
     AND GJAHR = @GS_INFO-GJAHR.

  IF LV_BKPF-AWREF_REV IS NOT  INITIAL. " 이미역분개됨
    GS_INFO-RESULT =  'S'.
    GS_INFO-RBELNR = LV_BKPF-STBLG.  " 역분개 전표번호
  ELSE.

    CLEAR :  GV_EXIT.
    CALL FUNCTION 'Z_FI_CANCEL_DOC_BDC'
      EXPORTING
        I_BUKRS   = PA_BUKRS
        I_BELNR   = GS_INFO-BELNR
        I_GJAHR   = GS_INFO-GJAHR
*       I_BUDAT   = LS_T0250-DSDAT
        I_STGRD   = '03'
      IMPORTING
        E_RESULT  = GS_INFO-RESULT
        E_RBELNR  = GS_INFO-RBELNR
        E_ERR_TXT = GS_INFO-ERR_TXT
      EXCEPTIONS
        NO_BELNR  = 1
        OTHERS    = 2.

  ENDIF.


  IF GS_INFO-RESULT EQ 'S'.
    <FS_DISP>-ICON = '@08@'." 성공
    CASE PV_VAL .
      WHEN  'YY'. "
*        <FS_DISP>-BELNR1 = SPACE." 성공

      WHEN  'RR'. "
        <FS_DISP>-BELNR3 = SPACE." 성공

    ENDCASE.

    "LOG TABLE UPDATE
    PERFORM  UPDATE_DATA_RTN USING PV_VAL.

  ELSE.
    GV_EXIT = ABAP_TRUE. "ERROR FLAG.
    <FS_DISP>-ICON = '@0A@'." 에러
    <FS_DISP>-MSG = GS_INFO-ERR_TXT." 에러
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_DATA_RTN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PV_VAL
*&---------------------------------------------------------------------*
FORM UPDATE_DATA_RTN  USING    PV_VAL.
  DATA: LS_T1260 LIKE ZCOT1260.

  CASE PV_VAL .
    WHEN 'YY'.

    WHEN 'RR'.
      UPDATE  ZCOT1260 SET BELNR3 = ''
              AEDAT  = SY-DATUM
              AEZET  = SY-UZEIT
              AENAM  = SY-UNAME
      WHERE BUKRS  = <FS_DISP>-BUKRS
        AND SPMON  = <FS_DISP>-SPMON
        AND FMATNR = <FS_DISP>-FMATNR
        AND WERKS  = <FS_DISP>-WERKS.

      IF SY-SUBRC EQ 0.

        DELETE FROM ZCOT1261
        WHERE BUKRS  = <FS_DISP>-BUKRS
        AND SPMON  = <FS_DISP>-SPMON
        AND FMATNR = <FS_DISP>-FMATNR
        AND WERKS  = <FS_DISP>-WERKS.

        IF SY-SUBRC EQ 0.

          COMMIT WORK AND WAIT.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ELSE.
        ROLLBACK WORK.

      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHK_ZCOR0450_POST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHK_ZCOR0450_POST .

  SELECT SINGLE *
    FROM ZCOT1260
    WHERE BUKRS = @PA_BUKRS
      AND SPMON = @PA_MONTH
      AND WERKS = @PA_WERKS
      AND BELNR3 NE ''
    INTO  @DATA(LS_1260).

  IF SY-SUBRC EQ 0.
    CLEAR GV_EXIT.
  ELSE.
    GV_EXIT_ZCOR0450 = ABAP_TRUE.

  ENDIF.


ENDFORM.
