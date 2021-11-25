*&--------------------------------------------------------------------&*
*& PROGRAM ID  :  ZCOR0450                                            &*
*& Title : [CO] 제조원가를 제품별로 집계하고 회계처리하는 프로그램    &*
*& Created By  :  BSGSM_FCM                                           &*
*& Created On  : 2020.03.03                                           &*
*& Description :                                                      &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2020.03.03 BSGSM_FCM    INITIAL RELEASE
*----------------------------------------------------------------------*
REPORT zcor0450 MESSAGE-ID zco01.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE zcor0450t01.
INCLUDE zcor0450t02. "재료비차이 선언용
INCLUDE zcor0450m01.  " SPEC 상세
INCLUDE zcor0450alv.
INCLUDE zcor0450salv.
INCLUDE zcor0450scr.
INCLUDE zcor0450o01.
INCLUDE zcor0450i01.
INCLUDE zcor0450f01.
INCLUDE zcor0450f02. " salv

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*

INITIALIZATION.
  PERFORM set_initvalue.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_month.
*---------------------------------------------------------------------*
  PERFORM f4_p_month.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON BLOCK bl1.
*---------------------------------------------------------------------*
  PERFORM check_bl1.

AT SELECTION-SCREEN.
*---------------------------------------------------------------------*
  PERFORM scr_user_command.


*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF  pa_month < '202007'.
*  IF  PA_MONTH < '202007' AND ( SY-SYSID = 'TCQ' OR  SY-SYSID = 'TCP' ).

**20년 7월 이전은 기표 불가

    gv_exit = 'X'.
    MESSAGE s000 WITH  TEXT-e77 DISPLAY LIKE gc_e.

  ELSE.

    CLEAR gv_exit.
    PERFORM check_auth_cond.

    PERFORM check_input_valid.
    PERFORM get_master.

  ENDIF.

  CHECK gv_exit IS INITIAL.


  CASE 'X'.
    WHEN p_ra1. " 회계처리

      DATA lv_flag.

      PERFORM chk_posted USING  lv_flag.

      gv_exit =   lv_flag .

      IF lv_flag IS INITIAL.       " 기표없으면 자료 생성으로


**  기표 없으면  기존 저장된 RAW 삭제  20210112....

        DELETE FROM zcot1260 WHERE  bukrs = pa_bukrs
                              AND  spmon = pa_month
                              AND  werks = pa_werks.

        IF sy-subrc EQ 0.
          COMMIT WORK AND WAIT.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
**  END BY 20210112..


*         MMT0610에서 가져오기..
        PERFORM selected_data_rtn.
*         당월 생산이 없지만  이월 재고가 있으면  매출이 있을꺼구..
*         SD 펑션에서 가져오기
        PERFORM selected_data_rtn2.


        CLEAR gv_tree_gl.
        PERFORM selected_data_rtn_acdoca USING gv_tree_gl.

        "  기초이월재고 가져오기
        PERFORM get_before_stock.

**         20200317 mm    ZMM_PERIOD_END_STOCK_QUANTITY  펑션으로 대체
        PERFORM get_lastday_stock_mm.  "

**        COLLECT 후  명칭 넣기
        PERFORM  last_collect.


        " spec if문   비율  미리 계산
        PERFORM compute_rate3.  " 구매처별 자재그룹별 제품별
        PERFORM compute_rate4.  " 구매처별 제품별
        PERFORM compute_rate5.  "  제품별  비율 .. 노무, 경비배부 활용

        IF gv_tree_gl IS NOT INITIAL.

          MESSAGE i000(zfi01) WITH TEXT-i99.

        ENDIF.
        " 버튼 비활성화  항상 UNB3 만 조회   20200911
        " 관리팀에서는 사용안함 이은지대리부서만 사용..!!
        IF p_chk  IS NOT INITIAL.
          DELETE gt_display WHERE mtart  NE 'UNB3'.
        ENDIF.


      ELSE.

        MESSAGE s000 WITH  TEXT-s10 DISPLAY LIKE  gc_e.
      ENDIF.


    WHEN p_ra2. " 결과조회

      PERFORM selected_data_rtn_1260.
  ENDCASE.


*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.

  IF gv_exit IS INITIAL.
    DESCRIBE TABLE gt_display LINES gv_lines.

    IF gv_lines IS INITIAL.
      MESSAGE s004 WITH gv_lines.
    ELSE.
      MESSAGE s039 WITH gv_lines.
    ENDIF.
    " 100번 screen을 호출

    SORT gt_display BY bukrs werks_name name1 fmatnr_maktx rmatnr_maktx.
    CALL SCREEN 0100.

  ENDIF.
