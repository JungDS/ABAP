*&---------------------------------------------------------------------*
*& Include          ZCOR0450O01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
* - Prefix 정의
*   1. PF_xxxx  : STATUS Prefix : PF_   //   xxxx: Screen No
*   2. TT_xxxx  : TITLEBAR Prefix : TT_   //   xxxx: Screen No
  DATA : lv_flagc.

  CLEAR: gv_title, gt_exclude[].


  CASE gc_x."
    WHEN p_ra1.  " 회계처리시 취소 버튼 숨김
      CLEAR lv_flagc.
      LOOP AT gt_display INTO gs_display WHERE belnr1 IS NOT INITIAL OR belnr3 IS NOT INITIAL.
        lv_flagc =  'X'.
        EXIT.
      ENDLOOP.

      IF lv_flagc IS INITIAL.

        APPEND : '&CANC' TO gt_exclude.
        APPEND '&DELE' TO gt_exclude.
        APPEND '&QUAN' TO gt_exclude.  "
      ELSE.
        APPEND : '&CANC' TO gt_exclude.    "  필요유무 확인할것..!!
        APPEND '&DELE' TO gt_exclude.

        APPEND '&RCOM' TO gt_exclude.
        APPEND '&LCOM' TO gt_exclude.
        APPEND '&ZCOGS' TO gt_exclude.
        APPEND '&POST' TO gt_exclude.
        APPEND '&QUAN' TO gt_exclude.

      ENDIF.

      gv_title =  gv_we_nm  && TEXT-tx1 && ' ' && pa_month+0(4) && '년'  &&  pa_month+4(2) &&'월 )'.

    WHEN p_ra2.  "조회시 회계처리 버튼 숨김
      RANGES : lr_uname  FOR usr21-bname.

      CLEAR lr_uname.  REFRESH  lr_uname.
      lr_uname-sign = 'I' . lr_uname-option = 'EQ'.
      lr_uname-low = 'BSGSM_FCM'. APPEND lr_uname.
      lr_uname-low = 'TSKCO1'. APPEND lr_uname.


      APPEND '&RCOM' TO gt_exclude.
      APPEND '&LCOM' TO gt_exclude.
      APPEND '&ZCOGS' TO gt_exclude.
      APPEND '&POST' TO gt_exclude.

      IF sy-uname NOT IN  lr_uname.
        APPEND '&QUAN' TO gt_exclude.  " 수량 대사 운영자 조회
      ENDIF.



***   -- Original Message ---
***From : "김호준(BSG_CO)"<bsg_co1@tsk.co.kr>
***To : "정수연(BSG_FCM)/위원/수행사(ERP)"<bsg_fcm1@tsk.co.kr>
***Date : 2020/03/26 목요일 오전 11:08:54
***Subject : 위원님 ZCOR0450 '자료삭제' 버튼 삭제요청 드립니다.
***
***위원님!
***김호준입니다.
***
***제조원가계산 및 전기 프로그램에서  '자료삭제' 버튼이 저장되어 있는 data를 테이블에서 삭제하는 기능이라면 기능삭제 좀 부탁드리겠습니다. (로직은 없애지 마시고, 버튼만 없앴으면 좋겠습니다. )
***
***=> 강과장님이 테스트 및 매뉴얼 작성 중 요청사항입니다.




      APPEND '&DELE' TO gt_exclude.  "

      gv_title =  gv_we_nm  && TEXT-tx2 && pa_month+0(4) && '년'  &&  pa_month+4(2) &&'월 )'.
  ENDCASE.



  SET PF-STATUS 'PF_0100' EXCLUDING gt_exclude  .
  SET TITLEBAR  'TT_0100' WITH gv_title.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_INIT_DISPLAY_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_init_display_0100 OUTPUT.

  "-- 화면의  GRID가 BOUND되었는지 확인한다.
  IF gr_grid1 IS NOT BOUND.

    "-- GRID의 INSTANCE를 생성한다.
    PERFORM create_instance_0100.

    "-- GRID의 LAYOUT 속성을 정의한다.
    PERFORM init_layout_0100.

    "-- ALV Standard toolbar button cotrol
    PERFORM set_grid_exclude_0100.

    "-- ALV Sort
    PERFORM alv_sort_0100.

    "-- Field Attribute을 사용자의 요구사항에 맞게 변경
    PERFORM append_fieldcat_0100.

    "-- ALV Events 등록
    PERFORM regist_alv_event_0100 USING gr_grid1.

    "-- ALV Display
    PERFORM display_alv_grid_0100.



*    "-- ALV Title
*    PERFORM DISPLAY_ALV_TITLE_0100.

  ELSE.

    "-- ALV Refresh
    PERFORM refresh_grid_0100.

  ENDIF.

ENDMODULE.                 " ALV_INIT_DISPLAY_0100  OUTPUT
