*----------------------------------------------------------------------*
***INCLUDE ZXCN1O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module PBO OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO OUTPUT.

*--------------------------------------------------------------------*
* [CO] ESG Pjt. 설비WBS 고객필드 UserExit - 2021.09.29 17:09:56, MDP_06
* - 프로젝트의 Profile 에 따라 기존 고객필드를 보여주거나,
*   설비WBS 전용필드를 보여줄지 로직을 분기
*--------------------------------------------------------------------*
  CASE GV_PROFL.
    WHEN 'Z000003'. " 설비 투자및관리비용

      LOOP AT SCREEN.
        IF SCREEN-GROUP2 EQ 'C2'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ELSEIF GV_DISPLAY = ABAP_TRUE.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ELSE.
          " 필수입력 필드
          IF SY-BATCH IS INITIAL.
            CASE SCREEN-NAME.
              WHEN 'ZCOS0021-ZZIZW'     " 투자사유
                OR 'ZCOS0021-ZZCD1'     " 설비분류 (대)
                OR 'ZCOS0020-ZZWBT'     " WBS 유형
                OR 'ZCOS0020-ZZCYP'.    " 통제유형

                SCREEN-REQUIRED = 1.
                MODIFY SCREEN.
              WHEN 'ZCOS0021-ZZTCV'.
                IF ZCOS0021-ZZWAE IS INITIAL.
                  SCREEN-INPUT = 0.
                  MODIFY SCREEN.
                ENDIF.
            ENDCASE.
          ENDIF.
        ENDIF.

      ENDLOOP.
    WHEN OTHERS.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 EQ 'C1' AND GV_DISPLAY = ABAP_TRUE.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDIF.

        IF SCREEN-GROUP2 EQ 'C3'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.

      ENDLOOP.
  ENDCASE.

ENDMODULE.
