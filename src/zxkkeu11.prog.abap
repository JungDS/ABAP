*&---------------------------------------------------------------------*
*& Include          ZXKKEU11
*&---------------------------------------------------------------------*

*-- WBS 기준으로 수주유형 정의
TABLES: PRTE, PRPS.

DATA LS_COPA TYPE CE11000.

IF I_OPERATING_CONCERN = '1000'.

  E_EXIT_IS_ACTIVE = ABAP_TRUE.

  CASE I_STEP_ID.

    WHEN 'U20'.

      MOVE-CORRESPONDING I_COPA_ITEM TO LS_COPA.

      SELECT SINGLE * FROM PRPS
       WHERE PSPNR = LS_COPA-PSPNR.

      IF SY-SUBRC = 0 AND PRPS-POSID(1) = 'P'.

        SELECT SINGLE * FROM PRTE
         WHERE POSNR = LS_COPA-PSPNR.

        IF PRTE-PSTRT(4) = SY-DATUM(4).   "신규(NEW)

          LS_COPA-WW080  = 'N'.

        ELSEIF  PRTE-PSTRT(4) < SY-DATUM(4) AND   "계속(CON)
                PRTE-PENDE(4) > SY-DATUM(4).

          LS_COPA-WW080  = 'C'.

        ELSEIF PRTE-PSTRT(4) < SY-DATUM(4) AND    "재입찰(REN)
               PRTE-PENDE(4) = SY-DATUM(4).

          LS_COPA-WW080  = 'R'.

        ENDIF.

      ELSEIF SY-SUBRC = 0 AND PRPS-POSID(1) <> 'P'.
        LS_COPA-WW080  = 'X'.
      ENDIF.

      MOVE-CORRESPONDING LS_COPA TO E_COPA_ITEM.

*--------------------------------------------------------------------*
* BU특성 추출
* [CO] ESG Pjt. PA특성 U01 - BU구분 - 2021.10.06 17:06:32, MDP_06
*--------------------------------------------------------------------*
      WHEN 'U01'.

        " 특정 T-code 에 대해서는 실행하지 않는다.
*        CASE SY-TCODE.
*          WHEN 'KEU5'. " 실제 평가 실행 ( 배부 )
*          WHEN 'KEUB'. " 계획 평가 실행 ( 배부 )
*          WHEN OTHERS.

            MOVE-CORRESPONDING I_COPA_ITEM TO LS_COPA.

            IF LS_COPA-PSPNR IS NOT INITIAL.
              CALL FUNCTION 'ZCO_GET_BU_TYPE_BY_MAPPING'
                EXPORTING
                  I_PSPNR = LS_COPA-PSPNR   " WBS 요소
                  I_COPA  = LS_COPA
                IMPORTING
                  E_WW120 = LS_COPA-WW120.  " BU구분

              " BU 매핑정보에 해당하지 않는 WBS 인 경우 에러 처리
              IF LS_COPA-WW120 IS INITIAL.
                RAISE DERIVATION_FAILED.
              ENDIF.
            ENDIF.

            MOVE-CORRESPONDING LS_COPA TO E_COPA_ITEM.


*        ENDCASE.

  ENDCASE.

ENDIF.
