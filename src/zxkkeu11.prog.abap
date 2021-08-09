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

  ENDCASE.

ENDIF.
