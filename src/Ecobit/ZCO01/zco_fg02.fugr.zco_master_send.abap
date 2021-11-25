FUNCTION ZCO_MASTER_SEND.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_BUKRS) TYPE  BUKRS
*"  TABLES
*"      T_CCTR STRUCTURE  ZCOS0160 OPTIONAL
*"      T_WBS STRUCTURE  ZCOS0170 OPTIONAL
*"  CHANGING
*"     VALUE(E_RESULT) TYPE  CHAR1 OPTIONAL
*"     VALUE(E_MSG) TYPE  CHAR255 OPTIONAL
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
* Tag  Date.       Author.        Description.
*----------------------------------------------------------------------*
* 20191218  수정
*           ZSDT0120 회사 플랜트 - WBS 관리 조회시 삭제 플래그 없는걸로 조회
* 20200709  코스트센터 범주,  WBS  유형 추가
*----------------------------------------------------------------------*
* V003 2021.11.23  BSGSM_SCM(KHJ) 1.코스트센터&플랜트 연동 쿼리문 수정
*----------------------------------------------------------------------*

  DATA : LT_CCTR_L TYPE ZCOS0160_L OCCURS 0,
         LS_CCTR_L LIKE LINE OF LT_CCTR_L,
         LT_WBS_L  TYPE ZCOS0170_L OCCURS 0,
         LS_WBS_L  LIKE LINE OF  LT_WBS_L.

  DATA : LS_CCTR TYPE ZCOS0160,
         LS_WBS  TYPE ZCOS0170.

  CLEAR: LT_CCTR_L, LT_CCTR_L[].
  CLEAR: LT_WBS_L, LT_WBS_L[].
  CLEAR: T_CCTR, T_CCTR[].
  CLEAR: T_WBS, T_WBS[].

  "********************************************************************
  "__ 코스트센터 정보 추출.
  "********************************************************************
  SELECT A~BUKRS, A~KOSTL, A~DATBI, A~DATAB,
         A~GSBER, B~LTEXT, A~PRCTR, A~KOSAR,
         C~WERKS
    FROM CSKS AS A
    LEFT JOIN CSKT AS B
      ON A~KOSTL = B~KOSTL
     AND A~DATBI = B~DATBI
     AND B~SPRAS = @SY-LANGU
    LEFT JOIN ZSDT0120 AS C      "__ [SD] 회사 플랜트 - WBS 관리
      ON A~KOSTL    = C~KOSTL
     AND C~MODUL    = 'M'
     AND C~LVORM = @SPACE    "mm 삭제지시자, 2019.12.18 주석 해제
     AND C~HR_LVORM = @SPACE  "CO-HR 플랜트 제외 2019.12.16 추가
     AND C~BUKRS    = @I_BUKRS
    INTO TABLE @LT_CCTR_L   "<<<<<<   수정 20200709...
   WHERE A~BUKRS = @I_BUKRS.

  CLEAR LS_CCTR_L.
  LOOP AT LT_CCTR_L  INTO LS_CCTR_L.
    CLEAR LS_CCTR.
    MOVE-CORRESPONDING LS_CCTR_L TO LS_CCTR.

    "CCTR  범주 치환...
    IF LS_CCTR_L-KOSAR IS INITIAL.

      LS_CCTR-CATE =  0.  "판관읿반

    ELSE.

      CASE LS_CCTR_L-KOSAR. "  코스트센터 범주
        WHEN 'R'.   "판관연구
          LS_CCTR-CATE =  1.
        WHEN 'M'.   " 원가제조
          LS_CCTR-CATE = 3.
        WHEN 'A' OR 'S'.    " 판관일반
          LS_CCTR-CATE = 0.
        WHEN  'P' .   " 원가도급
          LS_CCTR-CATE = 2. " 20200714  수정
        WHEN OTHERS.
          LS_CCTR-CATE =  0.  "판관읿반  by  강현수k

      ENDCASE.
    ENDIF.

    APPEND LS_CCTR TO T_CCTR.

    CLEAR LS_CCTR_L.
  ENDLOOP.

  "********************************************************************
  "__ [CO] E-HR 마스터 정보 전송 구조체(WBS) 데이터 담기.
  "********************************************************************
  SELECT DISTINCT
         A~PSPID, A~POST1,
         B~POSID, B~POST1 AS POST2,
         B~PBUKR, B~PGSBR, B~PRCTR, D~WERKS,
         C~PSTRT, C~PENDE, B~ZZWBT
    INTO TABLE @LT_WBS_L   "<<<<<<   수정 20200709...
     FROM PROJ AS A              "__ 프로젝트 정의
    INNER JOIN PRPS AS B         "__ WBS (작업분할구조) 요소 마스터 데이타
      ON A~PSPNR = B~PSPHI
    LEFT JOIN PRTE AS C          "__ 프로젝트품목에 대한 일정계획 데이타
      ON B~PSPNR = C~POSNR
    LEFT JOIN ZSDT0120 AS D      "__ [SD] 회사 플랜트 - WBS 관리
      ON B~PSPNR    = D~WBSNR
     AND D~MODUL    = 'M'
     AND D~LVORM = @SPACE    "mm 삭제지시자, 2019.12.18 주석 해제
     AND D~HR_LVORM = @SPACE  "CO-HR 플랜트 제외 2019.12.16 추가
     AND D~BUKRS    = @I_BUKRS
   WHERE A~LOEVM    = @SPACE
     AND B~LOEVM    = @SPACE
     AND B~PBUKR    = @I_BUKRS.

  CLEAR LS_WBS_L.
  LOOP AT LT_WBS_L INTO LS_WBS_L.
    CLEAR LS_WBS.
    MOVE-CORRESPONDING LS_WBS_L TO LS_WBS.

    "wbs  범주 치환...
    IF LS_WBS_L-ZZWBT IS INITIAL.

      LS_WBS-CATE =  2.  "원가도급

    ELSE.

      CASE LS_WBS_L-ZZWBT. "   wbs  유형
        WHEN 'W01' OR 'W06'. "   판관일반   영업,상품  20200714

          LS_WBS-CATE = 0.

        WHEN 'W02' OR   " 원가도급   공사
             'W03' OR                " 운영
             'W04' OR                "법인
             'W07' OR                "공통
             'W99'.                  "해당없음

          LS_WBS-CATE = 2.

        WHEN   'W05'.   "원가제조   제조

          LS_WBS-CATE = 3.

        WHEN OTHERS.

          LS_WBS-CATE = 2.  "원가도급   by 강현수k

      ENDCASE.

    ENDIF.

    CLEAR LS_WBS_L.

    APPEND LS_WBS TO T_WBS.

  ENDLOOP.

  IF T_CCTR[] IS INITIAL AND
     T_WBS[]  IS  INITIAL.
    E_MSG = 'E'.
    MESSAGE S006(ZCO01) INTO E_RESULT.
  ELSE.
    E_MSG = 'S'.
  ENDIF.

ENDFUNCTION.

* 재수정  20200714
*--- Original Message ---
*From : "강현수"<balla2000@tsk.co.kr>
*To : "정수연(BSG_FCM)/위원/수행사(ERP)"<bsg_fcm1@tsk.co.kr>
*Date : 2020/07/14 화요일 오후 4:18:24
*Subject : HR 시스템 보완 관련 인건비 범주 전사 확인 완료의 건

*전사 취합한 내역을 기반으로 변경사항 아래와 같이 발송드립니다. 참조바랍니다.

*CCTR
*
*판관일반 - A관리, S판관, (공백)                  =>0
*
*판관연구 - R연구(생성예정)                        =>1
*
*원가도급 - P프로젝트                               =>2
*
*원가제조 - M생산                                    =>3

**
**WBS
**일반 - W01(영업), W06(상품)                                   =>0
**
**판관연구 - 해당없음                                         =>1
**
**원가도급 - W02(공사), W03(운영), W04(법인), W07(공통), W99(해당없음), 공백             => 2
**
**원가제조 - W05(제조)                                                                 =>3
**

***--- Original Message ---
***From : "강현수"<balla2000@tsk.co.kr>
***To : "김현중/컨설턴트/솔루션사업본부 HR사업팀"<ssanddo@isu.co.kr>
***Cc : "정수연(BSG_FCM)/위원/수행사(ERP)"<bsg_fcm1@tsk.co.kr>
***Date : 2020/07/09 목요일 오후 3:54:10
***Subject : (수정) 기술개발비 추가관련 게정 구분 범주 참조의 건
***
***CCTR
***
***판관일반 - A관리, S판관, P프로젝트, (공백)    =>0
***판관연구 - R연구(생성예정)                    =>1
***원가도급 - 해당없음                           =>2
***원가제조 - M생산                              =>3
***
***
***WBS
***
***판관일반 - W01(영업)     =>0
***
***판관연구 - 해당없음      =>1
***
***원가도급 - W02(공사), W03(운영), W04(법인), W06(상품), W07(공통), W99(해당없음), 공백     => 2
***
***원가제조 - W05(제조)   => 3
***
