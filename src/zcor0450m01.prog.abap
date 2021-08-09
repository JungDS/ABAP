**
***제품별 제조원가를 계산하고 회계처리하는 프로그램
***플랜트별로 집계된 제조원가를 제품별로 집계하고 회계처리하는 프로그램
***회계처리하는 계정과 직접귀속할 수 없는 원가의 배부로직은 사전에 정의한다
***
***Business Rule
***- 플랜트별로 집계 및 실행
***- 회계처리하는 계정과 직접귀속할 수 없는 원가의 배부로직은 사전에 정의한다.
**
**
***   ZMMT0600 [MM] 원자재-제품 매핑 테이블
***   ZMMT0610 [MM] 생산 투입/입고 처리
**
*  MM SPEC에서...
*      1) 제품코드 입력 시
*         - 기 매핑한 원자재코드를 검색함
*           select MATNR2  ...
*            from ZMMT0600
*           where MATNR = 입력한 제품코드
*              and LVORM <> ‘X’
*           만일, 검색한 내역이 없으면 Error
*           Error Message “매핑한 원자재가 없습니다.”
*



***
***1.화면구성 및 layout설명
***- 관리회계 1000 디폴트로 하고 비활성 처리
***- 회사코드는 1900을 DEFAULT로 하고 수정금지
***- 회계년월은  실행일 기준 회계연도 + 월-1 월로 세팅, 수정가능
***- 플랜트는 19*의 플랜트만 선택할 수 있도록 하고, 수정선택 가능
**
***MM 기타입출고 지시서 SPEC 20200304  SQL에..
*** MMT0600의 테이블에  엘리먼트가 둘다 MATNR 이네
***  SPEC에서 열어서 확인함
**
***      1) 제품코드 입력 시
***         - 기 매핑한 원자재코드를 검색함
***           select MATNR2
***            from ZMMT0600
***           where MATNR = 입력한 제품코드
****              and LVORM <> ‘X’
***
***
**
**
**** ZSD_SALES_QTY_NETWR_CALCULATE :  월별 플랜트별 품번별 매출수량&금액 계산 펑션
**
**
**
***TSK 프리텍의 제품 전월기말재고수량을 검색하는 Function
**
***TSK 프리텍의 제품에 투입된 원자재의 구매처를 검색하는 Function
**
**

**  위에서 집계한 원재료비의 금액이
**제)원재료비  1900  505101001
**시산표상(ACDOCA테이블 )의 금액과 차이가 발생하면(사용자가 개발프로그램을 통해서 입력하지 않고 표준기능으로 처리할경우 발생) 이는 <배부방법1: 아래설명> 에 의하여 배부함
**<배부방법1>	재료비 계산 버튼 클릭시 화면에 제공



**
***           원재료차이2.......매가격차이-배부 (추적이 불가한경우)
***	위의 원재료비차이1번의 금액을 찾을 때  ★  해당 제품이 없어  ★★  (즉, 전월에 생산되어서 당월에 없을 수 있음 )
***    해당 제품에 직접 귀속하지 못하는 경우는
***      (해당 원재료의 고객과 대분류(자재그룹)가 동일한 제품의 생산수량 기준으로 배부하는등 )  <정상로직으로 반영되지 않는 경우>
***- 1순위:동일한 거래처/자재그룹의 제품 생산수량기준으로 배부
***- 2순위 : 동일한 자재그룹의 제품수량기준으로 배부
***- 3순위 : 해당 플랜트 전체 생산수량 기준으로 배부	재료비계산버튼클릭시 계산하여 화면에 제공
**
***        원재료차이3 (재고조정차이로 발생한경우
***계정은 아직 미확정임)	재고조정차이계정(XXXXXXX)으로 발생된 금액에 대한 로직
***- ACDOCA 테이블에서 해당년,월에 해당 계정으로 발생한 금액을 자재코드별로 집계하여
***  해당 제품의 대분류(자재그룹)이 동일한 제품의 생산수량 비율로 배부함
***<정상로직으로 반영되지 않는 경우>
***- 1순위:동일한 거래처/자재그룹의 제품 생산수량기준으로 배부
***- 2순위 : 동일한 자재그룹의 제품수량기준으로 배부
***- 3순위 : 해당 플랜트 전체 생산수량 기준으로 배부	재료비계산버튼클릭시 계산하여 화면에 제공
**
**
**
****         노무비
***ACDOCA테이블에서 계정~@ 505201001 ~ 505432001 사이에 있는
***계정의 금액을 합산하여  ★★   생산 수량 비율로   ★★ 금액을 배부함
*** (단수차이 주의)
*** 단수차이가 발생시는 금액이 가장 큰 항목에 반영
***   노무비,경비 배부 버튼 클릭시 계산
**
**
**
***              경비
***ACDOCA테이블에서 계정@ 505301001~505432001 사이에 있는 계정의 금액을 합산하여
*** 생산 수량 비율로 금액을 배부함 (단수차이 주의)
***단수차이가 발생시는 금액이 가장 큰 항목에 반영
***노무비,경비 배부 버튼 클릭시 계산
**
**
***            제조원가
*** 원재료비 + 차이1 + 차이2 + 차이3+ 노무비 + 경비  실행,
***   재료비, 노무비경비 클릭시 실시간 합계
**
**
****
****            판매수량  SD로부터 받음    ZSD_SALES_QTY_NETWR_CALCULATE  매출정보??? 집계 버튼 클릭시
****매출정보집계 버튼 클릭시
****            매출원가  당월 매출수량 * 매출단가
****단, 당월 기말 재고금액이 없으면 계산하지 말고 (전월기말재고금액 + 당월제조원가) 로 금액 대체 	단수차이 남기기 싫음
****기말정보집계 클릭시
**
****위 펑션에서 제품별 판매수량이랑 매출액을 받음
**
**
**
***         기말재고수량
***  당월기말 재고수량  Mm의 함수 호출
*** 기말정보집계 클릭시
**
**
*** 재고단가 = 매출단가
***	 ( 당월제조원가 + 전월 기말재고 ) / (당월생산수량 + 전월기말재고수량)	기말정보집계 클릭시
**
**
***           매출원가 = 	당월 매출수량 * 매출단가
***단, 당월 기말 재고금액이 없으면 계산하지 말고
*** (전월기말재고금액 + 당월제조원가) 로 금액 대체   단수차이 남기기 싫음
***기말정보집계 클릭시
**
**
**
***당월기말재고 ( 당월제조원가 + 전월기말재고 ) – 매출원가  기말정보집계 클릭시
**
**
**
***           제조원가전표
**
***(차) 제품(101302001) xxx   (대) 제조원가 제품 대체(505501001)  xxx 전표 발행
****   단, 전표 생성시 전기일은 최초화면 입력 년월의 마지막일자
***  전표생성시 제품코드 입력
***전표유형 ‘zm’
***제조원가 제품 대체 계정의 전표생성시 WBS 코드 입력
***전표헤더텍스트 : ‘YYYYMM_제품대체’ 입력
**
**
***            매출원가전표
**
*****위의 매출원가 필드의 금액에 대하여
*****(차) 제품매출원가(502101001) xxx   (대) 제품(505501001)  xxx 전표 발행
*****단, 전표 생성시 전기일은 최초화면 입력 년월의 마지막일자
*****전표생성시 제품코드 입력
*****전표유형 ‘ZM
*****매출원가계정의 라인에 대하여 WBS코드 입력
*****전표헤더텍스트 : ‘YYYYMM_제품대체’ 입력	결산전표전기 버튼 클리시
*****
**
**
***재료비
***   재료비 원재료비     : 제품생산시 투입한 원재료금액
***                         CBO의 원재료비와 시산(표준테이블)상의 재료비 금액과 차이가 날경우  배부방법 1..
***         원재료 구매가격차이 :  당월 해당 원재료의 투입 및 제품생산이 있는 경우 : 해당 제품에 직접 귀속
***                            당월 해당 원재료의 투입/제품 생산을 찾을 수 없는 경우  배부방법 1..
***          XX 원재료비   : 재고조정 등 원재료하위계정    배부방법 1..
**
**
**
**
**
***가공비  : 배부방법 <2>...
***   노무비 급여, 성과급, 수당 퇴직급여 등  505201001~505209001
*** 경비  노무비를 제외한 모든 제조비용  505301001 ~ 505432001
**
**
***배부방법<1>...
***	IF, 동일 플랜트 내 동일 구매거래처/대분류(자재그룹)의 생산제품이 있으면
***    그 수량 비율로 배부
***ELSEIF. 동일플랜트 내 동일 대분류(자재그룹)의 생산제품이 있으면
***    그 수량 비율로 배부
***ELSEIF. 동일 플랜트 내 모든 생산제품이 있으면
***     모든 생산 수량 비율로 배부
***ELSEIF.
***     에러 메시지
***ENDIF.
*****
****
****배부방법 <2>    해당 플랜트 내 제품 판매수량 비율로 배부
**


**20200311    배부방법 1   로직 SPEC  변경

*가격차이 계정에 대한
* 배부방법1
** 	우선순위1  	전표의 자재로 직접 귀속
**
**  우선순위2 동일거래처/자재그룹 내 제품에 배부  <<   비율을 재계산
**
**  우선순위3 동일거래처내 제품에 배부   <<   비율을 재계산
**
**  없으면 에러   팝업 메시지 후 전제에 배부
*              팝업메시로  표시후 네  선택시   전체 배부   <<   기존 비율 계산한거 사용한다
*            아니오 선택시       에러 아이콘을남긴다  ---> 03/12   alert 메세지로 만 표시 재요청


*
*배부방법1- 우선순위1	직접 귀속
*배부방법1-우선순위2  해당원재료의 구매처와 자재그룹이 동일한 제품의 생산수량 비율로 배부
*배부방법1-우선순위3  해당 원재료의 구매처가 동일한 제품의 생산수량비율로 배부
*
*"배부방법1-우선순위4
*= 배부방법2"	당월생산된 전제 제품에 제품수량기준으로 배부


*****1234는 우선순위임

*
*재료비
*505101001
*ZMMT0610-LVORM <> 'X' 인 레코드 대상의 RDMBTR 금액
*재료비
*1....


*B
*"재료비 차이
*mmt0610에서 처리안한거 "
*505101001
*"ACDOCA에서 계정이 50501001로 기표된 전표들증  ACDOCA-AWREF <>ZMMT0610-RMBLNR 인 것들이 대상임
*"
*재료비차이
*1...
*2....
*3....
*
*4....
*
*
*
*
*"구매가격차이1...
*"
*504101003
*"ACDOCA에 50401003으로 기표된 전표중 if ACDOCA-MATNR = ZMMT0610-RMATNR 인 레코드가 있을 때
*(단, ZMMT0610-LVORM <> 'X' ) "
*구매가격차이1
*1....
*
*
*"구매가격차이2...
*전월 처리된 송장을 당월에 수정송장"
*504101003
*"ACDOCA에 50401003으로 기표된 전표중 ACDOCA-MATNR = ZMMT0610-RMATNR 인 레코드가 있을 때
*(단, ZMMT0610-LVORM <> 'X' ) "
*구매가격차이2
*
*2...
*3....
*
*4...

**
**재고조정금액
**505101002
**ACDOCA에 50401003으로 기표된 전표대상
**재고조정차이
**1
**2
**3
**
**4

*
*노무비
*계정범위참조
*ACDOCA에서 계정이 노무비 계정인 것
*노무비
*
*
*
*
*1.............


**
**경비
**계정범위참조
**ACDOCA에서 계정이 경비인것
**경비
**
**
**
**
**1.............

**********************************************************
*            20200313  spec 변경
**********************************************************
*          모든 ACDOCA의 비용 가져올때 해당 플랜트의   wbs 로 가져온다
*         *wbs는  플랜트와 1:1이다..

*           사업영역 : 플랜트  1 : 1
*          prps에 매핑된 wbs  사업영역으로



***************************************
**20200313  전표기표관련 변경 사항
**************************************

*제조원가의 WBS는 M~  플랜트와 1:1    << 구두로 전달
**매출원가는 ZCOR0380 로직   계정과  WBS 매핑 로직 추가 <<< SPEC





**SPEC  BELNR2  필드 쓰임새 질문

*--- Original Message ---
*From : "김호준(BSG_CO)"<bsg_co1@tsk.co.kr>
*To : "정수연(BSG_FCM)"<bsg_fcm1@tsk.co.kr>
*Date : 2020/03/16 월요일 오후 1:55:57
*Subject : Re: CO 제조원가 ,매출원가 개발 관련 문의
*
*
*문의2> (결산 후) 재고도 없고, 제조도 하지 않았고, 매출도 하지 않았으면 굳이 CBO 테이블에 저장하지 않아도 될 듯합니다.
*
*문의3> 재고역분개는 필요없을 듯합니다. 필드 자체를 없애도 되겠습니다.

* 화면 필드카탈로그에서 BELNR2 필드소거..





* 20200317.  MM 펑션 적용
*--- Original Message ---
*From : "김호준(BSG_CO)"<bsg_co1@tsk.co.kr>
*To : "정수연(BSG_FCM)"<bsg_fcm1@tsk.co.kr>
*Date : 2020/03/17 화요일 오후 3:27:30
*Subject : Re: 단위 관련 개발 이슈 문의
*
*위원님... 단위의 변경은 전혀 없다고 합니다.


*
*
*    20200318    추가 요구사항

*--- Original Message ---
*From : "김호준(BSG_CO)"<bsg_co1@tsk.co.kr>
*To : "정수연(BSG_FCM)/위원/수행사(ERP)"<bsg_fcm1@tsk.co.kr>
*Date : 2020/03/18 수요일 오전 10:36:57
*Subject : 위원님! 우선 두가지만 먼저.... 부탁~
*
*1. 최초 실행시 상태 check 할 때
*    매핑된 원재료에 대한 validation check를 하는데...
*    ★★★★★★  '제품'에 대해서만 check 하도록 부탁드립니다.
*     (상품과 서비스는 화면에서 조회는 하지만 체크대상에서는 제외 )
*
*2. 기존에 플랜트에서 대상 wbs를 찾는 로직을
*    wbs 마스터에서 가지고 오거나 했는데..
*      (이부분은 sd에서 별도의 CBO 테이블을 가지고 간다고 합니다. )
**    -> ZSDT0120 테이블에서 해당 WERKS = 화면입력 플랜트 ,
*        BUKRS = 화면입력 회사코드 ,
*        MODUL = 'M'  인 WBSNR 필드를 WBS로 인식하도록 로직 좀 부탁드립니다.
**    ( 이 때 WBSNR 는 숫자로되어있는 WBS 번호라 PRPS-PSPNR을 읽어서. POSID를 읽어와야 WBS 코드가 됩니다. )
*



**아래 메일 사유로..ACDOCA 셀렉트시 역분개 포함으로  수정  함

***--- Original Message ---
***From : "김호준(BSG_CO)"<bsg_co1@tsk.co.kr>
***To : "정수연(BSG_FCM)/위원/수행사(ERP)"<bsg_fcm1@tsk.co.kr>
***Date : 2020/03/18 수요일 오후 2:45:50
***Subject : ACDOCA 테이블에서 계정별 금액 집계시 로직 수정요청
***
***
*** 역분개가 될 때 물류의 재고 수량 변동에 따라서 취소되는 금액의 변경이 있을 수 도 있습니다.
*** (전표의 전체 금액은 변화가 없으나.. 차이 금액 계산로직으로.. )
*** (제가 로직생각할 때 놓친부분입니다. )
***
*** ACDOCA 에서 계정별 금액을 집계할 때 역분개(취소)를 제외하지 말고
***  전체를 합산하는 방법으로 로직을 수정 좀 부탁드리겠습니다.
***이런 경우가 구매가격차이만 발생할 수 있다고 생각하지만.. .
***그리도, 혹시 모르니 모든 계정의 금액을 산출할 때도 동일하게 진행을 했으면 합니다.




**
**--- Original Message ---
**From : "김호준(BSG_CO)"<bsg_co1@tsk.co.kr>
**To : "정수연(BSG_FCM)"<bsg_fcm1@tsk.co.kr>
**Date : 2020/03/18 수요일 오후 12:41:36
**Subject : Re: [문의] Fwd: 위원님! 우선 두가지만 먼저.... 부탁~
**
**
**
** 1. 플랜트에 해당 하는 wbs를 찾는 모든 로직에 적용입니다. (재수정)
**   1.1  ZSDT0120 테이블에서 해당 WERKS = 화면입력 플랜트 , BUKRS = 화면입력 회사코드 ,  MODUL = 'M'  인 WBSNR 필드를 WBS로 인식
**    - acdoca에서 금액집계할 때의 wbs를 선택하는 부분도 해당
**    - 제조원가 전표처리하는 로직에도 해당
**  1.2  ZSDT0120 테이블에서 해당 WERKS = 화면입력 플랜트 , BUKRS = 화면입력 회사코드 ,  MODUL = 'S'  인 WBSNR 필드를 WBS로 인식
**    - 매출원가 기표할 때 매출원가 계정에 대한 wbs 코드를 결정하는 로직  (==> 이부분은 제가 zcor0380 한번 더 보고 말씀드리겠습니다. )
**
** 3. 제조원가 및 매출원가 전표 기표시 자재코드도 좀 넣어
**    제품 대체 및 제품, 매출원가 계정 모두  필드 matnr 필드에 해당 제품의 자재 코드 좀
**




**20200902  김호준위원 통화후 수정
*1.  기말 재고 수량이 없는경우  남은 재고금액은 매출원가로 보정
*2. 월별 제품매출, 수출  계정의 자재별 SUM 을  FI 원장 매출액 표시 열 초가





** 20200910~11
* 마이그레이션 6월 때문에  유지보수뷰 생성
* unb3  만 조회되도록 수정
* 역분개시 선택 안해도 일괄역분개
* 수불 매출이 없이 FI 매출만 있는 경우 APPEND 되도록 수정
* 제조매출( FI)    타이틀 수정
*제품의    자재유형 필드 추가
