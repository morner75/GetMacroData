### GetMacroData Toolkit

금융감독원 거시건전성 스트레스 테스트에 사용되는 데이터 수집을 위한 툴킷입니다.
툴킷의 개발 언어는 $\texttt{R}$ 이며, 주요 구성내용은 다음과 같습니다.

1. 한국은행 경제통계시스템(ECOS) OPENAPI를 이용한 거시금융 데이터의 수집
2. 통계청 국가통계포털(KOSIS) OPENAPI를 통한 국가통계 데이터의 수집
3. 금융감독원 금융통계정보시스템(FISIS)를 이용한 금융통계 데이터의 수집
4. 기타 국제 및 해외금융감독기구(IMF, BIS, World Bank, OECD, FRB)가 제공하는 통계정보의 수집

수집된 데이터는 연간/분기/월별/일별 주요 거시금융 데이터이며, Output 폴더에 xlsx 및 rds 확장자 형태로 저장되어 있고, 데이터에 대한 자세한 내용은 "description" 시트를 참조하시기 바랍니다. 

<img src="https://www.fss.or.kr/static/fss/img/sub/img_ci6.png" alt="image1" style="zoom:18%;" /> **금융감독원 시스템리스크분석팀**

