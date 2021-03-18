## 참고: https://tidyverse-korea.github.io/r-meetup-x-presser/kaggle/Meetup_3/crawling/getWebR.pdf
## 참고: https://mrkevinna.github.io/%ED%8A%B9%EA%B0%95-%EA%B8%B0%EC%97%85%EB%A6%AC%EB%B7%B0-%EB%B6%84%EC%84%9D-1/
# 필요한 라이브러리를 불러옵니다. 
library(httr) #요청
library(rvest) #요청
library(tidyverse) #정리

# 로그인 화면의 URI를 복사하여 URI 객체에 지정합니다.
URI <- 'https://www.jobplanet.co.kr/users/sign_in'

# 로그인 정보를 이용하여 HTTP 요청을 합니다. 
resp <- POST(url = URI,
             body = list('user[email]' = '계정',
                         'user[password]' = '비밀번호'))

# 응답 상태코드를 확인합니다. 200이면 정상입니다.
status_code(x = resp)

# 쿠키만 수집하여 myCookies 객체에 할당합니다. 
# 앞으로 HTTP 요청할 때 myCookies를 활용하면 로그인 상태로 HTML을 받을 수 있습니다. 
myCookies <- set_cookies(.cookies = unlist(x = cookies(x = resp)))

url_madup = 'https://www.jobplanet.co.kr/companies/198924/interviews'
resp = GET(url=url_madup, config = list(cookies = myCookies))
status_code(x=resp)

#기업명
resp %>% read_html() %>% html_node(css='body > div.body_wrap > div.cmp_hd > div.new_top_bnr > div > div.top_bnr_wrap > div > div > div.company_info_sec > div.company_info_box > div.company_name > h1 > a') %>% html_text()

#날짜
dates = resp %>% read_html() %>% html_nodes(css = 'div.content_top_ty2 > span.txt1 > span.txt2') %>% html_text()
dates = gsub('[^0-9]','',dates)
dates
# gsub 참고
## https://quantumcomputer.tistory.com/99
## 원래경로: #viewInterviewsList > div > div > section:nth-child(1) > div > div.content_top_ty2 > span.txt1 > span.txt2
## 이거지만 원래경로대로 하면 하나밖에 추출 못함

#면접질문
questions = resp %>% read_html() %>% html_nodes(css = 'div.ctbody_col2 > div > dl > dd:nth-child(2) > span') %>% html_text()
questions = gsub(' {2,}','',questions)
questions
## 원래경로: viewInterviewsList > div > div > section:nth-child(9) > div > div.ctbody_col2 > div > dl > dd:nth-child(2) > span

#마지막페이지
##한페이지당 5건 밖에 보이지 않기 때문에 
interviewCnt = resp %>% read_html() %>% html_node(css='#viewCompaniesMenu > ul > li.viewInterviews > a > span') %>% html_text()
interviewCnt = as.integer(interviewCnt)
pages=ceiling(interviewCnt / 5)
pages


#함수
##개별요소들을 데이터프레임으로 변환
myfunc = function(resp){
  
  #회사명
  company = resp %>% read_html() %>% html_node(css='body > div.body_wrap > div.cmp_hd > div.new_top_bnr > div > div.top_bnr_wrap > div > div > div.company_info_sec > div.company_info_box > div.company_name > h1 > a') %>% html_text()
  
  #면접날짜
  dates = resp %>% read_html() %>% html_nodes(css = 'div.content_top_ty2 > span.txt1 > span.txt2') %>% html_text()
  dates = gsub('[^0-9]','',dates)
  
  #면접질문
  questions = resp %>% read_html() %>% html_nodes(css = 'div.ctbody_col2 > div > dl > dd:nth-child(2) > span') %>% html_text()
  questions = gsub(' {2,}','',questions)
  questions = gsub('\n','',questions)
  
  df_page=data.frame(company,dates,questions)
  
  return(df_page)
}


# 반복문
df_interview = data.frame()
url_interview = 'https://www.jobplanet.co.kr/companies/198924/interviews'

for(page in 1:pages){
  cat('[', page, '/', pages, '] 진행중... ',fill=TRUE)
  # 첫페이지 경우 url 그대로 반환
  ## 첫페이지는 url뒤에 페이지 번호가 없다
  ifelse(page==1,
         assign('url_page',url_interview),
         assign('url_page',paste0(url_interview,'?page=',page))
  )
  
  resp = GET(url=url_page, config = list(cookies = myCookies))
  status_code(x=resp)
  myfunc(resp)
  df_interview = rbind(df_interview,df_page)
}

#csv로 저장 및 불러오기, 확인
write.csv(df_interview,'C:/Work_R/Others/interview.csv')
data=read.csv('C:/Work_R/Others/interview.csv')
View(data)
