
# About covid_survey
In 2022 Dec, we conducted a online survey to investigate the characteristics, symptoms, and response of Chinese COVID patients after the ending restriction.  

The question of our questionaire can be seen at [here](https://f.wps.cn/ksform/w/write/E1GHTEmC#routePromt). The filled questionaire can be seen in the `data/covid_survey.csv`. Initial risk report can be seen at [here](
https://mp.weixin.qq.com/s?__biz=MzI3MTMwMTgyOQ==&mid=2247483999&idx=1&sn=77ec35ced8c1ee71e4074dd011cf92e0&chksm=eac2aff5ddb526e3214ee7c67054003c8a90798550c1be791787a4e6425cc5c330bc63ccd5df&mpshare=1&scene=1&srcid=1229rBurrSSnF8e7SpwjuY7g&sharer_sharetime=1672327541176&sharer_shareid=a71cc24f8a748dc08f84fd7448a9b921&key=eb16c4aa1548428cc2864686ff55985b6bdacc09b5e9af4bdc3fa28ea05692eac471c3de2fa1f038d20f3cd04c4bead15782692f6b852b37098b6bbf50cd5472e6508afeb6894e748e67aad45719f5e74738f91d7dfaec79331cebf8bd9c357330ca3bd24d35879f97e48869d0beab0c765d2b844997c20e7805a62a363ac3da&ascene=1&uin=ODIxMDA3NjMw&devicetype=Windows+10+x64&version=6308011a&lang=zh_CN&exportkey=n_ChQIAhIQj4SybjXHsHdWixuO5hHN1RL0AQIE97dBBAEAAAAAAN5bAxvn4nQAAAAOpnltbLcz9gKNyK89dVj05wKGNYlFwq81hG9YdA6ucXSVna%2FoMqV2F9HXeJK9XpYMpea%2Btj4LPLYqrbE2Bh7OoFygNDZfaf%2FJKYbgMNnbrqMMn5ZbQ2ejoLj2IXhx%2BUEgZy9Oa6QMB6SrPTrK9vAeojyQndZ%2FVirDUDz2TDhU1ycPLUh2bE5Dk5NkWU8%2BeE05%2FEyER8NI4M1DBodLJxJxghvNx04oZuCnLn4agTWzJGI3o%2BLGHF2iRFvo0rAS%2FzCKfJwl7mwakH6l4gt3nzENXTfmZOq%2BrZZEhRog%2BYU%3D&acctmode=0&pass_ticket=dJ%2B6e5CP3ph7Q5iB1StMGqZb3%2FQ%2FSl1AGdQOaeLLx2ZpvHLd%2BKZm63IAkHLklHs41Tf%2Bi7USFkLNVD1F9eFZ3g%3D%3D&wx_header=1&fontgear=2).

Below is our analysis code.

# Requirements
- `R`

# Getting Started
Clone this repository via the commands:
```  
git clone https://github.com/zhanghaoyang0/covid_survey.git
cd covid_survey
```

Load packages and function
```
library(openxlsx) 
library(stringr)
library(dplyr)
library(stringi) 
library(R.utils)
library(ggplot2) 
library(mapchina)
library(data.table)
library(sf)
options(stringsAsFactors=F)
sf::sf_use_s2(FALSE)

get = function(key, vector, exact=F, v=F){
    if (exact==T){out =vector[grep(key, vector, invert=v)]}
    else {out = vector[grep(toupper(key), toupper(vector), invert=v)]}
    return(out)
}
```

load data and clean
```
df = read.xlsx('data/covid_survey.xlsx', sheet=1) 
# drop unused col
drop_col = c(names(df)[4], '答题时间', '提交时间', '您是为自己填写此问卷.还是为家属或朋友填写.')
df[,drop_col] = NULL
# rename
names(df) = gsub('在感染后|在感染后是否有出现以下.', '', names(df))
df = df%>%dplyr::rename(id='填写ID', sex='感染者的性别是？', age='感染者所处哪一年龄段？', region='感染发生所在的地区？',
    how_know='是通过怎样的方式发现自己“阳”了的？', infect_date='第一次发现感染的日期是？', n_vaccine='在此次感染前，感染者接种过几剂新冠疫苗？', howlong_lastvac='最后一剂疫苗的接种距离此次感染有多久？', infect_way='猜测是通过怎样的途径感染的？', 
    fever = '是否有出现发烧？', fever_duration = '从开始发烧到完全烧退持续了多少天的时间？',
    fever_pattern = '在发烧期间，体温的变化模式是怎样的？', n_fever='在过去的五年间，感染者平均每年大概有几次发烧？', 
    oral_throat_pain = '口腔或喉部】症状？:喉咙痛', oral_throat_cutting = '口腔或喉部】症状？:喉咙有刀割感',  
    oral_swallow_pain = '口腔或喉部】症状？:吞咽时疼痛',  oral_throat_dryitchy = '口腔或喉部】症状？:喉咙干痒', 
    oral_throat_hoarseness = '口腔或喉部】症状？:喉咙嘶哑',   oral_throat_phlegmy = '口腔或喉部】症状？:喉咙有痰', 
    oral_upperjaw = '口腔或喉部】症状？:上颚不适（疼痛、水肿、痒或异物感）',   oral_cough = '口腔或喉部】症状？:咳嗽', 
    oral_hemoptysis = '口腔或喉部】症状？:咳血（细微血丝为1，严重咳血为3）',
    eye_nose_tongue_congestion = '眼鼻舌】症状？:鼻塞',  eye_nose_tongue_runny_nose = '眼鼻舌】症状？:流鼻涕',
    eye_nose_tongue_sneeze = '眼鼻舌】症状？:打喷嚏', eye_nose_tongue_tear = '眼鼻舌】症状？:流泪', 
    eye_nose_tongue_eye_pain = '眼鼻舌】症状？:眼痛（包括眼珠转动时疼痛）', eye_nose_tongue_vision_blurry = '眼鼻舌】症状？:视力模糊',  
    eye_nose_tongue_dysosmia = '眼鼻舌】症状？:嗅觉减退', eye_nose_tongue_loss_taste = '眼鼻舌】症状？:味觉减退',
    eye_nose_tongue_taste_disorder = '眼鼻舌】症状？:味觉倒错（比如吃甜的东西会感觉到苦，或吃什么东西都有腐烂/汽油味）',
    digestive_loss_appetite = '消化道】症状？:食欲减退', digestive_increase_appetite = '消化道】症状？:食欲大增',
    digestive_nausea = '消化道】症状？:恶心', digestive_vomit = '消化道】症状？:呕吐', digestive_diarrhea = '消化道】症状？:腹泻',
    digestive_bellyache = '消化道】症状？:腹痛',  digestive_abdomen_distend = '消化道】症状？:腹胀', digestive_fart = '消化道】症状？:肠道不停产气（多屁）',
    nerve_brainfog = '神经性】症状？:脑雾',  nerve_headache = '神经性】症状？:头痛',  nerve_dizziness = '神经性】症状？:眩晕',
    nerve_insomnia = '神经性】症状？:失眠', nerve_somnolence = '神经性】症状？:嗜睡',
    organ_cardiopalmus = '器官性】症状？:心悸', organ_chesepain = '器官性】症状？:胸痛', organ_dyspnea = '器官性】症状？:呼吸困难',
    organ_hematuria = '器官性】症状？:尿血', organ_kidneypain = '器官性】症状？:肾脏部位疼痛',
    organ_hyposexuality = '器官性】症状？:性欲减退', organ_menstrual_disorder = '器官性】症状？:生理期异常', wholebody_fatigue = '全身性】症状？:疲劳/乏力',
    wholebody_muscularpain = '全身性】症状？:肌肉疼痛', wholebody_jointpain = '全身性】症状？:关节疼痛', wholebody_itchy = '全身性】症状？:全身瘙痒',
    drug = '是否有采用以下的药物？', supp = '是否有采用以下的营养素？', disease_duration ='感染者自我估计，病程持续了几天？'
)
# fill na with 0
symptom_cols = get('or|eye|digest|nerve|whole', names(df))
is.na(df[,symptom_cols]) # number of NA
df[,symptom_cols][is.na(df[,symptom_cols])] = 0        
```

Age and sex
```
df = df%>%mutate(age=gsub('岁', '', age))%>%
    mutate(age=ifelse(age%in%c('41-50', '51-60', '61-70'), '>40', age))%>%
    mutate(age=ifelse(age%in%c('12-18',  '18-24', '6-12', '3-6'), '<24', age))%>%
    mutate(age=factor(age, levels=c('<24', '24-30', '31-40', '>40')))
df = df%>%mutate(sex=factor(ifelse(sex=='女','female', 'male'), levels=c('female', 'male')))
table(df$age)
table(df$sex)
```

Score of sympton group
```
groups = c('oral', 'eye_nose_tongue', 'digestive', 'nerve', 'organ', 'wholebody', 'all')
for (group in groups){
    print(group)
    key = ifelse(group=='all', 'oral|eye_nose_tongue|digestive|nerve|organ|wholebody', group)
    cols = get(key, names(df))
    score = rowSums(df[, cols])/length(cols)/3 # normalize to 0-1
    df[,paste0(group, '_score')] = score
}
```

Vaccination
```
df = df%>%mutate(n_vaccine=ifelse(n_vaccine%in%c(3, 4), '>3', n_vaccine))%>%
    mutate(n_vaccine=factor(n_vaccine, levels=c('0', '1', '2', '>3')))
df = df%>%mutate(howlong_lastvac=ifelse(howlong_lastvac=='', 'no_vac', howlong_lastvac))%>%
    mutate(howlong_lastvac=gsub('个月', ' month', howlong_lastvac))%>%
    mutate(howlong_lastvac=ifelse(howlong_lastvac%in%c('<3 month', '3-6 month'), '<6 month', howlong_lastvac))%>%
    mutate(howlong_lastvac=factor(howlong_lastvac, levels=c('no_vac', '<6 month', '6-12 month', '>12 month')))
table(df$n_vaccine)
table(df$howlong_lastvac)
```

Covid duration
```
df = df%>%mutate(disease_duration=ifelse(disease_duration%in%c('7~10天', '10天以上'), '>7 day', disease_duration))%>%
    mutate(disease_duration=ifelse(disease_duration%in%c('', '小于3天'), '<3 day', disease_duration))%>%
    mutate(disease_duration=gsub('天', ' day', disease_duration))%>%
    mutate(disease_duration=gsub('~|～', '-', disease_duration))%>%
    mutate(disease_duration=factor(mutate(disease_duration, levels=c('<3 day', '3-5 day', '5-7 day', '>7 day'))
table(df$disease_duration)
```

Infected way
```
df = df%>%mutate(
    infect_entertainment=factor(as.numeric(grepl('消费', infect_way))), 
    infect_work=factor(as.numeric(grepl('工作', infect_way))), 
    infect_family=factor(as.numeric(grepl('家人', infect_way))), 
    infect_traffic=factor(as.numeric(grepl('交通', infect_way))), 
    infect_hosp=factor(as.numeric(grepl('医疗', infect_way))))

for (i in c('infect_entertainment', 'infect_work', 'infect_family', 'infect_traffic', 'infect_hosp')){
    print(i)
    print(table(df[,i]))
}
```

Region
```
df$region = gsub('维吾尔|壮族', '', df$region)
provs = citys = c()
for (i in 1:nrow(df)){
    item = df[i, 'region']
    item1 = strsplit(item, '自治区|自治州|特别行政区|省|市')[[1]][1]
    provs = c(provs, item1)
}
table(provs)
df$province = provs

tab = table(df$province)
tab = data.frame(cbind(names(tab), tab))
pop_tab = tab%>%rename(n=tab, Name_Province=V1)%>%mutate(n=as.numeric(n))%>%arrange(n)
pop_tab
```


Top 3 symptons in different region
```
res = data.frame()
provs =  names(rev(sort(table(df$province)))) # sort by n
for (prov in provs){
    sub = df%>%filter(province==prov)
    ns = c()
    for (i in symptom_cols){
        sub$y = ifelse(sub[,i]==0, 0, 1)
        ns = c(ns, i, sum(sub$y==1))
    }
    ns = data.frame(matrix(ns, ncol=2, byrow=T))%>%rename(symption=X1, n=X2)
    ns = ns%>%mutate(n=as.numeric(n))%>%mutate(prop=n/nrow(sub))%>%arrange(desc(prop))
    add = head(ns, 3)%>%mutate(region=prov)%>%relocate(region)
    res = rbind(res, add)
}
res
```

Association between symptons and age, sex, vaccination, infected way
```
basic_vars = c('age', 'sex')
add_vars = c('agesex', 'howlong_lastvac', 'n_vaccine', 'infect_work', 'infect_family', 'infect_traffic', 'infect_hosp')

out = c()
for (add_var in add_vars){
    if (add_var!='agesex'){
        formula = formula(paste0('y~', paste0(c(basic_vars, add_var), collapse='+')))
    }else{formula = formula(paste0('y~', paste0(c(basic_vars), collapse='+')))}
    for (i in symptom_cols){
        print(i)
        df$y = ifelse(df[,i]==0, 0, 1)
        r = glm(formula, df, family=binomial)
        coef = data.frame(summary(r)$coefficients)%>%tibble::rownames_to_column('VAR')
        coef = coef[2:nrow(coef), c(1, 2, 3, 5)]
        names(coef) = c('VAR', 'beta', 'se', 'p')
        if (add_var!='agesex'){coef = coef%>%filter(!grepl('age|sex', VAR))}
        if (!any(coef$p<0.05)){next}
        if (add_var=='agesex'){vars=basic_vars}else{vars=add_var}
        for (var in vars){
            for (group in levels(df[,var])){
                temp = df%>%filter(df[,var]==group)%>%pull(y)
                coef1 = coef[coef$VAR==paste0(var, group), c('beta', 'se', 'p')]
                if (nrow(coef1)== 0){coef1=c('NA', 'NA', 'NA')}
                if (group==levels(df[,var])[1]) {coef1=c('Ref.', 'NA', 'NA')}
                p1 = paste0(sum(temp==1), ' (', round(sum(temp==1)/length(temp)*100,2), '%)')
                p2 = paste0(sum(temp==0), ' (', round(sum(temp==0)/length(temp)*100,2), '%)')
                out = c(out, add_var, i, var, group, p1, p2, unlist(coef1))
            }
        }
    }
}

res = data.frame(matrix(out, ncol=9, byrow=T))
names(res) = c('add_var','sympton', 'var', 'group', 'ncase', 'nctrl', 'beta', 'se', 'p')

write.csv(res, './result/reg.csv', row.names=F, quote=F)
```


Plot score distribution
```
path_out = './plot/dist/'
groups = c('oral', 'eye_nose_tongue', 'digestive', 'nerve', 'organ', 'wholebody', 'all')
df_p = data.frame()
for (group in groups){
    sub = df[,c('age', 'sex', paste0(group, '_score'))]
    names(sub)[3] = 'score'
    sub$group = group
    df_p = rbind(df_p, sub)
}

res = data.frame()
for (i in c('age', 'sex')){
    df_p$group1 = df_p[,i]
    df_p1 = df_p%>%group_by(group, group1)%>%dplyr::summarise(mean=mean(score), sd=sd(score))
    palette = ifelse(i == 'age', 'Spectral', 'Set1')
    dodge <- position_dodge(width = 0.5)
    p = ggplot(df_p1, aes(x=group, y=mean, fill=group1)) +  
        geom_bar(width = 0.5, stat="identity", position = dodge) +  
        scale_fill_brewer(palette = palette) +
        ylim(0,1) + ylab('Average score') + xlab('Symptom group') + 
        labs(fill = capitalize(i)) +
        theme_bw() + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color="black"), 
        axis.text.y = element_text(color="black"))
    png(paste0(path_out, i, '.png'), width=1000, height=800, res=200)
    print(p)
    dev.off()
    res = rbind(res, data.frame(df_p1))
}
res 
```
![image](https://github.com/zhanghaoyang0/covid_survey/tree/master/plot/dist/age.png)
![image](https://github.com/zhanghaoyang0/covid_survey/tree/master/plot/dist/sex.png)


Plot region distribution
```
path_plot = './plot/map/'
data(china)
map = china
temp = china$Name_Province
temp = gsub('省|市|回族|壮族|维吾尔|特别行政区|自治区', '', temp)
map$Name_Province = temp
map = map%>%group_by(Name_Province)%>%dplyr::summarise(geometry=st_union(geometry))

# mean score
groups = c('oral', 'eye_nose_tongue', 'digestive', 'nerve', 'organ', 'wholebody', 'all')

res = data.frame()
for (group in groups){
    print(group)
    temp = df[, c('province', paste0(group, '_score'))]
    names(temp)[2] = 'score'
    temp = temp%>%dplyr::rename(Name_Province=province)%>%group_by(Name_Province)%>%dplyr::summarise(score=mean(score))
    sub = data.frame(temp)
    sub$group = group
    sub = sub%>%merge(pop_tab, 'Name_Province')%>%arrange(desc(score))
    res = rbind(res, head(sub))
    # add to map
    map1 = map%>%merge(temp, by='Name_Province', all.x=T)
    png(paste0(path_plot, ifelse(grepl('|', group, fixed=T), 'all', group), '_score', '.png'), width=1500, height=1500)
    p = ggplot(data = map1) +
            geom_sf(aes(fill = score)) +
            scale_fill_distiller(palette = "Spectral") +
            theme_bw() +
            geom_sf_label(aes(label = Name_Province))
    print(p)
    dev.off()
}

res # score
```
![image](https://github.com/zhanghaoyang0/covid_survey/tree/master/plot/map/all_score.png)


# Acknowledgement
Study design and data collection: [木森](https://www.zhihu.com/people/mooseOS) and [易楚](https://www.zhihu.com/people/kwindva).

# Feedback and comments
Add an issue or contact me via zhanghaoyang0@hotmail.com
