source('./source/source_basic.r')

df = read.csv('/home/yanglab_data/user/zhanghy/gwas/summary/test/covid/covid_survey.csv')
names(df) = gsub('在感染后|在感染后是否有出现以下.', '', names(df))
# drop unused col
drop_col = c(names(df)[4], '答题时间', '提交时间', '您是为自己填写此问卷.还是为家属或朋友填写.', '您是否愿意参与后续的随访计划.', '请留下您的微信号.方便后续我们联系到您')
df[,drop_col] = NULL
# rename
df = df%>%dplyr::rename(id='填写ID', sex='感染者的性别是.', age='感染者所处哪一年龄段.', region='感染发生所在的地区.',
    how_know='是通过怎样的方式发现自己.阳.了的.', infect_date='第一次发现感染的日期是.', n_vaccine='在此次感染前.感染者接种过几剂新冠疫苗.', 
    howlong_lastvac='最后一剂疫苗的接种距离此次感染有多久.', infect_way='猜测是通过怎样的途径感染的.', 
    fever = '是否有出现发烧.', fever_duration = '从开始发烧到完全烧退持续了多少天的时间.',
    fever_pattern = '在发烧期间.体温的变化模式是怎样的.', n_fever='在过去的五年间.感染者平均每年大概有几次发烧.', 
    oral_throat_pain = '口腔或喉部.症状..喉咙痛', oral_throat_cutting = '口腔或喉部.症状..喉咙有刀割感',  
    oral_swallow_pain = '口腔或喉部.症状..吞咽时疼痛',  oral_throat_dryitchy = '口腔或喉部.症状..喉咙干痒', 
    oral_throat_hoarseness = '口腔或喉部.症状..喉咙嘶哑',   oral_throat_phlegmy = '口腔或喉部.症状..喉咙有痰', 
    oral_upperjaw = '口腔或喉部.症状..上颚不适.疼痛.水肿.痒或异物感.',   oral_cough = '口腔或喉部.症状..咳嗽', 
    oral_hemoptysis = '口腔或喉部.症状..咳血.细微血丝为1.严重咳血为3.',
    eye_nose_tongue_congestion = '眼鼻舌.症状..鼻塞',  eye_nose_tongue_runny_nose = '眼鼻舌.症状..流鼻涕',
    eye_nose_tongue_sneeze = '眼鼻舌.症状..打喷嚏', eye_nose_tongue_tear = '眼鼻舌.症状..流泪', 
    eye_nose_tongue_eye_pain = '眼鼻舌.症状..眼痛.包括眼珠转动时疼痛.', eye_nose_tongue_vision_blurry = '眼鼻舌.症状..视力模糊',  
    eye_nose_tongue_dysosmia = '眼鼻舌.症状..嗅觉减退', eye_nose_tongue_loss_taste = '眼鼻舌.症状..味觉减退',
    eye_nose_tongue_taste_disorder = '眼鼻舌.症状..味觉倒错.比如吃甜的东西会感觉到苦.或吃什么东西都有腐烂.汽油味.',
    digestive_loss_appetite = '消化道.症状..食欲减退', digestive_increase_appetite = '消化道.症状..食欲大增',
    digestive_nausea = '消化道.症状..恶心', digestive_vomit = '消化道.症状..呕吐', digestive_diarrhea = '消化道.症状..腹泻',
    digestive_bellyache = '消化道.症状..腹痛',  digestive_abdomen_distend = '消化道.症状..腹胀', digestive_fart = '消化道.症状..肠道不停产气.多屁.',
    nerve_brainfog = '神经性.症状..脑雾',  nerve_headache = '神经性.症状..头痛',  nerve_dizziness = '神经性.症状..眩晕',
    nerve_insomnia = '神经性.症状..失眠', nerve_somnolence = '神经性.症状..嗜睡',
    organ_cardiopalmus = '器官性.症状..心悸', organ_chesepain = '器官性.症状..胸痛', organ_dyspnea = '器官性.症状..呼吸困难',
    organ_hematuria = '器官性.症状..尿血', organ_kidneypain = '器官性.症状..肾脏部位疼痛',
    organ_hyposexuality = '器官性.症状..性欲减退', organ_menstrual_disorder = '器官性.症状..生理期异常', wholebody_fatigue = '全身性.症状..疲劳.乏力',
    wholebody_muscularpain = '全身性.症状..肌肉疼痛', wholebody_jointpain = '全身性.症状..关节疼痛', wholebody_itchy = '全身性.症状..全身瘙痒',
    drug = '是否有采用以下的药物.', supp = '是否有采用以下的营养素.', disease_duration ='感染者自我估计.病程持续了几天.'
)


# fill na with 0
symptom_cols = get('or|eye|digest|nerve|whole', names(df))
df[,symptom_cols][is.na(df[,symptom_cols])] = 0        

# age and sex
df = df%>%mutate(age=gsub('岁', '', age))%>%
    mutate(age=ifelse(age%in%c('41-50', '51-60', '61-70'), '>40', age))%>%
    mutate(age=ifelse(age%in%c('12-18',  '18-24', '6-12', '3-6'), '<24', age))%>%
    mutate(age=factor(age, levels=c('<24', '24-30', '31-40', '>40')))
df = df%>%mutate(sex=factor(ifelse(sex=='女','female', 'male'), levels=c('female', 'male')))
table(df$age)
table(df$sex)

# score of sym group
groups = c('oral', 'eye_nose_tongue', 'digestive', 'nerve', 'organ', 'wholebody', 'all')
for (group in groups){
    print(group)
    key = ifelse(group=='all', 'oral|eye_nose_tongue|digestive|nerve|organ|wholebody', group)
    cols = get(key, names(df))
    score = rowSums(df[, cols])/length(cols)/3 # normalize to 0-1
    df[,paste0(group, '_score')] = score
}
 
# vac
df = df%>%mutate(n_vaccine=ifelse(n_vaccine%in%c(3, 4), '>3', n_vaccine))%>%
    mutate(n_vaccine=factor(n_vaccine, levels=c('0', '1', '2', '>3')))
df = df%>%mutate(howlong_lastvac=ifelse(howlong_lastvac=='', 'no_vac', howlong_lastvac))%>%
    mutate(howlong_lastvac=gsub('个月', ' month', howlong_lastvac))%>%
    mutate(howlong_lastvac=ifelse(howlong_lastvac%in%c('<3 month', '3-6 month'), '<6 month', howlong_lastvac))%>%
    mutate(howlong_lastvac=factor(howlong_lastvac, levels=c('no_vac', '<6 month', '6-12 month', '>12 month')))
table(df$n_vaccine)
table(df$howlong_lastvac)

# duration
df = df%>%mutate(disease_duration=ifelse(disease_duration%in%c('7~10天', '10天以上'), '>7 day', disease_duration))%>%
    mutate(disease_duration=ifelse(disease_duration%in%c('', '小于3天'), '<3 day', disease_duration))%>%
    mutate(disease_duration=gsub('天', ' day', disease_duration))%>%
    mutate(disease_duration=gsub('~|～', '-', disease_duration))%>%
    mutate(disease_duration=factor(mutate(disease_duration, levels=c('<3 day', '3-5 day', '5-7 day', '>7 day'))
table(df$disease_duration)

# # drug
# unique(df$drug)

# infect way
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
# region
df$region = gsub('维吾尔|壮族', '', df$region)
provs = citys = c()
for (i in 1:nrow(df)){
    item = df[i, 'region']
    item1 = strsplit(item, '自治区|自治州|特别行政区|省|市')[[1]][1]
    provs = c(provs, item1)
}
table(provs)
df$province = provs


### regression
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

# data.table::fwrite(res, '/home/yanglab_data/user/zhanghy/gwas/summary/test/covid/result/reg.csv', row.names=F, quote=F)


# top 3 sympton
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

tab = table(df$province)
tab = data.frame(cbind(names(tab), tab))
pop_tab = tab%>%rename(n=tab, Name_Province=V1)%>%mutate(n=as.numeric(n))%>%arrange(n)

#=====================================================================================
# hclst
#=====================================================================================
library(NbClust)
library(igraph)
library(factoextra)


path_out = '/home/yanglab_data/user/zhanghy/gwas/summary/test/covid/plot/'
d = dist(t(df[,symptom_cols]))
fit1 = hclust(d, method = "average")

p = plot(fit1,hang = -1,cex=.8,main = "title")

png(paste0(path_out, 'ttt.png'), width=3500, height=1500, res=200)
# plot(fit1,hang = -1,cex=.8,main = "title")
fviz_dend(fit1,k=4,rect =T,rect_fill = T,
    rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"))##K为聚类个数，rect_border为区域颜色填充
dev.off()



#=====================================================================================
# plot score dist
# https://stackoverflow.com/questions/58657802/forest-plot-with-subgroups-in-ggplot2
#=====================================================================================
library('ggplot2') 
path_out = '/home/yanglab_data/user/zhanghy/gwas/summary/test/covid/plot/dist/'
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
    res = rbind(res, df_p1)
}
data.frame(res) # description

# dodge <- position_dodge(width = 0.5)
# geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position = dodge, size = 0.5, width = 0.01) 



#=====================================================================================
# plot map
#=====================================================================================
# https://github.com/xmc811/mapchina
library(ggplot2)
library(mapchina)
library(data.table)
library(sf)
sf::sf_use_s2(FALSE)
path_plot = '/home/yanglab_data/user/zhanghy/gwas/summary/test/covid/plot/map/'

# data
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
    # # add to map
    # map1 = map%>%merge(temp, by='Name_Province', all.x=T)
    # png(paste0(path_plot, ifelse(grepl('|', group, fixed=T), 'all', group), '_score', '.png'), width=1500, height=1500)
    # p = ggplot(data = map1) +
    #         geom_sf(aes(fill = score)) +
    #         scale_fill_distiller(palette = "Spectral") +
    #         theme_bw() +
    #         geom_sf_label(aes(label = Name_Province))
    # print(p)
    # dev.off()
}

res # score

#=====================================================================================
# forest plot, drop
#=====================================================================================
library(forestplot)
library(dplyr)
path_out = '/home/yanglab_data/user/zhanghy/gwas/summary/test/covid/plot/forest/'

reg = res 
reg[reg=='Ref.'] = NA
reg = reg%>%mutate(beta=as.numeric(beta), se=as.numeric(se))
reg = reg%>%mutate(or=exp(beta), or_l=exp(beta-1.96*se), or_u=exp(beta+1.96*se))
groups = c('agesex', 'howlong_lastvac', 'n_vaccine', 'infect')

for (i in groups){
    sub = reg%>%filter(grepl(i, add_var))
    tabletext = rep(list(NA),nrow(sub))
    p = forestplot(tabletext,
        mean  = sub$or, lower = sub$or_l, upper =  sub$or_u,
        new_page = T, boxsize=0.4, line.margin=0.1, cex=11.5, vertices = T, xlog=T, zero=1, xlab=" ",
        lwd.zero = 2,
        xticks = c(0.25, 0.5, 1, 2, 4, 8), 
        clip=c(0.25, 8),
        col=fpColors(line="black", summary="black"),
        txt_gp = fpTxtGp(ticks = gpar(fontfamily = "", cex=1)))
    png(paste0(path_out, i, '_forest.png'))
    print(p)
    dev.off()
}






