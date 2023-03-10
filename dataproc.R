library(openxlsx2)
library(dataRetrieval)
library(ggpubr)
library(tidyverse)
library(magrittr)

# Rのプロジェクトフォルダにdataというフォルダを作り、そこにデータファイルを置いておく
dfolder<-'data'
dfiles<-dir(pattern='\\.xlsx$',dfolder)
ndf<-length(dfiles)
pfl<-2 # 何番目のファイルを処理するかを指定する

target_au<-as.matrix(c(6,7,12)) # ここで指定したすべてのAUが出現してるフレームを解析対象とする

# データ読み込み
dat<-data.frame(openxlsx2::read_xlsx(paste(dfolder,dfiles[pfl],sep='/')))

# 指定したAUのデータ(出現データの方）を抜き出し
lines<-apply(target_au, 1, function(x){
  colnm_c<-paste('X.AU',zeroPad(as.character(x),2),'_c',sep='')
  return(which(colnames(dat)==colnm_c))
})

# 抜き出したデータからAUが出現しているフレームを検出
t_tm<-dat[which(rowSums(dat[,lines])==length(target_au)),1]
t_ons<-t_tm[c(1,which(diff(allaus)>1)+1)]
t_end<-allaus[c(which(diff(allaus)>1), length(allaus))]
t_dur<-t_end-t_ons+1
target_tm<-data.frame(t(rbind(t_ons,t_end,t_dur)))

# 処理したいAUの強度データを指定
aurData<-c('X.AU06_r','X.AU07_r','X.AU12_r')

# 笑顔表出ごとに各AU強度の平均と最大、笑顔表出フレーム長、出現時間を抽出
sdf<-data.frame(matrix(nrow=nrow(target_tm), ncol=length(aurData)*2+2))
colnames(sdf)<-c('ts',as.vector(outer(c('mean','max'),aurData,paste,sep=',')),'dur')
for (i in 1:nrow(target_tm)){
  ts<-dat$X.timestamp[target_tm$t_ons[i]]
  vl<-matrix(ncol=2*length(aurData),nrow=1)
  for (j in 1:length(aurData)){
    colnum<-which(colnames(dat)==aurData[j])
    data<-dat[target_tm$t_ons[i]:target_tm$t_end[i],colnum]
    vl[1,(j-1)*2+1]<-mean(data)
    vl[1,(j-1)*2+2]<-max(data)
  }
  dur<-length(data)
  sdf[i,]<-c(ts,vl,dur)
}

# 抽出した強度データを表示のために整然化
lsdf<-sdf %>% tibble::rowid_to_column('feID') %>% 
  tidyr::gather(key=TempKey1, value=Amplitude, as.vector(outer(c('mean','max'),aurData,paste,sep=','))) %>%
  tidyr::separate(col=TempKey1, into=c('Method','AU'), sep=',',remove=TRUE,convert = FALSE)

# 整然データから平均と最大値を抜き出し
meandat<-lsdf[which(lsdf$Method=='mean'),]
maxdat<-lsdf[which(lsdf$Method=='max'),]

# 最大値グラフ作成
g1<-ggplot(maxdat,aes(x=feID,y=Amplitude,color=AU,size=dur))+geom_point()
g1<-g1+geom_text(aes(x=feID,y=Amplitude+0.2,color=AU,label=sprintf('%d',feID)))

# 平均値グラフ作成
g2<-ggplot(meandat,aes(x=feID,y=Amplitude,color=AU,size=dur))+geom_point()
g2<-g2+geom_text(aes(x=feID,y=Amplitude+0.2,color=AU,label=sprintf('%d',feID)))

# グラフ表示
ggpubr::ggarrange(g1, g2, nrow=2, ncol=1, align="hv", common.legend = TRUE)

# 以下、笑顔出現ごとの詳細データ抽出

# 抽出するデータ指定、各AUの強度、今は目の端、口の端のXY座標を指定している
# データ指定についてはOpenFaceのwiki参照
focusData<-list('X.AU06_r','X.AU07_r','X.AU12_r','X.X_36','X.X_45','X.X_48','X.X_53', 'X.Y_36','X.Y_45','X.Y_48','X.Y_53')
gdata<-apply(target_tm,1,function(y,tau=target_au,fD=focusData){
  fdat<-lapply(fD, function(z){
    colnum<-which(colnames(dat)==z)
    return(dat[y['t_ons']:y['t_end'],colnum])
  })
  longd<-do.call('rbind', fdat) %>% t() %>% data.frame() %>% magrittr::set_colnames(focusData) %>% tibble::rowid_to_column('frame') %>% pivot_longer(!frame, names_to='mark', values_to = 'value') 
  return(longd)
},target_au,focusData)


# 詳細な図を見たいとき
num<-38 # 何番目に表出した笑顔を見たいかを指定
# AUデータ
AUL<-str_detect(gdata[[num]]$mark, pattern='AU')

# X座標データ
XL<-str_detect(gdata[[num]]$mark, pattern='X.X')

# Y座標データ
YL<-str_detect(gdata[[num]]$mark, pattern='Y')

# グラフ作成
aug<-ggplot(gdata[[num]][AUL,],aes(x=frame, y=value, color=mark))+geom_line()
xg<-ggplot(gdata[[num]][XL,],aes(x=frame, y=value, color=mark))+geom_line()
yg<-ggplot(gdata[[num]][YL,],aes(x=frame, y=value, color=mark))+geom_line()

# グラフ表示
ggpubr::ggarrange(xg, yg, aug, nrow=2, ncol=2, align="hv", common.legend = FALSE)

