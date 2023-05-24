# 使い方の概要
# R, Rstudioを設定
# Rstudioで今回の実験のデータ処理用のプロジェクトを作っておくとよい。
# Rstudioでggpubr, tidyverse, magrittrというパッケージをインストール
# 元データをcsv形式に変換しておく
# 下の18行目を処理対象の参加者で抽出したいフレームに変更する。
# フレームの指定は開始フレーム, 終了フレームの並びで、抜き出したい数並べる。
# このスクリプト全体を実行（すべて選択してRunボタンなど）して、処理したいデータ(csv形式）を指定する。


library(ggpubr)
library(tidyverse)
library(magrittr)

# データ読み込み。ダイアログが開くので読み込むファイルを指定する。
fname<-file.choose()
fn<-substr(basename(fname),1,nchar(basename(fname))-5)
dat<-data.frame(read.csv(fname))

# フレーム指定
# 下の行の数値のところを抜き出したいフレーム番号にする
# 1つめのスタート, 1つめのエンド, 2つめのスタート, 2つめのエンド, ...
frames<-c(12512,12720,12850,13000,2740,2826,2972,3057,12083,12180,12315,12407)

n_target<-length(frames)/2
frames<-data.frame(matrix(frames,nrow=n_target, ncol=2,byrow=TRUE))
frames<-cbind(frames,frames[,2]-frames[,1]+1)
colnames(frames)<-c('start','end','duration')

# 処理したいAUの強度データを指定
#aurData<-c('X.AU06_r','X.AU07_r','X.AU12_r')
aurData<-c('AU06_r','AU07_r','AU12_r')

# 笑顔表出ごとに各AU強度の平均と最大、笑顔表出フレーム長、出現時間を抽出
#sdf<-data.frame(matrix(nrow=nrow(target_tm), ncol=length(aurData)*2+2))
sdf<-data.frame(matrix(nrow=nrow(frames), ncol=length(aurData)*2+2))
colnames(sdf)<-c('ts',as.vector(outer(c('mean','max'),aurData,paste,sep=',')),'dur')
for (i in 1:nrow(frames)){
  ts<-dat$timestamp[frames$start[i]]
  vl<-matrix(ncol=2*length(aurData),nrow=1)
  for (j in 1:length(aurData)){
    colnum<-which(colnames(dat)==aurData[j])
    data<-dat[frames$start[i]:frames$end[i],colnum]
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
ggsave(paste(fn,'.png',sep=''),dpi = 300, width = 9.6, height = 6.4)

# 以下、笑顔出現ごとの詳細データ抽出

# 抽出するデータ指定、各AUの強度、今は目の端、口の端のXY座標を指定している
# データ指定についてはOpenFaceのwiki参照
focusData<-list('AU06_r','AU07_r','AU12_r','X_36','X_45','X_48','X_53', 'Y_36','Y_45','Y_48','Y_53')
gdata<-apply(frames,1,function(y,tau=target_au,fD=focusData){
  fdat<-lapply(fD, function(z){
    colnum<-which(colnames(dat)==z)
    return(dat[y['start']:y['end'],colnum])
  })
  longd<-do.call('rbind', fdat) %>% t() %>% data.frame() %>% magrittr::set_colnames(focusData) %>% tibble::rowid_to_column('frame') %>% pivot_longer(!frame, names_to='mark', values_to = 'value') 
  return(longd)
},target_au,focusData)

xmax=5
auymax=5
coymax=200
coymin=-200

# 詳細な図作成

for (dd in 1:length(gdata)){
  num<-dd # 何番目に表出した笑顔を見たいかを指定
  # AUデータ
  AUL<-str_detect(gdata[[num]]$mark, pattern='AU')
  
  # X座標データ
  XL<-str_detect(gdata[[num]]$mark, pattern='X')
  
  # Y座標データ
  YL<-str_detect(gdata[[num]]$mark, pattern='Y')
  
  # グラフ作成
  aug<-ggplot(gdata[[num]][AUL,],aes(x=frame/30, y=value, color=mark))+geom_line()+xlim(0,xmax)+ylim(0,auymax)+xlab('Duration(sec.)')+ylab('Amplitude(a.u.)')
  xg<-ggplot(gdata[[num]][XL,],aes(x=frame/30, y=value, color=mark))+geom_line()+xlim(0,xmax)+xlab('Duration(sec.)')+ylab('Coordinate')+ylim(coymin,coymax)
  yg<-ggplot(gdata[[num]][YL,],aes(x=frame/30, y=value, color=mark))+geom_line()+xlim(0,xmax)+xlab('Duration(sec.)')+ylab('Coordinate')+ylim(coymin,coymax)
  
  # グラフ表示
  ggpubr::ggarrange(xg, yg, aug, nrow=2, ncol=2, align="hv", common.legend = FALSE)
  ggsave(paste(fn,'feID',as.character(num),'.png',sep=''),dpi = 300, width = 9.6, height = 6.4)
}
