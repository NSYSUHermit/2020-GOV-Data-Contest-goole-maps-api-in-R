"# 2020-GOV-Data-Contest-goole-maps-api-in-R" 
因為google maps api主要使用於python上，因此為了在R上執行，我使用了套件reticulate讓api能夠在R上執行。  

主要內部的function有五個：    
Function for count place，找尋經緯度在半徑內的店家資訊。  
Function for find town place，用經緯度找是在哪個鄉鎮市區。  
Function for find village place，用經緯度找是在哪個村里。  
Function for calculate the town ltds，計算一個鄉鎮市區內部所有間隔的經緯度座標。  
Function for calculate the town informations，計算鄉鎮市區的資料。  

前四個function主要是為了第五個function而建立的，主要是輸出一個地區所有經緯度座標的資訊。  
例如在(x1,y1)這個地點上，半徑300公尺內有15間診所、500公尺有3間大型醫院、5公里內有3間醫學中心等等，  
而政府單位便可利用這些資訊，來尋找適合建立老幼共托的地點。  

