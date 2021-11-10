;Shield_NM_CT - calculate shielding for radioactive sources and CT
;Copyright (C) 2021  Ellen Wasbo, Stavanger University Hospital, Norway
;ellen@wasbo.no
;
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License version 2
;as published by the Free Software Foundation.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Pugblic License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

pro drawfirst, drawid
  redrawrooms
end

pro Shield_NM_CT,  GROUP_LEADER=bMain

  COMMON SHNM, drawRooms, thisPath, btnColor, im, defaultImgPath, occMap, doseMap, doseRateMax, doseMapCT,maxIm,zoomFactor,zoomIn,szim, lastXY, lastFieldMarked, mouseDownRight, lastXYright,currTab, $
    lblStartPos,lblEndPos,startMap,endMap,txtCalib, calibFactor,txtMeasLen,startMeasLen,endMeasLen,lblStartMeasLen,lblEndMeasLen,$ 
    selShow,selOverlay,selDose,txtPos,txtPosM,txtOcc,txtDose,txtDoseNM,txtDoseRateNM,txtDoseCT,slideImg,alphaImg,slideOcc,alphaOcc,slideDose,alphaDose,$
    selFloor,txtH1,txtH0,txtC2,txtC1,txtC0,txtConCeil,txtLeadCeil,txtConFloor,txtLeadFloor,btnCorrThick,$
    sWalls, sAreas,sSources,sSourcesCT,subsWallsDef,subsAreasDef,subsSourcesDef,currsWall,currsArea,currsSource,currsSourceCT,selA,selS,selW,selCT,$
    lstWalls,lstAreas,lstSources,lstSourcesCT,materialList,isotopes,stdWall,shieldData,shieldDataCT,txtWorkDaysPrYear,workdays,$
    txtEditIdW,corrAlign,txtEditPosW,txtEditThickW,lstEditMatW,btnUseStdW,lblStdWthick,lblStdWmat,$
    txtEditIdA, txtEditPosA,txtEditOccA,$
    txtEditIdS,txtEditPosS,lstEditIsotope,btnEditInPat,txtEditA0,txtEditT1,txtEditT2,txtEditRestVoid,txtEditNS,$
    txtEditIdSCT,txtEditPosSCT,lstEditkV,txtEditRot,txtEditmas,txtEditkvcorr,txtEditNSCT,$ 
    lstDataIsotope,lstDataMat,txtDataT12,txtDataAlpha,txtDataBeta,txtDataGamma,txtDataTVL,txtDataConstAir,txtDataConstPat,$
    lstDatakV,txtDataAlphakV,txtDataBetakV,txtDataGammakV,$
    tCTsag,tCTcor,sagTab,corTab,defaultIsoPaths

  version='v1.0'
  ;programStops=-1
  thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('Shield_NM_CT'))+'\'
  zoomFactor=1.
  zoomIn=[0.,0.,1.,1.]
  mouseDownRight=0
  lastFieldMarked=[-1,-1,-1,-1]
  maxIm=[700,850];900,850
  
  defaultImgPath=thisPath+'images\25wide_grid.png'
  readNewImg, defaultImgPath
  
  workdays=230
  alphaImg=0.5
  alphaOcc=0.2
  alphaDose=0.7
  currTab=0

  materialList=['Lead','Concrete 2.35 g/m3']
  isotopes=['F18','Tc99m','I123','I131','Lu177']
  kVs=['120 kV','140 kv']
  stdWall=CREATE_STRUCT('material',1,'thickness',20.)
  
  ;ref RPII 09/01 https://inis.iaea.org/collection/NCLCollectionStore/_Public/41/081/41081618.pdf?r=1&r=1 recommended by IAEA SSG-446
  ;ref Lu-177 http://www.hpschapters.org/northcarolina/NSDS/177LuPDF.pdf
  ;alpha/beta/gamma from AAPM 108 
  ;'abg',[[2.479,-1.093,1.376],[0.2813,-0.2349,0.9653]] ref ??? for Tc-99m
  ;tvl concrete from C8 https://www.stuklex.fi/sv/ohje/ST1-10#a3, Lu-177 assumed as In-111 with similar lead tvl
  ;Patient absorption Lu-177 assumed to 50% (more conservative than tabulated in RPII 09/01 for other similar isotopes)
  ;halflife in hours, abg cm-1, tvl in cm, gammarayconstant mikroSv/h/MBq at 1m, same with patientconstant
  shieldData=CREATE_STRUCT($
    'F18', CREATE_STRUCT('halflife',109.77/60,'abg',[[1.5430,-0.4408,2.136],[0.1539,-0.1161,2.0752]],'tvl',[1.7,22.5],'gammaRayConst',0.143,'patientConst',0.092),$
    'Tc99m',CREATE_STRUCT('halflife',6.01,'tvl',[0.09,14.5],'gammaRayConst',0.0195,'patientConst',0.0075),$
    'I123',CREATE_STRUCT('halflife',13.,'tvl',[0.12,13.0],'gammaRayConst',0.041,'patientConst',0.015),$
    'I131',CREATE_STRUCT('halflife',193,'tvl',[1.1,21.0],'gammaRayConst',0.0575,'patientConst',0.023),$
    'Lu177',CREATE_STRUCT('halflife',161.5,'tvl',[0.21,16.0],'gammaRayConst',0.00517,'patientConst',0.0026))
  
  ;NCRP 147 mm-1
  shieldDataCT=CREATE_STRUCT($
    'kV120', [[2.246,5.73,0.547],[0.0383,0.0142,0.658]],$
    'kV140', [[2.009,3.99,0.342],[0.0336,0.0122,0.519]])
    
  ;default CT isodoses
  defaultIsoPaths=thisPath+['SiemensEdge140kVp_isocurves_cor.txt','SiemensEdge140kVp_isocurves_sag.txt']
  readCTisoDefault
  
  sWalls=!Null
  sAreas=!Null
  sSources=!Null
  sSourcesCT=!Null
  subsWallsDef=CREATE_STRUCT('id','','pos',[0.0,0.0,0.0,0.0],'material',0,'thickness',0.0,'std',0);if thickness = -1 then use standard thickness
  subsAreasDef=CREATE_STRUCT('id','','pos',[0.0,0.0,0.0,0.0],'occ',0.0)
  subsSourcesDef=CREATE_STRUCT('id','','pos',[0.0,0.0],'isotope','F18','inpatient',0,'a0',0.0,'t1',0.0,'t2',0.0,'restvoid',1.0,'nPrWorkDay',0.0)
  subsSourcesCTDef=CREATE_STRUCT('id','','pos',[0.0,0.0],'kVp',140,'rot',0.0,'kVcorr',1.0,'mAsPrPat',0.0,'nPrWorkDay',0.0)
  currsWall=subsWallsDef & currsArea=subsAreasDef & currsSource=subsSourcesDef & currsSourceCT=subsSourcesCTDef; holding current values in edit fields
  selA=-1 & selW=-1 & selS=-1 & selCT=-1;selected in lists

  font0="Tahoma*ITALIC*BOLD*18"

  bMain = WIDGET_BASE(TITLE='Shield_NM_CT '+version, MBAR=bar, /COLUMN, XSIZE=1650, YSIZE=950, XOFFSET=350, YOFFSET=20,/TLB_KILL_REQUEST_EVENTS)

  file_menu=WIDGET_BUTTON(bar, VALUE='File', /MENU)
  btn_load=WIDGET_BUTTON(file_menu, VALUe='Load map image', UVALUE='loadMap')
  btn_save=WIDGET_BUTTON(file_menu, VALUe='Save all to .dat file', UVALUE='save', ACCELERATOR='Ctrl+S')
  btn_open=WIDGET_BUTTON(file_menu, VALUe='Open .dat file', UVALUE='open', ACCELERATOR='Ctrl+O')
  btn_clearall=WIDGET_BUTTON(file_menu, VALUe='Clear all', UVALUE='clearall')
  btn_exit=WIDGET_BUTTON(file_menu, VALUe='Exit', UVALUE='Exit', ACCELERATOR='Ctrl+X', /SEPARATOR)

  bRow=WIDGET_BASE(bMain,/ROW)

  bLft=WIDGET_BASE(bRow,/COLUMN)

  bToolbars=WIDGET_BASE(bLft,/ROW)
  tbMap=WIDGET_BASE(bToolbars,/TOOLBAR,/ROW,XSIZE=500)
  ;btnRedraw=WIDGET_BUTTON(tbMap, VALUe=thisPath+'images\refresh.bmp',/BITMAP, TOOLTIP='Refresh image',UVALUE='redraw')
  btnLoad=WIDGET_BUTTON(tbMap, VALUe=thisPath+'images\importd.bmp',/BITMAP, TOOLTIP='Import map image',UVALUE='loadMap')
  btnOpen=WIDGET_BUTTON(tbMap, VALUe=thisPath+'images\importf.bmp',/BITMAP, TOOLTIP='Open saved .dat file',UVALUE='open')
  btnSave=WIDGET_BUTTON(tbMap, VALUe=thisPath+'images\save.bmp',/BITMAP, TOOLTIP='Save all to .dat file',UVALUE='save')
  btnSaveImg=WIDGET_BUTTON(tbMap, VALUe=thisPath+'images\camera.bmp',/BITMAP, TOOLTIP='Save as image',UVALUE='saveimg')
  btnCrop=WIDGET_BUTTON(tbMap, VALUE='Save cropped', TOOLTIP='Mark area in map (right mouseclick and drag) and then press this button to save that area as new .dat file', UVALUE='crop')

  drawRooms = WIDGET_DRAW(bLft, XSIZE=maxIm(0),YSIZE=maxIm(1), NOTIFY_REALIZE='drawfirst', KEYBOARD_EVENTS=1, /BUTTON_EVENTS, /MOTION_EVENTS, GRAPHICS_LEVEL=2, RETAIN=2, SENSITIVE=1)

  bRgt=WIDGET_BASE(bROw,/COLUMN)
  
  wtab=WIDGET_TAB(bRgt,UVALUE='tabs')
  tabVis=WIDGET_BASE(wtab, TITLE='Visualization', /COLUMN, UVALUE='tabVis')
  tabWalls=WIDGET_BASE(wtab, TITLE='Walls', /COLUMN, UVALUE='tabWalls')
  tabAreas=WIDGET_BASE(wtab, TITLE='Areas', /COLUMN, UVALUE='tabAreas')
  tabSources=WIDGET_BASE(wtab, TITLE='Sources', /COLUMN, UVALUE='tabSources')
  tabShield=WIDGET_BASE(wtab,TITLE='Shielding data', /COLUMN, UVALUE='tabShield')
  tabCT=WIDGET_BASE(wtab,TITLE='CT isodose', /COLUMN, UVALUE='tabCT')
  
  tbVis=WIDGET_BASE(tabVis, /ROW, /TOOLBAR)
  btnRefresh=WIDGET_BUTTON(tbVis, VALUE=thispath+'images\refresh.bmp',/BITMAP, TOOLTIP='Refresh image',UVALUE='refresh')
  btnZoomOut=WIDGET_BUTTON(tbVis, VALUE=thispath+'images\zoom_out.bmp',/BITMAP, TOOLTIP='Zoom out',UVALUE='zoomOut')
  btnGetZoomArea=WIDGET_BUTTON(tbVis, VALUE=thispath+'images\zoom_in.bmp',/BITMAP,TOOLTIP='Mark area in map (right mouseclick and drag) and press this button to zoom in on this area',UVALUE='getPosZ')

  mlVis0=WIDGET_LABEl(tabVis,VALUE='',YSIZE=15)

  bNumb=WIDGET_BASE(tabVis,/COLUMN)
  bPos=WIDGET_BASE(bNumb,/ROW)
  lblPos=WIDGET_LABEL(bPos,VALUE='Pos (pix)',XSIZE=125)
  txtPos=WIDGET_LABEL(bPos,VALUE='-,-',XSIZE=50)
  bPosM=WIDGET_BASE(bNumb,/ROW)
  lblPosM=WIDGET_LABEL(bPosM,VALUE='Pos (m)',XSIZE=125)
  txtPosM=WIDGET_LABEL(bPosM,VALUE='-,-',XSIZE=50)
  bOcc=WIDGET_BASE(bNumb,/ROW)
  lblOcc=WIDGET_LABEL(bOcc,VALUE='Occupancy factor',XSIZE=125)
  txtOcc=WIDGET_LABEL(bOcc,VALUE='-',XSIZE=50)
  bDose=WIDGET_BASE(bNumb,/ROW)
  lblDose=WIDGET_LABEL(bDose,VALUE='Dose NM+CT (mSv)',XSIZE=125)
  txtDose=WIDGET_LABEL(bDose,VALUE='-',XSIZE=50)
  bDoseNM=WIDGET_BASE(bNumb,/ROW)
  lblDoseNM=WIDGET_LABEL(bDoseNM,VALUE='Dose NM (mSv)',XSIZE=125)
  txtDoseNM=WIDGET_LABEL(bDoseNM,VALUE='-',XSIZE=50)
  bDoseMaxNM=WIDGET_BASE(bNumb,/ROW)
  lblDoseRateNM=WIDGET_LABEL(bDoseMaxNM,VALUE='Max dose rate NM (uSv/h)',XSIZE=125)
  txtDoseRateNM=WIDGET_LABEL(bDoseMaxNM,VALUE='-',XSIZE=50)
  bDoseCT=WIDGET_BASE(bNumb,/ROW)
  lblDoseCT=WIDGET_LABEL(bDoseCT,VALUE='Dose CT (mSv)',XSIZE=125)
  txtDoseCT=WIDGET_LABEL(bDoseCT,VALUE='-',XSIZE=50)
  mlVis3=WIDGET_LABEl(tabVis,VALUE='',XSIZE=15)
  lblWorkDaysPrYear=WIDGET_LABEL(bDose,VALUE='in total for ')
  txtWorkDaysPrYear=WIDGET_TEXT(bDose,VALUE='230',XSIZE=7,/EDITABLE)
  lblWorkDaysPrYear2=WIDGET_LABEL(bDose,VALUE='workdays')
  
  mlVis2=WIDGET_LABEl(tabVis,VALUE='',YSIZE=15)

  bKalib=WIDGET_BASE(tabVis,/ROW)
  lblKalibInfo=WIDGET_LABEL(bKalib, VALUE='Scale map:  ',XSIZE=100)
  btnStartPos=WIDGET_BUTTON(bKalib, VALUE='StartPos',TOOLTIP='Click on position in map and press this button to retrieve as start-position',UVALUE='startMap')
  lblStartPos=WIDGET_LABEL(bKalib, VALUE='-,-',XSIZE=50)
  btnEndPos=WIDGET_BUTTON(bKalib, VALUE='EndPos',TOOLTIP='Click on position in map and press this button to retrieve as end-position',UVALUE='endMap')
  lblEndPos=WIDGET_LABEL(bKalib, VALUE='-,-',XSIZE=50)
  txtCalib=WIDGET_TEXT(bKalib, VALUE='0',XSIZE=7,/EDITABLE,/KBRD_FOCUS_EVENTS)
  lblCalib_mm=WIDGET_LABEL(bKalib, VALUE='m')
  
  bMeasLen=WIDGET_BASE(tabVis,/ROW)
  lblMeasLenInfo=WIDGET_LABEL(bMeasLen, VALUE='Measure length:  ',XSIZE=100)
  btnStartMeasLen=WIDGET_BUTTON(bMeasLen, VALUE='StartPos',TOOLTIP='Click on position in map and press this button to retrieve as start-position',UVALUE='startMeasLen')
  lblStartMeasLen=WIDGET_LABEL(bMeasLen, VALUE='-,-',XSIZE=50)
  btnEndMeasLen=WIDGET_BUTTON(bMeasLen, VALUE='EndPos',TOOLTIP='Click on position in map and press this button to retrieve as end-position',UVALUE='endMeasLen')
  lblEndMeasLen=WIDGET_LABEL(bMeasLen, VALUE='-,-',XSIZE=50)
  txtMeasLen=WIDGET_TEXT(bMeasLen, VALUE='-',XSIZE=7)
  lblMeasLen_mm=WIDGET_LABEL(bMeasLen, VALUE='m')
  btnMeasLenClear=WIDGET_BUTTON(bMeasLen,VALUE='Clear',UVALUE='measLenClear')
  
  mlVis1=WIDGET_LABEl(tabVis,VALUE='',YSIZE=15)

  bShow=WIDGET_BASE(tabVis,/ROW)
  selShow=CW_BGROUP(bShow, ['Image map','Size scale','Walls','Wall thickness','Sources','CT sources'], LABEL_TOP='Show...', /NONEXCLUSIVE,/COLUMN, /FRAME, SET_VALUE=[1,1,1,1,1,1],UVALUE='selShow')
  selOverlay=CW_BGROUP(bShow, ['Occupancy factors', 'Dose','Max dose rate NM'], LABEL_TOP='Overlay...',/EXCLUSIVE,/COLUMN,/FRAME, SET_VALUE=0,UVALUE='selOverlay')
  selDose=CW_BGROUP(bShow,['Total dose','NM dose','CT dose'],LABEL_TOP='Dose...',/EXCLUSIVE,/COLUMN,/FRAME,SET_VALUE=0,UVALUE='selDose')

  bSliders=WIDGET_BASE(bShow, /COLUMN)
  lblSlideImg=WIDGET_LABEL(bSliders,VALUE='Color density map image %')
  slideImg=WIDGET_SLIDER(bSliders,/DRAG,MINIMUM=0,MAXIMUM=100,VALUE=100*alphaImg,UVALUE='slideImg')
  lblSlideOcc=WIDGET_LABEL(bSliders,VALUE='Color density occupancy factors %')
  slideOcc=WIDGET_SLIDER(bSliders,/DRAG,MINIMUM=0,MAXIMUM=100,VALUE=100*alphaOcc,UVALUE='slideOcc')
  lblSlideDose=WIDGET_LABEL(bSliders,VALUE='Color density dose %')
  slideDose=WIDGET_SLIDER(bSliders,/DRAG,MINIMUM=0,MAXIMUM=100,VALUE=100*alphaDose,UVALUE='slideDose')
  
  mlCol=WIDGET_LABEL(bShow, VALUE='', XSIZE=20)
  
  bColorbar=WIDGET_BASE(bShow,/COLUMN)
  lblColor=WIDGET_LABEL(bColorbar,VALUE='Colorbar')
  btnColor=WIDGET_BUTTON(bColorbar, VALUE=thispath+'images\colorBar_occ.bmp',/BITMAP)
  
  mlVis4=WIDGET_LABEl(tabVis,VALUE='',YSIZE=15)

  bFloors=WIDGET_BASE(tabVis,/ROW)
  selFloor=CW_BGROUP(bFloors, ['Floor +1', 'This floor', 'Floor -1'], LABEL_TOP='Show dose for floor...',/EXCLUSIVE,/COLUMN,/FRAME, SET_VALUE=1,UVALUE='selFloor')
  bFloorSett=WIDGET_BASE(bFloors,/COLUMN)
  bFloorThis=WIDGET_BASE(bFloorSett,/ROW)
  lblFloorThis=WIDGET_LABEL(bFloorThis, VALUE='Floor height this H1 (m)',XSIZE=160)
  txtH1=WIDGET_TEXT(bFloorThis, VALUE='4.000',XSIZE=7,/EDITABLE,/KBRD_FOCUS_EVENTS)
  bFloorbelow=WIDGET_BASE(bFloorSett,/ROW)
  lblFloorbelow=WIDGET_LABEL(bFloorbelow, VALUE='Floor height below H0 (m)',XSIZE=160)
  txtH0=WIDGET_TEXT(bFloorbelow, VALUE='4.000',XSIZE=7,/EDITABLE,/KBRD_FOCUS_EVENTS)
  bSourceAbove=WIDGET_BASE(bFloorSett,/ROW)
  lblSourceAbove=WIDGET_LABEL(bSourceAbove, VALUE='Source above floor C1 (m)',XSIZE=160)
  txtC1=WIDGET_TEXT(bSourceAbove, VALUE='1.0',XSIZE=7)
  bCalcAbove=WIDGET_BASE(bFloorSett,/ROW)
  lblCalcAbove=WIDGET_LABEL(bCalcAbove, VALUE='Calculate at height floor +1 C2 (m)',XSIZE=160)
  txtC2=WIDGET_TEXT(bCalcAbove, VALUE='0.5',XSIZE=7)
  bCalcBelow=WIDGET_BASE(bFloorSett,/ROW)
  lblCalcBelow=WIDGET_LABEL(bCalcBelow, VALUE='Calculate at height floor -1 C0 (m)',XSIZE=160)
  txtC0=WIDGET_TEXT(bCalcBelow, VALUE='1.7',XSIZE=7)
  imgFloors=WIDGET_BUTTON(bFloors, VALUE=thispath+'images\hoyder.bmp',/BITMAP)
  lblMlFloors=WIDGET_LABEl(tabVis,VALUE='',XSIZE=15)
  bFloorThick=WIDGET_BASE(bFloors,/COLUMN)
  bThickConCeil=WIDGET_BASE(bFloorThick,/ROW)
  lblConCeil=WIDGET_LABEL(bThickConCeil, VALUE='Concrete thickness cealing (cm)',XSIZE=160)
  txtConCeil=WIDGET_TEXT(bThickConCeil, VALUE='20.0',XSIZE=7,/EDITABLE,/KBRD_FOCUS_EVENTS)
  bThickLeadCeil=WIDGET_BASE(bFloorThick,/ROW)
  lblLeadCeil=WIDGET_LABEL(bThickLeadCeil, VALUE='Lead thickness cealing (cm)',XSIZE=160)
  txtLeadCeil=WIDGET_TEXT(bThickLeadCeil, VALUE='0.0',XSIZE=7,/EDITABLE,/KBRD_FOCUS_EVENTS)
  bThickConFloor=WIDGET_BASE(bFloorThick,/ROW)
  lblConFloor=WIDGET_LABEL(bThickConFloor, VALUE='Concrete thickness floor (cm)',XSIZE=160)
  txtConFloor=WIDGET_TEXT(bThickConFloor, VALUE='20.0',XSIZE=7,/EDITABLE,/KBRD_FOCUS_EVENTS)
  bThickLeadFloor=WIDGET_BASE(bFloorThick,/ROW)
  lblLeadFloor=WIDGET_LABEL(bThickLeadFloor, VALUE='Lead thickness floor (cm)',XSIZE=160)
  txtLeadFloor=WIDGET_TEXT(bThickLeadFloor, VALUE='0.0',XSIZE=7,/EDITABLE,/KBRD_FOCUS_EVENTS)
  
  bCorrThick=WIDGET_BASE(tabVis,/ROW,/NONEXCLUSIVE)
  btnCorrThick=WIDGET_BUTTON(bCorrThick,VALUE='Correct geometrically for material thickness (NB: might underestimate dose due to pathlength of rays scattered in the wall)',UVALUE='corrThick')
  ml=WIDGET_LABEL(tabVis, VALUE='', YSIZE=10)
  ml=WIDGET_LABEL(tabVis, VALUE='(For oblique walls the thickness correction will be ignored.)')

  ;*********** walls *****************
  lblMlwalls=WIDGET_LABEL(tabWalls,VALUE='',YSIZE=15)
  hWalls=WIDGET_LABEL(tabWalls, VALUE='Walls ', /ALIGN_LEFT,FONT=font0)
  bWalls=WIDGET_BASE(tabWalls, /ROW)
  lstWalls=WIDGET_TABLE(bWalls, /ALL_EVENTS, SCR_XSIZE=420, XSIZE=5, COLUMN_LABELS=['Wall ID','x1, y1, x2, y2','Thickness (cm)','Material','Std'],COLUMN_WIDTHS=[90,130,80,80,20],/NO_ROW_HEADERS,ALIGNMENT=0,SCR_YSIZE=500,UVALUE='lstWalls')
  mlW0=WIDGET_LABEL(bWalls, VALUE='', XSIZE=10)
  tbW=WIDGET_BASE(bWalls,/TOOLBAR,/COLUMN)
  btnDelW=WIDGET_BUTTON(tbW, VALUE=thispath+'images\delete.bmp',/BITMAP, TOOLTIP='Delete selected wall from list',UVALUE='deleteW')
  btnSaveW=WIDGET_BUTTON(tbW, VALUE=thispath+'images\plus.bmp',/BITMAP, TOOLTIP='Add new wall to list',UVALUE='addW')
  btnEditW=WIDGET_BUTTON(tbW, VALUE=thispath+'images\edit.bmp',/BITMAP, TOOLTIP='Update selected wall in list with current values',UVALUE='editW')
  btnCopyW=WIDGET_BUTTON(tbW, VALUE=thispath+'images\copy.bmp',/BITMAP, TOOLTIP='Copy table to clipboard',UVALUE='copyW')
  btnImportW=WIDGET_BUTTON(tbW, VALUE=thispath+'images\importd.bmp',/BITMAP, TOOLTIP='Import table from clipboard',UVALUE='importW')
  mlW=WIDGET_LABEL(bWalls, VALUE='', XSIZE=10)
  bAddW=WIDGET_BASE(bWalls,/COLUMN,XSIZE=400)
  bEditIdW=WIDGET_BASE(bAddW,/ROW)
  lblEditIdW=WIDGET_LABEL(bEditIdW,VALUE='ID', XSIZE=30)
  txtEditIdW=WIDGET_TEXT(bEditIdW, VALUE='', XSIZE=15, /EDITABLE)
  ml=WIDGET_LABEL(bEditIdW, VALUE='', XSIZE=50)
  corrAlign=CW_BGROUP(bEditIdW,['Straighten','Keep oblique'],LABEL_TOP='Align?',/EXCLUSIVE,/COLUMN,/FRAME,SET_VALUE=0,UVALUE='align')
  bEditPosW=WIDGET_BASE(bAddW,/ROW)
  lblEditPosW=WIDGET_LABEL(bEditPosW,VALUE='x1, y1, x2, y2', XSIZE=80)
  txtEditPosW=WIDGET_TEXT(bEditPosW, VALUE='', XSIZE=20, /EDITABLE)
  btnEditStartW=WIDGET_BUTTON(bEditPosW, VALUE='Get x1,y1',TOOLTIP='Click on position in map and press this button to retrieve as x1,y1',UVALUE='startWall',XSIZE=80)
  btnEditEndW=WIDGET_BUTTON(bEditPosW, VALUE='Get x2,y2',TOOLTIP='Click on position in map and press this button to retrieve as x2,y2',UVALUE='endWall',XSIZE=80)
  bEditThickW=WIDGET_BASE(bAddW,/ROW)
  lblEditThickW=WIDGET_LABEL(bEditThickW,VALUE='Thickness (cm)', XSIZE=80)
  txtEditThickW=WIDGET_TEXT(bEditThickW, VALUE='0', XSIZE=7, /EDITABLE)
  lblStdWml=WIDGET_LABEL(bEditThickW, VALUE='', XSIZE=50)
  lblStdW=WIDGET_LABEL(bEditThickW,VALUE='Standard:',XSIZE=60)
  lblStdWthick=WIDGET_LABEL(bEditThickW,VALUE=STRING(stdWall.thickness,FORMAT='(f0.1)'),XSIZE=30)
  lblStdWmm=WIDGET_LABEL(bEditThickW,VALUE='cm',XSIZE=30)
  bEditMatW=WIDGET_BASE(bAddW,/ROW)
  lblEditMatW=WIDGET_LABEL(bEditMatW,VALUE='Material', XSIZE=80)
  lstEditMatW=WIDGET_DROPLIST(bEditMatW, VALUE=materialList, XSIZE=100)
  lblStdWmat0=WIDGET_LABEL(bEditMatW,VALUE='   Standard:',XSIZE=60)
  lblStdWmat=WIDGET_LABEL(bEditMatW,VALUE=materialList(stdWall.material),XSIZE=90)
  bUseStdW=WIDGET_BASE(bAddW,/ROW,/NONEXCLUSIVE)
  btnUseStdW=WIDGET_BUTTON(bUseStdW,VALUE='Use standard wall', XSIZE=150,UVALUE='useStdW')
  btnStdWset=WIDGET_BUTTON(bAddW,VALUE='Change standard wall to selected thickness and material',UVALUE='setStdWall')

  ;****************areas*******************************************
  lblMlareas=WIDGET_LABEL(tabAreas,VALUE='',YSIZE=15)
  hAreas=WIDGET_LABEL(tabAreas, VALUE='Areas ', /ALIGN_LEFT,FONT=font0)
  bAreas=WIDGET_BASE(tabAreas, /ROW)
  lstAreas=WIDGET_TABLE(bAreas,/ALL_EVENTS,  SCR_XSIZE=420, XSIZE=3, COLUMN_LABELS=['Area ID','x1, y1, x2, y2','Occupancy factor'],COLUMN_WIDTHS=[90,130,100],/NO_ROW_HEADERS,ALIGNMENT=0,SCR_YSIZE=500,UVALUE='lstAreas')
  mlA0=WIDGET_LABEL(bAreas, VALUE='', XSIZE=10)
  tbA=WIDGET_BASE(bAreas,/TOOLBAR,/COLUMN)
  btnDelA=WIDGET_BUTTON(tbA, VALUE=thispath+'images\delete.bmp',/BITMAP, TOOLTIP='Delete selected area from list',UVALUE='deleteA')
  btnSaveA=WIDGET_BUTTON(tbA, VALUE=thispath+'images\plus.bmp',/BITMAP, TOOLTIP='Add new area to list',UVALUE='addA')
  btnEditA=WIDGET_BUTTON(tbA, VALUE=thispath+'images\edit.bmp',/BITMAP, TOOLTIP='Update selected area in list with current values',UVALUE='editA')
  btnCopyA=WIDGET_BUTTON(tbA, VALUE=thispath+'images\copy.bmp',/BITMAP, TOOLTIP='Copy table to clipboard',UVALUE='copyA')
  btnImportA=WIDGET_BUTTON(tbA, VALUE=thispath+'images\importd.bmp',/BITMAP, TOOLTIP='Import table from clipboard',UVALUE='importA')
  mlA=WIDGET_LABEL(bAreas, VALUE='', XSIZE=10)
  bAddA=WIDGET_BASE(bAreas,/COLUMN,XSIZE=500)
  bEditIdA=WIDGET_BASE(bAddA,/ROW)
  lblEditIdA=WIDGET_LABEL(bEditIdA,VALUE='ID', XSIZE=30)
  txtEditIdA=WIDGET_TEXT(bEditIdA, VALUE='', XSIZE=15, /EDITABLE)
  bEditPosA=WIDGET_BASE(bAddA,/ROW)
  lblEditPosA=WIDGET_LABEL(bEditPosA,VALUE='x1, y1, x2, y2', XSIZE=70)
  txtEditPosA=WIDGET_TEXT(bEditPosA, VALUE='', XSIZE=22, /EDITABLE)
  btnGetPosA=WIDGET_BUTTON(bEditPosA, VALUE='Get positions',TOOLTIP='Mark field in map (right mouseclick and drag) and press this button to retrieve positions from marked field as area',UVALUE='getPosA')
  bEditOccA=WIDGET_BASE(bAddA,/ROW)
  lblEditOccA=WIDGET_LABEL(bEditOccA,VALUE='Occupancy factor', XSIZE=90)
  txtEditOccA=WIDGET_TEXT(bEditOccA, VALUE='1.00', XSIZE=7, /EDITABLE)

  ;*********************** sources ********************************
  blMlsources=WIDGET_LABEL(tabSources,VALUE='',YSIZE=15)
  hSources=WIDGET_LABEL(tabSources, VALUE='Radioactive sources ', /ALIGN_LEFT,FONT=font0)
  bSources=WIDGET_BASE(tabSources, /ROW)
  lstSources=WIDGET_TABLE(bSources, /ALL_EVENTS, SCR_XSIZE=600, XSIZE=9, COLUMN_LABELS=['Source ID','x, y','Isotope','In patient?','A0 (MBq)','t1','duration','Rest void','# pr workday'],COLUMN_WIDTHS=[90,80,50,60,70,40,50,70, 70],/NO_ROW_HEADERS,ALIGNMENT=0,SCR_YSIZE=400,UVALUE='lstSources')
  mlS0=WIDGET_LABEL(bSources, VALUE='', XSIZE=10)
  tbS=WIDGET_BASE(bSources,/TOOLBAR,/COLUMN)
  btnDelS=WIDGET_BUTTON(tbS, VALUE=thispath+'images\delete.bmp',/BITMAP, TOOLTIP='Delete selected source from list',UVALUE='deleteS')
  btnSaveS=WIDGET_BUTTON(tbS, VALUE=thispath+'images\plus.bmp',/BITMAP, TOOLTIP='Add parameters as new source in table',UVALUE='addS')
  btnEditS=WIDGET_BUTTON(tbS, VALUE=thispath+'images\edit.bmp',/BITMAP, TOOLTIP='Update selected source in list with current values',UVALUE='editS')
  btnCopyS=WIDGET_BUTTON(tbS, VALUE=thispath+'images\copy.bmp',/BITMAP, TOOLTIP='Copy table to clipboard',UVALUE='copyS')
  btnImportS=WIDGET_BUTTON(tbS, VALUE=thispath+'images\importd.bmp',/BITMAP, TOOLTIP='Import table from clipboard',UVALUE='importS')
  mlS=WIDGET_LABEL(bSources, VALUE='', XSIZE=10)
  bAddS=WIDGET_BASE(bSources,/COLUMN,XSIZE=500)
  bEditIdS=WIDGET_BASE(bAddS,/ROW)
  lblEditIdS=WIDGET_LABEL(bEditIdS,VALUE='ID', XSIZE=30)
  txtEditIdS=WIDGET_TEXT(bEditIdS, VALUE='', XSIZE=15, /EDITABLE)
  bEditPosS=WIDGET_BASE(bAddS,/ROW)
  lblEditPosS=WIDGET_LABEL(bEditPosS,VALUE='x, y', XSIZE=30)
  txtEditPosS=WIDGET_TEXT(bEditPosS, VALUE='', XSIZE=20, /EDITABLE)
  btnGetPosS=WIDGET_BUTTON(bEditPosS, VALUE='Get x,y',TOOLTIP='Click on position in map and press this button to retrieve as x,y',UVALUE='getPosS')
  bEditIsotope=WIDGET_BASE(bAddS,/ROW)
  lblEditIsotope=WIDGET_LABEL(bEditIsotope,VALUE='Isotope', XSIZE=70)
  lstEditIsotope=WIDGET_DROPLIST(bEditIsotope, VALUE=isotopes, XSIZE=100)
  bEditInPat=WIDGET_BASE(bAddS,/ROW,/NONEXCLUSIVE)
  btnEditInPat=WIDGET_BUTTON(bEditInPat,VALUE='Use damping factor in patient', XSIZE=150)
  bEditA0=WIDGET_BASE(bAddS,/ROW)
  lblEditA0=WIDGET_LABEL(bEditA0,VALUE='A0 Activity at start (MBq)', XSIZE=150)
  txtEditA0=WIDGET_TEXT(bEditA0, VALUE='0', XSIZE=7, /EDITABLE)
  bEditT1=WIDGET_BASE(bAddS,/ROW)
  lblEditT1=WIDGET_LABEL(bEditT1,VALUE='T1 when activity reaches this position (hours)', XSIZE=220)
  txtEditT1=WIDGET_TEXT(bEditT1, VALUE='0.0', XSIZE=7, /EDITABLE)
  bEditT2=WIDGET_BASE(bAddS,/ROW)
  lblEditT2=WIDGET_LABEL(bEditT2,VALUE='Duration of activity at this position (hours)', XSIZE=220)
  txtEditT2=WIDGET_TEXT(bEditT2, VALUE='0.0', XSIZE=7, /EDITABLE)
  bEditRestVoid=WIDGET_BASE(bAddS,/ROW)
  lblEditRestVoid=WIDGET_LABEL(bEditRestVoid,VALUE='Rest fraction after voiding', XSIZE=220)
  txtEditRestVoid=WIDGET_TEXT(bEditRestVoid, VALUE='1.00', XSIZE=7, /EDITABLE)
  bEditNS=WIDGET_BASE(bAddS,/ROW)
  lblEditNS=WIDGET_LABEL(bEditNS,VALUE='Number of procedures pr working day', XSIZE=200)
  txtEditNS=WIDGET_TEXT(bEditNS, VALUE='0', XSIZE=7, /EDITABLE)
  
  mlS=WIDGET_LABEL(tabSources,VALUE='',YSIZE=20)
  
  hSourcesCT=WIDGET_LABEL(tabSources, VALUE='CT Sources ', /ALIGN_LEFT,FONT=font0)
  bSourcesCT=WIDGET_BASE(tabSources, /ROW)
  lstSourcesCT=WIDGET_TABLE(bSourcesCT, /ALL_EVENTS, SCR_XSIZE=540, XSIZE=7, COLUMN_LABELS=['Source ID','x, y','kVp','rotation','kVp corr.factor','mAs pr pat','# pr workday'],COLUMN_WIDTHS=[90,80,50,50,80,70,70],/NO_ROW_HEADERS,ALIGNMENT=0,SCR_YSIZE=200,UVALUE='lstSourcesCT')
  mlS0CT=WIDGET_LABEL(bSourcesCT, VALUE='', XSIZE=10)
  tbSCT=WIDGET_BASE(bSourcesCT,/TOOLBAR,/COLUMN)
  btnDelSCT=WIDGET_BUTTON(tbSCT, VALUE=thispath+'images\delete.bmp',/BITMAP, TOOLTIP='Delete selected source from list',UVALUE='deleteSCT')
  btnSaveSCT=WIDGET_BUTTON(tbSCT, VALUE=thispath+'images\plus.bmp',/BITMAP, TOOLTIP='Add parameters as new source in table',UVALUE='addSCT')
  btnEditSCT=WIDGET_BUTTON(tbSCT, VALUE=thispath+'images\edit.bmp',/BITMAP, TOOLTIP='Update selected source in list with current values',UVALUE='editSCT')
  btnCopySCT=WIDGET_BUTTON(tbSCT, VALUE=thispath+'images\copy.bmp',/BITMAP, TOOLTIP='Copy table to clipboard',UVALUE='copySCT')
  btnImportSCT=WIDGET_BUTTON(tbSCT, VALUE=thispath+'images\importd.bmp',/BITMAP, TOOLTIP='Import table from clipboard',UVALUE='importSCT')
  mlSCT=WIDGET_LABEL(bSourcesCT, VALUE='', XSIZE=10)
  bAddSCT=WIDGET_BASE(bSourcesCT,/COLUMN,XSIZE=500)
  bEditIdSCT=WIDGET_BASE(bAddSCT,/ROW)
  lblEditIdSCT=WIDGET_LABEL(bEditIdSCT,VALUE='ID', XSIZE=30)
  txtEditIdSCT=WIDGET_TEXT(bEditIdSCT, VALUE='', XSIZE=15, /EDITABLE)
  bEditPosSCT=WIDGET_BASE(bAddSCT,/ROW)
  lblEditPosSCT=WIDGET_LABEL(bEditPosSCT,VALUE='x, y', XSIZE=30)
  txtEditPosSCT=WIDGET_TEXT(bEditPosSCT, VALUE='', XSIZE=20, /EDITABLE)
  btnGetPosSCT=WIDGET_BUTTON(bEditPosSCT, VALUE='Get x,y',TOOLTIP='Click on position in map and press this button to retrieve as x,y',UVALUE='getPosSCT')
  bEditkV=WIDGET_BASE(bAddSCT,/ROW)
  lblEditkV=WIDGET_LABEL(bEditkV,VALUE='kVp', XSIZE=30)
  lstEditkV=WIDGET_DROPLIST(bEditkV, VALUE=['120','140'], XSIZE=100)
  bEditRot=WIDGET_BASE(bAddSCT,/ROW)
  lblEditRot=WIDGET_LABEL(bEditRot,VALUE='Rotation (degrees)', XSIZE=100)
  txtEditRot=WIDGET_TEXT(bEditRot, VALUE='0', XSIZE=5, /EDITABLE)
  bEditkvcorr=WIDGET_BASE(bAddSCT,/ROW)
  lblEditkvcorr=WIDGET_LABEL(bEditkvcorr,VALUE='kVp correction factor', XSIZE=100)
  txtEditkvcorr=WIDGET_TEXT(bEditkvcorr, VALUE='1.0', XSIZE=5, /EDITABLE)
  lblEditkvcorr2=WIDGET_LABEL(bEditkvcorr,VALUE=' to adjust isodose curves')
  bEditmas=WIDGET_BASE(bAddSCT,/ROW)
  lblEditmas=WIDGET_LABEL(bEditmas,VALUE='mAs pr patient', XSIZE=100)
  txtEditmas=WIDGET_TEXT(bEditmas, VALUE='0', XSIZE=5, /EDITABLE)
  bEditNSCT=WIDGET_BASE(bAddSCT,/ROW)
  lblEditNSCT=WIDGET_LABEL(bEditNSCT,VALUE='Number of procedures pr working day', XSIZE=200)
  txtEditNSCT=WIDGET_TEXT(bEditNSCT, VALUE='0', XSIZE=5, /EDITABLE)
  
  ;************************ shield settings ****************
mlSett=WIDGET_LABEL(tabShield,VALUE='',YSIZE=20)
  bDataMat=WIDGET_BASE(tabShield, /ROW)
  lblDataMat=WIDGET_LABEL(bDataMat,VALUE='Material', XSIZE=70)
  lstDataMat=WIDGET_DROPLIST(bDataMat, VALUE=materialList, XSIZE=100, UVALUE='dataMaterial')
  
  mlSh0=WIDGET_LABEl(tabVis,VALUE='',YSIZE=30)
  
  bIsoOrkV=WIDGET_BASE(tabShield,/ROW)
  bIso=WIDGET_BASE(bIsoOrkV,/COLUMN)
  bDataIsotope=WIDGET_BASE(bIso, /ROW)
  lblDataIsotope=WIDGET_LABEL(bDataIsotope,VALUE='Isotope', XSIZE=70)
  lstDataIsotope=WIDGET_DROPLIST(bDataIsotope, VALUE=isotopes, XSIZE=100, UVALUE='dataIsotope')
  bDataT12=WIDGET_BASE(bIso,/ROW)
  lblDataT12=WIDGET_LABEL(bDataT12, VALUE='Halflife (h)', XSIZE=70)
  txtDataT12=WIDGET_TEXT(bDataT12, VALUE='', XSIZE=7)
  bDataAlpha=WIDGET_BASE(bIso,/ROW)
  lblDataAlpha=WIDGET_LABEL(bDataAlpha, VALUE='Alpha', XSIZE=70)
  txtDataAlpha=WIDGET_TEXT(bDataAlpha, VALUE='', XSIZE=7)
  lblDataAlpha2=WIDGET_LABEL(bDataAlpha, VALUE='cm-1', XSIZE=70)
  bDataBeta=WIDGET_BASE(bIso,/ROW)
  lblDataBeta=WIDGET_LABEL(bDataBeta, VALUE='Beta', XSIZE=70)
  txtDataBeta=WIDGET_TEXT(bDataBeta, VALUE='', XSIZE=7)
  lblDataBeta2=WIDGET_LABEL(bDataBeta, VALUE='cm-1', XSIZE=70)
  bDataGamma=WIDGET_BASE(bIso,/ROW)
  lblDataGamma=WIDGET_LABEL(bDataGamma, VALUE='Gamma', XSIZE=70)
  txtDataGamma=WIDGET_TEXT(bDataGamma, VALUE='', XSIZE=7)
  bDataTVL=WIDGET_BASE(bIso,/ROW)
  lblDataTVL=WIDGET_LABEL(bDataTVL, VALUE='TVL (cm)', XSIZE=70)
  txtDataTVL=WIDGET_TEXT(bDataTVL, VALUE='', XSIZE=7)
  bDataConstAir=WIDGET_BASE(bIso,/ROW)
  lblDataConstAir=WIDGET_LABEL(bDataConstAir, VALUE='Gamma ray constant (uGy/MBq @ 1m)', XSIZE=230)
  txtDataConstAir=WIDGET_TEXT(bDataConstAir, VALUE='', XSIZE=7)
  bDataConstPat=WIDGET_BASE(bIso,/ROW)
  lblDataConstPat=WIDGET_LABEL(bDataConstPat, VALUE='Gamma ray constant from patient (uGy/MBq @ 1m)', XSIZE=230)
  txtDataConstPat=WIDGET_TEXT(bDataConstPat, VALUE='', XSIZE=7)
  
  mlSh1=WIDGET_LABEl(bIsoOrkV,VALUE='',XSIZE=15)
  
  bkV=WIDGET_BASE(bIsoOrkV,/COLUMN)
  bDatakV=WIDGET_BASE(bkV, /ROW)
  lblDatakV=WIDGET_LABEL(bDatakV,VALUE='CT kV', XSIZE=70)
  lstDatakV=WIDGET_DROPLIST(bDatakV, VALUE=kVs, XSIZE=100, UVALUE='datakV')
  bDataAlphakV=WIDGET_BASE(bkV,/ROW)
  lblDataAlphakV=WIDGET_LABEL(bDataAlphakV, VALUE='Alpha', XSIZE=70)
  txtDataAlphakV=WIDGET_TEXT(bDataAlphakV, VALUE='', XSIZE=7)
  lblDataAlphakV2=WIDGET_LABEL(bDataAlphakV, VALUE='mm-1', XSIZE=70)
  bDataBetakV=WIDGET_BASE(bkV,/ROW)
  lblDataBetakV=WIDGET_LABEL(bDataBetakV, VALUE='Beta', XSIZE=70)
  txtDataBetakV=WIDGET_TEXT(bDataBetakV, VALUE='', XSIZE=7)
  lblDataBetakV2=WIDGET_LABEL(bDataBetakV, VALUE='mm-1', XSIZE=70)
  bDataGammakV=WIDGET_BASE(bkV,/ROW)
  lblDataGammakV=WIDGET_LABEL(bDataGammakV, VALUE='Gamma', XSIZE=70)
  txtDataGammakV=WIDGET_TEXT(bDataGammakV, VALUE='', XSIZE=7)
  
  updShieldInfo, 0,0,0
  
  ;************************** CT ****************************
  hCTisoSag=WIDGET_LABEL(tabCT, VALUE='CT isodose sagittal (uGy/mAs) ', /ALIGN_LEFT,FONT=font0)
  txt2_35=STRING(ABS((0.5*FINDGEN(12))-2.),FORMAT='(f0.1)')+'m'
  tbIsoSag=WIDGET_BASE(tabCT, /ROW, /TOOLBAR)
  btnCopyIsoSag=WIDGET_BUTTON(tbIsoSag, VALUE=thispath+'images\copy.bmp',/BITMAP, TOOLTIP='Copy table to clipboard',UVALUE='copyIsoSag')
  btnImportISoSag=WIDGET_BUTTON(tbIsoSag, VALUE=thispath+'images\importd.bmp',/BITMAP, TOOLTIP='Import table from clipboard',UVALUE='importIsoSag')
  tCTsag=WIDGET_TABLE(tabCT,XSIZE=12,YSIZE=6,COLUMN_LABELS=txt2_35,ROW_LABELS=txt2_35[1:6],COLUMN_WIDTHS=50,ALIGNMENT=0, SCR_XSIZE=50*12+100, SCR_YSIZE=20*7+20)
  imgSag=WIDGET_BUTTON(tabCT, VALUE=thispath+'images\CTisoSag.bmp',/BITMAP)
  mlCT1=WIDGET_LABEl(tabCT,VALUE='',YSIZE=15)

  hCTisoCor=WIDGET_LABEL(tabCT, VALUE='CT isodose coronal (uGy/mAs) ', /ALIGN_LEFT,FONT=font0)
  tbIsoCor=WIDGET_BASE(tabCT, /ROW, /TOOLBAR)
  btnCopyIsoCor=WIDGET_BUTTON(tbIsoCor, VALUE=thispath+'images\copy.bmp',/BITMAP, TOOLTIP='Copy table to clipboard',UVALUE='copyIsoCor')
  btnImportIsoCor=WIDGET_BUTTON(tbIsoCor, VALUE=thispath+'images\importd.bmp',/BITMAP, TOOLTIP='Import table from clipboard',UVALUE='importIsoCor')
  bCor=WIDGET_BASE(tabCT,/ROW)
  tCTcor=WIDGET_TABLE(bCor,XSIZE=7,YSIZE=12,COLUMN_LABELS=txt2_35[1:7],ROW_LABELS=txt2_35,COLUMN_WIDTHS=50,ALIGNMENT=0, SCR_XSIZE=50*7+100, SCR_YSIZE=20*12+20)
  imgCor=WIDGET_BUTTON(bCor, VALUE=thispath+'images\CTisoCor.bmp',/BITMAP)
  
  WIDGET_CONTROL, tCTsag, SET_VALUE=STRING(sagTab)
  WIDGET_CONTROL, tCTcor, SET_VALUE=STRING(corTab)

  WIDGET_CONTROL, bMain, /REALIZE
  XMANAGER, 'Shield_NM_CT', bMain
  
  ;IF programStops NE 1 THEN updateVisuals, 'all'
  
end

