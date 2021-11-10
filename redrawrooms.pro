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

pro redrawRooms

  COMMON SHNM

  WIDGET_CONTROL,/HOURGLASS

  WIDGET_CONTROL, drawRooms, GET_VALUE = iDrawR
  oModel=OBJ_NEW('IDLgrModel')

  WIDGET_CONTROL, selShow, GET_VALUE=select2show
  WIDGET_CONTROL, selOverlay, GET_VALUE=select2overlay

  IF szim(0) GT maxIm(0) OR szim(1) GT maxIm(1) THEN BEGIN
    maxRelSz=szim[0:1]*1./maxIm
    zoomFactor=MAX(maxRelSz)
    viewPl=[0.,0.,maxIm(0)*zoomfactor,maxIm(1)*zoomfactor]
  ENDIF ELSE BEGIN
    zoomFactor=szim(0)*1./maxIm(0)
    viewPl=[0.,0.,maxIm(0),maxIm(1)]
  ENDELSE
  IF TOTAL(zoomIn[0:1]) NE 0. THEN BEGIN
    viewPl[0:1]=zoomIn[0:1]
  ENDIF
  IF zoomIn(2) NE 1. THEN viewPl[2:3]=zoomIn(2)*viewPl[2:3]

  oView=OBJ_NEW('IDLgrView', VIEWPLANE_RECT =viewPl)
  oPal=OBJ_NEW('IDLgrPalette')
  oPal->LoadCT, 0
  oPaletteROI=OBJ_NEW('IDLgrPalette')
  oPaletteROI->LoadCT, 0

  ;['Image map','Size calibration','Walls','Areas','Sources']
  IF select2show(0) EQ 1 THEN BEGIN
    ;imRed=im*alphaImg+255.*(1.-alphaImg)
    imSize = SIZE(im, /DIMENSIONS)
    oIm = OBJ_NEW('IDLgrImage', im, PALETTE=oPal, BLEND_FUNCTION = [3, 4],ALPHA_CHANNEL=alphaImg)
    oModel->Add, oIm
  ENDIF

  IF select2overlay EQ 0 THEN BEGIN;occupation factors
    IF N_ELEMENTS(occMap) NE N_ELEMENTS(im) THEN occMap=im*0.+1.
    IF MAX(occMap) GT 0 THEN BEGIN
      occPal=OBJ_NEW('IDLgrPalette')
      occPal->LoadCT, 25
      oOccMap = OBJ_NEW('IDLgrImage', 255*occMap , BLEND_FUNCTION = [3, 4], ALPHA_CHANNEL=alphaOcc, PALETTE=occPal)
      oModel->Add, oOccMap
      ;contourOcc=OBJ_NEW('IDLgrContour',occMap,C_COLOR=[[0,0,255],[0,255,255],[0,128,128],[0,0,128]], C_VALUE=[1.,.5,0.25,0.1],C_THICK=3);,C_FILL_PATTERN=1,FIll=1,ALPHA_CHANNEL=.5)
      ;oModel->Add, contourOcc
    ENDIF
  ENDIF

  IF select2overlay EQ 1 THEN BEGIN;dose
    ;IF SIZE(sSources,/TNAME) EQ 'STRUCT' THEN BEGIN
    IF N_ELEMENTS(doseMap) GT 0 OR N_ELEMENTS(doseMapCT) GT 0 THEN BEGIN
      IF N_ELEMENTS(doseMap) EQ 0 THEN doseMap = doseMapCT*0.
      IF N_ELEMENTS(doseMapCT) EQ 0 THEN doseMapCT = doseMap*0.
      IF MAX(doseMap) GT 0 OR MAX(doseMapCT) GT 0 THEN BEGIN
        nDim=SIZE(doseMap,/N_DIMENSIONS)
        IF nDim EQ 2 THEN doseImgNM=doseMap ELSE doseImgNM=TOTAL(doseMap,3)
        nDim=SIZE(doseMapCT,/N_DIMENSIONS)
        IF nDim EQ 2 THEN doseImgCT=doseMapCT ELSE doseImgCT=TOTAL(doseMapCT,3)
        IF N_ELEMENTS(occMap) EQ N_ELEMENTS(doseImgNM) THEN doseImgNM=doseImgNM*occMap
        IF N_ELEMENTS(occMap) EQ N_ELEMENTS(doseImgCT) THEN doseImgCT=doseImgCT*occMap
        IF N_ELEMENTS(WHERE(FINITE(doseImgNM, /NAN))) GT 1 THEN doseImgNM=FLTARR(szim(0),szim(1))
        WIDGET_CONTROL, selDose, GET_VALUE=dose2show
        CASE dose2show OF
          0: doseImg=doseImgNM+doseImgCT
          1: doseImg=doseImgNM
          2: doseImg=doseImgCT
        ENDCASE

        doseDisplay=0.0*doseImg
        above025=WHERE(doseImg GT 0.25)
        IF above025(0) NE -1 THEN BEGIN
          doseDisplay(above025)=64
          above05=WHERE(doseImg GT 0.5)
          IF above05(0) NE -1 THEN BEGIN
            doseDisplay(above05)=128
            above1=WHERE(doseImg GT 1.)
            IF above1(0) NE -1 THEN doseDisplay(above1)=255
          ENDIF
        ENDIF

        dosePal=OBJ_NEW('IDLgrPalette')
        dosePal->LoadCT, 75, File=thisPath+'colorsDose.tbl'
        oDoseMap = OBJ_NEW('IDLgrImage', doseDisplay , BLEND_FUNCTION = [3, 4], ALPHA_CHANNEL=alphaDose, PALETTE=dosePal)
        oModel->Add, oDoseMap
      ENDIF
    ENDIF
    ;ENDIF
  ENDIF

  IF select2overlay EQ 2 THEN BEGIN;doseRateMax
    IF N_ELEMENTS(doseRateMax) GT 0 THEN BEGIN
      IF MAX(doseRateMax) GT 0 THEN BEGIN
        nDim=SIZE(doseRateMax,/N_DIMENSIONS)
        IF nDim EQ 2 THEN doseRateImg=doseRateMax ELSE doseRateImg=TOTAL(doseRateMax,3)

        doseDisplay=0.0*doseRateImg
        above1=WHERE(doseRateImg GT 1.0)
        IF above1(0) NE -1 THEN BEGIN
          doseDisplay(above1)=128
        ENDIF
        above75=WHERE(doseRateImg GT 7.5)
        IF above75(0) NE -1 THEN BEGIN
          doseDisplay(above75)=255
        ENDIF

        doseMaxPal=OBJ_NEW('IDLgrPalette')
        doseMaxPal->LoadCT, 75, File=thisPath+'colorsDose.tbl'
        oDoseMaxMap = OBJ_NEW('IDLgrImage', doseDisplay , BLEND_FUNCTION = [3, 4], ALPHA_CHANNEL=alphaDose, PALETTE=doseMaxPal)
        oModel->Add, oDoseMaxMap
      ENDIF
    ENDIF
  ENDIF

  ;last XY annotation
  IF N_ELEMENTS(lastXY) NE 0 THEN BEGIN
    IF lastXY(0) GE 5 AND lastXY(1) GE 5 AND lastXY(0) LT maxIm(0)*zoomFactor-5 AND lastXY(1) LT  maxIm(1)*zoomFactor-5 THEN BEGIN
      lastLine1=OBJ_NEW('IDLgrPolyline', [[lastXY(0)-5,lastXY(1)-5],[lastXY(0)+5,lastXY(1)+5]], COLOR = 255*([1,0,0]), /DOUBLE, LINESTYLE=0)
      lastLine2=OBJ_NEW('IDLgrPolyline', [[lastXY(0)-5,lastXY(1)+5],[lastXY(0)+5,lastXY(1)-5]], COLOR = 255*([1,0,0]), /DOUBLE, LINESTYLE=0)
      oModel->Add, [lastLine1, lastLine2]
    ENDIF
  ENDIF

  IF select2show(1) EQ 1 THEN BEGIN;size calibration
    IF N_ELEMENTS(startMap) NE 0 THEN BEGIN
      startLine1=OBJ_NEW('IDLgrPolyline', [[startMap(0)-5,startMap(1)-5],[startMap(0)+5,startMap(1)+5]], COLOR = 255*([1,0,0]), /DOUBLE,THICK=2, LINESTYLE=0)
      startLine2=OBJ_NEW('IDLgrPolyline', [[startMap(0)-5,startMap(1)+5],[startMap(0)+5,startMap(1)-5]], COLOR = 255*([1,0,0]), /DOUBLE,THICK=2,  LINESTYLE=0)
      oModel->Add, [startLine1, startLine2]
      IF N_ELEMENTS(endMap) NE 0 THEN BEGIN
        startLine3=OBJ_NEW('IDLgrPolyline', [[startMap(0),startMap(1)],[endMap(0),endMap(1)]], COLOR = 255*([1,0,0]), /DOUBLE, THICK=2,LINESTYLE=0)
        oModel->Add, startLine3
        WIDGET_CONTROL, txtCalib, GET_VALUE=val
        calFlag=OBJ_NEW('IDLgrText', val+' m', LOCATIONS = [startMap(0)+22,0.5*(startMap(1)+endMap(1))-23], COLOR = 255*([1,0,0]))
        oModel->Add, calFlag
      ENDIF
    ENDIF
    IF N_ELEMENTS(endMap) NE 0 THEN BEGIN
      endLine1=OBJ_NEW('IDLgrPolyline', [[endMap(0)-5,endMap(1)-5],[endMap(0)+5,endMap(1)+5]], COLOR = 255*([1,0,0]), /DOUBLE, THICK=2, LINESTYLE=0)
      endLine2=OBJ_NEW('IDLgrPolyline', [[endMap(0)-5,endMap(1)+5],[endMap(0)+5,endMap(1)-5]], COLOR = 255*([1,0,0]), /DOUBLE, THICK=2, LINESTYLE=0)
      oModel->Add, [endLine1, endLine2]
    ENDIF
  ENDIF

  ;length measurements
  IF N_ELEMENTS(startMeasLen) NE 0 THEN BEGIN
    startLine1M=OBJ_NEW('IDLgrPolyline', [[startMeasLen(0)-5,startMeasLen(1)-5],[startMeasLen(0)+5,startMeasLen(1)+5]], COLOR = 255*([1,0,0]), /DOUBLE,THICK=2, LINESTYLE=0)
    startLine2M=OBJ_NEW('IDLgrPolyline', [[startMeasLen(0)-5,startMeasLen(1)+5],[startMeasLen(0)+5,startMeasLen(1)-5]], COLOR = 255*([1,0,0]), /DOUBLE,THICK=2,  LINESTYLE=0)
    oModel->Add, [startLine1M, startLine2M]
    IF N_ELEMENTS(endMeasLen) NE 0 THEN BEGIN
      startLine3=OBJ_NEW('IDLgrPolyline', [[startMeasLen(0),startMeasLen(1)],[endMeasLen(0),endMeasLen(1)]], COLOR = 255*([1,0,0]), /DOUBLE, THICK=2,LINESTYLE=2)
      oModel->Add, startLine3
    ENDIF
  ENDIF
  IF N_ELEMENTS(endMeasLen) NE 0 THEN BEGIN
    endLine1=OBJ_NEW('IDLgrPolyline', [[endMeasLen(0)-5,endMeasLen(1)-5],[endMeasLen(0)+5,endMeasLen(1)+5]], COLOR = 255*([1,0,0]), /DOUBLE, THICK=2, LINESTYLE=0)
    endLine2=OBJ_NEW('IDLgrPolyline', [[endMeasLen(0)-5,endMeasLen(1)+5],[endMeasLen(0)+5,endMeasLen(1)-5]], COLOR = 255*([1,0,0]), /DOUBLE, THICK=2, LINESTYLE=0)
    oModel->Add, [endLine1, endLine2]
  ENDIF

  ;walls
  WIDGET_CONTROL, selFloor, GET_VALUE=selF
  IF select2show(2) + select2show(3) GT 1  AND selF EQ 1 THEN BEGIN;walls - ignored if floor +/-1
    IF N_ELEMENTS(sWalls) NE 0 THEN BEGIN
      ;first all concrete, then all lead
      thicks=!Null
      FOR i=0, N_TAGS(sWalls)-1 DO BEGIN
        IF sWalls.(i).material EQ 1 THEN thicks=[thicks,sWalls.(i).thickness]
      ENDFOR
      IF N_ELEMENTS(thicks) GT 0 THEN thicks=thicks/20.;max(thicks) or user input?
      c=0
      FOR i=0, N_TAGS(sWalls)-1 DO BEGIN
        IF sWalls.(i).material EQ 1 THEN BEGIN
          IF select2show(2) EQ 1 THEN BEGIN
            wallLine=OBJ_NEW('IDLgrPolyline', [[sWalls.(i).pos(0),sWalls.(i).pos(1)],[sWalls.(i).pos(2),sWalls.(i).pos(3)]], THICK=7*thicks(c),  /DOUBLE, LINESTYLE=0);COLOR = 120*[0,1,0],
            oModel->Add, wallLine
            IF sWalls.(i).pos(0) EQ sWalls.(i).pos(2) THEN BEGIN;endmarkes on vertical wall
              wallLine=OBJ_NEW('IDLgrPolyline', [[sWalls.(i).pos(0)-5,sWalls.(i).pos(1)],[sWalls.(i).pos(2)+5,sWalls.(i).pos(1)]], THICK=3,  /DOUBLE, LINESTYLE=0);COLOR = 120*[0,1,0],
              oModel->Add, wallLine
              wallLine=OBJ_NEW('IDLgrPolyline', [[sWalls.(i).pos(0)-5,sWalls.(i).pos(3)],[sWalls.(i).pos(2)+5,sWalls.(i).pos(3)]], THICK=3,  /DOUBLE, LINESTYLE=0);COLOR = 120*[0,1,0],
              oModel->Add, wallLine
            ENDIF ELSE BEGIN;endmarkes on horisontal wall
              wallLine=OBJ_NEW('IDLgrPolyline', [[sWalls.(i).pos(0),sWalls.(i).pos(1)-5],[sWalls.(i).pos(0),sWalls.(i).pos(1)+5]], THICK=3,  /DOUBLE, LINESTYLE=0);COLOR = 120*[0,1,0],
              oModel->Add, wallLine
              wallLine=OBJ_NEW('IDLgrPolyline', [[sWalls.(i).pos(2),sWalls.(i).pos(3)-5],[sWalls.(i).pos(2),sWalls.(i).pos(3)+5]], THICK=3,  /DOUBLE, LINESTYLE=0);COLOR = 120*[0,1,0],
              oModel->Add, wallLine
            ENDELSE
          ENDIF
          IF select2show(3) EQ 1 THEN BEGIN
            IF sWalls.(i).pos(0) EQ sWalls.(i).pos(2) THEN BEGIN; vertical wall
              wallFlag=OBJ_NEW('IDLgrText', STRING(sWalls.(i).thickness, FORMAT='(i0)')+' cm', LOCATIONS = [sWalls.(i).pos(0)+22,0.5*(sWalls.(i).pos(1)+sWalls.(i).pos(3))-20])
              wallFlag->SetProperty, BASELINE=[1,99]
            ENDIF ELSE wallFlag=OBJ_NEW('IDLgrText', STRING(sWalls.(i).thickness, FORMAT='(i0)')+' cm', LOCATIONS = [0.5*(sWalls.(i).pos(0)+sWalls.(i).pos(2))-20,sWalls.(i).pos(1)+5])
            oModel->Add, wallFlag
          ENDIF
          c=c+1
        ENDIF

      ENDFOR

      ;lead
      thicks=!Null
      FOR i=0, N_TAGS(sWalls)-1 DO BEGIN
        IF sWalls.(i).material EQ 0 THEN thicks=[thicks,sWalls.(i).thickness]
      ENDFOR
      IF N_ELEMENTS(thicks) GT 0 THEN thicks=thicks/10.;max(thicks) or user input?
      c=0
      FOR i=0, N_TAGS(sWalls)-1 DO BEGIN
        IF sWalls.(i).material EQ 0 THEN BEGIN

          IF select2show(2) EQ 1 THEN BEGIN
            wallLine=OBJ_NEW('IDLgrPolyline', [[sWalls.(i).pos(0),sWalls.(i).pos(1)],[sWalls.(i).pos(2),sWalls.(i).pos(3)]], THICK=7*thicks(c),  COLOR = 255*[0,0,1],/DOUBLE, LINESTYLE=0);COLOR = 120*[0,1,0],
            oModel->Add, wallLine
            IF sWalls.(i).pos(0) EQ sWalls.(i).pos(2) THEN BEGIN;endmarkes on vertical wall
              wallLine=OBJ_NEW('IDLgrPolyline', [[sWalls.(i).pos(0)-5,sWalls.(i).pos(1)],[sWalls.(i).pos(2)+5,sWalls.(i).pos(1)]], THICK=3, COLOR = 255*[0,0,1], /DOUBLE, LINESTYLE=0);COLOR = 120*[0,1,0],
              oModel->Add, wallLine
              wallLine=OBJ_NEW('IDLgrPolyline', [[sWalls.(i).pos(0)-5,sWalls.(i).pos(3)],[sWalls.(i).pos(2)+5,sWalls.(i).pos(3)]], THICK=3, COLOR = 255*[0,0,1], /DOUBLE, LINESTYLE=0);COLOR = 120*[0,1,0],
              oModel->Add, wallLine
            ENDIF ELSE BEGIN;endmarkes on horisontal wall
              wallLine=OBJ_NEW('IDLgrPolyline', [[sWalls.(i).pos(0),sWalls.(i).pos(1)-5],[sWalls.(i).pos(0),sWalls.(i).pos(1)+5]], THICK=3, COLOR = 255*[0,0,1], /DOUBLE, LINESTYLE=0);COLOR = 120*[0,1,0],
              oModel->Add, wallLine
              wallLine=OBJ_NEW('IDLgrPolyline', [[sWalls.(i).pos(2),sWalls.(i).pos(3)-5],[sWalls.(i).pos(2),sWalls.(i).pos(3)+5]], THICK=3,  COLOR = 255*[0,0,1],/DOUBLE, LINESTYLE=0);COLOR = 120*[0,1,0],
              oModel->Add, wallLine
            ENDELSE
          ENDIF
          IF select2show(3) EQ 1 THEN BEGIN
            IF sWalls.(i).pos(0) EQ sWalls.(i).pos(2) THEN BEGIN; vertical wall
              wallFlag=OBJ_NEW('IDLgrText', STRING(sWalls.(i).thickness*10, FORMAT='(i0)')+' mm', LOCATIONS = [sWalls.(i).pos(0)+22,0.5*(sWalls.(i).pos(1)+sWalls.(i).pos(3))-20],COLOR = 255*[0,0,1])
              wallFlag->SetProperty, BASELINE=[1,99]
            ENDIF ELSE wallFlag=OBJ_NEW('IDLgrText', STRING(sWalls.(i).thickness*10, FORMAT='(i0)')+' mm', LOCATIONS =  [0.5*(sWalls.(i).pos(0)+sWalls.(i).pos(2))-20,sWalls.(i).pos(1)+5],COLOR = 255*[0,0,1])
            oModel->Add, wallFlag
          ENDIF

          c=c+1
        ENDIF

      ENDFOR
    ENDIF
  ENDIF

  ;current wall
  IF MIN(currsWall.pos) GT 0 AND currTab EQ 1 THEN BEGIN
    IF currsWall.pos(0) GE 5 AND currsWall.pos(1) GE 5 AND currsWall.pos(2) LT maxIm(0)*zoomFactor-5 AND currsWall.pos(3) LT  maxIm(1)*zoomFactor-5 THEN BEGIN
      currWallLine=OBJ_NEW('IDLgrPolyline', [[currsWall.pos(0),currsWall.pos(1)],[currsWall.pos(2),currsWall.pos(3)]], COLOR = 255*([1,0,0]), THICK=8, /DOUBLE, LINESTYLE=2)
      oModel->Add, currWallLine
    ENDIF
  ENDIF

  ;current area
  IF MAX(currsArea.pos) GT 0 AND currTab EQ 2 THEN BEGIN
    IF currsArea.pos(0) GE 0 AND currsArea.pos(1) GE 0 AND currsArea.pos(2) LT maxIm(0)*zoomFactor AND currsArea.pos(3) LT  maxIm(1)*zoomFactor THEN BEGIN
      areaLine=OBJ_NEW('IDLgrPolyline', [[currsArea.pos(0),currsArea.pos(1)],[currsArea.pos(0),currsArea.pos(3)],[currsArea.pos(2),currsArea.pos(3)],[currsArea.pos(2),currsArea.pos(1)],[currsArea.pos(0),currsArea.pos(1)]], COLOR = 255*([1,0,0]), THICK=3, /DOUBLE, LINESTYLE=0)
      oModel->Add, areaLine
    ENDIF
  ENDIF

  IF N_ELEMENTS(lastFieldMarked) EQ 4 THEN BEGIN
    fieldIm=INTARR(szim)+1
    fieldIm[lastFieldMarked(0):lastFieldMarked(2),lastFieldMarked(1):lastFieldMarked(3)]=0
    oField = OBJ_NEW('IDLgrImage', 255*fieldIm , BLEND_FUNCTION = [3, 4], ALPHA_CHANNEL=0.5, PALETTE=oPaletteROI)
    oModel->Add, oField
  ENDIF

  ;current Source
  IF MAX(currsSource.pos) GT 0 AND currTab EQ 3 THEN BEGIN
    IF currsSource.pos(0) GE 5 AND currsSource.pos(1) GE 5 AND currsSource.pos(0) LT maxIm(0)*zoomFactor-5 AND currsSource.pos(1) LT  maxIm(1)*zoomFactor-5 THEN BEGIN
      sourceLine1=OBJ_NEW('IDLgrPolyline', [[currsSource.pos(0)-5,currsSource.pos(1)-5],[currsSource.pos(0)+5,currsSource.pos(1)+5]], COLOR = 255*([1,0,0]), THICK=3,/DOUBLE, LINESTYLE=0)
      sourceLine2=OBJ_NEW('IDLgrPolyline', [[currsSource.pos(0)-5,currsSource.pos(1)+5],[currsSource.pos(0)+5,currsSource.pos(1)-5]], COLOR = 255*([1,0,0]), THICK=3,/DOUBLE, LINESTYLE=0)
      oModel->Add, [sourceLine1, sourceLine2]
    ENDIF
  ENDIF

  IF select2show(4) EQ 1 THEN BEGIN;sources
    IF N_ELEMENTS(sSources) NE 0 THEN BEGIN
      FOR i=0, N_TAGS(sSources)-1 DO BEGIN
        IF sSources.(i).pos(0) GE 5 AND sSources.(i).pos(1) GE 5 AND sSources.(i).pos(0) LT maxIm(0)*zoomFactor-5 AND sSources.(i).pos(1) LT  maxIm(1)*zoomFactor-5 THEN BEGIN
          sourceLine1=OBJ_NEW('IDLgrPolyline', [[sSources.(i).pos(0)-5,sSources.(i).pos(1)-5],[sSources.(i).pos(0)+5,sSources.(i).pos(1)+5]], COLOR = 255*([1,0,0]), THICK=2,/DOUBLE, LINESTYLE=0)
          sourceLine2=OBJ_NEW('IDLgrPolyline', [[sSources.(i).pos(0)-5,sSources.(i).pos(1)+5],[sSources.(i).pos(0)+5,sSources.(i).pos(1)-5]], COLOR = 255*([1,0,0]), THICK=2,/DOUBLE, LINESTYLE=0)
          oModel->Add, [sourceLine1, sourceLine2]
        ENDIF
      ENDFOR
    ENDIF
  ENDIF

  ;current Source
  ;  IF MAX(currsSourceCT.pos) GT 0 AND currTab EQ 3 THEN BEGIN
  ;  TODO - change to CT source when this visual is ready
  ;    IF currsSource.pos(0) GE 5 AND currsSource.pos(1) GE 5 AND currsSource.pos(0) LT maxIm(0)*zoomFactor-5 AND currsSource.pos(1) LT  maxIm(1)*zoomFactor-5 THEN BEGIN
  ;      sourceLine1=OBJ_NEW('IDLgrPolyline', [[currsSource.pos(0)-5,currsSource.pos(1)-5],[currsSource.pos(0)+5,currsSource.pos(1)+5]], COLOR = 255*([1,0,0]), THICK=3,/DOUBLE, LINESTYLE=0)
  ;      sourceLine2=OBJ_NEW('IDLgrPolyline', [[currsSource.pos(0)-5,currsSource.pos(1)+5],[currsSource.pos(0)+5,currsSource.pos(1)-5]], COLOR = 255*([1,0,0]), THICK=3,/DOUBLE, LINESTYLE=0)
  ;      oModel->Add, [sourceLine1, sourceLine2]
  ;    ENDIF
  ;  ENDIF

  IF select2show(5) EQ 1 THEN BEGIN; CT sources
    IF N_ELEMENTS(sSourcesCT) NE 0 THEN BEGIN
      FOR i=0, N_TAGS(sSourcesCT)-1 DO BEGIN
        IF sSourcesCT.(i).pos(0) GE 5 AND sSourcesCT.(i).pos(1) GE 5 AND sSourcesCT.(i).pos(0) LT maxIm(0)*zoomFactor-5 AND sSourcesCT.(i).pos(1) LT  maxIm(1)*zoomFactor-5 THEN BEGIN
          cosRot=COS(sSourcesCT.(i).rot/!radeg)
          sinRot=SIN(sSourcesCT.(i).rot/!radeg)
          sourceLineGantry=OBJ_NEW('IDLgrPolyline', [[sSourcesCT.(i).pos(0)-5*cosRot,sSourcesCT.(i).pos(1)-5*sinRot],[sSourcesCT.(i).pos(0)+5*cosRot,sSourcesCT.(i).pos(1)+5*sinRot]], COLOR = 255*([1,0,0]), THICK=2,/DOUBLE, LINESTYLE=0)
          sourceLineTable=OBJ_NEW('IDLgrPolyline', [[sSourcesCT.(i).pos(0),sSourcesCT.(i).pos(1)],[sSourcesCT.(i).pos(0)+10*sinRot,sSourcesCT.(i).pos(1)-10*cosRot]], COLOR = 255*([1,0,0]), THICK=2,/DOUBLE, LINESTYLE=0)
          oModel->Add, [sourceLineGantry, sourceLineTable]
        ENDIF
      ENDFOR
    ENDIF
  ENDIF

  oView->Add,oModel
  iDrawR->Draw,oView

  OBJ_DESTROY, [oView, oModel,oPal]
  IF OBJ_VALID(oIm) THEN OBJ_DESTROY, oIm
  IF OBJ_VALID(lastLine1) THEN OBJ_DESTROY, [lastLine1,lastLine2]
  IF OBJ_VALID(startLine1) THEN OBJ_DESTROY, [startLine1,startLine2]
  IF OBJ_VALID(endLine1) THEN OBJ_DESTROY, [endLine1,endLine2]
  IF OBJ_VALID(oGrid) THEN OBJ_DESTROY, oGrid

end