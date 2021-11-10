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

;replace numbered structure in structure of structures
;numb = id to replace
function replaceStructStruct, fullStruct, newSubStruct, numb, NEW_TAG_NAME=new_tag_name
  structNew=CREATE_STRUCT('EMPTY',0)
  counter=0
  ntags=N_TAGS(fullStruct)
  tagname=TAG_NAMES(fullStruct)
  FOR i=0, ntags-1 DO BEGIN
    stillEmpty=WHERE(TAG_NAMES(structNew) EQ 'EMPTY')
    IF i NE numb THEN BEGIN
      IF stillEmpty(0) EQ -1 THEN structNew=CREATE_STRUCT(structNew,tagname(counter),fullStruct.(i)) ELSE structNew=CREATE_STRUCT(tagname(0),fullStruct.(i))
      counter=counter+1
    ENDIF ELSE BEGIN
      IF N_ELEMENTS(new_tag_name) GT 0 THEN tname=new_tag_name ELSE tname=tagname(counter)
      IF stillEmpty(0) EQ -1 THEN structNew=CREATE_STRUCT(structNew,tname,newSubStruct) ELSE structNew=CREATE_STRUCT(tname,newSubStruct)
      counter=counter+1
    ENDELSE
  ENDFOR
  return, structNew
end

;remove ids from structure of structures
function removeIDstructstruct, struct, ids
  structNew=CREATE_STRUCT('EMPTY',0)
  counter=0
  ntags=N_TAGS(struct)
  tagname=TAG_NAMES(struct)
  cc=0
  FOR i=0, ntags-1 DO BEGIN
    inSel=WHERE(ids EQ i)
    IF inSel(0) EQ -1 THEN BEGIN
      stillEmpty=WHERE(TAG_NAMES(structNew) EQ 'EMPTY')
      IF stillEmpty(0) EQ -1 THEN structNew=CREATE_STRUCT(structNew,tagname(cc),struct.(i)) ELSE structNew=CREATE_STRUCT(tagname(cc),struct.(i))
      cc=cc+1
    ENDIF
  ENDFOR
  return, structNew
end

;assure . not , for float-inputs
function comma2pointFloat, txt
  txt=STRJOIN(STRSPLIT(txt, ',',/EXTRACT),'.')
  return, txt
end

;calculate dose for radioactive source - floor above or below
function addSource2Floor, sourceStruc, sizeMap,cFactor,shieldData,wd,floorThickLead,floorThickCon,floorDist,corrThick
  isoAvail=TAG_NAMES(shieldData)
  idThis=WHERE(isoAvail EQ STRUPCASE(sourceStruc.isotope))
  thisData=shieldData.(idThis(0))
  doseMapThis=FLTARR(sizeMap)
  pos=sourceStruc.pos

  ;distancemap (meters)
  distSource=FLTARR(sizeMap); in pixels
  FOR i=0, sizeMap(0)-1 DO BEGIN
    FOR j=0, sizeMap(1)-1 DO BEGIN
      distSource(i,j)=SQRT((i-pos(0))^2+(j-pos(1))^2)
    ENDFOR
  ENDFOR
  distSource2=(cFactor(0)*distSource)^2+floorDist^2
  distSource=SQRT(distSource2)
  distFactor=1./distSource2

  ;doseconstant - decay and gammaray constant or damping in patient
  doseConst=sourceStruc.a0*exp(-ALOG(2)*sourceStruc.t1/thisData.halflife);start activity * reduction until t1
  IF sourceStruc.inpatient THEN gRconst=thisData.patientConst ELSE gRconst=thisData.gammaRayConst

  doseRateMaxThis=doseConst*gRconst*sourceStruc.restvoid*distFactor

  doseConst=doseConst*sourceStruc.t2*(1./ALOG(2))*thisData.halflife/sourceStruc.t2*(1-exp(-ALOG(2)*sourceStruc.t2/thisData.halflife));  * integral of duration (t2)
  doseConst=doseConst*gRconst*sourceStruc.restvoid*sourceStruc.nPrWorkDay
  doseMapThis=doseConst*0.001*wd(0)*distFactor

  ;calculate B for all pixels in sector based on thickness and isotope
  IF corrThick THEN thickCorr=1./floorDist*distSource ELSE thickCorr=1.;correctionfactor floorthickness in direction from source
  dataNames=TAG_NAMES(thisData)
  ;concrete
  matId=1
  thickn=floorThickCon*thickCorr
  IF dataNames.HasValue('ABG') THEN BEGIN
    abg=thisData.abg[*,matId]
    B=((1+(abg(1)/abg(0)))*exp(abg(0)*abg(2)*thickn)-(abg(1)/abg(0)))^(-1./abg(2))
  ENDIF ELSE BEGIN
    B=0.1^(thickn/thisData.tvl(matId))
  ENDELSE
  doseMapThis=B*doseMapThis
  doseRateMaxThis=B*doseRateMaxThis
  ;lead
  IF floorThickLead GT 0. THEN BEGIN
    matID=0
    thickn=floorThickLead*thickCorr
    IF dataNames.HasValue('ABG') THEN BEGIN
      abg=thisData.abg[*,matId]
      B=((1+(abg(1)/abg(0)))*exp(abg(0)*abg(2)*thickn)-(abg(1)/abg(0)))^(-1./abg(2))
    ENDIF ELSE BEGIN
      B=0.1^(thickn/thisData.tvl(matId))
    ENDELSE
    doseMapThis=B*doseMapThis
    doseRateMaxThis=B*doseRateMaxThis
  ENDIF

  return, CREATE_STRUCT('doseMap',doseMapThis,'maxDoseRate',doseRateMaxThis)
end

;calculate dose for radioactive source
function addSource2Map, sourceStruc, strucWalls, sizeMap, cFactor, shieldData,wd, stdW, corrThick
  isoAvail=TAG_NAMES(shieldData)
  idThis=WHERE(isoAvail EQ STRUPCASE(sourceStruc.isotope))
  thisData=shieldData.(idThis(0))
  doseMapThis=FLTARR(sizeMap)
  pos=sourceStruc.pos
  ;distancemap (meters)
  distSource=FLTARR(sizeMap); in pixels
  FOR i=0, sizeMap(0)-1 DO BEGIN
    FOR j=0, sizeMap(1)-1 DO BEGIN
      distSource(i,j)=SQRT((i-pos(0))^2+(j-pos(1))^2)
    ENDFOR
  ENDFOR
  distSource=cFactor(0)*distSource
  distSource(WHERE(distSource LT 0.5))=.5;ignore closest 50cm to source
  ;1/(distmap^2)
  distFactor=distSource^2
  distFactor=1./distFactor

  ;doseconstant - decay and gammaray constant or damping in patient
  doseConst=sourceStruc.a0*exp(-ALOG(2)*sourceStruc.t1/thisData.halflife);start activity * reduction until t1
  IF sourceStruc.inpatient THEN gRconst=thisData.patientConst ELSE gRconst=thisData.gammaRayConst

  doseRateMaxThis=doseConst*gRconst*sourceStruc.restvoid*distFactor

  doseConst=doseConst*sourceStruc.t2*(1./ALOG(2))*thisData.halflife/sourceStruc.t2*(1-exp(-ALOG(2)*sourceStruc.t2/thisData.halflife));  * integral of duration (t2)
  doseConst=doseConst*gRconst*sourceStruc.restvoid*sourceStruc.nPrWorkDay
  doseMapThis=doseConst*0.001*wd(0)*distFactor

  ;correction for walls - find sector where wall affect the dose and correct B in this area
  wallAffectSector=FLTARR(sizeMap)
  IF N_ELEMENTS(strucWalls) GT 0 THEN BEGIN  ;for each wall
    FOR i=0, N_TAGS(strucWalls)-1 DO BEGIN
      ; horisontal, vertical or oblique wall?
      IF strucWalls.(i).pos(0) EQ strucWalls.(i).pos(2) THEN dir=1 ELSE dir=0; dir 1 = vertical
      ;  define sector affected by wall set to dist else 0
      yposW=[strucWalls.(i).pos(1),strucWalls.(i).pos(3)]
      xposW=[strucWalls.(i).pos(0),strucWalls.(i).pos(2)]

      IF dir EQ 1 THEN BEGIN; vertical
        y1=MIN(yposW) & y2=MAX(yposW)
        xposW=xposW(0)
        IF xposW GT pos(0) THEN xxStartEnd=[CEIL(xposW), sizeMap(0)-1] ELSE xxStartEnd=[0, FLOOR(xposW)]
        FOR xx= xxStartEnd(0), xxStartEnd(1) DO BEGIN
          geomFac=(xx-pos(0))/(xposW(0)-pos(0))
          yy1=ROUND(pos(1))+ROUND((y1-pos(1))*geomFac)
          yy2=ROUND(pos(1))+ROUND((y2-pos(1))*geomFac)
          IF yy1 LT 0 THEN yy1=0
          IF yy2 GT sizeMap(1)-1 THEN yy2=sizeMap(1)-1
          IF yy1 LE sizeMap(1)-1 AND yy2 GE 0 THEN wallAffectSector[xx,yy1:yy2]=distSource[xx,yy1:yy2]/(cFactor(0)*ABS(xx-pos(0)));thickness corr
        ENDFOR
      ENDIF ELSE BEGIN
        x1=MIN(xposW) & x2=MAX(xposW)
        yposW=yposW(0)
        IF yposW GT pos(1) THEN yyStartEnd=[CEIL(yposW), sizeMap(1)-1] ELSE yyStartEnd=[0, FLOOR(yposW)]
        FOR yy= yyStartEnd(0), yyStartEnd(1) DO BEGIN
          geomFac=(yy-pos(1))/(yposW-pos(1))
          xx1=ROUND(pos(0))+ROUND((x1-pos(0))*geomFac)
          xx2=ROUND(pos(0))+ROUND((x2-pos(0))*geomFac)
          IF xx1 LT 0 THEN xx1=0
          IF xx2 GT sizeMap(0)-1 THEN xx2=sizeMap(0)-1
          IF xx1 LE sizeMap(0)-1 AND xx2 GE 0 THEN wallAffectSector[xx1:xx2,yy]=distSource[xx1:xx2,yy]/(cFactor(0)*ABS(yy-pos(1)));thickness corr
        ENDFOR
      ENDELSE

      IF corrThick NE 1 THEN BEGIN
        idinSector=WHERE(wallAffectSector NE 0)
        wallAffectSector(idinSector)=1.
      ENDIF

      ;  calculate B for all pixels in sector based on eff.thickness and isotope
      dataNames=TAG_NAMES(thisData)
      IF strucWalls.(i).std EQ 1 THEN BEGIN
        matId=stdW.material
        thickn=stdW.thickness
      ENDIF ELSE BEGIN
        matId=strucWalls.(i).material
        thickn=strucWalls.(i).thickness
      ENDELSE
      IF dataNames.HasValue('ABG') THEN BEGIN
        abg=thisData.abg[*,matId]
        B=((1+(abg(1)/abg(0)))*exp(abg(0)*abg(2)*thickn*wallAffectSector)-(abg(1)/abg(0)))^(-1./abg(2))
      ENDIF ELSE BEGIN
        B=0.1^(thickn*wallAffectSector/thisData.tvl(matId))
      ENDELSE
      idnoSector=WHERE(wallAffectSector EQ 0)
      B(idnoSector)=1.
      doseMapThis=B*doseMapThis
      doseRateMaxThis=B*doseRateMaxThis

      wallAffectSector=0.*wallAffectSector
    ENDFOR

  ENDIF

  return, CREATE_STRUCT('doseMap',doseMapThis,'maxDoseRate',doseRateMaxThis)
end

;extrapolate horisontal isodose map for sag or cor table (recognized from size of table)
function generateCTmap, CTinputTab_orig
  CTinputTab=ROTATE(CTinputTab_orig,7)
  sz10cm=401;401=20m from iso, 201 = 10m from iso, 10cm resolution
  sz50cm=81;81=20m from iso, 41 = 10m from iso, 50cm resolution
  CTinputTab10cmRes=FLTARR(sz10cm,sz10cm);10m from iso, 10cm resolution
  CTinputTab50cmRes=FLTARR(sz50cm,sz50cm)
  ;check for zeros along edge
  szCTtab=SIZE(CTinputTab, /DIMENSIONS)
  edgeVals=[CTinputTab[*,0],TRANSPOSE(CTinputTab[0,*]),CTinputTab[*,szCTtab(1)-1],TRANSPOSE(CTinputTab[szCTtab(0)-1,*])]
  IF edgeVals.HasValue(0) THEN sv=DIALOG_MESSAGE('Zeros at edge of isodose curve table not accepted. Extrapolation from edge not possible.') ELSE BEGIN
    sz50Half=sz50cm/2
    IF szCTtab(0) EQ 7 AND szCTtab(1) EQ 12 THEN BEGIN;cor
      dx1=sz50Half-3 & dx2=sz50Half+3 & dy1=sz50Half-7 & dy2=sz50Half+4
    ENDIF ELSE BEGIN;sag
      dx1=sz50Half-4 & dx2=sz50Half+7 & dy1=sz50Half-2 & dy2=sz50Half+3
    ENDELSE
    CTinputTab50cmRes[dx1:dx2,dy1:dy2]=CTinputTab
    delta=FINDGEN(sz50cm)*.5-sz50Half*.5
    deltaX=FLTARR(sz50cm,sz50cm)
    FOR i=0, sz50cm-1 DO deltaX[*,i]=delta
    deltaY=ROTATE(deltaX,1)
    distArr2=deltaX^2+deltaY^2
    distArr=SQRT(distArr2)
    angArr=deltaX/distArr;calculate angle indicator for all positions
    qArr=INTARR(sz50cm,sz50cm)+4;set which quadrant where angArr not unique
    idQ1=WHERE(deltaX LT 0 AND deltaY LT 0) & qArr(idQ1)=1
    idQ2=WHERE(deltaX LT 0 AND deltaY GE 0) & qArr(idQ2)=2
    idQ3=WHERE(deltaX GE 0 AND deltaY LT 0) & qArr(idQ3)=3
    edgeArr=INTARR(sz50cm,sz50cm)
    edgeArr[dx1:dx2,dy1]=1 & edgeArr[dx1:dx2,dy2]=1 & edgeArr[dx1,dy1:dy2]=1 & edgeArr[dx2,dy1:dy2]=1
    edgeArr[dx1+1:dx2-1,dy1+1:dy2-1]=-1
    idEdge=WHERE(edgeArr EQ 1)
    distsXedge=deltaX(idEdge)
    distsYedge=deltaY(idEdge)
    edgeDose=CTinputTab50cmRes(idEdge)
    edgeAng=angArr(idEdge)
    edgeQ=qArr(idEdge)
    FOR i=0, sz50cm-1 DO BEGIN
      FOR j=0, sz50cm-1 DO BEGIN
        IF edgeArr(i,j) EQ 0 THEN BEGIN
          ;find edge value with closest direction
          idq=WHERE(edgeQ NE qArr(i,j))
          diff=edgeAng-angArr(i,j)
          diff(idq)=1000
          idmin=WHERE(ABS(diff) EQ MIN(ABS(diff)))
          distEdge2=distsXedge(idmin)^2+distsYedge(idmin)^2
          doseFac=distEdge2/distArr2(i,j)
          CTinputTab50cmRes(i,j)=doseFac*edgeDose(idmin)
        ENDIF
      ENDFOR
    ENDFOR
    sm=SMOOTH(CTinputTab50cmRes,2)
    extrapolids=WHERE(edgeArr EQ 0)
    CTinputTab50cmRes(extrapolids)=sm(extrapolids)
    CTinputTab10cmRes=CONGRID(CTinputTab50cmRes,sz10cm,sz10cm,/CENTER,/INTERP)
  ENDELSE
  return, CTinputTab10cmRes
end

;calculate dose for CT source - floor above or below. If floorDist negative = floor below
function addCTSource2Floor, sourceStruc, sizeMap,cFactor,shieldData,wd,floorThickLead,floorThickCon,floorDist,sag10cmRes, corrThick
  IF sourceStruc.kVp EQ 120 THEN thisData=shieldData.(0) ELSE thisData=shieldData.(1)
  doseMapThis=FLTARR(sizeMap)
  pos=sourceStruc.pos

  whichFloor=1
  IF floorDist LT 0 THEN BEGIN
    whichFloor=-1
    floorDist=ABS(floorDist)
  ENDIF

  ;distancemap (meters)
  distSource=FLTARR(sizeMap); in pixels
  FOR i=0, sizeMap(0)-1 DO BEGIN
    FOR j=0, sizeMap(1)-1 DO BEGIN
      distSource(i,j)=SQRT((i-pos(0))^2+(j-pos(1))^2)
    ENDFOR
  ENDFOR
  distSource2=(cFactor(0)*distSource)^2+floorDist^2
  distSource=SQRT(distSource2)

  szIso10cm=SIZE(sag10cmRes,/DIMENSIONS)
  szIso10cm=szIso10cm(0);assume quadratic
  half10cm=szIso10cm/2
  isodoseMap=FLTARR(szIso10cm,szIso10cm)
  dists=FINDGEN(szIso10cm)*.1-half10cm*.1
  FOR i=0, szIso10cm-1 DO BEGIN
    dist2Floor=SQRT(floorDist^2+((i-half10cm)*0.1)^2); which distance from floor 0 to get profile from sag10cmRes
    diffs=dists-whichFloor*dist2Floor
    yID=WHERE(ABS(diffs) EQ MIN(ABS(diffs)))
    isodoseMap[i,*]=REVERSE(sag10cmRes[*,yID])
  ENDFOR

  isodoseMap=ROT(isodoseMap,-sourceStruc.rot,/INTERP) ;isodose curves rotated

  ;resize to map size and add to map
  newHalfSize=ROUND(.1/cFactor*(szIso10cm-1))/2; m/pix in image / m/pix in isodosemap
  newSize=newHalfSize*2+1
  isodoseMap=CONGRID(isodoseMap,newSize(0),newSize(0),/CENTER,/INTERP)
  x1=ROUND(pos(0))-newHalfSize & x2=ROUND(pos(0))+newHalfSize & y1=ROUND(pos(1))-newHalfSize & y2=ROUND(pos(1))+newHalfSize
  xx1=0 & xx2=newSize-1 & yy1=0 & yy2=newSize-1
  IF x1 LT 0 THEN BEGIN
    xx1=-x1
    x1=0
  ENDIF
  IF x2 GT sizeMap(0)-1 THEN BEGIN
    x2=sizeMap(0)-1
    xx2=xx1+(x2-x1)
  ENDIF
  IF y1 LT 0 THEN BEGIN
    yy1=-y1
    y1=0
  ENDIF
  IF y2 GT sizeMap(1)-1 THEN BEGIN
    y2=sizeMap(1)-1
    yy2=yy1+y2-y1
  ENDIF

  doseMapThis[x1:x2,y1:y2]=isodoseMap[xx1:xx2,yy1:yy2]

  doseMapThis=0.001*wd(0)*sourceStruc.nPrWorkDay*sourceStruc.kVcorr*sourceStruc.mAsPrPat*doseMapThis;uGy to mSv * x workdays to get total dose not day-dose *  patients pr day * mAs Pr Patient * CT isodose

  ;calculate B for all pixels based on eff.thickness and kVp
  IF corrThick THEN thickCorr=1./floorDist*distSource ELSE thickCorr=1.;correctionfactor floorthickness in direction from source
  ;concrete
  matId=1
  thickn=10.*floorThickCon*thickCorr;*10 cm to mm
  abg=thisData[*,matId]
  B=((1+(abg(1)/abg(0)))*exp(abg(0)*abg(2)*thickn)-(abg(1)/abg(0)))^(-1./abg(2))

  doseMapThis=B*doseMapThis

  ;lead
  IF floorThickLead GT 0. THEN BEGIN
    matID=0
    thickn=10.*floorThickLead*thickCorr;*10 cm to mm
    abg=thisData[*,matId]
    B=((1+(abg(1)/abg(0)))*exp(abg(0)*abg(2)*thickn)-(abg(1)/abg(0)))^(-1./abg(2))

    doseMapThis=B*doseMapThis
  ENDIF
  return, doseMapThis
end

;calculate dose for CT source
function addCTSource2Map, sourceStruc, strucWalls, sizeMap, cFactor, shieldData,wd, stdW,CT10cmRes, corrThick
  IF sourceStruc.kVp EQ 120 THEN thisData=shieldData.(0) ELSE thisData=shieldData.(1)
  doseMapThis=FLTARR(sizeMap)
  pos=sourceStruc.pos

  ;distancemap (meters)
  distSource=FLTARR(sizeMap); in pixels
  FOR i=0, sizeMap(0)-1 DO BEGIN
    FOR j=0, sizeMap(1)-1 DO BEGIN
      distSource(i,j)=SQRT((i-pos(0))^2+(j-pos(1))^2)
    ENDFOR
  ENDFOR
  distSource=cFactor(0)*distSource
  distSource(WHERE(distSource LT 0.5))=.5;ignore closest 50cm to source

  isodoseMap=ROT(CT10cmRes,-sourceStruc.rot,/INTERP) ;isodose curves rotated

  ;resize to map size and add to map
  szIso10cm=SIZE(CT10cmRes,/DIMENSIONS)
  szIso10cm=szIso10cm(0);assume quadratic
  newHalfSize=ROUND(.1/cFactor*(szIso10cm-1))/2; m/pix in image / m/pix in isodosemap
  newSize=newHalfSize*2+1
  isodoseMap=CONGRID(isodoseMap,newSize(0),newSize(0),/CENTER,/INTERP)
  x1=ROUND(pos(0))-newHalfSize & x2=ROUND(pos(0))+newHalfSize & y1=ROUND(pos(1))-newHalfSize & y2=ROUND(pos(1))+newHalfSize
  xx1=0 & xx2=newSize-1 & yy1=0 & yy2=newSize-1
  IF x1 LT 0 THEN BEGIN
    xx1=-x1
    x1=0
  ENDIF
  IF x2 GT sizeMap(0)-1 THEN BEGIN
    x2=sizeMap(0)-1
    xx2=xx1+(x2-x1)
  ENDIF
  IF y1 LT 0 THEN BEGIN
    yy1=-y1
    y1=0
  ENDIF
  IF y2 GT sizeMap(1)-1 THEN BEGIN
    y2=sizeMap(1)-1
    yy2=yy1+y2-y1
  ENDIF

  doseMapThis[x1:x2,y1:y2]=isodoseMap[xx1:xx2,yy1:yy2]

  doseMapThis=0.001*wd(0)*sourceStruc.nPrWorkDay*sourceStruc.kVcorr *sourceStruc.mAsPrPat*doseMapThis;uGy to mSv * x workdays to get total dose not day-dose *  patients pr day * mAs Pr Patient * CT isodose

  ;correction for walls - find sector where wall affect the dose and correct B in this area

  wallAffectSector=FLTARR(sizeMap)
  IF N_ELEMENTS(strucWalls) GT 0 THEN BEGIN  ;for each wall
    FOR i=0, N_TAGS(strucWalls)-1 DO BEGIN
      ; horisontal or vertical wall?
      dir='obl'
      IF strucWalls.(i).pos(0) EQ strucWalls.(i).pos(2) THEN dir='vert'
      IF strucWalls.(i).pos(1) EQ strucWalls.(i).pos(3) THEN dir='hori'
      ;  define sector affected by wall set to dist else 0
      yposW=[strucWalls.(i).pos(1),strucWalls.(i).pos(3)]
      xposW=[strucWalls.(i).pos(0),strucWalls.(i).pos(2)]

      Case dir OF
        'vert': BEGIN; vertical
          y1=MIN(yposW) & y2=MAX(yposW)
          xposW=xposW(0)
          IF xposW GT pos(0) THEN xxStartEnd=[CEIL(xposW), sizeMap(0)-1] ELSE xxStartEnd=[0, FLOOR(xposW)]
          FOR xx= xxStartEnd(0), xxStartEnd(1) DO BEGIN
            geomFac=(xx-pos(0))/(xposW(0)-pos(0))
            yy1=ROUND(pos(1))+ROUND((y1-pos(1))*geomFac)
            yy2=ROUND(pos(1))+ROUND((y2-pos(1))*geomFac)
            IF yy1 LT 0 THEN yy1=0
            IF yy2 GT sizeMap(1)-1 THEN yy2=sizeMap(1)-1
            IF yy1 LE sizeMap(1)-1 AND yy2 GE 0 THEN wallAffectSector[xx,yy1:yy2]=distSource[xx,yy1:yy2]/(cFactor(0)*ABS(xx-pos(0)));thickness corr
          ENDFOR
        END
        'hori': BEGIN
          x1=MIN(xposW) & x2=MAX(xposW)
          yposW=yposW(0)
          IF yposW GT pos(1) THEN yyStartEnd=[CEIL(yposW), sizeMap(1)-1] ELSE yyStartEnd=[0, FLOOR(yposW)]
          FOR yy= yyStartEnd(0), yyStartEnd(1) DO BEGIN
            geomFac=(yy-pos(1))/(yposW-pos(1))
            xx1=ROUND(pos(0))+ROUND((x1-pos(0))*geomFac)
            xx2=ROUND(pos(0))+ROUND((x2-pos(0))*geomFac)
            IF xx1 LT 0 THEN xx1=0
            IF xx2 GT sizeMap(0)-1 THEN xx2=sizeMap(0)-1
            IF xx1 LE sizeMap(0)-1 AND xx2 GE 0 THEN wallAffectSector[xx1:xx2,yy]=distSource[xx1:xx2,yy]/(cFactor(0)*ABS(yy-pos(1)));thickness corr
          ENDFOR
        END
        'obl':BEGIN
          ;        wallAffectSector=calcWallAffectSector(sizeMap, pos, strucWalls.(i).pos); 1 inside, 0 outside as thickness correction is ignored
        END
        ELSE:
      ENDCASE

      ;  calculate B for all pixels in sector based on eff.thickness and isotope
      IF strucWalls.(i).std EQ 1 THEN BEGIN
        matId=stdW.material
        thickn=stdW.thickness
      ENDIF ELSE BEGIN
        matId=strucWalls.(i).material
        thickn=strucWalls.(i).thickness
      ENDELSE

      thickn=10.*thickn; cm to mm as alpha/beta/gamma factors given in mm-1

      IF corrThick NE 1 THEN BEGIN
        idinSector=WHERE(wallAffectSector NE 0)
        wallAffectSector(idinSector)=1.
      ENDIF

      abg=thisData[*,matId]
      B=((1+(abg(1)/abg(0)))*exp(abg(0)*abg(2)*thickn*wallAffectSector)-(abg(1)/abg(0)))^(-1./abg(2))

      idnoSector=WHERE(wallAffectSector EQ 0)
      B(idnoSector)=1.
      doseMapThis=B*doseMapThis

      wallAffectSector=0.*wallAffectSector
    ENDFOR

  ENDIF
  return, doseMapThis
end