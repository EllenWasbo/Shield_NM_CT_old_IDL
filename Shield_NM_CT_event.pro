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

pro Shield_NM_CT_event, event

  COMMON SHNM

  WIDGET_CONTROL, event.ID, GET_UVALUE=uval

  IF N_ELEMENTS(uval) GT 0 THEN BEGIN

    CASE uval OF

      'Exit': BEGIN
        sv=DIALOG_MESSAGE('Are you sure you have saved what you need?',/QUESTION)
        IF sv EQ 'Yes' THEN WIDGET_CONTROL, event.top, /DESTROY
        END
      'clearall': clearAll
      'loadMap':BEGIN
        adr=DIALOG_PICKFILE(TITLE='Select .png file to open.', /READ,FILTER='*.png',/FIX_FILTER)
        IF adr(0) NE '' THEN BEGIN
          readNewImg, adr
          updateVisuals, 'all'
        ENDIF
      END

      'save':BEGIN
        adr=DIALOG_PICKFILE(TITLE='Save current image and tables as .dat file.', /WRITE, FILTER='*.dat',/FIX_FILTER)
        IF adr NE '' THEN BEGIN
          IF STRMID(adr,2,/REVERSE_OFFSET) NE 'dat' THEN adr=adr+'.dat'

          IF N_ELEMENTS(startMap) NE 0 AND N_ELEMENTS(endMap) NE 0 THEN BEGIN
            WIDGET_CONTROL, txtCalib, GET_VALUE=lenCalib
            WIDGET_CONTROL, txtH0, GET_VALUE=H0
            WIDGET_CONTROL, txtH1, GET_VALUE=H1
            WIDGET_CONTROL, txtConCeil, GET_VALUE=ConCeil
            WIDGET_CONTROL, txtLeadCeil, GET_VALUE=LeadCeil
            WIDGET_CONTROL, txtConFloor, GET_VALUE=ConFloor
            WIDGET_CONTROL, txtLeadFloor, GET_VALUE=LeadFloor
            SAVE, im,sWalls,sAreas,sSources,sSourcesCT,startMap,endMap,lenCalib,workdays, stdWall, sagTab,corTab, H0,H1,ConCeil,LeadCeil,ConFloor,LeadFloor,FILENAME=adr
          ENDIF ELSE SAVE, im,sWalls,sAreas,sSources,sSourcesCT, workdays,stdWall,sagTab,corTab,FILENAME=adr
        ENDIF
      END

      'saveimg':BEGIN
        adr=DIALOG_PICKFILE(TITLE='Save current visualization as png-image.', /WRITE, FILTER='*.png',/FIX_FILTER)
        IF adr NE '' THEN BEGIN
          IF STRMID(adr,2,/REVERSE_OFFSET) NE 'png' THEN adr=adr+'.png'
          WIDGET_CONTROL, drawRooms, GET_VALUE = iDrawR
          myimage = iDrawR->Read()
          myimage->GetProperty, DATA=currimage
          WRITE_PNG, adr, currimage
        ENDIF
      END

      'crop':BEGIN
        IF ARRAY_EQUAL(zoomIn,[0.,0.,1.,1.]) THEN BEGIN
          If lastFieldMarked(0) NE -1 THEN BEGIN
            pos=lastFieldMarked
            lastFieldMarked=[-1,-1,-1,-1]

            adr=DIALOG_PICKFILE(TITLE='Save selected area as .dat file. Sources, walls and areas outside the area will be removed. Please Cancel to save current setup if needed.', /WRITE, FILTER='*.dat',/FIX_FILTER)
            IF adr NE '' THEN BEGIN
              IF STRMID(adr,2,/REVERSE_OFFSET) NE 'dat' THEN adr=adr+'.dat'

              deltaX=pos(0)
              deltaY=pos(1)
              maxX=pos(2)
              maxY=pos(3)
              subtrArr=[pos(0),pos(1),pos(0),pos(1)]

              im=im[deltaX:maxX,deltaY:maxY]
              newWalls=!Null
              FOR i=0, N_TAGS(sWalls)- 1 DO BEGIN
                ;remove if outside, change x,y if inside
                curr=sWalls.(i)
                currPos=curr.pos
                IF currPos(0) LT maxX AND currPos(1) LT maxY AND currPos(2) GT deltaX AND currPos(3) GT deltaY THEN BEGIN;inside or partly inside
                  IF currPos(2) GT maxX THEN currPos(2) = maxX
                  IF currPos(3) GT maxY THEN currPos(3) = maxY
                  currPos=currPos-subtrArr
                  ltZero=WHERE(currPos LT 0)
                  IF ltZero(0) NE -1 THEN currPos(ltZero)=0
                  curr.pos=currPos
                  IF SIZE(newWalls,/TNAME) EQ 'STRUCT' THEN nAlready=N_TAGS(newWalls) ELSE nAlready=0
                  newWalls=CREATE_STRUCT(newWalls,'W'+STRING(nAlready,FORMAT='(i2)'),curr)
                ENDIF
              ENDFOR
              sWalls=newWalls

              newAreas=!Null
              FOR i=0, N_TAGS(sAreas)- 1 DO BEGIN
                ;remove if outside, change x,y if inside
                curr=sAreas.(i)
                currPos=curr.pos
                IF currPos(0) LT maxX AND currPos(1) LT maxY AND currPos(2) GT deltaX AND currPos(3) GT deltaY THEN BEGIN;inside or partly inside
                  IF currPos(2) GT maxX THEN currPos(2) = maxX
                  IF currPos(3) GT maxY THEN currPos(3) = maxY
                  currPos=currPos-subtrArr
                  ltZero=WHERE(currPos LT 0)
                  IF ltZero(0) NE -1 THEN currPos(ltZero)=0
                  curr.pos=currPos
                  IF SIZE(newAreas,/TNAME) EQ 'STRUCT' THEN nAlready=N_TAGS(newAreas) ELSE nAlready=0
                  newAreas=CREATE_STRUCT(newAreas,'A'+STRING(nAlready,FORMAT='(i2)'),curr)
                ENDIF
              ENDFOR
              sAreas=newAreas

              newSources=!Null
              FOR i=0, N_TAGS(sSources)- 1 DO BEGIN
                ;remove if outside, change x,y if inside
                curr=sSources.(i)
                currPos=curr.pos
                IF currPos(0) LT maxX AND currPos(1) LT maxY AND currPos(0) GT deltaX AND currPos(1) GT deltaY THEN BEGIN;inside
                  curr.pos=currPos-subtrArr[0:1]
                  IF SIZE(newSources,/TNAME) EQ 'STRUCT' THEN nAlready=N_TAGS(newSources) ELSE nAlready=0
                  newSources=CREATE_STRUCT(newSources,'S'+STRING(nAlready,FORMAT='(i2)'),curr)
                ENDIF
              ENDFOR
              sSources=newSources

              newSourcesCT=!Null
              FOR i=0, N_TAGS(sSourcesCT)- 1 DO BEGIN
                ;remove if outside, change x,y if inside
                curr=sSourcesCT.(i)
                currPos=curr.pos
                IF currPos(0) LT maxX AND currPos(1) LT maxY AND currPos(0) GT deltaX AND currPos(1) GT deltaY THEN BEGIN;inside
                  curr.pos=currPos-subtrArr[0:1]
                  IF SIZE(newSourcesCT,/TNAME) EQ 'STRUCT' THEN nAlready=N_TAGS(newSourcesCT) ELSE nAlready=0
                  newSourcesCT=CREATE_STRUCT(newSourcesCT,'SCT'+STRING(nAlready,FORMAT='(i2)'),curr)
                ENDIF
              ENDFOR
              sSourcesCT=newSourcesCT

              IF N_ELEMENTS(startMap) NE 0 AND N_ELEMENTS(endMap) NE 0 THEN BEGIN
                WIDGET_CONTROL, txtCalib, GET_VALUE=lenCalib
                ;TODO: if lenCalib fits inside im change xy if needed, else reduce and change lenCalib
                startMap=startMap-subtrArr[0:1]
                endMap=endMap-subtrArr[0:1]
                WIDGET_CONTROL, txtH0, GET_VALUE=H0
                WIDGET_CONTROL, txtH1, GET_VALUE=H1
                WIDGET_CONTROL, txtConCeil, GET_VALUE=ConCeil
                WIDGET_CONTROL, txtLeadCeil, GET_VALUE=LeadCeil
                WIDGET_CONTROL, txtConFloor, GET_VALUE=ConFloor
                WIDGET_CONTROL, txtLeadFloor, GET_VALUE=LeadFloor
                SAVE, im,sWalls,sAreas,sSources,sSourcesCT,startMap,endMap,lenCalib,workdays, stdWall, sagTab,corTab, H0,H1,ConCeil,LeadCeil,ConFloor,LeadFloor,FILENAME=adr
              ENDIF ELSE SAVE, im,sWalls,sAreas,sSources,sSourcesCT, workdays,stdWall,sagTab,corTab,FILENAME=adr

              clearAll
              WIDGET_CONTROL, /HOURGLASS
              RESTORE,adr

              IF N_ELEMENTS(im) GT 0 THEN szim=SIZE(im,/DIMENSIONS)
              IF N_ELEMENTS(startMap) NE 0 THEN BEGIN
                WIDGET_CONTROL, lblStartPos, SET_VALUE=STRING(startMap(0),FORMAT='(i0)')+','+STRING(startMap(1),FORMAT='(i0)')
                WIDGET_CONTROL, lblEndPos, SET_VALUE=STRING(endMap(0),FORMAT='(i0)')+','+STRING(endMap(1),FORMAT='(i0)')
                IF N_ELEMENTS(startMap) NE 0 AND N_ELEMENTS(endMap) NE 0 AND N_ELEMENTS(lenCalib) NE 0 THEN BEGIN
                  lenCalibPix=SQRT((endMap(0)-startMap(0))^2+(endMap(1)-startMap(1))^2)
                  calibFactor=FLOAT(lenCalib)/lenCalibPix
                ENDIF
                WIDGET_CONTROL, txtCalib,SET_VALUE=STRING(lenCalib,FORMAT='(f0)')
              ENDIF
              WIDGET_CONTROL, txtWorkDaysPrYear,SET_VALUE=STRING(workdays,FORMAT='(i0)')
              WIDGET_CONTROL, txtH0, SET_VALUE=H0
              WIDGET_CONTROL, txtH1, SET_VALUE=H1
              WIDGET_CONTROL, txtConCeil, SET_VALUE=ConCeil
              WIDGET_CONTROL, txtLeadCeil, SET_VALUE=LeadCeil
              WIDGET_CONTROL, txtConFloor, SET_VALUE=ConFloor
              WIDGET_CONTROL, txtLeadFloor, SET_VALUE=LeadFloor
              IF N_ELEMENTS(sagTab) GT 0 THEN WIDGET_CONTROL, tCTsag, SET_VALUE=STRING(sagTab)
              IF N_ELEMENTS(corTab) GT 0 THEN WIDGET_CONTROL, tCTcor, SET_VALUE=STRING(corTab)
              updateVisuals, 'all'
            ENDIF
          ENDIF ELSE sv=DIALOG_MESSAGE('No area selected to crop. Right mouseclick and drag.')
        ENDIF ELSE sv=DIALOG_MESSAGE('Zoom out before selecting the crop area.')
      END

      'open': BEGIN
        adr=DIALOG_PICKFILE(TITLE='Open dataset from .dat file.', /READ, FILTER='*.dat',/FIX_FILTER)
        IF adr NE '' THEN BEGIN

          clearAll
          WIDGET_CONTROL, /HOURGLASS
          RESTORE,adr
          ;FOR i=0, N_TAGS(sSources)-1 DO sSources=replaceStructStruct(sSources, CREATE_STRUCT(sSources.(i),'restvoid',1.0), i)
          IF N_ELEMENTS(im) GT 0 THEN szim=SIZE(im,/DIMENSIONS)
          IF N_ELEMENTS(startMap) NE 0 THEN BEGIN
            WIDGET_CONTROL, lblStartPos, SET_VALUE=STRING(startMap(0),FORMAT='(i0)')+','+STRING(startMap(1),FORMAT='(i0)')
            WIDGET_CONTROL, lblEndPos, SET_VALUE=STRING(endMap(0),FORMAT='(i0)')+','+STRING(endMap(1),FORMAT='(i0)')
            IF N_ELEMENTS(startMap) NE 0 AND N_ELEMENTS(endMap) NE 0 AND N_ELEMENTS(lenCalib) NE 0 THEN BEGIN
              lenCalibPix=SQRT((endMap(0)-startMap(0))^2+(endMap(1)-startMap(1))^2)
              calibFactor=FLOAT(lenCalib)/lenCalibPix           
            ENDIF
            IF N_ELEMENTS(lenCalib) NE 0 THEN WIDGET_CONTROL, txtCalib,SET_VALUE=STRING(lenCalib,FORMAT='(f0.3)')
          ENDIF
          WIDGET_CONTROL, txtWorkDaysPrYear,SET_VALUE=STRING(workdays,FORMAT='(i0)')
          WIDGET_CONTROL, txtH0, SET_VALUE=H0
          WIDGET_CONTROL, txtH1, SET_VALUE=H1
          WIDGET_CONTROL, txtConCeil, SET_VALUE=ConCeil
          WIDGET_CONTROL, txtLeadCeil, SET_VALUE=LeadCeil
          WIDGET_CONTROL, txtConFloor, SET_VALUE=ConFloor
          WIDGET_CONTROL, txtLeadFloor, SET_VALUE=LeadFloor
          IF N_ELEMENTS(sagTab) GT 0 THEN WIDGET_CONTROL, tCTsag, SET_VALUE=STRING(sagTab)
          IF N_ELEMENTS(corTab) GT 0 THEN WIDGET_CONTROL, tCTcor, SET_VALUE=STRING(corTab)
          updateVisuals, 'all'
        ENDIF
      END

      'zoomOut':BEGIN
        zoomIn=[0.,0.,1.,1.]
        redrawrooms
      END
      'getPosZ':BEGIN
        pos=lastFieldMarked
        lastFieldMarked=[-1,-1,-1,-1]
        zoomIn[0:1]=pos[0:1]
        zoomIn(2)=(pos(2)-pos(0))/maxIm(0)
        ;zoomIn(3)=(pos(3)-pos(1))/maxIm(1)
        redrawrooms
      END
      'slideImg':BEGIN
        WIDGET_CONTROL, slideImg, GET_VALUE=alphaImg100
        alphaImg=0.01*FLOAT(alphaImg100)
        redrawrooms
      END
      'slideOcc':BEGIN
        WIDGET_CONTROL, slideOcc, GET_VALUE=alphaOcc100
        alphaOcc=0.01*FLOAT(alphaOcc100)
        redrawrooms
      END
      'slideDose':BEGIN
        WIDGET_CONTROL, slideDose, GET_VALUE=alphaDose100
        alphaDose=0.01*FLOAT(alphaDose100)
        redrawrooms
      END
      'startMap': BEGIN
        startMap=lastXY        
        WIDGET_CONTROL, lblStartPos, SET_VALUE=STRING(lastXY(0),FORMAT='(i0)')+','+STRING(lastXY(1),FORMAT='(i0)')
        WIDGET_CONTROL, txtCalib, GET_VALUE=val
        IF N_ELEMENTS(startMap) NE 0 AND N_ELEMENTS(endMap) NE 0 THEN BEGIN
          lenCalib=SQRT((endMap(0)-startMap(0))^2+(endMap(1)-startMap(1))^2)
          calibFactor=FLOAT(val(0))/lenCalib
          updateVisuals,'overlay'
        ENDIF
      END
      'endMap':BEGIN
        endMap=lastXY
        WIDGET_CONTROL, lblEndPos, SET_VALUE=STRING(lastXY(0),FORMAT='(i0)')+','+STRING(lastXY(1),FORMAT='(i0)')
        WIDGET_CONTROL, txtCalib, GET_VALUE=val
        IF N_ELEMENTS(startMap) NE 0 AND N_ELEMENTS(endMap) NE 0 THEN BEGIN
          lenCalib=SQRT((endMap(0)-startMap(0))^2+(endMap(1)-startMap(1))^2)
          calibFactor=FLOAT(val(0))/lenCalib
          updateVisuals,'overlay'
        ENDIF
      END
      'startMeasLen': BEGIN
        startMeasLen=lastXY
        WIDGET_CONTROL, lblStartMeasLen, SET_VALUE=STRING(lastXY(0),FORMAT='(i0)')+','+STRING(lastXY(1),FORMAT='(i0)')
        IF N_ELEMENTS(endMeasLen) NE 0 THEN BEGIN
          IF N_ELEMENTS(calibFactor) NE 0 THEN BEGIN
            lenPix=SQRT((endMeasLen(0)-startMeasLen(0))^2+(endMeasLen(1)-startMeasLen(1))^2)
            lenM=lenPix*calibFactor
            WIDGET_CONTROL, txtMeasLen, SET_VALUE=STRING(lenM,FORMAT='(f0.4)')
            redrawrooms
          ENDIF
        ENDIF
      END
      'endMeasLen':BEGIN
        endMeasLen=lastXY
        WIDGET_CONTROL, lblEndMeasLen, SET_VALUE=STRING(lastXY(0),FORMAT='(i0)')+','+STRING(lastXY(1),FORMAT='(i0)')
        IF N_ELEMENTS(startMeasLen) NE 0 THEN BEGIN
          IF N_ELEMENTS(calibFactor) NE 0 THEN BEGIN
            lenPix=SQRT((endMeasLen(0)-startMeasLen(0))^2+(endMeasLen(1)-startMeasLen(1))^2)
            lenM=lenPix*calibFactor
            WIDGET_CONTROL, txtMeasLen, SET_VALUE=STRING(lenM,FORMAT='(f0.4)')
            redrawrooms
          ENDIF
        ENDIF
      END
      'measLenClear':BEGIN
        startMeasLen=!Null
        endMeasLen=!Null
        WIDGET_CONTROL, lblEndMeasLen, SET_VALUE='-,-'
        WIDGET_CONTROL, lblStartMeasLen, SET_VALUE='-,-'
        WIDGET_CONTROL, txtMeasLen, SET_VALUE='-'
        redrawrooms
      END
      'selShow':redrawrooms
      'selOverlay':BEGIN
        IF event.select THEN BEGIN
          WIDGET_CONTROL, selOverlay, GET_VALUE=select2overlay
          CASE select2overlay OF
            0: BEGIN
              currColor=thispath+'images\colorBar_occ.bmp'
              redrawrooms
              END
            1: BEGIN
              currColor=thispath+'images\colorBar_mSv.bmp'
              updateVisuals, 'overlay'
              END
            2: BEGIN
              currColor=thispath+'images\colorBar_doserate.bmp'
              updateVisuals, 'overlay'
              END
            ELSE: 
          ENDCASE      
          WIDGET_CONTROL, btnColor, SET_VALUE=currColor, /BITMAP
        ENDIF
      END
      'selDose':redrawrooms
      'selFloor':IF event.select THEN updateVisuals, 'overlay'
      'corrThick':updateVisuals, 'overlay'
      ;'refresh':updateVisuals, 'all'

      'addW':BEGIN
        WIDGET_CONTROL, txtEditIdW,GET_VALUE=id
        WIDGET_CONTROL, corrAlign, GET_VALUE=keepDir
        WIDGET_CONTROL, txtEditPosW, GET_VALUE=pos
        WIDGET_CONTROL, txtEditThickW, GET_VALUE=thick
        selMat=WIDGET_INFO(lstEditMatW, /DROPLIST_SELECT)
        IF STRLEN(id) GT 16 THEN id=STRING(id,FORMAT='(a16)')
        currsWall.id=id
        posArr=FLOAT(STRSPLIT(pos,',',/EXTRACT))
        IF N_ELEMENTS(posArr) EQ 4 THEN BEGIN
          IF keepDir EQ 0 THEN BEGIN
            diffX=posArr(2)-posArr(0)
            diffY=posArr(3)-posArr(1)
            IF ABS(diffX) LT ABS(diffY) THEN BEGIN
              posArr(0)=0.5*(posArr(0)+posArr(2))
              posArr(2)=posArr(0)
              IF posArr(3) LT posArr(1) THEN BEGIN
                oldPos1=posArr(1)
                posArr(1)=posArr(3)
                posArr(3)=oldPos1
              ENDIF
            ENDIF ELSE BEGIN
              posArr(1)=0.5*(posArr(1)+posArr(3))
              posArr(3)=posArr(1)
              IF posArr(2) LT posArr(0) THEN BEGIN
                oldPos0=posArr(0)
                posArr(0)=posArr(2)
                posArr(2)=oldPos0
              ENDIF
            ENDELSE
          ENDIF
          currsWall.pos=posArr
        ENDIF
        stdUsed=WIDGET_INFO(btnUseStdW, /BUTTON_SET)
        IF stdUsed THEN BEGIN
          currsWall.material=stdWall.material
          currsWall.thickness=stdWall.thickness
          currsWall.std=1
        ENDIF ELSE BEGIN
          currsWall.material=selMat
          currsWall.thickness=FLOAT(thick(0))
        ENDELSE
        IF SIZE(sWalls,/TNAME) EQ 'STRUCT' THEN nAlready=N_TAGS(sWalls) ELSE nAlready=0
        sWalls=CREATE_STRUCT(sWalls,'W'+STRING(nAlready,FORMAT='(i2)'),currsWall)
        selW=nAlready
        updateVisuals,'all'
      END
      'editW':BEGIN
        IF SIZE(sWalls,/TNAME) EQ 'STRUCT' AND selW GE 0 THEN BEGIN
          WIDGET_CONTROL, txtEditIdW,GET_VALUE=id
          WIDGET_CONTROL, corrAlign, GET_VALUE=keepDir
          WIDGET_CONTROL, txtEditPosW, GET_VALUE=pos
          WIDGET_CONTROL, txtEditThickW, GET_VALUE=thick
          selMat=WIDGET_INFO(lstEditMatW, /DROPLIST_SELECT)
          IF STRLEN(id) GT 16 THEN id=STRING(id,FORMAT='(a16)')
          currsWall.id=id
          posArr=FLOAT(STRSPLIT(pos,',',/EXTRACT))
          IF N_ELEMENTS(posArr) EQ 4 THEN BEGIN
            IF keepDir EQ 0 THEN BEGIN
              diffX=posArr(2)-posArr(0)
              diffY=posArr(3)-posArr(1)
              IF ABS(diffX) LT ABS(diffY) THEN BEGIN
                posArr(0)=0.5*(posArr(0)+posArr(2))
                posArr(2)=posArr(0)
                IF posArr(3) LT posArr(1) THEN BEGIN
                  oldPos1=posArr(1)
                  posArr(1)=posArr(3)
                  posArr(3)=oldPos1
                ENDIF
              ENDIF ELSE BEGIN
                posArr(1)=0.5*(posArr(1)+posArr(3))
                posArr(3)=posArr(1)
                IF posArr(2) LT posArr(0) THEN BEGIN
                  oldPos0=posArr(0)
                  posArr(0)=posArr(2)
                  posArr(2)=oldPos0
                ENDIF
              ENDELSE
            ENDIF
            currsWall.pos=posArr
          ENDIF
          stdUsed=WIDGET_INFO(btnUseStdW, /BUTTON_SET)
          IF stdUsed THEN BEGIN
            currsWall.material=stdWall.material
            currsWall.thickness=stdWall.thickness
            currsWall.std=1
          ENDIF ELSE BEGIN
            currsWall.material=selMat
            currsWall.thickness=FLOAT(thick(0))
            currsWall.std=0
          ENDELSE
          sWalls=replaceStructStruct(sWalls,currsWall,selW)
          updateVisuals,'all'
        ENDIF ELSE sv=DIALOG_MESSAGE('No wall in list to edit yet.')
      END
      'deleteW':BEGIN
        IF SIZE(sWalls,/TNAME) EQ 'STRUCT' AND selW GE 0 THEN BEGIN
          IF N_TAGS(sWalls) EQ 1 THEN BEGIN
            sWalls=!Null
            selW=-1
          ENDIF ELSE BEGIN
            sWalls=removeIDstructstruct(sWalls, selW)
            selW=0
          ENDELSE
          updateVisuals,'all'
        ENDIF
      END
      'copyW':BEGIN
        WIDGET_CONTROL, lstWalls, GET_VALUE=currTable
        CLIPBOARD.set, STRJOIN(currTable, STRING(9B))
      END
      'importW':BEGIN
        clipres=CLIPBOARD.GET()
        nRows=N_ELEMENTS(clipRes)
        err=0
        sWalls=!Null
        FOR r=0, nRows-1 DO BEGIN
          rowArr=STRSPLIT(clipRes(r),STRING(9B),/EXTRACT)
          IF N_ELEMENTS(rowArr) GE 4 THEN BEGIN
            id=rowArr(0)
            pos=FLOAT(STRSPLIT(rowArr(1),',',/EXTRACT))
            IF N_ELEMENTS(pos) NE 4 THEN pos=[0.,0.,0.,0.]  ;TODO check if pos is outside map
            IF N_ELEMENTS(rowArr) EQ 5 THEN BEGIN
              IF rowArr(4) EQ 'X' THEN stdUsed=1 ELSE stdUsed=0
            ENDIF ELSE stdUsed=0
            IF stdUsed THEN BEGIN
              thick=stdWall.thickness
              idMat=stdWall.material
            ENDIF ELSE BEGIN
              thick=FLOAT(rowArr(2))
              idMat=WHERE(rowArr(3) EQ materialList)
              IF idMat(0) EQ -1 THEN idMat=0
            ENDELSE
            thisWall=CREATE_STRUCT('id',id,'pos',pos,'thickness',thick,'material',idMat(0),'std',stdUsed)
            sWalls=CREATE_STRUCT(sWalls,'W'+STRING(r,FORMAT='(i2)'),thisWall)
          ENDIF ELSE BEGIN
            IF N_ELEMENTS(rowArr) NE 1 AND rowArr(0) NE '' THEN err=err+1
          ENDELSE
        ENDFOR
        IF err GT 0 THEN sv=DIALOG_MESSAGE('Found '+STRING(err,FORMAT='(i0)')+' rows not containing elements as expected. These will be ignored.')
        updateVisuals,'all'
      END
      'startWall': BEGIN
        currsWall.pos[0:1]=lastXY
        WIDGET_CONTROL, txtEditPosW, SET_VALUE=STRJOIN(STRING(currsWall.pos,FORMAT='(i0)'),', ')
        redrawrooms
      END
      'endWall':BEGIN
        currsWall.pos[2:3]=lastXY
        WIDGET_CONTROL, txtEditPosW, SET_VALUE=STRJOIN(STRING(currsWall.pos,FORMAT='(i0)'),', ')
        redrawrooms
      END
      'setStdWall':BEGIN
        WIDGET_CONTROL, txtEditThickW, GET_VALUE=thick
        selMat=WIDGET_INFO(lstEditMatW, /DROPLIST_SELECT)
        stdWall.thickness=FLOAT(thick(0))
        stdWall.material=selMat
        WIDGET_CONTROL, lblStdWthick, SET_VALUE=STRING(stdWall.thickness,FORMAT='(f0.2)')
        WIDGET_CONTROL, lblStdWmat,SET_VALUE=materialList(stdWall.material)
        updateVisuals,'all'
      END

      'addA':BEGIN
        WIDGET_CONTROL, txtEditIdA,GET_VALUE=id
        WIDGET_CONTROL, txtEditPosA, GET_VALUE=pos
        WIDGET_CONTROL, txtEditOccA, GET_VALUE=occval
        IF STRLEN(id) GT 16 THEN id=STRING(id,FORMAT='(a16)')
        currsArea.id=id
        posArr=FLOAT(STRSPLIT(pos,',',/EXTRACT))
        IF N_ELEMENTS(posArr) EQ 4 THEN currsArea.pos=posArr
        currsArea.occ=FLOAT(occval(0))
        IF SIZE(sAreas,/TNAME) EQ 'STRUCT' THEN nAlready=N_TAGS(sAreas) ELSE nAlready=0
        sAreas=CREATE_STRUCT(sAreas,'A'+STRING(nAlready,FORMAT='(i2)'),currsArea)
        selA=nAlready
        updateVisuals,'A'
      END
      'editA':BEGIN
        IF SIZE(sAreas,/TNAME) EQ 'STRUCT' AND selA GE 0 THEN BEGIN
          WIDGET_CONTROL, txtEditIdA,GET_VALUE=id
          WIDGET_CONTROL, txtEditPosA, GET_VALUE=pos
          WIDGET_CONTROL, txtEditOccA, GET_VALUE=occval
          IF STRLEN(id) GT 16 THEN id=STRING(id,FORMAT='(a16)')
          currsArea.id=id
          posArr=FLOAT(STRSPLIT(pos,',',/EXTRACT))
          IF N_ELEMENTS(posArr) EQ 4 THEN currsArea.pos=posArr
          currsArea.occ=FLOAT(occval(0))
          sAreas=replaceStructStruct(sAreas,currsArea,selA)
          updateVisuals,'A'
        ENDIF ELSE sv=DIALOG_MESSAGE('No area in list to edit yet.')
      END
      'deleteA':BEGIN
        IF SIZE(sAreas,/TNAME) EQ 'STRUCT' AND selA GE 0 THEN BEGIN
          IF N_TAGS(sAreas) EQ 1 THEN BEGIN
            sAreas=!Null
            selA=-1
          ENDIF ELSE BEGIN
            sAreas=removeIDstructstruct(sAreas, selA)
            selA=0
          ENDELSE
          updateVisuals,'A'
        ENDIF
      END
      'getPosA': If lastFieldMarked(0) NE -1 THEN BEGIN
        currsArea.pos=lastFieldMarked
        WIDGET_CONTROL, txtEditPosA, SET_VALUE=STRJOIN(STRING(currsArea.pos,FORMAT='(i0)'),', ')
        lastFieldMarked=[-1,-1,-1,-1]
        redrawrooms
      ENDIF
      'copyA':BEGIN
        WIDGET_CONTROL, lstAreas, GET_VALUE=currTable
        CLIPBOARD.set, STRJOIN(currTable, STRING(9B))
      END
      'importA':BEGIN
        clipres=CLIPBOARD.GET()
        nRows=N_ELEMENTS(clipRes)
        IF nRows GT 0 THEN BEGIN
          err=0
          sAreas=!Null
          FOR r=0, nRows-1 DO BEGIN
            rowArr=STRSPLIT(clipRes(r),STRING(9B),/EXTRACT)
            IF N_ELEMENTS(rowArr) EQ 3 THEN BEGIN
              id=rowArr(0)
              pos=FLOAT(STRSPLIT(rowArr(1),',',/EXTRACT))
              IF N_ELEMENTS(pos) NE 4 THEN pos=[0.,0.,0.,0.];TODO check if pos is outside map
              occ=FLOAT(rowArr(2))
              IF occ GT 1. THEN occ=1.
              IF occ LT 0. THEN occ=0.
              thisArea=CREATE_STRUCT('id',id,'pos',pos,'occ',occ)
              sAreas=CREATE_STRUCT(sAreas,'A'+STRING(r,FORMAT='(i2)'),thisArea)
            ENDIF ELSE BEGIN
              IF N_ELEMENTS(rowArr) NE 1 AND rowArr(0) NE '' THEN err=err+1
            ENDELSE
          ENDFOR
          IF err GT 0 THEN sv=DIALOG_MESSAGE('Found '+STRING(err,FORMAT='(i0)')+' rows not containing elements as expected. These will be ignored.')
          updateVisuals,'all'
        ENDIF
      END
      'addS':BEGIN
        WIDGET_CONTROL, txtEditIdS,GET_VALUE=id
        WIDGET_CONTROL, txtEditPosS, GET_VALUE=pos
        WIDGET_CONTROL, txtEditA0, GET_VALUE=a0
        WIDGET_CONTROL, txtEditT1, GET_VALUE=t1
        WIDGET_CONTROL, txtEditT2, GET_VALUE=t2
        WIDGET_CONTROL, txtEditRestVoid, GET_VALUE=restvoid
        WIDGET_CONTROL, txtEditNS, GET_VALUE=ns
        selIso=WIDGET_INFO(lstEditIsotope, /DROPLIST_SELECT)
        inpat=WIDGET_INFO(btnEditInPat, /BUTTON_SET)
        IF STRLEN(id) GT 16 THEN id=STRING(id,FORMAT='(a16)')
        currsSource.id=id
        posArr=FLOAT(STRSPLIT(pos,',',/EXTRACT))
        IF N_ELEMENTS(posArr) EQ 2 THEN currsSource.pos=posArr
        currsSource.inpatient=inpat
        currsSource.a0=FLOAT(a0(0))
        currsSource.t1=FLOAT(t1(0))
        currsSource.t2=FLOAT(t2(0))
        currsSource.restvoid=FLOAT(restvoid(0))
        currsSource.nPrWorkDay=FLOAT(ns(0))
        currsSource.isotope=isotopes(selIso)
        IF SIZE(sSources,/TNAME) EQ 'STRUCT' THEN nAlready=N_TAGS(sSources) ELSE nAlready=0
        sSources=CREATE_STRUCT(sSources,'S'+STRING(nAlready,FORMAT='(i2)'),currsSource)
        selS=nAlready
        updateVisuals,'S'
      END
      'editS':BEGIN
        IF SIZE(sSources,/TNAME) EQ 'STRUCT' AND selS GE 0 THEN BEGIN
          WIDGET_CONTROL, txtEditIdS,GET_VALUE=id
          WIDGET_CONTROL, txtEditPosS, GET_VALUE=pos
          WIDGET_CONTROL, txtEditA0, GET_VALUE=a0
          WIDGET_CONTROL, txtEditT1, GET_VALUE=t1
          WIDGET_CONTROL, txtEditT2, GET_VALUE=t2
          WIDGET_CONTROL, txtEditRestVoid, GET_VALUE=restvoid
          WIDGET_CONTROL, txtEditNS, GET_VALUE=ns
          selIso=WIDGET_INFO(lstEditIsotope, /DROPLIST_SELECT)
          inpat=WIDGET_INFO(btnEditInPat, /BUTTON_SET)
          IF STRLEN(id) GT 16 THEN id=STRING(id,FORMAT='(a16)')
          currsSource.id=id
          posArr=FLOAT(STRSPLIT(pos,',',/EXTRACT))
          IF N_ELEMENTS(posArr) EQ 2 THEN currsSource.pos=posArr
          currsSource.inpatient=inpat
          currsSource.a0=FLOAT(a0(0))
          currsSource.t1=FLOAT(t1(0))
          currsSource.t2=FLOAT(t2(0))
          currsSource.restvoid=FLOAT(restvoid(0))
          currsSource.nPrWorkDay=FLOAT(ns(0))
          currsSource.isotope=isotopes(selIso)
          sSources=replaceStructStruct(sSources,currsSource,selS)
          updateVisuals,'S'
        ENDIF ELSE sv=DIALOG_MESSAGE('No source in list to edit yet.')
      END
      'deleteS':BEGIN
        IF SIZE(sSources,/TNAME) EQ 'STRUCT' AND selS GE 0 THEN BEGIN
          IF N_TAGS(sSources) EQ 1 THEN BEGIN
            sSources=!Null
            selS=-1
          ENDIF ELSE BEGIN
            sSources=removeIDstructstruct(sSources, selS)
            selS=0
          ENDELSE
          updateVisuals,'S'
        ENDIF
      END
      'getPosS':BEGIN
        currsSource.pos=lastXY
        WIDGET_CONTROL, txtEditPosS, SET_VALUE=STRJOIN(STRING(currsSource.pos,FORMAT='(i0)'),', ')
      END
      'copyS':BEGIN
        WIDGET_CONTROL, lstSources, GET_VALUE=currTable
        CLIPBOARD.set, STRJOIN(currTable, STRING(9B))
      END
      'importS':BEGIN
        clipres=CLIPBOARD.GET()
        nRows=N_ELEMENTS(clipRes)
        IF nRows GT 0 THEN BEGIN
          err=0
          sSources=!Null
          FOR r=0, nRows-1 DO BEGIN
            rowArr=STRSPLIT(clipRes(r),STRING(9B),/EXTRACT)
            IF N_ELEMENTS(rowArr) EQ 9 THEN BEGIN
              id=rowArr(0)
              pos=FLOAT(STRSPLIT(rowArr(1),',',/EXTRACT))
              IF N_ELEMENTS(pos) NE 2 THEN pos=[0.,0.];TODO check if pos is outside map
              IF isotopes.HasValue(rowArr(2)) THEN iso=rowArr(2) ELSE BEGIN
                iso=isotopes(0)
                sv=DIALOG_MESSAGE('Isotope '+rowArr(2)+' not defined. Isotope '+iso+' set instead')
              ENDELSE
              IF LONG(rowArr(3)) EQ 0 THEN inpat=0 ELSE inpat=1
              thisS=CREATE_STRUCT('id',id,'pos',pos,'isotope',iso,'inpatient',inpat,'a0',FLOAT(rowArr(4)),'t1',FLOAT(rowArr(5)),'t2',FLOAT(rowArr(6)),'restVoid',FLOAT(rowArr(7)),'nPrWorkDay',FLOAT(rowArr(8)))
              sSources=CREATE_STRUCT(sSources,'S'+STRING(r,FORMAT='(i2)'),thisS)
            ENDIF ELSE BEGIN
              IF N_ELEMENTS(rowArr) NE 1 AND rowArr(0) NE '' THEN err=err+1
            ENDELSE
          ENDFOR
          IF err GT 0 THEN sv=DIALOG_MESSAGE('Found '+STRING(err,FORMAT='(i0)')+' rows not containing elements as expected. These will be ignored.')
          updateVisuals,'all'
        ENDIF
      END

      'dataMaterial': updShieldInfo, WIDGET_INFO(lstDataMat, /DROPLIST_SELECT), WIDGET_INFO(lstDataIsotope, /DROPLIST_SELECT),WIDGET_INFO(lstDatakV, /DROPLIST_SELECT)
      'dataIsotope':updShieldInfo, WIDGET_INFO(lstDataMat, /DROPLIST_SELECT), WIDGET_INFO(lstDataIsotope, /DROPLIST_SELECT),WIDGET_INFO(lstDatakV, /DROPLIST_SELECT)
      'datakV': updShieldInfo, WIDGET_INFO(lstDataMat, /DROPLIST_SELECT), WIDGET_INFO(lstDataIsotope, /DROPLIST_SELECT),WIDGET_INFO(lstDatakV, /DROPLIST_SELECT)

      'addSCT':BEGIN
        WIDGET_CONTROL, txtEditIdSCT,GET_VALUE=id
        WIDGET_CONTROL, txtEditPosSCT, GET_VALUE=pos
        WIDGET_CONTROL, txtEditRot, GET_VALUE=rotat
        WIDGET_CONTROL, txtEditkvcorr, GET_VALUE=kvcorr
        WIDGET_CONTROL, txtEditmAs, GET_VALUE=mAs
        WIDGET_CONTROL, txtEditNSCT, GET_VALUE=ns
        selkV=WIDGET_INFO(lstEditkV, /DROPLIST_SELECT)
        IF STRLEN(id) GT 16 THEN id=STRING(id,FORMAT='(a16)')
        currsSourceCT.id=id
        posArr=FLOAT(STRSPLIT(pos,',',/EXTRACT))
        IF N_ELEMENTS(posArr) EQ 2 THEN currsSourceCT.pos=posArr
        currsSourceCT.rot=LONG(rotat(0))
        currsSourceCT.kVcorr=FLOAT(kvcorr(0))
        currsSourceCT.mAsPrPat=LONG(mAs(0))
        currsSourceCT.nPrWorkDay=FLOAT(ns(0))
        IF selkV EQ 0 THEN currsSourceCT.kVp=120 ELSE currsSourceCT.kVp=140
        IF SIZE(sSourcesCT,/TNAME) EQ 'STRUCT' THEN nAlready=N_TAGS(sSourcesCT) ELSE nAlready=0
        sSourcesCT=CREATE_STRUCT(sSourcesCT,'SCT'+STRING(nAlready,FORMAT='(i2)'),currsSourceCT)
        selCT=nAlready
        updateVisuals,'SCT'
      END
      'editSCT':BEGIN
        IF SIZE(sSourcesCT,/TNAME) EQ 'STRUCT' AND selCT GE 0 THEN BEGIN
          WIDGET_CONTROL, txtEditIdSCT,GET_VALUE=id
          WIDGET_CONTROL, txtEditPosSCT, GET_VALUE=pos
          WIDGET_CONTROL, txtEditRot, GET_VALUE=rotat
          WIDGET_CONTROL, txtEditkvcorr, GET_VALUE=kvcorr
          WIDGET_CONTROL, txtEditmAs, GET_VALUE=mAs
          WIDGET_CONTROL, txtEditNSCT, GET_VALUE=ns
          selkV=WIDGET_INFO(lstEditkV, /DROPLIST_SELECT)

          IF STRLEN(id) GT 16 THEN id=STRING(id,FORMAT='(a16)')
          currsSourceCT.id=id
          posArr=FLOAT(STRSPLIT(pos,',',/EXTRACT))
          IF N_ELEMENTS(posArr) EQ 2 THEN currsSourceCT.pos=posArr
          currsSourceCT.rot=LONG(rotat(0))
          currsSourceCT.kVcorr=FLOAT(kvcorr(0))
          currsSourceCT.mAsPrPat=LONG(mAs(0))
          currsSourceCT.nPrWorkDay=FLOAT(ns(0))
          IF selkV EQ 0 THEN currsSourceCT.kVp=120 ELSE currsSourceCT.kVp=140
          sSourcesCT=replaceStructStruct(sSourcesCT,currsSourceCT,selCT)
          updateVisuals,'SCT'
        ENDIF ELSE sv=DIALOG_MESSAGE('No source in list to edit yet.')
      END
      'deleteSCT':BEGIN
        IF SIZE(sSourcesCT,/TNAME) EQ 'STRUCT' AND selCT GE 0 THEN BEGIN
          IF N_TAGS(sSourcesCT) EQ 1 THEN BEGIN
            sSourcesCT=!Null
            selCT=-1
          ENDIF ELSE BEGIN
            sSourcesCT=removeIDstructstruct(sSourcesCT, selCT)
            selCT=0
          ENDELSE
          updateVisuals,'SCT'
        ENDIF
      END
      'getPosSCT':BEGIN
        currsSourceCT.pos=lastXY
        WIDGET_CONTROL, txtEditPosSCT, SET_VALUE=STRJOIN(STRING(currsSourceCT.pos,FORMAT='(i0)'),', ')
      END
      'copySCT':BEGIN
        WIDGET_CONTROL, lstSourcesCT, GET_VALUE=currTable
        CLIPBOARD.set, STRJOIN(currTable, STRING(9B))
      END
      'importSCT':BEGIN
        clipres=CLIPBOARD.GET()
        nRows=N_ELEMENTS(clipRes)
        IF nRows GT 0 THEN BEGIN
          err=0
          sSourcesCT=!Null
          FOR r=0, nRows-1 DO BEGIN
            rowArr=STRSPLIT(clipRes(r),STRING(9B),/EXTRACT)
            IF N_ELEMENTS(rowArr) EQ 7 THEN BEGIN
              id=rowArr(0)
              pos=FLOAT(STRSPLIT(rowArr(1),',',/EXTRACT))
              IF N_ELEMENTS(pos) NE 2 THEN pos=[0.,0.];TODO check if pos is outside map
              kVp=LONG(rowArr(2))
              IF kVp NE 120 AND kVp NE 140 THEN kVp=140
              thisSCT=CREATE_STRUCT('id',id,'pos',pos,'kVp',kVp,'rot',LONG(rowArr(3)),'kVcorr',FLOAT(rowArr(4)),'mAsPrPat',LONG(rowArr(5)),'nPrWorkDay',FLOAT(rowArr(6)))
              sSourcesCT=CREATE_STRUCT(sSourcesCT,'SCT'+STRING(r,FORMAT='(i2)'),thisSCT)
            ENDIF ELSE BEGIN
              IF N_ELEMENTS(rowArr) NE 1 AND rowArr(0) NE '' THEN err=err+1
            ENDELSE
          ENDFOR
          IF err GT 0 THEN sv=DIALOG_MESSAGE('Found '+STRING(err,FORMAT='(i0)')+' rows not containing elements as expected. These will be ignored.')
          updateVisuals,'all'
        ENDIF
      END

      'copyIsoSag':BEGIN
        WIDGET_CONTROL, tCTsag, GET_VALUE=currTable
        CLIPBOARD.set, STRJOIN(currTable, STRING(9B))
      END
      'copyIsoCor':BEGIN
        WIDGET_CONTROL, tCTcor, GET_VALUE=currTable
        CLIPBOARD.set, STRJOIN(currTable, STRING(9B))
      END
      'importIsoSag':BEGIN
        clipres=CLIPBOARD.GET()
        nRows=N_ELEMENTS(clipRes)
        sagTab=FLTARR(12,6)
        IF nRows GE 6 THEN BEGIN
          FOR r=0, 5 DO BEGIN
            rowArr=STRSPLIT(clipRes(r),STRING(9B),/EXTRACT)
            FOR e=0, N_ELEMENTS(rowArr)-1 DO rowArr(e)=STRJOIN(STRSPLIT(rowArr(e),',',/EXTRACT),'.')
            rowArr=FLOAT(rowArr)
            IF N_ELEMENTS(rowArr) GT 12 THEN rowArr=rowArr[0:11]
            sagTab[0:N_ELEMENTS(rowArr)-1,r]=rowArr
          ENDFOR
          neg=WHERE(sagTab LT 0)
          IF neg(0) NE -1 THEN sagTab(neg)=0.
          WIDGET_CONTROL, tCTsag, SET_VALUE=STRING(sagTab)
          WIDGET_CONTROL, selFloor, GET_VALUE=selF
          IF selF NE 1 THEN updateVisuals,'SCTmap'
        ENDIF ELSE sv=DIALOG_MESSAGE('6 rows expected. Less than 6 rows found in clipboard.')
      END
      'importIsoCor':BEGIN
        clipres=CLIPBOARD.GET()
        nRows=N_ELEMENTS(clipRes)
        corTab=FLTARR(7,12)
        IF nRows GE 12 THEN BEGIN
          FOR r=0, 11 DO BEGIN
            rowArr=STRSPLIT(clipRes(r),STRING(9B),/EXTRACT)
            FOR e=0, N_ELEMENTS(rowArr)-1 DO rowArr(e)=STRJOIN(STRSPLIT(rowArr(e),',',/EXTRACT),'.')
            rowArr=FLOAT(rowArr)
            IF N_ELEMENTS(rowArr) GT 7 THEN rowArr=rowArr[0:11]
            corTab[0:N_ELEMENTS(rowArr)-1,r]=rowArr
          ENDFOR
          neg=WHERE(corTab LT 0)
          IF neg(0) NE -1 THEN corTab(neg)=0.
          WIDGET_CONTROL, tCTcor, SET_VALUE=STRING(corTab)
          WIDGET_CONTROL, selFloor, GET_VALUE=selF
          IF selF EQ 1 THEN updateVisuals,'SCTmap'
        ENDIF ELSE sv=DIALOG_MESSAGE('12 rows expected. Less than 12 rows found in clipboard.')
      END


      ELSE:
    ENDCASE
  ENDIF

  ;map events
  IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_DRAW') THEN BEGIN

    thisXY=[event.X,event.Y]*zoomFactor*zoomIn(2)+zoomIn[0:1]

    IF thisXY(0) LT 0 THEN thisXY(0)=0
    IF thisXY(1) LT 0 THEN thisXY(1)=0
    IF thisXY(0) GT szim(0)-1 THEN thisXY(0)=szim(0)-1
    IF thisXY(1) GT szim(1)-1 THEN thisXY(1)=szim(1)-1

    WIDGET_CONTROL,txtPos,SET_VALUE=STRING(thisXY(0),FORMAT='(i0)')+', '+STRING(thisXY(1),FORMAT='(i0)')
    IF N_ELEMENTS(calibFactor) GT 0 THEN WIDGET_CONTROL,txtPosM,SET_VALUE=STRING(thisXY(0)*calibFactor,FORMAT='(f0.1)')+', '+STRING(thisXY(1)*calibFactor,FORMAT='(f0.1)') ELSE WIDGET_CONTROL,txtPosM,SET_VALUE='-,-'

    IF N_ELEMENTS(occMap) GT 0 THEN BEGIN
      IF N_ELEMENTS(doseMap) GT 0 THEN BEGIN
        nDim=SIZE(doseMap,/N_DIMENSIONS)
        IF nDim EQ 2 THEN doseImg=doseMap ELSE doseImg=TOTAL(doseMap,3)
      ENDIF
      IF N_ELEMENTS(doseMapCT) GT 0 THEN BEGIN
        nDim=SIZE(doseMapCT,/N_DIMENSIONS)
        IF nDim EQ 2 THEN doseImg=doseMapCT ELSE doseImg=TOTAL(doseMapCT,3);both if dose just for one of them
      ENDIF
      IF N_ELEMENTS(occMap) EQ N_ELEMENTS(doseImg) THEN occThis=occMap[thisXY(0),thisXY(1)] ELSE occThis=1.
    ENDIF ELSE occThis=1.

    IF N_ELEMENTS(doseMap) GT 0 THEN BEGIN
      IF MAX(doseMap) NE 0 THEN BEGIN
        doseNM=occThis*TOTAL(doseMap[thisXY(0),thisXY(1),*]);sum all NM sources
        WIDGET_CONTROL,txtDoseNM,SET_VALUE=STRING(doseNM,FORMAT='(f0.3)')
        doseRateNM=TOTAL(doseRateMax[thisXY(0),thisXY(1),*]);all max at the same time
        ;doseRateNMmax=MAX(doseRateMax[thisXY(0),thisXY(1),*])
        WIDGET_CONTROL,txtDoseRateNM, SET_VALUE=STRING(doseRateNM, FORMAT='(f0.1)')
      ENDIF ELSE BEGIN
        doseNM=0
        WIDGET_CONTROL,txtDoseNM,SET_VALUE='-'
      ENDELSE
    ENDIF ELSE doseNM=0
    IF N_ELEMENTS(doseMapCT) GT 0 THEN BEGIN
      IF MAX(doseMapCT) NE 0 THEN BEGIN
        doseCT=occThis*TOTAL(doseMapCT[thisXY(0),thisXY(1),*]);sum all CT sources
        WIDGET_CONTROL,txtDoseCT,SET_VALUE=STRING(doseCT,FORMAT='(f0.3)')
      ENDIF ELSE BEGIN
        doseCT=0
        WIDGET_CONTROL,txtDoseCT,SET_VALUE='-'
      ENDELSE
    ENDIF ELSE doseCT=0

    IF doseNM + doseCT GT 0 THEN  WIDGET_CONTROL,txtDose,SET_VALUE=STRING(doseCT+doseNM,FORMAT='(f0.3)') ELSE WIDGET_CONTROL,txtDose,SET_VALUE='-'

    IF N_ELEMENTS(occMap) GT 2 THEN BEGIN
      IF MAX(occMap) NE 0 THEN WIDGET_CONTROL,txtOcc,SET_VALUE=STRING(occMap(thisXY(0),thisXY(1)),FORMAT='(f0.3)')
    ENDIF ELSE WIDGET_CONTROL,txtOcc,SET_VALUE='-'

    IF event.release EQ 1 AND event.type LE 1 THEN BEGIN
      lastXY=thisXY
      redrawrooms
    ENDIF

    IF event.press EQ 4 AND event.type EQ 0 AND mouseDownRight EQ 0 THEN BEGIN;right mouse press
      lastXYright=thisXY
      mouseDownRight = 1
    ENDIF

    IF event.release EQ 4 AND event.type EQ 1 AND mouseDownRight EQ 1 THEN BEGIN
      ;thisXY=[event.X,event.Y]*zoomFactor
      IF thisXY(0) GT szim(0)*zoomFactor THEN thisXY(0)=FLOOR(szim(0)*zoomFactor)
      IF thisXY(1) GT szim(1)*zoomFactor THEN thisXY(1)=FLOOR(szim(1)*zoomFactor)
      xes=[thisXY(0), lastXYright(0)]
      yes=[thisXY(1), lastXYright(1)]
      xes=xes(SORT(xes)) & yes=yes(SORT(yes))
      lastFieldMarked=[xes(0),yes(0),xes(1),yes(1)]
      redrawrooms
      mouseDownRight=0
    ENDIF

  ENDIF

  ;****************table events****************************************
  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_TABLE_CELL_SEL' THEN BEGIN

    IF event.sel_Top NE -1 THEN BEGIN; deselection

      CASE event.ID OF
        lstWalls:BEGIN
          tabSel=WIDGET_INFO(lstWalls,/TABLE_SELECT)
          selW=tabSel(1)
          updateVisuals,'Wsel'
        END
        lstAreas:BEGIN
          tabSel=WIDGET_INFO(lstAreas,/TABLE_SELECT)
          selA=tabSel(1)
          updateVisuals,'Asel'
        END
        lstSources:BEGIN
          tabSel=WIDGET_INFO(lstSources,/TABLE_SELECT)
          selS=tabSel(1)
          updateVisuals,'Ssel'
        END
        lstSourcesCT:BEGIN
          tabSel=WIDGET_INFO(lstSourcesCT,/TABLE_SELECT)
          selCT=tabSel(1)
          updateVisuals,'SCTsel'
        END
        ELSE:
      ENDCASE

    ENDIF
  ENDIF

  ;********************************************* Textfield changed **************************************************************
  IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KBRD_FOCUS') OR (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_TEXT_CH') THEN BEGIN
    action=0
    IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KBRD_FOCUS') THEN BEGIN
      IF event.enter EQ 0 THEN action=1 ; lost focus
    ENDIF
    IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_TEXT_CH') THEN BEGIN
      IF event.type EQ 0 THEN action=1 ;return or enter pressed
    ENDIF

    IF action EQ 1 THEN BEGIN
      CASE event.ID OF
        txtCalib: BEGIN
          WIDGET_CONTROL, txtCalib, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtCalib, SET_VALUE=STRING(val, FORMAT='(f0.3)')
          IF N_ELEMENTS(startMap) NE 0 AND N_ELEMENTS(endMap) NE 0 THEN BEGIN
            lenCalib=SQRT((endMap(0)-startMap(0))^2+(endMap(1)-startMap(1))^2)
            calibFactor=val/lenCalib
            updateVisuals,'overlay'
          ENDIF
        END
        txtWorkDaysPrYear:BEGIN
          WIDGET_CONTROL, txtWorkDaysPrYear, GET_VALUE=val
          val=LONG(val(0))
          WIDGET_CONTROL, txtWorkDaysPrYear, SET_VALUE=STRING(val, FORMAT='(i0)')
          workdays=val
          updateVisuals,'overlay'
        END
        txtH0:BEGIN
          WIDGET_CONTROL, txtH0, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtH0, SET_VALUE=STRING(val, FORMAT='(f0.3)')
          WIDGET_CONTROL, selFloor, GET_VALUE=selF
          IF selF EQ 2 THEN updateVisuals,'overlay'
        END
        txtH1:BEGIN
          WIDGET_CONTROL, txtH1, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtH1, SET_VALUE=STRING(val, FORMAT='(f0.3)')
          WIDGET_CONTROL, selFloor, GET_VALUE=selF
          IF selF Eq 0 THEN updateVisuals,'overlay'
        END
        txtConCeil:BEGIN
          WIDGET_CONTROL, txtConCeil, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtConCeil, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          WIDGET_CONTROL, selFloor, GET_VALUE=selF
          IF selF EQ 0 THEN updateVisuals,'overlay'
        END
        txtLeadCeil:BEGIN
          WIDGET_CONTROL, txtLeadCeil, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtLeadCeil, SET_VALUE=STRING(val, FORMAT='(f0.3)')
          WIDGET_CONTROL, selFloor, GET_VALUE=selF
          IF selF EQ 0 THEN updateVisuals,'overlay'
        END
        txtConFloor:BEGIN
          WIDGET_CONTROL, txtConFloor, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtConFloor, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          WIDGET_CONTROL, selFloor, GET_VALUE=selF
          IF selF EQ 2 THEN updateVisuals,'overlay'
        END
        txtLeadFloor:BEGIN
          WIDGET_CONTROL, txtLeadFloor, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtLeadFloor, SET_VALUE=STRING(val, FORMAT='(f0.3)')
          WIDGET_CONTROL, selFloor, GET_VALUE=selF
          IF selF EQ 2 THEN updateVisuals,'overlay'
        END
        txtEditThickW:BEGIN
          WIDGET_CONTROL, txtEditThickW, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtEditThickW, SET_VALUE=STRING(val, FORMAT='(f0.1)')
          IF WIDGET_INFO(lstEditMatW, /DROPLIST_SELECT) EQ 0 AND val GE 1. THEN BEGIN
            sv=DIALOG_MESSAGE('Material selected is lead and unit is cm. Make sure you intended to insert a value > 1 cm of lead?')
          ENDIF
        END
        txtEditOccA:BEGIN
          WIDGET_CONTROL, txtEditOccA, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          IF val GT 1. THEN val=1.
          WIDGET_CONTROL, txtEditOccA, SET_VALUE=STRING(val, FORMAT='(f0.2)')
        END
        txtEditA0: BEGIN
          WIDGET_CONTROL, txtEditA0, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtEditA0, SET_VALUE=STRING(val, FORMAT='(i0)')
        END
        txtEditT1:BEGIN
          WIDGET_CONTROL, txtEditT1, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtEditT1, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        END
        txtEditT2:BEGIN
          WIDGET_CONTROL, txtEditT2, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtEditT2, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        END
        txtEditRestVoid:BEGIN
          WIDGET_CONTROL, txtEditRestVoid, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          IF val GT 1. THEN val=1.
          WIDGET_CONTROL, txtEditRestVoid, SET_VALUE=STRING(val, FORMAT='(f0.2)')
        END
        txtEditNS:BEGIN
          WIDGET_CONTROL, txtEditNS, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtEditNS, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        END
        txtEditRot:BEGIN
          WIDGET_CONTROL, txtEditRot, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          IF val GT 359 THEN val=359
          WIDGET_CONTROL, txtEditRot, SET_VALUE=STRING(val, FORMAT='(i0)')
        END
        txtEditmas:BEGIN
          WIDGET_CONTROL, txtEditmas, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtEditmas, SET_VALUE=STRING(val, FORMAT='(i0)')
        END
        txtEditkvcorr:BEGIN
          WIDGET_CONTROL, txtEditkvcorr, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtEditkvcorr, SET_VALUE=STRING(val, FORMAT='(f0.2)')
        END
        txtEditNSCT: BEGIN
          WIDGET_CONTROL, txtEditNSCT, GET_VALUE=val
          val=ABS(FLOAT(comma2pointFloat(val(0))))
          WIDGET_CONTROL, txtEditNSCT, SET_VALUE=STRING(val, FORMAT='(f0.1)')
        END
        ELSE:
      ENDCASE
    ENDIF
  ENDIF

  IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_TAB') THEN BEGIN
    currTab=WIDGET_INFO(event.ID, /TAB_CURRENT)
    ;redrawrooms
  ENDIF

  ;******************* Exit program  by X in the corner***********************
  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
    ;programStops=1
    sv=DIALOG_MESSAGE('Are you sure you have saved what you need?',/QUESTION)
    IF sv EQ 'Yes' THEN WIDGET_CONTROL, event.top, /DESTROY
  ENDIF
end