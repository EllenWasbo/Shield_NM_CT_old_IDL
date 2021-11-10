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

pro updateVisuals, what

  COMMON SHNM

  WIDGET_CONTROL,/HOURGLASS
  WIDGET_CONTROL, selFloor, GET_VALUE=selF
  mapim=0
  updA=0
  updW=0
  updS=0
  updSCT=0
  WIDGET_CONTROL, selOverlay, GET_VALUE=select2overlay
  
  FOR i=0, N_ELEMENTS(what)-1 DO BEGIN
    CASE what(i) OF
      'all':BEGIN
        mapim=1
        updA=1
        updW=1
        updS=1
        updSCT=1
      END
      'overlay':BEGIN        
        CASE select2overlay OF
          0: updA=3
          1: BEGIN
            updS=3
            updSCT=3
          END
          2:updS=3
        ENDCASE
        END
      'A': updA=1 ; update visual list and occMap from area-structure - ignored if selected floor not this floor
      'Asel': updA=2 ;not update text in table, just set selected parameters
      'Amap': updA=3 ;just update occMap
      'W': updW=1 ; update visual list from wall-structure- ignored if selected floor not this floor
      'Wsel':updW=2 ;not update text in table, just set selected parameters
      'S': updS=1 ; update visual list from NM source-structure + calculate NM dose
      'Ssel':updS=2 ;not update text in table, just set selected parameters
      'Smap':updS=3 ;just update dosecalculations
      'SCT':updSCT=1; update visual list from CT source-structure + calculate CT dose
      'SCTsel':updSCT=2;not update text in table, just set selected parameters
      'SCTmap':updSCT=3;just update dosecalculations
      ELSE:
    ENDCASE
  ENDFOR

  ;floor above or floor below - walls ignored and occ.Factor = 1. for all
  IF selF NE 1 THEN BEGIN
    updA=0
    updW=0
    occMap=im*0.+1.
  ENDIF
  IF SIZE(occMAp, /TNAME) EQ 'UNDEFINED' THEN occMap=im*0.+1.

  IF updW GE 1 THEN BEGIN
    mapim=1
    IF SIZE(sWalls, /TNAME) EQ 'STRUCT' THEN BEGIN
      IF selW EQ -1 THEN selW=0

      IF updW EQ 1 THEN BEGIN;generate visual list from structure
        nWalls=N_TAGS(sWalls)
        strWalls=STRARR(5,nWalls)
        FOR i=0, nWalls-1 DO BEGIN
          IF sWalls.(i).std EQ 1 THEN BEGIN
            thickn=stdWall.thickness
            matid= stdWall.material
            stdTxt='X'
          ENDIF ELSE BEGIN
            thickn=sWalls.(i).thickness
            matid= sWalls.(i).material
            stdTxt=''
          ENDELSE
          strWalls[*,i]=[sWalls.(i).id,STRJOIN(STRING(sWalls.(i).pos,FORMAT='(f0.1)'),', '),STRING(thickn,FORMAT='(f0.2)'),materialList(matid),stdTxt]
        ENDFOR

        oldView=WIDGET_INFO(lstWalls, /TABLE_VIEW)
        IF selW-oldView(1) GT 5 THEN viewRow=selW-5 ELSE viewRow=oldView(1)
        IF viewRow LT 0 THEN viewRow=0
        WIDGET_CONTROL,lstWalls, TABLE_YSIZE=nWalls, SET_VALUE=strWalls
        WIDGET_CONTROL,lstWalls, SET_TABLE_SELECT=[0,selW,4,selW]
        WIDGET_CONTROL,lstWalls, SET_TABLE_VIEW=[0,viewRow]
      ENDIF
      
      ;update fields with selected row in list
      currsWall=sWalls.(selW)
      WIDGET_CONTROL, txtEditIdW,SET_VALUE=currsWall.id
      WIDGET_CONTROL, txtEditPosW, SET_VALUE=STRJOIN(STRING(currsWall.pos,FORMAT='(f0.1)'),', ')
      IF currsWall.std EQ 1 THEN BEGIN
        WIDGET_CONTROL, btnUseStdW, SET_BUTTON=1
        WIDGET_CONTROL, txtEditThickW, SET_VALUE=STRING(stdWall.thickness,FORMAT='(f0.2)')
        WIDGET_CONTROL, lstEditMatW, SET_DROPLIST_SELECT=stdWall.material
      ENDIF ELSE BEGIN
        WIDGET_CONTROL, btnUseStdW, SET_BUTTON=0
        WIDGET_CONTROL, txtEditThickW, SET_VALUE=STRING(currsWall.thickness,FORMAT='(f0.2)')
        WIDGET_CONTROL, lstEditMatW, SET_DROPLIST_SELECT=currsWall.material
      ENDELSE

    ENDIF ELSE BEGIN
      WIDGET_CONTROL,lstWalls,SET_VALUE=STRARR(5)
    ENDELSE
  ENDIF

  IF updA GE 1 THEN BEGIN
    mapim=1
    IF SIZE(sAreas, /TNAME) EQ 'STRUCT' THEN BEGIN
      IF selA EQ -1 THEN selA=0

      IF updA NE 2 THEN BEGIN;generate list and if updA=1 not 3, update table
        nAreas=N_TAGS(sAreas)
        strAreas=STRARR(3,nAreas)
        occMap=im*0.+1.
        szOccMap=SIZE(occMap, /DIMENSIONS)
        FOR i=0, nAreas-1 DO BEGIN
          strAreas[*,i]=[sAreas.(i).id,STRJOIN(STRING(sAreas.(i).pos,FORMAT='(f0.1)'),', '),STRING(sAreas.(i).occ,FORMAT='(f0.2)')]
          IF MIN(sAreas.(i).pos) NE -1 THEN BEGIN
            IF sAreas.(i).pos(2) GE szOccMap(0) THEN sAreas.(i).pos(2)=szOccMap(0)-1
            IF sAreas.(i).pos(3) GE szOccMap(1) THEN sAreas.(i).pos(3)=szOccMap(1)-1
            occMap[sAreas.(i).pos(0):sAreas.(i).pos(2),sAreas.(i).pos(1):sAreas.(i).pos(3)]=sAreas.(i).occ
          ENDIF
        ENDFOR
        IF updA EQ 1 THEN BEGIN
          oldView=WIDGET_INFO(lstAreas, /TABLE_VIEW)
          IF selA-oldView(1) GT 5 THEN viewRow=selA-5 ELSE viewRow=oldView(1)
          IF viewRow LT 0 THEN viewRow=0
          WIDGET_CONTROL,lstAreas, TABLE_YSIZE=nAreas, SET_VALUE=strAreas
          WIDGET_CONTROL,lstAreas, SET_TABLE_SELECT=[0,selA,2,selA]
          WIDGET_CONTROL,lstAreas, SET_TABLE_VIEW=[0,viewRow]
        ENDIF
      ENDIF

      IF updA LE 2 THEN BEGIN;update selection
        currsArea=sAreas.(selA)
        WIDGET_CONTROL, txtEditIdA,SET_VALUE=currsArea.id
        WIDGET_CONTROL, txtEditPosA, SET_VALUE=STRJOIN(STRING(currsArea.pos,FORMAT='(f0.1)'),', ')
        WIDGET_CONTROL, txtEditOccA, SET_VALUE=STRING(currsArea.occ,FORMAT='(f0.2)')
      ENDIF
    ENDIF ELSE BEGIN
      WIDGET_CONTROL,lstAreas,SET_VALUE=STRARR(3)
      occMap=im*0.+1.
    ENDELSE
  ENDIF

  IF updS GE 1 THEN BEGIN
    mapim=1
    IF SIZE(sSources, /TNAME) EQ 'STRUCT' THEN BEGIN
      IF selS EQ -1 THEN selS=0

      IF updS NE 2 THEN BEGIN;generate list and calculate dose (list only updated if updS = 1)
        nSources=N_TAGS(sSources)
        strSources=STRARR(9,nSources)
        doseMap=FLTARR(szim(0),szim(1),nSources)
        doseRateMax=FLTARR(szim(0),szim(1),nSources)
        ;IF N_ELEMENTS(calibFactor) GT 0 THEN BEGIN
        FOR i=0, nSources-1 DO BEGIN
          isono=WHERE(isotopes EQ sSources.(i).isotope)
          strSources[*,i]=[sSources.(i).id,STRJOIN(STRING(sSources.(i).pos,FORMAT='(f0.1)'),', '),isotopes(isono),STRING(sSources.(i).inpatient, FORMAT='(i0)'),STRING(sSources.(i).a0,$
            FORMAT='(f0.2)'),STRING(sSources.(i).t1,FORMAT='(f0.2)'),STRING(sSources.(i).t2,FORMAT='(f0.2)'),STRING(sSources.(i).restvoid,FORMAT='(f0.2)'),STRING(sSources.(i).nPrWorkDay,FORMAT='(f0.2)')]
          IF N_ELEMENTS(calibFactor) GT 0 AND select2overlay NE 0 THEN BEGIN; no calculation if dose is not shown or size calibration is not done
            CASE selF OF
              0:  BEGIN;floor +1
                WIDGET_CONTROL, txtH1,GET_VALUE=H1
                WIDGET_CONTROL, txtC1,GET_VALUE=C1
                WIDGET_CONTROL, txtC2,GET_VALUE=C2
                floorD=FLOAT(H1)-FLOAT(C1)+FLOAT(C2)
                WIDGET_CONTROL,txtConCeil,GET_VALUE=thickCon
                WIDGET_CONTROL,txtLeadCeil,GET_VALUE=thickLead
                calcRes=addSource2Floor(sSources.(i),szim,calibFactor,shieldData,workdays,thickLead(0),thickCon(0),floorD(0),WIDGET_INFO(btnCorrThick, /BUTTON_SET))
                doseMap[*,*,i]=calcRes.doseMap
                doseRateMax[*,*,i]=calcRes.maxDoseRate
              END

              1:  BEGIN
                calcRes=addSource2Map(sSources.(i),sWalls,szim,calibFactor,shieldData,workdays,stdWall,WIDGET_INFO(btnCorrThick, /BUTTON_SET)); this floor
                doseMap[*,*,i]=calcRes.doseMap
                doseRateMax[*,*,i]=calcRes.maxDoseRate
                END

              2:BEGIN;floor -1
                WIDGET_CONTROL, txtH0,GET_VALUE=H0
                WIDGET_CONTROL, txtC1,GET_VALUE=C1
                WIDGET_CONTROL, txtC0,GET_VALUE=C0
                floorD=FLOAT(H0)-FLOAT(C0)+FLOAT(C1)
                WIDGET_CONTROL,txtConFloor,GET_VALUE=thickCon
                WIDGET_CONTROL,txtLeadFloor,GET_VALUE=thickLead
                calcRes=addSource2Floor(sSources.(i),szim,calibFactor,shieldData,workdays,thickLead(0),thickCon(0),floorD(0),WIDGET_INFO(btnCorrThick, /BUTTON_SET))
                doseMap[*,*,i]=calcRes.doseMap
                doseRateMax[*,*,i]=calcRes.maxDoseRate
              END
            ENDCASE

          ENDIF
        ENDFOR
        IF N_ELEMENTS(calibFactor) EQ 0 THEN sv=DIALOG_MESSAGE('Map scaling not set. After setting map scaling. Refresh all.')

        IF updS EQ 1 THEN BEGIN
          oldView=WIDGET_INFO(lstSources, /TABLE_VIEW)
          IF selS-oldView(1) GT 5 THEN viewRow=selS-5 ELSE viewRow=oldView(1)
          IF viewRow LT 0 THEN viewRow=0
          WIDGET_CONTROL,lstSources, TABLE_YSIZE=nSources, SET_VALUE=strSources
          WIDGET_CONTROL,lstSources, SET_TABLE_SELECT=[0,selS,6,selS]
          WIDGET_CONTROL,lstSources, SET_TABLE_VIEW=[0,viewRow]
        ENDIF
      ENDIF

      IF updS LE 2 THEN BEGIN
        currsSource=sSources.(selS)
        WIDGET_CONTROL, txtEditIdS,SET_VALUE=currsSource.id
        WIDGET_CONTROL, btnEditInPat, SET_BUTTON=currsSource.inpatient
        WIDGET_CONTROL, txtEditPosS, SET_VALUE=STRJOIN(STRING(currsSource.pos,FORMAT='(f0.1)'),', ')
        WIDGET_CONTROL, txtEditA0, SET_VALUE=STRING(currsSource.a0,FORMAT='(f0.2)')
        WIDGET_CONTROL, txtEditT1, SET_VALUE=STRING(currsSource.t1,FORMAT='(f0.2)')
        WIDGET_CONTROL, txtEditT2, SET_VALUE=STRING(currsSource.t2,FORMAT='(f0.2)')
        WIDGET_CONTROL, txtEditRestVoid, SET_VALUE=STRING(currsSource.restvoid,FORMAT='(f0.2)')
        WIDGET_CONTROL, txtEditNS, SET_VALUE=STRING(currsSource.nPrWorkDay,FORMAT='(f0.2)')
        isono=WHERE(isotopes EQ currsSource.isotope)
        WIDGET_CONTROL, lstEditIsotope, SET_DROPLIST_SELECT=isono(0)
      ENDIF
    ENDIF ELSE BEGIN
      IF updS EQ 1 THEN WIDGET_CONTROL,lstSources,SET_VALUE=STRARR(8)
    ENDELSE
  ENDIF

  IF updSCT GE 1 THEN BEGIN
    mapim=1
    IF SIZE(sSourcesCT, /TNAME) EQ 'STRUCT' THEN BEGIN
      IF selCT EQ -1 THEN selCT=0

      IF updSCT NE 2 THEN BEGIN;generate list and calculate dose (list only updated if updSCT = 1)
        nSources=N_TAGS(sSourcesCT)
        strSources=STRARR(7,nSources)
        doseMapCT=FLTARR(szim(0),szim(1),nSources)
        FOR i=0, nSources-1 DO BEGIN
          strSources[*,i]=[sSourcesCT.(i).id,STRJOIN(STRING(sSourcesCT.(i).pos,FORMAT='(f0.1)'),', '),STRING(sSourcesCT.(i).kVp,FORMAT='(i0)'),STRING(sSourcesCT.(i).rot,FORMAT='(i0)'),STRING(sSourcesCT.(i).kVcorr,FORMAT='(f0.2)'),STRING(sSourcesCT.(i).mAsPrPat,FORMAT='(i0)'),STRING(sSourcesCT.(i).nPrWorkDay,FORMAT='(f0.2)')]
        ENDFOR
        IF N_ELEMENTS(calibFactor) GT 0 AND select2overlay EQ 1 THEN BEGIN
          CASE selF OF
            0:  BEGIN;floor +1
              IF N_ELEMENTS(sagTab) GT 0 THEN BEGIN
                WIDGET_CONTROL, txtH1,GET_VALUE=H1
                WIDGET_CONTROL, txtC1,GET_VALUE=C1
                WIDGET_CONTROL, txtC2,GET_VALUE=C2
                floorD=FLOAT(H1)-FLOAT(C1)+FLOAT(C2)
                WIDGET_CONTROL,txtConCeil,GET_VALUE=thickCon
                WIDGET_CONTROL,txtLeadCeil,GET_VALUE=thickLead
                CTarr=generateCTmap(sagTab)
                IF TOTAL(CTarr) GT 0 THEN BEGIN
                  FOR s=0, nSources-1 DO BEGIN
                    doseMapCT[*,*,s]=addCTSource2Floor(sSourcesCT.(s),szim,calibFactor,shieldDataCT,workdays,thickLead(0),thickCon(0),floorD(0),CTarr,WIDGET_INFO(btnCorrThick, /BUTTON_SET)); no calculation if dose is not shown or size calibration is not done
                  ENDFOR
                ENDIF
              ENDIF ELSE sv=DIALOG_MESSAGE('Sagital CT isodose map not defined yet. Calculations for CT dose cannot be performed.')
            END

            1:  BEGIN; this floor
              IF N_ELEMENTS(corTab) GT 0 THEN BEGIN
                CTarr=generateCTmap(corTab)
                IF TOTAL(CTarr) GT 0 THEN BEGIN
                  FOR s=0, nSources-1 DO BEGIN
                    doseMapCT[*,*,s]=addCTSource2Map(sSourcesCT.(s),sWalls,szim,calibFactor,shieldDataCT,workdays,stdWall,CTarr,WIDGET_INFO(btnCorrThick, /BUTTON_SET)); no calculation if dose is not shown or size calibration is not done
                  ENDFOR
                ENDIF
              ENDIF ELSE sv=DIALOG_MESSAGE('Coronal CT isodose map not defined yet. Calculations for CT dose cannot be performed.')
            END

            2:BEGIN;floor -1
              IF N_ELEMENTS(sagTab) GT 0 THEN BEGIN
                WIDGET_CONTROL, txtH0,GET_VALUE=H0
                WIDGET_CONTROL, txtC1,GET_VALUE=C1
                WIDGET_CONTROL, txtC0,GET_VALUE=C0
                floorD=FLOAT(H0)-FLOAT(C0)+FLOAT(C1)
                WIDGET_CONTROL,txtConCeil,GET_VALUE=thickCon
                WIDGET_CONTROL,txtLeadCeil,GET_VALUE=thickLead
                CTarr=generateCTmap(sagTab)
                IF TOTAL(CTarr) GT 0 THEN BEGIN
                  FOR s=0, nSources-1 DO BEGIN
                    doseMapCT[*,*,s]=addCTSource2Floor(sSourcesCT.(s),szim,calibFactor,shieldDataCT,workdays,thickLead(0),thickCon(0),-floorD(0),CTarr,WIDGET_INFO(btnCorrThick, /BUTTON_SET)); no calculation if dose is not shown or size calibration is not done
                  ENDFOR
                ENDIF
              ENDIF ELSE sv=DIALOG_MESSAGE('Sagital CT isodose map not defined yet. Calculations for CT dose cannot be performed.')
            END
          ENDCASE

        ENDIF
        IF N_ELEMENTS(calibFactor) EQ 0 THEN sv=DIALOG_MESSAGE('Map scaling not set. After setting map scaling. Refresh all.')

        IF updSCT EQ 1 THEN BEGIN
          oldView=WIDGET_INFO(lstSourcesCT, /TABLE_VIEW)
          IF selCT-oldView(1) GT 5 THEN viewRow=selCT-5 ELSE viewRow=oldView(1)
          IF viewRow LT 0 THEN viewRow=0
          WIDGET_CONTROL,lstSourcesCT, TABLE_YSIZE=nSources, SET_VALUE=strSources
          WIDGET_CONTROL,lstSourcesCT, SET_TABLE_SELECT=[0,selCT,6,selCT]
          WIDGET_CONTROL,lstSourcesCT, SET_TABLE_VIEW=[0,viewRow]
        ENDIF
      ENDIF
      
      IF updSCT LE 2 THEN BEGIN
        currsSourceCT=sSourcesCT.(selCT)
        WIDGET_CONTROL, txtEditIdSCT,SET_VALUE=currsSourceCT.id
        WIDGET_CONTROL, txtEditPosSCT, SET_VALUE=STRJOIN(STRING(currsSourceCT.pos,FORMAT='(f0.1)'),', ')
        WIDGET_CONTROL, txtEditRot, SET_VALUE=STRING(currsSourceCT.rot,FORMAT='(i0)')
        WIDGET_CONTROL, txtEditkvcorr, SET_VALUE=STRING(currsSourceCT.kVcorr,FORMAT='(f0.2)')
        WIDGET_CONTROL, txtEditmAs, SET_VALUE=STRING(currsSourceCT.mAsPrPat,FORMAT='(i0)')
        IF currsSourceCT.kVp EQ 120 THEN kvno=0 ELSE kvno=1
        WIDGET_CONTROL, lstEditkV, SET_DROPLIST_SELECt=kvno
        WIDGET_CONTROL, txtEditNSCT, SET_VALUE=STRING(currsSourceCT.nPrWorkDay,FORMAT='(f0.2)')
      ENDIF
    ENDIF ELSE BEGIN
      IF updSCT EQ 1 THEN WIDGET_CONTROL,lstSourcesCT,SET_VALUE=STRARR(7)
    ENDELSE
  ENDIF
  
  IF mapim THEN redrawrooms

end

