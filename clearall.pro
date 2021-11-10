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

pro clearAll
  COMMON SHNM
  sWalls=!Null
  sAreas=!Null
  sSources=!Null
  sSourcesCT=!Null
  doseMap=!Null
  doseRateMax=!Null
  doseMapCT=!Null
  subsWallsDef=CREATE_STRUCT('id','','pos',[0.0,0.0,0.0,0.0],'material',0,'thickness',0.0,'std',0);if thickness = -1 then use standard thickness
  subsAreasDef=CREATE_STRUCT('id','','pos',[0.0,0.0,0.0,0.0],'occ',0.0)
  subsSourcesDef=CREATE_STRUCT('id','','pos',[0.0,0.0],'isotope','F18','inpatient',0,'a0',0.0,'t1',0.0,'t2',0.0,'restvoid',1.0,'nPrWorkDay',0.0)
  subsSourcesCTDef=CREATE_STRUCT('id','','pos',[0.0,0.0],'kVp',140,'rot',0.0,'kVcorr',1.0,'mAsPrPat',0.0,'nPrWorkDay',0.0)
  currsWall=subsWallsDef & currsArea=subsAreasDef & currsSource=subsSourcesDef & currsSourceCT=subsSourcesCTDef; holding current values in edit fields
  selA=-1 & selW=-1 & selS=-1 & selCT=-1;selected in lists
  
  zoomFactor=1.
  zoomIn=[0.,0.,1.,1.]
  mouseDownRight=0
  lastFieldMarked=[-1,-1,-1,-1]
  readNewImg, defaultImgPath

  updateVisuals, 'all'
end