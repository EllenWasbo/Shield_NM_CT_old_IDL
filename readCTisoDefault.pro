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

pro readCTisoDefault
  COMMON SHNM

  OPENR, filenhet, defaultIsoPaths[1], /GET_LUN
  elem=''
  clipRes=!Null
  WHILE ~ EOF(filenhet) DO BEGIN
    READF, filenhet, elem
    IF elem NE '' THEN clipRes=[clipRes, elem]
  ENDWHILE
  CLOSE, filenhet
  FREE_LUN, filenhet

  sagTab=FLTARR(12,6)
  FOR r=0, 5 DO BEGIN
    rowArr=STRSPLIT(clipRes(r),STRING(9B),/EXTRACT)
    FOR e=0, N_ELEMENTS(rowArr)-1 DO rowArr(e)=STRJOIN(STRSPLIT(rowArr(e),',',/EXTRACT),'.')
    rowArr=FLOAT(rowArr)
    IF N_ELEMENTS(rowArr) GT 12 THEN rowArr=rowArr[0:11]
    sagTab[0:N_ELEMENTS(rowArr)-1,r]=rowArr
  ENDFOR

  OPENR, filenhet, defaultIsoPaths[0], /GET_LUN
  elem=''
  clipRes=!Null
  WHILE ~ EOF(filenhet) DO BEGIN
    READF, filenhet, elem
    IF elem NE '' THEN clipRes=[clipRes, elem]
  ENDWHILE
  CLOSE, filenhet
  FREE_LUN, filenhet

  corTab=FLTARR(7,12)

  FOR r=0, 11 DO BEGIN
    rowArr=STRSPLIT(clipRes(r),STRING(9B),/EXTRACT)
    FOR e=0, N_ELEMENTS(rowArr)-1 DO rowArr(e)=STRJOIN(STRSPLIT(rowArr(e),',',/EXTRACT),'.')
    rowArr=FLOAT(rowArr)
    IF N_ELEMENTS(rowArr) GT 7 THEN rowArr=rowArr[0:11]
    corTab[0:N_ELEMENTS(rowArr)-1,r]=rowArr
  ENDFOR


end