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

pro readNewImg, adr

  COMMON SHNM
  
  rim=READ_PNG(adr)
  sz=SIZE(rim,/DIMENSIONS)
  szim=sz[1:2]
  im=FLTARR(szim)
  im[*,*]=rim[0,*,*]
  rel=(1.0*maxIm)/szim
  IF min(rel) GT 1 THEN BEGIN
    IF rel(0) LT rel(1) THEN BEGIN
      szIm=ROUND(rel(0)*szIm)
      im=CONGRID(im, szim(0), szim(1) ,/INTERP)
    ENDIF ELSE BEGIN
      szIm=ROUND(rel(1)*szIm)
      im=CONGRID(im, szim(0), szim(1) ,/INTERP)
    ENDELSE
  ENDIF
  IF szIm(0) LT maxIm(0) THEN BEGIN; fix as code do not handle that the image do not fill the x-dimension
    imNew=FLTARR(maxIm(0),szIm(1))
    imNew[0:szIm(0)-1,*]=im
    im=imNew
    szIm=[maxIm(0), szIm(1)]
  ENDIF

end