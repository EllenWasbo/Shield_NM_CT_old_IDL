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

pro updShieldInfo, mat, iso, kV
COMMON SHNM

WIDGET_CONTROL, txtDataT12, SET_VALUE=STRING(shieldData.(iso).halflife, FORMAT='(f0.2)')
tnames=TAG_NAMES(shieldData.(iso))
IF tnames.HasValue('ABG') THEN BEGIN
  WIDGET_CONTROL, txtDataAlpha, SET_VALUE=STRING(shieldData.(iso).abg(0,mat), FORMAT='(f0.4)')
  WIDGET_CONTROL, txtDataBeta, SET_VALUE=STRING(shieldData.(iso).abg(1,mat), FORMAT='(f0.4)')
  WIDGET_CONTROL, txtDataGamma, SET_VALUE=STRING(shieldData.(iso).abg(2,mat), FORMAT='(f0.4)')
ENDIF ELSE BEGIN
  WIDGET_CONTROL, txtDataAlpha, SET_VALUE='-'
  WIDGET_CONTROL, txtDataBeta, SET_VALUE='-'
  WIDGET_CONTROL, txtDataGamma, SET_VALUE='-'
ENDELSE
WIDGET_CONTROL, txtDataTVL, SET_VALUE=STRING(shieldData.(iso).tvl(mat), FORMAT='(f0.2)')
WIDGET_CONTROL, txtDataConstAir, SET_VALUE=STRING(shieldData.(iso).gammaRayConst, FORMAT='(f0.4)')
WIDGET_CONTROL, txtDataConstPat, SET_VALUE=STRING(shieldData.(iso).patientConst, FORMAT='(f0.4)')

currT=shieldDataCT.(kV)
WIDGET_CONTROL, txtDataAlphakV, SET_VALUE=STRING(currT(0,mat), FORMAT='(f0.4)')
WIDGET_CONTROL, txtDataBetakV, SET_VALUE=STRING(currT(1,mat), FORMAT='(f0.4)')
WIDGET_CONTROL, txtDataGammakV, SET_VALUE=STRING(currT(2,mat), FORMAT='(f0.4)')

end