-- USE DATABASE_NAME

DECLARE @jh_tmp1	TINYINT = 1	-- pull VS for case and control
DECLARE @jh_tmp2	TINYINT = 0 -- blank
DECLARE @jh_lo		INT = -182
DECLARE @jh_hi		INT = 182

/* How to select vital signs
SELECT d.VitalResultNumeric, d.VitalResult, vt.VitalType, vt.VitalTypeAbbreviation
FROM (	SELECT TOP (100) *
		FROM Src.Vital_VitalSign) d
JOIN CDWWork.Dim.VitalType vt
	ON vt.VitalTypeSID = d.VitalTypeSID
*/
IF @jh_tmp1 = 1 BEGIN
	DROP TABLE IF EXISTS jh_bup_surgery_pain
	SELECT pts.scrssn, pts.PatientICN, pts.oprymd, vs.VitalSignEnteredDateTime, vt.VitalType, vs.VitalResult, vs.VitalResultNumeric
	INTO jh_bup_surgery_pain
	FROM jh_vasqip_all_pts pts
	JOIN Src.Vital_VitalSign vs
		ON vs.PatientSID = pts.PatientSID
	JOIN CDWWork.Dim.VitalType vt
		ON vs.VitalTypeSID = vt.VitalTypeSID
	WHERE vt.VitalType LIKE 'pain%' AND
		datediff(day, vs.VitalSignEnteredDateTime, pts.oprymd) BETWEEN @jh_lo AND @jh_hi
END

