-- USE DATABASE_NAME

DECLARE @jh_start	INT = -90
DECLARE @jh_end		INT = 365

-- added IssueDate, MaxDosePerDay, UnitDoseSchedule
-- added MedRoute from RxOutpatMedInstructions table

-- saves jh_vasqip_opioid_rx

DROP TABLE IF EXISTS jh_vasqip_opioid_rx
SELECT pts.PatientICN, pts.scrssn, pts.oprymd, pts.type, rx.DispensedDate, rx.IssueDate, rx.RxOutpatFillSID,
		datediff(day, pts.OPRYMD, rx.DispensedDate) "date_diff",
		rx.Qty, rx.QtyNumeric, rx.DaysSupply,
		ld.LocalDrugNameWithDose, ld.Strength, rxsig.Sig, ld.MaxDosePerDay, ld.UnitDoseSchedule, rxI.MedRoute,
		rx.FillType, rx.FillNumber
INTO jh_vasqip_opioid_rx
FROM jh_vasqip_all_pts pts

-- join on RxOutpat, LocalDrug, and DrugClass
JOIN Src.RxOut_RxOutpatFill rx
	ON pts.PatientSID = rx.PatientSID
JOIN CDWWork.Dim.LocalDrug ld
	ON ld.LocalDrugSID = rx.LocalDrugSID
JOIN CDWWork.Dim.DrugClass dclass
	ON dclass.DrugClassSID = ld.DrugClassSID
JOIN Src.RxOut_RxOutpatSig rxsig
	ON rxsig.RxOutpatSID = rx.RxOutpatSID
-- adding RxOutpatMedInstructions for MedRoute
JOIN Src.RxOut_RxOutpatMedInstructions rxI
	ON rxI.RxOutpatSID = rx.RxOutpatSID

-- where selection clause
WHERE datediff(day, pts.OPRYMD, rx.DispensedDate) BETWEEN @jh_start AND @jh_end
	AND
	dclass.DrugClassification LIKE 'opioid analgesic%'