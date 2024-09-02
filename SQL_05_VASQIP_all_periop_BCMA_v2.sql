-- USE DATABASE_NAME

DECLARE @jh_start	INT = -90
DECLARE @jh_end		INT = 90

DROP TABLE IF EXISTS jh_vasqip_opioid_bcma_rx
SELECT pts.PatientICN, pts.scrssn, pts.oprymd,
	pts.type, bdisp.ActionDateTime,
	datediff(day, pts.OPRYMD, bdisp.ActionDateTime) "date_diff",
	bdisp.DosesOrdered, bdisp.DosesGiven, bdisp.UnitOfAdministration,
	ld.LocalDrugNameWithDose, pharm.MedicationRoute
INTO jh_vasqip_opioid_bcma_rx

FROM jh_vasqip_all_pts pts

JOIN Src.BCMA_BCMADispensedDrug bdisp
	ON bdisp.PatientSID = pts.PatientSID
JOIN CDWWork.Dim.LocalDrug ld
	ON ld.LocalDrugSID = bdisp.LocalDrugSID
JOIN CDWWork.Dim.DrugClass dclass
	ON dclass.DrugClassSID = ld.DrugClassSID
-- adding medication route
JOIN Src.BCMA_BCMAMedicationLog blog
	ON blog.BCMAMedicationLogSID = bdisp.BCMAMedicationLogSID
JOIN CDWWork.Dim.PharmacyOrderableItem pharm	
	ON pharm.PharmacyOrderableItemSID = blog.PharmacyOrderableItemSID
WHERE datediff(day, pts.OPRYMD, bdisp.ActionDateTime) BETWEEN @jh_start AND @jh_end
	AND
	dclass.DrugClassification LIKE 'opioid analgesic%'
