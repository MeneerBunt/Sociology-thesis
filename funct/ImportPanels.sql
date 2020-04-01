SELECT
lp.EAN_CODE,
    pir.type,
    pir.bouwjaar,
    lp.geometry.sdo_point.x x,
    lp.geometry.sdo_point.y y 
FROM
    dms_bm.adwh_pir pir
RIGHT JOIN dms_bm.ls_leveringspunten lp ON aan_ean_code = lp.EAN_CODE

