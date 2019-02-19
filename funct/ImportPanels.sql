SELECT
    pir.pi_ean_code,
    pir.vermogen, 
    pir.type,
    pir.bouwjaar,
    pir.aan_ean_code,
    lp.geometry.sdo_point.x x,
    lp.geometry.sdo_point.y y 
FROM
    dms_bm.adwh_pir pir
LEFT JOIN dms_bm.ls_leveringspunten lp ON aan_ean_code = lp.EAN_CODE