
SELECT
    duurzame_energie,
    straatnaam,
    huisnummer,
    huisnummer_toevoeging,
    postcode,
    woonplaats,
    gemeente,
    BOUWJAAR,
    lokatie_x as X,
    lokatie_y as Y
FROM
    dms_bm.adwh_aansluitingen ad
LEFT JOIN dms_bm.adwh_pir pir on pir.AAN_EAN_CODE =  ad.EAN_CODE