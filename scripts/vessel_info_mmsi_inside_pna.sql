  /* Last run on January 19, 2019 */
SELECT
  mmsi,
  year,
  best_label,
  best_flag AS iso3
FROM
  `world-fishing-827.gfw_research.vessel_info_20181002`
WHERE
  mmsi IN (54822710,
    55311177,
    54892400,
    54890500,
    55311178,
    41243960,
    54889500,
    43272600,
    51005400,
    51006000,
    43270800,
    43116700,
    43150140,
    55311179,
    55311164,
    41600363,
    58477920,
    43241100,
    54805150,
    54893800,
    41243960,
    54892700,
    43110085,
    55311174,
    43139900,
    55311175,
    43284500,
    43186600,
    52900400,
    43112800,
    54842810,
    36627000,
    43115500,
    43294800,
    54848710,
    54805750,
    43241000,
    55311174,
    41600472,
    43130800,
    43115900,
    43167100,
    43181700,
    55311177,
    55311175,
    41221799,
    54822510,
    43150300,
    43248700,
    41623750,
    43254900,
    43296600,
    44180700,
    43160224,
    41600483,
    43170299,
    41600223,
    41337925,
    54805850,
    41650000,
    56301026,
    44320015,
    43170411,
    43268500,
    43138000,
    43181200,
    43297100,
    41689400,
    43131000,
    41622190,
    43243800,
    41221798,
    43168600,
    55311175,
    43150099,
    54868720,
    55311173,
    51006500,
    43262600,
    55311169,
    41656900,
    41624090,
    55311173,
    43151100,
    43133100,
    43167900,
    43294200,
    55311173,
    55311169,
    43256400,
    43167000,
    43110003,
    43118700,
    41641100,
    57690100,
    41604380,
    43165200,
    55311176,
    432261000,
    451004112,
    432549100,
    413320530,
    431435000,
    412439605,
    432738000,
    553111756,
    416532000,
    538060015,
    431595000,
    440583000,
    431932000,
    443179610,
    440640000,
    431787000,
    538060008,
    440339000,
    338074000,
    367767000,
    367170000,
    510056000,
    338540000,
    529435000,
    538060007,
    367179000,
    338394000,
    338793000,
    440822000,
    338298000,
    510070000,
    532461,
    510069000,
    367571490,
    440167000,
    557005600,
    366926988,
    440944000,
    412326972,
    440825000,
    576905000,
    440357000,
    416004043,
    367738980,
    557005700,
    520137000,
    366020000,
    412209234,
    520130000,
    520151000,
    412371197,
    520177000,
    520173000,
    520243000,
    510067000,
    412201818,
    416004214,
    538060019,
    412201817,
    520271000,
    416242900,
    416501000,
    366903000,
    416001500,
    553111759,
    640226000,
    416238800,
    416238700,
    440226000,
    412209233,
    412371182,
    412371181,
    510046000,
    416247600,
    416248700,
    338873000,
    538060016,
    367084000,
    533111749,
    368046000,
    431487000,
    431171000,
    416247900,
    416004000,
    553111735,
    416836000,
    510068000,
    412371192,
    416248600,
    510073000,
    416002545,
    416016600,
    412200116,
    510061000,
    416853000,
    440885000,
    416232600,
    455359000,
    412329416,
    416899000,
    431500430,
    416004369,
    538060017,
    412371198,
    413298000,
    412326882,
    412331063,
    416003561,
    455360000,
    431693000,
    431935000,
    431801000,
    416004583,
    431722000,
    412420796,
    431828000,
    431903000,
    431447000,
    431795000,
    440796000,
    431200970,
    431200910,
    440731000,
    431768000,
    440659000,
    440707000,
    440772000,
    441072000,
    416234800,
    412371222,
    412926000,
    412420918,
    557005500,
    413034000,
    412328731,
    432984000,
    416506000,
    412328729,
    431703710,
    431702850,
    412696240,
    431206000,
    440946000,
    431379000,
    431251000,
    412371207,
    412691530,
    431600275,
    412460279,
    412691520,
    416003822,
    412270024,
    412676430,
    412371206,
    412460278,
    412371208,
    412699510,
    518770000,
    416002195,
    412677470,
    412460296,
    520171000,
    412331011,
    412370006,
    412694570,
    412460297,
    416008800,
    568169862,
    709750116,
    412326877,
    412420239,
    431769000,
    440706000,
    444095214,
    412320019,
    416168500,
    412420911,
    412420914,
    412331027,
    441584000,
    577259000,
    577290000,
    412440029,
    412270001,
    412270059,
    412440032,
    412331031,
    412328796,
    412420915,
    412695550,
    412326798,
    416342000,
    416623000,
    412420197,
    416232500,
    412440031,
    412420876,
    577309000,
    416631000,
    412695580,
    577289000,
    577146000,
    416119800,
    416052600,
    412370008,
    412331014,
    416200071,
    412679240,
    416176700,
    416097500,
    577326000,
    416306430,
    416072600,
    416306451,
    413270220,
    576699000,
    412331029,
    412200079,
    416697000,
    412685220,
    416122800,
    412302021,
    577145000,
    412331026,
    412993000,
    412420985,
    416168600,
    412331032,
    412371178,
    416123900,
    416091900,
    413270430,
    412695630,
    412420905,
    576847000,
    440298000,
    412679230,
    412328714,
    412371191,
    735059088,
    529223000,
    111111777,
    412674190,
    416033600,
    413011000,
    412463904,
    576737000,
    416084500,
    520176000,
    440280000,
    367333000,
    416010900,
    412420435,
    412695430,
    416002469,
    412209183,
    416368000,
    359002000,
    412420848,
    412200078,
    412685210,
    440934000,
    412270058,
    577308000,
    735057638,
    412420851,
    412694640,
    431101320,
    412430940,
    224782000,
    440802000,
    412695560,
    576363000,
    735057587,
    412679180,
    441029000,
    412329461,
    412420862,
    412463901,
    345050700,
    576850000,
    412679210,
    412329426,
    576574000,
    9102875,
    412695650,
    416004594,
    416069700,
    416193900,
    416234900,
    520225000,
    416003955,
    416002681,
    416004044,
    416004268,
    416004246,
    416004453,
    416220800,
    416218900,
    416226600,
    416004073,
    416003814,
    416199800,
    416004158,
    416239700,
    416004582,
    416231500,
    416154700,
    416003089,
    416003074,
    416000235,
    412331122,
    412331123,
    416003741,
    416198800,
    416002768,
    416235500,
    416004299,
    529765000,
    412500156,
    416226700,
    431704420,
    412329403,
    412328759,
    412331103,
    416002374,
    416175800,
    412331102,
    431680000,
    431700200,
    412460287,
    431253000,
    416003676,
    412500157,
    431070000,
    416004248,
    431725000,
    431700970,
    412676580,
    416076700,
    431798000,
    431501259,
    510078000,
    510076000,
    431600760,
    431700030,
    412326881,
    431700060,
    367377660,
    432388000,
    412470420,
    412460286,
    431914000)