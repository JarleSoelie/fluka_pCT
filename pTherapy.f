*$ CREATE SOURCE.FOR
*COPY SOURCE
*
*=== source ===========================================================*
*
      SUBROUTINE SOURCE ( NOMORE )

      INCLUDE 'dblprc.inc'
      INCLUDE 'dimpar.inc'
      INCLUDE 'iounit.inc'
      INCLUDE 'beamcm.inc'
      INCLUDE 'caslim.inc'
      INCLUDE 'fheavy.inc'
      INCLUDE 'flkstk.inc'
      INCLUDE 'ioiocm.inc'
      INCLUDE 'ltclcm.inc'
      INCLUDE 'paprop.inc'
      INCLUDE 'sourcm.inc'
      INCLUDE 'sumcou.inc'
*----------------------------------------------------------------------*
*  
*
      DIMENSION CUMPR(0:51), ENEDGE(52)
*these corresponds to 50 entries in both CUMPR and ENEDGE
*----------------------------------------------------------------------*
*
*
      LOGICAL LFIRST
      SAVE LFIRST
      DATA LFIRST / .TRUE. /
*----------------------------------------------------------------------*
*   Proton energy group boundaries
      DATA ENEDGE /
     & 77.1182773886851E-03,
     & 78.0085858944591E-03,
     & 78.8915291193969E-03,
     & 79.7673079123845E-03,
     & 80.6361139727838E-03,
     & 81.4981304283696E-03,
     & 82.3535323669501E-03,
     & 83.2024873261408E-03,
     & 84.0451557452616E-03,
     & 84.8816913828841E-03,
     & 85.7122417031796E-03,
     & 86.5369482338763E-03,
     & 87.3559468983426E-03,
     & 88.1693683240534E-03,
     & 88.9773381294647E-03,
     & 89.7799771911226E-03,
     & 90.577401892649E-03,
     & 91.36972435709E-03,
     & 92.1570526639697E-03,
     & 92.9394910522649E-03,
     & 93.7171401104059E-03,
     & 94.4900969543033E-03,
     & 95.2584553943155E-03,
     & 96.0223060919857E-03,
     & 96.7817367073076E-03,
     & 97.5368320372116E-03,
     & 98.2876741459047E-03,
     & 99.0343424876438E-03,
     & 99.7769140224747E-03,
     & 100.515463325424E-03,
     & 101.250062689592E-03,
     & 101.98078222356E-03,
     & 102.70768994349E-03,
     & 103.430851860271E-03,
     & 104.150332062029E-03,
     & 104.866192792303E-03,
     & 105.578494524163E-03,
     & 106.287296030525E-03,
     & 106.992654450899E-03,
     & 107.694625354784E-03,
     & 108.393262801931E-03,
     & 109.088619399638E-03,
     & 109.780746357275E-03,
     & 110.469693538187E-03,
     & 111.155509509138E-03,
     & 111.838241587432E-03,
     & 112.517935885843E-03,
     & 113.194637355482E-03,
     & 113.868389826706E-03,
     & 114.539236048197E-03,
     & 115.207217724285E-03,
     & 115.872375550633E-03/

*   Cumulative spectrum
      DATA CUMPR / 0.D0,
*...+....1....+....2....+....3....+....4....+....5....+....6....+....7..
     & 0.003761782646171,
     & 0.011357217110222,
     & 0.019051173490196,
     & 0.026847045044525,
     & 0.034748417919131,
     & 0.042759086624975,
     & 0.050883071136116,
     & 0.059124635819418,
     & 0.067488310440235,
     & 0.075978913527693,
     & 0.084601578430005,
     & 0.093361782446104,
     & 0.10226537948698,
     & 0.111318636800872,
     & 0.120528276394369,
     & 0.129901521900501,
     & 0.139446151790596,
     & 0.149170560005786,
     & 0.159083825305684,
     & 0.169195790907599,
     & 0.179517156335242,
     & 0.19005958383185,
     & 0.200835822246695,
     & 0.211859852013537,
     & 0.223147055756059,
     & 0.234714420249635,
     & 0.246580777040109,
     & 0.258767091109207,
     & 0.271296809784166,
     & 0.284196287909703,
     & 0.297495310566906,
     & 0.311227741987737,
     & 0.325432339773053,
     & 0.340153788634234,
     & 0.355444030131292,
     & 0.371363998344268,
     & 0.387985922934853,
     & 0.405396442498639,
     & 0.423700903771731,
     & 0.443029445886872,
     & 0.463545861089213,
     & 0.485460943860131,
     & 0.509053439075702,
     & 0.534704602057566,
     & 0.56295892381017,
     & 0.59463995355789,
     & 0.631097358471131,
     & 0.674827543687882,
     & 0.731514725871152,
     & 0.822172058996108,
     & 1/

*----------------------------------------------------------------------*
*...+....1....+....2....+....3....+....4....+....5....+....6....+....7..
*----------------------------------------------------------------------*
      NOMORE = 0
*  +-------------------------------------------------------------------*
*  |  First call initializations:
      IF ( LFIRST ) THEN
*  |  *** The following 3 cards are mandatory ***
        TKESUM = ZERZER
        LFIRST = .FALSE.
        LUSSRC = .TRUE.
*  |  *** User initialization ***
      END IF
*  |
*  +-------------------------------------------------------------------*
*  Sample the energy group
      XI = FLRNDM(DUMMY)
      DO 500 K = 1, 52 
        IF(XI .LE. CUMPR(K)) THEN
          ENERGY = ENEDGE(K) - 
     &    (XI-CUMPR(K-1))*(ENEDGE(K)-ENEDGE(K+1))/(CUMPR(K)-CUMPR(K-1))
          GO TO 600          
        END IF
 500  CONTINUE
      STOP ' Failed to sample the energy group'
 600  CONTINUE
      kount=kount+1
*

*  Npflka is the stack counter: of course any time source is called it
*  must be =0
      NPFLKA = NPFLKA + 1
* Wt is the weight of the particle
      WTFLK (NPFLKA) = ONEONE
      WEIPRI = WEIPRI + WTFLK (NPFLKA)
* Particle type (1=proton.....). Ijbeam is the type set by the BEAM
* card
*  +-------------------------------------------------------------------*
*  |  Heavy ion:
      IF ( IJBEAM .EQ. -2 ) THEN
         IJHION = IPROZ  * 1000 + IPROA
         IJHION = IJHION * 100 + KXHEAV
         IONID  = IJHION
         CALL DCDION ( IONID )
         CALL SETION ( IONID )
         ILOFLK (NPFLKA) = IJHION
*  |
*  +-------------------------------------------------------------------*
*  |  Normal hadron:
      ELSE
         IONID  = IJBEAM
         ILOFLK (NPFLKA) = IJBEAM
      END IF
*  |
*  +-------------------------------------------------------------------*
* From this point .....
* Particle generation (1 for primaries)
      LOFLK (NPFLKA) = 1
* User dependent flag:
      LOUSE  (NPFLKA) = 0
* User dependent spare variables:
      DO 100 ISPR = 1, MKBMX1
         SPAREK (ISPR,NPFLKA) = ZERZER
 100  CONTINUE
* User dependent spare flags:
      DO 200 ISPR = 1, MKBMX2
         ISPARK (ISPR,NPFLKA) = 0
 200  CONTINUE
* Save the track number of the stack particle:
      ISPARK (MKBMX2,NPFLKA) = NPFLKA
      NPARMA = NPARMA + 1
      NUMPAR (NPFLKA) = NPARMA
      NEVENT (NPFLKA) = 0
      DFNEAR (NPFLKA) = +ZERZER
* ... to this point: don't change anything
* Particle age (s)
      AGESTK (NPFLKA) = +ZERZER
      AKNSHR (NPFLKA) = -TWOTWO
* Group number for "low" energy neutrons
      IGROUP (NPFLKA) = NEUGRP
* Kinetic energy of the particle (GeV)
      TKEFLK (NPFLKA) = ENERGY
* Particle momentum
      PMOFLK (NPFLKA) = SQRT ( TKEFLK (NPFLKA) * ( TKEFLK (NPFLKA)
     *                       + TWOTWO * AM (ILOFLK(NPFLKA)) ) )
*  Cosines (tx,ty,tz)
      TXFLK  (NPFLKA) = UBEAM
      TYFLK  (NPFLKA) = VBEAM
      TZFLK  (NPFLKA) = WBEAM
*     TZFLK  (NPFLKA) = SQRT ( ONEONE - TXFLK (NPFLKA)**2
*    &                       - TYFLK (NPFLKA)**2 )
* Polarization cosines:
      TXPOL (NPFLKA) = -TWOTWO
      TYPOL (NPFLKA) = +ZERZER
      TZPOL (NPFLKA) = +ZERZER
* Particle coordinates
* cover the volume in X and Y direction
* Gaussian sigma 0.297(=0.7FWHM) to account for lateral spread of beam
      CALL FLNRR2(RGAUS1, RGAUS2)
      XFLK (NPFLKA) = (-2.7 + 0.297*RGAUS1)+ FLRNDM(DUMMY)*5.4
      YFLK (NPFLKA) = (-2.7 + 0.297*RGAUS2)+ FLRNDM(DUMMY)*5.4
      ZFLK (NPFLKA) = ZBEAM
*  Calculate the total kinetic energy of the primaries: don't change
      IF ( ILOFLK (NPFLKA) .EQ. -2 .OR. ILOFLK (NPFLKA) .GT. 100000 )
     &   THEN
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
      ELSE IF ( ILOFLK (NPFLKA) .NE. 0 ) THEN
         TKESUM = TKESUM + ( TKEFLK (NPFLKA) + AMDISC (ILOFLK(NPFLKA)) )
     &          * WTFLK (NPFLKA)
      ELSE
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
      END IF
*  Flag this is prompt radiation
      LRADDC (NPFLKA) = .FALSE.
      RADDLY (NPFLKA) = ZERZER
*  Here we ask for the region number of the hitting point.
*     NREG (NPFLKA) = ...
*  The following line makes the starting region search much more
*  robust if particles are starting very close to a boundary:
      CALL GEOCRS ( TXFLK (NPFLKA), TYFLK (NPFLKA), TZFLK (NPFLKA) )
      CALL GEOREG ( XFLK  (NPFLKA), YFLK  (NPFLKA), ZFLK  (NPFLKA),
     &              NRGFLK(NPFLKA), IDISC )
*  Do not change these cards:
      CALL GEOHSM ( NHSPNT (NPFLKA), 1, -11, MLATTC )
      NLATTC (NPFLKA) = MLATTC
      CMPATH (NPFLKA) = ZERZER
      CALL SOEVSV
      RETURN
*=== End of subroutine Source =========================================*
      END
