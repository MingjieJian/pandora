      subroutine HOTROD
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1973 Oct 24
C---- Drives QUIVER.
C     !DASH
      save
C     !DASH
      real*8 DDT, TLTR, W, WTD, X, YFLUX
      integer IAPD, IBB, ICNXP, ICOEF, IDBH, IG, IH, IHINT, IHK, IHT,
     $        IIMG, IN, IOPAC, IPLANK, IRB, IRJ, IS, ISA, ISS, ITAU,
     $        ITDSTN, ITDSTO, ITW, IVEC, IW, IWS, IX, IXCBL, IXDT,
     $        IXJNU, IYAYB, JJABD, JJADT, JJLDT, JJTDN, JJZ, JN, MDTR1,
     $        MDTR2, MOX, MUX, N, NDT, NTRY1, NTRY2
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(21),NDT)
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 90),JJLDT)
      equivalence (IZOQ( 91),JJADT)
      equivalence (IZOQ( 62),JJTDN)
      equivalence (IZOQ( 92),JJABD)
      equivalence (IZOQ( 37),JJZ  )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 53),DDT  )
      equivalence (KZQ( 30),MDTR1)
      equivalence (KZQ( 31),MDTR2)
      equivalence (RZQ( 50),WTD  )
      equivalence (RZQ( 30),YFLUX)
      equivalence (RZQ( 51),TLTR )
C     !DASH
C     !EJECT
      external MOUND, GISORS, QUIVER, WGIVE, IGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(25)
      equivalence
     $(IN( 1),IXJNU ),(IN( 2),IPLANK),(IN( 3),ITDSTO),(IN( 4),IAPD  ),
     $(IN( 5),IXDT  ),(IN( 6),IRJ   ),(IN( 7),IRB   ),(IN( 8),ICNXP ),
     $(IN( 9),ISS   ),(IN(10),IHINT ),(IN(11),IHT   ),(IN(12),IDBH  ),
     $(IN(13),ICOEF ),(IN(14),IG    ),(IN(15),ITDSTN),(IN(16),IXCBL ),
     $(IN(17),IHK   ),(IN(18),IH    ),(IN(19),IOPAC ),(IN(20),ITAU  ),
     $(IN(21),ITW   ),(IN(22),IVEC  ),(IN(23),IBB   ),(IN(24),IYAYB ),
     $(IN(25),ISA   )
C
      dimension JN(3)
      equivalence
     $(JN( 1),NTRY1 ),(JN( 2),NTRY2 ),(JN( 3),IIMG  )
C
      call HI ('HOTROD')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call MOUND  (IN, IS,  MOX, 'HOTROD')
      call GISORS (JN, IWS, MUX, 'HOTROD')
C
      call QUIVER (NDT, N, DDT, MDTR1, MDTR2, X(JJLDT), X(JJADT),
     $             X(JJABD), W(IXCBL), X(JJTDN), W(ITDSTO), W(ITDSTN),
     $             W(IOPAC), W(ITAU), W(ISS), W(IXJNU), W(ICNXP),
     $             W(IPLANK), W(IAPD), W(IXDT), W(IRJ), W(IRB),
     $             IW(NTRY1), IW(NTRY2), YFLUX, WTD, X(JJZ), W(IHINT),
     $             W(IHT), W(IDBH), W(ICOEF), W(IG), W, IW(IIMG),
     $             W(IHK), W(IH), TLTR, W(ITW), W(IVEC), W(IBB),
     $             W(IYAYB), W(ISA))
C
C     (Give back W & IW allotments)
      call WGIVE  (W,  'HOTROD')
      call IGIVE  (IW, 'HOTROD')
C     !END
      call BYE ('HOTROD')
C
      return
      end
