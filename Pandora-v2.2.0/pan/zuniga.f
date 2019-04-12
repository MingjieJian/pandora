      subroutine ZUNIGA
     $(X,W,IW)
C
C     Rudolf Loeser, 1982 Jun 22
C---- Computes gas parameters,
C     adjusts NH for constant pressure (if needed), and
C     displays results,
C     for Hydrogen runs having no HSE calculation.
C     !DASH
      save
C     !DASH
      real*8 CPRSS, HEABD, W, WPRSS, X, dummy
      integer IEMN, IGD, IHELA, IHNEU, IIPTO, IN, IPNH, IS, IVEC, IVET,
     $        IW, IWS, IXPBL, JJDGM, JJH2N, JJHND, JJMSS, JJPEL, JJPEX,
     $        JJPGS, JJPMG, JJPTO, JJPTU, JJT5, JJTE, JJTKI, JJVM, JJVT,
     $        JJXNE, JJZ, JN, KASE, KODE, KPRSW, KTKIN, LASE, MO, MOX,
     $        MUX, N, jummy
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 55),JJPEL)
      equivalence (IZOQ( 56),JJPGS)
      equivalence (IZOQ( 57),JJPTU)
      equivalence (IZOQ( 58),JJPTO)
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(140),JJH2N)
      equivalence (IZOQ( 49),JJVT )
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(119),JJMSS)
      equivalence (IZOQ( 54),JJT5 )
      equivalence (IZOQ( 75),JJTKI)
      equivalence (IZOQ(205),JJPEX)
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(181),JJVM )
      equivalence (IZOQ(151),JJDGM)
      equivalence (IZOQ(265),JJPMG)
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
      equivalence (RZQ( 86),CPRSS)
      equivalence (RZQ( 87),WPRSS)
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 6),KPRSW)
      equivalence (LEST(11),KTKIN)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external  ROJAS, SORAJ, DAUCUS, MIDGE, POPIO, PONG, HOLDIN, LUNA,
     $          AMAZON, POPUTIL, FRANK, GHASTLY, WGIVE, IGIVE, HI, BYE
      intrinsic min
C
      dimension X(*), W(*), IW(*)
C
      dimension IN(8)
      equivalence
     $(IN( 1),IGD   ),(IN( 2),IHELA ),(IN( 3),IHNEU ),(IN( 4),IXPBL ),
     $(IN( 5),IVEC  ),(IN( 6),IVET  ),(IN( 7),IPNH  ),(IN( 8),IEMN  )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IIPTO )
C
      data LASE /0/
C
      call HI ('ZUNIGA')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call ROJAS    (IN, IS,  MOX, 'ZUNIGA')
      call SORAJ    (JN, IWS, MUX, 'ZUNIGA')
C
C     (Initialize populations buffer)
      call POPIO    ('INIT', jummy, W(IXPBL))
C     !EJECT
C---- Helium abundance
      call FRANK    ('HE ', 0, HEABD, dummy, dummy, dummy, KODE)
      call LUNA     (X, 'HE ', HEABD, W(IHELA))
C---- Pressures, gas density (? and Mach number)
      call GHASTLY  (N, X(JJPEL), X(JJPGS), X(JJPTU), X(JJPEX),
     $               X(JJPMG), W(IPNH), X(JJPTO), X(JJXNE), X(JJTE),
     $               X(JJHND), W(IHELA), X(JJH2N), X(JJVT), X(JJVM),
     $               W(IGD), W(IEMN))
C---- Mass
      call HOLDIN   (N, X(JJPTO), X(JJPMG), X(JJMSS))
C---- Adjust NH
      call AMAZON   (KPRSW, N, CPRSS, WPRSS, X(JJHND), X(JJPTO), KODE)
      if(KODE.eq.1) then
C----   Recompute velocities
        call DAUCUS (X, W)
      end if
C---- Print (using existing procedures)
      KASE = min(KTKIN,1)
      call POPUTIL  (W(IXPBL), 1, 0, dummy, 0, dummy, 1, W(IHNEU), 0,
     $               dummy)
      call PONG     (MO, N, X(JJZ), X(JJHND), X(JJXNE), X(JJPGS),
     $               W(IHNEU), X(JJPTO), W(IGD), X(JJT5), X(JJMSS),
     $               X(JJTE), X(JJVT), X(JJVM), X(JJDGM), X(JJPMG),
     $               X(JJTKI), W(IHELA), W(IVEC), W(IVET), KASE, LASE,
     $               IW(IIPTO))
      call MIDGE    (MO, KODE, CPRSS, WPRSS)
C
C     (Give back W & IW allotments)
      call WGIVE    (W,  'ZUNIGA')
      call IGIVE    (IW, 'ZUNIGA')
C     !END
      call BYE ('ZUNIGA')
C
      return
      end
