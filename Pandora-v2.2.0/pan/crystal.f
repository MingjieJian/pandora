      subroutine CRYSTAL
     $(X,IX,W,IW,N,NL,CHI,ASTAR,SA,BDU,BDQ,IMG)
C
C     Rudolf Loeser, 2003 Mar 13
C---- Supervises the calculation of BDQ: b-ratios from CHI = JBAR-S.
C     !DASH
      save
C     !DASH
      real*8 ASTAR, BDQ, BDU, CHI, RACK, SA, W, X, dummy
      integer IFO, IFUJ, IINVA, IMATA, IMG, IN, ITAU, ITUS, IVECC, IW,
     $        IWS, IX, JNEG, KMAT, KODE, KRJ, ML, MOX, N, NL
      logical EDITED, KILROY
      character LABEL*33, LEGEND*33
C     !DASH
      external LINEN, CORON, LARYS, MOVE1, CALYX, YUROK, TSUNI, ALARIC,
     $         MALDEN, WGIVE, MASHED, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               BDQ(N,NL), BDU(N,NL), SA(N,NT), CHI(N,NT), ASTAR(N,NT),
      dimension BDQ(*),    BDU(*),    SA(*),    CHI(*),    ASTAR(*),
C
C               IMG(N)
     $          IMG(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IVECC ),(IN( 2),IMATA ),(IN( 3),IINVA ),(IN( 4),IFO   )
C
      data LABEL    /'Matrix a for b-ratios from CHI'/
      data IFUJ     /0/
      data KRJ,ITUS /4, 0/
C
      call HI ('CRYSTAL')
C     !BEG
C     (Get, and allocate, W allotment)
      call LARYS  (IN, IWS, MOX, 'CRYSTAL')
C
      call MALDEN (KRJ, ITUS, LEGEND)
C     !EJECT
      KILROY = .true.
      ML     = NL-1
      do 100 ITAU = 1,N
C----   Compute matrix a and vector c
        call CALYX   (X, N, NL, ML, ITAU, CHI, ASTAR, SA, W(IMATA),
     $                W(IVECC))
C----   Save and invert a
        call MOVE1   (W(IMATA), (ML*ML), W(IINVA))
        call TSUNI   (W, IW, ML, W(IINVA), W(IMATA), ITAU, LABEL,
     $                KMAT)
C----   Compute basic b-ratios
        call YUROK   (ML, NL, ITAU, N, W(IINVA), KMAT, W(IVECC),
     $                BDU, JNEG, RACK)
C
C----   (? printout; to LUEO)
        call CORON   (ITAU, JNEG, KODE)
        if(KODE.gt.0) then
          call LINEN (KODE, ITAU, JNEG, LEGEND, NL, dummy, ML,
     $                W(IMATA), W(IVECC), W(IINVA), dummy, RACK, 2,
     $                KILROY)
        end if
  100 continue
C---- (? printout trailer)
      if(.not.KILROY) then
        call MASHED  ('LINEN')
      end if
C
C---- Edit out negatives
      EDITED = .false.
      call MOVE1     (BDU, (N*NL), BDQ)
      call ALARIC    (BDQ, IMG, W(IFO), N, NL, IFUJ, LEGEND, EDITED)
C
C     (Give back W allotment)
      call WGIVE     (W, 'CRYSTAL')
C     !END
      call BYE ('CRYSTAL')
C
      return
      end
