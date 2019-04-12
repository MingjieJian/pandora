      subroutine EGG
     $(KILROY,N,LU,K,J,ICE,DL,PHI,GTN,S,XQSF,VXI,XRD,YRD,
     $ ZDL,ZABS,ZSCA,ZSCR,ZBNM,ZBDN,ZAXA,ZAYA)
C
C     Rudolf Loeser, 1977 Jul 05
C---- Sets up Continuum Block contents, for DIOCLES.
C     !DASH
      save
C     !DASH
      real*8 DL, GTN, ONE, PHI, S, VXI, XKL, XQSF, XRD, XS, YRD, ZABS,
     $       ZAXA, ZAYA, ZBDN, ZBNM, ZDL, ZERO, ZSCA, ZSCR, dummy
      integer I, ICE, J, K, LU, N
      logical KHOLD, KILROY, PRINT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ABJECT, LINER, VITIM, CREEP, HI, BYE
C
C               ZABS(N), ZSCA(N), ZSCR(N), XQSF(N), ZBNM(N), ZBDN(N),
      dimension ZABS(*), ZSCA(*), ZSCR(*), XQSF(*), ZBNM(*), ZBDN(*),
C
C               VXI(N), PHI(N), GTN(N), ZAXA(N), ZAYA(N), S(N), DL(K),
     $          VXI(*), PHI(*), GTN(*), ZAXA(*), ZAYA(*), S(*), DL(*),
C
C               XRD(N), YRD(N)
     $          XRD(*), YRD(*)
C
      call HI ('EGG')
C     !BEG
      ZDL   = DL(J)
      KHOLD = ICE.eq.1
      do 100 I = 1,N
        XKL     = PHI(I)*GTN(I)
        ZABS(I) = XKL
        if(KHOLD) then
          ZSCA(I) = XKL*VXI(I)
          ZSCR(I) = XKL*(ONE-VXI(I))
          ZBNM(I) = XKL*XQSF(I)
          ZAXA(I) = VXI(I)
          ZAYA(I) = XQSF(I)
        else
          XS      = XRD(I)*S(I)+YRD(I)
          ZSCA(I) = ZERO
          ZSCR(I) = XKL
          ZBNM(I) = XKL*XS
          ZAXA(I) = ZERO
          ZAYA(I) = XS
        end if
        ZBDN(I) = ZSCR(I)
  100 continue
C     !EJECT
      if(LU.gt.0) then
        if(KILROY) then
          KILROY = .false.
          call ABJECT (LU)
          call LINER  (1, LU)
          write (LU,101)
  101     format(' ','PRD-related terms for the Background ',
     $               'Calculations that follow.')
        end if
C
        call VITIM    (LU, 0, K, J, DL, dummy, dummy, PRINT)
        if(PRINT) then
          call CREEP  (LU, N, ZABS, ZSCA, ZSCR, ZBNM, ZBDN, ZAXA, ZAYA)
        end if
      end if
C     !END
      call BYE ('EGG')
C
      return
      end
