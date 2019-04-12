      subroutine AMAZON
     $(KPRSW,N,CPRSS,WPRSS,HND,PTO,KODE)
C
C     Rudolf Loeser, 1984 Oct 17
C---- Adjusts NH for constant pressure.
C     !DASH
      save
C     !DASH
      real*8 CPRSS, HND, ONE, OW, PTO, RAT, WC, WPRSS, ZERO
      integer I, KODE, KPRSW, N
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
      external DIVIDE, SABOT, WENDY, HI, BYE
C
C               HND(N), PTO(N)
      dimension HND(*), PTO(*)
C
      call HI ('AMAZON')
C     !BEG
      KODE = 0
      if((KPRSW.gt.0).and.(WPRSS.ne.ZERO)) then
        KODE = 1
C
C----   Recompute NH
        WC = WPRSS*CPRSS
        OW = ONE-WPRSS
        do 100 I = 1,N
          call DIVIDE (WC,PTO(I),RAT)
          HND(I) = HND(I)*(RAT+OW)
  100   continue
C
C----   Save for iterative summary
        call SABOT    (HND)
C----   Continuum Recalculation control
        call WENDY    (HND,1,N,4,'AMAZON')
      end if
C     !END
      call BYE ('AMAZON')
C
      return
      end
