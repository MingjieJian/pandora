      subroutine RAISES
     $(DELTA,KOLEV,RRNU,WRAT,XNU,XNUC,LL,FAC,XK,KK)
C
C     Rudolf Loeser, 2005 Aug 10
C---- Makes sure no RRNU value equals any XK value.
C     Assumes that all RRNU values are distinct, and that
C     all XK values are distinct.
C     (This is version 2 of RAISES.)
C     !DASH
      save
C     !DASH
      real*8 DELTA, FAC, RRNU, WRAT, XK, XNU, XNUC
      integer K, KEQ, KK, KOLEV, L, LL
C     !DASH
      external COMPD, NOMAD, HI, BYE
C
C               MRZ = MRS+NSL+1
C
C               XK(KK), RRNU(MRZ), WRAT(MRZ), XNU(NSL), XNUC(NSL)
      dimension XK(*),  RRNU(*),   WRAT(*),   XNU(*),   XNUC(*)
C
      call HI ('RAISES')
C     !BEG
      do 101 L = 1,LL
        do 100 K = 1,KK
          call COMPD   (RRNU(L), XK(K), DELTA, KEQ)
          if(KEQ.eq.0) then
            RRNU(L) = FAC*RRNU(L)
            call NOMAD (RRNU(L), XNU, XNUC, KOLEV, WRAT(L))
            goto 101
          end if
  100   continue
  101 continue
C     !END
      call BYE ('RAISES')
C
      return
      end
