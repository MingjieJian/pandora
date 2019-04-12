      subroutine TANUNA
     $(LL,N,TNU,TNUSAV,KISSAV,KILSAV,KNT)
C
C     Rudolf Loeser, 2000 Feb 08
C---- Saves data for TNU-analysis.
C     !DASH
      save
C     !DASH
      real*8 TNU, TNUSAV
      integer KASE, KIL, KILSAV, KIS, KISSAV, KNT, LL, N
C     !DASH
      external SOTUR, MOVE1, HI, BYE
C
C               TNUSAV(N,KM), KISSAV(KM), KILSAV(KM), TNU(N)
      dimension TNUSAV(N,*),  KISSAV(*),  KILSAV(*),  TNU(*)
C
      call HI ('TANUNA')
C     !BEG
      call SOTUR   (2,KASE,KIS,KIL)
      if((KASE.ge.1).and.(KASE.le.3)) then
        KNT = KNT+1
        KISSAV(LL) = KIS
        KILSAV(LL) = KIL
        call MOVE1 (TNU,N,TNUSAV(1,LL))
      end if
C     !END
      call BYE ('TANUNA')
C
      return
      end
