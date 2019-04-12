      subroutine TERSE
     $(N,KK,KKPR,Z,XKKA,XKKB,TNU,KOLEV)
C
C     Rudolf Loeser, 1974 Dec 18
C---- Prints output for HELL.
C     (This is version 2 of TERSE.)
C     !DASH
      save
C     !DASH
      real*8 TNU, XKKA, XKKB, Z
      integer I, K1, KK, KKPR, KN, KOLEV, MO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external  LINER, SHIM, HI, BYE
      intrinsic min
C
C               Z(N), XKKA(N,KKX), XKKB(N,KKX), TNU(N,KKX)
      dimension Z(*), XKKA(N,*),   XKKB(N,*),   TNU(N,*)
C
      call HI ('TERSE')
C     !BEG
      if(MO.gt.0) then
        K1 = 1
C
        KN = min(KKPR,KK)
        if(KN.le.0) then
          KN = KK
        end if
C
        call LINER  (2, MO)
        write (MO,100) K1,KN
  100   format(' ',20X,2(9X,'Frequency(',I7,')',11X)/
     $         ' ',23X,34('-'),4X,34('-')/
     $         ' ',9X,'Z',10X,2(7X,'KKA',9X,'KKB',9X,'TNU',4X))
        call LINER  (1, MO)
C
        do 102 I = 1,N
          write (MO,101) Z(I),I,XKKA(I,K1),XKKB(I,K1),TNU(I,K1),
     $                          XKKA(I,KN),XKKB(I,KN),TNU(I,KN)
  101     format(' ',1PE16.8,I4,1X,3E12.4,2X,3E12.4)
          call SHIM (I, 5, MO)
  102   continue
      end if
C     !END
      call BYE ('TERSE')
C
      return
      end
