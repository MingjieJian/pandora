      subroutine LORCA
     $(NO,N,NT,INDEX,THETA,PE,PFO,PFE,PFT)
C
C     Rudolf Loeser, 2001 Jun 26
C---- Prints, for CORAL.
C     !DASH
      save
C     !DASH
      real*8 PE, PFE, PFO, PFT, THETA, dummy
      integer INDEX, N, NO, NT
C     !DASH
      external JABIRU, GRANADA, ABJECT, JACANA, MALAGA, LINER, HI, BYE
C
C               PFT(N,NMT), PFE(N,NT), PFO(N,NT), INDEX(NMT,2), PE(N),
      dimension PFT(*),     PFE(*),    PFO(*),    INDEX(*),     PE(*),
C
C               THETA(N)
     $          THETA(*)
C
      call HI ('LORCA')
C     !BEG
      if(NO.gt.0) then
        call LINER   (3,NO)
        call JABIRU  (NO)
        call GRANADA (NO,N,NT,INDEX,PFO,PFO,THETA,PE   ,1)
        call ABJECT  (NO)
        call JACANA  (NO)
        call GRANADA (NO,N,NT,INDEX,PFE,PFO,dummy,dummy,0)
        call ABJECT  (NO)
        call MALAGA  (NO,N,PFT)
      end if
C     !END
      call BYE ('LORCA')
C
      return
      end
