      subroutine BAYUK
     $(X,IX,NSL,IQRK,IQRL)
C
C     Rudolf Loeser, 1989 Jul 31
C---- sets up switch arrays for SALLY.
C     !DASH
      save
C     !DASH
      real*8 X
      integer IQRK, IQRL, IX, J, NSL
C     !DASH
      external SIERRA, HI, BYE
C
      dimension X(*), IX(*)
C
C               IQRK(NSL), IQRL(NSL)
      dimension IQRK(*),   IQRL(*)
C
      call HI ('BAYUK')
C     !BEG
      do 100 J = 1,NSL
        call SIERRA (X,IX,J,IQRK(J),IQRL(J))
  100 continue
C     !END
      call BYE ('BAYUK')
C
      return
      end
