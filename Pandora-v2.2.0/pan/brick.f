      subroutine BRICK
     $(N,NL,BDI,BDO,CONV,DIFMX,IMX,JMX,SAME)
C
C     Rudolf Loeser, 1978 May 04
C---- Examines differences between BDI and BDO, and returns with
C     KODE=1 if DIFMX, the largest relative difference, has magnitude
C     less than CONV, but with KODE=0 otherwise.
C     !DASH
      save
C     !DASH
      real*8 BDI, BDO, CONV, DIFMX
      integer IMX, INDMX, JMX, N, NL
      logical SAME
C     !DASH
      external CONVERD, HI, BYE
C
C               BDI(N,NL), BDO(N,NL)
      dimension BDI(*),    BDO(*)
C
      call HI ('BRICK')
C     !BEG
      call CONVERD (BDI,1,(N*NL),BDO,1,(N*NL),CONV,DIFMX,INDMX,SAME)
      if(.not.SAME) then
        JMX = INDMX/N
        IMX = INDMX-N*JMX
      end if
C     !END
      call BYE ('BRICK')
C
      return
      end
