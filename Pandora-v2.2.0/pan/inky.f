      subroutine INKY
     $(NO,N,NL,NSL,BDI,BDX,BDIJ,ITS,JBFSW)
C
C     Rudolf Loeser, 1978 May 04
C---- Gives detailed printout, for SPIKE.
C     !DASH
      save
C     !DASH
      real*8 BDI, BDIJ, BDX
      integer I, IB, IE, ITS, JBFSW, N, NL, NO, NSL
C     !DASH
      external  LINER, LILA, FORK, SPOON, KNIFE, HI, BYE
      intrinsic min
C
C               BDI(N,NL), BDX(N,NSL), BDIJ(N,NL-1)
      dimension BDI(*),    BDX(*),     BDIJ(*)
C
      call HI ('INKY')
C     !BEG
      if(NO.gt.0) then
C----   Print heading
        call LILA      (NO,JBFSW,ITS)
        IE = 0
  100   continue
          IB = IE+1
          IE = min(IE+10,N)
          call LINER   (2,NO)
          write (NO,101) (I,I=IB,IE)
  101     format(' ','Depth',2X,10I12)
C----     Print BDI
          call FORK    (NO,IB,IE,N,NL,BDI)
          if(JBFSW.eq.2) then
C----       Print BDX
            call SPOON (NO,IB,IE,N,NL,NSL,BDX)
          end if
C----     Print BDIJ
          call KNIFE   (NO,IB,IE,N,NL,BDIJ)
        if(IE.lt.N) goto 100
      end if
C     !END
      call BYE ('INKY')
C
      return
      end
