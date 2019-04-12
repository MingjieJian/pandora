      subroutine CONVERD
     $(A,INCA,NA,B,INCB,NB,DIF,DIFMX,INDMX,SAME)
C     Rudolf Loeser, 1983 Oct 31
C---- Determines whether A and B differ by more than DIF.
C     Given the array A, containing NA items of interest as successive
C     elements whose indices differ by INCA;
C     given the array B, containing NB items of interest as successive
C     elements whose indices differ by INCB;
C     "CONVERD" compares all corresponding pairs:
C               A(1+0*INCA) and B(1+0*INCB),
C               A(1+1*INCA) and B(1+1*INCB),
C               A(1+2*INCA) and B(1+2*INCB), etc,
C     and determines whether they differ relatively by less than DIF.
C---- Upon return,
C     SAME=.true.  if all such pairs differ by less than DIF,
C     SAME=.false. if not all do.
C     Also, DIFMX will be value of the largest relative difference,
C     which obtains between
C               A(1+(INDMX-1)*INCA) and B(1+(INDMX-1)*INCB).
C     !DASH
      save
C     !DASH
      real*8 A, B, DEL, DIF, DIFMX, DIV, HALF, ZERO
      integer I, IA, IB, INCA, INCB, INDMX, M, NA, NB
      logical SAME
C     !DASH
      intrinsic min, abs
C
      dimension A(*),B(*)
C
      data ZERO, HALF /0.D0, 5.D-1/
C
C     !BEG
      SAME  = .true.
      DIFMX = ZERO
      INDMX = 0
      M = min(NA,NB)
      if(M.gt.0) then
        IA = 1-INCA
        IB = 1-INCB
        do 100 I = 1,M
          IA = IA+INCA
          IB = IB+INCB
          DIV = HALF*(abs(A(IA))+abs(B(IB)))
          if(DIV.ne.ZERO) then
            DEL = abs(A(IA)-B(IB))/DIV
            if(DEL.gt.DIFMX) then
              DIFMX = DEL
              INDMX = I
            end if
          end if
  100   continue
        if(DIFMX.gt.DIF) SAME = .false.
      end if
C     !END
C
      return
      end
