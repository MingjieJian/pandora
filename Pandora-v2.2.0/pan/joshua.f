      subroutine JOSHUA
     $(IU,IL,FLU,DUMP)
C     Rudolf Loeser, 1990 Oct 03
C---- Computes F(l,u), for ion broadening calculation.
C     (This is version 2 of JOSHUA.)
C     !DASH
      save
C     !DASH
      real*8 C1, C2, C3, C4, C5, C6, C7, ELL, ELLOG, ELPOW, F1, FAC,
     $       FLU, R2, R3, YOO
      integer IL, IU, JL, LUEO
      logical DUMP
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external  HALT, LINER, HI, BYE
      intrinsic min, max
C
      dimension R2(20), R3(20)
C
      data R2 / 0.00D0, 0.00D0, 0.00D0, 0.00D0, 7.40D0, 7.12D0, 6.92D0,
     $          6.77D0, 6.64D0, 6.54D0, 6.46D0, 6.38D0, 6.32D0, 6.26D0,
     $          6.21D0, 6.17D0, 6.13D0, 6.09D0, 6.06D0, 6.03D0 /
C
      data R3 / 0.00D0, 0.00D0, 0.00D0, 0.00D0, 2.07D1, 1.95D1, 1.86D1,
     $          1.79D1, 1.73D1, 1.69D1, 1.66D1, 1.63D1, 1.60D1, 1.58D1,
     $          1.56D1, 1.54D1, 1.53D1, 1.51D1, 1.50D1, 1.49D1 /
C
      data C1 / 2.25D0 /
      data C2 / 1.5D0 /
      data C3 / 6.6666666666666667D-1 /
      data C4 / 3.D0 /
      data C5 / 2.71D-1 /
      data C6 / 1.92D0 /
      data C7 / 4.22D-1 /
C     !EJECT
C
      call HI ('JOSHUA')
C     !BEG
      JL = max(min(IL,20),5)
      ELL   = JL
      ELLOG = log(C3*ELL)
      ELPOW = ELL**C7
      F1 = C1*(ELL**2)*(C2+C5*ELLOG+C6/ELPOW)
C
      if(IU.le.IL) then
        write (MSSLIN(1),100) IU,IL
  100   format('Transition indices, IU =',I12,' and IL =',I12,
     $         'are incorrect.')
        call HALT  ('JOSHUA', 1)
C
      else if(IU.eq.(IL+1)) then
        FLU = F1
      else if(IU.eq.(IL+2)) then
        FLU = F1*R2(JL)
      else if(IU.eq.(IL+3)) then
        FLU = F1*R3(JL)
      else
        YOO = IU
        FAC = ((YOO-ELL)/C4)**2
        FLU = F1*R3(JL)*FAC
      end if
C
      if(DUMP) then
        call LINER (1, LUEO)
        write (LUEO,101) IL,IU,FLU,F1
  101   format(' ','F(l,u) = F(',I2,',',I2,') =',1PE14.6,5X,'F1 =',
     $             E14.6)
      end if
C     !END
      call BYE ('JOSHUA')
C
      return
      end
