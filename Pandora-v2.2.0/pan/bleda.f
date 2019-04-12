      subroutine BLEDA
     $(N,XJNUO,SUM1,SUM2,XJNU,ITER)
C
C     Rudolf Loeser, 1982 Feb 05
C---- Prints debug data for angle-dependent Continuum Calculations.
C     !DASH
      save
C     !DASH
      real*8 SUM1, SUM2, XJNU, XJNUO
      integer ITER, LUEO, N
      character LINE*127
C     !DASH
      external  LINER, LABFIL, VECOUT, HI, BYE
C
C               XJNUO(N), SUM1(N), SUM2(N), XJNU(N)
      dimension XJNUO(*), SUM1(*), SUM2(*), XJNU(*)
C
      call HI ('BLEDA')
C     !BEG
      call LINER  (2, LUEO)
      write (LUEO,100) ITER
  100 format(' ','Iterative calculation of Jnu; ITER =',I2)
C
      call LABFIL ('Jnu-old'                    , LINE)
      call VECOUT (LUEO, XJNUO, N, LINE)
C
      call LABFIL ('Angle-integral in numerator', LINE)
      call VECOUT (LUEO, SUM1 , N, LINE)
C
      call LABFIL ('Angle-integral of XA'       , LINE)
      call VECOUT (LUEO, SUM2 , N, LINE)
C
      call LABFIL ('Jnu-new'                    , LINE)
      call VECOUT (LUEO, XJNU , N, LINE)
C     !END
      call BYE ('BLEDA')
C
      return
      end
