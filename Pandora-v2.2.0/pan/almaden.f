      subroutine ALMADEN
     $(NO,WT,NW,N,COEF,YFLUX)
C
C     Rudolf Loeser, 1984 Jun 21
C---- Prints, for TWINK.
C     (Special version of "BENITO".)
C     !DASH
      save
C     !DASH
      real*8 COEF, WT, YFLUX
      integer I, IC, IM, J, JE, JS, N, NO, NW
C     !DASH
      external  LINER, FLIPSD, HI, BYE
      intrinsic min
C
C               WT(Nmkuse), COEF(N,Nmkuse)
      dimension WT(*),      COEF(N,*)
C
      dimension IC(12)
C
      call HI ('ALMADEN')
C     !BEG
      if(NO.gt.0) then
        call LINER (4, NO)
        write (NO,100) YFLUX
  100   format(' ','Quadratic coefficients, computed with Y =',F7.4)
C
        JE = 0
  101   continue
          JS = JE+1
          JE = min((JE+12),NW)
          call LINER (2, NO)
          write (NO,102) (WT(J),J=JS,JE)
  102     format(' ',4X,1P12E10.2)
          call LINER (1, NO)
C
          do 104 I = 1,N
            write (NO,103) I,(COEF(I,J),J=JS,JE)
  103       format(' ',I4,1P12E10.2)
  104     continue
          IM = 0
          do 105 J = JS,JE
            IM = IM+1
            call FLIPSD (COEF(1,J), 1, N, IC(IM))
  105     continue
          write (NO,106) (IC(I),I=1,IM)
  106     format(' ',4X,12I10)
C
          if(JE.lt.NW) goto 101
        continue
      end if
C     !END
      call BYE ('ALMADEN')
C
      return
      end
