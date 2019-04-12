      subroutine MOTE
     $(LU,NL,NA,KLAB,I,N,A1,LAB1,A2,LAB2,A3,LAB3)
C
C     Rudolf Loeser, 2003 Jun 25
C---- Printing utility for the MYTU package.
C     !DASH
      save
C     !DASH
      real*8 A1, A2, A3
      integer I, J, JE, JS, KLAB, LU, N, NA, NL
      character LAB1*(*), LAB2*(*), LAB3*(*)
C     !DASH
      external  LINER, HI, BYE
      intrinsic min
C
C               A1(N,NL), A2(N,NL), A3(N,NL)
      dimension A1(N,*),  A2(N,*),  A3(N,*)
C
      call HI ('MOTE')
C     !BEG
      if(LU.gt.0) then
        JE = 0
  100   continue
          JS = JE+1
          JE = min((JE+7),NL)
C
          call LINER (1, LU)
C
          if(KLAB.gt.0) then
            write (LU,101) (J,J=JS,JE)
  101       format(' ',15X,7(I14,'/1'))
          else
            write (LU,102) (J,J=JS,JE)
  102       format(' ',15X,7I16)
          end if
C
  103     format(' ',A15,1P7E16.8)
C
          if(NA.ge.1) then
            write (LU,103) LAB1,(A1(I,J),J=JS,JE)
          end if
          if(NA.ge.2) then
            write (LU,103) LAB2,(A2(I,J),J=JS,JE)
          end if
          if(NA.eq.3) then
            write (LU,103) LAB3,(A3(I,J),J=JS,JE)
          end if
C
        if(JE.lt.NL) goto 100
      end if
C     !END
      call BYE ('MOTE')
C
      return
      end
