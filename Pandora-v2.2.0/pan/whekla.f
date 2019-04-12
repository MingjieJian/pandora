      subroutine WHEKLA
     $(J,XI,A,B,C,D,TE,DUMP,CI)
C
C     Rudolf Loeser, 2004 Jul 28
C---- Computes a value of CI(1,TE) according to
C
C     M. Arnaud & R. Rothenflug 1985, A&A, 60, 425-457.
C
C     (This is version 2 of WHEKLA.)
C     !DASH
      save
C     !DASH
      real*8 A, ARG, B, C, C1, C2, CI, D, DT, F, F1, F2, FAC, ONE, RT,
     $       SE, SUM, TE, TENTH, TRM, TWO, X, X1, XI, ZERO
      integer I, J, LUEO, M
      logical DUMP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(19),TENTH )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external HEESUM, LINER, HI, BYE
C
      dimension XI(3), A(3), B(3), C(3), D(3)
C
      data C1,C2 /0.8365D0, 11605.D0/
      data M /80/
C
      call HI ('WHEKLA')
C     !BEG
      if(DUMP) then
        call LINER  (2, LUEO)
        write (LUEO,100) TE,J,XI,A,B,C,D
  100   format(' ','CI(1) according to Arnaud & Rothenflug.',10X,
     $             'TE =',1PE13.6,5X,'j =',I2,0P/
     $         ' ','I =',3F12.4/
     $         ' ','A =',3F12.4,10X,'B =',3F12.4/
     $         ' ','C =',3F12.4,10X,'D =',3F12.4)
      end if
C
      RT  = sqrt(TE)
      FAC = C1/(RT**3)
C
      X1  = (C2*XI(1))/TE
      SUM = ZERO
      do 102 I = 1,J
        X  = (C2*XI(I))/TE
        DT = TENTH/X
        call HEESUM (X, DT, M, F1, F2)
        F = A(I)*(ONE-X*F1)+B(I)*(ONE+X-X*(TWO+X)*F1)+C(I)*F1+D(I)*X*F2
C
        ARG = X1-X
        SE  = exp(ARG)
        TRM = (SE/X)*F
        SUM = SUM+TRM
C
        if(DUMP) then
          write (LUEO,101) I,X,DT,F1,F2,F,SE,TRM
  101     format(' ','j =',I2,' x =',1PE14.6,', dt =',E14.6,
     $               ', f1(x) =',E14.6,', f2(x) =',E14.6,', F =',E14.6/
     $           ' ','exp =',E14.6,', sum term =',E14.6)
        end if
  102 continue
C
      CI = FAC*SUM
C
      if(DUMP) then
        write (LUEO,103) SUM,FAC,CI
  103   format(' ','sum =',1PE14.6,', factor =',E14.6,', CI(1) =',E14.6)
      end if
C     !END
      call BYE ('WHEKLA')
C
      return
      end
