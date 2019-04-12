      subroutine VISTA
     $(NO,N,XKL,SL,COP,CSF,XKT,ST,LINE)
C
C     Rudolf Loeser, 2004 Jul 01
C---- Prints details of ST.
C     (This is version 2 of VISTA.)
C     !DASH
      save
C     !DASH
      real*8 COP, CSF, DIV, SL, ST, XCON, XKL, XKT, XLIN
      integer I, N, NO
      character LINE*(*)
C     !DASH
      external DIVIDE, LINER, SHIM, HI, BYE
C
C               XKL(N), COP(N), CSF(N), XKT(N), SL(N), ST(N)
      dimension XKL(*), COP(*), CSF(*), XKT(*), SL(*), ST(*)
C
      call HI ('VISTA')
C     !BEG
      if(NO.gt.0) then
        call LINER    (3, NO)
        write (NO,100) LINE
  100   format(' ','Details of opacity-weighted frequency-independent ',
     $             '"total source function" ST = (KPC*CSF + KL*SL) / ',
     $             '(KPC + KL)'/
     $         ' ','        where ',A//
     $         ' ',8X,'----------- Opacity ----------',
     $             3X,'------- Source Function ------',
     $             20X,'--- Fraction of ST from  ---'/
     $         ' ',14X,'KPC',14X,'KL',14X,'CSF',13X,'SL',13X,'ST',
     $             15X,'Cont',11X,'Line')
        call LINER    (1, NO)
C
        do 102 I = 1,N
          DIV = (XKT(I)*ST(I))
          call DIVIDE ((COP(I)*CSF(I)), DIV, XCON)
          call DIVIDE ((XKL(I)*SL(I)),  DIV, XLIN)
          write (NO,101) I,COP(I),XKL(I),CSF(I),SL(I),ST(I),XCON,XLIN
  101     format(' ',I5,1X,1P2E16.8,1X,3E16.8,1X,0P2F15.10)
          call SHIM   (I, 5, NO)
  102   continue
      end if
C     !END
      call BYE ('VISTA')
C
      return
      end
