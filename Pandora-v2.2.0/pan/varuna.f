      subroutine VARUNA
     $(NO,N,TE,XNE,HN1,HNK,HE1N1,HE2N1,HE2NK,XLR,XLE,XLHE,XLH,XLT)
C
C     Rudolf Loeser, 1981 Feb 02
C---- Prints details of thermal conductivity calculation.
C     !DASH
      save
C     !DASH
      real*8 HE1N1, HE2N1, HE2NK, HN1, HNK, TE, XLE, XLH, XLHE, XLR,
     $       XLT, XNE
      integer I, N, NO
C     !DASH
      external ABJECT, VANUA, LINER, HI, BYE
C
C               HE2NK(N), XLHE(N), HN1(N), HNK(N), HE1N1(N), HE2N1(N),
      dimension HE2NK(*), XLHE(*), HN1(*), HNK(*), HE1N1(*), HE2N1(*),
C
C               XLR(N), XLE(N), XNE(N), XLH(N), XLT(N), TE(N)
     $          XLR(*), XLE(*), XNE(*), XLH(*), XLT(*), TE(*)
C
      call HI ('VARUNA')
C     !BEG
      if(NO.gt.0) then
        call ABJECT (NO)
        call VANUA  (NO, N, TE, XNE)
        call LINER  (4, NO)
        write (NO,100)
  100   format(' ','Details of Thermal Conductivity calculation.'//
     $         ' ',11X,'TE',8X,'NE',7X,'HN1',7X,'HNK',
     $             5X,'HE1N1',5X,'HE2N1',5X,'HE2NK',
     $             9X,'LR',8X,'LE',7X,'LHE',8X,'LH',8X,'LT')
        call LINER  (1, NO)
        write (NO,101) (I,TE(I),XNE(I),HN1(I),HNK(I),HE1N1(I),HE2N1(I),
     $                    HE2NK(I),XLR(I),XLE(I),XLHE(I),XLH(I),XLT(I),
     $                    I=1,N)
  101   format(5(' ',I3,1P7E10.3,1X,5E10.3/))
        call LINER  (3, NO)
        write (NO,102)
  102   format(' ','LR, LE, LHE, LH, and LT are the reactive, ',
     $             'electron, Helium, Hydrogen, and total ',
     $             'conductivities, respectively,'/
     $          ' ','from: T. Nowak and P. Ulmschneider, '
     $              'Astron.Astrophys. 60, 413 (1977).')
      end if
C     !END
      call BYE ('VARUNA')
C
      return
      end
