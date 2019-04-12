      subroutine DRIP2
     $(N,NL,NO,IETA,TAUK,ERT,S,B1,SLY,XJIK,CQUI,CQSI,KASE,LN,YL,
     $ YPRE,KN,KOLEV,EXLYM,TGLYM)
C
C     Rudolf Loeser, 1975 Jan 02
C---- Produces part of the HAWSER printout.
C     !DASH
      save
C     !DASH
      real*8 B1, CQSI, CQUI, ERT, EXLYM, S, SLY, TAUK, TGLYM, XJIK, YL,
     $       YPRE
      integer I, IETA, KASE, KN, KOLEV, LASE, LN, N, NL, NO
      character BLANK*1, CASE*1, MARC*1, STAR*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(45),STAR  )
C     !DASH
C     !EJECT
      external  ABJECT, LINER, MARKI, SHIM, PLANE, HI, BYE
      intrinsic min, max
C
C               CQUI(N,NL), CQSI(N,NL), B1(N), SLY(N,KKX), XJIK(N,KKX),
      dimension CQUI(N,*),  CQSI(N,*),  B1(*), SLY(N,*),   XJIK(N,*),
C
C               TAUK(N), S(N), ERT(N)
     $          TAUK(*), S(*), ERT(*)
C
      dimension CASE(4)
C
      data CASE /'A', 'B', 'C', '?'/
C
      call HI ('DRIP2')
C     !BEG
      if(NO.gt.0) then
        LASE = min(max(KASE,1),4)
C
        call ABJECT  (NO)
        write (NO,100) KOLEV,KN,KN,KOLEV,KOLEV
  100   format(' ',52X,2(4X,'Source',8X,'Mean',2X)/
     $         ' ',8X,'TNU(1)',38X,2(3X,'Function',4X,'Intensity'),
     $             2X,'-- Ionization Terms --'//
     $         ' ',9X,'TAUK',8X,'ERT',10X,'S',10X,'BD',I2,8X,'SLY(1)',
     $             7X,'J(1)',3X,'SLY(',I7,')',1X,'J(',I7,')',5X,'QU',
     $             I2,8X,'QS',I2)
        call LINER   (1, NO)
C
        do 102 I = 1,N
          call MARKI (I, IETA, MARC, STAR, BLANK)
          write (NO,101) I,MARC,TAUK(I),ERT(I),S(I),B1(I),SLY(I,1),
     $                   XJIK(I,1),SLY(I,KN),XJIK(I,KN),CQUI(I,KOLEV),
     $                   CQSI(I,KOLEV)
  101     format(' ',I3,A1,1P10E12.4)
          call SHIM  (I, 5, NO)
  102   continue
C
        call LINER   (1, NO)
        write (NO,103) CASE(LASE),LN,EXLYM,TGLYM
  103   format(' ',30X,'Case ',A1,',',I3/' ',23X,'EXLYM',1PE12.4/
     $         ' ',23X,'TGLYM',E12.4)
C
        call LINER   (1, NO)
        write (NO,104)
  104   format(' ','   *  S=ERT at this depth and below, where both ',
     $             'TAUK > TGLYM and TAUK*EP1**2 > EXLYM.')
        call PLANE   (NO, YL)
      end if
C     !END
      call BYE ('DRIP2')
C
      return
      end
