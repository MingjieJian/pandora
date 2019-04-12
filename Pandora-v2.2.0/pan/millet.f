      subroutine MILLET
     $(LU,N,NLH,AEL,ZMER,RZM,HND,ZHEL,XNC,XNE,SA,HNP,SUM,BD1)
C
C     Rudolf Loeser, 1994 May 19
C     Revised RL/SGK Apr  9 2014 
C---- Prints results from NE calculation.
C     (This is version 4 of MILLET.)
C     !DASH
      save
C     !DASH
      real*8 AEL, BD1, HND, HNP, RZM, SA, SUM, XNC, XNE, ZHEL, ZMER
      integer I, LU, N, NLH
C     !DASH
      external LINER, HI, BYE
C
C               ZHEL(N), AEL(N), RZM(N), HND(N), XNC(N), XNE(N), SA(N),
      dimension ZHEL(*), AEL(*), RZM(*), HND(*), XNC(*), XNE(*), SA(*),
C
C               ZMER(N), HNP(N), SUM(N), BD1(N)
     $          ZMER(*), HNP(*), SUM(*), BD1(*)
C
      call HI ('MILLET')
C     !BEG
      if(LU.gt.0) then
        call LINER   (1, LU)
        write (LU,100)
  100   format(' ',11X,'Added',11X,'Metal',27X,'Total',9X,'Electrons',
     $             8X,'Charged'/
     $         ' ',9X,'electrons',7X,'electrons',10X,'Z',12X,
     $             'Hydrogen',10X,'from',10X,'particle',8X,'Electron'/
     $         ' ',11X,'ratio',11X,'ratio',8X,'multiplier',8X,
     $             'density',9X,'Helium',10X,'density',9X,'density'//
     $         ' ',12X,'AEL',14X,'Z',14X,'RZM',13X,'HND',12X,'NHEL',
     $             13X,'XNC',13X,'XNE')
        call LINER   (1, LU)
        write (LU,101) (I,AEL(I),ZMER(I),RZM(I),HND(I),ZHEL(I),XNC(I),
     $                  XNE(I),I=1,N)
  101   format(5(' ',I4,1P7E16.8/))
C
        if(NLH.gt.0) then
          call LINER (3, LU)
          write (LU,102) NLH
  102     format(' ',43X,'Sum of',9X,'Level  1'/
     $           ' ',42X,'Hydrogen',8X,'Hydrogen'/
     $           ' ',40X,'Populations',6X,'departure'/
     $           ' ',9X,'I N T E R M E D I A T E S',6X,'Levels 2-',I2,
     $               5X,'coefficient'//
     $           ' ',12X,'SA',14X,'HNP',13X,'SUM',13X,'BD1')
          call LINER (1, LU)
          write (LU,103) (I,SA(I),HNP(I),SUM(I),BD1(I),I=1,N)
  103     format(5(' ',I4,1P4E16.8/))
        end if
      end if
C     !END
      call BYE ('MILLET')
C
      return
      end
