      subroutine BURDOCK
     $(LU,ABDC,ABDO)
C
C     Rudolf Loeser, 2006 Dec 28
C---- Prints a heading, for PUP.
C     (This is version 4 of BURDOCK.)
C     !DASH
      save
C     !DASH
      real*8 ABDC, ABDO
      integer LU
C     !DASH
      external ABJECT, LINER, HI, BYE
C
      call HI ('BURDOCK')
C     !BEG
      if(LU.gt.0) then
        call ABJECT (LU)
        write (LU,100) ABDC,ABDO
  100   format(' ','Calculation of CO, CH, and OH number densities.'//
     $         ' ','(For TE or TCO greater than 9000K, these number ',
     $             'densities are set = 0.)'//
     $         ' ','Abundance of C =',1PE12.4/
     $         ' ','Abundance of O =',E12.4//
     $         ' ','Given TE, TCO, HND and these abundances, the ',
     $             'following equations are solved'/
     $         ' ','to obtain CN, ON, CON, CHN and OHN:'/
     $         ' ',5X,'CON = CN * ON  * E(CO)'/
     $         ' ',5X,'CHN = CN * HND * E(CH)'/
     $         ' ',5X,'OHN = ON * HND * E(OH)'/
     $         ' ',5X,'CN + CON + CHN = ABDC * HND'/
     $         ' ',5X,'ON + CON + OHN = ABDO * HND')
        call LINER  (2, LU)
        write (LU,101)
  101   format(' ',13X,'HND',9X,'TE',8X,'TCO',6X,'E(CO)',6X,'E(CH)',
     $             6X,'E(OH)',9X,'CN',9X,'ON',8X,'CON',8X,'CHN',
     $             8X,'OHN')
        call LINER  (1, LU)
      end if
C     !END
      call BYE ('BURDOCK')
C
      return
      end
