      subroutine PHAROT
     $(NO)
C
C     Rudolf Loeser, 2006 Mar 08
C---- Prints error message header, for ERIKA / DOMINO.
C     !DASH
      save
C     !DASH
      integer NO
C     !COM
C---- OLIVIA      as of 2006 Mar 08
      real*8      XNUKH,RBAR,DNU,PT,R,TERJ
      integer     IUCE,ILCE
      common      /OLIVIA/ XNUKH,RBAR,DNU,PT,R
      common      /OLIVIB/ TERJ
      common      /OLIVIC/ IUCE,ILCE
C     Parameters for ERIKA: calculation of Collisional Ionization
C     Integral, for the impact-parameter method.
C     .
C     !DASH
      external LINER, HI, BYE
C
      call HI ('PHAROT')
C     !BEG
      if(NO.gt.0) then
        write (NO,100) IUCE,ILCE,TERJ
  100   format(' ','Error in calculation of impact-parameter ',
     $             'collision rate CE(',I2,'/',I2,') for TE =',
     $             1PE10.3)
        call LINER (1, NO)
      end if
C     !END
      call BYE ('PHAROT')
C
      return
      end
