      subroutine HEMP
     $(LU)
C
C     Rudolf Loeser, 2007 Jan 24
C---- Prints an ad-hoc message for O-III.
C     !DASH
      save
C     !DASH
      integer LU
C     !DASH
      external LINER, HI, BYE
C
      call HI ('HEMP')
C     !BEG
      if(LU.gt.0) then
        call LINER (2, LU)
        write (LU,100)
  100   format(' ','N O T E :  As of January 2007, level 8 of the ',
     $             'built-in O-III population ion = level 13 of ',
     $             'ion model specified in o3l13.atm.')
        call LINER (2, LU)
      end if
C     !END
      call BYE ('HEMP')
C
      return
      end
