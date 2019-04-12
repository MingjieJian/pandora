      subroutine MULTER
     $(NO,TOT,TITLE)
C
C     Rudolf Loeser, 1991 Jul 16
C---- Prints a heading for eclipse profile data
C     !DASH
      save
C     !DASH
      integer NO
      character TITLE*5, TOT*24
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C     !DASH
      external LINER, HI, BYE
C
      call HI ('MULTER')
C     !BEG
      write (NO,100) TOT
  100 format(' ',8X,'-----------------------------------------',
     $           10X,A)
      write (NO,101) TITLE
  101 format(' ',8X,A5,3X,'Intensity Profiles, in ergs/cm**2/s/sr/Hz')
C
      call LINER (1,NO)
      if(TITLE(1:1).eq.ALPHS(19)) then
        write (NO,102)
  102   format(' ',8X,'IS(I) is the intensity along the ray tangent ',
     $             'to the I''th depth')
      else
        write (NO,103)
  103   format(' ',8X,'ID(J) is the intensity along the ray tangent ',
     $             'to the J''th fractional radius')
      end if
C
      write (NO,104) TITLE(1:1)
  104 format(' ',8X,'II'A1,'   is the Integrated Intensity, in ',
     $           'ergs/cm**2/s/sr')
C
      if(TITLE(1:1).eq.ALPHS(19)) then
        write (NO,105)
  105   format(' ',8X,'IICM  is the Integrated Intensity per cm ',
     $             'strip, in ergs/cm/s/sr')
      end if
C     !END
      call BYE ('MULTER')
C
      return
      end
