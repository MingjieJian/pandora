      subroutine JABIRU
     $(NO)
C
C     Rudolf Loeser, 1985 Dec 16
C---- Prints a heading for CORAL.
C     !DASH
      save
C     !DASH
      integer NO
C     !DASH
      external HI, BYE
C
      call HI ('JABIRU')
C     !BEG
      if(NO.gt.0) then
        write (NO,100)
  100   format(' ','Partition Function: function of THETA ',
     $             '(= reciprocal temperature), PE (= electron ',
     $             'pressure, dyn/cm**2),'/
     $         ' ','as computed with the Hamburg (1966) data.'/
     $         ' ','(These values are strictly not valid at high ',
     $             'temperature and/or low density.)')
      end if
C     !END
      call BYE ('JABIRU')
C
      return
      end
