      subroutine MOHUCA
     $(NO,TOT,TITLE)
C
C     Rudolf Loeser, 1991 Jul 16
C---- Prints a heading for eclipse profile data.
C     !DASH
      save
C     !DASH
      integer NO
      character TITLE*5, TOT*24
C     !DASH
      external LINER, HI, BYE
C
      call HI ('MOHUCA')
C     !BEG
      write (NO,100) TOT
  100 format(' ',8X,'------------------------------------------------',
     $              '-------------',10X,A)
      write (NO,101) TITLE
  101 format(' ',A5,3X,'Depth index of the intersection closest ',
     $           'to unit Optical Depth, and the Optical Depth ',
     $           'at that intersection.'/
     $       ' ',8X,'(A minus sign indicates that the intersection ',
     $              'lies beyond the midpoint (tangency point) of ',
     $              'the ray.)')
      call LINER (1,NO)
C     !END
      call BYE ('MOHUCA')
C
      return
      end
