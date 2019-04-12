      subroutine ALLIUM
     $(NO,L,I,ONAME,MLAB,LEGS)
C
C     Rudolf Loeser, 1989 Apr 17
C---- Prints for FOP via ALYSSUM.
C     !DASH
      save
C     !DASH
      integer I, L, NO
      character LEGS*109, MLAB*4, ONAME*8
C     !DASH
      external SHIM, HI, BYE
C
      call HI ('ALLIUM')
C     !BEG
      write (NO,100) I,ONAME,MLAB,LEGS
  100 format(' ',I3,1X,A8,1X,A4,1X,A109)
      call SHIM (L,5,NO)
C     !END
      call BYE ('ALLIUM')
C
      return
      end
