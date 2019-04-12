      subroutine PUMP
     $(XVAL,IBEG,IEND,TIT)
C
C     Rudolf Loeser, 1982 May 05
C---- Sets up depth index as a graph abscissa.
C     !DASH
      save
C     !DASH
      real*8 XVAL
      integer IBEG, IEND
      character TIT*(*)
C     !DASH
      external  INDARRD, HI, BYE
C
C               XVAL(N)
      dimension XVAL(*)
C
C
      call HI ('PUMP')
C     !BEG
      call INDARRD (XVAL,1,IBEG,IEND)
      TIT = 'Z-index'
C     !END
      call BYE ('PUMP')
C
      return
      end
