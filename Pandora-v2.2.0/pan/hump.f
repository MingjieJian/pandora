      subroutine HUMP
     $(Z,XVAL,IBEG,IEND,TIT)
C
C     Rudolf Loeser, 1982 May 05
C---- Sets up Z as a graph abscissa.
C     !DASH
      save
C     !DASH
      real*8 XVAL, Z
      integer IBEG, IEND, KNT
      character TIT*(*)
C     !DASH
      external MOVE1, HI, BYE
C
C               Z(N), XVAL(N)
      dimension Z(*), XVAL(*)
C
      call HI ('HUMP')
C     !BEG
      KNT = IEND-(IBEG-1)
      call MOVE1 (Z(IBEG),KNT,XVAL(IBEG))
      TIT = 'Z'
C     !END
      call BYE ('HUMP')
C
      return
      end
