      subroutine LIDCEK
     $(CALLER,PART,BNAME,LINNAM)
C
C     Rudolf Loeser, 1999 Dec 07
C---- Consistency check for I/O of Line Intensity Data Blocks.
C     !DASH
      save
C     !DASH
      real*8 BNAME
      integer LINNAM, NAME, PART
      character CALLER*(*)
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, HI, BYE
C
      call HI ('LIDCEK')
C     !BEG
      NAME = BNAME
      if(NAME.ne.LINNAM) then
        write (MSSLIN(1),100) PART,NAME,LINNAM
  100   format('Regarding Line Intensity Data Block-',I1,': NAME = ',
     $         I10,', but LINNAM(Thule) = ',I4)
        call HALT ((CALLER//'/LIDCEK'), 1)
      end if
C     !END
      call BYE ('LIDCEK')
C
      return
      end
