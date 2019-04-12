      subroutine HALT
     $(CALLER,N)
C
C     Rudolf Loeser, 2002 Feb 25
C---- Prints error message and aborts.
C     !DASH
      save
C     !DASH
      integer I, LUEO, N
      character CALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external MESHED, ABORT, HI, BYE
C
      call HI ('HALT')
C     !BEG
      call MESHED (CALLER,1)
C
      if((N.gt.0).and.(N.lt.5)) then
        do 101 I = 1,N
          write (LUEO,100) MSSLIN(I)
  100     format(' ',A)
  101   continue
      end if
C
      call ABORT
C     !END
      call BYE ('HALT')
C
      return
      end
