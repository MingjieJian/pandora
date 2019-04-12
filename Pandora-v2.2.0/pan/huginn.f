      subroutine HUGINN
     $(NT,NTMX,CALLER)
C
C     Rudolf Loeser, 2004 Aug 06
C---- Error stop for table augmentation.
C     (This is version 3 of HUGINN.)
C     !DASH
      save
C     !DASH
      integer NT, NTMX
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
      call HI ('HUGINN')
C     !BEG
      write (MSSLIN(1),100) CALLER,NT,NTMX
  100 format('Error in ',A,': NT =',I12,', NTMX =',I12,
     $       ': too many items.')
      call HALT ('HUGINN', 1)
C     !END
      call BYE ('HUGINN')
C
      return
      end
