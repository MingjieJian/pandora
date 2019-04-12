      subroutine WHISKEY
     $(MET,SWITCH,KNT)
C
C     Rudolf Loeser, 1984 Oct 24
C---- Sets up control switches indicating the remaining methods
C     to be tried for the Statistical Equilibrium calculations.
C     (This is version 2 of WHISKEY.)
C     !DASH
      save
C     !DASH
      integer I, KNT, MET
      logical SWITCH
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
C               SWITCH(KNT)
      dimension SWITCH(*)
C
      call HI ('WHISKEY')
C     !BEG
      if((MET.lt.0).or.(MET.ge.KNT))then
        write (MSSLIN(1),100) MET,KNT
  100   format('MET =',I12,', KNT =',I12,'; this is not OK.')
        call HALT ('WHISKEY',1)
      end if
C
      do 101 I = 1,KNT
        SWITCH(I) = .true.
  101 continue
C
      SWITCH(MET+1) = .false.
C     !END
      call BYE ('WHISKEY')
C
      return
      end
