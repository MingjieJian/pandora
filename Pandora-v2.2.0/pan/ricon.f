      subroutine RICON
     $(REG,CON,INP)
C
C     Rudolf Loeser, 1982 Nov 18
C---- Computes processing-mode switches.
C
C     Note: JSTIN takes precedence over JSTCN.
C
C     (This is version 2 of RICON.)
C     !DASH
      save
C     !DASH
      integer JSTCN, JSTIN
      logical CON, INP, REG
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 35),JSTCN)
      equivalence (KZQ(  7),JSTIN)
C     !DASH
      external HI, BYE
C
      call HI ('RICON')
C     !BEG
      REG = .true.
      CON = .false.
      INP = .false.
C
      if(JSTCN.gt.0) then
        CON = .true.
        REG = .false.
      end if
C
      if(JSTIN.gt.0) then
        INP = .true.
        REG = .false.
        CON = .false.
      end if
C     !END
      call BYE ('RICON')
C
      return
      end
