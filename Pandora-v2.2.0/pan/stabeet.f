      subroutine STABEET
     $(N,HEK,SHE2,BETA)
C
C     Rudolf Loeser, 1998 Dec 14
C---- Sets up BETA, for diffusion.
C     !DASH
      save
C     !DASH
      real*8 BETA, HEK, SHE2
      integer IBTSW, N
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(166),IBTSW)
C     !DASH
      external MOVE1, ARRAVE, HALT, HI, BYE
C
C               HEK(N), SHE2(N), BETA(N)
      dimension HEK(*), SHE2(*), BETA(*)
C
      call HI ('STABEET')
C     !BEG
      if(IBTSW.eq.0) then
        call ARRAVE (HEK, SHE2, BETA, N)
      else if(IBTSW.eq.1) then
        call MOVE1  (HEK , N, BETA)
      else if(IBTSW.eq.2) then
        call MOVE1  (SHE2, N, BETA)
      else
        write (MSSLIN(1),100) IBTSW
  100   format('IBTSW =',I12,', which is not 0, 1, or 2.')
        call HALT   ('STABEET', 1)
      end if
C     !END
      call BYE ('STABEET')
C
      return
      end
