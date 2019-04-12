      subroutine LAMB
     $(X,A,V)
C
C     Rudolf Loeser, 1978 Apr 27
C---- Selects one of several ways of computing the Voigt function.
C     !DASH
      save
C     !DASH
      real*8 A, V, X
      integer IVOIT
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
      equivalence (KZQ( 38),IVOIT)
C
C---- VOISTA      as of 1984 Apr 24
      integer     KOUNVC
      common      /VOISTA/ KOUNVC
C     Counts number of times Voigt Function is evaluated.
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external RVOIGT, DVOIGT, PVOIGT, HALT, HI, BYE
C
      call HI ('LAMB')
C     !BEG
      if(IVOIT.eq.2) then
        call DVOIGT (X, A, V)
      else if(IVOIT.eq.1) then
        call RVOIGT (X, A, V)
      else if(IVOIT.eq.3) then
        call PVOIGT (X, A, V)
C
      else
        write (MSSLIN(1),100) IVOIT
  100   format('IVOIT =',I12,', which is not 1, 2 or 3.')
        call HALT   ('LAMB', 1)
      end if
C
      KOUNVC = KOUNVC+1
C     !END
      call BYE ('LAMB')
C
      return
      end
