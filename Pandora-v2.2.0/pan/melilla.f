      subroutine MELILLA
     $(KEQ,Y,A,B,C,D,E,F,E1Y,GAMMA)
C
C     Rudolf Loeser, 2006 Aug 11
C---- Sets up the calculation of GAMMA,
C     needed for computing CE using the procedures of Suno & Kato.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, D, DX, E, E1Y, F, GAMMA, TENTH, X, Y, Z, ZERO
      integer KEQ, NT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(19),TENTH )
C     !DASH
      external MELLO6, MELLO7, MELLO10, MELLO11, HI, BYE
C
      parameter (NT=102)
      dimension X(NT), Z(NT)
C
      call HI ('MELILLA')
C     !BEG
      GAMMA = ZERO
      E1Y   = ZERO
C
      if(Y.gt.ZERO) then
        DX = TENTH/Y
        if(KEQ.eq.6) then
          call MELLO6  (Y, A, B, C, D, E,    GAMMA, NT, DX, X, Z)
        else if(KEQ.eq.7) then
          call MELLO7  (Y, A, B, C, D, E, F, GAMMA, NT, DX, X, Z)
        else if(KEQ.eq.10) then
          call MELLO10 (Y, A, B, C, D, E,    E1Y, GAMMA)
        else if(KEQ.eq.11) then
          call MELLO11 (Y, A, B, C, D, E, F, E1Y, GAMMA)
        end if
      end if
C     !END
      call BYE ('MELILLA')
C
      return
      end
