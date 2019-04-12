      subroutine MUTTON
     $(X,A,V)
C
C     Rudolf Loeser, 1978 Apr 27
C---- Computes a value of the VOIGT profile.
C     !DASH
      save
C     !DASH
      real*8 A, C, U, US, V, VOITC, VS, X, XS, ZERO
      integer IFLGU, IFLGX
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
      equivalence (RZQ(106),VOITC)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  COMPD, LAMB, HI, BYE
      intrinsic abs
C
      data C,XS,US /1.D-14, -1.D+30, -1.D+30/
C
      call HI ('MUTTON')
C     !BEG
      U = A
      if(abs(U).lt.VOITC) then
        U = ZERO
      end if
C
      call COMPD  (X, XS, C, IFLGX)
      call COMPD  (U, US, C, IFLGU)
      if((IFLGX.ne.0).or.(IFLGU.ne.0)) then
        XS = X
        US = U
        call LAMB (XS, US, VS)
      end if
C
      V = VS
C     !END
      call BYE ('MUTTON')
C
      return
      end
