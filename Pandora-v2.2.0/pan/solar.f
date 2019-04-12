      subroutine SOLAR
     $(TE,DE,P,A,X,CK,DUMP,CI)
C
C     Rudolf Loeser, 2005 Aug 31
C---- Computes a value of CI(1,TE) according to
C
C     G. S. Voronov (1997),
C     Atomic Data and Nuclear Data Tables, 65, 1.
C
C     !DASH
      save
C     !DASH
      real*8 A, CI, CK, CON24, DE, ONE, P, PU, RAT, ROOTU, TE, TRM, U,
     $       X
      integer LUEO
      logical DUMP, KILROY
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external RIGEL, DIVIDE, LINER, HI, BYE
C
      data KILROY /.true./
C
      call HI ('SOLAR')
C     !BEG
      if(DUMP) then
        call LINER (2, LUEO)
        write (LUEO,100) TE,DE,P,A,X,CK
  100   format(' ','CI(1) according to Voronov.',5X,'** TE =',1PE13.6//
     $         ' ','DE =',E12.4,5X,'P =',E12.4,5X,'A =',E12.4,5X,
     $             'X =',E12.4,5X,'K =',E12.4)
      end if
C
      if(KILROY) then
        KILROY = .false.
        call RIGEL (24, CON24)
      end if
C
      call DIVIDE  (DE, TE, RAT)
      U = CON24*RAT
C
      TRM = ONE
      if(P.eq.ONE) then
        ROOTU = sqrt(U)
        TRM   = TRM+ROOTU
      end if
C
      PU = U**CK
      call DIVIDE  ((A*TRM*PU), (X+U), CI)
C
      if(DUMP) then
        write (LUEO,101) U,PU,TRM,CI
  101   format(' ','U =',1PE12.4,5X,'PU =',E12.4,5X,'TRM =',E12.4,5X,
     $             '** CI(1) =',E14.6)
      end if
C     !END
      call BYE ('SOLAR')
C
      return
      end
