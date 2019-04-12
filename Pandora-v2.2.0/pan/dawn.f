      subroutine DAWN
     $(IU,IL,XNU,JST,TE,V,DWEST)
C
C     Rudolf Loeser, 1992 Apr 03
C---- Computes estimated Doppler width for Hydrogen Stark splitting.
C     !DASH
      save
C     !DASH
      real*8 CLIGHT, DNU, DWEST, FAC, ONE, ROOT, TE, V, XNU, dummy
      integer IL, IU, JST
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 3),CLIGHT)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DOPPLER, HI, BYE
C
C               XNU(NSL), TE(N), V(N)
      dimension XNU(*),   TE(*), V(*)
C
      data FAC /1.D20/
C
      call HI ('DAWN')
C     !BEG
      DNU = XNU(IU)-XNU(IL)
      call DOPPLER (DNU,TE(JST),ONE,(V(JST)**2),ROOT,dummy)
      DWEST = ((FAC*DNU)/(CLIGHT**2))*ROOT
C     !END
      call BYE ('DAWN')
C
      return
      end
