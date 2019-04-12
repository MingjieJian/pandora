      subroutine BROWN
     $(NSL,NPQ,CP)
C
C     Rudolf Loeser, 1992 Mar 27
C---- Computes default photoionization cross-sections for Hydrogen.
C     (This is version 5 of BROWN.)
C     !DASH
      save
C     !DASH
      real*8 CP, HYLYK, PQN, ZERO
      integer J, JDCPI, JDKNT, NPQ, NSL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- APOLLO      as of 2006 Dec 04
      integer     MEST
      dimension   MEST(28)
      common      /APOLLO/ MEST
C     Atomic model parameter default values indicators
      equivalence (MEST(13),JDCPI)
      equivalence (MEST(21),JDKNT)
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON(17),HYLYK )
C     !DASH
      external HI, BYE
C
C               NPQ(NSL), CP(NSL)
      dimension NPQ(*),   CP(*)
C     !EJECT
C
      call HI ('BROWN')
C     !BEG
      do 100 J = 1,NSL
C
        if(CP(J).eq.ZERO) then
          PQN   = NPQ(J)
          CP(J) = PQN*HYLYK
          JDCPI = JDCPI+1
        else
          JDKNT = JDKNT+1
        end if
C
  100 continue
C     !END
      call BYE ('BROWN')
C
      return
      end
