      subroutine NIRVANA
     $(TELM,XNELM,TET,NT,XNET,NE,I,J,LEQ)
C
C     Rudolf Loeser, 1980 Dec 19
C---- Picks interpolation control indices, for TANYA.
C     (This is version 2 of NIRVANA.)
C     !DASH
      save
C     !DASH
      real*8 TELM, TET, XNELM, XNET, ZERO
      integer I, J, JAB, LEQ, LOOKP, LOOKT, NE, NT, jummy
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external LOOKSD, HI, BYE
C
C               XNET(NT), TET(NE)
      dimension XNET(*),  TET(*)
C
      dimension JAB(4)
C
      data JAB /0, 1, 1, 2/
C
      call HI ('NIRVANA')
C     !BEG
      call LOOKSD (XNET,NE,ZERO,XNELM,J,jummy,LOOKP)
      call LOOKSD (TET ,NT,ZERO,TELM ,I,jummy,LOOKT)
      LEQ = 3*JAB(LOOKP)+JAB(LOOKT)+1
C     !END
      call BYE ('NIRVANA')
C
      return
      end
