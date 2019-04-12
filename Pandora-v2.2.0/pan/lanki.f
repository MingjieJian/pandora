      subroutine LANKI
     $(XLM,W1,W2,JQ)
C
C     Rudolf Loeser, 2003 Jan 07
C---- Computes weights for Highest H Ly lines contributions.
C     (This is version 2 of LANKI.)
C     !DASH
      save
C     !DASH
      real*8 DW, ONE, RYDBRG, W1, W2, WQ, XLM, ZERO
      integer JQ, NLY, NQLYM, NQS
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
      equivalence (KZQ(184),NLY  )
      equivalence (KZQ(188),NQLYM)
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 4),RYDBRG)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external KITH, HI, BYE
C
      data NQS /-1000/
C     !EJECT
C
      call HI ('LANKI')
C     !BEG
      W1 = ONE
      W2 = ZERO
      JQ = NQLYM
C
      if(NQLYM.ge.NLY) then
        if(NQS.ne.NQLYM) then
          NQS = NQLYM
          call KITH (NQS,WQ)
        end if
C
        if((XLM.gt.RYDBRG).and.(XLM.le.WQ)) then
          DW = WQ-RYDBRG
          W1 = (XLM-RYDBRG)/DW
          W2 = (WQ-XLM)/DW
        end if
      end if
C     !END
      call BYE ('LANKI')
C
      return
      end
