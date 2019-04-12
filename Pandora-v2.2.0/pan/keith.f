      subroutine KEITH
     $(N,XLM,TE,XNE,VM,XLMXX,WLIN,XNU,ROOT,CLN,AN1,AN1S,SK,STKFN,
     $ XLIM,X)
C
C     Rudolf Loeser, 2002 Sep 18
C---- Computes X, distance from the core of the Hydrogen Lyman N/1
C     line, in Doppler widths, and other parameters.
C     (This is version 2 of KEITH.)
C     !DASH
      save
C     !DASH
      real*8 AN1, AN1S, ANGPCM, CLIGHT, CLN, FAC, FEC, PE, PW, ROOT, SK,
     $       STKFN, TE, VM, WLIN, X, XLIM, XLM, XLMXC, XLMXX, XNE, XNU,
     $       ZERO
      integer JHLSK, N
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
      equivalence (RZQ(162),XLMXC)
      equivalence (KZQ(190),JHLSK)
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 3),CLIGHT)
      equivalence (TUNI( 6),ANGPCM)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external  KINAL, HI, BYE
      intrinsic abs
C
C               XLMXX(LLY)
      dimension XLMXX(*)
C
      data FAC,FEC,PW /1.D-22, 1.D12, 6.6666666666666667D-1/
C
      call HI ('KEITH')
C     !BEG
      CLN = WLIN/ANGPCM
C
      call KINAL (N, TE, VM, XNU, ROOT)
      X = (abs(XLM-WLIN)/XLM)*(CLIGHT/ROOT)
C
      if(JHLSK.gt.0) then
        PE    = (XNE/FEC)**PW
        STKFN = SK*PE
      else
        STKFN = ZERO
      end if
C
      if(XLMXC.gt.ZERO) then
        XLIM = XLMXC
      else
        XLIM = XLMXX(1)
      end if
C     !END
      call BYE ('KEITH')
C
      return
      end
