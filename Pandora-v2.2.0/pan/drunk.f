      subroutine DRUNK
     $(DW,DP,TAUM,XXC)
C
C     Rudolf Loeser, 1988 Jul 20
C---- Computes XXC, for DEAR.
C     !DASH
      save
C     !DASH
      real*8 A, DP, DW, FAC, FOUR, FXCC, PI, SIX, TAUCL, TAUM, TCLL,
     $       THI, THIRD, TLOG, XCL, XXC
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
      equivalence (RZQ(102),XCL  )
      equivalence (RZQ(103),TAUCL)
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 1),PI    )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(13),THIRD )
      equivalence (DLIT( 5),FOUR  )
      equivalence (DLIT( 7),SIX   )
C     !DASH
C     !EJECT
      external  DIVIDE, HI, BYE
      intrinsic max
C
      data THI /1.D6/
C
      call HI ('DRUNK')
C     !BEG
      if(TAUM.le.TAUCL) then
        XXC = XCL
      else
        call DIVIDE   (DP, DW, A)
        FAC = (FOUR*A)/PI
C
        if(TAUM.ge.THI) then
          XXC = (FAC*TAUM)**THIRD
        else
C
          FXCC = (FAC*THI)**THIRD
          TCLL = log10(TAUCL)
          TLOG = log10(TAUM)
          XXC  = ((SIX-TLOG)*XCL+(TLOG-TCLL)*FXCC)/(SIX-TCLL)
        end if
C
        XXC = max(XXC,XCL)
      end if
C     !END
      call BYE ('DRUNK')
C
      return
      end
