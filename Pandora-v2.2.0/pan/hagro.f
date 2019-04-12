      subroutine HAGRO
     $(CGR,CLOGG,LGGIN)
C
C     Rudolf Loeser, 1985 Feb 07
C---- Checks on gravity values.
C     !DASH
      save
C     !DASH
      real*8 CGR, CLOGG, ONE, SOLSGR, TEN, X, ZERO
      integer LGGIN, LUEO
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 9),SOLSGR)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT(11),TEN   )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external MESHED, MASHED, HI, BYE
C
      call HI ('HAGRO')
C     !BEG
      if(CLOGG.ne.ZERO) then
        LGGIN = 1
C
        if(CGR.ne.ZERO) then
          call MESHED ('HAGRO', 3)
          write (LUEO,100) CLOGG,CGR
  100     format(' ','CLOGG =',1PE16.8,5X,'CGR =',E16.8//
     $           ' ','Do not specify both CLOGG and CGR in the ',
     $               'input. CGR has been accepted, and CLOGG ',
     $               'recomputed from it.')
          call MASHED ('HAGRO')
C
          LGGIN = 0
          X     = CGR*SOLSGR
          CLOGG = log10(X)
        else
          X   = TEN**CLOGG
          CGR = X/SOLSGR
        end if
C
      else
        LGGIN = 0
        if(CGR.eq.ZERO) then
          CGR = ONE
        end if
        X     = CGR*SOLSGR
        CLOGG = log10(X)
      end if
C     !END
      call BYE ('HAGRO')
C
      return
      end
