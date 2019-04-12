      subroutine HOGRI
     $(ADS,ADMAS)
C
C     Rudolf Loeser, 1991 Sep 19
C---- Checks angular diameter data.
C     (This is version 2 of HOGRI.)
C     !DASH
      save
C     !DASH
      real*8 ADMAS, ADS, ONE, SOLDIA, ZERO
      integer LUEO
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON(18),SOLDIA)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      external MESHED, MASHED, HI, BYE
C
      call HI ('HOGRI')
C     !BEG
      if(ADMAS.ne.ZERO) then
C
        if(ADS.ne.ZERO) then
          call MESHED ('HOGRI', 3)
          write (LUEO,100) ADMAS,ADS
  100     format(' ','ADMAS =',1PE16.8,5X,'ADS =',E16.8/
     $           ' ','ADMAS and ADS may not both be specified in the ',
     $               'input. ADS has been accepted, and ADMAS ',
     $               'obtained from it.')
          call MASHED ('HOGRI')
C
          ADMAS = ADS*SOLDIA
        else
          ADS   = ADMAS/SOLDIA
        end if
C
      else
        if(ADS.eq.ZERO) then
          ADS = ONE
        end if
        ADMAS = ADS*SOLDIA
      end if
C     !END
      call BYE ('HOGRI')
C
      return
      end
