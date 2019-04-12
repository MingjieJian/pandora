      subroutine SIMEON
     $(KAVNT,KAVNP,KAVNZ,KWA,WAVCA,ALOMI,ALOMA,LUKA)
C
C     Rudolf Loeser, 1993 Sep 08
C---- Sets up data for "averaged" line opacity calculation.
C     !DASH
      save
C     !DASH
      real*8 ALOMA, ALOMI, WAVCA, ZERO
      integer I, KAVNP, KAVNT, KAVNZ, KWA, LUKA, NW
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external  HALT, HI, BYE
      intrinsic max
C
C               WAVCA(KWA)
      dimension WAVCA(*)
C     !EJECT
C
      call HI ('SIMEON')
C     !BEG
      rewind LUKA
C
      read (LUKA) NW,KAVNT,KAVNP,KAVNZ
      if(NW.ne.KWA) then
        write (MSSLIN(1),100) NW,KWA
  100   format('NW =',I12,', KWA =',I12,'; they should be equal.')
        call HALT ('SIMEON',1)
      end if
C
      KAVNT = max(KAVNT,1)
      KAVNP = max(KAVNP,1)
      KAVNZ = max(KAVNZ,1)
C
      if(KWA.gt.0) then
        read (LUKA) (WAVCA(I),I=1,KWA)
        ALOMI = WAVCA(  1)
        ALOMA = WAVCA(KWA)
C
      else
        ALOMI = ZERO
        ALOMA = ZERO
      end if
C     !END
      call BYE ('SIMEON')
C
      return
      end
