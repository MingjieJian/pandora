      subroutine RICCALL
     $(N,TE,BETA,NO,WAVE)
C
C     Rudolf Loeser, 1983 Aug 16
C---- Exhibits a BETA table.
C     !DASH
      save
C     !DASH
      real*8 BETA, FNU, FNUZ, FRQUNT, HNUKT, RAT, TE, WAVE, ZMAX, ZMIN
      integer I, IMAX, IMIN, N, NO
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 3),FRQUNT)
C     !DASH
      external PROD, MINMAXD, ANGIE, LINER, PRIVET, HI, BYE
C
C               BETA(N), TE(N)
      dimension BETA(*), TE(*)
C
      call HI ('RICCALL')
C     !BEG
      do 100 I = 1,N
        call PROD  (TE(I), WAVE, 2, HNUKT, BETA(I))
  100 continue
C
      call MINMAXD (BETA, 1, N, IMIN, IMAX)
      ZMIN = log10(BETA(IMIN))
      ZMAX = log10(BETA(IMAX))
      RAT  = ZMAX-ZMIN
      call ANGIE   (WAVE, FNU)
      FNUZ = FNU*FRQUNT
C
      call LINER   (2,NO)
      write (NO,101) WAVE,FNU,FNUZ,BETA(IMIN),BETA(IMAX),RAT
  101 format(' ','at',1PE20.12,' Angstroms, =',E20.12,
     $           ' frequency units, =',E20.12,' Hz.'//
     $       ' ','Minimum =',1PE16.8,', Maximum =',E16.8,
     $           ', log(Ratio) =',0PF10.4)
      call PRIVET  (NO, BETA, N)
C     !END
      call BYE ('RICCALL')
C
      return
      end
