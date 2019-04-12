      subroutine HEMRY
     $(N,BHORIZ,PMG)
C
C     Rudolf Loeser, 2006 Nov 10
C---- Computes magnetic pressure.
C     !DASH
      save
C     !DASH
      real*8 BHORIZ, EIGHT, FAC, ONE, PI, PMG
      integer I, N
C     !COM
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
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 9),EIGHT )
C     !DASH
      external HI, BYE
C
C               BHORIZ(N), PMG(N)
      dimension BHORIZ(*), PMG(*)
C
      call HI ('HEMRY')
C     !BEG
      FAC = ONE/(EIGHT*PI)
      do 100 I = 1,N
        PMG(I) = FAC*(BHORIZ(I)**2)
  100 continue
C     !END
      call BYE ('HEMRY')
C
      return
      end
