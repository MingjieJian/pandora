      subroutine TRINCA
     $(TAB,N)
C
C     Rudolf Loeser, 2002 Jul 30
C---- Converts a "cm" table to "km".
C     (See also CATRIN.)
C     !DASH
      save
C     !DASH
      real*8 CMPKM, TAB
      integer N
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 5),CMPKM )
C     !DASH
      external CONDIV, HI, BYE
C
C               TAB(N)
      dimension TAB(*)
C
      call HI ('TRINCA')
C     !BEG
      call CONDIV (CMPKM,TAB,N)
C     !END
      call BYE ('TRINCA')
C
      return
      end
