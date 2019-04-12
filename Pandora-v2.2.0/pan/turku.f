      subroutine TURKU
     $(TAUK,SCON,BK,N,NB,J1,J2)
C
C     Rudolf Loeser, 1996 Feb 29
C---- Dumps, for WAGRIN.
C     !DASH
      save
C     !DASH
      real*8 BK, SCON, TAUK
      integer J1, J2, LUEO, N, NB
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, ARROUT, HI, BYE
C
C               TAUK(N,NB), SCON(N,NB), BK(N,NB)
      dimension TAUK(*),    SCON(*),    BK(*)
C
      call HI ('TURKU')
C     !BEG
      call LINER  (2, LUEO)
      write (LUEO,100) J1,J2
  100 format(' ','Continuum data for this batch of wavelengths.'/
     $       ' ','Selected sets are:',2I5)
      call ARROUT (LUEO, TAUK, N, NB, 'TAUK')
      call ARROUT (LUEO, SCON, N, NB, 'SCON')
      call ARROUT (LUEO, BK,   N, NB, 'B'   )
C     !END
      call BYE ('TURKU')
C
      return
      end
