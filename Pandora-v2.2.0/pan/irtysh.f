      subroutine IRTYSH
     $(SFLX,SDIF,N)
C
C     Rudolf Loeser, 1981 Sep 10
C---- Computes integrated differences, for BASHKIR.
C     !DASH
      save
C     !DASH
      real*8 SDIF, SFLX
      integer N
C     !DASH
      external MOVE1, CONSUB, HI, BYE
C
C               SFLX(N), SDIF(N)
      dimension SFLX(*), SDIF(*)
C
      call HI ('IRTYSH')
C     !BEG
      call MOVE1  (SFLX   ,N,SDIF)
      call CONSUB (SFLX(1),SDIF,N)
C     !END
      call BYE ('IRTYSH')
C
      return
      end
