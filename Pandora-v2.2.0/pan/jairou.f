      subroutine JAIROU
     $(KOPAC)
C
C     Rudolf Loeser, 1995 Mar 02
C---- Suppresses "Line Opacities" contributions to the Background.
C     (This is version 3 of JAIROU.)
C     !DASH
      save
C     !DASH
      integer KOPAC
C     !DASH
      external HI, BYE
C
C               KOPAC(Nopac)
      dimension KOPAC(*)
C
      call HI ('JAIROU')
C     !BEG
      KOPAC(22) = 0
      KOPAC(23) = 0
      KOPAC(24) = 0
      KOPAC(25) = 0
      KOPAC(31) = 0
      KOPAC(32) = 0
C     !END
      call BYE ('JAIROU')
C
      return
      end
