      subroutine SUNDEW
     $(KOPAC,KOPAT,NOPAC)
C
C     Rudolf Loeser, 1995 Mar 09
C---- Sets up contributor switches for "true" continuum.
C     (This is version 2 of SUNDEW.)
C     !DASH
      save
C     !DASH
      integer I, J, KOPAC, KOPAT, NOPAC
C     !COM
C---- FORGO       as of 2007 Jan 12
      integer     MLINCC,NLINCC,LLINCC
      parameter   (MLINCC=18)
      dimension   LLINCC(MLINCC)
      common      /FORGO/ NLINCC,LLINCC
C     List of continuum contributors omitted from "true" continuum
C     .
C     !DASH
      external MOVEI, HI, BYE
C
C               KOPAC(Nopac), KOPAT(Nopac)
      dimension KOPAC(*),     KOPAT(*)
C
      call HI ('SUNDEW')
C     !BEG
      call MOVEI (KOPAC, 1, NOPAC, KOPAT, 1, NOPAC)
C
      do 100 I = 1,NLINCC
        J = LLINCC(I)
        KOPAT(J) = 0
  100 continue
C     !END
      call BYE ('SUNDEW')
C
      return
      end
