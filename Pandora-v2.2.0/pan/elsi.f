      subroutine ELSI
     $(IS,IE,PHI,XKPC,GTN,XKPL,OPAC)
C
C     Rudolf Loeser, 1980 Sep 22
C---- Computes Line Opacity, XKPL, and Total Opacity, OPAC (/cm).
C     (This is version 2 of ELSI.)
C     !DASH
      save
C     !DASH
      real*8 GTN, OPAC, PHI, XKPC, XKPL
      integer IE, IS, KOUNT
C     !DASH
      external ARRMUL, ARRADD, HI, BYE
C
      dimension PHI(*), XKPC(*), GTN(*), XKPL(*), OPAC(*)
C
C
      call HI ('ELSI')
C     !BEG
      KOUNT = IE-IS+1
      call ARRMUL (GTN(IS),  PHI(IS),  XKPL(IS), KOUNT)
      call ARRADD (XKPC(IS), XKPL(IS), OPAC(IS), KOUNT)
C     !END
      call BYE ('ELSI')
C
      return
      end
