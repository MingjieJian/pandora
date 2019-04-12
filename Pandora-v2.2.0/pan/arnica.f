      subroutine ARNICA
     $(N,S,OPAC,XJNU,FD)
C
C     Rudolf Loeser, 1981 Jul 21
C---- Computes flux derivative, for CSF calculation.
C     !DASH
      save
C     !DASH
      real*8 FD, OPAC, S, XJNU
      integer N
C     !DASH
      external ARRSUB, ARRMUL, HI, BYE
C
C               S(N), OPAC(N), XJNU(N), FD(N)
      dimension S(*), OPAC(*), XJNU(*), FD(*)
C
      call HI ('ARNICA')
C     !BEG
      call ARRSUB (S,XJNU,FD,N)
      call ARRMUL (OPAC,FD,FD,N)
C     !END
      call BYE ('ARNICA')
C
      return
      end
