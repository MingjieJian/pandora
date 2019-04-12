      subroutine GUNNAR
     $(N,XLM,GBF)
C
C     Rudolf Loeser, 2003 Aug 07
C---- Computes the Hydrogen bound-free Gaunt factor
C     for level N at wavelength XLAM (Angstroms).
C     (This is version 2 of GUNNAR.)
C     !DASH
      save
C     !DASH
      real*8 GBF, XLM
      integer N
      logical OLD
C     !DASH
      external NARUNG, NURGAN, HI, BYE
C
      data OLD /.false./
C
      call HI ('GUNNAR')
C     !BEG
      if(OLD) then
        call NARUNG (N,XLM,GBF)
      else
C
        call NURGAN (N,XLM,GBF)
C
      end if
C     !END
      call BYE ('GUNNAR')
C
      return
      end
