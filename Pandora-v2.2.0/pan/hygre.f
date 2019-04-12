      subroutine HYGRE
     $(QELSM,KSTRK,IVOIT,NVOIT)
C
C     Rudolf Loeser, 1991 Dec 13
C---- Sets Voigt calculation switches for Hydrogen.
C     !DASH
      save
C     !DASH
      integer IVOIT, KSTRK, NVOIT
      character QELSM*8
C     !DASH
      external HI, BYE
C
      call HI ('HYGRE')
C     !BEG
      if((QELSM.eq.'H  ').and.(KSTRK.gt.0)) then
        IVOIT = 3
        NVOIT = 1
      end if
C     !END
      call BYE ('HYGRE')
C
      return
      end
