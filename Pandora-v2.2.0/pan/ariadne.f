      subroutine ARIADNE
     $(I,N,EMU,VXS,DP,DW,GTN,VP,DV,PHI,CKL)
C
C     Rudolf Loeser, 1983 Feb 28
C---- Dumps, for Opacity Calculation along rays.
C     (This is version 4 of ARIADNE.)
C     !DASH
      save
C     !DASH
      real*8 CKL, DP, DV, DW, EMU, GTN, PHI, VP, VXS
      integer I, LUEO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external VECOUT, HI, BYE
C
C               EMU(N), VXS(N), DP(N), DW(N), GTN(N), PHI(N), CKL(N),
      dimension EMU(*), VXS(*), DP(*), DW(*), GTN(*), PHI(*), CKL(*),
C
C               VP(N), DV(N)
     $          VP(*), DV(*)
C
      call HI ('ARIADNE')
C     !BEG
      if(I.eq.1) then
        call VECOUT (LUEO, EMU, N, 'MU'                         )
        call VECOUT (LUEO, VXS, N, 'VXS'                        )
        call VECOUT (LUEO, DP , N, 'DP'                         )
        call VECOUT (LUEO, DW , N, 'DW'                         )
        call VECOUT (LUEO, GTN, N, 'GTN'                        )
      end if
C
      call VECOUT   (LUEO, VP , N, 'VP - relative projected VXS')
      call VECOUT   (LUEO, DV , N, 'DV - wavelength shift'      )
      call VECOUT   (LUEO, PHI, N, 'PHI - comoving'             )
      call VECOUT   (LUEO, CKL, N, 'CKL - Line Opacity'         )
C     !END
      call BYE ('ARIADNE')
C
      return
      end
