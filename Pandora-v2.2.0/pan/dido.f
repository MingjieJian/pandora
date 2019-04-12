      subroutine DIDO
     $(X,F,N,TAU,KODE,TOPT,LAB,IMG,W)
C
C     Rudolf Loeser, 1981 Oct 30.
C---- Computes TAU along a ray.
C     Returns with KODE=1 if TAU seems OK, =0 if not.
C     (This is version 3 of DIDO.)
C     !DASH
      save
C     !DASH
      real*8 F, TAU, W, X
      integer IMG, KODE, N
      logical TOPT
      character LAB*(*)
C     !DASH
      external LUNGA, LUCAN, HI, BYE
C
      dimension W(*)
C
C               X(N), F(N), TAU(N), IMG(N)
      dimension X(*), F(*), TAU(*), IMG(*)
C
C
      call HI ('DIDO')
C     !BEG
      if(TOPT) then
        call LUNGA (X, F, N, TAU, KODE, LAB, IMG, W)
      else
        call LUCAN (X, F, N, TAU, KODE, LAB        )
      end if
C     !END
      call BYE ('DIDO')
C
      return
      end
