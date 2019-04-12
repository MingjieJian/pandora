      subroutine STENKA
     $(XN1,SN1V,XNK,SNKV)
C
C     Rudolf Loeser, 2000 Jul 18
C---- Initializes the iterative analysis of Special-N1,NK.
C     !DASH
      save
C     !DASH
      real*8 SN1V, SNKV, XN1, XNK
C     !COM
C---- SPONNE      as of 2000 Dec 18
      integer     N1SKNT,NKSKNT
      common      /SPONNE/ N1SKNT,NKSKNT
C     Control parameter for Special-N1,NK iterative summary
C     .
C     !DASH
      external STICK, HI, BYE
C
C               ITMX = 3*ITN1R+1
C
C               XN1(N), SN1V(N,ITMX), XNK(N), SNKV(N,ITMX)
      dimension XN1(*), SN1V(*),      XNK(*), SNKV(*)
C
      call HI ('STENKA')
C     !BEG
      N1SKNT = 0
      NKSKNT = 0
C
      call STICK (3,XN1,SN1V,XNK,SNKV)
C     !END
      call BYE ('STENKA')
C
      return
      end
