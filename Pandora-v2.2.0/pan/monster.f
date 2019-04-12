      subroutine MONSTER
     $(TF,NW,LFB,LINFLX,LINK)
C
C     Rudolf Loeser, 1991 Mar 18
C---- Provides checksums, for APEX.
C     (This is version 3 of MONSTER.)
C     !DASH
      save
C     !DASH
      real*8 TF
      integer LFB, LINK, NW
      logical LINFLX
      character FAB*1, LAB*18, TIT*24
C     !DASH
      external MORNEST, CHECKER, HI, BYE
C
C               TF(Nmkuse)
      dimension TF(*)
C
      call HI ('MONSTER')
C     !BEG
      call MORNEST (LINFLX, LINK, LFB, LAB, FAB)
C
      write (TIT,100) LAB,FAB
  100 format(A18,' Flx ',A1)
C
      call CHECKER (TF, 1, NW, TIT)
C     !END
      call BYE ('MONSTER')
C
      return
      end
