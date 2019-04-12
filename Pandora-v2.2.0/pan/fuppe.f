      subroutine FUPPE
     $(EMU,L,NW,EMINT,LFB,LININT,LINK)
C
C     Rudolf Loeser, 1989 Nov 13
C---- Provides checksums, for VISHNU.
C     !DASH
      save
C     !DASH
      real*8 EMINT, EMU
      integer J, L, LFB, LINK, NW
      logical LININT
      character FAB*1, LAB*18, TIT*27
C     !DASH
      external MORNEST, CHECKER, HI, BYE
C
C               EMU(L), EMINT(Nmkuse,L)
      dimension EMU(*), EMINT(NW,*)
C
      call HI ('FUPPE')
C     !BEG
      call MORNEST   (LININT, LINK, LFB, LAB, FAB)
C
      do 101 J = 1,L
        write (TIT,100) LAB,FAB,J
  100   format(A18,' Int ',A1,I3)
C
        call CHECKER (EMINT(1,J), 1, NW, TIT)
  101 continue
C     !END
      call BYE ('FUPPE')
C
      return
      end
