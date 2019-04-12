      subroutine OLIVINE
     $(TRD,TVW,TSK,TRS,TIB,F,DPM,C,DP)
C
C     Rudolf Loeser, 1980 Aug 14
C---- Computes Damping Parameter.
C     (This is version 2 of OLIVINE.)
C     !DASH
      save
C     !DASH
      real*8 C, DP, DPM, F, TIB, TRD, TRS, TSK, TVW
      integer I, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external GABBRO, HI, BYE
C
C               TRD(N), TVW(N), TSK(N), TRS(N), DP(N), TIB(N), F(5),
      dimension TRD(*), TVW(*), TSK(*), TRS(*), DP(*), TIB(*), F(*),
C
C               C(5)
     $          C(*)
C
      call HI ('OLIVINE')
C     !BEG
      do 100 I = 1,N
        call GABBRO (TRD(I),TVW(I),TSK(I),TRS(I),TIB(I),F,DPM,C,DP(I))
  100 continue
C     !END
      call BYE ('OLIVINE')
C
      return
      end
