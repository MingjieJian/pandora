      subroutine JEKYLL
     $(JU,JL,MTR,KIJ,KU,KL,KTR)
C
C     Rudolf Loeser, 1991 Aug 23
C---- Reduces the arrays of transition indices, JU and JL (length MTR),
C     to arrays of radiative transition indices, KU and KL (length KTR).
C     (JU/JL) is a radiative transition if 1 .le. KIJ(JU,JL) .le. KLIM.
C     !DASH
      save
C     !DASH
      integer I, JL, JU, KIJ, KL, KTR, KU, MTR
      logical USE
C     !DASH
      external PECCARY, HI, BYE
C
C               JU(MUL), JL(MUL), KU(MUL), KL(MUL), KIJ(MUL)
      dimension JU(*),   JL(*),   KU(*),   KL(*),   KIJ(*)
C
      call HI ('JEKYLL')
C     !BEG
      KTR = 0
      do 100 I = 1,MTR
        call PECCARY (JU(I),JL(I),KIJ,USE)
C
        if(USE) then
          KTR = KTR+1
          KU(KTR) = JU(I)
          KL(KTR) = JL(I)
        end if
C
  100 continue
C     !END
      call BYE ('JEKYLL')
C
      return
      end
