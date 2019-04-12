      subroutine CLAUDE
     $(DF,CF,NF, DH,CH,NH)
C
C     Rudolf Loeser, 1992 Apr 09
C---- Extracts a half set from a full set for TABOR.
C     !DASH
      save
C     !DASH
      real*8 C, CF, CH, DF, DH, HALF, ZERO
      integer I, NF, NH
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
C     !DASH
      external HI, BYE
C
C               DF(NF), CF(NF), DH(NH), CH(NH)
      dimension DF(*),  CF(*),  DH(*),  CH(*)
C
      call HI ('CLAUDE')
C     !BEG
      NH = 0
      do 100 I = 1,NF
        if(DF(I).ge.ZERO) then
          if(DF(I).eq.ZERO) then
            C = CF(I)*HALF
          else
            C = CF(I)
          end if
C
          NH = NH+1
          DH(NH) = DF(I)
          CH(NH) = C
        end if
  100 continue
C     !END
      call BYE ('CLAUDE')
C
      return
      end
