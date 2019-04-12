      subroutine WESLEY
     $(DH,CH,NH,DF,CF,NF)
C
C     Rudolf Loeser, 1992 Apr 09
C---- Makes a full set from a half set for TABOR.
C     !DASH
      save
C     !DASH
      real*8 CF, CH, DF, DH, TWO, ZERO
      integer I, IL, IR, IS, NF, NH
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external HI, BYE
C
C               DH(NH), CH(NH), DF(NF), CF(NF)
      dimension DH(*),  CH(*),  DF(*),  CF(*)
C
      call HI ('WESLEY')
C     !BEG
      if(DH(1).eq.ZERO) then
        DF(NH) = DH(1)
        CF(NH) = CH(1)*TWO
C
        NF = 2*NH-1
        IR = NH
        IL = NH
        IS = 2
      else
        NF = 2*NH
        IR = NH
        IL = NH+1
        IS = 1
      end if
C
      do 100 I = IS,NH
        IR = IR+1
        DF(IR) = DH(I)
        CF(IR) = CH(I)
        IL = IL-1
        DF(IL) =-DH(I)
        CF(IL) = CH(I)
  100 continue
C     !END
      call BYE ('WESLEY')
C
      return
      end
