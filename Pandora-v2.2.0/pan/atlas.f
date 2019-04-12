      subroutine ATLAS
     $(KIJ,NL)
C
C     Rudolf Loeser, 2006 Jun 12
C---- Fiddles with KIJ when OPTHINL = on.
C     !DASH
      save
C     !DASH
      integer I, J, KIJ, NL
C     !DASH
      external HI, BYE
C
C               KIJ(NL,NL)
      dimension KIJ(NL,*)
C
      call HI ('ATLAS')
C     !BEG
      do 101 J = 1,NL
        do 100 I = 1,NL
          if((KIJ(I,J).gt.0).and.(KIJ(I,J).ne.3)) then
            KIJ(I,J) = 3
          end if
  100   continue
  101 continue
C     !END
      call BYE ('ATLAS')
C
      return
      end
