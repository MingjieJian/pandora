      subroutine MINK
     $(JLEV,NPL,IN,IP)
C
C     Rudolf Loeser, 1972 Mar 02
C---- Computes MINK-type indices.
C     (This is version 3 of MINK.)
C     !DASH
      save
C     !DASH
      integer I, IN, IP, JLEV, NPL
C     !DASH
      external HI, BYE
C
      dimension NPL(*)
C
      call HI ('MINK')
C     !BEG
      IN = 1
      if(JLEV.gt.1) then
        do 100 I = 1,(JLEV-1)
          IN = IN+NPL(I)
  100   continue
      end if
      IP = IN+JLEV-1
C     !END
      call BYE ('MINK')
C
      return
      end
