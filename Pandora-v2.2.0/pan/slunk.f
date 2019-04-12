      subroutine SLUNK
     $(LZA)
C
C     Rudolf Loeser, 1971 Feb 01
C---- Inspects ZAUX-table lengths and indices.
C     !DASH
      save
C     !DASH
      integer I, LZA, LZM, NZM
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(13),LZM)
      equivalence (JZQ(14),NZM)
C     !DASH
      external HI, BYE
C
C               LZA(50)
      dimension LZA(*)
C
      call HI ('SLUNK')
C     !BEG
      LZM = 0
      NZM = 0
      do 100 I = 1,50
        if(LZA(I).gt.0) then
          NZM = I
          if(LZM.lt.LZA(I)) then
            LZM = LZA(I)
          end if
        end if
  100 continue
C     !END
      call BYE ('SLUNK')
C
      return
      end
