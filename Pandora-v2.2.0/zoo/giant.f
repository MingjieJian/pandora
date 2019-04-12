      subroutine GIANT
     $(LETTER,LINUM,P,WIDTH,IMAGE)
C     Rudolf Loeser, 1979 Jan 28
C---- Returns line "LINUM" of the "IMAGE" of the character "LETTER",
C     constructed with the printing element "P"; also returns
C     the "WIDTH" (in printing elements) of the image of "LETTER".
C---- If the image of "LETTER" is not known, then blanks will be
C     returned in "IMAGE", with "WIDTH"=7.
C     !DASH
      save
C     !DASH
      integer ASCIP, I, INDEX, L, LINUM, WIDTH
      character IMAGE*(*), LETTER*1, P*1
C     !DASH
      external  SCHEMA
      intrinsic ichar
C----
      dimension I(128),L(128)
      data      I /
     $  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
     $  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
     $  1,  2,  3,  4,  5,  6,  1,  7,  8,  9, 10, 11, 12, 13, 14, 15,
     $ 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
     $  1, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46,
     $ 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,  1,  1,
     $  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
     $  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, 61, 62, 63,  1,  1 /
      data      L /
     $  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,
     $  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,
     $  7,  4,  5,  9,  9, 10,  7,  2,  4,  4,  7,  6,  3,  6,  4, 10,
     $  9,  4,  7,  8,  9,  8,  8,  8,  9,  8,  4,  4,  7,  6,  7,  6,
     $  7, 12,  8,  9,  9,  9,  9,  9, 10,  4,  5, 10,  8, 11, 10,  9,
     $  8,  9,  9,  9,  8, 10, 11, 13, 10, 10,  8,  4, 10,  4,  8,  7,
     $  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,
     $  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  4,  1,  4,  7,  7 /
C
C     !BEG
      ASCIP = ichar(LETTER)+1
      INDEX = I(ASCIP)
      WIDTH = L(ASCIP)
      call SCHEMA (INDEX,WIDTH,LINUM,P,IMAGE)
C     !END
C
      return
      end
