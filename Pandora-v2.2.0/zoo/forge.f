      subroutine FORGE
     $(KOD,WIDTH,P,IMAGE)
C     Rudolf Loeser, 1979 Jan 26
C---- Constructs a line image for SCHEMA.
C     !DASH
      save
C     !DASH
      integer I, JOD, KOD, POS, WIDTH
      character IMAGE*(*), P*1
C     !DASH
      dimension POS(16)
C
      data POS /   32768,   16384,    8192,    4096,
     $              2048,    1024,     512,     256,
     $               128,      64,      32,      16,
     $                 8,       4,       2,       1 /
C
C     !BEG
      JOD = KOD
      do 100 I = 1,WIDTH
        if(JOD.ge.POS(I)) then
          IMAGE(I:I) = P
          JOD = JOD-POS(I)
        end if
  100 continue
C     !END
C
      return
      end
