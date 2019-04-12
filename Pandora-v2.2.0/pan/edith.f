      subroutine EDITH
     $(F,N, CRIT,KODE,MODE, KMSS,LABEL,IMG,OLD,KERM,NERM, BAD)
C
C     Rudolf Loeser, 1990 Jun 21
C---- Uses EDIT1 (q.v.) to edit "bad" values out of F (length N).
C     (Length of LABEL should not exceed 100 characters.)
C
C     KODE = 1: "bad" means  .LT. CRIT;
C          = 2               .LE.     ;
C          = 3               .EQ.     ;
C          = 4               .GE.     ;
C          = 5               .GT.     .
C
C     MODE = 1: linear      interpolation;
C          = 2: logarithmic interpolation.
C
C     KERM = count of advisory messages;
C     NERM = limit of advisory messages.
C     KMSS   tells whether to write advisory messages (limited by
C            NERM when appropriate) to unit LUEO.
C
C---- Upon return, BAD tells whether there were any "bad" values.
C     (This is version 4 of EDITH.)
C     !DASH
      save
C     !DASH
      real*8 CRIT, F, OLD
      integer IMG, KERM, KMSS, KODE, MODE, N, NERM
      logical ALLBAD, BAD
      character LABEL*(*)
C     !DASH
      external ITTY, MOVE1, EDIT1, UTTA, HI, BYE
C
C               F(N), IMG(N), OLD(N)
      dimension F(*), IMG(*), OLD(*)
C
C
      call HI ('EDITH')
C     !BEG
C---- Make sure control parameters are OK, and save originals
      call ITTY   (KODE, MODE)
      call MOVE1  (F, N, OLD)
C---- Edit (if necessary)
      call EDIT1  (F, N, CRIT, KODE, MODE, IMG, BAD, ALLBAD)
      if(BAD) then
C----   Print a record of the changes (if needed, and permitted by KMSS)
C       >>>>> Aborts the run if ALLBAD
        call UTTA (F, N, CRIT, KODE, MODE, KMSS, LABEL, IMG, OLD,
     $             KERM, NERM, ALLBAD)
      end if
C     !END
      call BYE ('EDITH')
C
      return
      end
