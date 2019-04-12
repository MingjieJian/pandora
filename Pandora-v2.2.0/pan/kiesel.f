      subroutine KIESEL
     $(F,N,CRIT,KODE,R,KMSS,LABEL,IMG,OLD,KERM,NERM,BAD)
C
C     Rudolf Loeser, 1990 Jun 29
C---- Uses EDIT2 (q.v.) to edit "bad" values out of F by replacing
C     them with the corresponding values of R.
C
C     KODE = 1: "bad" means  .LT. CRIT;
C          = 2               .LE.     ;
C          = 3               .EQ.     ;
C          = 4               .GE.     ;
C          = 5               .GT.     .
C
C     KERM = count of error messages.
C     NERM = limit of error messages.
C     KMSS   tells whether to write advisory messages (limited by
C            NERM when appropriate) to unit LUEO.
C
C---- Upon return, BAD tells whether there were any "bad" values.
C     (This is version 2 of KIESEL.)
C     !DASH
      save
C     !DASH
      real*8 CRIT, F, OLD, R
      integer IMG, KERM, KMSS, KODE, N, NERM
      logical BAD
      character LABEL*(*)
C     !DASH
      external ITTY, MOVE1, EDIT2, ATTU, HI, BYE
C
C               F(N), R(N), IMG(N), OLD(N)
      dimension F(*), R(*), IMG(*), OLD(*)
C
      call HI ('KIESEL')
C     !BEG
C---- Make sure control parameter is OK, and save originals
      call ITTY   (KODE, 1)
      call MOVE1  (F, N, OLD)
C---- Edit (if necessary)
      call EDIT2  (F, N, CRIT, KODE, R, IMG, BAD)
      if(BAD) then
C----   Print a record of the changes (if needed, and permitted by KMSS)
        call ATTU (F, N, CRIT, KODE, R, KMSS, LABEL, IMG, OLD,
     $             KERM, NERM)
      end if
C     !END
      call BYE ('KIESEL')
C
      return
      end
