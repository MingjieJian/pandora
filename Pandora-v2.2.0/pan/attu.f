      subroutine ATTU
     $(F,N,CRIT,KODE,R,KMSS,LABEL,IMG,OLD,KERM,NERM)
C
C     Rudolf Loeser, 2002 Nov 06
C---- Printing utility, for editing.
C     (See also UTTA.)
C     !DASH
      save
C     !DASH
      real*8 CRIT, F, OLD, R
      integer IMG, KERM, KMSS, KODE, LUEO, N, NERM
      logical ERROR, NORMAL
      character HEADER*127, LABEL*(*), TITLE*127
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external  ETTA, TOOT, MESHED, MASHED, VECOUT, IMGPRNT, HI, BYE
      intrinsic max
C
C               F(N), OLD(N), IMG(N), R(N)
      dimension F(*), OLD(*), IMG(*), R(*)
C     !EJECT
C
      call HI ('ATTU')
C     !BEG
      call ETTA       (LABEL, TITLE, KERM, NERM, KMSS, NORMAL, ERROR)
C
      if(NORMAL.or.ERROR) then
        call TOOT     (HEADER, CRIT, KODE)
        call MESHED   ('KIESEL', 2)
        write (LUEO,100) HEADER,TITLE
  100   format(' ',A127)
        call IMGPRNT  (LUEO, IMG, N, 1)
        if(NORMAL) then
          call VECOUT (LUEO, OLD, N, 'Given set:')
          call VECOUT (LUEO, R  , N, 'Replacement set:')
          call VECOUT (LUEO, F  , N, 'Edited set:')
          KERM = KERM+1
        end if
        call MASHED   ('KIESEL')
      end if
C     !END
      call BYE ('ATTU')
C
      return
      end
