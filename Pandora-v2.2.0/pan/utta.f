      subroutine UTTA
     $(F,N,CRIT,KODE,MODE,KMSS,LABEL,IMG,OLD,KERM,NERM,ALLBAD)
C
C     Rudolf Loeser, 2002 Nov 05
C---- Printing utility, for editing.
C     (See also ATTU.)
C     !DASH
      save
C     !DASH
      real*8 CRIT, F, OLD
      integer IMG, KERM, KMSS, KODE, LUEO, MODE, N, NERM
      logical ALLBAD, ERROR, NORMAL
      character HEADER*127, LABEL*(*), TITLE*127
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external  ETTA, ABORT, MESHED, MASHED, VECOUT, LINER, IMGPRNT,
     $          OTTO, HI, BYE
      intrinsic max
C
C               F(N), OLD(N), IMG(N)
      dimension F(*), OLD(*), IMG(*)
C     !EJECT
C
      call HI ('UTTA')
C     !BEG
      call ETTA         (LABEL, TITLE, KERM, NERM, KMSS, NORMAL, ERROR)
      call OTTO         (HEADER, CRIT, KODE, MODE)
C
      if(ALLBAD) then
        call MESHED     ('EDITH', 1)
        write (LUEO,100) HEADER,TITLE
  100   format(' ',A127)
        call VECOUT     (LUEO, OLD, N, 'Given set:')
        call LINER      (1, LUEO)
        write (LUEO,101)
  101   format(' ','********** The given set is all bad - quit.')
        call ABORT
      else
C
        if(NORMAL.or.ERROR) then
          call MESHED   ('EDITH', 3)
          write (LUEO,100) HEADER,TITLE
          call IMGPRNT  (LUEO, IMG, N, 1)
          if(NORMAL) then
            call VECOUT (LUEO, OLD, N, 'Given set:')
            call VECOUT (LUEO, F  , N, 'Edited set:')
            KERM = KERM+1
          end if
          call MASHED   ('EDITH')
        end if
C
      end if
C     !END
      call BYE ('UTTA')
C
      return
      end
