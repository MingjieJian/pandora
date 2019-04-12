      subroutine MYNAH
     $(X,W,PHI,COP,GTN,TAU,EDINT,EDTAU,IMG,KODE)
C
C     Rudolf Loeser, 1969 Jan 05
C---- Supervises the TAU calculation.
C     !DASH
      save
C     !DASH
      real*8 COP, GTN, PHI, TAU, W, X
      integer IMG, JUNK, KODE, N
      logical EDINT, EDTAU
      character LABEL*100
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external CHARLEY, BELLOW, BUMBLE, HI, BYE
C
      dimension X(*), W(*)
C
C               PHI(N), COP(N), GTN(N), TAU(N), IMG(N)
      dimension PHI(*), COP(*), GTN(*), TAU(*), IMG(*)
C
      call HI ('MYNAH')
C     !BEG
C---- Set up dump label
      call BELLOW   (KODE, LABEL)
C
C---- Calculate
      call CHARLEY  (X, W, 1, N, PHI, COP, GTN, TAU, LABEL, JUNK,
     $               EDINT, EDTAU, IMG)
C
      if(JUNK.gt.0) then
C----   Trouble: print message and stop
        call BUMBLE (LABEL, JUNK, TAU, N, 'MYNAH', 1)
      end if
C     !END
      call BYE ('MYNAH')
C
      return
      end
