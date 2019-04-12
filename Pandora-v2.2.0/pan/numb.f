      subroutine NUMB
     $(X,W,BDI,KLTE,ABDEL,XNK,XND,IMG)
C     Rudolf Loeser, 1975 Jul 30
C---- Computes number densities.
C     !DASH
      save
C     !DASH
      real*8 ABDEL, BDI, W, X, XND, XNK
      integer IABDI, IMG, IN, IS, ISO, ISUM, J, KLTE, MOX, N, NL
      logical DUMP
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- ESPY        as of 2004 May 18
      logical     ESPION
      common      /ESPY/ ESPION
C     "Values range" constraint switch
C     .
C     !DASH
      external MOOD, ULAN, NAMIB, JASON, KULLOCH, MASHED, BUMF, WGIVE,
     $         HI, BYE
C
      dimension X(*), W(*)
C
C               XNK(N), XND(N,NL), ABDEL(N), BDI(N,NL), IMG(N)
      dimension XNK(*), XND(N,*),  ABDEL(*), BDI(*),    IMG(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IABDI ),(IN( 2),ISUM  ),(IN( 3),ISO   )
C     !EJECT
C
      call HI ('NUMB')
C     !BEG
C     (Get, and allocate, W allotment)
      call MOOD      (IN, IS, MOX, 'NUMB')
C
C---- Get ion abundance
      call ULAN      (X, N, ABDEL, W(IABDI))
C
C---- ( ?  dump, and header)
      call NAMIB     (DUMP, 'NUMB')
C---- Get continuum number density
      call JASON     (X,    KLTE, DUMP, BDI, W(IABDI), XNK,
     $                W(ISUM), W(ISO))
C---- Get levels number desities
      do 100 J = 1,NL
        call KULLOCH (X, J, KLTE, DUMP, BDI, W(IABDI), XND(1,J),
     $                W(ISUM), W(ISO))
  100 continue
      if(DUMP) then
        call MASHED  ('NUMB')
      end if
C
      if(ESPION) then
C----   Edit out "underflow"
        call BUMF    (XND, XNK, W(ISO), IMG)
      end if
C
C     (Give back W allotment
      call WGIVE     (W, 'NUMB')
C     !END
      call BYE ('NUMB')
C
      return
      end
