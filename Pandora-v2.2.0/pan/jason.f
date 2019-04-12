      subroutine JASON
     $(X,NLTE,DUMP,BDI,ABDION,XNK,SUM,SO)
C
C     Rudolf Loeser, 1980 Aug 13
C---- Drives MEDEA to compute XNK, the number density of ionized atoms.
C     If NLTE=1, then nonLTE; if =0, then LTE.
C     !DASH
      save
C     !DASH
      real*8 ABDION, BDI, SO, SUM, X, XNK
      integer JJGM, JJPF, JJSA, JJXNE, NL, NLTE
      logical DUMP
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ(235),JJSA )
      equivalence (IZOQ(141),JJPF )
      equivalence (IZOQ( 16),JJGM )
C     !DASH
      external MEDEA, DAMEE, HI, BYE
C
      dimension X(*)
C
C               ABDION(N), XNK(N), SUM(N), SO(N), BDI(N,NL)
      dimension ABDION(*), XNK(*), SUM(*), SO(*), BDI(*)
C
      call HI ('JASON')
C     !BEG
      call MEDEA   (NL, NLTE, ABDION, X(JJXNE), X(JJSA), BDI, X(JJGM),
     $              XNK, SUM, SO)
      if(DUMP) then
        call DAMEE (NLTE, ABDION, X(JJPF), SUM, X(JJSA), SO, XNK)
      end if
C     !END
      call BYE ('JASON')
C
      return
      end
