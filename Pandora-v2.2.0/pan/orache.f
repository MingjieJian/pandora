      subroutine ORACHE
     $(XCBL,KPRD)
C
C     Rudolf Loeser, 1995 Mar 03
C---- Sets the PRD switch in Continuum Data Blocks for
C     radiative transitions.
C     (This is version 2 of ORACHE.)
C     !DASH
      save
C     !DASH
      real*8 XCBL
      integer KKLPRD, KPRD
C     !COM
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(34),KKLPRD)
C     !DASH
      external HI, BYE
C
C               XCBL(Miklen)
      dimension XCBL(*)
C
      call HI ('ORACHE')
C     !BEG
      if(KPRD.ne.0) then
        XCBL(KKLPRD) = KPRD
      end if
C     !END
      call BYE ('ORACHE')
C
      return
      end
