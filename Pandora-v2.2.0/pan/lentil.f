      subroutine LENTIL
     $(J,NSL,IQRK,IQRL,KOOL,JOOL,KSHL,DORK,DORL,DOJN)
C
C     Rudolf Loeser, 1980 Mar 11
C---- Sets control switches, for MANU.
C     (This is version 3 of LENTIL.)
C     !DASH
      save
C     !DASH
      integer IQRK, IQRL, J, NSL
      logical DOJN, DORK, DORL, JOOL, KOOL, KSHL
C     !DASH
      external HI, BYE
C
C               IQRK(NSL), IQRL(NSL)
      dimension IQRK(*),   IQRL(*)
C
      call HI ('LENTIL')
C     !BEG
      if(J.gt.NSL) then
        KSHL = .true.
        KOOL = .false.
        DORK = .false.
        DORL = .false.
C
      else
        KSHL = .false.
        KOOL = JOOL
        DORK = IQRK(J).gt.0
        DORL = IQRL(J).gt.0
      end if
C
      DOJN = DORK.or.DORL.or.KSHL
C     !END
      call BYE ('LENTIL')
C
      return
      end
