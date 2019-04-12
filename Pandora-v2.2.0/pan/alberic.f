      subroutine ALBERIC
     $(NO,LINE,LEN,RNUL,RNUR,XNU,XNUC,JLEV)
C
C     Rudolf Loeser, 1994 Jul 05
C---- Adds a wavelength legend to the JNU graph, for HARP.
C     !DASH
      save
C     !DASH
      real*8 RNUL, RNUR, XNU, XNUC, ZXH, ZXL
      integer JLEV, LEN, NO
      character LINE*(*)
C     !DASH
      external NOMAD, BELVOIR, HI, BYE
C
C               XNU(NSL), XNUC(NSL)
      dimension XNU(*),   XNUC(*)
C
      call HI ('ALBERIC')
C     !BEG
      if(NO.gt.0) then
        call NOMAD   (RNUL, XNU, XNUC, JLEV, ZXL)
        call NOMAD   (RNUR, XNU, XNUC, JLEV, ZXH)
C
        call BELVOIR (NO, LINE, LEN, ZXL, ZXH)
      end if
C     !END
      call BYE ('ALBERIC')
C
      return
      end
