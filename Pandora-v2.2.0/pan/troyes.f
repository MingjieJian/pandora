      subroutine TROYES
     $(IT,NT,TAUK,NW,WN,WH,ILFLX,LABEL,WRK)
C
C     Rudolf Loeser, 1982 Feb 04
C---- Prints debug data for angle-dependent Continuum Calculations.
C     !DASH
      save
C     !DASH
      real*8 TAUK, WH, WN, WRK
      integer ILFLX, IT, LUEO, NT, NW
      character LABEL*100
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, COMPACT, ARROUT, HI, BYE
C
C               TAUK(NT,NT), WH(NW,NW), WN(NW,NW), WRK(NT,NT)
      dimension TAUK(*),     WH(*),     WN(*),     WRK(*)
C
      call HI ('TROYES')
C     !BEG
      call LINER   (2, LUEO)
C
      write (LUEO,100) LABEL
  100 format(' ',A100)
C
      call COMPACT  (TAUK, IT, NT, NT, WRK)
      call ARROUT   (LUEO, WRK, IT, NT, 'TAUK'                    )
C
      call ARROUT   (LUEO, WN , NW, NW, 'WN, after WNJUNK-editing')
C
      if(ILFLX.gt.0) then
        call ARROUT (LUEO, WH , NW, NW, 'WH, after WNJUNK-editing')
      end if
C
      call LINER   (2, LUEO)
C     !END
      call BYE ('TROYES')
C
      return
      end
