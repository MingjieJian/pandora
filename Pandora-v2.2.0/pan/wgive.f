      subroutine WGIVE
     $(W,CALLER)
C
C     Rudolf Loeser, 1997 Oct 31
C---- Releases a W allotment, after verifying the integrity of the
C     start of storage.
C     !DASH
      save
C     !DASH
      real*8 W
      integer I, LUEO
      character CALLER*(*)
C     !COM
C---- WSAFE       as of 1997 Oct 31
      real*8      XSIGNAL
      common      /WSAFE/ XSIGNAL
C     .
C---- SALLOC      as of 1997 Oct 31
      integer     ISALLOC
      common      /SALLOC/ ISALLOC
C     Index of first allocatable cell in WORLD and IWORLD
C     (To reserve initial cells for error checking).
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, LINER, ABORT, WRLS, HI, BYE
C
      dimension W(*)
C
      call HI ('WGIVE')
C     !BEG
      if((W(1).ne.XSIGNAL).or.(W(ISALLOC-1).ne.XSIGNAL)) then
        call MESHED ('WGIVE',1)
        write (LUEO,100) CALLER,XSIGNAL
  100   format(' ','Storage corruption noted by WGIVE, called from ',A/
     $         ' ','SIGNAL =',1PE20.12)
        call LINER  (1,LUEO)
        write (LUEO,101) (W(I),I=1,(ISALLOC-1))
  101   format(' ',1P5E20.12)
        call ABORT
      end if
C
      call WRLS     (CALLER)
C     !END
      call BYE ('WGIVE')
C
      return
      end
