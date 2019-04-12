      subroutine IGIVE
     $(IW,CALLER)
C
C     Rudolf Loeser, 1997 Oct 31
C---- Releases an IW allotment, after verifying the integrity of the
C     start of storage.
C     !DASH
      save
C     !DASH
      integer I, IW, LUEO
      character CALLER*(*)
C     !COM
C---- IWSAFE      as of 1997 Oct 31
      integer     IXSIGNL
      common      /IWSAFE/ IXSIGNL
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
      external MESHED, ABORT, LINER, IRLS, HI, BYE
C
      dimension IW(*)
C
      call HI ('IGIVE')
C     !BEG
      if((IW(1).ne.IXSIGNL).or.(IW(ISALLOC-1).ne.IXSIGNL)) then
        call MESHED ('IGIVE',1)
        write (LUEO,100) CALLER,IXSIGNL
  100   format(' ','Storage corruption noted by IGIVE, called from ',A/
     $         ' ','SIGNAL =',I10)
        call LINER  (1,LUEO)
        write (LUEO,101) (IW(I),I=1,(ISALLOC-1))
  101   format(' ',10I10)
        call ABORT
      end if
C
      call IRLS     (CALLER)
C     !END
      call BYE ('IGIVE')
C
      return
      end
